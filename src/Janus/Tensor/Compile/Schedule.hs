{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Topological DAG scheduling, liveness-interval buffer coloring, and
-- zero-allocation static scratchpad planning for @Janus.Tensor@.
--
-- For statically-sized graphs, 'planScratchBytes' computes the exact peak byte
-- requirement of all intermediate buffers (with 64-byte SIMD alignment and
-- aggressive slot reuse across non-overlapping lifetimes). Under 'ScratchAuto'
-- (the default, 64 KiB budget) or 'ScratchCallerBuf', compiled C programs emit
-- **zero** @malloc@/@calloc@/@aligned_alloc@/@free@ calls!
module Janus.Tensor.Compile.Schedule
  ( -- * Memory & Scratchpad Allocation Policy
    ScratchPolicy (..),
    defaultScratchPolicy,

    -- * Scheduled Stages & Buffer Slots
    SomeStage (..),
    BufferSlot (..),
    ScheduledPlan (..),
    scheduleNode,
    planScratchBytes,
  )
where

import Control.Monad.State.Strict (State, execState, gets, modify')
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Foreign.Storable (Storable (sizeOf))
import Janus.Tensor.Algebra (TensorScalar)
import Janus.Tensor.Contract.Hypergraph
  ( BinaryStepMeta (..),
    ContractionTree (..),
    HyperEdge (..),
    Hypergraph (..),
    dimProduct,
    fromContractSpec,
  )
import Janus.Tensor.Contract.Plan (PlanConfig, planContraction)
import Janus.Tensor.IR
  ( AccumProgram (..),
    AccumStmt (..),
    ContractOperand (..),
    ContractSpec (..),
    Digest,
    HExprArgs (..),
    HNodeArgs (..),
    IxExpr (..),
    RawExpr (..),
    RawNode (..),
    STProgram (..),
    STStmt (..),
    rawNodeDigest,
    rawNodeShape,
  )
import Janus.Tensor.Index (DimEnv, sixEvalSize, sixStaticSize)

--------------------------------------------------------------------------------
-- 1. Scratchpad Allocation Policy
--------------------------------------------------------------------------------

data ScratchPolicy
  = -- | If all dimensions are static and @spScratchBytes <= budgetBytes@, place
    -- the entire scratchpad on the C stack ('withStackBytes' — ZERO heap
    -- allocation!). Otherwise allocate a single contiguous arena via
    -- 'alignedMalloc' / 'free'.
    ScratchAuto !Int
  | -- | Always use a caller-provided scratch buffer (ZERO heap allocation for
    -- ANY statically-sized program!).
    ScratchCallerBuf
  | -- | Always allocate a single contiguous arena via 'alignedMalloc' / 'free'.
    ScratchAlloc
  deriving (Eq, Ord, Show)

defaultScratchPolicy :: ScratchPolicy
defaultScratchPolicy = ScratchAuto (64 * 1024)

--------------------------------------------------------------------------------
-- 2. Scheduled Stages & Interval-Colored Buffer Slots
--------------------------------------------------------------------------------

data SomeStage where
  SomeStage ::
    (TensorScalar a) =>
    { stDigest :: !Digest,
      stNode :: !(RawNode i a),
      stNumElems :: !Int,
      stElemBytes :: !Int,
      stContractTree :: !(Maybe ContractionTree),
      stExtraScratchBytes :: !Int
    } ->
    SomeStage

data BufferSlot = BufferSlot
  { bsDigest :: !Digest,
    bsOffsetBytes :: !Int,
    bsSizeBytes :: !Int,
    bsNumElems :: !Int,
    bsElemBytes :: !Int
  }
  deriving (Eq, Show)

data ScheduledPlan = ScheduledPlan
  { spRootDigest :: !Digest,
    spStages :: ![SomeStage],
    spSlots :: !(Map.Map Digest BufferSlot),
    spScratchBytes :: !Int,
    spIsStatic :: !Bool
  }

align64 :: Int -> Int
align64 n = if n <= 0 then 0 else ((n + 63) `div` 64) * 64

-- | Compute the peak internal scratch bytes needed while executing a binary
-- 'ContractionTree' (excluding the final root output, which is written into
-- the stage's own destination buffer).
contractInternalScratchBytes :: Hypergraph -> ContractionTree -> Int
contractInternalScratchBytes hg = fst . go True
  where
    go isRoot (CLeaf eid survLs) =
      let rawLs = maybe [] heLabels (IM.lookup eid (hgEdges hg))
          sz = align64 (ceiling (dimProduct (hgVerts hg) survLs) * hgElemBytes hg)
       in if not isRoot && rawLs /= survLs
            then (sz, sz)
            else (0, 0)
    go isRoot (CBinary m l r) =
      let (pL, liveL) = go False l
          (pR, liveR) = go False r
          outB = if isRoot then 0 else align64 (ceiling (bsOutBytes m))
          -- Inside a binary node, children intermediates live while computing this node
          peakChildren = max pL (liveL + pR)
          peakHere = max peakChildren (liveL + liveR + outB)
       in (peakHere, outB)

-- | Compute extra internal scratchpad bytes needed inside a stage (e.g. local
-- 'MTab' buffers inside 'NRunST', or intermediate contraction tensors inside
-- 'NContract').
stageExtraBytes :: forall i a. (TensorScalar a) => DimEnv -> RawNode i a -> Maybe ContractionTree -> Int
stageExtraBytes env node mTree = case node of
  NContract _ _ spec ->
    case mTree of
      Just tr -> contractInternalScratchBytes (fromContractSpec env spec) tr
      Nothing -> 0
  NRunST _ _ (STProgram stms _) ->
    sum [align64 (sixEvalSize env s * sizeOf (undefined :: Double)) | STNewMTab _ s _ <- stms]
  NAssocScan _ _ s _ ->
    -- Double-buffer workspace for inclusive parallel scan (up to 16 bytes/elem for SIMDPair Double)
    align64 (sixEvalSize env s * 16)
  _ -> 0

--------------------------------------------------------------------------------
-- 3. Post-Order DAG Traversal & Linear-Scan Interval Coloring
--------------------------------------------------------------------------------

data CollectState = CollectState
  { csVisited :: !(Set.Set Digest),
    csRevStages :: ![SomeStage],
    csDeps :: !(Map.Map Digest [Digest]),
    csAllStatic :: !Bool
  }

-- | Topologically schedule a 'RawNode' DAG, plan optimal 'ContractionTree's
-- for every 'NContract' node, and pack all intermediate buffers into a single
-- flat 64-byte-aligned scratchpad via linear-scan liveness interval coloring.
scheduleNode ::
  forall i a.
  (TensorScalar a) =>
  PlanConfig ->
  DimEnv ->
  RawNode i a ->
  ScheduledPlan
scheduleNode planCfg env root =
  let finalSt =
        execState
          (visitNode root)
          CollectState
            { csVisited = Set.empty,
              csRevStages = [],
              csDeps = Map.empty,
              csAllStatic = True
            }
      stages = reverse (csRevStages finalSt)
      rootD = rawNodeDigest root
      -- Compute liveness intervals (defIdx, lastUseIdx) for intermediate nodes
      -- (inputs are caller-owned; root writes directly into caller output buffer)
      lastUseMap =
        foldl
          ( \acc (idx, stg) ->
              let d = stDigest stg
                  deps = Map.findWithDefault [] d (csDeps finalSt)
               in foldl (\m dep -> Map.insertWith max dep idx m) acc deps
          )
          Map.empty
          (zip [0 :: Int ..] stages)
      -- Collect intermediate buffer requests: (defIdx, lastUseIdx, digest, sizeBytes, nElems, elemB)
      intermediates =
        [ ( idx,
            Map.findWithDefault idx d lastUseMap,
            d,
            align64 (stNumElems stg * stElemBytes stg),
            stNumElems stg,
            stElemBytes stg
          )
          | (idx, stg) <- zip [0 :: Int ..] stages,
            let d = stDigest stg,
            d /= rootD,
            not (isInputStage stg)
        ]
      -- Linear-scan interval coloring for intermediate buffers
      (slotMap, interPeak) = packIntervals intermediates
      -- Plus maximum extra per-stage scratch (non-overlapping across sequential stages!)
      maxExtra = foldl max 0 (map stExtraScratchBytes stages)
      totalScratch = align64 (interPeak + maxExtra)
   in ScheduledPlan
        { spRootDigest = rootD,
          spStages = stages,
          spSlots = slotMap,
          spScratchBytes = totalScratch,
          spIsStatic = csAllStatic finalSt
        }
  where
    isInputStage (SomeStage _ (NInput {}) _ _ _ _) = True
    isInputStage _ = False

    visitNode :: forall j b. (TensorScalar b) => RawNode j b -> State CollectState Digest
    visitNode node = do
      let d = rawNodeDigest node
      seen <- gets (Set.member d . csVisited)
      if seen
        then pure d
        else do
          modify' (\st -> st {csVisited = Set.insert d (csVisited st)})
          let sh = rawNodeShape node
              isSt = isJust (sixStaticSize sh)
          modify' (\st -> st {csAllStatic = csAllStatic st && isSt})
          deps <- case node of
            NInput {} -> pure []
            NFor _ _ _ body -> visitExpr body
            NReindex _ _ inner -> (: []) <$> visitNode inner
            NScatter _ _ _ _ _ dstIx valE -> do
              d1 <- visitIx dstIx
              d2 <- visitExpr valE
              pure (d1 <> d2)
            NRunAccum _ _ _ (AccumProgram stms) -> concat <$> mapM visitAccStmt stms
            NRunST _ _ (STProgram stms _) -> concat <$> mapM visitSTStmt stms
            NScan _ _ initE xs _ _ stepE -> do
              d1 <- visitExpr initE
              d2 <- visitNode xs
              d3 <- visitExpr stepE
              pure (d1 <> (d2 : d3))
            NAssocScan _ _ _ xs -> (: []) <$> visitNode xs
            NContract _ _ spec ->
              mapM (\(ContractOperand opN _) -> visitNode opN) (csOperands spec)
            NKernelPrim _ _ args -> visitHNodes args
            NAnnotate _ _ inner -> (: []) <$> visitNode inner
          let mTree = case node of
                NContract _ _ spec ->
                  let hg = fromContractSpec env spec
                   in Just (planContraction planCfg hg)
                _ -> Nothing
              nElems = sixEvalSize env sh
              elemB = sizeOf (undefined :: b)
              extraB = stageExtraBytes env node mTree
              stg = SomeStage d node nElems elemB mTree extraB
          modify' $ \st ->
            st
              { csRevStages = stg : csRevStages st,
                csDeps = Map.insert d deps (csDeps st)
              }
          pure d

    visitHNodes :: forall shs b. (TensorScalar b) => HNodeArgs shs b -> State CollectState [Digest]
    visitHNodes HNNil = pure []
    visitHNodes (HNCons n rest) = do
      d <- visitNode n
      ds <- visitHNodes rest
      pure (d : ds)

    visitHExprs :: HExprArgs ts -> State CollectState [Digest]
    visitHExprs HANil = pure []
    visitHExprs (HACons e rest) = do
      d1 <- visitExpr e
      d2 <- visitHExprs rest
      pure (d1 <> d2)

    visitIx :: IxExpr k -> State CollectState [Digest]
    visitIx = \case
      IxPair a b -> (<>) <$> visitIx a <*> visitIx b
      IxFst p -> visitIx p
      IxSnd p -> visitIx p
      IxInL _ a -> visitIx a
      IxInR _ b -> visitIx b
      IxTagE _ a -> visitIx a
      IxUntagE a -> visitIx a
      IxApplyIso _ a -> visitIx a
      IxFromOrd _ e -> visitExpr e
      _ -> pure []

    visitExpr :: RawExpr b -> State CollectState [Digest]
    visitExpr = \case
      ELit {} -> pure []
      EScalarVar {} -> pure []
      EOrd _ ix -> visitIx ix
      EShapeSize {} -> pure []
      EUnOp _ _ x -> visitExpr x
      EBinOp _ _ x y -> (<>) <$> visitExpr x <*> visitExpr y
      ECmp _ _ x y -> (<>) <$> visitExpr x <*> visitExpr y
      ESelect _ c t f -> do
        d1 <- visitExpr c
        d2 <- visitExpr t
        d3 <- visitExpr f
        pure (d1 <> d2 <> d3)
      ECast _ x -> visitExpr x
      EIndex _ n ix -> do
        dn <- visitNode n
        di <- visitIx ix
        pure (dn : di)
      EReduce _ _ _ _ body -> visitExpr body
      EScalarPrim _ _ args -> visitHExprs args
      EAnnotate _ _ e -> visitExpr e
      EReadSTRef {} -> pure []
      EReadMTab _ _ _ ix -> visitIx ix
      ERunSTScalar _ (STProgram stms res) -> do
        ds <- concat <$> mapM visitSTStmt stms
        dr <- visitExpr res
        pure (ds <> dr)

    visitAccStmt :: AccumStmt k b -> State CollectState [Digest]
    visitAccStmt = \case
      AccumWrite ix e -> (<>) <$> visitIx ix <*> visitExpr e
      AccumParFor _ _ body -> concat <$> mapM visitAccStmt body
      AccumWhen c body -> (<>) <$> visitExpr c <*> (concat <$> mapM visitAccStmt body)

    visitSTStmt :: STStmt -> State CollectState [Digest]
    visitSTStmt = \case
      STNewRef _ e -> visitExpr e
      STWriteRef _ e -> visitExpr e
      STNewMTab _ _ n -> (: []) <$> visitNode n
      STWriteMTab _ _ ix e -> (<>) <$> visitIx ix <*> visitExpr e
      STFor _ _ body -> concat <$> mapM visitSTStmt body
      STWhile c body -> (<>) <$> visitExpr c <*> (concat <$> mapM visitSTStmt body)
      STWhen c body -> (<>) <$> visitExpr c <*> (concat <$> mapM visitSTStmt body)

-- | First-fit interval packing of @(defIdx, lastUseIdx, digest, sizeBytes, nElems, elemB)@
-- into a single contiguous byte buffer.
packIntervals ::
  [(Int, Int, Digest, Int, Int, Int)] ->
  (Map.Map Digest BufferSlot, Int)
packIntervals reqs =
  let sorted = sortOn (\(defI, _, _, _, _, _) -> defI) reqs
      go [] _ acc peak = (acc, peak)
      go ((defI, endI, dig, sz, nE, eB) : rest) active acc peak =
        -- Evict intervals whose lifetime ended strictly before defI
        let live = filter (\(_, _, e) -> e >= defI) active
            -- Find lowest 64-byte-aligned offset not overlapping any live [off, off+len)
            off = findFreeOffset 0 sz (sortOn (\(o, _, _) -> o) live)
            slot = BufferSlot dig off sz nE eB
            newPeak = max peak (off + sz)
         in go rest ((off, off + sz, endI) : live) (Map.insert dig slot acc) newPeak
      findFreeOffset cand _ [] = align64 cand
      findFreeOffset cand sz ((o, oEnd, _) : more)
        | align64 cand + sz <= o = align64 cand
        | otherwise = findFreeOffset (max cand oEnd) sz more
   in go sorted [] Map.empty 0

-- | Compute the exact peak scratchpad size in bytes required to execute a
-- statically-sized (or 'DimEnv'-bound) 'RawNode' DAG without any heap allocation.
planScratchBytes :: (TensorScalar a) => PlanConfig -> DimEnv -> RawNode i a -> Int
planScratchBytes cfg env = spScratchBytes . scheduleNode cfg env
