{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fusion and algebraic simplification pass for @Janus.Tensor@ IR graphs:
--
-- * Representable β-fusion: inlines single-use/cheap 'NFor' producers indexed
--   by 'EIndex' (@Index (For s v body) ix ~> substIxVar v ix body@).
-- * Structured optic fusion: collapses chained 'NReindex' nodes and eliminates
--   identity isomorphisms ('ixId') at zero runtime cost.
-- * Automatic contraction lifting (delegating to 'recognizeContractions').
module Janus.Tensor.Compile.Fuse
  ( FuseConfig (..),
    defaultFuseConfig,
    fuseNode,
    substIxVar,
    substIxVarInIx,
  )
where

import Data.Type.Equality ((:~:) (..))
import Janus.Tensor.Algebra (TensorScalar)
import Janus.Tensor.Contract.Recognize (recognizeContractions)
import Janus.Tensor.IR
  ( AccumProgram (..),
    AccumStmt (..),
    ContractOperand (..),
    ContractSpec (..),
    HExprArgs (..),
    HNodeArgs (..),
    IxExpr (..),
    RawExpr (..),
    RawNode (..),
    STProgram (..),
    STStmt (..),
    ixExprShape,
    mkAnnotateExpr,
    mkAnnotateNode,
    mkAssocScan,
    mkBinOp,
    mkCast,
    mkCmp,
    mkContract,
    mkFor,
    mkIndex,
    mkKernelPrimNode,
    mkOrd,
    mkReduce,
    mkReindex,
    mkRunAccum,
    mkRunST,
    mkRunSTScalar,
    mkScalarPrim,
    mkScan,
    mkScatter,
    mkSelect,
    mkUnOp,
  )
import Janus.Tensor.Index
  ( IxIso (..),
    IxMorph (..),
    eqSIx,
    ixCompIso,
    ixCompPrism,
  )

data FuseConfig = FuseConfig
  { fcRecognizeContractions :: !Bool,
    fcFuseForIndex :: !Bool,
    fcFuseReindex :: !Bool
  }
  deriving (Eq, Show)

defaultFuseConfig :: FuseConfig
defaultFuseConfig =
  FuseConfig
    { fcRecognizeContractions = True,
      fcFuseForIndex = True,
      fcFuseReindex = True
    }

-- | Run contraction recognition, optic composition fusion, and β-reduction
-- producer-consumer fusion on a 'RawNode' DAG.
fuseNode :: FuseConfig -> RawNode i a -> RawNode i a
fuseNode cfg n0 =
  let n1 = if fcRecognizeContractions cfg then recognizeContractions n0 else n0
   in goNode n1
  where
    goNode :: forall j b. RawNode j b -> RawNode j b
    goNode = \case
      n@(NInput {}) -> n
      NFor _ s v body ->
        mkFor s v (goExpr body)
      NReindex _ m inner ->
        let inner' = goNode inner
         in if fcFuseReindex cfg
              then simplifyReindex m inner'
              else mkReindex m inner'
      NScatter _ cm si sj v dstIx valE ->
        mkScatter cm si sj v (goIx dstIx) (goExpr valE)
      NRunAccum _ cm s (AccumProgram stms) ->
        mkRunAccum cm s (AccumProgram (map goAccumStmt stms))
      NRunST _ s (STProgram stms res) ->
        mkRunST s (STProgram (map goSTStmt stms) res)
      NScan _ s initE xs cv xv stepE ->
        mkScan s (goExpr initE) (goNode xs) cv xv (goExpr stepE)
      NAssocScan _ m s xs ->
        mkAssocScan m s (goNode xs)
      NContract _ sr spec ->
        let ops' = [ContractOperand (goNode opN) ls | ContractOperand opN ls <- csOperands spec]
         in mkContract sr (spec {csOperands = ops'})
      NKernelPrim _ spec args ->
        mkKernelPrimNode spec (goHNodes args)
      NAnnotate _ attrs inner ->
        mkAnnotateNode attrs (goNode inner)

    goHNodes :: HNodeArgs shs b -> HNodeArgs shs b
    goHNodes HNNil = HNNil
    goHNodes (HNCons n rest) = HNCons (goNode n) (goHNodes rest)

    goHExprs :: HExprArgs ts -> HExprArgs ts
    goHExprs HANil = HANil
    goHExprs (HACons e rest) = HACons (goExpr e) (goHExprs rest)

    goIx :: IxExpr k -> IxExpr k
    goIx = \case
      IxPair a b -> IxPair (goIx a) (goIx b)
      IxFst (IxPair a _) -> goIx a
      IxFst p -> IxFst (goIx p)
      IxSnd (IxPair _ b) -> goIx b
      IxSnd p -> IxSnd (goIx p)
      IxInL sb a -> IxInL sb (goIx a)
      IxInR sa b -> IxInR sa (goIx b)
      IxTagE p a -> IxTagE p (goIx a)
      IxUntagE (IxTagE _ a) -> goIx a
      IxUntagE a -> IxUntagE (goIx a)
      IxApplyIso (IsoRefl _) a -> goIx a
      IxApplyIso iso a -> IxApplyIso iso (goIx a)
      IxFromOrd s e -> IxFromOrd s (goExpr e)
      other -> other

    goExpr :: forall b. RawExpr b -> RawExpr b
    goExpr = \case
      e@(ELit {}) -> e
      e@(EScalarVar {}) -> e
      EOrd _ ix -> mkOrd (goIx ix)
      e@(EShapeSize {}) -> e
      EUnOp _ op x -> mkUnOp op (goExpr x)
      EBinOp _ op x y -> mkBinOp op (goExpr x) (goExpr y)
      ECmp _ op x y -> mkCmp op (goExpr x) (goExpr y)
      ESelect _ c t f -> mkSelect (goExpr c) (goExpr t) (goExpr f)
      ECast _ x -> mkCast (goExpr x)
      EIndex _ node ix ->
        let node' = goNode node
            ix' = goIx ix
         in case node' of
              -- β-fusion: inline cheap pointwise NFor directly into consumer!
              NFor _ _ v body
                | fcFuseForIndex cfg && isCheapPointwise body ->
                    goExpr (substIxVar v ix' body)
              -- Iso reindex fusion: indexing a reindex-by-iso directly applies the iso to ix!
              NReindex _ (MorphIso iso) inner
                | fcFuseReindex cfg ->
                    goExpr (mkIndex inner (IxApplyIso iso ix'))
              _ -> mkIndex node' ix'
      EReduce _ m s v body -> mkReduce m s v (goExpr body)
      EScalarPrim _ spec args -> mkScalarPrim spec (goHExprs args)
      EAnnotate _ attrs e -> mkAnnotateExpr attrs (goExpr e)
      e@(EReadSTRef {}) -> e
      EReadMTab d tid s ix -> EReadMTab d tid s (goIx ix)
      ERunSTScalar _ (STProgram stms r) -> mkRunSTScalar (STProgram (map goSTStmt stms) (goExpr r))

    goAccumStmt :: AccumStmt k b -> AccumStmt k b
    goAccumStmt = \case
      AccumWrite ix e -> AccumWrite (goIx ix) (goExpr e)
      AccumParFor s v body -> AccumParFor s v (map goAccumStmt body)
      AccumWhen c body -> AccumWhen (goExpr c) (map goAccumStmt body)

    goSTStmt :: STStmt -> STStmt
    goSTStmt = \case
      STNewRef r e -> STNewRef r (goExpr e)
      STWriteRef r e -> STWriteRef r (goExpr e)
      STNewMTab t s n -> STNewMTab t s (goNode n)
      STWriteMTab t s ix e -> STWriteMTab t s (goIx ix) (goExpr e)
      STFor s v body -> STFor s v (map goSTStmt body)
      STWhile c body -> STWhile (goExpr c) (map goSTStmt body)
      STWhen c body -> STWhen (goExpr c) (map goSTStmt body)

-- | Collapse chained 'NReindex' morphisms and drop identity isomorphisms.
simplifyReindex :: (TensorScalar a) => IxMorph i j -> RawNode j a -> RawNode i a
simplifyReindex (MorphIso (IsoRefl _)) inner = inner
simplifyReindex (MorphIso iso1) (NReindex _ (MorphIso iso2) inner) =
  mkReindex (MorphIso (ixCompIso iso2 iso1)) inner
simplifyReindex (MorphPrism p1) (NReindex _ (MorphPrism p2) inner) =
  mkReindex (MorphPrism (ixCompPrism p2 p1)) inner
simplifyReindex m inner = mkReindex m inner

-- | Heuristic: an 'NFor' body is safe and profitable to β-inline into a
-- consumer when it contains no inner reductions or local-ST loops (preventing
-- accidental asymptotic blowup).
isCheapPointwise :: RawExpr a -> Bool
isCheapPointwise = \case
  ELit {} -> True
  EScalarVar {} -> True
  EOrd {} -> True
  EShapeSize {} -> True
  EUnOp _ _ x -> isCheapPointwise x
  EBinOp _ _ x y -> isCheapPointwise x && isCheapPointwise y
  ECmp _ _ x y -> isCheapPointwise x && isCheapPointwise y
  ESelect _ c t f -> isCheapPointwise c && isCheapPointwise t && isCheapPointwise f
  ECast _ x -> isCheapPointwise x
  EIndex {} -> True
  EReduce {} -> False
  EScalarPrim _ _ args -> cheapArgs args
  EAnnotate _ _ x -> isCheapPointwise x
  EReadSTRef {} -> True
  EReadMTab {} -> True
  ERunSTScalar {} -> False
  where
    cheapArgs :: HExprArgs ts -> Bool
    cheapArgs HANil = True
    cheapArgs (HACons e rest) = isCheapPointwise e && cheapArgs rest

--------------------------------------------------------------------------------
-- Capture-Avoiding Index Variable Substitution
--------------------------------------------------------------------------------

substIxVarInIx :: forall k j. Int -> IxExpr k -> IxExpr j -> IxExpr j
substIxVarInIx targetVar repl = go
  where
    go :: forall m. IxExpr m -> IxExpr m
    go = \case
      v@(IxVar vid s)
        | vid == targetVar,
          Just Refl <- eqSIx (ixExprShape repl) s ->
            repl
        | otherwise -> v
      c@(IxConst {}) -> c
      IxPair a b -> IxPair (go a) (go b)
      IxFst p -> case go p of
        IxPair a _ -> a
        p' -> IxFst p'
      IxSnd p -> case go p of
        IxPair _ b -> b
        p' -> IxSnd p'
      IxInL sb a -> IxInL sb (go a)
      IxInR sa b -> IxInR sa (go b)
      IxTagE p a -> IxTagE p (go a)
      IxUntagE a -> IxUntagE (go a)
      IxApplyIso iso a -> IxApplyIso iso (go a)
      IxFromOrd s e -> IxFromOrd s (substIxVar targetVar repl e)

substIxVar :: forall k a. Int -> IxExpr k -> RawExpr a -> RawExpr a
substIxVar targetVar repl = go
  where
    go :: forall b. RawExpr b -> RawExpr b
    go = \case
      e@(ELit {}) -> e
      e@(EScalarVar {}) -> e
      EOrd _ ix -> mkOrd (substIxVarInIx targetVar repl ix)
      e@(EShapeSize {}) -> e
      EUnOp _ op x -> mkUnOp op (go x)
      EBinOp _ op x y -> mkBinOp op (go x) (go y)
      ECmp _ op x y -> mkCmp op (go x) (go y)
      ESelect _ c t f -> mkSelect (go c) (go t) (go f)
      ECast _ x -> mkCast (go x)
      EIndex _ node ix -> mkIndex (goNode node) (substIxVarInIx targetVar repl ix)
      EReduce _ m s v body
        | v == targetVar -> mkReduce m s v body
        | otherwise -> mkReduce m s v (go body)
      EScalarPrim _ spec args -> mkScalarPrim spec (goHExprs args)
      EAnnotate _ attrs e -> mkAnnotateExpr attrs (go e)
      e@(EReadSTRef {}) -> e
      EReadMTab d tid s ix -> EReadMTab d tid s (substIxVarInIx targetVar repl ix)
      ERunSTScalar _ (STProgram stms r) ->
        mkRunSTScalar (STProgram (map goSTStmt stms) (go r))

    goHExprs :: HExprArgs ts -> HExprArgs ts
    goHExprs HANil = HANil
    goHExprs (HACons e rest) = HACons (go e) (goHExprs rest)

    goNode :: forall m b. RawNode m b -> RawNode m b
    goNode = \case
      n@(NInput {}) -> n
      NFor _ s v body
        | v == targetVar -> mkFor s v body
        | otherwise -> mkFor s v (go body)
      NReindex _ m inner -> mkReindex m (goNode inner)
      NScatter _ cm si sj v dstIx valE
        | v == targetVar -> mkScatter cm si sj v dstIx valE
        | otherwise -> mkScatter cm si sj v (substIxVarInIx targetVar repl dstIx) (go valE)
      NRunAccum _ cm s (AccumProgram stms) ->
        mkRunAccum cm s (AccumProgram (map goAccStmt stms))
      NRunST _ s (STProgram stms res) ->
        mkRunST s (STProgram (map goSTStmt stms) res)
      NScan _ s initE xs cv xv stepE ->
        mkScan s (go initE) (goNode xs) cv xv (go stepE)
      NAssocScan _ m s xs -> mkAssocScan m s (goNode xs)
      NContract _ sr spec ->
        let ops' = [ContractOperand (goNode opN) ls | ContractOperand opN ls <- csOperands spec]
         in mkContract sr (spec {csOperands = ops'})
      NKernelPrim _ spec args -> mkKernelPrimNode spec (goHNodes args)
      NAnnotate _ attrs inner -> mkAnnotateNode attrs (goNode inner)

    goHNodes :: HNodeArgs shs b -> HNodeArgs shs b
    goHNodes HNNil = HNNil
    goHNodes (HNCons n rest) = HNCons (goNode n) (goHNodes rest)

    goAccStmt :: AccumStmt m b -> AccumStmt m b
    goAccStmt = \case
      AccumWrite ix e -> AccumWrite (substIxVarInIx targetVar repl ix) (go e)
      AccumParFor s v body
        | v == targetVar -> AccumParFor s v body
        | otherwise -> AccumParFor s v (map goAccStmt body)
      AccumWhen c body -> AccumWhen (go c) (map goAccStmt body)

    goSTStmt :: STStmt -> STStmt
    goSTStmt = \case
      STNewRef r e -> STNewRef r (go e)
      STWriteRef r e -> STWriteRef r (go e)
      STNewMTab t s n -> STNewMTab t s (goNode n)
      STWriteMTab t s ix e -> STWriteMTab t s (substIxVarInIx targetVar repl ix) (go e)
      STFor s v body
        | v == targetVar -> STFor s v body
        | otherwise -> STFor s v (map goSTStmt body)
      STWhile c body -> STWhile (go c) (map goSTStmt body)
      STWhen c body -> STWhen (go c) (map goSTStmt body)
