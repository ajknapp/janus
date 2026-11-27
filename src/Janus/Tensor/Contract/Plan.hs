{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Optimal N-ary Einstein tensor contraction planning:
--
-- * 'planExactDP': exact connected-subset dynamic programming (provably optimal
--   for @N <= 14@ operands).
-- * 'planExactTopK': exact top-@K@ structurally distinct Pareto contraction trees
--   for empirical benchmarking via 'Janus.Tensor.Bench'.
-- * 'planHeuristic' / 'planHeuristicTopK': scalable min-fill / min-cost greedy
--   + deterministic multi-start + 2-opt tree-rotation refinement for large
--   hypergraphs (@N > 14@), respecting optional peak-memory budgets.
module Janus.Tensor.Contract.Plan
  ( PlanConfig (..),
    defaultPlanConfig,
    PlanCandidate (..),
    planContraction,
    planTopK,
    planExactDP,
    planExactTopK,
    planHeuristic,
    planHeuristicTopK,
  )
where

import Data.Bits (popCount, shiftL, testBit, xor, (.&.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Word (Word64)
import Janus.Tensor.Contract.Hypergraph
  ( BinaryStepMeta (..),
    ContractionTree (..),
    CostModel (..),
    HyperEdge (..),
    Hypergraph (..),
    IndexId,
    PlanMetrics (..),
    classifyBinaryStep,
    dimProduct,
    evalTreeMetrics,
    gpuRooflineCostModel,
    treeLabels,
    treeSignature,
  )

data PlanConfig = PlanConfig
  { pcCostModel :: !CostModel,
    pcMaxMemoryBytes :: !(Maybe Double),
    pcExactThreshold :: !Int,
    pcTopK :: !Int,
    pcNumGreedyRestarts :: !Int
  }

defaultPlanConfig :: PlanConfig
defaultPlanConfig =
  PlanConfig
    { pcCostModel = gpuRooflineCostModel,
      pcMaxMemoryBytes = Nothing,
      pcExactThreshold = 14,
      pcTopK = 5,
      pcNumGreedyRestarts = 16
    }

data PlanCandidate = PlanCandidate
  { pcTree :: !ContractionTree,
    pcMetrics :: !PlanMetrics,
    pcId :: !String,
    pcDescription :: !String
  }
  deriving stock (Eq, Show)

mkCandidate :: CostModel -> Hypergraph -> Int -> ContractionTree -> PlanCandidate
mkCandidate cm hg rankIdx tree =
  let m = evalTreeMetrics cm hg tree
      sig = treeSignature tree
   in PlanCandidate
        { pcTree = tree,
          pcMetrics = m,
          pcId = "plan_" <> show rankIdx <> "_" <> sanitize sig,
          pcDescription = sig <> " [flops=" <> show (round (pmFlops m) :: Integer) <> "]"
        }
  where
    sanitize = map (\c -> if c == '(' || c == ')' || c == ',' then '_' else c)

--------------------------------------------------------------------------------
-- Shared Helpers
--------------------------------------------------------------------------------

-- | Compute the surviving output labels of a subset of operand indices.
survivingLabels :: Hypergraph -> [Int] -> IS.IntSet -> [IndexId]
survivingLabels hg allIds subsetIds =
  let vertsIn =
        IS.unions
          [ IS.fromList (heLabels (hgEdges hg IM.! eid))
            | eid <- IS.toList subsetIds
          ]
      vertsOut =
        IS.union
          (IS.fromList (hgOutLabels hg))
          ( IS.unions
              [ IS.fromList (heLabels (hgEdges hg IM.! eid))
                | eid <- allIds,
                  not (IS.member eid subsetIds)
              ]
          )
      kept = IS.intersection vertsIn vertsOut
   in if subsetIds == IS.fromList allIds
        then hgOutLabels hg
        else case IS.toList subsetIds of
          -- Preserve exact input axis order for single leaves (zero-copy, no spurious transpose!)
          [singleEid] ->
            [v | v <- heLabels (hgEdges hg IM.! singleEid), IS.member v kept]
          _ -> IS.toAscList kept

dedupTopK :: Int -> [(Double, ContractionTree)] -> [(Double, ContractionTree)]
dedupTopK k = take k . go Set.empty . sortOn fst
  where
    go _ [] = []
    go seen ((sc, tr) : rest) =
      let sig = treeSignature tr
       in if Set.member sig seen
            then go seen rest
            else (sc, tr) : go (Set.insert sig seen) rest

--------------------------------------------------------------------------------
-- 1. Exact Connected-Subset Dynamic Programming (N <= 14) & Top-K DP
--------------------------------------------------------------------------------

planExactDP :: CostModel -> Hypergraph -> ContractionTree
planExactDP cm hg =
  case planExactTopK 1 cm hg of
    (c : _) -> pcTree c
    [] -> error "planExactDP: cannot plan empty hypergraph with 0 operands"

planExactTopK :: Int -> CostModel -> Hypergraph -> [PlanCandidate]
planExactTopK kRaw cm hg =
  let k = max 1 kRaw
      edges = IM.toAscList (hgEdges hg)
      n = length edges
      allIds = map fst edges
   in case edges of
        [] -> []
        [(eid, _)] ->
          let leaf = CLeaf eid (hgOutLabels hg)
           in [mkCandidate cm hg 0 leaf]
        _ ->
          let idVec = IM.fromList (zip [0 ..] allIds)
              fullMask = (1 `shiftL` n) - 1 :: Int
              maskToSet m =
                IS.fromList [idVec IM.! bit | bit <- [0 .. n - 1], testBit m bit]
              -- Labels per bitmask (for fast connectivity check)
              edgeLabelSets = IM.fromList [(bit, IS.fromList (heLabels (snd e))) | (bit, e) <- zip [0 ..] edges]
              maskLabels m =
                IS.unions [edgeLabelSets IM.! bit | bit <- [0 .. n - 1], testBit m bit]
              -- Build DP table bottom-up by popCount
              masksBySize =
                IM.fromListWith (++) [(popCount m, [m]) | m <- [1 .. fullMask]]
              dp0 =
                IM.fromList
                  [ ( 1 `shiftL` bit,
                      let eid = idVec IM.! bit
                          surv = survivingLabels hg allIds (IS.singleton eid)
                          tr = CLeaf eid surv
                       in [(0.0, tr)]
                    )
                    | bit <- [0 .. n - 1]
                  ]
              stepMask dp mask =
                let subSet = maskToSet mask
                    outLs = survivingLabels hg allIds subSet
                    -- Enumerate proper non-empty submasks with symmetry breaking: sub < rest
                    submasks = collectSubmasks mask
                    connectedSplits =
                      [ (sub, rest)
                        | (sub, rest) <- submasks,
                          not (IS.null (IS.intersection (maskLabels sub) (maskLabels rest)))
                      ]
                    splits = if null connectedSplits then submasks else connectedSplits
                    cands =
                      [ (scL + scR + cmStepScore cm meta, CBinary meta tL tR)
                        | (sub, rest) <- splits,
                          (scL, tL) <- IM.findWithDefault [] sub dp,
                          (scR, tR) <- IM.findWithDefault [] rest dp,
                          let meta = classifyBinaryStep (hgVerts hg) (hgElemBytes hg) (treeLabels tL) (treeLabels tR) outLs
                      ]
                 in IM.insert mask (dedupTopK k cands) dp
              dpFinal =
                foldl
                  (\dp sz -> foldl stepMask dp (IM.findWithDefault [] sz masksBySize))
                  dp0
                  [2 .. n]
              topTrees = map snd (IM.findWithDefault [] fullMask dpFinal)
              ranked = sortOn (pmScore . evalTreeMetrics cm hg) topTrees
           in zipWith (mkCandidate cm hg) [0 ..] ranked
  where
    collectSubmasks :: Int -> [(Int, Int)]
    collectSubmasks mask = go ((mask - 1) .&. mask)
      where
        go 0 = []
        go sub =
          let rest = mask `xor` sub
              next = (sub - 1) .&. mask
           in if sub < rest
                then (sub, rest) : go next
                else go next

--------------------------------------------------------------------------------
-- 2. Scalable Greedy + Multi-Start + 2-Opt Tree-Rotation Heuristic (N > 14)
--------------------------------------------------------------------------------

splitMixNext :: Word64 -> (Word64, Double)
splitMixNext s =
  let s1 = s + 0x9e3779b97f4a7c15
      z0 = s1
      z1 = (z0 `xor` (z0 `div` (2 ^ (30 :: Int)))) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `div` (2 ^ (27 :: Int)))) * 0x94d049bb133111eb
      z3 = z2 `xor` (z2 `div` (2 ^ (31 :: Int)))
      u = fromIntegral (z3 `mod` 1000000) / 1000000.0
   in (s1, u)

-- | Single greedy contraction run with a scoring mode and optional perturbation.
greedyRun :: CostModel -> Hypergraph -> Int -> Word64 -> ContractionTree
greedyRun cm hg mode seed0 =
  let initNodes =
        [ ( IS.singleton eid,
            CLeaf eid (survivingLabels hg allIds (IS.singleton eid))
          )
          | eid <- allIds
        ]
   in loop initNodes seed0
  where
    allIds = IM.keys (hgEdges hg)
    loop [] _ = error "greedyRun: empty hypergraph"
    loop [(_, tr)] _ = tr
    loop nodes seed =
      let indexed = zip [0 :: Int ..] nodes
          pairs =
            [ ((i, nI), (j, nJ))
              | (i, nI) <- indexed,
                (j, nJ) <- indexed,
                i < j
            ]
          (seed', scoredPairs) =
            foldr
              ( \pair@((_, (pSL, pTL)), (_, (pSR, pTR))) (st, acc) ->
                  let (st1, u) = splitMixNext st
                      mergedSet = IS.union pSL pSR
                      outLs = survivingLabels hg allIds mergedSet
                      meta = classifyBinaryStep (hgVerts hg) (hgElemBytes hg) (treeLabels pTL) (treeLabels pTR) outLs
                      connected = not (null (bsK meta)) || not (null (bsBatch meta))
                      outSize = dimProduct (hgVerts hg) outLs
                      inSize = dimProduct (hgVerts hg) (treeLabels pTL) + dimProduct (hgVerts hg) (treeLabels pTR)
                      baseScore = case mode `mod` 4 of
                        0 -> cmStepScore cm meta
                        1 -> outSize - 0.5 * inSize -- classic min-size / net-reduction greedy
                        2 -> bsFlops meta
                        _ -> outSize * (1.0 + 0.1 * fromIntegral (length outLs))
                      connPenalty = if connected then 1.0 else 1.0e9
                      noise = if mode >= 4 then 0.8 + 0.4 * u else 1.0
                   in (st1, (baseScore * connPenalty * noise, meta, pair) : acc)
              )
              (seed, [])
              pairs
       in case sortOn (\(sc, _, _) -> sc) scoredPairs of
            [] -> error "greedyRun: no candidate pairs"
            ((_, bestMeta, ((bestI, (sL, tL)), (bestJ, (sR, tR)))) : _) ->
              let newNode = (IS.union sL sR, CBinary bestMeta tL tR)
                  restNodes = [n | (idx, n) <- indexed, idx /= bestI && idx /= bestJ]
               in loop (newNode : restNodes) seed'

-- | Rebuild metadata on a tree after a 2-opt structural rotation so every node's
-- 'BinaryStepMeta' and surviving output labels remain exact.
rebuildTree :: Hypergraph -> ContractionTree -> ContractionTree
rebuildTree hg = snd . go
  where
    allIds = IM.keys (hgEdges hg)
    go (CLeaf eid _) =
      let s = IS.singleton eid
       in (s, CLeaf eid (survivingLabels hg allIds s))
    go (CBinary _ l r) =
      let (sL, l') = go l
          (sR, r') = go r
          s = IS.union sL sR
          outLs = survivingLabels hg allIds s
          meta = classifyBinaryStep (hgVerts hg) (hgElemBytes hg) (treeLabels l') (treeLabels r') outLs
       in (s, CBinary meta l' r')

-- | Generate single-step 2-opt local-search tree rotations:
--   * @((A, B), C) -> (A, (B, C))@ and @(B, (A, C))@
--   * @(A, (B, C)) -> ((A, B), C)@ and @((A, C), B)@
treeRotations :: Hypergraph -> ContractionTree -> [ContractionTree]
treeRotations hg tr = map (rebuildTree hg) (go tr)
  where
    go (CLeaf _ _) = []
    go (CBinary m l r) =
      let topRots =
            ( case l of
                CBinary _ a b ->
                  [ CBinary m a (CBinary m b r),
                    CBinary m b (CBinary m a r)
                  ]
                _ -> []
            )
              <> ( case r of
                     CBinary _ b c ->
                       [ CBinary m (CBinary m l b) c,
                         CBinary m (CBinary m l c) b
                       ]
                     _ -> []
                 )
          leftRots = [CBinary m l' r | l' <- go l]
          rightRots = [CBinary m l r' | r' <- go r]
       in topRots <> leftRots <> rightRots

-- | Iterated 2-opt hill climbing until local optimum (capped at 32 steps).
refine2Opt :: PlanConfig -> Hypergraph -> ContractionTree -> (ContractionTree, [ContractionTree])
refine2Opt cfg hg = go (32 :: Int) []
  where
    scoreOf tr =
      let m = evalTreeMetrics (pcCostModel cfg) hg tr
          memPen = case pcMaxMemoryBytes cfg of
            Just lim | pmPeakBytes m > lim -> 1.0e18 * (pmPeakBytes m - lim)
            _ -> 0.0
       in pmScore m + memPen
    go :: Int -> [ContractionTree] -> ContractionTree -> (ContractionTree, [ContractionTree])
    go 0 visited cur = (cur, cur : visited)
    go steps visited cur =
      let curSc = scoreOf cur
          nbrs = treeRotations hg cur
          scoredNbrs = sortOn fst [(scoreOf n, n) | n <- nbrs]
       in case scoredNbrs of
            ((bestSc, bestN) : _)
              | bestSc < curSc * (1.0 - 1.0e-12) ->
                  go (steps - 1) (cur : visited) bestN
            _ -> (cur, cur : map snd (take 4 scoredNbrs) <> visited)

planHeuristicTopK :: PlanConfig -> Hypergraph -> [PlanCandidate]
planHeuristicTopK cfg hg =
  case IM.toList (hgEdges hg) of
    [] -> []
    [(eid, _)] ->
      [mkCandidate (pcCostModel cfg) hg 0 (CLeaf eid (hgOutLabels hg))]
    _ ->
      let restarts = max 4 (pcNumGreedyRestarts cfg)
          rawTrees =
            [ greedyRun (pcCostModel cfg) hg mode (0x123456789abcdef0 + fromIntegral mode * 9973)
              | mode <- [0 .. restarts - 1]
            ]
          allExplored = concatMap (snd . refine2Opt cfg hg) rawTrees
          scoreOf tr =
            let m = evalTreeMetrics (pcCostModel cfg) hg tr
                memPen = case pcMaxMemoryBytes cfg of
                  Just lim | pmPeakBytes m > lim -> 1.0e18 * (pmPeakBytes m - lim)
                  _ -> 0.0
             in pmScore m + memPen
          deduped =
            dedupTopK (max 1 (pcTopK cfg)) [(scoreOf tr, tr) | tr <- allExplored]
       in zipWith (mkCandidate (pcCostModel cfg) hg) [0 ..] (map snd deduped)

planHeuristic :: PlanConfig -> Hypergraph -> ContractionTree
planHeuristic cfg hg =
  case planHeuristicTopK cfg hg of
    (c : _) -> pcTree c
    [] -> error "planHeuristic: empty hypergraph"

--------------------------------------------------------------------------------
-- 3. Unified Entry Points
--------------------------------------------------------------------------------

-- | Compute the top-@K@ structurally distinct contraction trees for empirical
-- benchmarking or direct execution. Uses exact connected-subset DP for
-- @N <= pcExactThreshold@ and multi-start greedy + 2-opt refinement otherwise.
planTopK :: PlanConfig -> Hypergraph -> [PlanCandidate]
planTopK cfg hg =
  let n = IM.size (hgEdges hg)
   in if n <= pcExactThreshold cfg
        then
          let exactCands = planExactTopK (pcTopK cfg) (pcCostModel cfg) hg
           in case pcMaxMemoryBytes cfg of
                Nothing -> exactCands
                Just lim ->
                  let within = filter (\c -> pmPeakBytes (pcMetrics c) <= lim) exactCands
                   in if null within then exactCands else within
        else planHeuristicTopK cfg hg

-- | Compute the single best contraction tree according to 'PlanConfig'.
planContraction :: PlanConfig -> Hypergraph -> ContractionTree
planContraction cfg hg =
  case planTopK (cfg {pcTopK = 1}) hg of
    (c : _) -> pcTree c
    [] -> error "planContraction: empty hypergraph"
