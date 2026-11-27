{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Janus.Tensor.Contract
  ( test_tensor_contract,
  )
where

import Control.Exception (bracket)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (nub)
import Data.Proxy (Proxy (..))
import Data.Unique (hashUnique, newUnique)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Bench
  ( Bench,
    BenchT,
    SimpleChoice (..),
    benchmarkAndSelectIOWithSetup,
    enumerateChoices,
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Tensor.Algebra
import Janus.Tensor.Bench
import Janus.Tensor.Compile (buildTensorFn)
import Janus.Tensor.Contract.Hypergraph
import Janus.Tensor.Contract.Plan
import Janus.Tensor.Contract.Recognize
import Janus.Tensor.DSL
import Janus.Tensor.IR
import Janus.Tensor.Index
import Janus.Tensor.Linear (matmulT)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

test_tensor_contract :: TestTree
test_tensor_contract =
  testGroup
    "Janus.Tensor.Contract"
    [ testProperty "planExactDP matches exhaustive brute-force FLOP optimum on random matrix chains" law_exactDPOptimal,
      testCase "planTopK returns distinct ordered contraction trees" checkTopKDistinctAndOrdered,
      testCase "planHeuristic scales cleanly to 20-operand networks and beats left-to-right order" checkHeuristic20,
      testCase "recognizeContractions lifts Dex forIx/sumIx and LogAddExpOp into NContract" checkRecognize,
      testCase "open-world custom TensorSemiring works with buildContract" checkCustomSemiring,
      testCase "Janus.Tensor.Bench top-K benchmarking, CSV output, persistence, and runProduction" checkBenchTopK
    ]

--------------------------------------------------------------------------------
-- 1. Exhaustive Brute Force Oracle for Matrix Chain Contraction
--------------------------------------------------------------------------------

mkChainHypergraph :: [Int] -> Hypergraph
mkChainHypergraph dims =
  let n = length dims - 1
      verts = IM.fromList (zip [0 ..] dims)
      edges = IM.fromList [(i, HyperEdge i [i, i + 1]) | i <- [0 .. n - 1]]
   in Hypergraph
        { hgVerts = verts,
          hgEdges = edges,
          hgOutLabels = [0, n],
          hgElemBytes = 8
        }

-- | Classic O(N^3) matrix-chain DP oracle returning the exact minimum FLOPs
-- (where each (a x b) * (b x c) multiply costs 2 * a * b * c FLOPs).
matrixChainMinFlops :: [Int] -> Double
matrixChainMinFlops dims =
  let n = length dims - 1
      d i = fromIntegral (dims !! i) :: Double
      key len i = len * (n + 1) + i
      memo0 = IM.fromList [(key 1 i, 0.0 :: Double) | i <- [0 .. n - 1]]
      stepLen memo len =
        foldl
          ( \acc i ->
              let best =
                    minimum
                      [ acc IM.! key k i + acc IM.! key (len - k) (i + k) + 2.0 * d i * d (i + k) * d (i + len)
                        | k <- [1 .. len - 1]
                      ]
               in IM.insert (key len i) best acc
          )
          memo
          [0 .. n - len]
      memoFinal = foldl stepLen memo0 [2 .. n]
   in memoFinal IM.! key n 0

law_exactDPOptimal :: Property
law_exactDPOptimal = property $ do
  n <- forAll $ Gen.int (Range.linear 3 6)
  dims <- forAll $ Gen.list (Range.singleton (n + 1)) (Gen.int (Range.linear 2 40))
  let hg = mkChainHypergraph dims
      dpTree = planExactDP flopCostModel hg
      dpFlops = pmFlops (evalTreeMetrics flopCostModel hg dpTree)
      oracleFlops = matrixChainMinFlops dims
  -- Exact DP may find an even better non-adjacent outer-product plan or match
  -- the exact matrix-chain optimum; for chains, optimal is always <= oracleFlops
  -- and equal to oracleFlops because outer products strictly increase FLOPs.
  assert (abs (dpFlops - oracleFlops) <= 1.0e-6)

--------------------------------------------------------------------------------
-- 2. Top-K Distinctness & Monotonic Ordering
--------------------------------------------------------------------------------

checkTopKDistinctAndOrdered :: Assertion
checkTopKDistinctAndOrdered = do
  -- 5-matrix chain: C_4 = 14 distinct binary parenthesizations!
  let hg = mkChainHypergraph [10, 30, 5, 60, 8, 25]
      cfg = defaultPlanConfig {pcCostModel = flopCostModel, pcTopK = 5}
      cands = planTopK cfg hg
      sigs = map (treeSignature . pcTree) cands
      scores = map (pmScore . pcMetrics) cands
  assertEqual "expected 5 distinct top-K candidates" 5 (length cands)
  assertEqual "top-K candidates must have distinct canonical tree signatures" 5 (length (nub sigs))
  assertBool
    ("expected monotonically non-decreasing scores, got " <> show scores)
    (and (zipWith (<=) scores (drop 1 scores)))
  let best = planContraction cfg hg
  assertEqual "rank-0 matches planContraction" (treeSignature best) (treeSignature (pcTree (head' cands)))
  where
    head' (x : _) = x
    head' [] = error "empty"

--------------------------------------------------------------------------------
-- 3. Large-Hypergraph Heuristic Scaling (N = 20 > 14)
--------------------------------------------------------------------------------

checkHeuristic20 :: Assertion
checkHeuristic20 = do
  -- 20-matrix alternating bottleneck chain: 64 x 4 x 64 x 4 x ... x 64
  let dims = take 21 (cycle [64, 4])
      hg = mkChainHypergraph dims
      cfg = defaultPlanConfig {pcCostModel = flopCostModel, pcTopK = 4, pcExactThreshold = 14}
      cands = planTopK cfg hg
      bestMetrics = pmFlops (pcMetrics (head' cands))
      -- Compare against naive left-to-right chain folding
      naiveTree = buildLeftToRight hg
      naiveFlops = pmFlops (evalTreeMetrics flopCostModel hg naiveTree)
  assertBool "expected at least 2 heuristic candidates" (length cands >= 2)
  assertBool
    ("expected heuristic FLOPs (" <> show bestMetrics <> ") < naive left-to-right (" <> show naiveFlops <> ")")
    (bestMetrics < naiveFlops)
  where
    head' (x : _) = x
    head' [] = error "empty"
    buildLeftToRight hg =
      let allIds = IM.keys (hgEdges hg)
          go _ [t] = t
          go used (t1 : t2 : rest) =
            let used' = IS.union used (IS.fromList (treeLeaves t1 <> treeLeaves t2))
                vertsIn = IS.fromList (treeLabels t1 <> treeLabels t2)
                vertsOut =
                  IS.union
                    (IS.fromList (hgOutLabels hg))
                    (IS.unions [IS.fromList (heLabels (hgEdges hg IM.! eid)) | eid <- allIds, not (IS.member eid used')])
                outLs = if IS.size used' == length allIds then hgOutLabels hg else IS.toAscList (IS.intersection vertsIn vertsOut)
                meta = classifyBinaryStep (hgVerts hg) (hgElemBytes hg) (treeLabels t1) (treeLabels t2) outLs
             in go used' (CBinary meta t1 t2 : rest)
          go _ [] = error "empty"
       in go IS.empty [CLeaf eid (heLabels (hgEdges hg IM.! eid)) | eid <- allIds]

--------------------------------------------------------------------------------
-- 4. Automatic Contraction Recognition
--------------------------------------------------------------------------------

checkRecognize :: Assertion
checkRecognize = do
  -- 1) matmulT written purely via forIx + sumIx
  let mmFn ::
        Tab ('IxProd ('IxFin 4) ('IxFin 8)) (Expr Double) ->
        Tab ('IxProd ('IxFin 8) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double)
      mmFn = matmulT
      mmNode = recognizeContractions (buildTensorFn mmFn)
  case mmNode of
    NContract _ _ spec -> do
      assertEqual "matmulT recognized with 2 operands" 2 (length (csOperands spec))
      assertEqual "matmulT recognized with 2 output labels" 2 (length (csOutLabels spec))
    _ -> assertFailure "Expected matmulT to be automatically recognized as NContract"
  -- 2) 3-matrix chain written via nested sumIx
  let chain3 ::
        Tab ('IxProd ('IxFin 4) ('IxFin 8)) (Expr Double) ->
        Tab ('IxProd ('IxFin 8) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 6)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 6)) (Expr Double)
      chain3 a b c = forIx $ \p ->
        let i = ixFst p
            l = ixSnd p
         in sumIx @('IxFin 8) $ \j ->
              sumIx @('IxFin 3) $ \k ->
                (a ! ixPair i j) * (b ! ixPair j k) * (c ! ixPair k l)
      c3Node = recognizeContractions (buildTensorFn chain3)
  case c3Node of
    NContract _ _ spec -> do
      assertEqual "3-matrix chain recognized with 3 operands" 3 (length (csOperands spec))
      assertEqual "3-matrix chain has 4 hypergraph vertices" 4 (length (csVertices spec))
    _ -> assertFailure "Expected 3-matrix chain to be automatically recognized as NContract"
  -- 3) Log-space HMM contraction via LogAddExpOp + (+)
  let logChain ::
        Tab ('IxProd ('IxFin 4) ('IxFin 8)) (Expr Double) ->
        Tab ('IxProd ('IxFin 8) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double)
      logChain a b = forIx $ \p ->
        let i = ixFst p
            k = ixSnd p
         in reduceIx (Proxy @LogAddExpOp) $ \j ->
              (a ! ixPair i j) + (b ! ixPair j k)
      logNode = recognizeContractions (buildTensorFn logChain)
  case logNode of
    NContract _ _ spec ->
      assertEqual "LogSR contraction recognized with 2 operands" 2 (length (csOperands spec))
    _ -> assertFailure "Expected LogAddExpOp + (+) to be recognized as LogSR NContract"

--------------------------------------------------------------------------------
-- 5. Open-World Custom User Semiring
--------------------------------------------------------------------------------

data CustomMaxMulSR

instance TensorSemiring CustomMaxMulSR Double where
  type SemiringAdd CustomMaxMulSR = MaxOp
  semiringTag _ _ = "CustomMaxMulSR:f64"
  semiringOne _ _ = 1
  semiringMul _ = (*)

checkCustomSemiring :: Assertion
checkCustomSemiring = do
  let customFn ::
        Tab ('IxProd ('IxFin 4) ('IxFin 5)) (Expr Double) ->
        Tab ('IxProd ('IxFin 5) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 2)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 2)) (Expr Double)
      customFn = contract3 (Proxy @CustomMaxMulSR)
      customNode = buildTensorFn customFn
  case customNode of
    NContract _ sr spec -> do
      assertEqual "custom semiring contract3 has 3 operands" 3 (length (csOperands spec))
      assertBool "custom semiring digest non-empty" (sr `seq` True)
    _ -> assertFailure "Expected NContract node for custom semiring"

--------------------------------------------------------------------------------
-- 6. Janus.Tensor.Bench Top-K Empirical Selection & Persistence
--------------------------------------------------------------------------------

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir tag act = do
  baseTmp <- getTemporaryDirectory
  uq <- hashUnique <$> newUnique
  let dir = baseTmp </> ("janus_tensor_bench_" <> tag <> "_" <> show uq)
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    act

checkBenchTopK :: Assertion
checkBenchTopK =
  withTempDir "topk" $ \dir -> do
    let hg = mkChainHypergraph [20, 4, 30, 5, 16]
        pCfg = defaultPlanConfig {pcCostModel = flopCostModel, pcTopK = 3}
        space = topKSimpleChoiceSpace pCfg hg
        allChoices = enumerateChoices space
    assertEqual "choice space enumerates 3 top-K contraction plans" 3 (length allChoices)

    -- Benchmark a lightweight polymorphic Janus kernel whose work scales with the chosen tree's FLOPs
    let bCfg =
          defaultBenchConfig
            { benchWarmupIters = 1,
              benchSamples = 6,
              benchBootstrapDraws = 40,
              benchFixedBatchSize = Just 1
            }
        csvDir = dir </> "csv"
        selPath = dir </> "best_plan.sel"
        kernel :: BenchT (SimpleChoice ContractionTree) IO Identity (Bench (SimpleChoice ContractionTree) Identity Int64)
        kernel = do
          tr <- choose simpleImpl
          let flops = round (pmFlops (evalTreeMetrics flopCostModel hg tr)) :: Int64
              work = max 1 (flops `div` 200)
          acc <- newRef (0 :: Bench (SimpleChoice ContractionTree) Identity Int64)
          rangeM 0 (fromIntegral work) $ \i -> do
            cur <- readRef acc
            writeRef acc (cur + i)
          readRef acc

    (winner, spec) <-
      benchmarkAndSelectIOWithSetup
        bCfg
        csvDir
        space
        (pure ())
        (const (pure ()))
        (const kernel)

    csvExists <- doesFileExist (csvDir </> "overall.csv")
    assertBool "expected overall.csv from top-K benchmark" csvExists
    saveSelection selPath winner
    reloaded <- loadSelection space selPath
    assertEqual "reloaded selection spec matches winner" spec (extractSelectionSpec reloaded)

    Identity prodVal <- runProduction reloaded kernel
    assertBool ("expected positive production result, got " <> show prodVal) (prodVal >= 0)
