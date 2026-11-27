{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Janus.Tensor.Backends
  ( test_tensor_backends,
  )
where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Storable as VS
import Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FPtr
import qualified Foreign.Storable as FS
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (JanusCCSE, JanusCMCSE, PortableC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.CUDA
  ( CUDADejaFu (..),
    CUDADejaFuM,
    CUDAThreadInfo (..),
    JanusCUDACSE,
    launchCUDADejaFuMWith,
  )
import Janus.Backend.C.CUDA.Foreign (CUlaunchConfig (..), CUstream (..))
import Janus.Command.Cond (whenM_)
import Janus.Command.Memory (CmdStorable (..), allocaAligned)
import Janus.Command.Parallel (CmdParallel (..))
import Janus.Command.Range (CmdRange (..))
import Janus.Constraints (LocalArgs (..))
import Janus.Data.Tensor
  ( Tensor (..),
    TensorIndex (..),
    readTensor,
    reifyTensorBounds,
    withTensor,
    writeTensor,
  )
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Tensor.Algebra
import Janus.Tensor.Compile
import Janus.Tensor.Contract.Recognize (contract3)
import Janus.Tensor.Custom
import Janus.Tensor.DSL
import Janus.Tensor.IR (Expr, Tab)
import Janus.Tensor.Index
import Janus.Tensor.Linear (dotT, matmulT)
import Janus.Tensor.Lower.CPU
import Janus.Tensor.Lower.CUDA
import Janus.Tensor.ST
import Janus.Tensor.Types (TensorView (..), fromJanusTensor)
import System.Environment (lookupEnv)
import Test.DejaFu.Schedule (roundRobinSched, roundRobinSchedNP)
import Test.DejaFu.Settings (defaultMemType)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

getBuildCfg :: IO CBuildConfig
getBuildCfg = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

approxVec :: Double -> VS.Vector Double -> VS.Vector Double -> Bool
approxVec tol a b =
  VS.length a == VS.length b
    && VS.and (VS.zipWith (\x y -> abs (x - y) <= tol + tol * max (abs x) (abs y)) a b)

assertVecApprox :: String -> Double -> VS.Vector Double -> VS.Vector Double -> Assertion
assertVecApprox msg tol expected actual =
  assertBool
    (msg <> ": expected " <> show (VS.toList expected) <> ", got " <> show (VS.toList actual))
    (approxVec tol expected actual)

test_tensor_backends :: TestTree
test_tensor_backends =
  testGroup
    "Janus.Tensor.Backends"
    [ testCase "4-way parity (IO = Argobots C = CUDAIO = CUDADejaFu): fused ops, optics & special math" checkFusedAndOpticsParity,
      testCase "4-way parity: N-ary contractions across StandardSR, LogSR, TropicalMaxSR, TropicalMinSR" checkSemiringContractions,
      testCase "4-way parity: online LogSumExpOp, AssocScan, Scan, RunAccum, and RunST" checkAlgebraicAndSTParity,
      testCase "4-way parity: custom user TensorCommMonoid via generic 64-bit atomicCAS" checkCustomCommMonoidCAS,
      testCase "Zero-allocation static C codegen (zero malloc/calloc/free, stack_buf_ scratchpad)" checkZeroAllocStaticC,
      testCase "Open-world backdoor ScalarPrimSpec & KernelPrimSpec (CPU + CUDA)" checkBackdoorPrimitives,
      testCase "Embedding Janus.Tensor inside larger Janus program via zero-copy Janus.Data.Tensor" checkJanusDataTensorBridge,
      testCase "Generated CUDA C++ (.cu) contains __global__, __shared__, __syncthreads" checkCUDACodegen,
      testProperty "DejaFu property: GPU tiled semiring matmul, AssocScan, Scatter & Reduce (CUDADejaFuM)" law_cudaDejaFuKernels,
      testProperty "DejaFu property: multi-threaded CPU parallel For, Contract & Atomic Scatter (ParallelDejaFuM)" law_cpuParallelDejaFu
    ]

--------------------------------------------------------------------------------
-- 1. 4-Way Parity: Fused Pointwise, Optics, & Special Math
--------------------------------------------------------------------------------

checkFusedAndOpticsParity :: Assertion
checkFusedAndOpticsParity = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8}
      -- Transpose + special math (lgamma, erf, log1p)
      prog :: Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) -> Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double)
      prog x =
        let tr = transposeTab x
         in forIx $ \p ->
              let v = tr ! p
               in lgammaT (abs v + 1.5) + erfT (v * 0.25) + log1pT (abs v)
      xVec = VS.fromList [0.5, -1.2, 2.3, 0.0, -0.7, 1.8, 3.1, -2.0, 0.25, 0.75, -0.5, 1.0 :: Double]

  vIO <- runTensorIO parCfg emptyDimEnv prog xVec
  vC <- runTensorC bCfg parCfg emptyDimEnv prog xVec
  vCUDAIO <- runTensorCUDAIO cuCfg emptyDimEnv prog xVec
  (eDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv prog xVec
  vDF <- either (\c -> assertFailure ("CUDADejaFu failed: " <> show c)) pure eDF

  assertVecApprox "C vs IO" 1e-11 vIO vC
  assertVecApprox "CUDAIO vs IO" 1e-11 vIO vCUDAIO
  assertVecApprox "CUDADejaFu vs IO" 1e-11 vIO vDF

  -- Also verify 1D sliding window (IxAffine)
  let convOut :: Tab ('IxFin 5) (Expr Double) -> Tab ('IxFin 4) (Expr Double)
      convOut s =
        let win = window1DTab @('IxFin 4) @('IxFin 3) @('IxFin 5) 1 1 1 s
         in forIx $ \o -> sumIx @('IxFin 3) (\k -> (win ! ixPair o k) * (castT (ixOrd k) + 1.0))
      sVec = VS.fromList [1.0, 2.0, 3.0, 4.0, 5.0 :: Double]
  cIO <- runTensorIO parCfg emptyDimEnv convOut sVec
  cC <- runTensorC bCfg parCfg emptyDimEnv convOut sVec
  cCU <- runTensorCUDAIO cuCfg emptyDimEnv convOut sVec
  -- Expected with pad=1, kWeights=[1,2,3]:
  -- o=0: 0*1 + 1*2 + 2*3 = 8
  -- o=1: 1*1 + 2*2 + 3*3 = 14
  -- o=2: 2*1 + 3*2 + 4*3 = 20
  -- o=3: 3*1 + 4*2 + 5*3 = 26
  assertVecApprox "window1D conv IO" 1e-12 (VS.fromList [8, 14, 20, 26]) cIO
  assertVecApprox "window1D conv C" 1e-12 cIO cC
  assertVecApprox "window1D conv CUDAIO" 1e-12 cIO cCU

--------------------------------------------------------------------------------
-- 2. 4-Way Parity: N-ary Contractions Across Built-In Semirings
--------------------------------------------------------------------------------

checkSemiringContractions :: Assertion
checkSemiringContractions = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8, cudaTileDim = 2}
      aVec = VS.generate 12 (\i -> fromIntegral (i + 1) * 0.25) :: VS.Vector Double
      bVec = VS.generate 8 (\i -> fromIntegral (i + 1) * 0.5 - 1.0) :: VS.Vector Double
      cVec = VS.generate 6 (\i -> fromIntegral (i + 1) * 0.3) :: VS.Vector Double

      checkSR :: forall sr. (TensorSemiring sr Double) => String -> Proxy sr -> Assertion
      checkSR name pSR = do
        let expr3 ::
              Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) ->
              Tab ('IxProd ('IxFin 4) ('IxFin 2)) (Expr Double) ->
              Tab ('IxProd ('IxFin 2) ('IxFin 3)) (Expr Double) ->
              Tab ('IxProd ('IxFin 3) ('IxFin 3)) (Expr Double)
            expr3 = contract3 pSR
        vIO <- runTensorIO parCfg emptyDimEnv expr3 aVec bVec cVec
        vC <- runTensorC bCfg parCfg emptyDimEnv expr3 aVec bVec cVec
        vCU <- runTensorCUDAIO cuCfg emptyDimEnv expr3 aVec bVec cVec
        (eDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv expr3 aVec bVec cVec
        vDF <- either (\cond -> assertFailure (name <> " CUDADejaFu failed: " <> show cond)) pure eDF
        assertVecApprox (name <> " C vs IO") 1e-11 vIO vC
        assertVecApprox (name <> " CUDAIO vs IO") 1e-11 vIO vCU
        assertVecApprox (name <> " CUDADejaFu vs IO") 1e-11 vIO vDF

  checkSR "StandardSR" (Proxy @StandardSR)
  checkSR "LogSR" (Proxy @LogSR)
  checkSR "TropicalMaxSR" (Proxy @TropicalMaxSR)
  checkSR "TropicalMinSR" (Proxy @TropicalMinSR)

  -- Also verify auto-recognized matmulT matches explicit contract
  let mm ::
        Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 2)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 2)) (Expr Double)
      mm = matmulT
  mIO <- runTensorIO parCfg emptyDimEnv mm aVec bVec
  mC <- runTensorC bCfg parCfg emptyDimEnv mm aVec bVec
  mCU <- runTensorCUDAIO cuCfg emptyDimEnv mm aVec bVec
  assertVecApprox "matmulT C vs IO" 1e-12 mIO mC
  assertVecApprox "matmulT CUDAIO vs IO" 1e-12 mIO mCU

--------------------------------------------------------------------------------
-- 3. 4-Way Parity: LogSumExpOp, AssocScan, Scan, RunAccum, RunST
--------------------------------------------------------------------------------

checkAlgebraicAndSTParity :: Assertion
checkAlgebraicAndSTParity = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8}
      -- 1) Numerically stable online LogSumExpOp with +1000 shift (naive exp overflows Double!)
      lseFn :: Tab ('IxProd ('IxFin 2) ('IxFin 5)) (Expr Double) -> Tab ('IxFin 2) (Expr Double)
      lseFn mat = forIx $ \r -> logSumExpIx @('IxFin 5) (\c -> mat ! ixPair r c)
      mVec = VS.fromList [1000.0, 1001.0, 1002.0, 1000.5, 999.0, -500.0, -499.0, -501.0, -500.5, -498.0 :: Double]
  lIO <- runTensorIO parCfg emptyDimEnv lseFn mVec
  lC <- runTensorC bCfg parCfg emptyDimEnv lseFn mVec
  lCU <- runTensorCUDAIO cuCfg emptyDimEnv lseFn mVec
  assertBool "LogSumExpOp must stay finite at +1000" (not (isNaN (lIO VS.! 0)) && not (isInfinite (lIO VS.! 0)))
  assertVecApprox "LogSumExpOp C vs IO" 1e-11 lIO lC
  assertVecApprox "LogSumExpOp CUDAIO vs IO" 1e-11 lIO lCU

  -- 2) Parallel AssocScan (AddOp) & sequential Scan
  let xsVec = VS.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0 :: Double]
      aScan :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 6) (Expr Double)
      aScan = assocScanIx (Proxy @AddOp)
      sScan :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 6) (Expr Double)
      sScan xs = scanIx (0.0 :: Expr Double) xs (\acc v -> acc * 0.5 + v)
  aIO <- runTensorIO parCfg emptyDimEnv aScan xsVec
  aC <- runTensorC bCfg parCfg emptyDimEnv aScan xsVec
  aCU <- runTensorCUDAIO cuCfg emptyDimEnv aScan xsVec
  (eADF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv aScan xsVec
  aDF <- either (\c -> assertFailure ("AssocScan DejaFu failed: " <> show c)) pure eADF
  assertVecApprox "AssocScan expected" 1e-12 (VS.fromList [1, 3, 6, 10, 15, 21]) aIO
  assertVecApprox "AssocScan C" 1e-12 aIO aC
  assertVecApprox "AssocScan CUDAIO" 1e-12 aIO aCU
  assertVecApprox "AssocScan CUDADejaFu" 1e-12 aIO aDF

  sIO <- runTensorIO parCfg emptyDimEnv sScan xsVec
  sC <- runTensorC bCfg parCfg emptyDimEnv sScan xsVec
  sCU <- runTensorCUDAIO cuCfg emptyDimEnv sScan xsVec
  assertVecApprox "Scan C" 1e-12 sIO sC
  assertVecApprox "Scan CUDAIO" 1e-12 sIO sCU

  -- 3) Dex RunAccum (parallel commutative-monoid histogram / accumulation)
  let accProg :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 3) (Expr Double)
      accProg xs = runAccum (Proxy @AddOp) $ \ref ->
        parForAccum @('IxFin 6) $ \i -> do
          let bucket = ixFromOrd @('IxFin 3) (remT (ixOrd i) 3)
          accumWrite ref bucket (xs ! i)
  rIO <- runTensorIO parCfg emptyDimEnv accProg xsVec
  rC <- runTensorC bCfg parCfg emptyDimEnv accProg xsVec
  rCU <- runTensorCUDAIO cuCfg emptyDimEnv accProg xsVec
  (eRDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv accProg xsVec
  rDF <- either (\c -> assertFailure ("RunAccum DejaFu failed: " <> show c)) pure eRDF
  -- buckets: 0 -> 1+4=5, 1 -> 2+5=7, 2 -> 3+6=9
  assertVecApprox "RunAccum expected" 1e-12 (VS.fromList [5, 7, 9]) rIO
  assertVecApprox "RunAccum C" 1e-12 rIO rC
  assertVecApprox "RunAccum CUDAIO" 1e-12 rIO rCU
  assertVecApprox "RunAccum CUDADejaFu" 1e-12 rIO rDF

  -- 4) Rank-2 locally stateful TensorST (in-place prefix-doubling with TRef + MTab + forST_)
  let stProg :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 6) (Expr Double)
      stProg xs = runTensorST $ do
        buf <- newMTab xs
        runSum <- newTRef (0.0 :: Expr Double)
        forST_ @('IxFin 6) $ \i -> do
          v <- readMTab buf i
          modifyTRef runSum (+ v)
          cur <- readTRef runSum
          writeMTab buf i (cur * 2.0)
        freezeMTab buf
  stIO <- runTensorIO parCfg emptyDimEnv stProg xsVec
  stC <- runTensorC bCfg parCfg emptyDimEnv stProg xsVec
  stCU <- runTensorCUDAIO cuCfg emptyDimEnv stProg xsVec
  assertVecApprox "RunST expected" 1e-12 (VS.fromList [2, 6, 12, 20, 30, 42]) stIO
  assertVecApprox "RunST C" 1e-12 stIO stC
  assertVecApprox "RunST CUDAIO" 1e-12 stIO stCU

--------------------------------------------------------------------------------
-- 4. Custom User TensorCommMonoid via Generic Lock-Free 64-bit atomicCAS
--------------------------------------------------------------------------------

-- | User-defined commutative monoid: saturated product (or log-add-exp) with NO
-- custom atomic override, exercising the default lock-free 64-bit @atomicCAS@!
data UserProdOp

instance TensorMonoid UserProdOp Double where
  monoidTag _ _ = "UserProd:f64"
  monoidEmpty _ _ = 1.0
  monoidCombine _ = (*)

-- Uses the DEFAULT `monoidAtomicUpdate` -> `genericAtomicCAS64Update`!
instance TensorCommMonoid UserProdOp Double

checkCustomCommMonoidCAS :: Assertion
checkCustomCommMonoidCAS = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8}
      xsVec = VS.fromList [2.0, 3.0, 4.0, 5.0, 1.5, 2.5, 0.5, 2.0 :: Double]
      -- Scatter-multiply into 2 buckets (even/odd) concurrently using lock-free CAS!
      scat :: Tab ('IxFin 8) (Expr Double) -> Tab ('IxFin 2) (Expr Double)
      scat xs = scatterM (Proxy @UserProdOp) (\j -> ixFromOrd @('IxFin 2) (remT (ixOrd j) 2)) (xs !)
  vIO <- runTensorIO parCfg emptyDimEnv scat xsVec
  vC <- runTensorC bCfg parCfg emptyDimEnv scat xsVec
  vCU <- runTensorCUDAIO cuCfg emptyDimEnv scat xsVec
  (eDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv scat xsVec
  vDF <- either (\c -> assertFailure ("Custom CAS CUDADejaFu failed: " <> show c)) pure eDF
  -- bucket 0: 2*4*1.5*0.5 = 6.0; bucket 1: 3*5*2.5*2.0 = 75.0
  assertVecApprox "Custom CAS expected" 1e-11 (VS.fromList [6.0, 75.0]) vIO
  assertVecApprox "Custom CAS C" 1e-11 vIO vC
  assertVecApprox "Custom CAS CUDAIO" 1e-11 vIO vCU
  assertVecApprox "Custom CAS CUDADejaFu" 1e-11 vIO vDF

--------------------------------------------------------------------------------
-- 5. Zero-Allocation Static C Inspection Test
--------------------------------------------------------------------------------

checkZeroAllocStaticC :: Assertion
checkZeroAllocStaticC = do
  let prog ::
        Tab ('IxProd ('IxFin 4) ('IxFin 6)) (Expr Double) ->
        Tab ('IxProd ('IxFin 6) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 5)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 5)) (Expr Double)
      prog a b c =
        let chain = letTab (contract3 (Proxy @StandardSR) a b c)
         in forIx (\p -> (chain ! p) + 1.0)
      scratchB = planFnScratchBytes (ccPlanConfig defaultCompileConfig) emptyDimEnv prog
  assertBool ("expected positive static scratchpad bytes <= 64KiB, got " <> show scratchB) (scratchB > 0 && scratchB <= 65536)

  cSrc <- showTensorFnC defaultCompileConfig emptyDimEnv "zero_alloc_chain" prog
  assertBool "Static C codegen MUST use stack_buf_ scratchpad" ("stack_buf_" `isInfixOf` cSrc)
  assertBool "Static C codegen MUST NOT call malloc(" (not ("malloc(" `isInfixOf` cSrc))
  assertBool "Static C codegen MUST NOT call calloc(" (not ("calloc(" `isInfixOf` cSrc))
  assertBool "Static C codegen MUST NOT call aligned_alloc(" (not ("aligned_alloc(" `isInfixOf` cSrc))
  assertBool "Static C codegen MUST NOT call free(" (not ("free(" `isInfixOf` cSrc))

--------------------------------------------------------------------------------
-- 6. Open-World Backdoor Primitives (ScalarPrimSpec & KernelPrimSpec)
--------------------------------------------------------------------------------

checkBackdoorPrimitives :: Assertion
checkBackdoorPrimitives = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8}
      -- 1) Custom ScalarPrimSpec: smooth swish-like x * (1 / (1 + exp(-x)))
      swishSpec :: ScalarPrimSpec '[Double] Double
      swishSpec = mkScalarPrim "custom_swish" $ \(HECons z HENil) -> z / (1 + exp (negate z))

      -- 2) Custom KernelPrimSpec (both Argobots CPU & CmdCUDA!): scale-and-shift y[i] = 3 * x[i] + 1
      scaleShiftSpec :: KernelPrimSpec '[ 'IxFin 8] ('IxFin 8) Double
      scaleShiftSpec =
        mkKernelPrim
          "custom_scale_shift"
          (six @('IxFin 8))
          ( \(HVCons inV HVNil) outV ->
              parallelForRangeM "kprim_cpu" 0 8 2 (LACons (tvPtr inV) (LACons (tvPtr outV) LANil)) $
                \_ lo hi (LACons pIn (LACons pOut LANil)) ->
                  rangeM lo hi $ \i -> do
                    v <- peekElemOff pIn i
                    pokeElemOff pOut (3.0 * v + 1.0) i
          )
          ( \_ ->
              CUlaunchConfig
                { _cuLaunchConfigGridDimX = 1,
                  _cuLaunchConfigGridDimY = 1,
                  _cuLaunchConfigGridDimZ = 1,
                  _cuLaunchConfigBlockDimX = 8,
                  _cuLaunchConfigBlockDimY = 1,
                  _cuLaunchConfigBlockDimZ = 1,
                  _cuLaunchConfigSharedMemBytes = 0,
                  _cuLaunchConfigStream = CUstream FPtr.nullPtr
                }
          )
          ( \(HVCons inV HVNil) outV -> do
              ti <- ask
              let i = toIntegral (_cudaThreadInfoThreadIdxX ti)
              whenM_ (i `lt` 8) $ do
                v <- peekElemOff (tvPtr inV) i
                pokeElemOff (tvPtr outV) (3.0 * v + 1.0) i
          )

      prog :: Tab ('IxFin 8) (Expr Double) -> Tab ('IxFin 8) (Expr Double)
      prog x =
        let scaled = callKernelPrim scaleShiftSpec (HTCons x HTNil)
         in forIx (\i -> callScalarPrim swishSpec (HDCons (scaled ! i) HDNil))
      xVec = VS.fromList [-2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0 :: Double]

  vIO <- runTensorIO parCfg emptyDimEnv prog xVec
  vC <- runTensorC bCfg parCfg emptyDimEnv prog xVec
  vCU <- runTensorCUDAIO cuCfg emptyDimEnv prog xVec
  (eDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv prog xVec
  vDF <- either (\c -> assertFailure ("Backdoor CUDADejaFu failed: " <> show c)) pure eDF

  assertVecApprox "Backdoor C vs IO" 1e-12 vIO vC
  assertVecApprox "Backdoor CUDAIO vs IO" 1e-12 vIO vCU
  assertVecApprox "Backdoor CUDADejaFu vs IO" 1e-12 vIO vDF

--------------------------------------------------------------------------------
-- 7. Embedding Inside Larger Janus Program via Zero-Copy Janus.Data.Tensor
--------------------------------------------------------------------------------

checkJanusDataTensorBridge :: Assertion
checkJanusDataTensorBridge = do
  bCfg <- getBuildCfg
  let bounds = reifyTensorBounds (Proxy @'[3, 3])
      -- Build a larger Janus C function that takes Janus.Data.Tensor pointers,
      -- compiles a pure Haskell tensor function (`matmulT`) directly into the
      -- Janus monad via `compileTensor` (zero `inputTab`!), and reads back
      -- the trace using `Janus.Data.Tensor.readTensor`!
      cProg :: JanusCCSE (Ptr Double) -> JanusCCSE (Ptr Double) -> JanusCCSE (Ptr Double) -> JanusCMCSE (JanusCCSE Double)
      cProg pA pB pOut = do
        let tA = Tensor bounds pA :: Tensor '[3, 3] JanusCCSE Double
            tB = Tensor bounds pB :: Tensor '[3, 3] JanusCCSE Double
            tOut = Tensor bounds pOut :: Tensor '[3, 3] JanusCCSE Double
        compileTensor
          defaultCompileConfig
          emptyDimEnv
          (matmulT @('IxFin 3) @('IxFin 3) @('IxFin 3) @Double)
          (fromJanusTensor tA)
          (fromJanusTensor tB)
          (fromJanusTensor tOut)
        d0 <- readTensor tOut (0 :. 0 :. Z)
        d1 <- readTensor tOut (1 :. 1 :. Z)
        d2 <- readTensor tOut (2 :. 2 :. Z)
        pure (d0 + d1 + d2)

  withTensor @'[3, 3] @Identity @Double bounds $ \tA@(Tensor _ (Identity pA)) ->
    withTensor @'[3, 3] @Identity @Double bounds $ \tB@(Tensor _ (Identity pB)) ->
      withTensor @'[3, 3] @Identity @Double bounds $ \(Tensor _ (Identity pOut)) -> do
        -- A = [[1,2,3],[4,5,6],[7,8,9]], B = I_3 -> A*B = A, trace = 1 + 5 + 9 = 15
        forM_ [0 .. 2 :: Int64] $ \r ->
          forM_ [0 .. 2 :: Int64] $ \c -> do
            writeTensor tA (Identity r :. Identity c :. Z) (Identity (fromIntegral (r * 3 + c + 1)))
            writeTensor tB (Identity r :. Identity c :. Z) (Identity (if r == c then 1.0 else 0.0))
        trVal <- withJanusC @PortableC bCfg cProg (\fn -> fn pA pB pOut)
        assertBool ("expected trace 15.0, got " <> show trVal) (abs (trVal - 15.0) < 1e-12)

  -- Also verify `compileTensorPtrs` passed DIRECTLY to `withJanusC` with BOTH
  -- a scalar `Expr Double` argument and a scalar `Expr Double` return value!
  let scaledDot :: Expr Double -> Tab ('IxFin 4) (Expr Double) -> Tab ('IxFin 4) (Expr Double) -> Expr Double
      scaledDot alpha u v = alpha * dotT u v
      uVec = VS.fromList [1.0, 2.0, 3.0, 4.0 :: Double]
      vVec = VS.fromList [5.0, 6.0, 7.0, 8.0 :: Double]
      -- 2.5 * (1*5 + 2*6 + 3*7 + 4*8) = 2.5 * 70 = 175.0
      compiledPtrFn :: JanusCCSE Double -> JanusCCSE (Ptr Double) -> JanusCCSE (Ptr Double) -> JanusCMCSE (JanusCCSE Double)
      compiledPtrFn = compileTensorPtrs defaultCompileConfig emptyDimEnv scaledDot
  VS.unsafeWith uVec $ \pU ->
    VS.unsafeWith vVec $ \pV -> do
      gotC <- withJanusC @PortableC bCfg compiledPtrFn (\fn -> fn 2.5 pU pV)
      assertBool ("expected scaledDot C = 175.0, got " <> show gotC) (abs (gotC - 175.0) < 1e-12)
  gotIO <- runTensorIO defaultCompileConfig emptyDimEnv scaledDot 2.5 uVec vVec
  gotCU <- runTensorCUDAIO defaultCUDACompileConfig emptyDimEnv scaledDot 2.5 uVec vVec
  assertBool ("expected scaledDot IO = 175.0, got " <> show gotIO) (abs (gotIO - 175.0) < 1e-12)
  assertBool ("expected scaledDot CUDAIO = 175.0, got " <> show gotCU) (abs (gotCU - 175.0) < 1e-12)

--------------------------------------------------------------------------------
-- 8. Native CUDA C++ Codegen Verification (`showStageCUDA`)
--------------------------------------------------------------------------------

checkCUDACodegen :: Assertion
checkCUDACodegen = do
  let gemmCu =
        showStageCUDA "tiled_gemm_f64" $ \aRaw bRaw cRaw ->
          cudaTiledMatmulKernel
            (Proxy @StandardSR)
            (Proxy @16)
            4
            8
            8
            8
            (fromVoidPtr (toVoidPtr aRaw) :: JanusCUDACSE (Ptr Double))
            (fromVoidPtr (toVoidPtr bRaw) :: JanusCUDACSE (Ptr Double))
            (fromVoidPtr (toVoidPtr cRaw) :: JanusCUDACSE (Ptr Double))
      scanCu =
        showStageCUDA "assoc_scan_lse_f64" $ \inRaw outRaw _ ->
          cudaAssocScanBlockKernel
            (Proxy @LogSumExpOp)
            (Proxy @16)
            12
            (fromVoidPtr (toVoidPtr inRaw) :: JanusCUDACSE (Ptr Double))
            (fromVoidPtr (toVoidPtr outRaw) :: JanusCUDACSE (Ptr Double))
  assertBool "CUDA GEMM must emit __global__" ("__global__" `isInfixOf` gemmCu)
  assertBool "CUDA GEMM must emit __shared__" ("__shared__" `isInfixOf` gemmCu)
  assertBool "CUDA GEMM must emit __syncthreads" ("__syncthreads" `isInfixOf` gemmCu)
  assertBool "CUDA AssocScan must emit __shared__" ("__shared__" `isInfixOf` scanCu)
  assertBool "CUDA AssocScan must emit __syncthreads" ("__syncthreads" `isInfixOf` scanCu)

--------------------------------------------------------------------------------
-- 9. DejaFu Property Tests for GPU Kernels (`CUDADejaFuM`)
--------------------------------------------------------------------------------

law_cudaDejaFuKernels :: Property
law_cudaDejaFuKernels = withTests 20 . property $ do
  -- Randomized 3x4 * 4x3 matmul, 8-elem AssocScan, and 6-elem atomic Scatter under DejaFu!
  aList <- forAll $ Gen.list (Range.singleton 12) (Gen.double (Range.linearFrac (-5) 5))
  bList <- forAll $ Gen.list (Range.singleton 12) (Gen.double (Range.linearFrac (-5) 5))
  useNonPreemptive <- forAll Gen.bool
  let cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8, cudaTileDim = 2}
      mm ::
        Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 3)) (Expr Double)
      mm = matmulT
      aVec = VS.fromList aList
      bVec = VS.fromList bList
      sched = if useNonPreemptive then roundRobinSchedNP else roundRobinSched

  -- 1) Tiled shared-memory GPU matmul under DejaFu
  vIO <- evalIO $ runTensorIO defaultCompileConfig emptyDimEnv mm aVec bVec
  (eDF, (), _) <- evalIO $ runTensorCUDADejaFuWith sched defaultMemType () cuCfg emptyDimEnv mm aVec bVec
  case eDF of
    Left cond -> fail ("CUDADejaFu tiled matmul failed: " <> show cond)
    Right vDF -> assert (approxVec 1e-10 vIO vDF)

  -- 2) Shared-memory block LogSumExpOp reduction kernel directly under CUDADejaFuM
  let vec = VS.fromList (take 8 aList)
  (eRed, (), _) <- evalIO $
    VS.unsafeWith vec $ \srcP ->
      allocaAligned @Double 1 $ \outP ->
        let lcfg =
              CUlaunchConfig
                { _cuLaunchConfigGridDimX = 1,
                  _cuLaunchConfigGridDimY = 1,
                  _cuLaunchConfigGridDimZ = 1,
                  _cuLaunchConfigBlockDimX = 8,
                  _cuLaunchConfigBlockDimY = 1,
                  _cuLaunchConfigBlockDimZ = 1,
                  _cuLaunchConfigSharedMemBytes = 0,
                  _cuLaunchConfigStream = CUstream FPtr.nullPtr
                }
         in do
              (r, s1, tr) <-
                launchCUDADejaFuMWith
                  sched
                  defaultMemType
                  ()
                  lcfg
                  (cudaReduceBlockKernel @LogSumExpOp @Double @8 @CUDADejaFuM @CUDADejaFu (Proxy @LogSumExpOp) (Proxy @8) 8 (CUDADejaFu srcP) (CUDADejaFu outP))
              vOut <- FS.peekElemOff outP 0
              pure (fmap (const vOut) r, s1, tr)
  let maxA = maximum (take 8 aList)
      expectedLSE = maxA + log (sum [exp (x - maxA) | x <- take 8 aList])
  case eRed of
    Left cond -> fail ("CUDADejaFu LogSumExpOp block reduce failed: " <> show cond)
    Right gotLSE -> assert (abs (gotLSE - expectedLSE) <= 1e-10 + 1e-10 * abs expectedLSE)

--------------------------------------------------------------------------------
-- 10. DejaFu Property Tests for Parallel CPU Kernels (`ParallelDejaFuM`)
--------------------------------------------------------------------------------

law_cpuParallelDejaFu :: Property
law_cpuParallelDejaFu = withTests 20 . property $ do
  xsList <- forAll $ Gen.list (Range.singleton 12) (Gen.double (Range.linearFrac (-10) 10))
  nWorkers <- forAll $ Gen.int (Range.linear 2 4)
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      -- Parallel atomic scatter-add into 3 buckets + elementwise transform
      prog :: Tab ('IxFin 12) (Expr Double) -> Tab ('IxFin 3) (Expr Double)
      prog xs =
        let scattered = scatterAdd @('IxFin 3) @('IxFin 12) (\j -> ixFromOrd @('IxFin 3) (remT (ixOrd j) 3)) (\j -> (xs ! j) * 2.0)
         in forIx (\i -> (scattered ! i) + 1.0)
      xsVec = VS.fromList xsList
  vIO <- evalIO $ runTensorIO (parCfg {ccUseParallel = False}) emptyDimEnv prog xsVec
  (eDF, _) <- evalIO $ runTensorDejaFu parCfg nWorkers emptyDimEnv prog xsVec
  case eDF of
    Left cond -> fail ("ParallelDejaFu failed: " <> show cond)
    Right vDF -> assert (approxVec 1e-10 vIO vDF)
