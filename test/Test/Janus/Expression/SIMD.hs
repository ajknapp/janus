{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Expression.SIMD where

import Control.Concurrent (runInBoundThread)
import Control.Exception (evaluate, try)
import Control.Lens ((&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray, withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (sizeOf))
import GHC.Float (expm1, log1p)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (acquireJanusC, acquireJanusCWith, releaseJanusC, showJanusCWith, withJanusC, withJanusCWith)
import Janus.Backend.C.Build (CBuildConfig (..), cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.CUDA
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.LinuxX86_64
import Janus.Command.Cond (CmdCond, whenM_)
import qualified Janus.Command.Memory as M
import Janus.Command.Range (CmdRange, rangeM)
import Janus.Command.Ref (CmdRef, letM, modifyRef, newRef, readRef)
import Janus.Expression.Cast (ExpIntegralCast (toIntegral))
import Janus.Expression.Inject (ExpInject (inject))
import Janus.Expression.Let (ExpLet (let_))
import Janus.Expression.Math (FMA (fma), Hypot (hypot))
import Janus.Expression.Ord (ExpOrd (lt))
import Janus.Expression.SIMD
import Janus.Typed (JanusTyped)
import System.Environment (lookupEnv)
import Test.DejaFu.Schedule (roundRobinSchedNP)
import Test.DejaFu.Settings (defaultMemType)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import Test.Tasty.Hedgehog (testProperty)

getTestBuildConfig :: IO CBuildConfig
getTestBuildConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

--------------------------------------------------------------------------------
-- 1. Target Feature Lattices, Admission, & Explicit Cross-Target Codegen
--------------------------------------------------------------------------------

unit_janus_elem_tag :: IO ()
unit_janus_elem_tag = do
  let allTags = [minBound .. maxBound :: JanusElemTag]
      instanceTags =
        [ simdElemTag (Proxy @Int8),
          simdElemTag (Proxy @Word8),
          simdElemTag (Proxy @Int16),
          simdElemTag (Proxy @Word16),
          simdElemTag (Proxy @Int32),
          simdElemTag (Proxy @Word32),
          simdElemTag (Proxy @Int64),
          simdElemTag (Proxy @Word64),
          simdElemTag (Proxy @Float),
          simdElemTag (Proxy @Double)
        ]
  allTags @?= instanceTags
  map fromEnum allTags @?= [0 .. 9]
  (minBound :: JanusElemTag) @?= JanusElemInt8
  (maxBound :: JanusElemTag) @?= JanusElemDouble
  assertBool "Show & Ord instances work" (JanusElemInt8 < JanusElemDouble && show JanusElemFloat == "JanusElemFloat")

unit_x86_and_cuda_feature_lattices :: IO ()
unit_x86_and_cuda_feature_lattices = do
  -- Implication closure: enabling AVX2 implies AVX, SSE4.2, SSE4.1, SSSE3, SSE3, SSE2
  let avx2Only = x86Feature X86_AVX2
  assertBool "AVX2 implies AVX" (hasX86Feature avx2Only X86_AVX)
  assertBool "AVX2 implies SSE4.2" (hasX86Feature avx2Only X86_SSE4_2)
  assertBool "AVX2 implies SSE4.1" (hasX86Feature avx2Only X86_SSE4_1)
  assertBool "AVX2 implies SSSE3" (hasX86Feature avx2Only X86_SSSE3)
  assertBool "AVX2 implies SSE2" (hasX86Feature avx2Only X86_SSE2)

  -- Downward stripping: removing AVX strips AVX2, FMA, and F16C
  let stripped = withoutX86Feature X86_AVX x86AVX2
  assertBool "Stripping AVX removes AVX2" (not (hasX86Feature stripped X86_AVX2))
  assertBool "Stripping AVX removes FMA" (not (hasX86Feature stripped X86_FMA))
  assertBool "Stripping AVX preserves SSE4.2" (hasX86Feature stripped X86_SSE4_2)

  -- Lattice laws
  (x86SSE4_1 \/ x86AVX2) @?= x86AVX2
  (x86SSE4_1 /\ x86AVX2) @?= x86SSE4_1
  (lowerBound :: X86Features) @?= x86Baseline

  -- CUDA SM lattice & capabilities
  assertBool "sm_60 lacks dp4a" (not (cudaHasDP4A cudaSM60))
  assertBool "sm_61 has dp4a" (cudaHasDP4A cudaSM61)
  (cudaSM60 \/ cudaSM80) @?= cudaSM80
  (cudaSM60 /\ cudaSM80) @?= cudaSM60

  -- Host query & scoped FFI target override
  hostFs <- observeHostX86Features
  assertBool "Host supports baseline SSE2" (admitsX86Features hostFs x86Baseline)
  withHostX86Features x86Baseline $ do
    active <- getActiveHostX86Features
    active @?= x86Baseline
  restored <- getActiveHostX86Features
  restored @?= hostFs

unit_explicit_cross_target_codegen_selection :: IO ()
unit_explicit_cross_target_codegen_selection = do
  -- x86-64: 8xFloat FMA + Gather under SSE2 vs AVX2+FMA
  let x86Kern ::
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Int32) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
        LinuxX86_64CM (LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float))
      x86Kern base idx b c = do
        a <- vgather base idx
        pure (vfma a b c)

      cSSE2 = showJanusCWith x86Baseline "kern" x86Kern
      cAVX2 = showJanusCWith x86AVX2 "kern" x86Kern

  assertBool "SSE2 codegen avoids __m256" (not ("__m256" `isInfixOf` cSSE2))
  assertBool "SSE2 codegen uses __m128 pair" ("__m128" `isInfixOf` cSSE2)
  assertBool "AVX2 codegen uses __m256" ("__m256" `isInfixOf` cAVX2)
  assertBool "AVX2 codegen uses _mm256_fmadd_ps" ("_mm256_fmadd_ps" `isInfixOf` cAVX2)
  assertBool "AVX2 codegen uses _mm256_i32gather_ps" ("_mm256_i32gather_ps" `isInfixOf` cAVX2)

  -- CUDA: packed 4xInt8 SIMD intrinsics & SM-gated __dp4a
  let cudaKern ::
        JanusCUDA (Ptr Int32) ->
        JanusCUDA (SIMDVector JanusCUDA 4 Int8) ->
        JanusCUDA (SIMDVector JanusCUDA 4 Int8) ->
        JanusCUDAM ()
      cudaKern out a b = do
        let s = vadd a b
            sat = vadds s b
            msk = vgt sat a
            blended = vselect msk sat a
            dot = vdp4aS blended b 10
        M.pokeElemOff out dot 0

      cu60 = showJanusCUDAWith cudaSM60 "cuda_kern" cudaKern
      cu80 = showJanusCUDAWith cudaSM80 "cuda_kern" cudaKern

  assertBool "CUDA codegen emits __vadd4" ("__vadd4" `isInfixOf` cu80)
  assertBool "CUDA codegen emits __vaddss4" ("__vaddss4" `isInfixOf` cu80)
  assertBool "CUDA codegen emits __vcmpgts4" ("__vcmpgts4" `isInfixOf` cu80)
  assertBool "CUDA sm_80 emits __dp4a" ("__dp4a" `isInfixOf` cu80)
  assertBool "CUDA sm_60 falls back without __dp4a" (not ("__dp4a" `isInfixOf` cu60))

--------------------------------------------------------------------------------
-- 2. Differential Property Tests: Identity Tiers vs CUDAIO CPU vs LinuxX86_64C
--------------------------------------------------------------------------------

int8x16Composite ::
  ( ExpSIMDArith e 16 Int8,
    ExpSIMDSat e 16 Int8,
    ExpSIMDCmp e 16 Int8,
    ExpSIMDBits e 16 Int8,
    Num (e (SIMDVector e 16 Int8))
  ) =>
  e (SIMDVector e 16 Int8) ->
  e (SIMDVector e 16 Int8) ->
  e (SIMDVector e 16 Int8)
int8x16Composite a b =
  let s = vadds a b
      d = vsubs a b
      av = vavg a b
      ad = vabsdiff a b
      ab = vabss a
      mn = vmin s d
      mx = vmax av ad
      m = vlt a b `mor` veq a 0
   in vselect m (mn + ab) (mx - vpopcnt b)

floatx8Composite ::
  ( ExpSIMDFloat e 8 Float,
    ExpSIMDCmp e 8 Float,
    ExpLet e,
    JanusTyped e (SIMDVector e 8 Float),
    Num (e (SIMDVector e 8 Float)),
    Fractional (e (SIMDVector e 8 Float)),
    Floating (e (SIMDVector e 8 Float)),
    FMA (e (SIMDVector e 8 Float))
  ) =>
  e (SIMDVector e 8 Float) ->
  e (SIMDVector e 8 Float) ->
  e (SIMDVector e 8 Float) ->
  e (SIMDVector e 8 Float)
floatx8Composite a b c =
  let_ (fma a b c) $ \f ->
    let rounded = vroundNearest f + vfloor a - vceil b + vtrunc c
        pos = vmax (abs a + 1.0) 1.0
        sq = sqrt pos
     in vselect (f `vge` 0.0) (rounded + sq) (vfms a b c / pos)

propInt8x16AllTiers ::
  IO (SIMDVector LinuxX86_64C 16 Int8 -> SIMDVector LinuxX86_64C 16 Int8 -> SIMDVector LinuxX86_64C 16 Int8) ->
  IO (SIMDVector LinuxX86_64C 16 Int8 -> SIMDVector LinuxX86_64C 16 Int8 -> SIMDVector LinuxX86_64C 16 Int8) ->
  Property
propInt8x16AllTiers getSSE2Fn getHostFn = property $ do
  fnSSE2 <- liftIO getSSE2Fn
  fnHost <- liftIO getHostFn
  xs <- forAll $ Gen.list (Range.singleton 16) (Gen.int8 Range.constantBounded)
  ys <- forAll $ Gen.list (Range.singleton 16) (Gen.int8 Range.constantBounded)

  let cpuOut =
        simdToList . getCUDAIO $
          int8x16Composite @CUDAIO (CUDAIO (simdFromList xs)) (CUDAIO (simdFromList ys))

  hostFs <- liftIO observeHostX86Features
  forM_ [x86Baseline, x86SSSE3, x86SSE4_1, x86AVX, x86AVX2] $ \tier ->
    when (admitsX86Features hostFs tier) $ do
      ffiOut <- liftIO $ withHostX86Features tier $ do
        let Identity r = int8x16Composite @Identity (Identity (simdFromList xs)) (Identity (simdFromList ys))
        pure (simdToList r)
      ffiOut === cpuOut

  let cSSE2Out = simdToList $ fnSSE2 (simdFromList xs) (simdFromList ys)
      cHostOut = simdToList $ fnHost (simdFromList xs) (simdFromList ys)
  cSSE2Out === cpuOut
  cHostOut === cpuOut

propFloatx8AllTiers ::
  IO (SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float) ->
  IO (SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float -> SIMDVector LinuxX86_64C 8 Float) ->
  Property
propFloatx8AllTiers getSSE2Fn getHostFn = property $ do
  fnSSE2 <- liftIO getSSE2Fn
  fnHost <- liftIO getHostFn
  let genF = Gen.float (Range.linearFracFrom 0 (-50.0) 50.0)
  as <- forAll $ Gen.list (Range.singleton 8) genF
  bs <- forAll $ Gen.list (Range.singleton 8) genF
  cs <- forAll $ Gen.list (Range.singleton 8) genF

  let cpuOut =
        simdToList . getCUDAIO $
          floatx8Composite @CUDAIO
            (CUDAIO (simdFromList as))
            (CUDAIO (simdFromList bs))
            (CUDAIO (simdFromList cs))

  hostFs <- liftIO observeHostX86Features
  forM_ [x86Baseline, x86SSE4_1, x86AVX, x86AVX2] $ \tier ->
    when (admitsX86Features hostFs tier) $ do
      ffiOut <- liftIO $ withHostX86Features tier $ do
        let Identity r =
              floatx8Composite @Identity
                (Identity (simdFromList as))
                (Identity (simdFromList bs))
                (Identity (simdFromList cs))
        pure (simdToList r)
      ffiOut === cpuOut

  let cSSE2Out = simdToList $ fnSSE2 (simdFromList as) (simdFromList bs) (simdFromList cs)
      cHostOut = simdToList $ fnHost (simdFromList as) (simdFromList bs) (simdFromList cs)
  cSSE2Out === cpuOut
  cHostOut === cpuOut

test_simd_differential :: TestTree
test_simd_differential =
  let acqSSE2Int = do
        cfg <- getTestBuildConfig
        acquireJanusCWith x86Baseline cfg (int8x16Composite @LinuxX86_64C)
      acqHostInt = do
        cfg <- getTestBuildConfig
        acquireJanusC cfg (int8x16Composite @LinuxX86_64C)
      acqSSE2Flt = do
        cfg <- getTestBuildConfig
        acquireJanusCWith x86Baseline cfg (floatx8Composite @LinuxX86_64C)
      acqHostFlt = do
        cfg <- getTestBuildConfig
        acquireJanusC cfg (floatx8Composite @LinuxX86_64C)
   in withResource acqSSE2Int releaseJanusC $ \rSSE2I ->
        withResource acqHostInt releaseJanusC $ \rHostI ->
          withResource acqSSE2Flt releaseJanusC $ \rSSE2F ->
            withResource acqHostFlt releaseJanusC $ \rHostF ->
              testGroup
                "SIMD cross-tier differential properties"
                [ testProperty "16xInt8 arith/sat/cmp/bits across FFI tiers, CPU emu, SSE2 C, & Host C" $
                    propInt8x16AllTiers (fst <$> rSSE2I) (fst <$> rHostI),
                  testProperty "8xFloat 256-bit fma/round/sqrt/select across FFI tiers, CPU emu, SSE2 lowered C, & Host C" $
                    propFloatx8AllTiers (fst <$> rSSE2F) (fst <$> rHostF)
                ]

--------------------------------------------------------------------------------
-- 3. Shifts, Shuffles, Reductions, Casts, Mask Queries, & Dot Products
--------------------------------------------------------------------------------

unit_simd_categories_exhaustive :: IO ()
unit_simd_categories_exhaustive = do
  cfg <- getTestBuildConfig

  -- A. Shifts (uniform, immediate, and AVX2 variable shifts) on 8 x Int32
  let shiftExpr ::
        (ExpSIMDBits e 8 Int32, Num (e (SIMDVector e 8 Int32)), Num (e Int32)) =>
        e (SIMDVector e 8 Int32) ->
        e (SIMDVector e 8 Int32) ->
        e (SIMDVector e 8 Int32)
      shiftExpr v counts =
        (v `vshl` 2) + vshrImm (Proxy @1) v + (v `vshlVar` counts) + (v `vshrVar` counts)

      vIn = [-100, -1, 0, 1, 7, 1024, -65536, 42 :: Int32]
      cIn = [0, 1, 2, 3, 4, 5, 31, 35 :: Int32] -- includes >= 32 edge case!
      Identity rShiftFFI = shiftExpr @Identity (Identity (simdFromList vIn)) (Identity (simdFromList cIn))
      CUDAIO rShiftCPU = shiftExpr @CUDAIO (CUDAIO (simdFromList vIn)) (CUDAIO (simdFromList cIn))

  simdToList rShiftFFI @?= simdToList rShiftCPU
  rShiftC <- withJanusC cfg (shiftExpr @LinuxX86_64C) $ \f ->
    pure $ simdToList $ f (simdFromList vIn) (simdFromList cIn)
  rShiftC @?= simdToList rShiftCPU

  -- B. Permutations, Byte Shuffles, Split/Concat, & Horizontal Reductions
  let shufRedExpr ::
        forall e.
        ( ExpSIMDShuffle e 4 Int32,
          ExpSIMDSplit e 4 2 Int32,
          ExpSIMDReduce e 4 Int32,
          ExpSIMDCast e,
          ExpSIMDArith e 4 Int32,
          Num (e Int32)
        ) =>
        e (SIMDVector e 4 Int32) ->
        e (SIMDVector e 4 Int32) ->
        e Int32
      shufRedExpr v idx =
        let s1 = vshuffleLanes [3, 2, 1, 0] v
            s2 = vpermuteVar s1 idx
            (lo, hi) = vsplit @e @4 @2 s2
            rejoined = vconcat @e @4 @2 hi lo
            asF = vcvt @_ @4 @Int32 @Float rejoined
            backI = vcvt @_ @4 @Float @Int32 asF
         in vreduceAdd backI + vreduceMax v - vreduceMin v

      xs4 = [10, -20, 30, 40 :: Int32]
      ix4 = [1, 3, 0, 2 :: Int32]
      Identity redFFI = shufRedExpr @Identity (Identity (simdFromList xs4)) (Identity (simdFromList ix4))
      CUDAIO redCPU = shufRedExpr @CUDAIO (CUDAIO (simdFromList xs4)) (CUDAIO (simdFromList ix4))

  redFFI @?= redCPU
  redC <- withJanusC cfg (shufRedExpr @LinuxX86_64C) $ \f ->
    pure $ f (simdFromList xs4) (simdFromList ix4)
  redC @?= redCPU

  -- C. Mask Queries (mmovemask, mpopcount, mfirstTrue, mallOf, manyOf, mnoneOf)
  let maskQueryExpr ::
        (ExpSIMDCmp e 8 Int16, Num (e Int32), ExpIntegralCast e Word32 Int32) =>
        e (SIMDVector e 8 Int16) ->
        e (SIMDVector e 8 Int16) ->
        e Int32
      maskQueryExpr a b =
        let m = a `vgt` b
         in toIntegral (mmovemask m) * 100 + mpopcount m * 10 + mfirstTrue m

      ma8 = [1, 5, 2, 9, -3, 0, 8, 4 :: Int16]
      mb8 = [2, 3, 2, 1, -1, 0, 7, 9 :: Int16]
      Identity mqFFI = maskQueryExpr @Identity (Identity (simdFromList ma8)) (Identity (simdFromList mb8))
      CUDAIO mqCPU = maskQueryExpr @CUDAIO (CUDAIO (simdFromList ma8)) (CUDAIO (simdFromList mb8))

  mqFFI @?= mqCPU
  mqC <- withJanusC cfg (maskQueryExpr @LinuxX86_64C) $ \f ->
    pure $ f (simdFromList ma8) (simdFromList mb8)
  mqC @?= mqCPU

  -- D. Dot Products (dp4aS, dp4aU, dp4aVecS, dp2aLoS, dp2aHiS, vsad8) & NarrowSat
  let dotExpr ::
        forall e.
        (ExpSIMDDot e, ExpSIMDCast e, ExpSIMDReduce e 4 Int32, Num (e Int32), ExpIntegralCast e Word32 Int32) =>
        e (SIMDVector e 4 Int8) ->
        e (SIMDVector e 4 Int8) ->
        e (SIMDVector e 16 Int8) ->
        e (SIMDVector e 16 Word8) ->
        e (SIMDVector e 2 Int16) ->
        e Int32
      dotExpr a4 b4 vec16s vec16u s2 =
        let d1 = vdp4aS a4 b4 100
            d2 = vdp2aLoS s2 b4 d1
            d3 = vdp2aHiS s2 b4 d2
            v4 = vdp4aVecS @e @16 @4 vec16s vec16s (vsplat 0)
            sad = vsad8 vec16u (vbitcast vec16s)
         in d3 + vreduceAdd v4 + toIntegral sad

      da4 = [-10, 20, -30, 40 :: Int8]
      db4 = [3, -4, 5, -6 :: Int8]
      dv16s = [fromIntegral (i * 3 - 20) | i <- [0 .. 15 :: Int]] :: [Int8]
      dv16u = [fromIntegral (i * 11 + 5) | i <- [0 .. 15 :: Int]] :: [Word8]
      ds2 = [300, -250 :: Int16]

      Identity dotFFI =
        dotExpr @Identity
          (Identity (simdFromList da4))
          (Identity (simdFromList db4))
          (Identity (simdFromList dv16s))
          (Identity (simdFromList dv16u))
          (Identity (simdFromList ds2))
      CUDAIO dotCPU =
        dotExpr @CUDAIO
          (CUDAIO (simdFromList da4))
          (CUDAIO (simdFromList db4))
          (CUDAIO (simdFromList dv16s))
          (CUDAIO (simdFromList dv16u))
          (CUDAIO (simdFromList ds2))

  dotFFI @?= dotCPU
  dotC <- withJanusC cfg (dotExpr @LinuxX86_64C) $ \f ->
    pure $ f (simdFromList da4) (simdFromList db4) (simdFromList dv16s) (simdFromList dv16u) (simdFromList ds2)
  dotC @?= dotCPU

--------------------------------------------------------------------------------
-- 4. Architecture-Agnostic End-to-End Polymorphic Kernels Across All Backends
--------------------------------------------------------------------------------

-- | Architecture-agnostic vectorized ReLU(a * x + y) kernel with masked tail
-- handling and mutable Ref SIMD accumulation. Runs identically across
-- Identity/IO, LinuxX86_64C (SSE2 & AVX2), CUDAIO, CUDADejaFu, and GPU JanusCUDA!
simdChunkTransform ::
  forall m e.
  ( Monad m,
    CmdRef m e,
    CmdSIMDMem m e 4 Float,
    ExpSIMDFloat e 4 Float,
    ExpSIMDCmp e 4 Float,
    ExpSIMDReduce e 4 Float,
    ExpSIMDArith e 4 Int32,
    ExpSIMDCmp e 4 Int32,
    ExpSIMDCast e,
    M.ExpPtr e Float,
    JanusTyped e (SIMDVector e 4 Float),
    Num (e (SIMDVector e 4 Float)),
    Num (e Int32),
    Num (e Int64),
    ExpIntegralCast e Int32 Int64
  ) =>
  e Int32 ->
  e Int32 ->
  e Float ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  m ()
simdChunkTransform n baseIdx alpha px py pout = do
  let lanes = vadd (vsplat baseIdx) (viota @e @4 @Int32)
      inBoundsInt = lanes `vlt` vsplat n
      -- Bitcast 4x32-bit integer mask to 4x32-bit float mask (canonical all-1s/0s)
      inBoundsVec :: e (SIMDVector e 4 Float)
      inBoundsVec = vbitcast (vselect inBoundsInt (vsplat (-1 :: e Int32)) (vsplat 0))
      inBoundsMask = inBoundsVec `vne` 0
      off = toIntegral baseIdx
  vx <- vloadMasked inBoundsMask 0 (M.ptrIndex px off)
  vy <- vloadMasked inBoundsMask 0 (M.ptrIndex py off)
  accRef <- newRef (0 :: e (SIMDVector e 4 Float))
  modifyRef accRef (\acc -> acc + vfma (vsplat alpha) vx vy)
  raw <- readRef accRef
  let clipped = vselect (raw `vlt` 0) 0 raw
  vstoreMasked inBoundsMask (M.ptrIndex pout off) clipped

cpuSimdLoop ::
  ( Monad m,
    CmdRange m e,
    CmdRef m e,
    CmdSIMDMem m e 4 Float,
    ExpSIMDFloat e 4 Float,
    ExpSIMDCmp e 4 Float,
    ExpSIMDReduce e 4 Float,
    ExpSIMDArith e 4 Int32,
    ExpSIMDCmp e 4 Int32,
    ExpSIMDCast e,
    M.ExpPtr e Float,
    ExpOrd e Int64,
    JanusTyped e Int64,
    JanusTyped e (SIMDVector e 4 Float),
    Num (e (SIMDVector e 4 Float)),
    Num (e Int32),
    Num (e Int64),
    ExpIntegralCast e Int32 Int64,
    ExpIntegralCast e Int64 Int32
  ) =>
  e Int32 ->
  e Int64 ->
  e Float ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  m ()
cpuSimdLoop n numChunks alpha px py pout =
  rangeM 0 numChunks $ \chunk ->
    simdChunkTransform n (toIntegral chunk * 4) alpha px py pout

cudaSimdKernel ::
  ( Monad m,
    MonadReader (CUDAThreadInfo e) m,
    CmdCond m e,
    CmdRef m e,
    CmdSIMDMem m e 4 Float,
    ExpSIMDFloat e 4 Float,
    ExpSIMDCmp e 4 Float,
    ExpSIMDReduce e 4 Float,
    ExpSIMDArith e 4 Int32,
    ExpSIMDCmp e 4 Int32,
    ExpSIMDCast e,
    M.ExpPtr e Float,
    ExpOrd e Int32,
    JanusTyped e Int32,
    JanusTyped e (SIMDVector e 4 Float),
    Num (e (SIMDVector e 4 Float)),
    Num (e Int32),
    Num (e Int64),
    ExpIntegralCast e Int32 Int64
  ) =>
  e Int32 ->
  e Float ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  m ()
cudaSimdKernel n alpha px py pout = do
  bdim <- asks _cudaThreadInfoBlockDimX
  bidx <- asks _cudaThreadInfoBlockIdxX
  tidx <- asks _cudaThreadInfoThreadIdxX
  tid <- letM (bdim * bidx + tidx)
  base <- letM (tid * 4)
  whenM_ (base `lt` n) $
    simdChunkTransform n base alpha px py pout

-- | Architecture-agnostic packed 4xInt8 / 4xWord8 CUDA & x86 SIMD kernel
-- exercising __vadd4, __vaddss4, __vabs4, __vcmpgts4, __byte_perm, and __dp4a!
packedByteSimdKernel ::
  forall m e.
  ( Monad m,
    MonadReader (CUDAThreadInfo e) m,
    CmdCond m e,
    CmdRef m e,
    CmdSIMDMem m e 4 Int8,
    ExpSIMDSat e 4 Int8,
    ExpSIMDCmp e 4 Int8,
    ExpSIMDShuffle e 4 Int8,
    ExpSIMDVec e 4 Word8,
    ExpInject e Word8,
    ExpSIMDDot e,
    ExpSIMDCast e,
    M.ExpPtr e Int8,
    M.CmdStorable m e Int32,
    ExpOrd e Int32,
    JanusTyped e Int32,
    Num (e Int32),
    Num (e Int64),
    ExpIntegralCast e Int32 Int64
  ) =>
  e Int32 ->
  e (Ptr Int8) ->
  e (Ptr Int8) ->
  e (Ptr Int32) ->
  m ()
packedByteSimdKernel numGroups pa pb pout = do
  bdim <- asks _cudaThreadInfoBlockDimX
  bidx <- asks _cudaThreadInfoBlockIdxX
  tidx <- asks _cudaThreadInfoThreadIdxX
  gid <- letM (bdim * bidx + tidx)
  whenM_ (gid `lt` numGroups) $ do
    let byteOff = toIntegral (gid * 4)
    va <- vload @m @e @4 @Int8 (M.ptrIndex pa byteOff)
    vb <- vload @m @e @4 @Int8 (M.ptrIndex pb byteOff)
    let satSum = vadds va vb
        wrapped = vadd (vabs va) vb
        chosen = vselect (satSum `vgt` vb) satSum wrapped
        revCtrl :: e (SIMDVector e 4 Word8)
        revCtrl = vfromLanes [inject 3, inject 2, inject 1, inject 0]
        shuf = vshuffleBytes chosen revCtrl
        dotVal = vdp4aS shuf vb 7
    M.pokeElemOff pout dotVal (toIntegral gid)

unit_polymorphic_relu_fma_all_backends :: IO ()
unit_polymorphic_relu_fma_all_backends = do
  cfg <- getTestBuildConfig
  -- Use n = 23 (non-multiple of 4!) to thoroughly test tail-masked loads/stores + race-freedom
  let n = 23 :: Int
      chunks = (n + 3) `div` 4
      alpha = 2.5 :: Float
      xs = [fromIntegral i - 10.0 | i <- [0 .. n - 1]] :: [Float]
      ys = [fromIntegral (n - i) * 0.5 | i <- [0 .. n - 1]] :: [Float]
      expected = [max 0.0 (fma alpha x y) | (x, y) <- zip xs ys]
      sentinel = -999.0 :: Float
      allocLen = chunks * 4

  withArray xs $ \px ->
    withArray ys $ \py ->
      allocaBytes (allocLen * sizeOf (0 :: Float)) $ \pout -> do
        let resetOut = pokeArray pout (replicate allocLen sentinel)
            verifyOut _label = do
              actual <- peekArray n pout
              actual @?= expected
              -- Verify sentinel past n was NOT overwritten by masked store!
              tailVals <- peekArray (allocLen - n) (pout `plusPtr'` n)
              tailVals @?= replicate (allocLen - n) sentinel

        -- 1. IO / Identity (FFI intrinsics)
        resetOut
        cpuSimdLoop @IO @Identity (fromIntegral n) (fromIntegral chunks) (Identity alpha) (Identity px) (Identity py) (Identity pout)
        verifyOut "Identity/IO"

        -- 2. LinuxX86_64C with explicit SSE2 baseline
        resetOut
        withJanusCWith x86Baseline cfg (cpuSimdLoop @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
          fn (fromIntegral n) (fromIntegral chunks) alpha px py pout
        verifyOut "LinuxX86_64C (SSE2)"

        -- 3. LinuxX86_64C with explicit x86AVX (AVX1 without AVX2/FMA) and host-queried target features
        hostX86 <- observeHostX86Features
        when (admitsX86Features hostX86 x86AVX) $ do
          resetOut
          withJanusCWith x86AVX cfg (cpuSimdLoop @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
            fn (fromIntegral n) (fromIntegral chunks) alpha px py pout
          verifyOut "LinuxX86_64C (AVX1)"

        resetOut
        withJanusC cfg (cpuSimdLoop @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
          fn (fromIntegral n) (fromIntegral chunks) alpha px py pout
        verifyOut "LinuxX86_64C (Host)"

        -- 4. CUDAIO CPU Emulation (explicit cudaSM60 & default)
        let launchCfg =
              CUlaunchConfig
                { _cuLaunchConfigGridDimX = 2,
                  _cuLaunchConfigGridDimY = 1,
                  _cuLaunchConfigGridDimZ = 1,
                  _cuLaunchConfigBlockDimX = 4,
                  _cuLaunchConfigBlockDimY = 1,
                  _cuLaunchConfigBlockDimZ = 1,
                  _cuLaunchConfigSharedMemBytes = 0,
                  _cuLaunchConfigStream = CUstream nullPtr
                }
        resetOut
        launchCUDAIOMWithFeatures cudaSM60 launchCfg $
          cudaSimdKernel @CUDAIOM @CUDAIO (fromIntegral n) (inject alpha) (inject px) (inject py) (inject pout)
        verifyOut "CUDAIO"

        -- 5. CUDADejaFu deterministic schedule exploration (roundRobin & preemptive)
        resetOut
        (resDF, _trace) <-
          launchCUDADejaFuM launchCfg $
            cudaSimdKernel @CUDADejaFuM @CUDADejaFu (fromIntegral n) (inject alpha) (inject px) (inject py) (inject pout)
        resDF @?= Right ()
        verifyOut "CUDADejaFu"

        resetOut
        (resNP, (), _traceNP) <-
          launchCUDADejaFuMWithFeatures cudaSM80 roundRobinSchedNP defaultMemType () launchCfg $
            cudaSimdKernel @CUDADejaFuM @CUDADejaFu (fromIntegral n) (inject alpha) (inject px) (inject py) (inject pout)
        resNP @?= Right ()
        verifyOut "CUDADejaFu (NP)"

        -- 6. GPU JanusCUDA execution (when CUDA driver & device are available)
        runInBoundThread $
          try (cuInit 0) >>= \case
            Left (CUException CUDA_ERROR_STUB_LIBRARY) -> pure ()
            Left (CUException CUDA_ERROR_NO_DEVICE) -> pure ()
            Left e -> assertFailure ("Unexpected CUDA init error: " <> show e)
            Right () -> alloca $ \pctx ->
              withCuDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \ctx -> do
                let nbytes = allocLen * sizeOf (0 :: Float)
                resetOut
                withCuDeviceArray nbytes $ \dx ->
                  withCuDeviceArray nbytes $ \dy ->
                    withCuDeviceArray nbytes $ \dout -> do
                      cuMemcpyHtoD dx (castPtr px) (fromIntegral (n * sizeOf (0 :: Float)))
                      cuMemcpyHtoD dy (castPtr py) (fromIntegral (n * sizeOf (0 :: Float)))
                      cuMemcpyHtoD dout (castPtr pout) (fromIntegral nbytes)
                      withJanusCUDA dev ctx cfg (\m a x y o -> cudaSimdKernel @JanusCUDAM @JanusCUDA m a x y o) $ \k -> do
                        k launchCfg (fromIntegral n) alpha dx dy dout
                        cuCtxSynchronize
                        cuMemcpyDtoH (castPtr pout) dout (fromIntegral nbytes)
                        verifyOut "JanusCUDA GPU"
  where
    plusPtr' p k = castPtr p `plusPtr` (k * sizeOf (0 :: Float))

-- Helper namespace shim for pointer offset in test
unit_packed_byte_cuda_and_cpu_simd :: IO ()
unit_packed_byte_cuda_and_cpu_simd = do
  cfg <- getTestBuildConfig
  let groups = 12 :: Int
      totalBytes = groups * 4
      as = [fromIntegral ((i * 17 - 90) `mod` 127) | i <- [0 .. totalBytes - 1]] :: [Int8]
      bs = [fromIntegral ((i * 13 - 50) `mod` 120) | i <- [0 .. totalBytes - 1]] :: [Int8]
      launchCfg =
        CUlaunchConfig
          { _cuLaunchConfigGridDimX = 2,
            _cuLaunchConfigGridDimY = 1,
            _cuLaunchConfigGridDimZ = 1,
            _cuLaunchConfigBlockDimX = 8,
            _cuLaunchConfigBlockDimY = 1,
            _cuLaunchConfigBlockDimZ = 1,
            _cuLaunchConfigSharedMemBytes = 0,
            _cuLaunchConfigStream = CUstream nullPtr
          }

  withArray as $ \pa ->
    withArray bs $ \pb ->
      allocaBytes (groups * 4) $ \(poutIO :: Ptr Int32) ->
        allocaBytes (groups * 4) $ \(poutDF :: Ptr Int32) -> do
          launchCUDAIOM launchCfg $
            packedByteSimdKernel @CUDAIOM @CUDAIO (fromIntegral groups) (inject pa) (inject pb) (inject poutIO)
          (resDF, _) <-
            launchCUDADejaFuM launchCfg $
              packedByteSimdKernel @CUDADejaFuM @CUDADejaFu (fromIntegral groups) (inject pa) (inject pb) (inject poutDF)
          resDF @?= Right ()
          outIO <- peekArray groups poutIO
          outDF <- peekArray groups poutDF
          outIO @?= outDF

          -- Also run on GPU when CUDA device is present (both sm_60 fallback & native device SM!)
          runInBoundThread $
            try (cuInit 0) >>= \case
              Left (CUException CUDA_ERROR_STUB_LIBRARY) -> pure ()
              Left (CUException CUDA_ERROR_NO_DEVICE) -> pure ()
              Left e -> assertFailure ("Unexpected CUDA error: " <> show e)
              Right () -> alloca $ \pctx ->
                withCuDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \ctx ->
                  allocaBytes (groups * 4) $ \(poutGPU :: Ptr Int32) ->
                    withCuDeviceArray totalBytes $ \da ->
                      withCuDeviceArray totalBytes $ \db ->
                        withCuDeviceArray (groups * 4) $ \dout -> do
                          cuMemcpyHtoD da (castPtr pa) (fromIntegral totalBytes)
                          cuMemcpyHtoD db (castPtr pb) (fromIntegral totalBytes)
                          devFs <- observeCUDAFeatures dev
                          assertBool "Observed GPU SM >= 5.0" (admitsCUDAFeatures devFs cudaSM50)
                          badTarget <-
                            try (withJanusCUDAWith (cudaSM 99 0) dev ctx cfg (\g x y o -> packedByteSimdKernel @JanusCUDAM @JanusCUDA g x y o) (\_ -> pure ()))
                          case badTarget of
                            Left (UnsupportedCUDATarget {}) -> pure ()
                            Left e -> assertFailure ("Unexpected target exception: " <> show e)
                            Right () -> assertFailure "Expected UnsupportedCUDATarget for sm_990"
                          withJanusCUDA dev ctx cfg (\g x y o -> packedByteSimdKernel @JanusCUDAM @JanusCUDA g x y o) $ \k -> do
                            k launchCfg (fromIntegral groups) da db dout
                            cuCtxSynchronize
                            cuMemcpyDtoH (castPtr poutGPU) dout (fromIntegral (groups * 4))
                            outGPU <- peekArray groups poutGPU
                            outGPU @?= outIO

--------------------------------------------------------------------------------
-- 5. SLEEF Vector Math on CPU Linux (LinuxX86_64C & Identity)
--------------------------------------------------------------------------------

sleefTranscendentalExpr ::
  (Floating v, Hypot v) =>
  v ->
  v ->
  v
sleefTranscendentalExpr x y =
  let t1 = sin x + cos y + tan (x * 0.5)
      t2 = asin (x * 0.5) + acos (x * 0.5) + atan y
      t3 = sinh (x * 0.5) + cosh (y * 0.5) + tanh x
      t4 = asinh x + acosh (abs y + 1.25) + atanh (x * 0.5)
      t5 = exp (x * 0.5) + log (abs y + 0.5) + (abs x + 0.5) ** (y * 0.5)
      t6 = expm1 (x * 0.25) + log1p (abs y) + hypot x y + logBase 2 (abs x + 1.0)
   in t1 + t2 + t3 + t4 + t5 + t6

unit_sleef_linux_floating :: IO ()
unit_sleef_linux_floating = do
  cfg <- getTestBuildConfig

  -- 1. Verify cross-target SLEEF symbol & header selection across SSE2, SSE4.2, AVX, and AVX2+FMA
  let f8Expr ::
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float)
      f8Expr = sleefTranscendentalExpr

      d4Expr ::
        LinuxX86_64C (SIMDVector LinuxX86_64C 4 Double) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 4 Double) ->
        LinuxX86_64C (SIMDVector LinuxX86_64C 4 Double)
      d4Expr = sleefTranscendentalExpr

      cSSE2 = showJanusCWith x86Baseline "sleef_f8" f8Expr
      cSSE4 = showJanusCWith x86SSE4_2 "sleef_f8" f8Expr
      cAVX1 = showJanusCWith x86AVX "sleef_f8" f8Expr
      cAVX2 = showJanusCWith x86AVX2 "sleef_f8" f8Expr
      cD4AVX2 = showJanusCWith x86AVX2 "sleef_d4" d4Expr

  assertBool "SLEEF header included" ("#include <sleef.h>" `isInfixOf` cSSE2)
  assertBool "SSE2 uses Sleef_expf4_u10sse2" ("Sleef_expf4_u10sse2" `isInfixOf` cSSE2)
  assertBool "SSE2 uses Sleef_powf4_u10sse2" ("Sleef_powf4_u10sse2" `isInfixOf` cSSE2)
  assertBool "SSE2 uses Sleef_hypotf4_u05sse2" ("Sleef_hypotf4_u05sse2" `isInfixOf` cSSE2)
  assertBool "SSE4.2 uses Sleef_expf4_u10sse4" ("Sleef_expf4_u10sse4" `isInfixOf` cSSE4)
  assertBool "AVX1 uses Sleef_expf8_u10avx" ("Sleef_expf8_u10avx" `isInfixOf` cAVX1)
  assertBool "AVX2+FMA uses Sleef_expf8_u10avx2" ("Sleef_expf8_u10avx2" `isInfixOf` cAVX2)
  assertBool "AVX2+FMA uses Sleef_hypotf8_u05avx2" ("Sleef_hypotf8_u05avx2" `isInfixOf` cAVX2)
  assertBool "AVX2+FMA Double uses Sleef_expd4_u10avx2" ("Sleef_expd4_u10avx2" `isInfixOf` cD4AVX2)

  -- 2. Live execution & accuracy verification for 8xFloat across tiers
  let xsF = [-0.9, -0.5, -0.15, 0.0, 0.2, 0.45, 0.75, 0.95 :: Float]
      ysF = [0.3, -0.8, 1.1, 0.5, -0.4, 1.5, -1.2, 0.7 :: Float]
      refF = zipWith sleefTranscendentalExpr xsF ysF

  ffiSSE2 <-
    withHostX86Features x86Baseline $ do
      let Identity v =
            sleefTranscendentalExpr
              (Identity (simdFromList @(SIMDVector Identity 8 Float) xsF))
              (Identity (simdFromList ysF))
          ls = simdToList v
      mapM_ evaluate ls
      pure ls
  let Identity ffiHostVec =
        sleefTranscendentalExpr
          (Identity (simdFromList @(SIMDVector Identity 8 Float) xsF))
          (Identity (simdFromList ysF))
      ffiHost = simdToList ffiHostVec

  cOutSSE2 <- withJanusCWith x86Baseline cfg f8Expr $ \fn ->
    pure $ fn (simdFromList xsF) (simdFromList ysF)
  cOutHost <- withJanusC cfg f8Expr $ \fn ->
    pure $ fn (simdFromList xsF) (simdFromList ysF)

  -- Exact tier-for-tier match between Identity FFI and LinuxX86_64C SLEEF calls
  simdToList cOutSSE2 @?= ffiSSE2
  simdToList cOutHost @?= ffiHost

  hostX86 <- observeHostX86Features
  when (admitsX86Features hostX86 x86AVX) $ do
    cOutAVX <- withJanusCWith x86AVX cfg f8Expr $ \fn ->
      pure $ fn (simdFromList xsF) (simdFromList ysF)
    ffiAVX <-
      withHostX86Features x86AVX $ do
        let Identity v =
              sleefTranscendentalExpr
                (Identity (simdFromList @(SIMDVector Identity 8 Float) xsF))
                (Identity (simdFromList ysF))
            ls = simdToList v
        mapM_ evaluate ls
        pure ls
    simdToList cOutAVX @?= ffiAVX

  forM_ (zip (simdToList cOutHost) refF) $ \(actual, expected) -> do
    let relErr = abs (actual - expected) / max 1.0 (abs expected)
    assertBool ("8xFloat SLEEF accuracy: got " <> show actual <> ", expected " <> show expected) (relErr < 1e-5)

  -- 3. Live execution & accuracy verification for 4xDouble across tiers
  let xsD = [-0.85, -0.25, 0.35, 0.9 :: Double]
      ysD = [0.4, -1.1, 1.3, -0.6 :: Double]
      refD = zipWith sleefTranscendentalExpr xsD ysD

  cD4SSE2 <- withJanusCWith x86Baseline cfg d4Expr $ \fn ->
    pure $ fn (simdFromList xsD) (simdFromList ysD)
  cD4Host <- withJanusC cfg d4Expr $ \fn ->
    pure $ fn (simdFromList xsD) (simdFromList ysD)
  let Identity ffiD4Host =
        sleefTranscendentalExpr
          (Identity (simdFromList @(SIMDVector Identity 4 Double) xsD))
          (Identity (simdFromList ysD))

  simdToList cD4Host @?= simdToList ffiD4Host
  forM_ (zip (simdToList cD4SSE2) refD) $ \(actual, expected) -> do
    let relErr = abs (actual - expected) / max 1.0 (abs expected)
    assertBool ("4xDouble SSE2 SLEEF accuracy: got " <> show actual <> ", expected " <> show expected) (relErr < 1e-13)
  forM_ (zip (simdToList cD4Host) refD) $ \(actual, expected) -> do
    let relErr = abs (actual - expected) / max 1.0 (abs expected)
    assertBool ("4xDouble Host SLEEF accuracy: got " <> show actual <> ", expected " <> show expected) (relErr < 1e-13)
