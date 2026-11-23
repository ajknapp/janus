{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.SPMD where

import Control.Category ((>>>))
import Control.Concurrent (runInBoundThread)
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable (Storable (peekElemOff, sizeOf))
import GHC.Generics (Generic, Rep)
import Hedgehog (Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (JanusC, JanusCM, acquireJanusC, acquireJanusCWith, releaseJanusC, showJanusCWith, withJanusC, withJanusCWith)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.CUDA
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.LinuxX86_64
import qualified Janus.Command.Array as A
import Janus.Command.Case (CmdCase, switchCase)
import Janus.Command.Cond (CmdCond, ifThenElseM_, whenM_)
import Janus.Command.Range (CmdRange, rangeM)
import Janus.Command.Ref (CmdRef, modifyRef, newRef, readRef, writeRef)
import Janus.Command.While (CmdWhile, whileM)
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Control.Fold (drain, maximumF, minimumF, sumF)
import Janus.Control.Pipe (delayP, dropP, filterP, pipeM, runPipe, takeP)
import Janus.Control.Stream (rangeS)
import Janus.Control.Unfold (range)
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool (false))
import Janus.Expression.Cast (ExpBoolCast (fromBool), ExpFloatingCast (toFloating), ExpIntegralCast (toIntegral), ExpPtrCast)
import Janus.Expression.Cond (ExpCond (ifThenElse))
import Janus.Expression.Eq (ExpEq (eq, ne))
import Janus.Expression.Inject (ExpInject (inject))
import Janus.Expression.Integral (ExpIntegral (quotient, remainder))
import Janus.Expression.Let (ExpLet (let_))
import Janus.Expression.Math (ExpComplex (imPart, rePart), FMA (fma), Hypot (hypot))
import Janus.Expression.MurmurHash (GExpHash, defaultHashWithSalt, hash64WithSalt)
import Janus.Expression.Ord (ExpOrd (gt, le, lt))
import Janus.Expression.SIMD
import Janus.Fix (ExpFix (fun))
import Janus.SPMD
import Janus.Typed (JanusTyped)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import Test.Tasty.Hedgehog (testProperty)

getSPMDTestConfig :: IO CBuildConfig
getSPMDTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

--------------------------------------------------------------------------------
-- 1. 64-Byte Aligned Unlifted Allocations & SIMD Alignment Enforcement
--------------------------------------------------------------------------------

cAlignmentKernel :: JanusC Int64 -> JanusCM (JanusC Int64)
cAlignmentKernel n = do
  p1 <- A.malloc n
  p2 <- A.calloc n 4
  p3 <- A.realloc p1 (n * 8 + 17)
  t <- A.newTensor (A.tensorBound (Proxy @'[32])) :: JanusCM (A.Vector 32 JanusC Float)
  A.writeTensor t (3 A.:. A.Z) 42.5
  v <- A.readTensor t (3 A.:. A.Z)
  A.free p2
  A.free p3
  A.freeTensor t
  pure (toIntegral v)

unit_aligned_allocations_and_tensors :: IO ()
unit_aligned_allocations_and_tensors = do
  cfg <- getSPMDTestConfig

  -- 1. IO / Identity CmdMem 64-byte alignment across odd sizes & realloc
  forM_ [0, 1, 13, 63, 64, 129, 1000 :: Int64] $ \sz -> do
    Identity pm <- A.malloc @IO @Identity (Identity sz)
    (fromIntegral (ptrToWordPtr pm) `mod` A.janusSIMDAlignment) @?= 0
    Identity pr <- A.realloc @IO @Identity (Identity pm) (Identity (sz + 257))
    (fromIntegral (ptrToWordPtr pr) `mod` A.janusSIMDAlignment) @?= 0
    A.free @IO @Identity (Identity pr)

    Identity pc <- A.calloc @IO @Identity (Identity (max 1 sz)) 4
    (fromIntegral (ptrToWordPtr pc) `mod` A.janusSIMDAlignment) @?= 0
    v0 <- peekElemOff (castPtr pc :: Ptr Int32) 0
    v0 @?= 0
    A.free @IO @Identity (Identity pc)

  -- 2. withTensor & withAlignedArray 64-byte alignment + misaligned vloadAligned detection
  let bds = A.tensorBound (Proxy @'[16])
  A.withTensor bds $ \(A.Tensor _ (Identity tp) :: A.Vector 16 Identity Float) -> do
    (fromIntegral (ptrToWordPtr tp) `mod` A.janusSIMDAlignment) @?= 0
    pokeArray tp [fromIntegral i | i <- [0 .. 15 :: Int]]
    Identity vecAligned <- vloadAligned @IO @Identity @8 @Float (Identity tp)
    simdToList vecAligned @?= [0 .. 7]

    -- Intentionally misaligned (+4 bytes) must be rejected by vloadAligned
    badLoad <- try (vloadAligned @IO @Identity @8 @Float (Identity (tp `plusPtr` 4)))
    case badLoad :: Either SomeException (Identity (SIMDVector Identity 8 Float)) of
      Left _ -> pure ()
      Right _ -> assertFailure "Expected vloadAligned to reject misaligned pointer"

  -- 3. Compiled JanusC CmdMem + newTensor/freeTensor
  resC <- withJanusC cfg cAlignmentKernel (\fn -> fn 13)
  resC @?= 42

--------------------------------------------------------------------------------
-- 2. Optimal Non-Splitting Gang Size Selection & Aligned SIMD Codegen
--------------------------------------------------------------------------------

-- Unmodified abstract Janus scalar step operating on a 1D Vector
abstractFmaReluStep ::
  ( Monad m,
    A.CmdStorable m e a,
    ExpOrd e a,
    ExpCond e,
    FMA (e a),
    JanusTyped e a,
    Num (e a),
    Num (e Int64)
  ) =>
  e a ->
  A.Vector d e a ->
  A.Vector d e a ->
  A.Vector d e a ->
  A.TensorIndex '[d] e ->
  m ()
abstractFmaReluStep alpha vx vy vout idx = do
  x <- A.readTensor vx idx
  y <- A.readTensor vy idx
  let z = fma alpha x y
  A.writeTensor vout idx (ifThenElse (z `lt` 0) 0 z)

-- Unmodified abstract Janus 3-point stencil step (tests aligned i vs unaligned i-1, i+1)
abstractStencilStep ::
  ( Monad m,
    A.CmdStorable m e Float,
    Fractional (e Float),
    Num (e Int64)
  ) =>
  A.Vector d e Float ->
  A.Vector d e Float ->
  e Int64 ->
  m ()
abstractStencilStep vin vout i = do
  xm <- A.readTensor vin ((i - 1) A.:. A.Z)
  x0 <- A.readTensor vin (i A.:. A.Z)
  xp <- A.readTensor vin ((i + 1) A.:. A.Z)
  A.writeTensor vout (i A.:. A.Z) (0.25 * xm + 0.5 * x0 + 0.25 * xp)

unit_optimal_gang_selection_and_aligned_codegen :: IO ()
unit_optimal_gang_selection_and_aligned_codegen = do
  -- 1. Verify optimal gang size table across SSE2, AVX1, AVX2, and CUDA
  optimalX86GangSize x86Baseline (Proxy @Float) @?= 4
  optimalX86GangSize x86SSE4_2 (Proxy @Float) @?= 4
  optimalX86GangSize x86AVX (Proxy @Float) @?= 8
  optimalX86GangSize x86AVX2 (Proxy @Float) @?= 8

  -- Crucial AVX1 vs AVX2 distinction: AVX1 has 256b float (__m256) but only 128b int (__m128i)!
  optimalX86GangSize x86AVX (Proxy @Int32) @?= 4
  optimalX86GangSize x86AVX2 (Proxy @Int32) @?= 8
  optimalX86GangSize x86AVX (Proxy @Double) @?= 4
  optimalX86GangSize x86AVX (Proxy @Int64) @?= 2
  optimalX86GangSize x86AVX2 (Proxy @Int64) @?= 4

  optimalX86GangSize x86Baseline (Proxy @Double) @?= 2
  optimalX86GangSize x86AVX2 (Proxy @Double) @?= 4
  optimalX86GangSize x86Baseline (Proxy @Int8) @?= 16
  optimalX86GangSize x86AVX2 (Proxy @Int8) @?= 32

  optimalCUDAGangSize cudaSM80 (Proxy @Float) @?= 4
  optimalCUDAGangSize cudaSM80 (Proxy @Double) @?= 2
  optimalCUDAGangSize cudaSM80 (Proxy @Int32) @?= 4
  optimalCUDAGangSize cudaSM80 (Proxy @Int16) @?= 8
  optimalCUDAGangSize cudaSM80 (Proxy @Int8) @?= 16

  -- 2. Verify generated C code uses aligned SIMD intrinsics and NEVER splits main types
  let bds64 = A.tensorBound (Proxy @'[64])
      floatX86Kern ::
        LinuxX86_64C Float ->
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64CM ()
      floatX86Kern alpha px py pout = do
        let vx = A.Tensor bds64 px
            vy = A.Tensor bds64 py
            vout = A.Tensor bds64 pout
        foreachOptimalTensorM (Proxy @Float) vx $ \idx ->
          abstractFmaReluStep (uniform alpha) (liftTensor vx) (liftTensor vy) (liftTensor vout) idx

      cFloatSSE2 = showJanusCWith x86Baseline "spmd_f32_sse2" floatX86Kern
      cFloatAVX2 = showJanusCWith x86AVX2 "spmd_f32_avx2" floatX86Kern

  assertBool "SSE2 Float uses aligned _mm_load_ps" ("_mm_load_ps" `isInfixOf` cFloatSSE2)
  assertBool "SSE2 Float uses aligned _mm_store_ps" ("_mm_store_ps" `isInfixOf` cFloatSSE2)
  assertBool "SSE2 Float avoids __m256 and struct splitting" (not ("__m256" `isInfixOf` cFloatSSE2) && not ("simdPairFst" `isInfixOf` cFloatSSE2))

  assertBool "AVX2 Float uses aligned _mm256_load_ps" ("_mm256_load_ps" `isInfixOf` cFloatAVX2)
  assertBool "AVX2 Float uses _mm256_fmadd_ps" ("_mm256_fmadd_ps" `isInfixOf` cFloatAVX2)
  assertBool "AVX2 Float uses aligned _mm256_store_ps" ("_mm256_store_ps" `isInfixOf` cFloatAVX2)
  assertBool "AVX2 Float avoids struct splitting" (not ("simdPairFst" `isInfixOf` cFloatAVX2))

  -- Int32 on AVX1 (must pick 4-wide _mm_load_si128) vs AVX2 (8-wide _mm256_load_si256)
  let int32X86Kern ::
        LinuxX86_64C (Ptr Int32) ->
        LinuxX86_64C (Ptr Int32) ->
        LinuxX86_64CM ()
      int32X86Kern px pout = do
        let vx = A.Tensor bds64 px
            vout = A.Tensor bds64 pout
        foreachOptimalTensorM (Proxy @Int32) vx $ \idx -> do
          x <- A.readTensor (liftTensor vx) idx
          A.writeTensor (liftTensor vout) idx (x * 3 + 1)

      cIntAVX1 = showJanusCWith x86AVX "spmd_i32_avx1" int32X86Kern
      cIntAVX2 = showJanusCWith x86AVX2 "spmd_i32_avx2" int32X86Kern

  assertBool "AVX1 Int32 picks 4-wide _mm_load_si128 without splitting" ("_mm_load_si128" `isInfixOf` cIntAVX1 && not ("simdPairFst" `isInfixOf` cIntAVX1))
  assertBool "AVX2 Int32 picks 8-wide _mm256_load_si256" ("_mm256_load_si256" `isInfixOf` cIntAVX2)

  -- Double on AVX2 (must pick 4-wide _mm256_load_pd without splitting)
  let doubleX86Kern ::
        LinuxX86_64C (Ptr Double) ->
        LinuxX86_64C (Ptr Double) ->
        LinuxX86_64CM ()
      doubleX86Kern px pout = do
        let vx = A.Tensor bds64 px
            vout = A.Tensor bds64 pout
        foreachOptimalTensorM (Proxy @Double) vx $ \idx -> do
          x <- A.readTensor (liftTensor vx) idx
          A.writeTensor (liftTensor vout) idx (x * x)

      cDblAVX2 = showJanusCWith x86AVX2 "spmd_f64_avx2" doubleX86Kern

  assertBool "AVX2 Double picks 4-wide _mm256_load_pd without splitting" ("_mm256_load_pd" `isInfixOf` cDblAVX2 && "_mm256_store_pd" `isInfixOf` cDblAVX2 && not ("simdPairFst" `isInfixOf` cDblAVX2))

  -- Stencil: aligned load for i, unaligned loads for i-1 and i+1, aligned store for i
  let stencilKern ::
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64C (Ptr Float) ->
        LinuxX86_64CM ()
      stencilKern pin pout = do
        let vin = A.Tensor bds64 pin
            vout = A.Tensor bds64 pout
        foreachAlignedM (Proxy @8) 8 56 $ \i ->
          abstractStencilStep (liftTensor vin) (liftTensor vout) i

      cStencil = showJanusCWith x86AVX2 "spmd_stencil" stencilKern

  assertBool "Stencil emits _mm256_load_ps for aligned center" ("_mm256_load_ps" `isInfixOf` cStencil)
  assertBool "Stencil emits _mm256_loadu_ps for shifted neighbors" ("_mm256_loadu_ps" `isInfixOf` cStencil)
  assertBool "Stencil emits _mm256_store_ps for aligned output" ("_mm256_store_ps" `isInfixOf` cStencil)

  -- CUDA: withOptimalGang (Proxy @Float) emits 4-wide float4 without splitting
  let cudaSpmdKern ::
        JanusCUDA (Ptr Float) ->
        JanusCUDA (Ptr Float) ->
        JanusCUDAM ()
      cudaSpmdKern px pout = do
        let vx = A.Tensor bds64 px
            vout = A.Tensor bds64 pout
        foreachOptimalTensorM (Proxy @Float) vx $ \idx -> do
          x <- A.readTensor (liftTensor vx) idx
          A.writeTensor (liftTensor vout) idx (x + 1.0)

      cuCode = showJanusCUDA "cuda_spmd_f32" cudaSpmdKern

  assertBool "CUDA Float SPMD uses float4 without splitting" ("float4" `isInfixOf` cuCode && not ("simdPairFst" `isInfixOf` cuCode))
  assertBool "CUDA vloadAligned qualifies pointee (const float4 *), not pointer" ("const float4 *" `isInfixOf` cuCode && not ("float4 *const" `isInfixOf` cuCode))

  let minBoundC = showJanusCWith x86AVX2 "spmd_minbound_i64" (minBound :: SPMD 4 LinuxX86_64C Int64)
  assertBool "Int64 minBound avoids unsigned-overflow decimal literal" (not ("-9223372036854775808" `isInfixOf` minBoundC))

--------------------------------------------------------------------------------
-- 3. Mixed Unlifted + SPMD Subprogram Execution Across All Backends
--------------------------------------------------------------------------------

-- Polymorphic unlifted program with SPMD subcomponents:
-- 1) Transforms non-multiple length (d = 23) array via foreachOptimalTensorM
-- 2) Computes dot product via foldForeachOptimalTensorM + gangReduceAdd back into unlifted scalar
mixedUnliftedProgram ::
  forall m e.
  ( CmdOptimalGang m e,
    FMA (e Float),
    Num (e Float)
  ) =>
  e Float ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  e (Ptr Float) ->
  m (e Float)
mixedUnliftedProgram alpha px py pout = do
  let bds23 = A.tensorBound (Proxy @'[23])
      vx = A.Tensor bds23 px
      vy = A.Tensor bds23 py
      vout = A.Tensor bds23 pout
  foreachOptimalTensorM (Proxy @Float) vx $ \idx ->
    abstractFmaReluStep (uniform alpha) (liftTensor vx) (liftTensor vy) (liftTensor vout) idx
  foldForeachOptimalTensorM
    (Proxy @Float)
    vout
    0
    ( \acc idx -> do
        o <- A.readTensor (liftTensor vout) idx
        x <- A.readTensor (liftTensor vx) idx
        pure (acc + o * x)
    )
    gangReduceAdd

unit_mixed_unlifted_and_spmd_subprograms :: IO ()
unit_mixed_unlifted_and_spmd_subprograms = do
  cfg <- getSPMDTestConfig
  let n = 23 :: Int
      allocLen = 32 :: Int
      sentinel = -777.0 :: Float
      alpha = 1.75 :: Float
      xs = [fromIntegral i * 0.5 - 4.0 | i <- [0 .. n - 1]] :: [Float]
      ys = [fromIntegral (n - i) * 0.25 | i <- [0 .. n - 1]] :: [Float]
      expectedOut = [max 0.0 (fma alpha x y) | (x, y) <- zip xs ys]
      expectedDot = sum (zipWith (*) expectedOut xs)

  A.withAlignedArray (xs <> replicate (allocLen - n) 99.0) $ \px ->
    A.withAlignedArray (ys <> replicate (allocLen - n) 99.0) $ \py ->
      A.allocaAligned allocLen $ \(pout :: Ptr Float) -> do
        let resetOut = pokeArray pout (replicate allocLen sentinel)
            checkOut label actualDot = do
              actual <- peekArray n pout
              forM_ (zip actual expectedOut) $ \(a, e) ->
                assertBool (label <> " out mismatch: " <> show (a, e)) (abs (a - e) < 1e-4)
              sentinels <- peekArray (allocLen - n) (pout `plusPtr` (n * sizeOf (0 :: Float)))
              sentinels @?= replicate (allocLen - n) sentinel
              assertBool (label <> " dot mismatch: " <> show (actualDot, expectedDot)) (abs (actualDot - expectedDot) < 1e-3)

        -- 1. IO / Identity (SSE2 scoped & Host)
        resetOut
        Identity dotIO_SSE2 <- withHostX86Features x86Baseline $
          mixedUnliftedProgram @IO @Identity (Identity alpha) (Identity px) (Identity py) (Identity pout)
        checkOut "Identity (SSE2)" dotIO_SSE2

        resetOut
        Identity dotIO_Host <-
          mixedUnliftedProgram @IO @Identity (Identity alpha) (Identity px) (Identity py) (Identity pout)
        checkOut "Identity (Host)" dotIO_Host

        -- 2. LinuxX86_64C (SSE2, AVX1, Host AVX2)
        resetOut
        dotC_SSE2 <- withJanusCWith x86Baseline cfg (mixedUnliftedProgram @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
          fn alpha px py pout
        checkOut "LinuxX86_64C (SSE2)" dotC_SSE2

        hostFs <- observeHostX86Features
        when (admitsX86Features hostFs x86AVX) $ do
          resetOut
          dotC_AVX <- withJanusCWith x86AVX cfg (mixedUnliftedProgram @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
            fn alpha px py pout
          checkOut "LinuxX86_64C (AVX1)" dotC_AVX

        resetOut
        dotC_Host <- withJanusC cfg (mixedUnliftedProgram @LinuxX86_64CM @LinuxX86_64C) $ \fn ->
          fn alpha px py pout
        checkOut "LinuxX86_64C (Host)" dotC_Host

        -- 3. CUDAIO & CUDADejaFu
        let launch1 =
              CUlaunchConfig
                { _cuLaunchConfigGridDimX = 1,
                  _cuLaunchConfigGridDimY = 1,
                  _cuLaunchConfigGridDimZ = 1,
                  _cuLaunchConfigBlockDimX = 1,
                  _cuLaunchConfigBlockDimY = 1,
                  _cuLaunchConfigBlockDimZ = 1,
                  _cuLaunchConfigSharedMemBytes = 0,
                  _cuLaunchConfigStream = CUstream nullPtr
                }
        A.allocaAligned 1 $ \(pDot :: Ptr Float) -> do
          resetOut
          launchCUDAIOM launch1 $ do
            d <- mixedUnliftedProgram @CUDAIOM @CUDAIO (inject alpha) (inject px) (inject py) (inject pout)
            A.pokeElemOff (inject pDot) d 0
          [dotCUDAIO] <- peekArray 1 pDot
          checkOut "CUDAIO" dotCUDAIO

          resetOut
          (resDF, _) <- launchCUDADejaFuM launch1 $ do
            d <- mixedUnliftedProgram @CUDADejaFuM @CUDADejaFu (inject alpha) (inject px) (inject py) (inject pout)
            A.pokeElemOff (inject pDot) d 0
          resDF @?= Right ()
          [dotDF] <- peekArray 1 pDot
          checkOut "CUDADejaFu" dotDF

          -- 4. GPU JanusCUDA when hardware present
          runInBoundThread $
            try (cuInit 0) >>= \case
              Left (CUException CUDA_ERROR_STUB_LIBRARY) -> pure ()
              Left (CUException CUDA_ERROR_NO_DEVICE) -> pure ()
              Left e -> assertFailure ("Unexpected CUDA error: " <> show e)
              Right () ->
                let nbytes = allocLen * sizeOf (0 :: Float)
                    gpuKern :: JanusCUDA Float -> JanusCUDA (Ptr Float) -> JanusCUDA (Ptr Float) -> JanusCUDA (Ptr Float) -> JanusCUDA (Ptr Float) -> JanusCUDAM ()
                    gpuKern a dx dy dout ddot = do
                      d <- mixedUnliftedProgram a dx dy dout
                      A.pokeElemOff ddot d 0
                 in A.allocaAligned 1 $ \(pctx :: Ptr CUctx) ->
                      withCuDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \ctx -> do
                        resetOut
                        withCuDeviceArray nbytes $ \dx ->
                          withCuDeviceArray nbytes $ \dy ->
                            withCuDeviceArray nbytes $ \dout ->
                              withCuDeviceArray 16 $ \ddot -> do
                                cuMemcpyHtoD dx (castPtr px) (fromIntegral nbytes)
                                cuMemcpyHtoD dy (castPtr py) (fromIntegral nbytes)
                                cuMemcpyHtoD dout (castPtr pout) (fromIntegral nbytes)
                                withJanusCUDA dev ctx cfg gpuKern $ \k -> do
                                  k launch1 alpha dx dy dout ddot
                                  cuCtxSynchronize
                                  cuMemcpyDtoH (castPtr pout) dout (fromIntegral nbytes)
                                  cuMemcpyDtoH (castPtr pDot) ddot 4
                                  [dotGPU] <- peekArray 1 pDot
                                  checkOut "JanusCUDA GPU" dotGPU

  -- 5. batchTensors: run unmodified A.innerProduct & A.vectorNorm across 4 independent vectors in parallel!
  let bds8 = A.tensorBound (Proxy @'[8])
  A.withTensor bds8 $ \(v0 :: A.Vector 8 Identity Float) ->
    A.withTensor bds8 $ \(v1 :: A.Vector 8 Identity Float) ->
      A.withTensor bds8 $ \(v2 :: A.Vector 8 Identity Float) ->
        A.withTensor bds8 $ \(v3 :: A.Vector 8 Identity Float) -> do
          forM_ [0 .. 7 :: Int64] $ \i -> do
            A.writeTensor v0 (Identity i A.:. A.Z) (Identity (fromIntegral (i + 1)))
            A.writeTensor v1 (Identity i A.:. A.Z) (Identity (fromIntegral (2 * i + 1)))
            A.writeTensor v2 (Identity i A.:. A.Z) (Identity (fromIntegral (i * i)))
            A.writeTensor v3 (Identity i A.:. A.Z) (Identity 1.0)
          let batched = batchTensors @4 [v0, v1, v2, v3]
          norms <- runSPMDM $ A.vectorNorm2 batched
          spmdHostToList norms
            @?= [ sum [(fromIntegral (i + 1)) ^ (2 :: Int) | i <- [0 .. 7 :: Int]],
                  sum [(fromIntegral (2 * i + 1)) ^ (2 :: Int) | i <- [0 .. 7 :: Int]],
                  sum [(fromIntegral (i * i)) ^ (2 :: Int) | i <- [0 .. 7 :: Int]],
                  8.0
                ]

--------------------------------------------------------------------------------
-- 4. Divergent Control Flow, Safe Integer Division, & Per-Lane newTensor
--------------------------------------------------------------------------------

-- Unmodified abstract Mandelbrot kernel
mandelbrot ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    ExpOrd e Float,
    ExpOrd e Int32,
    ExpBool e,
    JanusTyped e Float,
    JanusTyped e Int32,
    JanusTyped e Bool,
    Fractional (e Float),
    Num (e Int32)
  ) =>
  e Int32 ->
  e Float ->
  e Float ->
  m (e Int32)
mandelbrot maxIter cx cy = do
  zr <- newRef 0
  zi <- newRef 0
  iter <- newRef 0
  i0 <- readRef iter
  whileM (i0 `lt` maxIter) $ \condRef -> do
    r <- readRef zr
    im <- readRef zi
    let r2 = r * r
        i2 = im * im
        escaped = (r2 + i2) `gt` 4.0
    ifThenElseM_
      escaped
      (writeRef condRef false)
      ( do
          writeRef zr (r2 - i2 + cx)
          writeRef zi (2.0 * r * im + cy)
          modifyRef iter (+ 1)
          it' <- readRef iter
          writeRef condRef (it' `lt` maxIter)
      )
  readRef iter

-- Unmodified safe division (both pure ifThenElse and effectful whenM_)
safeDivKernel ::
  ( Monad m,
    CmdCond m e,
    CmdRef m e,
    ExpCond e,
    ExpEq e Int32,
    ExpIntegral e Int32,
    JanusTyped e Int32,
    Num (e Int32)
  ) =>
  e Int32 ->
  e Int32 ->
  m (e Int32)
safeDivKernel x y = do
  let purePart = ifThenElse (y `eq` 0) (-1) (x `quotient` y)
  effRef <- newRef 99
  whenM_ (y `ne` 0) $
    writeRef effRef (x `remainder` y)
  effPart <- readRef effRef
  pure (purePart * 100 + effPart)

-- Per-lane switchCase, varying rangeM, withRefPtr, and per-lane newTensor/freeTensor
divergentScratchKernel ::
  forall m e.
  ( Monad m,
    CmdCase m e,
    CmdRange m e,
    CmdCond m e,
    CmdRef m e,
    A.CmdMem m e,
    A.CmdRefPtr m e Int64,
    A.CmdStorable m e Int64,
    ExpPtrCast e,
    ExpOrd e Int64,
    JanusTyped e Int64,
    JanusTyped e Bool,
    Num (e Int64)
  ) =>
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e Int64)
divergentScratchKernel tag lo hi = do
  -- Allocate private per-lane scratch tensor only when tag /= 99
  resRef <- newRef 0
  whenM_ (tag `ne` 99) $ do
    scratch <- A.newTensor (A.tensorBound (Proxy @'[4])) :: m (A.Vector 4 e Int64)
    A.writeTensor scratch (0 A.:. A.Z) 10
    rangeM lo hi $ \i -> do
      cur <- A.readTensor scratch (0 A.:. A.Z)
      A.writeTensor scratch (0 A.:. A.Z) (cur + i)
    base <- A.readTensor scratch (0 A.:. A.Z)
    A.freeTensor scratch
    picked <-
      switchCase
        tag
        [ (0, pure (base + 1)),
          (1, pure (base * 2)),
          (2, pure (base - 5))
        ]
        (pure (base + 100))
    A.withRefPtr resRef $ \p ->
      A.poke p picked
  readRef resRef

unit_divergent_control_flow_and_safe_division :: IO ()
unit_divergent_control_flow_and_safe_division = do
  cfg <- getSPMDTestConfig

  -- 1. Mandelbrot across 8 lanes (including masked entry on IO/Identity & full gang on C)
  let cxs = [3.0, 1.0, 0.3, -0.5, 0.0, -2.1, 0.25, -0.1 :: Float]
      cys = [0.0, 1.0, 0.6, 0.5, 0.0, 0.0, 0.0, 0.8 :: Float]
      maxIters = replicate 8 (20 :: Int32)
  expectedMandel <- mapM (\(x, y) -> runIdentity <$> mandelbrot @IO @Identity 20 (Identity x) (Identity y)) (zip cxs cys)

  spmdMandelIO <-
    runSPMDM $
      mandelbrot @(SPMDM 8 IO Identity) @(SPMD 8 Identity)
        (spmdHostFromList maxIters)
        (spmdHostFromList cxs)
        (spmdHostFromList cys)
  spmdHostToList spmdMandelIO @?= expectedMandel

  -- Masked entry: disable lanes 4 and 6; must still terminate without infinite loop!
  let partialMask = simdMaskFromBools @(SIMDMask Identity 8 Int32) [True, True, True, True, False, True, False, True]
  spmdMandelMasked <-
    runSPMDMWithMask (Identity partialMask) $
      mandelbrot @(SPMDM 8 IO Identity) @(SPMD 8 Identity)
        (spmdHostFromList maxIters)
        (spmdHostFromList cxs)
        (spmdHostFromList cys)
  let maskedList = spmdHostToList spmdMandelMasked
  [maskedList !! i | i <- [0, 1, 2, 3, 5, 7]] @?= [expectedMandel !! i | i <- [0, 1, 2, 3, 5, 7]]

  -- Compiled LinuxX86_64C Mandelbrot (direct SPMD ABI evaluation!)
  spmdMandelC <-
    withJanusC cfg (mandelbrot @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C)) $ \fn ->
      fn maxIters cxs cys
  spmdMandelC @?= expectedMandel

  -- 2. Safe integer division & remainder with 0 and INT32_MIN / -1 in lanes
  let divXs = [100, 50, minBound, -37, 42, 0, minBound, 19 :: Int32]
      divYs = [7, 0, -1, 5, 0, 0, 0, -4 :: Int32]
  -- Note: for minBound / -1 our overflow guard returns minBound (quot) and 0 (rem) without SIGFPE!
  divOutIO <-
    runSPMDM $
      safeDivKernel @(SPMDM 8 IO Identity) @(SPMD 8 Identity)
        (spmdHostFromList divXs)
        (spmdHostFromList divYs)
  divOutC <-
    withJanusC cfg (safeDivKernel @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C)) $ \fn ->
      fn divXs divYs
  spmdHostToList divOutIO @?= divOutC
  -- Check non-overflow lanes against scalar Identity
  forM_ [0, 1, 3, 4, 5, 6, 7] $ \i -> do
    Identity refVal <- safeDivKernel @IO @Identity (Identity (divXs !! i)) (Identity (divYs !! i))
    (divOutC !! i) @?= refVal

  -- 3. Divergent switchCase + varying rangeM + per-lane newTensor/freeTensor + withRefPtr
  let tags = [0, 1, 2, 7, 99, 1, 0, 99 :: Int64]
      los = [0, 2, 5, 1, 0, 3, 4, 0 :: Int64]
      his = [4, 5, 5, 4, 10, 6, 2, 8 :: Int64] -- includes empty ranges (5..5, 4..2) and masked-out tag=99!
  expectedScratch <-
    mapM
      (\(t, l, h) -> runIdentity <$> divergentScratchKernel @IO @Identity (Identity t) (Identity l) (Identity h))
      (zip3 tags los his)

  scratchIO <-
    runSPMDM $
      divergentScratchKernel @(SPMDM 8 IO Identity) @(SPMD 8 Identity)
        (spmdHostFromList tags)
        (spmdHostFromList los)
        (spmdHostFromList his)
  spmdHostToList scratchIO @?= expectedScratch

  scratchC <-
    withJanusC cfg (divergentScratchKernel @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C)) $ \fn ->
      fn tags los his
  scratchC @?= expectedScratch

--------------------------------------------------------------------------------
-- 5. Unmodified Stream / Pipe / Fold / Complex / MurmurHash / ExpFix
--------------------------------------------------------------------------------

spmdPipelineSum ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    ExpBool e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  e Int64 ->
  m (e Int64)
spmdPipelineSum n =
  let evens = filterP (\x -> (x `remainder` 2) `eq` 0)
      window = dropP 2 >>> takeP 4 >>> delayP 1
      doubled = pipeM (\x -> pure (x * 2))
   in runDrain drainSum $ runPipe (evens >>> window >>> doubled) (rangeS 0 n)

spmdCartesianSum ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    ExpBool e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  e Int64 ->
  m (e Int64)
spmdCartesianSum n =
  runDrain drainSum $ do
    i <- rangeS 0 n
    j <- rangeS 0 i
    pure (10 * i + j)

spmdUnfoldFoldStats ::
  ( Monad m,
    CmdRef m e,
    CmdWhile m e,
    ExpLet e,
    ExpCond e,
    ExpOrd e Int64,
    Bounded (e Int64),
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  e Int64 ->
  e Int64 ->
  m (e Int64)
spmdUnfoldFoldStats l u = do
  let combined = (\s mn mx -> s + mn * 10 + mx * 100) <$> sumF <*> minimumF <*> maximumF
  drain (range l u) combined

data SampleRecord e = SampleRecord
  { srId :: e Int64,
    srScore :: e Float,
    srCount :: e Word32
  }
  deriving (Generic)

complexExpr ::
  (Floating (e (Complex Float)), ExpComplex e Float, ExpLet e, JanusTyped e (Complex Float), JanusTyped e Float, Num (e Float)) =>
  e (Complex Float) ->
  e (Complex Float) ->
  e Float
complexExpr z1 z2 =
  let_ (exp (z1 * 0.5) + sin z2 + sqrt (z1 * z1 + 1.0)) $ \w ->
    rePart w + imPart w

unit_unmodified_streams_complex_hash_and_fix :: IO ()
unit_unmodified_streams_complex_hash_and_fix = do
  cfg <- getSPMDTestConfig

  -- 1. Stream + stateful Pipe (filterP >>> dropP >>> takeP >>> delayP) at 8 x Int64!
  let ns8 = [0, 3, 6, 9, 12, 15, 20, 25 :: Int64]
  expectedPipe <- mapM (fmap runIdentity . spmdPipelineSum @IO @Identity . Identity) ns8

  pipeIO <- runSPMDM $ spmdPipelineSum @(SPMDM 8 IO Identity) @(SPMD 8 Identity) (spmdHostFromList ns8)
  spmdHostToList pipeIO @?= expectedPipe

  pipeC <- withJanusC cfg (spmdPipelineSum @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C)) ($ ns8)
  pipeC @?= expectedPipe

  -- 2. Nested Stream Monad bind (spmdCartesianSum) with varying bounds [0..5]
  let cs4 = [0, 1, 3, 5 :: Int64]
  expectedCart <- mapM (fmap runIdentity . spmdCartesianSum @IO @Identity . Identity) cs4

  cartIO <- runSPMDM $ spmdCartesianSum @(SPMDM 4 IO Identity) @(SPMD 4 Identity) (spmdHostFromList cs4)
  spmdHostToList cartIO @?= expectedCart

  cartC <- withJanusC cfg (spmdCartesianSum @(SPMDM 4 LinuxX86_64CM LinuxX86_64C) @(SPMD 4 LinuxX86_64C)) ($ cs4)
  cartC @?= expectedCart

  -- 3. Unfold + Applicative Fold (sumF, minimumF, maximumF using Bounded)
  let fls = [1, -5, 10, 3 :: Int64]
      fus = [5, 0, 12, 8 :: Int64]
  expectedFold <- mapM (\(l, u) -> runIdentity <$> spmdUnfoldFoldStats @IO @Identity (Identity l) (Identity u)) (zip fls fus)
  foldIO <- runSPMDM $ spmdUnfoldFoldStats @(SPMDM 4 IO Identity) @(SPMD 4 Identity) (spmdHostFromList fls) (spmdHostFromList fus)
  spmdHostToList foldIO @?= expectedFold
  foldC <- withJanusC cfg (spmdUnfoldFoldStats @(SPMDM 4 LinuxX86_64CM LinuxX86_64C) @(SPMD 4 LinuxX86_64C)) (\fn -> fn fls fus)
  foldC @?= expectedFold

  -- 4. Complex Float SoA SIMD math across Identity and LinuxX86_64C
  let zs1 = [0.2 :+ 0.3, (-0.4) :+ 0.7, 0.5 :+ (-0.2), 0.1 :+ 0.9 :: Complex Float]
      zs2 = [0.6 :+ (-0.1), 0.3 :+ 0.4, (-0.2) :+ 0.5, 0.8 :+ (-0.3) :: Complex Float]
      refComplex = [runIdentity (complexExpr @Identity (Identity a) (Identity b)) | (a, b) <- zip zs1 zs2]
      spmdComplexIO = spmdHostToList $ complexExpr @(SPMD 4 Identity) (spmdHostFromList zs1) (spmdHostFromList zs2)
  spmdComplexC <- withJanusC cfg (complexExpr @(SPMD 4 LinuxX86_64C)) (\fn -> pure (fn zs1 zs2))
  forM_ (zip spmdComplexIO refComplex) $ \(a, b) -> assertBool "Complex IO accuracy" (abs (a - b) < 1e-4)
  forM_ (zip spmdComplexC refComplex) $ \(a, b) -> assertBool "Complex C accuracy" (abs (a - b) < 1e-4)

  -- 5. Generic MurmurHash across 4 lanes
  let hashKern ::
        forall e.
        ( JanusTyped e Word64,
          JanusTyped e Int64,
          JanusTyped e Float,
          JanusTyped e Word32,
          ExpBits e Word64,
          Num (e Word64),
          ExpLet e,
          GExpHash e (Rep (SampleRecord e))
        ) =>
        e Word64 ->
        e Int64 ->
        e Float ->
        e Word32 ->
        e Word64
      hashKern salt i sc cnt =
        defaultHashWithSalt salt (SampleRecord i sc cnt) `hash64WithSalt` salt

      salts = [11, 22, 33, 44 :: Word64]
      ids = [-10, 0, 999, 123456 :: Int64]
      scores = [1.5, -2.25, 0.0, 42.125 :: Float]
      counts = [1, 7, 100, 65535 :: Word32]
      expectedHash =
        [ runIdentity (hashKern @Identity (Identity s) (Identity i) (Identity f) (Identity c))
          | (s, i, f, c) <- zip4 salts ids scores counts
        ]
      hashIO =
        spmdHostToList $
          hashKern @(SPMD 4 Identity)
            (spmdHostFromList salts)
            (spmdHostFromList ids)
            (spmdHostFromList scores)
            (spmdHostFromList counts)
  hashIO @?= expectedHash
  hashC <- withJanusC cfg (hashKern @(SPMD 4 LinuxX86_64C)) (\fn -> pure (fn salts ids scores counts))
  hashC @?= expectedHash

  -- 6. Effectful recursive factorial via ExpFix on SPMD 4 Identity
  let factM ::
        SPMD 4 Identity Int32 ->
        SPMDM 4 IO Identity (SPMD 4 Identity Int32)
      factM = fun (Proxy @(SPMD 4 Identity)) "factM" $ \self k -> do
        r <- newRef 1
        whenM_ (k `gt` 1) $ do
          sub <- self (k - 1)
          writeRef r (k * sub)
        readRef r
  factOut <- runSPMDM (factM (spmdHostFromList [0, 1, 5, 7]))
  spmdHostToList factOut @?= [1, 1, 120, 5040]
  where
    zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []

--------------------------------------------------------------------------------
-- 6. Hedgehog Cross-Tier Differential Property Test
--------------------------------------------------------------------------------

spmdCompositeKernel ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdRange m e,
    ExpCond e,
    ExpLet e,
    ExpOrd e Float,
    ExpOrd e Int64,
    ExpBoolCast e Float,
    ExpFloatingCast e Int64 Float,
    Hypot (e Float),
    FMA (e Float),
    Floating (e Float),
    JanusTyped e Float,
    JanusTyped e Int64,
    JanusTyped e Bool,
    Num (e Int64)
  ) =>
  e Int64 ->
  e Float ->
  e Float ->
  m (e Float)
spmdCompositeKernel steps x y = do
  acc <- newRef x
  rangeM 0 steps $ \k -> do
    cur <- readRef acc
    let kf = toFloating k
        h = hypot cur (y + 0.5)
        nxt = let_ (fma cur 0.75 (sin kf)) $ \t ->
          ifThenElse (t `gt` y) (t - 0.25 * h) (t + fromBool (cur `le` 0))
    ifThenElseM_
      (cur `lt` 10.0)
      (writeRef acc nxt)
      (writeRef acc (cur * 0.5))
  readRef acc

propSPMDMatchesScalarAllTiers ::
  IO ([Int64] -> [Float] -> [Float] -> IO [Float]) ->
  IO ([Int64] -> [Float] -> [Float] -> IO [Float]) ->
  Property
propSPMDMatchesScalarAllTiers getSSE2 getHost = property $ do
  fnSSE2 <- liftIO getSSE2
  fnHost <- liftIO getHost
  steps <- forAll $ Gen.list (Range.singleton 8) (Gen.int64 (Range.linear 0 6))
  xs <- forAll $ Gen.list (Range.singleton 8) (Gen.float (Range.linearFracFrom 0 (-15.0) 15.0))
  ys <- forAll $ Gen.list (Range.singleton 8) (Gen.float (Range.linearFracFrom 0 (-15.0) 15.0))

  scalarRef <- liftIO $
    mapM
      (\(s, x, y) -> runIdentity <$> spmdCompositeKernel @IO @Identity (Identity s) (Identity x) (Identity y))
      (zip3 steps xs ys)

  outIO <- liftIO $
    fmap spmdHostToList $
      runSPMDM $
        spmdCompositeKernel @(SPMDM 8 IO Identity) @(SPMD 8 Identity)
          (spmdHostFromList steps)
          (spmdHostFromList xs)
          (spmdHostFromList ys)

  outSSE2 <- liftIO $ fnSSE2 steps xs ys
  outHost <- liftIO $ fnHost steps xs ys

  let close xs1 xs2 = all (\(a, b) -> abs (a - b) < 1e-4) (zip xs1 xs2)
  assert (close outIO scalarRef)
  assert (close outSSE2 scalarRef)
  assert (close outHost scalarRef)

test_spmd_differential :: TestTree
test_spmd_differential =
  let acqSSE2 = do
        cfg <- getSPMDTestConfig
        acquireJanusCWith x86Baseline cfg (spmdCompositeKernel @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C))
      acqHost = do
        cfg <- getSPMDTestConfig
        acquireJanusC cfg (spmdCompositeKernel @(SPMDM 8 LinuxX86_64CM LinuxX86_64C) @(SPMD 8 LinuxX86_64C))
   in withResource acqSSE2 releaseJanusC $ \rSSE2 ->
        withResource acqHost releaseJanusC $ \rHost ->
          testGroup
            "SPMD cross-tier differential properties"
            [ testProperty "8-lane divergent loop + math + casts matches scalar Identity across IO, SSE2 C, & Host AVX2 C" $
                propSPMDMatchesScalarAllTiers (fst <$> rSSE2) (fst <$> rHost)
            ]
