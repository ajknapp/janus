{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Janus.Fix where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.List (isInfixOf)
import Data.Proxy
import Data.Semilattice.Lower (Lower (..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc.Policy (dynamicAlloc)
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE)
import Janus.Backend.C.Target (TargetFeatureKey, TargetFeatureOf, targetFeatureKey, x86AVX2, x86FeatureKey)
import Janus.Command.Cond (whenM_)
import qualified Janus.Command.Memory as M
import Janus.Command.Ref (newRef, readRef, writeRef)
import qualified Janus.Data.DynVec as DV
import Janus.Data.HKD (HKD (..), JanusHKD)
import Janus.Data.Layout (Elem)
import Janus.Expression.Cast (ExpFloatingCast (toFloating), ExpIntegralCast (toIntegral))
import Janus.Expression.Cond
import Janus.Expression.Ord
import Janus.Fix
import Janus.Typed
import System.Environment
import System.Mem (performGC)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, (@?=))
import Test.Tasty.Hedgehog

fac ::
  forall e a.
  ( Num (e a),
    ExpCond e,
    ExpOrd e a,
    ExpFix e (e a -> e a),
    JanusTyped e a
  ) =>
  e a ->
  e a
fac = fun (Proxy @e) "fac" $ \self n -> ifThenElse (n `le` 0) 1 (n * self (n - 1))

compiledFacEqualsInterpretedFac :: IO (Int64 -> Int64) -> Property
compiledFacEqualsInterpretedFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = runIdentity $ fac (Identity n)
      m = f' n
  k === m

compiledFacEqualsSpecFac :: IO (Int64 -> Int64) -> Property
compiledFacEqualsSpecFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = product [1 .. n]
      m = f' n
  k === m

unit_withJanusC_lazy_dl_lifetime :: IO ()
unit_withJanusC_lazy_dl_lifetime = do
  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
  lazyConst <- withJanusC cfg (fac @JanusCCSE @Int64 10) pure
  lazyApp <- withJanusC cfg (fac @JanusCCSE @Int64) (\f -> pure (f 10))
  escapedFn <- withJanusC cfg (fac @JanusCCSE @Int64) pure
  performGC
  threadDelay 10000
  lazyConst @?= 3628800
  lazyApp @?= 3628800
  escapedFn 10 @?= 3628800

newtype CustomFeatures = CustomFeatures {customFastMultiplier :: Int64}
  deriving (Eq, Ord, Show)

instance Lower CustomFeatures where
  lowerBound = CustomFeatures 1

type instance TargetFeatureOf "custom-linux-test" = CustomFeatures

type instance TargetFeatureOf "custom-freebsd-test" = CustomFeatures

customLinuxKey, customFreeBSDKey :: TargetFeatureKey CustomFeatures
customLinuxKey = targetFeatureKey @"custom-linux-test"
customFreeBSDKey = targetFeatureKey @"custom-freebsd-test"

data CustomLinuxOS

data CustomFreeBSDOS

data CustomArch os

type CustomTestTarget = CustomArch CustomLinuxOS

type CustomFreeBSDTarget = CustomArch CustomFreeBSDOS

sharedCustomFeatureProg ::
  forall os.
  (JanusCTarget (CustomArch os), CTargetFeatures (CustomArch os) ~ CustomFeatures) =>
  JanusCTCSE (CustomArch os) Int64 ->
  JanusCMTCSE (CustomArch os) (JanusCTCSE (CustomArch os) Int64)
sharedCustomFeatureProg x = do
  CustomFeatures mult <- askCTargetFeatures @(CustomArch os)
  pure (x * fromIntegral mult)

instance JanusCTarget (CustomArch CustomLinuxOS) where
  type CTargetFeatures (CustomArch CustomLinuxOS) = CustomFeatures
  cTargetFeatureKey _ = customLinuxKey
  observeCTargetFeatures _ = pure (CustomFeatures 1)
  ensureCTargetAdmitted _ _ = pure ()
  cTargetBanner _ (CustomFeatures mult) =
    "/* janus-custom-linux: mult=" <> show mult <> " */\n"
  cTargetBuildFlags _ _ _ = pure ([], [])

instance JanusCTarget (CustomArch CustomFreeBSDOS) where
  type CTargetFeatures (CustomArch CustomFreeBSDOS) = CustomFeatures
  cTargetFeatureKey _ = customFreeBSDKey
  observeCTargetFeatures _ = pure (CustomFeatures 2)
  ensureCTargetAdmitted _ _ = pure ()
  cTargetBanner _ (CustomFeatures mult) =
    "/* janus-custom-freebsd: mult=" <> show mult <> " */\n"
  cTargetBuildFlags _ _ _ = pure ([], [])

data CustomPair f = CustomPair
  { cpScale :: !(f Int64),
    cpBias :: !(f Int64)
  }
  deriving (Generic)

instance JanusHKD CustomPair

unit_expfix_on_linux_x86_64_and_custom_target :: IO ()
unit_expfix_on_linux_x86_64_and_custom_target = do
  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir

  -- 1. ExpFix works directly on LinuxX86_64CCSE via target-polymorphic JanusCT
  x86Res <- withJanusC cfg (fac @LinuxX86_64CCSE @Int64) (\f -> pure (f 10))
  x86Res @?= 3628800

  -- 2. Open-world TargetFeatureKey: two OS targets sharing CustomFeatures + x86FeatureKey
  --    coexist in CBuildConfig without colliding and share codegen via askCTargetFeatures
  let customFac = fac @(JanusCTCSE CustomTestTarget) @Int64
      cBannerSrc = showJanusCWith (CustomFeatures 4) "custom_fac" customFac
  assertBool
    "custom target banner emitted"
    ("/* janus-custom-linux: mult=4 */" `isInfixOf` cBannerSrc)

  customFacRes <- withJanusC cfg customFac (\f -> pure (f 8))
  customFacRes @?= 40320

  let cfgMulti =
        cfg
          & cBuildConfigTargetFeature x86FeatureKey ?~ x86AVX2
          & cBuildConfigTargetFeature customLinuxKey ?~ CustomFeatures 10
          & cBuildConfigTargetFeature customFreeBSDKey ?~ CustomFeatures 30
  (cfgMulti ^. cBuildConfigTargetFeature x86FeatureKey) @?= Just x86AVX2
  (cfgMulti ^. cBuildConfigTargetFeature customLinuxKey) @?= Just (CustomFeatures 10)
  (cfgMulti ^. cBuildConfigTargetFeature customFreeBSDKey) @?= Just (CustomFeatures 30)

  linuxRes <- withJanusC cfgMulti (sharedCustomFeatureProg @CustomLinuxOS) (\f -> f 7)
  freebsdRes <- withJanusC cfgMulti (sharedCustomFeatureProg @CustomFreeBSDOS) (\f -> f 7)
  linuxRes @?= 70
  freebsdRes @?= 210

  let hkdProg :: HKD CustomPair (JanusCTCSE CustomTestTarget) -> JanusCTCSE CustomTestTarget Int64 -> JanusCTCSE CustomTestTarget Int64
      hkdProg (HKD p) x = cpScale p * x + cpBias p
  hkdRes <- withJanusC cfg hkdProg (\f -> pure (f (CustomPair (Identity 7) (Identity 11)) 5))
  hkdRes @?= 46

  let buildAndSumVec :: JanusCTCSE CustomTestTarget Int64 -> JanusCMTCSE CustomTestTarget (DV.DynVec (JanusCTCSE CustomTestTarget) Int64)
      buildAndSumVec x = do
        v <- DV.new @Int64 dynamicAlloc
        DV.push dynamicAlloc v x
        DV.push dynamicAlloc v (x * 2)
        DV.push dynamicAlloc v (x * 3)
        pure v
      readVecLen :: DV.DynVec (JanusCTCSE CustomTestTarget) Int64 -> JanusCMTCSE CustomTestTarget (JanusCTCSE CustomTestTarget Int64)
      readVecLen v = do
        n <- DV.len v
        a0 <- DV.readVec v 0
        a2 <- DV.readVec v 2
        DV.free dynamicAlloc v
        pure (n * 1000 + a0 + a2)
  vecHandle <- withJanusC cfg buildAndSumVec (\mk -> mk 9)
  vecSummary <- withJanusC cfg readVecLen (\rd -> rd vecHandle)
  vecSummary @?= (3000 + 9 + 27)

polyConstLocal ::
  forall e a.
  (Num (e a), ExpFix e (e a)) =>
  e a
polyConstLocal = fun (Proxy @e) "poly_const" $ \_ -> 7

polyPushSeqLocal ::
  forall m e a.
  ( DV.DynVecCtx m e a,
    Elem e a ~ e a,
    Num (e a),
    ExpOrd e Int64,
    ExpFix e (DV.DynVec e a -> e Int64 -> e a -> m ())
  ) =>
  DV.DynVec e a ->
  e Int64 ->
  e a ->
  m ()
polyPushSeqLocal =
  fun (Proxy @e) "poly_push_seq" $ \self dv cnt val ->
    whenM_ (cnt `gt` 0) $ do
      DV.push dynamicAlloc dv val
      self dv (cnt - 1) (val + 1)

unit_polymorphic_and_compound_local_functions :: IO ()
unit_polymorphic_and_compound_local_functions = do
  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir

      polyKernel ::
        forall m e.
        ( DV.DynVecCtx m e Int32,
          DV.DynVecCtx m e Int64,
          ExpFloatingCast e Int32 Double,
          ExpFloatingCast e Int64 Double,
          ExpIntegralCast e Int32 Int64,
          ExpFix e (e Int32 -> e Int32),
          ExpFix e (e Int64 -> e Int64),
          ExpFix e (e Double -> e Double),
          ExpFix e (e Int32),
          ExpFix e (e Int64),
          ExpFix e (DV.DynVec e Int32 -> e Int64 -> e Int32 -> m ()),
          ExpFix e (DV.DynVec e Int64 -> e Int64 -> e Int64 -> m ()),
          ExpFix e (HKD CustomPair e -> (e Int64, e Int64) -> m (e Int64, e Int64))
        ) =>
        e Int32 ->
        e Int64 ->
        m (e Double)
      polyKernel a32 b64 = do
        -- 1. Same helper name "fac" specialized at 3 input types (Int32, Int64, Double) in one TU
        let f32 = fac @e @Int32 a32
            f64 = fac @e @Int64 b64
            fDbl = fac @e @Double 4.0
        -- 2. Return-type-polymorphic 0-arg helper "poly_const" specialized at Int32 and Int64
            c32 = polyConstLocal @e @Int32
            c64 = polyConstLocal @e @Int64

        -- 3. Polymorphic void monadic helper "poly_push_seq" over DynVec Int32 and DynVec Int64 handles
        dv32 <- DV.new @Int32 dynamicAlloc
        dv64 <- DV.new @Int64 dynamicAlloc
        polyPushSeqLocal @m @e @Int32 dv32 3 10
        polyPushSeqLocal @m @e @Int64 dv64 3 100
        v32 <- DV.readVec dv32 2
        v64 <- DV.readVec dv64 2
        DV.free dynamicAlloc dv32
        DV.free dynamicAlloc dv64

        -- 4. Effectful multi-return (tuple) + HKD parameter local function; verify side effect runs ONCE per call
        sideRef <- newRef (0 :: e Int64)
        let pairStep :: HKD CustomPair e -> (e Int64, e Int64) -> m (e Int64, e Int64)
            pairStep =
              fun (Proxy @e) "pair_step" $ \_ (HKD p) (x, y) -> do
                pure (cpScale p * x + cpBias p, cpScale p * y - cpBias p)
        (r1, r2) <- pairStep (HKD (CustomPair 3 5)) (10, 20)
        cur <- readRef sideRef
        writeRef sideRef (cur + r1 + r2)
        sTot <- readRef sideRef

        let intSum = toIntegral f32 + f64 + toIntegral c32 + c64 + toIntegral v32 + v64 + sTot
        pure (toFloating intSum + fDbl)

  -- Also verify single-evaluation of side effects inside a monadic local function via a pointer counter
  let sideEffectOnceProg :: JanusCCSE (Ptr Int64) -> JanusCMCSE (JanusCCSE Int64)
      sideEffectOnceProg cntPtr = do
        let bumpAndReturn :: JanusCCSE (Ptr Int64) -> JanusCCSE Int64 -> JanusCMCSE (JanusCCSE Int64, JanusCCSE Int64)
            bumpAndReturn =
              fun (Proxy @JanusCCSE) "bump_once" $ \_ p x -> do
                old <- M.peek p
                M.poke p (old + 1)
                pure (x + 10, x * 2)
        (u, v) <- bumpAndReturn cntPtr 7
        c <- M.peek cntPtr
        pure (c * 1000 + u + v)

  cSrc <- showJanusC @PortableC "poly_entry" (polyKernel @JanusCMCSE @JanusCCSE)
  assertBool "mangled local fac helper emitted" ("janus_local_fac_" `isInfixOf` cSrc)
  assertBool "mangled local poly_push_seq helper emitted" ("janus_local_poly_push_seq_" `isInfixOf` cSrc)

  Identity ioOut <- polyKernel @IO @Identity (Identity 5) (Identity 6)
  cOut <- withJanusC @PortableC cfg (polyKernel @JanusCMCSE @JanusCCSE) (\fn -> fn 5 6)
  cOut @?= ioOut

  onceOut <- withJanusC @PortableC cfg sideEffectOnceProg (\fn -> with (0 :: Int64) fn)
  onceOut @?= (1000 + 17 + 14)

test_fix :: TestTree
test_fix = do
  let acquire f = do
        tmpdir <- getEnv "TMPDIR"
        acquireJanusC (defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir) f
  withResource (acquire (fac @JanusCCSE @Int64)) releaseJanusC $ \k ->
    let k' = fst <$> k
     in testGroup
        "fix"
        [ testProperty "compiled fac = interpreted fac" (compiledFacEqualsInterpretedFac k'),
          testProperty "compiled fac = spec fac" (compiledFacEqualsSpecFac k')
        ]
