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
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc.Policy (dynamicAlloc)
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE)
import Janus.Backend.C.Target (TargetFeatureKey, TargetFeatureOf, targetFeatureKey, x86AVX2, x86FeatureKey)
import qualified Janus.Data.DynVec as DV
import Janus.Data.HKD (HKD (..), JanusHKD)
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
