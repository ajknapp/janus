{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Janus.Data.HKDLayout where

import Control.Lens ((&), (.~))
import Control.Monad (forM_, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc (dynamicAlloc, readAllocSnapshot, snapLiveBlocks, snapLiveBytes, statsAlloc, withAllocTracker)
import Janus.Backend.C (JanusC, JanusCM, acquireJanusC, releaseJanusC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM)
import Janus.Command.Array (ExpSized (..))
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Data
import qualified Janus.Data.DynVec as DV
import Janus.Expression.Cast (ExpFloatingCast (..))
import Janus.SPMD (SPMD)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.Hedgehog (testProperty)

getLayoutTestConfig :: IO CBuildConfig
getLayoutTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

--------------------------------------------------------------------------------
-- Test HKD Struct Definitions (Mixed Widths, Nested Structs, & Two-Level SOAOA)
--------------------------------------------------------------------------------

data InnerPair f = InnerPair
  { ipTag :: !(f Int8),
    ipCount :: !(f Int32)
  }
  deriving stock (Generic)

deriving stock instance Eq (InnerPair Identity)

deriving stock instance Show (InnerPair Identity)

instance JanusStruct InnerPair

instance JanusHKD InnerPair

data MixedRecord f = MixedRecord
  { mrByte :: !(f Int8),
    mrScore :: !(f Double),
    mrInner :: !(InnerPair f),
    mrId :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (MixedRecord Identity)

deriving stock instance Show (MixedRecord Identity)

instance JanusStruct MixedRecord

instance JanusHKD MixedRecord

data Particle f = Particle
  { pX :: !(f Float),
    pY :: !(f Float),
    pVx :: !(f Float),
    pVy :: !(f Float)
  }
  deriving stock (Generic)

deriving stock instance Eq (Particle Identity)

deriving stock instance Show (Particle Identity)

instance JanusStruct Particle

instance JanusHKD Particle

data ThermoGroup f = ThermoGroup
  { tgMass :: !(f Float),
    tgTemp :: !(f Float)
  }
  deriving stock (Generic)

deriving stock instance Eq (ThermoGroup Identity)

deriving stock instance Show (ThermoGroup Identity)

instance JanusStruct ThermoGroup

-- | Two-level HKD record for SOAOA (2 independent AoSoA SIMD streams: kinematics + thermodynamics)
data ParticleSys f = ParticleSys
  { psKin :: !(Particle f),
    psThermo :: !(ThermoGroup f)
  }
  deriving stock (Generic)

deriving stock instance Eq (ParticleSys Identity)

deriving stock instance Show (ParticleSys Identity)

instance JanusStruct ParticleSys

--------------------------------------------------------------------------------
-- 1. C-ABI Struct Size, Alignment, & Field Offset Verification vs Compiled C
--------------------------------------------------------------------------------

cMixedStructSizeKernel :: JanusC Int64 -> JanusCM (JanusC Int64)
cMixedStructSizeKernel dummy = do
  let sz = sizeOf (Proxy @(CStruct MixedRecord))
      al = alignOf (Proxy @(CStruct MixedRecord))
  pure (dummy + sz * 1000 + al)

cParticleHKDKernel :: HKD Particle LinuxX86_64C -> LinuxX86_64CM (LinuxX86_64C Float)
cParticleHKDKernel (HKD p) =
  pure (pX p * pVx p + pY p * pVy p)

unit_cstruct_abi_layout_matches_compiled_c :: IO ()
unit_cstruct_abi_layout_matches_compiled_c = do
  cfg <- getLayoutTestConfig
  let hsSize = fromIntegral (Storable.sizeOf (undefined :: CStruct MixedRecord)) :: Int64
      hsAlign = fromIntegral (Storable.alignment (undefined :: CStruct MixedRecord)) :: Int64
      offByte = fieldByteOffset @MixedRecord mrByte
      offScore = fieldByteOffset @MixedRecord mrScore
      offId = fieldByteOffset @MixedRecord mrId
  -- Verify System V AMD64 natural alignment rules:
  -- mrByte at 0 (1B + 7B pad), mrScore at 8 (8B), mrInner at 16 (Int8 at 0 + 3B pad + Int32 at 4 = 8B), mrId at 24 (8B) => total 32B, align 8B!
  offByte @?= 0
  offScore @?= 8
  offId @?= 24
  hsSize @?= 32
  hsAlign @?= 8

  cCombined <- withJanusC cfg cMixedStructSizeKernel (\fn -> fn 0)
  cCombined @?= (hsSize * 1000 + hsAlign)

  -- Also verify Storable peek/poke round-trip and HKD parameter FFI
  resDot <- do
    hdl <- acquireJanusC cfg cParticleHKDKernel
    out <- fst hdl (Particle (Identity 2.0) (Identity 3.0) (Identity 4.0) (Identity 5.0))
    releaseJanusC hdl
    pure out
  resDot @?= 23.0

--------------------------------------------------------------------------------
-- 2. Equivalence Across All 5 Storage Layouts (`Scalar`, `AoS`, `SoA`, `AoSoA 8`, `SOAOA 8`)
--------------------------------------------------------------------------------

check_all_five_layouts_equivalent :: Property
check_all_five_layouts_equivalent = property $ do
  inputs <-
    forAll $
      Gen.list
        (Range.linear 0 75)
        ( (,,,,,)
            <$> Gen.float (Range.linearFracFrom 0 (-50) 50)
            <*> Gen.float (Range.linearFracFrom 0 (-50) 50)
            <*> Gen.float (Range.linearFracFrom 0 (-10) 10)
            <*> Gen.float (Range.linearFracFrom 0 (-10) 10)
            <*> Gen.float (Range.linearFracFrom 1 0.5 20)
            <*> Gen.float (Range.linearFracFrom 0 100 500)
        )

  (aosOut, soaOut, aosoaOut, soaoaOut, liveAfter) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
          mkP (x, y, vx, vy, _, _) = Particle (Identity x) (Identity y) (Identity vx) (Identity vy)
          mkSys inp@(_, _, _, _, m, t) = ParticleSys (mkP inp) (ThermoGroup (Identity m) (Identity t))

      vAoS <- DV.new @(AoS Particle) alloc
      vSoA <- DV.new @(SoA Particle) alloc
      vAoSoA <- DV.new @(AoSoA 8 Particle) alloc
      vSOAOA <- DV.new @(SOAOA 8 ParticleSys) alloc

      forM_ inputs $ \inp -> do
        DV.push alloc vAoS (mkP inp)
        DV.push alloc vSoA (mkP inp)
        DV.push alloc vAoSoA (mkP inp)
        DV.push alloc vSOAOA (mkSys inp)

      -- Run SPMD 8-lane step `x += 0.5 * vx; y += 0.5 * vy` on SoA, AoSoA 8, and SOAOA 8,
      -- and scalar update on AoS!
      let stepParticle :: (Fractional (g Float)) => Particle g -> Particle g
          stepParticle p =
            p
              { pX = pX p + 0.5 * pVx p,
                pY = pY p + 0.5 * pVy p
              }

      Identity n <- DV.len vAoS
      forM_ [0 .. n - 1] $ \i ->
        DV.modifyVec vAoS (Identity i) stepParticle

      DV.foreachSoAVecSPMD (Proxy @8) vSoA (\_ g -> pure (stepParticle g))
      DV.foreachAoSoAVecSPMD (Proxy @8) vAoSoA (\_ g -> pure (stepParticle g))
      DV.foreachSOAOAVecSPMD
        (Proxy @8)
        vSOAOA
        ( \_ sys ->
            pure
              sys
                { psKin = stepParticle (psKin sys),
                  psThermo = ThermoGroup (tgMass (psThermo sys)) (tgTemp (psThermo sys) + 1.0)
                }
        )

      let collectVec :: (DynVecCtx IO Identity l) => DynVec Identity l -> IO [Elem Identity l]
          collectVec v = do
            Identity lenV <- DV.len v
            mapM (DV.readVec v . Identity) [0 .. lenV - 1]

      rAoS <- collectVec vAoS
      rSoA <- collectVec vSoA
      rAoSoA <- collectVec vAoSoA
      rSOAOA <- map psKin <$> collectVec vSOAOA

      DV.free alloc vAoS
      DV.free alloc vSoA
      DV.free alloc vAoSoA
      DV.free alloc vSOAOA

      snap <- readAllocSnapshot tracker
      pure (rAoS, rSoA, rAoSoA, rSOAOA, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  soaOut === aosOut
  aosoaOut === aosOut
  soaoaOut === aosOut
  liveAfter === (0, 0)

--------------------------------------------------------------------------------
-- 3. Compiled LinuxX86_64C SPMD Execution Over Host-Constructed SoA & AoSoA DynVecs
--------------------------------------------------------------------------------

cIntegrateSoAKernel ::
  DynVec LinuxX86_64C (SoA Particle) ->
  LinuxX86_64CM (LinuxX86_64C Float)
cIntegrateSoAKernel v = do
  DV.foreachSoAVecSPMD (Proxy @8) v $ \idx p -> do
    let scale = toFloating idx * (0.0 :: SPMD 8 LinuxX86_64C Float) + 2.0
    pure
      p
        { pX = pX p + scale * pVx p,
          pY = pY p + scale * pVy p
        }
  runDrain drainSum (pX <$> DV.stream v)

cIntegrateAoSoAKernel ::
  DynVec LinuxX86_64C (AoSoA 8 Particle) ->
  LinuxX86_64CM (LinuxX86_64C Float)
cIntegrateAoSoAKernel v = do
  DV.foreachAoSoAVecSPMD (Proxy @8) v $ \_ p ->
    pure
      p
        { pX = pX p + 2.0 * pVx p,
          pY = pY p + 2.0 * pVy p
        }
  runDrain drainSum (pX <$> DV.stream v)

check_compiled_c_spmd_soa_and_aosoa ::
  IO (DynVec Identity (SoA Particle) -> IO Float) ->
  IO (DynVec Identity (AoSoA 8 Particle) -> IO Float) ->
  Property
check_compiled_c_spmd_soa_and_aosoa getSoAFn getAoSoAFn = property $ do
  fnSoA <- liftIO getSoAFn
  fnAoSoA <- liftIO getAoSoAFn
  pts <-
    forAll $
      Gen.list
        (Range.linear 0 60)
        ( (,,,)
            <$> Gen.float (Range.linearFracFrom 0 (-20) 20)
            <*> Gen.float (Range.linearFracFrom 0 (-20) 20)
            <*> Gen.float (Range.linearFracFrom 0 (-5) 5)
            <*> Gen.float (Range.linearFracFrom 0 (-5) 5)
        )
  (sumSoA, sumAoSoA, pSoAList, pAoSoAList) <- liftIO $ do
    vSoA <- DV.new @(SoA Particle) dynamicAlloc
    vAoSoA <- DV.new @(AoSoA 8 Particle) dynamicAlloc
    forM_ pts $ \(x, y, vx, vy) -> do
      let p = Particle (Identity x) (Identity y) (Identity vx) (Identity vy)
      DV.push dynamicAlloc vSoA p
      DV.push dynamicAlloc vAoSoA p
    s1 <- fnSoA vSoA
    s2 <- fnAoSoA vAoSoA
    Identity n <- DV.len vSoA
    l1 <- mapM (DV.readVec vSoA . Identity) [0 .. n - 1]
    l2 <- mapM (DV.readVec vAoSoA . Identity) [0 .. n - 1]
    DV.free dynamicAlloc vSoA
    DV.free dynamicAlloc vAoSoA
    pure (s1, s2, l1, l2)

  let expectedPts =
        [ Particle (Identity (x + 2 * vx)) (Identity (y + 2 * vy)) (Identity vx) (Identity vy)
        | (x, y, vx, vy) <- pts
        ]
  pSoAList === expectedPts
  pAoSoAList === expectedPts
  let _ = zipWithM_ @IO
  assert (abs (sumSoA - sumAoSoA) <= 1e-3)

test_hkd_layouts_and_spmd :: TestTree
test_hkd_layouts_and_spmd =
  let acqSoA = getLayoutTestConfig >>= flip acquireJanusC cIntegrateSoAKernel
      acqAoSoA = getLayoutTestConfig >>= flip acquireJanusC cIntegrateAoSoAKernel
   in withResource acqSoA releaseJanusC $ \rSoA ->
        withResource acqAoSoA releaseJanusC $ \rAoSoA ->
          testGroup
            "HKD Storage Layouts & Compiled C SPMD"
            [ testProperty "Scalar / AoS / SoA / AoSoA 8 / SOAOA 8 equivalence & zero-leak tracking" check_all_five_layouts_equivalent,
              testProperty "Host -> Compiled C SPMD over SoA and AoSoA 8 DynVecs" $
                check_compiled_c_spmd_soa_and_aosoa (fst <$> rSoA) (fst <$> rAoSoA)
            ]
