{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Janus.Alloc
  ( Alloc,
    AllocTrackerSpine,
    CmdAlloc,
    destroyFixedBuf,
    destroyRecycler,
    dynamicAlloc,
    fallbackAlloc,
    fixedAlloc,
    freeAllocTracker,
    newAllocTracker,
    newFixedBuf,
    newRecycler,
    readAllocSnapshot,
    recyclingAlloc,
    segregateAlloc,
    snapLiveBlocks,
    snapLiveBytes,
    snapTotalAllocs,
    statsAlloc,
    withAllocTracker,
    withFixedBuf,
  )
import Janus.Backend.C (JanusC, JanusCM, JanusCTyped, acquireJanusC, releaseJanusC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64, LinuxX86_64C, LinuxX86_64CM)
import Janus.Command.Memory (ExpSized (..))
import Janus.Command.Cond (whenM_)
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Data
import qualified Janus.Data.DynVec as DV
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpFloatingCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.SPMD (SPMD)
import Janus.Typed (JanusTyped)
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

--------------------------------------------------------------------------------
-- 4. Normal & Scoped Buffer `new -> reallocate -> free` Across All 5 Layouts
--------------------------------------------------------------------------------

fillRangeBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Buf a e ->
  e Int64 ->
  e Int64 ->
  (e Int64 -> Elem e a) ->
  m ()
fillRangeBuf b lo hi mk =
  whenM_ (lnot (isNullBuf @a (Proxy @m) b)) $
    rangeM lo hi $ \i -> writeBuf @a b i (mk i)

fillRangeScopedBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  ScopedBuf m e a ->
  e Int64 ->
  e Int64 ->
  (e Int64 -> Elem e a) ->
  m ()
fillRangeScopedBuf sb lo hi mk = do
  b <- scopedBuf sb
  fillRangeBuf @a b lo hi mk

sumBufPrefix ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a, JanusTyped e Float, Num (e Float)) =>
  Buf a e ->
  e Int64 ->
  (Elem e a -> e Float) ->
  m (e Float)
sumBufPrefix b n proj = do
  acc <- newRef (0 :: e Float)
  whenM_ (lnot (isNullBuf @a (Proxy @m) b)) $
    rangeM 0 n $ \i -> do
      v <- readBuf @a b i
      cur <- readRef acc
      writeRef acc (cur + proj v)
  readRef acc

sumScopedBufPrefix ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a, JanusTyped e Float, Num (e Float)) =>
  ScopedBuf m e a ->
  e Int64 ->
  (Elem e a -> e Float) ->
  m (e Float)
sumScopedBufPrefix sb n proj = do
  b <- scopedBuf sb
  sumBufPrefix @a b n proj

exerciseEmptyNoAlloc ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Alloc e ->
  m (e Bool)
exerciseEmptyNoAlloc alloc = do
  b0 <- allocBuf @a alloc 0
  bNeg <- allocBuf @a alloc (-4)
  b0Re <- reallocBuf @a alloc b0 0 0 0
  bNegRe <- reallocBuf @a alloc bNeg 0 (-3) 0
  freeBuf @a alloc b0Re 0
  freeBuf @a alloc bNegRe 0
  sbOk <-
    withBuf @a alloc 0 $ \sb -> do
      c0 <- scopedBufCap sb
      bz <- reallocScopedBuf sb 0 0
      cz <- scopedBufCap sb
      freeScopedBuf sb
      pure ((c0 `eq` 0) `land` isNullBuf @a (Proxy @m) bz `land` (cz `eq` 0))
  pure
    ( isNullBuf @a (Proxy @m) b0
        `land` isNullBuf @a (Proxy @m) bNeg
        `land` isNullBuf @a (Proxy @m) b0Re
        `land` isNullBuf @a (Proxy @m) bNegRe
        `land` sbOk
    )

exerciseLayoutLifecycle ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a, JanusTyped e Float, Fractional (e Float)) =>
  Alloc e ->
  Alloc e ->
  (e Int64 -> Elem e a) ->
  (Elem e a -> e Float) ->
  m (e Float)
exerciseLayoutLifecycle composite tinyPolicy mkElem projElem = do
  -- Part A: Normal buffer `allocBuf -> reallocBuf* -> freeBuf`
  b0 <- allocBuf @a composite 0
  b0' <- reallocBuf @a composite b0 0 0 0
  b5 <- reallocBuf @a composite b0' 0 5 0
  fillRangeBuf @a b5 0 5 (\i -> mkElem (i + 1))
  -- Same-tile / same-aligned-slab growth (5 -> 7)
  b7 <- reallocBuf @a composite b5 5 7 5
  fillRangeBuf @a b7 5 7 (\i -> mkElem (i + 1))
  -- Cross-tile / cross-slab growth with `liveLen > oldCap` (clamped to `oldCap = 7`)
  b25 <- reallocBuf @a composite b7 7 25 25
  fillRangeBuf @a b25 7 25 (\i -> mkElem (i + 1))
  -- Identical-capacity realloc (`25 -> 25`)
  b25Same <- reallocBuf @a composite b25 25 25 25
  -- Shrink (`25 -> 10`)
  b10 <- reallocBuf @a composite b25Same 25 10 10
  sumNorm10 <- sumBufPrefix @a b10 10 projElem
  -- Shrink to zero (`10 -> 0`) and negative (`0 -> -4`), then re-grow (`0 -> 12`)
  bZero <- reallocBuf @a composite b10 10 0 0
  bZero2 <- reallocBuf @a composite bZero 0 (-4) 0
  b12 <- reallocBuf @a composite bZero2 0 12 0
  fillRangeBuf @a b12 0 12 (\i -> mkElem ((i + 1) * 2))
  sumNorm12 <- sumBufPrefix @a b12 12 projElem
  freeBuf @a composite b12 12

  -- Normal buffer OOM on `allocBuf` and `reallocBuf`
  bFailNew <- allocBuf @a tinyPolicy 100000
  freeBuf @a tinyPolicy bFailNew 100000
  bSmall <- allocBuf @a tinyPolicy 4
  fillRangeBuf @a bSmall 0 4 (\i -> mkElem (10 + i))
  bFailGrow <- reallocBuf @a tinyPolicy bSmall 4 100000 4
  sumNormOom <- sumBufPrefix @a bSmall 4 projElem
  freeBuf @a tinyPolicy bSmall 4

  let normOk =
        isNullBuf @a (Proxy @m) b0
          `land` isNullBuf @a (Proxy @m) b0'
          `land` isNullBuf @a (Proxy @m) bZero
          `land` isNullBuf @a (Proxy @m) bZero2
          `land` isNullBuf @a (Proxy @m) bFailNew
          `land` isNullBuf @a (Proxy @m) bFailGrow
      normScore = ifThenElse normOk (sumNorm10 + sumNorm12 + sumNormOom) (-1000000.0)

  -- Part B1: Scoped buffer `withBuf -> reallocScopedBuf* -> scope-exit free`
  scopedScoreA <-
    withBuf @a composite 0 $ \sb -> do
      c0 <- scopedBufCap sb
      bz0 <- reallocScopedBuf sb 0 0
      _ <- reallocScopedBuf sb 5 0
      fillRangeScopedBuf sb 0 5 (\i -> mkElem (i + 1))
      _ <- reallocScopedBuf sb 7 5
      fillRangeScopedBuf sb 5 7 (\i -> mkElem (i + 1))
      _ <- reallocScopedBuf sb 25 25
      fillRangeScopedBuf sb 7 25 (\i -> mkElem (i + 1))
      _ <- reallocScopedBuf sb 25 25
      _ <- reallocScopedBuf sb 10 10
      s10 <- sumScopedBufPrefix sb 10 projElem
      bz1 <- reallocScopedBuf sb 0 0
      cz1 <- scopedBufCap sb
      _ <- reallocScopedBuf sb 12 0
      c12 <- scopedBufCap sb
      fillRangeScopedBuf sb 0 12 (\i -> mkElem ((i + 1) * 2))
      s12 <- sumScopedBufPrefix sb 12 projElem
      let okA =
            (c0 `eq` 0)
              `land` isNullBuf @a (Proxy @m) bz0
              `land` isNullBuf @a (Proxy @m) bz1
              `land` (cz1 `eq` 0)
              `land` (c12 `eq` 12)
      pure (ifThenElse okA (s10 + s12) (-1000000.0))

  -- Part B2: Explicit early `freeScopedBuf` (idempotent + re-allocatable in same scope)
  scopedScoreB <-
    withBuf @a composite 9 $ \sb -> do
      fillRangeScopedBuf sb 0 9 (\i -> mkElem (i + 1))
      s9 <- sumScopedBufPrefix sb 9 projElem
      freeScopedBuf sb
      bFreed <- scopedBuf sb
      cFreed <- scopedBufCap sb
      freeScopedBuf sb
      _ <- reallocScopedBuf sb 6 0
      cRe <- scopedBufCap sb
      fillRangeScopedBuf sb 0 6 (\i -> mkElem ((i + 1) * 3))
      s6 <- sumScopedBufPrefix sb 6 projElem
      let okB =
            isNullBuf @a (Proxy @m) bFreed
              `land` (cFreed `eq` 0)
              `land` (cRe `eq` 6)
      pure (ifThenElse okB (s9 + s6) (-1000000.0))

  -- Part B3: Scoped buffer OOM on initial `withBuf` (`scopedBufCap` must be 0)
  scopedScoreC <-
    withBuf @a tinyPolicy 100000 $ \sb -> do
      bOom <- scopedBuf sb
      cOom <- scopedBufCap sb
      pure (ifThenElse (isNullBuf @a (Proxy @m) bOom `land` (cOom `eq` 0)) 11.0 (-1000000.0))

  -- Part B4: Scoped buffer OOM on `reallocScopedBuf` (preserves live buffer & cap)
  scopedScoreD <-
    withBuf @a tinyPolicy 4 $ \sb -> do
      fillRangeScopedBuf sb 0 4 (\i -> mkElem (20 + i))
      bFail <- reallocScopedBuf sb 100000 4
      cKeep <- scopedBufCap sb
      sKeep <- sumScopedBufPrefix sb 4 projElem
      pure (ifThenElse (isNullBuf @a (Proxy @m) bFail `land` (cKeep `eq` 4)) sKeep (-1000000.0))

  pure (normScore + scopedScoreA + scopedScoreB + scopedScoreC + scopedScoreD)

lifecycleAllLayoutsKernel ::
  forall m e.
  ( CmdAlloc m e,
    LayoutElemCtx m e Int64,
    LayoutElemCtx m e (AoS Particle),
    LayoutElemCtx m e (SoA Particle),
    LayoutElemCtx m e (AoSoA 8 Particle),
    LayoutElemCtx m e (SOAOA 8 ParticleSys),
    JanusTyped e Float,
    ExpFloatingCast e Int64 Float,
    Fractional (e Float),
    JanusCTyped (CStruct AllocTrackerSpine)
  ) =>
  m (e Float)
lifecycleAllLayoutsKernel = do
  tracker <- newAllocTracker
  fb <- newFixedBuf dynamicAlloc 768
  rec <- newRecycler dynamicAlloc 8
  let composite =
        statsAlloc
          tracker
          ( segregateAlloc
              512
              (fallbackAlloc (fixedAlloc fb) (recyclingAlloc rec dynamicAlloc))
              dynamicAlloc
          )

  -- Verify `new(0) -> realloc(0) -> free(0)` across all 5 layouts performs 0 allocations
  z1 <- exerciseEmptyNoAlloc @Int64 composite
  z2 <- exerciseEmptyNoAlloc @(AoS Particle) composite
  z3 <- exerciseEmptyNoAlloc @(SoA Particle) composite
  z4 <- exerciseEmptyNoAlloc @(AoSoA 8 Particle) composite
  z5 <- exerciseEmptyNoAlloc @(SOAOA 8 ParticleSys) composite
  snapZero <- readAllocSnapshot tracker
  let zeroOk =
        z1
          `land` z2
          `land` z3
          `land` z4
          `land` z5
          `land` (snapTotalAllocs snapZero `eq` 0)
          `land` (snapLiveBlocks snapZero `eq` 0)

  tinyTracker <- newAllocTracker
  tinyFb <- newFixedBuf dynamicAlloc 512
  let tinyPolicy = statsAlloc tinyTracker (fixedAlloc tinyFb)
      mkPart :: e Int64 -> Particle e
      mkPart v =
        let fv = toFloating v
         in Particle fv (fv + 1.0) (fv * 2.0) (fv - 3.0)
      projPart :: Particle e -> e Float
      projPart = pX
      mkSys :: e Int64 -> ParticleSys e
      mkSys v =
        let fv = toFloating v
         in ParticleSys (mkPart v) (ThermoGroup (fv + 5.0) (fv + 10.0))
      projSys :: ParticleSys e -> e Float
      projSys s = pX (psKin s)

  sScalar <- exerciseLayoutLifecycle @Int64 composite tinyPolicy id toFloating
  sAoS <- exerciseLayoutLifecycle @(AoS Particle) composite tinyPolicy mkPart projPart
  sSoA <- exerciseLayoutLifecycle @(SoA Particle) composite tinyPolicy mkPart projPart
  sAoSoA <- exerciseLayoutLifecycle @(AoSoA 8 Particle) composite tinyPolicy mkPart projPart
  sSOAOA <- exerciseLayoutLifecycle @(SOAOA 8 ParticleSys) composite tinyPolicy mkSys projSys

  snapMain <- readAllocSnapshot tracker
  snapTiny <- readAllocSnapshot tinyTracker

  destroyRecycler dynamicAlloc rec
  destroyFixedBuf dynamicAlloc fb
  destroyFixedBuf dynamicAlloc tinyFb
  freeAllocTracker tracker
  freeAllocTracker tinyTracker

  let leakPenalty =
        toFloating
          ( snapLiveBlocks snapMain * 1000000
              + snapLiveBytes snapMain * 1000
              + snapLiveBlocks snapTiny * 1000000
              + snapLiveBytes snapTiny * 1000
          )
      zeroPenalty = ifThenElse zeroOk 0.0 (-10000000.0)
  pure (sScalar + sAoS + sSoA + sAoSoA + sSOAOA + leakPenalty + zeroPenalty)

unit_normal_and_scoped_buf_new_realloc_free_all_layouts :: IO ()
unit_normal_and_scoped_buf_new_realloc_free_all_layouts = do
  cfg <- getLayoutTestConfig

  -- Also verify directly in IO that SoA negative-cap and OOM `allocBuf` / `reallocBuf`
  -- produce exact `nullBuf` across ALL column pointers (not only column 0)
  withFixedBuf 256 $ \tinyFb -> do
    let tinyAlloc = fixedAlloc tinyFb
        PtrStruct nullCols = nullBuf @(SoA Particle) (Proxy @IO)
    PtrStruct negCols <- allocBuf @(SoA Particle) dynamicAlloc (-10)
    PtrStruct oomCols <- allocBuf @(SoA Particle) tinyAlloc 100000
    b4 <- allocBuf @(SoA Particle) tinyAlloc 4
    PtrStruct oomReCols <- reallocBuf @(SoA Particle) tinyAlloc b4 4 100000 4
    freeBuf @(SoA Particle) tinyAlloc b4 4
    let colsList c = [getPtrOf (pX c), getPtrOf (pY c), getPtrOf (pVx c), getPtrOf (pVy c)]
    colsList negCols @?= colsList nullCols
    colsList oomCols @?= colsList nullCols
    colsList oomReCols @?= colsList nullCols

  Identity ioScore <- lifecycleAllLayoutsKernel @IO @Identity
  cScore <- withJanusC @LinuxX86_64 cfg (lifecycleAllLayoutsKernel @LinuxX86_64CM @LinuxX86_64C) id
  -- Each of the 5 layouts scores 257 (normal) + 416 (scoped) = 673 -> 5 * 673 = 3365
  ioScore @?= 3365.0
  cScore @?= 3365.0

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
