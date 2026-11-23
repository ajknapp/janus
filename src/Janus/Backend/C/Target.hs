{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Janus.Backend.C.Target
  ( -- * Target Admission Exceptions
    UnsupportedTargetException (..),

    -- * Open-World Dependent Target Feature Map Key
    TargetFeatureOf,
    TargetFeatureKey (..),
    targetFeatureKey,
    TargetFeatureMap,
    portableCFeatureKey,
    x86FeatureKey,
    cudaFeatureKey,

    -- * x86-64 Target Feature Lattice
    X86Feature (..),
    X86Features,
    x86FeatureBit,
    x86Feature,
    x86FeaturesFromList,
    x86FeaturesToList,
    x86FeaturesRawMask,
    x86FeaturesFromRawMask,
    hasX86Feature,
    withoutX86Feature,
    normalizeX86Features,
    admitsX86Features,
    ensureX86Admitted,
    x86CompilerFlags,

    -- * x86-64 Presets
    x86Baseline,
    x86_64_v1,
    x86SSE3,
    x86SSSE3,
    x86SSE4_1,
    x86SSE4_2,
    x86_64_v2,
    x86AVX,
    x86AVX2,
    x86_64_v3,

    -- * x86-64 Host Observation & Scoped FFI Dispatch
    observeHostX86Features,
    getActiveHostX86Features,
    withHostX86Features,
    observeSleefFlags,

    -- * CUDA Target Feature Lattice
    CUDAFeatures (..),
    cudaSM,
    cudaSMMajor,
    cudaSMMinor,
    cudaHasVideoSIMD,
    cudaHasDP4A,
    cudaHasDP2A,
    cudaHas128BitVecMem,
    admitsCUDAFeatures,
    ensureCUDAAdmitted,

    -- * CUDA Presets & Device Observation
    cudaSM50,
    cudaSM60,
    cudaSM61,
    cudaSM70,
    cudaSM75,
    cudaSM80,
    cudaSM86,
    cudaSM89,
    cudaSM90,
    observeCUDAFeatures,
    observeDefaultCUDAFeatures,
  )
where

import Control.Exception (Exception, SomeException, bracket, throwIO, try)
import Control.Monad (unless)
import Data.Bits (Bits (..))
import Data.Constraint (Dict (..))
import Data.Constraint.Compose (ComposeC)
import Data.Constraint.Extras (Has (argDict))
import Data.Dependent.HashMap (DHashMap)
import Data.Functor.Identity (Identity)
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Show (GShow (..), defaultGshowsPrec)
import Data.Hashable (Hashable (..))
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Some (Some (..))
import Data.Type.Equality ((:~:) (..))
import Data.Type.Ord (OrderingI (..))
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes, free, malloc)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, cmpSymbol, sameSymbol, symbolVal)
import Janus.CBits ()
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Janus.Backend.C.CUDA.Foreign
  ( CUdevice,
    pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR,
    pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR,
    cuDeviceGetAttribute,
    cuInit,
    withCuDevice,
  )

-- | Thrown when attempting to execute a kernel compiled or configured for
-- hardware features that the target device does not support.
data UnsupportedTargetException
  = UnsupportedX86Target
      { _unsupportedX86Requested :: X86Features,
        _unsupportedX86Observed :: X86Features
      }
  | UnsupportedCUDATarget
      { _unsupportedCUDARequested :: CUDAFeatures,
        _unsupportedCUDAObserved :: CUDAFeatures
      }
  deriving stock (Eq, Ord, Show)

instance Exception UnsupportedTargetException

--------------------------------------------------------------------------------
-- x86-64 Feature Lattice (through AVX2)
--------------------------------------------------------------------------------

data X86Feature
  = X86_SSE2
  | X86_SSE3
  | X86_SSSE3
  | X86_SSE4_1
  | X86_SSE4_2
  | X86_POPCNT
  | X86_PCLMUL
  | X86_AES
  | X86_AVX
  | X86_F16C
  | X86_FMA
  | X86_AVX2
  | X86_BMI1
  | X86_BMI2
  | X86_LZCNT
  | X86_MOVBE
  deriving stock (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance Hashable X86Feature

x86FeatureBit :: X86Feature -> Word64
x86FeatureBit f = bit (fromEnum f)

-- | Downward-closed set of x86-64 ISA extensions above the architectural SSE2
-- baseline. Forms a distributive bounded lattice under '(\/)' and '(/\)'.
newtype X86Features = X86Features {unX86Features :: Word64}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance Show X86Features where
  showsPrec d fs =
    showParen (d > 10) $
      showString "x86FeaturesFromList " . shows (x86FeaturesToList fs)

allKnownMask :: Word64
allKnownMask =
  foldr (\f acc -> acc .|. x86FeatureBit f) 0 [minBound .. maxBound :: X86Feature]

-- | Upward implication closure: enabling a higher ISA implies its prerequisites
-- (e.g. AVX2 -> AVX -> SSE4.2 -> SSE4.1 -> SSSE3 -> SSE3 -> SSE2).
normalizeX86Features :: Word64 -> X86Features
normalizeX86Features raw0 = X86Features (go (raw0 .&. allKnownMask .|. x86FeatureBit X86_SSE2))
  where
    step m =
      let m1 = if testBit m (fromEnum X86_AVX2) then m .|. x86FeatureBit X86_AVX else m
          m2 = if testBit m1 (fromEnum X86_FMA) then m1 .|. x86FeatureBit X86_AVX else m1
          m3 = if testBit m2 (fromEnum X86_F16C) then m2 .|. x86FeatureBit X86_AVX else m2
          m4 = if testBit m3 (fromEnum X86_AVX) then m3 .|. x86FeatureBit X86_SSE4_2 else m3
          m5 = if testBit m4 (fromEnum X86_SSE4_2) then m4 .|. x86FeatureBit X86_SSE4_1 else m4
          m6 = if testBit m5 (fromEnum X86_SSE4_1) then m5 .|. x86FeatureBit X86_SSSE3 else m5
          m7 = if testBit m6 (fromEnum X86_SSSE3) then m6 .|. x86FeatureBit X86_SSE3 else m6
          m8 = m7 .|. x86FeatureBit X86_SSE2
       in m8
    go m =
      let m' = step m
       in if m' == m then m else go m'

-- | Downward stripping closure: disabling a prerequisite also disables any
-- feature that requires it.
stripDependents :: Word64 -> X86Features
stripDependents raw0 = X86Features (go ((raw0 .&. allKnownMask) .|. x86FeatureBit X86_SSE2))
  where
    has m f = testBit m (fromEnum f)
    clr m f = clearBit m (fromEnum f)
    step m =
      let m1 = if not (has m X86_SSE3) then clr m X86_SSSE3 else m
          m2 = if not (has m1 X86_SSSE3) then clr m1 X86_SSE4_1 else m1
          m3 = if not (has m2 X86_SSE4_1) then clr m2 X86_SSE4_2 else m2
          m4 = if not (has m3 X86_SSE4_2) then clr m3 X86_AVX else m3
          m5 =
            if not (has m4 X86_AVX)
              then clr (clr (clr m4 X86_AVX2) X86_FMA) X86_F16C
              else m4
       in m5
    go m =
      let m' = step m
       in if m' == m then m else go m'

x86FeaturesRawMask :: X86Features -> Word64
x86FeaturesRawMask = unX86Features

x86FeaturesFromRawMask :: Word64 -> X86Features
x86FeaturesFromRawMask = stripDependents

x86Feature :: X86Feature -> X86Features
x86Feature = normalizeX86Features . x86FeatureBit

x86FeaturesFromList :: [X86Feature] -> X86Features
x86FeaturesFromList = normalizeX86Features . foldr (\f acc -> acc .|. x86FeatureBit f) 0

x86FeaturesToList :: X86Features -> [X86Feature]
x86FeaturesToList fs = filter (hasX86Feature fs) [minBound .. maxBound]

hasX86Feature :: X86Features -> X86Feature -> Bool
hasX86Feature (X86Features m) f = (m .&. x86FeatureBit f) /= 0

withoutX86Feature :: X86Feature -> X86Features -> X86Features
withoutX86Feature X86_SSE2 fs = fs
withoutX86Feature f (X86Features m) = stripDependents (clearBit m (fromEnum f))

instance Semigroup X86Features where
  X86Features a <> X86Features b = normalizeX86Features (a .|. b)

instance Monoid X86Features where
  mempty = x86Baseline

instance Join X86Features where
  (\/) = (<>)

instance Meet X86Features where
  X86Features a /\ X86Features b = stripDependents (a .&. b)

instance Lower X86Features where
  lowerBound = x86Baseline

instance Upper X86Features where
  upperBound = X86Features allKnownMask

-- | @admitsX86Features observed required@ holds iff @observed@ supports every
-- feature in @required@ (analogous to @ekmett/native@'s @classify_isa(...).admitted()@).
admitsX86Features :: X86Features -> X86Features -> Bool
admitsX86Features (X86Features obs) (X86Features req) = (obs .&. req) == req

ensureX86Admitted :: X86Features -> X86Features -> IO ()
ensureX86Admitted observed requested =
  unless (admitsX86Features observed requested) $
    throwIO (UnsupportedX86Target requested observed)

x86Baseline, x86_64_v1, x86SSE3, x86SSSE3, x86SSE4_1, x86SSE4_2, x86_64_v2, x86AVX, x86AVX2, x86_64_v3 :: X86Features
x86Baseline = x86Feature X86_SSE2
x86_64_v1 = x86Baseline
x86SSE3 = x86Feature X86_SSE3
x86SSSE3 = x86Feature X86_SSSE3
x86SSE4_1 = x86Feature X86_SSE4_1
x86SSE4_2 = x86Feature X86_SSE4_2
x86_64_v2 = x86FeaturesFromList [X86_SSE4_2, X86_POPCNT]
x86AVX = x86FeaturesFromList [X86_AVX, X86_POPCNT]
x86AVX2 =
  x86FeaturesFromList
    [ X86_AVX2,
      X86_FMA,
      X86_F16C,
      X86_BMI1,
      X86_BMI2,
      X86_LZCNT,
      X86_MOVBE,
      X86_POPCNT
    ]
x86_64_v3 = x86AVX2

-- | Exact GCC/Clang target flags for compiling a translation unit strictly to
-- the requested 'X86Features' level without leaking higher host instructions.
x86CompilerFlags :: X86Features -> [String]
x86CompilerFlags fs =
  enabledFlags <> disabledFlags <> ["-mno-avx512f"]
  where
    flagPair = \case
      X86_SSE2 -> ("-msse2", "-mno-sse2")
      X86_SSE3 -> ("-msse3", "-mno-sse3")
      X86_SSSE3 -> ("-mssse3", "-mno-ssse3")
      X86_SSE4_1 -> ("-msse4.1", "-mno-sse4.1")
      X86_SSE4_2 -> ("-msse4.2", "-mno-sse4.2")
      X86_POPCNT -> ("-mpopcnt", "-mno-popcnt")
      X86_PCLMUL -> ("-mpclmul", "-mno-pclmul")
      X86_AES -> ("-maes", "-mno-aes")
      X86_AVX -> ("-mavx", "-mno-avx")
      X86_F16C -> ("-mf16c", "-mno-f16c")
      X86_FMA -> ("-mfma", "-mno-fma")
      X86_AVX2 -> ("-mavx2", "-mno-avx2")
      X86_BMI1 -> ("-mbmi", "-mno-bmi")
      X86_BMI2 -> ("-mbmi2", "-mno-bmi2")
      X86_LZCNT -> ("-mlzcnt", "-mno-lzcnt")
      X86_MOVBE -> ("-mmovbe", "-mno-movbe")
    allFeats = [minBound .. maxBound :: X86Feature]
    enabledFlags = [fst (flagPair f) | f <- allFeats, hasX86Feature fs f]
    disabledFlags = [snd (flagPair f) | f <- allFeats, not (hasX86Feature fs f)]

foreign import ccall unsafe "janus_query_x86_features"
  c_janus_query_x86_features :: IO Word64

foreign import ccall unsafe "janus_get_x86_features"
  c_janus_get_x86_features :: IO Word64

foreign import ccall unsafe "janus_set_x86_features"
  c_janus_set_x86_features :: Word64 -> IO ()

foreign import ccall unsafe "janus_query_sleef_libdir"
  c_janus_query_sleef_libdir :: CString -> CInt -> IO CInt

-- | Discover compiler (@-I...@) and linker (@-L...@, @-Wl,-rpath,...@, @-lsleef@)
-- flags for linking SLEEF vector math into runtime-compiled Linux x86-64 shared
-- objects, using @SLEEF_PATH@ when set or resolving the loaded @libsleef.so@ via
-- @dladdr@.
observeSleefFlags :: IO ([String], [String])
observeSleefFlags = do
  mEnv <- lookupEnv "SLEEF_PATH"
  case mEnv of
    Just prefix | not (null prefix) -> flagsFromPrefix prefix
    _ -> do
      mLibDir <- allocaBytes 4096 $ \buf -> do
        n <- c_janus_query_sleef_libdir buf 4096
        if n > 0
          then Just <$> peekCString buf
          else pure Nothing
      case mLibDir of
        Just libDir -> do
          let prefix = takeDirectory libDir
              incDir = prefix </> "include"
          hasInc <- doesDirectoryExist incDir
          let cflags = ["-I" <> incDir | hasInc]
              ldflags = ["-L" <> libDir, "-Wl,-rpath," <> libDir, "-lsleef"]
          pure (cflags, ldflags)
        Nothing -> pure ([], ["-lsleef"])
  where
    flagsFromPrefix prefix = do
      let incDir = prefix </> "include"
          libDir = prefix </> "lib"
      hasInc <- doesDirectoryExist incDir
      hasLib <- doesDirectoryExist libDir
      let cflags = ["-I" <> incDir | hasInc]
          ldflags =
            (if hasLib then ["-L" <> libDir, "-Wl,-rpath," <> libDir] else [])
              <> ["-lsleef"]
      pure (cflags, ldflags)

-- | Observe hardware & OS vector-save capabilities of the host x86-64 CPU.
observeHostX86Features :: IO X86Features
observeHostX86Features = x86FeaturesFromRawMask <$> c_janus_query_x86_features

-- | Read the currently active thread-local x86-64 feature dispatch mask used by
-- 'Identity' / 'IO' FFI operations.
getActiveHostX86Features :: IO X86Features
getActiveHostX86Features = x86FeaturesFromRawMask <$> c_janus_get_x86_features

-- | Scope the active thread-local x86-64 FFI dispatch mask to an explicit
-- 'X86Features' set after validating hardware admission.
withHostX86Features :: X86Features -> IO a -> IO a
withHostX86Features requested action = do
  observed <- observeHostX86Features
  ensureX86Admitted observed requested
  prev <- c_janus_get_x86_features
  bracket
    (c_janus_set_x86_features (x86FeaturesRawMask requested))
    (\_ -> c_janus_set_x86_features prev)
    (\_ -> action)

--------------------------------------------------------------------------------
-- CUDA Feature Lattice
--------------------------------------------------------------------------------

data CUDAFeatures = CUDAFeatures
  { _cudaSMMajor :: !Int,
    _cudaSMMinor :: !Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

instance Hashable CUDAFeatures

cudaSM :: Int -> Int -> CUDAFeatures
cudaSM = CUDAFeatures

cudaSMMajor :: CUDAFeatures -> Int
cudaSMMajor = _cudaSMMajor

cudaSMMinor :: CUDAFeatures -> Int
cudaSMMinor = _cudaSMMinor

instance Semigroup CUDAFeatures where
  (<>)= max

instance Monoid CUDAFeatures where
  mempty = cudaSM50

instance Join CUDAFeatures where
  (\/) = max

instance Meet CUDAFeatures where
  (/\) = min

instance Lower CUDAFeatures where
  lowerBound = cudaSM50

cudaHasVideoSIMD :: CUDAFeatures -> Bool
cudaHasVideoSIMD (CUDAFeatures maj _) = maj >= 3

cudaHasDP4A :: CUDAFeatures -> Bool
cudaHasDP4A f = f >= cudaSM 6 1

cudaHasDP2A :: CUDAFeatures -> Bool
cudaHasDP2A f = f >= cudaSM 6 1

cudaHas128BitVecMem :: CUDAFeatures -> Bool
cudaHas128BitVecMem (CUDAFeatures maj _) = maj >= 3

admitsCUDAFeatures :: CUDAFeatures -> CUDAFeatures -> Bool
admitsCUDAFeatures observed requested = observed >= requested

ensureCUDAAdmitted :: CUDAFeatures -> CUDAFeatures -> IO ()
ensureCUDAAdmitted observed requested =
  unless (admitsCUDAFeatures observed requested) $
    throwIO (UnsupportedCUDATarget requested observed)

cudaSM50, cudaSM60, cudaSM61, cudaSM70, cudaSM75, cudaSM80, cudaSM86, cudaSM89, cudaSM90 :: CUDAFeatures
cudaSM50 = CUDAFeatures 5 0
cudaSM60 = CUDAFeatures 6 0
cudaSM61 = CUDAFeatures 6 1
cudaSM70 = CUDAFeatures 7 0
cudaSM75 = CUDAFeatures 7 5
cudaSM80 = CUDAFeatures 8 0
cudaSM86 = CUDAFeatures 8 6
cudaSM89 = CUDAFeatures 8 9
cudaSM90 = CUDAFeatures 9 0

-- | Query compute capability features directly from a 'CUdevice'.
observeCUDAFeatures :: CUdevice -> IO CUDAFeatures
observeCUDAFeatures dev = bracket malloc free $ \ip -> do
  major <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR dev >> peek ip
  minor <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR dev >> peek ip
  pure (CUDAFeatures (fromIntegral major) (fromIntegral minor))

-- | Observe CUDA device 0 features if a CUDA driver/device is available on this
-- host, falling back to 'cudaSM80' for headless CPU emulation.
observeDefaultCUDAFeatures :: IO CUDAFeatures
observeDefaultCUDAFeatures = do
  res <- try (cuInit 0 >> withCuDevice 0 observeCUDAFeatures)
  case res :: Either SomeException CUDAFeatures of
    Right fs -> pure fs
    Left _ -> pure cudaSM80

--------------------------------------------------------------------------------
-- Open-World Dependent Target Feature Map Key
--------------------------------------------------------------------------------

type family TargetFeatureOf (tag :: Symbol) :: K.Type

data TargetFeatureKey (f :: K.Type) where
  TargetFeatureKey ::
    (KnownSymbol tag, Eq f, Ord f, Show f, f ~ TargetFeatureOf tag) =>
    Proxy tag ->
    TargetFeatureKey f

targetFeatureKey ::
  forall tag f.
  (KnownSymbol tag, Eq f, Ord f, Show f, f ~ TargetFeatureOf tag) =>
  TargetFeatureKey f
targetFeatureKey = TargetFeatureKey (Proxy @tag)

instance Eq (TargetFeatureKey f) where
  TargetFeatureKey p1 == TargetFeatureKey p2 = symbolVal p1 == symbolVal p2

instance Ord (TargetFeatureKey f) where
  compare (TargetFeatureKey p1) (TargetFeatureKey p2) =
    compare (symbolVal p1) (symbolVal p2)

instance Show (TargetFeatureKey f) where
  showsPrec d (TargetFeatureKey p) =
    showParen (d > 10) $ showString "TargetFeatureKey @" . shows (symbolVal p)

instance GShow TargetFeatureKey where
  gshowsPrec = defaultGshowsPrec

instance GEq TargetFeatureKey where
  geq (TargetFeatureKey p1) (TargetFeatureKey p2) =
    case sameSymbol p1 p2 of
      Just Refl -> Just Refl
      Nothing -> Nothing

instance GCompare TargetFeatureKey where
  gcompare (TargetFeatureKey p1) (TargetFeatureKey p2) =
    case cmpSymbol p1 p2 of
      LTI -> GLT
      EQI -> GEQ
      GTI -> GGT

instance Hashable (TargetFeatureKey f) where
  hashWithSalt s (TargetFeatureKey p) = hashWithSalt s (symbolVal p)

instance Hashable (Some TargetFeatureKey) where
  hashWithSalt s (Some k) = hashWithSalt s k

instance Has (ComposeC Eq Identity) TargetFeatureKey where
  argDict (TargetFeatureKey _) = Dict

instance Has (ComposeC Ord Identity) TargetFeatureKey where
  argDict (TargetFeatureKey _) = Dict

instance Has (ComposeC Show Identity) TargetFeatureKey where
  argDict (TargetFeatureKey _) = Dict

type TargetFeatureMap = DHashMap TargetFeatureKey Identity

type instance TargetFeatureOf "portable-c" = ()

type instance TargetFeatureOf "linux-x86_64" = X86Features

type instance TargetFeatureOf "cuda" = CUDAFeatures

portableCFeatureKey :: TargetFeatureKey ()
portableCFeatureKey = targetFeatureKey @"portable-c"

x86FeatureKey :: TargetFeatureKey X86Features
x86FeatureKey = targetFeatureKey @"linux-x86_64"

cudaFeatureKey :: TargetFeatureKey CUDAFeatures
cudaFeatureKey = targetFeatureKey @"cuda"
