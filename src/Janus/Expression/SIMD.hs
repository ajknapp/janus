{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Expression.SIMD
  ( -- * Target Feature Lattices & Device Queries (re-exported)
    module Janus.Backend.C.Target,

    -- * Shapes, Scalar Metadata, & Wide Unrolling
    SIMDElem (..),
    CPUSIMDElem,
    SIMDShape (..),
    RawSIMD (..),
    RawMask (..),
    Wide (..),
    wideReplicate,
    wideGenerate,
    wideFromList,
    wideToList,
    wideIndex,
    wideTreeReduce,

    -- * Root SIMD Typeclass & Associated Vector / Mask Types
    ExpSIMD (..),
    HostSIMDValue (..),
    HostSIMDMask (..),

    -- * Thematic SIMD Expression Typeclasses
    ExpSIMDVec (..),
    ExpSIMDMask (..),
    ExpSIMDCmp (..),
    ExpSIMDArith (..),
    ExpSIMDSat (..),
    ExpSIMDFloat (..),
    ExpSIMDBits (..),
    ExpSIMDShuffle (..),
    SIMDHalfPair,
    ExpSIMDSplit (..),
    ExpSIMDReduce (..),
    SIMDDp4aShape,
    ExpSIMDDot (..),
    SIMDPackShape,
    ExpSIMDCast (..),

    -- * Thematic SIMD Memory Command Typeclass
    CmdSIMDMem (..),

    -- * Multi-Register Wide Combinators
    wload,
    wstore,
    wsplat,
    wzero,
    wadd,
    wsub,
    wmul,
    wfma,
    wreduceVec,
  )
where

import Control.Monad (forM_)
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable)
import GHC.Float (expm1, log1p)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Janus.Backend.C.Target
import Janus.Expression.Math (FMA (..), Hypot (..))
import Janus.Expression.SIMD.CPU (CPUSIMDElem)
import Janus.Expression.SIMD.FFI
import Janus.Expression.SIMD.Types

--------------------------------------------------------------------------------
-- Root SIMD Typeclass & Host Conversion Helpers
--------------------------------------------------------------------------------

-- | Root typeclass introducing backend-specialized associated SIMD vector and
-- mask data types. Every thematic SIMD typeclass operates over
-- @e (SIMDVector e n a)@ and @e (SIMDMask e n a)@.
class ExpSIMD (e :: K.Type -> K.Type) where
  data SIMDVector e (n :: Nat) a
  data SIMDMask e (n :: Nat) a

class (SIMDShape n a) => HostSIMDValue v n a | v -> n a where
  simdFromList :: [a] -> v
  simdToList :: v -> [a]

class (SIMDShape n a) => HostSIMDMask m n a | m -> n a where
  simdMaskFromBools :: [Bool] -> m
  simdMaskToBools :: m -> [Bool]

--------------------------------------------------------------------------------
-- Category 1: Construction, Broadcast, & Lane Access
--------------------------------------------------------------------------------

class (ExpSIMD e, SIMDShape n a) => ExpSIMDVec e n a where
  vzero :: e (SIMDVector e n a)
  vsplat :: e a -> e (SIMDVector e n a)
  viota :: e (SIMDVector e n a)
  vfromLanes :: [e a] -> e (SIMDVector e n a)
  vextractLane :: (KnownNat i) => Proxy i -> e (SIMDVector e n a) -> e a
  vinsertLane :: (KnownNat i) => Proxy i -> e a -> e (SIMDVector e n a) -> e (SIMDVector e n a)

--------------------------------------------------------------------------------
-- Category 2: Masks, Conditional Blend, & Horizontal Mask Queries
--------------------------------------------------------------------------------

class (ExpSIMD e, SIMDShape n a) => ExpSIMDMask e n a where
  mfalse :: e (SIMDMask e n a)
  mtrue :: e (SIMDMask e n a)
  mand :: e (SIMDMask e n a) -> e (SIMDMask e n a) -> e (SIMDMask e n a)
  mor :: e (SIMDMask e n a) -> e (SIMDMask e n a) -> e (SIMDMask e n a)
  mxor :: e (SIMDMask e n a) -> e (SIMDMask e n a) -> e (SIMDMask e n a)
  mandnot :: e (SIMDMask e n a) -> e (SIMDMask e n a) -> e (SIMDMask e n a)
  mnot :: e (SIMDMask e n a) -> e (SIMDMask e n a)
  vselect :: e (SIMDMask e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  mmovemask :: e (SIMDMask e n a) -> e Word32
  mpopcount :: e (SIMDMask e n a) -> e Int32
  mfirstTrue :: e (SIMDMask e n a) -> e Int32
  mallOf :: e (SIMDMask e n a) -> e Bool
  manyOf :: e (SIMDMask e n a) -> e Bool
  mnoneOf :: e (SIMDMask e n a) -> e Bool

infixr 3 `mand`
infixr 2 `mor`

--------------------------------------------------------------------------------
-- Category 3: Relational & Equality Comparisons
--------------------------------------------------------------------------------

class (ExpSIMDMask e n a) => ExpSIMDCmp e n a where
  veq :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)
  vne :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)
  vlt :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)
  vle :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)
  vgt :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)
  vge :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)

infix 4 `veq`, `vne`, `vlt`, `vle`, `vgt`, `vge`

--------------------------------------------------------------------------------
-- Category 4: Standard Lane-Wise Arithmetic & Extrema
--------------------------------------------------------------------------------

class (ExpSIMDVec e n a) => ExpSIMDArith e n a where
  vadd :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vsub :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vmul :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vneg :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vabs :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vmin :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vmax :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)

infixl 6 `vadd`, `vsub`
infixl 7 `vmul`

--------------------------------------------------------------------------------
-- Category 5: Saturating Arithmetic, Rounding Average, & Absolute Difference
--------------------------------------------------------------------------------

class (ExpSIMDArith e n a) => ExpSIMDSat e n a where
  vadds :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vsubs :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vabss :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vavg :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vabsdiff :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)

--------------------------------------------------------------------------------
-- Category 6: Floating-Point Math, FMA, & IEEE Rounding
--------------------------------------------------------------------------------

class (ExpSIMDArith e n a) => ExpSIMDFloat e n a where
  vdiv :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vsqrt :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vrsqrtApprox :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vrcpApprox :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vfma :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vfms :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vfnma :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vfnms :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vmulAdd :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vfloor :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vceil :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vtrunc :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vroundNearest :: e (SIMDVector e n a) -> e (SIMDVector e n a)

infixl 7 `vdiv`

--------------------------------------------------------------------------------
-- Category 7: Bitwise Logic, Shifts (Uniform, Immediate, Variable), & Popcount
--------------------------------------------------------------------------------

class (ExpSIMDVec e n a) => ExpSIMDBits e n a where
  vand :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vor :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vxor :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vandnot :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vnot :: e (SIMDVector e n a) -> e (SIMDVector e n a)
  vshl :: e (SIMDVector e n a) -> e Int32 -> e (SIMDVector e n a)
  vshr :: e (SIMDVector e n a) -> e Int32 -> e (SIMDVector e n a)
  vshlImm :: (KnownNat k) => Proxy k -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vshrImm :: (KnownNat k) => Proxy k -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vshlVar :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vshrVar :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vpopcnt :: e (SIMDVector e n a) -> e (SIMDVector e n a)

infixl 8 `vshl`, `vshr`, `vshlVar`, `vshrVar`
infixl 7 `vand`, `vor`
infixl 6 `vxor`

--------------------------------------------------------------------------------
-- Category 8: Permutations, Byte Shuffles, Zip, & Split/Concat
--------------------------------------------------------------------------------

class (ExpSIMDVec e n a) => ExpSIMDShuffle e n a where
  vshuffleLanes :: [Int] -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vpermuteVar :: (SIMDShape n Int32) => e (SIMDVector e n a) -> e (SIMDVector e n Int32) -> e (SIMDVector e n a)
  vshuffleBytes :: (SIMDShape b Word8) => e (SIMDVector e n a) -> e (SIMDVector e b Word8) -> e (SIMDVector e n a)
  vzipLo :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)
  vzipHi :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)

class (SIMDShape n2 a, SIMDShape n a) => SIMDHalfPair (n2 :: Nat) (n :: Nat) a | n2 -> n, n -> n2

instance (SIMDElem a, SIMDShape 4 a, SIMDShape 2 a) => SIMDHalfPair 4 2 a
instance (SIMDElem a, SIMDShape 8 a, SIMDShape 4 a) => SIMDHalfPair 8 4 a
instance (SIMDElem a, SIMDShape 16 a, SIMDShape 8 a) => SIMDHalfPair 16 8 a
instance (SIMDElem a, SIMDShape 32 a, SIMDShape 16 a) => SIMDHalfPair 32 16 a

class (ExpSIMD e, SIMDHalfPair n2 n a) => ExpSIMDSplit e n2 n a where
  vsplit :: e (SIMDVector e n2 a) -> (e (SIMDVector e n a), e (SIMDVector e n a))
  vconcat :: e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n2 a)

--------------------------------------------------------------------------------
-- Category 9: Horizontal Reductions
--------------------------------------------------------------------------------

class (ExpSIMDVec e n a) => ExpSIMDReduce e n a where
  vreduceAdd :: e (SIMDVector e n a) -> e a
  vreduceMin :: e (SIMDVector e n a) -> e a
  vreduceMax :: e (SIMDVector e n a) -> e a
  vreduceAnd :: e (SIMDVector e n a) -> e a
  vreduceOr :: e (SIMDVector e n a) -> e a
  vreduceXor :: e (SIMDVector e n a) -> e a

--------------------------------------------------------------------------------
-- Category 10: Dot Products (dp4a, dp2a) & Sum of Absolute Differences (SAD)
--------------------------------------------------------------------------------

class (SIMDShape m Int8, SIMDShape m Word8, SIMDShape k Int32, SIMDShape k Word32) => SIMDDp4aShape (m :: Nat) (k :: Nat) | m -> k, k -> m

instance SIMDDp4aShape 8 2
instance SIMDDp4aShape 16 4
instance SIMDDp4aShape 32 8

class (ExpSIMD e) => ExpSIMDDot e where
  vdp4aS :: e (SIMDVector e 4 Int8) -> e (SIMDVector e 4 Int8) -> e Int32 -> e Int32
  vdp4aU :: e (SIMDVector e 4 Word8) -> e (SIMDVector e 4 Word8) -> e Word32 -> e Word32
  vdp4aVecS :: (SIMDDp4aShape m k) => e (SIMDVector e m Int8) -> e (SIMDVector e m Int8) -> e (SIMDVector e k Int32) -> e (SIMDVector e k Int32)
  vdp4aVecU :: (SIMDDp4aShape m k) => e (SIMDVector e m Word8) -> e (SIMDVector e m Word8) -> e (SIMDVector e k Word32) -> e (SIMDVector e k Word32)
  vdp2aLoS :: e (SIMDVector e 2 Int16) -> e (SIMDVector e 4 Int8) -> e Int32 -> e Int32
  vdp2aHiS :: e (SIMDVector e 2 Int16) -> e (SIMDVector e 4 Int8) -> e Int32 -> e Int32
  vdp2aLoU :: e (SIMDVector e 2 Word16) -> e (SIMDVector e 4 Word8) -> e Word32 -> e Word32
  vdp2aHiU :: e (SIMDVector e 2 Word16) -> e (SIMDVector e 4 Word8) -> e Word32 -> e Word32
  vsad8 :: (SIMDShape n Word8) => e (SIMDVector e n Word8) -> e (SIMDVector e n Word8) -> e Word32

--------------------------------------------------------------------------------
-- Category 11: Bitcasts, Numeric Conversions, & Saturating Narrowing
--------------------------------------------------------------------------------

class (SIMDShape n a, SIMDShape m b, CPUSIMDElem a, CPUSIMDElem b) => SIMDPackShape (n :: Nat) (m :: Nat) a b | n a b -> m

instance SIMDPackShape 2 4 Int16 Int8
instance SIMDPackShape 2 4 Int16 Word8
instance SIMDPackShape 2 4 Word16 Word8
instance SIMDPackShape 4 8 Int16 Int8
instance SIMDPackShape 4 8 Int16 Word8
instance SIMDPackShape 4 8 Word16 Word8
instance SIMDPackShape 8 16 Int16 Int8
instance SIMDPackShape 8 16 Int16 Word8
instance SIMDPackShape 8 16 Word16 Word8
instance SIMDPackShape 16 32 Int16 Int8
instance SIMDPackShape 16 32 Int16 Word8
instance SIMDPackShape 16 32 Word16 Word8
instance SIMDPackShape 2 4 Int32 Int16
instance SIMDPackShape 2 4 Int32 Word16
instance SIMDPackShape 2 4 Word32 Word16
instance SIMDPackShape 4 8 Int32 Int16
instance SIMDPackShape 4 8 Int32 Word16
instance SIMDPackShape 4 8 Word32 Word16
instance SIMDPackShape 8 16 Int32 Int16
instance SIMDPackShape 8 16 Int32 Word16
instance SIMDPackShape 8 16 Word32 Word16

class (ExpSIMD e) => ExpSIMDCast e where
  vbitcast :: (SIMDShape n a, SIMDShape m b) => e (SIMDVector e n a) -> e (SIMDVector e m b)
  vcvt :: (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) => e (SIMDVector e n a) -> e (SIMDVector e n b)
  vnarrowSat :: (SIMDPackShape n m a b) => e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e m b)

--------------------------------------------------------------------------------
-- Category 12: Aligned, Unaligned, & Masked Memory + Gather
--------------------------------------------------------------------------------

class (Monad m, ExpSIMD e, SIMDShape n a) => CmdSIMDMem m e n a | m -> e, e -> m where
  vload :: e (Ptr a) -> m (e (SIMDVector e n a))
  vloadAligned :: e (Ptr a) -> m (e (SIMDVector e n a))
  vstore :: e (Ptr a) -> e (SIMDVector e n a) -> m ()
  vstoreAligned :: e (Ptr a) -> e (SIMDVector e n a) -> m ()
  vloadMasked :: e (SIMDMask e n a) -> e (SIMDVector e n a) -> e (Ptr a) -> m (e (SIMDVector e n a))
  vstoreMasked :: e (SIMDMask e n a) -> e (Ptr a) -> e (SIMDVector e n a) -> m ()
  vgather :: (SIMDShape n Int32) => e (Ptr a) -> e (SIMDVector e n Int32) -> m (e (SIMDVector e n a))
  vgatherMasked ::
    (SIMDShape n Int32) =>
    e (SIMDMask e n a) ->
    e (SIMDVector e n a) ->
    e (Ptr a) ->
    e (SIMDVector e n Int32) ->
    m (e (SIMDVector e n a))

--------------------------------------------------------------------------------
-- Identity / IO Instances (C Compiler Intrinsics via FFI)
--------------------------------------------------------------------------------

instance ExpSIMD Identity where
  newtype SIMDVector Identity n a = IdentitySIMDVector {getIdentitySIMDVector :: RawSIMD n a}
    deriving stock (Eq, Ord)
  newtype SIMDMask Identity n a = IdentitySIMDMask {getIdentitySIMDMask :: RawMask n a}
    deriving stock (Eq, Ord)

deriving newtype instance (SIMDShape n a) => Show (SIMDVector Identity n a)
deriving newtype instance (SIMDShape n a) => Show (SIMDMask Identity n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDVector Identity n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask Identity n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector Identity n a) n a where
  simdFromList = IdentitySIMDVector . rawSIMDFromList
  simdToList = rawSIMDToList . getIdentitySIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask Identity n a) n a where
  simdMaskFromBools = IdentitySIMDMask . rawMaskFromBools
  simdMaskToBools = rawMaskToBools . getIdentitySIMDMask

instance (SIMDShape n a, Num a) => Num (SIMDVector Identity n a) where
  IdentitySIMDVector a + IdentitySIMDVector b = IdentitySIMDVector (ffiBinOp opAdd a b)
  IdentitySIMDVector a - IdentitySIMDVector b = IdentitySIMDVector (ffiBinOp opSub a b)
  IdentitySIMDVector a * IdentitySIMDVector b = IdentitySIMDVector (ffiBinOp opMul a b)
  negate (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopNeg a)
  abs (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAbs a)
  signum (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopSignum a)
  fromInteger k = IdentitySIMDVector (ffiSplat (fromInteger k :: a))

instance (SIMDShape n a, Fractional a) => Fractional (SIMDVector Identity n a) where
  IdentitySIMDVector a / IdentitySIMDVector b = IdentitySIMDVector (ffiBinOp opDiv a b)
  recip x = fromRational 1 / x
  fromRational q = IdentitySIMDVector (ffiSplat (fromRational q :: a))

instance (SIMDShape n a, Floating a) => Floating (SIMDVector Identity n a) where
  pi = IdentitySIMDVector (ffiSplat (pi :: a))
  exp (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopExp a)
  log (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopLog a)
  sqrt (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopSqrt a)
  IdentitySIMDVector x ** IdentitySIMDVector y = IdentitySIMDVector (ffiBinOp opPow x y)
  logBase x y = log y / log x
  sin (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopSin a)
  cos (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopCos a)
  tan (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopTan a)
  asin (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAsin a)
  acos (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAcos a)
  atan (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAtan a)
  sinh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopSinh a)
  cosh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopCosh a)
  tanh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopTanh a)
  asinh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAsinh a)
  acosh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAcosh a)
  atanh (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopAtanh a)
  log1p (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopLog1p a)
  expm1 (IdentitySIMDVector a) = IdentitySIMDVector (ffiUnOp unopExpm1 a)

instance (SIMDShape n a, Floating a) => Hypot (SIMDVector Identity n a) where
  hypot (IdentitySIMDVector a) (IdentitySIMDVector b) =
    IdentitySIMDVector (ffiBinOp opHypot a b)

instance (SIMDShape n a, FMA a) => FMA (SIMDVector Identity n a) where
  fma (IdentitySIMDVector a) (IdentitySIMDVector b) (IdentitySIMDVector c) =
    IdentitySIMDVector (ffiTernOp ternopFma a b c)

instance (SIMDShape n a) => Join (SIMDMask Identity n a) where
  IdentitySIMDMask a \/ IdentitySIMDMask b = IdentitySIMDMask (ffiMaskOr a b)

instance (SIMDShape n a) => Meet (SIMDMask Identity n a) where
  IdentitySIMDMask a /\ IdentitySIMDMask b = IdentitySIMDMask (ffiMaskAnd a b)

instance (SIMDShape n a) => Lower (SIMDMask Identity n a) where
  lowerBound = IdentitySIMDMask (rawMaskFromBools (replicate (simdLanes (Proxy @n) (Proxy @a)) False))

instance (SIMDShape n a) => Upper (SIMDMask Identity n a) where
  upperBound = IdentitySIMDMask (rawMaskFromBools (replicate (simdLanes (Proxy @n) (Proxy @a)) True))

instance (SIMDShape n a, Num a) => ExpSIMDVec Identity n a where
  vzero = Identity (IdentitySIMDVector ffiZero)
  vsplat (Identity x) = Identity (IdentitySIMDVector (ffiSplat x))
  viota = Identity (IdentitySIMDVector ffiIota)
  vfromLanes xs = Identity (IdentitySIMDVector (rawSIMDFromList (map runIdentity xs)))
  vextractLane p (Identity (IdentitySIMDVector v)) =
    Identity (rawSIMDIndex (fromIntegral (natVal p)) v)
  vinsertLane p (Identity x) (Identity (IdentitySIMDVector v)) =
    Identity (IdentitySIMDVector (rawSIMDUpdate (fromIntegral (natVal p)) x v))

instance (SIMDShape n a) => ExpSIMDMask Identity n a where
  mfalse = Identity lowerBound
  mtrue = Identity upperBound
  mand = coerce (ffiMaskAnd @n @a)
  mor = coerce (ffiMaskOr @n @a)
  mxor = coerce (ffiMaskXor @n @a)
  mandnot = coerce (ffiMaskAndNot @n @a)
  mnot = coerce (ffiMaskNot @n @a)
  vselect = coerce (ffiSelect @n @a)
  mmovemask (Identity (IdentitySIMDMask m)) = Identity (ffiMovemask m)
  mpopcount (Identity (IdentitySIMDMask m)) = Identity (ffiMaskPopcount m)
  mfirstTrue (Identity (IdentitySIMDMask m)) = Identity (ffiMaskFirstTrue m)
  mallOf (Identity (IdentitySIMDMask m)) =
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        full = if lanes == 32 then maxBound else (1 `shiftL` lanes) - 1
     in Identity (ffiMovemask m == full)
  manyOf (Identity (IdentitySIMDMask m)) = Identity (ffiMovemask m /= 0)
  mnoneOf (Identity (IdentitySIMDMask m)) = Identity (ffiMovemask m == 0)

instance (SIMDShape n a) => ExpSIMDCmp Identity n a where
  veq = coerce (ffiCmp @n @a cmpEq)
  vne = coerce (ffiCmp @n @a cmpNe)
  vlt = coerce (ffiCmp @n @a cmpLt)
  vle = coerce (ffiCmp @n @a cmpLe)
  vgt = coerce (ffiCmp @n @a cmpGt)
  vge = coerce (ffiCmp @n @a cmpGe)

instance (SIMDShape n a, Num a) => ExpSIMDArith Identity n a where
  vadd = (+)
  vsub = (-)
  vmul = (*)
  vneg = negate
  vabs = abs
  vmin = coerce (ffiBinOp @n @a opMin)
  vmax = coerce (ffiBinOp @n @a opMax)

instance (SIMDShape n a, Num a) => ExpSIMDSat Identity n a where
  vadds = coerce (ffiBinOp @n @a opAdds)
  vsubs = coerce (ffiBinOp @n @a opSubs)
  vabss = coerce (ffiUnOp @n @a unopAbss)
  vavg = coerce (ffiBinOp @n @a opAvg)
  vabsdiff = coerce (ffiBinOp @n @a opAbsDiff)

instance (SIMDShape n a, Floating a, FMA a) => ExpSIMDFloat Identity n a where
  vdiv = (/)
  vsqrt = sqrt
  vrsqrtApprox = coerce (ffiUnOp @n @a unopRsqrt)
  vrcpApprox = coerce (ffiUnOp @n @a unopRcp)
  vfma = fma
  vfms = coerce (ffiTernOp @n @a ternopFms)
  vfnma = coerce (ffiTernOp @n @a ternopFnma)
  vfnms = coerce (ffiTernOp @n @a ternopFnms)
  vmulAdd = coerce (ffiTernOp @n @a ternopMulAdd)
  vfloor = coerce (ffiUnOp @n @a unopFloor)
  vceil = coerce (ffiUnOp @n @a unopCeil)
  vtrunc = coerce (ffiUnOp @n @a unopTrunc)
  vroundNearest = coerce (ffiUnOp @n @a unopRound)

instance (SIMDShape n a, Num a) => ExpSIMDBits Identity n a where
  vand = coerce (ffiBinOp @n @a opAnd)
  vor = coerce (ffiBinOp @n @a opOr)
  vxor = coerce (ffiBinOp @n @a opXor)
  vandnot = coerce (ffiBinOp @n @a opAndNot)
  vnot = coerce (ffiUnOp @n @a unopNot)
  vshl (Identity (IdentitySIMDVector v)) (Identity c) =
    Identity (IdentitySIMDVector (ffiShift False v c))
  vshr (Identity (IdentitySIMDVector v)) (Identity c) =
    Identity (IdentitySIMDVector (ffiShift True v c))
  vshlImm p v = vshl v (Identity (fromIntegral (natVal p)))
  vshrImm p v = vshr v (Identity (fromIntegral (natVal p)))
  vshlVar = coerce (ffiBinOp @n @a opShlVar)
  vshrVar = coerce (ffiBinOp @n @a opShrVar)
  vpopcnt = coerce (ffiUnOp @n @a unopPopcnt)

instance (SIMDShape n a, Num a) => ExpSIMDShuffle Identity n a where
  vshuffleLanes idxs (Identity (IdentitySIMDVector v)) =
    Identity (IdentitySIMDVector (ffiPermuteLanes v (Left idxs)))
  vpermuteVar (Identity (IdentitySIMDVector v)) (Identity (IdentitySIMDVector idxs)) =
    Identity (IdentitySIMDVector (ffiPermuteLanes v (Right idxs)))
  vshuffleBytes (Identity (IdentitySIMDVector v)) (Identity (IdentitySIMDVector ctrl)) =
    Identity (IdentitySIMDVector (ffiShuffleBytes v ctrl))
  vzipLo = coerce (ffiBinOp @n @a opZipLo)
  vzipHi = coerce (ffiBinOp @n @a opZipHi)

instance (SIMDHalfPair n2 n a) => ExpSIMDSplit Identity n2 n a where
  vsplit (Identity (IdentitySIMDVector v)) =
    let (lo, hi) = ffiSplit @n2 @n @a v
     in (Identity (IdentitySIMDVector lo), Identity (IdentitySIMDVector hi))
  vconcat (Identity (IdentitySIMDVector lo)) (Identity (IdentitySIMDVector hi)) =
    Identity (IdentitySIMDVector (ffiConcat @n @n2 @a lo hi))

instance (SIMDShape n a, Num a) => ExpSIMDReduce Identity n a where
  vreduceAdd (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redAdd v)
  vreduceMin (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redMin v)
  vreduceMax (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redMax v)
  vreduceAnd (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redAnd v)
  vreduceOr (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redOr v)
  vreduceXor (Identity (IdentitySIMDVector v)) = Identity (ffiReduce redXor v)

instance ExpSIMDDot Identity where
  vdp4aS (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp4aS a b c)
  vdp4aU (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp4aU a b c)
  vdp4aVecS (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity (IdentitySIMDVector c)) =
    Identity (IdentitySIMDVector (ffiDp4aVecS a b c))
  vdp4aVecU (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity (IdentitySIMDVector c)) =
    Identity (IdentitySIMDVector (ffiDp4aVecU a b c))
  vdp2aLoS (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp2aS False a b c)
  vdp2aHiS (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp2aS True a b c)
  vdp2aLoU (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp2aU False a b c)
  vdp2aHiU (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) (Identity c) =
    Identity (ffiDp2aU True a b c)
  vsad8 (Identity (IdentitySIMDVector a)) (Identity (IdentitySIMDVector b)) =
    Identity (ffiSadU8 a b)

instance ExpSIMDCast Identity where
  vbitcast (Identity (IdentitySIMDVector v)) = Identity (IdentitySIMDVector (ffiBitcast v))
  vcvt (Identity (IdentitySIMDVector v)) = Identity (IdentitySIMDVector (ffiCvt v))
  vnarrowSat (Identity (IdentitySIMDVector lo)) (Identity (IdentitySIMDVector hi)) =
    Identity (IdentitySIMDVector (ffiPackSat lo hi))

instance (SIMDShape n a) => CmdSIMDMem IO Identity n a where
  vload (Identity p) = Identity . IdentitySIMDVector <$> ffiLoad p
  vloadAligned (Identity p) = Identity . IdentitySIMDVector <$> ffiLoadAligned p
  vstore (Identity p) (Identity (IdentitySIMDVector v)) = ffiStore p v
  vstoreAligned (Identity p) (Identity (IdentitySIMDVector v)) = ffiStoreAligned p v
  vloadMasked (Identity (IdentitySIMDMask m)) (Identity (IdentitySIMDVector fb)) (Identity p) =
    Identity . IdentitySIMDVector <$> ffiLoadMasked m fb p
  vstoreMasked (Identity (IdentitySIMDMask m)) (Identity p) (Identity (IdentitySIMDVector v)) =
    ffiStoreMasked m p v
  vgather (Identity p) (Identity (IdentitySIMDVector idxs)) =
    Identity . IdentitySIMDVector <$> ffiGather p idxs
  vgatherMasked (Identity (IdentitySIMDMask m)) (Identity (IdentitySIMDVector fb)) (Identity p) (Identity (IdentitySIMDVector idxs)) =
    Identity . IdentitySIMDVector <$> ffiGatherMasked m fb p idxs

--------------------------------------------------------------------------------
-- Wide (k-way Multi-Register Unrolling) Combinators
--------------------------------------------------------------------------------

wsplat :: (KnownNat k, ExpSIMDVec e n a) => e a -> Wide k (e (SIMDVector e n a))
wsplat = wideReplicate . vsplat

wzero :: (KnownNat k, ExpSIMDVec e n a) => Wide k (e (SIMDVector e n a))
wzero = wideReplicate vzero

wadd, wsub, wmul :: (KnownNat k, ExpSIMDArith e n a) => Wide k (e (SIMDVector e n a)) -> Wide k (e (SIMDVector e n a)) -> Wide k (e (SIMDVector e n a))
wadd = liftA2 vadd
wsub = liftA2 vsub
wmul = liftA2 vmul

wfma :: (KnownNat k, ExpSIMDFloat e n a) => Wide k (e (SIMDVector e n a)) -> Wide k (e (SIMDVector e n a)) -> Wide k (e (SIMDVector e n a)) -> Wide k (e (SIMDVector e n a))
wfma a b c = vfma <$> a <*> b <*> c

wreduceVec :: (ExpSIMDArith e n a) => Wide k (e (SIMDVector e n a)) -> e (SIMDVector e n a)
wreduceVec = wideTreeReduce vadd

wload ::
  forall k m e n a.
  (KnownNat k, CmdSIMDMem m e n a) =>
  (e (Ptr a) -> Int -> e (Ptr a)) ->
  e (Ptr a) ->
  m (Wide k (e (SIMDVector e n a)))
wload stepPtr base =
  let stride = simdLanes (Proxy @n) (Proxy @a)
   in traverse (\i -> vload (stepPtr base (i * stride))) (wideGenerate @k id)

wstore ::
  forall k m e n a.
  (KnownNat k, CmdSIMDMem m e n a) =>
  (e (Ptr a) -> Int -> e (Ptr a)) ->
  e (Ptr a) ->
  Wide k (e (SIMDVector e n a)) ->
  m ()
wstore stepPtr base w =
  let stride = simdLanes (Proxy @n) (Proxy @a)
   in forM_ (zip [0 ..] (wideToList w)) $ \(i, v) ->
        vstore (stepPtr base (i * stride)) v
  where
    _ = plusPtr
