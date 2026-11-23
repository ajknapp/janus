{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Core Janus expression ('ReaderExp'), reference ('ReaderRef'), and command
-- instances for @('ReaderT' r m, 'ReaderExp' r e)@.
--
-- Any Janus backend @(m, e)@ lifted through 'ReaderT' @r@ automatically inherits
-- all scalar, math, pointer, C-struct, fixpoint, SIMD, memory, allocator, and
-- control-flow typeclasses.
module Janus.Control.Reader.Base
  ( -- * Reader Expression & Reference Wrappers
    ReaderExp (..),
    liftReaderExp,
    ReaderRef (..),
    ExpNewtype (..),

    -- * Associated SIMD Wrappers for ReaderExp
    SIMDVector (..),
    SIMDMask (..),
  )
where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Class (lift)
import Data.Coerce (Coercible, coerce)
import Data.Function (fix)
import Data.Functor.Identity (Identity (..))
import qualified Data.Kind as K
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import GHC.Float (expm1, log1p)
import Janus.Backend.C (JanusCT (..), JanusCTyped (..), RVal (..))
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..), CmdSync (..), JanusCUDA (..))
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import Janus.Command.Memory
  ( CmdDynamicAligned (..),
    CmdMem (..),
    CmdMemCopy (..),
    CmdRefPtr (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (Cell (..), CmdRef (..), RefCell (..))
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Data.HKD (ExpPtrWord (..), ExpStructPtr (..))
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast
  ( ExpBoolCast (..),
    ExpFloatingCast (..),
    ExpIntegralCast (..),
    ExpPtrCast (..),
  )
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Extract (ExpExtract (..))
import Janus.Expression.Inject (ExpInject (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.MurmurHash (ExpMurmurHash (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD
import Janus.Fix (ExpFix (..), JanusCFix (..), jfix)
import Janus.SPMD.Types (ExpPair (..))
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- Reader Expression, Reference, & Representational Coercion Wrappers
--------------------------------------------------------------------------------

-- | Zero-cost Janus expression wrapper carrying phantom environment type @r@ to
-- pair with @'ReaderT' r m@ under bidirectional @m -> e, e -> m@ functional
-- dependencies.
newtype ReaderExp (r :: K.Type) (e :: K.Type -> K.Type) a = ReaderExp
  { getReaderExp :: e a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type instance JanusTyped (ReaderExp r e) = JanusTyped e

-- | Lift an underlying Janus expression @e a@ into @'ReaderExp' r e a@.
liftReaderExp :: e a -> ReaderExp r e a
liftReaderExp = ReaderExp

-- | Mutable reference wrapper for @('ReaderT' r m, 'ReaderExp' r e)@.
newtype ReaderRef (r :: K.Type) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = ReaderRef
  { getReaderRef :: Ref m e a
  }

-- | Zero-cost representational coercion inside Janus expression functors @e@,
-- used to lift associated 'SIMDVector' / 'SIMDMask' newtypes without exposing
-- internal codegen monad constructors to downstream modules.
class ExpNewtype (e :: K.Type -> K.Type) where
  coerceExpNewtype :: (Coercible a b) => e a -> e b

instance ExpNewtype Identity where
  coerceExpNewtype = coerce

instance ExpNewtype (JanusCT t) where
  coerceExpNewtype (JanusCT m) = JanusCT (fmap (\(RVal x) -> RVal x) m)

instance ExpNewtype JanusCUDA where
  coerceExpNewtype (JanusCUDA e) = JanusCUDA (coerceExpNewtype e)

instance ExpNewtype CUDAIO where
  coerceExpNewtype = coerce

instance ExpNewtype CUDADejaFu where
  coerceExpNewtype = coerce

instance (ExpNewtype e) => ExpNewtype (ReaderExp r e) where
  coerceExpNewtype (ReaderExp e) = ReaderExp (coerceExpNewtype e)

--------------------------------------------------------------------------------
-- Scalar & Algebraic Expression Instances on `ReaderExp r e`
--------------------------------------------------------------------------------

deriving newtype instance {-# OVERLAPPABLE #-} (Num (e a)) => Num (ReaderExp r e a)

deriving newtype instance {-# OVERLAPPABLE #-} (Bounded (e a)) => Bounded (ReaderExp r e a)

deriving newtype instance {-# OVERLAPPABLE #-} (Fractional (e a)) => Fractional (ReaderExp r e a)

deriving newtype instance {-# OVERLAPPABLE #-} (Floating (e a)) => Floating (ReaderExp r e a)

instance {-# OVERLAPPABLE #-} (FMA (e a)) => FMA (ReaderExp r e a) where
  fma (ReaderExp a) (ReaderExp b) (ReaderExp d) = ReaderExp (fma a b d)

instance {-# OVERLAPPABLE #-} (Hypot (e a)) => Hypot (ReaderExp r e a) where
  hypot (ReaderExp a) (ReaderExp b) = ReaderExp (hypot a b)

instance {-# OVERLAPPABLE #-} (Join (e a)) => Join (ReaderExp r e a) where
  ReaderExp a \/ ReaderExp b = ReaderExp (a \/ b)

instance {-# OVERLAPPABLE #-} (Meet (e a)) => Meet (ReaderExp r e a) where
  ReaderExp a /\ ReaderExp b = ReaderExp (a /\ b)

instance {-# OVERLAPPABLE #-} (Lower (e a)) => Lower (ReaderExp r e a) where
  lowerBound = ReaderExp lowerBound

instance {-# OVERLAPPABLE #-} (Upper (e a)) => Upper (ReaderExp r e a) where
  upperBound = ReaderExp upperBound

instance (ExpBool e) => ExpBool (ReaderExp r e) where
  false = ReaderExp false
  true = ReaderExp true
  land (ReaderExp a) (ReaderExp b) = ReaderExp (land a b)
  lor (ReaderExp a) (ReaderExp b) = ReaderExp (lor a b)
  lnot (ReaderExp a) = ReaderExp (lnot a)

instance (ExpEq e a) => ExpEq (ReaderExp r e) a where
  eq (ReaderExp a) (ReaderExp b) = ReaderExp (eq a b)
  ne (ReaderExp a) (ReaderExp b) = ReaderExp (ne a b)

instance (ExpOrd e a) => ExpOrd (ReaderExp r e) a where
  lt (ReaderExp a) (ReaderExp b) = ReaderExp (lt a b)
  le (ReaderExp a) (ReaderExp b) = ReaderExp (le a b)
  gt (ReaderExp a) (ReaderExp b) = ReaderExp (gt a b)
  ge (ReaderExp a) (ReaderExp b) = ReaderExp (ge a b)

instance (ExpBits e a) => ExpBits (ReaderExp r e) a where
  iand (ReaderExp a) (ReaderExp b) = ReaderExp (iand a b)
  ior (ReaderExp a) (ReaderExp b) = ReaderExp (ior a b)
  ieor (ReaderExp a) (ReaderExp b) = ReaderExp (ieor a b)
  inot (ReaderExp a) = ReaderExp (inot a)
  lshift (ReaderExp a) (ReaderExp b) = ReaderExp (lshift a b)
  rshift (ReaderExp a) (ReaderExp b) = ReaderExp (rshift a b)

instance (ExpBoolCast e a) => ExpBoolCast (ReaderExp r e) a where
  fromBool (ReaderExp b) = ReaderExp (fromBool b)
  toBool (ReaderExp a) = ReaderExp (toBool a)

instance (ExpFloatingCast e a b) => ExpFloatingCast (ReaderExp r e) a b where
  toFloating (ReaderExp a) = ReaderExp (toFloating a)

instance (ExpIntegralCast e a b) => ExpIntegralCast (ReaderExp r e) a b where
  toIntegral (ReaderExp a) = ReaderExp (toIntegral a)

instance (ExpPtrCast e) => ExpPtrCast (ReaderExp r e) where
  ptrCast (ReaderExp p) = ReaderExp (ptrCast p)
  toVoidPtr (ReaderExp p) = ReaderExp (toVoidPtr p)
  fromVoidPtr (ReaderExp p) = ReaderExp (fromVoidPtr p)

instance (ExpCond e) => ExpCond (ReaderExp r e) where
  ifThenElse (ReaderExp p) (ReaderExp t) (ReaderExp f) = ReaderExp (ifThenElse p t f)

instance (ExpExtract e a) => ExpExtract (ReaderExp r e) a where
  extract (ReaderExp a) = extract a

instance (ExpInject e a) => ExpInject (ReaderExp r e) a where
  inject a = ReaderExp (inject a)

instance (ExpIntegral e a) => ExpIntegral (ReaderExp r e) a where
  quotient (ReaderExp a) (ReaderExp b) = ReaderExp (quotient a b)
  remainder (ReaderExp a) (ReaderExp b) = ReaderExp (remainder a b)

instance (ExpLet e) => ExpLet (ReaderExp r e) where
  let_ (ReaderExp x) f = ReaderExp (let_ x (getReaderExp . f . ReaderExp))

instance (ExpComplex e a) => ExpComplex (ReaderExp r e) a where
  rePart (ReaderExp z) = ReaderExp (rePart z)
  imPart (ReaderExp z) = ReaderExp (imPart z)

instance (ExpMurmurHash e (e a)) => ExpMurmurHash (ReaderExp r e) (ReaderExp r e a) where
  murmurHashWithSalt (ReaderExp salt) (ReaderExp x) = ReaderExp (murmurHashWithSalt salt x)

instance (ExpSized e a) => ExpSized (ReaderExp r e) a where
  sizeOf p = ReaderExp (sizeOf p)
  alignOf p = ReaderExp (alignOf p)

instance (ExpPtr e a) => ExpPtr (ReaderExp r e) a where
  nullPtr = ReaderExp nullPtr
  ptrAdd (ReaderExp p) (ReaderExp i) = ReaderExp (ptrAdd p i)
  ptrIndex (ReaderExp p) (ReaderExp i) = ReaderExp (ptrIndex p i)

instance (ExpPtrWord e) => ExpPtrWord (ReaderExp r e) where
  ptrToWord64Expr (ReaderExp p) = ReaderExp (ptrToWord64Expr p)
  word64ToPtrExpr (ReaderExp w) = ReaderExp (word64ToPtrExpr w)

instance (ExpStructPtr e) => ExpStructPtr (ReaderExp r e) where
  structFieldPtr (ReaderExp p) off fname = ReaderExp (structFieldPtr p off fname)

instance (ExpPair e) => ExpPair (ReaderExp r e) where
  mkPair (ReaderExp a) (ReaderExp b) = ReaderExp (mkPair a b)
  fstPair (ReaderExp p) = ReaderExp (fstPair p)
  sndPair (ReaderExp p) = ReaderExp (sndPair p)
  mkVoidPtr (ReaderExp p) = ReaderExp (mkVoidPtr p)
  unVoidPtr (ReaderExp p) = ReaderExp (unVoidPtr p)
  mkSPMDUnit = ReaderExp mkSPMDUnit
  coerceExp (ReaderExp x) = ReaderExp (coerceExp x)

instance ExpFix (ReaderExp r Identity) fn where
  fun _ _ = fix
  {-# INLINE fun #-}

instance (Typeable r, Typeable t, JanusCTyped a) => JanusCFix (ReaderExp r (JanusCT t) a) where
  jcfun name args params n (ReaderExp body) = ReaderExp (jcfun name args params n body)
  jcfake name args (ReaderExp body) = ReaderExp (jcfake name args body)

instance (Typeable r, Typeable t, JanusCTyped a, JanusCFix fn) => JanusCFix (ReaderExp r (JanusCT t) a -> fn) where
  jcfun name args params n body (ReaderExp x) =
    jcfun name args params n (body . ReaderExp) x
  jcfake name args f (ReaderExp x) =
    jcfake name args (f . ReaderExp) x

instance (JanusCFix fn) => ExpFix (ReaderExp r (JanusCT t)) fn where
  fun _ = jfix

--------------------------------------------------------------------------------
-- SIMD Expression & Memory Instances on `(ReaderT r m, ReaderExp r e)`
--------------------------------------------------------------------------------

instance (ExpSIMD e) => ExpSIMD (ReaderExp r e) where
  newtype SIMDVector (ReaderExp r e) n a = ReaderSIMDVector
    { getReaderSIMDVector :: SIMDVector e n a
    }
  newtype SIMDMask (ReaderExp r e) n a = ReaderSIMDMask
    { getReaderSIMDMask :: SIMDMask e n a
    }

deriving newtype instance (Eq (SIMDVector e n a)) => Eq (SIMDVector (ReaderExp r e) n a)

deriving newtype instance (Ord (SIMDVector e n a)) => Ord (SIMDVector (ReaderExp r e) n a)

deriving newtype instance (Show (SIMDVector e n a)) => Show (SIMDVector (ReaderExp r e) n a)

deriving newtype instance (Storable (SIMDVector e n a)) => Storable (SIMDVector (ReaderExp r e) n a)

deriving newtype instance
  (Typeable r, Typeable e, Typeable n, Typeable a, JanusCTyped (SIMDVector e n a)) =>
  JanusCTyped (SIMDVector (ReaderExp r e) n a)

deriving newtype instance (Eq (SIMDMask e n a)) => Eq (SIMDMask (ReaderExp r e) n a)

deriving newtype instance (Ord (SIMDMask e n a)) => Ord (SIMDMask (ReaderExp r e) n a)

deriving newtype instance (Show (SIMDMask e n a)) => Show (SIMDMask (ReaderExp r e) n a)

deriving newtype instance (Storable (SIMDMask e n a)) => Storable (SIMDMask (ReaderExp r e) n a)

deriving newtype instance
  (Typeable r, Typeable e, Typeable n, Typeable a, JanusCTyped (SIMDMask e n a)) =>
  JanusCTyped (SIMDMask (ReaderExp r e) n a)

instance
  (SIMDShape n a, HostSIMDValue (SIMDVector e n a) n a) =>
  HostSIMDValue (SIMDVector (ReaderExp r e) n a) n a
  where
  simdFromList = ReaderSIMDVector . simdFromList
  simdToList = simdToList . getReaderSIMDVector

instance
  (SIMDShape n a, HostSIMDMask (SIMDMask e n a) n a) =>
  HostSIMDMask (SIMDMask (ReaderExp r e) n a) n a
  where
  simdMaskFromBools = ReaderSIMDMask . simdMaskFromBools
  simdMaskToBools = simdMaskToBools . getReaderSIMDMask

{-# INLINE toBaseVec #-}
toBaseVec :: (ExpNewtype e) => ReaderExp r e (SIMDVector (ReaderExp r e) n a) -> e (SIMDVector e n a)
toBaseVec (ReaderExp v) = coerceExpNewtype v

{-# INLINE fromBaseVec #-}
fromBaseVec :: (ExpNewtype e) => e (SIMDVector e n a) -> ReaderExp r e (SIMDVector (ReaderExp r e) n a)
fromBaseVec v = ReaderExp (coerceExpNewtype v)

{-# INLINE toBaseMask #-}
toBaseMask :: (ExpNewtype e) => ReaderExp r e (SIMDMask (ReaderExp r e) n a) -> e (SIMDMask e n a)
toBaseMask (ReaderExp m) = coerceExpNewtype m

{-# INLINE fromBaseMask #-}
fromBaseMask :: (ExpNewtype e) => e (SIMDMask e n a) -> ReaderExp r e (SIMDMask (ReaderExp r e) n a)
fromBaseMask m = ReaderExp (coerceExpNewtype m)

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Num (e (SIMDVector e n a))) =>
  Num (ReaderExp r e (SIMDVector (ReaderExp r e) n a))
  where
  a + b = fromBaseVec (toBaseVec a + toBaseVec b)
  a - b = fromBaseVec (toBaseVec a - toBaseVec b)
  a * b = fromBaseVec (toBaseVec a * toBaseVec b)
  negate a = fromBaseVec (negate (toBaseVec a))
  abs a = fromBaseVec (abs (toBaseVec a))
  signum a = fromBaseVec (signum (toBaseVec a))
  fromInteger k = fromBaseVec (fromInteger k)

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Fractional (e (SIMDVector e n a))) =>
  Fractional (ReaderExp r e (SIMDVector (ReaderExp r e) n a))
  where
  a / b = fromBaseVec (toBaseVec a / toBaseVec b)
  recip a = fromBaseVec (recip (toBaseVec a))
  fromRational q = fromBaseVec (fromRational q)

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Floating (e (SIMDVector e n a))) =>
  Floating (ReaderExp r e (SIMDVector (ReaderExp r e) n a))
  where
  pi = fromBaseVec pi
  exp a = fromBaseVec (exp (toBaseVec a))
  log a = fromBaseVec (log (toBaseVec a))
  sqrt a = fromBaseVec (sqrt (toBaseVec a))
  a ** b = fromBaseVec (toBaseVec a ** toBaseVec b)
  logBase a b = fromBaseVec (logBase (toBaseVec a) (toBaseVec b))
  sin a = fromBaseVec (sin (toBaseVec a))
  cos a = fromBaseVec (cos (toBaseVec a))
  tan a = fromBaseVec (tan (toBaseVec a))
  asin a = fromBaseVec (asin (toBaseVec a))
  acos a = fromBaseVec (acos (toBaseVec a))
  atan a = fromBaseVec (atan (toBaseVec a))
  sinh a = fromBaseVec (sinh (toBaseVec a))
  cosh a = fromBaseVec (cosh (toBaseVec a))
  tanh a = fromBaseVec (tanh (toBaseVec a))
  asinh a = fromBaseVec (asinh (toBaseVec a))
  acosh a = fromBaseVec (acosh (toBaseVec a))
  atanh a = fromBaseVec (atanh (toBaseVec a))
  log1p a = fromBaseVec (log1p (toBaseVec a))
  expm1 a = fromBaseVec (expm1 (toBaseVec a))

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, FMA (e (SIMDVector e n a))) =>
  FMA (ReaderExp r e (SIMDVector (ReaderExp r e) n a))
  where
  fma a b d = fromBaseVec (fma (toBaseVec a) (toBaseVec b) (toBaseVec d))

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Hypot (e (SIMDVector e n a))) =>
  Hypot (ReaderExp r e (SIMDVector (ReaderExp r e) n a))
  where
  hypot a b = fromBaseVec (hypot (toBaseVec a) (toBaseVec b))

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Join (e (SIMDMask e n a))) =>
  Join (ReaderExp r e (SIMDMask (ReaderExp r e) n a))
  where
  a \/ b = fromBaseMask (toBaseMask a \/ toBaseMask b)

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Meet (e (SIMDMask e n a))) =>
  Meet (ReaderExp r e (SIMDMask (ReaderExp r e) n a))
  where
  a /\ b = fromBaseMask (toBaseMask a /\ toBaseMask b)

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Lower (e (SIMDMask e n a))) =>
  Lower (ReaderExp r e (SIMDMask (ReaderExp r e) n a))
  where
  lowerBound = fromBaseMask lowerBound

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, Upper (e (SIMDMask e n a))) =>
  Upper (ReaderExp r e (SIMDMask (ReaderExp r e) n a))
  where
  upperBound = fromBaseMask upperBound

instance (ExpNewtype e, ExpSIMDVec e n a) => ExpSIMDVec (ReaderExp r e) n a where
  vzero = fromBaseVec vzero
  vsplat (ReaderExp x) = fromBaseVec (vsplat x)
  viota = fromBaseVec viota
  vfromLanes xs = fromBaseVec (vfromLanes (map getReaderExp xs))
  vextractLane p v = ReaderExp (vextractLane p (toBaseVec v))
  vinsertLane p (ReaderExp x) v = fromBaseVec (vinsertLane p x (toBaseVec v))

instance (ExpNewtype e, ExpSIMDMask e n a) => ExpSIMDMask (ReaderExp r e) n a where
  mfalse = fromBaseMask mfalse
  mtrue = fromBaseMask mtrue
  mand a b = fromBaseMask (mand (toBaseMask a) (toBaseMask b))
  mor a b = fromBaseMask (mor (toBaseMask a) (toBaseMask b))
  mxor a b = fromBaseMask (mxor (toBaseMask a) (toBaseMask b))
  mandnot a b = fromBaseMask (mandnot (toBaseMask a) (toBaseMask b))
  mnot a = fromBaseMask (mnot (toBaseMask a))
  vselect m a b = fromBaseVec (vselect (toBaseMask m) (toBaseVec a) (toBaseVec b))
  mmovemask m = ReaderExp (mmovemask (toBaseMask m))
  mpopcount m = ReaderExp (mpopcount (toBaseMask m))
  mfirstTrue m = ReaderExp (mfirstTrue (toBaseMask m))
  mallOf m = ReaderExp (mallOf (toBaseMask m))
  manyOf m = ReaderExp (manyOf (toBaseMask m))
  mnoneOf m = ReaderExp (mnoneOf (toBaseMask m))

instance (ExpNewtype e, ExpSIMDCmp e n a) => ExpSIMDCmp (ReaderExp r e) n a where
  veq a b = fromBaseMask (veq (toBaseVec a) (toBaseVec b))
  vne a b = fromBaseMask (vne (toBaseVec a) (toBaseVec b))
  vlt a b = fromBaseMask (vlt (toBaseVec a) (toBaseVec b))
  vle a b = fromBaseMask (vle (toBaseVec a) (toBaseVec b))
  vgt a b = fromBaseMask (vgt (toBaseVec a) (toBaseVec b))
  vge a b = fromBaseMask (vge (toBaseVec a) (toBaseVec b))

instance (ExpNewtype e, ExpSIMDArith e n a) => ExpSIMDArith (ReaderExp r e) n a where
  vadd a b = fromBaseVec (vadd (toBaseVec a) (toBaseVec b))
  vsub a b = fromBaseVec (vsub (toBaseVec a) (toBaseVec b))
  vmul a b = fromBaseVec (vmul (toBaseVec a) (toBaseVec b))
  vneg a = fromBaseVec (vneg (toBaseVec a))
  vabs a = fromBaseVec (vabs (toBaseVec a))
  vmin a b = fromBaseVec (vmin (toBaseVec a) (toBaseVec b))
  vmax a b = fromBaseVec (vmax (toBaseVec a) (toBaseVec b))

instance (ExpNewtype e, ExpSIMDSat e n a) => ExpSIMDSat (ReaderExp r e) n a where
  vadds a b = fromBaseVec (vadds (toBaseVec a) (toBaseVec b))
  vsubs a b = fromBaseVec (vsubs (toBaseVec a) (toBaseVec b))
  vabss a = fromBaseVec (vabss (toBaseVec a))
  vavg a b = fromBaseVec (vavg (toBaseVec a) (toBaseVec b))
  vabsdiff a b = fromBaseVec (vabsdiff (toBaseVec a) (toBaseVec b))

instance (ExpNewtype e, ExpSIMDFloat e n a) => ExpSIMDFloat (ReaderExp r e) n a where
  vdiv a b = fromBaseVec (vdiv (toBaseVec a) (toBaseVec b))
  vsqrt a = fromBaseVec (vsqrt (toBaseVec a))
  vrsqrtApprox a = fromBaseVec (vrsqrtApprox (toBaseVec a))
  vrcpApprox a = fromBaseVec (vrcpApprox (toBaseVec a))
  vfma a b d = fromBaseVec (vfma (toBaseVec a) (toBaseVec b) (toBaseVec d))
  vfms a b d = fromBaseVec (vfms (toBaseVec a) (toBaseVec b) (toBaseVec d))
  vfnma a b d = fromBaseVec (vfnma (toBaseVec a) (toBaseVec b) (toBaseVec d))
  vfnms a b d = fromBaseVec (vfnms (toBaseVec a) (toBaseVec b) (toBaseVec d))
  vmulAdd a b d = fromBaseVec (vmulAdd (toBaseVec a) (toBaseVec b) (toBaseVec d))
  vfloor a = fromBaseVec (vfloor (toBaseVec a))
  vceil a = fromBaseVec (vceil (toBaseVec a))
  vtrunc a = fromBaseVec (vtrunc (toBaseVec a))
  vroundNearest a = fromBaseVec (vroundNearest (toBaseVec a))

instance (ExpNewtype e, ExpSIMDBits e n a) => ExpSIMDBits (ReaderExp r e) n a where
  vand a b = fromBaseVec (vand (toBaseVec a) (toBaseVec b))
  vor a b = fromBaseVec (vor (toBaseVec a) (toBaseVec b))
  vxor a b = fromBaseVec (vxor (toBaseVec a) (toBaseVec b))
  vandnot a b = fromBaseVec (vandnot (toBaseVec a) (toBaseVec b))
  vnot a = fromBaseVec (vnot (toBaseVec a))
  vshl a (ReaderExp s) = fromBaseVec (vshl (toBaseVec a) s)
  vshr a (ReaderExp s) = fromBaseVec (vshr (toBaseVec a) s)
  vshlImm p a = fromBaseVec (vshlImm p (toBaseVec a))
  vshrImm p a = fromBaseVec (vshrImm p (toBaseVec a))
  vshlVar a b = fromBaseVec (vshlVar (toBaseVec a) (toBaseVec b))
  vshrVar a b = fromBaseVec (vshrVar (toBaseVec a) (toBaseVec b))
  vpopcnt a = fromBaseVec (vpopcnt (toBaseVec a))

instance (ExpNewtype e, ExpSIMDShuffle e n a) => ExpSIMDShuffle (ReaderExp r e) n a where
  vshuffleLanes idxs a = fromBaseVec (vshuffleLanes idxs (toBaseVec a))
  vpermuteVar a idxs = fromBaseVec (vpermuteVar (toBaseVec a) (toBaseVec idxs))
  vshuffleBytes a ctrl = fromBaseVec (vshuffleBytes (toBaseVec a) (toBaseVec ctrl))
  vzipLo a b = fromBaseVec (vzipLo (toBaseVec a) (toBaseVec b))
  vzipHi a b = fromBaseVec (vzipHi (toBaseVec a) (toBaseVec b))

instance (ExpNewtype e, ExpSIMDSplit e n2 n a) => ExpSIMDSplit (ReaderExp r e) n2 n a where
  vsplit v =
    let (lo, hi) = vsplit (toBaseVec v)
     in (fromBaseVec lo, fromBaseVec hi)
  vconcat lo hi = fromBaseVec (vconcat (toBaseVec lo) (toBaseVec hi))

instance (ExpNewtype e, ExpSIMDReduce e n a) => ExpSIMDReduce (ReaderExp r e) n a where
  vreduceAdd a = ReaderExp (vreduceAdd (toBaseVec a))
  vreduceMin a = ReaderExp (vreduceMin (toBaseVec a))
  vreduceMax a = ReaderExp (vreduceMax (toBaseVec a))
  vreduceAnd a = ReaderExp (vreduceAnd (toBaseVec a))
  vreduceOr a = ReaderExp (vreduceOr (toBaseVec a))
  vreduceXor a = ReaderExp (vreduceXor (toBaseVec a))

instance (ExpNewtype e, ExpSIMDDot e) => ExpSIMDDot (ReaderExp r e) where
  vdp4aS a b (ReaderExp acc) = ReaderExp (vdp4aS (toBaseVec a) (toBaseVec b) acc)
  vdp4aU a b (ReaderExp acc) = ReaderExp (vdp4aU (toBaseVec a) (toBaseVec b) acc)
  vdp4aVecS a b acc = fromBaseVec (vdp4aVecS (toBaseVec a) (toBaseVec b) (toBaseVec acc))
  vdp4aVecU a b acc = fromBaseVec (vdp4aVecU (toBaseVec a) (toBaseVec b) (toBaseVec acc))
  vdp2aLoS a b (ReaderExp acc) = ReaderExp (vdp2aLoS (toBaseVec a) (toBaseVec b) acc)
  vdp2aHiS a b (ReaderExp acc) = ReaderExp (vdp2aHiS (toBaseVec a) (toBaseVec b) acc)
  vdp2aLoU a b (ReaderExp acc) = ReaderExp (vdp2aLoU (toBaseVec a) (toBaseVec b) acc)
  vdp2aHiU a b (ReaderExp acc) = ReaderExp (vdp2aHiU (toBaseVec a) (toBaseVec b) acc)
  vsad8 a b = ReaderExp (vsad8 (toBaseVec a) (toBaseVec b))

instance (ExpNewtype e, ExpSIMDCast e) => ExpSIMDCast (ReaderExp r e) where
  vbitcast a = fromBaseVec (vbitcast (toBaseVec a))
  vcvt a = fromBaseVec (vcvt (toBaseVec a))
  vnarrowSat lo hi = fromBaseVec (vnarrowSat (toBaseVec lo) (toBaseVec hi))

instance (ExpNewtype e, CmdSIMDMem m e n a) => CmdSIMDMem (ReaderT r m) (ReaderExp r e) n a where
  vload (ReaderExp p) = lift (fromBaseVec <$> vload p)
  vloadAligned (ReaderExp p) = lift (fromBaseVec <$> vloadAligned p)
  vstore (ReaderExp p) v = lift (vstore p (toBaseVec v))
  vstoreAligned (ReaderExp p) v = lift (vstoreAligned p (toBaseVec v))
  vloadMasked m fb (ReaderExp p) =
    lift (fromBaseVec <$> vloadMasked (toBaseMask m) (toBaseVec fb) p)
  vstoreMasked m (ReaderExp p) v =
    lift (vstoreMasked (toBaseMask m) p (toBaseVec v))
  vgather (ReaderExp p) idxs =
    lift (fromBaseVec <$> vgather p (toBaseVec idxs))
  vgatherMasked m fb (ReaderExp p) idxs =
    lift (fromBaseVec <$> vgatherMasked (toBaseMask m) (toBaseVec fb) p (toBaseVec idxs))

--------------------------------------------------------------------------------
-- Command & Control Instances on `(ReaderT r m, ReaderExp r e)`
--------------------------------------------------------------------------------

instance (CmdRef m e) => CmdRef (ReaderT r m) (ReaderExp r e) where
  type Ref (ReaderT r m) (ReaderExp r e) a = ReaderRef r m e a
  letM (ReaderExp x) = lift (ReaderExp <$> letM x)
  newRef (ReaderExp x) = lift (ReaderRef <$> newRef x)
  newRef' = lift (ReaderRef <$> newRef')
  readRef (ReaderRef ref) = lift (ReaderExp <$> readRef ref)
  writeRef (ReaderRef ref) (ReaderExp x) = lift (writeRef ref x)

instance (CmdRef m e, JanusTyped e a) => RefCell (ReaderT r m) (ReaderExp r e) (ReaderExp r e a) where
  newCell x = do
    ref <- newRef x
    pure (Cell (readRef ref) (writeRef ref))
  newCell' = do
    ref <- newRef'
    pure (Cell (readRef ref) (writeRef ref))

instance (CmdCond m e) => CmdCond (ReaderT r m) (ReaderExp r e) where
  ifThenElseM_ (ReaderExp p) t f =
    ReaderT $ \env ->
      ifThenElseM_ p (runReaderT t env) (runReaderT f env)

instance (CmdWhile m e) => CmdWhile (ReaderT r m) (ReaderExp r e) where
  whileM (ReaderExp p) body =
    ReaderT $ \env ->
      whileM p (\ref -> runReaderT (body (ReaderRef ref)) env)

instance (CmdRange m e) => CmdRange (ReaderT r m) (ReaderExp r e) where
  rangeM (ReaderExp l) (ReaderExp u) body =
    ReaderT $ \env ->
      rangeM l u (\i -> runReaderT (body (ReaderExp i)) env)

instance (CmdCase m e) => CmdCase (ReaderT r m) (ReaderExp r e) where
  switchCase_ (ReaderExp tag) cases def =
    ReaderT $ \env ->
      switchCase_
        tag
        [(k, runReaderT act env) | (k, act) <- cases]
        (runReaderT def env)

instance (Monad m, CmdMem m e) => CmdMem (ReaderT r m) (ReaderExp r e) where
  malloc (ReaderExp sz) = lift (ReaderExp <$> malloc sz)
  calloc (ReaderExp n) (ReaderExp sz) = lift (ReaderExp <$> calloc n sz)
  realloc (ReaderExp p) (ReaderExp sz) = lift (ReaderExp <$> realloc p sz)
  free (ReaderExp p) = lift (free p)

instance (Monad m, CmdDynamicAligned m e) => CmdDynamicAligned (ReaderT r m) (ReaderExp r e) where
  alignedMalloc (ReaderExp sz) (ReaderExp al) = lift (ReaderExp <$> alignedMalloc sz al)

instance (Monad m, CmdMemCopy m e) => CmdMemCopy (ReaderT r m) (ReaderExp r e) where
  cmdMemcpy (ReaderExp dst) (ReaderExp src) (ReaderExp n) = lift (cmdMemcpy dst src n)
  cmdMemmove (ReaderExp dst) (ReaderExp src) (ReaderExp n) = lift (cmdMemmove dst src n)
  cmdMemset (ReaderExp dst) (ReaderExp val) (ReaderExp n) = lift (cmdMemset dst val n)

instance (Monad m, CmdStorable m e a) => CmdStorable (ReaderT r m) (ReaderExp r e) a where
  peek (ReaderExp p) = lift (ReaderExp <$> peek p)
  poke (ReaderExp p) (ReaderExp x) = lift (poke p x)
  peekElemOff (ReaderExp p) (ReaderExp i) = lift (ReaderExp <$> peekElemOff p i)
  pokeElemOff (ReaderExp p) (ReaderExp x) (ReaderExp i) = lift (pokeElemOff p x i)

instance (Monad m, CmdRefPtr m e a) => CmdRefPtr (ReaderT r m) (ReaderExp r e) a where
  withRefPtr (ReaderRef ref) f =
    ReaderT $ \env ->
      withRefPtr ref (\p -> runReaderT (f (ReaderExp p)) env)

instance (Monad m, CmdIO m e) => CmdIO (ReaderT r m) (ReaderExp r e) where
  stdout = ReaderExp stdout
  stderr = ReaderExp stderr
  fopen (ReaderExp path) (ReaderExp mode) = lift (ReaderExp <$> fopen path mode)
  fclose (ReaderExp h) = lift (ReaderExp <$> fclose h)

instance (CmdString m e) => CmdString (ReaderT r m) (ReaderExp r e) where
  withString s f =
    ReaderT $ \env ->
      withString s (\cs -> runReaderT (f (ReaderExp cs)) env)

instance (Monad m, CmdPutString m e) => CmdPutString (ReaderT r m) (ReaderExp r e) where
  hputString (ReaderExp h) (ReaderExp s) = lift (hputString h s)

instance (Monad m, CmdFormat m e a) => CmdFormat (ReaderT r m) (ReaderExp r e) a where
  hformat (ReaderExp h) (ReaderExp x) term = lift (hformat h x term)

instance (Monad m, CmdTicToc m e) => CmdTicToc (ReaderT r m) (ReaderExp r e) where
  tic = lift ((\(Tic t) -> Tic (ReaderExp t)) <$> tic)
  toc = lift ((\(Toc t) -> Toc (ReaderExp t)) <$> toc)

instance (Monad m, CmdSync m) => CmdSync (ReaderT r m) where
  syncWarp = lift syncWarp
  syncBlock = lift syncBlock
