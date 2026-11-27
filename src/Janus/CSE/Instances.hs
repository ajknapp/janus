{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Janus expression, SIMD, fixpoint, reference, memory, I/O, and control-flow
-- typeclass instances for @('CSEM' m e, 'CSEExp' e)@.
module Janus.CSE.Instances
  ( CSEFixField (..),
    GCSEFixLeaves (..),
    CSEFixParam (..),
    CSEFixMonRet (..),
    CSEFixRets,
    CSEFix (..),
    CSEFixLeafBuilder (..),
  )
where

import Control.Monad.Reader (MonadReader (..))
import qualified Data.ByteString as BS
import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64, Int8)
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import qualified Data.Sequence as Seq
import Data.Type.Equality ((:~:) (Refl), testEquality)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64, Word8)
import Foreign.C (CFile, CInt, CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import GHC.Float (expm1, log1p)
import GHC.Generics
import GHC.TypeNats (KnownNat, natVal)
import Janus.Backend.C.CUDA.Atomic
  ( CmdAtomic (..),
    CmdAtomicAdd (..),
    CmdAtomicBits (..),
    CmdAtomicMinMax (..),
  )
import Janus.Backend.C.CUDA.SharedMem (CmdSharedMem (..))
import Janus.Backend.C.CUDA.Types (CUDAThreadInfo (..), CmdSync (..), mapCUDAThreadInfo)
import Janus.Backend.C.CUDA.Warp (CmdWarpShuffle (..))
import Janus.Backend.C.Raw (JanusCTyped)
import Janus.CSE.Interpret (interpretCSEExp)
import Janus.CSE.Optimize (CSEExpParamLeaf (..), SomeCSEExp (..), TargetExpElem, cseExp, cseM)
import Janus.CSE.Types
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import Janus.Command.Parallel (CmdParallel (..))
import Janus.Command.Memory
  ( CmdDynamicAligned (..),
    CmdMem (..),
    CmdMemCopy (..),
    CmdRefPtr (..),
    CmdStackArray (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (Cell (..), CmdRef (..), RefCell (..))
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Constraints (CmdJanus, CmdJanusIO, CmdLocalFun (..), ExpJanus, LocalArgs (..), withJanusType)
import Janus.Control.Reader.Base (ExpNewtype (..), ReaderExp (..))
import Janus.Data.HKD (ExpPtrWord (..), ExpStructPtr (..), HKD (..), PtrOf (..), PtrStruct (..))
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
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.MurmurHash (ExpMurmurHash (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD
import Janus.Expression.SpecialMath (ExpSpecialMath (..))
import Janus.Fix (ExpFix (..))
import Janus.SPMD.Typed (SPMDTyped (..))
import Janus.SPMD.Types (ExpPair (..), SIMDPair, SPMD (..), SPMDM, SPMDRep, liftSPMD, runSPMD, runSPMDM)
import Janus.Typed (KnownJanusType)
import qualified Type.Reflection as TR

--------------------------------------------------------------------------------
-- Internal Smart-Constructor Helpers
--------------------------------------------------------------------------------

directBind :: forall e a. (CSETyped e a) => Maybe (TR.TypeRep a, BindCap e a)
directBind = Just (TR.typeRep @a, BindDirect @e @a)

litTyped :: forall e a. (CSETyped e a) => BS.ByteString -> [BS.ByteString] -> e a -> CSEExp e a
litTyped tag imms = mkExpLit tag imms (directBind @e @a)

unOpTyped ::
  forall e a b.
  (Typeable a, CSETyped e b) =>
  BS.ByteString ->
  [BS.ByteString] ->
  Bool ->
  (e a -> e b) ->
  CSEExp e a ->
  CSEExp e b
unOpTyped tag imms canTrap fn x =
  mkExpOp
    tag
    (encodeTypeRep (TR.typeRep @a) : imms)
    canTrap
    (directBind @e @b)
    (HCons x HNil)
    (\(HCons vx HNil) -> fn vx)

binOpTyped ::
  forall e a b c.
  (Typeable a, Typeable b, CSETyped e c) =>
  BS.ByteString ->
  [BS.ByteString] ->
  Bool ->
  (e a -> e b -> e c) ->
  CSEExp e a ->
  CSEExp e b ->
  CSEExp e c
binOpTyped tag imms canTrap fn x y =
  mkExpOp
    tag
    (encodeTypeRep (TR.typeRep @a) : encodeTypeRep (TR.typeRep @b) : imms)
    canTrap
    (directBind @e @c)
    (HCons x (HCons y HNil))
    (\(HCons vx (HCons vy HNil)) -> fn vx vy)

ternOpTyped ::
  forall e a b c d.
  (Typeable a, Typeable b, Typeable c, CSETyped e d) =>
  BS.ByteString ->
  [BS.ByteString] ->
  Bool ->
  (e a -> e b -> e c -> e d) ->
  CSEExp e a ->
  CSEExp e b ->
  CSEExp e c ->
  CSEExp e d
ternOpTyped tag imms canTrap fn x y z =
  mkExpOp
    tag
    (encodeTypeRep (TR.typeRep @a) : encodeTypeRep (TR.typeRep @b) : encodeTypeRep (TR.typeRep @c) : imms)
    canTrap
    (directBind @e @d)
    (HCons x (HCons y (HCons z HNil)))
    (\(HCons vx (HCons vy (HCons vz HNil))) -> fn vx vy vz)

--------------------------------------------------------------------------------
-- Scalar & Algebraic Expression Instances on `CSEExp e`
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (Num (e a), CSETyped e a) => Num (CSEExp e a) where
  (+) = binOpTyped "Num.+" [] False (+)
  (-) = binOpTyped "Num.-" [] False (-)
  (*) = binOpTyped "Num.*" [] False (*)
  negate = unOpTyped "Num.negate" [] False negate
  abs = unOpTyped "Num.abs" [] False abs
  signum = unOpTyped "Num.signum" [] False signum
  fromInteger n = litTyped "Num.fromInteger" [encodeInteger n] (fromInteger n)

instance {-# OVERLAPPABLE #-} (Bounded (e a), CSETyped e a) => Bounded (CSEExp e a) where
  minBound = litTyped "Bounded.minBound" [] minBound
  maxBound = litTyped "Bounded.maxBound" [] maxBound

instance {-# OVERLAPPABLE #-} (Fractional (e a), CSETyped e a) => Fractional (CSEExp e a) where
  (/) = binOpTyped "Fractional./" [] False (/)
  recip = unOpTyped "Fractional.recip" [] False recip
  fromRational q = litTyped "Fractional.fromRational" [encodeRational q] (fromRational q)

instance {-# OVERLAPPABLE #-} (Floating (e a), CSETyped e a) => Floating (CSEExp e a) where
  pi = litTyped "Floating.pi" [] pi
  exp = unOpTyped "Floating.exp" [] False exp
  log = unOpTyped "Floating.log" [] False log
  sqrt = unOpTyped "Floating.sqrt" [] False sqrt
  (**) = binOpTyped "Floating.**" [] False (**)
  logBase = binOpTyped "Floating.logBase" [] False logBase
  sin = unOpTyped "Floating.sin" [] False sin
  cos = unOpTyped "Floating.cos" [] False cos
  tan = unOpTyped "Floating.tan" [] False tan
  asin = unOpTyped "Floating.asin" [] False asin
  acos = unOpTyped "Floating.acos" [] False acos
  atan = unOpTyped "Floating.atan" [] False atan
  sinh = unOpTyped "Floating.sinh" [] False sinh
  cosh = unOpTyped "Floating.cosh" [] False cosh
  tanh = unOpTyped "Floating.tanh" [] False tanh
  asinh = unOpTyped "Floating.asinh" [] False asinh
  acosh = unOpTyped "Floating.acosh" [] False acosh
  atanh = unOpTyped "Floating.atanh" [] False atanh
  log1p = unOpTyped "Floating.log1p" [] False log1p
  expm1 = unOpTyped "Floating.expm1" [] False expm1

instance {-# OVERLAPPABLE #-} (FMA (e a), CSETyped e a) => FMA (CSEExp e a) where
  fma = ternOpTyped "FMA.fma" [] False fma

instance {-# OVERLAPPABLE #-} (Hypot (e a), CSETyped e a) => Hypot (CSEExp e a) where
  hypot = binOpTyped "Hypot.hypot" [] False hypot

instance {-# OVERLAPPABLE #-} (Join (e a), CSETyped e a) => Join (CSEExp e a) where
  (\/) = binOpTyped "Join.\\/" [] False (\/)

instance {-# OVERLAPPING #-} (Join (e Bool)) => Join (CSEExp e Bool) where
  (\/) a b = mkExpShortCircuit SCLor a b (\/)

instance {-# OVERLAPPABLE #-} (Meet (e a), CSETyped e a) => Meet (CSEExp e a) where
  (/\) = binOpTyped "Meet./\\" [] False (/\)

instance {-# OVERLAPPING #-} (Meet (e Bool)) => Meet (CSEExp e Bool) where
  (/\) a b = mkExpShortCircuit SCLand a b (/\)

instance {-# OVERLAPPABLE #-} (Lower (e a), CSETyped e a) => Lower (CSEExp e a) where
  lowerBound = litTyped "Lower.lowerBound" [] lowerBound

instance {-# OVERLAPPING #-} (Lower (e Bool)) => Lower (CSEExp e Bool) where
  lowerBound = mkExpLit "Bool.false" [] (Just (TR.typeRep @Bool, BindBool)) lowerBound

instance {-# OVERLAPPABLE #-} (Upper (e a), CSETyped e a) => Upper (CSEExp e a) where
  upperBound = litTyped "Upper.upperBound" [] upperBound

instance {-# OVERLAPPING #-} (Upper (e Bool)) => Upper (CSEExp e Bool) where
  upperBound = mkExpLit "Bool.true" [] (Just (TR.typeRep @Bool, BindBool)) upperBound

instance (ExpBool e) => ExpBool (CSEExp e) where
  false = mkExpLit "Bool.false" [] (Just (TR.typeRep @Bool, BindBool)) false
  true = mkExpLit "Bool.true" [] (Just (TR.typeRep @Bool, BindBool)) true
  land a b = mkExpShortCircuit SCLand a b land
  lor a b = mkExpShortCircuit SCLor a b lor
  lnot a =
    mkExpOp
      "ExpBool.lnot"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a HNil)
      (\(HCons va HNil) -> lnot va)

instance (ExpEq e a) => ExpEq (CSEExp e) a where
  eq a b =
    mkExpOp
      "ExpEq.eq"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> eq va vb)
  ne a b =
    mkExpOp
      "ExpEq.ne"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> ne va vb)

instance (ExpOrd e a) => ExpOrd (CSEExp e) a where
  lt a b =
    mkExpOp
      "ExpOrd.lt"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> lt va vb)
  le a b =
    mkExpOp
      "ExpOrd.le"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> le va vb)
  gt a b =
    mkExpOp
      "ExpOrd.gt"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> gt va vb)
  ge a b =
    mkExpOp
      "ExpOrd.ge"
      []
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> ge va vb)

instance (ExpBits e a, CSETyped e a) => ExpBits (CSEExp e) a where
  iand = binOpTyped "ExpBits.iand" [] False iand
  ior = binOpTyped "ExpBits.ior" [] False ior
  ieor = binOpTyped "ExpBits.ieor" [] False ieor
  inot = unOpTyped "ExpBits.inot" [] False inot
  lshift = binOpTyped "ExpBits.lshift" [] False lshift
  rshift = binOpTyped "ExpBits.rshift" [] False rshift

instance (ExpBoolCast e a, CSETyped e a) => ExpBoolCast (CSEExp e) a where
  fromBool = unOpTyped "ExpBoolCast.fromBool" [] False fromBool
  toBool a =
    mkExpOp
      "ExpBoolCast.toBool"
      [encodeTypeRep (TR.typeRep @a)]
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons a HNil)
      (\(HCons va HNil) -> toBool va)

instance (ExpFloatingCast e a b, Typeable a, CSETyped e b) => ExpFloatingCast (CSEExp e) a b where
  toFloating = unOpTyped "ExpFloatingCast.toFloating" [] False toFloating

instance (ExpIntegralCast e a b, Typeable a, CSETyped e b) => ExpIntegralCast (CSEExp e) a b where
  toIntegral = unOpTyped "ExpIntegralCast.toIntegral" [] False toIntegral

instance (ExpPtrCast e) => ExpPtrCast (CSEExp e) where
  ptrCast ::
    forall a b.
    (CSETyped e a, CSETyped e b) =>
    CSEExp e (Ptr a) ->
    CSEExp e (Ptr b)
  ptrCast p =
    mkExpOp
      "ExpPtrCast.ptrCast"
      [encodeTypeRep (TR.typeRep @(Ptr a)), encodeTypeRep (TR.typeRep @(Ptr b))]
      False
      (Just (TR.typeRep @(Ptr b), BindPtrOf (BindDirect @e @b)))
      (HCons p HNil)
      (\(HCons vp HNil) -> ptrCast vp)
  toVoidPtr ::
    forall a.
    (CSETyped e a) =>
    CSEExp e (Ptr a) ->
    CSEExp e (Ptr ())
  toVoidPtr p =
    mkExpOp
      "ExpPtrCast.toVoidPtr"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      False
      (Just (TR.typeRep @(Ptr ()), BindVoidPtr))
      (HCons p HNil)
      (\(HCons vp HNil) -> toVoidPtr vp)
  fromVoidPtr ::
    forall a.
    (CSETyped e a) =>
    CSEExp e (Ptr ()) ->
    CSEExp e (Ptr a)
  fromVoidPtr p =
    mkExpOp
      "ExpPtrCast.fromVoidPtr"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      False
      (Just (TR.typeRep @(Ptr a), BindPtrOf (BindDirect @e @a)))
      (HCons p HNil)
      (\(HCons vp HNil) -> fromVoidPtr vp)

instance (ExpCond e) => ExpCond (CSEExp e) where
  ifThenElse ::
    forall a.
    (CSETyped e a) =>
    CSEExp e Bool ->
    CSEExp e a ->
    CSEExp e a ->
    CSEExp e a
  ifThenElse c t f = mkExpCond (TR.typeRep @a) (BindDirect @e @a) c t f ifThenElse

instance (CSETargetExp e, ExpExtract e a) => ExpExtract (CSEExp e) a where
  extract expr = extract (interpretCSEExp (cseExp expr))

instance (ExpInject e a, Show a) => ExpInject (CSEExp e) a where
  inject x = mkExpLit "ExpInject.inject" [encodeStr (show x)] Nothing (inject x)

instance (ExpIntegral e a, CSETyped e a) => ExpIntegral (CSEExp e) a where
  quotient = binOpTyped "ExpIntegral.quotient" [] True quotient
  remainder = binOpTyped "ExpIntegral.remainder" [] True remainder

instance (ExpComplex e a, CSETyped e a) => ExpComplex (CSEExp e) a where
  rePart = unOpTyped "ExpComplex.rePart" [] False rePart
  imPart = unOpTyped "ExpComplex.imPart" [] False imPart

instance (ExpMurmurHash e (e a)) => ExpMurmurHash (CSEExp e) (CSEExp e a) where
  murmurHashWithSalt salt x =
    mkExpOp
      "ExpMurmurHash.murmurHashWithSalt"
      []
      False
      (Just (TR.typeRep @Word64, BindWord64))
      (HCons salt (HCons x HNil))
      (\(HCons vs (HCons vx HNil)) -> murmurHashWithSalt vs vx)

instance (ExpSized e a, Typeable a) => ExpSized (CSEExp e) a where
  sizeOf p =
    mkExpLit
      "ExpSized.sizeOf"
      [encodeTypeRep (TR.typeRep @a)]
      (Just (TR.typeRep @Int64, BindInt64))
      (sizeOf p)
  alignOf p =
    mkExpLit
      "ExpSized.alignOf"
      [encodeTypeRep (TR.typeRep @a)]
      (Just (TR.typeRep @Int64, BindInt64))
      (alignOf p)

instance (ExpPtr e a, KnownJanusType e a, Typeable a) => ExpPtr (CSEExp e) a where
  nullPtr =
    mkExpLit
      "ExpPtr.nullPtr"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      (Just (TR.typeRep @(Ptr a), BindPtrOf (BindDirect @e @a)))
      nullPtr
  ptrAdd p i =
    mkExpOp
      "ExpPtr.ptrAdd"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      False
      (Just (TR.typeRep @(Ptr a), BindPtrOf (BindDirect @e @a)))
      (HCons p (HCons i HNil))
      (\(HCons vp (HCons vi HNil)) -> ptrAdd vp vi)
  ptrIndex p i =
    mkExpOp
      "ExpPtr.ptrIndex"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      False
      (Just (TR.typeRep @(Ptr a), BindPtrOf (BindDirect @e @a)))
      (HCons p (HCons i HNil))
      (\(HCons vp (HCons vi HNil)) -> ptrIndex vp vi)

instance (ExpPtrWord e) => ExpPtrWord (CSEExp e) where
  ptrToWord64Expr p =
    mkExpOp
      "ExpPtrWord.ptrToWord64"
      []
      False
      (Just (TR.typeRep @Word64, BindWord64))
      (HCons p HNil)
      (\(HCons vp HNil) -> ptrToWord64Expr vp)
  word64ToPtrExpr ::
    forall a.
    (CSETyped e a) =>
    CSEExp e Word64 ->
    CSEExp e (Ptr a)
  word64ToPtrExpr w =
    mkExpOp
      "ExpPtrWord.word64ToPtr"
      [encodeTypeRep (TR.typeRep @(Ptr a))]
      False
      (Just (TR.typeRep @(Ptr a), BindPtrOf (BindDirect @e @a)))
      (HCons w HNil)
      (\(HCons vw HNil) -> word64ToPtrExpr vw)

instance (ExpStructPtr e) => ExpStructPtr (CSEExp e) where
  structFieldPtr ::
    forall structTy field.
    (CSETyped e field) =>
    CSEExp e (Ptr structTy) ->
    Int ->
    String ->
    CSEExp e (Ptr field)
  structFieldPtr p off fname =
    mkExpOp
      "ExpStructPtr.structFieldPtr"
      [encodeInt off, encodeStr fname, encodeTypeRep (TR.typeRep @(Ptr field))]
      False
      (Just (TR.typeRep @(Ptr field), BindPtrOf (BindDirect @e @field)))
      (HCons p HNil)
      (\(HCons vp HNil) -> structFieldPtr vp off fname)

instance (ExpPair e) => ExpPair (CSEExp e) where
  mkPair ::
    forall a b.
    (CSETyped e a, CSETyped e b) =>
    CSEExp e a ->
    CSEExp e b ->
    CSEExp e (SIMDPair a b)
  mkPair a b =
    mkExpOp
      "ExpPair.mkPair"
      [encodeTypeRep (TR.typeRep @a), encodeTypeRep (TR.typeRep @b)]
      False
      (Just (TR.typeRep @(SIMDPair a b), BindPairOf (BindDirect @e @a) (BindDirect @e @b)))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> mkPair va vb)
  fstPair ::
    forall a b.
    (CSETyped e a, CSETyped e b) =>
    CSEExp e (SIMDPair a b) ->
    CSEExp e a
  fstPair p =
    mkExpOp
      "ExpPair.fstPair"
      [encodeTypeRep (TR.typeRep @(SIMDPair a b)), encodeTypeRep (TR.typeRep @a)]
      False
      (directBind @e @a)
      (HCons p HNil)
      (\(HCons vp HNil) -> fstPair vp)
  sndPair ::
    forall a b.
    (CSETyped e a, CSETyped e b) =>
    CSEExp e (SIMDPair a b) ->
    CSEExp e b
  sndPair p =
    mkExpOp
      "ExpPair.sndPair"
      [encodeTypeRep (TR.typeRep @(SIMDPair a b)), encodeTypeRep (TR.typeRep @b)]
      False
      (directBind @e @b)
      (HCons p HNil)
      (\(HCons vp HNil) -> sndPair vp)
  mkVoidPtr p =
    mkExpZeroCostOp
      "ExpPair.mkVoidPtr"
      []
      Nothing
      (HCons p HNil)
      (\(HCons vp HNil) -> mkVoidPtr vp)
  unVoidPtr p =
    mkExpZeroCostOp
      "ExpPair.unVoidPtr"
      []
      (Just (TR.typeRep @(Ptr ()), BindVoidPtr))
      (HCons p HNil)
      (\(HCons vp HNil) -> unVoidPtr vp)
  mkSPMDUnit = mkExpLit "ExpPair.mkSPMDUnit" [] Nothing mkSPMDUnit
  coerceExp ::
    forall a b.
    (Coercible a b, CSETyped e b) =>
    CSEExp e a ->
    CSEExp e b
  coerceExp x =
    mkExpOp
      "ExpPair.coerceExp"
      [encodeTypeRep (TR.typeRep @b)]
      False
      (directBind @e @b)
      (HCons x HNil)
      (\(HCons vx HNil) -> coerceExp vx)

--------------------------------------------------------------------------------
-- Local Recursive Functions (`CSEFix` & `ExpFix`)
--------------------------------------------------------------------------------

class CSEFixField (e :: K.Type -> K.Type) (fieldCSE :: K.Type) | fieldCSE -> e where
  type CSEFixFieldTarget fieldCSE :: K.Type
  type CSEFixFieldPrepend e fieldCSE (tail :: [K.Type]) :: [K.Type]
  cseFixFieldTypes :: Proxy e -> Proxy fieldCSE -> [TR.SomeTypeRep]
  cseFixFieldRetSpec :: HRetSpec e tail -> HRetSpec e (CSEFixFieldPrepend e fieldCSE tail)
  cseFixFieldAlloc ::
    Digest ->
    Int ->
    HArgs (VarId e) tail ->
    (Int, HArgs (VarId e) (CSEFixFieldPrepend e fieldCSE tail), fieldCSE)
  cseFixFieldExtract :: fieldCSE -> HArgs (CSEExp e) tail -> HArgs (CSEExp e) (CSEFixFieldPrepend e fieldCSE tail)
  cseFixFieldRebuild :: HArgs (CSEExp e) (CSEFixFieldPrepend e fieldCSE tail) -> (fieldCSE, HArgs (CSEExp e) tail)
  cseFixFieldConsumeTarget ::
    (CSETargetExp e) =>
    HArgs e (CSEFixFieldPrepend e fieldCSE tail) ->
    (CSEFixFieldTarget fieldCSE, HArgs e tail)
  cseFixFieldPrependTarget ::
    (CSETargetExp e) =>
    CSEFixFieldTarget fieldCSE ->
    HArgs e tail ->
    HArgs e (CSEFixFieldPrepend e fieldCSE tail)

instance (CSEExpParamLeaf e a, Typeable a) => CSEFixField e (CSEExp e a) where
  type CSEFixFieldTarget (CSEExp e a) = e (TargetExpElem e a)
  type CSEFixFieldPrepend e (CSEExp e a) tail = a ': tail
  cseFixFieldTypes _ _ = [TR.SomeTypeRep (TR.typeRep @a)]
  cseFixFieldRetSpec rest = HRSCons (TR.typeRep @a) (cseLeafBindCap @e @a) rest
  cseFixFieldAlloc fk idx rest =
    let vid = VarId (VarLocalFun fk idx) (TR.typeRep @a) (Just (cseLeafBindCap @e @a))
     in (idx + 1, HCons vid rest, mkExpVar vid)
  cseFixFieldExtract x rest = HCons x rest
  cseFixFieldRebuild (HCons x rest) = (x, rest)
  cseFixFieldConsumeTarget (HCons v rest) = (coerceExpNewtype v, rest)
  cseFixFieldPrependTarget tgt rest = HCons (coerceExpNewtype tgt) rest

instance (CSETyped e (Ptr a)) => CSEFixField e (PtrOf (CSEExp e) a) where
  type CSEFixFieldTarget (PtrOf (CSEExp e) a) = PtrOf e a
  type CSEFixFieldPrepend e (PtrOf (CSEExp e) a) tail = Ptr a ': tail
  cseFixFieldTypes _ _ = [TR.SomeTypeRep (TR.typeRep @(Ptr a))]
  cseFixFieldRetSpec rest = HRSCons (TR.typeRep @(Ptr a)) (BindDirect @e @(Ptr a)) rest
  cseFixFieldAlloc fk idx rest =
    let vid = VarId (VarLocalFun fk idx) (TR.typeRep @(Ptr a)) (Just (BindDirect @e @(Ptr a)))
     in (idx + 1, HCons vid rest, PtrOf (mkExpVar vid))
  cseFixFieldExtract (PtrOf p) rest = HCons p rest
  cseFixFieldRebuild (HCons p rest) = (PtrOf p, rest)
  cseFixFieldConsumeTarget (HCons v rest) = (PtrOf v, rest)
  cseFixFieldPrependTarget (PtrOf tgt) rest = HCons tgt rest

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEFixLeaves e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  CSEFixField e (sub (PtrOf (CSEExp e)))
  where
  type CSEFixFieldTarget (sub (PtrOf (CSEExp e))) = sub (PtrOf e)
  type CSEFixFieldPrepend e (sub (PtrOf (CSEExp e))) tail = GCSEFixPrepend e (Rep (sub (PtrOf (CSEExp e)))) tail
  cseFixFieldTypes pe _ = gFixTypes @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) pe
  cseFixFieldRetSpec = gFixRetSpec @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e)))
  cseFixFieldAlloc fk idx0 rest =
    let (idx1, vids, repF) = gFixAlloc @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) fk idx0 rest
     in (idx1, vids, to repF)
  cseFixFieldExtract s = gFixExtract @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) (from s)
  cseFixFieldRebuild hs0 =
    let (repS, hs1) = gFixRebuild @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) hs0
     in (to repS, hs1)
  cseFixFieldConsumeTarget hs0 =
    let (repT, hs1) = gFixConsumeTarget @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) hs0
     in (to repT, hs1)
  cseFixFieldPrependTarget s =
    gFixPrependTarget @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) (from s)

instance
  ( Generic (sub (CSEExp e)),
    Generic (sub e),
    GCSEFixLeaves e (Rep (sub (CSEExp e))) (Rep (sub e))
  ) =>
  CSEFixField e (sub (CSEExp e))
  where
  type CSEFixFieldTarget (sub (CSEExp e)) = sub e
  type CSEFixFieldPrepend e (sub (CSEExp e)) tail = GCSEFixPrepend e (Rep (sub (CSEExp e))) tail
  cseFixFieldTypes pe _ = gFixTypes @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) pe
  cseFixFieldRetSpec = gFixRetSpec @e @(Rep (sub (CSEExp e))) @(Rep (sub e))
  cseFixFieldAlloc fk idx0 rest =
    let (idx1, vids, repF) = gFixAlloc @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) fk idx0 rest
     in (idx1, vids, to repF)
  cseFixFieldExtract s = gFixExtract @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) (from s)
  cseFixFieldRebuild hs0 =
    let (repS, hs1) = gFixRebuild @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) hs0
     in (to repS, hs1)
  cseFixFieldConsumeTarget hs0 =
    let (repT, hs1) = gFixConsumeTarget @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) hs0
     in (to repT, hs1)
  cseFixFieldPrependTarget s =
    gFixPrependTarget @e @(Rep (sub (CSEExp e))) @(Rep (sub e)) (from s)

class GCSEFixLeaves (e :: K.Type -> K.Type) (repCSE :: K.Type -> K.Type) (repTgt :: K.Type -> K.Type) where
  type GCSEFixPrepend e repCSE (tail :: [K.Type]) :: [K.Type]
  gFixTypes :: Proxy e -> [TR.SomeTypeRep]
  gFixRetSpec :: HRetSpec e tail -> HRetSpec e (GCSEFixPrepend e repCSE tail)
  gFixAlloc ::
    Digest ->
    Int ->
    HArgs (VarId e) tail ->
    (Int, HArgs (VarId e) (GCSEFixPrepend e repCSE tail), repCSE p)
  gFixExtract :: repCSE p -> HArgs (CSEExp e) tail -> HArgs (CSEExp e) (GCSEFixPrepend e repCSE tail)
  gFixRebuild :: HArgs (CSEExp e) (GCSEFixPrepend e repCSE tail) -> (repCSE p, HArgs (CSEExp e) tail)
  gFixConsumeTarget :: (CSETargetExp e) => HArgs e (GCSEFixPrepend e repCSE tail) -> (repTgt p, HArgs e tail)
  gFixPrependTarget :: (CSETargetExp e) => repTgt p -> HArgs e tail -> HArgs e (GCSEFixPrepend e repCSE tail)

instance (GCSEFixLeaves e fCSE fTgt) => GCSEFixLeaves e (M1 i c fCSE) (M1 i c fTgt) where
  type GCSEFixPrepend e (M1 i c fCSE) tail = GCSEFixPrepend e fCSE tail
  gFixTypes pe = gFixTypes @e @fCSE @fTgt pe
  gFixRetSpec = gFixRetSpec @e @fCSE @fTgt
  gFixAlloc fk idx0 rest =
    let (idx1, vids, fVal) = gFixAlloc @e @fCSE @fTgt fk idx0 rest
     in (idx1, vids, M1 fVal)
  gFixExtract (M1 x) = gFixExtract @e @fCSE @fTgt x
  gFixRebuild hs0 =
    let (x, hs1) = gFixRebuild @e @fCSE @fTgt hs0
     in (M1 x, hs1)
  gFixConsumeTarget hs0 =
    let (x, hs1) = gFixConsumeTarget @e @fCSE @fTgt hs0
     in (M1 x, hs1)
  gFixPrependTarget (M1 x) = gFixPrependTarget @e @fCSE @fTgt x

instance GCSEFixLeaves e U1 U1 where
  type GCSEFixPrepend e U1 tail = tail
  gFixTypes _ = []
  gFixRetSpec = id
  gFixAlloc _ idx rest = (idx, rest, U1)
  gFixExtract U1 = id
  gFixRebuild hs = (U1, hs)
  gFixConsumeTarget hs = (U1, hs)
  gFixPrependTarget U1 = id

instance
  (GCSEFixLeaves e fCSE fTgt, GCSEFixLeaves e gCSE gTgt) =>
  GCSEFixLeaves e (fCSE :*: gCSE) (fTgt :*: gTgt)
  where
  type GCSEFixPrepend e (fCSE :*: gCSE) tail = GCSEFixPrepend e fCSE (GCSEFixPrepend e gCSE tail)
  gFixTypes pe = gFixTypes @e @fCSE @fTgt pe <> gFixTypes @e @gCSE @gTgt pe
  gFixRetSpec = gFixRetSpec @e @fCSE @fTgt . gFixRetSpec @e @gCSE @gTgt
  gFixAlloc fk idx0 rest =
    let (idx1, fVids, fVal) = gFixAlloc @e @fCSE @fTgt fk idx0 gVids
        (idx2, gVids, gVal) = gFixAlloc @e @gCSE @gTgt fk idx1 rest
     in (idx2, fVids, fVal :*: gVal)
  gFixExtract (fx :*: gx) = gFixExtract @e @fCSE @fTgt fx . gFixExtract @e @gCSE @gTgt gx
  gFixRebuild hs0 =
    let (fx, hs1) = gFixRebuild @e @fCSE @fTgt hs0
        (gx, hs2) = gFixRebuild @e @gCSE @gTgt hs1
     in (fx :*: gx, hs2)
  gFixConsumeTarget hs0 =
    let (fx, hs1) = gFixConsumeTarget @e @fCSE @fTgt hs0
        (gx, hs2) = gFixConsumeTarget @e @gCSE @gTgt hs1
     in (fx :*: gx, hs2)
  gFixPrependTarget (fx :*: gx) =
    gFixPrependTarget @e @fCSE @fTgt fx . gFixPrependTarget @e @gCSE @gTgt gx

instance
  (CSEFixField e fieldCSE, CSEFixFieldTarget fieldCSE ~ fieldTgt) =>
  GCSEFixLeaves e (K1 i fieldCSE) (K1 i fieldTgt)
  where
  type GCSEFixPrepend e (K1 i fieldCSE) tail = CSEFixFieldPrepend e fieldCSE tail
  gFixTypes pe = cseFixFieldTypes pe (Proxy @fieldCSE)
  gFixRetSpec = cseFixFieldRetSpec @e @fieldCSE
  gFixAlloc fk idx0 rest =
    let (idx1, vids, x) = cseFixFieldAlloc @e @fieldCSE fk idx0 rest
     in (idx1, vids, K1 x)
  gFixExtract (K1 x) = cseFixFieldExtract @e @fieldCSE x
  gFixRebuild hs0 =
    let (x, hs1) = cseFixFieldRebuild @e @fieldCSE hs0
     in (K1 x, hs1)
  gFixConsumeTarget hs0 =
    let (x, hs1) = cseFixFieldConsumeTarget @e @fieldCSE hs0
     in (K1 x, hs1)
  gFixPrependTarget (K1 x) = cseFixFieldPrependTarget @e @fieldCSE x

class CSEFixParam (e :: K.Type -> K.Type) (p :: K.Type) where
  type CSEFixParamTarget e p :: K.Type
  type CSEFixParamPrepend e p (tail :: [K.Type]) :: [K.Type]
  type CSEFixParamPrepend e p tail = GCSEFixPrepend e (Rep p) tail

  cseFixParamTypes :: Proxy e -> Proxy p -> [TR.SomeTypeRep]
  default cseFixParamTypes ::
    (GCSEFixLeaves e (Rep p) (Rep (CSEFixParamTarget e p))) =>
    Proxy e ->
    Proxy p ->
    [TR.SomeTypeRep]
  cseFixParamTypes pe _ = gFixTypes @e @(Rep p) @(Rep (CSEFixParamTarget e p)) pe

  cseFixAllocParam ::
    Digest ->
    Int ->
    HArgs (VarId e) tail ->
    (Int, HArgs (VarId e) (CSEFixParamPrepend e p tail), p)
  default cseFixAllocParam ::
    ( Generic p,
      GCSEFixLeaves e (Rep p) (Rep (CSEFixParamTarget e p)),
      CSEFixParamPrepend e p tail ~ GCSEFixPrepend e (Rep p) tail
    ) =>
    Digest ->
    Int ->
    HArgs (VarId e) tail ->
    (Int, HArgs (VarId e) (CSEFixParamPrepend e p tail), p)
  cseFixAllocParam fk idx0 rest =
    let (idx1, vids, repP) = gFixAlloc @e @(Rep p) @(Rep (CSEFixParamTarget e p)) fk idx0 rest
     in (idx1, vids, to repP)

  cseFixExtractParam :: p -> HArgs (CSEExp e) tail -> HArgs (CSEExp e) (CSEFixParamPrepend e p tail)
  default cseFixExtractParam ::
    ( Generic p,
      GCSEFixLeaves e (Rep p) (Rep (CSEFixParamTarget e p)),
      CSEFixParamPrepend e p tail ~ GCSEFixPrepend e (Rep p) tail
    ) =>
    p ->
    HArgs (CSEExp e) tail ->
    HArgs (CSEExp e) (CSEFixParamPrepend e p tail)
  cseFixExtractParam x = gFixExtract @e @(Rep p) @(Rep (CSEFixParamTarget e p)) (from x)

  cseFixConsumeParamTarget ::
    (CSETargetExp e) =>
    HArgs e (CSEFixParamPrepend e p tail) ->
    (CSEFixParamTarget e p, HArgs e tail)
  default cseFixConsumeParamTarget ::
    ( CSETargetExp e,
      Generic (CSEFixParamTarget e p),
      GCSEFixLeaves e (Rep p) (Rep (CSEFixParamTarget e p)),
      CSEFixParamPrepend e p tail ~ GCSEFixPrepend e (Rep p) tail
    ) =>
    HArgs e (CSEFixParamPrepend e p tail) ->
    (CSEFixParamTarget e p, HArgs e tail)
  cseFixConsumeParamTarget hs0 =
    let (repT, hs1) = gFixConsumeTarget @e @(Rep p) @(Rep (CSEFixParamTarget e p)) hs0
     in (to repT, hs1)

  cseFixPrependParamTarget ::
    (CSETargetExp e) =>
    CSEFixParamTarget e p ->
    HArgs e tail ->
    HArgs e (CSEFixParamPrepend e p tail)
  default cseFixPrependParamTarget ::
    ( CSETargetExp e,
      Generic (CSEFixParamTarget e p),
      GCSEFixLeaves e (Rep p) (Rep (CSEFixParamTarget e p)),
      CSEFixParamPrepend e p tail ~ GCSEFixPrepend e (Rep p) tail
    ) =>
    CSEFixParamTarget e p ->
    HArgs e tail ->
    HArgs e (CSEFixParamPrepend e p tail)
  cseFixPrependParamTarget tgt =
    gFixPrependTarget @e @(Rep p) @(Rep (CSEFixParamTarget e p)) (from tgt)

instance (CSEExpParamLeaf e a, Typeable a) => CSEFixParam e (CSEExp e a) where
  type CSEFixParamTarget e (CSEExp e a) = e (TargetExpElem e a)
  type CSEFixParamPrepend e (CSEExp e a) tail = a ': tail
  cseFixParamTypes _ _ = [TR.SomeTypeRep (TR.typeRep @a)]
  cseFixAllocParam fk idx rest =
    let vid = VarId (VarLocalFun fk idx) (TR.typeRep @a) (Just (cseLeafBindCap @e @a))
     in (idx + 1, HCons vid rest, mkExpVar vid)
  cseFixExtractParam x rest = HCons x rest
  cseFixConsumeParamTarget (HCons v rest) = (coerceExpNewtype v, rest)
  cseFixPrependParamTarget tgt rest = HCons (coerceExpNewtype tgt) rest

instance CSEFixParam e () where
  type CSEFixParamTarget e () = ()
  type CSEFixParamPrepend e () tail = tail
  cseFixParamTypes _ _ = []
  cseFixAllocParam _ idx rest = (idx, rest, ())
  cseFixExtractParam () = id
  cseFixConsumeParamTarget hs = ((), hs)
  cseFixPrependParamTarget () = id

instance (CSEFixParam e p1, CSEFixParam e p2) => CSEFixParam e (p1, p2) where
  type CSEFixParamTarget e (p1, p2) = (CSEFixParamTarget e p1, CSEFixParamTarget e p2)
  type CSEFixParamPrepend e (p1, p2) tail = CSEFixParamPrepend e p1 (CSEFixParamPrepend e p2 tail)
  cseFixParamTypes pe _ = cseFixParamTypes pe (Proxy @p1) <> cseFixParamTypes pe (Proxy @p2)
  cseFixAllocParam fk idx0 rest =
    let (idx1, vids1, f1) = cseFixAllocParam @e @p1 fk idx0 vids2
        (idx2, vids2, f2) = cseFixAllocParam @e @p2 fk idx1 rest
     in (idx2, vids1, (f1, f2))
  cseFixExtractParam (a, b) = cseFixExtractParam @e @p1 a . cseFixExtractParam @e @p2 b
  cseFixConsumeParamTarget hs0 =
    let (t1, hs1) = cseFixConsumeParamTarget @e @p1 hs0
        (t2, hs2) = cseFixConsumeParamTarget @e @p2 hs1
     in ((t1, t2), hs2)
  cseFixPrependParamTarget (t1, t2) =
    cseFixPrependParamTarget @e @p1 t1 . cseFixPrependParamTarget @e @p2 t2

instance (CSEFixParam e p1, CSEFixParam e p2, CSEFixParam e p3) => CSEFixParam e (p1, p2, p3) where
  type CSEFixParamTarget e (p1, p2, p3) = (CSEFixParamTarget e p1, CSEFixParamTarget e p2, CSEFixParamTarget e p3)
  type
    CSEFixParamPrepend e (p1, p2, p3) tail =
      CSEFixParamPrepend e p1 (CSEFixParamPrepend e p2 (CSEFixParamPrepend e p3 tail))
  cseFixParamTypes pe _ =
    cseFixParamTypes pe (Proxy @p1) <> cseFixParamTypes pe (Proxy @p2) <> cseFixParamTypes pe (Proxy @p3)
  cseFixAllocParam fk idx0 rest =
    let (idx1, vids1, f1) = cseFixAllocParam @e @p1 fk idx0 vids2
        (idx2, vids2, f2) = cseFixAllocParam @e @p2 fk idx1 vids3
        (idx3, vids3, f3) = cseFixAllocParam @e @p3 fk idx2 rest
     in (idx3, vids1, (f1, f2, f3))
  cseFixExtractParam (a, b, c) =
    cseFixExtractParam @e @p1 a . cseFixExtractParam @e @p2 b . cseFixExtractParam @e @p3 c
  cseFixConsumeParamTarget hs0 =
    let (t1, hs1) = cseFixConsumeParamTarget @e @p1 hs0
        (t2, hs2) = cseFixConsumeParamTarget @e @p2 hs1
        (t3, hs3) = cseFixConsumeParamTarget @e @p3 hs2
     in ((t1, t2, t3), hs3)
  cseFixPrependParamTarget (t1, t2, t3) =
    cseFixPrependParamTarget @e @p1 t1
      . cseFixPrependParamTarget @e @p2 t2
      . cseFixPrependParamTarget @e @p3 t3

instance
  (CSEFixParam e p1, CSEFixParam e p2, CSEFixParam e p3, CSEFixParam e p4) =>
  CSEFixParam e (p1, p2, p3, p4)
  where
  type
    CSEFixParamTarget e (p1, p2, p3, p4) =
      (CSEFixParamTarget e p1, CSEFixParamTarget e p2, CSEFixParamTarget e p3, CSEFixParamTarget e p4)
  type
    CSEFixParamPrepend e (p1, p2, p3, p4) tail =
      CSEFixParamPrepend e p1 (CSEFixParamPrepend e p2 (CSEFixParamPrepend e p3 (CSEFixParamPrepend e p4 tail)))
  cseFixParamTypes pe _ =
    cseFixParamTypes pe (Proxy @p1)
      <> cseFixParamTypes pe (Proxy @p2)
      <> cseFixParamTypes pe (Proxy @p3)
      <> cseFixParamTypes pe (Proxy @p4)
  cseFixAllocParam fk idx0 rest =
    let (idx1, vids1, f1) = cseFixAllocParam @e @p1 fk idx0 vids2
        (idx2, vids2, f2) = cseFixAllocParam @e @p2 fk idx1 vids3
        (idx3, vids3, f3) = cseFixAllocParam @e @p3 fk idx2 vids4
        (idx4, vids4, f4) = cseFixAllocParam @e @p4 fk idx3 rest
     in (idx4, vids1, (f1, f2, f3, f4))
  cseFixExtractParam (a, b, c, d) =
    cseFixExtractParam @e @p1 a
      . cseFixExtractParam @e @p2 b
      . cseFixExtractParam @e @p3 c
      . cseFixExtractParam @e @p4 d
  cseFixConsumeParamTarget hs0 =
    let (t1, hs1) = cseFixConsumeParamTarget @e @p1 hs0
        (t2, hs2) = cseFixConsumeParamTarget @e @p2 hs1
        (t3, hs3) = cseFixConsumeParamTarget @e @p3 hs2
        (t4, hs4) = cseFixConsumeParamTarget @e @p4 hs3
     in ((t1, t2, t3, t4), hs4)
  cseFixPrependParamTarget (t1, t2, t3, t4) =
    cseFixPrependParamTarget @e @p1 t1
      . cseFixPrependParamTarget @e @p2 t2
      . cseFixPrependParamTarget @e @p3 t3
      . cseFixPrependParamTarget @e @p4 t4

instance
  ( KnownNat n,
    Typeable n,
    Typeable a,
    SPMDTyped n (CSEExp e) a,
    SPMDTyped n e a,
    CSEExpParamLeaf e (SPMDRep n (CSEExp e) a),
    TargetExpElem e (SPMDRep n (CSEExp e) a) ~ SPMDRep n e a,
    Typeable (SPMDRep n (CSEExp e) a)
  ) =>
  CSEFixParam e (SPMD n (CSEExp e) a)
  where
  type CSEFixParamTarget e (SPMD n (CSEExp e) a) = SPMD n e a
  type CSEFixParamPrepend e (SPMD n (CSEExp e) a) tail = SPMDRep n (CSEExp e) a ': tail
  cseFixParamTypes _ _ =
    [ TR.SomeTypeRep (TR.typeRep @n),
      TR.SomeTypeRep (TR.typeRep @a),
      TR.SomeTypeRep (TR.typeRep @(SPMDRep n (CSEExp e) a))
    ]
  cseFixAllocParam fk idx rest =
    let tr = TR.typeRep @(SPMDRep n (CSEExp e) a)
        cap = cseLeafBindCap @e @(SPMDRep n (CSEExp e) a)
        vid = VarId (VarLocalFun fk idx) tr (Just cap)
     in (idx + 1, HCons vid rest, SPMD (pure (spmdDematerialize @n @(CSEExp e) @a (mkExpVar vid))))
  cseFixExtractParam x rest =
    HCons (spmdMaterialize @n @(CSEExp e) @a (runSPMD x)) rest
  cseFixConsumeParamTarget (HCons v rest) =
    (SPMD (pure (spmdDematerialize @n @e @a (coerceExpNewtype v))), rest)
  cseFixPrependParamTarget tgt rest =
    HCons (coerceExpNewtype (spmdMaterialize @n @e @a (runSPMD tgt))) rest

instance
  ( Generic (d (CSEExp e)),
    Generic (d e),
    GCSEFixLeaves e (Rep (d (CSEExp e))) (Rep (d e))
  ) =>
  CSEFixParam e (HKD d (CSEExp e))
  where
  type CSEFixParamTarget e (HKD d (CSEExp e)) = HKD d e

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEFixLeaves e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  CSEFixParam e (PtrStruct sub (CSEExp e))
  where
  type CSEFixParamTarget e (PtrStruct sub (CSEExp e)) = PtrStruct sub e

class CSEFixMonRet (e :: K.Type -> K.Type) (res :: K.Type) where
  type CSEFixRetTarget e res :: K.Type
  type CSEFixRetPrepend e res (tail :: [K.Type]) :: [K.Type]
  type CSEFixRetPrepend e res tail = GCSEFixPrepend e (Rep res) tail

  cseFixRetTypes :: Proxy e -> Proxy res -> [TR.SomeTypeRep]
  default cseFixRetTypes ::
    (GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res))) =>
    Proxy e ->
    Proxy res ->
    [TR.SomeTypeRep]
  cseFixRetTypes pe _ = gFixTypes @e @(Rep res) @(Rep (CSEFixRetTarget e res)) pe

  cseFixRetSpec :: HRetSpec e tail -> HRetSpec e (CSEFixRetPrepend e res tail)
  default cseFixRetSpec ::
    ( GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res)),
      CSEFixRetPrepend e res tail ~ GCSEFixPrepend e (Rep res) tail
    ) =>
    HRetSpec e tail ->
    HRetSpec e (CSEFixRetPrepend e res tail)
  cseFixRetSpec = gFixRetSpec @e @(Rep res) @(Rep (CSEFixRetTarget e res))

  cseFixExtractRet :: res -> HArgs (CSEExp e) tail -> HArgs (CSEExp e) (CSEFixRetPrepend e res tail)
  default cseFixExtractRet ::
    ( Generic res,
      GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res)),
      CSEFixRetPrepend e res tail ~ GCSEFixPrepend e (Rep res) tail
    ) =>
    res ->
    HArgs (CSEExp e) tail ->
    HArgs (CSEExp e) (CSEFixRetPrepend e res tail)
  cseFixExtractRet x = gFixExtract @e @(Rep res) @(Rep (CSEFixRetTarget e res)) (from x)

  cseFixRebuildRet :: HArgs (CSEExp e) (CSEFixRetPrepend e res tail) -> (res, HArgs (CSEExp e) tail)
  default cseFixRebuildRet ::
    ( Generic res,
      GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res)),
      CSEFixRetPrepend e res tail ~ GCSEFixPrepend e (Rep res) tail
    ) =>
    HArgs (CSEExp e) (CSEFixRetPrepend e res tail) ->
    (res, HArgs (CSEExp e) tail)
  cseFixRebuildRet hs0 =
    let (repR, hs1) = gFixRebuild @e @(Rep res) @(Rep (CSEFixRetTarget e res)) hs0
     in (to repR, hs1)

  cseFixHArgsToTarget ::
    (CSETargetExp e) =>
    HArgs e (CSEFixRetPrepend e res tail) ->
    (CSEFixRetTarget e res, HArgs e tail)
  default cseFixHArgsToTarget ::
    ( CSETargetExp e,
      Generic (CSEFixRetTarget e res),
      GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res)),
      CSEFixRetPrepend e res tail ~ GCSEFixPrepend e (Rep res) tail
    ) =>
    HArgs e (CSEFixRetPrepend e res tail) ->
    (CSEFixRetTarget e res, HArgs e tail)
  cseFixHArgsToTarget hs0 =
    let (repT, hs1) = gFixConsumeTarget @e @(Rep res) @(Rep (CSEFixRetTarget e res)) hs0
     in (to repT, hs1)

  cseFixTargetToHArgs ::
    (CSETargetExp e) =>
    CSEFixRetTarget e res ->
    HArgs e tail ->
    HArgs e (CSEFixRetPrepend e res tail)
  default cseFixTargetToHArgs ::
    ( CSETargetExp e,
      Generic (CSEFixRetTarget e res),
      GCSEFixLeaves e (Rep res) (Rep (CSEFixRetTarget e res)),
      CSEFixRetPrepend e res tail ~ GCSEFixPrepend e (Rep res) tail
    ) =>
    CSEFixRetTarget e res ->
    HArgs e tail ->
    HArgs e (CSEFixRetPrepend e res tail)
  cseFixTargetToHArgs tgt =
    gFixPrependTarget @e @(Rep res) @(Rep (CSEFixRetTarget e res)) (from tgt)

type CSEFixRets e res = CSEFixRetPrepend e res '[]

instance CSEFixMonRet e () where
  type CSEFixRetTarget e () = ()
  type CSEFixRetPrepend e () tail = tail
  cseFixRetTypes _ _ = []
  cseFixRetSpec = id
  cseFixExtractRet () = id
  cseFixRebuildRet hs = ((), hs)
  cseFixHArgsToTarget hs = ((), hs)
  cseFixTargetToHArgs () = id

instance (CSEExpParamLeaf e a, Typeable a) => CSEFixMonRet e (CSEExp e a) where
  type CSEFixRetTarget e (CSEExp e a) = e (TargetExpElem e a)
  type CSEFixRetPrepend e (CSEExp e a) tail = a ': tail
  cseFixRetTypes _ _ = [TR.SomeTypeRep (TR.typeRep @a)]
  cseFixRetSpec rest = HRSCons (TR.typeRep @a) (cseLeafBindCap @e @a) rest
  cseFixExtractRet x rest = HCons x rest
  cseFixRebuildRet (HCons x rest) = (x, rest)
  cseFixHArgsToTarget (HCons v rest) = (coerceExpNewtype v, rest)
  cseFixTargetToHArgs tgt rest = HCons (coerceExpNewtype tgt) rest

instance (CSEFixMonRet e r1, CSEFixMonRet e r2) => CSEFixMonRet e (r1, r2) where
  type CSEFixRetTarget e (r1, r2) = (CSEFixRetTarget e r1, CSEFixRetTarget e r2)
  type CSEFixRetPrepend e (r1, r2) tail = CSEFixRetPrepend e r1 (CSEFixRetPrepend e r2 tail)
  cseFixRetTypes pe _ = cseFixRetTypes pe (Proxy @r1) <> cseFixRetTypes pe (Proxy @r2)
  cseFixRetSpec = cseFixRetSpec @e @r1 . cseFixRetSpec @e @r2
  cseFixExtractRet (a, b) = cseFixExtractRet @e @r1 a . cseFixExtractRet @e @r2 b
  cseFixRebuildRet hs0 =
    let (a, hs1) = cseFixRebuildRet @e @r1 hs0
        (b, hs2) = cseFixRebuildRet @e @r2 hs1
     in ((a, b), hs2)
  cseFixHArgsToTarget hs0 =
    let (t1, hs1) = cseFixHArgsToTarget @e @r1 hs0
        (t2, hs2) = cseFixHArgsToTarget @e @r2 hs1
     in ((t1, t2), hs2)
  cseFixTargetToHArgs (t1, t2) =
    cseFixTargetToHArgs @e @r1 t1 . cseFixTargetToHArgs @e @r2 t2

instance (CSEFixMonRet e r1, CSEFixMonRet e r2, CSEFixMonRet e r3) => CSEFixMonRet e (r1, r2, r3) where
  type CSEFixRetTarget e (r1, r2, r3) = (CSEFixRetTarget e r1, CSEFixRetTarget e r2, CSEFixRetTarget e r3)
  type
    CSEFixRetPrepend e (r1, r2, r3) tail =
      CSEFixRetPrepend e r1 (CSEFixRetPrepend e r2 (CSEFixRetPrepend e r3 tail))
  cseFixRetTypes pe _ =
    cseFixRetTypes pe (Proxy @r1) <> cseFixRetTypes pe (Proxy @r2) <> cseFixRetTypes pe (Proxy @r3)
  cseFixRetSpec = cseFixRetSpec @e @r1 . cseFixRetSpec @e @r2 . cseFixRetSpec @e @r3
  cseFixExtractRet (a, b, c) =
    cseFixExtractRet @e @r1 a . cseFixExtractRet @e @r2 b . cseFixExtractRet @e @r3 c
  cseFixRebuildRet hs0 =
    let (a, hs1) = cseFixRebuildRet @e @r1 hs0
        (b, hs2) = cseFixRebuildRet @e @r2 hs1
        (c, hs3) = cseFixRebuildRet @e @r3 hs2
     in ((a, b, c), hs3)
  cseFixHArgsToTarget hs0 =
    let (t1, hs1) = cseFixHArgsToTarget @e @r1 hs0
        (t2, hs2) = cseFixHArgsToTarget @e @r2 hs1
        (t3, hs3) = cseFixHArgsToTarget @e @r3 hs2
     in ((t1, t2, t3), hs3)
  cseFixTargetToHArgs (t1, t2, t3) =
    cseFixTargetToHArgs @e @r1 t1
      . cseFixTargetToHArgs @e @r2 t2
      . cseFixTargetToHArgs @e @r3 t3

instance
  (CSEFixMonRet e r1, CSEFixMonRet e r2, CSEFixMonRet e r3, CSEFixMonRet e r4) =>
  CSEFixMonRet e (r1, r2, r3, r4)
  where
  type
    CSEFixRetTarget e (r1, r2, r3, r4) =
      (CSEFixRetTarget e r1, CSEFixRetTarget e r2, CSEFixRetTarget e r3, CSEFixRetTarget e r4)
  type
    CSEFixRetPrepend e (r1, r2, r3, r4) tail =
      CSEFixRetPrepend e r1 (CSEFixRetPrepend e r2 (CSEFixRetPrepend e r3 (CSEFixRetPrepend e r4 tail)))
  cseFixRetTypes pe _ =
    cseFixRetTypes pe (Proxy @r1)
      <> cseFixRetTypes pe (Proxy @r2)
      <> cseFixRetTypes pe (Proxy @r3)
      <> cseFixRetTypes pe (Proxy @r4)
  cseFixRetSpec = cseFixRetSpec @e @r1 . cseFixRetSpec @e @r2 . cseFixRetSpec @e @r3 . cseFixRetSpec @e @r4
  cseFixExtractRet (a, b, c, d) =
    cseFixExtractRet @e @r1 a
      . cseFixExtractRet @e @r2 b
      . cseFixExtractRet @e @r3 c
      . cseFixExtractRet @e @r4 d
  cseFixRebuildRet hs0 =
    let (a, hs1) = cseFixRebuildRet @e @r1 hs0
        (b, hs2) = cseFixRebuildRet @e @r2 hs1
        (c, hs3) = cseFixRebuildRet @e @r3 hs2
        (d, hs4) = cseFixRebuildRet @e @r4 hs3
     in ((a, b, c, d), hs4)
  cseFixHArgsToTarget hs0 =
    let (t1, hs1) = cseFixHArgsToTarget @e @r1 hs0
        (t2, hs2) = cseFixHArgsToTarget @e @r2 hs1
        (t3, hs3) = cseFixHArgsToTarget @e @r3 hs2
        (t4, hs4) = cseFixHArgsToTarget @e @r4 hs3
     in ((t1, t2, t3, t4), hs4)
  cseFixTargetToHArgs (t1, t2, t3, t4) =
    cseFixTargetToHArgs @e @r1 t1
      . cseFixTargetToHArgs @e @r2 t2
      . cseFixTargetToHArgs @e @r3 t3
      . cseFixTargetToHArgs @e @r4 t4

instance
  ( KnownNat n,
    Typeable n,
    Typeable a,
    SPMDTyped n (CSEExp e) a,
    SPMDTyped n e a,
    CSEExpParamLeaf e (SPMDRep n (CSEExp e) a),
    TargetExpElem e (SPMDRep n (CSEExp e) a) ~ SPMDRep n e a,
    Typeable (SPMDRep n (CSEExp e) a)
  ) =>
  CSEFixMonRet e (SPMD n (CSEExp e) a)
  where
  type CSEFixRetTarget e (SPMD n (CSEExp e) a) = SPMD n e a
  type CSEFixRetPrepend e (SPMD n (CSEExp e) a) tail = SPMDRep n (CSEExp e) a ': tail
  cseFixRetTypes _ _ =
    [ TR.SomeTypeRep (TR.typeRep @n),
      TR.SomeTypeRep (TR.typeRep @a),
      TR.SomeTypeRep (TR.typeRep @(SPMDRep n (CSEExp e) a))
    ]
  cseFixRetSpec rest =
    HRSCons (TR.typeRep @(SPMDRep n (CSEExp e) a)) (cseLeafBindCap @e @(SPMDRep n (CSEExp e) a)) rest
  cseFixExtractRet x rest =
    HCons (spmdMaterialize @n @(CSEExp e) @a (runSPMD x)) rest
  cseFixRebuildRet (HCons v rest) =
    (SPMD (pure (spmdDematerialize @n @(CSEExp e) @a v)), rest)
  cseFixHArgsToTarget (HCons v rest) =
    (SPMD (pure (spmdDematerialize @n @e @a (coerceExpNewtype v))), rest)
  cseFixTargetToHArgs tgt rest =
    HCons (coerceExpNewtype (spmdMaterialize @n @e @a (runSPMD tgt))) rest

instance
  ( Generic (d (CSEExp e)),
    Generic (d e),
    GCSEFixLeaves e (Rep (d (CSEExp e))) (Rep (d e))
  ) =>
  CSEFixMonRet e (HKD d (CSEExp e))
  where
  type CSEFixRetTarget e (HKD d (CSEExp e)) = HKD d e

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEFixLeaves e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  CSEFixMonRet e (PtrStruct sub (CSEExp e))
  where
  type CSEFixRetTarget e (PtrStruct sub (CSEExp e)) = PtrStruct sub e

class CSEFix (e :: K.Type -> K.Type) (r :: K.Type) | r -> e where
  type CSEFixTarget e r :: K.Type
  type CSEFixParams e r :: [K.Type]
  type CSEFixLeaf e r :: K.Type
  type CSEFixTargetLeaf e r :: K.Type
  cseFixSigTypes :: Proxy e -> Proxy r -> ([TR.SomeTypeRep], [TR.SomeTypeRep])
  cseAllocFixParams :: Digest -> Int -> (HArgs (VarId e) (CSEFixParams e r), r -> CSEFixLeaf e r)
  cseMakeFixStub :: (HArgs (CSEExp e) (CSEFixParams e r) -> CSEFixLeaf e r) -> r
  cseCurryFixTarget :: (CSETargetExp e) => (HArgs e (CSEFixParams e r) -> CSEFixTargetLeaf e r) -> CSEFixTarget e r
  cseUncurryFixTarget :: (CSETargetExp e) => CSEFixTarget e r -> HArgs e (CSEFixParams e r) -> CSEFixTargetLeaf e r

instance (CSEExpParamLeaf e b, Typeable b) => CSEFix e (CSEExp e b) where
  type CSEFixTarget e (CSEExp e b) = e (TargetExpElem e b)
  type CSEFixParams e (CSEExp e b) = '[]
  type CSEFixLeaf e (CSEExp e b) = CSEExp e b
  type CSEFixTargetLeaf e (CSEExp e b) = e b
  cseFixSigTypes _ _ = ([], [TR.SomeTypeRep (TR.typeRep @b)])
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = coerceExpNewtype (k HNil)
  cseUncurryFixTarget v HNil = coerceExpNewtype v

instance
  ( KnownNat n,
    Typeable n,
    Typeable b,
    SPMDTyped n (CSEExp e) b,
    SPMDTyped n e b,
    CSEExpParamLeaf e (SPMDRep n (CSEExp e) b),
    TargetExpElem e (SPMDRep n (CSEExp e) b) ~ SPMDRep n e b,
    Typeable (SPMDRep n (CSEExp e) b)
  ) =>
  CSEFix e (SPMD n (CSEExp e) b)
  where
  type CSEFixTarget e (SPMD n (CSEExp e) b) = SPMD n e b
  type CSEFixParams e (SPMD n (CSEExp e) b) = '[]
  type CSEFixLeaf e (SPMD n (CSEExp e) b) = SPMD n (CSEExp e) b
  type CSEFixTargetLeaf e (SPMD n (CSEExp e) b) = e (SPMDRep n (CSEExp e) b)
  cseFixSigTypes _ _ =
    ( [],
      [ TR.SomeTypeRep (TR.typeRep @n),
        TR.SomeTypeRep (TR.typeRep @b),
        TR.SomeTypeRep (TR.typeRep @(SPMDRep n (CSEExp e) b))
      ]
    )
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = SPMD (pure (spmdDematerialize @n @e @b (coerceExpNewtype (k HNil))))
  cseUncurryFixTarget v HNil = coerceExpNewtype (spmdMaterialize @n @e @b (runSPMD v))

instance (Functor m, CSEFixMonRet e res) => CSEFix e (CSEM m e res) where
  type CSEFixTarget e (CSEM m e res) = m (CSEFixRetTarget e res)
  type CSEFixParams e (CSEM m e res) = '[]
  type CSEFixLeaf e (CSEM m e res) = CSEM m e res
  type CSEFixTargetLeaf e (CSEM m e res) = m (HArgs e (CSEFixRets e res))
  cseFixSigTypes pe _ = ([], cseFixRetTypes pe (Proxy @res))
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = fst . cseFixHArgsToTarget @e @res @'[] <$> k HNil
  cseUncurryFixTarget act HNil = (\t -> cseFixTargetToHArgs @e @res @'[] t HNil) <$> act

instance (Monad m, CSEFixMonRet e res) => CSEFix e (SPMDM n (CSEM m e) (CSEExp e) res) where
  type CSEFixTarget e (SPMDM n (CSEM m e) (CSEExp e) res) = SPMDM n m e (CSEFixRetTarget e res)
  type CSEFixParams e (SPMDM n (CSEM m e) (CSEExp e) res) = '[]
  type CSEFixLeaf e (SPMDM n (CSEM m e) (CSEExp e) res) = SPMDM n (CSEM m e) (CSEExp e) res
  type CSEFixTargetLeaf e (SPMDM n (CSEM m e) (CSEExp e) res) = m (HArgs e (CSEFixRets e res))
  cseFixSigTypes pe _ = ([], cseFixRetTypes pe (Proxy @res))
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = liftSPMD (fst . cseFixHArgsToTarget @e @res @'[] <$> k HNil)
  cseUncurryFixTarget act HNil = (\t -> cseFixTargetToHArgs @e @res @'[] t HNil) <$> runSPMDM act

instance (CSEFixParam e p, CSEFix e r) => CSEFix e (p -> r) where
  type CSEFixTarget e (p -> r) = CSEFixParamTarget e p -> CSEFixTarget e r
  type CSEFixParams e (p -> r) = CSEFixParamPrepend e p (CSEFixParams e r)
  type CSEFixLeaf e (p -> r) = CSEFixLeaf e r
  type CSEFixTargetLeaf e (p -> r) = CSEFixTargetLeaf e r
  cseFixSigTypes pe _ =
    let (inRest, outTys) = cseFixSigTypes pe (Proxy @r)
     in (cseFixParamTypes pe (Proxy @p) <> inRest, outTys)
  cseAllocFixParams funKey idx0 =
    let (idx1, allVids, formalP) = cseFixAllocParam @e @p funKey idx0 restVids
        (restVids, peelRest) = cseAllocFixParams @e @r funKey idx1
     in (allVids, \fn -> peelRest (fn formalP))
  cseMakeFixStub mkLeaf =
    \arg -> cseMakeFixStub @e @r (\restArgs -> mkLeaf (cseFixExtractParam @e @p arg restArgs))
  cseCurryFixTarget k =
    \x -> cseCurryFixTarget @e @r (\xs -> k (cseFixPrependParamTarget @e @p x xs))
  cseUncurryFixTarget fn allArgs =
    let (x, restArgs) = cseFixConsumeParamTarget @e @p allArgs
     in cseUncurryFixTarget @e @r (fn x) restArgs

class CSEFixLeafBuilder (e :: K.Type -> K.Type) (r :: K.Type) where
  cseBuildLeaf ::
    Digest ->
    String ->
    HArgs (VarId e) params ->
    (((HArgs e params -> CSEFixTargetLeaf e r) -> HArgs e params -> CSEFixTargetLeaf e r) -> HArgs e params -> CSEFixTargetLeaf e r) ->
    CSEFixLeaf e r ->
    (HArgs (CSEExp e) params -> CSEFixLeaf e r)

instance (CSEExpParamLeaf e b, Typeable b) => CSEFixLeafBuilder e (CSEExp e b) where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let pfd =
          PureFunDef
            { pfdKey = funKey,
              pfdName = name,
              pfdParams = formalParams,
              pfdRetType = TR.typeRep @b,
              pfdRetBind = cseLeafBindCap @e @b,
              pfdBody = cseExp rawBody,
              pfdEmitTarget = emitWithTarget
            }
     in mkExpFixCall pfd

instance
  ( SPMDTyped n (CSEExp e) b,
    CSEExpParamLeaf e (SPMDRep n (CSEExp e) b),
    Typeable (SPMDRep n (CSEExp e) b)
  ) =>
  CSEFixLeafBuilder e (SPMD n (CSEExp e) b)
  where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let rawRepBody = spmdMaterialize @n @(CSEExp e) @b (runSPMD rawBody)
        pfd =
          PureFunDef
            { pfdKey = funKey,
              pfdName = name,
              pfdParams = formalParams,
              pfdRetType = TR.typeRep @(SPMDRep n (CSEExp e) b),
              pfdRetBind = cseLeafBindCap @e @(SPMDRep n (CSEExp e) b),
              pfdBody = cseExp rawRepBody,
              pfdEmitTarget = emitWithTarget
            }
     in \callArgs -> SPMD (pure (spmdDematerialize @n @(CSEExp e) @b (mkExpFixCall pfd callArgs)))

instance (CSEFixMonRet e res) => CSEFixLeafBuilder e (CSEM m e res) where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let retSpec = cseFixRetSpec @e @res HRSNil
        rawHBody = (\r -> cseFixExtractRet @e @res r HNil) <$> rawBody
        mfd =
          MonadicFunDef
            { mfdKey = funKey,
              mfdName = name,
              mfdParams = formalParams,
              mfdRetSpec = retSpec,
              mfdBody = cseM rawHBody,
              mfdEmitTarget = emitWithTarget
            }
     in \callArgs -> do
          vids <- allocHRetVars retSpec
          emitStm (StmFixCall vids mfd callArgs)
          let varExps = runIdentity (mapHArgsM (Identity . mkExpVar) vids)
              (res, HNil) = cseFixRebuildRet @e @res @'[] varExps
           in pure res

instance (CSEFixMonRet e res) => CSEFixLeafBuilder e (SPMDM n (CSEM m e) (CSEExp e) res) where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let innerBuilder =
          cseBuildLeaf
            @e
            @(CSEM m e res)
            funKey
            name
            formalParams
            emitWithTarget
            (runSPMDM rawBody)
     in liftSPMD . innerBuilder

instance (CSEFixLeafBuilder e r) => CSEFixLeafBuilder e (p -> r) where
  cseBuildLeaf = cseBuildLeaf @e @r

instance
  ( CSETargetExp e,
    CSEFix e r,
    CSEFixLeafBuilder e r,
    ExpFix e (CSEFixTarget e r)
  ) =>
  ExpFix (CSEExp e) r
  where
  fun _ name stepFn =
    let (inTrs, outTrs) = cseFixSigTypes (Proxy @e) (Proxy @r)
        sigImms =
          encodeStr name
            : encodeInt (length inTrs)
            : map encodeSomeTypeRep inTrs
            <> (encodeInt (length outTrs) : map encodeSomeTypeRep outTrs)
        funKey = mkNodeDigest "ExpFix" sigImms []
        (formalParams, peelFn) = cseAllocFixParams @e @r funKey 0
        emitWithTarget step targetArgs =
          let targetCurried =
                fun
                  (Proxy @e)
                  name
                  (\selfCurried -> cseCurryFixTarget @e @r (step (cseUncurryFixTarget @e @r selfCurried)))
           in cseUncurryFixTarget @e @r targetCurried targetArgs
        callBuilder = cseBuildLeaf @e @r funKey name formalParams emitWithTarget rawBody
        selfStub = cseMakeFixStub @e @r callBuilder
        rawBody = peelFn (stepFn selfStub)
     in selfStub

instance (ExpFix (CSEExp e) r) => ExpFix (SPMD n (CSEExp e)) r where
  fun _ = fun (Proxy @(CSEExp e))

class ReaderCSEFix (env :: K.Type) (e :: K.Type -> K.Type) (fn :: K.Type) | fn -> env e where
  type ReaderCSEFixInner fn :: K.Type
  toInnerCSEFix :: fn -> ReaderCSEFixInner fn
  fromInnerCSEFix :: ReaderCSEFixInner fn -> fn

instance ReaderCSEFix env e (ReaderExp env (CSEExp e) a) where
  type ReaderCSEFixInner (ReaderExp env (CSEExp e) a) = CSEExp e a
  toInnerCSEFix = getReaderExp
  fromInnerCSEFix = ReaderExp

instance (ReaderCSEFix env e fn) => ReaderCSEFix env e (ReaderExp env (CSEExp e) a -> fn) where
  type ReaderCSEFixInner (ReaderExp env (CSEExp e) a -> fn) = CSEExp e a -> ReaderCSEFixInner fn
  toInnerCSEFix f = toInnerCSEFix . f . ReaderExp
  fromInnerCSEFix g = fromInnerCSEFix . g . getReaderExp

instance
  (ReaderCSEFix env e fn, ExpFix (CSEExp e) (ReaderCSEFixInner fn)) =>
  ExpFix (ReaderExp env (CSEExp e)) fn
  where
  fun _ name step =
    fromInnerCSEFix $
      fun (Proxy @(CSEExp e)) name (toInnerCSEFix . step . fromInnerCSEFix)

--------------------------------------------------------------------------------
-- SIMD Expression & Memory Instances on `(CSEM m e, CSEExp e)`
--------------------------------------------------------------------------------

simdVecBind ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  Maybe (TR.TypeRep (SIMDVector (CSEExp e) n a), BindCap e (SIMDVector (CSEExp e) n a))
simdVecBind = Just (TR.typeRep @(SIMDVector (CSEExp e) n a), BindSIMDVec)

simdMaskBind ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  Maybe (TR.TypeRep (SIMDMask (CSEExp e) n a), BindCap e (SIMDMask (CSEExp e) n a))
simdMaskBind = Just (TR.typeRep @(SIMDMask (CSEExp e) n a), BindSIMDMask)

{-# INLINE unwrapVec #-}
unwrapVec :: (ExpNewtype e) => e (SIMDVector (CSEExp e) n a) -> e (SIMDVector e n a)
unwrapVec = coerceExpNewtype

{-# INLINE wrapVec #-}
wrapVec :: (ExpNewtype e) => e (SIMDVector e n a) -> e (SIMDVector (CSEExp e) n a)
wrapVec = coerceExpNewtype

{-# INLINE unwrapMask #-}
unwrapMask :: (ExpNewtype e) => e (SIMDMask (CSEExp e) n a) -> e (SIMDMask e n a)
unwrapMask = coerceExpNewtype

{-# INLINE wrapMask #-}
wrapMask :: (ExpNewtype e) => e (SIMDMask e n a) -> e (SIMDMask (CSEExp e) n a)
wrapMask = coerceExpNewtype

vecUnOp ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  BS.ByteString ->
  [BS.ByteString] ->
  (e (SIMDVector e n a) -> e (SIMDVector e n a)) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a)
vecUnOp tag imms fn v =
  mkExpOp
    tag
    imms
    False
    (simdVecBind @e @n @a)
    (HCons v HNil)
    (\(HCons x HNil) -> wrapVec (fn (unwrapVec x)))

vecBinOp ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  BS.ByteString ->
  [BS.ByteString] ->
  (e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a)
vecBinOp tag imms fn a b =
  mkExpOp
    tag
    imms
    False
    (simdVecBind @e @n @a)
    (HCons a (HCons b HNil))
    (\(HCons va (HCons vb HNil)) -> wrapVec (fn (unwrapVec va) (unwrapVec vb)))

vecTernOp ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  BS.ByteString ->
  (e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDVector e n a)) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a)
vecTernOp tag fn a b c =
  mkExpOp
    tag
    []
    False
    (simdVecBind @e @n @a)
    (HCons a (HCons b (HCons c HNil)))
    (\(HCons va (HCons vb (HCons vc HNil))) -> wrapVec (fn (unwrapVec va) (unwrapVec vb) (unwrapVec vc)))

maskBinOp ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  BS.ByteString ->
  (e (SIMDMask e n a) -> e (SIMDMask e n a) -> e (SIMDMask e n a)) ->
  CSEExp e (SIMDMask (CSEExp e) n a) ->
  CSEExp e (SIMDMask (CSEExp e) n a) ->
  CSEExp e (SIMDMask (CSEExp e) n a)
maskBinOp tag fn a b =
  mkExpOp
    tag
    []
    False
    (simdMaskBind @e @n @a)
    (HCons a (HCons b HNil))
    (\(HCons va (HCons vb HNil)) -> wrapMask (fn (unwrapMask va) (unwrapMask vb)))

vecCmpOp ::
  forall e n a.
  (CSESIMDTarget e, SIMDShape n a) =>
  BS.ByteString ->
  (e (SIMDVector e n a) -> e (SIMDVector e n a) -> e (SIMDMask e n a)) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDVector (CSEExp e) n a) ->
  CSEExp e (SIMDMask (CSEExp e) n a)
vecCmpOp tag fn a b =
  mkExpOp
    tag
    []
    False
    (simdMaskBind @e @n @a)
    (HCons a (HCons b HNil))
    (\(HCons va (HCons vb HNil)) -> wrapMask (fn (unwrapVec va) (unwrapVec vb)))

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Num (e (SIMDVector e n a))) =>
  Num (CSEExp e (SIMDVector (CSEExp e) n a))
  where
  (+) = vecBinOp "SIMD.Num.+" [] (+)
  (-) = vecBinOp "SIMD.Num.-" [] (-)
  (*) = vecBinOp "SIMD.Num.*" [] (*)
  negate = vecUnOp "SIMD.Num.negate" [] negate
  abs = vecUnOp "SIMD.Num.abs" [] abs
  signum = vecUnOp "SIMD.Num.signum" [] signum
  fromInteger k = mkExpLit "SIMD.Num.fromInteger" [encodeInteger k] (simdVecBind @e @n @a) (wrapVec (fromInteger k))

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Fractional (e (SIMDVector e n a))) =>
  Fractional (CSEExp e (SIMDVector (CSEExp e) n a))
  where
  (/) = vecBinOp "SIMD.Fractional./" [] (/)
  recip = vecUnOp "SIMD.Fractional.recip" [] recip
  fromRational q = mkExpLit "SIMD.Fractional.fromRational" [encodeRational q] (simdVecBind @e @n @a) (wrapVec (fromRational q))

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Floating (e (SIMDVector e n a))) =>
  Floating (CSEExp e (SIMDVector (CSEExp e) n a))
  where
  pi = mkExpLit "SIMD.Floating.pi" [] (simdVecBind @e @n @a) (wrapVec pi)
  exp = vecUnOp "SIMD.Floating.exp" [] exp
  log = vecUnOp "SIMD.Floating.log" [] log
  sqrt = vecUnOp "SIMD.Floating.sqrt" [] sqrt
  (**) = vecBinOp "SIMD.Floating.**" [] (**)
  logBase = vecBinOp "SIMD.Floating.logBase" [] logBase
  sin = vecUnOp "SIMD.Floating.sin" [] sin
  cos = vecUnOp "SIMD.Floating.cos" [] cos
  tan = vecUnOp "SIMD.Floating.tan" [] tan
  asin = vecUnOp "SIMD.Floating.asin" [] asin
  acos = vecUnOp "SIMD.Floating.acos" [] acos
  atan = vecUnOp "SIMD.Floating.atan" [] atan
  sinh = vecUnOp "SIMD.Floating.sinh" [] sinh
  cosh = vecUnOp "SIMD.Floating.cosh" [] cosh
  tanh = vecUnOp "SIMD.Floating.tanh" [] tanh
  asinh = vecUnOp "SIMD.Floating.asinh" [] asinh
  acosh = vecUnOp "SIMD.Floating.acosh" [] acosh
  atanh = vecUnOp "SIMD.Floating.atanh" [] atanh
  log1p = vecUnOp "SIMD.Floating.log1p" [] log1p
  expm1 = vecUnOp "SIMD.Floating.expm1" [] expm1

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, FMA (e (SIMDVector e n a))) =>
  FMA (CSEExp e (SIMDVector (CSEExp e) n a))
  where
  fma = vecTernOp "SIMD.FMA.fma" fma

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Hypot (e (SIMDVector e n a))) =>
  Hypot (CSEExp e (SIMDVector (CSEExp e) n a))
  where
  hypot = vecBinOp "SIMD.Hypot.hypot" [] hypot

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Join (e (SIMDMask e n a))) =>
  Join (CSEExp e (SIMDMask (CSEExp e) n a))
  where
  (\/) = maskBinOp "SIMD.Mask.\\/" (\/)

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Meet (e (SIMDMask e n a))) =>
  Meet (CSEExp e (SIMDMask (CSEExp e) n a))
  where
  (/\) = maskBinOp "SIMD.Mask./\\" (/\)

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Lower (e (SIMDMask e n a))) =>
  Lower (CSEExp e (SIMDMask (CSEExp e) n a))
  where
  lowerBound = mkExpLit "SIMD.Mask.lowerBound" [] (simdMaskBind @e @n @a) (wrapMask lowerBound)

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, SIMDShape n a, Upper (e (SIMDMask e n a))) =>
  Upper (CSEExp e (SIMDMask (CSEExp e) n a))
  where
  upperBound = mkExpLit "SIMD.Mask.upperBound" [] (simdMaskBind @e @n @a) (wrapMask upperBound)

instance (CSESIMDTarget e, ExpSIMDVec e n a, CSETyped e a) => ExpSIMDVec (CSEExp e) n a where
  vzero = mkExpLit "SIMD.vzero" [] (simdVecBind @e @n @a) (wrapVec (vzero @e @n @a))
  vsplat x =
    mkExpOp
      "SIMD.vsplat"
      [encodeTypeRep (TR.typeRep @a)]
      False
      (simdVecBind @e @n @a)
      (HCons x HNil)
      (\(HCons vx HNil) -> wrapVec (vsplat vx))
  viota = mkExpLit "SIMD.viota" [] (simdVecBind @e @n @a) (wrapVec (viota @e @n @a))
  vfromLanes xs =
    mkExpListOp
      "SIMD.vfromLanes"
      [encodeTypeRep (TR.typeRep @a)]
      (simdVecBind @e @n @a)
      xs
      (wrapVec . vfromLanes)
  vextractLane p v =
    mkExpOp
      "SIMD.vextractLane"
      [encodeInt (fromIntegral (natVal p))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vextractLane p (unwrapVec vv))
  vinsertLane p x v =
    mkExpOp
      "SIMD.vinsertLane"
      [encodeInt (fromIntegral (natVal p))]
      False
      (simdVecBind @e @n @a)
      (HCons x (HCons v HNil))
      (\(HCons vx (HCons vv HNil)) -> wrapVec (vinsertLane p vx (unwrapVec vv)))

instance (CSESIMDTarget e, ExpSIMDMask e n a) => ExpSIMDMask (CSEExp e) n a where
  mfalse = mkExpLit "SIMD.mfalse" [] (simdMaskBind @e @n @a) (wrapMask (mfalse @e @n @a))
  mtrue = mkExpLit "SIMD.mtrue" [] (simdMaskBind @e @n @a) (wrapMask (mtrue @e @n @a))
  mand = maskBinOp "SIMD.mand" mand
  mor = maskBinOp "SIMD.mor" mor
  mxor = maskBinOp "SIMD.mxor" mxor
  mandnot = maskBinOp "SIMD.mandnot" mandnot
  mnot m =
    mkExpOp
      "SIMD.mnot"
      []
      False
      (simdMaskBind @e @n @a)
      (HCons m HNil)
      (\(HCons vm HNil) -> wrapMask (mnot (unwrapMask vm)))
  vselect m a b =
    mkExpOp
      "SIMD.vselect"
      []
      False
      (simdVecBind @e @n @a)
      (HCons m (HCons a (HCons b HNil)))
      (\(HCons vm (HCons va (HCons vb HNil))) -> wrapVec (vselect (unwrapMask vm) (unwrapVec va) (unwrapVec vb)))
  mmovemask m =
    mkExpOp
      "SIMD.mmovemask"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Word32, BindWord32))
      (HCons m HNil)
      (\(HCons vm HNil) -> mmovemask (unwrapMask vm))
  mpopcount m =
    mkExpOp
      "SIMD.mpopcount"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Int32, BindInt32))
      (HCons m HNil)
      (\(HCons vm HNil) -> mpopcount (unwrapMask vm))
  mfirstTrue m =
    mkExpOp
      "SIMD.mfirstTrue"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Int32, BindInt32))
      (HCons m HNil)
      (\(HCons vm HNil) -> mfirstTrue (unwrapMask vm))
  mallOf m =
    mkExpOp
      "SIMD.mallOf"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons m HNil)
      (\(HCons vm HNil) -> mallOf (unwrapMask vm))
  manyOf m =
    mkExpOp
      "SIMD.manyOf"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons m HNil)
      (\(HCons vm HNil) -> manyOf (unwrapMask vm))
  mnoneOf m =
    mkExpOp
      "SIMD.mnoneOf"
      [encodeTypeRep (TR.typeRep @(SIMDMask (CSEExp e) n a))]
      False
      (Just (TR.typeRep @Bool, BindBool))
      (HCons m HNil)
      (\(HCons vm HNil) -> mnoneOf (unwrapMask vm))

instance (CSESIMDTarget e, ExpSIMDCmp e n a) => ExpSIMDCmp (CSEExp e) n a where
  veq = vecCmpOp "SIMD.veq" veq
  vne = vecCmpOp "SIMD.vne" vne
  vlt = vecCmpOp "SIMD.vlt" vlt
  vle = vecCmpOp "SIMD.vle" vle
  vgt = vecCmpOp "SIMD.vgt" vgt
  vge = vecCmpOp "SIMD.vge" vge

instance (CSESIMDTarget e, ExpSIMDArith e n a, CSETyped e a) => ExpSIMDArith (CSEExp e) n a where
  vadd = vecBinOp "SIMD.vadd" [] vadd
  vsub = vecBinOp "SIMD.vsub" [] vsub
  vmul = vecBinOp "SIMD.vmul" [] vmul
  vneg = vecUnOp "SIMD.vneg" [] vneg
  vabs = vecUnOp "SIMD.vabs" [] vabs
  vmin = vecBinOp "SIMD.vmin" [] vmin
  vmax = vecBinOp "SIMD.vmax" [] vmax

instance (CSESIMDTarget e, ExpSIMDSat e n a, CSETyped e a) => ExpSIMDSat (CSEExp e) n a where
  vadds = vecBinOp "SIMD.vadds" [] vadds
  vsubs = vecBinOp "SIMD.vsubs" [] vsubs
  vabss = vecUnOp "SIMD.vabss" [] vabss
  vavg = vecBinOp "SIMD.vavg" [] vavg
  vabsdiff = vecBinOp "SIMD.vabsdiff" [] vabsdiff

instance (CSESIMDTarget e, ExpSIMDFloat e n a, CSETyped e a) => ExpSIMDFloat (CSEExp e) n a where
  vdiv = vecBinOp "SIMD.vdiv" [] vdiv
  vsqrt = vecUnOp "SIMD.vsqrt" [] vsqrt
  vrsqrtApprox = vecUnOp "SIMD.vrsqrtApprox" [] vrsqrtApprox
  vrcpApprox = vecUnOp "SIMD.vrcpApprox" [] vrcpApprox
  vfma = vecTernOp "SIMD.vfma" vfma
  vfms = vecTernOp "SIMD.vfms" vfms
  vfnma = vecTernOp "SIMD.vfnma" vfnma
  vfnms = vecTernOp "SIMD.vfnms" vfnms
  vmulAdd = vecTernOp "SIMD.vmulAdd" vmulAdd
  vfloor = vecUnOp "SIMD.vfloor" [] vfloor
  vceil = vecUnOp "SIMD.vceil" [] vceil
  vtrunc = vecUnOp "SIMD.vtrunc" [] vtrunc
  vroundNearest = vecUnOp "SIMD.vroundNearest" [] vroundNearest

instance (CSESIMDTarget e, ExpSIMDBits e n a, CSETyped e a) => ExpSIMDBits (CSEExp e) n a where
  vand = vecBinOp "SIMD.vand" [] vand
  vor = vecBinOp "SIMD.vor" [] vor
  vxor = vecBinOp "SIMD.vxor" [] vxor
  vandnot = vecBinOp "SIMD.vandnot" [] vandnot
  vnot = vecUnOp "SIMD.vnot" [] vnot
  vshl v s =
    mkExpOp
      "SIMD.vshl"
      []
      False
      (simdVecBind @e @n @a)
      (HCons v (HCons s HNil))
      (\(HCons vv (HCons vs HNil)) -> wrapVec (vshl (unwrapVec vv) vs))
  vshr v s =
    mkExpOp
      "SIMD.vshr"
      []
      False
      (simdVecBind @e @n @a)
      (HCons v (HCons s HNil))
      (\(HCons vv (HCons vs HNil)) -> wrapVec (vshr (unwrapVec vv) vs))
  vshlImm p = vecUnOp "SIMD.vshlImm" [encodeInt (fromIntegral (natVal p))] (vshlImm p)
  vshrImm p = vecUnOp "SIMD.vshrImm" [encodeInt (fromIntegral (natVal p))] (vshrImm p)
  vshlVar = vecBinOp "SIMD.vshlVar" [] vshlVar
  vshrVar = vecBinOp "SIMD.vshrVar" [] vshrVar
  vpopcnt = vecUnOp "SIMD.vpopcnt" [] vpopcnt

instance (CSESIMDTarget e, ExpSIMDShuffle e n a, CSETyped e a) => ExpSIMDShuffle (CSEExp e) n a where
  vshuffleLanes idxs = vecUnOp "SIMD.vshuffleLanes" (map encodeInt idxs) (vshuffleLanes idxs)
  vpermuteVar v idxs =
    mkExpOp
      "SIMD.vpermuteVar"
      []
      False
      (simdVecBind @e @n @a)
      (HCons v (HCons idxs HNil))
      (\(HCons vv (HCons vi HNil)) -> wrapVec (vpermuteVar (unwrapVec vv) (unwrapVec vi)))
  vshuffleBytes v ctrl =
    mkExpOp
      "SIMD.vshuffleBytes"
      []
      False
      (simdVecBind @e @n @a)
      (HCons v (HCons ctrl HNil))
      (\(HCons vv (HCons vc HNil)) -> wrapVec (vshuffleBytes (unwrapVec vv) (unwrapVec vc)))
  vzipLo = vecBinOp "SIMD.vzipLo" [] vzipLo
  vzipHi = vecBinOp "SIMD.vzipHi" [] vzipHi

instance (CSESIMDTarget e, ExpSIMDSplit e n2 n a) => ExpSIMDSplit (CSEExp e) n2 n a where
  vsplit v =
    let lo =
          mkExpOp
            "SIMD.vsplit.lo"
            []
            False
            (simdVecBind @e @n @a)
            (HCons v HNil)
            (\(HCons vv HNil) -> wrapVec (fst (vsplit (unwrapVec vv))))
        hi =
          mkExpOp
            "SIMD.vsplit.hi"
            []
            False
            (simdVecBind @e @n @a)
            (HCons v HNil)
            (\(HCons vv HNil) -> wrapVec (snd (vsplit (unwrapVec vv))))
     in (lo, hi)
  vconcat lo hi =
    mkExpOp
      "SIMD.vconcat"
      []
      False
      (simdVecBind @e @n2 @a)
      (HCons lo (HCons hi HNil))
      (\(HCons vlo (HCons vhi HNil)) -> wrapVec (vconcat (unwrapVec vlo) (unwrapVec vhi)))

instance (CSESIMDTarget e, ExpSIMDReduce e n a, CSETyped e a) => ExpSIMDReduce (CSEExp e) n a where
  vreduceAdd v =
    mkExpOp
      "SIMD.vreduceAdd"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceAdd (unwrapVec vv))
  vreduceMin v =
    mkExpOp
      "SIMD.vreduceMin"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceMin (unwrapVec vv))
  vreduceMax v =
    mkExpOp
      "SIMD.vreduceMax"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceMax (unwrapVec vv))
  vreduceAnd v =
    mkExpOp
      "SIMD.vreduceAnd"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceAnd (unwrapVec vv))
  vreduceOr v =
    mkExpOp
      "SIMD.vreduceOr"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceOr (unwrapVec vv))
  vreduceXor v =
    mkExpOp
      "SIMD.vreduceXor"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (directBind @e @a)
      (HCons v HNil)
      (\(HCons vv HNil) -> vreduceXor (unwrapVec vv))

instance (CSESIMDTarget e, ExpSIMDDot e) => ExpSIMDDot (CSEExp e) where
  vdp4aS a b acc =
    mkExpOp
      "SIMD.vdp4aS"
      []
      False
      (Just (TR.typeRep @Int32, BindInt32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp4aS (unwrapVec va) (unwrapVec vb) vacc)
  vdp4aU a b acc =
    mkExpOp
      "SIMD.vdp4aU"
      []
      False
      (Just (TR.typeRep @Word32, BindWord32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp4aU (unwrapVec va) (unwrapVec vb) vacc)
  vdp4aVecS ::
    forall m k.
    (SIMDDp4aShape m k) =>
    CSEExp e (SIMDVector (CSEExp e) m Int8) ->
    CSEExp e (SIMDVector (CSEExp e) m Int8) ->
    CSEExp e (SIMDVector (CSEExp e) k Int32) ->
    CSEExp e (SIMDVector (CSEExp e) k Int32)
  vdp4aVecS a b acc =
    mkExpOp
      "SIMD.vdp4aVecS"
      []
      False
      (simdVecBind @e @k @Int32)
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> wrapVec (vdp4aVecS (unwrapVec va) (unwrapVec vb) (unwrapVec vacc)))
  vdp4aVecU ::
    forall m k.
    (SIMDDp4aShape m k) =>
    CSEExp e (SIMDVector (CSEExp e) m Word8) ->
    CSEExp e (SIMDVector (CSEExp e) m Word8) ->
    CSEExp e (SIMDVector (CSEExp e) k Word32) ->
    CSEExp e (SIMDVector (CSEExp e) k Word32)
  vdp4aVecU a b acc =
    mkExpOp
      "SIMD.vdp4aVecU"
      []
      False
      (simdVecBind @e @k @Word32)
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> wrapVec (vdp4aVecU (unwrapVec va) (unwrapVec vb) (unwrapVec vacc)))
  vdp2aLoS a b acc =
    mkExpOp
      "SIMD.vdp2aLoS"
      []
      False
      (Just (TR.typeRep @Int32, BindInt32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp2aLoS (unwrapVec va) (unwrapVec vb) vacc)
  vdp2aHiS a b acc =
    mkExpOp
      "SIMD.vdp2aHiS"
      []
      False
      (Just (TR.typeRep @Int32, BindInt32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp2aHiS (unwrapVec va) (unwrapVec vb) vacc)
  vdp2aLoU a b acc =
    mkExpOp
      "SIMD.vdp2aLoU"
      []
      False
      (Just (TR.typeRep @Word32, BindWord32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp2aLoU (unwrapVec va) (unwrapVec vb) vacc)
  vdp2aHiU a b acc =
    mkExpOp
      "SIMD.vdp2aHiU"
      []
      False
      (Just (TR.typeRep @Word32, BindWord32))
      (HCons a (HCons b (HCons acc HNil)))
      (\(HCons va (HCons vb (HCons vacc HNil))) -> vdp2aHiU (unwrapVec va) (unwrapVec vb) vacc)
  vsad8 ::
    forall n.
    (SIMDShape n Word8) =>
    CSEExp e (SIMDVector (CSEExp e) n Word8) ->
    CSEExp e (SIMDVector (CSEExp e) n Word8) ->
    CSEExp e Word32
  vsad8 a b =
    mkExpOp
      "SIMD.vsad8"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n Word8))]
      False
      (Just (TR.typeRep @Word32, BindWord32))
      (HCons a (HCons b HNil))
      (\(HCons va (HCons vb HNil)) -> vsad8 (unwrapVec va) (unwrapVec vb))

instance (CSESIMDTarget e, ExpSIMDCast e) => ExpSIMDCast (CSEExp e) where
  vbitcast ::
    forall n a m b.
    (SIMDShape n a, SIMDShape m b) =>
    CSEExp e (SIMDVector (CSEExp e) n a) ->
    CSEExp e (SIMDVector (CSEExp e) m b)
  vbitcast v =
    mkExpOp
      "SIMD.vbitcast"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (simdVecBind @e @m @b)
      (HCons v HNil)
      (\(HCons vv HNil) -> wrapVec (vbitcast (unwrapVec vv)))
  vcvt ::
    forall n a b.
    (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
    CSEExp e (SIMDVector (CSEExp e) n a) ->
    CSEExp e (SIMDVector (CSEExp e) n b)
  vcvt v =
    mkExpOp
      "SIMD.vcvt"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (simdVecBind @e @n @b)
      (HCons v HNil)
      (\(HCons vv HNil) -> wrapVec (vcvt (unwrapVec vv)))
  vnarrowSat ::
    forall n m a b.
    (SIMDPackShape n m a b) =>
    CSEExp e (SIMDVector (CSEExp e) n a) ->
    CSEExp e (SIMDVector (CSEExp e) n a) ->
    CSEExp e (SIMDVector (CSEExp e) m b)
  vnarrowSat lo hi =
    mkExpOp
      "SIMD.vnarrowSat"
      [encodeTypeRep (TR.typeRep @(SIMDVector (CSEExp e) n a))]
      False
      (simdVecBind @e @m @b)
      (HCons lo (HCons hi HNil))
      (\(HCons vlo (HCons vhi HNil)) -> wrapVec (vnarrowSat (unwrapVec vlo) (unwrapVec vhi)))

instance (CSESIMDTarget e, CmdSIMDMem m e n a) => CmdSIMDMem (CSEM m e) (CSEExp e) n a where
  vload p =
    emitEffectVal
      (TR.typeRep @(SIMDVector (CSEExp e) n a))
      (Just BindSIMDVec)
      (HCons p HNil)
      (\(HCons vp HNil) -> wrapVec <$> vload vp)
  vloadAligned p =
    emitEffectVal
      (TR.typeRep @(SIMDVector (CSEExp e) n a))
      (Just BindSIMDVec)
      (HCons p HNil)
      (\(HCons vp HNil) -> wrapVec <$> vloadAligned vp)
  vstore p v =
    emitEffectUnit
      (HCons p (HCons v HNil))
      (\(HCons vp (HCons vv HNil)) -> vstore vp (unwrapVec vv))
  vstoreAligned p v =
    emitEffectUnit
      (HCons p (HCons v HNil))
      (\(HCons vp (HCons vv HNil)) -> vstoreAligned vp (unwrapVec vv))
  vloadMasked m fb p =
    emitEffectVal
      (TR.typeRep @(SIMDVector (CSEExp e) n a))
      (Just BindSIMDVec)
      (HCons m (HCons fb (HCons p HNil)))
      (\(HCons vm (HCons vfb (HCons vp HNil))) -> wrapVec <$> vloadMasked (unwrapMask vm) (unwrapVec vfb) vp)
  vstoreMasked m p v =
    emitEffectUnit
      (HCons m (HCons p (HCons v HNil)))
      (\(HCons vm (HCons vp (HCons vv HNil))) -> vstoreMasked (unwrapMask vm) vp (unwrapVec vv))
  vgather p idxs =
    emitEffectVal
      (TR.typeRep @(SIMDVector (CSEExp e) n a))
      (Just BindSIMDVec)
      (HCons p (HCons idxs HNil))
      (\(HCons vp (HCons vi HNil)) -> wrapVec <$> vgather vp (unwrapVec vi))
  vgatherMasked m fb p idxs =
    emitEffectVal
      (TR.typeRep @(SIMDVector (CSEExp e) n a))
      (Just BindSIMDVec)
      (HCons m (HCons fb (HCons p (HCons idxs HNil))))
      (\(HCons vm (HCons vfb (HCons vp (HCons vi HNil)))) -> wrapVec <$> vgatherMasked (unwrapMask vm) (unwrapVec vfb) vp (unwrapVec vi))

--------------------------------------------------------------------------------
-- Monadic Command & Control-Flow Instances on `(CSEM m e, CSEExp e)`
--------------------------------------------------------------------------------

allocLocalRef :: forall m e a. (CSETyped e a) => CSEM m e (CSERef m e a)
allocLocalRef = CSEM $ \_ n ->
  (n + 1, ProgLinear Seq.empty (CSERef (RefLocal n) (TR.typeRep @a) (BindDirect @e @a)))

instance (CmdRef m e) => CmdRef (CSEM m e) (CSEExp e) where
  type Ref (CSEM m e) (CSEExp e) a = CSERef m e a
  letM :: forall a. (CSETyped e a) => CSEExp e a -> CSEM m e (CSEExp e a)
  letM rhs = do
    let cap = BindDirect @e @a
    vid <- allocSSAId (TR.typeRep @a) (Just cap)
    emitStm (StmLetM vid cap rhs)
    pure (mkExpVar vid)
  newRef :: forall a. (CSETyped e a) => CSEExp e a -> CSEM m e (CSERef m e a)
  newRef initVal = do
    ref <- allocLocalRef @m @e @a
    emitStm (StmNewRef ref (Just initVal))
    pure ref
  newRef' :: forall a. (CSETyped e a) => CSEM m e (CSERef m e a)
  newRef' = do
    ref <- allocLocalRef @m @e @a
    emitStm (StmNewRef ref Nothing)
    pure ref
  readRef :: forall a. (CSETyped e a) => CSERef m e a -> CSEM m e (CSEExp e a)
  readRef ref = do
    vid <- allocSSAId (TR.typeRep @a) (Just (BindDirect @e @a))
    emitStm (StmReadRef vid ref)
    pure (mkExpVar vid)
  writeRef ref val = emitStm (StmWriteRef ref val)

instance (CmdRef m e, KnownJanusType e a, Typeable a) => RefCell (CSEM m e) (CSEExp e) (CSEExp e a) where
  newCell x = do
    ref <- newRef x
    pure (Cell (readRef ref) (writeRef ref))
  newCell' = do
    ref <- newRef'
    pure (Cell (readRef ref) (writeRef ref))

instance (ExpJanus e) => ExpJanus (CSEExp e)

localArgsSomeTypeReps :: LocalArgs f ts -> [TR.SomeTypeRep]
localArgsSomeTypeReps LANil = []
localArgsSomeTypeReps (LACons (_ :: f a) rest) =
  TR.SomeTypeRep (TR.typeRep @a) : localArgsSomeTypeReps rest

allocLocalArgsCSE ::
  forall e ts.
  (ExpJanus e) =>
  Digest ->
  Int ->
  LocalArgs (CSEExp e) ts ->
  (HArgs (VarId e) ts, LocalArgs (CSEExp e) ts)
allocLocalArgsCSE _ _ LANil = (HNil, LANil)
allocLocalArgsCSE fk idx (LACons (_ :: CSEExp e a) rest) =
  withJanusType @a @e $
    let tr = TR.typeRep @a
        cap = BindDirect @e @a
        vid = VarId (VarLocalFun fk idx) tr (Just cap)
        (restVids, restFormalLA) = allocLocalArgsCSE @e fk (idx + 1) rest
     in (HCons vid restVids, LACons (mkExpVar vid) restFormalLA)

localArgsToHArgs :: LocalArgs f ts -> HArgs f ts
localArgsToHArgs LANil = HNil
localArgsToHArgs (LACons x rest) = HCons x (localArgsToHArgs rest)

hArgsToLocalArgs :: LocalArgs proxy ts -> HArgs f ts -> LocalArgs f ts
hArgsToLocalArgs LANil HNil = LANil
hArgsToLocalArgs (LACons _ restProxy) (HCons x xs) =
  LACons x (hArgsToLocalArgs restProxy xs)

instance (CmdJanus m e) => CmdLocalFun (CSEM m e) (CSEExp e) where
  emitLocalFunArgsM ::
    forall ts b.
    (Typeable b, Storable b, JanusCTyped b) =>
    String ->
    ((LocalArgs (CSEExp e) ts -> CSEM m e (CSEExp e b)) -> LocalArgs (CSEExp e) ts -> CSEM m e (CSEExp e b)) ->
    LocalArgs (CSEExp e) ts ->
    CSEM m e (CSEExp e b)
  emitLocalFunArgsM name step actualArgs =
    withJanusType @b @e $ do
      let inTrs = localArgsSomeTypeReps actualArgs
          trB = TR.typeRep @b
          capB = BindDirect @e @b
          sigImms =
            encodeStr name
              : encodeInt (length inTrs)
              : map encodeSomeTypeRep inTrs
              <> [encodeInt 1, encodeTypeRep trB]
          funKey = mkNodeDigest "LocalFunM" sigImms []
          (formalVids, formalLocalArgs) = allocLocalArgsCSE @e funKey 0 actualArgs
          retSpec = HRSCons trB capB HRSNil
          selfStub callLocalArgs = do
            let callHArgs = localArgsToHArgs callLocalArgs
            vid <- allocSSAId trB (Just capB)
            emitStm (StmFixCall (HCons vid HNil) mfd callHArgs)
            pure (mkExpVar vid)
          rawHBody = (\r -> HCons r HNil) <$> step selfStub formalLocalArgs
          emitTarget stepH tgtHArgs = do
            resVal <-
              emitLocalFunArgsM @m @e
                name
                ( \selfTgt formalTgtLA -> do
                    let selfH hArgs = (\v -> HCons v HNil) <$> selfTgt (hArgsToLocalArgs actualArgs hArgs)
                    hsOut <- stepH selfH (localArgsToHArgs formalTgtLA)
                    case hsOut of
                      HCons r HNil -> pure r
                )
                (hArgsToLocalArgs actualArgs tgtHArgs)
            pure (HCons resVal HNil)
          mfd =
            MonadicFunDef
              { mfdKey = funKey,
                mfdName = name,
                mfdParams = formalVids,
                mfdRetSpec = retSpec,
                mfdBody = cseM rawHBody,
                mfdEmitTarget = emitTarget
              }
      selfStub actualArgs

  emitLocalFunArgsM_ ::
    forall ts.
    String ->
    ((LocalArgs (CSEExp e) ts -> CSEM m e ()) -> LocalArgs (CSEExp e) ts -> CSEM m e ()) ->
    LocalArgs (CSEExp e) ts ->
    CSEM m e ()
  emitLocalFunArgsM_ name step actualArgs = do
    let inTrs = localArgsSomeTypeReps actualArgs
        sigImms =
          encodeStr name
            : encodeInt (length inTrs)
            : map encodeSomeTypeRep inTrs
            <> [encodeInt 0]
        funKey = mkNodeDigest "LocalFunM_" sigImms []
        (formalVids, formalLocalArgs) = allocLocalArgsCSE @e funKey 0 actualArgs
        selfStub callLocalArgs = do
          let callHArgs = localArgsToHArgs callLocalArgs
          emitStm (StmFixCall HNil mfd callHArgs)
        rawHBody = HNil <$ step selfStub formalLocalArgs
        emitTarget stepH tgtHArgs = do
          emitLocalFunArgsM_ @m @e
            name
            ( \selfTgt formalTgtLA -> do
                let selfH hArgs = HNil <$ selfTgt (hArgsToLocalArgs actualArgs hArgs)
                hsOut <- stepH selfH (localArgsToHArgs formalTgtLA)
                case hsOut of
                  HNil -> pure ()
            )
            (hArgsToLocalArgs actualArgs tgtHArgs)
          pure HNil
        mfd =
          MonadicFunDef
            { mfdKey = funKey,
              mfdName = name,
              mfdParams = formalVids,
              mfdRetSpec = HRSNil,
              mfdBody = cseM rawHBody,
              mfdEmitTarget = emitTarget
            }
    selfStub actualArgs

instance (CmdJanus m e) => CmdJanus (CSEM m e) (CSEExp e)

instance (CmdJanusIO m e) => CmdJanusIO (CSEM m e) (CSEExp e)

instance (CmdCond m e) => CmdCond (CSEM m e) (CSEExp e) where
  ifThenElseM_ cond tAct fAct = CSEM $ \tfm n0 ->
    let (n1, tTree) = unCSEM tAct tfm n0
        (n2, fTree) = unCSEM fAct tfm n1
        tBlk = progTreeUnitToBlock tTree
        fBlk = progTreeUnitToBlock fTree
     in (n2, ProgLinear (Seq.singleton (ItemStm (StmIf cond tBlk fBlk))) ())

instance (CmdWhile m e) => CmdWhile (CSEM m e) (CSEExp e) where
  whileM cond bodyFn = CSEM $ \tfm n0 ->
    let loopRef = CSERef (RefLocal n0) (TR.typeRep @Bool) BindBool
        (n1, bTree) = unCSEM (bodyFn loopRef) tfm (n0 + 1)
        bodyBlk = progTreeUnitToBlock bTree
     in (n1, ProgLinear (Seq.singleton (ItemStm (StmWhile cond loopRef bodyBlk))) ())

instance (CmdRange m e) => CmdRange (CSEM m e) (CSEExp e) where
  rangeM lo hi bodyFn = CSEM $ \tfm n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @Int64) (Just BindInt64)
        (n1, bTree) = unCSEM (bodyFn (mkExpVar vid)) tfm (n0 + 1)
        bodyBlk = progTreeUnitToBlock bTree
     in (n1, ProgLinear (Seq.singleton (ItemStm (StmRange lo hi vid bodyBlk))) ())

instance (CmdCase m e) => CmdCase (CSEM m e) (CSEExp e) where
  switchCase_ tag cases defAct = CSEM $ \tfm n0 ->
    let stepCase (n, acc) (k, act) =
          let (n', tree) = unCSEM act tfm n
              blk = progTreeUnitToBlock tree
           in (n', acc <> [(k, blk)])
        (n1, caseBlks) = foldl stepCase (n0, []) cases
        (n2, defTree) = unCSEM defAct tfm n1
        defBlk = progTreeUnitToBlock defTree
     in (n2, ProgLinear (Seq.singleton (ItemStm (StmSwitch tag caseBlks defBlk))) ())

instance (CmdMem m e) => CmdMem (CSEM m e) (CSEExp e) where
  malloc sz =
    emitEffectVal
      (TR.typeRep @(Ptr ()))
      (Just BindVoidPtr)
      (HCons sz HNil)
      (\(HCons vsz HNil) -> malloc vsz)
  calloc cnt sz =
    emitEffectVal
      (TR.typeRep @(Ptr ()))
      (Just BindVoidPtr)
      (HCons cnt (HCons sz HNil))
      (\(HCons vc (HCons vsz HNil)) -> calloc vc vsz)
  realloc p sz =
    emitEffectVal
      (TR.typeRep @(Ptr ()))
      (Just BindVoidPtr)
      (HCons p (HCons sz HNil))
      (\(HCons vp (HCons vsz HNil)) -> realloc vp vsz)
  free p =
    emitEffectUnit (HCons p HNil) (\(HCons vp HNil) -> free vp)

instance (CmdDynamicAligned m e) => CmdDynamicAligned (CSEM m e) (CSEExp e) where
  alignedMalloc sz al =
    emitEffectVal
      (TR.typeRep @(Ptr ()))
      (Just BindVoidPtr)
      (HCons sz (HCons al HNil))
      (\(HCons vsz (HCons val HNil)) -> alignedMalloc vsz val)

instance (CmdMemCopy m e) => CmdMemCopy (CSEM m e) (CSEExp e) where
  cmdMemcpy dst src n =
    emitEffectUnit
      (HCons dst (HCons src (HCons n HNil)))
      (\(HCons vd (HCons vs (HCons vn HNil))) -> cmdMemcpy vd vs vn)
  cmdMemmove dst src n =
    emitEffectUnit
      (HCons dst (HCons src (HCons n HNil)))
      (\(HCons vd (HCons vs (HCons vn HNil))) -> cmdMemmove vd vs vn)
  cmdMemset dst val n =
    emitEffectUnit
      (HCons dst (HCons val (HCons n HNil)))
      (\(HCons vd (HCons vv (HCons vn HNil))) -> cmdMemset vd vv vn)

instance (CmdStorable m e a, Typeable a) => CmdStorable (CSEM m e) (CSEExp e) a where
  peek p =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p HNil)
      (\(HCons vp HNil) -> peek vp)
  poke p val =
    emitEffectUnit
      (HCons p (HCons val HNil))
      (\(HCons vp (HCons vv HNil)) -> poke vp vv)
  peekElemOff p idx =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons idx HNil))
      (\(HCons vp (HCons vi HNil)) -> peekElemOff vp vi)
  pokeElemOff p val idx =
    emitEffectUnit
      (HCons p (HCons val (HCons idx HNil)))
      (\(HCons vp (HCons vv (HCons vi HNil))) -> pokeElemOff vp vv vi)

instance (CmdRefPtr m e a, Typeable a) => CmdRefPtr (CSEM m e) (CSEExp e) a where
  withRefPtr ref bodyFn = CSEM $ \tfm n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @(Ptr a)) (Just (BindPtrOf (BindDirect @e @a)))
        (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) tfm (n0 + 1)
     in (n1, wrapScopeWith (ItemWithRefPtr ref vid) innerTree)

instance (CmdIO m e) => CmdIO (CSEM m e) (CSEExp e) where
  stdout = mkExpLit "CmdIO.stdout" [] (Just (TR.typeRep @(Ptr CFile), BindCFileP)) stdout
  stderr = mkExpLit "CmdIO.stderr" [] (Just (TR.typeRep @(Ptr CFile), BindCFileP)) stderr
  fopen path mode =
    emitEffectVal
      (TR.typeRep @(Ptr CFile))
      (Just BindCFileP)
      (HCons path (HCons mode HNil))
      (\(HCons vp (HCons vm HNil)) -> fopen vp vm)
  fclose h =
    emitEffectVal
      (TR.typeRep @CInt)
      (Just BindCInt)
      (HCons h HNil)
      (\(HCons vh HNil) -> fclose vh)

instance (CmdString m e) => CmdString (CSEM m e) (CSEExp e) where
  withString s bodyFn = CSEM $ \tfm n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @CString) (Just BindCString)
        (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) tfm (n0 + 1)
     in (n1, wrapScopeWith (ItemWithString s vid) innerTree)

instance (CmdPutString m e) => CmdPutString (CSEM m e) (CSEExp e) where
  hputString h s =
    emitEffectUnit
      (HCons h (HCons s HNil))
      (\(HCons vh (HCons vs HNil)) -> hputString vh vs)

instance (CmdFormat m e a) => CmdFormat (CSEM m e) (CSEExp e) a where
  hformat h val term =
    emitEffectUnit
      (HCons h (HCons val HNil))
      (\(HCons vh (HCons vv HNil)) -> hformat vh vv term)

instance (Functor m, CmdTicToc m e) => CmdTicToc (CSEM m e) (CSEExp e) where
  tic = do
    t <-
      emitEffectVal
        (TR.typeRep @Word64)
        (Just BindWord64)
        HNil
        (\HNil -> getTic <$> tic)
    pure (Tic t)
  toc = do
    t <-
      emitEffectVal
        (TR.typeRep @Word64)
        (Just BindWord64)
        HNil
        (\HNil -> getToc <$> toc)
    pure (Toc t)

instance (CmdSync m) => CmdSync (CSEM m e) where
  syncWarp = emitEffectUnit HNil (\HNil -> syncWarp)
  syncBlock = emitEffectUnit HNil (\HNil -> syncBlock)

cudaThreadInfoVarIds :: CUDAThreadInfo (VarId e)
cudaThreadInfoVarIds =
  let mk i = VarId (VarCUDAField i) (TR.typeRep @Int32) (Just BindInt32)
   in CUDAThreadInfo
        { _cudaThreadInfoBlockIdxX = mk 0,
          _cudaThreadInfoBlockIdxY = mk 1,
          _cudaThreadInfoBlockIdxZ = mk 2,
          _cudaThreadInfoBlockDimX = mk 3,
          _cudaThreadInfoBlockDimY = mk 4,
          _cudaThreadInfoBlockDimZ = mk 5,
          _cudaThreadInfoThreadIdxX = mk 6,
          _cudaThreadInfoThreadIdxY = mk 7,
          _cudaThreadInfoThreadIdxZ = mk 8,
          _cudaThreadInfoGridDimX = mk 9,
          _cudaThreadInfoGridDimY = mk 10,
          _cudaThreadInfoGridDimZ = mk 11
        }

instance
  (MonadReader (CUDAThreadInfo e) m) =>
  MonadReader (CUDAThreadInfo (CSEExp e)) (CSEM m e)
  where
  ask = do
    let tiVars = cudaThreadInfoVarIds @e
    emitStm (StmCUDAAsk tiVars)
    pure (mapCUDAThreadInfo mkExpVar tiVars)
  local modFn (CSEM buildFn) = CSEM $ \tfm n0 ->
    let tiVars = cudaThreadInfoVarIds @e
        tiModified = modFn (mapCUDAThreadInfo mkExpVar tiVars)
        sub =
          Map.fromList
            [ (VarCUDAField 0, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockIdxX tiModified)),
              (VarCUDAField 1, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockIdxY tiModified)),
              (VarCUDAField 2, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockIdxZ tiModified)),
              (VarCUDAField 3, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockDimX tiModified)),
              (VarCUDAField 4, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockDimY tiModified)),
              (VarCUDAField 5, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoBlockDimZ tiModified)),
              (VarCUDAField 6, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoThreadIdxX tiModified)),
              (VarCUDAField 7, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoThreadIdxY tiModified)),
              (VarCUDAField 8, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoThreadIdxZ tiModified)),
              (VarCUDAField 9, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoGridDimX tiModified)),
              (VarCUDAField 10, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoGridDimY tiModified)),
              (VarCUDAField 11, SomeCSEExp (TR.typeRep @Int32) (_cudaThreadInfoGridDimZ tiModified))
            ]
        (n1, tree) = buildFn tfm n0
     in (n1, substCUDAInTree sub tree)
    where
      substCUDAInExp :: forall x. Map.Map VarKey (SomeCSEExp e) -> CSEExp e x -> CSEExp e x
      substCUDAInExp sub expr =
        let meta = expMeta expr
         in case expr of
              ExpVar _ (VarId vk tr _) ->
                case Map.lookup vk sub of
                  Just (SomeCSEExp trSub eSub)
                    | Just Refl <- testEquality tr trSub -> eSub
                  _ -> expr
              ExpLit {} -> expr
              ExpOp _ tag imms canTrap zeroCost args eval ->
                let args' = runIdentity (mapHArgsM (Identity . substCUDAInExp sub) args)
                 in if zeroCost
                      then mkExpZeroCostOp tag imms (emBind meta) args' eval
                      else mkExpOp tag imms canTrap (emBind meta) args' eval
              ExpListOp _ tag imms xs eval ->
                mkExpListOp tag imms (emBind meta) (map (substCUDAInExp sub) xs) eval
              ExpShortCircuit _ op l r eval ->
                mkExpShortCircuit op (substCUDAInExp sub l) (substCUDAInExp sub r) eval
              ExpCondNode _ c t f eval ->
                case emBind meta of
                  Just (tr, cap) ->
                    mkExpCond tr cap (substCUDAInExp sub c) (substCUDAInExp sub t) (substCUDAInExp sub f) eval
                  Nothing -> expr
              ExpLetNode _ vid capA capB rhs body ->
                case emBind meta of
                  Just (trB, _) ->
                    mkExpCSELet vid capA trB capB (substCUDAInExp sub rhs) (substCUDAInExp sub body)
                  Nothing -> expr
              ExpFixCall _ pfd args ->
                mkExpFixCall pfd (runIdentity (mapHArgsM (Identity . substCUDAInExp sub) args))

      substCUDAInBlk sub = fmap (substCUDAInItem sub)
      substCUDAInItem sub = \case
        ItemStm s -> ItemStm (substCUDAInStm sub s)
        ItemWithRefPtr ref vid b -> ItemWithRefPtr ref vid (substCUDAInBlk sub b)
        ItemWithString str vid b -> ItemWithString str vid (substCUDAInBlk sub b)
        ItemWithSharedArray pn vid b -> ItemWithSharedArray pn vid (substCUDAInBlk sub b)
        ItemWithStackBytes nb vid b -> ItemWithStackBytes nb vid (substCUDAInBlk sub b)
        ItemOptimalGang tr b2 b4 b8 b16 b32 ->
          ItemOptimalGang
            tr
            (substCUDAInBlk sub b2)
            (substCUDAInBlk sub b4)
            (substCUDAInBlk sub b8)
            (substCUDAInBlk sub b16)
            (substCUDAInBlk sub b32)
      substCUDAInStm sub = \case
        StmLetM vid cap rhs -> StmLetM vid cap (substCUDAInExp sub rhs)
        StmEffectVal vid args eval ->
          StmEffectVal vid (runIdentity (mapHArgsM (Identity . substCUDAInExp sub) args)) eval
        StmEffectUnit args eval ->
          StmEffectUnit (runIdentity (mapHArgsM (Identity . substCUDAInExp sub) args)) eval
        StmNewRef ref mb -> StmNewRef ref (fmap (substCUDAInExp sub) mb)
        StmReadRef vid ref -> StmReadRef vid ref
        StmWriteRef ref val -> StmWriteRef ref (substCUDAInExp sub val)
        StmIf c t f -> StmIf (substCUDAInExp sub c) (substCUDAInBlk sub t) (substCUDAInBlk sub f)
        StmWhile c ref b -> StmWhile (substCUDAInExp sub c) ref (substCUDAInBlk sub b)
        StmRange l u vid b -> StmRange (substCUDAInExp sub l) (substCUDAInExp sub u) vid (substCUDAInBlk sub b)
        StmSwitch tag cs defB ->
          StmSwitch (substCUDAInExp sub tag) [(k, substCUDAInBlk sub b) | (k, b) <- cs] (substCUDAInBlk sub defB)
        StmCUDAAsk ti -> StmCUDAAsk ti
        StmFixCall vid mfd args ->
          StmFixCall vid mfd (runIdentity (mapHArgsM (Identity . substCUDAInExp sub) args))

      substCUDAInTree sub = \case
        ProgLinear b r -> ProgLinear (substCUDAInBlk sub b) r
        ProgOptimalGang tr b2 b4 b8 b16 b32 ->
          ProgOptimalGang
            tr
            (substCUDAInTree sub b2)
            (substCUDAInTree sub b4)
            (substCUDAInTree sub b8)
            (substCUDAInTree sub b16)
            (substCUDAInTree sub b32)

--------------------------------------------------------------------------------
-- Special Math, Shared Memory, Warp Shuffle, Atomics, & Parallel For Instances
--------------------------------------------------------------------------------

instance (ExpSpecialMath e a, Floating (CSEExp e a), CSETyped e a) => ExpSpecialMath (CSEExp e) a where
  lgammaE = unOpTyped "lgamma" [] False lgammaE
  digammaE = unOpTyped "digamma" [] False digammaE
  trigammaE = unOpTyped "trigamma" [] False trigammaE
  erfE = unOpTyped "erf" [] False erfE
  erfcE = unOpTyped "erfc" [] False erfcE
  erfinvE = unOpTyped "erfinv" [] False erfinvE
  log1pE = unOpTyped "log1p" [] False log1pE
  expm1E = unOpTyped "expm1" [] False expm1E
  rsqrtE = unOpTyped "rsqrt" [] False rsqrtE

instance (CmdSharedMem m e, ExpJanus e) => CmdSharedMem (CSEM m e) (CSEExp e) where
  withSharedArray ::
    forall n a r.
    (KnownNat n, Typeable a, Storable a, JanusCTyped a) =>
    Proxy n ->
    (CSEExp e (Ptr a) -> CSEM m e r) ->
    CSEM m e r
  withSharedArray pn bodyFn =
    withJanusType @(Ptr a) @e $
      CSEM $ \tfm n0 ->
        let vid = VarId (VarSSA n0) (TR.typeRep @(Ptr a)) (Just (BindDirect @e @(Ptr a)))
            (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) tfm (n0 + 1)
         in (n1, wrapScopeWith (ItemWithSharedArray pn vid) innerTree)

instance (CmdStackArray m e, ExpJanus e) => CmdStackArray (CSEM m e) (CSEExp e) where
  withStackBytes nBytes bodyFn =
    withJanusType @(Ptr Word8) @e $
      CSEM $ \tfm n0 ->
        let vid = VarId (VarSSA n0) (TR.typeRep @(Ptr Word8)) (Just (BindDirect @e @(Ptr Word8)))
            (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) tfm (n0 + 1)
         in (n1, wrapScopeWith (ItemWithStackBytes nBytes vid) innerTree)

instance (CmdWarpShuffle m e a, CSETyped e a) => CmdWarpShuffle (CSEM m e) (CSEExp e) a where
  shflDownSync m v d =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons m (HCons v (HCons d HNil)))
      (\(HCons m' (HCons v' (HCons d' HNil))) -> shflDownSync m' v' d')
  shflUpSync m v d =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons m (HCons v (HCons d HNil)))
      (\(HCons m' (HCons v' (HCons d' HNil))) -> shflUpSync m' v' d')
  shflXorSync m v d =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons m (HCons v (HCons d HNil)))
      (\(HCons m' (HCons v' (HCons d' HNil))) -> shflXorSync m' v' d')
  shflSync m v d =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons m (HCons v (HCons d HNil)))
      (\(HCons m' (HCons v' (HCons d' HNil))) -> shflSync m' v' d')

instance (CmdAtomic m e) => CmdAtomic (CSEM m e) (CSEExp e) where
  atomicCAS32Ptr p c v =
    emitEffectVal
      (TR.typeRep @Word32)
      (Just BindWord32)
      (HCons p (HCons c (HCons v HNil)))
      (\(HCons p' (HCons c' (HCons v' HNil))) -> atomicCAS32Ptr p' c' v')
  atomicCAS64Ptr p c v =
    emitEffectVal
      (TR.typeRep @Word64)
      (Just BindWord64)
      (HCons p (HCons c (HCons v HNil)))
      (\(HCons p' (HCons c' (HCons v' HNil))) -> atomicCAS64Ptr p' c' v')

instance (CmdAtomicAdd m e a, CSETyped e a) => CmdAtomicAdd (CSEM m e) (CSEExp e) a where
  atomicAddPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicAddPtr p' v')

instance (CmdAtomicMinMax m e a, CSETyped e a) => CmdAtomicMinMax (CSEM m e) (CSEExp e) a where
  atomicMinPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicMinPtr p' v')
  atomicMaxPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicMaxPtr p' v')

instance (CmdAtomicBits m e a, CSETyped e a) => CmdAtomicBits (CSEM m e) (CSEExp e) a where
  atomicAndPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicAndPtr p' v')
  atomicOrPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicOrPtr p' v')
  atomicXorPtr p v =
    emitEffectVal
      (TR.typeRep @a)
      (Just (BindDirect @e @a))
      (HCons p (HCons v HNil))
      (\(HCons p' (HCons v' HNil)) -> atomicXorPtr p' v')

parLocalArgsToHArgs :: LocalArgs f ts -> HArgs f ts
parLocalArgsToHArgs LANil = HNil
parLocalArgsToHArgs (LACons x rest) = HCons x (parLocalArgsToHArgs rest)

parRebuildLocalArgs :: LocalArgs proxy ts -> HArgs f ts -> LocalArgs f ts
parRebuildLocalArgs LANil HNil = LANil
parRebuildLocalArgs (LACons _ pRest) (HCons x rest) = LACons x (parRebuildLocalArgs pRest rest)

instance
  ( CmdParallel m e,
    CmdJanus (CSEM m e) (CSEExp e)
  ) =>
  CmdParallel (CSEM m e) (CSEExp e)
  where
  parallelForRangeM label lo hi minGrain args body =
    emitEffectUnit
      (HCons lo (HCons hi (HCons minGrain (parLocalArgsToHArgs args))))
      (\(HCons lo' (HCons hi' (HCons grain' hArgs'))) -> parallelForRangeM label lo' hi' grain' (parRebuildLocalArgs args hArgs') body)
