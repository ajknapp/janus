{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Janus expression, SIMD, fixpoint, reference, memory, I/O, and control-flow
-- typeclass instances for @('CSEM' m e, 'CSEExp' e)@.
module Janus.CSE.Instances
  ( CSEFix (..),
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
import GHC.Float (expm1, log1p)
import GHC.TypeNats (natVal)
import Janus.Backend.C.CUDA.Types (CUDAThreadInfo (..), CmdSync (..), mapCUDAThreadInfo)
import Janus.CSE.Interpret (interpretCSEExp)
import Janus.CSE.Optimize (SomeCSEExp (..), cseExp, cseM)
import Janus.CSE.Types
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
import Janus.Constraints (CmdJanus, CmdJanusIO, ExpJanus)
import Janus.Control.Reader.Base (ExpNewtype (..))
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
import Janus.Fix (ExpFix (..))
import Janus.SPMD.Types (ExpPair (..), SIMDPair)
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

instance ExpLet (CSEExp e) where
  let_ = mkExpUserLet

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

class CSEFix (e :: K.Type -> K.Type) (r :: K.Type) | r -> e where
  type CSEFixTarget r :: K.Type
  type CSEFixParams r :: [K.Type]
  type CSEFixLeaf r :: K.Type
  type CSEFixTargetLeaf r :: K.Type
  cseAllocFixParams :: Digest -> Int -> (HArgs (VarId e) (CSEFixParams r), r -> CSEFixLeaf r)
  cseMakeFixStub :: (HArgs (CSEExp e) (CSEFixParams r) -> CSEFixLeaf r) -> r
  cseCurryFixTarget :: (HArgs e (CSEFixParams r) -> CSEFixTargetLeaf r) -> CSEFixTarget r
  cseUncurryFixTarget :: CSEFixTarget r -> HArgs e (CSEFixParams r) -> CSEFixTargetLeaf r

instance (CSETyped e b) => CSEFix e (CSEExp e b) where
  type CSEFixTarget (CSEExp e b) = e b
  type CSEFixParams (CSEExp e b) = '[]
  type CSEFixLeaf (CSEExp e b) = CSEExp e b
  type CSEFixTargetLeaf (CSEExp e b) = e b
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = k HNil
  cseUncurryFixTarget v HNil = v

instance (CSETyped e b) => CSEFix e (CSEM m e (CSEExp e b)) where
  type CSEFixTarget (CSEM m e (CSEExp e b)) = m (e b)
  type CSEFixParams (CSEM m e (CSEExp e b)) = '[]
  type CSEFixLeaf (CSEM m e (CSEExp e b)) = CSEM m e (CSEExp e b)
  type CSEFixTargetLeaf (CSEM m e (CSEExp e b)) = m (e b)
  cseAllocFixParams _ _ = (HNil, id)
  cseMakeFixStub mkLeaf = mkLeaf HNil
  cseCurryFixTarget k = k HNil
  cseUncurryFixTarget v HNil = v

instance (CSETyped e a, CSEFix e r) => CSEFix e (CSEExp e a -> r) where
  type CSEFixTarget (CSEExp e a -> r) = e a -> CSEFixTarget r
  type CSEFixParams (CSEExp e a -> r) = a ': CSEFixParams r
  type CSEFixLeaf (CSEExp e a -> r) = CSEFixLeaf r
  type CSEFixTargetLeaf (CSEExp e a -> r) = CSEFixTargetLeaf r
  cseAllocFixParams funKey idx =
    let vid = VarId (VarLocalFun funKey idx) (TR.typeRep @a) (Just (BindDirect @e @a))
        (restVids, peelRest) = cseAllocFixParams @e @r funKey (idx + 1)
     in (HCons vid restVids, \fn -> peelRest (fn (mkExpVar vid)))
  cseMakeFixStub mkLeaf =
    \arg -> cseMakeFixStub @e @r (\restArgs -> mkLeaf (HCons arg restArgs))
  cseCurryFixTarget k =
    \x -> cseCurryFixTarget @e @r (\xs -> k (HCons x xs))
  cseUncurryFixTarget fn (HCons x xs) =
    cseUncurryFixTarget @e @r (fn x) xs

class CSEFixLeafBuilder (e :: K.Type -> K.Type) (r :: K.Type) where
  cseBuildLeaf ::
    Digest ->
    String ->
    HArgs (VarId e) params ->
    (((HArgs e params -> CSEFixTargetLeaf r) -> HArgs e params -> CSEFixTargetLeaf r) -> HArgs e params -> CSEFixTargetLeaf r) ->
    CSEFixLeaf r ->
    (HArgs (CSEExp e) params -> CSEFixLeaf r)

instance (CSETyped e b) => CSEFixLeafBuilder e (CSEExp e b) where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let pfd =
          PureFunDef
            { pfdKey = funKey,
              pfdName = name,
              pfdParams = formalParams,
              pfdRetType = TR.typeRep @b,
              pfdRetBind = BindDirect @e @b,
              pfdBody = cseExp rawBody,
              pfdEmitTarget = emitWithTarget
            }
     in mkExpFixCall pfd

instance (CSETyped e b) => CSEFixLeafBuilder e (CSEM m e (CSEExp e b)) where
  cseBuildLeaf funKey name formalParams emitWithTarget rawBody =
    let mfd =
          MonadicFunDef
            { mfdKey = funKey,
              mfdName = name,
              mfdParams = formalParams,
              mfdRetType = TR.typeRep @b,
              mfdRetBind = BindDirect @e @b,
              mfdBody = cseM rawBody,
              mfdEmitTarget = emitWithTarget
            }
     in \callArgs -> do
          vid <- allocSSAId (TR.typeRep @b) (Just (BindDirect @e @b))
          emitStm (StmFixCall vid mfd callArgs)
          pure (mkExpVar vid)

instance (CSEFixLeafBuilder e r) => CSEFixLeafBuilder e (CSEExp e a -> r) where
  cseBuildLeaf = cseBuildLeaf @e @r

instance
  ( Typeable (CSEFixTarget r),
    CSEFix e r,
    CSEFixLeafBuilder e r,
    ExpFix e (CSEFixTarget r)
  ) =>
  ExpFix (CSEExp e) r
  where
  fun _ name stepFn =
    let funKey =
          mkNodeDigest
            "ExpFix"
            [encodeStr name, encodeTypeRep (TR.typeRep @(CSEFixTarget r))]
            []
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
allocLocalRef = CSEM $ \n ->
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

instance (CmdJanus m e) => CmdJanus (CSEM m e) (CSEExp e)

instance (CmdJanusIO m e) => CmdJanusIO (CSEM m e) (CSEExp e)

runBranchTree ::
  Int ->
  ProgTree m e () ->
  (Int, Block m e)
runBranchTree n0 = \case
  ProgLinear blk () -> (n0, blk)
  _ -> (n0, Seq.empty)

instance (CmdCond m e) => CmdCond (CSEM m e) (CSEExp e) where
  ifThenElseM_ cond tAct fAct = CSEM $ \n0 ->
    let (n1, tTree) = unCSEM tAct n0
        (n2, fTree) = unCSEM fAct n1
     in case (tTree, fTree) of
          (ProgLinear tBlk (), ProgLinear fBlk ()) ->
            (n2, ProgLinear (Seq.singleton (ItemStm (StmIf cond tBlk fBlk))) ())
          _ ->
            let (_, tB) = runBranchTree n1 tTree
                (_, fB) = runBranchTree n2 fTree
             in (n2, ProgLinear (Seq.singleton (ItemStm (StmIf cond tB fB))) ())

instance (CmdWhile m e) => CmdWhile (CSEM m e) (CSEExp e) where
  whileM cond bodyFn = CSEM $ \n0 ->
    let loopRef = CSERef (RefLocal n0) (TR.typeRep @Bool) BindBool
        (n1, bTree) = unCSEM (bodyFn loopRef) (n0 + 1)
        (_, bodyBlk) = runBranchTree n1 bTree
     in (n1, ProgLinear (Seq.singleton (ItemStm (StmWhile cond loopRef bodyBlk))) ())

instance (CmdRange m e) => CmdRange (CSEM m e) (CSEExp e) where
  rangeM lo hi bodyFn = CSEM $ \n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @Int64) (Just BindInt64)
        (n1, bTree) = unCSEM (bodyFn (mkExpVar vid)) (n0 + 1)
        (_, bodyBlk) = runBranchTree n1 bTree
     in (n1, ProgLinear (Seq.singleton (ItemStm (StmRange lo hi vid bodyBlk))) ())

instance (CmdCase m e) => CmdCase (CSEM m e) (CSEExp e) where
  switchCase_ tag cases defAct = CSEM $ \n0 ->
    let stepCase (n, acc) (k, act) =
          let (n', tree) = unCSEM act n
              (_, blk) = runBranchTree n' tree
           in (n', acc <> [(k, blk)])
        (n1, caseBlks) = foldl stepCase (n0, []) cases
        (n2, defTree) = unCSEM defAct n1
        (_, defBlk) = runBranchTree n2 defTree
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
  withRefPtr ref bodyFn = CSEM $ \n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @(Ptr a)) (Just (BindPtrOf (BindDirect @e @a)))
        (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) (n0 + 1)
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
  withString s bodyFn = CSEM $ \n0 ->
    let vid = VarId (VarSSA n0) (TR.typeRep @CString) (Just BindCString)
        (n1, innerTree) = unCSEM (bodyFn (mkExpVar vid)) (n0 + 1)
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
  local modFn (CSEM buildFn) = CSEM $ \n0 ->
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
        (n1, tree) = buildFn n0
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
