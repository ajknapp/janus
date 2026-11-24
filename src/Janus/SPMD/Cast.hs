{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.SPMD.Cast where

import Control.Monad.Fix (fix)
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex (..))
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import Foreign.C.Types (CDouble, CFloat)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C.CUDA.Types (CUDADejaFu, CUDAIO)
import qualified Janus.Command.Memory as M
import Janus.Expression.Cast
  ( ExpBoolCast (..),
    ExpFloatingCast (..),
    ExpIntegralCast (..),
    ExpPtrCast (..),
  )
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Inject (ExpInject (..))
import Janus.Expression.MurmurHash (ExpHash', ExpMurmurHash (..), hash64WithSalt)
import Janus.Expression.SIMD (ExpSIMDCast (..), ExpSIMDMask (..), SIMDShape, SIMDVector)
import Janus.Fix (ExpFix (..))
import Janus.SPMD.Expression ()
import Janus.SPMD.Raw
import Janus.SPMD.Typed
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 11b. Casts, Bounded, ExpInject, ExpMurmurHash, ExpSized, ExpPtr, ExpFix
--------------------------------------------------------------------------------

instance (Bounded a, ExpInject (SPMD n e) a) => Bounded (SPMD n e a) where
  minBound = inject minBound
  maxBound = inject maxBound

instance
  ( SPMDTyped n e a,
    ExpCond (SPMD n e),
    ExpEq (SPMD n e) a,
    Num (SPMD n e a)
  ) =>
  ExpBoolCast (SPMD n e) a
  where
  fromBool b = ifThenElse b 1 0
  toBool x = x `ne` 0

class SPMDCvtByLayout (la :: Layout) (lb :: Layout) (n :: Nat) e a b where
  spmdCvtL :: SPMDExprByLayout la n e a -> SPMDExprByLayout lb n e b

instance (SPMDRawCvt 'LDirect 'LDirect n e a b) => SPMDCvtByLayout 'LDirect 'LDirect n e a b where
  spmdCvtL = rawCvt @'LDirect @'LDirect @n @e @a @b

instance (SPMDRawCvt ('LSplit h) ('LSplit h) n e a b) => SPMDCvtByLayout ('LSplit h) ('LSplit h) n e a b where
  spmdCvtL = rawCvt @('LSplit h) @('LSplit h) @n @e @a @b

instance (SPMDRawCvt 'LDirect ('LSplit h) n e a b) => SPMDCvtByLayout 'LDirect ('LSplit h) n e a b where
  spmdCvtL = rawCvt @'LDirect @('LSplit h) @n @e @a @b

instance (SPMDRawCvt ('LSplit h) 'LDirect n e a b) => SPMDCvtByLayout ('LSplit h) 'LDirect n e a b where
  spmdCvtL = rawCvt @('LSplit h) @'LDirect @n @e @a @b

instance (SPMDRawCvt 'LPad4 'LPad4 2 e a b) => SPMDCvtByLayout 'LPad4 'LPad4 2 e a b where
  spmdCvtL = rawCvt @'LPad4 @'LPad4 @2 @e @a @b

instance (SPMDRawCvt 'LPad4 'LDirect 2 e a b) => SPMDCvtByLayout 'LPad4 'LDirect 2 e a b where
  spmdCvtL = rawCvt @'LPad4 @'LDirect @2 @e @a @b

instance (SPMDRawCvt 'LDirect 'LPad4 2 e a b) => SPMDCvtByLayout 'LDirect 'LPad4 2 e a b where
  spmdCvtL = rawCvt @'LDirect @'LPad4 @2 @e @a @b

instance
  ( rl ~ SPMDRawLayout n Int64,
    SPMDRawNum rl n e Int64,
    Num (e Int64),
    SPMDRawCvt rl 'LDirect n e Int64 b
  ) =>
  SPMDCvtByLayout ('LInt64 rl) 'LDirect n e Int64 b
  where
  spmdCvtL x = rawCvt @rl @'LDirect @n @e @Int64 @b (materializeInt64 x)

instance
  ( rl ~ SPMDRawLayout n Int64,
    SPMDRawNum rl n e Int64,
    Num (e Int64),
    SPMDRawCvt rl ('LSplit h) n e Int64 b
  ) =>
  SPMDCvtByLayout ('LInt64 rl) ('LSplit h) n e Int64 b
  where
  spmdCvtL x = rawCvt @rl @('LSplit h) @n @e @Int64 @b (materializeInt64 x)

instance
  ( rl ~ SPMDRawLayout 2 Int64,
    SPMDRawNum rl 2 e Int64,
    Num (e Int64),
    SPMDRawCvt rl 'LPad4 2 e Int64 b
  ) =>
  SPMDCvtByLayout ('LInt64 rl) 'LPad4 2 e Int64 b
  where
  spmdCvtL x = rawCvt @rl @'LPad4 @2 @e @Int64 @b (materializeInt64 x)

instance
  (rl ~ SPMDRawLayout n Int64) =>
  SPMDCvtByLayout ('LInt64 rl) ('LInt64 rl) n e Int64 Int64
  where
  spmdCvtL = id

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawCvt 'LDirect rl n e a Int64) =>
  SPMDCvtByLayout 'LDirect ('LInt64 rl) n e a Int64
  where
  spmdCvtL v = I64Varying (rawCvt @'LDirect @rl @n @e @a @Int64 v)

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawCvt ('LSplit h) rl n e a Int64) =>
  SPMDCvtByLayout ('LSplit h) ('LInt64 rl) n e a Int64
  where
  spmdCvtL v = I64Varying (rawCvt @('LSplit h) @rl @n @e @a @Int64 v)

instance
  (rl ~ SPMDRawLayout 2 Int64, SPMDRawCvt 'LPad4 rl 2 e a Int64) =>
  SPMDCvtByLayout 'LPad4 ('LInt64 rl) 2 e a Int64
  where
  spmdCvtL v = I64Varying (rawCvt @'LPad4 @rl @2 @e @a @Int64 v)

instance
  (la0 ~ SPMDLayout n a0, SPMDCvtByLayout la0 lb n e a0 b) =>
  SPMDCvtByLayout ('LNewtype a0) lb n e a b
  where
  spmdCvtL = spmdCvtL @la0 @lb @n @e @a0 @b

instance
  (lb0 ~ SPMDLayout n b0, SPMDCvtByLayout 'LDirect lb0 n e a b0) =>
  SPMDCvtByLayout 'LDirect ('LNewtype b0) n e a b
  where
  spmdCvtL = spmdCvtL @'LDirect @lb0 @n @e @a @b0

instance
  (lb0 ~ SPMDLayout n b0, SPMDCvtByLayout ('LSplit h) lb0 n e a b0) =>
  SPMDCvtByLayout ('LSplit h) ('LNewtype b0) n e a b
  where
  spmdCvtL = spmdCvtL @('LSplit h) @lb0 @n @e @a @b0

instance
  (lb0 ~ SPMDLayout 2 b0, SPMDCvtByLayout 'LPad4 lb0 2 e a b0) =>
  SPMDCvtByLayout 'LPad4 ('LNewtype b0) 2 e a b
  where
  spmdCvtL = spmdCvtL @'LPad4 @lb0 @2 @e @a @b0

instance
  (lb0 ~ SPMDLayout n b0, SPMDCvtByLayout ('LInt64 rl) lb0 n e Int64 b0) =>
  SPMDCvtByLayout ('LInt64 rl) ('LNewtype b0) n e Int64 b
  where
  spmdCvtL = spmdCvtL @('LInt64 rl) @lb0 @n @e @Int64 @b0

type family IsFloatTarget (b :: K.Type) :: Bool where
  IsFloatTarget Float = 'True
  IsFloatTarget Double = 'True
  IsFloatTarget CFloat = 'True
  IsFloatTarget CDouble = 'True
  IsFloatTarget _ = 'False

instance
  ( la ~ SPMDLayout n a,
    lb ~ SPMDLayout n b,
    IsFloatTarget b ~ 'True,
    SPMDCvtByLayout la lb n e a b
  ) =>
  ExpFloatingCast (SPMD n e) a b
  where
  toFloating (SPMD ma) = SPMD (fmap (spmdCvtL @la @lb @n @e @a @b) ma)

instance
  ( la ~ SPMDLayout n a,
    lb ~ SPMDLayout n b,
    IsFloatTarget b ~ 'False,
    SPMDCvtByLayout la lb n e a b
  ) =>
  ExpIntegralCast (SPMD n e) a b
  where
  toIntegral (SPMD ma) = SPMD (fmap (spmdCvtL @la @lb @n @e @a @b) ma)

instance (ExpPtrCast e, PtrTreeOps n e VoidPtr) => ExpPtrCast (SPMD n e) where
  toVoidPtr :: forall a. (JanusTyped (SPMD n e) a) => SPMD n e (Ptr a) -> SPMD n e (Ptr ())
  toVoidPtr (SPMD mp) = SPMD (fmap (spmdToVoidPtr @n @e @a) mp)
  fromVoidPtr :: forall a. (JanusTyped (SPMD n e) a) => SPMD n e (Ptr ()) -> SPMD n e (Ptr a)
  fromVoidPtr (SPMD mvp) = SPMD (fmap (spmdFromVoidPtr @n @e @a) mvp)
  ptrCast = fromVoidPtr . toVoidPtr

-- ExpInject
class SPMDInjectByLayout (l :: Layout) (n :: Nat) e a where
  spmdInjectL :: a -> SPMDExprByLayout l n e a

instance (SPMDRawVec 'LDirect n e a, ExpInject e a) => SPMDInjectByLayout 'LDirect n e a where
  spmdInjectL x = rawSplat @'LDirect @n @e @a (inject x)

instance (SPMDRawVec ('LSplit h) n e a, ExpInject e a) => SPMDInjectByLayout ('LSplit h) n e a where
  spmdInjectL x = rawSplat @('LSplit h) @n @e @a (inject x)

instance (SPMDRawVec 'LPad4 2 e a, ExpInject e a) => SPMDInjectByLayout 'LPad4 2 e a where
  spmdInjectL x = rawSplat @'LPad4 @2 @e @a (inject x)

instance SPMDInjectByLayout ('LInt64 rl) n e Int64 where
  spmdInjectL = I64Const

instance (SPMDInjectByLayout (SPMDLayout n base) n e base, Coercible a base) => SPMDInjectByLayout ('LNewtype base) n e a where
  spmdInjectL x = spmdInjectL @(SPMDLayout n base) @n @e @base (coerce x)

instance (ExpSIMDMask e n (SPMDMaskElem n)) => SPMDInjectByLayout 'LBool n e Bool where
  spmdInjectL b = if b then mtrue else mfalse

instance (ExpInject e (Ptr b)) => SPMDInjectByLayout ('LPtr b) n e (Ptr b) where
  spmdInjectL p = PtrUniform True (inject p)

instance (ExpInject e (Ptr ())) => SPMDInjectByLayout 'LVoidPtr n e (Ptr ()) where
  spmdInjectL p = VoidPtrUniform True (inject p)

instance (ExpPair e) => SPMDInjectByLayout 'LUnit n e () where
  spmdInjectL () = mkSPMDUnit

instance
  ( rl ~ SPMDLayout n b,
    SPMDExpr n e b ~ e (SPMDRep n e b),
    SPMDInjectByLayout rl n e b,
    ExpPair e,
    JanusTyped e (SPMDRep n e b)
  ) =>
  SPMDInjectByLayout ('LComplex b) n e (Complex b)
  where
  spmdInjectL (r :+ i) =
    mkPair (spmdInjectL @rl @n @e @b r) (spmdInjectL @rl @n @e @b i)

instance (l ~ SPMDLayout n a, SPMDInjectByLayout l n e a) => ExpInject (SPMD n e) a where
  inject x = SPMD (pure (spmdInjectL @l @n @e @a x))

-- ExpMurmurHash
class SPMDMurmurElem (n :: Nat) e a where
  spmdElemToWord64 :: SPMD n e a -> SPMD n e Word64

instance
  ( la ~ SPMDLayout n a,
    lw ~ SPMDRawLayout n Word64,
    IsFloatTarget a ~ 'False,
    SPMDCvtByLayout la lw n e a Word64
  ) =>
  SPMDMurmurElem n e a
  where
  spmdElemToWord64 = toIntegral

instance {-# OVERLAPPING #-}
  ( lw32 ~ SPMDRawLayout n Word32,
    lw64 ~ SPMDRawLayout n Word64,
    SPMDCvtByLayout lw32 lw64 n e Word32 Word64,
    SPMDBitcastFloat32 n e
  ) =>
  SPMDMurmurElem n e Float
  where
  spmdElemToWord64 x = toIntegral (spmdBitcastFloatToWord32 x)

instance {-# OVERLAPPING #-}
  (SPMDBitcastFloat64 n e) =>
  SPMDMurmurElem n e Double
  where
  spmdElemToWord64 = spmdBitcastDoubleToWord64

class SPMDBitcastFloat32 (n :: Nat) e where
  spmdBitcastFloatToWord32 :: SPMD n e Float -> SPMD n e Word32

instance (SIMDShape 2 Float, SIMDShape 2 Word32, ExpSIMDCast e) => SPMDBitcastFloat32 2 e where
  spmdBitcastFloatToWord32 (SPMD mf) = SPMD (fmap (vbitcast @e @2 @Float @2 @Word32) mf)

instance (SIMDShape 4 Float, SIMDShape 4 Word32, ExpSIMDCast e) => SPMDBitcastFloat32 4 e where
  spmdBitcastFloatToWord32 (SPMD mf) = SPMD (fmap (vbitcast @e @4 @Float @4 @Word32) mf)

instance (SIMDShape 8 Float, SIMDShape 8 Word32, ExpSIMDCast e) => SPMDBitcastFloat32 8 e where
  spmdBitcastFloatToWord32 (SPMD mf) = SPMD (fmap (vbitcast @e @8 @Float @8 @Word32) mf)

instance
  ( ExpSIMDCast e,
    ExpPair e,
    JanusTyped e (SIMDVector e 8 Float),
    JanusTyped e (SIMDVector e 8 Word32)
  ) =>
  SPMDBitcastFloat32 16 e
  where
  spmdBitcastFloatToWord32 (SPMD mf) =
    SPMD (fmap (\p -> mkPair (vbitcast @e @8 @Float @8 @Word32 (fstPair p)) (vbitcast @e @8 @Float @8 @Word32 (sndPair p))) mf)

instance
  ( ExpSIMDCast e,
    ExpPair e,
    JanusTyped e (SIMDVector e 8 Float),
    JanusTyped e (SIMDVector e 8 Word32),
    JanusTyped e (SIMDPair (SIMDVector e 8 Float) (SIMDVector e 8 Float)),
    JanusTyped e (SIMDPair (SIMDVector e 8 Word32) (SIMDVector e 8 Word32))
  ) =>
  SPMDBitcastFloat32 32 e
  where
  spmdBitcastFloatToWord32 (SPMD mf) =
    let conv16 p = mkPair (vbitcast @e @8 @Float @8 @Word32 (fstPair p)) (vbitcast @e @8 @Float @8 @Word32 (sndPair p))
     in SPMD (fmap (\q -> mkPair (conv16 (fstPair q)) (conv16 (sndPair q))) mf)

class SPMDBitcastFloat64 (n :: Nat) e where
  spmdBitcastDoubleToWord64 :: SPMD n e Double -> SPMD n e Word64

instance (SIMDShape 2 Double, SIMDShape 2 Word64, ExpSIMDCast e) => SPMDBitcastFloat64 2 e where
  spmdBitcastDoubleToWord64 (SPMD md) = SPMD (fmap (vbitcast @e @2 @Double @2 @Word64) md)

instance (SIMDShape 4 Double, SIMDShape 4 Word64, ExpSIMDCast e) => SPMDBitcastFloat64 4 e where
  spmdBitcastDoubleToWord64 (SPMD md) = SPMD (fmap (vbitcast @e @4 @Double @4 @Word64) md)

instance
  ( ExpSIMDCast e,
    ExpPair e,
    JanusTyped e (SIMDVector e 4 Double),
    JanusTyped e (SIMDVector e 4 Word64)
  ) =>
  SPMDBitcastFloat64 8 e
  where
  spmdBitcastDoubleToWord64 (SPMD md) =
    SPMD (fmap (\p -> mkPair (vbitcast @e @4 @Double @4 @Word64 (fstPair p)) (vbitcast @e @4 @Double @4 @Word64 (sndPair p))) md)

instance
  ( ExpSIMDCast e,
    ExpPair e,
    JanusTyped e (SIMDVector e 4 Double),
    JanusTyped e (SIMDVector e 4 Word64),
    JanusTyped e (SIMDPair (SIMDVector e 4 Double) (SIMDVector e 4 Double)),
    JanusTyped e (SIMDPair (SIMDVector e 4 Word64) (SIMDVector e 4 Word64))
  ) =>
  SPMDBitcastFloat64 16 e
  where
  spmdBitcastDoubleToWord64 (SPMD md) =
    let conv8 p = mkPair (vbitcast @e @4 @Double @4 @Word64 (fstPair p)) (vbitcast @e @4 @Double @4 @Word64 (sndPair p))
     in SPMD (fmap (\q -> mkPair (conv8 (fstPair q)) (conv8 (sndPair q))) md)

instance
  ( ExpSIMDCast e,
    ExpPair e,
    JanusTyped e (SIMDVector e 4 Double),
    JanusTyped e (SIMDVector e 4 Word64),
    JanusTyped e (SIMDPair (SIMDVector e 4 Double) (SIMDVector e 4 Double)),
    JanusTyped e (SIMDPair (SIMDVector e 4 Word64) (SIMDVector e 4 Word64)),
    JanusTyped e (SIMDPair (SIMDPair (SIMDVector e 4 Double) (SIMDVector e 4 Double)) (SIMDPair (SIMDVector e 4 Double) (SIMDVector e 4 Double))),
    JanusTyped e (SIMDPair (SIMDPair (SIMDVector e 4 Word64) (SIMDVector e 4 Word64)) (SIMDPair (SIMDVector e 4 Word64) (SIMDVector e 4 Word64)))
  ) =>
  SPMDBitcastFloat64 32 e
  where
  spmdBitcastDoubleToWord64 (SPMD md) =
    let conv8 p = mkPair (vbitcast @e @4 @Double @4 @Word64 (fstPair p)) (vbitcast @e @4 @Double @4 @Word64 (sndPair p))
        conv16 q = mkPair (conv8 (fstPair q)) (conv8 (sndPair q))
     in SPMD (fmap (\r -> mkPair (conv16 (fstPair r)) (conv16 (sndPair r))) md)

instance
  ( ExpHash' (SPMD n e),
    SPMDMurmurElem n e a
  ) =>
  ExpMurmurHash (SPMD n e) (SPMD n e a)
  where
  murmurHashWithSalt seed x = hash64WithSalt seed (spmdElemToWord64 x)

-- ExpSized & ExpPtr
instance
  (SPMDRawLayout n Int64 ~ rl, SPMDTyped n e a, M.ExpSized e a) =>
  M.ExpSized (SPMD n e) a
  where
  sizeOf p = SPMD (pure (I64Uniform False (M.sizeOf p)))
  alignOf p = SPMD (pure (I64Uniform False (M.alignOf p)))

instance
  ( KnownNat n,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    rl ~ SPMDRawLayout n Int64,
    SPMDRawVec rl n e Int64,
    PtrTreeOps n e (Ptr a),
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  M.ExpPtr (SPMD n e) a
  where
  nullPtr = SPMD (pure (PtrUniform True M.nullPtr))
  ptrAdd (SPMD mp) (SPMD mi) = SPMD $ do
    pe <- mp
    ie <- mi
    let w = fromIntegral (natVal (Proxy @n)) :: Int64
    pure $ case (pe, ie) of
      (PtrUniform al p, I64Const 0) -> PtrUniform al p
      (PtrUniform _ p, I64Const c) -> PtrUniform False (M.ptrAdd p (fromIntegral c))
      (PtrUniform _ p, I64Uniform _ u) -> PtrUniform False (M.ptrAdd p u)
      (pOther, iOther) ->
        let ps = ptrToLanes pOther
            is = int64ToLanes iOther
            _ = w
         in PtrVarying (ptrTreeFromList @n @e @(Ptr a) (zipWith M.ptrAdd ps is))
  ptrIndex (SPMD mp) (SPMD mi) = SPMD $ do
    pe <- mp
    ie <- mi
    let w = fromIntegral (natVal (Proxy @n)) :: Int64
    pure $ case (pe, ie) of
      (PtrUniform al p, I64Const 0) -> PtrUniform al p
      (PtrUniform al p, I64Const c) -> PtrUniform (al && c `mod` w == 0) (M.ptrIndex p (fromIntegral c))
      (PtrUniform al1 p, I64Uniform al2 u) -> PtrUniform (al1 && al2) (M.ptrIndex p u)
      (PtrUniform al1 p, I64UnitStride al2 base) -> PtrUnitStride (al1 && al2) (M.ptrIndex p base)
      (PtrUnitStride al p, I64Const 0) -> PtrUnitStride al p
      (PtrUnitStride al p, I64Const c) -> PtrUnitStride (al && c `mod` w == 0) (M.ptrIndex p (fromIntegral c))
      (PtrUnitStride al1 p, I64Uniform al2 u) -> PtrUnitStride (al1 && al2) (M.ptrIndex p u)
      (pOther, iOther) ->
        let ps = ptrToLanes pOther
            is = int64ToLanes iOther
         in PtrVarying (ptrTreeFromList @n @e @(Ptr a) (zipWith M.ptrIndex ps is))

-- ExpFix (direct fixpoint on Haskell-evaluated SIMD backends)
instance ExpFix (SPMD n Identity) r where
  fun _ _ = fix
  {-# INLINE fun #-}

instance ExpFix (SPMD n CUDAIO) r where
  fun _ _ = fix
  {-# INLINE fun #-}

instance ExpFix (SPMD n CUDADejaFu) r where
  fun _ _ = fix
  {-# INLINE fun #-}
