{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.SPMD.Lane where

import Control.Monad.Reader (MonadReader (..))
import Data.Coerce (Coercible)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import qualified Janus.Command.Array as A
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.SIMD
  ( ExpSIMDCmp (..),
    ExpSIMDMask (..),
    ExpSIMDVec (..),
    SIMDVector,
  )
import Janus.SPMD.Raw
import Janus.SPMD.Typed
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 13. ISPC-Style Combinators, Tensor Lifting, & Horizontal Gang Reductions
--------------------------------------------------------------------------------

programCount :: forall n e a. (KnownNat n, Num (SPMD n e a)) => SPMD n e a
programCount = fromIntegral (natVal (Proxy @n))

class SPMDLanesByLayout (l :: Layout) (n :: Nat) e a where
  spmdSplatL :: e a -> SPMDExprByLayout l n e a
  spmdIotaL :: SPMDExprByLayout l n e a
  spmdFromLanesL :: [e a] -> SPMDExprByLayout l n e a
  spmdToLanesL :: SPMDExprByLayout l n e a -> [e a]

instance (SPMDRawVec 'LDirect n e a) => SPMDLanesByLayout 'LDirect n e a where
  spmdSplatL = rawSplat @'LDirect @n @e @a
  spmdIotaL = rawIota @'LDirect @n @e @a
  spmdFromLanesL = rawFromLanes @'LDirect @n @e @a
  spmdToLanesL = rawToLanes @'LDirect @n @e @a

instance (SPMDRawVec ('LSplit h) n e a) => SPMDLanesByLayout ('LSplit h) n e a where
  spmdSplatL = rawSplat @('LSplit h) @n @e @a
  spmdIotaL = rawIota @('LSplit h) @n @e @a
  spmdFromLanesL = rawFromLanes @('LSplit h) @n @e @a
  spmdToLanesL = rawToLanes @('LSplit h) @n @e @a

instance (SPMDRawVec 'LPad4 2 e a) => SPMDLanesByLayout 'LPad4 2 e a where
  spmdSplatL = rawSplat @'LPad4 @2 @e @a
  spmdIotaL = rawIota @'LPad4 @2 @e @a
  spmdFromLanesL = rawFromLanes @'LPad4 @2 @e @a
  spmdToLanesL = rawToLanes @'LPad4 @2 @e @a

instance
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawVec rl n e Int64, Num (e Int64)) =>
  SPMDLanesByLayout ('LInt64 rl) n e Int64
  where
  spmdSplatL u = I64Uniform False u
  spmdIotaL = I64UnitStride True 0
  spmdFromLanesL xs = I64Varying (rawFromLanes @rl @n @e @Int64 xs)
  spmdToLanesL = int64ToLanes

instance
  (KnownNat n, PtrTreeOps n e (Ptr b), A.ExpPtr e b, Num (e Int64)) =>
  SPMDLanesByLayout ('LPtr b) n e (Ptr b)
  where
  spmdSplatL p = PtrUniform False p
  spmdIotaL = error "programIndex: unsupported for Ptr"
  spmdFromLanesL ps = PtrVarying (ptrTreeFromList @n ps)
  spmdToLanesL = ptrToLanes

instance
  ( lb ~ SPMDLayout n base,
    SPMDLanesByLayout lb n e base,
    Coercible a base,
    ExpPair e,
    JanusTyped e a,
    JanusTyped e base
  ) =>
  SPMDLanesByLayout ('LNewtype base) n e a
  where
  spmdSplatL x = spmdSplatL @lb @n @e @base (coerceExp x)
  spmdIotaL = spmdIotaL @lb @n @e @base
  spmdFromLanesL xs = spmdFromLanesL @lb @n @e @base (map coerceExp xs)
  spmdToLanesL v = map coerceExp (spmdToLanesL @lb @n @e @base v)

instance
  ( KnownNat n,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpSIMDCmp e n (SPMDMaskElem n),
    ExpSIMDVec e n (SPMDMaskElem n),
    ExpBool e,
    ExpCond e,
    ExpBits e Word32,
    JanusTyped e (SPMDMaskElem n),
    Num (e Word32),
    Num (e (SPMDMaskElem n))
  ) =>
  SPMDLanesByLayout 'LBool n e Bool
  where
  spmdSplatL b = vsplat (ifThenElse b 1 0) `vne` (vzero @e @n @(SPMDMaskElem n))
  spmdIotaL = mfalse
  spmdFromLanesL bs = vfromLanes [ifThenElse b 1 0 | b <- bs] `vne` (vzero @e @n @(SPMDMaskElem n))
  spmdToLanesL m = laneActivePreds @n m

programIndex :: forall n e a l. (l ~ SPMDLayout n a, SPMDLanesByLayout l n e a) => SPMD n e a
programIndex = SPMD (pure (spmdIotaL @l @n @e @a))

spmdSplat :: forall n e a l. (l ~ SPMDLayout n a, SPMDLanesByLayout l n e a) => e a -> SPMD n e a
spmdSplat x = SPMD (pure (spmdSplatL @l @n @e @a x))

uniform :: forall n e a l. (l ~ SPMDLayout n a, SPMDLanesByLayout l n e a) => e a -> SPMD n e a
uniform = spmdSplat

spmdAlignedIndex :: e Int64 -> SPMD n e Int64
spmdAlignedIndex base = SPMD (pure (I64Uniform True base))

spmdAlignedUnitStride :: e Int64 -> SPMD n e Int64
spmdAlignedUnitStride base = SPMD (pure (I64UnitStride True base))

spmdUnitStride :: e Int64 -> SPMD n e Int64
spmdUnitStride base = SPMD (pure (I64UnitStride False base))

uniformPtr :: (SPMDLayout n (Ptr a) ~ 'LPtr a) => e (Ptr a) -> SPMD n e (Ptr a)
uniformPtr p = SPMD (pure (PtrUniform False p))

uniformAlignedPtr :: (SPMDLayout n (Ptr a) ~ 'LPtr a) => e (Ptr a) -> SPMD n e (Ptr a)
uniformAlignedPtr p = SPMD (pure (PtrUniform True p))

rebaseTensorBound :: A.TensorBound ds e -> A.TensorBound ds e'
rebaseTensorBound A.TensorBoundNil = A.TensorBoundNil
rebaseTensorBound (A.TensorBoundCons p rest) = A.TensorBoundCons p (rebaseTensorBound rest)

-- | Lift an unlifted Janus 'A.Tensor' (whose memory is 64-byte aligned via
-- 'A.withTensor' / 'A.newTensor') into an SPMD 'A.Tensor' with aligned provenance.
liftTensor :: (SPMDLayout n (Ptr a) ~ 'LPtr a) => A.Tensor ds e a -> A.Tensor ds (SPMD n e) a
liftTensor (A.Tensor tb p) = A.Tensor (rebaseTensorBound tb) (uniformAlignedPtr p)

liftTensorUnaligned :: (SPMDLayout n (Ptr a) ~ 'LPtr a) => A.Tensor ds e a -> A.Tensor ds (SPMD n e) a
liftTensorUnaligned (A.Tensor tb p) = A.Tensor (rebaseTensorBound tb) (uniformPtr p)

-- | Batch @n@ independent unlifted 'A.Tensor's (one per SIMD lane) into a
-- single SPMD 'A.Tensor' so unmodified array algorithms run @n@ instances in parallel.
batchTensors ::
  forall n ds e a.
  (KnownNat n, SPMDLayout n (Ptr a) ~ 'LPtr a, PtrTreeOps n e (Ptr a)) =>
  [A.Tensor ds e a] ->
  A.Tensor ds (SPMD n e) a
batchTensors [] = error "batchTensors: empty tensor list"
batchTensors ts@(A.Tensor tb0 _ : _) =
  let ps = [p | A.Tensor _ p <- ts]
   in A.Tensor (rebaseTensorBound tb0) (SPMD (pure (PtrVarying (ptrTreeFromList @n @e @(Ptr a) ps))))

spmdFromLanes :: forall n e a l. (l ~ SPMDLayout n a, SPMDLanesByLayout l n e a) => [e a] -> SPMD n e a
spmdFromLanes xs = SPMD (pure (spmdFromLanesL @l @n @e @a xs))

spmdToLanes :: forall n e a l. (l ~ SPMDLayout n a, SPMDLanesByLayout l n e a) => SPMD n e a -> [e a]
spmdToLanes x = spmdToLanesL @l @n @e @a (runSPMD x)

spmdFromSIMD :: (SPMDLayout n a ~ 'LDirect) => e (SIMDVector e n a) -> SPMD n e a
spmdFromSIMD v = SPMD (pure v)

spmdToSIMD :: (SPMDLayout n a ~ 'LDirect) => SPMD n e a -> e (SIMDVector e n a)
spmdToSIMD = runSPMD

-- Horizontal Gang Reductions (Unmasked & Mask-Aware)
gangAny, gangAll, gangNone ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMD n e Bool ->
  e Bool
gangAny b = manyOf (runSPMD b)
gangAll b = mallOf (runSPMD b)
gangNone b = mnoneOf (runSPMD b)

class SPMDReduceByLayout (l :: Layout) (n :: Nat) e a where
  spmdReduceAddL :: SPMDExprByLayout l n e a -> e a
  spmdReduceMinL :: SPMDExprByLayout l n e a -> e a
  spmdReduceMaxL :: SPMDExprByLayout l n e a -> e a

instance (SPMDRawReduce 'LDirect n e a) => SPMDReduceByLayout 'LDirect n e a where
  spmdReduceAddL = rawReduceAdd @'LDirect @n @e @a
  spmdReduceMinL = rawReduceMin @'LDirect @n @e @a
  spmdReduceMaxL = rawReduceMax @'LDirect @n @e @a

instance (SPMDRawReduce ('LSplit h) n e a) => SPMDReduceByLayout ('LSplit h) n e a where
  spmdReduceAddL = rawReduceAdd @('LSplit h) @n @e @a
  spmdReduceMinL = rawReduceMin @('LSplit h) @n @e @a
  spmdReduceMaxL = rawReduceMax @('LSplit h) @n @e @a

instance (SPMDRawReduce 'LPad4 2 e a) => SPMDReduceByLayout 'LPad4 2 e a where
  spmdReduceAddL = rawReduceAdd @'LPad4 @2 @e @a
  spmdReduceMinL = rawReduceMin @'LPad4 @2 @e @a
  spmdReduceMaxL = rawReduceMax @'LPad4 @2 @e @a

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, SPMDRawReduce rl n e Int64, Num (e Int64)) =>
  SPMDReduceByLayout ('LInt64 rl) n e Int64
  where
  spmdReduceAddL x = rawReduceAdd @rl @n @e @Int64 (materializeInt64 x)
  spmdReduceMinL x = rawReduceMin @rl @n @e @Int64 (materializeInt64 x)
  spmdReduceMaxL x = rawReduceMax @rl @n @e @Int64 (materializeInt64 x)

gangReduceAdd, gangReduceMin, gangReduceMax ::
  forall n e a l.
  (l ~ SPMDLayout n a, SPMDReduceByLayout l n e a) =>
  SPMD n e a ->
  e a
gangReduceAdd x = spmdReduceAddL @l @n @e @a (runSPMD x)
gangReduceMin x = spmdReduceMinL @l @n @e @a (runSPMD x)
gangReduceMax x = spmdReduceMaxL @l @n @e @a (runSPMD x)

spmdAny :: (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) => SPMD n e Bool -> SPMDM n m e (e Bool)
spmdAny b = do
  msk <- ask
  c <- evalSPMD b
  pure (manyOf (spmdMaskAndRaw msk c))

spmdAll :: (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) => SPMD n e Bool -> SPMDM n m e (e Bool)
spmdAll b = do
  msk <- ask
  c <- evalSPMD b
  pure $ case msk of
    SPMDMaskAllTrue -> mallOf c
    SPMDMask m -> mallOf (mnot m `mor` c)

spmdNone :: (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) => SPMD n e Bool -> SPMDM n m e (e Bool)
spmdNone b = do
  msk <- ask
  c <- evalSPMD b
  pure (mnoneOf (spmdMaskAndRaw msk c))

spmdReduceAdd ::
  forall n m e a l.
  (Monad m, l ~ SPMDLayout n a, SPMDReduceByLayout l n e a, SPMDTyped n e a, Num (SPMD n e a)) =>
  SPMD n e a ->
  SPMDM n m e (e a)
spmdReduceAdd x = do
  msk <- ask
  ex <- evalSPMD x
  z <- evalSPMD (0 :: SPMD n e a)
  let blended = case msk of
        SPMDMaskAllTrue -> ex
        SPMDMask m ->
          spmdDematerialize @n @e @a $
            spmdSelectRaw @n @e @a m (spmdMaterialize @n @e @a ex) (spmdMaterialize @n @e @a z)
  pure (spmdReduceAddL @l @n @e @a blended)

spmdReduceMin ::
  forall n m e a l.
  (Monad m, l ~ SPMDLayout n a, SPMDReduceByLayout l n e a, SPMDTyped n e a, Bounded (SPMD n e a)) =>
  SPMD n e a ->
  SPMDM n m e (e a)
spmdReduceMin x = do
  msk <- ask
  ex <- evalSPMD x
  top <- evalSPMD (maxBound :: SPMD n e a)
  let blended = case msk of
        SPMDMaskAllTrue -> ex
        SPMDMask m ->
          spmdDematerialize @n @e @a $
            spmdSelectRaw @n @e @a m (spmdMaterialize @n @e @a ex) (spmdMaterialize @n @e @a top)
  pure (spmdReduceMinL @l @n @e @a blended)

spmdReduceMax ::
  forall n m e a l.
  (Monad m, l ~ SPMDLayout n a, SPMDReduceByLayout l n e a, SPMDTyped n e a, Bounded (SPMD n e a)) =>
  SPMD n e a ->
  SPMDM n m e (e a)
spmdReduceMax x = do
  msk <- ask
  ex <- evalSPMD x
  bot <- evalSPMD (minBound :: SPMD n e a)
  let blended = case msk of
        SPMDMaskAllTrue -> ex
        SPMDMask m ->
          spmdDematerialize @n @e @a $
            spmdSelectRaw @n @e @a m (spmdMaterialize @n @e @a ex) (spmdMaterialize @n @e @a bot)
  pure (spmdReduceMaxL @l @n @e @a blended)
