{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.SPMD.Typed where

import Data.Coerce (Coercible)
import Data.Complex (Complex)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import qualified Janus.Command.Memory as M
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.SIMD
import Janus.SPMD.Raw
import Janus.SPMD.Types
import Janus.Typed (JanusTyped, KnownJanusType)

--------------------------------------------------------------------------------
-- 9. Int64 & Ptr Provenance Materialization
--------------------------------------------------------------------------------

materializeInt64 ::
  forall n e rl.
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  e (SPMDRep n e Int64)
materializeInt64 = \case
  I64Const c -> rawFromInteger @rl @n @e @Int64 (toInteger c)
  I64Uniform _ u -> rawSplat @rl @n @e @Int64 u
  I64UnitStride _ b -> rawAdd @rl @n @e @Int64 (rawSplat @rl @n @e @Int64 b) (rawIota @rl @n @e @Int64)
  I64Varying v -> v

int64ToLanes ::
  forall n e rl.
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawVec rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  [e Int64]
int64ToLanes = \case
  I64Const c -> replicate (fromIntegral (natVal (Proxy @n))) (fromIntegral c)
  I64Uniform _ u -> replicate (fromIntegral (natVal (Proxy @n))) u
  I64UnitStride _ b -> [b + fromIntegral i | i <- [0 .. fromIntegral (natVal (Proxy @n)) - 1 :: Int64]]
  I64Varying v -> rawToLanes @rl @n @e @Int64 v

materializePtr ::
  forall n e a.
  (PtrTreeOps n e (Ptr a), M.ExpPtr e a, Num (e Int64)) =>
  SPMDPtrExpr n e a ->
  e (PtrTree n (Ptr a))
materializePtr = \case
  PtrUniform _ p -> ptrTreeReplicate @n p
  PtrUnitStride _ p ->
    let lanes = fromIntegral (natVal (Proxy @n)) :: Int64
     in ptrTreeFromList @n [M.ptrIndex p (fromIntegral i) | i <- [0 .. lanes - 1]]
  PtrVarying t -> t

ptrToLanes ::
  forall n e a.
  (PtrTreeOps n e (Ptr a), M.ExpPtr e a, Num (e Int64)) =>
  SPMDPtrExpr n e a ->
  [e (Ptr a)]
ptrToLanes = \case
  PtrUniform _ p -> replicate (fromIntegral (natVal (Proxy @n))) p
  PtrUnitStride _ p ->
    let lanes = fromIntegral (natVal (Proxy @n)) :: Int64
     in [M.ptrIndex p (fromIntegral i) | i <- [0 .. lanes - 1]]
  PtrVarying t -> ptrTreeToList @n t

materializeVoidPtr ::
  forall n e.
  (PtrTreeOps n e VoidPtr, ExpPair e) =>
  SPMDVoidPtrExpr n e ->
  e (PtrTree n VoidPtr)
materializeVoidPtr = \case
  VoidPtrUniform _ p -> ptrTreeReplicate @n (mkVoidPtr p)
  VoidPtrVarying t -> t

--------------------------------------------------------------------------------
-- 10. JanusTyped (SPMD n e) = SPMDTyped n e
--------------------------------------------------------------------------------

class
  (JanusTyped e (SPMDRepByLayout l n e a)) =>
  SPMDTypedByLayout (l :: Layout) (n :: Nat) e a
  where
  spmdDefaultByLayout :: SPMDExprByLayout l n e a
  spmdMaterializeByLayout :: SPMDExprByLayout l n e a -> e (SPMDRepByLayout l n e a)
  spmdDematerializeByLayout :: e (SPMDRepByLayout l n e a) -> SPMDExprByLayout l n e a
  spmdSelectRawByLayout ::
    e (SIMDMask e n (SPMDMaskElem n)) ->
    e (SPMDRepByLayout l n e a) ->
    e (SPMDRepByLayout l n e a) ->
    e (SPMDRepByLayout l n e a)
  spmdLetByLayout ::
    (ExpLet e, JanusTyped e r) =>
    SPMDExprByLayout l n e a ->
    (SPMDExprByLayout l n e a -> e r) ->
    e r
  spmdToVoidPtrByLayout ::
    (ExpPtrCast e, PtrTreeOps n e VoidPtr) =>
    Proxy l ->
    SPMDExpr n e (Ptr a) ->
    SPMDVoidPtrExpr n e
  spmdFromVoidPtrByLayout ::
    (ExpPtrCast e, PtrTreeOps n e VoidPtr) =>
    Proxy l ->
    SPMDVoidPtrExpr n e ->
    SPMDExpr n e (Ptr a)

defaultToVoidPtr ::
  forall n e a.
  (JanusTyped e a, ExpPtrCast e, PtrTreeOps n e (Ptr a), PtrTreeOps n e VoidPtr, M.ExpPtr e a, Num (e Int64)) =>
  SPMDPtrExpr n e a ->
  SPMDVoidPtrExpr n e
defaultToVoidPtr = \case
  PtrUniform al p -> VoidPtrUniform al (toVoidPtr p)
  other -> VoidPtrVarying (ptrTreeFromList @n (map (mkVoidPtr . toVoidPtr) (ptrToLanes other)))

defaultFromVoidPtr ::
  forall n e a.
  (JanusTyped e a, ExpPtrCast e, PtrTreeOps n e (Ptr a), PtrTreeOps n e VoidPtr) =>
  SPMDVoidPtrExpr n e ->
  SPMDPtrExpr n e a
defaultFromVoidPtr = \case
  VoidPtrUniform al vp -> PtrUniform al (fromVoidPtr vp)
  VoidPtrVarying vt -> PtrVarying (ptrTreeFromList @n @e @(Ptr a) (map (fromVoidPtr . unVoidPtr) (ptrTreeToList @n @e @VoidPtr vt)))

instance
  ( SPMDRawVec 'LDirect n e a,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    JanusTyped e a,
    JanusTyped e (SIMDVector e n a),
    PtrTreeOps n e (Ptr a),
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout 'LDirect n e a
  where
  spmdDefaultByLayout = rawZero @'LDirect @n @e @a
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout = rawSelect @'LDirect @n @e @a
  spmdLetByLayout = let_
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( SPMDRawVec ('LSplit h) n e a,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    JanusTyped e a,
    JanusTyped e (SIMDPair (SPMDRepByLayout (SPMDRawLayout h a) h e a) (SPMDRepByLayout (SPMDRawLayout h a) h e a)),
    PtrTreeOps n e (Ptr a),
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout ('LSplit h) n e a
  where
  spmdDefaultByLayout = rawZero @('LSplit h) @n @e @a
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout = rawSelect @('LSplit h) @n @e @a
  spmdLetByLayout = let_
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( SPMDRawVec 'LPad4 2 e a,
    SPMDLayout 2 (Ptr a) ~ 'LPtr a,
    JanusTyped e a,
    JanusTyped e (SIMDVector e 4 a),
    PtrTreeOps 2 e (Ptr a),
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout 'LPad4 2 e a
  where
  spmdDefaultByLayout = rawZero @'LPad4 @2 @e @a
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout = rawSelect @'LPad4 @2 @e @a
  spmdLetByLayout = let_
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( rl ~ SPMDRawLayout n Int64,
    SPMDRawNum rl n e Int64,
    JanusTyped e Int64,
    JanusTyped e (SPMDRepByLayout rl n e Int64),
    PtrTreeOps n e (Ptr Int64),
    M.ExpPtr e Int64,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout ('LInt64 rl) n e Int64
  where
  spmdDefaultByLayout = I64Const 0
  spmdMaterializeByLayout = materializeInt64
  spmdDematerializeByLayout = I64Varying
  spmdSelectRawByLayout = rawSelect @rl @n @e @Int64
  spmdLetByLayout expr k = case expr of
    I64Const c -> k (I64Const c)
    I64Uniform al u -> let_ u (\u' -> k (I64Uniform al u'))
    I64UnitStride al b -> let_ b (\b' -> k (I64UnitStride al b'))
    I64Varying v -> let_ v (\v' -> k (I64Varying v'))
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( SPMDTyped n e base,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    Coercible a base,
    JanusTyped e a,
    JanusTyped e (SPMDRep n e base),
    PtrTreeOps n e (Ptr a),
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout ('LNewtype base) n e a
  where
  spmdDefaultByLayout = spmdDefaultExpr @n @e @base
  spmdMaterializeByLayout = spmdMaterialize @n @e @base
  spmdDematerializeByLayout = spmdDematerialize @n @e @base
  spmdSelectRawByLayout = spmdSelectRaw @n @e @base
  spmdLetByLayout = spmdLetRaw @n @e @base
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( SPMDTyped n e b,
    ExpPair e,
    JanusTyped e (Complex b),
    JanusTyped e (SIMDPair (SPMDRep n e b) (SPMDRep n e b)),
    PtrTreeOps n e (Ptr (Complex b)),
    M.ExpPtr e (Complex b),
    Num (e Int64)
  ) =>
  SPMDTypedByLayout ('LComplex b) n e (Complex b)
  where
  spmdDefaultByLayout =
    let z = spmdMaterialize @n @e @b (spmdDefaultExpr @n @e @b)
     in mkPair z z
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout m t f =
    mkPair
      (spmdSelectRaw @n @e @b m (fstPair t) (fstPair f))
      (spmdSelectRaw @n @e @b m (sndPair t) (sndPair f))
  spmdLetByLayout = let_
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( ExpSIMDMask e n (SPMDMaskElem n),
    JanusTyped e Bool,
    JanusTyped e (SIMDMask e n (SPMDMaskElem n)),
    PtrTreeOps n e (Ptr Bool),
    M.ExpPtr e Bool,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout 'LBool n e Bool
  where
  spmdDefaultByLayout = mfalse
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout m t f = (m `mand` t) `mor` (m `mandnot` f)
  spmdLetByLayout = let_
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( KnownNat n,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    ExpCond e,
    JanusTyped e (Ptr b),
    PtrTreeOps n e (Ptr b),
    PtrTreeOps n e (Ptr (Ptr b)),
    M.ExpPtr e b,
    M.ExpPtr e (Ptr b),
    Num (e Int64)
  ) =>
  SPMDTypedByLayout ('LPtr b) n e (Ptr b)
  where
  spmdDefaultByLayout = PtrUniform True M.nullPtr
  spmdMaterializeByLayout = materializePtr
  spmdDematerializeByLayout = PtrVarying
  spmdSelectRawByLayout m t f =
    let preds = laneActivePreds @n m
        ts = ptrTreeToList @n @e @(Ptr b) t
        fs = ptrTreeToList @n @e @(Ptr b) f
     in ptrTreeFromList @n @e @(Ptr b) (zipWith3 ifThenElse preds ts fs)
  spmdLetByLayout expr k = case expr of
    PtrUniform al p -> let_ p (\p' -> k (PtrUniform al p'))
    PtrUnitStride al p -> let_ p (\p' -> k (PtrUnitStride al p'))
    PtrVarying t -> let_ t (\t' -> k (PtrVarying t'))
  spmdToVoidPtrByLayout _ = defaultToVoidPtr
  spmdFromVoidPtrByLayout _ = defaultFromVoidPtr

instance
  ( KnownNat n,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    ExpCond e,
    ExpPair e,
    JanusTyped e Word8,
    JanusTyped e VoidPtr,
    PtrTreeOps n e VoidPtr,
    M.ExpPtr e Word8,
    ExpPtrCast e,
    Num (e Int64)
  ) =>
  SPMDTypedByLayout 'LVoidPtr n e (Ptr ())
  where
  spmdDefaultByLayout = VoidPtrUniform True (toVoidPtr (M.nullPtr @e @Word8))
  spmdMaterializeByLayout = materializeVoidPtr
  spmdDematerializeByLayout = VoidPtrVarying
  spmdSelectRawByLayout m t f =
    let preds = laneActivePreds @n m
        ts = ptrTreeToList @n @e @VoidPtr t
        fs = ptrTreeToList @n @e @VoidPtr f
     in ptrTreeFromList @n @e @VoidPtr (zipWith3 ifThenElse preds ts fs)
  spmdLetByLayout expr k = case expr of
    VoidPtrUniform al p ->
      let_ (mkVoidPtr p) (\vp' -> k (VoidPtrUniform al (unVoidPtr vp')))
    VoidPtrVarying t -> let_ t (\t' -> k (VoidPtrVarying t'))
  spmdToVoidPtrByLayout _ _ = error "spmdToVoidPtrByLayout: Ptr (Ptr ()) unsupported"
  spmdFromVoidPtrByLayout _ _ = error "spmdFromVoidPtrByLayout: Ptr (Ptr ()) unsupported"

instance
  ( ExpPair e,
    JanusTyped e SPMDUnit
  ) =>
  SPMDTypedByLayout 'LUnit n e ()
  where
  spmdDefaultByLayout = mkSPMDUnit
  spmdMaterializeByLayout = id
  spmdDematerializeByLayout = id
  spmdSelectRawByLayout _ t _ = t
  spmdLetByLayout u k = let_ u k
  spmdToVoidPtrByLayout _ = id
  spmdFromVoidPtrByLayout _ = id

class
  (SPMDTypedByLayout (SPMDLayout n a) n e a, JanusTyped e (SPMDRep n e a)) =>
  SPMDTyped (n :: Nat) e a
  where
  spmdDefaultExpr :: SPMDExpr n e a
  spmdMaterialize :: SPMDExpr n e a -> e (SPMDRep n e a)
  spmdDematerialize :: e (SPMDRep n e a) -> SPMDExpr n e a
  spmdSelectRaw ::
    e (SIMDMask e n (SPMDMaskElem n)) ->
    e (SPMDRep n e a) ->
    e (SPMDRep n e a) ->
    e (SPMDRep n e a)
  spmdLetRaw ::
    (ExpLet e, JanusTyped e r) =>
    SPMDExpr n e a ->
    (SPMDExpr n e a -> e r) ->
    e r
  spmdToVoidPtr ::
    (ExpPtrCast e, PtrTreeOps n e VoidPtr) =>
    SPMDExpr n e (Ptr a) ->
    SPMDVoidPtrExpr n e
  spmdFromVoidPtr ::
    (ExpPtrCast e, PtrTreeOps n e VoidPtr) =>
    SPMDVoidPtrExpr n e ->
    SPMDExpr n e (Ptr a)

instance
  (l ~ SPMDLayout n a, SPMDTypedByLayout l n e a, JanusTyped e (SPMDRepByLayout l n e a)) =>
  SPMDTyped n e a
  where
  spmdDefaultExpr = spmdDefaultByLayout @l @n @e @a
  spmdMaterialize = spmdMaterializeByLayout @l @n @e @a
  spmdDematerialize = spmdDematerializeByLayout @l @n @e @a
  spmdSelectRaw = spmdSelectRawByLayout @l @n @e @a
  spmdLetRaw = spmdLetByLayout @l @n @e @a
  spmdToVoidPtr = spmdToVoidPtrByLayout @l @n @e @a (Proxy @l)
  spmdFromVoidPtr = spmdFromVoidPtrByLayout @l @n @e @a (Proxy @l)

type instance JanusTyped (SPMD n e) = SPMDTyped n e

instance (SPMDTyped n e a) => KnownJanusType (SPMD n e) a
