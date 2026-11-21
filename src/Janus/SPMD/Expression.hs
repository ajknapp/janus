{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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

module Janus.SPMD.Expression where

import Control.Monad.Reader (MonadReader (..), runReader)
import Data.Complex (Complex)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import GHC.Float (expm1, log1p)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD (ExpSIMDMask (..), SIMDMask)
import Janus.SPMD.Raw
import Janus.SPMD.Typed
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 11. Pure Expression Instances on SPMD n e
--------------------------------------------------------------------------------

-- Affine Int64 arithmetic preserving alignment and unit-stride provenance
addInt64Expr ::
  forall n e rl.
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e
addInt64Expr a b =
  let w = fromIntegral (natVal (Proxy @n)) :: Int64
   in case (a, b) of
        (I64Const 0, y) -> y
        (x, I64Const 0) -> x
        (I64Const x, I64Const y) -> I64Const (x + y)
        (I64Const c, I64Uniform al u) -> I64Uniform (al && c `mod` w == 0) (fromIntegral c + u)
        (I64Uniform al u, I64Const c) -> I64Uniform (al && c `mod` w == 0) (u + fromIntegral c)
        (I64Uniform al1 u1, I64Uniform al2 u2) -> I64Uniform (al1 && al2) (u1 + u2)
        (I64Const c, I64UnitStride al base) -> I64UnitStride (al && c `mod` w == 0) (fromIntegral c + base)
        (I64UnitStride al base, I64Const c) -> I64UnitStride (al && c `mod` w == 0) (base + fromIntegral c)
        (I64Uniform al1 u, I64UnitStride al2 base) -> I64UnitStride (al1 && al2) (u + base)
        (I64UnitStride al1 base, I64Uniform al2 u) -> I64UnitStride (al1 && al2) (base + u)
        _ -> I64Varying (rawAdd @rl @n @e @Int64 (materializeInt64 a) (materializeInt64 b))

subInt64Expr ::
  forall n e rl.
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e
subInt64Expr a b =
  let w = fromIntegral (natVal (Proxy @n)) :: Int64
   in case (a, b) of
        (x, I64Const 0) -> x
        (I64Const x, I64Const y) -> I64Const (x - y)
        (I64Uniform al u, I64Const c) -> I64Uniform (al && c `mod` w == 0) (u - fromIntegral c)
        (I64Const c, I64Uniform al u) -> I64Uniform (al && c `mod` w == 0) (fromIntegral c - u)
        (I64Uniform al1 u1, I64Uniform al2 u2) -> I64Uniform (al1 && al2) (u1 - u2)
        (I64UnitStride al base, I64Const c) -> I64UnitStride (al && c `mod` w == 0) (base - fromIntegral c)
        (I64UnitStride al1 base, I64Uniform al2 u) -> I64UnitStride (al1 && al2) (base - u)
        _ -> I64Varying (rawSub @rl @n @e @Int64 (materializeInt64 a) (materializeInt64 b))

mulInt64Expr ::
  forall n e rl.
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e
mulInt64Expr a b =
  let w = fromIntegral (natVal (Proxy @n)) :: Int64
   in case (a, b) of
        (I64Const 0, _) -> I64Const 0
        (_, I64Const 0) -> I64Const 0
        (I64Const 1, y) -> y
        (x, I64Const 1) -> x
        (I64Const x, I64Const y) -> I64Const (x * y)
        (I64Const c, I64Uniform al u) -> I64Uniform (al || c `mod` w == 0) (fromIntegral c * u)
        (I64Uniform al u, I64Const c) -> I64Uniform (al || c `mod` w == 0) (u * fromIntegral c)
        (I64Uniform al1 u1, I64Uniform al2 u2) -> I64Uniform (al1 || al2) (u1 * u2)
        _ -> I64Varying (rawMul @rl @n @e @Int64 (materializeInt64 a) (materializeInt64 b))

negInt64Expr ::
  forall n e rl.
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDInt64Expr n e ->
  SPMDInt64Expr n e
negInt64Expr = \case
  I64Const c -> I64Const (negate c)
  I64Uniform al u -> I64Uniform al (negate u)
  other -> I64Varying (rawNeg @rl @n @e @Int64 (materializeInt64 other))

-- Layout-directed Num
class SPMDNumByLayout (l :: Layout) (n :: Nat) e a where
  spmdAddL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdSubL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdMulL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdNegL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAbsL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdSignumL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdFromIntegerL :: Integer -> SPMDExprByLayout l n e a

instance (SPMDRawNum 'LDirect n e a) => SPMDNumByLayout 'LDirect n e a where
  spmdAddL = rawAdd @'LDirect @n @e @a
  spmdSubL = rawSub @'LDirect @n @e @a
  spmdMulL = rawMul @'LDirect @n @e @a
  spmdNegL = rawNeg @'LDirect @n @e @a
  spmdAbsL = rawAbs @'LDirect @n @e @a
  spmdSignumL = rawSignum @'LDirect @n @e @a
  spmdFromIntegerL = rawFromInteger @'LDirect @n @e @a

instance (SPMDRawNum ('LSplit h) n e a) => SPMDNumByLayout ('LSplit h) n e a where
  spmdAddL = rawAdd @('LSplit h) @n @e @a
  spmdSubL = rawSub @('LSplit h) @n @e @a
  spmdMulL = rawMul @('LSplit h) @n @e @a
  spmdNegL = rawNeg @('LSplit h) @n @e @a
  spmdAbsL = rawAbs @('LSplit h) @n @e @a
  spmdSignumL = rawSignum @('LSplit h) @n @e @a
  spmdFromIntegerL = rawFromInteger @('LSplit h) @n @e @a

instance (SPMDRawNum 'LPad4 2 e a) => SPMDNumByLayout 'LPad4 2 e a where
  spmdAddL = rawAdd @'LPad4 @2 @e @a
  spmdSubL = rawSub @'LPad4 @2 @e @a
  spmdMulL = rawMul @'LPad4 @2 @e @a
  spmdNegL = rawNeg @'LPad4 @2 @e @a
  spmdAbsL = rawAbs @'LPad4 @2 @e @a
  spmdSignumL = rawSignum @'LPad4 @2 @e @a
  spmdFromIntegerL = rawFromInteger @'LPad4 @2 @e @a

instance
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, Num (e Int64)) =>
  SPMDNumByLayout ('LInt64 rl) n e Int64
  where
  spmdAddL = addInt64Expr
  spmdSubL = subInt64Expr
  spmdMulL = mulInt64Expr
  spmdNegL = negInt64Expr
  spmdAbsL = \case
    I64Const c -> I64Const (abs c)
    other -> I64Varying (rawAbs @rl @n @e @Int64 (materializeInt64 other))
  spmdSignumL = \case
    I64Const c -> I64Const (signum c)
    other -> I64Varying (rawSignum @rl @n @e @Int64 (materializeInt64 other))
  spmdFromIntegerL = I64Const . fromInteger

instance (SPMDNumByLayout (SPMDLayout n base) n e base) => SPMDNumByLayout ('LNewtype base) n e a where
  spmdAddL = spmdAddL @(SPMDLayout n base) @n @e @base
  spmdSubL = spmdSubL @(SPMDLayout n base) @n @e @base
  spmdMulL = spmdMulL @(SPMDLayout n base) @n @e @base
  spmdNegL = spmdNegL @(SPMDLayout n base) @n @e @base
  spmdAbsL = spmdAbsL @(SPMDLayout n base) @n @e @base
  spmdSignumL = spmdSignumL @(SPMDLayout n base) @n @e @base
  spmdFromIntegerL = spmdFromIntegerL @(SPMDLayout n base) @n @e @base

instance
  ( rl ~ SPMDLayout n b,
    SPMDExpr n e b ~ e (SPMDRep n e b),
    SPMDNumByLayout rl n e b,
    SPMDFloatByLayout rl n e b,
    SPMDCmpByLayout rl n e b,
    SPMDTyped n e b,
    ExpPair e,
    JanusTyped e (SIMDPair (SPMDRep n e b) (SPMDRep n e b))
  ) =>
  SPMDNumByLayout ('LComplex b) n e (Complex b)
  where
  spmdAddL z1 z2 =
    mkPair
      (spmdAddL @rl @n @e @b (fstPair z1) (fstPair z2))
      (spmdAddL @rl @n @e @b (sndPair z1) (sndPair z2))
  spmdSubL z1 z2 =
    mkPair
      (spmdSubL @rl @n @e @b (fstPair z1) (fstPair z2))
      (spmdSubL @rl @n @e @b (sndPair z1) (sndPair z2))
  spmdMulL z1 z2 =
    let r1 = fstPair z1
        i1 = sndPair z1
        r2 = fstPair z2
        i2 = sndPair z2
     in mkPair
          (spmdSubL @rl @n @e @b (spmdMulL @rl @n @e @b r1 r2) (spmdMulL @rl @n @e @b i1 i2))
          (spmdAddL @rl @n @e @b (spmdMulL @rl @n @e @b r1 i2) (spmdMulL @rl @n @e @b i1 r2))
  spmdNegL z = mkPair (spmdNegL @rl @n @e @b (fstPair z)) (spmdNegL @rl @n @e @b (sndPair z))
  spmdAbsL z =
    let mag = spmdHypotL @rl @n @e @b (fstPair z) (sndPair z)
     in mkPair mag (spmdFromIntegerL @rl @n @e @b 0)
  spmdSignumL z =
    let r = fstPair z
        i = sndPair z
        z0 = spmdFromIntegerL @rl @n @e @b 0
        mag = spmdHypotL @rl @n @e @b r i
        isZero = spmdEqL @rl @n @e @b mag z0
        safeMag = spmdSelectRaw @n @e @b isZero (spmdFromIntegerL @rl @n @e @b 1) mag
        sr = spmdSelectRaw @n @e @b isZero z0 (spmdDivL @rl @n @e @b r safeMag)
        si = spmdSelectRaw @n @e @b isZero z0 (spmdDivL @rl @n @e @b i safeMag)
     in mkPair sr si
  spmdFromIntegerL k =
    mkPair (spmdFromIntegerL @rl @n @e @b k) (spmdFromIntegerL @rl @n @e @b 0)

instance (l ~ SPMDLayout n a, SPMDNumByLayout l n e a) => Num (SPMD n e a) where
  SPMD a + SPMD b = SPMD (liftA2 (spmdAddL @l @n @e @a) a b)
  SPMD a - SPMD b = SPMD (liftA2 (spmdSubL @l @n @e @a) a b)
  SPMD a * SPMD b = SPMD (liftA2 (spmdMulL @l @n @e @a) a b)
  negate (SPMD a) = SPMD (fmap (spmdNegL @l @n @e @a) a)
  abs (SPMD a) = SPMD (fmap (spmdAbsL @l @n @e @a) a)
  signum (SPMD a) = SPMD (fmap (spmdSignumL @l @n @e @a) a)
  fromInteger k = SPMD (pure (spmdFromIntegerL @l @n @e @a k))

-- Layout-directed Fractional, Floating, FMA, Hypot
class (SPMDNumByLayout l n e a) => SPMDFloatByLayout (l :: Layout) (n :: Nat) e a where
  spmdDivL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdRecipL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdFromRationalL :: Rational -> SPMDExprByLayout l n e a
  spmdPiL :: SPMDExprByLayout l n e a
  spmdExpL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdLogL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdSqrtL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdPowL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdSinL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdCosL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdTanL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAsinL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAcosL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAtanL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdSinhL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdCoshL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdTanhL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAsinhL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAcoshL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdAtanhL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdLog1pL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdExpm1L :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdFmaL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdHypotL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a

instance (SPMDRawFloat 'LDirect n e a) => SPMDFloatByLayout 'LDirect n e a where
  spmdDivL = rawDiv @'LDirect @n @e @a
  spmdRecipL = rawRecip @'LDirect @n @e @a
  spmdFromRationalL = rawFromRational @'LDirect @n @e @a
  spmdPiL = rawPi @'LDirect @n @e @a
  spmdExpL = rawExp @'LDirect @n @e @a
  spmdLogL = rawLog @'LDirect @n @e @a
  spmdSqrtL = rawSqrt @'LDirect @n @e @a
  spmdPowL = rawPow @'LDirect @n @e @a
  spmdSinL = rawSin @'LDirect @n @e @a
  spmdCosL = rawCos @'LDirect @n @e @a
  spmdTanL = rawTan @'LDirect @n @e @a
  spmdAsinL = rawAsin @'LDirect @n @e @a
  spmdAcosL = rawAcos @'LDirect @n @e @a
  spmdAtanL = rawAtan @'LDirect @n @e @a
  spmdSinhL = rawSinh @'LDirect @n @e @a
  spmdCoshL = rawCosh @'LDirect @n @e @a
  spmdTanhL = rawTanh @'LDirect @n @e @a
  spmdAsinhL = rawAsinh @'LDirect @n @e @a
  spmdAcoshL = rawAcosh @'LDirect @n @e @a
  spmdAtanhL = rawAtanh @'LDirect @n @e @a
  spmdLog1pL = rawLog1p @'LDirect @n @e @a
  spmdExpm1L = rawExpm1 @'LDirect @n @e @a
  spmdFmaL = rawFma @'LDirect @n @e @a
  spmdHypotL = rawHypot @'LDirect @n @e @a

instance (SPMDRawFloat ('LSplit h) n e a) => SPMDFloatByLayout ('LSplit h) n e a where
  spmdDivL = rawDiv @('LSplit h) @n @e @a
  spmdRecipL = rawRecip @('LSplit h) @n @e @a
  spmdFromRationalL = rawFromRational @('LSplit h) @n @e @a
  spmdPiL = rawPi @('LSplit h) @n @e @a
  spmdExpL = rawExp @('LSplit h) @n @e @a
  spmdLogL = rawLog @('LSplit h) @n @e @a
  spmdSqrtL = rawSqrt @('LSplit h) @n @e @a
  spmdPowL = rawPow @('LSplit h) @n @e @a
  spmdSinL = rawSin @('LSplit h) @n @e @a
  spmdCosL = rawCos @('LSplit h) @n @e @a
  spmdTanL = rawTan @('LSplit h) @n @e @a
  spmdAsinL = rawAsin @('LSplit h) @n @e @a
  spmdAcosL = rawAcos @('LSplit h) @n @e @a
  spmdAtanL = rawAtan @('LSplit h) @n @e @a
  spmdSinhL = rawSinh @('LSplit h) @n @e @a
  spmdCoshL = rawCosh @('LSplit h) @n @e @a
  spmdTanhL = rawTanh @('LSplit h) @n @e @a
  spmdAsinhL = rawAsinh @('LSplit h) @n @e @a
  spmdAcoshL = rawAcosh @('LSplit h) @n @e @a
  spmdAtanhL = rawAtanh @('LSplit h) @n @e @a
  spmdLog1pL = rawLog1p @('LSplit h) @n @e @a
  spmdExpm1L = rawExpm1 @('LSplit h) @n @e @a
  spmdFmaL = rawFma @('LSplit h) @n @e @a
  spmdHypotL = rawHypot @('LSplit h) @n @e @a

instance (SPMDFloatByLayout (SPMDLayout n base) n e base) => SPMDFloatByLayout ('LNewtype base) n e a where
  spmdDivL = spmdDivL @(SPMDLayout n base) @n @e @base
  spmdRecipL = spmdRecipL @(SPMDLayout n base) @n @e @base
  spmdFromRationalL = spmdFromRationalL @(SPMDLayout n base) @n @e @base
  spmdPiL = spmdPiL @(SPMDLayout n base) @n @e @base
  spmdExpL = spmdExpL @(SPMDLayout n base) @n @e @base
  spmdLogL = spmdLogL @(SPMDLayout n base) @n @e @base
  spmdSqrtL = spmdSqrtL @(SPMDLayout n base) @n @e @base
  spmdPowL = spmdPowL @(SPMDLayout n base) @n @e @base
  spmdSinL = spmdSinL @(SPMDLayout n base) @n @e @base
  spmdCosL = spmdCosL @(SPMDLayout n base) @n @e @base
  spmdTanL = spmdTanL @(SPMDLayout n base) @n @e @base
  spmdAsinL = spmdAsinL @(SPMDLayout n base) @n @e @base
  spmdAcosL = spmdAcosL @(SPMDLayout n base) @n @e @base
  spmdAtanL = spmdAtanL @(SPMDLayout n base) @n @e @base
  spmdSinhL = spmdSinhL @(SPMDLayout n base) @n @e @base
  spmdCoshL = spmdCoshL @(SPMDLayout n base) @n @e @base
  spmdTanhL = spmdTanhL @(SPMDLayout n base) @n @e @base
  spmdAsinhL = spmdAsinhL @(SPMDLayout n base) @n @e @base
  spmdAcoshL = spmdAcoshL @(SPMDLayout n base) @n @e @base
  spmdAtanhL = spmdAtanhL @(SPMDLayout n base) @n @e @base
  spmdLog1pL = spmdLog1pL @(SPMDLayout n base) @n @e @base
  spmdExpm1L = spmdExpm1L @(SPMDLayout n base) @n @e @base
  spmdFmaL = spmdFmaL @(SPMDLayout n base) @n @e @base
  spmdHypotL = spmdHypotL @(SPMDLayout n base) @n @e @base

-- Complex Floating (SoA complex arithmetic in SIMD registers)
instance
  ( rl ~ SPMDLayout n b,
    SPMDExpr n e b ~ e (SPMDRep n e b),
    SPMDFloatByLayout rl n e b,
    SPMDCmpByLayout rl n e b,
    SPMDTyped n e b,
    ExpPair e,
    JanusTyped e (SIMDPair (SPMDRep n e b) (SPMDRep n e b))
  ) =>
  SPMDFloatByLayout ('LComplex b) n e (Complex b)
  where
  spmdDivL z1 z2 =
    let r1 = fstPair z1
        i1 = sndPair z1
        r2 = fstPair z2
        i2 = sndPair z2
        denom = spmdAddL @rl @n @e @b (spmdMulL @rl @n @e @b r2 r2) (spmdMulL @rl @n @e @b i2 i2)
        nr = spmdAddL @rl @n @e @b (spmdMulL @rl @n @e @b r1 r2) (spmdMulL @rl @n @e @b i1 i2)
        ni = spmdSubL @rl @n @e @b (spmdMulL @rl @n @e @b i1 r2) (spmdMulL @rl @n @e @b r1 i2)
     in mkPair (spmdDivL @rl @n @e @b nr denom) (spmdDivL @rl @n @e @b ni denom)
  spmdRecipL z = spmdDivL @('LComplex b) @n @e @(Complex b) (spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1) z
  spmdFromRationalL q = mkPair (spmdFromRationalL @rl @n @e @b q) (spmdFromIntegerL @rl @n @e @b 0)
  spmdPiL = mkPair (spmdPiL @rl @n @e @b) (spmdFromIntegerL @rl @n @e @b 0)
  spmdExpL z =
    let ex = spmdExpL @rl @n @e @b (fstPair z)
        im = sndPair z
     in mkPair (spmdMulL @rl @n @e @b ex (spmdCosL @rl @n @e @b im)) (spmdMulL @rl @n @e @b ex (spmdSinL @rl @n @e @b im))
  spmdLogL z =
    let r = fstPair z
        im = sndPair z
        mag = spmdHypotL @rl @n @e @b r im
        -- atan2(im, r)
        z0 = spmdFromIntegerL @rl @n @e @b 0
        piV = spmdPiL @rl @n @e @b
        halfPi = spmdMulL @rl @n @e @b (spmdFromRationalL @rl @n @e @b 0.5) piV
        baseAtan = spmdAtanL @rl @n @e @b (spmdDivL @rl @n @e @b im r)
        rLt0 = spmdLtL @rl @n @e @b r z0
        imGe0 = spmdGeL @rl @n @e @b im z0
        rEq0 = spmdEqL @rl @n @e @b r z0
        imGt0 = spmdGtL @rl @n @e @b im z0
        imLt0 = spmdLtL @rl @n @e @b im z0
        quadShift = spmdSelectRaw @n @e @b imGe0 (spmdAddL @rl @n @e @b baseAtan piV) (spmdSubL @rl @n @e @b baseAtan piV)
        axisAngle = spmdSelectRaw @n @e @b imGt0 halfPi (spmdSelectRaw @n @e @b imLt0 (spmdNegL @rl @n @e @b halfPi) z0)
        theta = spmdSelectRaw @n @e @b rEq0 axisAngle (spmdSelectRaw @n @e @b rLt0 quadShift baseAtan)
     in mkPair (spmdLogL @rl @n @e @b mag) theta
  spmdSqrtL z =
    let r = fstPair z
        im = sndPair z
        z0 = spmdFromIntegerL @rl @n @e @b 0
        half = spmdFromRationalL @rl @n @e @b 0.5
        mag = spmdHypotL @rl @n @e @b r im
        u = spmdSqrtL @rl @n @e @b (spmdMulL @rl @n @e @b half (spmdAddL @rl @n @e @b mag (spmdAbsL @rl @n @e @b r)))
        v = spmdSqrtL @rl @n @e @b (spmdMulL @rl @n @e @b half (spmdSubL @rl @n @e @b mag (spmdAbsL @rl @n @e @b r)))
        vSigned = spmdSelectRaw @n @e @b (spmdLtL @rl @n @e @b im z0) (spmdNegL @rl @n @e @b v) v
        rGe0 = spmdGeL @rl @n @e @b r z0
        outR = spmdSelectRaw @n @e @b rGe0 u (spmdAbsL @rl @n @e @b v)
        outI = spmdSelectRaw @n @e @b rGe0 vSigned (spmdSelectRaw @n @e @b (spmdLtL @rl @n @e @b im z0) (spmdNegL @rl @n @e @b u) u)
     in mkPair outR outI
  spmdPowL x y = spmdExpL @('LComplex b) @n @e @(Complex b) (spmdMulL @('LComplex b) @n @e @(Complex b) y (spmdLogL @('LComplex b) @n @e @(Complex b) x))
  spmdSinL z =
    let r = fstPair z
        im = sndPair z
     in mkPair
          (spmdMulL @rl @n @e @b (spmdSinL @rl @n @e @b r) (spmdCoshL @rl @n @e @b im))
          (spmdMulL @rl @n @e @b (spmdCosL @rl @n @e @b r) (spmdSinhL @rl @n @e @b im))
  spmdCosL z =
    let r = fstPair z
        im = sndPair z
     in mkPair
          (spmdMulL @rl @n @e @b (spmdCosL @rl @n @e @b r) (spmdCoshL @rl @n @e @b im))
          (spmdNegL @rl @n @e @b (spmdMulL @rl @n @e @b (spmdSinL @rl @n @e @b r) (spmdSinhL @rl @n @e @b im)))
  spmdTanL z = spmdDivL @('LComplex b) @n @e @(Complex b) (spmdSinL @('LComplex b) @n @e @(Complex b) z) (spmdCosL @('LComplex b) @n @e @(Complex b) z)
  spmdSinhL z =
    let r = fstPair z
        im = sndPair z
     in mkPair
          (spmdMulL @rl @n @e @b (spmdSinhL @rl @n @e @b r) (spmdCosL @rl @n @e @b im))
          (spmdMulL @rl @n @e @b (spmdCoshL @rl @n @e @b r) (spmdSinL @rl @n @e @b im))
  spmdCoshL z =
    let r = fstPair z
        im = sndPair z
     in mkPair
          (spmdMulL @rl @n @e @b (spmdCoshL @rl @n @e @b r) (spmdCosL @rl @n @e @b im))
          (spmdMulL @rl @n @e @b (spmdSinhL @rl @n @e @b r) (spmdSinL @rl @n @e @b im))
  spmdTanhL z = spmdDivL @('LComplex b) @n @e @(Complex b) (spmdSinhL @('LComplex b) @n @e @(Complex b) z) (spmdCoshL @('LComplex b) @n @e @(Complex b) z)
  spmdAsinhL z =
    let one = spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1
        z2 = spmdMulL @('LComplex b) @n @e @(Complex b) z z
     in spmdLogL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) z (spmdSqrtL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) z2 one)))
  spmdAcoshL z =
    let one = spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1
        zp1 = spmdSqrtL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) z one)
        zm1 = spmdSqrtL @('LComplex b) @n @e @(Complex b) (spmdSubL @('LComplex b) @n @e @(Complex b) z one)
     in spmdLogL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) z (spmdMulL @('LComplex b) @n @e @(Complex b) zp1 zm1))
  spmdAtanhL z =
    let one = spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1
        half = spmdFromRationalL @('LComplex b) @n @e @(Complex b) 0.5
        q = spmdDivL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) one z) (spmdSubL @('LComplex b) @n @e @(Complex b) one z)
     in spmdMulL @('LComplex b) @n @e @(Complex b) half (spmdLogL @('LComplex b) @n @e @(Complex b) q)
  spmdAsinL z =
    let iUnit = mkPair (spmdFromIntegerL @rl @n @e @b 0) (spmdFromIntegerL @rl @n @e @b 1)
        negI = mkPair (spmdFromIntegerL @rl @n @e @b 0) (spmdFromIntegerL @rl @n @e @b (-1))
     in spmdMulL @('LComplex b) @n @e @(Complex b) negI (spmdAsinhL @('LComplex b) @n @e @(Complex b) (spmdMulL @('LComplex b) @n @e @(Complex b) iUnit z))
  spmdAcosL z =
    let halfPi = mkPair (spmdMulL @rl @n @e @b (spmdFromRationalL @rl @n @e @b 0.5) (spmdPiL @rl @n @e @b)) (spmdFromIntegerL @rl @n @e @b 0)
     in spmdSubL @('LComplex b) @n @e @(Complex b) halfPi (spmdAsinL @('LComplex b) @n @e @(Complex b) z)
  spmdAtanL z =
    let iUnit = mkPair (spmdFromIntegerL @rl @n @e @b 0) (spmdFromIntegerL @rl @n @e @b 1)
        negI = mkPair (spmdFromIntegerL @rl @n @e @b 0) (spmdFromIntegerL @rl @n @e @b (-1))
     in spmdMulL @('LComplex b) @n @e @(Complex b) negI (spmdAtanhL @('LComplex b) @n @e @(Complex b) (spmdMulL @('LComplex b) @n @e @(Complex b) iUnit z))
  spmdLog1pL z =
    spmdLogL @('LComplex b) @n @e @(Complex b) (spmdAddL @('LComplex b) @n @e @(Complex b) (spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1) z)
  spmdExpm1L z =
    spmdSubL @('LComplex b) @n @e @(Complex b) (spmdExpL @('LComplex b) @n @e @(Complex b) z) (spmdFromIntegerL @('LComplex b) @n @e @(Complex b) 1)
  spmdFmaL x y c = spmdAddL @('LComplex b) @n @e @(Complex b) (spmdMulL @('LComplex b) @n @e @(Complex b) x y) c
  spmdHypotL x y =
    spmdSqrtL @('LComplex b) @n @e @(Complex b)
      ( spmdAddL @('LComplex b) @n @e @(Complex b)
          (spmdMulL @('LComplex b) @n @e @(Complex b) x x)
          (spmdMulL @('LComplex b) @n @e @(Complex b) y y)
      )

instance (l ~ SPMDLayout n a, SPMDFloatByLayout l n e a) => Fractional (SPMD n e a) where
  SPMD a / SPMD b = SPMD (liftA2 (spmdDivL @l @n @e @a) a b)
  recip (SPMD a) = SPMD (fmap (spmdRecipL @l @n @e @a) a)
  fromRational q = SPMD (pure (spmdFromRationalL @l @n @e @a q))

instance (l ~ SPMDLayout n a, SPMDFloatByLayout l n e a) => Floating (SPMD n e a) where
  pi = SPMD (pure (spmdPiL @l @n @e @a))
  exp (SPMD a) = SPMD (fmap (spmdExpL @l @n @e @a) a)
  log (SPMD a) = SPMD (fmap (spmdLogL @l @n @e @a) a)
  sqrt (SPMD a) = SPMD (fmap (spmdSqrtL @l @n @e @a) a)
  SPMD a ** SPMD b = SPMD (liftA2 (spmdPowL @l @n @e @a) a b)
  logBase x y = log y / log x
  sin (SPMD a) = SPMD (fmap (spmdSinL @l @n @e @a) a)
  cos (SPMD a) = SPMD (fmap (spmdCosL @l @n @e @a) a)
  tan (SPMD a) = SPMD (fmap (spmdTanL @l @n @e @a) a)
  asin (SPMD a) = SPMD (fmap (spmdAsinL @l @n @e @a) a)
  acos (SPMD a) = SPMD (fmap (spmdAcosL @l @n @e @a) a)
  atan (SPMD a) = SPMD (fmap (spmdAtanL @l @n @e @a) a)
  sinh (SPMD a) = SPMD (fmap (spmdSinhL @l @n @e @a) a)
  cosh (SPMD a) = SPMD (fmap (spmdCoshL @l @n @e @a) a)
  tanh (SPMD a) = SPMD (fmap (spmdTanhL @l @n @e @a) a)
  asinh (SPMD a) = SPMD (fmap (spmdAsinhL @l @n @e @a) a)
  acosh (SPMD a) = SPMD (fmap (spmdAcoshL @l @n @e @a) a)
  atanh (SPMD a) = SPMD (fmap (spmdAtanhL @l @n @e @a) a)
  log1p (SPMD a) = SPMD (fmap (spmdLog1pL @l @n @e @a) a)
  expm1 (SPMD a) = SPMD (fmap (spmdExpm1L @l @n @e @a) a)

instance (l ~ SPMDLayout n a, SPMDFloatByLayout l n e a) => FMA (SPMD n e a) where
  fma (SPMD a) (SPMD b) (SPMD c) = SPMD (spmdFmaL @l @n @e @a <$> a <*> b <*> c)

instance (l ~ SPMDLayout n a, SPMDFloatByLayout l n e a) => Hypot (SPMD n e a) where
  hypot (SPMD a) (SPMD b) = SPMD (liftA2 (spmdHypotL @l @n @e @a) a b)

-- Semilattices & ExpBool on SPMD n e Bool
instance (ExpSIMDMask e n (SPMDMaskElem n)) => Join (SPMD n e Bool) where
  SPMD a \/ SPMD b = SPMD (liftA2 mor a b)

instance (ExpSIMDMask e n (SPMDMaskElem n)) => Meet (SPMD n e Bool) where
  SPMD a /\ SPMD b = SPMD (liftA2 mand a b)

instance (ExpSIMDMask e n (SPMDMaskElem n)) => Lower (SPMD n e Bool) where
  lowerBound = SPMD (pure mfalse)

instance (ExpSIMDMask e n (SPMDMaskElem n)) => Upper (SPMD n e Bool) where
  upperBound = SPMD (pure mtrue)

instance (ExpSIMDMask e n (SPMDMaskElem n)) => ExpBool (SPMD n e) where
  false = SPMD (pure mfalse)
  true = SPMD (pure mtrue)
  land (SPMD a) (SPMD b) = SPMD (liftA2 mand a b)
  lor (SPMD a) (SPMD b) = SPMD (liftA2 mor a b)
  lnot (SPMD a) = SPMD (fmap mnot a)

-- ExpEq & ExpOrd
class SPMDEqByLayout (l :: Layout) (n :: Nat) e a where
  spmdEqL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))
  spmdNeL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))

class (SPMDEqByLayout l n e a) => SPMDCmpByLayout (l :: Layout) (n :: Nat) e a where
  spmdLtL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))
  spmdLeL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))
  spmdGtL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))
  spmdGeL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> e (SIMDMask e n (SPMDMaskElem n))

instance (SPMDRawCmp 'LDirect n e a) => SPMDEqByLayout 'LDirect n e a where
  spmdEqL = rawEq @'LDirect @n @e @a
  spmdNeL = rawNe @'LDirect @n @e @a

instance (SPMDRawCmp 'LDirect n e a) => SPMDCmpByLayout 'LDirect n e a where
  spmdLtL = rawLt @'LDirect @n @e @a
  spmdLeL = rawLe @'LDirect @n @e @a
  spmdGtL = rawGt @'LDirect @n @e @a
  spmdGeL = rawGe @'LDirect @n @e @a

instance (SPMDRawCmp ('LSplit h) n e a) => SPMDEqByLayout ('LSplit h) n e a where
  spmdEqL = rawEq @('LSplit h) @n @e @a
  spmdNeL = rawNe @('LSplit h) @n @e @a

instance (SPMDRawCmp ('LSplit h) n e a) => SPMDCmpByLayout ('LSplit h) n e a where
  spmdLtL = rawLt @('LSplit h) @n @e @a
  spmdLeL = rawLe @('LSplit h) @n @e @a
  spmdGtL = rawGt @('LSplit h) @n @e @a
  spmdGeL = rawGe @('LSplit h) @n @e @a

instance (SPMDRawCmp 'LPad4 2 e a) => SPMDEqByLayout 'LPad4 2 e a where
  spmdEqL = rawEq @'LPad4 @2 @e @a
  spmdNeL = rawNe @'LPad4 @2 @e @a

instance (SPMDRawCmp 'LPad4 2 e a) => SPMDCmpByLayout 'LPad4 2 e a where
  spmdLtL = rawLt @'LPad4 @2 @e @a
  spmdLeL = rawLe @'LPad4 @2 @e @a
  spmdGtL = rawGt @'LPad4 @2 @e @a
  spmdGeL = rawGe @'LPad4 @2 @e @a

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, SPMDRawCmp rl n e Int64, Num (e Int64)) =>
  SPMDEqByLayout ('LInt64 rl) n e Int64
  where
  spmdEqL x y = rawEq @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)
  spmdNeL x y = rawNe @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, SPMDRawCmp rl n e Int64, Num (e Int64)) =>
  SPMDCmpByLayout ('LInt64 rl) n e Int64
  where
  spmdLtL x y = rawLt @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)
  spmdLeL x y = rawLe @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)
  spmdGtL x y = rawGt @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)
  spmdGeL x y = rawGe @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y)

instance (SPMDEqByLayout (SPMDLayout n base) n e base) => SPMDEqByLayout ('LNewtype base) n e a where
  spmdEqL = spmdEqL @(SPMDLayout n base) @n @e @base
  spmdNeL = spmdNeL @(SPMDLayout n base) @n @e @base

instance (SPMDCmpByLayout (SPMDLayout n base) n e base) => SPMDCmpByLayout ('LNewtype base) n e a where
  spmdLtL = spmdLtL @(SPMDLayout n base) @n @e @base
  spmdLeL = spmdLeL @(SPMDLayout n base) @n @e @base
  spmdGtL = spmdGtL @(SPMDLayout n base) @n @e @base
  spmdGeL = spmdGeL @(SPMDLayout n base) @n @e @base

instance (ExpSIMDMask e n (SPMDMaskElem n)) => SPMDEqByLayout 'LBool n e Bool where
  spmdEqL a b = mnot (a `mxor` b)
  spmdNeL a b = a `mxor` b

instance (ExpSIMDMask e n (SPMDMaskElem n)) => SPMDCmpByLayout 'LBool n e Bool where
  spmdLtL a b = a `mandnot` b
  spmdLeL a b = mnot a `mor` b
  spmdGtL a b = b `mandnot` a
  spmdGeL a b = a `mor` mnot b

instance
  ( rl ~ SPMDLayout n b,
    SPMDExpr n e b ~ e (SPMDRep n e b),
    SPMDEqByLayout rl n e b,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpPair e,
    JanusTyped e (SPMDRep n e b)
  ) =>
  SPMDEqByLayout ('LComplex b) n e (Complex b)
  where
  spmdEqL z1 z2 =
    spmdEqL @rl @n @e @b (fstPair z1) (fstPair z2) `mand` spmdEqL @rl @n @e @b (sndPair z1) (sndPair z2)
  spmdNeL z1 z2 =
    spmdNeL @rl @n @e @b (fstPair z1) (fstPair z2) `mor` spmdNeL @rl @n @e @b (sndPair z1) (sndPair z2)

instance (l ~ SPMDLayout n a, SPMDEqByLayout l n e a) => ExpEq (SPMD n e) a where
  eq (SPMD a) (SPMD b) = SPMD (liftA2 (spmdEqL @l @n @e @a) a b)
  ne (SPMD a) (SPMD b) = SPMD (liftA2 (spmdNeL @l @n @e @a) a b)

instance (l ~ SPMDLayout n a, SPMDCmpByLayout l n e a) => ExpOrd (SPMD n e) a where
  lt (SPMD a) (SPMD b) = SPMD (liftA2 (spmdLtL @l @n @e @a) a b)
  le (SPMD a) (SPMD b) = SPMD (liftA2 (spmdLeL @l @n @e @a) a b)
  gt (SPMD a) (SPMD b) = SPMD (liftA2 (spmdGtL @l @n @e @a) a b)
  ge (SPMD a) (SPMD b) = SPMD (liftA2 (spmdGeL @l @n @e @a) a b)

-- ExpBits
class (SPMDEqByLayout l n e a) => SPMDBitsByLayout (l :: Layout) (n :: Nat) e a where
  spmdIandL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdIorL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdIeorL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdInotL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdLshiftL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdRshiftL :: SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a

instance (SPMDRawBits 'LDirect n e a) => SPMDBitsByLayout 'LDirect n e a where
  spmdIandL = rawBitAnd @'LDirect @n @e @a
  spmdIorL = rawBitOr @'LDirect @n @e @a
  spmdIeorL = rawBitXor @'LDirect @n @e @a
  spmdInotL = rawBitNot @'LDirect @n @e @a
  spmdLshiftL = rawShl @'LDirect @n @e @a
  spmdRshiftL = rawShr @'LDirect @n @e @a

instance (SPMDRawBits ('LSplit h) n e a) => SPMDBitsByLayout ('LSplit h) n e a where
  spmdIandL = rawBitAnd @('LSplit h) @n @e @a
  spmdIorL = rawBitOr @('LSplit h) @n @e @a
  spmdIeorL = rawBitXor @('LSplit h) @n @e @a
  spmdInotL = rawBitNot @('LSplit h) @n @e @a
  spmdLshiftL = rawShl @('LSplit h) @n @e @a
  spmdRshiftL = rawShr @('LSplit h) @n @e @a

instance (SPMDRawBits 'LPad4 2 e a) => SPMDBitsByLayout 'LPad4 2 e a where
  spmdIandL = rawBitAnd @'LPad4 @2 @e @a
  spmdIorL = rawBitOr @'LPad4 @2 @e @a
  spmdIeorL = rawBitXor @'LPad4 @2 @e @a
  spmdInotL = rawBitNot @'LPad4 @2 @e @a
  spmdLshiftL = rawShl @'LPad4 @2 @e @a
  spmdRshiftL = rawShr @'LPad4 @2 @e @a

instance
  (rl ~ SPMDRawLayout n Int64, SPMDRawNum rl n e Int64, SPMDRawBits rl n e Int64, Num (e Int64)) =>
  SPMDBitsByLayout ('LInt64 rl) n e Int64
  where
  spmdIandL x y = I64Varying (rawBitAnd @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y))
  spmdIorL x y = I64Varying (rawBitOr @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y))
  spmdIeorL x y = I64Varying (rawBitXor @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y))
  spmdInotL x = I64Varying (rawBitNot @rl @n @e @Int64 (materializeInt64 x))
  spmdLshiftL x y = I64Varying (rawShl @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y))
  spmdRshiftL x y = I64Varying (rawShr @rl @n @e @Int64 (materializeInt64 x) (materializeInt64 y))

instance (SPMDBitsByLayout (SPMDLayout n base) n e base) => SPMDBitsByLayout ('LNewtype base) n e a where
  spmdIandL = spmdIandL @(SPMDLayout n base) @n @e @base
  spmdIorL = spmdIorL @(SPMDLayout n base) @n @e @base
  spmdIeorL = spmdIeorL @(SPMDLayout n base) @n @e @base
  spmdInotL = spmdInotL @(SPMDLayout n base) @n @e @base
  spmdLshiftL = spmdLshiftL @(SPMDLayout n base) @n @e @base
  spmdRshiftL = spmdRshiftL @(SPMDLayout n base) @n @e @base

instance (l ~ SPMDLayout n a, SPMDBitsByLayout l n e a) => ExpBits (SPMD n e) a where
  iand (SPMD a) (SPMD b) = SPMD (liftA2 (spmdIandL @l @n @e @a) a b)
  ior (SPMD a) (SPMD b) = SPMD (liftA2 (spmdIorL @l @n @e @a) a b)
  ieor (SPMD a) (SPMD b) = SPMD (liftA2 (spmdIeorL @l @n @e @a) a b)
  inot (SPMD a) = SPMD (fmap (spmdInotL @l @n @e @a) a)
  lshift (SPMD a) (SPMD b) = SPMD (liftA2 (spmdLshiftL @l @n @e @a) a b)
  rshift (SPMD a) (SPMD b) = SPMD (liftA2 (spmdRshiftL @l @n @e @a) a b)

-- ExpIntegral (reads the SPMDMask from Reader to guard inactive lanes!)
class (SPMDNumByLayout l n e a) => SPMDIntegralByLayout (l :: Layout) (n :: Nat) e a where
  spmdQuotL :: SPMDMask n e -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a
  spmdRemL :: SPMDMask n e -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a -> SPMDExprByLayout l n e a

instance (SPMDRawIntegral 'LDirect n e a) => SPMDIntegralByLayout 'LDirect n e a where
  spmdQuotL = rawQuot @'LDirect @n @e @a
  spmdRemL = rawRem @'LDirect @n @e @a

instance (SPMDRawIntegral ('LSplit h) n e a) => SPMDIntegralByLayout ('LSplit h) n e a where
  spmdQuotL = rawQuot @('LSplit h) @n @e @a
  spmdRemL = rawRem @('LSplit h) @n @e @a

instance (SPMDRawIntegral 'LPad4 2 e a) => SPMDIntegralByLayout 'LPad4 2 e a where
  spmdQuotL = rawQuot @'LPad4 @2 @e @a
  spmdRemL = rawRem @'LPad4 @2 @e @a

instance
  (KnownNat n, rl ~ SPMDRawLayout n Int64, SPMDRawIntegral rl n e Int64, Num (e Int64)) =>
  SPMDIntegralByLayout ('LInt64 rl) n e Int64
  where
  spmdQuotL msk a b = case (a, b) of
    (I64Const x, I64Const y) | y /= 0 && not (x == minBound && y == -1) -> I64Const (x `quot` y)
    _ -> I64Varying (rawQuot @rl @n @e @Int64 msk (materializeInt64 a) (materializeInt64 b))
  spmdRemL msk a b = case (a, b) of
    (I64Const x, I64Const y) | y /= 0 && not (x == minBound && y == -1) -> I64Const (x `rem` y)
    _ -> I64Varying (rawRem @rl @n @e @Int64 msk (materializeInt64 a) (materializeInt64 b))

instance (SPMDIntegralByLayout (SPMDLayout n base) n e base) => SPMDIntegralByLayout ('LNewtype base) n e a where
  spmdQuotL = spmdQuotL @(SPMDLayout n base) @n @e @base
  spmdRemL = spmdRemL @(SPMDLayout n base) @n @e @base

instance (l ~ SPMDLayout n a, SPMDIntegralByLayout l n e a) => ExpIntegral (SPMD n e) a where
  quotient (SPMD ma) (SPMD mb) = SPMD $ do
    msk <- ask
    a <- ma
    b <- mb
    pure (spmdQuotL @l @n @e @a msk a b)
  remainder (SPMD ma) (SPMD mb) = SPMD $ do
    msk <- ask
    a <- ma
    b <- mb
    pure (spmdRemL @l @n @e @a msk a b)

-- ExpCond & ExpLet
instance (ExpSIMDMask e n (SPMDMaskElem n)) => ExpCond (SPMD n e) where
  ifThenElse :: forall a. (JanusTyped (SPMD n e) a) => SPMD n e Bool -> SPMD n e a -> SPMD n e a -> SPMD n e a
  ifThenElse (SPMD mc) (SPMD mt) (SPMD mf) = SPMD $ do
    cur <- ask
    c <- mc
    t <- local (const (spmdMaskAnd cur c)) mt
    f <- local (const (spmdMaskAndNot c cur)) mf
    pure $
      spmdDematerialize @n @e @a $
        spmdSelectRaw @n @e @a c (spmdMaterialize @n @e @a t) (spmdMaterialize @n @e @a f)

instance (ExpLet e) => ExpLet (SPMD n e) where
  let_ :: forall a b. (JanusTyped (SPMD n e) a, JanusTyped (SPMD n e) b) => SPMD n e a -> (SPMD n e a -> SPMD n e b) -> SPMD n e b
  let_ (SPMD ma) f = SPMD $ do
    cur <- ask
    let xa = runReader ma cur
        rawRes =
          spmdLetRaw @n @e @a xa $ \xa' ->
            spmdMaterialize @n @e @b (runReader (getSPMD (f (SPMD (pure xa')))) cur)
    pure (spmdDematerialize @n @e @b rawRes)

-- ExpComplex
instance
  ( SPMDExpr n e a ~ e (SPMDRep n e a),
    ExpPair e,
    JanusTyped e (SPMDRep n e a)
  ) =>
  ExpComplex (SPMD n e) a
  where
  rePart (SPMD mz) = SPMD (fmap fstPair mz)
  imPart (SPMD mz) = SPMD (fmap sndPair mz)
