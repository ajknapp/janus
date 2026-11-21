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
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.SPMD.Raw where

import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (eqT)
import GHC.Float (expm1, log1p)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Inject (ExpInject (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (FMA (..), Hypot (..))
import Janus.Expression.Ord (ExpOrd (..), max', min')
import Janus.Expression.SIMD
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 7. Raw Scalar Vector Operations (SPMDRawVec: 'LDirect, 'LSplit, 'LPad4)
--------------------------------------------------------------------------------

class
  (JanusTyped e (SPMDRepByLayout rl n e a)) =>
  SPMDRawVec (rl :: Layout) (n :: Nat) e a
  where
  rawZero :: e (SPMDRepByLayout rl n e a)
  rawSplat :: e a -> e (SPMDRepByLayout rl n e a)
  rawIota :: e (SPMDRepByLayout rl n e a)
  rawFromLanes :: [e a] -> e (SPMDRepByLayout rl n e a)
  rawToLanes :: e (SPMDRepByLayout rl n e a) -> [e a]
  rawSelect ::
    e (SIMDMask e n (SPMDMaskElem n)) ->
    e (SPMDRepByLayout rl n e a) ->
    e (SPMDRepByLayout rl n e a) ->
    e (SPMDRepByLayout rl n e a)

instance
  ( me ~ SPMDMaskElem n,
    SIMDShape n a,
    CPUSIMDElem a,
    CPUSIMDElem me,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n a,
    ExpSIMDVec e n me,
    ExpSIMDCmp e n me,
    ExpSIMDCast e,
    Num (e a),
    Num (e me),
    JanusTyped e (SIMDVector e n a)
  ) =>
  SPMDRawVec 'LDirect n e a
  where
  rawZero = vzero
  rawSplat = vsplat
  rawIota = viota
  rawFromLanes = vfromLanes
  rawToLanes v = [extractLaneAt i v | i <- [0 .. simdLanes (Proxy @n) (Proxy @a) - 1]]
  rawSelect m t f = vselect (maskFromCanonical @n @e @a m) t f

instance
  ( rlH ~ SPMDRawLayout h a,
    KnownNat n,
    KnownNat h,
    ExpPair e,
    SPMDMaskSplit n h e,
    SPMDRawVec rlH h e a,
    SPMDRawNum rlH h e a,
    Num (e a),
    JanusTyped e (SIMDPair (SPMDRepByLayout rlH h e a) (SPMDRepByLayout rlH h e a))
  ) =>
  SPMDRawVec ('LSplit h) n e a
  where
  rawZero = mkPair (rawZero @rlH @h @e @a) (rawZero @rlH @h @e @a)
  rawSplat x =
    let hx = rawSplat @rlH @h @e @a x
     in mkPair hx hx
  rawIota =
    let hVal = fromIntegral (natVal (Proxy @h)) :: Integer
        lo = rawIota @rlH @h @e @a
        hi = rawAdd @rlH @h @e @a lo (rawSplat @rlH @h @e @a (fromInteger hVal))
     in mkPair lo hi
  rawFromLanes xs =
    let h = fromIntegral (natVal (Proxy @h)) :: Int
     in mkPair (rawFromLanes @rlH @h @e @a (take h xs)) (rawFromLanes @rlH @h @e @a (drop h xs))
  rawToLanes p =
    rawToLanes @rlH @h @e @a (fstPair p) <> rawToLanes @rlH @h @e @a (sndPair p)
  rawSelect m t f =
    let (mLo, mHi) = splitSPMDMask @n @h @e m
     in mkPair
          (rawSelect @rlH @h @e @a mLo (fstPair t) (fstPair f))
          (rawSelect @rlH @h @e @a mHi (sndPair t) (sndPair f))

instance
  ( SIMDShape 4 a,
    CPUSIMDElem a,
    ExpSIMDVec e 4 a,
    ExpSIMDCmp e 4 a,
    ExpSIMDVec e 4 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDMask e 2 Int64,
    ExpSIMDCast e,
    Num (e a),
    Num (e Int64),
    JanusTyped e (SIMDVector e 4 a)
  ) =>
  SPMDRawVec 'LPad4 2 e a
  where
  rawZero = vzero
  rawSplat = vsplat
  rawIota = viota
  rawFromLanes [x0, x1] = vfromLanes [x0, x1, 0, 0]
  rawFromLanes xs = error $ "rawFromLanes @2 (LPad4): expected 2 elements, got " <> show (length xs)
  rawToLanes v = [extractLaneAt 0 v, extractLaneAt 1 v]
  rawSelect m2 t f = vselect (pad4MaskFrom2 @e @a m2) t f

--------------------------------------------------------------------------------
-- 8. Raw Arithmetic, Comparisons, Bitwise, Integral, & Casts
--------------------------------------------------------------------------------

class (SPMDRawVec rl n e a) => SPMDRawNum (rl :: Layout) (n :: Nat) e a where
  rawAdd :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawSub :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawMul :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawNeg :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAbs :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawSignum :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawFromInteger :: Integer -> e (SPMDRepByLayout rl n e a)

instance
  ( SPMDRawVec 'LDirect n e a,
    Num (e (SIMDVector e n a))
  ) =>
  SPMDRawNum 'LDirect n e a
  where
  rawAdd = (+)
  rawSub = (-)
  rawMul = (*)
  rawNeg = negate
  rawAbs = abs
  rawSignum = signum
  rawFromInteger = fromInteger

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawVec ('LSplit h) n e a,
    SPMDRawNum rlH h e a,
    ExpPair e
  ) =>
  SPMDRawNum ('LSplit h) n e a
  where
  rawAdd x y = mkPair (rawAdd @rlH @h @e @a (fstPair x) (fstPair y)) (rawAdd @rlH @h @e @a (sndPair x) (sndPair y))
  rawSub x y = mkPair (rawSub @rlH @h @e @a (fstPair x) (fstPair y)) (rawSub @rlH @h @e @a (sndPair x) (sndPair y))
  rawMul x y = mkPair (rawMul @rlH @h @e @a (fstPair x) (fstPair y)) (rawMul @rlH @h @e @a (sndPair x) (sndPair y))
  rawNeg x = mkPair (rawNeg @rlH @h @e @a (fstPair x)) (rawNeg @rlH @h @e @a (sndPair x))
  rawAbs x = mkPair (rawAbs @rlH @h @e @a (fstPair x)) (rawAbs @rlH @h @e @a (sndPair x))
  rawSignum x = mkPair (rawSignum @rlH @h @e @a (fstPair x)) (rawSignum @rlH @h @e @a (sndPair x))
  rawFromInteger k = let hk = rawFromInteger @rlH @h @e @a k in mkPair hk hk

instance
  ( SPMDRawVec 'LPad4 2 e a,
    Num (e (SIMDVector e 4 a))
  ) =>
  SPMDRawNum 'LPad4 2 e a
  where
  rawAdd = (+)
  rawSub = (-)
  rawMul = (*)
  rawNeg = negate
  rawAbs = abs
  rawSignum = signum
  rawFromInteger = fromInteger

-- Raw Relational & Equality Comparisons
class (SPMDRawVec rl n e a) => SPMDRawCmp (rl :: Layout) (n :: Nat) e a where
  rawEq :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))
  rawNe :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))
  rawLt :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))
  rawLe :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))
  rawGt :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))
  rawGe :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SIMDMask e n (SPMDMaskElem n))

instance
  ( me ~ SPMDMaskElem n,
    SPMDRawVec 'LDirect n e a,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n a,
    ExpSIMDCmp e n me,
    ExpSIMDVec e n me,
    ExpSIMDCast e,
    CPUSIMDElem a,
    CPUSIMDElem me,
    Num (e a),
    Num (e me)
  ) =>
  SPMDRawCmp 'LDirect n e a
  where
  rawEq x y = maskToCanonical @n @e @a (x `veq` y)
  rawNe x y = maskToCanonical @n @e @a (x `vne` y)
  rawLt x y = maskToCanonical @n @e @a (x `vlt` y)
  rawLe x y = maskToCanonical @n @e @a (x `vle` y)
  rawGt x y = maskToCanonical @n @e @a (x `vgt` y)
  rawGe x y = maskToCanonical @n @e @a (x `vge` y)

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawVec ('LSplit h) n e a,
    SPMDRawCmp rlH h e a,
    SPMDMaskSplit n h e,
    ExpPair e
  ) =>
  SPMDRawCmp ('LSplit h) n e a
  where
  rawEq x y = concatSPMDMask @n @h @e (rawEq @rlH @h @e @a (fstPair x) (fstPair y)) (rawEq @rlH @h @e @a (sndPair x) (sndPair y))
  rawNe x y = concatSPMDMask @n @h @e (rawNe @rlH @h @e @a (fstPair x) (fstPair y)) (rawNe @rlH @h @e @a (sndPair x) (sndPair y))
  rawLt x y = concatSPMDMask @n @h @e (rawLt @rlH @h @e @a (fstPair x) (fstPair y)) (rawLt @rlH @h @e @a (sndPair x) (sndPair y))
  rawLe x y = concatSPMDMask @n @h @e (rawLe @rlH @h @e @a (fstPair x) (fstPair y)) (rawLe @rlH @h @e @a (sndPair x) (sndPair y))
  rawGt x y = concatSPMDMask @n @h @e (rawGt @rlH @h @e @a (fstPair x) (fstPair y)) (rawGt @rlH @h @e @a (sndPair x) (sndPair y))
  rawGe x y = concatSPMDMask @n @h @e (rawGe @rlH @h @e @a (fstPair x) (fstPair y)) (rawGe @rlH @h @e @a (sndPair x) (sndPair y))

instance
  ( SPMDRawVec 'LPad4 2 e a,
    SIMDShape 4 a,
    CPUSIMDElem a,
    ExpSIMDVec e 4 a,
    ExpSIMDCmp e 4 a,
    ExpSIMDMask e 4 a,
    ExpSIMDVec e 4 Int64,
    ExpSIMDCmp e 2 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDSplit e 4 2 Int64,
    ExpSIMDCast e,
    Num (e a),
    Num (e Int64)
  ) =>
  SPMDRawCmp 'LPad4 2 e a
  where
  rawEq x y = pad4MaskTo2 @e @a (x `veq` y)
  rawNe x y = pad4MaskTo2 @e @a (x `vne` y)
  rawLt x y = pad4MaskTo2 @e @a (x `vlt` y)
  rawLe x y = pad4MaskTo2 @e @a (x `vle` y)
  rawGt x y = pad4MaskTo2 @e @a (x `vgt` y)
  rawGe x y = pad4MaskTo2 @e @a (x `vge` y)

-- Raw Bitwise Operations
class (SPMDRawCmp rl n e a) => SPMDRawBits (rl :: Layout) (n :: Nat) e a where
  rawBitAnd :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawBitOr :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawBitXor :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawBitNot :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawShl :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawShr :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)

instance
  (SPMDRawCmp 'LDirect n e a, ExpSIMDBits e n a) =>
  SPMDRawBits 'LDirect n e a
  where
  rawBitAnd = vand
  rawBitOr = vor
  rawBitXor = vxor
  rawBitNot = vnot
  rawShl = vshlVar
  rawShr = vshrVar

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawCmp ('LSplit h) n e a,
    SPMDRawBits rlH h e a,
    ExpPair e
  ) =>
  SPMDRawBits ('LSplit h) n e a
  where
  rawBitAnd x y = mkPair (rawBitAnd @rlH @h @e @a (fstPair x) (fstPair y)) (rawBitAnd @rlH @h @e @a (sndPair x) (sndPair y))
  rawBitOr x y = mkPair (rawBitOr @rlH @h @e @a (fstPair x) (fstPair y)) (rawBitOr @rlH @h @e @a (sndPair x) (sndPair y))
  rawBitXor x y = mkPair (rawBitXor @rlH @h @e @a (fstPair x) (fstPair y)) (rawBitXor @rlH @h @e @a (sndPair x) (sndPair y))
  rawBitNot x = mkPair (rawBitNot @rlH @h @e @a (fstPair x)) (rawBitNot @rlH @h @e @a (sndPair x))
  rawShl x y = mkPair (rawShl @rlH @h @e @a (fstPair x) (fstPair y)) (rawShl @rlH @h @e @a (sndPair x) (sndPair y))
  rawShr x y = mkPair (rawShr @rlH @h @e @a (fstPair x) (fstPair y)) (rawShr @rlH @h @e @a (sndPair x) (sndPair y))

instance
  (SPMDRawCmp 'LPad4 2 e a, ExpSIMDBits e 4 a) =>
  SPMDRawBits 'LPad4 2 e a
  where
  rawBitAnd = vand
  rawBitOr = vor
  rawBitXor = vxor
  rawBitNot = vnot
  rawShl = vshlVar
  rawShr = vshrVar

-- Raw Mask-Safe Integer Division & Remainder
class (SPMDRawNum rl n e a, SPMDRawCmp rl n e a) => SPMDRawIntegral (rl :: Layout) (n :: Nat) e a where
  rawQuot :: SPMDMask n e -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawRem :: SPMDMask n e -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)

safeScalarQuotRem ::
  forall e a.
  (SIMDElem a, Bounded a, ExpInject e a, ExpIntegral e a, ExpOrd e a, ExpCond e, ExpLet e, JanusTyped e a, JanusTyped e Bool, Num (e a)) =>
  Bool ->
  e a ->
  e a ->
  e a
safeScalarQuotRem isQuot x0 y0 =
  let_ x0 $ \x ->
    let_ y0 $ \y ->
      let_ (y `eq` 0) $ \zeroGuard ->
        let_ (ifThenElse zeroGuard 1 y) $ \safeY1 ->
          if simdElemSigned (Proxy @a)
            then
              let minB = negate (inject (maxBound :: a)) - 1
               in let_ ((x `eq` minB) `land` (safeY1 `eq` (-1))) $ \overflow ->
                    let_ (ifThenElse overflow 1 safeY1) $ \safeY ->
                      if isQuot
                        then ifThenElse zeroGuard 0 (ifThenElse overflow minB (x `quotient` safeY))
                        else ifThenElse (zeroGuard `lor` overflow) 0 (x `remainder` safeY)
            else
              if isQuot
                then ifThenElse zeroGuard 0 (x `quotient` safeY1)
                else ifThenElse zeroGuard 0 (x `remainder` safeY1)

instance
  ( me ~ SPMDMaskElem n,
    SPMDRawNum 'LDirect n e a,
    SPMDRawCmp 'LDirect n e a,
    SIMDElem a,
    CPUSIMDElem a,
    CPUSIMDElem me,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n a,
    ExpSIMDMask e n a,
    ExpSIMDVec e n me,
    ExpSIMDCmp e n me,
    ExpSIMDCast e,
    Bounded a,
    ExpInject e a,
    ExpIntegral e a,
    ExpOrd e a,
    ExpCond e,
    ExpLet e,
    JanusTyped e a,
    JanusTyped e Bool,
    Num (e a),
    Num (e me)
  ) =>
  SPMDRawIntegral 'LDirect n e a
  where
  rawQuot msk x y =
    let (x', y') = case msk of
          SPMDMaskAllTrue -> (x, y)
          SPMDMask m ->
            let ma = maskFromCanonical @n @e @a m
             in (vselect ma x vzero, vselect ma y (vsplat 1))
     in rawFromLanes @'LDirect @n @e @a (zipWith (safeScalarQuotRem True) (rawToLanes @'LDirect @n @e @a x') (rawToLanes @'LDirect @n @e @a y'))
  rawRem msk x y =
    let (x', y') = case msk of
          SPMDMaskAllTrue -> (x, y)
          SPMDMask m ->
            let ma = maskFromCanonical @n @e @a m
             in (vselect ma x vzero, vselect ma y (vsplat 1))
     in rawFromLanes @'LDirect @n @e @a (zipWith (safeScalarQuotRem False) (rawToLanes @'LDirect @n @e @a x') (rawToLanes @'LDirect @n @e @a y'))

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawNum ('LSplit h) n e a,
    SPMDRawCmp ('LSplit h) n e a,
    SPMDRawIntegral rlH h e a,
    SPMDMaskSplit n h e,
    ExpPair e
  ) =>
  SPMDRawIntegral ('LSplit h) n e a
  where
  rawQuot msk x y =
    let (mLo, mHi) = splitExecMask @n @h msk
     in mkPair
          (rawQuot @rlH @h @e @a mLo (fstPair x) (fstPair y))
          (rawQuot @rlH @h @e @a mHi (sndPair x) (sndPair y))
  rawRem msk x y =
    let (mLo, mHi) = splitExecMask @n @h msk
     in mkPair
          (rawRem @rlH @h @e @a mLo (fstPair x) (fstPair y))
          (rawRem @rlH @h @e @a mHi (sndPair x) (sndPair y))

instance
  ( SPMDRawNum 'LPad4 2 e a,
    SPMDRawCmp 'LPad4 2 e a,
    SIMDShape 4 a,
    SIMDElem a,
    CPUSIMDElem a,
    ExpSIMDVec e 4 a,
    ExpSIMDCmp e 4 a,
    ExpSIMDMask e 4 a,
    ExpSIMDVec e 4 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDMask e 2 Int64,
    ExpSIMDCast e,
    Bounded a,
    ExpInject e a,
    ExpIntegral e a,
    ExpOrd e a,
    ExpCond e,
    ExpLet e,
    JanusTyped e a,
    JanusTyped e Bool,
    Num (e Int64),
    Num (e a)
  ) =>
  SPMDRawIntegral 'LPad4 2 e a
  where
  rawQuot msk x y =
    let (x', y') = case msk of
          SPMDMaskAllTrue -> (x, y)
          SPMDMask m ->
            let ma = pad4MaskFrom2 @e @a m
             in (vselect ma x vzero, vselect ma y (vsplat 1))
     in rawFromLanes @'LPad4 @2 @e @a (zipWith (safeScalarQuotRem True) (rawToLanes @'LPad4 @2 @e @a x') (rawToLanes @'LPad4 @2 @e @a y'))
  rawRem msk x y =
    let (x', y') = case msk of
          SPMDMaskAllTrue -> (x, y)
          SPMDMask m ->
            let ma = pad4MaskFrom2 @e @a m
             in (vselect ma x vzero, vselect ma y (vsplat 1))
     in rawFromLanes @'LPad4 @2 @e @a (zipWith (safeScalarQuotRem False) (rawToLanes @'LPad4 @2 @e @a x') (rawToLanes @'LPad4 @2 @e @a y'))

-- Raw Fractional, Floating, FMA, Hypot
class (SPMDRawNum rl n e a) => SPMDRawFloat (rl :: Layout) (n :: Nat) e a where
  rawDiv :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawRecip :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawFromRational :: Rational -> e (SPMDRepByLayout rl n e a)
  rawPi :: e (SPMDRepByLayout rl n e a)
  rawExp :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawLog :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawSqrt :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawPow :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawSin :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawCos :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawTan :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAsin :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAcos :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAtan :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawSinh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawCosh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawTanh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAsinh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAcosh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawAtanh :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawLog1p :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawExpm1 :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawFma :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)
  rawHypot :: e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a) -> e (SPMDRepByLayout rl n e a)

instance
  ( SPMDRawNum 'LDirect n e a,
    Floating (e (SIMDVector e n a)),
    FMA (e (SIMDVector e n a)),
    Hypot (e (SIMDVector e n a))
  ) =>
  SPMDRawFloat 'LDirect n e a
  where
  rawDiv = (/)
  rawRecip = recip
  rawFromRational = fromRational
  rawPi = pi
  rawExp = exp
  rawLog = log
  rawSqrt = sqrt
  rawPow = (**)
  rawSin = sin
  rawCos = cos
  rawTan = tan
  rawAsin = asin
  rawAcos = acos
  rawAtan = atan
  rawSinh = sinh
  rawCosh = cosh
  rawTanh = tanh
  rawAsinh = asinh
  rawAcosh = acosh
  rawAtanh = atanh
  rawLog1p = log1p
  rawExpm1 = expm1
  rawFma = fma
  rawHypot = hypot

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawNum ('LSplit h) n e a,
    SPMDRawFloat rlH h e a,
    ExpPair e
  ) =>
  SPMDRawFloat ('LSplit h) n e a
  where
  rawDiv x y = mkPair (rawDiv @rlH @h @e @a (fstPair x) (fstPair y)) (rawDiv @rlH @h @e @a (sndPair x) (sndPair y))
  rawRecip x = mkPair (rawRecip @rlH @h @e @a (fstPair x)) (rawRecip @rlH @h @e @a (sndPair x))
  rawFromRational q = let hq = rawFromRational @rlH @h @e @a q in mkPair hq hq
  rawPi = mkPair (rawPi @rlH @h @e @a) (rawPi @rlH @h @e @a)
  rawExp x = mkPair (rawExp @rlH @h @e @a (fstPair x)) (rawExp @rlH @h @e @a (sndPair x))
  rawLog x = mkPair (rawLog @rlH @h @e @a (fstPair x)) (rawLog @rlH @h @e @a (sndPair x))
  rawSqrt x = mkPair (rawSqrt @rlH @h @e @a (fstPair x)) (rawSqrt @rlH @h @e @a (sndPair x))
  rawPow x y = mkPair (rawPow @rlH @h @e @a (fstPair x) (fstPair y)) (rawPow @rlH @h @e @a (sndPair x) (sndPair y))
  rawSin x = mkPair (rawSin @rlH @h @e @a (fstPair x)) (rawSin @rlH @h @e @a (sndPair x))
  rawCos x = mkPair (rawCos @rlH @h @e @a (fstPair x)) (rawCos @rlH @h @e @a (sndPair x))
  rawTan x = mkPair (rawTan @rlH @h @e @a (fstPair x)) (rawTan @rlH @h @e @a (sndPair x))
  rawAsin x = mkPair (rawAsin @rlH @h @e @a (fstPair x)) (rawAsin @rlH @h @e @a (sndPair x))
  rawAcos x = mkPair (rawAcos @rlH @h @e @a (fstPair x)) (rawAcos @rlH @h @e @a (sndPair x))
  rawAtan x = mkPair (rawAtan @rlH @h @e @a (fstPair x)) (rawAtan @rlH @h @e @a (sndPair x))
  rawSinh x = mkPair (rawSinh @rlH @h @e @a (fstPair x)) (rawSinh @rlH @h @e @a (sndPair x))
  rawCosh x = mkPair (rawCosh @rlH @h @e @a (fstPair x)) (rawCosh @rlH @h @e @a (sndPair x))
  rawTanh x = mkPair (rawTanh @rlH @h @e @a (fstPair x)) (rawTanh @rlH @h @e @a (sndPair x))
  rawAsinh x = mkPair (rawAsinh @rlH @h @e @a (fstPair x)) (rawAsinh @rlH @h @e @a (sndPair x))
  rawAcosh x = mkPair (rawAcosh @rlH @h @e @a (fstPair x)) (rawAcosh @rlH @h @e @a (sndPair x))
  rawAtanh x = mkPair (rawAtanh @rlH @h @e @a (fstPair x)) (rawAtanh @rlH @h @e @a (sndPair x))
  rawLog1p x = mkPair (rawLog1p @rlH @h @e @a (fstPair x)) (rawLog1p @rlH @h @e @a (sndPair x))
  rawExpm1 x = mkPair (rawExpm1 @rlH @h @e @a (fstPair x)) (rawExpm1 @rlH @h @e @a (sndPair x))
  rawFma x y z =
    mkPair
      (rawFma @rlH @h @e @a (fstPair x) (fstPair y) (fstPair z))
      (rawFma @rlH @h @e @a (sndPair x) (sndPair y) (sndPair z))
  rawHypot x y = mkPair (rawHypot @rlH @h @e @a (fstPair x) (fstPair y)) (rawHypot @rlH @h @e @a (sndPair x) (sndPair y))

-- Inductive Numeric Conversions Across Arbitrary Raw Layouts
class SPMDRawCvt (la :: Layout) (lb :: Layout) (n :: Nat) e a b where
  rawCvt :: e (SPMDRepByLayout la n e a) -> e (SPMDRepByLayout lb n e b)

instance
  (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b, ExpSIMDCast e) =>
  SPMDRawCvt 'LDirect 'LDirect n e a b
  where
  rawCvt v = case eqT @a @b of
    Just Refl -> v
    Nothing -> vcvt @e @n @a @b v

instance
  ( rlHa ~ SPMDRawLayout h a,
    rlHb ~ SPMDRawLayout h b,
    SPMDRawCvt rlHa rlHb h e a b,
    ExpPair e,
    JanusTyped e (SPMDRepByLayout rlHa h e a),
    JanusTyped e (SPMDRepByLayout rlHb h e b)
  ) =>
  SPMDRawCvt ('LSplit h) ('LSplit h) n e a b
  where
  rawCvt p = mkPair (rawCvt @rlHa @rlHb @h @e @a @b (fstPair p)) (rawCvt @rlHa @rlHb @h @e @a @b (sndPair p))

instance
  ( rlHb ~ SPMDRawLayout h b,
    ExpSIMDSplit e n h a,
    SPMDRawCvt 'LDirect rlHb h e a b,
    ExpPair e,
    JanusTyped e (SPMDRepByLayout rlHb h e b)
  ) =>
  SPMDRawCvt 'LDirect ('LSplit h) n e a b
  where
  rawCvt v =
    let (lo, hi) = vsplit @e @n @h v
     in mkPair (rawCvt @'LDirect @rlHb @h @e @a @b lo) (rawCvt @'LDirect @rlHb @h @e @a @b hi)

instance
  ( rlHa ~ SPMDRawLayout h a,
    ExpSIMDSplit e n h b,
    SPMDRawCvt rlHa 'LDirect h e a b,
    ExpPair e,
    JanusTyped e (SPMDRepByLayout rlHa h e a)
  ) =>
  SPMDRawCvt ('LSplit h) 'LDirect n e a b
  where
  rawCvt p =
    vconcat @e @n @h
      (rawCvt @rlHa @'LDirect @h @e @a @b (fstPair p))
      (rawCvt @rlHa @'LDirect @h @e @a @b (sndPair p))

instance
  (SIMDShape 4 a, SIMDShape 4 b, CPUSIMDElem a, CPUSIMDElem b, ExpSIMDCast e) =>
  SPMDRawCvt 'LPad4 'LPad4 2 e a b
  where
  rawCvt v = case eqT @a @b of
    Just Refl -> v
    Nothing -> vcvt @e @4 @a @b v

instance
  (SIMDShape 4 a, SIMDShape 4 b, SIMDShape 2 b, CPUSIMDElem a, CPUSIMDElem b, ExpSIMDCast e, ExpSIMDSplit e 4 2 b) =>
  SPMDRawCvt 'LPad4 'LDirect 2 e a b
  where
  rawCvt v = fst (vsplit @e @4 @2 (vcvt @e @4 @a @b v))

instance
  (SIMDShape 2 a, SIMDShape 4 a, SIMDShape 4 b, CPUSIMDElem a, CPUSIMDElem b, ExpSIMDCast e, ExpSIMDVec e 2 a, ExpSIMDSplit e 4 2 a, Num (e a)) =>
  SPMDRawCvt 'LDirect 'LPad4 2 e a b
  where
  rawCvt v = vcvt @e @4 @a @b (vconcat @e @4 @2 v vzero)

-- Raw Horizontal Reductions
class (SPMDRawVec rl n e a) => SPMDRawReduce (rl :: Layout) (n :: Nat) e a where
  rawReduceAdd :: e (SPMDRepByLayout rl n e a) -> e a
  rawReduceMin :: e (SPMDRepByLayout rl n e a) -> e a
  rawReduceMax :: e (SPMDRepByLayout rl n e a) -> e a

instance (SPMDRawVec 'LDirect n e a, ExpSIMDReduce e n a) => SPMDRawReduce 'LDirect n e a where
  rawReduceAdd = vreduceAdd
  rawReduceMin = vreduceMin
  rawReduceMax = vreduceMax

instance
  ( rlH ~ SPMDRawLayout h a,
    SPMDRawVec ('LSplit h) n e a,
    SPMDRawReduce rlH h e a,
    ExpPair e,
    Num (e a),
    ExpOrd e a,
    ExpCond e,
    JanusTyped e a
  ) =>
  SPMDRawReduce ('LSplit h) n e a
  where
  rawReduceAdd p = rawReduceAdd @rlH @h @e @a (fstPair p) + rawReduceAdd @rlH @h @e @a (sndPair p)
  rawReduceMin p = min' (rawReduceMin @rlH @h @e @a (fstPair p)) (rawReduceMin @rlH @h @e @a (sndPair p))
  rawReduceMax p = max' (rawReduceMax @rlH @h @e @a (fstPair p)) (rawReduceMax @rlH @h @e @a (sndPair p))

instance
  ( SPMDRawVec 'LPad4 2 e a,
    ExpSIMDVec e 4 a,
    Num (e a),
    ExpOrd e a,
    ExpCond e,
    JanusTyped e a
  ) =>
  SPMDRawReduce 'LPad4 2 e a
  where
  rawReduceAdd v = extractLaneAt 0 v + extractLaneAt 1 v
  rawReduceMin v = min' (extractLaneAt 0 v) (extractLaneAt 1 v)
  rawReduceMax v = max' (extractLaneAt 0 v) (extractLaneAt 1 v)
