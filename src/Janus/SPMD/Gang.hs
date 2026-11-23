{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -O0 #-}

module Janus.SPMD.Gang where

import Control.Monad (when)
import Data.Complex (Complex)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C.Target
  ( CUDAFeatures,
    X86Feature (..),
    X86Features,
    getActiveHostX86Features,
    hasX86Feature,
  )
import qualified Janus.Command.Array as A
import Janus.Command.Case (CmdCase)
import Janus.Command.Cond (CmdCond, whenM_)
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile)
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cast (ExpBoolCast, ExpFloatingCast, ExpIntegralCast, ExpPtrCast)
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq)
import Janus.Expression.Inject (ExpInject)
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet)
import Janus.Expression.Math (ExpComplex, FMA, Hypot)
import Janus.Expression.MurmurHash (ExpMurmurHash)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD (ExpSIMDMask, SIMDElem (..))
import Janus.SPMD.Cast ()
import Janus.SPMD.Command ()
import Janus.SPMD.Expression ()
import Janus.SPMD.Lane
  ( SPMDLanesByLayout,
    SPMDReduceByLayout,
    spmdAlignedUnitStride,
    spmdUnitStride,
    uniform,
  )
import Janus.SPMD.Typed (SPMDTyped)
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 15. Optimal Non-Splitting Gang Size Selection & Strip-Mining Loops
--------------------------------------------------------------------------------

data GangSize (n :: Nat) where
  Gang2 :: GangSize 2
  Gang4 :: GangSize 4
  Gang8 :: GangSize 8
  Gang16 :: GangSize 16
  Gang32 :: GangSize 32

-- | Static non-splitting gang width given target hardware vector register byte
-- size (@16@ for SSE2 / CUDA @float4@, @32@ for AVX / AVX2) and main element type @a@.
type family MaxSingleRegGang (regBytes :: Nat) (a :: K.Type) :: Nat where
  MaxSingleRegGang 32 Int8 = 32
  MaxSingleRegGang 32 Word8 = 32
  MaxSingleRegGang 32 Int16 = 16
  MaxSingleRegGang 32 Word16 = 16
  MaxSingleRegGang 32 Int32 = 8
  MaxSingleRegGang 32 Word32 = 8
  MaxSingleRegGang 32 Float = 8
  MaxSingleRegGang 32 Int64 = 4
  MaxSingleRegGang 32 Word64 = 4
  MaxSingleRegGang 32 Double = 4
  MaxSingleRegGang 16 Int8 = 16
  MaxSingleRegGang 16 Word8 = 16
  MaxSingleRegGang 16 Int16 = 8
  MaxSingleRegGang 16 Word16 = 8
  MaxSingleRegGang 16 Int32 = 4
  MaxSingleRegGang 16 Word32 = 4
  MaxSingleRegGang 16 Float = 4
  MaxSingleRegGang 16 Int64 = 2
  MaxSingleRegGang 16 Word64 = 2
  MaxSingleRegGang 16 Double = 2

clampGangWidth :: Int -> Int
clampGangWidth k
  | k >= 32 = 32
  | k >= 16 = 16
  | k >= 8 = 8
  | k >= 4 = 4
  | otherwise = 2

-- | Calculate the widest x86-64 gang size for a main type @a@ that fits in a
-- single hardware SIMD register without struct splitting (accounting for SSE2
-- 16B, AVX1 32B-float / 16B-integer, and AVX2 32B-all).
optimalX86GangSize :: forall a. (SIMDElem a) => X86Features -> Proxy a -> Int
optimalX86GangSize fs pa =
  let has256 =
        if simdElemIsFloat pa
          then hasX86Feature fs X86_AVX
          else hasX86Feature fs X86_AVX2
      regBytes = if has256 then 32 else 16
   in clampGangWidth (regBytes `div` simdElemBytes pa)

-- | Calculate the widest CUDA per-thread SIMD gang size for a main type @a@
-- that fits in a single @float4@ / @double2@ / @int4@ / @uint4@ 16-byte vector
-- without struct splitting.
optimalCUDAGangSize :: forall a. (SIMDElem a) => CUDAFeatures -> Proxy a -> Int
optimalCUDAGangSize _ pa =
  clampGangWidth (16 `div` simdElemBytes pa)

type SPMDElemCtx n m e a =
  ( SPMDTyped n e a,
    Num (SPMD n e a),
    ExpEq (SPMD n e) a,
    ExpOrd (SPMD n e) a,
    ExpBoolCast (SPMD n e) a,
    ExpInject (SPMD n e) a,
    A.ExpSized (SPMD n e) a,
    A.ExpPtr (SPMD n e) a,
    A.CmdStorable (SPMDM n m e) (SPMD n e) a,
    SPMDLanesByLayout (SPMDLayout n a) n e a,
    SPMDReduceByLayout (SPMDLayout n a) n e a,
    PtrTreeOps n e (Ptr a)
  )

type SPMDIntElemCtx n m e a =
  ( SPMDElemCtx n m e a,
    Bounded (SPMD n e a),
    ExpBits (SPMD n e) a,
    ExpIntegral (SPMD n e) a,
    ExpIntegralCast (SPMD n e) a Int64,
    ExpIntegralCast (SPMD n e) Int64 a,
    ExpIntegralCast (SPMD n e) a Int32,
    ExpIntegralCast (SPMD n e) Int32 a,
    ExpFloatingCast (SPMD n e) a Float,
    ExpFloatingCast (SPMD n e) a Double
  )

type SPMDFloatElemCtx n m e a =
  ( SPMDElemCtx n m e a,
    Fractional (SPMD n e a),
    Floating (SPMD n e a),
    FMA (SPMD n e a),
    Hypot (SPMD n e a),
    ExpIntegralCast (SPMD n e) a Int64,
    ExpIntegralCast (SPMD n e) a Int32,
    ExpFloatingCast (SPMD n e) a Float,
    ExpFloatingCast (SPMD n e) a Double,
    SPMDTyped n e (Complex a),
    Num (SPMD n e (Complex a)),
    Fractional (SPMD n e (Complex a)),
    Floating (SPMD n e (Complex a)),
    ExpEq (SPMD n e) (Complex a),
    ExpComplex (SPMD n e) a
  )

-- | Rich bundled constraint providing all SPMD expression and command
-- instances at gang width @n@ across all standard Janus scalar types.
class
  ( KnownNat n,
    Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdRange m e,
    ExpBool e,
    ExpCond e,
    ExpLet e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool,
    ExpSIMDMask e n (SPMDMaskElem n),
    JanusTyped (SPMD n e) Bool,
    JanusTyped (SPMD n e) (),
    JanusTyped (SPMD n e) (Ptr ()),
    ExpBool (SPMD n e),
    ExpCond (SPMD n e),
    ExpLet (SPMD n e),
    ExpPtrCast (SPMD n e),
    CmdRef (SPMDM n m e) (SPMD n e),
    CmdCond (SPMDM n m e) (SPMD n e),
    CmdWhile (SPMDM n m e) (SPMD n e),
    CmdRange (SPMDM n m e) (SPMD n e),
    CmdCase (SPMDM n m e) (SPMD n e),
    A.CmdMem (SPMDM n m e) (SPMD n e),
    SPMDIntElemCtx n m e Int8,
    SPMDIntElemCtx n m e Word8,
    SPMDIntElemCtx n m e Int16,
    SPMDIntElemCtx n m e Word16,
    SPMDIntElemCtx n m e Int32,
    SPMDIntElemCtx n m e Word32,
    SPMDIntElemCtx n m e Int64,
    SPMDIntElemCtx n m e Word64,
    SPMDFloatElemCtx n m e Float,
    SPMDFloatElemCtx n m e Double,
    ExpMurmurHash (SPMD n e) (SPMD n e Int32),
    ExpMurmurHash (SPMD n e) (SPMD n e Int64),
    ExpMurmurHash (SPMD n e) (SPMD n e Word32),
    ExpMurmurHash (SPMD n e) (SPMD n e Word64),
    ExpMurmurHash (SPMD n e) (SPMD n e Float),
    ExpMurmurHash (SPMD n e) (SPMD n e Double)
  ) =>
  SPMDGang (n :: Nat) m e

instance
  ( KnownNat n,
    Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdRange m e,
    ExpBool e,
    ExpCond e,
    ExpLet e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool,
    ExpSIMDMask e n (SPMDMaskElem n),
    JanusTyped (SPMD n e) Bool,
    JanusTyped (SPMD n e) (),
    JanusTyped (SPMD n e) (Ptr ()),
    ExpBool (SPMD n e),
    ExpCond (SPMD n e),
    ExpLet (SPMD n e),
    ExpPtrCast (SPMD n e),
    CmdRef (SPMDM n m e) (SPMD n e),
    CmdCond (SPMDM n m e) (SPMD n e),
    CmdWhile (SPMDM n m e) (SPMD n e),
    CmdRange (SPMDM n m e) (SPMD n e),
    CmdCase (SPMDM n m e) (SPMD n e),
    A.CmdMem (SPMDM n m e) (SPMD n e),
    SPMDIntElemCtx n m e Int8,
    SPMDIntElemCtx n m e Word8,
    SPMDIntElemCtx n m e Int16,
    SPMDIntElemCtx n m e Word16,
    SPMDIntElemCtx n m e Int32,
    SPMDIntElemCtx n m e Word32,
    SPMDIntElemCtx n m e Int64,
    SPMDIntElemCtx n m e Word64,
    SPMDFloatElemCtx n m e Float,
    SPMDFloatElemCtx n m e Double,
    ExpMurmurHash (SPMD n e) (SPMD n e Int32),
    ExpMurmurHash (SPMD n e) (SPMD n e Int64),
    ExpMurmurHash (SPMD n e) (SPMD n e Word32),
    ExpMurmurHash (SPMD n e) (SPMD n e Word64),
    ExpMurmurHash (SPMD n e) (SPMD n e Float),
    ExpMurmurHash (SPMD n e) (SPMD n e Double)
  ) =>
  SPMDGang n m e

dispatchGangInt ::
  forall m e r.
  (SPMDGang 2 m e, SPMDGang 4 m e, SPMDGang 8 m e, SPMDGang 16 m e, SPMDGang 32 m e) =>
  Int ->
  (forall n. (SPMDGang n m e) => Proxy n -> m r) ->
  m r
dispatchGangInt w k = case clampGangWidth w of
  2 -> k (Proxy @2)
  4 -> k (Proxy @4)
  8 -> k (Proxy @8)
  16 -> k (Proxy @16)
  _ -> k (Proxy @32)

-- | Transition from an unlifted @(m, e)@ program to an SPMD subprogram at the
-- optimal non-splitting gang width for a main element type @a@.
class (Monad m) => CmdOptimalGang m e | m -> e, e -> m where
  withOptimalGang ::
    (SIMDElem a) =>
    Proxy a ->
    (forall n. (SPMDGang n m e) => Proxy n -> m r) ->
    m r

instance CmdOptimalGang IO Identity where
  withOptimalGang pa k = do
    fs <- getActiveHostX86Features
    dispatchGangInt (optimalX86GangSize fs pa) k

-- Strip-Mining Combinators Over Unlifted Ranges & Tensors
foreachImpl ::
  forall n m e.
  (SPMDGang n m e) =>
  Bool ->
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachImpl knownAligned p l u body = do
  let w = fromIntegral (natVal p) :: Int64
      wE = fromIntegral w :: e Int64
  total <- letM (ifThenElse (u `gt` l) (u - l) 0)
  numFull <- letM (total `quotient` wE)
  alignedEnd <- letM (l + numFull * wE)
  rangeM 0 numFull $ \g -> do
    base <- letM (l + g * wE)
    let idx = if knownAligned then spmdAlignedUnitStride base else spmdUnitStride base
    withSPMD p (body idx)
  whenM_ (alignedEnd `lt` u) $ do
    let tailIdx = if knownAligned then spmdAlignedUnitStride alignedEnd else spmdUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform u) (body tailIdx)

-- | Strip-mine @[0, u)@ into an aligned unmasked main SIMD loop plus a masked tail.
foreach0M ::
  forall n m e.
  (SPMDGang n m e) =>
  Proxy n ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreach0M p u = foreachImpl True p 0 u

-- | Strip-mine @[l, u)@ where @l@ is known to be a multiple of @n@.
foreachAlignedM ::
  forall n m e.
  (SPMDGang n m e) =>
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachAlignedM = foreachImpl True

-- | Strip-mine a general @[l, u)@ range into unmasked SIMD gangs plus a masked tail.
foreachM ::
  forall n m e.
  (SPMDGang n m e) =>
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachM = foreachImpl False

-- | Strip-mine a 1D 'A.Vector' @[0, d)@ with static tail elimination when @d % n == 0@.
foreachTensorM ::
  forall d n m e a.
  (KnownNat d, SPMDGang n m e) =>
  Proxy n ->
  A.Vector d e a ->
  (A.TensorIndex '[d] (SPMD n e) -> SPMDM n m e ()) ->
  m ()
foreachTensorM p _ body = do
  let total = fromIntegral (natVal (Proxy @d)) :: Int64
      w = fromIntegral (natVal p) :: Int64
      wE = fromIntegral w :: e Int64
      fullGangs = total `quot` w
      remElems = total `rem` w
  when (fullGangs > 0) $
    rangeM 0 (fromIntegral fullGangs) $ \g -> do
      base <- letM (g * wE)
      withSPMD p (body (spmdAlignedUnitStride base A.:. A.Z))
  when (remElems > 0) $ do
    let tailStart = fullGangs * w
        tailIdx = SPMD (pure (I64UnitStride True (fromIntegral tailStart)))
    withSPMDMasked (tailIdx `lt` fromIntegral total) (body (tailIdx A.:. A.Z))

foldForeach0M ::
  forall n m e s.
  (SPMDGang n m e, SPMDTyped n e s) =>
  Proxy n ->
  e Int64 ->
  SPMD n e s ->
  (SPMD n e s -> SPMD n e Int64 -> SPMDM n m e (SPMD n e s)) ->
  m (SPMD n e s)
foldForeach0M p u s0 step = do
  accRef <- withSPMD p (newRef s0)
  foreach0M p u $ \i -> do
    cur <- readRef accRef
    nxt <- step cur i
    writeRef accRef nxt
  withSPMD p (readRef accRef)

foldForeachTensorM ::
  forall d n m e a s.
  (KnownNat d, SPMDGang n m e, SPMDTyped n e s) =>
  Proxy n ->
  A.Vector d e a ->
  SPMD n e s ->
  (SPMD n e s -> A.TensorIndex '[d] (SPMD n e) -> SPMDM n m e (SPMD n e s)) ->
  m (SPMD n e s)
foldForeachTensorM p vec s0 step = do
  accRef <- withSPMD p (newRef s0)
  foreachTensorM p vec $ \idx -> do
    cur <- readRef accRef
    nxt <- step cur idx
    writeRef accRef nxt
  withSPMD p (readRef accRef)

foreachOptimal0M ::
  (CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  e Int64 ->
  (forall n. (SPMDGang n m e) => SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachOptimal0M pm u body =
  withOptimalGang pm $ \pn -> foreach0M pn u body

foreachOptimalM ::
  (CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  e Int64 ->
  e Int64 ->
  (forall n. (SPMDGang n m e) => SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachOptimalM pm l u body =
  withOptimalGang pm $ \pn -> foreachM pn l u body

foreachOptimalTensorM ::
  (KnownNat d, CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  A.Vector d e a ->
  (forall n. (SPMDGang n m e) => A.TensorIndex '[d] (SPMD n e) -> SPMDM n m e ()) ->
  m ()
foreachOptimalTensorM pm vec body =
  withOptimalGang pm $ \pn -> foreachTensorM pn vec body

foldForeachOptimal0M ::
  (CmdOptimalGang m e, SIMDElem mainTy, forall n. (SPMDGang n m e) => SPMDTyped n e s) =>
  Proxy mainTy ->
  e Int64 ->
  (forall n. (SPMDGang n m e) => SPMD n e s) ->
  (forall n. (SPMDGang n m e) => SPMD n e s -> SPMD n e Int64 -> SPMDM n m e (SPMD n e s)) ->
  (forall n. (SPMDGang n m e) => SPMD n e s -> e s) ->
  m (e s)
foldForeachOptimal0M pm u s0 step reduceFn =
  withOptimalGang pm $ \pn ->
    reduceFn <$> foldForeach0M pn u s0 step

foldForeachOptimalTensorM ::
  (KnownNat d, CmdOptimalGang m e, SIMDElem mainTy, forall n. (SPMDGang n m e) => SPMDTyped n e s) =>
  Proxy mainTy ->
  A.Vector d e a ->
  (forall n. (SPMDGang n m e) => SPMD n e s) ->
  (forall n. (SPMDGang n m e) => SPMD n e s -> A.TensorIndex '[d] (SPMD n e) -> SPMDM n m e (SPMD n e s)) ->
  (forall n. (SPMDGang n m e) => SPMD n e s -> e s) ->
  m (e s)
foldForeachOptimalTensorM pm vec s0 step reduceFn =
  withOptimalGang pm $ \pn ->
    reduceFn <$> foldForeachTensorM pn vec s0 step
