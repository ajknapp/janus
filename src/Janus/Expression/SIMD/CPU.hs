{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Janus.Expression.SIMD.CPU
  ( -- * Pure CPU-Emulated Vector & Mask Types
    CPUSIMD (..),
    CPUMask (..),
    cpuSIMDToRaw,
    cpuSIMDFromRaw,
    cpuMaskToRaw,
    cpuMaskFromRaw,

    -- * Per-Element Pure CPU Semantics
    CPUSIMDElem (..),

    -- * Pure CPU Vector Operations
    cpuSplat,
    cpuZero,
    cpuIota,
    cpuFromLanes,
    cpuExtractLane,
    cpuInsertLane,
    cpuMaskFalse,
    cpuMaskTrue,
    cpuMaskAnd,
    cpuMaskOr,
    cpuMaskXor,
    cpuMaskAndNot,
    cpuMaskNot,
    cpuSelect,
    cpuMovemask,
    cpuMaskPopcount,
    cpuMaskFirstTrue,
    cpuAllOf,
    cpuAnyOf,
    cpuNoneOf,
    cpuEq,
    cpuNe,
    cpuLt,
    cpuLe,
    cpuGt,
    cpuGe,
    cpuAdd,
    cpuSub,
    cpuMul,
    cpuNeg,
    cpuAbs,
    cpuMin,
    cpuMax,
    cpuAdds,
    cpuSubs,
    cpuAbss,
    cpuAvg,
    cpuAbsDiff,
    cpuBitAnd,
    cpuBitOr,
    cpuBitXor,
    cpuBitAndNot,
    cpuBitNot,
    cpuShl,
    cpuShr,
    cpuShlVar,
    cpuShrVar,
    cpuPopcnt,
    cpuPermuteLanes,
    cpuShuffleBytes,
    cpuZipLo,
    cpuZipHi,
    cpuSplit,
    cpuConcat,
    cpuReduceAdd,
    cpuReduceMin,
    cpuReduceMax,
    cpuReduceAnd,
    cpuReduceOr,
    cpuReduceXor,
    cpuDp4aS,
    cpuDp4aU,
    cpuDp4aVecS,
    cpuDp4aVecU,
    cpuDp2aS,
    cpuDp2aU,
    cpuSadU8,
    cpuBitcast,
    cpuCvt,
    cpuPackSat,
    cpuDiv,
    cpuSqrt,
    cpuRsqrtApprox,
    cpuRcpApprox,
    cpuFma,
    cpuFms,
    cpuFnma,
    cpuFnms,
    cpuMulAdd,
    cpuFloor,
    cpuCeil,
    cpuTrunc,
    cpuRoundNearest,

    -- * MonadIO Memory Emulation (Safe for IO & DejaFu ConcIO)
    cpuLoad,
    cpuLoadAligned,
    cpuStore,
    cpuStoreAligned,
    cpuLoadMasked,
    cpuStoreMasked,
    cpuGather,
    cpuGatherMasked,
  )
where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (Bits (..), popCount)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, ptrToWordPtr)
import Foreign.Storable (Storable (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import GHC.IO (unsafeDupablePerformIO)
import GHC.TypeLits (Nat)
import Janus.Expression.Math (FMA (..), Hypot (..))
import Janus.Expression.SIMD.Types

--------------------------------------------------------------------------------
-- Pure CPU Vector & Mask Containers
--------------------------------------------------------------------------------

newtype CPUSIMD (n :: Nat) a = CPUSIMD {unCPUSIMD :: [a]}
  deriving stock (Eq, Ord, Show)

newtype CPUMask (n :: Nat) a = CPUMask {unCPUMask :: [Bool]}
  deriving stock (Eq, Ord, Show)

cpuSIMDToRaw :: (SIMDShape n a) => CPUSIMD n a -> RawSIMD n a
cpuSIMDToRaw (CPUSIMD xs) = rawSIMDFromList xs

cpuSIMDFromRaw :: (SIMDShape n a) => RawSIMD n a -> CPUSIMD n a
cpuSIMDFromRaw = CPUSIMD . rawSIMDToList

cpuMaskToRaw :: (SIMDShape n a) => CPUMask n a -> RawMask n a
cpuMaskToRaw (CPUMask bs) = rawMaskFromBools bs

cpuMaskFromRaw :: (SIMDShape n a) => RawMask n a -> CPUMask n a
cpuMaskFromRaw = CPUMask . rawMaskToBools

instance (SIMDShape n a) => Storable (CPUSIMD n a) where
  sizeOf _ = simdBytes (Proxy @n) (Proxy @a)
  alignment _ = min 32 (simdBytes (Proxy @n) (Proxy @a))
  peek p = cpuSIMDFromRaw <$> peek (castPtr p :: Ptr (RawSIMD n a))
  poke p v = poke (castPtr p :: Ptr (RawSIMD n a)) (cpuSIMDToRaw v)

instance (SIMDShape n a) => Storable (CPUMask n a) where
  sizeOf _ = simdBytes (Proxy @n) (Proxy @a)
  alignment _ = min 32 (simdBytes (Proxy @n) (Proxy @a))
  peek p = cpuMaskFromRaw <$> peek (castPtr p :: Ptr (RawMask n a))
  poke p m = poke (castPtr p :: Ptr (RawMask n a)) (cpuMaskToRaw m)

--------------------------------------------------------------------------------
-- Per-Element Pure CPU Semantics
--------------------------------------------------------------------------------

class (SIMDElem a, Ord a, Num a) => CPUSIMDElem a where
  cpuElemMin :: a -> a -> a
  cpuElemMin x y = if x < y then x else y

  cpuElemMax :: a -> a -> a
  cpuElemMax x y = if x > y then x else y

  cpuElemAdds :: a -> a -> a
  cpuElemSubs :: a -> a -> a
  cpuElemAbss :: a -> a
  cpuElemAvg :: a -> a -> a
  cpuElemAbsDiff :: a -> a -> a
  cpuElemToBytes :: a -> [Word8]
  cpuElemFromBytes :: [Word8] -> a
  cpuElemShl :: a -> Word64 -> a
  cpuElemShr :: a -> Word64 -> a
  cpuElemShiftCount :: a -> Word64
  cpuElemPopcnt :: a -> a
  cpuElemToRational :: a -> Rational
  cpuElemFromIntegerTrunc :: Integer -> a
  cpuElemFromDouble :: Double -> a

storableToBytes :: forall a. (Storable a) => a -> [Word8]
storableToBytes x =
  let sz = sizeOf (undefined :: a)
   in unsafeDupablePerformIO $
        alloca $ \(p :: Ptr a) -> do
          poke p x
          let p8 = castPtr p :: Ptr Word8
          mapM (peekElemOff p8) [0 .. sz - 1]

storableFromBytes :: forall a. (Storable a) => [Word8] -> a
storableFromBytes ws =
  let sz = sizeOf (undefined :: a)
   in unsafeDupablePerformIO $
        allocaBytes sz $ \(p8 :: Ptr Word8) -> do
          mapM_ (uncurry (pokeElemOff p8)) (zip [0 .. sz - 1] ws)
          peek (castPtr p8 :: Ptr a)

clampInteger :: Integer -> Integer -> Integer -> Integer
clampInteger lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

signedAdds :: forall a. (Integral a, Bounded a) => a -> a -> a
signedAdds x y =
  fromInteger $
    clampInteger (toInteger (minBound :: a)) (toInteger (maxBound :: a)) (toInteger x + toInteger y)

signedSubs :: forall a. (Integral a, Bounded a) => a -> a -> a
signedSubs x y =
  fromInteger $
    clampInteger (toInteger (minBound :: a)) (toInteger (maxBound :: a)) (toInteger x - toInteger y)

signedAbss :: forall a. (Integral a, Bounded a) => a -> a
signedAbss x
  | x == minBound = maxBound
  | x < 0 = negate x
  | otherwise = x

unsignedAdds :: forall a. (Integral a, Bounded a) => a -> a -> a
unsignedAdds x y =
  fromInteger $ min (toInteger (maxBound :: a)) (toInteger x + toInteger y)

unsignedSubs :: forall a. (Integral a) => a -> a -> a
unsignedSubs x y
  | x >= y = x - y
  | otherwise = 0

intAvg :: (Integral a) => a -> a -> a
intAvg x y = fromInteger ((toInteger x + toInteger y + 1) ` shiftR` 1)

intAbsDiff :: (Integral a) => a -> a -> a
intAbsDiff x y = fromInteger (abs (toInteger x - toInteger y))

instance CPUSIMDElem Int8 where
  cpuElemAdds = signedAdds
  cpuElemSubs = signedSubs
  cpuElemAbss = signedAbss
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 8 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 8 then (if x < 0 then -1 else 0) else shiftR x (fromIntegral c)
  cpuElemShiftCount x = fromIntegral (fromIntegral x :: Word8)
  cpuElemPopcnt x = fromIntegral (popCount (fromIntegral x :: Word8))
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Word8 where
  cpuElemAdds = unsignedAdds
  cpuElemSubs = unsignedSubs
  cpuElemAbss = id
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 8 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 8 then 0 else shiftR x (fromIntegral c)
  cpuElemShiftCount = fromIntegral
  cpuElemPopcnt = fromIntegral . popCount
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Int16 where
  cpuElemAdds = signedAdds
  cpuElemSubs = signedSubs
  cpuElemAbss = signedAbss
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 16 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 16 then (if x < 0 then -1 else 0) else shiftR x (fromIntegral c)
  cpuElemShiftCount x = fromIntegral (fromIntegral x :: Word16)
  cpuElemPopcnt x = fromIntegral (popCount (fromIntegral x :: Word16))
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Word16 where
  cpuElemAdds = unsignedAdds
  cpuElemSubs = unsignedSubs
  cpuElemAbss = id
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 16 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 16 then 0 else shiftR x (fromIntegral c)
  cpuElemShiftCount = fromIntegral
  cpuElemPopcnt = fromIntegral . popCount
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Int32 where
  cpuElemAdds = signedAdds
  cpuElemSubs = signedSubs
  cpuElemAbss = signedAbss
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 32 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 32 then (if x < 0 then -1 else 0) else shiftR x (fromIntegral c)
  cpuElemShiftCount x = fromIntegral (fromIntegral x :: Word32)
  cpuElemPopcnt x = fromIntegral (popCount (fromIntegral x :: Word32))
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Word32 where
  cpuElemAdds = unsignedAdds
  cpuElemSubs = unsignedSubs
  cpuElemAbss = id
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 32 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 32 then 0 else shiftR x (fromIntegral c)
  cpuElemShiftCount = fromIntegral
  cpuElemPopcnt = fromIntegral . popCount
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Int64 where
  cpuElemAdds = signedAdds
  cpuElemSubs = signedSubs
  cpuElemAbss = signedAbss
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 64 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 64 then (if x < 0 then -1 else 0) else shiftR x (fromIntegral c)
  cpuElemShiftCount x = fromIntegral x
  cpuElemPopcnt x = fromIntegral (popCount (fromIntegral x :: Word64))
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Word64 where
  cpuElemAdds = unsignedAdds
  cpuElemSubs = unsignedSubs
  cpuElemAbss = id
  cpuElemAvg = intAvg
  cpuElemAbsDiff = intAbsDiff
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = if c >= 64 then 0 else shiftL x (fromIntegral c)
  cpuElemShr x c = if c >= 64 then 0 else shiftR x (fromIntegral c)
  cpuElemShiftCount = id
  cpuElemPopcnt = fromIntegral . popCount
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble d = fromInteger (truncate d)

instance CPUSIMDElem Float where
  cpuElemMin x y = if x < y then x else y
  cpuElemMax x y = if x > y then x else y
  cpuElemAdds = (+)
  cpuElemSubs = (-)
  cpuElemAbss = abs
  cpuElemAvg x y = 0.5 * (x + y)
  cpuElemAbsDiff x y = abs (x - y)
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = castWord32ToFloat (if c >= 32 then 0 else shiftL (castFloatToWord32 x) (fromIntegral c))
  cpuElemShr x c = castWord32ToFloat (if c >= 32 then 0 else shiftR (castFloatToWord32 x) (fromIntegral c))
  cpuElemShiftCount x = fromIntegral (castFloatToWord32 x)
  cpuElemPopcnt x = fromIntegral (popCount (castFloatToWord32 x))
  cpuElemToRational = toRational
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble = realToFrac

instance CPUSIMDElem Double where
  cpuElemMin x y = if x < y then x else y
  cpuElemMax x y = if x > y then x else y
  cpuElemAdds = (+)
  cpuElemSubs = (-)
  cpuElemAbss = abs
  cpuElemAvg x y = 0.5 * (x + y)
  cpuElemAbsDiff x y = abs (x - y)
  cpuElemToRational = toRational
  cpuElemToBytes = storableToBytes
  cpuElemFromBytes = storableFromBytes
  cpuElemShl x c = castWord64ToDouble (if c >= 64 then 0 else shiftL (castDoubleToWord64 x) (fromIntegral c))
  cpuElemShr x c = castWord64ToDouble (if c >= 64 then 0 else shiftR (castDoubleToWord64 x) (fromIntegral c))
  cpuElemShiftCount = castDoubleToWord64
  cpuElemPopcnt x = fromIntegral (popCount (castDoubleToWord64 x))
  cpuElemFromIntegerTrunc = fromInteger
  cpuElemFromDouble = id

--------------------------------------------------------------------------------
-- Algebraic Typeclass Instances on CPUSIMD / CPUMask
--------------------------------------------------------------------------------

instance (SIMDShape n a, CPUSIMDElem a) => Num (CPUSIMD n a) where
  (+) = cpuAdd
  (-) = cpuSub
  (*) = cpuMul
  negate = cpuNeg
  abs = cpuAbs
  signum (CPUSIMD xs) = CPUSIMD (map signum xs)
  fromInteger k = cpuSplat (fromInteger k)

instance (SIMDShape n a, CPUSIMDElem a, Fractional a) => Fractional (CPUSIMD n a) where
  CPUSIMD xs / CPUSIMD ys = CPUSIMD (zipWith (/) xs ys)
  recip (CPUSIMD xs) = CPUSIMD (map recip xs)
  fromRational q = cpuSplat (fromRational q)

instance (SIMDShape n a, CPUSIMDElem a, Floating a) => Floating (CPUSIMD n a) where
  pi = cpuSplat pi
  exp (CPUSIMD xs) = CPUSIMD (map exp xs)
  log (CPUSIMD xs) = CPUSIMD (map log xs)
  sqrt (CPUSIMD xs) = CPUSIMD (map sqrt xs)
  CPUSIMD xs ** CPUSIMD ys = CPUSIMD (zipWith (**) xs ys)
  sin (CPUSIMD xs) = CPUSIMD (map sin xs)
  cos (CPUSIMD xs) = CPUSIMD (map cos xs)
  tan (CPUSIMD xs) = CPUSIMD (map tan xs)
  asin (CPUSIMD xs) = CPUSIMD (map asin xs)
  acos (CPUSIMD xs) = CPUSIMD (map acos xs)
  atan (CPUSIMD xs) = CPUSIMD (map atan xs)
  sinh (CPUSIMD xs) = CPUSIMD (map sinh xs)
  cosh (CPUSIMD xs) = CPUSIMD (map cosh xs)
  tanh (CPUSIMD xs) = CPUSIMD (map tanh xs)
  asinh (CPUSIMD xs) = CPUSIMD (map asinh xs)
  acosh (CPUSIMD xs) = CPUSIMD (map acosh xs)
  atanh (CPUSIMD xs) = CPUSIMD (map atanh xs)

instance (SIMDShape n a, CPUSIMDElem a, FMA a) => FMA (CPUSIMD n a) where
  fma (CPUSIMD as) (CPUSIMD bs) (CPUSIMD cs) = CPUSIMD (zipWith3 fma as bs cs)

instance (SIMDShape n a, CPUSIMDElem a, Floating a, Hypot a) => Hypot (CPUSIMD n a) where
  hypot (CPUSIMD as) (CPUSIMD bs) = CPUSIMD (zipWith hypot as bs)

instance (SIMDShape n a) => Join (CPUMask n a) where
  (\/) = cpuMaskOr

instance (SIMDShape n a) => Meet (CPUMask n a) where
  (/\) = cpuMaskAnd

instance (SIMDShape n a) => Lower (CPUMask n a) where
  lowerBound = cpuMaskFalse

instance (SIMDShape n a) => Upper (CPUMask n a) where
  upperBound = cpuMaskTrue

--------------------------------------------------------------------------------
-- Pure CPU Vector & Mask Operations
--------------------------------------------------------------------------------

cpuSplat :: forall n a. (SIMDShape n a) => a -> CPUSIMD n a
cpuSplat x = CPUSIMD (replicate (simdLanes (Proxy @n) (Proxy @a)) x)

cpuZero :: (SIMDShape n a, Num a) => CPUSIMD n a
cpuZero = cpuSplat 0

cpuIota :: forall n a. (SIMDShape n a, Num a) => CPUSIMD n a
cpuIota = CPUSIMD [fromIntegral i | i <- [0 .. simdLanes (Proxy @n) (Proxy @a) - 1]]

cpuFromLanes :: forall n a. (SIMDShape n a) => [a] -> CPUSIMD n a
cpuFromLanes xs =
  let n = simdLanes (Proxy @n) (Proxy @a)
   in if length xs /= n
        then error $ "cpuFromLanes: expected " <> show n <> " lanes, got " <> show (length xs)
        else CPUSIMD xs

cpuExtractLane :: forall n a. Int -> CPUSIMD n a -> a
cpuExtractLane i (CPUSIMD xs) = xs !! i

cpuInsertLane :: forall n a. Int -> a -> CPUSIMD n a -> CPUSIMD n a
cpuInsertLane i val (CPUSIMD xs) = CPUSIMD (take i xs <> [val] <> drop (i + 1) xs)

cpuMaskFalse :: forall n a. (SIMDShape n a) => CPUMask n a
cpuMaskFalse = CPUMask (replicate (simdLanes (Proxy @n) (Proxy @a)) False)

cpuMaskTrue :: forall n a. (SIMDShape n a) => CPUMask n a
cpuMaskTrue = CPUMask (replicate (simdLanes (Proxy @n) (Proxy @a)) True)

cpuMaskAnd, cpuMaskOr, cpuMaskXor, cpuMaskAndNot :: forall n a. CPUMask n a -> CPUMask n a -> CPUMask n a
cpuMaskAnd (CPUMask a) (CPUMask b) = CPUMask (zipWith (&&) a b)
cpuMaskOr (CPUMask a) (CPUMask b) = CPUMask (zipWith (||) a b)
cpuMaskXor (CPUMask a) (CPUMask b) = CPUMask (zipWith (/=) a b)
cpuMaskAndNot (CPUMask a) (CPUMask b) = CPUMask (zipWith (\x y -> not x && y) a b)

cpuMaskNot :: forall n a. CPUMask n a -> CPUMask n a
cpuMaskNot (CPUMask a) = CPUMask (map not a)

cpuSelect :: forall n a. CPUMask n a -> CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuSelect (CPUMask ms) (CPUSIMD ts) (CPUSIMD fs) =
  CPUSIMD (zipWith3 (\m t f -> if m then t else f) ms ts fs)

cpuMovemask :: forall n a. CPUMask n a -> Word32
cpuMovemask (CPUMask ms) =
  foldr (\(i, b) acc -> if b then setBit acc i else acc) 0 (zip [0 ..] ms)

cpuMaskPopcount :: forall n a. CPUMask n a -> Int32
cpuMaskPopcount (CPUMask ms) = fromIntegral (length (filter id ms))

cpuMaskFirstTrue :: forall n a. CPUMask n a -> Int32
cpuMaskFirstTrue (CPUMask ms) =
  case [fromIntegral i | (i, True) <- zip [0 :: Int ..] ms] of
    (k : _) -> k
    [] -> -1

cpuAllOf, cpuAnyOf, cpuNoneOf :: forall n a. CPUMask n a -> Bool
cpuAllOf (CPUMask ms) = and ms
cpuAnyOf (CPUMask ms) = or ms
cpuNoneOf (CPUMask ms) = not (or ms)

cpuEq, cpuNe, cpuLt, cpuLe, cpuGt, cpuGe :: forall n a. (Ord a) => CPUSIMD n a -> CPUSIMD n a -> CPUMask n a
cpuEq (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (==) a b)
cpuNe (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (/=) a b)
cpuLt (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (<) a b)
cpuLe (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (<=) a b)
cpuGt (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (>) a b)
cpuGe (CPUSIMD a) (CPUSIMD b) = CPUMask (zipWith (>=) a b)

cpuAdd, cpuSub, cpuMul, cpuMin, cpuMax :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuAdd (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith (+) a b)
cpuSub (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith (-) a b)
cpuMul (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith (*) a b)
cpuMin (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemMin a b)
cpuMax (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemMax a b)

cpuNeg, cpuAbs :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a
cpuNeg (CPUSIMD a) = CPUSIMD (map negate a)
cpuAbs (CPUSIMD a) = CPUSIMD (map abs a)

cpuAdds, cpuSubs, cpuAvg, cpuAbsDiff :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuAdds (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemAdds a b)
cpuSubs (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemSubs a b)
cpuAvg (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemAvg a b)
cpuAbsDiff (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith cpuElemAbsDiff a b)

cpuAbss :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a
cpuAbss (CPUSIMD a) = CPUSIMD (map cpuElemAbss a)

cpuBitwiseBin :: forall n a. (CPUSIMDElem a) => (Word8 -> Word8 -> Word8) -> CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuBitwiseBin f (CPUSIMD xs) (CPUSIMD ys) =
  CPUSIMD (zipWith (\x y -> cpuElemFromBytes (zipWith f (cpuElemToBytes x) (cpuElemToBytes y))) xs ys)

cpuBitAnd, cpuBitOr, cpuBitXor, cpuBitAndNot :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuBitAnd = cpuBitwiseBin (.&.)
cpuBitOr = cpuBitwiseBin (.|.)
cpuBitXor = cpuBitwiseBin xor
cpuBitAndNot = cpuBitwiseBin (\x y -> complement x .&. y)

cpuBitNot :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a
cpuBitNot (CPUSIMD xs) = CPUSIMD (map (cpuElemFromBytes . map complement . cpuElemToBytes) xs)

cpuShl, cpuShr :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> Int32 -> CPUSIMD n a
cpuShl (CPUSIMD xs) cnt =
  let ucnt = fromIntegral (fromIntegral cnt :: Word32)
   in CPUSIMD (map (`cpuElemShl` ucnt) xs)
cpuShr (CPUSIMD xs) cnt =
  let ucnt = fromIntegral (fromIntegral cnt :: Word32)
   in CPUSIMD (map (`cpuElemShr` ucnt) xs)

cpuShlVar, cpuShrVar :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuShlVar (CPUSIMD xs) (CPUSIMD cs) = CPUSIMD (zipWith (\x c -> cpuElemShl x (cpuElemShiftCount c)) xs cs)
cpuShrVar (CPUSIMD xs) (CPUSIMD cs) = CPUSIMD (zipWith (\x c -> cpuElemShr x (cpuElemShiftCount c)) xs cs)

cpuPopcnt :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> CPUSIMD n a
cpuPopcnt (CPUSIMD xs) = CPUSIMD (map cpuElemPopcnt xs)

cpuPermuteLanes :: forall n a. (SIMDShape n a) => CPUSIMD n a -> Either [Int] (CPUSIMD n Int32) -> CPUSIMD n a
cpuPermuteLanes (CPUSIMD xs) spec =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      mask = fromIntegral (lanes - 1) :: Word32
      rawIdxs = case spec of
        Left is -> map fromIntegral is
        Right (CPUSIMD is) -> map (fromIntegral :: Int32 -> Word32) is
   in CPUSIMD [xs !! fromIntegral (i .&. mask) | i <- rawIdxs]

cpuShuffleBytes :: forall n a b. (SIMDShape n a, CPUSIMDElem a, SIMDShape b Word8) => CPUSIMD n a -> CPUSIMD b Word8 -> CPUSIMD n a
cpuShuffleBytes (CPUSIMD xs) (CPUSIMD ctrls)
  | simdBytes (Proxy @n) (Proxy @a) /= simdBytes (Proxy @b) (Proxy @Word8) =
      error "cpuShuffleBytes: byte control vector width mismatch"
  | otherwise =
      let srcBytes = concatMap cpuElemToBytes xs
          total = length srcBytes
          mask = fromIntegral (total - 1) :: Word8
          sz = simdElemBytes (Proxy @a)
          outBytes = [if testBit c 7 then 0 else srcBytes !! fromIntegral (c .&. mask) | c <- ctrls]
          chunk [] = []
          chunk bs = take sz bs : chunk (drop sz bs)
       in CPUSIMD (map cpuElemFromBytes (chunk outBytes))

cpuZipLo, cpuZipHi :: forall n a. (SIMDShape n a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuZipLo (CPUSIMD xs) (CPUSIMD ys) =
  let half = simdLanes (Proxy @n) (Proxy @a) `div` 2
   in CPUSIMD (concat [[x, y] | (x, y) <- zip (take half xs) (take half ys)])
cpuZipHi (CPUSIMD xs) (CPUSIMD ys) =
  let half = simdLanes (Proxy @n) (Proxy @a) `div` 2
   in CPUSIMD (concat [[x, y] | (x, y) <- zip (drop half xs) (drop half ys)])

cpuSplit :: forall n2 n a. (SIMDShape n a) => CPUSIMD n2 a -> (CPUSIMD n a, CPUSIMD n a)
cpuSplit (CPUSIMD xs) =
  let half = simdLanes (Proxy @n) (Proxy @a)
   in (CPUSIMD (take half xs), CPUSIMD (drop half xs))

cpuConcat :: forall n n2 a. CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n2 a
cpuConcat (CPUSIMD lo) (CPUSIMD hi) = CPUSIMD (lo <> hi)

cpuReduceAdd, cpuReduceMin, cpuReduceMax, cpuReduceAnd, cpuReduceOr, cpuReduceXor :: forall n a. (CPUSIMDElem a) => CPUSIMD n a -> a
cpuReduceAdd (CPUSIMD xs) = foldl1 (+) xs
cpuReduceMin (CPUSIMD xs) = foldl1 cpuElemMin xs
cpuReduceMax (CPUSIMD xs) = foldl1 cpuElemMax xs
cpuReduceAnd (CPUSIMD xs) = foldl1 (\a b -> cpuElemFromBytes (zipWith (.&.) (cpuElemToBytes a) (cpuElemToBytes b))) xs
cpuReduceOr (CPUSIMD xs) = foldl1 (\a b -> cpuElemFromBytes (zipWith (.|.) (cpuElemToBytes a) (cpuElemToBytes b))) xs
cpuReduceXor (CPUSIMD xs) = foldl1 (\a b -> cpuElemFromBytes (zipWith xor (cpuElemToBytes a) (cpuElemToBytes b))) xs

cpuDp4aS :: CPUSIMD 4 Int8 -> CPUSIMD 4 Int8 -> Int32 -> Int32
cpuDp4aS (CPUSIMD as) (CPUSIMD bs) acc =
  acc + sum (zipWith (\a b -> fromIntegral a * fromIntegral b) as bs)

cpuDp4aU :: CPUSIMD 4 Word8 -> CPUSIMD 4 Word8 -> Word32 -> Word32
cpuDp4aU (CPUSIMD as) (CPUSIMD bs) acc =
  acc + sum (zipWith (\a b -> fromIntegral a * fromIntegral b) as bs)

cpuDp4aVecS :: forall m k. CPUSIMD m Int8 -> CPUSIMD m Int8 -> CPUSIMD k Int32 -> CPUSIMD k Int32
cpuDp4aVecS (CPUSIMD as) (CPUSIMD bs) (CPUSIMD accs) =
  let groups [] _ = []
      groups _ [] = []
      groups xs ys = zipWith (\a b -> fromIntegral a * fromIntegral b) (take 4 xs) (take 4 ys) : groups (drop 4 xs) (drop 4 ys)
   in CPUSIMD (zipWith (\ps c -> c + sum ps) (groups as bs) accs)

cpuDp4aVecU :: forall m k. CPUSIMD m Word8 -> CPUSIMD m Word8 -> CPUSIMD k Word32 -> CPUSIMD k Word32
cpuDp4aVecU (CPUSIMD as) (CPUSIMD bs) (CPUSIMD accs) =
  let groups [] _ = []
      groups _ [] = []
      groups xs ys = zipWith (\a b -> fromIntegral a * fromIntegral b) (take 4 xs) (take 4 ys) : groups (drop 4 xs) (drop 4 ys)
   in CPUSIMD (zipWith (\ps c -> c + sum ps) (groups as bs) accs)

cpuDp2aS :: Bool -> CPUSIMD 2 Int16 -> CPUSIMD 4 Int8 -> Int32 -> Int32
cpuDp2aS isHi (CPUSIMD [a0, a1]) (CPUSIMD bs) acc =
  case if isHi then drop 2 bs else take 2 bs of
    [b0, b1] -> acc + fromIntegral a0 * fromIntegral b0 + fromIntegral a1 * fromIntegral b1
    _ -> error "cpuDp2aS: invalid lane count"
cpuDp2aS _ _ _ _ = error "cpuDp2aS: invalid lane count"

cpuDp2aU :: Bool -> CPUSIMD 2 Word16 -> CPUSIMD 4 Word8 -> Word32 -> Word32
cpuDp2aU isHi (CPUSIMD [a0, a1]) (CPUSIMD bs) acc =
  case if isHi then drop 2 bs else take 2 bs of
    [b0, b1] -> acc + fromIntegral a0 * fromIntegral b0 + fromIntegral a1 * fromIntegral b1
    _ -> error "cpuDp2aU: invalid lane count"
cpuDp2aU _ _ _ _ = error "cpuDp2aU: invalid lane count"

cpuSadU8 :: forall n. CPUSIMD n Word8 -> CPUSIMD n Word8 -> Word32
cpuSadU8 (CPUSIMD as) (CPUSIMD bs) =
  sum [if a >= b then fromIntegral (a - b) else fromIntegral (b - a) | (a, b) <- zip as bs]

cpuBitcast :: forall n a m b. (SIMDShape n a, SIMDShape m b) => CPUSIMD n a -> CPUSIMD m b
cpuBitcast v
  | simdBytes (Proxy @n) (Proxy @a) == simdBytes (Proxy @m) (Proxy @b) =
      let RawSIMD bb = cpuSIMDToRaw v
       in cpuSIMDFromRaw (RawSIMD bb)
  | otherwise = error "cpuBitcast: mismatched byte widths"

cpuCvt :: forall n a b. (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) => CPUSIMD n a -> CPUSIMD n b
cpuCvt (CPUSIMD xs) = CPUSIMD (map cvtOne xs)
  where
    srcFloat = simdElemIsFloat (Proxy @a)
    dstFloat = simdElemIsFloat (Proxy @b)
    cvtOne x
      | dstFloat = cpuElemFromDouble (fromRational (cpuElemToRational x))
      | srcFloat = cpuElemFromIntegerTrunc (truncate (fromRational (cpuElemToRational x) :: Double))
      | otherwise = cpuElemFromIntegerTrunc (numeratorOfInt (cpuElemToRational x))
    numeratorOfInt r = floor r

cpuPackSat :: forall n m a b. (SIMDShape n a, SIMDShape m b, CPUSIMDElem a, CPUSIMDElem b) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD m b
cpuPackSat (CPUSIMD lo) (CPUSIMD hi) = CPUSIMD (map clampOne (lo <> hi))
  where
    dstBytes = simdElemBytes (Proxy @b)
    dstBits = dstBytes * 8
    dstSigned = simdElemSigned (Proxy @b)
    loBound = if dstSigned then -(1 `shiftL` (dstBits - 1)) else 0
    hiBound = if dstSigned then (1 `shiftL` (dstBits - 1)) - 1 else (1 `shiftL` dstBits) - 1
    clampOne x =
      let iv = floor (cpuElemToRational x) :: Integer
       in cpuElemFromIntegerTrunc (clampInteger loBound hiBound iv)

cpuDiv :: forall n a. (Fractional a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuDiv (CPUSIMD a) (CPUSIMD b) = CPUSIMD (zipWith (/) a b)

cpuSqrt, cpuRsqrtApprox, cpuRcpApprox :: forall n a. (Floating a) => CPUSIMD n a -> CPUSIMD n a
cpuSqrt (CPUSIMD xs) = CPUSIMD (map sqrt xs)
cpuRsqrtApprox (CPUSIMD xs) = CPUSIMD (map (\x -> 1 / sqrt x) xs)
cpuRcpApprox (CPUSIMD xs) = CPUSIMD (map recip xs)

cpuFma, cpuFms, cpuFnma, cpuFnms, cpuMulAdd :: forall n a. (FMA a, Num a) => CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a -> CPUSIMD n a
cpuFma (CPUSIMD a) (CPUSIMD b) (CPUSIMD c) = CPUSIMD (zipWith3 fma a b c)
cpuFms (CPUSIMD a) (CPUSIMD b) (CPUSIMD c) = CPUSIMD (zipWith3 (\x y z -> fma x y (negate z)) a b c)
cpuFnma (CPUSIMD a) (CPUSIMD b) (CPUSIMD c) = CPUSIMD (zipWith3 (\x y z -> fma (negate x) y z) a b c)
cpuFnms (CPUSIMD a) (CPUSIMD b) (CPUSIMD c) = CPUSIMD (zipWith3 (\x y z -> fma (negate x) y (negate z)) a b c)
cpuMulAdd (CPUSIMD a) (CPUSIMD b) (CPUSIMD c) = CPUSIMD (zipWith3 fma a b c)

roundTiesToEven :: (RealFloat a) => a -> a
roundTiesToEven x
  | isNaN x || isInfinite x = x
  | abs x >= 4503599627370496 = x
  | otherwise =
      let r = fromInteger (round x :: Integer)
       in if r == 0 && x < 0 then -0.0 else r

floorSafe, ceilSafe, truncSafe :: (RealFloat a) => a -> a
floorSafe x
  | isNaN x || isInfinite x || abs x >= 4503599627370496 = x
  | otherwise = fromInteger (floor x :: Integer)
ceilSafe x
  | isNaN x || isInfinite x || abs x >= 4503599627370496 = x
  | otherwise = fromInteger (ceiling x :: Integer)
truncSafe x
  | isNaN x || isInfinite x || abs x >= 4503599627370496 = x
  | otherwise = fromInteger (truncate x :: Integer)

cpuFloor, cpuCeil, cpuTrunc, cpuRoundNearest :: forall n a. (RealFloat a) => CPUSIMD n a -> CPUSIMD n a
cpuFloor (CPUSIMD xs) = CPUSIMD (map floorSafe xs)
cpuCeil (CPUSIMD xs) = CPUSIMD (map ceilSafe xs)
cpuTrunc (CPUSIMD xs) = CPUSIMD (map truncSafe xs)
cpuRoundNearest (CPUSIMD xs) = CPUSIMD (map roundTiesToEven xs)

--------------------------------------------------------------------------------
-- MonadIO Memory Emulation
--------------------------------------------------------------------------------

cpuLoad, cpuLoadAligned :: forall m n a. (MonadIO m, SIMDShape n a) => Ptr a -> m (CPUSIMD n a)
cpuLoad p =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
   in CPUSIMD <$> mapM (liftIO . peekElemOff p) [0 .. lanes - 1]
cpuLoadAligned p = do
  let reqAlign = min 16 (simdBytes (Proxy @n) (Proxy @a))
      addr = fromIntegral (ptrToWordPtr p) :: Int
  when (addr `mod` reqAlign /= 0) $
    liftIO $ ioError $ userError $ "cpuLoadAligned: pointer " <> show p <> " is not " <> show reqAlign <> "-byte aligned"
  cpuLoad p

cpuStore, cpuStoreAligned :: forall m n a. (MonadIO m, SIMDShape n a) => Ptr a -> CPUSIMD n a -> m ()
cpuStore p (CPUSIMD xs) =
  forM_ (zip [0 ..] xs) (\(i, x) -> liftIO (pokeElemOff p i x))
cpuStoreAligned p v = do
  let reqAlign = min 16 (simdBytes (Proxy @n) (Proxy @a))
      addr = fromIntegral (ptrToWordPtr p) :: Int
  when (addr `mod` reqAlign /= 0) $
    liftIO $ ioError $ userError $ "cpuStoreAligned: pointer " <> show p <> " is not " <> show reqAlign <> "-byte aligned"
  cpuStore p v

cpuLoadMasked :: forall m n a. (MonadIO m, SIMDShape n a) => CPUMask n a -> CPUSIMD n a -> Ptr a -> m (CPUSIMD n a)
cpuLoadMasked (CPUMask ms) (CPUSIMD fbs) p =
  fmap CPUSIMD $
    forM (zip3 [0 ..] ms fbs) $ \(i, active, fb) ->
      if active then liftIO (peekElemOff p i) else pure fb

cpuStoreMasked :: forall m n a. (MonadIO m, SIMDShape n a) => CPUMask n a -> Ptr a -> CPUSIMD n a -> m ()
cpuStoreMasked (CPUMask ms) p (CPUSIMD xs) =
  forM_ (zip3 [0 ..] ms xs) $ \(i, active, x) ->
    when active $ liftIO (pokeElemOff p i x)

cpuGather :: forall m n a. (MonadIO m, SIMDShape n a) => Ptr a -> CPUSIMD n Int32 -> m (CPUSIMD n a)
cpuGather p (CPUSIMD idxs) =
  CPUSIMD <$> mapM (liftIO . peekElemOff p . fromIntegral) idxs

cpuGatherMasked :: forall m n a. (MonadIO m, SIMDShape n a) => CPUMask n a -> CPUSIMD n a -> Ptr a -> CPUSIMD n Int32 -> m (CPUSIMD n a)
cpuGatherMasked (CPUMask ms) (CPUSIMD fbs) p (CPUSIMD idxs) =
  fmap CPUSIMD $
    forM (zip3 ms fbs idxs) $ \(active, fb, idx) ->
      if active then liftIO (peekElemOff p (fromIntegral idx)) else pure fb
