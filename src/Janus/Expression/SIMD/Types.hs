{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Janus.Expression.SIMD.Types
  ( -- * Unpinned Byte Buffer for Unlifted FFI
    ByteBuf (..),
    allocByteBuf,
    allocByteBufPure,
    byteBufSize,
    peekByteBufBytes,
    pokeByteBufBytes,

    -- * Scalar Lane Element Metadata
    SIMDElem (..),
    JANUS_ELEM_TAG,

    -- * Validated (n, a) SIMD Shapes
    SIMDShape (..),

    -- * Raw Storage Types
    RawSIMD (..),
    RawMask (..),
    rawSIMDFromList,
    rawSIMDToList,
    rawSIMDIndex,
    rawSIMDUpdate,
    rawMaskFromBools,
    rawMaskToBools,
    rawMaskIndex,

    -- * Multi-Register Wide Unrolling (ekmett/native wide<V, K>)
    Wide (..),
    wideReplicate,
    wideGenerate,
    wideFromList,
    wideToList,
    wideIndex,
    wideTreeReduce,
  )
where

import Data.Bits (Bits (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.Exts
  ( ByteArray#,
    Int (..),
    MutableByteArray#,
    RealWorld,
    indexWord8Array#,
    newByteArray#,
    sizeofByteArray#,
    unsafeFreezeByteArray#,
    writeWord8Array#,
  )
import GHC.IO (IO (..), unsafeDupablePerformIO)
import GHC.TypeLits (KnownNat, Nat, natVal)
import GHC.Word (Word8 (..))

--------------------------------------------------------------------------------
-- Unpinned ByteArray# Wrapper
--------------------------------------------------------------------------------

data ByteBuf = ByteBuf ByteArray#

byteBufSize :: ByteBuf -> Int
byteBufSize (ByteBuf ba#) = I# (sizeofByteArray# ba#)

byteBufIndexWord8 :: ByteBuf -> Int -> Word8
byteBufIndexWord8 (ByteBuf ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

byteBufToWord8s :: ByteBuf -> [Word8]
byteBufToWord8s bb = [byteBufIndexWord8 bb i | i <- [0 .. byteBufSize bb - 1]]

byteBufFromWord8s :: [Word8] -> ByteBuf
byteBufFromWord8s ws =
  let !n@(I# n#) = length ws
   in unsafeDupablePerformIO $ do
        mba <- IO $ \s0 ->
          case newByteArray# n# s0 of
            (# s1, mba# #) -> (# s1, MutableBuf mba# #)
        let go _ [] = pure ()
            go (I# i#) (W8# w# : rest) = do
              IO $ \s -> (# writeWord8Array# (unMutableBuf mba) i# w# s, () #)
              go (I# i# + 1) rest
        go (0 :: Int) ws
        let _ = n
        IO $ \s ->
          case unsafeFreezeByteArray# (unMutableBuf mba) s of
            (# s', ba# #) -> (# s', ByteBuf ba# #)

data MutableBuf = MutableBuf {unMutableBuf :: MutableByteArray# RealWorld}

allocByteBuf :: Int -> (MutableByteArray# RealWorld -> IO ()) -> IO ByteBuf
allocByteBuf (I# n#) fill = do
  MutableBuf mba# <- IO $ \s0 ->
    case newByteArray# n# s0 of
      (# s1, m# #) -> (# s1, MutableBuf m# #)
  fill mba#
  IO $ \s2 ->
    case unsafeFreezeByteArray# mba# s2 of
      (# s3, ba# #) -> (# s3, ByteBuf ba# #)

allocByteBufPure :: Int -> (MutableByteArray# RealWorld -> IO ()) -> ByteBuf
allocByteBufPure n fill = unsafeDupablePerformIO (allocByteBuf n fill)

peekByteBufBytes :: Int -> Ptr a -> IO ByteBuf
peekByteBufBytes n ptr = do
  let p8 = castPtr ptr :: Ptr Word8
  ws <- mapM (peekElemOff p8) [0 .. n - 1]
  pure (byteBufFromWord8s ws)

pokeByteBufBytes :: Int -> Ptr a -> ByteBuf -> IO ()
pokeByteBufBytes n ptr bb = do
  let p8 = castPtr ptr :: Ptr Word8
  mapM_ (\i -> pokeElemOff p8 i (byteBufIndexWord8 bb i)) [0 .. n - 1]

instance Eq ByteBuf where
  a == b = byteBufSize a == byteBufSize b && byteBufToWord8s a == byteBufToWord8s b

instance Ord ByteBuf where
  compare a b = compare (byteBufToWord8s a) (byteBufToWord8s b)

--------------------------------------------------------------------------------
-- Scalar Lane Element Metadata
--------------------------------------------------------------------------------

type JANUS_ELEM_TAG = Int

class (Storable a, Eq a, Show a, Typeable a) => SIMDElem a where
  simdElemTag :: Proxy a -> JANUS_ELEM_TAG
  simdElemBytes :: Proxy a -> Int
  simdElemSigned :: Proxy a -> Bool
  simdElemIsFloat :: Proxy a -> Bool
  simdElemCTypeName :: Proxy a -> String

instance SIMDElem Int8 where
  simdElemTag _ = 0
  simdElemBytes _ = 1
  simdElemSigned _ = True
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "int8_t"

instance SIMDElem Word8 where
  simdElemTag _ = 1
  simdElemBytes _ = 1
  simdElemSigned _ = False
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "uint8_t"

instance SIMDElem Int16 where
  simdElemTag _ = 2
  simdElemBytes _ = 2
  simdElemSigned _ = True
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "int16_t"

instance SIMDElem Word16 where
  simdElemTag _ = 3
  simdElemBytes _ = 2
  simdElemSigned _ = False
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "uint16_t"

instance SIMDElem Int32 where
  simdElemTag _ = 4
  simdElemBytes _ = 4
  simdElemSigned _ = True
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "int32_t"

instance SIMDElem Word32 where
  simdElemTag _ = 5
  simdElemBytes _ = 4
  simdElemSigned _ = False
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "uint32_t"

instance SIMDElem Int64 where
  simdElemTag _ = 6
  simdElemBytes _ = 8
  simdElemSigned _ = True
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "int64_t"

instance SIMDElem Word64 where
  simdElemTag _ = 7
  simdElemBytes _ = 8
  simdElemSigned _ = False
  simdElemIsFloat _ = False
  simdElemCTypeName _ = "uint64_t"

instance SIMDElem Float where
  simdElemTag _ = 8
  simdElemBytes _ = 4
  simdElemSigned _ = True
  simdElemIsFloat _ = True
  simdElemCTypeName _ = "float"

instance SIMDElem Double where
  simdElemTag _ = 9
  simdElemBytes _ = 8
  simdElemSigned _ = True
  simdElemIsFloat _ = True
  simdElemCTypeName _ = "double"

--------------------------------------------------------------------------------
-- Validated (n, a) Shapes (32-bit, 64-bit, 128-bit, 256-bit)
--------------------------------------------------------------------------------

class (KnownNat n, SIMDElem a, Typeable n) => SIMDShape (n :: Nat) (a :: K.Type) where
  simdLanes :: Proxy n -> Proxy a -> Int
  simdLanes pn _ = fromIntegral (natVal pn)

  simdBytes :: Proxy n -> Proxy a -> Int
  simdBytes pn pa = simdLanes pn pa * simdElemBytes pa

-- 32-bit (4 bytes)
instance SIMDShape 4 Int8
instance SIMDShape 4 Word8
instance SIMDShape 2 Int16
instance SIMDShape 2 Word16

-- 64-bit (8 bytes)
instance SIMDShape 8 Int8
instance SIMDShape 8 Word8
instance SIMDShape 4 Int16
instance SIMDShape 4 Word16
instance SIMDShape 2 Int32
instance SIMDShape 2 Word32
instance SIMDShape 2 Float

-- 128-bit (16 bytes)
instance SIMDShape 16 Int8
instance SIMDShape 16 Word8
instance SIMDShape 8 Int16
instance SIMDShape 8 Word16
instance SIMDShape 4 Int32
instance SIMDShape 4 Word32
instance SIMDShape 4 Float
instance SIMDShape 2 Int64
instance SIMDShape 2 Word64
instance SIMDShape 2 Double

-- 256-bit (32 bytes)
instance SIMDShape 32 Int8
instance SIMDShape 32 Word8
instance SIMDShape 16 Int16
instance SIMDShape 16 Word16
instance SIMDShape 8 Int32
instance SIMDShape 8 Word32
instance SIMDShape 8 Float
instance SIMDShape 4 Int64
instance SIMDShape 4 Word64
instance SIMDShape 4 Double

--------------------------------------------------------------------------------
-- RawSIMD & RawMask Storage
--------------------------------------------------------------------------------

newtype RawSIMD (n :: Nat) a = RawSIMD {unRawSIMD :: ByteBuf}
  deriving stock (Eq, Ord)

newtype RawMask (n :: Nat) a = RawMask {unRawMask :: ByteBuf}
  deriving stock (Eq, Ord)

instance (SIMDShape n a) => Show (RawSIMD n a) where
  showsPrec d v =
    showParen (d > 10) $
      showString "SIMDVector " . shows (rawSIMDToList v)

instance (SIMDShape n a) => Show (RawMask n a) where
  showsPrec d m =
    showParen (d > 10) $
      showString "SIMDMask " . shows (rawMaskToBools m)

instance (SIMDShape n a) => Storable (RawSIMD n a) where
  sizeOf _ = simdBytes (Proxy @n) (Proxy @a)
  alignment _ = min 16 (simdBytes (Proxy @n) (Proxy @a))
  peek p = RawSIMD <$> peekByteBufBytes (simdBytes (Proxy @n) (Proxy @a)) p
  poke p (RawSIMD bb) = pokeByteBufBytes (simdBytes (Proxy @n) (Proxy @a)) p bb

instance (SIMDShape n a) => Storable (RawMask n a) where
  sizeOf _ = simdBytes (Proxy @n) (Proxy @a)
  alignment _ = min 16 (simdBytes (Proxy @n) (Proxy @a))
  peek p = RawMask <$> peekByteBufBytes (simdBytes (Proxy @n) (Proxy @a)) p
  poke p (RawMask bb) = pokeByteBufBytes (simdBytes (Proxy @n) (Proxy @a)) p bb

rawSIMDFromList :: forall n a. (SIMDShape n a) => [a] -> RawSIMD n a
rawSIMDFromList xs =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
   in if length xs /= lanes
        then error $ "rawSIMDFromList: expected " <> show lanes <> " lanes, got " <> show (length xs)
        else unsafeDupablePerformIO $
          allocaBytes bytes $ \(p :: Ptr a) -> do
            mapM_ (uncurry (pokeElemOff p)) (zip [0 ..] xs)
            RawSIMD <$> peekByteBufBytes bytes p

rawSIMDToList :: forall n a. (SIMDShape n a) => RawSIMD n a -> [a]
rawSIMDToList (RawSIMD bb) =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
   in unsafeDupablePerformIO $
        allocaBytes bytes $ \(p :: Ptr a) -> do
          pokeByteBufBytes bytes p bb
          mapM (peekElemOff p) [0 .. lanes - 1]

rawSIMDIndex :: forall n a. (SIMDShape n a) => Int -> RawSIMD n a -> a
rawSIMDIndex idx (RawSIMD bb) =
  let sz = simdElemBytes (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
   in unsafeDupablePerformIO $
        allocaBytes bytes $ \(p :: Ptr Word8) -> do
          pokeByteBufBytes bytes p bb
          peek (castPtr (p `plusPtr` (idx * sz)) :: Ptr a)

rawSIMDUpdate :: forall n a. (SIMDShape n a) => Int -> a -> RawSIMD n a -> RawSIMD n a
rawSIMDUpdate idx val v =
  let xs = rawSIMDToList v
      xs' = take idx xs <> [val] <> drop (idx + 1) xs
   in rawSIMDFromList xs'

rawMaskFromBools :: forall n a. (SIMDShape n a) => [Bool] -> RawMask n a
rawMaskFromBools bs =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      sz = simdElemBytes (Proxy @a)
   in if length bs /= lanes
        then error $ "rawMaskFromBools: expected " <> show lanes <> " lanes, got " <> show (length bs)
        else
          let ws = concatMap (\b -> replicate sz (if b then 0xFF else 0x00)) bs
           in RawMask (byteBufFromWord8s ws)

rawMaskToBools :: forall n a. (SIMDShape n a) => RawMask n a -> [Bool]
rawMaskToBools (RawMask bb) =
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      sz = simdElemBytes (Proxy @a)
   in [testBit (byteBufIndexWord8 bb (i * sz + (sz - 1))) 7 | i <- [0 .. lanes - 1]]

rawMaskIndex :: forall n a. (SIMDShape n a) => Int -> RawMask n a -> Bool
rawMaskIndex idx (RawMask bb) =
  let sz = simdElemBytes (Proxy @a)
   in testBit (byteBufIndexWord8 bb (idx * sz + (sz - 1))) 7

--------------------------------------------------------------------------------
-- Wide (ekmett/native wide<V, K>)
--------------------------------------------------------------------------------

-- | Fixed-multiplicity tuple of @k@ SIMD values used to express unrolled,
-- independent register chains without spilling.
newtype Wide (k :: Nat) v = Wide {unWide :: V.Vector v}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

wideReplicate :: forall k v. (KnownNat k) => v -> Wide k v
wideReplicate x = Wide (V.replicate (fromIntegral (natVal (Proxy @k))) x)

wideGenerate :: forall k v. (KnownNat k) => (Int -> v) -> Wide k v
wideGenerate f = Wide (V.generate (fromIntegral (natVal (Proxy @k))) f)

wideFromList :: forall k v. (KnownNat k) => [v] -> Wide k v
wideFromList xs =
  let k = fromIntegral (natVal (Proxy @k))
   in if length xs /= k
        then error $ "wideFromList: expected " <> show k <> " elements, got " <> show (length xs)
        else Wide (V.fromList xs)

wideToList :: Wide k v -> [v]
wideToList = V.toList . unWide

wideIndex :: Int -> Wide k v -> v
wideIndex i (Wide v) = v V.! i

-- | Balanced binary tree reduction across the @k@ independent registers of a
-- 'Wide' group, minimizing critical-path dependency depth.
wideTreeReduce :: (v -> v -> v) -> Wide k v -> v
wideTreeReduce f (Wide vec)
  | V.null vec = error "wideTreeReduce: empty Wide (k must be >= 1)"
  | otherwise = go (V.toList vec)
  where
    go [] = error "wideTreeReduce: impossible"
    go [x] = x
    go xs = go (pairUp xs)
    pairUp [] = []
    pairUp [x] = [x]
    pairUp (x : y : rest) = f x y : pairUp rest

instance (KnownNat k) => Applicative (Wide k) where
  pure = wideReplicate
  Wide fs <*> Wide xs = Wide (V.zipWith ($) fs xs)
