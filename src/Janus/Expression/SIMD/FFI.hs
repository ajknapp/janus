{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Janus.Expression.SIMD.FFI
  ( -- * Opcode Constants
    opAdd,
    opSub,
    opMul,
    opDiv,
    opMin,
    opMax,
    opAdds,
    opSubs,
    opAvg,
    opAbsDiff,
    opAnd,
    opOr,
    opXor,
    opAndNot,
    opShlVar,
    opShrVar,
    opZipLo,
    opZipHi,
    opPow,
    opHypot,
    unopNeg,
    unopAbs,
    unopAbss,
    unopNot,
    unopPopcnt,
    unopSqrt,
    unopRsqrt,
    unopRcp,
    unopFloor,
    unopCeil,
    unopTrunc,
    unopRound,
    unopSignum,
    unopExp,
    unopLog,
    unopSin,
    unopCos,
    unopTan,
    unopAsin,
    unopAcos,
    unopAtan,
    unopSinh,
    unopCosh,
    unopTanh,
    unopAsinh,
    unopAcosh,
    unopAtanh,
    unopExpm1,
    unopLog1p,
    ternopSelect,
    ternopFma,
    ternopFms,
    ternopFnma,
    ternopFnms,
    ternopMulAdd,
    cmpEq,
    cmpNe,
    cmpLt,
    cmpLe,
    cmpGt,
    cmpGe,
    redAdd,
    redMin,
    redMax,
    redAnd,
    redOr,
    redXor,

    -- * High-Level FFI Wrappers over RawSIMD / RawMask
    ffiSplat,
    ffiZero,
    ffiIota,
    ffiBinOp,
    ffiUnOp,
    ffiTernOp,
    ffiSelect,
    ffiCmp,
    ffiShift,
    ffiMaskAnd,
    ffiMaskOr,
    ffiMaskXor,
    ffiMaskAndNot,
    ffiMaskNot,
    ffiMovemask,
    ffiMaskPopcount,
    ffiMaskFirstTrue,
    ffiPermuteLanes,
    ffiShuffleBytes,
    ffiReduce,
    ffiDp4aS,
    ffiDp4aU,
    ffiDp4aVecS,
    ffiDp4aVecU,
    ffiDp2aS,
    ffiDp2aU,
    ffiSadU8,
    ffiCvt,
    ffiPackSat,
    ffiBitcast,
    ffiSplit,
    ffiConcat,
    ffiLoad,
    ffiLoadAligned,
    ffiStore,
    ffiStoreAligned,
    ffiLoadMasked,
    ffiStoreMasked,
    ffiGather,
    ffiGatherMasked,
  )
where

import Data.Int (Int16, Int32, Int8)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (peek, poke))
import GHC.Exts (ByteArray#, MutableByteArray#, RealWorld, copyByteArray#)
import GHC.Exts (Int (..))
import GHC.IO (IO (..), unsafeDupablePerformIO)
import Janus.CBits ()
import Janus.Expression.SIMD.Types

opAdd, opSub, opMul, opDiv, opMin, opMax, opAdds, opSubs, opAvg, opAbsDiff :: CInt
opAdd = 0
opSub = 1
opMul = 2
opDiv = 3
opMin = 4
opMax = 5
opAdds = 6
opSubs = 7
opAvg = 8
opAbsDiff = 9

opAnd, opOr, opXor, opAndNot, opShlVar, opShrVar, opZipLo, opZipHi, opPow, opHypot :: CInt
opAnd = 10
opOr = 11
opXor = 12
opAndNot = 13
opShlVar = 14
opShrVar = 15
opZipLo = 16
opZipHi = 17
opPow = 18
opHypot = 19

unopNeg, unopAbs, unopAbss, unopNot, unopPopcnt, unopSqrt, unopRsqrt, unopRcp :: CInt
unopNeg = 0
unopAbs = 1
unopAbss = 2
unopNot = 3
unopPopcnt = 4
unopSqrt = 5
unopRsqrt = 6
unopRcp = 7

unopFloor, unopCeil, unopTrunc, unopRound, unopSignum :: CInt
unopFloor = 8
unopCeil = 9
unopTrunc = 10
unopRound = 11
unopSignum = 12

unopExp, unopLog, unopSin, unopCos, unopTan, unopAsin, unopAcos, unopAtan :: CInt
unopExp = 13
unopLog = 14
unopSin = 15
unopCos = 16
unopTan = 17
unopAsin = 18
unopAcos = 19
unopAtan = 20

unopSinh, unopCosh, unopTanh, unopAsinh, unopAcosh, unopAtanh, unopExpm1, unopLog1p :: CInt
unopSinh = 21
unopCosh = 22
unopTanh = 23
unopAsinh = 24
unopAcosh = 25
unopAtanh = 26
unopExpm1 = 27
unopLog1p = 28

ternopSelect, ternopFma, ternopFms, ternopFnma, ternopFnms, ternopMulAdd :: CInt
ternopSelect = 0
ternopFma = 1
ternopFms = 2
ternopFnma = 3
ternopFnms = 4
ternopMulAdd = 5

cmpEq, cmpNe, cmpLt, cmpLe, cmpGt, cmpGe :: CInt
cmpEq = 0
cmpNe = 1
cmpLt = 2
cmpLe = 3
cmpGt = 4
cmpGe = 5

redAdd, redMin, redMax, redAnd, redOr, redXor :: CInt
redAdd = 0
redMin = 1
redMax = 2
redAnd = 3
redOr = 4
redXor = 5

--------------------------------------------------------------------------------
-- Raw C FFI Imports (UnliftedFFITypes)
--------------------------------------------------------------------------------

foreign import ccall unsafe "janus_simd_splat"
  c_splat :: CInt -> CInt -> MutableByteArray# RealWorld -> Ptr () -> IO ()

foreign import ccall unsafe "janus_simd_iota"
  c_iota :: CInt -> CInt -> MutableByteArray# RealWorld -> IO ()

foreign import ccall unsafe "janus_simd_binop"
  c_binop :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_unop"
  c_unop :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_ternop"
  c_ternop :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_cmp"
  c_cmp :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_shift"
  c_shift :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> Int32 -> IO ()

foreign import ccall unsafe "janus_simd_movemask"
  c_movemask :: CInt -> CInt -> ByteArray# -> IO Word32

foreign import ccall unsafe "janus_simd_mask_popcount"
  c_mask_popcount :: CInt -> CInt -> ByteArray# -> IO Int32

foreign import ccall unsafe "janus_simd_mask_first_true"
  c_mask_first_true :: CInt -> CInt -> ByteArray# -> IO Int32

foreign import ccall unsafe "janus_simd_permute_lanes"
  c_permute_lanes :: CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_permute_lanes"
  c_permute_lanes_ptr :: CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> Ptr Int32 -> IO ()

foreign import ccall unsafe "janus_simd_shuffle_bytes"
  c_shuffle_bytes :: CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_reduce"
  c_reduce :: CInt -> CInt -> CInt -> Ptr () -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_dp4a_s"
  c_dp4a_s :: ByteArray# -> ByteArray# -> Int32 -> IO Int32

foreign import ccall unsafe "janus_simd_dp4a_u"
  c_dp4a_u :: ByteArray# -> ByteArray# -> Word32 -> IO Word32

foreign import ccall unsafe "janus_simd_dp4a_vec_s"
  c_dp4a_vec_s :: CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_dp4a_vec_u"
  c_dp4a_vec_u :: CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_dp2a_s"
  c_dp2a_s :: CInt -> ByteArray# -> ByteArray# -> Int32 -> IO Int32

foreign import ccall unsafe "janus_simd_dp2a_u"
  c_dp2a_u :: CInt -> ByteArray# -> ByteArray# -> Word32 -> IO Word32

foreign import ccall unsafe "janus_simd_sad_u8"
  c_sad_u8 :: CInt -> ByteArray# -> ByteArray# -> IO Word32

foreign import ccall unsafe "janus_simd_cvt"
  c_cvt :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_pack_sat"
  c_pack_sat :: CInt -> CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_load"
  c_load :: CInt -> MutableByteArray# RealWorld -> Ptr () -> IO ()

foreign import ccall unsafe "janus_simd_load_aligned"
  c_load_aligned :: CInt -> MutableByteArray# RealWorld -> Ptr () -> IO ()

foreign import ccall unsafe "janus_simd_store"
  c_store :: CInt -> Ptr () -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_store_aligned"
  c_store_aligned :: CInt -> Ptr () -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_load_masked"
  c_load_masked :: CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> Ptr () -> IO ()

foreign import ccall unsafe "janus_simd_store_masked"
  c_store_masked :: CInt -> CInt -> Ptr () -> ByteArray# -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_gather"
  c_gather :: CInt -> CInt -> MutableByteArray# RealWorld -> Ptr () -> ByteArray# -> IO ()

foreign import ccall unsafe "janus_simd_gather_masked"
  c_gather_masked :: CInt -> CInt -> MutableByteArray# RealWorld -> ByteArray# -> ByteArray# -> Ptr () -> ByteArray# -> IO ()

--------------------------------------------------------------------------------
-- High-Level Wrappers
--------------------------------------------------------------------------------

elemC :: forall a. (SIMDElem a) => CInt
elemC = fromIntegral (fromEnum (simdElemTag (Proxy @a)))

lanesC :: forall n a. (SIMDShape n a) => CInt
lanesC = fromIntegral (simdLanes (Proxy @n) (Proxy @a))

bytesInt :: forall n a. (SIMDShape n a) => Int
bytesInt = simdBytes (Proxy @n) (Proxy @a)

ffiSplat :: forall n a. (SIMDShape n a) => a -> RawSIMD n a
ffiSplat x =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      alloca $ \(p :: Ptr a) -> do
        poke p x
        c_splat (elemC @a) (lanesC @n @a) dst# (castPtr p)

ffiZero :: forall n a. (SIMDShape n a, Num a) => RawSIMD n a
ffiZero = ffiSplat (0 :: a)

ffiIota :: forall n a. (SIMDShape n a) => RawSIMD n a
ffiIota =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_iota (elemC @a) (lanesC @n @a) dst#

ffiBinOp :: forall n a. (SIMDShape n a) => CInt -> RawSIMD n a -> RawSIMD n a -> RawSIMD n a
ffiBinOp op (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_binop op (elemC @a) (lanesC @n @a) dst# a# b#

ffiUnOp :: forall n a. (SIMDShape n a) => CInt -> RawSIMD n a -> RawSIMD n a
ffiUnOp op (RawSIMD (ByteBuf a#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_unop op (elemC @a) (lanesC @n @a) dst# a#

ffiTernOp :: forall n a. (SIMDShape n a) => CInt -> RawSIMD n a -> RawSIMD n a -> RawSIMD n a -> RawSIMD n a
ffiTernOp op (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) (RawSIMD (ByteBuf c#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_ternop op (elemC @a) (lanesC @n @a) dst# a# b# c#

ffiSelect :: forall n a. (SIMDShape n a) => RawMask n a -> RawSIMD n a -> RawSIMD n a -> RawSIMD n a
ffiSelect (RawMask (ByteBuf m#)) (RawSIMD (ByteBuf t#)) (RawSIMD (ByteBuf f#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_ternop ternopSelect (elemC @a) (lanesC @n @a) dst# m# t# f#

ffiCmp :: forall n a. (SIMDShape n a) => CInt -> RawSIMD n a -> RawSIMD n a -> RawMask n a
ffiCmp op (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) =
  RawMask $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_cmp op (elemC @a) (lanesC @n @a) dst# a# b#

ffiShift :: forall n a. (SIMDShape n a) => Bool -> RawSIMD n a -> Int32 -> RawSIMD n a
ffiShift isRight (RawSIMD (ByteBuf a#)) cnt =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_shift (if isRight then 1 else 0) (elemC @a) (lanesC @n @a) dst# a# cnt

ffiMaskBin :: forall n a. (SIMDShape n a) => CInt -> RawMask n a -> RawMask n a -> RawMask n a
ffiMaskBin op (RawMask (ByteBuf a#)) (RawMask (ByteBuf b#)) =
  RawMask $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_binop op (elemC @a) (lanesC @n @a) dst# a# b#

ffiMaskAnd, ffiMaskOr, ffiMaskXor, ffiMaskAndNot :: (SIMDShape n a) => RawMask n a -> RawMask n a -> RawMask n a
ffiMaskAnd = ffiMaskBin opAnd
ffiMaskOr = ffiMaskBin opOr
ffiMaskXor = ffiMaskBin opXor
ffiMaskAndNot = ffiMaskBin opAndNot

ffiMaskNot :: forall n a. (SIMDShape n a) => RawMask n a -> RawMask n a
ffiMaskNot (RawMask (ByteBuf a#)) =
  RawMask $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_unop unopNot (elemC @a) (lanesC @n @a) dst# a#

ffiMovemask :: forall n a. (SIMDShape n a) => RawMask n a -> Word32
ffiMovemask (RawMask (ByteBuf m#)) =
  unsafeDupablePerformIO $ c_movemask (elemC @a) (lanesC @n @a) m#

ffiMaskPopcount :: forall n a. (SIMDShape n a) => RawMask n a -> Int32
ffiMaskPopcount (RawMask (ByteBuf m#)) =
  unsafeDupablePerformIO $ c_mask_popcount (elemC @a) (lanesC @n @a) m#

ffiMaskFirstTrue :: forall n a. (SIMDShape n a) => RawMask n a -> Int32
ffiMaskFirstTrue (RawMask (ByteBuf m#)) =
  unsafeDupablePerformIO $ c_mask_first_true (elemC @a) (lanesC @n @a) m#

ffiPermuteLanes :: forall n a. (SIMDShape n a) => RawSIMD n a -> Either [Int] (RawSIMD n Int32) -> RawSIMD n a
ffiPermuteLanes (RawSIMD (ByteBuf s#)) (Right (RawSIMD (ByteBuf idx#))) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      c_permute_lanes (elemC @a) (lanesC @n @a) dst# s# idx#
ffiPermuteLanes (RawSIMD (ByteBuf s#)) (Left idxs) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @a) $ \dst# ->
      withArray (map fromIntegral idxs) $ \pIdx ->
        c_permute_lanes_ptr (elemC @a) (lanesC @n @a) dst# s# pIdx

ffiShuffleBytes :: forall n a b. (SIMDShape n a, SIMDShape b Word8) => RawSIMD n a -> RawSIMD b Word8 -> RawSIMD n a
ffiShuffleBytes (RawSIMD (ByteBuf s#)) (RawSIMD (ByteBuf ctrl#))
  | simdBytes (Proxy @n) (Proxy @a) /= simdBytes (Proxy @b) (Proxy @Word8) =
      error "ffiShuffleBytes: byte control vector width mismatch"
  | otherwise =
      let b = bytesInt @n @a
       in RawSIMD $
            allocByteBufPure b $ \dst# ->
              c_shuffle_bytes (fromIntegral b) dst# s# ctrl#

ffiReduce :: forall n a. (SIMDShape n a) => CInt -> RawSIMD n a -> a
ffiReduce op (RawSIMD (ByteBuf s#)) =
  unsafeDupablePerformIO $
    alloca $ \(p :: Ptr a) -> do
      c_reduce op (elemC @a) (lanesC @n @a) (castPtr p) s#
      peek p

ffiDp4aS :: RawSIMD 4 Int8 -> RawSIMD 4 Int8 -> Int32 -> Int32
ffiDp4aS (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) acc =
  unsafeDupablePerformIO $ c_dp4a_s a# b# acc

ffiDp4aU :: RawSIMD 4 Word8 -> RawSIMD 4 Word8 -> Word32 -> Word32
ffiDp4aU (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) acc =
  unsafeDupablePerformIO $ c_dp4a_u a# b# acc

ffiDp4aVecS :: forall k m. (SIMDShape m Int8, SIMDShape k Int32) => RawSIMD m Int8 -> RawSIMD m Int8 -> RawSIMD k Int32 -> RawSIMD k Int32
ffiDp4aVecS (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) (RawSIMD (ByteBuf acc#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @k @Int32) $ \dst# ->
      c_dp4a_vec_s (lanesC @k @Int32) dst# a# b# acc#

ffiDp4aVecU :: forall k m. (SIMDShape m Word8, SIMDShape k Word32) => RawSIMD m Word8 -> RawSIMD m Word8 -> RawSIMD k Word32 -> RawSIMD k Word32
ffiDp4aVecU (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) (RawSIMD (ByteBuf acc#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @k @Word32) $ \dst# ->
      c_dp4a_vec_u (lanesC @k @Word32) dst# a# b# acc#

ffiDp2aS :: Bool -> RawSIMD 2 Int16 -> RawSIMD 4 Int8 -> Int32 -> Int32
ffiDp2aS isHi (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) acc =
  unsafeDupablePerformIO $ c_dp2a_s (if isHi then 1 else 0) a# b# acc

ffiDp2aU :: Bool -> RawSIMD 2 Word16 -> RawSIMD 4 Word8 -> Word32 -> Word32
ffiDp2aU isHi (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) acc =
  unsafeDupablePerformIO $ c_dp2a_u (if isHi then 1 else 0) a# b# acc

ffiSadU8 :: forall n. (SIMDShape n Word8) => RawSIMD n Word8 -> RawSIMD n Word8 -> Word32
ffiSadU8 (RawSIMD (ByteBuf a#)) (RawSIMD (ByteBuf b#)) =
  unsafeDupablePerformIO $ c_sad_u8 (lanesC @n @Word8) a# b#

ffiCvt :: forall n a b. (SIMDShape n a, SIMDShape n b) => RawSIMD n a -> RawSIMD n b
ffiCvt (RawSIMD (ByteBuf s#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @n @b) $ \dst# ->
      c_cvt (elemC @a) (elemC @b) (lanesC @n @a) dst# s#

ffiPackSat :: forall n m a b. (SIMDShape n a, SIMDShape m b) => RawSIMD n a -> RawSIMD n a -> RawSIMD m b
ffiPackSat (RawSIMD (ByteBuf lo#)) (RawSIMD (ByteBuf hi#)) =
  RawSIMD $
    allocByteBufPure (bytesInt @m @b) $ \dst# ->
      c_pack_sat (elemC @a) (elemC @b) (lanesC @n @a) dst# lo# hi#

ffiBitcast :: forall n a m b. (SIMDShape n a, SIMDShape m b) => RawSIMD n a -> RawSIMD m b
ffiBitcast (RawSIMD bb)
  | simdBytes (Proxy @n) (Proxy @a) == simdBytes (Proxy @m) (Proxy @b) = RawSIMD bb
  | otherwise = error "ffiBitcast: mismatched byte widths"

ffiSplit :: forall n2 n a. (SIMDShape n2 a, SIMDShape n a) => RawSIMD n2 a -> (RawSIMD n a, RawSIMD n a)
ffiSplit (RawSIMD (ByteBuf src#)) =
  let !half@(I# half#) = bytesInt @n @a
      lo = allocByteBufPure half $ \dst# -> IO (\s -> (# copyByteArray# src# 0# dst# 0# half# s, () #))
      hi = allocByteBufPure half $ \dst# -> IO (\s -> (# copyByteArray# src# half# dst# 0# half# s, () #))
   in (RawSIMD lo, RawSIMD hi)

ffiConcat :: forall n n2 a. (SIMDShape n a, SIMDShape n2 a) => RawSIMD n a -> RawSIMD n a -> RawSIMD n2 a
ffiConcat (RawSIMD (ByteBuf lo#)) (RawSIMD (ByteBuf hi#)) =
  let !half@(I# half#) = bytesInt @n @a
      full = allocByteBufPure (2 * half) $ \dst# -> do
        IO (\s -> (# copyByteArray# lo# 0# dst# 0# half# s, () #))
        IO (\s -> (# copyByteArray# hi# 0# dst# half# half# s, () #))
   in RawSIMD full

ffiLoad :: forall n a. (SIMDShape n a) => Ptr a -> IO (RawSIMD n a)
ffiLoad p = do
  let b = bytesInt @n @a
  RawSIMD <$> allocByteBuf b (\dst# -> c_load (fromIntegral b) dst# (castPtr p))

ffiLoadAligned :: forall n a. (SIMDShape n a) => Ptr a -> IO (RawSIMD n a)
ffiLoadAligned p = do
  let b = bytesInt @n @a
  RawSIMD <$> allocByteBuf b (\dst# -> c_load_aligned (fromIntegral b) dst# (castPtr p))

ffiStore :: forall n a. (SIMDShape n a) => Ptr a -> RawSIMD n a -> IO ()
ffiStore p (RawSIMD (ByteBuf s#)) =
  c_store (fromIntegral (bytesInt @n @a)) (castPtr p) s#

ffiStoreAligned :: forall n a. (SIMDShape n a) => Ptr a -> RawSIMD n a -> IO ()
ffiStoreAligned p (RawSIMD (ByteBuf s#)) =
  c_store_aligned (fromIntegral (bytesInt @n @a)) (castPtr p) s#

ffiLoadMasked :: forall n a. (SIMDShape n a) => RawMask n a -> RawSIMD n a -> Ptr a -> IO (RawSIMD n a)
ffiLoadMasked (RawMask (ByteBuf m#)) (RawSIMD (ByteBuf fb#)) p =
  RawSIMD <$> allocByteBuf (bytesInt @n @a) (\dst# -> c_load_masked (elemC @a) (lanesC @n @a) dst# m# fb# (castPtr p))

ffiStoreMasked :: forall n a. (SIMDShape n a) => RawMask n a -> Ptr a -> RawSIMD n a -> IO ()
ffiStoreMasked (RawMask (ByteBuf m#)) p (RawSIMD (ByteBuf s#)) =
  c_store_masked (elemC @a) (lanesC @n @a) (castPtr p) m# s#

ffiGather :: forall n a. (SIMDShape n a, SIMDShape n Int32) => Ptr a -> RawSIMD n Int32 -> IO (RawSIMD n a)
ffiGather p (RawSIMD (ByteBuf idx#)) =
  RawSIMD <$> allocByteBuf (bytesInt @n @a) (\dst# -> c_gather (elemC @a) (lanesC @n @a) dst# (castPtr p) idx#)

ffiGatherMasked :: forall n a. (SIMDShape n a, SIMDShape n Int32) => RawMask n a -> RawSIMD n a -> Ptr a -> RawSIMD n Int32 -> IO (RawSIMD n a)
ffiGatherMasked (RawMask (ByteBuf m#)) (RawSIMD (ByteBuf fb#)) p (RawSIMD (ByteBuf idx#)) =
  RawSIMD <$> allocByteBuf (bytesInt @n @a) (\dst# -> c_gather_masked (elemC @a) (lanesC @n @a) dst# m# fb# (castPtr p) idx#)
