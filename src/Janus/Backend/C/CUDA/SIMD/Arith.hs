{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.Arith where

import Data.Bits (Bits (..))
import Data.Int (Int32, Int64)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word64)
import Janus.Backend.C.Raw
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Language.C.Quote as C

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, JanusLitC a) => Num (JanusCUDA (SIMDVector JanusCUDA n a)) where
  (+) = vadd
  (-) = vsub
  (*) = vmul
  negate = vneg
  abs = vabs
  signum (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    let z = cuInt 0
        one = if simdElemIsFloat (Proxy @a) then C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc else cuInt 1
        negOne = UnOp Negate one noLoc
        body x
          | not (simdElemSigned (Proxy @a)) = Cond (BinOp Gt x z noLoc) (cuInt 1) z noLoc
          | simdElemIsFloat (Proxy @a) = Cond (BinOp Gt x z noLoc) one (Cond (BinOp Lt x z noLoc) negOne x noLoc) noLoc
          | otherwise = cCastElem (BinOp Sub (BinOp Gt x z noLoc) (BinOp Lt x z noLoc) noLoc)
        cCastElem = cuCast (simdElemCTypeName (Proxy @a))
    RVal <$> emitCUDALaneUn @n @a body ev
  fromInteger k = vsplat (fromInteger k)

-- 4. ExpSIMDArith JanusCUDA (__vadd2/4, __vsub2/4, __vneg2/4, __vabs2/4, __vmin*2/4, __vmax*2/4)
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith JanusCUDA n a where
  vadd (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd2" [x, y]) a b
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneBin @n @a (\x y -> BinOp Add x y noLoc) a b
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vsub (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub2" [x, y]) a b
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneBin @n @a (\x y -> BinOp Sub x y noLoc) a b
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vmul (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> if simdElemIsFloat (Proxy @a)
      then emitCUDALaneBin @n @a (\x y -> BinOp Mul x y noLoc) a b
      else do
        let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
        emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Mul (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vneg (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg4" [x]) a
      JanusElemWord8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg4" [x]) a
      JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg2" [x]) a
      JanusElemWord16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg2" [x]) a
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneUn @n @a (\x -> UnOp Negate x noLoc) a
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneUn @n @a (\x -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (unsignedIntConst (0 :: Word64)) (cuCast uTy x) noLoc)) a

  vabs v@(JanusCUDA (JanusC ma))
    | not (simdElemSigned (Proxy @a)) = v
    | otherwise = JanusCUDA $ JanusC $ do
        RVal a <- ma
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabs4" [x]) a
          JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabs2" [x]) a
          JanusElemFloat -> emitCUDALaneUn @n @a (\x -> cuCall "fabsf" [x]) a
          JanusElemDouble -> emitCUDALaneUn @n @a (\x -> cuCall "fabs" [x]) a
          _ -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneUn @n @a (\x -> Cond (BinOp Lt x (cuInt 0) noLoc) (cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (unsignedIntConst (0 :: Word64)) (cuCast uTy x) noLoc)) x noLoc) a

  vmin (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmins4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vminu4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmins2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vminu2" [x, y]) a b
      _ -> emitCUDALaneBin @n @a (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc) a b

  vmax (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxs4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxu4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxs2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxu2" [x, y]) a b
      _ -> emitCUDALaneBin @n @a (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc) a b

-- 5. ExpSIMDSat JanusCUDA (__vaddss*, __vaddus*, __vsubss*, __vsubus*, __vabsss*, __vavgu*, __vabsdiff*)
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat JanusCUDA n a where
  vadds a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vadd a b
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddss4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddus4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddss2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddus2" [x, y]) ea eb
          _ | not (simdElemSigned (Proxy @a)) ->
            emitCUDALaneBin @n @a (\x y -> Cond (BinOp Lt (BinOp Add x y noLoc) x noLoc) (UnOp Not (cuCast (simdElemCTypeName (Proxy @a)) (unsignedIntConst (0 :: Word64))) noLoc) (BinOp Add x y noLoc) noLoc) ea eb
          JanusElemInt32 -> do
            let s x y = BinOp Add (cuCast "int64_t" x) (cuCast "int64_t" y) noLoc
                mx = signedIntConst (fromIntegral (maxBound :: Int32) :: Int64)
                mn = signedIntConst (fromIntegral (minBound :: Int32) :: Int64)
            emitCUDALaneBin @n @a (\x y -> cuCast "int32_t" (Cond (BinOp Gt (s x y) mx noLoc) mx (Cond (BinOp Lt (s x y) mn noLoc) mn (s x y) noLoc) noLoc)) ea eb
          _ -> do
            -- 64-bit signed saturating add without __int128
            let body x y =
                  let sumU = BinOp Add (cuCast "uint64_t" x) (cuCast "uint64_t" y) noLoc
                      resS = cuCast "int64_t" sumU
                      ovf = BinOp Ge (BinOp And (UnOp Not (BinOp Xor x y noLoc) noLoc) (BinOp Xor x resS noLoc) noLoc) (cuInt 0) noLoc
                      satVal = Cond (BinOp Lt x (cuInt 0) noLoc) (cuVar "INT64_MIN") (cuVar "INT64_MAX") noLoc
                   in Cond ovf resS satVal noLoc
            emitCUDALaneBin @n @a body ea eb

  vsubs a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vsub a b
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubss4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubus4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubss2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubus2" [x, y]) ea eb
          _ | not (simdElemSigned (Proxy @a)) ->
            emitCUDALaneBin @n @a (\x y -> Cond (BinOp Ge x y noLoc) (BinOp Sub x y noLoc) (unsignedIntConst (0 :: Word64)) noLoc) ea eb
          JanusElemInt32 -> do
            let d x y = BinOp Sub (cuCast "int64_t" x) (cuCast "int64_t" y) noLoc
                mx = signedIntConst (fromIntegral (maxBound :: Int32) :: Int64)
                mn = signedIntConst (fromIntegral (minBound :: Int32) :: Int64)
            emitCUDALaneBin @n @a (\x y -> cuCast "int32_t" (Cond (BinOp Gt (d x y) mx noLoc) mx (Cond (BinOp Lt (d x y) mn noLoc) mn (d x y) noLoc) noLoc)) ea eb
          _ -> do
            let body x y =
                  let diffU = BinOp Sub (cuCast "uint64_t" x) (cuCast "uint64_t" y) noLoc
                      resS = cuCast "int64_t" diffU
                      noOvf = BinOp Ge (BinOp And (BinOp Xor x y noLoc) (BinOp Xor x resS noLoc) noLoc) (cuInt 0) noLoc
                      satVal = Cond (BinOp Lt x (cuInt 0) noLoc) (cuVar "INT64_MIN") (cuVar "INT64_MAX") noLoc
                   in Cond noOvf resS satVal noLoc
            emitCUDALaneBin @n @a body ea eb

  vabss v@(JanusCUDA (JanusC ma))
    | not (simdElemSigned (Proxy @a)) || simdElemIsFloat (Proxy @a) = vabs v
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabsss4" [x]) ea
          JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabsss2" [x]) ea
          _ -> do
            let bits = simdElemBytes (Proxy @a) * 8
                sminE = if bits == 64 then cuVar "INT64_MIN" else cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64))
                smaxE = if bits == 64 then cuVar "INT64_MAX" else cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst (((1 `shiftL` (bits - 1)) - 1) :: Int64))
                absBody x = Cond (BinOp Eq x sminE noLoc) smaxE (Cond (BinOp Lt x (cuInt 0) noLoc) (UnOp Negate x noLoc) x noLoc) noLoc
            emitCUDALaneUn @n @a absBody ea

  vavg (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let halfLit = if simdElemTag (Proxy @a) == JanusElemFloat then C.Const (FloatConst "0.5f" 0.5 noLoc) noLoc else C.Const (DoubleConst "0.5" 0.5 noLoc) noLoc
        RVal <$> emitCUDALaneBin @n @a (\x y -> BinOp Mul halfLit (BinOp Add x y noLoc) noLoc) ea eb
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
            b8 = unsignedIntConst (0x80808080 :: Word32)
            b16 = unsignedIntConst (0x80008000 :: Word32)
        RVal <$> case tag of
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vavgu4" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vavgu2" [x, y]) ea eb
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> BinOp Xor (cuCall "__vavgu4" [BinOp Xor x b8 noLoc, BinOp Xor y b8 noLoc]) b8 noLoc) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> BinOp Xor (cuCall "__vavgu2" [BinOp Xor x b16 noLoc, BinOp Xor y b16 noLoc]) b16 noLoc) ea eb
          _ -> do
            -- Branchless Dietz rounding average: (x | y) - ((x ^ y) >> 1)
            emitCUDALaneBin @n @a (\x y -> BinOp Sub (BinOp Or x y noLoc) (BinOp Rsh (BinOp Xor x y noLoc) (cuInt 1) noLoc) noLoc) ea eb

  vabsdiff a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vabs (vsub a b)
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffs4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffu4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffs2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffu2" [x, y]) ea eb
          _ -> do
            let a' = JanusCUDA (JanusC (pure (RVal ea))) :: JanusCUDA (SIMDVector JanusCUDA n a)
                b' = JanusCUDA (JanusC (pure (RVal eb))) :: JanusCUDA (SIMDVector JanusCUDA n a)
            RVal eRes <- getJanusC $ getJanusCUDA $ vsub (vmax a' b') (vmin a' b')
            pure eRes
