{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.Shuffle where

import Control.Monad (forM_, when)
import Data.Bits (Bits (..))
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word8)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 8. ExpSIMDShuffle & ExpSIMDSplit
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle LinuxX86_64C n a where
  vshuffleLanes idxs (JanusC mv) = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    when (length idxs /= lanes) $ error "vshuffleLanes: index list length mismatch"
    srcArr <- emitUninitLocal arrTy
    dstArr <- emitUninitLocal arrTy
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [srcArr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ (zip [0 :: Int ..] idxs) $ \(outI, inI) -> do
      let masked = inI .&. (lanes - 1)
      emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst outI) noLoc) JustAssign (Index srcArr (signedIntConst masked) noLoc) noLoc) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst bytes]) noLoc
    pure (RVal res)

  vpermuteVar (JanusC mv) (JanusC midx) = JanusC $ do
    addHeader "string.h"
    fs <- askX86Features
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
    RVal ev0 <- mv
    RVal eidx0 <- midx
    ev <- emitLocalVal ty ev0
    eidx <- emitLocalVal ity eidx0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        bytes = lanes * sz
    if hasX86Feature fs X86_AVX2 && sz == 4 && lanes == 8
      then do
        let perm =
              if simdElemTag (Proxy @a) == JanusElemFloat
                then cCall "_mm256_permutevar8x32_ps" [ev, eidx]
                else cCall "_mm256_permutevar8x32_epi32" [ev, eidx]
        RVal <$> emitLocalVal ty perm
      else do
        let JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            JCType ispec _ = namedCType "int32_t"
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        srcArr <- emitUninitLocal (JCType espec arrDec)
        idxArr <- emitUninitLocal (JCType ispec arrDec)
        dstArr <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [srcArr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let k = BinOp And (cCast "uint32_t" (Index idxArr (signedIntConst i) noLoc)) (unsignedIntConst (lanes - 1)) noLoc
          emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index srcArr k noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst bytes]) noLoc
        pure (RVal res)

  vshuffleBytes ::
    forall b.
    (SIMDShape b Word8) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C b Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n a)
  vshuffleBytes (JanusC mv) (JanusC mc) = JanusC $ do
    addHeader "string.h"
    fs <- askX86Features
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    cty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C b Word8))
    RVal ev0 <- mv
    RVal ec0 <- mc
    ev <- emitLocalVal ty ev0
    ec <- emitLocalVal cty ec0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    if bytes == 16 && hasX86Feature fs X86_SSSE3
      then RVal <$> emitLocalVal ty (fromM128i pa (cCall "_mm_shuffle_epi8" [asM128i pa ev, ec]))
      else do
        let JCType bspec _ = namedCType "uint8_t"
            arrDec = Array [] (ArraySize False (signedIntConst bytes) noLoc) (DeclRoot noLoc) noLoc
        sb <- emitUninitLocal (JCType bspec arrDec)
        cb <- emitUninitLocal (JCType bspec arrDec)
        db <- emitUninitLocal (JCType bspec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [sb, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [cb, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. bytes - 1] $ \i -> do
          let ci = Index cb (signedIntConst i) noLoc
              sel = Cond (BinOp And ci (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (0 :: Word32)) (Index sb (BinOp And ci (unsignedIntConst (bytes - 1)) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index db (signedIntConst i) noLoc) JustAssign sel noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, db, signedIntConst bytes]) noLoc
        pure (RVal res)

  vzipLo (JanusC ma) (JanusC mb) = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 16
      then do
        let fn = case simdElemTag (Proxy @a) of
              JanusElemInt8 -> "_mm_unpacklo_epi8"
              JanusElemWord8 -> "_mm_unpacklo_epi8"
              JanusElemInt16 -> "_mm_unpacklo_epi16"
              JanusElemWord16 -> "_mm_unpacklo_epi16"
              JanusElemInt32 -> "_mm_unpacklo_epi32"
              JanusElemWord32 -> "_mm_unpacklo_epi32"
              JanusElemInt64 -> "_mm_unpacklo_epi64"
              JanusElemWord64 -> "_mm_unpacklo_epi64"
              JanusElemFloat -> "_mm_unpacklo_ps"
              JanusElemDouble -> "_mm_unpacklo_pd"
        RVal <$> emitLocalVal ty (cCall fn [ea, eb])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            half = lanes `div` 2
            JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        aa <- emitUninitLocal (JCType espec arrDec)
        ab <- emitUninitLocal (JCType espec arrDec)
        ar <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. half - 1] $ \i -> do
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst i) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
        pure (RVal res)

  vzipHi (JanusC ma) (JanusC mb) = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 16
      then do
        let fn = case simdElemTag (Proxy @a) of
              JanusElemInt8 -> "_mm_unpackhi_epi8"
              JanusElemWord8 -> "_mm_unpackhi_epi8"
              JanusElemInt16 -> "_mm_unpackhi_epi16"
              JanusElemWord16 -> "_mm_unpackhi_epi16"
              JanusElemInt32 -> "_mm_unpackhi_epi32"
              JanusElemWord32 -> "_mm_unpackhi_epi32"
              JanusElemInt64 -> "_mm_unpackhi_epi64"
              JanusElemWord64 -> "_mm_unpackhi_epi64"
              JanusElemFloat -> "_mm_unpackhi_ps"
              JanusElemDouble -> "_mm_unpackhi_pd"
        RVal <$> emitLocalVal ty (cCall fn [ea, eb])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            half = lanes `div` 2
            JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        aa <- emitUninitLocal (JCType espec arrDec)
        ab <- emitUninitLocal (JCType espec arrDec)
        ar <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. half - 1] $ \i -> do
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst (half + i)) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst (half + i)) noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
        pure (RVal res)

instance (SIMDHalfPair n2 n a) => ExpSIMDSplit LinuxX86_64C n2 n a where
  vsplit (JanusC mv) =
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        extractPart (offset :: Int) = JanusC $ do
          addHeader "string.h"
          ty2 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n2 a))
          ty1 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
          RVal ev0 <- mv
          ev <- emitLocalVal ty2 ev0
          out <- emitUninitLocal ty1
          let srcPtr = BinOp Add (cConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst offset) noLoc
          emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst halfB]) noLoc
          pure (RVal out)
     in (extractPart 0, extractPart halfB)

  vconcat (JanusC mlo) (JanusC mhi) = JanusC $ do
    addHeader "string.h"
    ty1 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    ty2 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n2 a))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal ty1 elo0
    ehi <- emitLocalVal ty1 ehi0
    out <- emitUninitLocal ty2
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        dstPtr = cPtrCast "uint8_t" (UnOp AddrOf out noLoc)
    emitStmt $ Exp (Just $ cCall "memcpy" [dstPtr, UnOp AddrOf elo noLoc, signedIntConst halfB]) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [BinOp Add dstPtr (signedIntConst halfB) noLoc, UnOp AddrOf ehi noLoc, signedIntConst halfB]) noLoc
    pure (RVal out)
