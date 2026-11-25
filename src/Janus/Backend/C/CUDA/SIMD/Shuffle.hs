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

module Janus.Backend.C.CUDA.SIMD.Shuffle where

import Control.Monad (forM_, when)
import Data.Bits (Bits (..))
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word8)
import Janus.Backend.C.Raw
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 8. ExpSIMDShuffle & ExpSIMDSplit JanusCUDA
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle JanusCUDA n a where
  vshuffleLanes idxs (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    when (length idxs /= lanes) $ error "vshuffleLanes: index list length mismatch"
    sa <- cuUninit arrTy
    da <- cuUninit arrTy
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ (zip [0 :: Int ..] idxs) $ \(outI, inI) ->
      cuStmt $ Exp (Just $ Assign (Index da (signedIntConst outI) noLoc) JustAssign (Index sa (signedIntConst (inI .&. (lanes - 1))) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
    pure (RVal out)

  vpermuteVar (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC midx)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
    RVal ev0 <- mv
    RVal eidx0 <- midx
    ev <- emitLocalVal ty ev0
    eidx <- emitLocalVal ity eidx0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType ispec _ = cuNamedType "int32_t"
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    sa <- cuUninit (JCType espec arrDec)
    ia <- cuUninit (JCType ispec arrDec)
    da <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let k = BinOp And (cuCast "uint32_t" (Index ia (signedIntConst i) noLoc)) (unsignedIntConst (lanes - 1)) noLoc
      cuStmt $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index sa k noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
    pure (RVal out)

  vshuffleBytes ::
    forall b.
    (SIMDShape b Word8) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA b Word8) ->
    JanusCUDA (SIMDVector JanusCUDA n a)
  vshuffleBytes (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    cty <- getJanusCType (Proxy @(SIMDVector JanusCUDA b Word8))
    RVal ev0 <- mv
    RVal ec0 <- mc
    ev <- emitLocalVal ty ev0
    ec <- emitLocalVal cty ec0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 4
      then do
        -- Use CUDA __byte_perm for 4-byte vectors + high-bit zero mask!
        -- Selector nibbles: (c0 & 3) | ((c1 & 3) << 4) | ((c2 & 3) << 8) | ((c3 & 3) << 12)
        let nib (k :: Int) = BinOp Lsh (BinOp And (BinOp Rsh ec (cuInt (k * 8)) noLoc) (unsignedIntConst (3 :: Word32)) noLoc) (cuInt (k * 4)) noLoc
            sel = foldl1 (\x y -> BinOp Or x y noLoc) [nib 0, nib 1, nib 2, nib 3]
            permed = cuCall "__byte_perm" [ev, unsignedIntConst (0 :: Word32), sel]
            zeroMask = cuCall "__vcmpltu4" [ec, unsignedIntConst (0x80808080 :: Word32)]
        RVal <$> emitLocalVal ty (BinOp And permed zeroMask noLoc)
      else do
        let JCType bspec _ = cuNamedType "uint8_t"
            arrDec = Array [] (ArraySize False (signedIntConst bytes) noLoc) (DeclRoot noLoc) noLoc
        sb <- cuUninit (JCType bspec arrDec)
        cb <- cuUninit (JCType bspec arrDec)
        db <- cuUninit (JCType bspec arrDec)
        out <- cuUninit ty
        cuStmt $ Exp (Just $ cuCall "memcpy" [sb, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [cb, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. bytes - 1] $ \i -> do
          let ci = Index cb (signedIntConst i) noLoc
              s = Cond (BinOp And ci (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (0 :: Word32)) (Index sb (BinOp And ci (unsignedIntConst (bytes - 1)) noLoc) noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index db (signedIntConst i) noLoc) JustAssign s noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, db, signedIntConst bytes]) noLoc
        pure (RVal out)

  vzipLo (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        half = lanes `div` 2
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    aa <- cuUninit (JCType espec arrDec)
    ab <- cuUninit (JCType espec arrDec)
    ar <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst i) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst i) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

  vzipHi (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        half = lanes `div` 2
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    aa <- cuUninit (JCType espec arrDec)
    ab <- cuUninit (JCType espec arrDec)
    ar <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst (half + i)) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst (half + i)) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

instance (SIMDHalfPair n2 n a) => ExpSIMDSplit JanusCUDA n2 n a where
  vsplit (JanusCUDA (JanusC mv)) =
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        extractPart (off :: Int) = JanusCUDA $ JanusC $ do
          addHeader "string.h"
          ty2 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n2 a))
          ty1 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
          RVal ev0 <- mv
          ev <- emitLocalVal ty2 ev0
          out <- cuUninit ty1
          cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, BinOp Add (cuConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst off) noLoc, signedIntConst halfB]) noLoc
          pure (RVal out)
     in (extractPart 0, extractPart halfB)

  vconcat (JanusCUDA (JanusC mlo)) (JanusCUDA (JanusC mhi)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty1 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    ty2 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n2 a))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal ty1 elo0
    ehi <- emitLocalVal ty1 ehi0
    out <- cuUninit ty2
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        dstPtr = cuPtrCast "uint8_t" (UnOp AddrOf out noLoc)
    cuStmt $ Exp (Just $ cuCall "memcpy" [dstPtr, UnOp AddrOf elo noLoc, signedIntConst halfB]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [BinOp Add dstPtr (signedIntConst halfB) noLoc, UnOp AddrOf ehi noLoc, signedIntConst halfB]) noLoc
    pure (RVal out)
