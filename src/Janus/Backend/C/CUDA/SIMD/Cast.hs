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

module Janus.Backend.C.CUDA.SIMD.Cast where

import Control.Monad (forM_, when)
import Data.Bits (Bits (..))
import Data.Int (Int64)
import Data.Loc
import Data.Proxy
import Janus.Backend.C
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 11. ExpSIMDCast JanusCUDA
instance ExpSIMDCast JanusCUDA where
  vbitcast ::
    forall n a m b.
    (SIMDShape n a, SIMDShape m b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA m b)
  vbitcast (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    let b1 = simdBytes (Proxy @n) (Proxy @a)
        b2 = simdBytes (Proxy @m) (Proxy @b)
    when (b1 /= b2) $ error "vbitcast: mismatched byte widths"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA m b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf ev noLoc, signedIntConst b1]) noLoc
    pure (RVal out)

  vcvt ::
    forall n a b.
    (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA n b)
  vcvt (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sb = simdBytes (Proxy @n) (Proxy @a)
        db = simdBytes (Proxy @n) (Proxy @b)
        JCType sspec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = cuNamedType (simdElemCTypeName (Proxy @b))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    sa <- cuUninit (JCType sspec arrDec)
    da <- cuUninit (JCType dspec arrDec)
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst sb]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let idx = signedIntConst i
      cuStmt $ Exp (Just $ Assign (Index da idx noLoc) JustAssign (cuCast (simdElemCTypeName (Proxy @b)) (Index sa idx noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst db]) noLoc
    pure (RVal out)

  vnarrowSat ::
    forall n m a b.
    (SIMDPackShape n m a b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA m b)
  vnarrowSat (JanusCUDA (JanusC mlo)) (JanusCUDA (JanusC mhi)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA m b))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal srcTy elo0
    ehi <- emitLocalVal srcTy ehi0
    let half = simdLanes (Proxy @n) (Proxy @a)
        total = 2 * half
        sb = simdBytes (Proxy @n) (Proxy @a)
        db = simdBytes (Proxy @m) (Proxy @b)
        dstBits = simdElemBytes (Proxy @b) * 8
        dstSigned = simdElemSigned (Proxy @b)
        dmin = if dstSigned then -(1 `shiftL` (dstBits - 1) :: Int64) else 0
        dmax = (if dstSigned then (1 `shiftL` (dstBits - 1)) - 1 else (1 `shiftL` dstBits) - 1) :: Int64
        JCType sspec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = cuNamedType (simdElemCTypeName (Proxy @b))
        sDec = Array [] (ArraySize False (signedIntConst half) noLoc) (DeclRoot noLoc) noLoc
        dDec = Array [] (ArraySize False (signedIntConst total) noLoc) (DeclRoot noLoc) noLoc
    alo <- cuUninit (JCType sspec sDec)
    ahi <- cuUninit (JCType sspec sDec)
    adst <- cuUninit (JCType dspec dDec)
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [alo, UnOp AddrOf elo noLoc, signedIntConst sb]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ahi, UnOp AddrOf ehi noLoc, signedIntConst sb]) noLoc
    let clampExp v =
          let v64 = cuCast "int64_t" v
              loE = signedIntConst dmin
              hiE = signedIntConst dmax
           in cuCast (simdElemCTypeName (Proxy @b)) (Cond (BinOp Lt v64 loE noLoc) loE (Cond (BinOp Gt v64 hiE noLoc) hiE v64 noLoc) noLoc)
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index adst (signedIntConst i) noLoc) JustAssign (clampExp (Index alo (signedIntConst i) noLoc)) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index adst (signedIntConst (half + i)) noLoc) JustAssign (clampExp (Index ahi (signedIntConst i) noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, adst, signedIntConst db]) noLoc
    pure (RVal out)
