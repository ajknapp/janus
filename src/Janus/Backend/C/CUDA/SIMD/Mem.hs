{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.Mem where

import Control.Monad (forM_)
import Control.Monad.Reader (lift)
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Janus.Backend.C
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 12. CmdSIMDMem JanusCUDAM JanusCUDA
instance (SIMDShape n a, Typeable a, JanusCTyped a) => CmdSIMDMem JanusCUDAM JanusCUDA n a where
  vload (JanusCUDA (JanusC mp)) = JanusCUDAM $ lift $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    pty <- getJanusCType (Proxy @(Ptr a))
    RVal ep0 <- mp
    ep <- emitLocalVal pty ep0
    out <- cuUninit vty
    -- Safe element-by-element load for unaligned global/shared pointers
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (JanusCUDA (JanusC (pure (RVal out))))

  vloadAligned (JanusCUDA (JanusC mp)) = JanusCUDAM $ lift $ do
    vty@(JCType spec dec) <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ep0 <- mp
    -- Coalesced wide load (e.g. ld.global.v4.f32 / ld.global.v4.u32)
    let castP = Cast (Type (setTypeQualifiers [Tconst noLoc] spec) (Ptr [] dec noLoc) noLoc) ep0 noLoc
    out <- emitLocalVal vty (UnOp Deref castP noLoc)
    pure (JanusCUDA (JanusC (pure (RVal out))))

  vstore (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) = JanusCUDAM $ lift $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    pty <- getJanusCType (Proxy @(Ptr a))
    RVal ep0 <- mp
    RVal ev0 <- mv
    ep <- emitLocalVal pty ep0
    ev <- emitLocalVal vty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc

  vstoreAligned (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) = JanusCUDAM $ lift $ do
    vty@(JCType spec dec) <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ep0 <- mp
    RVal ev0 <- mv
    ev <- emitLocalVal vty ev0
    let castP = Cast (Type spec (Ptr [] dec noLoc) noLoc) ep0 noLoc
    cuStmt $ Exp (Just $ Assign (UnOp Deref castP noLoc) JustAssign ev noLoc) noLoc

  vloadMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mfb)) (JanusCUDA (JanusC mp)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal efb0 <- mfb
      RVal ep0 <- mp
      em <- emitLocalVal mty em0
      efb <- emitLocalVal vty efb0
      ep <- emitLocalVal pty ep0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignLoad = BlockStm $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignLoad] noLoc) Nothing noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))

  vstoreMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal ep0 <- mp
      RVal ev0 <- mv
      em <- emitLocalVal mty em0
      ep <- emitLocalVal pty ep0
      ev <- emitLocalVal vty ev0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- cuUninit (JCType espec arrDec)
      cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignStore = BlockStm $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignStore] noLoc) Nothing noLoc

  vgather (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC midx)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal ep0 <- mp
      RVal eidx0 <- midx
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          bytes = simdBytes (Proxy @n) (Proxy @a)
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      ia <- cuUninit (JCType ispec arrDec)
      da <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      forM_ [0 .. lanes - 1] $ \i ->
        cuStmt $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index ep (Index ia (signedIntConst i) noLoc) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))

  vgatherMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mfb)) (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC midx)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal efb0 <- mfb
      RVal ep0 <- mp
      RVal eidx0 <- midx
      em <- emitLocalVal mty em0
      efb <- emitLocalVal vty efb0
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      ia <- cuUninit (JCType ispec arrDec)
      da <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [da, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignGather = BlockStm $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index ep (Index ia (signedIntConst i) noLoc) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignGather] noLoc) Nothing noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))
