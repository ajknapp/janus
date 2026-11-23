{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.Mem where

import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 12. CmdSIMDMem
emitX86Load :: forall n a. (SIMDShape n a) => Bool -> Exp -> JanusCM Exp
emitX86Load aligned ptrExp = do
  fs <- askX86Features
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  p <- emitLocalVal (JCType (DeclSpec [] [Tconst noLoc] (Tvoid noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)) ptrExp
  let b = simdBytes (Proxy @n) (Proxy @a)
      tag = simdElemTag (Proxy @a)
      load128 ptr128 = case tag of
        JanusElemFloat -> cCall (if aligned then "_mm_load_ps" else "_mm_loadu_ps") [cConstPtrCast "float" ptr128]
        JanusElemDouble -> cCall (if aligned then "_mm_load_pd" else "_mm_loadu_pd") [cConstPtrCast "double" ptr128]
        _ -> cCall (if aligned then "_mm_load_si128" else "_mm_loadu_si128") [cConstPtrCast "__m128i" ptr128]
  case b of
    16 -> emitLocalVal ty (load128 p)
    32
      | x86Has256Reg fs (Proxy @a) ->
          let call = case tag of
                JanusElemFloat -> cCall (if aligned then "_mm256_load_ps" else "_mm256_loadu_ps") [cConstPtrCast "float" p]
                JanusElemDouble -> cCall (if aligned then "_mm256_load_pd" else "_mm256_loadu_pd") [cConstPtrCast "double" p]
                _ -> cCall (if aligned then "_mm256_load_si256" else "_mm256_loadu_si256") [cConstPtrCast "__m256i" p]
           in emitLocalVal ty call
      | otherwise -> do
          let halfTy = namedCType (x86128RegName (Proxy @a))
              pHi = BinOp Add (cConstPtrCast "uint8_t" p) (cInt 16) noLoc
          lo <- emitLocalVal halfTy (load128 p)
          hi <- emitLocalVal halfTy (load128 pHi)
          emitPairStruct ty lo hi
    _ -> emitMemcpyLoad @(SIMDVector LinuxX86_64C n a) b p

emitX86Store :: forall n a. (SIMDShape n a) => Bool -> Exp -> Exp -> JanusCM ()
emitX86Store aligned ptrExp valExp = do
  fs <- askX86Features
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  p <- emitLocalVal voidPtrType ptrExp
  v <- emitLocalVal ty valExp
  let b = simdBytes (Proxy @n) (Proxy @a)
      tag = simdElemTag (Proxy @a)
      store128 ptr128 val128 = case tag of
        JanusElemFloat -> cCall (if aligned then "_mm_store_ps" else "_mm_storeu_ps") [cPtrCast "float" ptr128, val128]
        JanusElemDouble -> cCall (if aligned then "_mm_store_pd" else "_mm_storeu_pd") [cPtrCast "double" ptr128, val128]
        _ -> cCall (if aligned then "_mm_store_si128" else "_mm_storeu_si128") [cPtrCast "__m128i" ptr128, val128]
  case b of
    16 -> emitStmt (Exp (Just (store128 p v)) noLoc)
    32
      | x86Has256Reg fs (Proxy @a) ->
          let call = case tag of
                JanusElemFloat -> cCall (if aligned then "_mm256_store_ps" else "_mm256_storeu_ps") [cPtrCast "float" p, v]
                JanusElemDouble -> cCall (if aligned then "_mm256_store_pd" else "_mm256_storeu_pd") [cPtrCast "double" p, v]
                _ -> cCall (if aligned then "_mm256_store_si256" else "_mm256_storeu_si256") [cPtrCast "__m256i" p, v]
           in emitStmt (Exp (Just call) noLoc)
      | otherwise -> do
          let pHi = BinOp Add (cPtrCast "uint8_t" p) (cInt 16) noLoc
          emitStmt (Exp (Just (store128 p (cMember v "lo"))) noLoc)
          emitStmt (Exp (Just (store128 pHi (cMember v "hi"))) noLoc)
    _ -> do
      addHeader "string.h"
      emitStmt (Exp (Just $ cCall "memcpy" [p, UnOp AddrOf v noLoc, signedIntConst b]) noLoc)

instance (SIMDShape n a, Typeable a, JanusCTyped a) => CmdSIMDMem LinuxX86_64CM LinuxX86_64C n a where
  vload (JanusC mp) = liftJanusCM $ do
    RVal p <- mp
    v <- emitX86Load @n @a False p
    pure (JanusC (pure (RVal v)))

  vloadAligned (JanusC mp) = liftJanusCM $ do
    RVal p <- mp
    v <- emitX86Load @n @a True p
    pure (JanusC (pure (RVal v)))

  vstore (JanusC mp) (JanusC mv) = liftJanusCM $ do
    RVal p <- mp
    RVal v <- mv
    emitX86Store @n @a False p v

  vstoreAligned (JanusC mp) (JanusC mv) = liftJanusCM $ do
    RVal p <- mp
    RVal v <- mv
    emitX86Store @n @a True p v

  vloadMasked (JanusC mm) (JanusC mfb) (JanusC mp) =
    liftJanusCM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- emitUninitLocal (JCType espec arrDec)
      out <- emitUninitLocal vty
      emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignLoad = BlockStm $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignLoad] noLoc) Nothing noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
      pure (JanusC (pure (RVal out)))

  vstoreMasked (JanusC mm) (JanusC mp) (JanusC mv) =
    liftJanusCM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- emitUninitLocal (JCType espec arrDec)
      emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignStore = BlockStm $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignStore] noLoc) Nothing noLoc

  vgather (JanusC mp) (JanusC midx) =
    liftJanusCM $ do
      addHeader "string.h"
      fs <- askX86Features
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal ep0 <- mp
      RVal eidx0 <- midx
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          tag = simdElemTag (Proxy @a)
      out <- case (hasX86Feature fs X86_AVX2, tag, lanes) of
        (True, JanusElemFloat, 8) -> emitLocalVal vty (cCall "_mm256_i32gather_ps" [cConstPtrCast "float" ep, eidx, cInt 4])
        (True, JanusElemFloat, 4) -> emitLocalVal vty (cCall "_mm_i32gather_ps" [cConstPtrCast "float" ep, eidx, cInt 4])
        (True, t, 8) | t `elem` [JanusElemInt32, JanusElemWord32] -> emitLocalVal vty (cCall "_mm256_i32gather_epi32" [cConstPtrCast "int" ep, eidx, cInt 4])
        (True, t, 4) | t `elem` [JanusElemInt32, JanusElemWord32] -> emitLocalVal vty (cCall "_mm_i32gather_epi32" [cConstPtrCast "int" ep, eidx, cInt 4])
        _ -> do
          let JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
              JCType ispec _ = namedCType "int32_t"
              arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
          idxArr <- emitUninitLocal (JCType ispec arrDec)
          dstArr <- emitUninitLocal (JCType espec arrDec)
          res <- emitUninitLocal vty
          emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
          forM_ [0 .. lanes - 1] $ \i ->
            emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index ep (Index idxArr (signedIntConst i) noLoc) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst (lanes * sz)]) noLoc
          pure res
      pure (JanusC (pure (RVal out)))

  vgatherMasked (JanusC mm) (JanusC mfb) (JanusC mp) (JanusC midx) =
    liftJanusCM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = namedCType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      idxArr <- emitUninitLocal (JCType ispec arrDec)
      dstArr <- emitUninitLocal (JCType espec arrDec)
      out <- emitUninitLocal vty
      emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [dstArr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignGather = BlockStm $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index ep (Index idxArr (signedIntConst i) noLoc) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignGather] noLoc) Nothing noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, dstArr, signedIntConst bytes]) noLoc
      pure (JanusC (pure (RVal out)))
