{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.Bits where

import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Word (Word64)
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 7. ExpSIMDBits JanusCUDA
emitCUDABitWordBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a)
emitCUDABitWordBin opW (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  RVal ea0 <- ma
  RVal eb0 <- mb
  ea <- emitLocalVal ty ea0
  eb <- emitLocalVal ty eb0
  let bytes = simdBytes (Proxy @n) (Proxy @a)
      words32 = bytes `div` 4
      JCType u32spec _ = cuNamedType "uint32_t"
      arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType u32spec arrDec
  aw <- cuUninit arrTy
  bw <- cuUninit arrTy
  rw <- cuUninit arrTy
  out <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. words32 - 1] $ \w -> do
    let wi = signedIntConst w
    cuStmt $ Exp (Just $ Assign (Index rw wi noLoc) JustAssign (opW (Index aw wi noLoc) (Index bw wi noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst bytes]) noLoc
  pure (RVal out)

instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits JanusCUDA n a where
  vand = emitCUDABitWordBin (\x y -> BinOp And x y noLoc)
  vor = emitCUDABitWordBin (\x y -> BinOp Or x y noLoc)
  vxor = emitCUDABitWordBin (\x y -> BinOp Xor x y noLoc)
  vandnot = emitCUDABitWordBin (\x y -> BinOp And (UnOp Not x noLoc) y noLoc)
  vnot v =
    vxor
      v
      ( JanusCUDA $ JanusC $ do
          addHeader "string.h"
          ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
          out <- cuUninit ty
          cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0xFF, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
          pure (RVal out)
      )

  vshl (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = cuNamedType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- cuUninit (JCType uspec arrDec)
        out <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= cuUninit
        evLoc <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= (`emitLocalVal` ev)
        cuStmt $ Exp (Just $ cuCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              sh = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (Index arrU idx noLoc) c noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign sh noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal out)
      else do
        let body x = cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cuCast uTy x) c noLoc) noLoc
        RVal <$> emitCUDALaneUn @n @a body ev

  vshr (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = cuNamedType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- cuUninit (JCType uspec arrDec)
        out <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= cuUninit
        evLoc <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= (`emitLocalVal` ev)
        cuStmt $ Exp (Just $ cuCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              sh = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (Index arrU idx noLoc) c noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign sh noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal out)
      else do
        let body x
              | simdElemSigned (Proxy @a) =
                  let sx = cuCast sTy x
                      fill = Cond (BinOp Lt sx (cuInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cuInt 0) noLoc
                   in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx c noLoc) noLoc
              | otherwise =
                  cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cuCast uTy x) c noLoc) noLoc
        RVal <$> emitCUDALaneUn @n @a body ev

  vshlImm p v = vshl v (fromIntegral (natVal p))
  vshrImm p v = vshr v (fromIntegral (natVal p))

  vshlVar (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        body x y =
          let uc = cuCast uTy y
           in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cuCast uTy x) uc noLoc) noLoc
    RVal <$> emitCUDALaneBin @n @a body a b

  vshrVar (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
        body x y
          | simdElemSigned (Proxy @a) =
              let uc = cuCast uTy y
                  sx = cuCast sTy x
                  fill = Cond (BinOp Lt sx (cuInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cuInt 0) noLoc
               in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx uc noLoc) noLoc
          | otherwise =
              let uc = cuCast uTy y
               in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cuCast uTy x) uc noLoc) noLoc
    RVal <$> emitCUDALaneBin @n @a body a b

  vpopcnt (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        popFn = if bits == 64 then "__popcll" else "__popc"
        argCast = if bits == 64 then "unsigned long long" else "uint32_t"
        body x = cuCast (simdElemCTypeName (Proxy @a)) (cuCall popFn [cuCast argCast (cuCast uTy x)])
    RVal <$> emitCUDALaneUn @n @a body ev
