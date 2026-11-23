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

module Janus.Backend.C.CUDA.SIMD.ReduceDot where

import Control.Monad (forM_)
import Data.Int (Int32, Int8)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word8)
import Janus.Backend.C
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 9. ExpSIMDReduce JanusCUDA
emitCUDAReduce ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA a
emitCUDAReduce combine (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  RVal ev0 <- mv
  ev <- emitLocalVal ty ev0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemName = simdElemCTypeName (Proxy @a)
      JCType espec _ = cuNamedType elemName
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  arr <- cuUninit (JCType espec arrDec)
  cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
  acc <- emitLocalVal (cuNamedType elemName) (Index arr (cuInt 0) noLoc)
  forM_ [1 .. lanes - 1] $ \i ->
    cuStmt $ Exp (Just $ Assign acc JustAssign (combine acc (Index arr (signedIntConst i) noLoc)) noLoc) noLoc
  pure (RVal acc)

instance (SIMDShape n a, CPUSIMDElem a, JanusCTyped a) => ExpSIMDReduce JanusCUDA n a where
  vreduceAdd =
    let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
        comb x y
          | simdElemIsFloat (Proxy @a) = BinOp Add x y noLoc
          | otherwise = cuCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cuCast uTy x) (cuCast uTy y) noLoc)
     in emitCUDAReduce comb
  vreduceMin = emitCUDAReduce (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc)
  vreduceMax = emitCUDAReduce (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc)
  vreduceAnd (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc AndAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceOr (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc OrAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceXor (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc XorAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)

-- 10. ExpSIMDDot JanusCUDA (__dp4a, __dp2a_lo, __dp2a_hi on sm_61+, __vsadu4)
emitCUDADp4aSWord :: CUDAFeatures -> Exp -> Exp -> Exp -> JanusCM Exp
emitCUDADp4aSWord fs ea eb ec
  | cudaHasDP4A fs = do
      addHeader "sm_61_intrinsics.h"
      emitLocalVal (cuNamedType "int32_t") (cuCall "__dp4a" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
  | otherwise = do
      a <- emitLocalVal (cuNamedType "uint32_t") ea
      b <- emitLocalVal (cuNamedType "uint32_t") eb
      acc <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec)
      let byteS u k = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh u (signedIntConst (k * 8)) noLoc))
          term k = cuCast "uint32_t" (BinOp Mul (byteS a k) (byteS b k) noLoc)
          s4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
      emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" s4)

emitCUDADp4aUWord :: CUDAFeatures -> Exp -> Exp -> Exp -> JanusCM Exp
emitCUDADp4aUWord fs ea eb ec
  | cudaHasDP4A fs = do
      addHeader "sm_61_intrinsics.h"
      emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp4a" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
  | otherwise = do
      a <- emitLocalVal (cuNamedType "uint32_t") ea
      b <- emitLocalVal (cuNamedType "uint32_t") eb
      acc <- emitLocalVal (cuNamedType "uint32_t") ec
      let byteU u k = BinOp And (BinOp Rsh u (signedIntConst (k * 8)) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          term k = BinOp Mul (byteU a k) (byteU b k) noLoc
          s4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
      emitLocalVal (cuNamedType "uint32_t") s4

instance ExpSIMDDot JanusCUDA where
  vdp4aS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitCUDADp4aSWord fs a b c

  vdp4aU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitCUDADp4aUWord fs a b c

  vdp4aVecS ::
    forall m k.
    (SIMDDp4aShape m k) =>
    JanusCUDA (SIMDVector JanusCUDA m Int8) ->
    JanusCUDA (SIMDVector JanusCUDA m Int8) ->
    JanusCUDA (SIMDVector JanusCUDA k Int32) ->
    JanusCUDA (SIMDVector JanusCUDA k Int32)
  vdp4aVecS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      fs <- askCUDAFeatures
      tyA <- getJanusCType (Proxy @(SIMDVector JanusCUDA m Int8))
      tyK <- getJanusCType (Proxy @(SIMDVector JanusCUDA k Int32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Int32)
          JCType u32spec _ = cuNamedType "uint32_t"
          JCType i32spec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- cuUninit (JCType u32spec arrDec)
      bw <- cuUninit (JCType u32spec arrDec)
      cw <- cuUninit (JCType i32spec arrDec)
      rw <- cuUninit (JCType i32spec arrDec)
      out <- cuUninit tyK
      cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitCUDADp4aSWord fs (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        cuStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal out)

  vdp4aVecU ::
    forall m k.
    (SIMDDp4aShape m k) =>
    JanusCUDA (SIMDVector JanusCUDA m Word8) ->
    JanusCUDA (SIMDVector JanusCUDA m Word8) ->
    JanusCUDA (SIMDVector JanusCUDA k Word32) ->
    JanusCUDA (SIMDVector JanusCUDA k Word32)
  vdp4aVecU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      fs <- askCUDAFeatures
      tyA <- getJanusCType (Proxy @(SIMDVector JanusCUDA m Word8))
      tyK <- getJanusCType (Proxy @(SIMDVector JanusCUDA k Word32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Word32)
          JCType u32spec _ = cuNamedType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- cuUninit (JCType u32spec arrDec)
      bw <- cuUninit (JCType u32spec arrDec)
      cw <- cuUninit (JCType u32spec arrDec)
      rw <- cuUninit (JCType u32spec arrDec)
      out <- cuUninit tyK
      cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitCUDADp4aUWord fs (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        cuStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal out)

  vdp2aLoS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCall "__dp2a_lo" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = cuCast "int32_t" (cuCast "int16_t" a)
              a1 = cuCast "int32_t" (cuCast "int16_t" (BinOp Rsh a (cuInt 16) noLoc))
              b0 = cuCast "int32_t" (cuCast "int8_t" b)
              b1 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 8) noLoc))
              prod = cuCast "uint32_t" (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc)
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" (BinOp Add (cuCast "uint32_t" ec) prod noLoc))

  vdp2aHiS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCall "__dp2a_hi" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = cuCast "int32_t" (cuCast "int16_t" a)
              a1 = cuCast "int32_t" (cuCast "int16_t" (BinOp Rsh a (cuInt 16) noLoc))
              b2 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 16) noLoc))
              b3 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 24) noLoc))
              prod = cuCast "uint32_t" (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc)
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" (BinOp Add (cuCast "uint32_t" ec) prod noLoc))

  vdp2aLoU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp2a_lo" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
              a1 = BinOp Rsh a (cuInt 16) noLoc
              b0 = BinOp And b (unsignedIntConst (0xFF :: Word32)) noLoc
              b1 = BinOp And (BinOp Rsh b (cuInt 8) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc) noLoc)

  vdp2aHiU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp2a_hi" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
              a1 = BinOp Rsh a (cuInt 16) noLoc
              b2 = BinOp And (BinOp Rsh b (cuInt 16) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
              b3 = BinOp Rsh b (cuInt 24) noLoc
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc) noLoc)

  vsad8 ::
    forall n.
    (SIMDShape n Word8) =>
    JanusCUDA (SIMDVector JanusCUDA n Word8) ->
    JanusCUDA (SIMDVector JanusCUDA n Word8) ->
    JanusCUDA Word32
  vsad8 (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Word8))
      RVal ea0 <- ma
      RVal eb0 <- mb
      ea <- emitLocalVal ty ea0
      eb <- emitLocalVal ty eb0
      let bytes = simdBytes (Proxy @n) (Proxy @Word8)
          words32 = bytes `div` 4
      if words32 == 1
        then RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__vsadu4" [ea, eb])
        else do
          let JCType u32spec _ = cuNamedType "uint32_t"
              arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
          aw <- cuUninit (JCType u32spec arrDec)
          bw <- cuUninit (JCType u32spec arrDec)
          cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
          cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
          acc <- emitLocalVal (cuNamedType "uint32_t") (unsignedIntConst (0 :: Word32))
          forM_ [0 .. words32 - 1] $ \w -> do
            let wi = signedIntConst w
            cuStmt $ Exp (Just $ Assign acc AddAssign (cuCall "__vsadu4" [Index aw wi noLoc, Index bw wi noLoc]) noLoc) noLoc
          pure (RVal acc)
