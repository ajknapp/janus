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

module Janus.Backend.C.LinuxX86_64.SIMD.ReduceDot where

import Control.Monad (forM_)
import Data.Int (Int32, Int8)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word8)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 9. ExpSIMDReduce
emitX86Reduce ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C a
emitX86Reduce combine (JanusC mv) = JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  RVal ev0 <- mv
  ev <- emitLocalVal ty ev0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemName = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemName
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  arr <- emitUninitLocal (JCType espec arrDec)
  emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
  acc <- emitLocalVal (namedCType elemName) (Index arr (cInt 0) noLoc)
  forM_ [1 .. lanes - 1] $ \i ->
    emitStmt $ Exp (Just $ Assign acc JustAssign (combine acc (Index arr (signedIntConst i) noLoc)) noLoc) noLoc
  pure (RVal acc)

instance (SIMDShape n a, CPUSIMDElem a, JanusCTyped a) => ExpSIMDReduce LinuxX86_64C n a where
  vreduceAdd =
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        comb x y
          | simdElemIsFloat (Proxy @a) = BinOp Add x y noLoc
          | otherwise = cCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cCast uTy x) (cCast uTy y) noLoc)
     in emitX86Reduce comb
  vreduceMin = emitX86Reduce (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc)
  vreduceMax = emitX86Reduce (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc)
  vreduceAnd v = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC v
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc AndAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceOr v = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC v
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc OrAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceXor v = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC v
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc XorAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)

-- 10. ExpSIMDDot
emitX86Dp4aSWord :: Exp -> Exp -> Exp -> JanusCM Exp
emitX86Dp4aSWord ea eb eacc = do
  a <- emitLocalVal (namedCType "uint32_t") ea
  b <- emitLocalVal (namedCType "uint32_t") eb
  acc <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" eacc)
  let byteS u k = cCast "int32_t" (cCast "int8_t" (BinOp Rsh u (signedIntConst (k * 8)) noLoc))
      term k = cCast "uint32_t" (BinOp Mul (byteS a k) (byteS b k) noLoc)
      sum4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
  emitLocalVal (namedCType "int32_t") (cCast "int32_t" sum4)

emitX86Dp4aUWord :: Exp -> Exp -> Exp -> JanusCM Exp
emitX86Dp4aUWord ea eb eacc = do
  a <- emitLocalVal (namedCType "uint32_t") ea
  b <- emitLocalVal (namedCType "uint32_t") eb
  acc <- emitLocalVal (namedCType "uint32_t") eacc
  let byteU u k = BinOp And (BinOp Rsh u (signedIntConst (k * 8)) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
      term k = BinOp Mul (byteU a k) (byteU b k) noLoc
      sum4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
  emitLocalVal (namedCType "uint32_t") sum4

instance ExpSIMDDot LinuxX86_64C where
  vdp4aS (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitX86Dp4aSWord a b c

  vdp4aU (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitX86Dp4aUWord a b c

  vdp4aVecS ::
    forall m k.
    (SIMDDp4aShape m k) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C m Int8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m Int8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Int32) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Int32)
  vdp4aVecS (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      addHeader "string.h"
      tyA <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m Int8))
      tyK <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C k Int32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Int32)
          JCType u32spec _ = namedCType "uint32_t"
          JCType i32spec _ = namedCType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- emitUninitLocal (JCType u32spec arrDec)
      bw <- emitUninitLocal (JCType u32spec arrDec)
      cw <- emitUninitLocal (JCType i32spec arrDec)
      rw <- emitUninitLocal (JCType i32spec arrDec)
      res <- emitUninitLocal tyK
      emitStmt $ Exp (Just $ cCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitX86Dp4aSWord (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        emitStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal res)

  vdp4aVecU ::
    forall m k.
    (SIMDDp4aShape m k) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C m Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Word32) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Word32)
  vdp4aVecU (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      addHeader "string.h"
      tyA <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m Word8))
      tyK <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C k Word32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Word32)
          JCType u32spec _ = namedCType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- emitUninitLocal (JCType u32spec arrDec)
      bw <- emitUninitLocal (JCType u32spec arrDec)
      cw <- emitUninitLocal (JCType u32spec arrDec)
      rw <- emitUninitLocal (JCType u32spec arrDec)
      res <- emitUninitLocal tyK
      emitStmt $ Exp (Just $ cCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitX86Dp4aUWord (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        emitStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal res)

  vdp2aLoS (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = cCast "int32_t" (cCast "int16_t" a)
          a1 = cCast "int32_t" (cCast "int16_t" (BinOp Rsh a (cInt 16) noLoc))
          b0 = cCast "int32_t" (cCast "int8_t" b)
          b1 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 8) noLoc))
          prod = cCast "uint32_t" (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc)
      RVal <$> emitLocalVal (namedCType "int32_t") (cCast "int32_t" (BinOp Add (cCast "uint32_t" ec) prod noLoc))

  vdp2aHiS (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = cCast "int32_t" (cCast "int16_t" a)
          a1 = cCast "int32_t" (cCast "int16_t" (BinOp Rsh a (cInt 16) noLoc))
          b2 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 16) noLoc))
          b3 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 24) noLoc))
          prod = cCast "uint32_t" (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc)
      RVal <$> emitLocalVal (namedCType "int32_t") (cCast "int32_t" (BinOp Add (cCast "uint32_t" ec) prod noLoc))

  vdp2aLoU (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
          a1 = BinOp Rsh a (cInt 16) noLoc
          b0 = BinOp And b (unsignedIntConst (0xFF :: Word32)) noLoc
          b1 = BinOp And (BinOp Rsh b (cInt 8) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
      RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc) noLoc)

  vdp2aHiU (JanusC ma) (JanusC mb) (JanusC mc) =
    JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
          a1 = BinOp Rsh a (cInt 16) noLoc
          b2 = BinOp And (BinOp Rsh b (cInt 16) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          b3 = BinOp Rsh b (cInt 24) noLoc
      RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc) noLoc)

  vsad8 ::
    forall n.
    (SIMDShape n Word8) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n Word8) ->
    LinuxX86_64C Word32
  vsad8 (JanusC ma) (JanusC mb) =
    JanusC $ do
      fs <- askX86Features
      ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Word8))
      RVal ea0 <- ma
      RVal eb0 <- mb
      ea <- emitLocalVal ty ea0
      eb <- emitLocalVal ty eb0
      let lanes = simdLanes (Proxy @n) (Proxy @Word8)
      case lanes of
        32 | hasX86Feature fs X86_AVX2 -> do
          s <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_sad_epu8" [ea, eb])
          s128 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_add_epi64" [cCall "_mm256_castsi256_si128" [s], cCall "_mm256_extracti128_si256" [s, cInt 1]])
          let lo = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s128])
              hi = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [s128, cInt 8]])
          RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add lo hi noLoc)
        16 -> do
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [ea, eb])
          let lo = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s])
              hi = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [s, cInt 8]])
          RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add lo hi noLoc)
        8 -> do
          xa <- toXMM128 @Word8 8 ea
          xb <- toXMM128 @Word8 8 eb
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [xa, xb])
          RVal <$> emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s]))
        _ -> do
          xa <- toXMM128 @Word8 4 ea
          xb <- toXMM128 @Word8 4 eb
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [xa, xb])
          RVal <$> emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s]))
