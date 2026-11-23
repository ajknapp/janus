{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.Bits where

import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Word (Word64)
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

-- 7. ExpSIMDBits
bitBinX86 ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
bitBinX86 fn128 fn256ps fn256pd fn256si =
  let pa = Proxy @a
   in liftX86Bin @n @a
        (\_ x y -> pure $ fromM128i pa (cCall fn128 [asM128i pa x, asM128i pa y]))
        ( Just
            ( \_ x y -> pure $ case simdElemTag pa of
                JanusElemFloat -> cCall fn256ps [x, y]
                JanusElemDouble -> cCall fn256pd [x, y]
                _ -> cCall fn256si [x, y]
            )
        )

instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits LinuxX86_64C n a where
  vand = bitBinX86 "_mm_and_si128" "_mm256_and_ps" "_mm256_and_pd" "_mm256_and_si256"
  vor = bitBinX86 "_mm_or_si128" "_mm256_or_ps" "_mm256_or_pd" "_mm256_or_si256"
  vxor = bitBinX86 "_mm_xor_si128" "_mm256_xor_ps" "_mm256_xor_pd" "_mm256_xor_si256"
  vandnot = bitBinX86 "_mm_andnot_si128" "_mm256_andnot_ps" "_mm256_andnot_pd" "_mm256_andnot_si256"
  vnot v =
    let pa = Proxy @a
        ones128 _ x = pure $ fromM128i pa (cCall "_mm_xor_si128" [asM128i pa x, cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)]])
        ones256 _ x = pure $ case simdElemTag pa of
          JanusElemFloat -> cCall "_mm256_xor_ps" [x, cCall "_mm256_castsi256_ps" [cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]]
          JanusElemDouble -> cCall "_mm256_xor_pd" [x, cCall "_mm256_castsi256_pd" [cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]]
          _ -> cCall "_mm256_xor_si256" [x, cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]
     in liftX86Un @n @a ones128 (Just ones256) v

  vshl (JanusC mv) (JanusC mc) = JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        body x =
          let ux = if simdElemIsFloat (Proxy @a) then error "vshl float" else cCast uTy x
           in cCast (simdElemCTypeName (Proxy @a)) $
                Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh ux c noLoc) noLoc
    if simdElemIsFloat (Proxy @a)
      then do
        -- Bitwise shift on raw integer lanes of equal width via union/memcpy loop
        addHeader "string.h"
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = namedCType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- emitUninitLocal (JCType uspec arrDec)
        resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
        evLoc <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` ev)
        emitStmt $ Exp (Just $ cCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              shifted = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (Index arrU idx noLoc) c noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign shifted noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal resVar)
      else RVal <$> emitLaneLoopUn @n @a body ev

  vshr (JanusC mv) (JanusC mc) = JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = namedCType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- emitUninitLocal (JCType uspec arrDec)
        resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
        evLoc <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` ev)
        emitStmt $ Exp (Just $ cCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              shifted = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (Index arrU idx noLoc) c noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign shifted noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal resVar)
      else do
        let body x
              | simdElemSigned (Proxy @a) =
                  let sx = cCast sTy x
                      fill = Cond (BinOp Lt sx (cInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cInt 0) noLoc
                   in cCast (simdElemCTypeName (Proxy @a)) $
                        Cond (BinOp Ge c (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx c noLoc) noLoc
              | otherwise =
                  let ux = cCast uTy x
                   in cCast (simdElemCTypeName (Proxy @a)) $
                        Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh ux c noLoc) noLoc
        RVal <$> emitLaneLoopUn @n @a body ev

  vshlImm p v = vshl v (fromIntegral (natVal p))
  vshrImm p v = vshr v (fromIntegral (natVal p))

  vshlVar a b = JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_AVX2 && tag `elem` [JanusElemInt32, JanusElemWord32, JanusElemInt64, JanusElemWord64]
      then do
        let sll128 = if tag `elem` [JanusElemInt32, JanusElemWord32] then "_mm_sllv_epi32" else "_mm_sllv_epi64"
            sll256 = if tag `elem` [JanusElemInt32, JanusElemWord32] then "_mm256_sllv_epi32" else "_mm256_sllv_epi64"
        getJanusC $ liftX86Bin @n @a (\_ x y -> pure $ cCall sll128 [x, y]) (Just (\_ x y -> pure $ cCall sll256 [x, y])) a b
      else do
        RVal ea <- getJanusC a
        RVal eb <- getJanusC b
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            body x y =
              let uc = cCast uTy y
               in cCast (simdElemCTypeName (Proxy @a)) $
                    Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cCast uTy x) uc noLoc) noLoc
        RVal <$> emitLaneLoopBin @n @a body ea eb

  vshrVar a b = JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_AVX2 && tag `elem` [JanusElemInt32, JanusElemWord32, JanusElemWord64]
      then do
        let srl128 = case tag of
              JanusElemInt32 -> "_mm_srav_epi32"
              JanusElemWord32 -> "_mm_srlv_epi32"
              _ -> "_mm_srlv_epi64"
            srl256 = case tag of
              JanusElemInt32 -> "_mm256_srav_epi32"
              JanusElemWord32 -> "_mm256_srlv_epi32"
              _ -> "_mm256_srlv_epi64"
        getJanusC $ liftX86Bin @n @a (\_ x y -> pure $ cCall srl128 [x, y]) (Just (\_ x y -> pure $ cCall srl256 [x, y])) a b
      else do
        RVal ea <- getJanusC a
        RVal eb <- getJanusC b
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            sTy = "int" <> show bits <> "_t"
            body x y
              | simdElemSigned (Proxy @a) =
                  let uc = cCast uTy y
                      sx = cCast sTy x
                      fill = Cond (BinOp Lt sx (cInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cInt 0) noLoc
                   in cCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx uc noLoc) noLoc
              | otherwise =
                  let uc = cCast uTy y
                   in cCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cCast uTy x) uc noLoc) noLoc
        RVal <$> emitLaneLoopBin @n @a body ea eb

  vpopcnt v = JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_SSSE3 && tag `elem` [JanusElemInt8, JanusElemWord8]
      then do
        let pop8_128 _ x = do
              lut <- emitLocalVal (namedCType "__m128i") (cCall "_mm_setr_epi8" (map signedIntConst ([0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4] :: [Int])))
              msk <- emitLocalVal (namedCType "__m128i") (cCall "_mm_set1_epi8" [cInt 0x0F])
              let lo = cCall "_mm_shuffle_epi8" [lut, cCall "_mm_and_si128" [x, msk]]
                  hi = cCall "_mm_shuffle_epi8" [lut, cCall "_mm_and_si128" [cCall "_mm_srli_epi16" [x, cInt 4], msk]]
              pure $ cCall "_mm_add_epi8" [lo, hi]
            pop8_256 _ x = do
              lut <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_setr_epi8" (map signedIntConst ([0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4] :: [Int])))
              msk <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_set1_epi8" [cInt 0x0F])
              let lo = cCall "_mm256_shuffle_epi8" [lut, cCall "_mm256_and_si256" [x, msk]]
                  hi = cCall "_mm256_shuffle_epi8" [lut, cCall "_mm256_and_si256" [cCall "_mm256_srli_epi16" [x, cInt 4], msk]]
              pure $ cCall "_mm256_add_epi8" [lo, hi]
        getJanusC $ liftX86Un @n @a pop8_128 (Just pop8_256) v
      else do
        RVal ev <- getJanusC v
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            body x = cCast (simdElemCTypeName (Proxy @a)) (cCall "__builtin_popcountll" [cCast "uint64_t" (cCast uTy x)])
        RVal <$> emitLaneLoopUn @n @a body ev
