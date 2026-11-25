{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.Arith where

import Data.Bits (Bits (..))
import Data.Int (Int64)
import Data.Loc
import Data.Proxy
import Data.Word (Word32)
import Janus.Backend.C.Raw
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.SIMD.Bits ()
import Janus.Backend.C.LinuxX86_64.SIMD.MaskCmp (x86Cmp128)
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Language.C.Quote as C

--------------------------------------------------------------------------------
-- Instruction Selection Implementations (SSE2 .. AVX2)
--------------------------------------------------------------------------------

x86Add128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Add128 _ a b = pure $ case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm_add_epi8" [a, b]
  JanusElemWord8 -> cCall "_mm_add_epi8" [a, b]
  JanusElemInt16 -> cCall "_mm_add_epi16" [a, b]
  JanusElemWord16 -> cCall "_mm_add_epi16" [a, b]
  JanusElemInt32 -> cCall "_mm_add_epi32" [a, b]
  JanusElemWord32 -> cCall "_mm_add_epi32" [a, b]
  JanusElemInt64 -> cCall "_mm_add_epi64" [a, b]
  JanusElemWord64 -> cCall "_mm_add_epi64" [a, b]
  JanusElemFloat -> cCall "_mm_add_ps" [a, b]
  JanusElemDouble -> cCall "_mm_add_pd" [a, b]

x86Add256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Add256 _ a b = pure $ case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm256_add_epi8" [a, b]
  JanusElemWord8 -> cCall "_mm256_add_epi8" [a, b]
  JanusElemInt16 -> cCall "_mm256_add_epi16" [a, b]
  JanusElemWord16 -> cCall "_mm256_add_epi16" [a, b]
  JanusElemInt32 -> cCall "_mm256_add_epi32" [a, b]
  JanusElemWord32 -> cCall "_mm256_add_epi32" [a, b]
  JanusElemInt64 -> cCall "_mm256_add_epi64" [a, b]
  JanusElemWord64 -> cCall "_mm256_add_epi64" [a, b]
  JanusElemFloat -> cCall "_mm256_add_ps" [a, b]
  JanusElemDouble -> cCall "_mm256_add_pd" [a, b]

x86Sub128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Sub128 _ a b = pure $ case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm_sub_epi8" [a, b]
  JanusElemWord8 -> cCall "_mm_sub_epi8" [a, b]
  JanusElemInt16 -> cCall "_mm_sub_epi16" [a, b]
  JanusElemWord16 -> cCall "_mm_sub_epi16" [a, b]
  JanusElemInt32 -> cCall "_mm_sub_epi32" [a, b]
  JanusElemWord32 -> cCall "_mm_sub_epi32" [a, b]
  JanusElemInt64 -> cCall "_mm_sub_epi64" [a, b]
  JanusElemWord64 -> cCall "_mm_sub_epi64" [a, b]
  JanusElemFloat -> cCall "_mm_sub_ps" [a, b]
  JanusElemDouble -> cCall "_mm_sub_pd" [a, b]

x86Sub256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Sub256 _ a b = pure $ case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm256_sub_epi8" [a, b]
  JanusElemWord8 -> cCall "_mm256_sub_epi8" [a, b]
  JanusElemInt16 -> cCall "_mm256_sub_epi16" [a, b]
  JanusElemWord16 -> cCall "_mm256_sub_epi16" [a, b]
  JanusElemInt32 -> cCall "_mm256_sub_epi32" [a, b]
  JanusElemWord32 -> cCall "_mm256_sub_epi32" [a, b]
  JanusElemInt64 -> cCall "_mm256_sub_epi64" [a, b]
  JanusElemWord64 -> cCall "_mm256_sub_epi64" [a, b]
  JanusElemFloat -> cCall "_mm256_sub_ps" [a, b]
  JanusElemDouble -> cCall "_mm256_sub_pd" [a, b]

x86Mul128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Mul128 fs a b = case simdElemTag (Proxy @a) of
  JanusElemFloat -> pure $ cCall "_mm_mul_ps" [a, b]
  JanusElemDouble -> pure $ cCall "_mm_mul_pd" [a, b]
  JanusElemInt16 -> pure $ cCall "_mm_mullo_epi16" [a, b]
  JanusElemWord16 -> pure $ cCall "_mm_mullo_epi16" [a, b]
  t | t == JanusElemInt32 || t == JanusElemWord32 ->
      if hasX86Feature fs X86_SSE4_1
        then pure $ cCall "_mm_mullo_epi32" [a, b]
        else do
          m02 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_mul_epu32" [a, b])
          m13 <-
            emitLocalVal
              (namedCType "__m128i")
              (cCall "_mm_mul_epu32" [cCall "_mm_srli_si128" [a, cInt 4], cCall "_mm_srli_si128" [b, cInt 4]])
          pure $
            cCall
              "_mm_unpacklo_epi32"
              [ cCall "_mm_shuffle_epi32" [m02, cInt 8],
                cCall "_mm_shuffle_epi32" [m13, cInt 8]
              ]
  t | t == JanusElemInt8 || t == JanusElemWord8 -> do
      ev <- emitLocalVal (namedCType "__m128i") (cCall "_mm_mullo_epi16" [a, b])
      od <-
        emitLocalVal
          (namedCType "__m128i")
          (cCall "_mm_mullo_epi16" [cCall "_mm_srli_epi16" [a, cInt 8], cCall "_mm_srli_epi16" [b, cInt 8]])
      pure $
        cCall
          "_mm_or_si128"
          [ cCall "_mm_and_si128" [ev, cCall "_mm_set1_epi16" [cInt 0x00FF]],
            cCall "_mm_slli_epi16" [od, cInt 8]
          ]
  _ -> do
    -- 64-bit integer multiply: extract 2x uint64_t and repack
    a0 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [a]))
    b0 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [b]))
    a1 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [a, cInt 8]]))
    b1 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [b, cInt 8]]))
    pure $
      cCall
        "_mm_set_epi64x"
        [ cCast "long long" (BinOp Mul a1 b1 noLoc),
          cCast "long long" (BinOp Mul a0 b0 noLoc)
        ]

x86Mul256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Mul256 _ a b = case simdElemTag (Proxy @a) of
  JanusElemFloat -> pure $ cCall "_mm256_mul_ps" [a, b]
  JanusElemDouble -> pure $ cCall "_mm256_mul_pd" [a, b]
  JanusElemInt16 -> pure $ cCall "_mm256_mullo_epi16" [a, b]
  JanusElemWord16 -> pure $ cCall "_mm256_mullo_epi16" [a, b]
  JanusElemInt32 -> pure $ cCall "_mm256_mullo_epi32" [a, b]
  JanusElemWord32 -> pure $ cCall "_mm256_mullo_epi32" [a, b]
  t | t == JanusElemInt8 || t == JanusElemWord8 -> do
      ev <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_mullo_epi16" [a, b])
      od <-
        emitLocalVal
          (namedCType "__m256i")
          (cCall "_mm256_mullo_epi16" [cCall "_mm256_srli_epi16" [a, cInt 8], cCall "_mm256_srli_epi16" [b, cInt 8]])
      pure $
        cCall
          "_mm256_or_si256"
          [ cCall "_mm256_and_si256" [ev, cCall "_mm256_set1_epi16" [cInt 0x00FF]],
            cCall "_mm256_slli_epi16" [od, cInt 8]
          ]
  _ -> do
    alo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [a])
    ahi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [a, cInt 1])
    blo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [b])
    bhi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [b, cInt 1])
    rlo <- x86Mul128 @a x86AVX2 alo blo
    rhi <- x86Mul128 @a x86AVX2 ahi bhi
    pure $ cCall "_mm256_set_m128i" [rhi, rlo]

--------------------------------------------------------------------------------
-- Num, ExpSIMDArith, & ExpSIMDSat
--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, JanusLitC a) => Num (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  (+) = vadd
  (-) = vsub
  (*) = vmul
  negate = vneg
  abs = vabs
  signum v = JanusC $ do
    RVal ev <- getJanusC v
    let tag = simdElemTag (Proxy @a)
        z = cInt 0
        one = if simdElemIsFloat (Proxy @a) then C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc else cInt 1
        negOne = UnOp Negate one noLoc
    r <- emitLaneLoopUn @n @a (\x -> if simdElemSigned (Proxy @a) then (if tag == JanusElemFloat || tag == JanusElemDouble then Cond (BinOp Gt x z noLoc) one (Cond (BinOp Lt x z noLoc) negOne x noLoc) noLoc else BinOp Sub (BinOp Gt x z noLoc) (BinOp Lt x z noLoc) noLoc) else Cond (BinOp Gt x z noLoc) (cInt 1) z noLoc) ev
    pure (RVal r)
  fromInteger k = vsplat (fromInteger k)

-- 4. ExpSIMDArith
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith LinuxX86_64C n a where
  vadd = liftX86Bin @n @a (x86Add128 @a) (Just (x86Add256 @a))
  vsub = liftX86Bin @n @a (x86Sub128 @a) (Just (x86Sub256 @a))
  vmul = liftX86Bin @n @a (x86Mul128 @a) (Just (x86Mul256 @a))
  vneg v
    | simdElemIsFloat (Proxy @a) =
        let signMask128 _ x =
              pure $
                if simdElemTag (Proxy @a) == JanusElemFloat
                  then cCall "_mm_xor_ps" [x, cCall "_mm_set1_ps" [C.Const (FloatConst "-0.0f" (-0.0) noLoc) noLoc]]
                  else cCall "_mm_xor_pd" [x, cCall "_mm_set1_pd" [C.Const (DoubleConst "-0.0" (-0.0) noLoc) noLoc]]
            signMask256 _ x =
              pure $
                if simdElemTag (Proxy @a) == JanusElemFloat
                  then cCall "_mm256_xor_ps" [x, cCall "_mm256_set1_ps" [C.Const (FloatConst "-0.0f" (-0.0) noLoc) noLoc]]
                  else cCall "_mm256_xor_pd" [x, cCall "_mm256_set1_pd" [C.Const (DoubleConst "-0.0" (-0.0) noLoc) noLoc]]
         in liftX86Un @n @a signMask128 (Just signMask256) v
    | otherwise = vsub vzero v

  vabs v
    | not (simdElemSigned (Proxy @a)) = v
    | simdElemTag (Proxy @a) == JanusElemFloat =
        liftX86Un @n @a
          (\_ x -> pure $ cCall "_mm_and_ps" [x, cCall "_mm_castsi128_ps" [cCall "_mm_set1_epi32" [cInt 0x7FFFFFFF]]])
          (Just (\_ x -> pure $ cCall "_mm256_and_ps" [x, cCall "_mm256_castsi256_ps" [cCall "_mm256_set1_epi32" [cInt 0x7FFFFFFF]]]))
          v
    | simdElemTag (Proxy @a) == JanusElemDouble =
        liftX86Un @n @a
          (\_ x -> pure $ cCall "_mm_and_pd" [x, cCall "_mm_castsi128_pd" [cCall "_mm_set1_epi64x" [signedIntConst (0x7FFFFFFFFFFFFFFF :: Int64)]]])
          (Just (\_ x -> pure $ cCall "_mm256_and_pd" [x, cCall "_mm256_castsi256_pd" [cCall "_mm256_set1_epi64x" [signedIntConst (0x7FFFFFFFFFFFFFFF :: Int64)]]]))
          v
    | otherwise =
        let abs128 fs x
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == JanusElemInt8 = pure $ cCall "_mm_abs_epi8" [x]
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == JanusElemInt16 = pure $ cCall "_mm_abs_epi16" [x]
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == JanusElemInt32 = pure $ cCall "_mm_abs_epi32" [x]
              | otherwise = do
                  negX <- x86Sub128 @a fs (x86Zero128 @a) x
                  m <- x86Cmp128 @a Lt fs x (x86Zero128 @a)
                  pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, negX], cCall "_mm_andnot_si128" [m, x]]
            abs256 _ x = case simdElemTag (Proxy @a) of
              JanusElemInt8 -> pure $ cCall "_mm256_abs_epi8" [x]
              JanusElemInt16 -> pure $ cCall "_mm256_abs_epi16" [x]
              JanusElemInt32 -> pure $ cCall "_mm256_abs_epi32" [x]
              _ -> do
                lo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [x])
                hi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [x, cInt 1])
                rlo <- abs128 x86AVX2 lo
                rhi <- abs128 x86AVX2 hi
                pure $ cCall "_mm256_set_m128i" [rhi, rlo]
         in liftX86Un @n @a abs128 (Just abs256) v

  vmin a b =
    let min128 fs x y = case simdElemTag (Proxy @a) of
          JanusElemFloat -> pure $ cCall "_mm_min_ps" [x, y]
          JanusElemDouble -> pure $ cCall "_mm_min_pd" [x, y]
          JanusElemWord8 -> pure $ cCall "_mm_min_epu8" [x, y]
          JanusElemInt16 -> pure $ cCall "_mm_min_epi16" [x, y]
          JanusElemInt8 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epi8" [x, y]
          JanusElemWord16 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epu16" [x, y]
          JanusElemInt32 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epi32" [x, y]
          JanusElemWord32 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epu32" [x, y]
          _ -> do
            m <- x86Cmp128 @a Lt fs x y
            pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, x], cCall "_mm_andnot_si128" [m, y]]
        min256 _ x y = case simdElemTag (Proxy @a) of
          JanusElemFloat -> pure $ cCall "_mm256_min_ps" [x, y]
          JanusElemDouble -> pure $ cCall "_mm256_min_pd" [x, y]
          JanusElemInt8 -> pure $ cCall "_mm256_min_epi8" [x, y]
          JanusElemWord8 -> pure $ cCall "_mm256_min_epu8" [x, y]
          JanusElemInt16 -> pure $ cCall "_mm256_min_epi16" [x, y]
          JanusElemWord16 -> pure $ cCall "_mm256_min_epu16" [x, y]
          JanusElemInt32 -> pure $ cCall "_mm256_min_epi32" [x, y]
          JanusElemWord32 -> pure $ cCall "_mm256_min_epu32" [x, y]
          _ -> do
            lo <- min128 x86AVX2 (cCall "_mm256_castsi256_si128" [x]) (cCall "_mm256_castsi256_si128" [y])
            hi <- min128 x86AVX2 (cCall "_mm256_extracti128_si256" [x, cInt 1]) (cCall "_mm256_extracti128_si256" [y, cInt 1])
            pure $ cCall "_mm256_set_m128i" [hi, lo]
     in liftX86Bin @n @a min128 (Just min256) a b

  vmax a b =
    let max128 fs x y = case simdElemTag (Proxy @a) of
          JanusElemFloat -> pure $ cCall "_mm_max_ps" [x, y]
          JanusElemDouble -> pure $ cCall "_mm_max_pd" [x, y]
          JanusElemWord8 -> pure $ cCall "_mm_max_epu8" [x, y]
          JanusElemInt16 -> pure $ cCall "_mm_max_epi16" [x, y]
          JanusElemInt8 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epi8" [x, y]
          JanusElemWord16 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epu16" [x, y]
          JanusElemInt32 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epi32" [x, y]
          JanusElemWord32 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epu32" [x, y]
          _ -> do
            m <- x86Cmp128 @a Gt fs x y
            pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, x], cCall "_mm_andnot_si128" [m, y]]
        max256 _ x y = case simdElemTag (Proxy @a) of
          JanusElemFloat -> pure $ cCall "_mm256_max_ps" [x, y]
          JanusElemDouble -> pure $ cCall "_mm256_max_pd" [x, y]
          JanusElemInt8 -> pure $ cCall "_mm256_max_epi8" [x, y]
          JanusElemWord8 -> pure $ cCall "_mm256_max_epu8" [x, y]
          JanusElemInt16 -> pure $ cCall "_mm256_max_epi16" [x, y]
          JanusElemWord16 -> pure $ cCall "_mm256_max_epu16" [x, y]
          JanusElemInt32 -> pure $ cCall "_mm256_max_epi32" [x, y]
          JanusElemWord32 -> pure $ cCall "_mm256_max_epu32" [x, y]
          _ -> do
            lo <- max128 x86AVX2 (cCall "_mm256_castsi256_si128" [x]) (cCall "_mm256_castsi256_si128" [y])
            hi <- max128 x86AVX2 (cCall "_mm256_extracti128_si256" [x, cInt 1]) (cCall "_mm256_extracti128_si256" [y, cInt 1])
            pure $ cCall "_mm256_set_m128i" [hi, lo]
     in liftX86Bin @n @a max128 (Just max256) a b

-- 5. ExpSIMDSat
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat LinuxX86_64C n a where
  vadds a b
    | simdElemIsFloat (Proxy @a) = vadd a b
    | simdElemTag (Proxy @a) `elem` [JanusElemInt8, JanusElemWord8, JanusElemInt16, JanusElemWord16] =
        let adds128 _ x y = pure $ case simdElemTag (Proxy @a) of
              JanusElemInt8 -> cCall "_mm_adds_epi8" [x, y]
              JanusElemWord8 -> cCall "_mm_adds_epu8" [x, y]
              JanusElemInt16 -> cCall "_mm_adds_epi16" [x, y]
              _ -> cCall "_mm_adds_epu16" [x, y]
            adds256 _ x y = pure $ case simdElemTag (Proxy @a) of
              JanusElemInt8 -> cCall "_mm256_adds_epi8" [x, y]
              JanusElemWord8 -> cCall "_mm256_adds_epu8" [x, y]
              JanusElemInt16 -> cCall "_mm256_adds_epi16" [x, y]
              _ -> cCall "_mm256_adds_epu16" [x, y]
         in liftX86Bin @n @a adds128 (Just adds256) a b
    | not (simdElemSigned (Proxy @a)) =
        -- Unsigned saturating add: sum = x + y; sum < x ? ~0 : sum
        JanusC $ do
          RVal ea <- getJanusC a
          RVal eb <- getJanusC b
          RVal <$> emitLaneLoopBin @n @a (\x y -> Cond (BinOp Lt (BinOp Add x y noLoc) x noLoc) (UnOp Not (cInt 0) noLoc) (BinOp Add x y noLoc) noLoc) ea eb
    | otherwise =
        JanusC $ do
          RVal ea <- getJanusC a
          RVal eb <- getJanusC b
          let bits = simdElemBytes (Proxy @a) * 8
              mx = if bits == 64 then cVar "INT64_MAX" else signedIntConst ((1 `shiftL` (bits - 1)) - 1 :: Int64)
              mn = if bits == 64 then cVar "INT64_MIN" else signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64)
              body x y =
                let s = BinOp Add (cCast "__int128" x) (cCast "__int128" y) noLoc
                 in cCast (simdElemCTypeName (Proxy @a)) (Cond (BinOp Gt s (cCast "__int128" mx) noLoc) (cCast "__int128" mx) (Cond (BinOp Lt s (cCast "__int128" mn) noLoc) (cCast "__int128" mn) s noLoc) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vsubs a b
    | simdElemIsFloat (Proxy @a) = vsub a b
    | simdElemTag (Proxy @a) `elem` [JanusElemInt8, JanusElemWord8, JanusElemInt16, JanusElemWord16] =
        let subs128 _ x y = pure $ case simdElemTag (Proxy @a) of
              JanusElemInt8 -> cCall "_mm_subs_epi8" [x, y]
              JanusElemWord8 -> cCall "_mm_subs_epu8" [x, y]
              JanusElemInt16 -> cCall "_mm_subs_epi16" [x, y]
              _ -> cCall "_mm_subs_epu16" [x, y]
            subs256 _ x y = pure $ case simdElemTag (Proxy @a) of
              JanusElemInt8 -> cCall "_mm256_subs_epi8" [x, y]
              JanusElemWord8 -> cCall "_mm256_subs_epu8" [x, y]
              JanusElemInt16 -> cCall "_mm256_subs_epi16" [x, y]
              _ -> cCall "_mm256_subs_epu16" [x, y]
         in liftX86Bin @n @a subs128 (Just subs256) a b
    | not (simdElemSigned (Proxy @a)) = vsub (vmax a b) b
    | otherwise =
        JanusC $ do
          RVal ea <- getJanusC a
          RVal eb <- getJanusC b
          let bits = simdElemBytes (Proxy @a) * 8
              mx = if bits == 64 then cVar "INT64_MAX" else signedIntConst ((1 `shiftL` (bits - 1)) - 1 :: Int64)
              mn = if bits == 64 then cVar "INT64_MIN" else signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64)
              body x y =
                let d = BinOp Sub (cCast "__int128" x) (cCast "__int128" y) noLoc
                 in cCast (simdElemCTypeName (Proxy @a)) (Cond (BinOp Gt d (cCast "__int128" mx) noLoc) (cCast "__int128" mx) (Cond (BinOp Lt d (cCast "__int128" mn) noLoc) (cCast "__int128" mn) d noLoc) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vabss v
    | not (simdElemSigned (Proxy @a)) || simdElemIsFloat (Proxy @a) = vabs v
    | otherwise = JanusC $ do
        RVal ev <- getJanusC v
        let bits = simdElemBytes (Proxy @a) * 8
            sminE = if bits == 64 then cVar "INT64_MIN" else cCast (simdElemCTypeName (Proxy @a)) (signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64))
            smaxE = if bits == 64 then cVar "INT64_MAX" else cCast (simdElemCTypeName (Proxy @a)) (signedIntConst (((1 `shiftL` (bits - 1)) - 1) :: Int64))
            absBody x = Cond (BinOp Eq x sminE noLoc) smaxE (Cond (BinOp Lt x (cInt 0) noLoc) (UnOp Negate x noLoc) x noLoc) noLoc
        RVal <$> emitLaneLoopUn @n @a absBody ev

  vavg a b
    | simdElemIsFloat (Proxy @a) =
        let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
            halfLit = if isF32 then C.Const (FloatConst "0.5f" 0.5 noLoc) noLoc else C.Const (DoubleConst "0.5" 0.5 noLoc) noLoc
            avg128 _ x y =
              pure $
                if isF32
                  then cCall "_mm_mul_ps" [cCall "_mm_set1_ps" [halfLit], cCall "_mm_add_ps" [x, y]]
                  else cCall "_mm_mul_pd" [cCall "_mm_set1_pd" [halfLit], cCall "_mm_add_pd" [x, y]]
            avg256 _ x y =
              pure $
                if isF32
                  then cCall "_mm256_mul_ps" [cCall "_mm256_set1_ps" [halfLit], cCall "_mm256_add_ps" [x, y]]
                  else cCall "_mm256_mul_pd" [cCall "_mm256_set1_pd" [halfLit], cCall "_mm256_add_pd" [x, y]]
         in liftX86Bin @n @a avg128 (Just avg256) a b
    | simdElemTag (Proxy @a) == JanusElemWord8 =
        liftX86Bin @n @a (\_ x y -> pure $ cCall "_mm_avg_epu8" [x, y]) (Just (\_ x y -> pure $ cCall "_mm256_avg_epu8" [x, y])) a b
    | simdElemTag (Proxy @a) == JanusElemWord16 =
        liftX86Bin @n @a (\_ x y -> pure $ cCall "_mm_avg_epu16" [x, y]) (Just (\_ x y -> pure $ cCall "_mm256_avg_epu16" [x, y])) a b
    | simdElemTag (Proxy @a) == JanusElemInt8 =
        let avgI8_128 _ x y = do
              let bs = cCall "_mm_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))]
              pure $ cCall "_mm_xor_si128" [cCall "_mm_avg_epu8" [cCall "_mm_xor_si128" [x, bs], cCall "_mm_xor_si128" [y, bs]], bs]
            avgI8_256 _ x y = do
              let bs = cCall "_mm256_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))]
              pure $ cCall "_mm256_xor_si256" [cCall "_mm256_avg_epu8" [cCall "_mm256_xor_si256" [x, bs], cCall "_mm256_xor_si256" [y, bs]], bs]
         in liftX86Bin @n @a avgI8_128 (Just avgI8_256) a b
    | simdElemTag (Proxy @a) == JanusElemInt16 =
        let avgI16_128 _ x y = do
              let bs = cCall "_mm_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))]
              pure $ cCall "_mm_xor_si128" [cCall "_mm_avg_epu16" [cCall "_mm_xor_si128" [x, bs], cCall "_mm_xor_si128" [y, bs]], bs]
            avgI16_256 _ x y = do
              let bs = cCall "_mm256_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))]
              pure $ cCall "_mm256_xor_si256" [cCall "_mm256_avg_epu16" [cCall "_mm256_xor_si256" [x, bs], cCall "_mm256_xor_si256" [y, bs]], bs]
         in liftX86Bin @n @a avgI16_128 (Just avgI16_256) a b
    | otherwise =
        JanusC $ do
          RVal ea <- getJanusC a
          RVal eb <- getJanusC b
          let wideTy = if simdElemSigned (Proxy @a) then "__int128" else "unsigned __int128"
              body x y = cCast (simdElemCTypeName (Proxy @a)) (BinOp Rsh (BinOp Add (BinOp Add (cCast wideTy x) (cCast wideTy y) noLoc) (cInt 1) noLoc) (cInt 1) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vabsdiff a b
    | simdElemIsFloat (Proxy @a) = vabs (vsub a b)
    | simdElemTag (Proxy @a) == JanusElemWord8 = vor (vsubs a b) (vsubs b a)
    | simdElemTag (Proxy @a) == JanusElemWord16 = vor (vsubs a b) (vsubs b a)
    | otherwise = vsub (vmax a b) (vmin a b)
