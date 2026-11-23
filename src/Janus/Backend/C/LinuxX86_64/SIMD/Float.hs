{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.Float where

import Control.Monad (forM_)
import Data.Loc
import Data.Proxy
import GHC.Float (expm1, log1p)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.SIMD.Arith ()
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.Math (FMA (..), Hypot (..))
import Janus.Expression.SIMD
import Language.C.Quote as C

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => Fractional (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  (/) = vdiv
  recip x = fromRational 1 / x
  fromRational q = vsplat (fromRational q)

sleefSuffix128 :: X86Features -> String
sleefSuffix128 fs
  | hasX86Feature fs X86_AVX2 && hasX86Feature fs X86_FMA = "avx2128"
  | hasX86Feature fs X86_SSE4_1 = "sse4"
  | otherwise = "sse2"

sleefSuffix256 :: X86Features -> String
sleefSuffix256 fs
  | hasX86Feature fs X86_AVX2 && hasX86Feature fs X86_FMA = "avx2"
  | otherwise = "avx"

sleefUnOp ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
sleefUnOp opName ulp =
  let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
      w128 = if isF32 then "f4_" else "d2_"
      w256 = if isF32 then "f8_" else "d4_"
      fn128 fs x = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w128 <> ulp <> sleefSuffix128 fs) [x])
      fn256 fs x = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w256 <> ulp <> sleefSuffix256 fs) [x])
   in liftX86Un @n @a fn128 (Just fn256)

sleefBinOp ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
sleefBinOp opName ulp =
  let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
      w128 = if isF32 then "f4_" else "d2_"
      w256 = if isF32 then "f8_" else "d4_"
      fn128 fs x y = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w128 <> ulp <> sleefSuffix128 fs) [x, y])
      fn256 fs x y = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w256 <> ulp <> sleefSuffix256 fs) [x, y])
   in liftX86Bin @n @a fn128 (Just fn256)

instance (SIMDShape n Float) => Floating (LinuxX86_64C (SIMDVector LinuxX86_64C n Float)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp = sleefUnOp "exp" "u10"
  log = sleefUnOp "log" "u10"
  (**) = sleefBinOp "pow" "u10"
  logBase x y = log y / log x
  sin = sleefUnOp "sin" "u10"
  cos = sleefUnOp "cos" "u10"
  tan = sleefUnOp "tan" "u10"
  asin = sleefUnOp "asin" "u10"
  acos = sleefUnOp "acos" "u10"
  atan = sleefUnOp "atan" "u10"
  sinh = sleefUnOp "sinh" "u10"
  cosh = sleefUnOp "cosh" "u10"
  tanh = sleefUnOp "tanh" "u10"
  asinh = sleefUnOp "asinh" "u10"
  acosh = sleefUnOp "acosh" "u10"
  atanh = sleefUnOp "atanh" "u10"
  log1p = sleefUnOp "log1p" "u10"
  expm1 = sleefUnOp "expm1" "u10"

instance (SIMDShape n Double) => Floating (LinuxX86_64C (SIMDVector LinuxX86_64C n Double)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp = sleefUnOp "exp" "u10"
  log = sleefUnOp "log" "u10"
  (**) = sleefBinOp "pow" "u10"
  logBase x y = log y / log x
  sin = sleefUnOp "sin" "u10"
  cos = sleefUnOp "cos" "u10"
  tan = sleefUnOp "tan" "u10"
  asin = sleefUnOp "asin" "u10"
  acos = sleefUnOp "acos" "u10"
  atan = sleefUnOp "atan" "u10"
  sinh = sleefUnOp "sinh" "u10"
  cosh = sleefUnOp "cosh" "u10"
  tanh = sleefUnOp "tanh" "u10"
  asinh = sleefUnOp "asinh" "u10"
  acosh = sleefUnOp "acosh" "u10"
  atanh = sleefUnOp "atanh" "u10"
  log1p = sleefUnOp "log1p" "u10"
  expm1 = sleefUnOp "expm1" "u10"

instance {-# OVERLAPPING #-} (SIMDShape n Float) => Hypot (LinuxX86_64C (SIMDVector LinuxX86_64C n Float)) where
  hypot = sleefBinOp "hypot" "u05"

instance {-# OVERLAPPING #-} (SIMDShape n Double) => Hypot (LinuxX86_64C (SIMDVector LinuxX86_64C n Double)) where
  hypot = sleefBinOp "hypot" "u05"

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => FMA (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  fma = vfma

-- 6. ExpSIMDFloat
x86FmaOp ::
  forall n a.
  (SIMDShape n a, CPUSIMDElem a) =>
  String ->
  String ->
  (Exp -> Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
x86FmaOp fn128 fn256 scalarFallback (JanusC ma) (JanusC mb) (JanusC mc) =
  JanusC $ do
    addHeader "math.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    RVal ec0 <- mc
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    ec <- emitLocalVal ty ec0
    let b = simdBytes (Proxy @n) (Proxy @a)
    if hasX86Feature fs X86_FMA
      then case b of
        32 | x86Has256Reg fs (Proxy @a) -> RVal <$> emitLocalVal ty (cCall fn256 [ea, eb, ec])
        32 -> do
          rlo <- fromXMM128 @a 16 (cCall fn128 [cMember ea "lo", cMember eb "lo", cMember ec "lo"])
          rhi <- fromXMM128 @a 16 (cCall fn128 [cMember ea "hi", cMember eb "hi", cMember ec "hi"])
          RVal <$> emitPairStruct ty rlo rhi
        _ -> do
          xa <- toXMM128 @a b ea
          xb <- toXMM128 @a b eb
          xc <- toXMM128 @a b ec
          RVal <$> fromXMM128 @a b (cCall fn128 [xa, xb, xc])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            elemTyStr = simdElemCTypeName (Proxy @a)
            JCType espec _ = namedCType elemTyStr
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
            arrTy = JCType espec arrDec
        aa <- emitUninitLocal arrTy
        ab <- emitUninitLocal arrTy
        ac <- emitUninitLocal arrTy
        ar <- emitUninitLocal arrTy
        resVar <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst b]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst b]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ac, UnOp AddrOf ec noLoc, signedIntConst b]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
          emitStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (scalarFallback (Index aa idx noLoc) (Index ab idx noLoc) (Index ac idx noLoc)) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst b]) noLoc
        pure (RVal resVar)

instance (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a) => ExpSIMDFloat LinuxX86_64C n a where
  vdiv a b =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
        div128 _ x y
          | simdBytes (Proxy @n) (Proxy @a) < 16 = do
              -- Set unused upper lanes of divisor to 1.0f to remain safe even under FP exceptions
              let one = C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc
                  paddedY = cCall "_mm_move_ss" [cCall "_mm_set1_ps" [one], y]
                  paddedY2 = cCall "_mm_shuffle_ps" [y, cCall "_mm_set1_ps" [one], cInt 0xE4]
              pure $ cCall "_mm_div_ps" [x, if simdBytes (Proxy @n) (Proxy @a) == 8 then paddedY2 else paddedY]
          | otherwise = pure $ cCall (if isF32 then "_mm_div_ps" else "_mm_div_pd") [x, y]
        div256 _ x y = pure $ cCall (if isF32 then "_mm256_div_ps" else "_mm256_div_pd") [x, y]
     in liftX86Bin @n @a div128 (Just div256) a b

  vsqrt =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
     in liftX86Un @n @a
          (\_ x -> pure $ cCall (if isF32 then "_mm_sqrt_ps" else "_mm_sqrt_pd") [x])
          (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_sqrt_ps" else "_mm256_sqrt_pd") [x]))

  vrsqrtApprox v
    | simdElemTag (Proxy @a) == JanusElemFloat =
        liftX86Un @n @a (\_ x -> pure $ cCall "_mm_rsqrt_ps" [x]) (Just (\_ x -> pure $ cCall "_mm256_rsqrt_ps" [x])) v
    | otherwise =
        let oneD = C.Const (DoubleConst "1.0" 1.0 noLoc) noLoc
         in liftX86Un @n @a (\_ x -> pure $ cCall "_mm_div_pd" [cCall "_mm_set1_pd" [oneD], cCall "_mm_sqrt_pd" [x]]) (Just (\_ x -> pure $ cCall "_mm256_div_pd" [cCall "_mm256_set1_pd" [oneD], cCall "_mm256_sqrt_pd" [x]])) v

  vrcpApprox v
    | simdElemTag (Proxy @a) == JanusElemFloat =
        liftX86Un @n @a (\_ x -> pure $ cCall "_mm_rcp_ps" [x]) (Just (\_ x -> pure $ cCall "_mm256_rcp_ps" [x])) v
    | otherwise =
        let oneD = C.Const (DoubleConst "1.0" 1.0 noLoc) noLoc
         in liftX86Un @n @a (\_ x -> pure $ cCall "_mm_div_pd" [cCall "_mm_set1_pd" [oneD], x]) (Just (\_ x -> pure $ cCall "_mm256_div_pd" [cCall "_mm256_set1_pd" [oneD], x])) v

  vfma =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fmadd_ps" else "_mm_fmadd_pd") (if isF32 then "_mm256_fmadd_ps" else "_mm256_fmadd_pd") (\x y z -> cCall cFma [x, y, z])

  vfms =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fmsub_ps" else "_mm_fmsub_pd") (if isF32 then "_mm256_fmsub_ps" else "_mm256_fmsub_pd") (\x y z -> cCall cFma [x, y, UnOp Negate z noLoc])

  vfnma =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fnmadd_ps" else "_mm_fnmadd_pd") (if isF32 then "_mm256_fnmadd_ps" else "_mm256_fnmadd_pd") (\x y z -> cCall cFma [UnOp Negate x noLoc, y, z])

  vfnms =
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fnmsub_ps" else "_mm_fnmsub_pd") (if isF32 then "_mm256_fnmsub_ps" else "_mm256_fnmsub_pd") (\x y z -> cCall cFma [UnOp Negate x noLoc, y, UnOp Negate z noLoc])

  vmulAdd a b c =
    JanusC $ do
      fs <- askX86Features
      getJanusC $ if hasX86Feature fs X86_FMA then vfma a b c else vadd (vmul a b) c

  vfloor (JanusC mv) = JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_floor_ps" else "_mm_floor_pd") [x])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_floor_ps" else "_mm256_floor_pd") [x]))
              (JanusC mv)
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "floorf" else "floor") [x]) e

  vceil (JanusC mv) = JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_ceil_ps" else "_mm_ceil_pd") [x])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_ceil_ps" else "_mm256_ceil_pd") [x]))
              (JanusC mv)
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "ceilf" else "ceil") [x]) e

  vtrunc (JanusC mv) = JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_round_ps" else "_mm_round_pd") [x, cInt 11])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_round_ps" else "_mm256_round_pd") [x, cInt 11]))
              (JanusC mv)
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "truncf" else "trunc") [x]) e

  vroundNearest (JanusC mv) = JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == JanusElemFloat
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_round_ps" else "_mm_round_pd") [x, cInt 8])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_round_ps" else "_mm256_round_pd") [x, cInt 8]))
              (JanusC mv)
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "nearbyintf" else "nearbyint") [x]) e
