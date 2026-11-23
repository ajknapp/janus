{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.Float where

import Control.Monad (forM_)
import Data.Loc
import Data.Proxy
import Janus.Backend.C
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.SIMD.Arith ()
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.Math (FMA (..), Hypot (..))
import Janus.Expression.SIMD
import Language.C.Quote as C

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => Fractional (JanusCUDA (SIMDVector JanusCUDA n a)) where
  (/) = vdiv
  recip x = fromRational 1 / x
  fromRational q = vsplat (fromRational q)

instance (SIMDShape n Float) => Floating (JanusCUDA (SIMDVector JanusCUDA n Float)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "expf" [x]) e
  log (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "logf" [x]) e
  (JanusCUDA (JanusC ma)) ** (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Float (\x y -> cuCall "powf" [x, y]) a b
  sin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "sinf" [x]) e
  cos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "cosf" [x]) e
  tan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "tanf" [x]) e
  asin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "asinf" [x]) e
  acos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "acosf" [x]) e
  atan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "atanf" [x]) e
  sinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "sinhf" [x]) e
  cosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "coshf" [x]) e
  tanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "tanhf" [x]) e
  asinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "asinhf" [x]) e
  acosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "acoshf" [x]) e
  atanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "atanhf" [x]) e

instance (SIMDShape n Double) => Floating (JanusCUDA (SIMDVector JanusCUDA n Double)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "exp" [x]) e
  log (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "log" [x]) e
  (JanusCUDA (JanusC ma)) ** (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Double (\x y -> cuCall "pow" [x, y]) a b
  sin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "sin" [x]) e
  cos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "cos" [x]) e
  tan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "tan" [x]) e
  asin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "asin" [x]) e
  acos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "acos" [x]) e
  atan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "atan" [x]) e
  sinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "sinh" [x]) e
  cosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "cosh" [x]) e
  tanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "tanh" [x]) e
  asinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "asinh" [x]) e
  acosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "acosh" [x]) e
  atanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "atanh" [x]) e

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => FMA (JanusCUDA (SIMDVector JanusCUDA n a)) where
  fma = vfma

instance {-# OVERLAPPING #-} (SIMDShape n Float) => Hypot (JanusCUDA (SIMDVector JanusCUDA n Float)) where
  hypot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Float (\x y -> cuCall "hypotf" [x, y]) a b

instance {-# OVERLAPPING #-} (SIMDShape n Double) => Hypot (JanusCUDA (SIMDVector JanusCUDA n Double)) where
  hypot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Double (\x y -> cuCall "hypot" [x, y]) a b

-- 6. ExpSIMDFloat JanusCUDA
emitCUDAFloatTern ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a)
emitCUDAFloatTern fn (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
  JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    RVal ec0 <- mc
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    ec <- emitLocalVal ty ec0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    aa <- cuUninit arrTy
    ab <- cuUninit arrTy
    ac <- cuUninit arrTy
    ar <- cuUninit arrTy
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ac, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let idx = signedIntConst i
      cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc) (Index ab idx noLoc) (Index ac idx noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

instance (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a) => ExpSIMDFloat JanusCUDA n a where
  vdiv (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @a (\x y -> BinOp Div x y noLoc) a b

  vsqrt (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn = if simdElemTag (Proxy @a) == JanusElemFloat then "sqrtf" else "sqrt"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall fn [x]) a

  vrsqrtApprox (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn = if simdElemTag (Proxy @a) == JanusElemFloat then "rsqrtf" else "rsqrt"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall fn [x]) a

  vrcpApprox (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn x = if simdElemTag (Proxy @a) == JanusElemFloat then cuCall "__frcp_rn" [x] else cuCall "__drcp_rn" [x]
    RVal <$> emitCUDALaneUn @n @a fn a

  vfma =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [x, y, z])
  vfms =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [x, y, UnOp Negate z noLoc])
  vfnma =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [UnOp Negate x noLoc, y, z])
  vfnms =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [UnOp Negate x noLoc, y, UnOp Negate z noLoc])
  vmulAdd = vfma

  vfloor (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "floorf" else "floor"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vceil (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "ceilf" else "ceil"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vtrunc (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "truncf" else "trunc"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vroundNearest (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "rintf" else "rint"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
