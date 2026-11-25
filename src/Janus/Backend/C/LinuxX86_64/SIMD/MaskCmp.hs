{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.SIMD.MaskCmp where

import Control.Monad (forM_)
import Data.Bits (Bits (..))
import Data.Int (Int32)
import Data.Loc
import Data.Proxy
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Word (Word32, Word64)
import Janus.Backend.C.Raw
import Janus.Backend.C.LinuxX86_64.SIMD.Base
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.Cast (janusCReallyUnsafeCast)
import Janus.Expression.Eq (eq, ne)
import Janus.Expression.Inject (inject)
import Janus.Expression.SIMD
import Language.C.Quote as C

instance (SIMDShape n a) => Join (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  (\/) = mor

instance (SIMDShape n a) => Meet (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  (/\) = mand

instance (SIMDShape n a) => Lower (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  lowerBound = mfalse

instance (SIMDShape n a) => Upper (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  upperBound = mtrue

-- 2. ExpSIMDMask
maskBinX86 ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  (Proxy a -> Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a)
maskBinX86 op128i op256 (JanusC ma) (JanusC mb) =
  JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32
        | x86Has256Reg fs pa -> emitLocalVal ty (op256 pa ea eb)
        | otherwise -> do
            rlo <- fromXMM128 @a 16 (fromM128i pa (op128i (asM128i pa (cMember ea "lo")) (asM128i pa (cMember eb "lo"))))
            rhi <- fromXMM128 @a 16 (fromM128i pa (op128i (asM128i pa (cMember ea "hi")) (asM128i pa (cMember eb "hi"))))
            emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xb <- toXMM128 @a b eb
        fromXMM128 @a b (fromM128i pa (op128i (asM128i pa xa) (asM128i pa xb)))
    pure (RVal r)

instance (SIMDShape n a) => ExpSIMDMask LinuxX86_64C n a where
  mfalse = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    let b = simdBytes (Proxy @n) (Proxy @a)
    r <- case b of
      4 -> emitLocalVal ty (unsignedIntConst (0 :: Word32))
      8 -> emitLocalVal ty (unsignedIntConst (0 :: Word64))
      16 -> emitLocalVal ty (x86Zero128 @a)
      32
        | x86Has256Reg fs (Proxy @a) -> emitLocalVal ty (x86Zero256 @a)
        | otherwise -> do
            z <- emitLocalVal (namedCType (x86128RegName (Proxy @a))) (x86Zero128 @a)
            emitPairStruct ty z z
      _ -> error "mfalse: invalid size"
    pure (RVal r)

  mtrue = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
        ones128 = fromM128i pa (cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)])
        ones256 = fromM256i pa (cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)])
    r <- case b of
      4 -> emitLocalVal ty (unsignedIntConst (0xFFFFFFFF :: Word32))
      8 -> emitLocalVal ty (unsignedIntConst (0xFFFFFFFFFFFFFFFF :: Word64))
      16 -> emitLocalVal ty ones128
      32
        | x86Has256Reg fs pa -> emitLocalVal ty ones256
        | otherwise -> do
            o <- emitLocalVal (namedCType (x86128RegName pa)) ones128
            emitPairStruct ty o o
      _ -> error "mtrue: invalid size"
    pure (RVal r)

  mand = maskBinX86 (\x y -> cCall "_mm_and_si128" [x, y]) (\p x y -> case simdElemTag p of JanusElemFloat -> cCall "_mm256_and_ps" [x, y]; JanusElemDouble -> cCall "_mm256_and_pd" [x, y]; _ -> cCall "_mm256_and_si256" [x, y])
  mor = maskBinX86 (\x y -> cCall "_mm_or_si128" [x, y]) (\p x y -> case simdElemTag p of JanusElemFloat -> cCall "_mm256_or_ps" [x, y]; JanusElemDouble -> cCall "_mm256_or_pd" [x, y]; _ -> cCall "_mm256_or_si256" [x, y])
  mxor = maskBinX86 (\x y -> cCall "_mm_xor_si128" [x, y]) (\p x y -> case simdElemTag p of JanusElemFloat -> cCall "_mm256_xor_ps" [x, y]; JanusElemDouble -> cCall "_mm256_xor_pd" [x, y]; _ -> cCall "_mm256_xor_si256" [x, y])
  mandnot = maskBinX86 (\x y -> cCall "_mm_andnot_si128" [x, y]) (\p x y -> case simdElemTag p of JanusElemFloat -> cCall "_mm256_andnot_ps" [x, y]; JanusElemDouble -> cCall "_mm256_andnot_pd" [x, y]; _ -> cCall "_mm256_andnot_si256" [x, y])
  mnot m = mxor m mtrue

  vselect (JanusC mm) (JanusC mt) (JanusC mf) =
    JanusC $ do
      ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      fs <- askX86Features
      RVal em0 <- mm
      RVal et0 <- mt
      RVal ef0 <- mf
      em <- emitLocalVal mty em0
      et <- emitLocalVal ty et0
      ef <- emitLocalVal ty ef0
      let b = simdBytes (Proxy @n) (Proxy @a)
          pa = Proxy @a
          sel128 xm xt xf
            | hasX86Feature fs X86_SSE4_1 = case simdElemTag pa of
                JanusElemFloat -> cCall "_mm_blendv_ps" [xf, xt, xm]
                JanusElemDouble -> cCall "_mm_blendv_pd" [xf, xt, xm]
                _ -> cCall "_mm_blendv_epi8" [xf, xt, xm]
            | otherwise =
                fromM128i pa $
                  cCall
                    "_mm_or_si128"
                    [ cCall "_mm_and_si128" [asM128i pa xm, asM128i pa xt],
                      cCall "_mm_andnot_si128" [asM128i pa xm, asM128i pa xf]
                    ]
      r <- case b of
        32
          | x86Has256Reg fs pa -> do
              let e256 = case simdElemTag pa of
                    JanusElemFloat -> cCall "_mm256_blendv_ps" [ef, et, em]
                    JanusElemDouble -> cCall "_mm256_blendv_pd" [ef, et, em]
                    _ -> cCall "_mm256_blendv_epi8" [ef, et, em]
              emitLocalVal ty e256
          | otherwise -> do
              rlo <- fromXMM128 @a 16 (sel128 (cMember em "lo") (cMember et "lo") (cMember ef "lo"))
              rhi <- fromXMM128 @a 16 (sel128 (cMember em "hi") (cMember et "hi") (cMember ef "hi"))
              emitPairStruct ty rlo rhi
        _ -> do
          xm <- toXMM128 @a b em
          xt <- toXMM128 @a b et
          xf <- toXMM128 @a b ef
          fromXMM128 @a b (sel128 xm xt xf)
      pure (RVal r)

  mmovemask (JanusC mm) = JanusC $ do
    mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    RVal em0 <- mm
    em <- emitLocalVal mty em0
    let sz = simdElemBytes (Proxy @a)
        lanes = simdLanes (Proxy @n) (Proxy @a)
        b = lanes * sz
        lmask = if lanes == 32 then 0xFFFFFFFF :: Word32 else (1 `shiftL` lanes) - 1
    case (sz, b) of
      (1, _) | b <= 16 -> do
        xm <- toXMM128 @a b em
        pure . RVal $ BinOp And (cCast "uint32_t" (cCall "_mm_movemask_epi8" [asM128i (Proxy @a) xm])) (unsignedIntConst lmask) noLoc
      (1, 32) | hasX86Feature fs X86_AVX2 ->
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_epi8" [em])
      (4, _) | b <= 16 -> do
        xm <- toXMM128 @a b em
        let ps = if simdElemTag (Proxy @a) == JanusElemFloat then xm else cCall "_mm_castsi128_ps" [xm]
        pure . RVal $ BinOp And (cCast "uint32_t" (cCall "_mm_movemask_ps" [ps])) (unsignedIntConst lmask) noLoc
      (4, 32) | x86Has256Reg fs (Proxy @a) -> do
        let ps = if simdElemTag (Proxy @a) == JanusElemFloat then em else cCall "_mm256_castsi256_ps" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_ps" [ps])
      (8, 16) -> do
        let pd = if simdElemTag (Proxy @a) == JanusElemDouble then em else cCall "_mm_castsi128_pd" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm_movemask_pd" [pd])
      (8, 32) | x86Has256Reg fs (Proxy @a) -> do
        let pd = if simdElemTag (Proxy @a) == JanusElemDouble then em else cCall "_mm256_castsi256_pd" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_pd" [pd])
      _ -> do
        acc <- emitLocalVal (namedCType "uint32_t") (unsignedIntConst (0 :: Word32))
        let bytePtr = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
        forM_ [0 .. lanes - 1] $ \i -> do
          let msb = Index bytePtr (signedIntConst (i * sz + (sz - 1))) noLoc
              bitVal = Cond (BinOp And msb (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (bit i :: Word32)) (unsignedIntConst (0 :: Word32)) noLoc
          emitStmt $ Exp (Just $ Assign acc OrAssign bitVal noLoc) noLoc
        pure (RVal acc)

  mpopcount m = JanusC $ do
    fs <- askX86Features
    RVal bits <- getJanusC (mmovemask m)
    bvar <- emitLocalVal (namedCType "uint32_t") bits
    let e =
          if hasX86Feature fs X86_POPCNT
            then cCast "int32_t" (cCall "_mm_popcnt_u32" [bvar])
            else cCast "int32_t" (cCall "__builtin_popcount" [bvar])
    pure (RVal e)

  mfirstTrue m = JanusC $ do
    RVal bits <- getJanusC (mmovemask m)
    bvar <- emitLocalVal (namedCType "uint32_t") bits
    let e =
          Cond
            (BinOp Eq bvar (unsignedIntConst (0 :: Word32)) noLoc)
            (signedIntConst (-1 :: Int32))
            (cCast "int32_t" (cCall "__builtin_ctz" [bvar]))
            noLoc
    pure (RVal e)

  mallOf m =
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        full = if lanes == 32 then maxBound else (1 `shiftL` lanes) - 1
     in mmovemask m `eq` inject full
  manyOf m = mmovemask m `ne` inject 0
  mnoneOf m = mmovemask m `eq` inject 0

-- 3. ExpSIMDCmp
x86Cmp128 :: forall a. (SIMDElem a) => BinOp -> X86Features -> Exp -> Exp -> JanusCM Exp
x86Cmp128 cop fs a b = case simdElemTag (Proxy @a) of
  JanusElemFloat -> pure $ case cop of
    Eq -> cCall "_mm_cmpeq_ps" [a, b]
    Ne -> cCall "_mm_cmpneq_ps" [a, b]
    Lt -> cCall "_mm_cmplt_ps" [a, b]
    Le -> cCall "_mm_cmple_ps" [a, b]
    Gt -> cCall "_mm_cmpgt_ps" [a, b]
    _ -> cCall "_mm_cmpge_ps" [a, b]
  JanusElemDouble -> pure $ case cop of
    Eq -> cCall "_mm_cmpeq_pd" [a, b]
    Ne -> cCall "_mm_cmpneq_pd" [a, b]
    Lt -> cCall "_mm_cmplt_pd" [a, b]
    Le -> cCall "_mm_cmple_pd" [a, b]
    Gt -> cCall "_mm_cmpgt_pd" [a, b]
    _ -> cCall "_mm_cmpge_pd" [a, b]
  tag -> do
    let ones = cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)]
        notI x = cCall "_mm_xor_si128" [x, ones]
        eqI x y = case tag of
          JanusElemInt8 -> cCall "_mm_cmpeq_epi8" [x, y]
          JanusElemWord8 -> cCall "_mm_cmpeq_epi8" [x, y]
          JanusElemInt16 -> cCall "_mm_cmpeq_epi16" [x, y]
          JanusElemWord16 -> cCall "_mm_cmpeq_epi16" [x, y]
          JanusElemInt32 -> cCall "_mm_cmpeq_epi32" [x, y]
          JanusElemWord32 -> cCall "_mm_cmpeq_epi32" [x, y]
          _
            | hasX86Feature fs X86_SSE4_1 -> cCall "_mm_cmpeq_epi64" [x, y]
            | otherwise ->
                let c32 = cCall "_mm_cmpeq_epi32" [x, y]
                 in cCall "_mm_and_si128" [c32, cCall "_mm_shuffle_epi32" [c32, cInt 0xB1]]
        bias = case tag of
          JanusElemWord8 -> Just (cCall "_mm_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))])
          JanusElemWord16 -> Just (cCall "_mm_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))])
          JanusElemWord32 -> Just (cCall "_mm_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))])
          JanusElemWord64 -> Just (cCall "_mm_set1_epi64x" [cCast "long long" (unsignedIntConst (0x8000000000000000 :: Word64))])
          _ -> Nothing
        (ax, bx) = case bias of
          Just bs -> (cCall "_mm_xor_si128" [a, bs], cCall "_mm_xor_si128" [b, bs])
          Nothing -> (a, b)
        gtSigned x y = case tag of
          JanusElemInt8 -> pure $ cCall "_mm_cmpgt_epi8" [x, y]
          JanusElemWord8 -> pure $ cCall "_mm_cmpgt_epi8" [x, y]
          JanusElemInt16 -> pure $ cCall "_mm_cmpgt_epi16" [x, y]
          JanusElemWord16 -> pure $ cCall "_mm_cmpgt_epi16" [x, y]
          JanusElemInt32 -> pure $ cCall "_mm_cmpgt_epi32" [x, y]
          JanusElemWord32 -> pure $ cCall "_mm_cmpgt_epi32" [x, y]
          _
            | hasX86Feature fs X86_SSE4_2 -> pure $ cCall "_mm_cmpgt_epi64" [x, y]
            | otherwise -> do
                -- SSE2 64-bit signed > via 32-bit limbs
                let b64 = cCall "_mm_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))]
                xu <- emitLocalVal (namedCType "__m128i") (cCall "_mm_xor_si128" [x, b64])
                yu <- emitLocalVal (namedCType "__m128i") (cCall "_mm_xor_si128" [y, b64])
                gt32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpgt_epi32" [x, y])
                gtu32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpgt_epi32" [xu, yu])
                eq32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpeq_epi32" [x, y])
                let hiGt = cCall "_mm_shuffle_epi32" [gt32, cInt 0xF5]
                    hiEq = cCall "_mm_shuffle_epi32" [eq32, cInt 0xF5]
                    loGt = cCall "_mm_shuffle_epi32" [gtu32, cInt 0xA0]
                pure $ cCall "_mm_or_si128" [hiGt, cCall "_mm_and_si128" [hiEq, loGt]]
    case cop of
      Eq -> pure (eqI a b)
      Ne -> pure (notI (eqI a b))
      Gt -> gtSigned ax bx
      Lt -> gtSigned bx ax
      Ge -> notI <$> gtSigned bx ax
      _ -> notI <$> gtSigned ax bx

x86Cmp256 :: forall a. (SIMDElem a) => BinOp -> X86Features -> Exp -> Exp -> JanusCM Exp
x86Cmp256 cop _ a b = case simdElemTag (Proxy @a) of
  JanusElemFloat ->
    let imm = case cop of
          Eq -> "_CMP_EQ_OQ"
          Ne -> "_CMP_NEQ_UQ"
          Lt -> "_CMP_LT_OQ"
          Le -> "_CMP_LE_OQ"
          Gt -> "_CMP_GT_OQ"
          _ -> "_CMP_GE_OQ"
     in pure $ cCall "_mm256_cmp_ps" [a, b, cVar imm]
  JanusElemDouble ->
    let imm = case cop of
          Eq -> "_CMP_EQ_OQ"
          Ne -> "_CMP_NEQ_UQ"
          Lt -> "_CMP_LT_OQ"
          Le -> "_CMP_LE_OQ"
          Gt -> "_CMP_GT_OQ"
          _ -> "_CMP_GE_OQ"
     in pure $ cCall "_mm256_cmp_pd" [a, b, cVar imm]
  tag -> do
    let ones = cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]
        notI x = cCall "_mm256_xor_si256" [x, ones]
        eqI x y = case tag of
          JanusElemInt8 -> cCall "_mm256_cmpeq_epi8" [x, y]
          JanusElemWord8 -> cCall "_mm256_cmpeq_epi8" [x, y]
          JanusElemInt16 -> cCall "_mm256_cmpeq_epi16" [x, y]
          JanusElemWord16 -> cCall "_mm256_cmpeq_epi16" [x, y]
          JanusElemInt32 -> cCall "_mm256_cmpeq_epi32" [x, y]
          JanusElemWord32 -> cCall "_mm256_cmpeq_epi32" [x, y]
          _ -> cCall "_mm256_cmpeq_epi64" [x, y]
        bias = case tag of
          JanusElemWord8 -> Just (cCall "_mm256_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))])
          JanusElemWord16 -> Just (cCall "_mm256_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))])
          JanusElemWord32 -> Just (cCall "_mm256_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))])
          JanusElemWord64 -> Just (cCall "_mm256_set1_epi64x" [cCast "long long" (unsignedIntConst (0x8000000000000000 :: Word64))])
          _ -> Nothing
        (ax, bx) = case bias of
          Just bs -> (cCall "_mm256_xor_si256" [a, bs], cCall "_mm256_xor_si256" [b, bs])
          Nothing -> (a, b)
        gtSigned x y = case tag of
          JanusElemInt8 -> cCall "_mm256_cmpgt_epi8" [x, y]
          JanusElemWord8 -> cCall "_mm256_cmpgt_epi8" [x, y]
          JanusElemInt16 -> cCall "_mm256_cmpgt_epi16" [x, y]
          JanusElemWord16 -> cCall "_mm256_cmpgt_epi16" [x, y]
          JanusElemInt32 -> cCall "_mm256_cmpgt_epi32" [x, y]
          JanusElemWord32 -> cCall "_mm256_cmpgt_epi32" [x, y]
          _ -> cCall "_mm256_cmpgt_epi64" [x, y]
    pure $ case cop of
      Eq -> eqI a b
      Ne -> notI (eqI a b)
      Gt -> gtSigned ax bx
      Lt -> gtSigned bx ax
      Ge -> notI (gtSigned bx ax)
      _ -> notI (gtSigned ax bx)

instance (SIMDShape n a) => ExpSIMDCmp LinuxX86_64C n a where
  veq a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Eq) (Just (x86Cmp256 @a Eq)) a b)
  vne a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Ne) (Just (x86Cmp256 @a Ne)) a b)
  vlt a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Lt) (Just (x86Cmp256 @a Lt)) a b)
  vle a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Le) (Just (x86Cmp256 @a Le)) a b)
  vgt a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Gt) (Just (x86Cmp256 @a Gt)) a b)
  vge a b = janusCReallyUnsafeCast (liftX86Bin @n @a (x86Cmp128 @a Ge) (Just (x86Cmp256 @a Ge)) a b)
