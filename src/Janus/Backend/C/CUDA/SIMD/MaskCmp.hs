{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.MaskCmp where

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
import Janus.Backend.C
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.Types
import Janus.Expression.Eq (eq, ne)
import Janus.Expression.Inject (inject)
import Janus.Expression.SIMD
import Language.C.Quote as C

instance (SIMDShape n a) => Join (JanusCUDA (SIMDMask JanusCUDA n a)) where (\/) = mor
instance (SIMDShape n a) => Meet (JanusCUDA (SIMDMask JanusCUDA n a)) where (/\) = mand
instance (SIMDShape n a) => Lower (JanusCUDA (SIMDMask JanusCUDA n a)) where lowerBound = mfalse
instance (SIMDShape n a) => Upper (JanusCUDA (SIMDMask JanusCUDA n a)) where upperBound = mtrue

-- 2. ExpSIMDMask JanusCUDA
instance (SIMDShape n a) => ExpSIMDMask JanusCUDA n a where
  mfalse = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    out <- cuUninit mty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  mtrue = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    out <- cuUninit mty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0xFF, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  mand (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp And x y noLoc) a b

  mor (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp Or x y noLoc) a b

  mxor (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp Xor x y noLoc) a b

  mandnot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp And (UnOp Not x noLoc) y noLoc) a b

  mnot (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal <$> emitCUDAPackedWordUn @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x -> UnOp Not x noLoc) a

  vselect (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mt)) (JanusCUDA (JanusC mf)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      RVal em0 <- mm
      RVal et0 <- mt
      RVal ef0 <- mf
      em <- emitLocalVal mty em0
      et <- emitLocalVal vty et0
      ef <- emitLocalVal vty ef0
      let bytes = simdBytes (Proxy @n) (Proxy @a)
          words32 = bytes `div` 4
          JCType u32spec _ = cuNamedType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
          arrTy = JCType u32spec arrDec
      mw <- cuUninit arrTy
      tw <- cuUninit arrTy
      fw <- cuUninit arrTy
      rw <- cuUninit arrTy
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [mw, UnOp AddrOf em noLoc, signedIntConst bytes]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [tw, UnOp AddrOf et noLoc, signedIntConst bytes]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [fw, UnOp AddrOf ef noLoc, signedIntConst bytes]) noLoc
      forM_ [0 .. words32 - 1] $ \w -> do
        let wi = signedIntConst w
            m = Index mw wi noLoc
            t = Index tw wi noLoc
            f = Index fw wi noLoc
            lop3 = BinOp Or (BinOp And m t noLoc) (BinOp And (UnOp Not m noLoc) f noLoc) noLoc
        cuStmt $ Exp (Just $ Assign (Index rw wi noLoc) JustAssign lop3 noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst bytes]) noLoc
      pure (RVal out)

  mmovemask (JanusCUDA (JanusC mm)) = JanusCUDA $ JanusC $ do
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    RVal em0 <- mm
    em <- emitLocalVal mty em0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        bytePtr = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
    acc <- emitLocalVal (cuNamedType "uint32_t") (unsignedIntConst (0 :: Word32))
    forM_ [0 .. lanes - 1] $ \i -> do
      let msb = Index bytePtr (signedIntConst (i * sz + (sz - 1))) noLoc
          bitE = Cond (BinOp And msb (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (bit i :: Word32)) (unsignedIntConst (0 :: Word32)) noLoc
      cuStmt $ Exp (Just $ Assign acc OrAssign bitE noLoc) noLoc
    pure (RVal acc)

  mpopcount m = JanusCUDA $ JanusC $ do
    RVal bits <- getJanusC (getJanusCUDA (mmovemask m))
    pure . RVal $ cuCast "int32_t" (cuCall "__popc" [bits])

  mfirstTrue m = JanusCUDA $ JanusC $ do
    RVal bits <- getJanusC (getJanusCUDA (mmovemask m))
    bvar <- emitLocalVal (cuNamedType "uint32_t") bits
    -- CUDA __ffs(0) == 0, and __ffs(x) is 1-based for x != 0; subtracting 1 yields -1 for 0 and 0-based index otherwise!
    pure . RVal $ BinOp Sub (cuCast "int32_t" (cuCall "__ffs" [bvar])) (signedIntConst (1 :: Int32)) noLoc

  mallOf m =
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        full = if lanes == 32 then maxBound else (1 `shiftL` lanes) - 1
     in mmovemask m `eq` inject full
  manyOf m = mmovemask m `ne` inject 0
  mnoneOf m = mmovemask m `eq` inject 0

-- 3. ExpSIMDCmp JanusCUDA (using native __vcmp*2 / __vcmp*4 for 8-bit & 16-bit!)
emitCUDACmpFallback ::
  forall n a.
  (SIMDShape n a) =>
  BinOp ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDACmpFallback cop ea0 eb0 = do
  addHeader "string.h"
  vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
  ea <- emitLocalVal vty ea0
  eb <- emitLocalVal vty eb0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      sz = simdElemBytes (Proxy @a)
      bytes = lanes * sz
      uMaskTy = "uint" <> show (sz * 8) <> "_t"
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      JCType mspec _ = cuNamedType uMaskTy
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  aa <- cuUninit (JCType espec arrDec)
  ab <- cuUninit (JCType espec arrDec)
  am <- cuUninit (JCType mspec arrDec)
  out <- cuUninit mty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  let allOnes = UnOp Not (cuCast uMaskTy (unsignedIntConst (0 :: Word64))) noLoc
      zeroM = cuCast uMaskTy (unsignedIntConst (0 :: Word64))
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
        c = BinOp cop (Index aa idx noLoc) (Index ab idx noLoc) noLoc
    cuStmt $ Exp (Just $ Assign (Index am idx noLoc) JustAssign (Cond c allOnes zeroM noLoc) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, am, signedIntConst bytes]) noLoc
  pure out

cudaVideoCmpName :: JanusElemTag -> BinOp -> Maybe String
cudaVideoCmpName tag cop = case (tag, cop) of
  (JanusElemInt8, Eq) -> Just "__vcmpeq4"
  (JanusElemWord8, Eq) -> Just "__vcmpeq4"
  (JanusElemInt16, Eq) -> Just "__vcmpeq2"
  (JanusElemWord16, Eq) -> Just "__vcmpeq2"
  (JanusElemInt8, Ne) -> Just "__vcmpne4"
  (JanusElemWord8, Ne) -> Just "__vcmpne4"
  (JanusElemInt16, Ne) -> Just "__vcmpne2"
  (JanusElemWord16, Ne) -> Just "__vcmpne2"
  (JanusElemInt8, Lt) -> Just "__vcmplts4"
  (JanusElemWord8, Lt) -> Just "__vcmpltu4"
  (JanusElemInt16, Lt) -> Just "__vcmplts2"
  (JanusElemWord16, Lt) -> Just "__vcmpltu2"
  (JanusElemInt8, Le) -> Just "__vcmples4"
  (JanusElemWord8, Le) -> Just "__vcmpleu4"
  (JanusElemInt16, Le) -> Just "__vcmples2"
  (JanusElemWord16, Le) -> Just "__vcmpleu2"
  (JanusElemInt8, Gt) -> Just "__vcmpgts4"
  (JanusElemWord8, Gt) -> Just "__vcmpgtu4"
  (JanusElemInt16, Gt) -> Just "__vcmpgts2"
  (JanusElemWord16, Gt) -> Just "__vcmpgtu2"
  (JanusElemInt8, Ge) -> Just "__vcmpges4"
  (JanusElemWord8, Ge) -> Just "__vcmpgeu4"
  (JanusElemInt16, Ge) -> Just "__vcmpges2"
  (JanusElemWord16, Ge) -> Just "__vcmpgeu2"
  _ -> Nothing

emitCUDACmp ::
  forall n a.
  (SIMDShape n a) =>
  BinOp ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDMask JanusCUDA n a)
emitCUDACmp cop (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
  RVal a <- ma
  RVal b <- mb
  case cudaVideoCmpName (simdElemTag (Proxy @a)) cop of
    Just fn -> RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> cuCall fn [x, y]) a b
    Nothing -> RVal <$> emitCUDACmpFallback @n @a cop a b

instance (SIMDShape n a) => ExpSIMDCmp JanusCUDA n a where
  veq = emitCUDACmp Eq
  vne = emitCUDACmp Ne
  vlt = emitCUDACmp Lt
  vle = emitCUDACmp Le
  vgt = emitCUDACmp Gt
  vge = emitCUDACmp Ge
