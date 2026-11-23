{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.SIMD.Base where

import Control.Lens ((%~), (&), (+~), (.~), (^.), ix)
import Control.Monad (forM_, when)
import Control.Monad.State (modify)
import Data.Loc
import Data.Proxy
import Foreign.Storable (Storable)
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.CUDA.Types
import Janus.Expression.SIMD
import Janus.Expression.SIMD.Types (rawMaskFromBools, rawMaskToBools, rawSIMDFromList, rawSIMDToList)
import Language.C.Quote as C

--------------------------------------------------------------------------------
-- JanusCUDA Device SIMD Codegen (__v*2, __v*4, __dp4a, __dp2a, uint4/float4)
--------------------------------------------------------------------------------

instance ExpSIMD JanusCUDA where
  newtype SIMDVector JanusCUDA n a = JanusCUDASIMDVector {getJanusCUDASIMDVector :: RawSIMD n a}
    deriving stock (Eq, Ord)
  newtype SIMDMask JanusCUDA n a = JanusCUDASIMDMask {getJanusCUDASIMDMask :: RawMask n a}
    deriving stock (Eq, Ord)

deriving newtype instance (SIMDShape n a) => Show (SIMDVector JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Show (SIMDMask JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDVector JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask JanusCUDA n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector JanusCUDA n a) n a where
  simdFromList = JanusCUDASIMDVector . rawSIMDFromList
  simdToList = rawSIMDToList . getJanusCUDASIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask JanusCUDA n a) n a where
  simdMaskFromBools = JanusCUDASIMDMask . rawMaskFromBools
  simdMaskToBools = rawMaskToBools . getJanusCUDASIMDMask

cuNamedType :: String -> JCType
cuNamedType nm = JCType (DeclSpec [] [] (Tnamed (Id nm noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

cu16BVecName :: forall a. (SIMDElem a) => Proxy a -> String
cu16BVecName p = case simdElemTag p of
  JanusElemFloat -> "float4"
  JanusElemDouble -> "double2"
  JanusElemInt32 -> "int4"
  JanusElemInt64 -> "longlong2"
  JanusElemWord64 -> "ulonglong2"
  _ -> "uint4"

cu8BVecName :: forall a. (SIMDElem a) => Proxy a -> String
cu8BVecName p = case simdElemTag p of
  JanusElemFloat -> "float2"
  JanusElemInt32 -> "int2"
  _ -> "uint2"

cudaSIMDType :: forall n a. (SIMDShape n a) => JanusCM JCType
cudaSIMDType = do
  addHeader "stdint.h"
  addHeader "string.h"
  case simdBytes (Proxy @n) (Proxy @a) of
    4 -> pure (cuNamedType "uint32_t")
    8 -> pure (cuNamedType (cu8BVecName (Proxy @a)))
    16 -> pure (cuNamedType (cu16BVecName (Proxy @a)))
    32 -> do
      let JCType hspec _ = cuNamedType (cu16BVecName (Proxy @a))
          fgs =
            [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
              FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
            ]
      pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "cudaSIMDType: unsupported byte size"

cudaMaskType :: forall n a. (SIMDShape n a) => JanusCM JCType
cudaMaskType = do
  addHeader "stdint.h"
  addHeader "string.h"
  case simdBytes (Proxy @n) (Proxy @a) of
    4 -> pure (cuNamedType "uint32_t")
    8 -> pure (cuNamedType "uint2")
    16 -> pure (cuNamedType "uint4")
    32 -> do
      let JCType hspec _ = cuNamedType "uint4"
          fgs =
            [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
              FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
            ]
      pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "cudaMaskType: unsupported byte size"

instance (SIMDShape n a) => JanusCTyped (SIMDVector JanusCUDA n a) where
  jctype _ = cudaSIMDType @n @a

instance (SIMDShape n a) => JanusCTyped (SIMDMask JanusCUDA n a) where
  jctype _ = cudaMaskType @n @a

cuVar :: String -> Exp
cuVar s = Var (Id s noLoc) noLoc

cuCall :: String -> [Exp] -> Exp
cuCall fn args = FnCall (cuVar fn) args noLoc

cuCast :: String -> Exp -> Exp
cuCast ty e =
  let JCType spec dec = cuNamedType ty
   in Cast (Type spec dec noLoc) e noLoc

cuPtrCast :: String -> Exp -> Exp
cuPtrCast ty e =
  let JCType spec dec = cuNamedType ty
   in Cast (Type spec (Ptr [] dec noLoc) noLoc) e noLoc

cuConstPtrCast :: String -> Exp -> Exp
cuConstPtrCast ty e =
  Cast (Type (DeclSpec [] [Tconst noLoc] (Tnamed (Id ty noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc) noLoc) e noLoc

cuInt :: Int -> Exp
cuInt = signedIntConst

cuMem :: Exp -> String -> Exp
cuMem e f = Member e (Id f noLoc) noLoc

cuStmt :: Stm -> JanusCM ()
cuStmt s = modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm s]

cuUninit :: JCType -> JanusCM Exp
cuUninit (JCType spec dec) = do
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl])
  pure (Var vid noLoc)

-- Apply a 32-bit CUDA SIMD intrinsic (e.g. __vadd4, __vadd2, __vcmpgts4) across
-- 4B (uint32_t), 8B (uint2), 16B (uint4), or 32B ({ uint4 lo; uint4 hi; })
emitCUDAPackedWordBin ::
  forall resTy.
  (JanusCTyped resTy) =>
  Int ->
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDAPackedWordBin bytes opWord ea0 eb0 = do
  rty <- getJanusCType (Proxy @resTy)
  ea <- emitLocalVal rty ea0
  eb <- emitLocalVal rty eb0
  case bytes of
    4 -> emitLocalVal rty (opWord ea eb)
    8 ->
      emitLocalVal rty $
        cuCall
          "make_uint2"
          [ opWord (cuMem ea "x") (cuMem eb "x"),
            opWord (cuMem ea "y") (cuMem eb "y")
          ]
    16 ->
      emitLocalVal rty $
        cuCall
          "make_uint4"
          [ opWord (cuMem ea "x") (cuMem eb "x"),
            opWord (cuMem ea "y") (cuMem eb "y"),
            opWord (cuMem ea "z") (cuMem eb "z"),
            opWord (cuMem ea "w") (cuMem eb "w")
          ]
    32 -> do
      out <- cuUninit rty
      let mk4 l r =
            cuCall
              "make_uint4"
              [ opWord (cuMem l "x") (cuMem r "x"),
                opWord (cuMem l "y") (cuMem r "y"),
                opWord (cuMem l "z") (cuMem r "z"),
                opWord (cuMem l "w") (cuMem r "w")
              ]
      cuStmt $ Exp (Just $ Assign (cuMem out "lo") JustAssign (mk4 (cuMem ea "lo") (cuMem eb "lo")) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (cuMem out "hi") JustAssign (mk4 (cuMem ea "hi") (cuMem eb "hi")) noLoc) noLoc
      pure out
    _ -> error "emitCUDAPackedWordBin: invalid byte count"

emitCUDAPackedWordUn ::
  forall resTy.
  (JanusCTyped resTy) =>
  Int ->
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitCUDAPackedWordUn bytes opWord ea0 = do
  rty <- getJanusCType (Proxy @resTy)
  ea <- emitLocalVal rty ea0
  case bytes of
    4 -> emitLocalVal rty (opWord ea)
    8 -> emitLocalVal rty $ cuCall "make_uint2" [opWord (cuMem ea "x"), opWord (cuMem ea "y")]
    16 ->
      emitLocalVal rty $
        cuCall "make_uint4" [opWord (cuMem ea "x"), opWord (cuMem ea "y"), opWord (cuMem ea "z"), opWord (cuMem ea "w")]
    32 -> do
      out <- cuUninit rty
      let mk4 v = cuCall "make_uint4" [opWord (cuMem v "x"), opWord (cuMem v "y"), opWord (cuMem v "z"), opWord (cuMem v "w")]
      cuStmt $ Exp (Just $ Assign (cuMem out "lo") JustAssign (mk4 (cuMem ea "lo")) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (cuMem out "hi") JustAssign (mk4 (cuMem ea "hi")) noLoc) noLoc
      pure out
    _ -> error "emitCUDAPackedWordUn: invalid byte count"

emitCUDALaneBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDALaneBin fn ea0 eb0 = do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  ea <- emitLocalVal ty ea0
  eb <- emitLocalVal ty eb0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- cuUninit arrTy
  ab <- cuUninit arrTy
  ar <- cuUninit arrTy
  res <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc) (Index ab idx noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
  pure res

emitCUDALaneUn ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitCUDALaneUn fn ea0 = do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  ea <- emitLocalVal ty ea0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- cuUninit arrTy
  ar <- cuUninit arrTy
  res <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
  pure res

-- 1. ExpSIMDVec JanusCUDA
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDVec JanusCUDA n a where
  vzero = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  vsplat (JanusCUDA (JanusC ms)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal s0 <- ms
    s <- emitLocalVal (cuNamedType (simdElemCTypeName (Proxy @a))) s0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign s noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  viota = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst i)) noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  vfromLanes es = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    when (length es /= lanes) $ error "vfromLanes: lane count mismatch"
    arr <- cuUninit (JCType espec arrDec)
    forM_ (zip [0 :: Int ..] es) $ \(i, JanusCUDA (JanusC me)) -> do
      RVal ev <- me
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign ev noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  vextractLane p (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    let srcPtr = BinOp Add (cuConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst (idx * sz)) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst sz]) noLoc
    pure (RVal out)

  vinsertLane p (JanusCUDA (JanusC mx)) (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ex <- mx
    RVal ev0 <- mv
    res <- emitLocalVal ty ev0
    sx <- emitLocalVal (cuNamedType (simdElemCTypeName (Proxy @a))) ex
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        dstPtr = BinOp Add (cuPtrCast "uint8_t" (UnOp AddrOf res noLoc)) (signedIntConst (idx * sz)) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [dstPtr, UnOp AddrOf sx noLoc, signedIntConst sz]) noLoc
    pure (RVal res)
