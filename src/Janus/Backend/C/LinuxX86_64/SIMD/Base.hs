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

module Janus.Backend.C.LinuxX86_64.SIMD.Base where

import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.State (modify)
import Data.Loc
import Data.Proxy
import Data.Word (Word32, Word64)
import Foreign.LibFFI (argPtr, callFFI, retVoid)
import qualified Foreign.Storable as Storable
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64.Types
import Janus.Backend.C.Target
import Janus.Expression.SIMD
import Janus.Expression.SIMD.Types (rawMaskFromBools, rawMaskToBools, rawSIMDFromList, rawSIMDToList)
import Language.C.Quote as C

--------------------------------------------------------------------------------
-- ExpSIMD LinuxX86_64C: Associated Types, Indirect Value ABI, & Register Types
--------------------------------------------------------------------------------

emitMemcpyLoad :: forall a. (JanusCTyped a) => Int -> Exp -> JanusCM Exp
emitMemcpyLoad bytes ptrExp = do
  addHeader "string.h"
  JCType spec dec <- getJanusCType (Proxy @a)
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
      cp = FnCall (Var (Id "memcpy" noLoc) noLoc) [UnOp AddrOf (Var vid noLoc) noLoc, ptrExp, signedIntConst bytes] noLoc
      stm = BlockStm $ Exp (Just cp) noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl, stm])
  pure (Var vid noLoc)

instance ExpSIMD LinuxX86_64C where
  newtype SIMDVector LinuxX86_64C n a = LinuxX86_64SIMDVector {getLinuxX86_64SIMDVector :: RawSIMD n a}
    deriving stock (Eq, Ord)
  newtype SIMDMask LinuxX86_64C n a = LinuxX86_64SIMDMask {getLinuxX86_64SIMDMask :: RawMask n a}
    deriving stock (Eq, Ord)

deriving newtype instance (SIMDShape n a) => Show (SIMDVector LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Show (SIMDMask LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Storable.Storable (SIMDVector LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Storable.Storable (SIMDMask LinuxX86_64C n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector LinuxX86_64C n a) n a where
  simdFromList = LinuxX86_64SIMDVector . rawSIMDFromList
  simdToList = rawSIMDToList . getLinuxX86_64SIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask LinuxX86_64C n a) n a where
  simdMaskFromBools = LinuxX86_64SIMDMask . rawMaskFromBools
  simdMaskToBools = rawMaskToBools . getLinuxX86_64SIMDMask

instance (SIMDShape n a) => JanusCABIParam LinuxX86_64 (SIMDVector LinuxX86_64C n a) where
  jcEmitABIParam _ k =
    emitIndirectABIParam @(SIMDVector LinuxX86_64C n a) (simdBytes (Proxy @n) (Proxy @a)) k
  jcWithABIArg _ v k = withIndirectStorableArg v (k . argPtr)
  jcABIParamIsIndirect _ _ = True
  jcABIParamTHSpec _ = indirectABIParamTHSpec

instance (SIMDShape n a) => JanusCABIRet LinuxX86_64 (SIMDVector LinuxX86_64C n a) where
  jcFinishABIRet _ = emitIndirectABIRet (simdBytes (Proxy @n) (Proxy @a))
  jcCallABIRet _ _ fp args =
    withIndirectStorableRet $ \pOut ->
      callFFI fp retVoid (reverse args <> [argPtr pOut])
  jcABIRetIsIndirect _ _ = True
  jcABIRetTHSpec _ = indirectABIRetTHSpec

instance (SIMDShape n a) => JanusCABIParam LinuxX86_64 (SIMDMask LinuxX86_64C n a) where
  jcEmitABIParam _ k =
    emitIndirectABIParam @(SIMDMask LinuxX86_64C n a) (simdBytes (Proxy @n) (Proxy @a)) k
  jcWithABIArg _ m k = withIndirectStorableArg m (k . argPtr)
  jcABIParamIsIndirect _ _ = True
  jcABIParamTHSpec _ = indirectABIParamTHSpec

instance (SIMDShape n a) => JanusCABIRet LinuxX86_64 (SIMDMask LinuxX86_64C n a) where
  jcFinishABIRet _ = emitIndirectABIRet (simdBytes (Proxy @n) (Proxy @a))
  jcCallABIRet _ _ fp args =
    withIndirectStorableRet $ \pOut ->
      callFFI fp retVoid (reverse args <> [argPtr pOut])
  jcABIRetIsIndirect _ _ = True
  jcABIRetTHSpec _ = indirectABIRetTHSpec

namedCType :: String -> JCType
namedCType nm = JCType (DeclSpec [] [] (Tnamed (Id nm noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

x86128RegName :: forall a. (SIMDElem a) => Proxy a -> String
x86128RegName p = case simdElemTag p of
  JanusElemFloat -> "__m128"
  JanusElemDouble -> "__m128d"
  _ -> "__m128i"

x86Has256Reg :: forall a. (SIMDElem a) => X86Features -> Proxy a -> Bool
x86Has256Reg fs p =
  if simdElemIsFloat p
    then hasX86Feature fs X86_AVX
    else hasX86Feature fs X86_AVX2

x86256RegName :: forall a. (SIMDElem a) => Proxy a -> String
x86256RegName p = case simdElemTag p of
  JanusElemFloat -> "__m256"
  JanusElemDouble -> "__m256d"
  _ -> "__m256i"

x86SIMDType :: forall n a. (SIMDShape n a) => JanusCM JCType
x86SIMDType = do
  addHeader "immintrin.h"
  addHeader "stdint.h"
  addHeader "string.h"
  fs <- askX86Features
  let b = simdBytes (Proxy @n) (Proxy @a)
  case b of
    4 -> pure (namedCType "uint32_t")
    8 -> pure (namedCType "uint64_t")
    16 -> pure (namedCType (x86128RegName (Proxy @a)))
    32
      | x86Has256Reg fs (Proxy @a) -> pure (namedCType (x86256RegName (Proxy @a)))
      | otherwise -> do
          let JCType hspec _ = namedCType (x86128RegName (Proxy @a))
              fgs =
                [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
                  FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
                ]
          pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "x86SIMDType: unsupported byte width"

instance (SIMDShape n a) => JanusCTyped (SIMDVector LinuxX86_64C n a) where
  jctype _ = x86SIMDType @n @a

instance (SIMDShape n a) => JanusCTyped (SIMDMask LinuxX86_64C n a) where
  jctype _ = getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))

--------------------------------------------------------------------------------
-- AST & <immintrin.h> Lowering Helpers
--------------------------------------------------------------------------------

cVar :: String -> Exp
cVar s = Var (Id s noLoc) noLoc

cCall :: String -> [Exp] -> Exp
cCall fn args = FnCall (cVar fn) args noLoc

cType :: JCType -> Type
cType (JCType spec dec) = Type spec dec noLoc

cNamedType :: String -> Type
cNamedType = cType . namedCType

cCast :: String -> Exp -> Exp
cCast ty e = Cast (cNamedType ty) e noLoc

cPtrCast :: String -> Exp -> Exp
cPtrCast ty e =
  let JCType spec dec = namedCType ty
   in Cast (Type spec (Ptr [] dec noLoc) noLoc) e noLoc

cConstPtrCast :: String -> Exp -> Exp
cConstPtrCast ty e =
  Cast (Type (DeclSpec [] [Tconst noLoc] (Tnamed (Id ty noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc) noLoc) e noLoc

cInt :: Int -> Exp
cInt = signedIntConst

cMember :: Exp -> String -> Exp
cMember e m = Member e (Id m noLoc) noLoc

emitStmt :: Stm -> JanusCM ()
emitStmt stm = modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm stm]

emitUninitLocal :: JCType -> JanusCM Exp
emitUninitLocal (JCType spec dec) = do
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl])
  pure (Var vid noLoc)

emitPairStruct :: JCType -> Exp -> Exp -> JanusCM Exp
emitPairStruct pairTy lo hi =
  emitLocalVal pairTy $
    CompoundLit
      (cType pairTy)
      [ (Just (Designation [MemberDesignator (Id "lo" noLoc) noLoc] noLoc), ExpInitializer lo noLoc),
        (Just (Designation [MemberDesignator (Id "hi" noLoc) noLoc] noLoc), ExpInitializer hi noLoc)
      ]
      noLoc

-- Convert a 4B/8B/16B vector expression into its 128-bit XMM register (__m128 / __m128d / __m128i)
toXMM128 :: forall a. (SIMDElem a) => Int -> Exp -> JanusCM Exp
toXMM128 16 e = pure e
toXMM128 4 e = do
  let xi = cCall "_mm_cvtsi32_si128" [cCast "int32_t" e]
  case simdElemTag (Proxy @a) of
    JanusElemFloat -> emitLocalVal (namedCType "__m128") (cCall "_mm_castsi128_ps" [xi])
    JanusElemDouble -> emitLocalVal (namedCType "__m128d") (cCall "_mm_castsi128_pd" [xi])
    _ -> emitLocalVal (namedCType "__m128i") xi
toXMM128 8 e = do
  let xi = cCall "_mm_cvtsi64_si128" [cCast "int64_t" e]
  case simdElemTag (Proxy @a) of
    JanusElemFloat -> emitLocalVal (namedCType "__m128") (cCall "_mm_castsi128_ps" [xi])
    JanusElemDouble -> emitLocalVal (namedCType "__m128d") (cCall "_mm_castsi128_pd" [xi])
    _ -> emitLocalVal (namedCType "__m128i") xi
toXMM128 _ _ = error "toXMM128: invalid byte size"

fromXMM128 :: forall a. (SIMDElem a) => Int -> Exp -> JanusCM Exp
fromXMM128 16 e = emitLocalVal (namedCType (x86128RegName (Proxy @a))) e
fromXMM128 4 e = do
  let asI = case simdElemTag (Proxy @a) of
        JanusElemFloat -> cCall "_mm_castps_si128" [e]
        JanusElemDouble -> cCall "_mm_castpd_si128" [e]
        _ -> e
  emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si32" [asI]))
fromXMM128 8 e = do
  let asI = case simdElemTag (Proxy @a) of
        JanusElemFloat -> cCall "_mm_castps_si128" [e]
        JanusElemDouble -> cCall "_mm_castpd_si128" [e]
        _ -> e
  emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [asI]))
fromXMM128 _ _ = error "fromXMM128: invalid byte size"

asM128i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
asM128i p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm_castps_si128" [e]
  JanusElemDouble -> cCall "_mm_castpd_si128" [e]
  _ -> e

fromM128i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
fromM128i p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm_castsi128_ps" [e]
  JanusElemDouble -> cCall "_mm_castsi128_pd" [e]
  _ -> e

asM256i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
asM256i p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm256_castps_si256" [e]
  JanusElemDouble -> cCall "_mm256_castpd_si256" [e]
  _ -> e

fromM256i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
fromM256i p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm256_castsi256_ps" [e]
  JanusElemDouble -> cCall "_mm256_castsi256_pd" [e]
  _ -> e

-- Scalar lane fallback inside generated C using stack array round-trip
emitLaneLoopBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitLaneLoopBin bodyFn va vb = do
  addHeader "string.h"
  addHeader "stdint.h"
  addHeader "math.h"
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemTyStr = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemTyStr
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- emitUninitLocal arrTy
  ab <- emitUninitLocal arrTy
  ar <- emitUninitLocal arrTy
  resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
  va' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` va)
  vb' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` vb)
  emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf va' noLoc, signedIntConst bytes]) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf vb' noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
        lhs = Index ar idx noLoc
        rhs = bodyFn (Index aa idx noLoc) (Index ab idx noLoc)
    emitStmt $ Exp (Just $ Assign lhs JustAssign rhs noLoc) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst bytes]) noLoc
  pure resVar

emitLaneLoopUn ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitLaneLoopUn bodyFn va = do
  addHeader "string.h"
  addHeader "stdint.h"
  addHeader "math.h"
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemTyStr = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemTyStr
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- emitUninitLocal arrTy
  ar <- emitUninitLocal arrTy
  resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
  va' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` va)
  emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf va' noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    emitStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (bodyFn (Index aa idx noLoc)) noLoc) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst bytes]) noLoc
  pure resVar

extractLo256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
extractLo256 p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm256_castps256_ps128" [e]
  JanusElemDouble -> cCall "_mm256_castpd256_pd128" [e]
  _ -> cCall "_mm256_castsi256_si128" [e]

extractHi256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
extractHi256 p e = case simdElemTag p of
  JanusElemFloat -> cCall "_mm256_extractf128_ps" [e, cInt 1]
  JanusElemDouble -> cCall "_mm256_extractf128_pd" [e, cInt 1]
  _ -> cCall "_mm256_extracti128_si256" [e, cInt 1]

combine256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp -> Exp
combine256 p lo hi = case simdElemTag p of
  JanusElemFloat -> cCall "_mm256_insertf128_ps" [cCall "_mm256_castps128_ps256" [lo], hi, cInt 1]
  JanusElemDouble -> cCall "_mm256_insertf128_pd" [cCall "_mm256_castpd128_pd256" [lo], hi, cInt 1]
  _ -> cCall "_mm256_inserti128_si256" [cCall "_mm256_castsi128_si256" [lo], hi, cInt 1]

liftX86Bin ::
  forall n a.
  (SIMDShape n a) =>
  (X86Features -> Exp -> Exp -> JanusCM Exp) ->
  Maybe (X86Features -> Exp -> Exp -> JanusCM Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
liftX86Bin f128 mf256 (JanusC ma) (JanusC mb) =
  JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32 -> case mf256 of
        Just f256 | x86Has256Reg fs pa -> do
          r256 <- f256 fs ea eb
          emitLocalVal ty r256
        Nothing | x86Has256Reg fs pa -> do
          rlo <- f128 fs (extractLo256 pa ea) (extractLo256 pa eb) >>= fromXMM128 @a 16
          rhi <- f128 fs (extractHi256 pa ea) (extractHi256 pa eb) >>= fromXMM128 @a 16
          emitLocalVal ty (combine256 pa rlo rhi)
        _ -> do
          rlo <- f128 fs (cMember ea "lo") (cMember eb "lo") >>= fromXMM128 @a 16
          rhi <- f128 fs (cMember ea "hi") (cMember eb "hi") >>= fromXMM128 @a 16
          emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xb <- toXMM128 @a b eb
        xr <- f128 fs xa xb
        fromXMM128 @a b xr
    pure (RVal r)

liftX86Un ::
  forall n a.
  (SIMDShape n a) =>
  (X86Features -> Exp -> JanusCM Exp) ->
  Maybe (X86Features -> Exp -> JanusCM Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
liftX86Un f128 mf256 (JanusC ma) =
  JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    ea <- emitLocalVal ty ea0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32 -> case mf256 of
        Just f256 | x86Has256Reg fs pa -> do
          r256 <- f256 fs ea
          emitLocalVal ty r256
        Nothing | x86Has256Reg fs pa -> do
          rlo <- f128 fs (extractLo256 pa ea) >>= fromXMM128 @a 16
          rhi <- f128 fs (extractHi256 pa ea) >>= fromXMM128 @a 16
          emitLocalVal ty (combine256 pa rlo rhi)
        _ -> do
          rlo <- f128 fs (cMember ea "lo") >>= fromXMM128 @a 16
          rhi <- f128 fs (cMember ea "hi") >>= fromXMM128 @a 16
          emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xr <- f128 fs xa
        fromXMM128 @a b xr
    pure (RVal r)

--------------------------------------------------------------------------------
-- Broadcast / Zero Helpers & ExpSIMDVec
--------------------------------------------------------------------------------

x86Set1_128 :: forall a. (SIMDElem a) => Exp -> Exp
x86Set1_128 s = case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm_set1_epi8" [cCast "char" s]
  JanusElemWord8 -> cCall "_mm_set1_epi8" [cCast "char" s]
  JanusElemInt16 -> cCall "_mm_set1_epi16" [cCast "short" s]
  JanusElemWord16 -> cCall "_mm_set1_epi16" [cCast "short" s]
  JanusElemInt32 -> cCall "_mm_set1_epi32" [cCast "int" s]
  JanusElemWord32 -> cCall "_mm_set1_epi32" [cCast "int" s]
  JanusElemInt64 -> cCall "_mm_set1_epi64x" [cCast "long long" s]
  JanusElemWord64 -> cCall "_mm_set1_epi64x" [cCast "long long" s]
  JanusElemFloat -> cCall "_mm_set1_ps" [s]
  JanusElemDouble -> cCall "_mm_set1_pd" [s]

x86Set1_256 :: forall a. (SIMDElem a) => Exp -> Exp
x86Set1_256 s = case simdElemTag (Proxy @a) of
  JanusElemInt8 -> cCall "_mm256_set1_epi8" [cCast "char" s]
  JanusElemWord8 -> cCall "_mm256_set1_epi8" [cCast "char" s]
  JanusElemInt16 -> cCall "_mm256_set1_epi16" [cCast "short" s]
  JanusElemWord16 -> cCall "_mm256_set1_epi16" [cCast "short" s]
  JanusElemInt32 -> cCall "_mm256_set1_epi32" [cCast "int" s]
  JanusElemWord32 -> cCall "_mm256_set1_epi32" [cCast "int" s]
  JanusElemInt64 -> cCall "_mm256_set1_epi64x" [cCast "long long" s]
  JanusElemWord64 -> cCall "_mm256_set1_epi64x" [cCast "long long" s]
  JanusElemFloat -> cCall "_mm256_set1_ps" [s]
  JanusElemDouble -> cCall "_mm256_set1_pd" [s]

x86Zero128 :: forall a. (SIMDElem a) => Exp
x86Zero128 = case simdElemTag (Proxy @a) of
  JanusElemFloat -> cCall "_mm_setzero_ps" []
  JanusElemDouble -> cCall "_mm_setzero_pd" []
  _ -> cCall "_mm_setzero_si128" []

x86Zero256 :: forall a. (SIMDElem a) => Exp
x86Zero256 = case simdElemTag (Proxy @a) of
  JanusElemFloat -> cCall "_mm256_setzero_ps" []
  JanusElemDouble -> cCall "_mm256_setzero_pd" []
  _ -> cCall "_mm256_setzero_si256" []

-- 1. ExpSIMDVec
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDVec LinuxX86_64C n a where
  vzero = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
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
      _ -> error "vzero: invalid size"
    pure (RVal r)

  vsplat (JanusC ms) = JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal s0 <- ms
    let sTy = namedCType (simdElemCTypeName (Proxy @a))
    s <- emitLocalVal sTy s0
    let b = simdBytes (Proxy @n) (Proxy @a)
    r <- case b of
      32
        | x86Has256Reg fs (Proxy @a) -> emitLocalVal ty (x86Set1_256 @a s)
        | otherwise -> do
            h <- emitLocalVal (namedCType (x86128RegName (Proxy @a))) (x86Set1_128 @a s)
            emitPairStruct ty h h
      _ -> fromXMM128 @a b (x86Set1_128 @a s)
    pure (RVal r)

  viota = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arrVar <- emitUninitLocal (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \k ->
      emitStmt $ Exp (Just $ Assign (Index arrVar (signedIntConst k) noLoc) JustAssign (cCast (simdElemCTypeName (Proxy @a)) (signedIntConst k)) noLoc) noLoc
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, arrVar, signedIntConst bytes]) noLoc
    pure (RVal res)

  vfromLanes es = JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    when (length es /= lanes) $ error "vfromLanes: lane count mismatch"
    arrVar <- emitUninitLocal (JCType espec arrDec)
    forM_ (zip [0 :: Int ..] es) $ \(i, JanusC me) -> do
      RVal ev <- me
      emitStmt $ Exp (Just $ Assign (Index arrVar (signedIntConst i) noLoc) JustAssign ev noLoc) noLoc
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, arrVar, signedIntConst bytes]) noLoc
    pure (RVal res)

  vextractLane p (JanusC mv) = JanusC $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- mv
    ev <- emitLocalVal vty ev0
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        sTy = namedCType (simdElemCTypeName (Proxy @a))
    out <- emitUninitLocal sTy
    let srcPtr = BinOp Add (cConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst (idx * sz)) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst sz]) noLoc
    pure (RVal out)

  vinsertLane p (JanusC mx) (JanusC mv) = JanusC $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ex <- mx
    RVal ev0 <- mv
    res <- emitLocalVal vty ev0
    let sTy = namedCType (simdElemCTypeName (Proxy @a))
    sx <- emitLocalVal sTy ex
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        dstPtr = BinOp Add (cPtrCast "uint8_t" (UnOp AddrOf res noLoc)) (signedIntConst (idx * sz)) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [dstPtr, UnOp AddrOf sx noLoc, signedIntConst sz]) noLoc
    pure (RVal res)
