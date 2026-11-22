{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Data.HKD
  ( -- * C-ABI Struct Representation & Field Pointer Wrappers
    CStruct (..),
    PtrOf (..),
    PtrStruct (..),
    unPtrStruct,
    TileField (..),
    TileStruct (..),
    unTileStruct,
    FixedArray (..),
    fixedArrayFromList,
    fixedArrayToList,
    fixedArrayElemPtr,
    alignUpInt,
    alignUpExpr,
    ptrToWord64Expr,
    word64ToPtrExpr,

    -- * Generic C-ABI Struct Layout, Storable, Codegen & Field Addressing
    ExpStructPtr (..),
    JanusStruct (..),
    GJanusStructLayout (..),
    GJanusStructProject (..),
    GJanusStructVal (..),
    peekStruct,
    pokeStruct,
    fieldByteOffset,
    containerOfPtr,

    -- * Generic RefCell for HKD Records
    GRefCell (..),
    newHKDCell,
    newHKDCell',

    -- * Zero-Overhead Host (Identity) <-> Janus (e) HKD FFI Marshalling
    JanusHKD (..),
    GHKDLeaves (..),
    HKD (..),
    mkPtrHandleJCParam,
    mkPtrHandleJCEval,
    mkPtrHandleJCRetParam,
    mkPtrHandleJCRetEval,
    mkPtrHandleX86Param,
    mkPtrHandleX86EvalParam,
    mkPtrHandleX86DeferArg,
    mkPtrHandleX86DeferCont,
    mkPtrHandleX86RetParam,
    mkPtrHandleX86RetEval,
    mkPtrHandleX86RetDeferArg,
    mkPtrHandleX86RetDeferCont,
    mkPtrHandleTHParamSpec,
    mkPtrHandleTHRetSpec,
  )
where

import Control.Monad (forM_)
import Data.Bits (Bits (complement, (.&.)))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.LibFFI (Arg, argPtr, callFFI, retPtr, retVoid)
import Foreign.Ptr (Ptr, castPtr, castPtrToFunPtr, intPtrToPtr, plusPtr, ptrToIntPtr, ptrToWordPtr, wordPtrToPtr)
import qualified Foreign.Storable as Storable
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Backend.C.Target (X86Features)
import Janus.Command.Array (CmdStorable (..), ExpPtr (..))
import Janus.Command.Ref (Cell (..), CmdRef, RefCell (..))
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.FFI.Arg (FFIArg (..))
import Janus.Typed (JanusTyped)
import Language.C.Quote
  ( ArraySize (ArraySize),
    Decl (Array, DeclRoot),
    DeclSpec (DeclSpec),
    Exp (Cast, Member, PtrMember, UnOp, Var),
    Field (Field),
    FieldGroup (FieldGroup),
    Id (Id),
    Param (Param),
    Type (Type),
    TypeSpec (Tnamed, Tstruct),
    UnOp (AddrOf),
  )
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------
-- Alignment & Pointer-Integer Conversion Utilities
--------------------------------------------------------------------------------

{-# INLINE alignUpInt #-}
alignUpInt :: Int -> Int -> Int
alignUpInt off align =
  let a = max 1 align
   in (off + a - 1) .&. complement (a - 1)

-- | Align @x@ upward to power-of-2 @align@ in expression language @e@.
alignUpExpr :: (ExpBits e Int64, Num (e Int64)) => e Int64 -> e Int64 -> e Int64
alignUpExpr x align = (x + (align - 1)) `iand` inot (align - 1)

class ExpPtrWord (e :: K.Type -> K.Type) where
  ptrToWord64Expr :: e (Ptr a) -> e Word64
  word64ToPtrExpr :: (JanusTyped e a) => e Word64 -> e (Ptr a)

instance ExpPtrWord Identity where
  ptrToWord64Expr (Identity p) = Identity (fromIntegral (ptrToWordPtr p))
  word64ToPtrExpr (Identity w) = Identity (wordPtrToPtr (fromIntegral w))

instance ExpPtrWord JanusC where
  ptrToWord64Expr (JanusC mp) = JanusC $ do
    addHeader "stdint.h"
    RVal p <- mp
    let uptrTy = Type (DeclSpec [] [] (Tnamed (Id "uintptr_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc
        u64Ty = Type (DeclSpec [] [] (Tnamed (Id "uint64_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc
    pure $ RVal $ Cast u64Ty (Cast uptrTy p noLoc) noLoc
  word64ToPtrExpr :: forall a. (JanusCTyped a) => JanusC Word64 -> JanusC (Ptr a)
  word64ToPtrExpr (JanusC mw) = JanusC $ do
    addHeader "stdint.h"
    RVal w <- mw
    JCType spec dec <- getJanusCType (Proxy @(Ptr a))
    let uptrTy = Type (DeclSpec [] [] (Tnamed (Id "uintptr_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc
    pure $ RVal $ Cast (Type spec dec noLoc) (Cast uptrTy w noLoc) noLoc

instance ExpPtrWord LinuxX86_64C where
  ptrToWord64Expr (LinuxX86_64C p) = LinuxX86_64C (ptrToWord64Expr p)
  word64ToPtrExpr (LinuxX86_64C w) = LinuxX86_64C (word64ToPtrExpr w)

--------------------------------------------------------------------------------
-- FixedArray (w :: Nat) a — ISO C Fixed-Length Array `a field[w]`
--------------------------------------------------------------------------------

newtype FixedArray (w :: Nat) a = FixedArray {getFixedArray :: V.Vector a}
  deriving stock (Eq, Ord, Show)

fixedArrayFromList :: forall w a. (KnownNat w) => [a] -> FixedArray w a
fixedArrayFromList xs =
  let w = fromIntegral (natVal (Proxy @w))
      v = V.fromListN w xs
   in if V.length v == w
        then FixedArray v
        else error $ "fixedArrayFromList: expected " <> show w <> " elements"

fixedArrayToList :: FixedArray w a -> [a]
fixedArrayToList (FixedArray v) = V.toList v

instance (KnownNat w, Storable.Storable a) => Storable.Storable (FixedArray w a) where
  sizeOf _ = fromIntegral (natVal (Proxy @w)) * Storable.sizeOf (undefined :: a)
  alignment _ = max 1 (Storable.alignment (undefined :: a))
  peek p = do
    let w = fromIntegral (natVal (Proxy @w))
        elemPtr = castPtr p :: Ptr a
    FixedArray <$> V.generateM w (Storable.peekElemOff elemPtr)
  poke p (FixedArray v) = do
    let w = fromIntegral (natVal (Proxy @w))
        elemPtr = castPtr p :: Ptr a
    forM_ [0 .. w - 1] $ \i ->
      Storable.pokeElemOff elemPtr i (v V.! i)

instance (KnownNat w, Typeable w, JanusCTyped a) => JanusCTyped (FixedArray w a) where
  jctype _ = do
    JCType spec dec <- getJanusCType (Proxy @a)
    let w = fromIntegral (natVal (Proxy @w)) :: Int
    pure $ JCType spec (Array [] (ArraySize False (signedIntConst w) noLoc) dec noLoc)

fixedArrayElemPtr :: (ExpPtrCast e, JanusTyped e a, JanusTyped e (FixedArray w a)) => e (Ptr (FixedArray w a)) -> e (Ptr a)
fixedArrayElemPtr = fromVoidPtr . toVoidPtr

--------------------------------------------------------------------------------
-- CStruct & PtrOf — C-ABI Struct Representation for Higher-Kinded Records
--------------------------------------------------------------------------------

newtype CStruct (s :: (K.Type -> K.Type) -> K.Type) = CStruct {unCStruct :: s Identity}

deriving stock instance (Eq (s Identity)) => Eq (CStruct s)

deriving stock instance (Ord (s Identity)) => Ord (CStruct s)

deriving stock instance (Show (s Identity)) => Show (CStruct s)

newtype PtrOf (e :: K.Type -> K.Type) a = PtrOf {getPtrOf :: e (Ptr a)}

deriving stock instance (Eq (e (Ptr a))) => Eq (PtrOf e a)

deriving stock instance (Show (e (Ptr a))) => Show (PtrOf e a)

--------------------------------------------------------------------------------
-- Struct Member Pointer Projection (`&p->field` / `plusPtr`)
--------------------------------------------------------------------------------

class (ExpPtrWord e) => ExpStructPtr (e :: K.Type -> K.Type) where
  -- | Given a struct pointer @p@, byte offset @off@ (used by 'Identity'), and
  -- C member name @fname@ (used by C backends), return @&p->fname@.
  structFieldPtr :: (JanusTyped e field) => e (Ptr structTy) -> Int -> String -> e (Ptr field)

instance ExpStructPtr Identity where
  structFieldPtr (Identity p) off _ = Identity (castPtr p `plusPtr` off)

cPtrMemberLVal :: Exp -> String -> Exp
cPtrMemberLVal (UnOp AddrOf lval _) fname = Member lval (Id fname noLoc) noLoc
cPtrMemberLVal p fname = PtrMember p (Id fname noLoc) noLoc

instance ExpStructPtr JanusC where
  structFieldPtr (JanusC mp) _ fname = JanusC $ do
    RVal p <- mp
    pure $ RVal $ UnOp AddrOf (cPtrMemberLVal p fname) noLoc

instance ExpStructPtr LinuxX86_64C where
  structFieldPtr (LinuxX86_64C p) off fname = LinuxX86_64C (structFieldPtr p off fname)

--------------------------------------------------------------------------------
-- Generic C-ABI Struct Layout, Storable, C Typedef, & Pointer Projection
--------------------------------------------------------------------------------

class
  ( Generic (s Identity),
    Generic (s (PtrOf Identity)),
    Typeable s
  ) =>
  JanusStruct (s :: (K.Type -> K.Type) -> K.Type)
  where
  cStructSizeAlign :: Proxy s -> (Int, Int)
  default cStructSizeAlign :: (GJanusStructLayout (Rep (s Identity))) => Proxy s -> (Int, Int)
  cStructSizeAlign _ =
    let (endOff, maxAl) = gStructSizeAlign (Proxy @(Rep (s Identity))) 0 1
        al = max 1 maxAl
     in (alignUpInt (max 1 endOff) al, al)

  cStructPeekAt :: Ptr () -> Int -> IO (s Identity, Int)
  default cStructPeekAt :: (GJanusStructLayout (Rep (s Identity))) => Ptr () -> Int -> IO (s Identity, Int)
  cStructPeekAt base off0 = do
    let (totSz, structAl) = cStructSizeAlign (Proxy @s)
        alignedStart = alignUpInt off0 structAl
    (repVal, _) <- gStructPeek (base `plusPtr` alignedStart) 0
    pure (to repVal, alignedStart + totSz)

  cStructPokeAt :: Ptr () -> Int -> s Identity -> IO Int
  default cStructPokeAt :: (GJanusStructLayout (Rep (s Identity))) => Ptr () -> Int -> s Identity -> IO Int
  cStructPokeAt base off0 val = do
    let (totSz, structAl) = cStructSizeAlign (Proxy @s)
        alignedStart = alignUpInt off0 structAl
    _ <- gStructPoke (base `plusPtr` alignedStart) 0 (from val)
    pure (alignedStart + totSz)

  cStructFieldsC :: Proxy s -> JanusCM [FieldGroup]
  default cStructFieldsC :: (GJanusStructLayout (Rep (s Identity))) => Proxy s -> JanusCM [FieldGroup]
  cStructFieldsC _ = gStructFieldsC (Proxy @(Rep (s Identity))) ""

  projectPtrs :: forall e. (ExpStructPtr e, Generic (s (PtrOf e)), GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e)))) => e (Ptr (CStruct s)) -> s (PtrOf e)
  projectPtrs p =
    let (repPtrs, _) = gProjectPtrs @e @(Rep (s Identity)) @(Rep (s (PtrOf e))) p 0 ""
     in to repPtrs

instance (JanusStruct s) => Storable.Storable (CStruct s) where
  sizeOf _ = fst (cStructSizeAlign (Proxy @s))
  alignment _ = snd (cStructSizeAlign (Proxy @s))
  peek p = CStruct . fst <$> cStructPeekAt @s (castPtr p) 0
  poke p (CStruct v) = cStructPokeAt @s (castPtr p) 0 v >> pure ()

instance (JanusStruct s) => JanusCTyped (CStruct s) where
  jctype _ = do
    fgs <- cStructFieldsC (Proxy @s)
    pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)

-- | Compute the exact byte offset of a leaf field within @CStruct s@.
fieldByteOffset ::
  forall s a.
  ( JanusStruct s,
    GJanusStructProject Identity (Rep (s Identity)) (Rep (s (PtrOf Identity)))
  ) =>
  (s (PtrOf Identity) -> PtrOf Identity a) ->
  Int
fieldByteOffset sel =
  let base = intPtrToPtr 4096 :: Ptr (CStruct s)
      PtrOf (Identity fieldPtr) = sel (projectPtrs (Identity base))
   in fromIntegral (ptrToIntPtr fieldPtr - ptrToIntPtr base)

-- | Recover a pointer to @CStruct s@ from a pointer to one of its fields
-- (Linux kernel @container_of@ pointer arithmetic).
containerOfPtr ::
  forall s a e.
  ( JanusStruct s,
    GJanusStructProject Identity (Rep (s Identity)) (Rep (s (PtrOf Identity))),
    ExpPtr e Word8,
    ExpPtrCast e,
    JanusTyped e a,
    JanusTyped e Word8,
    JanusTyped e (CStruct s),
    Num (e Int64)
  ) =>
  (s (PtrOf Identity) -> PtrOf Identity a) ->
  e (Ptr a) ->
  e (Ptr (CStruct s))
containerOfPtr sel fp =
  let off = fromIntegral (fieldByteOffset sel) :: Int64
      bytePtr = fromVoidPtr (toVoidPtr fp) :: e (Ptr Word8)
      baseBytePtr = ptrAdd bytePtr (fromIntegral (-off))
   in fromVoidPtr (toVoidPtr baseBytePtr)

--------------------------------------------------------------------------------
-- GJanusStructLayout: Size, Alignment, Storable & C Field Generation
--------------------------------------------------------------------------------

class GJanusStructLayout (rep :: K.Type -> K.Type) where
  gStructSizeAlign :: Proxy rep -> Int -> Int -> (Int, Int)
  gStructPeek :: Ptr () -> Int -> IO (rep p, Int)
  gStructPoke :: Ptr () -> Int -> rep p -> IO Int
  gStructFieldsC :: Proxy rep -> String -> JanusCM [FieldGroup]

instance (GJanusStructLayout f) => GJanusStructLayout (D1 c f) where
  gStructSizeAlign _ = gStructSizeAlign (Proxy @f)
  gStructPeek p off = do
    (x, off') <- gStructPeek p off
    pure (M1 x, off')
  gStructPoke p off (M1 x) = gStructPoke p off x
  gStructFieldsC _ = gStructFieldsC (Proxy @f)

instance (GJanusStructLayout f) => GJanusStructLayout (C1 c f) where
  gStructSizeAlign _ = gStructSizeAlign (Proxy @f)
  gStructPeek p off = do
    (x, off') <- gStructPeek p off
    pure (M1 x, off')
  gStructPoke p off (M1 x) = gStructPoke p off x
  gStructFieldsC _ = gStructFieldsC (Proxy @f)

instance (KnownSymbol name, GJanusStructLayout f) => GJanusStructLayout (S1 ('MetaSel ('Just name) u s l) f) where
  gStructSizeAlign _ = gStructSizeAlign (Proxy @f)
  gStructPeek p off = do
    (x, off') <- gStructPeek p off
    pure (M1 x, off')
  gStructPoke p off (M1 x) = gStructPoke p off x
  gStructFieldsC _ _ = gStructFieldsC (Proxy @f) (symbolVal (Proxy @name))

instance (GJanusStructLayout f, GJanusStructLayout g) => GJanusStructLayout (f :*: g) where
  gStructSizeAlign _ off0 al0 =
    let (off1, al1) = gStructSizeAlign (Proxy @f) off0 al0
     in gStructSizeAlign (Proxy @g) off1 al1
  gStructPeek p off0 = do
    (fx, off1) <- gStructPeek p off0
    (gx, off2) <- gStructPeek p off1
    pure (fx :*: gx, off2)
  gStructPoke p off0 (fx :*: gx) = do
    off1 <- gStructPoke p off0 fx
    gStructPoke p off1 gx
  gStructFieldsC _ nm = (<>) <$> gStructFieldsC (Proxy @f) nm <*> gStructFieldsC (Proxy @g) nm

instance (Storable.Storable a, JanusCTyped a) => GJanusStructLayout (K1 i (Identity a)) where
  gStructSizeAlign _ off0 al0 =
    let sz = Storable.sizeOf (undefined :: a)
        al = max 1 (Storable.alignment (undefined :: a))
        offAligned = alignUpInt off0 al
     in (offAligned + sz, max al0 al)
  gStructPeek p off0 = do
    let sz = Storable.sizeOf (undefined :: a)
        al = max 1 (Storable.alignment (undefined :: a))
        offAligned = alignUpInt off0 al
    val <- Storable.peek (castPtr (p `plusPtr` offAligned) :: Ptr a)
    pure (K1 (Identity val), offAligned + sz)
  gStructPoke p off0 (K1 (Identity val)) = do
    let sz = Storable.sizeOf (undefined :: a)
        al = max 1 (Storable.alignment (undefined :: a))
        offAligned = alignUpInt off0 al
    Storable.poke (castPtr (p `plusPtr` offAligned) :: Ptr a) val
    pure (offAligned + sz)
  gStructFieldsC _ fname = do
    JCType spec dec <- getJanusCType (Proxy @a)
    pure [FieldGroup spec [Field (Just (Id fname noLoc)) (Just dec) Nothing noLoc] noLoc]

instance (Storable.Storable (Ptr a), JanusCTyped a) => GJanusStructLayout (K1 i (PtrOf Identity a)) where
  gStructSizeAlign _ off0 al0 =
    let sz = Storable.sizeOf (undefined :: Ptr a)
        al = max 1 (Storable.alignment (undefined :: Ptr a))
        offAligned = alignUpInt off0 al
     in (offAligned + sz, max al0 al)
  gStructPeek p off0 = do
    let sz = Storable.sizeOf (undefined :: Ptr a)
        al = max 1 (Storable.alignment (undefined :: Ptr a))
        offAligned = alignUpInt off0 al
    val <- Storable.peek (castPtr (p `plusPtr` offAligned) :: Ptr (Ptr a))
    pure (K1 (PtrOf (Identity val)), offAligned + sz)
  gStructPoke p off0 (K1 (PtrOf (Identity val))) = do
    let sz = Storable.sizeOf (undefined :: Ptr a)
        al = max 1 (Storable.alignment (undefined :: Ptr a))
        offAligned = alignUpInt off0 al
    Storable.poke (castPtr (p `plusPtr` offAligned) :: Ptr (Ptr a)) val
    pure (offAligned + sz)
  gStructFieldsC _ fname = do
    JCType spec dec <- getJanusCType (Proxy @(Ptr a))
    pure [FieldGroup spec [Field (Just (Id fname noLoc)) (Just dec) Nothing noLoc] noLoc]

instance (JanusStruct sub) => GJanusStructLayout (K1 i (sub Identity)) where
  gStructSizeAlign _ off0 al0 =
    let (sz, al) = cStructSizeAlign (Proxy @sub)
        offAligned = alignUpInt off0 al
     in (offAligned + sz, max al0 al)
  gStructPeek p off0 = do
    (v, off1) <- cStructPeekAt @sub p off0
    pure (K1 v, off1)
  gStructPoke p off0 (K1 v) = cStructPokeAt @sub p off0 v
  gStructFieldsC _ fname = do
    JCType spec dec <- getJanusCType (Proxy @(CStruct sub))
    pure [FieldGroup spec [Field (Just (Id fname noLoc)) (Just dec) Nothing noLoc] noLoc]

instance (JanusStruct (PtrStruct sub), Coercible (PtrStruct sub Identity) (sub (PtrOf Identity))) => GJanusStructLayout (K1 i (sub (PtrOf Identity))) where
  gStructSizeAlign _ off0 al0 =
    let (sz, al) = cStructSizeAlign (Proxy @(PtrStruct sub))
        offAligned = alignUpInt off0 al
     in (offAligned + sz, max al0 al)
  gStructPeek p off0 = do
    (v, off1) <- cStructPeekAt @(PtrStruct sub) p off0
    pure (K1 (coerce v), off1)
  gStructPoke p off0 (K1 v) = cStructPokeAt @(PtrStruct sub) p off0 (coerce v)
  gStructFieldsC _ fname = do
    JCType spec dec <- getJanusCType (Proxy @(CStruct (PtrStruct sub)))
    pure [FieldGroup spec [Field (Just (Id fname noLoc)) (Just dec) Nothing noLoc] noLoc]

newtype PtrStruct (sub :: (K.Type -> K.Type) -> K.Type) (f :: K.Type -> K.Type)
  = PtrStruct (sub (PtrOf f))
  deriving stock (Generic)

unPtrStruct :: PtrStruct sub f -> sub (PtrOf f)
unPtrStruct (PtrStruct x) = x

deriving stock instance (Eq (sub (PtrOf Identity))) => Eq (PtrStruct sub Identity)

deriving stock instance (Show (sub (PtrOf Identity))) => Show (PtrStruct sub Identity)

instance
  ( Generic (sub (PtrOf Identity)),
    GJanusStructLayout (Rep (sub (PtrOf Identity)))
  ) =>
  GJanusStructLayout (S1 ('MetaSel 'Nothing u ss l) (K1 i (sub (PtrOf Identity))))
  where
  gStructSizeAlign _ off0 al0 =
    gStructSizeAlign (Proxy @(Rep (sub (PtrOf Identity)))) off0 al0
  gStructPeek p off0 = do
    (repV, off1) <- gStructPeek @(Rep (sub (PtrOf Identity))) p off0
    pure (M1 (K1 (to repV)), off1)
  gStructPoke p off0 (M1 (K1 v)) =
    gStructPoke @(Rep (sub (PtrOf Identity))) p off0 (from v)
  gStructFieldsC _ fname =
    gStructFieldsC (Proxy @(Rep (sub (PtrOf Identity)))) fname

instance
  ( Typeable sub,
    Generic (sub (PtrOf Identity)),
    GJanusStructLayout (Rep (sub (PtrOf Identity)))
  ) =>
  JanusStruct (PtrStruct sub)

-- | Functor mapping each scalar leaf @a@ of an HKD record to a @w@-element
-- C array @f (FixedArray w a)@ for @AoSoA@ and @SOAOA@ SIMD tiles.
newtype TileField (w :: Nat) (f :: K.Type -> K.Type) a = TileField
  { unTileField :: f (FixedArray w a)
  }

deriving stock instance (Eq (f (FixedArray w a))) => Eq (TileField w f a)

deriving stock instance (Show (f (FixedArray w a))) => Show (TileField w f a)

newtype TileStruct (w :: Nat) (s :: (K.Type -> K.Type) -> K.Type) (f :: K.Type -> K.Type)
  = TileStruct (s (TileField w f))
  deriving stock (Generic)

unTileStruct :: TileStruct w s f -> s (TileField w f)
unTileStruct (TileStruct x) = x

deriving stock instance (Eq (s (TileField w Identity))) => Eq (TileStruct w s Identity)

deriving stock instance (Show (s (TileField w Identity))) => Show (TileStruct w s Identity)

instance
  ( Generic (s (TileField w Identity)),
    GJanusStructLayout (Rep (s (TileField w Identity)))
  ) =>
  GJanusStructLayout (S1 ('MetaSel 'Nothing u ss l) (K1 i (s (TileField w Identity))))
  where
  gStructSizeAlign _ off0 al0 =
    gStructSizeAlign (Proxy @(Rep (s (TileField w Identity)))) off0 al0
  gStructPeek p off0 = do
    (repV, off1) <- gStructPeek @(Rep (s (TileField w Identity))) p off0
    pure (M1 (K1 (to repV)), off1)
  gStructPoke p off0 (M1 (K1 v)) =
    gStructPoke @(Rep (s (TileField w Identity))) p off0 (from v)
  gStructFieldsC _ fname =
    gStructFieldsC (Proxy @(Rep (s (TileField w Identity)))) fname

instance
  ( Typeable w,
    Typeable s,
    Generic (s (TileField w Identity)),
    GJanusStructLayout (Rep (s (TileField w Identity)))
  ) =>
  JanusStruct (TileStruct w s)

instance
  (KnownNat w, Typeable w, Storable.Storable a, JanusCTyped a) =>
  GJanusStructLayout (K1 i (TileField w Identity a))
  where
  gStructSizeAlign _ off0 al0 =
    gStructSizeAlign (Proxy @(K1 i (Identity (FixedArray w a)))) off0 al0
  gStructPeek p off0 = do
    (K1 v, off1) <- gStructPeek @(K1 i (Identity (FixedArray w a))) p off0
    pure (K1 (TileField v), off1)
  gStructPoke p off0 (K1 (TileField v)) =
    gStructPoke @(K1 i (Identity (FixedArray w a))) p off0 (K1 v)
  gStructFieldsC _ fname =
    gStructFieldsC (Proxy @(K1 i (Identity (FixedArray w a)))) fname

instance
  ( JanusStruct (TileStruct w sub),
    Coercible (TileStruct w sub Identity) (sub (TileField w Identity))
  ) =>
  GJanusStructLayout (K1 i (sub (TileField w Identity)))
  where
  gStructSizeAlign _ off0 al0 =
    let (sz, al) = cStructSizeAlign (Proxy @(TileStruct w sub))
        offAligned = alignUpInt off0 al
     in (offAligned + sz, max al0 al)
  gStructPeek p off0 = do
    (v, off1) <- cStructPeekAt @(TileStruct w sub) p off0
    pure (K1 (coerce v), off1)
  gStructPoke p off0 (K1 v) = cStructPokeAt @(TileStruct w sub) p off0 (coerce v)
  gStructFieldsC _ fname = do
    JCType spec dec <- getJanusCType (Proxy @(CStruct (TileStruct w sub)))
    pure [FieldGroup spec [Field (Just (Id fname noLoc)) (Just dec) Nothing noLoc] noLoc]

--------------------------------------------------------------------------------
-- GJanusStructProject: Projecting `e (Ptr (CStruct s))` -> `s (PtrOf e)`
--------------------------------------------------------------------------------

class GJanusStructProject (e :: K.Type -> K.Type) (repId :: K.Type -> K.Type) (repPtr :: K.Type -> K.Type) where
  gProjectPtrs :: e (Ptr structRoot) -> Int -> String -> (repPtr p, Int)

instance (GJanusStructProject e fId fPtr) => GJanusStructProject e (D1 c fId) (D1 c fPtr) where
  gProjectPtrs p off nm =
    let (x, off') = gProjectPtrs @e @fId @fPtr p off nm
     in (M1 x, off')

instance (GJanusStructProject e fId fPtr) => GJanusStructProject e (C1 c fId) (C1 c fPtr) where
  gProjectPtrs p off nm =
    let (x, off') = gProjectPtrs @e @fId @fPtr p off nm
     in (M1 x, off')

instance
  (KnownSymbol name, GJanusStructProject e fId fPtr) =>
  GJanusStructProject e (S1 ('MetaSel ('Just name) u s l) fId) (S1 ('MetaSel ('Just name) u s l) fPtr)
  where
  gProjectPtrs p off _ =
    let (x, off') = gProjectPtrs @e @fId @fPtr p off (symbolVal (Proxy @name))
     in (M1 x, off')

instance
  ( Generic (sub (PtrOf Identity)),
    Generic (sub (PtrOf (PtrOf e))),
    GJanusStructProject e (Rep (sub (PtrOf Identity))) (Rep (sub (PtrOf (PtrOf e))))
  ) =>
  GJanusStructProject
    e
    (S1 ('MetaSel 'Nothing u ss l) (K1 i (sub (PtrOf Identity))))
    (S1 ('MetaSel 'Nothing u ss l) (K1 i (sub (PtrOf (PtrOf e)))))
  where
  gProjectPtrs p off0 fname =
    let (repX, off1) = gProjectPtrs @e @(Rep (sub (PtrOf Identity))) @(Rep (sub (PtrOf (PtrOf e)))) p off0 fname
     in (M1 (K1 (to repX)), off1)

instance
  ( Generic (s (TileField w Identity)),
    Generic (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (s (TileField w Identity))) (Rep (s (TileField w (PtrOf e))))
  ) =>
  GJanusStructProject
    e
    (S1 ('MetaSel 'Nothing u ss l) (K1 i (s (TileField w Identity))))
    (S1 ('MetaSel 'Nothing u ss l) (K1 i (s (TileField w (PtrOf e)))))
  where
  gProjectPtrs p off0 fname =
    let (repX, off1) = gProjectPtrs @e @(Rep (s (TileField w Identity))) @(Rep (s (TileField w (PtrOf e)))) p off0 fname
     in (M1 (K1 (to repX)), off1)

instance
  (GJanusStructProject e fId fPtr, GJanusStructProject e gId gPtr) =>
  GJanusStructProject e (fId :*: gId) (fPtr :*: gPtr)
  where
  gProjectPtrs p off0 nm =
    let (fx, off1) = gProjectPtrs @e @fId @fPtr p off0 nm
        (gx, off2) = gProjectPtrs @e @gId @gPtr p off1 nm
     in (fx :*: gx, off2)

instance
  (ExpStructPtr e, Storable.Storable a, JanusTyped e a) =>
  GJanusStructProject e (K1 i (Identity a)) (K1 i (PtrOf e a))
  where
  gProjectPtrs p off0 fname =
    let sz = Storable.sizeOf (undefined :: a)
        al = max 1 (Storable.alignment (undefined :: a))
        offAligned = alignUpInt off0 al
     in (K1 (PtrOf (structFieldPtr p offAligned fname)), offAligned + sz)

instance
  ( ExpStructPtr e,
    Storable.Storable (Ptr a),
    JanusTyped e (Ptr a)
  ) =>
  GJanusStructProject e (K1 i (PtrOf Identity a)) (K1 i (PtrOf e (Ptr a)))
  where
  gProjectPtrs p off0 fname =
    let sz = Storable.sizeOf (undefined :: Ptr a)
        al = max 1 (Storable.alignment (undefined :: Ptr a))
        offAligned = alignUpInt off0 al
     in (K1 (PtrOf (structFieldPtr p offAligned fname)), offAligned + sz)

instance
  ( ExpStructPtr e,
    Storable.Storable (Ptr a),
    JanusTyped e (Ptr a)
  ) =>
  GJanusStructProject e (K1 i (PtrOf Identity a)) (K1 i (PtrOf (PtrOf e) a))
  where
  gProjectPtrs p off0 fname =
    let sz = Storable.sizeOf (undefined :: Ptr a)
        al = max 1 (Storable.alignment (undefined :: Ptr a))
        offAligned = alignUpInt off0 al
     in (K1 (PtrOf (PtrOf (structFieldPtr p offAligned fname))), offAligned + sz)

instance
  ( ExpStructPtr e,
    JanusStruct (PtrStruct sub),
    Generic (PtrStruct sub (PtrOf e)),
    JanusTyped e (CStruct (PtrStruct sub)),
    GJanusStructProject e (Rep (PtrStruct sub Identity)) (Rep (PtrStruct sub (PtrOf e)))
  ) =>
  GJanusStructProject e (K1 i (sub (PtrOf Identity))) (K1 i (sub (PtrOf (PtrOf e))))
  where
  gProjectPtrs p off0 fname =
    let (sz, al) = cStructSizeAlign (Proxy @(PtrStruct sub))
        offAligned = alignUpInt off0 al
        subPtr = structFieldPtr p offAligned fname :: e (Ptr (CStruct (PtrStruct sub)))
        PtrStruct subPP = projectPtrs subPtr :: PtrStruct sub (PtrOf e)
     in (K1 subPP, offAligned + sz)

instance
  ( ExpStructPtr e,
    KnownNat w,
    Typeable w,
    Storable.Storable a,
    JanusTyped e (FixedArray w a)
  ) =>
  GJanusStructProject e (K1 i (TileField w Identity a)) (K1 i (TileField w (PtrOf e) a))
  where
  gProjectPtrs p off0 fname =
    let sz = Storable.sizeOf (undefined :: FixedArray w a)
        al = max 1 (Storable.alignment (undefined :: FixedArray w a))
        offAligned = alignUpInt off0 al
     in (K1 (TileField (PtrOf (structFieldPtr p offAligned fname))), offAligned + sz)

instance
  ( ExpStructPtr e,
    JanusStruct (TileStruct w sub),
    Coercible (TileStruct w sub (PtrOf e)) (sub (TileField w (PtrOf e))),
    Generic (TileStruct w sub (PtrOf e)),
    JanusTyped e (CStruct (TileStruct w sub)),
    GJanusStructProject e (Rep (TileStruct w sub Identity)) (Rep (TileStruct w sub (PtrOf e)))
  ) =>
  GJanusStructProject e (K1 i (sub (TileField w Identity))) (K1 i (sub (TileField w (PtrOf e))))
  where
  gProjectPtrs p off0 fname =
    let (sz, al) = cStructSizeAlign (Proxy @(TileStruct w sub))
        offAligned = alignUpInt off0 al
        subPtr = structFieldPtr p offAligned fname :: e (Ptr (CStruct (TileStruct w sub)))
        ts = projectPtrs subPtr :: TileStruct w sub (PtrOf e)
     in (K1 (coerce ts), offAligned + sz)

instance
  ( ExpStructPtr e,
    JanusStruct sub,
    Generic (sub (PtrOf e)),
    JanusTyped e (CStruct sub),
    GJanusStructProject e (Rep (sub Identity)) (Rep (sub (PtrOf e)))
  ) =>
  GJanusStructProject e (K1 i (sub Identity)) (K1 i (sub (PtrOf e)))
  where
  gProjectPtrs p off0 fname =
    let (sz, al) = cStructSizeAlign (Proxy @sub)
        offAligned = alignUpInt off0 al
        subPtr = structFieldPtr p offAligned fname :: e (Ptr (CStruct sub))
     in (K1 (projectPtrs subPtr), offAligned + sz)

--------------------------------------------------------------------------------
-- GJanusStructVal: Reading & Writing Whole HKD Records via `s (PtrOf e)`
--------------------------------------------------------------------------------

class (Monad m) => GJanusStructVal m (e :: K.Type -> K.Type) (repPtr :: K.Type -> K.Type) (repVal :: K.Type -> K.Type) where
  gPeekStruct :: repPtr p -> m (repVal p)
  gPokeStruct :: repPtr p -> repVal p -> m ()

instance (GJanusStructVal m e fPtr fVal) => GJanusStructVal m e (M1 i c fPtr) (M1 i c fVal) where
  gPeekStruct (M1 p) = M1 <$> gPeekStruct @m @e p
  gPokeStruct (M1 p) (M1 v) = gPokeStruct @m @e p v

instance (GJanusStructVal m e fPtr fVal, GJanusStructVal m e gPtr gVal) => GJanusStructVal m e (fPtr :*: gPtr) (fVal :*: gVal) where
  gPeekStruct (fp :*: gp) = (:*:) <$> gPeekStruct @m @e fp <*> gPeekStruct @m @e gp
  gPokeStruct (fp :*: gp) (fv :*: gv) = gPokeStruct @m @e fp fv >> gPokeStruct @m @e gp gv

instance (Monad m, CmdStorable m e a) => GJanusStructVal m e (K1 i (PtrOf e a)) (K1 i (e a)) where
  gPeekStruct (K1 (PtrOf p)) = K1 <$> peek p
  gPokeStruct (K1 (PtrOf p)) (K1 v) = poke p v

instance (Monad m, CmdStorable m e (Ptr a)) => GJanusStructVal m e (K1 i (PtrOf e (Ptr a))) (K1 i (PtrOf e a)) where
  gPeekStruct (K1 (PtrOf pp)) = K1 . PtrOf <$> peek pp
  gPokeStruct (K1 (PtrOf pp)) (K1 (PtrOf pv)) = poke pp pv

instance (Monad m, CmdStorable m e (Ptr a)) => GJanusStructVal m e (K1 i (PtrOf (PtrOf e) a)) (K1 i (PtrOf e a)) where
  gPeekStruct (K1 (PtrOf (PtrOf pp))) = K1 . PtrOf <$> peek pp
  gPokeStruct (K1 (PtrOf (PtrOf pp))) (K1 (PtrOf pv)) = poke pp pv

instance
  ( Monad m,
    Generic (sub (PtrOf e)),
    Generic (sub e),
    GJanusStructVal m e (Rep (sub (PtrOf e))) (Rep (sub e))
  ) =>
  GJanusStructVal m e (K1 i (sub (PtrOf e))) (K1 i (sub e))
  where
  gPeekStruct (K1 subP) = K1 . to <$> gPeekStruct @m @e (from subP)
  gPokeStruct (K1 subP) (K1 subV) = gPokeStruct @m @e (from subP) (from subV)

instance
  ( Monad m,
    Generic (sub (PtrOf (PtrOf e))),
    Generic (sub (PtrOf e)),
    GJanusStructVal m e (Rep (sub (PtrOf (PtrOf e)))) (Rep (sub (PtrOf e)))
  ) =>
  GJanusStructVal m e (K1 i (sub (PtrOf (PtrOf e)))) (K1 i (sub (PtrOf e)))
  where
  gPeekStruct (K1 subPP) = K1 . to <$> gPeekStruct @m @e (from subPP)
  gPokeStruct (K1 subPP) (K1 subP) = gPokeStruct @m @e (from subPP) (from subP)

peekStruct ::
  forall m e s.
  ( JanusStruct s,
    ExpStructPtr e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  e (Ptr (CStruct s)) ->
  m (s e)
peekStruct p = to <$> gPeekStruct @m @e (from (projectPtrs p))

pokeStruct ::
  forall m e s.
  ( JanusStruct s,
    ExpStructPtr e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  e (Ptr (CStruct s)) ->
  s e ->
  m ()
pokeStruct p v = gPokeStruct @m @e (from (projectPtrs p)) (from v)

--------------------------------------------------------------------------------
-- Generic RefCell Support for HKD Records (`s e`)
--------------------------------------------------------------------------------

class (CmdRef m e) => GRefCell m e (rep :: K.Type -> K.Type) where
  gNewCell :: rep p -> m (Cell m (rep p))
  gNewCell' :: m (Cell m (rep p))

instance (GRefCell m e f) => GRefCell m e (M1 i c f) where
  gNewCell (M1 x) = do
    c <- gNewCell @m @e x
    pure (Cell (M1 <$> readCell c) (\(M1 x') -> writeCell c x'))
  gNewCell' = do
    c <- gNewCell' @m @e
    pure (Cell (M1 <$> readCell c) (\(M1 x') -> writeCell c x'))

instance (GRefCell m e f, GRefCell m e g) => GRefCell m e (f :*: g) where
  gNewCell (fx :*: gx) = do
    cf <- gNewCell @m @e fx
    cg <- gNewCell @m @e gx
    pure (Cell ((:*:) <$> readCell cf <*> readCell cg) (\(fx' :*: gx') -> writeCell cf fx' >> writeCell cg gx'))
  gNewCell' = do
    cf <- gNewCell' @m @e
    cg <- gNewCell' @m @e
    pure (Cell ((:*:) <$> readCell cf <*> readCell cg) (\(fx' :*: gx') -> writeCell cf fx' >> writeCell cg gx'))

instance (RefCell m e a) => GRefCell m e (K1 i a) where
  gNewCell (K1 x) = do
    c <- newCell x
    pure (Cell (K1 <$> readCell c) (\(K1 x') -> writeCell c x'))
  gNewCell' = do
    c <- newCell'
    pure (Cell (K1 <$> readCell c) (\(K1 x') -> writeCell c x'))

newHKDCell :: forall m e a. (Generic a, GRefCell m e (Rep a)) => a -> m (Cell m a)
newHKDCell x = do
  c <- gNewCell @m @e (from x)
  pure (Cell (to <$> readCell c) (writeCell c . from))

newHKDCell' :: forall m e a. (Generic a, GRefCell m e (Rep a)) => m (Cell m a)
newHKDCell' = do
  c <- gNewCell' @m @e
  pure (Cell (to <$> readCell c) (writeCell c . from))

--------------------------------------------------------------------------------
-- Reusable Single-Pointer Handle FFI Helpers (Zero-Copy Identity <-> C)
--------------------------------------------------------------------------------

mkPtrHandleJCParam ::
  forall p h r.
  (JanusCTyped p, JanusCParam r) =>
  (JanusC (Ptr p) -> h) ->
  String ->
  Int ->
  JanusCM [Param] ->
  JanusCBackend ->
  (h -> r) ->
  [JCFunc]
mkPtrHandleJCParam wrap name n params backend f =
  let params' = do
        JCType spec dec <- getJanusCType (Proxy @(Ptr p))
        ps <- params
        pure (Param (Just (mkArgId n)) spec dec noLoc : ps)
      argExp = wrap (JanusC (pure (RVal (Var (mkArgId n) noLoc))))
   in jcparam name (n + 1) params' backend (f argExp)

mkPtrHandleJCEval ::
  forall p hHs hC r.
  (JanusCEvaluate r) =>
  (hHs -> Identity (Ptr p)) ->
  (JanusC (Ptr p) -> hC) ->
  ForeignPtr () ->
  [Arg] ->
  (hC -> r) ->
  hHs ->
  JanusCEval r
mkPtrHandleJCEval unwrapHs wrapC dlFPtr args f valHs =
  let Identity rawPtr = unwrapHs valHs
      dummy = wrapC (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc))))
   in jceval dlFPtr (argPtr rawPtr : args) (f dummy)

mkPtrHandleJCRetParam ::
  forall p hC.
  (JanusCTyped p) =>
  (hC -> JanusC (Ptr p)) ->
  String ->
  Int ->
  JanusCM [Param] ->
  JanusCBackend ->
  JanusCM hC ->
  [JCFunc]
mkPtrHandleJCRetParam unwrapC name n params backend ma =
  let maW8 :: JanusCM (JanusC (Ptr Word8))
      maW8 = fromVoidPtr . toVoidPtr . unwrapC <$> ma
   in jcparamImpl (Proxy @JanusC) name n params backend maW8

mkPtrHandleJCRetEval ::
  forall p hHs hC.
  (Identity (Ptr p) -> hHs) ->
  ForeignPtr () ->
  [Arg] ->
  JanusCM hC ->
  IO hHs
mkPtrHandleJCRetEval wrapHs dlFPtr args _ =
  withForeignPtr dlFPtr $ \fp -> do
    raw <- callFFI (castPtrToFunPtr fp) (retPtr retVoid) (reverse args)
    pure (wrapHs (Identity (castPtr raw)))

mkPtrHandleX86Param ::
  forall p h r.
  (JanusCTyped p, LinuxX86_64Param r) =>
  (LinuxX86_64C (Ptr p) -> h) ->
  X86Features ->
  String ->
  Int ->
  JanusCM [Param] ->
  (h -> r) ->
  [JCFunc]
mkPtrHandleX86Param wrap fs name n mparams f =
  x86param fs name n mparams (\(pArg :: LinuxX86_64C (Ptr p)) -> f (wrap pArg))

mkPtrHandleX86EvalParam ::
  forall p hHs hC r b.
  (JanusCTyped p, X86DeferCont r) =>
  (hHs -> Identity (Ptr p)) ->
  (LinuxX86_64C (Ptr p) -> hC) ->
  ForeignPtr () ->
  (([Arg] -> IO b) -> [Arg] -> IO b) ->
  [Arg] ->
  (hC -> r) ->
  hHs ->
  LinuxX86_64CEval r
mkPtrHandleX86EvalParam unwrapHs wrapC dlFPtr _ args f valHs =
  let Identity rawPtr = unwrapHs valHs
      dummy = wrapC (LinuxX86_64C (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)))))
   in x86DeferCont (\k -> k (argPtr rawPtr : args)) dlFPtr (f dummy)

mkPtrHandleX86DeferArg ::
  forall p b hHs hC r.
  (LinuxX86_64ABIParam b, X86DeferCont r) =>
  (hHs -> Identity (Ptr p)) ->
  (LinuxX86_64C (Ptr p) -> hC) ->
  b ->
  ForeignPtr () ->
  [Arg] ->
  (hC -> r) ->
  hHs ->
  LinuxX86_64CEval r
mkPtrHandleX86DeferArg unwrapHs wrapC prevVal dlFPtr args f valHs =
  let Identity rawPtr = unwrapHs valHs
      dummy = wrapC (LinuxX86_64C (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)))))
   in x86DeferCont (\k -> x86WithArg prevVal (\ap -> k (argPtr rawPtr : ap : args))) dlFPtr (f dummy)

mkPtrHandleX86DeferCont ::
  forall p hHs hC r.
  (X86DeferCont r) =>
  (hHs -> Identity (Ptr p)) ->
  (LinuxX86_64C (Ptr p) -> hC) ->
  (([Arg] -> IO (LinuxX86_64BaseRet r)) -> IO (LinuxX86_64BaseRet r)) ->
  ForeignPtr () ->
  (hC -> r) ->
  hHs ->
  LinuxX86_64CEval r
mkPtrHandleX86DeferCont unwrapHs wrapC withArgs dlFPtr f valHs =
  let Identity rawPtr = unwrapHs valHs
      dummy = wrapC (LinuxX86_64C (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)))))
   in x86DeferCont (\k -> withArgs (\as -> k (argPtr rawPtr : as))) dlFPtr (f dummy)

mkPtrHandleX86RetParam ::
  forall p hC.
  (JanusCTyped p) =>
  (hC -> LinuxX86_64C (Ptr p)) ->
  X86Features ->
  String ->
  Int ->
  JanusCM [Param] ->
  LinuxX86_64CM hC ->
  [JCFunc]
mkPtrHandleX86RetParam unwrapC fs name n mparams ma =
  let maPtr :: LinuxX86_64CM (LinuxX86_64C (Ptr Word8))
      maPtr = fromVoidPtr . toVoidPtr . unwrapC <$> ma
   in x86param fs name n mparams maPtr

mkPtrHandleX86RetEval ::
  forall p hHs hC b.
  (Identity (Ptr p) -> hHs) ->
  ForeignPtr () ->
  (([Arg] -> IO b) -> [Arg] -> IO b) ->
  [Arg] ->
  LinuxX86_64CM hC ->
  IO hHs
mkPtrHandleX86RetEval wrapHs dlFPtr _ args _ =
  withForeignPtr dlFPtr $ \fp -> do
    raw <- callFFI (castPtrToFunPtr fp) (retPtr retVoid) (reverse args)
    pure (wrapHs (Identity (castPtr raw)))

mkPtrHandleX86RetDeferArg ::
  forall p a hHs hC.
  (LinuxX86_64ABIParam a) =>
  (Identity (Ptr p) -> hHs) ->
  a ->
  ForeignPtr () ->
  [Arg] ->
  LinuxX86_64CM hC ->
  IO hHs
mkPtrHandleX86RetDeferArg wrapHs x dlFPtr args _ =
  withForeignPtr dlFPtr $ \fp ->
    x86WithArg x $ \ax -> do
      raw <- callFFI (castPtrToFunPtr fp) (retPtr retVoid) (reverse (ax : args))
      pure (wrapHs (Identity (castPtr raw)))

mkPtrHandleX86RetDeferCont ::
  forall p hHs hC.
  (Identity (Ptr p) -> hHs) ->
  (([Arg] -> IO hHs) -> IO hHs) ->
  ForeignPtr () ->
  LinuxX86_64CM hC ->
  IO hHs
mkPtrHandleX86RetDeferCont wrapHs withArgs dlFPtr _ =
  withForeignPtr dlFPtr $ \fp ->
    withArgs $ \as -> do
      raw <- callFFI (castPtrToFunPtr fp) (retPtr retVoid) (reverse as)
      pure (wrapHs (Identity (castPtr raw)))

mkPtrHandleTHParamSpec ::
  forall p hHs.
  (Typeable hHs, Coercible hHs (Ptr p)) =>
  Proxy hHs ->
  THParamSpec
mkPtrHandleTHParamSpec ph =
  THParamSpec
    { thParamUserType = typeRepToTHType (typeRep ph),
      thParamRawType = TH.AppT (TH.ConT ''Ptr) (TH.TupleT 0),
      thParamMode = THParamPure (\e -> TH.AppE (TH.VarE 'coerceToVoidPtr) e)
    }

mkPtrHandleTHRetSpec ::
  forall p hHs.
  (Typeable hHs, Coercible (Ptr p) hHs) =>
  THRetEffect ->
  Proxy hHs ->
  Proxy p ->
  THRetSpec
mkPtrHandleTHRetSpec eff ph _ =
  THRetSpec
    { thRetEffect = eff,
      thRetUserBaseType = typeRepToTHType (typeRep ph),
      thRetMode =
        THRetPureMap
          (TH.AppT (TH.ConT ''Ptr) (TH.TupleT 0))
          (\e -> TH.AppE (TH.VarE 'coerceFromVoidPtr) e)
    }

{-# INLINE coerceToVoidPtr #-}
coerceToVoidPtr :: forall a p. (Coercible a (Ptr p)) => a -> Ptr ()
coerceToVoidPtr x = castPtr (coerce x :: Ptr p)

{-# INLINE coerceFromVoidPtr #-}
coerceFromVoidPtr :: forall a p. (Coercible (Ptr p) a) => Ptr () -> a
coerceFromVoidPtr p = coerce (castPtr p :: Ptr p)

--------------------------------------------------------------------------------
-- Generic Multi-Field HKD Wrapper (`HKD d e`) for Arbitrary HKD Records
--------------------------------------------------------------------------------

newtype HKD (d :: (K.Type -> K.Type) -> K.Type) (e :: K.Type -> K.Type) = HKD
  { unHKD :: d e
  }

class JanusHKD (d :: (K.Type -> K.Type) -> K.Type) where
  hkdEmitParamsJC :: Int -> JanusCM [Param] -> (d JanusC, Int, JanusCM [Param])
  hkdEmitParamsX86 :: Int -> JanusCM [Param] -> (d LinuxX86_64C, Int, JanusCM [Param])
  hkdPackArgs :: d Identity -> [Arg] -> [Arg]
  hkdDummyJC :: d JanusC
  hkdDummyX86 :: d LinuxX86_64C

  default hkdEmitParamsJC ::
    (Generic (d JanusC), GHKDLeaves (Rep (d Identity)) (Rep (d JanusC)) (Rep (d LinuxX86_64C))) =>
    Int ->
    JanusCM [Param] ->
    (d JanusC, Int, JanusCM [Param])
  hkdEmitParamsJC n mp =
    let (repJC, n', mp') = gEmitParamsJC @(Rep (d Identity)) @(Rep (d JanusC)) @(Rep (d LinuxX86_64C)) n mp
     in (to repJC, n', mp')

  default hkdEmitParamsX86 ::
    (Generic (d LinuxX86_64C), GHKDLeaves (Rep (d Identity)) (Rep (d JanusC)) (Rep (d LinuxX86_64C))) =>
    Int ->
    JanusCM [Param] ->
    (d LinuxX86_64C, Int, JanusCM [Param])
  hkdEmitParamsX86 n mp =
    let (repX, n', mp') = gEmitParamsX86 @(Rep (d Identity)) @(Rep (d JanusC)) @(Rep (d LinuxX86_64C)) n mp
     in (to repX, n', mp')

  default hkdPackArgs ::
    (Generic (d Identity), GHKDLeaves (Rep (d Identity)) (Rep (d JanusC)) (Rep (d LinuxX86_64C))) =>
    d Identity ->
    [Arg] ->
    [Arg]
  hkdPackArgs v acc = gPackArgs @(Rep (d Identity)) @(Rep (d JanusC)) @(Rep (d LinuxX86_64C)) (from v) acc

  default hkdDummyJC ::
    (Generic (d JanusC), GHKDLeaves (Rep (d Identity)) (Rep (d JanusC)) (Rep (d LinuxX86_64C))) =>
    d JanusC
  hkdDummyJC = to (gDummyJC @(Rep (d Identity)) @(Rep (d JanusC)) @(Rep (d LinuxX86_64C)))

  default hkdDummyX86 ::
    (Generic (d LinuxX86_64C), GHKDLeaves (Rep (d Identity)) (Rep (d JanusC)) (Rep (d LinuxX86_64C))) =>
    d LinuxX86_64C
  hkdDummyX86 = to (gDummyX86 @(Rep (d Identity)) @(Rep (d JanusC)) @(Rep (d LinuxX86_64C)))

class GHKDLeaves (rId :: K.Type -> K.Type) (rJC :: K.Type -> K.Type) (rX86 :: K.Type -> K.Type) where
  gEmitParamsJC :: Int -> JanusCM [Param] -> (rJC p, Int, JanusCM [Param])
  gEmitParamsX86 :: Int -> JanusCM [Param] -> (rX86 p, Int, JanusCM [Param])
  gPackArgs :: rId p -> [Arg] -> [Arg]
  gDummyJC :: rJC p
  gDummyX86 :: rX86 p

instance (GHKDLeaves fId fJC fX86) => GHKDLeaves (M1 i c fId) (M1 i c fJC) (M1 i c fX86) where
  gEmitParamsJC n mp = let (r, n', mp') = gEmitParamsJC @fId @fJC @fX86 n mp in (M1 r, n', mp')
  gEmitParamsX86 n mp = let (r, n', mp') = gEmitParamsX86 @fId @fJC @fX86 n mp in (M1 r, n', mp')
  gPackArgs (M1 x) acc = gPackArgs @fId @fJC @fX86 x acc
  gDummyJC = M1 (gDummyJC @fId @fJC @fX86)
  gDummyX86 = M1 (gDummyX86 @fId @fJC @fX86)

instance
  (GHKDLeaves fId fJC fX86, GHKDLeaves gId gJC gX86) =>
  GHKDLeaves (fId :*: gId) (fJC :*: gJC) (fX86 :*: gX86)
  where
  gEmitParamsJC n0 mp0 =
    let (rf, n1, mp1) = gEmitParamsJC @fId @fJC @fX86 n0 mp0
        (rg, n2, mp2) = gEmitParamsJC @gId @gJC @gX86 n1 mp1
     in (rf :*: rg, n2, mp2)
  gEmitParamsX86 n0 mp0 =
    let (rf, n1, mp1) = gEmitParamsX86 @fId @fJC @fX86 n0 mp0
        (rg, n2, mp2) = gEmitParamsX86 @gId @gJC @gX86 n1 mp1
     in (rf :*: rg, n2, mp2)
  gPackArgs (fx :*: gx) acc =
    let acc1 = gPackArgs @fId @fJC @fX86 fx acc
     in gPackArgs @gId @gJC @gX86 gx acc1
  gDummyJC = gDummyJC @fId @fJC @fX86 :*: gDummyJC @gId @gJC @gX86
  gDummyX86 = gDummyX86 @fId @fJC @fX86 :*: gDummyX86 @gId @gJC @gX86

instance (JanusCTyped a, FFIArg a) => GHKDLeaves (K1 i (Identity a)) (K1 i (JanusC a)) (K1 i (LinuxX86_64C a)) where
  gEmitParamsJC n mp =
    let mp' = do
          JCType spec dec <- getJanusCType (Proxy @a)
          ps <- mp
          pure (Param (Just (mkArgId n)) spec dec noLoc : ps)
        v = JanusC (pure (RVal (Var (mkArgId n) noLoc)))
     in (K1 v, n + 1, mp')
  gEmitParamsX86 n mp =
    let mp' = do
          JCType spec dec <- getJanusCType (Proxy @a)
          ps <- mp
          pure (Param (Just (mkArgId n)) spec dec noLoc : ps)
        v = LinuxX86_64C (JanusC (pure (RVal (Var (mkArgId n) noLoc))))
     in (K1 v, n + 1, mp')
  gPackArgs (K1 (Identity x)) acc = arg x : acc
  gDummyJC = K1 (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc))))
  gDummyX86 = K1 (LinuxX86_64C (JanusC (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)))))

instance (JanusHKD sub) => GHKDLeaves (K1 i (sub Identity)) (K1 i (sub JanusC)) (K1 i (sub LinuxX86_64C)) where
  gEmitParamsJC n mp = let (s, n', mp') = hkdEmitParamsJC @sub n mp in (K1 s, n', mp')
  gEmitParamsX86 n mp = let (s, n', mp') = hkdEmitParamsX86 @sub n mp in (K1 s, n', mp')
  gPackArgs (K1 s) acc = hkdPackArgs s acc
  gDummyJC = K1 (hkdDummyJC @sub)
  gDummyX86 = K1 (hkdDummyX86 @sub)

type instance JanusCEval (HKD d JanusC -> r) = d Identity -> JanusCEval r

type instance JanusCRetType (HKD d JanusC -> r) = JanusCRetType r

instance (JanusHKD d, JanusCParam r) => JanusCParam (HKD d JanusC -> r) where
  jcparam name n params backend f =
    let (valJC, n', params') = hkdEmitParamsJC @d n params
     in jcparam name n' params' backend (f (HKD valJC))

instance (JanusHKD d, JanusCEvaluate r) => JanusCEvaluate (HKD d JanusC -> r) where
  jceval dlFPtr args f valHs =
    jceval dlFPtr (hkdPackArgs valHs args) (f (HKD (hkdDummyJC @d)))

type instance LinuxX86_64CEval (HKD d LinuxX86_64C -> r) = d Identity -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (HKD d LinuxX86_64C -> r) = LinuxX86_64BaseRet r

instance (JanusHKD d, LinuxX86_64Param r) => LinuxX86_64Param (HKD d LinuxX86_64C -> r) where
  x86param fs name n mparams f =
    let (valX86, n', mparams') = hkdEmitParamsX86 @d n mparams
     in x86param fs name n' mparams' (f (HKD valX86))

instance (JanusHKD d, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (HKD d LinuxX86_64C -> r) where
  x86eval dlFPtr _ args f valHs =
    x86DeferCont (\k -> k (hkdPackArgs valHs args)) dlFPtr (f (HKD (hkdDummyX86 @d)))

instance (JanusHKD b, X86DeferCont r) => X86DeferCall (HKD b LinuxX86_64C -> r) where
  x86DeferArg x dlFPtr args f valHs =
    x86DeferCont (\k -> x86WithArg x (\ax -> k (hkdPackArgs valHs (ax : args)))) dlFPtr (f (HKD (hkdDummyX86 @b)))

instance (JanusHKD d, X86DeferCont r) => X86DeferCont (HKD d LinuxX86_64C -> r) where
  x86DeferCont withArgs dlFPtr f valHs =
    x86DeferCont (\k -> withArgs (\as -> k (hkdPackArgs valHs as))) dlFPtr (f (HKD (hkdDummyX86 @d)))
