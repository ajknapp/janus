{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Data.Layout
  ( -- * Storage Layout Tags (`Scalar`, `AoS`, `SoA`, `AoSoA w`, `SOAOA w`)
    AoS,
    SoA,
    AoSoA,
    SOAOA,
    ScalarBuf (..),
    InlineScalar (..),
    AoSBuf (..),
    InlineAoS (..),
    InlineSoA (..),
    AoSoABuf (..),
    InlineAoSoA (..),
    SOAOABuf (..),
    InlineSOAOA (..),
    CountGroups,
    CeilDiv,
    Max1,

    -- * Unified StorageLayout Class, Scoped Buffers, & Constraints
    StorageLayout (..),
    ScopedBuf (..),
    withBuf,
    scopedBuf,
    scopedBufCap,
    readScopedBuf,
    writeScopedBuf,
    reallocScopedBuf,
    LayoutCtx,
    InlineLayoutCtx,
    peekBufStruct,
    pokeBufStruct,
    streamBuf,
    reverseStreamBuf,
    drainToBuf,

    -- * Generic Layout Machinery (Exported for Type Inference & Custom Records)
    GSoAColumns (..),
    GInlineToSoA (..),
    GTileLane (..),
    GSOAOAGroups (..),

    -- * SPMD / SIMD Gang Accessors & Strip-Mining Loops for HKD Layouts
    GSoASPMD (..),
    GTileSPMD (..),
    GSOAOASPMD (..),
    readSoAGang,
    writeSoAGang,
    foreachSoASPMD,
    readAoSoATile,
    readAoSoATileWithAlign,
    writeAoSoATile,
    writeAoSoATileWithAlign,
    foreachAoSoASPMD,
    readSOAOATile,
    readSOAOATileWithAlign,
    writeSOAOATile,
    writeSOAOATileWithAlign,
    soaoaStreamPtrAt,
    foreachSOAOASPMD,

    -- * Unified Layout-Polymorphic SPMD / SIMD Classes & Buffer Combinators
    LayoutUnitSPMD (..),
    LayoutSliceSPMD (..),
    LayoutGatherSPMD (..),
    foreachBufSPMD,
    traverseBufSPMD_,
    foldBufSPMD,
    foreachMaskedBufSPMD,
    foldMaskedBufSPMD,
    foreachSliceBufSPMD,
    traverseSliceBufSPMD_,
    foldSliceBufSPMD,
  )
where

import Data.Bits (countTrailingZeros)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types
  ( CBool,
    CChar,
    CDouble,
    CFloat,
    CInt,
    CLLong,
    CLong,
    CSChar,
    CShort,
    CSize,
    CUChar,
    CUInt,
    CULLong,
    CULong,
    CUShort,
  )
import Foreign.Ptr (Ptr)
import qualified Foreign.Storable as Storable
import GHC.Generics
import GHC.TypeNats (CmpNat, Div, KnownNat, Mod, Nat, natVal, type (*), type (+))
import Janus.Alloc.Dynamic (isNullVoidPtr)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocBytesAligned,
    allocTypedArray,
    freeBytes,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C (JanusCTyped)
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
    janusSIMDAlignment,
  )
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain, drainForEach, drainIndexed, drain_, runDrain)
import Janus.Control.Stream (Stream, rangeS, reverseRangeS, streamM)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( Layout (..),
    SPMD,
    SPMDGang,
    SPMDLayout,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    spmdAlignedUnitStride,
    spmdUnitStride,
    uniform,
    uniformAlignedPtr,
    uniformPtr,
    withSPMD,
    withSPMDMasked,
  )
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- Layout Mode Phantom Tags & Type-Level Arithmetic
--------------------------------------------------------------------------------

data AoS (s :: (K.Type -> K.Type) -> K.Type)

data SoA (s :: (K.Type -> K.Type) -> K.Type)

data AoSoA (w :: Nat) (s :: (K.Type -> K.Type) -> K.Type)

data SOAOA (w :: Nat) (outer :: (K.Type -> K.Type) -> K.Type)

type family Max1 (n :: Nat) :: Nat where
  Max1 0 = 1
  Max1 n = n

type family CeilDiv (n :: Nat) (w :: Nat) :: Nat where
  CeilDiv 0 _ = 1
  CeilDiv n w = CeilDivHelper (CmpNat (Mod n w) 0) (Div n w)

type family CeilDivHelper (cmp :: Ordering) (q :: Nat) :: Nat where
  CeilDivHelper 'EQ q = Max1 q
  CeilDivHelper _ q = q + 1

type family CountRepGroups (rep :: K.Type -> K.Type) :: Nat where
  CountRepGroups (D1 _ f) = CountRepGroups f
  CountRepGroups (C1 _ f) = CountRepGroups f
  CountRepGroups (S1 _ _) = 1
  CountRepGroups (f :*: g) = CountRepGroups f + CountRepGroups g

type CountGroups (outer :: (K.Type -> K.Type) -> K.Type) = CountRepGroups (Rep (outer Identity))

--------------------------------------------------------------------------------
-- Buffer & Inline Storage HKD Struct Wrappers
--------------------------------------------------------------------------------

newtype ScalarBuf a f = ScalarBuf
  { unScalarBuf :: f (Ptr a)
  }
  deriving stock (Generic)

deriving stock instance Eq (ScalarBuf a Identity)

deriving stock instance Show (ScalarBuf a Identity)

instance (Typeable a, Storable.Storable (Ptr a), JanusCTyped a) => JanusStruct (ScalarBuf a)

newtype InlineScalar (n :: Nat) a f = InlineScalar
  { unInlineScalar :: f (FixedArray (Max1 n) a)
  }
  deriving stock (Generic)

deriving stock instance (KnownNat (Max1 n), Eq a) => Eq (InlineScalar n a Identity)

deriving stock instance (KnownNat (Max1 n), Show a) => Show (InlineScalar n a Identity)

instance
  ( KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    Typeable a,
    Storable.Storable a,
    JanusCTyped a
  ) =>
  JanusStruct (InlineScalar n a)

newtype AoSBuf (s :: (K.Type -> K.Type) -> K.Type) f = AoSBuf
  { unAoSBuf :: f (Ptr (CStruct s))
  }
  deriving stock (Generic)

deriving stock instance Eq (AoSBuf s Identity)

deriving stock instance Show (AoSBuf s Identity)

instance (Typeable s, JanusStruct s) => JanusStruct (AoSBuf s)

newtype InlineAoS (n :: Nat) (s :: (K.Type -> K.Type) -> K.Type) f = InlineAoS
  { unInlineAoS :: f (FixedArray (Max1 n) (CStruct s))
  }
  deriving stock (Generic)

instance
  ( KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    Typeable s,
    JanusStruct s
  ) =>
  JanusStruct (InlineAoS n s)

newtype InlineSoA (n :: Nat) (s :: (K.Type -> K.Type) -> K.Type) f = InlineSoA
  { unInlineSoA :: f (CStruct (TileStruct (Max1 n) s))
  }
  deriving stock (Generic)

instance
  ( KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    Typeable s,
    JanusStruct (TileStruct (Max1 n) s)
  ) =>
  JanusStruct (InlineSoA n s)

newtype AoSoABuf (w :: Nat) (s :: (K.Type -> K.Type) -> K.Type) f = AoSoABuf
  { unAoSoABuf :: f (Ptr (CStruct (TileStruct w s)))
  }
  deriving stock (Generic)

deriving stock instance Eq (AoSoABuf w s Identity)

deriving stock instance Show (AoSoABuf w s Identity)

instance (Typeable w, Typeable s, JanusStruct (TileStruct w s)) => JanusStruct (AoSoABuf w s)

newtype InlineAoSoA (n :: Nat) (w :: Nat) (s :: (K.Type -> K.Type) -> K.Type) f = InlineAoSoA
  { unInlineAoSoA :: f (FixedArray (CeilDiv n w) (CStruct (TileStruct w s)))
  }
  deriving stock (Generic)

instance
  ( KnownNat (CeilDiv n w),
    Typeable n,
    Typeable w,
    Typeable (CeilDiv n w),
    Typeable s,
    JanusStruct (TileStruct w s)
  ) =>
  JanusStruct (InlineAoSoA n w s)

newtype SOAOABuf (w :: Nat) (outer :: (K.Type -> K.Type) -> K.Type) f = SOAOABuf
  { unSOAOABuf :: f (Ptr (Ptr Word8))
  }
  deriving stock (Generic)

deriving stock instance Eq (SOAOABuf w outer Identity)

deriving stock instance Show (SOAOABuf w outer Identity)

instance
  ( Typeable w,
    Typeable outer
  ) =>
  JanusStruct (SOAOABuf w outer)

newtype InlineSOAOA (n :: Nat) (w :: Nat) (outer :: (K.Type -> K.Type) -> K.Type) f = InlineSOAOA
  { unInlineSOAOA :: f (FixedArray (Max1 (SOAOAInlineBytes n w outer)) Word8)
  }
  deriving stock (Generic)

instance
  ( KnownNat (Max1 (SOAOAInlineBytes n w outer)),
    Typeable n,
    Typeable w,
    Typeable outer,
    Typeable (Max1 (SOAOAInlineBytes n w outer))
  ) =>
  JanusStruct (InlineSOAOA n w outer)

--------------------------------------------------------------------------------
-- Unified StorageLayout Class
--------------------------------------------------------------------------------

class StorageLayout a where
  type Elem (e :: K.Type -> K.Type) a :: K.Type
  type Buf a = (r :: (K.Type -> K.Type) -> K.Type) | r -> a
  type InlineBuf (n :: Nat) a = (r :: (K.Type -> K.Type) -> K.Type) | r -> n a
  type LayoutElemCtx (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a :: K.Constraint
  type LayoutInlineCtx (n :: Nat) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a :: K.Constraint

  nullBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Proxy m -> Buf a e
  isNullBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Proxy m -> Buf a e -> e Bool
  newBufStateRefs ::
    (CmdAlloc m e, LayoutElemCtx m e a) =>
    Buf a e ->
    e Int64 ->
    m (m (Buf a e, e Int64), Buf a e -> e Int64 -> m ())
  allocBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Alloc e -> e Int64 -> m (Buf a e)
  reallocBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Alloc e -> Buf a e -> e Int64 -> e Int64 -> e Int64 -> m (Buf a e)
  freeBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Alloc e -> Buf a e -> e Int64 -> m ()
  readBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Buf a e -> e Int64 -> m (Elem e a)
  writeBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Buf a e -> e Int64 -> Elem e a -> m ()
  copyBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Buf a e -> e Int64 -> Buf a e -> e Int64 -> e Int64 -> m ()
  moveBuf :: (CmdAlloc m e, LayoutElemCtx m e a) => Buf a e -> e Int64 -> e Int64 -> e Int64 -> m ()
  inlineAsBuf ::
    (CmdAlloc m e, LayoutElemCtx m e a, LayoutInlineCtx n m e a) =>
    Proxy n ->
    e (Ptr (CStruct (InlineBuf n a))) ->
    m (Buf a e)

-- | Scoped mutable buffer handle managed by 'withBuf'.
--
-- Tracks both the live 'Buf' pointer(s) and current capacity in mutable Janus
-- references across any 'reallocScopedBuf' calls inside the continuation so
-- 'withBuf' always frees the latest buffer with its latest capacity in the
-- owning 'Alloc' policy.
data ScopedBuf (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = ScopedBuf
  { sbAlloc :: !(Alloc e),
    sbReadState :: !(m (Buf a e, e Int64)),
    sbWriteState :: !(Buf a e -> e Int64 -> m ())
  }

-- | Read the current live 'Buf' from a 'ScopedBuf'.
scopedBuf :: (Monad m) => ScopedBuf m e a -> m (Buf a e)
scopedBuf sb = fst <$> sbReadState sb

-- | Read the current live element capacity from a 'ScopedBuf'.
scopedBufCap :: (Monad m) => ScopedBuf m e a -> m (e Int64)
scopedBufCap sb = snd <$> sbReadState sb

-- | Read element @i@ from the current live buffer of a 'ScopedBuf'.
readScopedBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  ScopedBuf m e a ->
  e Int64 ->
  m (Elem e a)
readScopedBuf sb i = do
  b <- scopedBuf sb
  readBuf @a b i

-- | Write element @i@ into the current live buffer of a 'ScopedBuf'.
writeScopedBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  ScopedBuf m e a ->
  e Int64 ->
  Elem e a ->
  m ()
writeScopedBuf sb i v = do
  b <- scopedBuf sb
  writeBuf @a b i v

-- | Resize a 'ScopedBuf' to @newCap@ elements (preserving @min liveLen newCap@
-- elements) in its owning 'Alloc' policy, updating the tracked buffer and
-- capacity in place.
--
-- * If @newCap <= 0@, the current buffer is freed and the tracked state becomes
--   @(nullBuf, 0)@ so exit cleanup does not double-free.
-- * If @newCap > 0@ and reallocation succeeds, the tracked state becomes
--   @(newBuf, newCap)@ so exit cleanup frees the new buffer with @newCap@.
-- * If @newCap > 0@ and reallocation fails (returns 'nullBuf'), the previous
--   @(oldBuf, oldCap)@ remains live and tracked so exit cleanup still frees it.
reallocScopedBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  ScopedBuf m e a ->
  e Int64 ->
  e Int64 ->
  m (Buf a e)
reallocScopedBuf sb newCap liveLen = do
  (oldB, oldCap) <- sbReadState sb
  clampedNew <- letM (ifThenElse (newCap `le` 0) 0 newCap)
  newB <- reallocBuf @a (sbAlloc sb) oldB oldCap clampedNew liveLen
  ifThenElseM_
    (clampedNew `le` 0)
    (sbWriteState sb (nullBuf @a (Proxy @m)) 0)
    ( whenM_ (lnot (isNullBuf @a (Proxy @m) newB)) $
        sbWriteState sb newB clampedNew
    )
  pure newB

-- | Allocate a scoped buffer of @initCap@ elements of layout @a@ under
-- allocation policy @alloc@, run @k@ with a 'ScopedBuf' handle (supporting
-- 'reallocScopedBuf' within @k@), and automatically 'freeBuf' the latest live
-- buffer and capacity upon completion.
withBuf ::
  forall a m e r.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Alloc e ->
  e Int64 ->
  (ScopedBuf m e a -> m r) ->
  m r
withBuf alloc initCap k = do
  clampedInit <- letM (ifThenElse (initCap `le` 0) 0 initCap)
  buf0 <- allocBuf @a alloc clampedInit
  (rd, wr) <- newBufStateRefs @a buf0 clampedInit
  let sb =
        ScopedBuf
          { sbAlloc = alloc,
            sbReadState = rd,
            sbWriteState = wr
          }
  res <- k sb
  (finalB, finalCap) <- rd
  freeBuf @a alloc finalB finalCap
  pure res

type LayoutCtx m e a =
  ( StorageLayout a,
    CmdAlloc m e,
    LayoutElemCtx m e a,
    JanusStruct (Buf a),
    ExpSized e (CStruct (Buf a)),
    JanusTyped e (CStruct (Buf a)),
    Generic (Buf a (PtrOf e)),
    Generic (Buf a e),
    GJanusStructProject e (Rep (Buf a Identity)) (Rep (Buf a (PtrOf e))),
    GJanusStructVal m e (Rep (Buf a (PtrOf e))) (Rep (Buf a e))
  )

type InlineLayoutCtx n m e a =
  ( LayoutCtx m e a,
    LayoutInlineCtx n m e a,
    JanusStruct (InlineBuf n a),
    ExpSized e (CStruct (InlineBuf n a)),
    JanusTyped e (CStruct (InlineBuf n a))
  )

peekBufStruct ::
  forall a m e.
  (LayoutCtx m e a) =>
  e (Ptr (CStruct (Buf a))) ->
  m (Buf a e)
peekBufStruct = peekStruct

pokeBufStruct ::
  forall a m e.
  (LayoutCtx m e a) =>
  e (Ptr (CStruct (Buf a))) ->
  Buf a e ->
  m ()
pokeBufStruct = pokeStruct

streamBuf ::
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Buf a e ->
  e Int64 ->
  Stream m (Elem e a)
streamBuf buf n = streamM (rangeS 0 n) (readBuf buf)

reverseStreamBuf ::
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Buf a e ->
  e Int64 ->
  Stream m (Elem e a)
reverseStreamBuf buf n = streamM (reverseRangeS 0 n) (readBuf buf)

drainToBuf ::
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Buf a e ->
  e Int64 ->
  Drain m (Elem e a) (e Int64)
drainToBuf buf startOff = drainIndexed (\i v -> writeBuf buf (startOff + i) v)

--------------------------------------------------------------------------------
-- 1. Scalar Layout Implementation & Instances
--------------------------------------------------------------------------------

type ScalarElemCtx m e a =
  ( CmdStorable m e a,
    ExpSized e a,
    ExpPtr e a,
    JanusTyped e a,
    JanusTyped e (Ptr a),
    CmdStorable m e (Ptr a),
    Typeable a,
    Storable.Storable (Ptr a),
    JanusCTyped a
  )

type ScalarInlineCtx n e a =
  ( KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    Storable.Storable a,
    JanusCTyped a,
    JanusTyped e (FixedArray (Max1 n) a)
  )

scalarNullBuf :: (ExpPtr e a) => ScalarBuf a e
scalarNullBuf = ScalarBuf nullPtr

scalarIsNullBuf :: (CmdAlloc m e, ScalarElemCtx m e a) => ScalarBuf a e -> e Bool
scalarIsNullBuf (ScalarBuf p) = isNullVoidPtr (toVoidPtr p)

scalarNewBufStateRefs ::
  (CmdAlloc m e, ScalarElemCtx m e a) =>
  ScalarBuf a e ->
  e Int64 ->
  m (m (ScalarBuf a e, e Int64), ScalarBuf a e -> e Int64 -> m ())
scalarNewBufStateRefs (ScalarBuf p0) cap0 = do
  pRef <- newRef p0
  cRef <- newRef cap0
  pure
    ( (,) <$> (ScalarBuf <$> readRef pRef) <*> readRef cRef,
      \(ScalarBuf p) c -> writeRef pRef p >> writeRef cRef c
    )

scalarAllocBuf :: forall a m e. (CmdAlloc m e, ScalarElemCtx m e a) => Alloc e -> e Int64 -> m (ScalarBuf a e)
scalarAllocBuf alloc cap = ScalarBuf <$> allocTypedArray @a alloc cap

scalarReallocBuf ::
  forall a m e.
  (CmdAlloc m e, ScalarElemCtx m e a) =>
  Alloc e ->
  ScalarBuf a e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (ScalarBuf a e)
scalarReallocBuf alloc (ScalarBuf oldP) oldCap newCap _ =
  ScalarBuf <$> reallocTypedArray @a alloc oldP oldCap newCap

scalarFreeBuf :: forall a m e. (CmdAlloc m e, ScalarElemCtx m e a) => Alloc e -> ScalarBuf a e -> e Int64 -> m ()
scalarFreeBuf alloc (ScalarBuf p) cap = freeTypedArray @a alloc p cap

scalarReadBuf :: (ScalarElemCtx m e a) => ScalarBuf a e -> e Int64 -> m (e a)
scalarReadBuf (ScalarBuf p) i = peekElemOff p i

scalarWriteBuf :: (ScalarElemCtx m e a) => ScalarBuf a e -> e Int64 -> e a -> m ()
scalarWriteBuf (ScalarBuf p) i v = pokeElemOff p v i

scalarCopyBuf ::
  forall a m e.
  (CmdAlloc m e, ScalarElemCtx m e a) =>
  ScalarBuf a e ->
  e Int64 ->
  ScalarBuf a e ->
  e Int64 ->
  e Int64 ->
  m ()
scalarCopyBuf (ScalarBuf dst) dstOff (ScalarBuf src) srcOff cnt =
  whenM_ (cnt `gt` 0) $ do
    let sz = sizeOf (Proxy @a)
    cmdMemcpy (toVoidPtr (ptrIndex dst dstOff)) (toVoidPtr (ptrIndex src srcOff)) (cnt * sz)

scalarMoveBuf ::
  forall a m e.
  (CmdAlloc m e, ScalarElemCtx m e a) =>
  ScalarBuf a e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m ()
scalarMoveBuf (ScalarBuf p) dstOff srcOff cnt =
  whenM_ ((cnt `gt` 0) `land` (dstOff `ne` srcOff)) $ do
    let sz = sizeOf (Proxy @a)
    cmdMemmove (toVoidPtr (ptrIndex p dstOff)) (toVoidPtr (ptrIndex p srcOff)) (cnt * sz)

scalarInlineAsBuf ::
  forall n a m e.
  ( Applicative m,
    ExpStructPtr e,
    ExpPtrCast e,
    KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    Typeable a,
    Storable.Storable a,
    JanusCTyped a,
    JanusTyped e a,
    JanusTyped e (FixedArray (Max1 n) a)
  ) =>
  e (Ptr (CStruct (InlineScalar n a))) ->
  m (ScalarBuf a e)
scalarInlineAsBuf pInline =
  let InlineScalar (PtrOf pArr) = projectPtrs pInline
   in pure (ScalarBuf (fixedArrayElemPtr pArr))

#define INST_SCALAR(TY) \
instance StorageLayout (TY) where { \
  type Elem e (TY) = e (TY); \
  type Buf (TY) = ScalarBuf (TY); \
  type InlineBuf n (TY) = InlineScalar n (TY); \
  type LayoutElemCtx m e (TY) = ScalarElemCtx m e (TY); \
  type LayoutInlineCtx n m e (TY) = ScalarInlineCtx n e (TY); \
  nullBuf _ = scalarNullBuf; \
  isNullBuf _ = scalarIsNullBuf; \
  newBufStateRefs = scalarNewBufStateRefs; \
  allocBuf = scalarAllocBuf; \
  reallocBuf = scalarReallocBuf; \
  freeBuf = scalarFreeBuf; \
  readBuf = scalarReadBuf; \
  writeBuf = scalarWriteBuf; \
  copyBuf = scalarCopyBuf; \
  moveBuf = scalarMoveBuf; \
  inlineAsBuf _ = scalarInlineAsBuf }

INST_SCALAR(Bool)
INST_SCALAR(CBool)
INST_SCALAR(Int8)
INST_SCALAR(Int16)
INST_SCALAR(Int32)
INST_SCALAR(Int64)
INST_SCALAR(Word8)
INST_SCALAR(Word16)
INST_SCALAR(Word32)
INST_SCALAR(Word64)
INST_SCALAR(Float)
INST_SCALAR(Double)
INST_SCALAR(CChar)
INST_SCALAR(CSChar)
INST_SCALAR(CUChar)
INST_SCALAR(CShort)
INST_SCALAR(CUShort)
INST_SCALAR(CInt)
INST_SCALAR(CUInt)
INST_SCALAR(CLong)
INST_SCALAR(CULong)
INST_SCALAR(CLLong)
INST_SCALAR(CULLong)
INST_SCALAR(CSize)
INST_SCALAR(CFloat)
INST_SCALAR(CDouble)

instance StorageLayout (Ptr p) where
  type Elem e (Ptr p) = e (Ptr p)
  type Buf (Ptr p) = ScalarBuf (Ptr p)
  type InlineBuf n (Ptr p) = InlineScalar n (Ptr p)
  type LayoutElemCtx m e (Ptr p) = ScalarElemCtx m e (Ptr p)
  type LayoutInlineCtx n m e (Ptr p) = ScalarInlineCtx n e (Ptr p)
  nullBuf _ = scalarNullBuf
  isNullBuf _ = scalarIsNullBuf
  newBufStateRefs = scalarNewBufStateRefs
  allocBuf = scalarAllocBuf
  reallocBuf = scalarReallocBuf
  freeBuf = scalarFreeBuf
  readBuf = scalarReadBuf
  writeBuf = scalarWriteBuf
  copyBuf = scalarCopyBuf
  moveBuf = scalarMoveBuf
  inlineAsBuf _ = scalarInlineAsBuf

instance StorageLayout (CStruct s) where
  type Elem e (CStruct s) = e (CStruct s)
  type Buf (CStruct s) = ScalarBuf (CStruct s)
  type InlineBuf n (CStruct s) = InlineScalar n (CStruct s)
  type LayoutElemCtx m e (CStruct s) = ScalarElemCtx m e (CStruct s)
  type LayoutInlineCtx n m e (CStruct s) = ScalarInlineCtx n e (CStruct s)
  nullBuf _ = scalarNullBuf
  isNullBuf _ = scalarIsNullBuf
  newBufStateRefs = scalarNewBufStateRefs
  allocBuf = scalarAllocBuf
  reallocBuf = scalarReallocBuf
  freeBuf = scalarFreeBuf
  readBuf = scalarReadBuf
  writeBuf = scalarWriteBuf
  copyBuf = scalarCopyBuf
  moveBuf = scalarMoveBuf
  inlineAsBuf _ = scalarInlineAsBuf

--------------------------------------------------------------------------------
-- 2. AoS (Array of Structs) Layout (`AoS s`)
--------------------------------------------------------------------------------

type AoSElemCtx m e s =
  ( Typeable s,
    JanusStruct s,
    ScalarElemCtx m e (CStruct s),
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  )

instance StorageLayout (AoS s) where
  type Elem e (AoS s) = s e
  type Buf (AoS s) = AoSBuf s
  type InlineBuf n (AoS s) = InlineAoS n s
  type LayoutElemCtx m e (AoS s) = AoSElemCtx m e s
  type LayoutInlineCtx n m e (AoS s) = ScalarInlineCtx n e (CStruct s)

  nullBuf _ = AoSBuf nullPtr
  isNullBuf _ (AoSBuf p) = isNullVoidPtr (toVoidPtr p)
  newBufStateRefs (AoSBuf p0) cap0 = do
    pRef <- newRef p0
    cRef <- newRef cap0
    pure
      ( (,) <$> (AoSBuf <$> readRef pRef) <*> readRef cRef,
        \(AoSBuf p) c -> writeRef pRef p >> writeRef cRef c
      )
  allocBuf alloc cap = AoSBuf . unScalarBuf <$> scalarAllocBuf @(CStruct s) alloc cap
  reallocBuf alloc (AoSBuf oldP) oldCap newCap len =
    AoSBuf . unScalarBuf <$> scalarReallocBuf @(CStruct s) alloc (ScalarBuf oldP) oldCap newCap len
  freeBuf alloc (AoSBuf p) cap = scalarFreeBuf @(CStruct s) alloc (ScalarBuf p) cap
  readBuf (AoSBuf p) i = peekStruct (ptrIndex p i)
  writeBuf (AoSBuf p) i v = pokeStruct (ptrIndex p i) v
  copyBuf (AoSBuf d) dOff (AoSBuf s) sOff cnt =
    scalarCopyBuf @(CStruct s) (ScalarBuf d) dOff (ScalarBuf s) sOff cnt
  moveBuf (AoSBuf p) dOff sOff cnt =
    scalarMoveBuf @(CStruct s) (ScalarBuf p) dOff sOff cnt
  inlineAsBuf _ pInline =
    let InlineAoS (PtrOf pArr) = projectPtrs pInline
     in pure (AoSBuf (fixedArrayElemPtr pArr))

--------------------------------------------------------------------------------
-- 3. SoA (Struct of 64B-Aligned Arrays in Single Slab) Layout (`SoA s`)
--------------------------------------------------------------------------------

class GSoAColumns m (e :: K.Type -> K.Type) (repPtr :: K.Type -> K.Type) (repVal :: K.Type -> K.Type) where
  gSoANull :: repPtr p
  gSoATotalBytes :: Proxy repPtr -> Proxy repVal -> e Int64 -> e Int64
  gSoACarve :: e (Ptr Word8) -> e Int64 -> e Int64 -> (repPtr p, e Int64)
  gSoAFirstVoidPtr :: repPtr p -> e (Ptr ())
  gSoARead :: repPtr p -> e Int64 -> m (repVal p)
  gSoAWrite :: repPtr p -> e Int64 -> repVal p -> m ()
  gSoACopy :: repPtr p -> e Int64 -> repPtr p -> e Int64 -> e Int64 -> m ()
  gSoAMove :: repPtr p -> e Int64 -> e Int64 -> e Int64 -> m ()

instance (Monad m, GSoAColumns m e fPtr fVal) => GSoAColumns m e (M1 i c fPtr) (M1 i c fVal) where
  gSoANull = M1 (gSoANull @m @e @fPtr @fVal)
  gSoATotalBytes _ _ = gSoATotalBytes @m @e (Proxy @fPtr) (Proxy @fVal)
  gSoACarve base off cap =
    let (r, off') = gSoACarve @m @e @fPtr @fVal base off cap
     in (M1 r, off')
  gSoAFirstVoidPtr (M1 x) = gSoAFirstVoidPtr @m @e @fPtr @fVal x
  gSoARead (M1 p) i = M1 <$> gSoARead @m @e p i
  gSoAWrite (M1 p) i (M1 v) = gSoAWrite @m @e p i v
  gSoACopy (M1 d) dOff (M1 s) sOff cnt = gSoACopy @m @e @fPtr @fVal d dOff s sOff cnt
  gSoAMove (M1 p) dOff sOff cnt = gSoAMove @m @e @fPtr @fVal p dOff sOff cnt

instance
  (Monad m, GSoAColumns m e fPtr fVal, GSoAColumns m e gPtr gVal, Num (e Int64)) =>
  GSoAColumns m e (fPtr :*: gPtr) (fVal :*: gVal)
  where
  gSoANull = gSoANull @m @e @fPtr @fVal :*: gSoANull @m @e @gPtr @gVal
  gSoATotalBytes _ _ cap =
    gSoATotalBytes @m @e (Proxy @fPtr) (Proxy @fVal) cap
      + gSoATotalBytes @m @e (Proxy @gPtr) (Proxy @gVal) cap
  gSoACarve base off0 cap =
    let (rf, off1) = gSoACarve @m @e @fPtr @fVal base off0 cap
        (rg, off2) = gSoACarve @m @e @gPtr @gVal base off1 cap
     in (rf :*: rg, off2)
  gSoAFirstVoidPtr (fx :*: _) = gSoAFirstVoidPtr @m @e @fPtr @fVal fx
  gSoARead (fp :*: gp) i = (:*:) <$> gSoARead @m @e fp i <*> gSoARead @m @e gp i
  gSoAWrite (fp :*: gp) i (fv :*: gv) = gSoAWrite @m @e fp i fv >> gSoAWrite @m @e gp i gv
  gSoACopy (fd :*: gd) dOff (fs :*: gs) sOff cnt =
    gSoACopy @m @e @fPtr @fVal fd dOff fs sOff cnt >> gSoACopy @m @e @gPtr @gVal gd dOff gs sOff cnt
  gSoAMove (fp :*: gp) dOff sOff cnt =
    gSoAMove @m @e @fPtr @fVal fp dOff sOff cnt >> gSoAMove @m @e @gPtr @gVal gp dOff sOff cnt

instance
  ( CmdAlloc m e,
    CmdStorable m e a,
    ExpSized e a,
    ExpPtr e a,
    JanusTyped e a
  ) =>
  GSoAColumns m e (K1 i (PtrOf e a)) (K1 i (e a))
  where
  gSoANull = K1 (PtrOf nullPtr)
  gSoATotalBytes _ _ cap =
    alignUpExpr (cap * sizeOf (Proxy @a)) (fromIntegral janusSIMDAlignment)
  gSoACarve base off0 cap =
    let colBytes = alignUpExpr (cap * sizeOf (Proxy @a)) (fromIntegral janusSIMDAlignment)
        colPtr = fromVoidPtr (toVoidPtr (ptrAdd base off0)) :: e (Ptr a)
     in (K1 (PtrOf colPtr), off0 + colBytes)
  gSoAFirstVoidPtr (K1 (PtrOf p)) = toVoidPtr p
  gSoARead (K1 (PtrOf p)) i = K1 <$> peekElemOff p i
  gSoAWrite (K1 (PtrOf p)) i (K1 v) = pokeElemOff p v i
  gSoACopy (K1 (PtrOf dst)) dOff (K1 (PtrOf src)) sOff cnt =
    whenM_ (cnt `gt` 0) $
      cmdMemcpy (toVoidPtr (ptrIndex dst dOff)) (toVoidPtr (ptrIndex src sOff)) (cnt * sizeOf (Proxy @a))
  gSoAMove (K1 (PtrOf p)) dOff sOff cnt =
    whenM_ ((cnt `gt` 0) `land` (dOff `ne` sOff)) $
      cmdMemmove (toVoidPtr (ptrIndex p dOff)) (toVoidPtr (ptrIndex p sOff)) (cnt * sizeOf (Proxy @a))

instance
  ( Monad m,
    Generic (sub (PtrOf e)),
    Generic (sub e),
    GSoAColumns m e (Rep (sub (PtrOf e))) (Rep (sub e))
  ) =>
  GSoAColumns m e (K1 i (sub (PtrOf e))) (K1 i (sub e))
  where
  gSoANull = K1 (to (gSoANull @m @e @(Rep (sub (PtrOf e))) @(Rep (sub e))))
  gSoATotalBytes _ _ = gSoATotalBytes @m @e (Proxy @(Rep (sub (PtrOf e)))) (Proxy @(Rep (sub e)))
  gSoACarve base off0 cap =
    let (r, off1) = gSoACarve @m @e @(Rep (sub (PtrOf e))) @(Rep (sub e)) base off0 cap
     in (K1 (to r), off1)
  gSoAFirstVoidPtr (K1 sub) = gSoAFirstVoidPtr @m @e @(Rep (sub (PtrOf e))) @(Rep (sub e)) (from sub)
  gSoARead (K1 subP) i = K1 . to <$> gSoARead @m @e (from subP) i
  gSoAWrite (K1 subP) i (K1 subV) = gSoAWrite @m @e (from subP) i (from subV)
  gSoACopy (K1 d) dOff (K1 s) sOff cnt = gSoACopy @m @e @(Rep (sub (PtrOf e))) @(Rep (sub e)) (from d) dOff (from s) sOff cnt
  gSoAMove (K1 p) dOff sOff cnt = gSoAMove @m @e @(Rep (sub (PtrOf e))) @(Rep (sub e)) (from p) dOff sOff cnt

class GInlineToSoA (n :: Nat) (e :: K.Type -> K.Type) (repTile :: K.Type -> K.Type) (repPtr :: K.Type -> K.Type) where
  gInlineToSoA :: repTile p -> repPtr p

instance (GInlineToSoA n e fTile fPtr) => GInlineToSoA n e (M1 i c fTile) (M1 i c fPtr) where
  gInlineToSoA (M1 x) = M1 (gInlineToSoA @n @e x)

instance (GInlineToSoA n e fTile fPtr, GInlineToSoA n e gTile gPtr) => GInlineToSoA n e (fTile :*: gTile) (fPtr :*: gPtr) where
  gInlineToSoA (fx :*: gx) = gInlineToSoA @n @e fx :*: gInlineToSoA @n @e gx

instance
  (ExpPtrCast e, JanusTyped e a, JanusTyped e (FixedArray n a)) =>
  GInlineToSoA n e (K1 i (TileField n (PtrOf e) a)) (K1 i (PtrOf e a))
  where
  gInlineToSoA (K1 (TileField (PtrOf pArr))) = K1 (PtrOf (fixedArrayElemPtr pArr))

instance
  ( Generic (sub (TileField n (PtrOf e))),
    Generic (sub (PtrOf e)),
    GInlineToSoA n e (Rep (sub (TileField n (PtrOf e)))) (Rep (sub (PtrOf e)))
  ) =>
  GInlineToSoA n e (K1 i (sub (TileField n (PtrOf e)))) (K1 i (sub (PtrOf e)))
  where
  gInlineToSoA (K1 subT) = K1 (to (gInlineToSoA @n @e (from subT)))

type SoAElemCtx m e s =
  ( Typeable s,
    Generic (s (PtrOf e)),
    Generic (s e),
    JanusStruct (PtrStruct s),
    GSoAColumns m e (Rep (s (PtrOf e))) (Rep (s e))
  )

type SoAInlineCtx n e s =
  ( KnownNat (Max1 n),
    Typeable n,
    Typeable (Max1 n),
    JanusStruct (TileStruct (Max1 n) s),
    JanusTyped e (CStruct (TileStruct (Max1 n) s)),
    Generic (TileStruct (Max1 n) s (PtrOf e)),
    Generic (s (TileField (Max1 n) (PtrOf e))),
    Coercible (TileStruct (Max1 n) s (PtrOf e)) (s (TileField (Max1 n) (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct (Max1 n) s Identity)) (Rep (TileStruct (Max1 n) s (PtrOf e))),
    GInlineToSoA (Max1 n) e (Rep (s (TileField (Max1 n) (PtrOf e)))) (Rep (s (PtrOf e)))
  )

instance StorageLayout (SoA s) where
  type Elem e (SoA s) = s e
  type Buf (SoA s) = PtrStruct s
  type InlineBuf n (SoA s) = InlineSoA n s
  type LayoutElemCtx m e (SoA s) = SoAElemCtx m e s
  type LayoutInlineCtx n m e (SoA s) = SoAInlineCtx n e s

  nullBuf :: forall m e. (SoAElemCtx m e s) => Proxy m -> PtrStruct s e
  nullBuf _ =
    PtrStruct (to (gSoANull @m @e @(Rep (s (PtrOf e))) @(Rep (s e))))

  isNullBuf :: forall m e. (CmdAlloc m e, SoAElemCtx m e s) => Proxy m -> PtrStruct s e -> e Bool
  isNullBuf _ (PtrStruct ptrs) =
    isNullVoidPtr (gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs))

  newBufStateRefs ::
    forall m e.
    (CmdAlloc m e, SoAElemCtx m e s) =>
    PtrStruct s e ->
    e Int64 ->
    m (m (PtrStruct s e, e Int64), PtrStruct s e -> e Int64 -> m ())
  newBufStateRefs (PtrStruct ptrs0) cap0 = do
    let base0 =
          fromVoidPtr (gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs0)) :: e (Ptr Word8)
    baseRef <- newRef base0
    capRef <- newRef cap0
    let rd = do
          b <- readRef baseRef
          c <- readRef capRef
          let effCap = ifThenElse (isNullVoidPtr (toVoidPtr b)) 0 c
              (repPtrs, _) = gSoACarve @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) b 0 effCap
          pure (PtrStruct (to repPtrs), c)
        wr (PtrStruct ptrs) c = do
          let b =
                fromVoidPtr (gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs)) :: e (Ptr Word8)
          writeRef baseRef b
          writeRef capRef c
    pure (rd, wr)

  allocBuf :: forall m e. (CmdAlloc m e, SoAElemCtx m e s) => Alloc e -> e Int64 -> m (PtrStruct s e)
  allocBuf alloc cap = do
    let totBytes = gSoATotalBytes @m @e (Proxy @(Rep (s (PtrOf e)))) (Proxy @(Rep (s e))) cap
    vp <-
      ifThenElseM_WithRef
        (cap `le` 0)
        (pure (toVoidPtr (nullPtr @e @Word8)))
        (allocBytesAligned alloc totBytes (fromIntegral janusSIMDAlignment))
    let (repPtrs, _) = gSoACarve @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (fromVoidPtr vp) 0 cap
        -- When cap <= 0, vp is NULL and off0 starts at 0 with colBytes = 0, so every carved column pointer is NULL!
    pure (PtrStruct (to repPtrs))

  reallocBuf ::
    forall m e.
    (CmdAlloc m e, SoAElemCtx m e s) =>
    Alloc e ->
    PtrStruct s e ->
    e Int64 ->
    e Int64 ->
    e Int64 ->
    m (PtrStruct s e)
  reallocBuf alloc oldBuf oldCap newCap liveLen = do
    newBuf <- allocBuf @(SoA s) alloc newCap
    let oldBase = gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from (unPtrStruct oldBuf))
        newBase = gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from (unPtrStruct newBuf))
        copyCnt = ifThenElse (liveLen `lt` newCap) liveLen newCap
    whenM_ (lnot (isNullVoidPtr oldBase) `land` (oldCap `gt` 0)) $
      ifThenElseM_
        (newCap `le` 0)
        (freeBuf @(SoA s) alloc oldBuf oldCap)
        ( whenM_ (lnot (isNullVoidPtr newBase)) $ do
            whenM_ (copyCnt `gt` 0) $
              copyBuf @(SoA s) newBuf 0 oldBuf 0 copyCnt
            freeBuf @(SoA s) alloc oldBuf oldCap
        )
    pure newBuf

  freeBuf :: forall m e. (CmdAlloc m e, SoAElemCtx m e s) => Alloc e -> PtrStruct s e -> e Int64 -> m ()
  freeBuf alloc (PtrStruct ptrs) cap = do
    let base = gSoAFirstVoidPtr @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs)
        totBytes = gSoATotalBytes @m @e (Proxy @(Rep (s (PtrOf e)))) (Proxy @(Rep (s e))) cap
    whenM_ (lnot (isNullVoidPtr base) `land` (cap `gt` 0)) $
      freeBytes alloc base totBytes

  readBuf :: forall m e. (CmdAlloc m e, SoAElemCtx m e s) => PtrStruct s e -> e Int64 -> m (s e)
  readBuf (PtrStruct ptrs) i =
    to <$> gSoARead @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs) i

  writeBuf :: forall m e. (CmdAlloc m e, SoAElemCtx m e s) => PtrStruct s e -> e Int64 -> s e -> m ()
  writeBuf (PtrStruct ptrs) i val =
    gSoAWrite @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from ptrs) i (from val)

  copyBuf ::
    forall m e.
    (CmdAlloc m e, SoAElemCtx m e s) =>
    PtrStruct s e ->
    e Int64 ->
    PtrStruct s e ->
    e Int64 ->
    e Int64 ->
    m ()
  copyBuf (PtrStruct d) dOff (PtrStruct s) sOff cnt =
    gSoACopy @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from d) dOff (from s) sOff cnt

  moveBuf ::
    forall m e.
    (CmdAlloc m e, SoAElemCtx m e s) =>
    PtrStruct s e ->
    e Int64 ->
    e Int64 ->
    e Int64 ->
    m ()
  moveBuf (PtrStruct p) dOff sOff cnt =
    gSoAMove @m @e @(Rep (s (PtrOf e))) @(Rep (s e)) (from p) dOff sOff cnt

  inlineAsBuf ::
    forall n m e.
    (CmdAlloc m e, SoAElemCtx m e s, SoAInlineCtx n e s) =>
    Proxy n ->
    e (Ptr (CStruct (InlineSoA n s))) ->
    m (PtrStruct s e)
  inlineAsBuf _ pInline =
    let InlineSoA (PtrOf pTiles) = projectPtrs pInline
        tsPtrs = projectPtrs pTiles :: TileStruct (Max1 n) s (PtrOf e)
        tileFields = coerce tsPtrs :: s (TileField (Max1 n) (PtrOf e))
     in pure (PtrStruct (to (gInlineToSoA @(Max1 n) @e (from tileFields))))

--------------------------------------------------------------------------------
-- 4. AoSoA w s (Single-Stream Array of Struct of w-Lane SIMD Arrays)
--------------------------------------------------------------------------------

class GTileLane (w :: Nat) m (e :: K.Type -> K.Type) (repTile :: K.Type -> K.Type) (repVal :: K.Type -> K.Type) where
  gReadTileLane :: repTile p -> e Int64 -> m (repVal p)
  gWriteTileLane :: repTile p -> e Int64 -> repVal p -> m ()

instance (Monad m, GTileLane w m e fTile fVal) => GTileLane w m e (M1 i c fTile) (M1 i c fVal) where
  gReadTileLane (M1 t) lane = M1 <$> gReadTileLane @w @m @e t lane
  gWriteTileLane (M1 t) lane (M1 v) = gWriteTileLane @w @m @e t lane v

instance (Monad m, GTileLane w m e fTile fVal, GTileLane w m e gTile gVal) => GTileLane w m e (fTile :*: gTile) (fVal :*: gVal) where
  gReadTileLane (ft :*: rt) lane = (:*:) <$> gReadTileLane @w @m @e ft lane <*> gReadTileLane @w @m @e rt lane
  gWriteTileLane (ft :*: rt) lane (fv :*: gv) = gWriteTileLane @w @m @e ft lane fv >> gWriteTileLane @w @m @e rt lane gv

instance
  ( Monad m,
    CmdStorable m e a,
    ExpPtrCast e,
    JanusTyped e a,
    JanusTyped e (FixedArray w a)
  ) =>
  GTileLane w m e (K1 i (TileField w (PtrOf e) a)) (K1 i (e a))
  where
  gReadTileLane (K1 (TileField (PtrOf pArr))) lane =
    K1 <$> peekElemOff (fixedArrayElemPtr pArr) lane
  gWriteTileLane (K1 (TileField (PtrOf pArr))) lane (K1 v) =
    pokeElemOff (fixedArrayElemPtr pArr) v lane

instance
  ( Monad m,
    Generic (sub (TileField w (PtrOf e))),
    Generic (sub e),
    GTileLane w m e (Rep (sub (TileField w (PtrOf e)))) (Rep (sub e))
  ) =>
  GTileLane w m e (K1 i (sub (TileField w (PtrOf e)))) (K1 i (sub e))
  where
  gReadTileLane (K1 subT) lane = K1 . to <$> gReadTileLane @w @m @e (from subT) lane
  gWriteTileLane (K1 subT) lane (K1 subV) = gWriteTileLane @w @m @e (from subT) lane (from subV)

tileCountExpr :: forall w e. (KnownNat w, ExpBits e Int64, ExpCond e, ExpOrd e Int64, JanusTyped e Int64, Num (e Int64)) => Proxy w -> e Int64 -> e Int64
tileCountExpr pw cap =
  let wInt = fromIntegral (natVal pw) :: Int
      sh = fromIntegral (countTrailingZeros wInt) :: Int64
      wE = fromIntegral wInt :: e Int64
   in ifThenElse (cap `le` 0) 0 ((cap + (wE - 1)) `rshift` fromIntegral sh)

tileAndLane :: forall w e. (KnownNat w, ExpBits e Int64, Num (e Int64)) => Proxy w -> e Int64 -> (e Int64, e Int64)
tileAndLane pw idx =
  let wInt = fromIntegral (natVal pw) :: Int
      sh = fromIntegral (countTrailingZeros wInt) :: Int64
      mask = fromIntegral (wInt - 1) :: Int64
   in (idx `rshift` fromIntegral sh, idx `iand` fromIntegral mask)

type AoSoAElemCtx w m e s =
  ( KnownNat w,
    Typeable w,
    Typeable s,
    JanusStruct (TileStruct w s),
    ScalarElemCtx m e (CStruct (TileStruct w s)),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s e),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileLane w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s e))
  )

type AoSoAInlineCtx n w e s =
  ( KnownNat (CeilDiv n w),
    Typeable n,
    Typeable (CeilDiv n w),
    JanusTyped e (FixedArray (CeilDiv n w) (CStruct (TileStruct w s)))
  )

readTilePtrLane ::
  forall w m e s.
  (AoSoAElemCtx w m e s, CmdAlloc m e) =>
  e (Ptr (CStruct (TileStruct w s))) ->
  e Int64 ->
  m (s e)
readTilePtrLane pTiles idx = do
  let (tIdx, lIdx) = tileAndLane (Proxy @w) idx
      ts = projectPtrs (ptrIndex pTiles tIdx) :: TileStruct w s (PtrOf e)
      tf = coerce ts :: s (TileField w (PtrOf e))
  to <$> gReadTileLane @w @m @e (from tf) lIdx

writeTilePtrLane ::
  forall w m e s.
  (AoSoAElemCtx w m e s, CmdAlloc m e) =>
  e (Ptr (CStruct (TileStruct w s))) ->
  e Int64 ->
  s e ->
  m ()
writeTilePtrLane pTiles idx val = do
  let (tIdx, lIdx) = tileAndLane (Proxy @w) idx
      ts = projectPtrs (ptrIndex pTiles tIdx) :: TileStruct w s (PtrOf e)
      tf = coerce ts :: s (TileField w (PtrOf e))
  gWriteTileLane @w @m @e (from tf) lIdx (from val)

instance StorageLayout (AoSoA w s) where
  type Elem e (AoSoA w s) = s e
  type Buf (AoSoA w s) = AoSoABuf w s
  type InlineBuf n (AoSoA w s) = InlineAoSoA n w s
  type LayoutElemCtx m e (AoSoA w s) = AoSoAElemCtx w m e s
  type LayoutInlineCtx n m e (AoSoA w s) = AoSoAInlineCtx n w e s

  nullBuf _ = AoSoABuf nullPtr
  isNullBuf _ (AoSoABuf p) = isNullVoidPtr (toVoidPtr p)
  newBufStateRefs (AoSoABuf p0) cap0 = do
    pRef <- newRef p0
    cRef <- newRef cap0
    pure
      ( (,) <$> (AoSoABuf <$> readRef pRef) <*> readRef cRef,
        \(AoSoABuf p) c -> writeRef pRef p >> writeRef cRef c
      )
  allocBuf alloc cap =
    AoSoABuf . unScalarBuf <$> scalarAllocBuf @(CStruct (TileStruct w s)) alloc (tileCountExpr (Proxy @w) cap)
  reallocBuf alloc (AoSoABuf oldP) oldCap newCap _ =
    AoSoABuf . unScalarBuf
      <$> scalarReallocBuf @(CStruct (TileStruct w s)) alloc (ScalarBuf oldP) (tileCountExpr (Proxy @w) oldCap) (tileCountExpr (Proxy @w) newCap) 0
  freeBuf alloc (AoSoABuf p) cap =
    scalarFreeBuf @(CStruct (TileStruct w s)) alloc (ScalarBuf p) (tileCountExpr (Proxy @w) cap)
  readBuf (AoSoABuf pTiles) i = readTilePtrLane @w pTiles i
  writeBuf (AoSoABuf pTiles) i v = writeTilePtrLane @w pTiles i v

  copyBuf (AoSoABuf pDst) dOff (AoSoABuf pSrc) sOff cnt =
    whenM_ (cnt `gt` 0) $
      ifThenElseM_
        ((dOff `eq` 0) `land` (sOff `eq` 0))
        (scalarCopyBuf @(CStruct (TileStruct w s)) (ScalarBuf pDst) 0 (ScalarBuf pSrc) 0 (tileCountExpr (Proxy @w) cnt))
        ( runDrain
            ( drainForEach $ \k -> do
                v <- readTilePtrLane @w pSrc (sOff + k)
                writeTilePtrLane @w pDst (dOff + k) v
            )
            (rangeS 0 cnt)
        )

  moveBuf (AoSoABuf pTiles) dOff sOff cnt =
    whenM_ ((cnt `gt` 0) `land` (dOff `ne` sOff)) $
      ifThenElseM_
        (dOff `lt` sOff)
        ( runDrain
            ( drainForEach $ \k -> do
                v <- readTilePtrLane @w pTiles (sOff + k)
                writeTilePtrLane @w pTiles (dOff + k) v
            )
            (rangeS 0 cnt)
        )
        ( runDrain drain_ $
            streamM (reverseRangeS 0 cnt) $ \k -> do
              v <- readTilePtrLane @w pTiles (sOff + k)
              writeTilePtrLane @w pTiles (dOff + k) v
        )

  inlineAsBuf _ pInline =
    let InlineAoSoA (PtrOf pArr) = projectPtrs pInline
     in pure (AoSoABuf (fixedArrayElemPtr pArr))

--------------------------------------------------------------------------------
-- 5. SOAOA w outer (Struct of Array of Struct of w-Lane SIMD Array)
--------------------------------------------------------------------------------

type family InlineSOAOABytes (nTiles :: Nat) (rep :: K.Type -> K.Type) :: Nat where
  InlineSOAOABytes nTiles (D1 _ f) = InlineSOAOABytes nTiles f
  InlineSOAOABytes nTiles (C1 _ f) = InlineSOAOABytes nTiles f
  InlineSOAOABytes nTiles (S1 _ _) = nTiles * 512 + 64
  InlineSOAOABytes nTiles (f :*: g) = InlineSOAOABytes nTiles f + InlineSOAOABytes nTiles g

class GSOAOAGroups (w :: Nat) m (e :: K.Type -> K.Type) (repId :: K.Type -> K.Type) (repVal :: K.Type -> K.Type) where
  gSOAOATotalBytes :: Proxy repId -> Proxy repVal -> e Int64 -> e Int64
  gSOAOACarve :: Proxy repId -> Proxy repVal -> e (Ptr (Ptr Word8)) -> e Int64 -> e (Ptr Word8) -> e Int64 -> e Int64 -> m (e Int64, e Int64)
  gSOAOARead :: Proxy repId -> e (Ptr (Ptr Word8)) -> e Int64 -> e Int64 -> m (repVal p, e Int64)
  gSOAOAWrite :: Proxy repId -> e (Ptr (Ptr Word8)) -> e Int64 -> e Int64 -> repVal p -> m (e Int64)
  gSOAOACopyFast :: Proxy repId -> Proxy repVal -> e (Ptr (Ptr Word8)) -> e (Ptr (Ptr Word8)) -> e Int64 -> e Int64 -> m (e Int64)

instance (Monad m, GSOAOAGroups w m e fId fVal) => GSOAOAGroups w m e (D1 c fId) (D1 c fVal) where
  gSOAOATotalBytes _ _ = gSOAOATotalBytes @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOACarve _ _ = gSOAOACarve @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOARead _ arr g0 idx = do
    (r, g1) <- gSOAOARead @w @m @e (Proxy @fId) arr g0 idx
    pure (M1 r, g1)
  gSOAOAWrite _ arr g0 idx (M1 v) = gSOAOAWrite @w @m @e (Proxy @fId) arr g0 idx v
  gSOAOACopyFast _ _ = gSOAOACopyFast @w @m @e (Proxy @fId) (Proxy @fVal)

instance (Monad m, GSOAOAGroups w m e fId fVal) => GSOAOAGroups w m e (C1 c fId) (C1 c fVal) where
  gSOAOATotalBytes _ _ = gSOAOATotalBytes @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOACarve _ _ = gSOAOACarve @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOARead _ arr g0 idx = do
    (r, g1) <- gSOAOARead @w @m @e (Proxy @fId) arr g0 idx
    pure (M1 r, g1)
  gSOAOAWrite _ arr g0 idx (M1 v) = gSOAOAWrite @w @m @e (Proxy @fId) arr g0 idx v
  gSOAOACopyFast _ _ = gSOAOACopyFast @w @m @e (Proxy @fId) (Proxy @fVal)

instance (Monad m, GSOAOAGroups w m e fId fVal) => GSOAOAGroups w m e (S1 c fId) (S1 c fVal) where
  gSOAOATotalBytes _ _ = gSOAOATotalBytes @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOACarve _ _ = gSOAOACarve @w @m @e (Proxy @fId) (Proxy @fVal)
  gSOAOARead _ arr g0 idx = do
    (r, g1) <- gSOAOARead @w @m @e (Proxy @fId) arr g0 idx
    pure (M1 r, g1)
  gSOAOAWrite _ arr g0 idx (M1 v) = gSOAOAWrite @w @m @e (Proxy @fId) arr g0 idx v
  gSOAOACopyFast _ _ = gSOAOACopyFast @w @m @e (Proxy @fId) (Proxy @fVal)

instance
  (Monad m, GSOAOAGroups w m e fId fVal, GSOAOAGroups w m e gId gVal, Num (e Int64)) =>
  GSOAOAGroups w m e (fId :*: gId) (fVal :*: gVal)
  where
  gSOAOATotalBytes _ _ nTiles =
    gSOAOATotalBytes @w @m @e (Proxy @fId) (Proxy @fVal) nTiles
      + gSOAOATotalBytes @w @m @e (Proxy @gId) (Proxy @gVal) nTiles
  gSOAOACarve _ _ arr g0 base off0 nTiles = do
    (g1, off1) <- gSOAOACarve @w @m @e (Proxy @fId) (Proxy @fVal) arr g0 base off0 nTiles
    gSOAOACarve @w @m @e (Proxy @gId) (Proxy @gVal) arr g1 base off1 nTiles
  gSOAOARead _ arr g0 idx = do
    (rf, g1) <- gSOAOARead @w @m @e (Proxy @fId) arr g0 idx
    (rg, g2) <- gSOAOARead @w @m @e (Proxy @gId) arr g1 idx
    pure (rf :*: rg, g2)
  gSOAOAWrite _ arr g0 idx (vf :*: vg) = do
    g1 <- gSOAOAWrite @w @m @e (Proxy @fId) arr g0 idx vf
    gSOAOAWrite @w @m @e (Proxy @gId) arr g1 idx vg
  gSOAOACopyFast _ _ dstArr srcArr g0 nTiles = do
    g1 <- gSOAOACopyFast @w @m @e (Proxy @fId) (Proxy @fVal) dstArr srcArr g0 nTiles
    gSOAOACopyFast @w @m @e (Proxy @gId) (Proxy @gVal) dstArr srcArr g1 nTiles

instance
  ( CmdAlloc m e,
    AoSoAElemCtx w m e sub
  ) =>
  GSOAOAGroups w m e (K1 i (sub Identity)) (K1 i (sub e))
  where
  gSOAOATotalBytes _ _ nTiles =
    alignUpExpr (nTiles * sizeOf (Proxy @(CStruct (TileStruct w sub)))) (fromIntegral janusSIMDAlignment)
  gSOAOACarve _ _ arr gIdx base off0 nTiles = do
    let grpBytes = alignUpExpr (nTiles * sizeOf (Proxy @(CStruct (TileStruct w sub)))) (fromIntegral janusSIMDAlignment)
        grpPtr = ptrAdd base off0
    pokeElemOff arr grpPtr gIdx
    pure (gIdx + 1, off0 + grpBytes)
  gSOAOARead _ arr gIdx idx = do
    rawP <- peekElemOff arr gIdx
    let pTiles = fromVoidPtr (toVoidPtr rawP) :: e (Ptr (CStruct (TileStruct w sub)))
    subVal <- readTilePtrLane @w pTiles idx
    pure (K1 subVal, gIdx + 1)
  gSOAOAWrite _ arr gIdx idx (K1 subVal) = do
    rawP <- peekElemOff arr gIdx
    let pTiles = fromVoidPtr (toVoidPtr rawP) :: e (Ptr (CStruct (TileStruct w sub)))
    writeTilePtrLane @w pTiles idx subVal
    pure (gIdx + 1)
  gSOAOACopyFast _ _ dstArr srcArr gIdx nTiles = do
    dRaw <- peekElemOff dstArr gIdx
    sRaw <- peekElemOff srcArr gIdx
    let bytes = nTiles * sizeOf (Proxy @(CStruct (TileStruct w sub)))
    cmdMemcpy (toVoidPtr dRaw) (toVoidPtr sRaw) bytes
    pure (gIdx + 1)

type SOAOAElemCtx w m e outer =
  ( KnownNat w,
    Typeable w,
    Typeable outer,
    KnownNat (CountGroups outer),
    Typeable (CountGroups outer),
    Generic (outer Identity),
    Generic (outer e),
    ExpPtr e (Ptr Word8),
    JanusTyped e (Ptr (Ptr Word8)),
    CmdStorable m e (Ptr (Ptr Word8)),
    ExpSized e (CStruct (SOAOABuf w outer)),
    JanusTyped e (CStruct (SOAOABuf w outer)),
    GSOAOAGroups w m e (Rep (outer Identity)) (Rep (outer e))
  )

type SOAOADirBytes outer = (CeilDiv (CountGroups outer) 8) * 64

type SOAOAInlineBytes n w outer =
  SOAOADirBytes outer + 64 + InlineSOAOABytes (CeilDiv n w) (Rep (outer Identity))

type SOAOAInlineCtx n w e outer =
  ( KnownNat (CeilDiv n w),
    KnownNat (Max1 (SOAOAInlineBytes n w outer)),
    Typeable n,
    Typeable (SOAOAInlineBytes n w outer),
    Typeable (Max1 (SOAOAInlineBytes n w outer)),
    JanusTyped e (FixedArray (Max1 (SOAOAInlineBytes n w outer)) Word8)
  )

soaoaDirBytesInt :: forall outer. (KnownNat (CountGroups outer)) => Proxy outer -> Int
soaoaDirBytesInt _ =
  alignUpInt (8 * fromIntegral (natVal (Proxy @(CountGroups outer)))) janusSIMDAlignment

soaoaStreamPtrAt ::
  forall w sub outer m e.
  ( CmdAlloc m e,
    JanusTyped e (CStruct (TileStruct w sub))
  ) =>
  SOAOABuf w outer e ->
  e Int64 ->
  m (AoSoABuf w sub e)
soaoaStreamPtrAt (SOAOABuf arr) grpIdx = do
  raw <- peekElemOff arr grpIdx
  pure (AoSoABuf (fromVoidPtr (toVoidPtr raw)))

instance StorageLayout (SOAOA w outer) where
  type Elem e (SOAOA w outer) = outer e
  type Buf (SOAOA w outer) = SOAOABuf w outer
  type InlineBuf n (SOAOA w outer) = InlineSOAOA n w outer
  type LayoutElemCtx m e (SOAOA w outer) = SOAOAElemCtx w m e outer
  type LayoutInlineCtx n m e (SOAOA w outer) = SOAOAInlineCtx n w e outer

  nullBuf _ = SOAOABuf nullPtr
  isNullBuf _ (SOAOABuf arr) = isNullVoidPtr (toVoidPtr arr)
  newBufStateRefs (SOAOABuf arr0) cap0 = do
    pRef <- newRef arr0
    cRef <- newRef cap0
    pure
      ( (,) <$> (SOAOABuf <$> readRef pRef) <*> readRef cRef,
        \(SOAOABuf arr) c -> writeRef pRef arr >> writeRef cRef c
      )

  allocBuf :: forall m e. (CmdAlloc m e, SOAOAElemCtx w m e outer) => Alloc e -> e Int64 -> m (SOAOABuf w outer e)
  allocBuf alloc cap = do
    let dirBytes = fromIntegral (soaoaDirBytesInt (Proxy @outer)) :: e Int64
        nTiles = tileCountExpr (Proxy @w) cap
        totBytes = dirBytes + gSOAOATotalBytes @w @m @e (Proxy @(Rep (outer Identity))) (Proxy @(Rep (outer e))) nTiles
    outRef <- newRef (nullPtr @e @(Ptr Word8))
    whenM_ (cap `gt` 0) $ do
      vpSlab <- allocBytesAligned alloc totBytes (fromIntegral janusSIMDAlignment)
      whenM_ (lnot (isNullVoidPtr vpSlab)) $ do
        let arr = fromVoidPtr vpSlab :: e (Ptr (Ptr Word8))
            payloadBase = ptrAdd (fromVoidPtr vpSlab :: e (Ptr Word8)) dirBytes
        _ <- gSOAOACarve @w @m @e (Proxy @(Rep (outer Identity))) (Proxy @(Rep (outer e))) arr 0 payloadBase 0 nTiles
        writeRef outRef arr
    SOAOABuf <$> readRef outRef

  reallocBuf ::
    forall m e.
    (CmdAlloc m e, SOAOAElemCtx w m e outer) =>
    Alloc e ->
    SOAOABuf w outer e ->
    e Int64 ->
    e Int64 ->
    e Int64 ->
    m (SOAOABuf w outer e)
  reallocBuf alloc oldB@(SOAOABuf oldArr) oldCap newCap liveLen = do
    newB@(SOAOABuf newArr) <- allocBuf @(SOAOA w outer) alloc newCap
    whenM_ (lnot (isNullVoidPtr (toVoidPtr oldArr)) `land` (oldCap `gt` 0)) $
      ifThenElseM_
        (newCap `le` 0)
        (freeBuf @(SOAOA w outer) alloc oldB oldCap)
        ( whenM_ (lnot (isNullVoidPtr (toVoidPtr newArr))) $ do
            let copyCnt = ifThenElse (liveLen `lt` newCap) liveLen newCap
            whenM_ (copyCnt `gt` 0) $ do
              _ <-
                gSOAOACopyFast @w @m @e
                  (Proxy @(Rep (outer Identity)))
                  (Proxy @(Rep (outer e)))
                  newArr
                  oldArr
                  0
                  (tileCountExpr (Proxy @w) copyCnt)
              pure ()
            freeBuf @(SOAOA w outer) alloc oldB oldCap
        )
    pure newB

  freeBuf ::
    forall m e.
    (CmdAlloc m e, SOAOAElemCtx w m e outer) =>
    Alloc e ->
    SOAOABuf w outer e ->
    e Int64 ->
    m ()
  freeBuf alloc (SOAOABuf arr) cap = do
    let dirBytes = fromIntegral (soaoaDirBytesInt (Proxy @outer)) :: e Int64
        totBytes = dirBytes + gSOAOATotalBytes @w @m @e (Proxy @(Rep (outer Identity))) (Proxy @(Rep (outer e))) (tileCountExpr (Proxy @w) cap)
    whenM_ (lnot (isNullVoidPtr (toVoidPtr arr)) `land` (cap `gt` 0)) $
      freeBytes alloc (toVoidPtr arr) totBytes

  readBuf ::
    forall m e.
    (CmdAlloc m e, SOAOAElemCtx w m e outer) =>
    SOAOABuf w outer e ->
    e Int64 ->
    m (outer e)
  readBuf (SOAOABuf arr) idx = do
    (repVal, _) <- gSOAOARead @w @m @e (Proxy @(Rep (outer Identity))) arr 0 idx
    pure (to repVal)

  writeBuf ::
    forall m e.
    (CmdAlloc m e, SOAOAElemCtx w m e outer) =>
    SOAOABuf w outer e ->
    e Int64 ->
    outer e ->
    m ()
  writeBuf (SOAOABuf arr) idx val = do
    _ <- gSOAOAWrite @w @m @e (Proxy @(Rep (outer Identity))) arr 0 idx (from val)
    pure ()

  copyBuf dst dOff src sOff cnt =
    whenM_ (cnt `gt` 0) $
      runDrain
        ( drainForEach $ \k -> do
            v <- readBuf @(SOAOA w outer) src (sOff + k)
            writeBuf @(SOAOA w outer) dst (dOff + k) v
        )
        (rangeS 0 cnt)

  moveBuf b dOff sOff cnt =
    whenM_ ((cnt `gt` 0) `land` (dOff `ne` sOff)) $
      ifThenElseM_
        (dOff `lt` sOff)
        ( runDrain
            ( drainForEach $ \k -> do
                v <- readBuf @(SOAOA w outer) b (sOff + k)
                writeBuf @(SOAOA w outer) b (dOff + k) v
            )
            (rangeS 0 cnt)
        )
        ( runDrain drain_ $
            streamM (reverseRangeS 0 cnt) $ \k -> do
              v <- readBuf @(SOAOA w outer) b (sOff + k)
              writeBuf @(SOAOA w outer) b (dOff + k) v
        )

  inlineAsBuf ::
    forall n m e.
    (CmdAlloc m e, SOAOAElemCtx w m e outer, SOAOAInlineCtx n w e outer) =>
    Proxy n ->
    e (Ptr (CStruct (InlineSOAOA n w outer))) ->
    m (SOAOABuf w outer e)
  inlineAsBuf _ pInline = do
    let InlineSOAOA (PtrOf pByteArr) = projectPtrs pInline
        rawBytes = fixedArrayElemPtr pByteArr
        alignedW = alignUpWord64Layout (ptrToWord64Expr rawBytes) (fromIntegral janusSIMDAlignment)
        alignedBase = word64ToPtrExpr alignedW :: e (Ptr Word8)
        dirBytes = fromIntegral (soaoaDirBytesInt (Proxy @outer)) :: e Int64
        arr = fromVoidPtr (toVoidPtr alignedBase) :: e (Ptr (Ptr Word8))
        payloadBase = ptrAdd alignedBase dirBytes
        nTiles = fromIntegral (natVal (Proxy @(CeilDiv n w))) :: e Int64
    _ <- gSOAOACarve @w @m @e (Proxy @(Rep (outer Identity))) (Proxy @(Rep (outer e))) arr 0 payloadBase 0 nTiles
    pure (SOAOABuf arr)

ifThenElseM_WithRef ::
  forall m e.
  (CmdAlloc m e) =>
  e Bool ->
  m (e (Ptr ())) ->
  m (e (Ptr ())) ->
  m (e (Ptr ()))
ifThenElseM_WithRef cond tAct fAct = do
  r <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    cond
    (tAct >>= writeRef r . fromVoidPtr)
    (fAct >>= writeRef r . fromVoidPtr)
  toVoidPtr <$> readRef r

alignUpWord64Layout :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64 -> e Word64
alignUpWord64Layout x al = (x + (al - 1)) `iand` inot (al - 1)

--------------------------------------------------------------------------------
-- SPMD / SIMD Gang Accessors Over SoA, AoSoA w, and SOAOA w
--------------------------------------------------------------------------------

class GSoASPMD (w :: Nat) m e (repPtr :: K.Type -> K.Type) (repGang :: K.Type -> K.Type) where
  gReadSoAGang :: Bool -> repPtr p -> SPMD w e Int64 -> SPMDM w m e (repGang p)
  gWriteSoAGang :: Bool -> repPtr p -> SPMD w e Int64 -> repGang p -> SPMDM w m e ()

instance (Monad m, GSoASPMD w m e fPtr fGang) => GSoASPMD w m e (M1 i c fPtr) (M1 i c fGang) where
  gReadSoAGang al (M1 p) idx = M1 <$> gReadSoAGang @w @m @e al p idx
  gWriteSoAGang al (M1 p) idx (M1 v) = gWriteSoAGang @w @m @e al p idx v

instance (Monad m, GSoASPMD w m e fPtr fGang, GSoASPMD w m e gPtr gGang) => GSoASPMD w m e (fPtr :*: gPtr) (fGang :*: gGang) where
  gReadSoAGang al (fp :*: gp) idx = (:*:) <$> gReadSoAGang @w @m @e al fp idx <*> gReadSoAGang @w @m @e al gp idx
  gWriteSoAGang al (fp :*: gp) idx (fv :*: gv) = gWriteSoAGang @w @m @e al fp idx fv >> gWriteSoAGang @w @m @e al gp idx gv

instance
  (Monad m, CmdStorable (SPMDM w m e) (SPMD w e) a, SPMDLayout w (Ptr a) ~ 'LPtr a) =>
  GSoASPMD w m e (K1 i (PtrOf e a)) (K1 i (SPMD w e a))
  where
  gReadSoAGang al (K1 (PtrOf p)) idx =
    let spPtr = if al then uniformAlignedPtr p else uniformPtr p
     in K1 <$> peekElemOff spPtr idx
  gWriteSoAGang al (K1 (PtrOf p)) idx (K1 v) =
    let spPtr = if al then uniformAlignedPtr p else uniformPtr p
     in pokeElemOff spPtr v idx

instance
  ( Monad m,
    Generic (sub (PtrOf e)),
    Generic (sub (SPMD w e)),
    GSoASPMD w m e (Rep (sub (PtrOf e))) (Rep (sub (SPMD w e)))
  ) =>
  GSoASPMD w m e (K1 i (sub (PtrOf e))) (K1 i (sub (SPMD w e)))
  where
  gReadSoAGang al (K1 subP) idx = K1 . to <$> gReadSoAGang @w @m @e al (from subP) idx
  gWriteSoAGang al (K1 subP) idx (K1 subV) = gWriteSoAGang @w @m @e al (from subP) idx (from subV)

readSoAGang ::
  forall w m e s.
  ( Functor m,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Bool ->
  PtrStruct s e ->
  SPMD w e Int64 ->
  SPMDM w m e (s (SPMD w e))
readSoAGang aligned (PtrStruct ptrs) idx =
  to <$> gReadSoAGang @w @m @e aligned (from ptrs) idx

writeSoAGang ::
  forall w m e s.
  ( Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Bool ->
  PtrStruct s e ->
  SPMD w e Int64 ->
  s (SPMD w e) ->
  SPMDM w m e ()
writeSoAGang aligned (PtrStruct ptrs) idx val =
  gWriteSoAGang @w @m @e aligned (from ptrs) idx (from val)

-- | Strip-mine @[0, len)@ of an @SoA s@ buffer into aligned unmasked SIMD gangs
-- plus a masked tail gang.
foreachSoASPMD ::
  forall w m e s.
  ( SPMDGang w m e,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  PtrStruct s e ->
  e Int64 ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachSoASPMD pw buf len step = do
  let wE = fromIntegral (natVal pw) :: e Int64
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = spmdAlignedUnitStride base
        withSPMD pw $ do
          cur <- readSoAGang @w True buf idx
          nxt <- step idx cur
          writeSoAGang @w True buf idx nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      cur <- readSoAGang @w True buf tailIdx
      nxt <- step tailIdx cur
      writeSoAGang @w True buf tailIdx nxt

class GTileSPMD (w :: Nat) m e (repTile :: K.Type -> K.Type) (repGang :: K.Type -> K.Type) where
  gReadTileSPMD :: Bool -> repTile p -> SPMDM w m e (repGang p)
  gWriteTileSPMD :: Bool -> repTile p -> repGang p -> SPMDM w m e ()

instance (Monad m, GTileSPMD w m e fTile fGang) => GTileSPMD w m e (M1 i c fTile) (M1 i c fGang) where
  gReadTileSPMD al (M1 t) = M1 <$> gReadTileSPMD @w @m @e al t
  gWriteTileSPMD al (M1 t) (M1 v) = gWriteTileSPMD @w @m @e al t v

instance (Monad m, GTileSPMD w m e fTile fGang, GTileSPMD w m e gTile gGang) => GTileSPMD w m e (fTile :*: gTile) (fGang :*: gGang) where
  gReadTileSPMD al (ft :*: rt) = (:*:) <$> gReadTileSPMD @w @m @e al ft <*> gReadTileSPMD @w @m @e al rt
  gWriteTileSPMD al (ft :*: rt) (fv :*: gv) = gWriteTileSPMD @w @m @e al ft fv >> gWriteTileSPMD @w @m @e al rt gv

instance
  ( Monad m,
    CmdStorable (SPMDM w m e) (SPMD w e) a,
    SPMDLayout w (Ptr a) ~ 'LPtr a,
    ExpPtrCast e,
    Num (e Int64),
    JanusTyped e a,
    JanusTyped e (FixedArray w a)
  ) =>
  GTileSPMD w m e (K1 i (TileField w (PtrOf e) a)) (K1 i (SPMD w e a))
  where
  gReadTileSPMD al (K1 (TileField (PtrOf pArr))) =
    let p0 = fixedArrayElemPtr pArr
        spPtr = if al then uniformAlignedPtr p0 else uniformPtr p0
        idx0 = if al then spmdAlignedUnitStride 0 else spmdUnitStride 0
     in K1 <$> peekElemOff spPtr idx0
  gWriteTileSPMD al (K1 (TileField (PtrOf pArr))) (K1 v) =
    let p0 = fixedArrayElemPtr pArr
        spPtr = if al then uniformAlignedPtr p0 else uniformPtr p0
        idx0 = if al then spmdAlignedUnitStride 0 else spmdUnitStride 0
     in pokeElemOff spPtr v idx0

instance
  ( Monad m,
    Generic (sub (TileField w (PtrOf e))),
    Generic (sub (SPMD w e)),
    GTileSPMD w m e (Rep (sub (TileField w (PtrOf e)))) (Rep (sub (SPMD w e)))
  ) =>
  GTileSPMD w m e (K1 i (sub (TileField w (PtrOf e)))) (K1 i (sub (SPMD w e)))
  where
  gReadTileSPMD al (K1 subT) = K1 . to <$> gReadTileSPMD @w @m @e al (from subT)
  gWriteTileSPMD al (K1 subT) (K1 subV) = gWriteTileSPMD @w @m @e al (from subT) (from subV)

-- | Check whether every field inside @TileStruct w s@ is naturally aligned to @w * sizeof(field)@
-- when the tile array base is 64-byte aligned.
tileIsNaturallySIMDAligned :: forall w s. (KnownNat w, JanusStruct (TileStruct w s)) => Bool
tileIsNaturallySIMDAligned =
  let (totSz, _) = cStructSizeAlign (Proxy @(TileStruct w s))
      wBytesMin = fromIntegral (natVal (Proxy @w)) * 4
   in totSz `mod` min janusSIMDAlignment wBytesMin == 0

readAoSoATileWithAlign ::
  forall w m e s.
  ( Functor m,
    KnownNat w,
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  Bool ->
  AoSoABuf w s e ->
  e Int64 ->
  SPMDM w m e (s (SPMD w e))
readAoSoATileWithAlign baseAligned (AoSoABuf pTiles) tileIdx =
  let ts = projectPtrs (ptrIndex pTiles tileIdx) :: TileStruct w s (PtrOf e)
      tf = coerce ts :: s (TileField w (PtrOf e))
      al = baseAligned && tileIsNaturallySIMDAligned @w @s
   in to <$> gReadTileSPMD @w @m @e al (from tf)

readAoSoATile ::
  forall w m e s.
  ( Functor m,
    KnownNat w,
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  AoSoABuf w s e ->
  e Int64 ->
  SPMDM w m e (s (SPMD w e))
readAoSoATile = readAoSoATileWithAlign True

writeAoSoATileWithAlign ::
  forall w m e s.
  ( KnownNat w,
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  Bool ->
  AoSoABuf w s e ->
  e Int64 ->
  s (SPMD w e) ->
  SPMDM w m e ()
writeAoSoATileWithAlign baseAligned (AoSoABuf pTiles) tileIdx val =
  let ts = projectPtrs (ptrIndex pTiles tileIdx) :: TileStruct w s (PtrOf e)
      tf = coerce ts :: s (TileField w (PtrOf e))
      al = baseAligned && tileIsNaturallySIMDAligned @w @s
   in gWriteTileSPMD @w @m @e al (from tf) (from val)

writeAoSoATile ::
  forall w m e s.
  ( KnownNat w,
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  AoSoABuf w s e ->
  e Int64 ->
  s (SPMD w e) ->
  SPMDM w m e ()
writeAoSoATile = writeAoSoATileWithAlign True

foreachAoSoASPMD ::
  forall w m e s.
  ( SPMDGang w m e,
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  AoSoABuf w s e ->
  e Int64 ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachAoSoASPMD pw buf len step = do
  let wE = fromIntegral (natVal pw) :: e Int64
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \t -> do
        base <- letM (t * wE)
        withSPMD pw $ do
          cur <- readAoSoATile @w buf t
          nxt <- step (spmdAlignedUnitStride base) cur
          writeAoSoATile @w buf t nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      cur <- readAoSoATile @w buf numFull
      nxt <- step tailIdx cur
      writeAoSoATile @w buf numFull nxt

class GSOAOASPMD (w :: Nat) m e (repId :: K.Type -> K.Type) (repGang :: K.Type -> K.Type) where
  gReadSOAOATile :: Bool -> Proxy repId -> e (Ptr (Ptr Word8)) -> e Int64 -> e Int64 -> SPMDM w m e (repGang p, e Int64)
  gWriteSOAOATile :: Bool -> Proxy repId -> e (Ptr (Ptr Word8)) -> e Int64 -> e Int64 -> repGang p -> SPMDM w m e (e Int64)

instance (Monad m, GSOAOASPMD w m e fId fGang) => GSOAOASPMD w m e (M1 i c fId) (M1 i c fGang) where
  gReadSOAOATile al _ arr g0 t = do
    (r, g1) <- gReadSOAOATile @w @m @e al (Proxy @fId) arr g0 t
    pure (M1 r, g1)
  gWriteSOAOATile al _ arr g0 t (M1 v) = gWriteSOAOATile @w @m @e al (Proxy @fId) arr g0 t v

instance (Monad m, GSOAOASPMD w m e fId fGang, GSOAOASPMD w m e gId gGang) => GSOAOASPMD w m e (fId :*: gId) (fGang :*: gGang) where
  gReadSOAOATile al _ arr g0 t = do
    (rf, g1) <- gReadSOAOATile @w @m @e al (Proxy @fId) arr g0 t
    (rg, g2) <- gReadSOAOATile @w @m @e al (Proxy @gId) arr g1 t
    pure (rf :*: rg, g2)
  gWriteSOAOATile al _ arr g0 t (vf :*: vg) = do
    g1 <- gWriteSOAOATile @w @m @e al (Proxy @fId) arr g0 t vf
    gWriteSOAOATile @w @m @e al (Proxy @gId) arr g1 t vg

instance
  ( KnownNat w,
    Monad m,
    CmdStorable m e (Ptr Word8),
    ExpPtr e (CStruct (TileStruct w sub)),
    ExpPtrCast e,
    ExpStructPtr e,
    Num (e Int64),
    JanusTyped e Word8,
    JanusTyped e (CStruct (TileStruct w sub)),
    JanusStruct (TileStruct w sub),
    Generic (TileStruct w sub (PtrOf e)),
    Generic (sub (TileField w (PtrOf e))),
    Generic (sub (SPMD w e)),
    Coercible (TileStruct w sub (PtrOf e)) (sub (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w sub Identity)) (Rep (TileStruct w sub (PtrOf e))),
    GTileSPMD w m e (Rep (sub (TileField w (PtrOf e)))) (Rep (sub (SPMD w e)))
  ) =>
  GSOAOASPMD w m e (K1 i (sub Identity)) (K1 i (sub (SPMD w e)))
  where
  gReadSOAOATile al _ arr gIdx tileIdx = do
    rawP <- liftSPMD (peekElemOff arr gIdx)
    let pTiles = fromVoidPtr (toVoidPtr rawP) :: e (Ptr (CStruct (TileStruct w sub)))
    subGang <- readAoSoATileWithAlign @w al (AoSoABuf pTiles) tileIdx
    pure (K1 subGang, gIdx + 1)
  gWriteSOAOATile al _ arr gIdx tileIdx (K1 subGang) = do
    rawP <- liftSPMD (peekElemOff arr gIdx)
    let pTiles = fromVoidPtr (toVoidPtr rawP) :: e (Ptr (CStruct (TileStruct w sub)))
    writeAoSoATileWithAlign @w al (AoSoABuf pTiles) tileIdx subGang
    pure (gIdx + 1)

readSOAOATileWithAlign ::
  forall w m e outer.
  ( Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e))),
    Monad m,
    Num (e Int64)
  ) =>
  Bool ->
  e (Ptr (Ptr Word8)) ->
  e Int64 ->
  SPMDM w m e (outer (SPMD w e))
readSOAOATileWithAlign al streamArr tileIdx = do
  (repG, _) <- gReadSOAOATile @w @m @e al (Proxy @(Rep (outer Identity))) streamArr 0 tileIdx
  pure (to repG)

readSOAOATile ::
  forall w m e outer.
  ( Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e))),
    Monad m,
    Num (e Int64)
  ) =>
  e (Ptr (Ptr Word8)) ->
  e Int64 ->
  SPMDM w m e (outer (SPMD w e))
readSOAOATile = readSOAOATileWithAlign True

writeSOAOATileWithAlign ::
  forall w m e outer.
  ( Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e))),
    Monad m,
    Num (e Int64)
  ) =>
  Bool ->
  e (Ptr (Ptr Word8)) ->
  e Int64 ->
  outer (SPMD w e) ->
  SPMDM w m e ()
writeSOAOATileWithAlign al streamArr tileIdx val = do
  _ <- gWriteSOAOATile @w @m @e al (Proxy @(Rep (outer Identity))) streamArr 0 tileIdx (from val)
  pure ()

writeSOAOATile ::
  forall w m e outer.
  ( Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e))),
    Monad m,
    Num (e Int64)
  ) =>
  e (Ptr (Ptr Word8)) ->
  e Int64 ->
  outer (SPMD w e) ->
  SPMDM w m e ()
writeSOAOATile = writeSOAOATileWithAlign True

foreachSOAOASPMD ::
  forall w m e outer.
  ( SPMDGang w m e,
    CmdAlloc m e,
    SOAOAElemCtx w m e outer,
    Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e)))
  ) =>
  Proxy w ->
  SOAOABuf w outer e ->
  e Int64 ->
  (SPMD w e Int64 -> outer (SPMD w e) -> SPMDM w m e (outer (SPMD w e))) ->
  m ()
foreachSOAOASPMD pw (SOAOABuf streamArr) len step = do
  let wE = fromIntegral (natVal pw) :: e Int64
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \t -> do
        base <- letM (t * wE)
        withSPMD pw $ do
          cur <- readSOAOATile @w @m @e @outer streamArr t
          nxt <- step (spmdAlignedUnitStride base) cur
          writeSOAOATile @w @m @e @outer streamArr t nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      cur <- readSOAOATile @w @m @e @outer streamArr numFull
      nxt <- step tailIdx cur
      writeSOAOATile @w @m @e @outer streamArr numFull nxt

--------------------------------------------------------------------------------
-- Unified Layout-Polymorphic SPMD / SIMD Classes & Buffer Combinators
--------------------------------------------------------------------------------

-- | Unified unit-stride SPMD gang load/store across @Scalar@, @SoA s@,
-- @AoSoA w s@, and @SOAOA w outer@ buffers starting at index @0@.
class (StorageLayout a, SPMDGang w m e) => LayoutUnitSPMD (w :: Nat) m e a where
  readAlignedGangBuf ::
    Proxy w ->
    Bool ->
    Buf a e ->
    e Int64 ->
    SPMD w e Int64 ->
    SPMDM w m e (Elem (SPMD w e) a)
  writeAlignedGangBuf ::
    Proxy w ->
    Bool ->
    Buf a e ->
    e Int64 ->
    SPMD w e Int64 ->
    Elem (SPMD w e) a ->
    SPMDM w m e ()

-- | Unit-stride SPMD gang load/store at arbitrary contiguous slice offsets
-- @[startOff .. startOff + spanLen)@ (for @RingBuffer@, @Deque@, @CsrGraph@, @EytzingerVec@).
class (StorageLayout a, SPMDGang w m e) => LayoutSliceSPMD (w :: Nat) m e a where
  readUnitGangBuf ::
    Proxy w ->
    Bool ->
    Buf a e ->
    SPMD w e Int64 ->
    SPMDM w m e (Elem (SPMD w e) a)
  writeUnitGangBuf ::
    Proxy w ->
    Bool ->
    Buf a e ->
    SPMD w e Int64 ->
    Elem (SPMD w e) a ->
    SPMDM w m e ()

-- | Arbitrary-index SIMD gather & scatter (@idx :: SPMD w e Int64@) across @Scalar@ and @SoA s@.
class (StorageLayout a, SPMDGang w m e) => LayoutGatherSPMD (w :: Nat) m e a where
  gatherBufSPMD ::
    Buf a e ->
    SPMD w e Int64 ->
    SPMDM w m e (Elem (SPMD w e) a)
  scatterBufSPMD ::
    Buf a e ->
    SPMD w e Int64 ->
    Elem (SPMD w e) a ->
    SPMDM w m e ()

#define INST_SPMD_SCALAR(TY) \
instance (SPMDGang w m e) => LayoutUnitSPMD w m e (TY) where { \
  readAlignedGangBuf _ al (ScalarBuf p) _ idx = \
    peekElemOff (if al then uniformAlignedPtr p else uniformPtr p) idx; \
  writeAlignedGangBuf _ al (ScalarBuf p) _ idx v = \
    pokeElemOff (if al then uniformAlignedPtr p else uniformPtr p) v idx \
}; \
instance (SPMDGang w m e) => LayoutSliceSPMD w m e (TY) where { \
  readUnitGangBuf _ al (ScalarBuf p) idx = \
    peekElemOff (if al then uniformAlignedPtr p else uniformPtr p) idx; \
  writeUnitGangBuf _ al (ScalarBuf p) idx v = \
    pokeElemOff (if al then uniformAlignedPtr p else uniformPtr p) v idx \
}; \
instance (SPMDGang w m e) => LayoutGatherSPMD w m e (TY) where { \
  gatherBufSPMD (ScalarBuf p) idx = peekElemOff (uniformAlignedPtr p) idx; \
  scatterBufSPMD (ScalarBuf p) idx v = pokeElemOff (uniformAlignedPtr p) v idx \
}

INST_SPMD_SCALAR(Int8)
INST_SPMD_SCALAR(Word8)
INST_SPMD_SCALAR(Int16)
INST_SPMD_SCALAR(Word16)
INST_SPMD_SCALAR(Int32)
INST_SPMD_SCALAR(Word32)
INST_SPMD_SCALAR(Int64)
INST_SPMD_SCALAR(Word64)
INST_SPMD_SCALAR(Float)
INST_SPMD_SCALAR(Double)

instance
  ( SPMDGang w m e,
    StorageLayout (SoA s),
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  LayoutUnitSPMD w m e (SoA s)
  where
  readAlignedGangBuf _ al buf _ idx = readSoAGang @w al buf idx
  writeAlignedGangBuf _ al buf _ idx v = writeSoAGang @w al buf idx v

instance
  ( SPMDGang w m e,
    StorageLayout (SoA s),
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  LayoutSliceSPMD w m e (SoA s)
  where
  readUnitGangBuf _ al buf idx = readSoAGang @w al buf idx
  writeUnitGangBuf _ al buf idx v = writeSoAGang @w al buf idx v

instance
  ( SPMDGang w m e,
    StorageLayout (SoA s),
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  LayoutGatherSPMD w m e (SoA s)
  where
  gatherBufSPMD buf idx = readSoAGang @w False buf idx
  scatterBufSPMD buf idx v = writeSoAGang @w False buf idx v

instance
  ( SPMDGang w m e,
    StorageLayout (AoSoA w s),
    ExpPtr e (CStruct (TileStruct w s)),
    ExpStructPtr e,
    JanusStruct (TileStruct w s),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  LayoutUnitSPMD w m e (AoSoA w s)
  where
  readAlignedGangBuf _ al buf tileIdx _ = readAoSoATileWithAlign @w al buf tileIdx
  writeAlignedGangBuf _ al buf tileIdx _ v = writeAoSoATileWithAlign @w al buf tileIdx v

instance
  ( SPMDGang w m e,
    StorageLayout (SOAOA w outer),
    Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e)))
  ) =>
  LayoutUnitSPMD w m e (SOAOA w outer)
  where
  readAlignedGangBuf _ al (SOAOABuf streamArr) tileIdx _ =
    readSOAOATileWithAlign @w @m @e @outer al streamArr tileIdx
  writeAlignedGangBuf _ al (SOAOABuf streamArr) tileIdx _ v =
    writeSOAOATileWithAlign @w @m @e @outer al streamArr tileIdx v

-- | Strip-mine @[0, len)@ in-place across @Scalar@, @SoA s@, @AoSoA w s@, or @SOAOA w outer@.
foreachBufSPMD ::
  forall w a m e.
  (LayoutUnitSPMD w m e a, CmdAlloc m e) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachBufSPMD pw al buf len step = do
  let wE = fromIntegral (natVal pw) :: e Int64
      mkIdx base = if al then spmdAlignedUnitStride base else spmdUnitStride base
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = mkIdx base
        withSPMD pw $ do
          cur <- readAlignedGangBuf @w @m @e @a pw al buf g idx
          nxt <- step idx cur
          writeAlignedGangBuf @w @m @e @a pw al buf g idx nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = mkIdx alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      cur <- readAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx
      nxt <- step tailIdx cur
      writeAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx nxt

-- | Read-only strip-mined SIMD scan over @[0, len)@ across @Scalar@, @SoA s@, @AoSoA w s@, or @SOAOA w outer@.
traverseBufSPMD_ ::
  forall w a m e.
  (LayoutUnitSPMD w m e a, CmdAlloc m e) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseBufSPMD_ pw al buf len body = do
  let wE = fromIntegral (natVal pw) :: e Int64
      mkIdx base = if al then spmdAlignedUnitStride base else spmdUnitStride base
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = mkIdx base
        withSPMD pw $ do
          cur <- readAlignedGangBuf @w @m @e @a pw al buf g idx
          body idx cur
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = mkIdx alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      cur <- readAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx
      body tailIdx cur

-- | Masked-safe SIMD fold over @[0, len)@ across @Scalar@, @SoA s@, @AoSoA w s@, or @SOAOA w outer@.
foldBufSPMD ::
  forall w a s m e.
  (LayoutUnitSPMD w m e a, CmdAlloc m e, SPMDTyped w e s) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldBufSPMD pw al buf len s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseBufSPMD_ pw al buf len $ \idx val -> do
    cur <- readRef accRef
    nxt <- step cur idx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)

-- | Strip-mined in-place SIMD map over @[0, len)@ gated by a per-lane occupancy predicate
-- (skipping completely empty gangs in O(1) via @mmovemask == 0@!).
foreachMaskedBufSPMD ::
  forall w a m e.
  (LayoutUnitSPMD w m e a, CmdAlloc m e) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  (SPMD w e Int64 -> SPMDM w m e (SPMD w e Bool)) ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachMaskedBufSPMD pw al buf len isOccupiedM step = do
  let wE = fromIntegral (natVal pw) :: e Int64
      mkIdx base = if al then spmdAlignedUnitStride base else spmdUnitStride base
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = mkIdx base
        withSPMD pw $ do
          occ <- isOccupiedM idx
          whenM_ occ $ do
            cur <- readAlignedGangBuf @w @m @e @a pw al buf g idx
            nxt <- step idx cur
            writeAlignedGangBuf @w @m @e @a pw al buf g idx nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = mkIdx alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      occ <- isOccupiedM tailIdx
      whenM_ occ $ do
        cur <- readAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx
        nxt <- step tailIdx cur
        writeAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx nxt

-- | Masked-safe SIMD fold over occupied lanes in @[0, len)@.
foldMaskedBufSPMD ::
  forall w a s m e.
  (LayoutUnitSPMD w m e a, CmdAlloc m e, SPMDTyped w e s) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  (SPMD w e Int64 -> SPMDM w m e (SPMD w e Bool)) ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldMaskedBufSPMD pw al buf len isOccupiedM s0 step = do
  let wE = fromIntegral (natVal pw) :: e Int64
      mkIdx base = if al then spmdAlignedUnitStride base else spmdUnitStride base
  accRef <- withSPMD pw (newRef s0)
  numFull <- letM (ifThenElse (len `gt` 0) (len `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = mkIdx base
        withSPMD pw $ do
          occ <- isOccupiedM idx
          whenM_ occ $ do
            val <- readAlignedGangBuf @w @m @e @a pw al buf g idx
            cur <- readRef accRef
            nxt <- step cur idx val
            writeRef accRef nxt
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` len) $ do
    let tailIdx = mkIdx alignedEnd
    withSPMDMasked (tailIdx `lt` uniform len) $ do
      occ <- isOccupiedM tailIdx
      whenM_ occ $ do
        val <- readAlignedGangBuf @w @m @e @a pw al buf numFull tailIdx
        cur <- readRef accRef
        nxt <- step cur tailIdx val
        writeRef accRef nxt
  withSPMD pw (readRef accRef)

-- | Strip-mine a contiguous slice @[startOff .. startOff + spanLen)@ in @buf@,
-- passing @(logicalBase + k)@ as the logical index to @step@.
foreachSliceBufSPMD ::
  forall w a m e.
  (LayoutSliceSPMD w m e a, CmdAlloc m e) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSliceBufSPMD pw al buf startOff spanLen logicalBase step =
  whenM_ (spanLen `gt` 0) $ do
    let wE = fromIntegral (natVal pw) :: e Int64
        mkPhys base = if al then spmdAlignedUnitStride base else spmdUnitStride base
    numFull <- letM (spanLen `quotient` wE)
    alignedLocalEnd <- letM (numFull * wE)
    runDrain
      ( drainForEach $ \g -> do
          localBase <- letM (g * wE)
          let physIdx = mkPhys (startOff + localBase)
              logIdx = spmdUnitStride (logicalBase + localBase)
          withSPMD pw $ do
            cur <- readUnitGangBuf @w @m @e @a pw al buf physIdx
            nxt <- step logIdx cur
            writeUnitGangBuf @w @m @e @a pw al buf physIdx nxt
      )
      (rangeS 0 numFull)
    whenM_ (alignedLocalEnd `lt` spanLen) $ do
      let tailLocal = spmdAlignedUnitStride alignedLocalEnd
          physTail = mkPhys (startOff + alignedLocalEnd)
          logTail = spmdUnitStride (logicalBase + alignedLocalEnd)
      withSPMDMasked (tailLocal `lt` uniform spanLen) $ do
        cur <- readUnitGangBuf @w @m @e @a pw al buf physTail
        nxt <- step logTail cur
        writeUnitGangBuf @w @m @e @a pw al buf physTail nxt

traverseSliceBufSPMD_ ::
  forall w a m e.
  (LayoutSliceSPMD w m e a, CmdAlloc m e) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSliceBufSPMD_ pw al buf startOff spanLen logicalBase body =
  whenM_ (spanLen `gt` 0) $ do
    let wE = fromIntegral (natVal pw) :: e Int64
        mkPhys base = if al then spmdAlignedUnitStride base else spmdUnitStride base
    numFull <- letM (spanLen `quotient` wE)
    alignedLocalEnd <- letM (numFull * wE)
    runDrain
      ( drainForEach $ \g -> do
          localBase <- letM (g * wE)
          let physIdx = mkPhys (startOff + localBase)
              logIdx = spmdUnitStride (logicalBase + localBase)
          withSPMD pw $ do
            cur <- readUnitGangBuf @w @m @e @a pw al buf physIdx
            body logIdx cur
      )
      (rangeS 0 numFull)
    whenM_ (alignedLocalEnd `lt` spanLen) $ do
      let tailLocal = spmdAlignedUnitStride alignedLocalEnd
          physTail = mkPhys (startOff + alignedLocalEnd)
          logTail = spmdUnitStride (logicalBase + alignedLocalEnd)
      withSPMDMasked (tailLocal `lt` uniform spanLen) $ do
        cur <- readUnitGangBuf @w @m @e @a pw al buf physTail
        body logTail cur

foldSliceBufSPMD ::
  forall w a s m e.
  (LayoutSliceSPMD w m e a, CmdAlloc m e, SPMDTyped w e s) =>
  Proxy w ->
  Bool ->
  Buf a e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSliceBufSPMD pw al buf startOff spanLen logicalBase s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseSliceBufSPMD_ pw al buf startOff spanLen logicalBase $ \logIdx val -> do
    cur <- readRef accRef
    nxt <- step cur logIdx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)
