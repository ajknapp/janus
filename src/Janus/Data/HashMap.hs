{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Data.HashMap
  ( -- * HashKey Class (Powered by MurmurHash, Supporting Scalars & HKD Records)
    HashKey (..),
    GHashStruct (..),
    HashFilterCtx,
    HashLayoutCtx,

    -- * SwissTable Flat Open-Addressing HashMap Spine & Handle
    HashMapSpine (..),
    HashMap (..),
    HashMapCtx,

    -- * Lifecycle
    new,
    withCapacity,
    free,

    -- * Queries & Capacity Management
    size,
    capacity,
    isEmpty,
    clear,
    reserve,

    -- * Lookup, Insertion, Update, & Deletion
    insert,
    insertWith,
    lookup,
    withLookup,
    lookupPtrScalar,
    member,
    delete,

    -- * SWAR-Accelerated Streams, SPMD Kernels, & Drains
    swarOccupiedSlotsS,
    swarBitsS,
    probeGroupsS,
    toStream,
    keysS,
    valuesS,
    foreachValuesSPMD,
    foldValuesSPMD,
    containsKeyBatchSPMD,
    lookupScalarBatchSPMD,
    drainToHashMap,
    fromStream,
    drainAppendHashMap,
    drainAppendHashMapWith,
  )
where

import Control.Monad.Cont (ContT (..))
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
import GHC.Generics
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocBytesAligned,
    allocStruct,
    freeBytes,
    freeStruct,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), janusSIMDAlignment)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (CmdJanus, KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFind, drainFirst, drainForEach, drainLength, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream
  ( Stream (..),
    filterMapS,
    iterateS,
    statefulS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (ExpMurmurHash (..), defaultHashSalt)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    uniform,
    uniformAlignedPtr,
    withSPMD,
  )
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- HashKey Class (MurmurHash + Equality Over Scalars & Generic HKD Structs)
--------------------------------------------------------------------------------

class HashKey (e :: K.Type -> K.Type) a where
  hashElemWithSalt :: e Word64 -> Elem e a -> e Word64
  eqElem :: Elem e a -> Elem e a -> e Bool

#define INST_HASH_SCALAR(TY) \
instance (ExpMurmurHash e (e (TY)), ExpEq e (TY)) => HashKey e (TY) where { \
  hashElemWithSalt = murmurHashWithSalt; \
  eqElem = eq \
}

INST_HASH_SCALAR(Bool)
INST_HASH_SCALAR(CBool)
INST_HASH_SCALAR(Int8)
INST_HASH_SCALAR(Int16)
INST_HASH_SCALAR(Int32)
INST_HASH_SCALAR(Int64)
INST_HASH_SCALAR(Word8)
INST_HASH_SCALAR(Word16)
INST_HASH_SCALAR(Word32)
INST_HASH_SCALAR(Word64)
INST_HASH_SCALAR(Float)
INST_HASH_SCALAR(Double)
INST_HASH_SCALAR(CChar)
INST_HASH_SCALAR(CSChar)
INST_HASH_SCALAR(CUChar)
INST_HASH_SCALAR(CShort)
INST_HASH_SCALAR(CUShort)
INST_HASH_SCALAR(CInt)
INST_HASH_SCALAR(CUInt)
INST_HASH_SCALAR(CLong)
INST_HASH_SCALAR(CULong)
INST_HASH_SCALAR(CLLong)
INST_HASH_SCALAR(CULLong)
INST_HASH_SCALAR(CSize)
INST_HASH_SCALAR(CFloat)
INST_HASH_SCALAR(CDouble)

instance (ExpMurmurHash e (e (Ptr p)), ExpEq e (Ptr p)) => HashKey e (Ptr p) where
  hashElemWithSalt = murmurHashWithSalt
  eqElem = eq

class GHashStruct (e :: K.Type -> K.Type) (rep :: K.Type -> K.Type) where
  gHashStruct :: e Word64 -> rep p -> e Word64
  gEqStruct :: rep p -> rep p -> e Bool

instance (GHashStruct e f) => GHashStruct e (M1 i c f) where
  gHashStruct s (M1 x) = gHashStruct @e s x
  gEqStruct (M1 x) (M1 y) = gEqStruct @e x y

instance (ExpBool e, GHashStruct e f, GHashStruct e g) => GHashStruct e (f :*: g) where
  gHashStruct s0 (fx :*: gx) =
    let s1 = gHashStruct @e s0 fx
     in gHashStruct @e s1 gx
  gEqStruct (fx :*: gx) (fy :*: gy) =
    gEqStruct @e fx fy `land` gEqStruct @e gx gy

instance (ExpMurmurHash e (e a), ExpEq e a) => GHashStruct e (K1 i (e a)) where
  gHashStruct s (K1 x) = murmurHashWithSalt s x
  gEqStruct (K1 x) (K1 y) = x `eq` y

instance
  (Generic (sub e), GHashStruct e (Rep (sub e))) =>
  GHashStruct e (K1 i (sub e))
  where
  gHashStruct s (K1 sub) = gHashStruct @e s (from sub)
  gEqStruct (K1 x) (K1 y) = gEqStruct @e (from x) (from y)

instance (Generic (s e), GHashStruct e (Rep (s e))) => HashKey e (AoS s) where
  hashElemWithSalt s v = gHashStruct @e s (from v)
  eqElem x y = gEqStruct @e (from x) (from y)

instance (Generic (s e), GHashStruct e (Rep (s e))) => HashKey e (SoA s) where
  hashElemWithSalt s v = gHashStruct @e s (from v)
  eqElem x y = gEqStruct @e (from x) (from y)

instance (Generic (s e), GHashStruct e (Rep (s e))) => HashKey e (AoSoA w s) where
  hashElemWithSalt s v = gHashStruct @e s (from v)
  eqElem x y = gEqStruct @e (from x) (from y)

instance (Generic (outer e), GHashStruct e (Rep (outer e))) => HashKey e (SOAOA w outer) where
  hashElemWithSalt s v = gHashStruct @e s (from v)
  eqElem x y = gEqStruct @e (from x) (from y)

--------------------------------------------------------------------------------
-- SwissTable Control Byte & 8-Byte SWAR Group Operations
--------------------------------------------------------------------------------

ctrlEmptyByte :: Word8
ctrlEmptyByte = 0x80

ctrlDeletedByte :: Word8
ctrlDeletedByte = 0xFE

-- | Exact carry-free 8-byte SWAR match mask: bit @(8*i + 7)@ is @1@ iff byte @i@ of @w@ equals @h2@ (@0 <= h2 <= 0x7F@).
swarMatchH2 ::
  forall e.
  (ExpBits e Word64, ExpIntegralCast e Word8 Word64, Num (e Word64)) =>
  e Word64 ->
  e Word8 ->
  e Word64
swarMatchH2 w h2 =
  let splat = toIntegral h2 * 0x0101010101010101
      x = w `ieor` splat
      low7 = x `iand` 0x7F7F7F7F7F7F7F7F
      hasZeroLow7 = inot (low7 + 0x7F7F7F7F7F7F7F7F)
   in hasZeroLow7 `iand` inot x `iand` 0x8080808080808080

-- | Bit @(8*i + 7)@ is @1@ iff byte @i@ is @EMPTY@ (@0x80@, NOT @DELETED = 0xFE@ or @Full = 0x00..0x7F@).
swarMatchEmpty :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmpty w = w `iand` inot (w `lshift` 6) `iand` 0x8080808080808080

-- | Bit @(8*i + 7)@ is @1@ iff byte @i@ is @EMPTY@ (@0x80@) or @DELETED@ (@0xFE@).
swarMatchEmptyOrDeleted :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmptyOrDeleted w = w `iand` 0x8080808080808080

-- | Bit @(8*i + 7)@ is @1@ iff byte @i@ is occupied (@0x00..0x7F@).
swarMatchOccupied :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchOccupied w = inot w `iand` 0x8080808080808080

-- | Extract byte index @0..7@ of the lowest set byte-MSB bit in @m@ (@m != 0@).
swarLowestByteIdx ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Int64, Num (e Word64)) =>
  e Word64 ->
  e Int64
swarLowestByteIdx m =
  let lsb = (m `iand` (0 - m)) `rshift` 7
   in toIntegral ((lsb * 0x0001020304050607) `rshift` 56)

splitHash ::
  forall e.
  (ExpBits e Word64, ExpIntegralCast e Word64 Word8, Num (e Word64)) =>
  e Word64 ->
  (e Word64, e Word8)
splitHash h =
  let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
   in (h, h2)

maxLoadForCap :: (ExpBits e Int64, Num (e Int64)) => e Int64 -> e Int64
maxLoadForCap cap = cap - (cap `rshift` 3)

--------------------------------------------------------------------------------
-- HashMap Spine & Handle
--------------------------------------------------------------------------------

data HashMapSpine k v f = HashMapSpine
  { hmsCtrl :: !(f (Ptr Word8)),
    hmsKeys :: !(f (CStruct (Buf k))),
    hmsVals :: !(f (CStruct (Buf v))),
    hmsCap :: !(f Int64),
    hmsLen :: !(f Int64),
    hmsGrowthLeft :: !(f Int64),
    hmsSeed :: !(f Word64)
  }
  deriving stock (Generic)

instance (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) => JanusStruct (HashMapSpine k v)

newtype HashMap (e :: K.Type -> K.Type) k v = HashMap
  { unHashMap :: e (Ptr (CStruct (HashMapSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (HashMap Identity k v)

deriving stock instance Show (HashMap Identity k v)

type HashFilterCtx m e k = (CmdJanus m e, StorageLayout k, Typeable k, HashKey e k)

type HashLayoutCtx m e k = (LayoutCtx m e k, HashKey e k)

type HashMapCtx m e k v = (HashLayoutCtx m e k, LayoutCtx m e v)

new ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  m (HashMap e k v)
new alloc = do
  hdr <- allocStruct @(CStruct (HashMapSpine k v)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  v0 <- allocBuf @v alloc 0
  poke (getPtrOf (hmsCtrl f)) nullPtr
  pokeBufStruct (getPtrOf (hmsKeys f)) k0
  pokeBufStruct (getPtrOf (hmsVals f)) v0
  poke (getPtrOf (hmsCap f)) 0
  poke (getPtrOf (hmsLen f)) 0
  poke (getPtrOf (hmsGrowthLeft f)) 0
  poke (getPtrOf (hmsSeed f)) defaultHashSalt
  pure (HashMap hdr)

withCapacity ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (HashMap e k v)
withCapacity alloc minElems = do
  hm <- new alloc
  whenM_ (minElems `gt` 0) $
    reserve alloc hm minElems
  pure hm

free ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  m ()
free alloc (HashMap hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hmsCap f))
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hmsCtrl f))
    kb <- peekBufStruct (getPtrOf (hmsKeys f))
    vb <- peekBufStruct (getPtrOf (hmsVals f))
    freeBytes alloc (toVoidPtr ctrl) (cap + 8)
    freeBuf alloc kb cap
    freeBuf alloc vb cap
  freeStruct alloc hdr

size :: (HashMapCtx m e k v) => HashMap e k v -> m (e Int64)
size (HashMap hdr) = peek (getPtrOf (hmsLen (projectPtrs hdr)))

capacity :: (HashMapCtx m e k v) => HashMap e k v -> m (e Int64)
capacity (HashMap hdr) = peek (getPtrOf (hmsCap (projectPtrs hdr)))

isEmpty :: (HashMapCtx m e k v) => HashMap e k v -> m (e Bool)
isEmpty hm = (`eq` 0) <$> size hm

-- | Reset all control bytes to @EMPTY (0x80)@ and restore full @7/8@ growth budget without reallocating.
clear ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  m ()
clear (HashMap hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hmsCap f))
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hmsCtrl f))
    cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (cap + 8)
    poke (getPtrOf (hmsGrowthLeft f)) (maxLoadForCap cap)
  poke (getPtrOf (hmsLen f)) 0

-- | Stream all set byte-lane indices (@0 .. 7@) from a 64-bit SWAR match mask (@0x80@ MSBs).
swarBitsS ::
  forall m e.
  ( CmdAlloc m e
  ) =>
  e Word64 ->
  Stream m (e Int64)
swarBitsS mask0 =
  unfoldWhileM mask0 (\m -> pure (m `ne` 0)) $ \m ->
    pure (m `iand` (m - 1), swarLowestByteIdx m)

-- | Infinite triangular group-probe stream @g0, (g0 + 1) & mask, (g0 + 3) & mask, ...@
-- visiting all @mask + 1@ power-of-2 8-byte groups in a permutation.
probeGroupsS ::
  forall m e.
  ( CmdAlloc m e
  ) =>
  e Int64 ->
  e Int64 ->
  Stream m (e Int64)
probeGroupsS g0 groupMask =
  fst
    <$> iterateS
      (g0, 0 :: e Int64)
      (\(g, st) -> let st' = st + 1 in ((g + st') `iand` groupMask, st'))

-- | SWAR-accelerated pull 'Stream' yielding every occupied slot index across @[0 .. numGroups - 1]@.
swarOccupiedSlotsS ::
  forall m e.
  ( CmdAlloc m e
  ) =>
  e (Ptr Word64) ->
  e Int64 ->
  Stream m (e Int64)
swarOccupiedSlotsS ctrlWords numGroups =
  statefulS
    ((,) <$> newRef (0 :: e Int64) <*> newRef (0 :: e Word64))
    $ \(gRef, maskRef) -> ContT $ \kStep -> do
      curMask <- readRef maskRef
      ifThenElseM_
        (curMask `ne` 0)
        ( do
            gCur <- readRef gRef
            let lane = swarLowestByteIdx curMask
                slot = ((gCur - 1) `lshift` 3) + lane
            writeRef maskRef (curMask `iand` (curMask - 1))
            kStep (Yield slot)
        )
        ( do
            g <- readRef gRef
            ifThenElseM_
              (g `ge` numGroups)
              (kStep Stop)
              ( do
                  w <- peekElemOff ctrlWords g
                  writeRef gRef (g + 1)
                  writeRef maskRef (swarMatchOccupied w)
                  kStep Skip
              )
        )

rehashTo ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  e Int64 ->
  m ()
rehashTo alloc (HashMap hdr) newCap = do
  let f = projectPtrs hdr
  oldCap <- peek (getPtrOf (hmsCap f))
  oldCtrl <- peek (getPtrOf (hmsCtrl f))
  oldKeys <- peekBufStruct (getPtrOf (hmsKeys f))
  oldVals <- peekBufStruct (getPtrOf (hmsVals f))
  seed <- peek (getPtrOf (hmsSeed f))

  newCtrlVP <- allocBytesAligned alloc (newCap + 8) (fromIntegral janusSIMDAlignment)
  let newCtrl = fromVoidPtr newCtrlVP :: e (Ptr Word8)
      newCtrlWords = fromVoidPtr newCtrlVP :: e (Ptr Word64)
  cmdMemset newCtrlVP (fromIntegral ctrlEmptyByte) (newCap + 8)
  newKeys <- allocBuf @k alloc newCap
  newVals <- allocBuf @v alloc newCap

  let newNumGroups = newCap `rshift` 3
      newGroupMask = newNumGroups - 1
  rehashedCntRef <- newRef (0 :: e Int64)

  whenM_ (oldCap `gt` 0) $ do
    let oldCtrlWords = fromVoidPtr (toVoidPtr oldCtrl) :: e (Ptr Word64)
        oldNumGroups = oldCap `rshift` 3
    rehashed <-
      runDrain drainLength $
        streamM (swarOccupiedSlotsS oldCtrlWords oldNumGroups) $ \oldSlot -> do
          key <- readBuf oldKeys oldSlot
          val <- readBuf oldVals oldSlot
          h <- letM (hashElemWithSalt @e @k seed key)
          let (_, h2) = splitHash h
              g0 = toIntegral h `iand` newGroupMask
          (_, dstSlot) <-
            runDrain (drainFirst 0) $
              filterMapS
                ( \pg -> do
                    nw <- peekElemOff newCtrlWords pg
                    let emp = swarMatchEmpty nw
                    pure (emp `ne` 0, (pg `lshift` 3) + swarLowestByteIdx emp)
                )
                (probeGroupsS g0 newGroupMask)
          pokeElemOff newCtrl h2 dstSlot
          writeBuf newKeys dstSlot key
          writeBuf newVals dstSlot val
    writeRef rehashedCntRef rehashed
    freeBytes alloc (toVoidPtr oldCtrl) (oldCap + 8)
    freeBuf alloc oldKeys oldCap
    freeBuf alloc oldVals oldCap

  rehashedCnt <- readRef rehashedCntRef
  poke (getPtrOf (hmsCtrl f)) newCtrl
  pokeBufStruct (getPtrOf (hmsKeys f)) newKeys
  pokeBufStruct (getPtrOf (hmsVals f)) newVals
  poke (getPtrOf (hmsCap f)) newCap
  poke (getPtrOf (hmsLen f)) rehashedCnt
  poke (getPtrOf (hmsGrowthLeft f)) (maxLoadForCap newCap - rehashedCnt)

-- | Ensure capacity for at least @curLen + additional@ live entries without rehashing.
reserve ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  e Int64 ->
  m ()
reserve alloc hm@(HashMap hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (hmsLen f))
  curCap <- peek (getPtrOf (hmsCap f))
  growthLeft <- peek (getPtrOf (hmsGrowthLeft f))
  whenM_ (additional `gt` growthLeft) $ do
    neededElems <- letM (curLen + additional)
    (_, targetCap) <-
      runDrain
        (drainFind 16 (\cand -> maxLoadForCap cand `ge` neededElems))
        (iterateS (ifThenElse (curCap `lt` 16) 16 curCap) (`lshift` 1))
    rehashTo alloc hm targetCap

-- | Probe for an existing key, returning @(found, slotIdx)@.
findSlot ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  Elem e k ->
  m (e Bool, e Int64)
findSlot (HashMap hdr) key = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hmsCap f))
  foundRef <- newRef false
  slotRef <- newRef (0 :: e Int64)
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hmsCtrl f))
    kb <- peekBufStruct (getPtrOf (hmsKeys f))
    seed <- peek (getPtrOf (hmsSeed f))
    h <- letM (hashElemWithSalt @e @k seed key)
    let (_, h2) = splitHash h
        ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (cap `rshift` 3) - 1
        g0 = toIntegral h `iand` groupMask
    (_, (_, hitFound, hitSlot)) <-
      runDrain
        (drainFind (true, false, 0) (\(done, _, _) -> done))
        $ streamM (probeGroupsS g0 groupMask)
        $ \g -> do
          w <- peekElemOff ctrlWords g
          (hitInGrp, (_, candSlot)) <-
            runDrain (drainFind (false, 0) fst) $
              streamM (swarBitsS (swarMatchH2 w h2)) $ \lane -> do
                let s = (g `lshift` 3) + lane
                candKey <- readBuf kb s
                pure (eqElem @e @k candKey key, s)
          let done = hitInGrp `lor` (swarMatchEmpty w `ne` 0)
          pure (done, hitInGrp, candSlot)
    writeRef foundRef hitFound
    writeRef slotRef hitSlot
  (,) <$> readRef foundRef <*> readRef slotRef

-- | Insert or overwrite @(key, val)@.
insert ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert alloc hm key val = insertWith alloc hm (\_ newV -> newV) key val

-- | Insert @(key, val)@, or if @key@ is already present, update its value with @combine oldVal newVal@.
insertWith ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  (Elem e v -> Elem e v -> Elem e v) ->
  Elem e k ->
  Elem e v ->
  m ()
insertWith alloc hm@(HashMap hdr) combine key val = do
  (exists, existingSlot) <- findSlot hm key
  let f = projectPtrs hdr
  ifThenElseM_
    exists
    ( do
        vb <- peekBufStruct (getPtrOf (hmsVals f))
        oldV <- readBuf vb existingSlot
        writeBuf vb existingSlot (combine oldV val)
    )
    ( do
        growthLeft <- peek (getPtrOf (hmsGrowthLeft f))
        whenM_ (growthLeft `le` 0) $ do
          curCap <- peek (getPtrOf (hmsCap f))
          curLen <- peek (getPtrOf (hmsLen f))
          let nextCap =
                ifThenElse
                  (curCap `eq` 0)
                  16
                  (ifThenElse ((curLen * 2) `lt` maxLoadForCap curCap) curCap (curCap `lshift` 1))
          rehashTo alloc hm nextCap

        cap <- peek (getPtrOf (hmsCap f))
        ctrl <- peek (getPtrOf (hmsCtrl f))
        kb <- peekBufStruct (getPtrOf (hmsKeys f))
        vb <- peekBufStruct (getPtrOf (hmsVals f))
        seed <- peek (getPtrOf (hmsSeed f))
        h <- letM (hashElemWithSalt @e @k seed key)
        let (_, h2) = splitHash h
            ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
            groupMask = (cap `rshift` 3) - 1
            g0 = toIntegral h `iand` groupMask
        (_, slot) <-
          runDrain (drainFirst 0) $
            filterMapS
              ( \g -> do
                  w <- peekElemOff ctrlWords g
                  let avail = swarMatchEmptyOrDeleted w
                  pure (avail `ne` 0, (g `lshift` 3) + swarLowestByteIdx avail)
              )
              (probeGroupsS g0 groupMask)
        prevCtrl <- peekElemOff ctrl slot
        pokeElemOff ctrl h2 slot
        writeBuf kb slot key
        writeBuf vb slot val
        curLen <- peek (getPtrOf (hmsLen f))
        poke (getPtrOf (hmsLen f)) (curLen + 1)
        whenM_ (prevCtrl `eq` fromIntegral ctrlEmptyByte) $ do
          gl <- peek (getPtrOf (hmsGrowthLeft f))
          poke (getPtrOf (hmsGrowthLeft f)) (gl - 1)
    )

member ::
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  Elem e k ->
  m (e Bool)
member hm key = fst <$> findSlot hm key

lookup ::
  forall k v s m e.
  (HashMapCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  HashMap e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal hm@(HashMap hdr) key = do
  (found, slot) <- findSlot hm key
  valRef <- newRef defVal
  whenM_ found $ do
    vb <- peekBufStruct (getPtrOf (hmsVals (projectPtrs hdr)))
    readBuf vb slot >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withLookup ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup hm@(HashMap hdr) key onMiss onHit = do
  (found, slot) <- findSlot hm key
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (hmsVals (projectPtrs hdr)))
        v <- readBuf vb slot
        onHit v
    )
    onMiss

lookupPtrScalar ::
  forall k v m e.
  (HashMapCtx m e k v, Buf v ~ ScalarBuf v, ExpPtr e v, KnownJanusType e (Ptr v)) =>
  HashMap e k v ->
  Elem e k ->
  m (e (Ptr v))
lookupPtrScalar hm@(HashMap hdr) key = do
  (found, slot) <- findSlot hm key
  outRef <- newRef (nullPtr @e @v)
  whenM_ found $ do
    ScalarBuf pVals <- peekBufStruct (getPtrOf (hmsVals (projectPtrs hdr)))
    writeRef outRef (ptrIndex pVals slot)
  readRef outRef

-- | Delete @key@ if present, returning @true@ if an entry was removed.
-- Note: if the 8-byte group contains at least one @EMPTY@ byte, we can reset the slot
-- directly to @EMPTY@ and reclaim @growthLeft@ without leaving a tombstone!
delete ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  Elem e k ->
  m (e Bool)
delete hm@(HashMap hdr) key = do
  (found, slot) <- findSlot hm key
  whenM_ found $ do
    let f = projectPtrs hdr
    ctrl <- peek (getPtrOf (hmsCtrl f))
    curLen <- peek (getPtrOf (hmsLen f))
    poke (getPtrOf (hmsLen f)) (curLen - 1)
    let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        grp = slot `rshift` 3
    w <- peekElemOff ctrlWords grp
    ifThenElseM_
      (swarMatchEmpty w `ne` 0)
      ( do
          pokeElemOff ctrl (fromIntegral ctrlEmptyByte) slot
          gl <- peek (getPtrOf (hmsGrowthLeft f))
          poke (getPtrOf (hmsGrowthLeft f)) (gl + 1)
      )
      (pokeElemOff ctrl (fromIntegral ctrlDeletedByte) slot)
  pure found

-- | SWAR-accelerated 'Stream' over all occupied @(key, val)@ pairs, skipping empty/deleted
-- 8-slot groups in a single 64-bit word test.
toStream ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  HashMap e k v ->
  Stream m (Elem e k, Elem e v)
toStream (HashMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (hmsCap f))
            <*> peek (getPtrOf (hmsCtrl f))
            <*> peekBufStruct (getPtrOf (hmsKeys f))
            <*> peekBufStruct (getPtrOf (hmsVals f))
        )
        $ \(cap, ctrl, kb, vb) ->
          let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
              numGroups = cap `rshift` 3
           in streamM (swarOccupiedSlotsS ctrlWords numGroups) $ \slot ->
                (,) <$> readBuf kb slot <*> readBuf vb slot

keysS :: (HashMapCtx m e k v) => HashMap e k v -> Stream m (Elem e k)
keysS hm = fst <$> toStream hm

valuesS :: (HashMapCtx m e k v) => HashMap e k v -> Stream m (Elem e v)
valuesS hm = snd <$> toStream hm

-- | Strip-mined in-place SIMD map over all occupied value slots in @[0, cap)@.
foreachValuesSPMD ::
  forall w k v m e.
  (HashMapCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  HashMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (HashMap hdr) step = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hmsCap f))
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hmsCtrl f))
    vb <- peekBufStruct (getPtrOf (hmsVals f))
    foreachMaskedBufSPMD
      pw
      True
      vb
      cap
      ( \idx -> do
          c <- peekElemOff (uniformAlignedPtr ctrl) idx
          pure ((c `iand` 0x80) `eq` 0)
      )
      step

-- | Masked-safe SIMD fold over all occupied value slots in @[0, cap)@.
foldValuesSPMD ::
  forall w k v s m e.
  (HashMapCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  HashMap e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (HashMap hdr) s0 step = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hmsCap f))
  accRef <- withSPMD pw (newRef s0)
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hmsCtrl f))
    vb <- peekBufStruct (getPtrOf (hmsVals f))
    res <-
      foldMaskedBufSPMD
        pw
        True
        vb
        cap
        ( \idx -> do
            c <- peekElemOff (uniformAlignedPtr ctrl) idx
            pure ((c `iand` 0x80) `eq` 0)
        )
        s0
        step
    withSPMD pw (writeRef accRef res)
  withSPMD pw (readRef accRef)

probeHashMapBatchSPMD ::
  forall w k v m e.
  (HashMapCtx m e k v, SPMDGang w m e, HashKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  HashMap e k v ->
  Elem (SPMD w e) k ->
  (SPMD w e Int64 -> SPMDM w m e ()) ->
  SPMDM w m e (SPMD w e Bool)
probeHashMapBatchSPMD (HashMap hdr) qKeys onHit = do
  let f = projectPtrs hdr
  cap <- liftSPMD (peek (getPtrOf (hmsCap f)))
  foundRef <- newRef false
  whenM_ (uniform cap `gt` 0) $ do
    ctrl <- liftSPMD (peek (getPtrOf (hmsCtrl f)))
    kb <- liftSPMD (peekBufStruct (getPtrOf (hmsKeys f)))
    seed <- liftSPMD (peek (getPtrOf (hmsSeed f)))
    let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (cap `rshift` 3) - 1
    h <- letM (hashElemWithSalt @(SPMD w e) @k (uniform seed) qKeys)
    let h2w64 = (h `rshift` 57) `iand` 0x7F
        splat = h2w64 * 0x0101010101010101
        g0 = (toIntegral (h `iand` 0x7FFFFFFFFFFFFFFF) :: SPMD w e Int64) `iand` uniform groupMask
    runDrain drain_ $
      unfoldWhileM (g0, 0 :: SPMD w e Int64, true) (\(_, _, act) -> pure act) $ \(g, st, _) -> do
        cw <- peekElemOff (uniformAlignedPtr ctrlWords) g
        let x = cw `ieor` splat
            low7 = x `iand` 0x7F7F7F7F7F7F7F7F
            hasZeroLow7 = inot (low7 + 0x7F7F7F7F7F7F7F7F)
            matchMask0 = hasZeroLow7 `iand` inot x `iand` 0x8080808080808080
            empMask = cw `iand` inot (cw `lshift` 6) `iand` 0x8080808080808080
        runDrain drain_ $
          unfoldWhileM matchMask0 (\curM -> pure (curM `ne` 0)) $ \curM -> do
            let lsb = (curM `iand` (0 - curM)) `rshift` 7
                byteIdx = toIntegral (((lsb * 0x0001020304050607) `rshift` 56) `iand` 0x7) :: SPMD w e Int64
                sl = (g `lshift` 3) + byteIdx
            ck <- gatherBufSPMD kb sl
            let hit = eqElem @(SPMD w e) @k ck qKeys
            whenM_ hit $ do
              writeRef foundRef true
              onHit sl
            pure (ifThenElse hit 0 (curM `iand` (curM - 1)), ())
        fnd <- readRef foundRef
        let actNext = lnot fnd `land` (empMask `eq` 0)
            stNext = st + 1
            gNext = (g + stNext) `iand` uniform groupMask
        pure ((gNext, stNext, actNext), ())
  readRef foundRef

-- | Lane-parallel SIMD batch membership query probing @w@ keys simultaneously.
containsKeyBatchSPMD ::
  forall w k v m e.
  (HashMapCtx m e k v, SPMDGang w m e, HashKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  Proxy w ->
  HashMap e k v ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
containsKeyBatchSPMD _ hm qKeys =
  probeHashMapBatchSPMD hm qKeys (\_ -> pure ())

-- | Lane-parallel SIMD batch scalar value lookup probing @w@ keys simultaneously.
lookupScalarBatchSPMD ::
  forall w k v s m e.
  ( HashMapCtx m e k v,
    SPMDGang w m e,
    HashKey (SPMD w e) k,
    LayoutGatherSPMD w m e k,
    LayoutGatherSPMD w m e v,
    Elem (SPMD w e) v ~ SPMD w e s,
    SPMDTyped w e s
  ) =>
  Proxy w ->
  SPMD w e s ->
  HashMap e k v ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool, SPMD w e s)
lookupScalarBatchSPMD _ defVal hm@(HashMap hdr) qKeys = do
  valRef <- newRef defVal
  vb <- liftSPMD (peekBufStruct (getPtrOf (hmsVals (projectPtrs hdr))))
  found <-
    probeHashMapBatchSPMD hm qKeys $ \sl -> do
      gv <- gatherBufSPMD vb sl
      writeRef valRef gv
  val <- readRef valRef
  pure (found, val)

drainToHashMap ::
  forall k v m e.
  (HashMapCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e v) (HashMap e k v)
drainToHashMap alloc = Drain $ \s -> do
  hm <- new alloc
  runDrain (drainAppendHashMap alloc hm) s
  pure hm

fromStream ::
  (HashMapCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e v) ->
  m (HashMap e k v)
fromStream alloc = runDrain (drainToHashMap alloc)

drainAppendHashMap ::
  (HashMapCtx m e k v) =>
  Alloc e ->
  HashMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendHashMap alloc hm = drainForEach (\(k, v) -> insert alloc hm k v)

drainAppendHashMapWith ::
  (HashMapCtx m e k v) =>
  Alloc e ->
  (Elem e v -> Elem e v -> Elem e v) ->
  HashMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendHashMapWith alloc combine hm =
  drainForEach (\(k, v) -> insertWith alloc hm combine k v)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type HashMapABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCHost (HashMap (JanusCT _) k v) = HashMap Identity k v

deriving via
  ViaPtrHandle t (CStruct (HashMapSpine k v)) (HashMap Identity k v)
  instance
    (HashMapABI k v) => JanusCArg t (HashMap (JanusCT t) k v)

deriving via
  ViaPtrHandle t (CStruct (HashMapSpine k v)) (HashMap Identity k v)
  instance
    (HashMapABI k v) => JanusCValRet t (HashMap (JanusCT t) k v)
