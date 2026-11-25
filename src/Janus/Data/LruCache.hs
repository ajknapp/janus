{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Janus.Data.LruCache
  ( -- * Bounded Pre-Allocated Exact-LRU Cache Spine & Handle
    LruCacheSpine (..),
    LruCache (..),
    LruCacheCtx,

    -- * Lifecycle
    new,
    free,

    -- * Queries & Zero-Allocation Reset
    clear,
    size,
    capacity,
    isEmpty,

    -- * Promoting Lookup, Non-Promoting Inspection, O(1) Evicting Insertion, & Deletion
    containsKey,
    peekKey,
    withPeekKey,
    lookup,
    withLookup,
    insert,
    delete,

    -- * Recency-Ordered Streams, SPMD Kernels, & Drains
    mruStream,
    lruStream,
    mruKeysS,
    mruValuesS,
    foreachValuesSPMD,
    foldValuesSPMD,
    drainToLruCache,
    fromStream,
    drainAppendLruCache,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocBytesAligned,
    allocStruct,
    allocTypedArray,
    freeBytes,
    freeStruct,
    freeTypedArray,
  )
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    janusSIMDAlignment,
  )
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFind, drainFirst, drainForEach, drainLength, runDrain)
import Janus.Control.Stream
  ( Stream,
    filterMapS,
    iterateS,
    rangeS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import Janus.Data.HKD
import Janus.Data.HashMap (HashKey (..), HashLayoutCtx, probeGroupsS, swarBitsS)
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (defaultHashSalt)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDM,
    SPMDTyped,
    uniform,
    uniformAlignedPtr,
    withSPMD,
  )
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- SwissTable Control Byte & SWAR Helpers
--------------------------------------------------------------------------------

ctrlEmptyByte :: Word8
ctrlEmptyByte = 0x80

ctrlDeletedByte :: Word8
ctrlDeletedByte = 0xFE

swarMatchH2 ::
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

swarMatchEmpty :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmpty w = w `iand` inot (w `lshift` 6) `iand` 0x8080808080808080

swarMatchEmptyOrDeleted :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmptyOrDeleted w = w `iand` 0x8080808080808080

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
maxLoadForCap sCap = sCap - (sCap `rshift` 3)

setCtrlSlot ::
  (CmdAlloc m e) =>
  e (Ptr Word8) ->
  e Int64 ->
  e Int64 ->
  e Word8 ->
  m ()
setCtrlSlot ctrl sCap slot b = do
  pokeElemOff ctrl b slot
  whenM_ (slot `lt` 8) $
    pokeElemOff ctrl b (sCap + slot)

deleteSparseSlot ::
  forall m e.
  (CmdAlloc m e) =>
  e (Ptr Word8) ->
  e Int64 ->
  e (Ptr Int64) ->
  e Int64 ->
  m ()
deleteSparseSlot ctrl sCap growthLeftPtr slot = do
  let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      grp = slot `rshift` 3
  w <- peekElemOff ctrlWords grp
  ifThenElseM_
    (swarMatchEmpty w `ne` 0)
    ( do
        setCtrlSlot ctrl sCap slot (fromIntegral ctrlEmptyByte)
        gl <- peek growthLeftPtr
        poke growthLeftPtr (gl + 1)
    )
    (setCtrlSlot ctrl sCap slot (fromIntegral ctrlDeletedByte))

--------------------------------------------------------------------------------
-- LruCache Spine & Handle
--------------------------------------------------------------------------------

-- | Bounded exact-LRU cache spine backed by compact 32-bit array-index doubly-linked
-- recency links (@lcsMru -> lcsNext -> ... -> lcsLru@, @-1@ sentinel) and a pre-sized
-- SwissTable sparse index with zero-allocation tombstone compaction.
data LruCacheSpine k v f = LruCacheSpine
  { lcsCtrl :: !(f (Ptr Word8)),
    lcsSparse :: !(f (Ptr Int64)),
    lcsSparseCap :: !(f Int64),
    lcsGrowthLeft :: !(f Int64),
    lcsSeed :: !(f Word64),
    lcsKeys :: !(f (CStruct (Buf k))),
    lcsVals :: !(f (CStruct (Buf v))),
    lcsPrev :: !(f (Ptr Int32)),
    lcsNext :: !(f (Ptr Int32)),
    lcsMru :: !(f Int32),
    lcsLru :: !(f Int32),
    lcsFreeHead :: !(f Int32),
    lcsLen :: !(f Int64),
    lcsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance
  (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) =>
  JanusStruct (LruCacheSpine k v)

newtype LruCache (e :: K.Type -> K.Type) k v = LruCache
  { unLruCache :: e (Ptr (CStruct (LruCacheSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (LruCache Identity k v)

deriving stock instance Show (LruCache Identity k v)

type LruCacheCtx m e k v = (HashLayoutCtx m e k, LayoutCtx m e v)

--------------------------------------------------------------------------------
-- Recency List & Free-List Helpers
--------------------------------------------------------------------------------

initFreeSlots ::
  forall m e.
  (CmdAlloc m e) =>
  e (Ptr Int32) ->
  e (Ptr Int32) ->
  e Int64 ->
  m ()
initFreeSlots prevArr nextArr dCap =
  runDrain
    ( drainForEach $ \i -> do
        let nxt = ifThenElse ((i + 1) `lt` dCap) (toIntegral (i + 1)) (-1 :: e Int32)
        pokeElemOff prevArr (-1 :: e Int32) i
        pokeElemOff nextArr nxt i
    )
    (rangeS 0 dCap)

unlinkRecencyNode ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  e (Ptr (CStruct (LruCacheSpine k v))) ->
  e Int32 ->
  m ()
unlinkRecencyNode hdr node32 = do
  let f = projectPtrs hdr
      node64 = toIntegral node32 :: e Int64
  prevArr <- peek (getPtrOf (lcsPrev f))
  nextArr <- peek (getPtrOf (lcsNext f))
  prv <- peekElemOff prevArr node64
  nxt <- peekElemOff nextArr node64
  ifThenElseM_
    (prv `ge` 0)
    (pokeElemOff nextArr nxt (toIntegral prv))
    (poke (getPtrOf (lcsMru f)) nxt)
  ifThenElseM_
    (nxt `ge` 0)
    (pokeElemOff prevArr prv (toIntegral nxt))
    (poke (getPtrOf (lcsLru f)) prv)
  pokeElemOff prevArr (-1 :: e Int32) node64
  pokeElemOff nextArr (-1 :: e Int32) node64

pushFrontMru ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  e (Ptr (CStruct (LruCacheSpine k v))) ->
  e Int32 ->
  m ()
pushFrontMru hdr node32 = do
  let f = projectPtrs hdr
      node64 = toIntegral node32 :: e Int64
  prevArr <- peek (getPtrOf (lcsPrev f))
  nextArr <- peek (getPtrOf (lcsNext f))
  oldMru <- peek (getPtrOf (lcsMru f))
  pokeElemOff prevArr (-1 :: e Int32) node64
  pokeElemOff nextArr oldMru node64
  ifThenElseM_
    (oldMru `ge` 0)
    (pokeElemOff prevArr node32 (toIntegral oldMru))
    (poke (getPtrOf (lcsLru f)) node32)
  poke (getPtrOf (lcsMru f)) node32

promoteToMru ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  e (Ptr (CStruct (LruCacheSpine k v))) ->
  e Int32 ->
  m ()
promoteToMru hdr node32 = do
  let f = projectPtrs hdr
  curMru <- peek (getPtrOf (lcsMru f))
  whenM_ (curMru `ne` node32) $ do
    unlinkRecencyNode @k @v hdr node32
    pushFrontMru @k @v hdr node32

--------------------------------------------------------------------------------
-- Lifecycle & Queries
--------------------------------------------------------------------------------

-- | Pre-allocate a bounded exact-LRU cache with capacity @max 1 reqCap@.
new ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (LruCache e k v)
new alloc reqCap = do
  effCap <- letM (ifThenElse (reqCap `lt` 1) 1 reqCap)
  (_, sCap) <-
    runDrain
      (drainFind 16 (\cand -> cand `ge` (effCap * 2)))
      (iterateS (16 :: e Int64) (`lshift` 1))

  hdr <- allocStruct @(CStruct (LruCacheSpine k v)) alloc
  ctrlVP <- allocBytesAligned alloc (sCap + 8) (fromIntegral janusSIMDAlignment)
  cmdMemset ctrlVP (fromIntegral ctrlEmptyByte) (sCap + 8)
  sparse <- allocTypedArray @Int64 alloc sCap
  kb <- allocBuf @k alloc effCap
  vb <- allocBuf @v alloc effCap
  prevArr <- allocTypedArray @Int32 alloc effCap
  nextArr <- allocTypedArray @Int32 alloc effCap
  initFreeSlots prevArr nextArr effCap

  let f = projectPtrs hdr
  poke (getPtrOf (lcsCtrl f)) (fromVoidPtr ctrlVP)
  poke (getPtrOf (lcsSparse f)) sparse
  poke (getPtrOf (lcsSparseCap f)) sCap
  poke (getPtrOf (lcsGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (lcsSeed f)) defaultHashSalt
  pokeBufStruct (getPtrOf (lcsKeys f)) kb
  pokeBufStruct (getPtrOf (lcsVals f)) vb
  poke (getPtrOf (lcsPrev f)) prevArr
  poke (getPtrOf (lcsNext f)) nextArr
  poke (getPtrOf (lcsMru f)) (-1)
  poke (getPtrOf (lcsLru f)) (-1)
  poke (getPtrOf (lcsFreeHead f)) 0
  poke (getPtrOf (lcsLen f)) 0
  poke (getPtrOf (lcsCap f)) effCap
  pure (LruCache hdr)

free ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  Alloc e ->
  LruCache e k v ->
  m ()
free alloc (LruCache hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (lcsSparseCap f))
  dCap <- peek (getPtrOf (lcsCap f))
  ctrl <- peek (getPtrOf (lcsCtrl f))
  sparse <- peek (getPtrOf (lcsSparse f))
  kb <- peekBufStruct (getPtrOf (lcsKeys f))
  vb <- peekBufStruct (getPtrOf (lcsVals f))
  prevArr <- peek (getPtrOf (lcsPrev f))
  nextArr <- peek (getPtrOf (lcsNext f))
  freeBytes alloc (toVoidPtr ctrl) (sCap + 8)
  freeTypedArray @Int64 alloc sparse sCap
  freeBuf alloc kb dCap
  freeBuf alloc vb dCap
  freeTypedArray @Int32 alloc prevArr dCap
  freeTypedArray @Int32 alloc nextArr dCap
  freeStruct alloc hdr

clear ::
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  m ()
clear (LruCache hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (lcsSparseCap f))
  dCap <- peek (getPtrOf (lcsCap f))
  ctrl <- peek (getPtrOf (lcsCtrl f))
  prevArr <- peek (getPtrOf (lcsPrev f))
  nextArr <- peek (getPtrOf (lcsNext f))
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  initFreeSlots prevArr nextArr dCap
  poke (getPtrOf (lcsGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (lcsMru f)) (-1)
  poke (getPtrOf (lcsLru f)) (-1)
  poke (getPtrOf (lcsFreeHead f)) 0
  poke (getPtrOf (lcsLen f)) 0

size :: (LruCacheCtx m e k v) => LruCache e k v -> m (e Int64)
size (LruCache hdr) = peek (getPtrOf (lcsLen (projectPtrs hdr)))

capacity :: (LruCacheCtx m e k v) => LruCache e k v -> m (e Int64)
capacity (LruCache hdr) = peek (getPtrOf (lcsCap (projectPtrs hdr)))

isEmpty :: (LruCacheCtx m e k v) => LruCache e k v -> m (e Bool)
isEmpty lru = (`eq` 0) <$> size lru

--------------------------------------------------------------------------------
-- Zero-Allocation In-Place Tombstone Compaction & Probing
--------------------------------------------------------------------------------

-- | Sweep accumulated SwissTable @DELETED@ tombstones in-place with ZERO heap allocations
-- by walking the live recency list @lcsMru -> ... -> lcsLru@ and re-indexing into @lcsCtrl@.
compactLruSparse ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  e (Ptr (CStruct (LruCacheSpine k v))) ->
  m ()
compactLruSparse hdr = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (lcsSparseCap f))
  ctrl <- peek (getPtrOf (lcsCtrl f))
  sparse <- peek (getPtrOf (lcsSparse f))
  nextArr <- peek (getPtrOf (lcsNext f))
  mru0 <- peek (getPtrOf (lcsMru f))
  kb <- peekBufStruct (getPtrOf (lcsKeys f))
  seed <- peek (getPtrOf (lcsSeed f))
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
  liveCnt <-
    runDrain drainLength $
      streamM
        ( unfoldWhileM mru0 (\cur -> pure (cur `ge` 0)) $ \cur -> do
            let idx = toIntegral cur :: e Int64
            nxt <- peekElemOff nextArr idx
            pure (nxt, idx)
        )
        $ \dIdx -> do
          key <- readBuf kb dIdx
          h <- letM (hashElemWithSalt @e @k seed key)
          let (_, h2) = splitHash h
              g0 = toIntegral h `iand` groupMask
          (_, dstSlot) <-
            runDrain (drainFirst 0) $
              filterMapS
                ( \pg -> do
                    nw <- peekElemOff ctrlWords pg
                    let emp = swarMatchEmpty nw
                    pure (emp `ne` 0, (pg `lshift` 3) + swarLowestByteIdx emp)
                )
                (probeGroupsS g0 groupMask)
          setCtrlSlot ctrl sCap dstSlot h2
          pokeElemOff sparse dIdx dstSlot
  poke (getPtrOf (lcsGrowthLeft f)) (maxLoadForCap sCap - liveCnt)

findSlotAndDense ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  m (e Bool, e Int64, e Int64)
findSlotAndDense (LruCache hdr) key = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (lcsSparseCap f))
  ctrl <- peek (getPtrOf (lcsCtrl f))
  sparse <- peek (getPtrOf (lcsSparse f))
  kb <- peekBufStruct (getPtrOf (lcsKeys f))
  seed <- peek (getPtrOf (lcsSeed f))
  h <- letM (hashElemWithSalt @e @k seed key)
  let (_, h2) = splitHash h
      ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
      g0 = toIntegral h `iand` groupMask
  (_, (_, (hitFound, hitSlot, hitDense))) <-
    runDrain
      (drainFind (true, (false, 0, 0)) fst)
      $ streamM (probeGroupsS g0 groupMask)
      $ \g -> do
        w <- peekElemOff ctrlWords g
        (hitInGrp, (_, candSlot, candDense)) <-
          runDrain (drainFind (false, 0, 0) (\(ok, _, _) -> ok)) $
            streamM (swarBitsS (swarMatchH2 w h2)) $ \lane -> do
              let sl = (g `lshift` 3) + lane
              dIdx <- peekElemOff sparse sl
              candKey <- readBuf kb dIdx
              pure (eqElem @e @k candKey key, sl, dIdx)
        let done = hitInGrp `lor` (swarMatchEmpty w `ne` 0)
        pure (done, (hitInGrp, candSlot, candDense))
  pure (hitFound, hitSlot, hitDense)

findSparseSlotForDense ::
  forall k m e.
  (CmdAlloc m e, HashKey e k) =>
  e (Ptr Word8) ->
  e Int64 ->
  e (Ptr Int64) ->
  e Word64 ->
  Elem e k ->
  e Int64 ->
  m (e Int64)
findSparseSlotForDense ctrl sCap sparse seed key targetDenseIdx = do
  h <- letM (hashElemWithSalt @e @k seed key)
  let (_, h2) = splitHash h
      ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
      g0 = toIntegral h `iand` groupMask
  (_, (_, foundSlot)) <-
    runDrain
      (drainFind (false, 0) fst)
      $ streamM (probeGroupsS g0 groupMask)
      $ \g -> do
        w <- peekElemOff ctrlWords g
        (hitInGrp, (_, slOut)) <-
          runDrain (drainFind (false, 0) fst) $
            streamM (swarBitsS (swarMatchH2 w h2)) $ \lane -> do
              let sl = (g `lshift` 3) + lane
              dIdx <- peekElemOff sparse sl
              pure (dIdx `eq` targetDenseIdx, sl)
        pure (hitInGrp, slOut)
  pure foundSlot

--------------------------------------------------------------------------------
-- Lookup, Non-Promoting Inspection, O(1) Evicting Insertion, & Deletion
--------------------------------------------------------------------------------

-- | Check key membership WITHOUT promoting the entry in the recency list.
containsKey ::
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  m (e Bool)
containsKey lru key = (\(ok, _, _) -> ok) <$> findSlotAndDense lru key

-- | Read key's value if present WITHOUT promoting the entry in the recency list.
peekKey ::
  forall k v s m e.
  (LruCacheCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  LruCache e k v ->
  Elem e k ->
  m (e Bool, e s)
peekKey defVal lru@(LruCache hdr) key = do
  (found, _, dIdx) <- findSlotAndDense lru key
  valRef <- newRef defVal
  whenM_ found $ do
    vb <- peekBufStruct (getPtrOf (lcsVals (projectPtrs hdr)))
    readBuf vb dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withPeekKey ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withPeekKey lru@(LruCache hdr) key onMiss onHit = do
  (found, _, dIdx) <- findSlotAndDense lru key
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (lcsVals (projectPtrs hdr)))
        readBuf vb dIdx >>= onHit
    )
    onMiss

-- | Lookup @key@ in O(1), promoting it to @lcsMru@ on hit.
lookup ::
  forall k v s m e.
  (LruCacheCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  LruCache e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal lru@(LruCache hdr) key = do
  (found, _, dIdx) <- findSlotAndDense lru key
  valRef <- newRef defVal
  whenM_ found $ do
    let f = projectPtrs hdr
    vb <- peekBufStruct (getPtrOf (lcsVals f))
    promoteToMru @k @v hdr (toIntegral dIdx)
    readBuf vb dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withLookup ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup lru@(LruCache hdr) key onMiss onHit = do
  (found, _, dIdx) <- findSlotAndDense lru key
  ifThenElseM_
    found
    ( do
        let f = projectPtrs hdr
        vb <- peekBufStruct (getPtrOf (lcsVals f))
        promoteToMru @k @v hdr (toIntegral dIdx)
        readBuf vb dIdx >>= onHit
    )
    onMiss

-- | Zero-allocation O(1) bounded LRU insertion:
-- * On hit: updates value in-place and promotes slot to @lcsMru@.
-- * On miss: pops @lcsFreeHead@ if @size < capacity@, else evicts @lcsLru@ in O(1),
--   writes @(key, val)@, and links slot at @lcsMru@.
insert ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert lru@(LruCache hdr) key val = do
  (exists, _, existingDense) <- findSlotAndDense lru key
  let f = projectPtrs hdr
  ifThenElseM_
    exists
    ( do
        vb <- peekBufStruct (getPtrOf (lcsVals f))
        writeBuf vb existingDense val
        promoteToMru @k @v hdr (toIntegral existingDense)
    )
    ( do
        curLen <- peek (getPtrOf (lcsLen f))
        dCap <- peek (getPtrOf (lcsCap f))
        kb <- peekBufStruct (getPtrOf (lcsKeys f))
        vb <- peekBufStruct (getPtrOf (lcsVals f))
        nextArr <- peek (getPtrOf (lcsNext f))
        ctrl <- peek (getPtrOf (lcsCtrl f))
        sCap <- peek (getPtrOf (lcsSparseCap f))
        sparse <- peek (getPtrOf (lcsSparse f))
        seed <- peek (getPtrOf (lcsSeed f))

        slot32Ref <- newRef (0 :: e Int32)
        ifThenElseM_
          (curLen `lt` dCap)
          ( do
              freeHd <- peek (getPtrOf (lcsFreeHead f))
              nxtFree <- peekElemOff nextArr (toIntegral freeHd)
              poke (getPtrOf (lcsFreeHead f)) nxtFree
              poke (getPtrOf (lcsLen f)) (curLen + 1)
              writeRef slot32Ref freeHd
          )
          ( do
              victim32 <- peek (getPtrOf (lcsLru f))
              let victimIdx = toIntegral victim32 :: e Int64
              victimKey <- readBuf kb victimIdx
              victimSlot <- findSparseSlotForDense @k ctrl sCap sparse seed victimKey victimIdx
              deleteSparseSlot ctrl sCap (getPtrOf (lcsGrowthLeft f)) victimSlot
              unlinkRecencyNode @k @v hdr victim32
              writeRef slot32Ref victim32
          )

        gl <- peek (getPtrOf (lcsGrowthLeft f))
        whenM_ (gl `le` 0) $
          compactLruSparse @k @v hdr

        node32 <- readRef slot32Ref
        let dIdx = toIntegral node32 :: e Int64
        writeBuf kb dIdx key
        writeBuf vb dIdx val
        pushFrontMru @k @v hdr node32

        h <- letM (hashElemWithSalt @e @k seed key)
        let (_, h2) = splitHash h
            ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
            groupMask = (sCap `rshift` 3) - 1
            g0 = toIntegral h `iand` groupMask
        (_, dstSlot) <-
          runDrain (drainFirst 0) $
            filterMapS
              ( \g -> do
                  w <- peekElemOff ctrlWords g
                  let avail = swarMatchEmptyOrDeleted w
                  pure (avail `ne` 0, (g `lshift` 3) + swarLowestByteIdx avail)
              )
              (probeGroupsS g0 groupMask)
        prevCtrl <- peekElemOff ctrl dstSlot
        setCtrlSlot ctrl sCap dstSlot h2
        pokeElemOff sparse dIdx dstSlot
        whenM_ (prevCtrl `eq` fromIntegral ctrlEmptyByte) $ do
          glNow <- peek (getPtrOf (lcsGrowthLeft f))
          poke (getPtrOf (lcsGrowthLeft f)) (glNow - 1)
    )

-- | O(1) removal: deletes @key@ from SwissTable, unlinks its slot from the recency
-- doubly-linked list, and pushes the freed slot onto @lcsFreeHead@.
delete ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Elem e k ->
  m (e Bool)
delete lru@(LruCache hdr) key = do
  (found, targetSlot, dIdx) <- findSlotAndDense lru key
  whenM_ found $ do
    let f = projectPtrs hdr
        node32 = toIntegral dIdx :: e Int32
    ctrl <- peek (getPtrOf (lcsCtrl f))
    sCap <- peek (getPtrOf (lcsSparseCap f))
    nextArr <- peek (getPtrOf (lcsNext f))
    curLen <- peek (getPtrOf (lcsLen f))
    deleteSparseSlot ctrl sCap (getPtrOf (lcsGrowthLeft f)) targetSlot
    unlinkRecencyNode @k @v hdr node32
    oldFree <- peek (getPtrOf (lcsFreeHead f))
    pokeElemOff nextArr oldFree dIdx
    poke (getPtrOf (lcsFreeHead f)) node32
    poke (getPtrOf (lcsLen f)) (curLen - 1)
  pure found

--------------------------------------------------------------------------------
-- Recency-Ordered Streams & Drains
--------------------------------------------------------------------------------

-- | Stream live @(key, val)@ entries from most-recently used (@lcsMru@) to least-recently used (@lcsLru@).
mruStream ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Stream m (Elem e k, Elem e v)
mruStream (LruCache hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (lcsMru f))
            <*> peek (getPtrOf (lcsNext f))
            <*> peekBufStruct (getPtrOf (lcsKeys f))
            <*> peekBufStruct (getPtrOf (lcsVals f))
        )
        $ \(mru0, nextArr, kb, vb) ->
          unfoldWhileM mru0 (\cur -> pure (cur `ge` 0)) $ \cur -> do
            let idx = toIntegral cur :: e Int64
            nxt <- peekElemOff nextArr idx
            kVal <- readBuf kb idx
            vVal <- readBuf vb idx
            pure (nxt, (kVal, vVal))

-- | Stream live @(key, val)@ entries from least-recently used (@lcsLru@) to most-recently used (@lcsMru@).
lruStream ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Stream m (Elem e k, Elem e v)
lruStream (LruCache hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (lcsLru f))
            <*> peek (getPtrOf (lcsPrev f))
            <*> peekBufStruct (getPtrOf (lcsKeys f))
            <*> peekBufStruct (getPtrOf (lcsVals f))
        )
        $ \(lru0, prevArr, kb, vb) ->
          unfoldWhileM lru0 (\cur -> pure (cur `ge` 0)) $ \cur -> do
            let idx = toIntegral cur :: e Int64
            prv <- peekElemOff prevArr idx
            kVal <- readBuf kb idx
            vVal <- readBuf vb idx
            pure (prv, (kVal, vVal))

mruKeysS ::
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Stream m (Elem e k)
mruKeysS lru = fst <$> mruStream lru

mruValuesS ::
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Stream m (Elem e v)
mruValuesS lru = snd <$> mruStream lru

-- | Strip-mined in-place SIMD map over all occupied value slots without modifying LRU recency links.
foreachValuesSPMD ::
  forall w k v m e.
  (LruCacheCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  LruCache e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (LruCache hdr) step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (lcsLen f))
  whenM_ (curLen `gt` 0) $ do
    dCap <- peek (getPtrOf (lcsCap f))
    prevArr <- peek (getPtrOf (lcsPrev f))
    mru32 <- peek (getPtrOf (lcsMru f))
    vb <- peekBufStruct (getPtrOf (lcsVals f))
    let mru64 = toIntegral mru32 :: e Int64
    foreachMaskedBufSPMD
      pw
      True
      vb
      dCap
      ( \idx -> do
          prv <- peekElemOff (uniformAlignedPtr prevArr) idx
          pure ((prv `ge` 0) `lor` (idx `eq` uniform mru64))
      )
      step

-- | Masked-safe SIMD fold over all occupied value slots without modifying LRU recency links.
foldValuesSPMD ::
  forall w k v s m e.
  (LruCacheCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  LruCache e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (LruCache hdr) s0 step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (lcsLen f))
  accRef <- withSPMD pw (newRef s0)
  whenM_ (curLen `gt` 0) $ do
    dCap <- peek (getPtrOf (lcsCap f))
    prevArr <- peek (getPtrOf (lcsPrev f))
    mru32 <- peek (getPtrOf (lcsMru f))
    vb <- peekBufStruct (getPtrOf (lcsVals f))
    let mru64 = toIntegral mru32 :: e Int64
    res <-
      foldMaskedBufSPMD
        pw
        True
        vb
        dCap
        ( \idx -> do
            prv <- peekElemOff (uniformAlignedPtr prevArr) idx
            pure ((prv `ge` 0) `lor` (idx `eq` uniform mru64))
        )
        s0
        step
    withSPMD pw (writeRef accRef res)
  withSPMD pw (readRef accRef)

drainToLruCache ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  Drain m (Elem e k, Elem e v) (LruCache e k v)
drainToLruCache alloc reqCap = Drain $ \sIn -> do
  lru <- new alloc reqCap
  runDrain (drainAppendLruCache lru) sIn
  pure lru

fromStream ::
  forall k v m e.
  (LruCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  Stream m (Elem e k, Elem e v) ->
  m (LruCache e k v)
fromStream alloc reqCap = runDrain (drainToLruCache @k @v alloc reqCap)

drainAppendLruCache ::
  (LruCacheCtx m e k v) =>
  LruCache e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendLruCache lru = drainForEach (\(kIn, vIn) -> insert lru kIn vIn)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type LruCacheABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCHost (LruCache (JanusCT _) k v) = LruCache Identity k v

deriving via
  ViaPtrHandle t (CStruct (LruCacheSpine k v)) (LruCache Identity k v)
  instance
    (LruCacheABI k v) => JanusCArg t (LruCache (JanusCT t) k v)

deriving via
  ViaPtrHandle t (CStruct (LruCacheSpine k v)) (LruCache Identity k v)
  instance
    (LruCacheABI k v) => JanusCValRet t (LruCache (JanusCT t) k v)
