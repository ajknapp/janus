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

module Janus.Data.ClockCache
  ( -- * Bounded Pre-Allocated Second-Chance Clock Cache Spine & Handle
    ClockCacheSpine (..),
    ClockCache (..),
    ClockCacheCtx,

    -- * Lifecycle
    new,
    free,

    -- * Queries & Zero-Allocation Reset
    clear,
    size,
    capacity,
    isEmpty,

    -- * Lookup, Non-Touching Inspection, Evicting Insertion, & Deletion
    containsKey,
    peekKey,
    withPeekKey,
    lookup,
    withLookup,
    insert,
    delete,

    -- * Streams, SPMD Kernels, & Drains
    toStream,
    keysS,
    valuesS,
    foreachValuesSPMD,
    foldValuesSPMD,
    drainToClockCache,
    fromStream,
    drainAppendClockCache,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
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
    allocZeroedAligned,
    freeBytes,
    freeStruct,
    freeTypedArray,
  )
import Janus.Backend.C
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
    uniformAlignedPtr,
    withSPMD,
  )
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- SwissTable Control Byte, SWAR, & Reference-Bitmask Helpers
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

-- | Test-before-set reference bit: writes @1@ to @ccsRefBits@ ONLY when currently @0@
-- so repeated read hits avoid dirtying the bitmask cache line.
setRefBitIfZero ::
  forall m e.
  (CmdAlloc m e) =>
  e (Ptr Word64) ->
  e Int64 ->
  m ()
setRefBitIfZero refBits dIdx = do
  let wIdx = dIdx `rshift` 6
      bitMask = (1 :: e Word64) `lshift` toIntegral (dIdx `iand` 63)
  w <- peekElemOff refBits wIdx
  whenM_ ((w `iand` bitMask) `eq` 0) $
    pokeElemOff refBits (w `ior` bitMask) wIdx

clearRefBit ::
  forall m e.
  (CmdAlloc m e) =>
  e (Ptr Word64) ->
  e Int64 ->
  m ()
clearRefBit refBits dIdx = do
  let wIdx = dIdx `rshift` 6
      bitMask = (1 :: e Word64) `lshift` toIntegral (dIdx `iand` 63)
  w <- peekElemOff refBits wIdx
  whenM_ ((w `iand` bitMask) `ne` 0) $
    pokeElemOff refBits (w `iand` inot bitMask) wIdx

--------------------------------------------------------------------------------
-- ClockCache Spine & Handle
--------------------------------------------------------------------------------

-- | Bounded fixed-capacity Second-Chance Clock Cache spine.
-- Pre-allocates all dense buffers, occupancy/ref-bit masks, and SwissTable sparse
-- index at construction; steady-state churn performs **zero** heap allocations.
data ClockCacheSpine k v f = ClockCacheSpine
  { ccsCtrl :: !(f (Ptr Word8)),
    ccsSparse :: !(f (Ptr Int64)),
    ccsSparseCap :: !(f Int64),
    ccsGrowthLeft :: !(f Int64),
    ccsSeed :: !(f Word64),
    ccsKeys :: !(f (CStruct (Buf k))),
    ccsVals :: !(f (CStruct (Buf v))),
    ccsOccupied :: !(f (Ptr Word8)),
    ccsRefBits :: !(f (Ptr Word64)),
    ccsHand :: !(f Int64),
    ccsLen :: !(f Int64),
    ccsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance
  (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) =>
  JanusStruct (ClockCacheSpine k v)

newtype ClockCache (e :: K.Type -> K.Type) k v = ClockCache
  { unClockCache :: e (Ptr (CStruct (ClockCacheSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (ClockCache Identity k v)

deriving stock instance Show (ClockCache Identity k v)

type ClockCacheCtx m e k v = (HashLayoutCtx m e k, LayoutCtx m e v)

--------------------------------------------------------------------------------
-- Lifecycle & Queries
--------------------------------------------------------------------------------

-- | Pre-allocate a bounded 'ClockCache' with capacity @max 1 reqCap@.
new ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (ClockCache e k v)
new alloc reqCap = do
  effCap <- letM (ifThenElse (reqCap `lt` 1) 1 reqCap)
  (_, sCap) <-
    runDrain
      (drainFind 16 (\cand -> cand `ge` (effCap * 2)))
      (iterateS (16 :: e Int64) (`lshift` 1))

  hdr <- allocStruct @(CStruct (ClockCacheSpine k v)) alloc
  ctrlVP <- allocBytesAligned alloc (sCap + 8) (fromIntegral janusSIMDAlignment)
  cmdMemset ctrlVP (fromIntegral ctrlEmptyByte) (sCap + 8)
  sparse <- allocTypedArray @Int64 alloc sCap
  kb <- allocBuf @k alloc effCap
  vb <- allocBuf @v alloc effCap
  occVP <- allocZeroedAligned alloc effCap (fromIntegral janusSIMDAlignment)
  let numRefWords = (effCap + 63) `rshift` 6
  refVP <- allocZeroedAligned alloc (numRefWords * 8) (fromIntegral janusSIMDAlignment)

  let f = projectPtrs hdr
  poke (getPtrOf (ccsCtrl f)) (fromVoidPtr ctrlVP)
  poke (getPtrOf (ccsSparse f)) sparse
  poke (getPtrOf (ccsSparseCap f)) sCap
  poke (getPtrOf (ccsGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (ccsSeed f)) defaultHashSalt
  pokeBufStruct (getPtrOf (ccsKeys f)) kb
  pokeBufStruct (getPtrOf (ccsVals f)) vb
  poke (getPtrOf (ccsOccupied f)) (fromVoidPtr occVP)
  poke (getPtrOf (ccsRefBits f)) (fromVoidPtr refVP)
  poke (getPtrOf (ccsHand f)) 0
  poke (getPtrOf (ccsLen f)) 0
  poke (getPtrOf (ccsCap f)) effCap
  pure (ClockCache hdr)

free ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  Alloc e ->
  ClockCache e k v ->
  m ()
free alloc (ClockCache hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (ccsSparseCap f))
  dCap <- peek (getPtrOf (ccsCap f))
  ctrl <- peek (getPtrOf (ccsCtrl f))
  sparse <- peek (getPtrOf (ccsSparse f))
  kb <- peekBufStruct (getPtrOf (ccsKeys f))
  vb <- peekBufStruct (getPtrOf (ccsVals f))
  occ <- peek (getPtrOf (ccsOccupied f))
  refBits <- peek (getPtrOf (ccsRefBits f))
  let numRefWords = (dCap + 63) `rshift` 6
  freeBytes alloc (toVoidPtr ctrl) (sCap + 8)
  freeTypedArray @Int64 alloc sparse sCap
  freeBuf alloc kb dCap
  freeBuf alloc vb dCap
  freeBytes alloc (toVoidPtr occ) dCap
  freeBytes alloc (toVoidPtr refBits) (numRefWords * 8)
  freeStruct alloc hdr

clear ::
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  m ()
clear (ClockCache hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (ccsSparseCap f))
  dCap <- peek (getPtrOf (ccsCap f))
  ctrl <- peek (getPtrOf (ccsCtrl f))
  occ <- peek (getPtrOf (ccsOccupied f))
  refBits <- peek (getPtrOf (ccsRefBits f))
  let numRefWords = (dCap + 63) `rshift` 6
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  cmdMemset (toVoidPtr occ) 0 dCap
  cmdMemset (toVoidPtr refBits) 0 (numRefWords * 8)
  poke (getPtrOf (ccsGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (ccsHand f)) 0
  poke (getPtrOf (ccsLen f)) 0

size :: (ClockCacheCtx m e k v) => ClockCache e k v -> m (e Int64)
size (ClockCache hdr) = peek (getPtrOf (ccsLen (projectPtrs hdr)))

capacity :: (ClockCacheCtx m e k v) => ClockCache e k v -> m (e Int64)
capacity (ClockCache hdr) = peek (getPtrOf (ccsCap (projectPtrs hdr)))

isEmpty :: (ClockCacheCtx m e k v) => ClockCache e k v -> m (e Bool)
isEmpty cc = (`eq` 0) <$> size cc

--------------------------------------------------------------------------------
-- Zero-Allocation In-Place Tombstone Compaction & Probing
--------------------------------------------------------------------------------

-- | Sweep accumulated SwissTable @DELETED@ tombstones in-place with ZERO heap allocations
-- by resetting @ccsCtrl@ to @EMPTY@ and re-indexing all currently occupied dense slots.
compactClockSparse ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  e (Ptr (CStruct (ClockCacheSpine k v))) ->
  m ()
compactClockSparse hdr = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (ccsSparseCap f))
  dCap <- peek (getPtrOf (ccsCap f))
  ctrl <- peek (getPtrOf (ccsCtrl f))
  sparse <- peek (getPtrOf (ccsSparse f))
  occ <- peek (getPtrOf (ccsOccupied f))
  kb <- peekBufStruct (getPtrOf (ccsKeys f))
  seed <- peek (getPtrOf (ccsSeed f))
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
  liveCnt <-
    runDrain drainLength $
      streamM
        ( filterMapS
            (\i -> (\o -> (o `ne` 0, i)) <$> peekElemOff occ i)
            (rangeS 0 dCap)
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
  poke (getPtrOf (ccsGrowthLeft f)) (maxLoadForCap sCap - liveCnt)

findSlotAndDense ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  m (e Bool, e Int64, e Int64)
findSlotAndDense (ClockCache hdr) key = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (ccsSparseCap f))
  ctrl <- peek (getPtrOf (ccsCtrl f))
  sparse <- peek (getPtrOf (ccsSparse f))
  kb <- peekBufStruct (getPtrOf (ccsKeys f))
  seed <- peek (getPtrOf (ccsSeed f))
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
-- Lookup, Non-Touching Inspection, Evicting Insertion, & Deletion
--------------------------------------------------------------------------------

-- | Check key membership WITHOUT modifying its second-chance reference bit.
containsKey ::
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  m (e Bool)
containsKey cc key = (\(ok, _, _) -> ok) <$> findSlotAndDense cc key

-- | Read key's value if present WITHOUT modifying its second-chance reference bit.
peekKey ::
  forall k v s m e.
  (ClockCacheCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  ClockCache e k v ->
  Elem e k ->
  m (e Bool, e s)
peekKey defVal cc@(ClockCache hdr) key = do
  (found, _, dIdx) <- findSlotAndDense cc key
  valRef <- newRef defVal
  whenM_ found $ do
    vb <- peekBufStruct (getPtrOf (ccsVals (projectPtrs hdr)))
    readBuf vb dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withPeekKey ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withPeekKey cc@(ClockCache hdr) key onMiss onHit = do
  (found, _, dIdx) <- findSlotAndDense cc key
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (ccsVals (projectPtrs hdr)))
        readBuf vb dIdx >>= onHit
    )
    onMiss

-- | Lookup @key@, setting its second-chance reference bit (only if currently @0@) on hit.
lookup ::
  forall k v s m e.
  (ClockCacheCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  ClockCache e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal cc@(ClockCache hdr) key = do
  (found, _, dIdx) <- findSlotAndDense cc key
  valRef <- newRef defVal
  whenM_ found $ do
    let f = projectPtrs hdr
    vb <- peekBufStruct (getPtrOf (ccsVals f))
    refBits <- peek (getPtrOf (ccsRefBits f))
    setRefBitIfZero refBits dIdx
    readBuf vb dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withLookup ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup cc@(ClockCache hdr) key onMiss onHit = do
  (found, _, dIdx) <- findSlotAndDense cc key
  ifThenElseM_
    found
    ( do
        let f = projectPtrs hdr
        vb <- peekBufStruct (getPtrOf (ccsVals f))
        refBits <- peek (getPtrOf (ccsRefBits f))
        setRefBitIfZero refBits dIdx
        readBuf vb dIdx >>= onHit
    )
    onMiss

-- | Zero-allocation bounded insertion:
-- * On hit: updates value in-place and sets reference bit to @1@.
-- * On miss: claims a free slot if @size < capacity@, else sweeps @ccsHand@ clearing @1@
--   reference bits until evicting the first @0@ reference-bit victim, then writes @(key, val)@
--   with reference bit @1@.
insert ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert cc@(ClockCache hdr) key val = do
  (exists, _, existingDense) <- findSlotAndDense cc key
  let f = projectPtrs hdr
  ifThenElseM_
    exists
    ( do
        vb <- peekBufStruct (getPtrOf (ccsVals f))
        refBits <- peek (getPtrOf (ccsRefBits f))
        writeBuf vb existingDense val
        setRefBitIfZero refBits existingDense
    )
    ( do
        curLen <- peek (getPtrOf (ccsLen f))
        dCap <- peek (getPtrOf (ccsCap f))
        occ <- peek (getPtrOf (ccsOccupied f))
        refBits <- peek (getPtrOf (ccsRefBits f))
        kb <- peekBufStruct (getPtrOf (ccsKeys f))
        vb <- peekBufStruct (getPtrOf (ccsVals f))
        ctrl <- peek (getPtrOf (ccsCtrl f))
        sCap <- peek (getPtrOf (ccsSparseCap f))
        sparse <- peek (getPtrOf (ccsSparse f))
        seed <- peek (getPtrOf (ccsSeed f))

        targetDenseRef <- newRef (0 :: e Int64)
        ifThenElseM_
          (curLen `lt` dCap)
          ( do
              occAtLen <- peekElemOff occ curLen
              ifThenElseM_
                (occAtLen `eq` 0)
                (writeRef targetDenseRef curLen)
                ( do
                    (_, (_, freeIdx)) <-
                      runDrain (drainFind (false, 0) fst) $
                        streamM (rangeS 0 dCap) $ \i -> do
                          o <- peekElemOff occ i
                          pure (o `eq` 0, i)
                    writeRef targetDenseRef freeIdx
                )
              poke (getPtrOf (ccsLen f)) (curLen + 1)
          )
          ( do
              hand0 <- peek (getPtrOf (ccsHand f))
              (_, (_, victimIdx)) <-
                runDrain (drainFind (false, 0) fst) $
                  streamM
                    ( iterateS
                        hand0
                        (\idx -> let nxt = idx + 1 in ifThenElse (nxt `ge` dCap) 0 nxt)
                    )
                    $ \idx -> do
                      let wIdx = idx `rshift` 6
                          bitMask = (1 :: e Word64) `lshift` toIntegral (idx `iand` 63)
                      w <- peekElemOff refBits wIdx
                      isZeroRef <- newRef false
                      ifThenElseM_
                        ((w `iand` bitMask) `ne` 0)
                        (pokeElemOff refBits (w `iand` inot bitMask) wIdx)
                        ( do
                            let nextHand = ifThenElse ((idx + 1) `ge` dCap) 0 (idx + 1)
                            poke (getPtrOf (ccsHand f)) nextHand
                            writeRef isZeroRef true
                        )
                      z <- readRef isZeroRef
                      pure (z, idx)
              victimKey <- readBuf kb victimIdx
              victimSlot <- findSparseSlotForDense @k ctrl sCap sparse seed victimKey victimIdx
              deleteSparseSlot ctrl sCap (getPtrOf (ccsGrowthLeft f)) victimSlot
              pokeElemOff occ 0 victimIdx
              writeRef targetDenseRef victimIdx
          )

        gl <- peek (getPtrOf (ccsGrowthLeft f))
        whenM_ (gl `le` 0) $
          compactClockSparse @k @v hdr

        dIdx <- readRef targetDenseRef
        pokeElemOff occ 1 dIdx
        writeBuf kb dIdx key
        writeBuf vb dIdx val
        setRefBitIfZero refBits dIdx

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
          glNow <- peek (getPtrOf (ccsGrowthLeft f))
          poke (getPtrOf (ccsGrowthLeft f)) (glNow - 1)
    )

delete ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Elem e k ->
  m (e Bool)
delete cc@(ClockCache hdr) key = do
  (found, targetSlot, dIdx) <- findSlotAndDense cc key
  whenM_ found $ do
    let f = projectPtrs hdr
    ctrl <- peek (getPtrOf (ccsCtrl f))
    sCap <- peek (getPtrOf (ccsSparseCap f))
    occ <- peek (getPtrOf (ccsOccupied f))
    refBits <- peek (getPtrOf (ccsRefBits f))
    curLen <- peek (getPtrOf (ccsLen f))
    deleteSparseSlot ctrl sCap (getPtrOf (ccsGrowthLeft f)) targetSlot
    pokeElemOff occ 0 dIdx
    clearRefBit refBits dIdx
    poke (getPtrOf (ccsLen f)) (curLen - 1)
  pure found

--------------------------------------------------------------------------------
-- Streams & Drains
--------------------------------------------------------------------------------

toStream ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Stream m (Elem e k, Elem e v)
toStream (ClockCache hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (ccsCap f))
            <*> peek (getPtrOf (ccsOccupied f))
            <*> peekBufStruct (getPtrOf (ccsKeys f))
            <*> peekBufStruct (getPtrOf (ccsVals f))
        )
        $ \(dCap, occ, kb, vb) ->
          streamM
            ( filterMapS
                (\i -> (\o -> (o `ne` 0, i)) <$> peekElemOff occ i)
                (rangeS 0 dCap)
            )
            (\dIdx -> (,) <$> readBuf kb dIdx <*> readBuf vb dIdx)

keysS ::
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Stream m (Elem e k)
keysS cc = fst <$> toStream cc

valuesS ::
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Stream m (Elem e v)
valuesS cc = snd <$> toStream cc

-- | Strip-mined in-place SIMD map over all occupied value slots without modifying CLOCK reference bits.
foreachValuesSPMD ::
  forall w k v m e.
  (ClockCacheCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  ClockCache e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (ClockCache hdr) step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (ccsLen f))
  whenM_ (curLen `gt` 0) $ do
    dCap <- peek (getPtrOf (ccsCap f))
    occ <- peek (getPtrOf (ccsOccupied f))
    vb <- peekBufStruct (getPtrOf (ccsVals f))
    foreachMaskedBufSPMD
      pw
      True
      vb
      dCap
      ( \idx -> do
          o <- peekElemOff (uniformAlignedPtr occ) idx
          pure (o `ne` 0)
      )
      step

-- | Masked-safe SIMD fold over all occupied value slots without modifying CLOCK reference bits.
foldValuesSPMD ::
  forall w k v s m e.
  (ClockCacheCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  ClockCache e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (ClockCache hdr) s0 step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (ccsLen f))
  accRef <- withSPMD pw (newRef s0)
  whenM_ (curLen `gt` 0) $ do
    dCap <- peek (getPtrOf (ccsCap f))
    occ <- peek (getPtrOf (ccsOccupied f))
    vb <- peekBufStruct (getPtrOf (ccsVals f))
    res <-
      foldMaskedBufSPMD
        pw
        True
        vb
        dCap
        ( \idx -> do
            o <- peekElemOff (uniformAlignedPtr occ) idx
            pure (o `ne` 0)
        )
        s0
        step
    withSPMD pw (writeRef accRef res)
  withSPMD pw (readRef accRef)

drainToClockCache ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  Drain m (Elem e k, Elem e v) (ClockCache e k v)
drainToClockCache alloc reqCap = Drain $ \sIn -> do
  cc <- new alloc reqCap
  runDrain (drainAppendClockCache cc) sIn
  pure cc

fromStream ::
  forall k v m e.
  (ClockCacheCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  Stream m (Elem e k, Elem e v) ->
  m (ClockCache e k v)
fromStream alloc reqCap = runDrain (drainToClockCache @k @v alloc reqCap)

drainAppendClockCache ::
  (ClockCacheCtx m e k v) =>
  ClockCache e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendClockCache cc = drainForEach (\(kIn, vIn) -> insert cc kIn vIn)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type ClockCacheABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCHost (ClockCache (JanusCT _) k v) = ClockCache Identity k v

deriving via
  ViaPtrHandle t (CStruct (ClockCacheSpine k v)) (ClockCache Identity k v)
  instance
    (ClockCacheABI k v) => JanusCArg t (ClockCache (JanusCT t) k v)

deriving via
  ViaPtrHandle t (CStruct (ClockCacheSpine k v)) (ClockCache Identity k v)
  instance
    (ClockCacheABI k v) => JanusCValRet t (ClockCache (JanusCT t) k v)
