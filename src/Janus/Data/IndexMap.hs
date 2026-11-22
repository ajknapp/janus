{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Janus.Data.IndexMap
  ( -- * Dense Insertion-Ordered SwissTable IndexMap Spine & Handle
    IndexMapSpine (..),
    IndexMap (..),
    IndexMapCtx,

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

    -- * Key & Positional Lookup, Insertion, Update, & Removal
    containsKey,
    lookupIndex,
    lookup,
    withLookup,
    getAtIndex,
    getKeyAtIndex,
    getValAtIndex,
    writeValAtIndex,
    insert,
    insertWith,
    popBack,
    withPopBack,
    swapRemove,
    shiftRemove,

    -- * In-Place Zero-Allocation Key Sorting
    sortByKey,

    -- * Unit-Stride Dense Streams, SPMD Kernels, & Drains
    toStream,
    reverseStream,
    keysS,
    valuesS,
    foreachValuesSPMD,
    traverseValuesSPMD_,
    foldValuesSPMD,
    traverseKeyValuesSPMD_,
    containsKeyBatchSPMD,
    lookupScalarBatchSPMD,
    foreachScalarValuesSPMD,
    foreachSoAValuesSPMD,
    drainToIndexMap,
    fromStream,
    drainAppendIndexMap,
    drainAppendIndexMapWith,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import GHC.TypeNats (natVal)
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
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
    janusSIMDAlignment,
  )
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainFirst, drainForEach, drain_, runDrain)
import Janus.Control.Stream
  ( Stream,
    filterMapS,
    iterateS,
    rangeS,
    reverseRangeS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import Janus.Data.FlatMap (OrdKey (..))
import Janus.Data.HKD
import Janus.Data.HashMap (HashKey (..), probeGroupsS, swarBitsS, swarOccupiedSlotsS)
import Janus.Data.Layout
import Janus.Data.Sort (sortKeyValBufsBy)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.MurmurHash (defaultHashSalt)
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
    uniform,
    uniformAlignedPtr,
    withSPMD,
    withSPMDMasked,
  )
import Janus.Typed (JanusTyped)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- SwissTable Control Byte & 8-Byte SWAR Helpers
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

-- | Write a control byte at @slot@ and mirror slots @0 .. 7@ at @sCap + slot@.
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

-- | Delete an occupied SwissTable sparse slot, reclaiming @growthLeft@ immediately
-- if its 8-byte group already contains an @EMPTY@ byte.
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
-- IndexMap Spine & Handle
--------------------------------------------------------------------------------

-- | C-ABI spine for 'IndexMap': combines a sparse SwissTable index mapping
-- hash slots to @0 .. imsDenseLen - 1@ with hole-free contiguous dense buffers
-- @imsKeys@ and @imsVals@ preserving exact insertion order.
data IndexMapSpine k v f = IndexMapSpine
  { imsCtrl :: !(f (Ptr Word8)),
    imsSparse :: !(f (Ptr Int64)),
    imsSparseCap :: !(f Int64),
    imsGrowthLeft :: !(f Int64),
    imsSeed :: !(f Word64),
    imsKeys :: !(f (CStruct (Buf k))),
    imsVals :: !(f (CStruct (Buf v))),
    imsDenseLen :: !(f Int64),
    imsDenseCap :: !(f Int64)
  }
  deriving stock (Generic)

instance
  (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) =>
  JanusStruct (IndexMapSpine k v)

newtype IndexMap (e :: K.Type -> K.Type) k v = IndexMap
  { unIndexMap :: e (Ptr (CStruct (IndexMapSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (IndexMap Identity k v)

deriving stock instance Show (IndexMap Identity k v)

type IndexMapCtx m e k v =
  ( LayoutCtx m e k,
    LayoutCtx m e v,
    HashKey e k,
    Typeable k,
    Typeable v,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct (IndexMapSpine k v)),
    JanusTyped e (CStruct (IndexMapSpine k v))
  )

--------------------------------------------------------------------------------
-- Lifecycle, Queries, & Capacity Management
--------------------------------------------------------------------------------

new ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  m (IndexMap e k v)
new alloc = do
  hdr <- allocStruct @(CStruct (IndexMapSpine k v)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  v0 <- allocBuf @v alloc 0
  poke (getPtrOf (imsCtrl f)) nullPtr
  poke (getPtrOf (imsSparse f)) nullPtr
  poke (getPtrOf (imsSparseCap f)) 0
  poke (getPtrOf (imsGrowthLeft f)) 0
  poke (getPtrOf (imsSeed f)) defaultHashSalt
  pokeBufStruct (getPtrOf (imsKeys f)) k0
  pokeBufStruct (getPtrOf (imsVals f)) v0
  poke (getPtrOf (imsDenseLen f)) 0
  poke (getPtrOf (imsDenseCap f)) 0
  pure (IndexMap hdr)

withCapacity ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (IndexMap e k v)
withCapacity alloc minElems = do
  im <- new alloc
  whenM_ (minElems `gt` 0) $
    reserve alloc im minElems
  pure im

free ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  m ()
free alloc (IndexMap hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (imsSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (imsCtrl f))
    sparse <- peek (getPtrOf (imsSparse f))
    freeBytes alloc (toVoidPtr ctrl) (sCap + 8)
    freeTypedArray @Int64 alloc sparse sCap
  dCap <- peek (getPtrOf (imsDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (imsKeys f))
    vb <- peekBufStruct (getPtrOf (imsVals f))
    freeBuf alloc kb dCap
    freeBuf alloc vb dCap
  freeStruct alloc hdr

size :: (IndexMapCtx m e k v) => IndexMap e k v -> m (e Int64)
size (IndexMap hdr) = peek (getPtrOf (imsDenseLen (projectPtrs hdr)))

capacity :: (IndexMapCtx m e k v) => IndexMap e k v -> m (e Int64)
capacity (IndexMap hdr) = peek (getPtrOf (imsDenseCap (projectPtrs hdr)))

isEmpty :: (IndexMapCtx m e k v) => IndexMap e k v -> m (e Bool)
isEmpty im = (`eq` 0) <$> size im

clear ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  m ()
clear (IndexMap hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (imsSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (imsCtrl f))
    cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
    poke (getPtrOf (imsGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (imsDenseLen f)) 0

-- | Rebuild the sparse SwissTable index in-place over @imsKeys[0 .. curLen - 1]@
-- using already-allocated @ctrl@ and @sparse@ buffers of capacity @sCap > 0@.
rebuildSparseInPlace ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  e (Ptr (CStruct (IndexMapSpine k v))) ->
  m ()
rebuildSparseInPlace hdr = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (imsSparseCap f))
  curLen <- peek (getPtrOf (imsDenseLen f))
  ctrl <- peek (getPtrOf (imsCtrl f))
  sparse <- peek (getPtrOf (imsSparse f))
  kb <- peekBufStruct (getPtrOf (imsKeys f))
  seed <- peek (getPtrOf (imsSeed f))
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
  runDrain
    ( drainForEach $ \dIdx -> do
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
    )
    (rangeS 0 curLen)
  poke (getPtrOf (imsGrowthLeft f)) (maxLoadForCap sCap - curLen)

rehashSparseTo ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  e Int64 ->
  m ()
rehashSparseTo alloc (IndexMap hdr) newSCap = do
  let f = projectPtrs hdr
  oldSCap <- peek (getPtrOf (imsSparseCap f))
  ifThenElseM_
    ((newSCap `eq` oldSCap) `land` (oldSCap `gt` 0))
    (rebuildSparseInPlace @k @v hdr)
    ( do
        whenM_ (oldSCap `gt` 0) $ do
          oldCtrl <- peek (getPtrOf (imsCtrl f))
          oldSparse <- peek (getPtrOf (imsSparse f))
          freeBytes alloc (toVoidPtr oldCtrl) (oldSCap + 8)
          freeTypedArray @Int64 alloc oldSparse oldSCap
        newCtrlVP <- allocBytesAligned alloc (newSCap + 8) (fromIntegral janusSIMDAlignment)
        let newCtrl = fromVoidPtr newCtrlVP :: e (Ptr Word8)
        newSparse <- allocTypedArray @Int64 alloc newSCap
        poke (getPtrOf (imsCtrl f)) newCtrl
        poke (getPtrOf (imsSparse f)) newSparse
        poke (getPtrOf (imsSparseCap f)) newSCap
        rebuildSparseInPlace @k @v hdr
    )

ensureDenseCap ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  e Int64 ->
  m ()
ensureDenseCap alloc (IndexMap hdr) needed = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (imsDenseLen f))
  curDCap <- peek (getPtrOf (imsDenseCap f))
  whenM_ (needed `gt` curDCap) $ do
    (_, newDCap) <-
      runDrain
        (drainFind 8 (`ge` needed))
        (iterateS (ifThenElse (curDCap `lt` 8) 8 (curDCap * 2)) (* 2))
    oldK <- peekBufStruct (getPtrOf (imsKeys f))
    oldV <- peekBufStruct (getPtrOf (imsVals f))
    newK <- reallocBuf alloc oldK curDCap newDCap curLen
    newV <- reallocBuf alloc oldV curDCap newDCap curLen
    pokeBufStruct (getPtrOf (imsKeys f)) newK
    pokeBufStruct (getPtrOf (imsVals f)) newV
    poke (getPtrOf (imsDenseCap f)) newDCap

reserve ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  e Int64 ->
  m ()
reserve alloc im@(IndexMap hdr) additional =
  whenM_ (additional `gt` 0) $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (imsDenseLen f))
    needed <- letM (curLen + additional)
    ensureDenseCap alloc im needed
    curSCap <- peek (getPtrOf (imsSparseCap f))
    gl <- peek (getPtrOf (imsGrowthLeft f))
    whenM_ (additional `gt` gl) $ do
      (_, targetSCap) <-
        runDrain
          (drainFind 16 (\cand -> maxLoadForCap cand `ge` needed))
          (iterateS (ifThenElse (curSCap `lt` 16) 16 curSCap) (`lshift` 1))
      rehashSparseTo alloc im targetSCap

--------------------------------------------------------------------------------
-- Probing, Positional Access, Insertion, & Removal
--------------------------------------------------------------------------------

-- | Probe for @key@, returning @(found, sparseSlot, denseIdx)@.
findSlotAndDense ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m (e Bool, e Int64, e Int64)
findSlotAndDense (IndexMap hdr) key = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (imsSparseCap f))
  foundRef <- newRef false
  slotRef <- newRef (0 :: e Int64)
  denseRef <- newRef (0 :: e Int64)
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (imsCtrl f))
    sparse <- peek (getPtrOf (imsSparse f))
    kb <- peekBufStruct (getPtrOf (imsKeys f))
    seed <- peek (getPtrOf (imsSeed f))
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
    writeRef foundRef hitFound
    writeRef slotRef hitSlot
    writeRef denseRef hitDense
  (,,) <$> readRef foundRef <*> readRef slotRef <*> readRef denseRef

-- | Locate the exact occupied sparse slot pointing to a known live @(key, targetDenseIdx)@.
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

containsKey ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m (e Bool)
containsKey im key = (\(ok, _, _) -> ok) <$> findSlotAndDense im key

lookupIndex ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m (e Bool, e Int64)
lookupIndex im key = do
  (ok, _, dIdx) <- findSlotAndDense im key
  pure (ok, dIdx)

lookup ::
  forall k v s m e.
  (IndexMapCtx m e k v, Elem e v ~ e s, JanusTyped e s) =>
  e s ->
  IndexMap e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal im@(IndexMap hdr) key = do
  (found, _, dIdx) <- findSlotAndDense im key
  valRef <- newRef defVal
  whenM_ found $ do
    vb <- peekBufStruct (getPtrOf (imsVals (projectPtrs hdr)))
    readBuf vb dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withLookup ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup im@(IndexMap hdr) key onMiss onHit = do
  (found, _, dIdx) <- findSlotAndDense im key
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (imsVals (projectPtrs hdr)))
        readBuf vb dIdx >>= onHit
    )
    onMiss

getAtIndex ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  e Int64 ->
  m (Elem e k, Elem e v)
getAtIndex (IndexMap hdr) idx = do
  let f = projectPtrs hdr
  kb <- peekBufStruct (getPtrOf (imsKeys f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  (,) <$> readBuf kb idx <*> readBuf vb idx

getKeyAtIndex ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  e Int64 ->
  m (Elem e k)
getKeyAtIndex (IndexMap hdr) idx = do
  kb <- peekBufStruct (getPtrOf (imsKeys (projectPtrs hdr)))
  readBuf kb idx

getValAtIndex ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  e Int64 ->
  m (Elem e v)
getValAtIndex (IndexMap hdr) idx = do
  vb <- peekBufStruct (getPtrOf (imsVals (projectPtrs hdr)))
  readBuf vb idx

writeValAtIndex ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  e Int64 ->
  Elem e v ->
  m ()
writeValAtIndex (IndexMap hdr) idx val = do
  vb <- peekBufStruct (getPtrOf (imsVals (projectPtrs hdr)))
  writeBuf vb idx val

insert ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert alloc im key val = insertWith alloc im (\_ newV -> newV) key val

-- | Insert @(key, val)@ at the back if absent, or update its existing dense slot in-place
-- via @combine oldVal newVal@ while preserving insertion order.
insertWith ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  (Elem e v -> Elem e v -> Elem e v) ->
  Elem e k ->
  Elem e v ->
  m ()
insertWith alloc im@(IndexMap hdr) combine key val = do
  (exists, _, existingDense) <- findSlotAndDense im key
  let f = projectPtrs hdr
  ifThenElseM_
    exists
    ( do
        vb <- peekBufStruct (getPtrOf (imsVals f))
        oldV <- readBuf vb existingDense
        writeBuf vb existingDense (combine oldV val)
    )
    ( do
        curLen <- peek (getPtrOf (imsDenseLen f))
        curDCap <- peek (getPtrOf (imsDenseCap f))
        whenM_ (curLen `eq` curDCap) $
          ensureDenseCap alloc im (curLen + 1)

        gl <- peek (getPtrOf (imsGrowthLeft f))
        whenM_ (gl `le` 0) $ do
          curSCap <- peek (getPtrOf (imsSparseCap f))
          let nextSCap =
                ifThenElse
                  (curSCap `eq` 0)
                  16
                  (ifThenElse ((curLen * 2) `lt` maxLoadForCap curSCap) curSCap (curSCap `lshift` 1))
          rehashSparseTo alloc im nextSCap

        kb <- peekBufStruct (getPtrOf (imsKeys f))
        vb <- peekBufStruct (getPtrOf (imsVals f))
        writeBuf kb curLen key
        writeBuf vb curLen val
        poke (getPtrOf (imsDenseLen f)) (curLen + 1)

        sCap <- peek (getPtrOf (imsSparseCap f))
        ctrl <- peek (getPtrOf (imsCtrl f))
        sparse <- peek (getPtrOf (imsSparse f))
        seed <- peek (getPtrOf (imsSeed f))
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
        pokeElemOff sparse curLen dstSlot
        whenM_ (prevCtrl `eq` fromIntegral ctrlEmptyByte) $ do
          glNow <- peek (getPtrOf (imsGrowthLeft f))
          poke (getPtrOf (imsGrowthLeft f)) (glNow - 1)
    )

-- | Pop the most-recently inserted back entry in O(1), returning @(poppedOk, key, val)@.
popBack ::
  forall k v ks vs m e.
  (IndexMapCtx m e k v, Elem e k ~ e ks, Elem e v ~ e vs, JanusTyped e ks, JanusTyped e vs) =>
  e ks ->
  e vs ->
  IndexMap e k v ->
  m (e Bool, e ks, e vs)
popBack defK defV im = do
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  withPopBack
    im
    (pure ())
    ( \kOut vOut -> do
        writeRef okRef true
        writeRef kRef kOut
        writeRef vRef vOut
    )
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

-- | Effectful O(1) pop of the back entry supporting all 'StorageLayout's (including HKD structs).
withPopBack ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withPopBack (IndexMap hdr) onEmpty onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (imsDenseLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let lastIdx = curLen - 1
        kb <- peekBufStruct (getPtrOf (imsKeys f))
        vb <- peekBufStruct (getPtrOf (imsVals f))
        lastK <- readBuf kb lastIdx
        lastV <- readBuf vb lastIdx
        ctrl <- peek (getPtrOf (imsCtrl f))
        sCap <- peek (getPtrOf (imsSparseCap f))
        sparse <- peek (getPtrOf (imsSparse f))
        seed <- peek (getPtrOf (imsSeed f))
        lastSlot <- findSparseSlotForDense @k ctrl sCap sparse seed lastK lastIdx
        deleteSparseSlot ctrl sCap (getPtrOf (imsGrowthLeft f)) lastSlot
        poke (getPtrOf (imsDenseLen f)) lastIdx
        onHit lastK lastV
    )

-- | O(1) removal: deletes @key@'s sparse slot and swaps the last dense entry into @denseIdx@
-- (updating the moved key's sparse index). Does NOT preserve order of the swapped tail element.
swapRemove ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m (e Bool)
swapRemove im@(IndexMap hdr) key = do
  (found, targetSlot, denseIdx) <- findSlotAndDense im key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (imsDenseLen f))
    let lastIdx = curLen - 1
    ctrl <- peek (getPtrOf (imsCtrl f))
    sCap <- peek (getPtrOf (imsSparseCap f))
    sparse <- peek (getPtrOf (imsSparse f))
    deleteSparseSlot ctrl sCap (getPtrOf (imsGrowthLeft f)) targetSlot
    whenM_ (denseIdx `lt` lastIdx) $ do
      kb <- peekBufStruct (getPtrOf (imsKeys f))
      vb <- peekBufStruct (getPtrOf (imsVals f))
      seed <- peek (getPtrOf (imsSeed f))
      movedK <- readBuf kb lastIdx
      movedV <- readBuf vb lastIdx
      movedSlot <- findSparseSlotForDense @k ctrl sCap sparse seed movedK lastIdx
      pokeElemOff sparse denseIdx movedSlot
      writeBuf kb denseIdx movedK
      writeBuf vb denseIdx movedV
    poke (getPtrOf (imsDenseLen f)) lastIdx
  pure found

-- | O(N) order-preserving removal: deletes @key@'s sparse slot, shifts dense @[denseIdx + 1 .. len - 1]@
-- left by 1 via 'moveBuf', and decrements every occupied sparse entry @> denseIdx@ via 'swarOccupiedSlotsS'.
shiftRemove ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Elem e k ->
  m (e Bool)
shiftRemove im@(IndexMap hdr) key = do
  (found, targetSlot, denseIdx) <- findSlotAndDense im key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (imsDenseLen f))
    let lastIdx = curLen - 1
        tailCnt = lastIdx - denseIdx
    ctrl <- peek (getPtrOf (imsCtrl f))
    sCap <- peek (getPtrOf (imsSparseCap f))
    sparse <- peek (getPtrOf (imsSparse f))
    deleteSparseSlot ctrl sCap (getPtrOf (imsGrowthLeft f)) targetSlot
    whenM_ (tailCnt `gt` 0) $ do
      kb <- peekBufStruct (getPtrOf (imsKeys f))
      vb <- peekBufStruct (getPtrOf (imsVals f))
      moveBuf kb denseIdx (denseIdx + 1) tailCnt
      moveBuf vb denseIdx (denseIdx + 1) tailCnt
      let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
          numGroups = sCap `rshift` 3
      runDrain
        ( drainForEach $ \sl -> do
            dIdx <- peekElemOff sparse sl
            whenM_ (dIdx `gt` denseIdx) $
              pokeElemOff sparse (dIdx - 1) sl
        )
        (swarOccupiedSlotsS ctrlWords numGroups)
    poke (getPtrOf (imsDenseLen f)) lastIdx
  pure found

-- | Sort dense @(keys, vals)@ ascending by key in-place via 'sortKeyValBufsBy' and rebuild
-- the SwissTable sparse index in-place with ZERO heap allocations.
sortByKey ::
  forall k v m e.
  (IndexMapCtx m e k v, OrdKey e k) =>
  IndexMap e k v ->
  m ()
sortByKey (IndexMap hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (imsDenseLen f))
  whenM_ (curLen `gt` 1) $ do
    kb <- peekBufStruct (getPtrOf (imsKeys f))
    vb <- peekBufStruct (getPtrOf (imsVals f))
    sortKeyValBufsBy (ltKey @e @k) kb vb curLen
    rebuildSparseInPlace @k @v hdr

--------------------------------------------------------------------------------
-- Unit-Stride Dense Streams, SPMD Kernels, & Drains
--------------------------------------------------------------------------------

toStream ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Stream m (Elem e k, Elem e v)
toStream (IndexMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,)
            <$> peek (getPtrOf (imsDenseLen f))
            <*> peekBufStruct (getPtrOf (imsKeys f))
            <*> peekBufStruct (getPtrOf (imsVals f))
        )
        (\(n, kb, vb) -> streamM (rangeS 0 n) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

reverseStream ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Stream m (Elem e k, Elem e v)
reverseStream (IndexMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,)
            <$> peek (getPtrOf (imsDenseLen f))
            <*> peekBufStruct (getPtrOf (imsKeys f))
            <*> peekBufStruct (getPtrOf (imsVals f))
        )
        (\(n, kb, vb) -> streamM (reverseRangeS 0 n) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

keysS ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Stream m (Elem e k)
keysS (IndexMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (imsDenseLen f)) <*> peekBufStruct (getPtrOf (imsKeys f)))
        (\(n, kb) -> streamM (rangeS 0 n) (readBuf kb))

valuesS ::
  (IndexMapCtx m e k v) =>
  IndexMap e k v ->
  Stream m (Elem e v)
valuesS (IndexMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (imsDenseLen f)) <*> peekBufStruct (getPtrOf (imsVals f)))
        (\(n, vb) -> streamM (rangeS 0 n) (readBuf vb))

-- | Layout-polymorphic unit-stride in-place SIMD map over contiguous insertion-ordered values @[0, size)@.
foreachValuesSPMD ::
  forall w k v m e.
  (IndexMapCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  IndexMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (IndexMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (imsDenseLen f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  foreachBufSPMD pw True vb n step

-- | Layout-polymorphic read-only unit-stride SIMD traversal over contiguous insertion-ordered values @[0, size)@.
traverseValuesSPMD_ ::
  forall w k v m e.
  (IndexMapCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  IndexMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e ()) ->
  m ()
traverseValuesSPMD_ pw (IndexMap hdr) body = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (imsDenseLen f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  traverseBufSPMD_ pw True vb n body

-- | Layout-polymorphic masked-safe unit-stride SIMD fold over contiguous insertion-ordered values @[0, size)@.
foldValuesSPMD ::
  forall w k v s m e.
  (IndexMapCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  IndexMap e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (IndexMap hdr) s0 step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (imsDenseLen f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  foldBufSPMD pw True vb n s0 step

-- | Simultaneous unit-stride SIMD traversal over contiguous insertion-ordered @(keys, vals)@ across @[0, size)@.
traverseKeyValuesSPMD_ ::
  forall w k v m e.
  (IndexMapCtx m e k v, LayoutUnitSPMD w m e k, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  IndexMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) k -> Elem (SPMD w e) v -> SPMDM w m e ()) ->
  m ()
traverseKeyValuesSPMD_ pw (IndexMap hdr) body = do
  let f = projectPtrs hdr
      wE = fromIntegral (natVal pw) :: e Int64
  n <- peek (getPtrOf (imsDenseLen f))
  kb <- peekBufStruct (getPtrOf (imsKeys f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  numFull <- letM (ifThenElse (n `gt` 0) (n `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = spmdAlignedUnitStride base
        withSPMD pw $ do
          kGang <- readAlignedGangBuf @w @m @e @k pw True kb g idx
          vGang <- readAlignedGangBuf @w @m @e @v pw True vb g idx
          body idx kGang vGang
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` n) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform n) $ do
      kGang <- readAlignedGangBuf @w @m @e @k pw True kb numFull tailIdx
      vGang <- readAlignedGangBuf @w @m @e @v pw True vb numFull tailIdx
      body tailIdx kGang vGang

probeIndexMapBatchSPMD ::
  forall w k v m e.
  (IndexMapCtx m e k v, SPMDGang w m e, HashKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  IndexMap e k v ->
  Elem (SPMD w e) k ->
  (SPMD w e Int64 -> SPMDM w m e ()) ->
  SPMDM w m e (SPMD w e Bool)
probeIndexMapBatchSPMD (IndexMap hdr) qKeys onHit = do
  let f = projectPtrs hdr
  sCap <- liftSPMD (peek (getPtrOf (imsSparseCap f)))
  foundRef <- newRef false
  whenM_ (uniform sCap `gt` 0) $ do
    ctrl <- liftSPMD (peek (getPtrOf (imsCtrl f)))
    sparse <- liftSPMD (peek (getPtrOf (imsSparse f)))
    kb <- liftSPMD (peekBufStruct (getPtrOf (imsKeys f)))
    seed <- liftSPMD (peek (getPtrOf (imsSeed f)))
    let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (sCap `rshift` 3) - 1
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
            dIdx <- peekElemOff (uniformAlignedPtr sparse) sl
            ck <- gatherBufSPMD kb dIdx
            let hit = eqElem @(SPMD w e) @k ck qKeys
            whenM_ hit $ do
              writeRef foundRef true
              onHit dIdx
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
  (IndexMapCtx m e k v, SPMDGang w m e, HashKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  Proxy w ->
  IndexMap e k v ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
containsKeyBatchSPMD _ im qKeys =
  probeIndexMapBatchSPMD im qKeys (\_ -> pure ())

-- | Lane-parallel SIMD batch scalar value lookup probing @w@ keys simultaneously.
lookupScalarBatchSPMD ::
  forall w k v s m e.
  ( IndexMapCtx m e k v,
    SPMDGang w m e,
    HashKey (SPMD w e) k,
    LayoutGatherSPMD w m e k,
    LayoutGatherSPMD w m e v,
    Elem (SPMD w e) v ~ SPMD w e s,
    SPMDTyped w e s
  ) =>
  Proxy w ->
  SPMD w e s ->
  IndexMap e k v ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool, SPMD w e s)
lookupScalarBatchSPMD _ defVal im@(IndexMap hdr) qKeys = do
  valRef <- newRef defVal
  vb <- liftSPMD (peekBufStruct (getPtrOf (imsVals (projectPtrs hdr))))
  found <-
    probeIndexMapBatchSPMD im qKeys $ \dIdx -> do
      gv <- gatherBufSPMD vb dIdx
      writeRef valRef gv
  val <- readRef valRef
  pure (found, val)

foreachScalarValuesSPMD ::
  forall w k v m e.
  ( IndexMapCtx m e k v,
    Buf v ~ ScalarBuf v,
    SPMDGang w m e,
    SPMDLayout w (Ptr v) ~ 'LPtr v,
    CmdStorable (SPMDM w m e) (SPMD w e) v
  ) =>
  Proxy w ->
  IndexMap e k v ->
  (SPMD w e Int64 -> SPMD w e v -> SPMDM w m e (SPMD w e v)) ->
  m ()
foreachScalarValuesSPMD pw (IndexMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (imsDenseLen f))
  ScalarBuf rawPtr <- peekBufStruct (getPtrOf (imsVals f))
  let wE = fromIntegral (natVal pw) :: e Int64
      spPtr = uniformAlignedPtr rawPtr
  numFull <- letM (ifThenElse (n `gt` 0) (n `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = spmdAlignedUnitStride base
        withSPMD pw $ do
          cur <- peekElemOff spPtr idx
          nxt <- step idx cur
          pokeElemOff spPtr nxt idx
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` n) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform n) $ do
      cur <- peekElemOff spPtr tailIdx
      nxt <- step tailIdx cur
      pokeElemOff spPtr nxt tailIdx

foreachSoAValuesSPMD ::
  forall w k s m e.
  ( IndexMapCtx m e k (SoA s),
    SPMDGang w m e,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  IndexMap e k (SoA s) ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachSoAValuesSPMD pw (IndexMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (imsDenseLen f))
  vb <- peekBufStruct (getPtrOf (imsVals f))
  foreachSoASPMD pw vb n step

drainToIndexMap ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e v) (IndexMap e k v)
drainToIndexMap alloc = Drain $ \sIn -> do
  im <- new alloc
  runDrain (drainAppendIndexMap alloc im) sIn
  pure im

fromStream ::
  forall k v m e.
  (IndexMapCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e v) ->
  m (IndexMap e k v)
fromStream alloc = runDrain (drainToIndexMap @k @v alloc)

drainAppendIndexMap ::
  (IndexMapCtx m e k v) =>
  Alloc e ->
  IndexMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendIndexMap alloc im = drainForEach (\(kIn, vIn) -> insert alloc im kIn vIn)

drainAppendIndexMapWith ::
  (IndexMapCtx m e k v) =>
  Alloc e ->
  (Elem e v -> Elem e v -> Elem e v) ->
  IndexMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendIndexMapWith alloc combine im =
  drainForEach (\(kIn, vIn) -> insertWith alloc im combine kIn vIn)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type IndexMapABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCEval (IndexMap JanusC k v -> r) = IndexMap Identity k v -> JanusCEval r

type instance JanusCRetType (IndexMap JanusC k v -> r) = JanusCRetType r

instance (IndexMapABI k v, JanusCParam r) => JanusCParam (IndexMap JanusC k v -> r) where
  jcparam = mkPtrHandleJCParam IndexMap

instance (IndexMapABI k v, JanusCEvaluate r) => JanusCEvaluate (IndexMap JanusC k v -> r) where
  jceval = mkPtrHandleJCEval unIndexMap IndexMap

type instance JanusCEval (JanusCM (IndexMap JanusC k v)) = IO (IndexMap Identity k v)

type instance JanusCRetType (JanusCM (IndexMap JanusC k v)) = IndexMap Identity k v

instance (IndexMapABI k v) => JanusCParam (JanusCM (IndexMap JanusC k v)) where
  jcparam = mkPtrHandleJCRetParam unIndexMap

instance (IndexMapABI k v) => JanusCEvaluate (JanusCM (IndexMap JanusC k v)) where
  jceval = mkPtrHandleJCRetEval IndexMap

type instance LinuxX86_64CEval (IndexMap LinuxX86_64C k v -> r) = IndexMap Identity k v -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (IndexMap LinuxX86_64C k v -> r) = LinuxX86_64BaseRet r

instance (IndexMapABI k v, LinuxX86_64Param r) => LinuxX86_64Param (IndexMap LinuxX86_64C k v -> r) where
  x86param = mkPtrHandleX86Param IndexMap

instance (IndexMapABI k v, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (IndexMap LinuxX86_64C k v -> r) where
  x86eval = mkPtrHandleX86EvalParam unIndexMap IndexMap

instance (IndexMapABI k v, X86DeferCont r) => X86DeferCall (IndexMap LinuxX86_64C k v -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unIndexMap IndexMap

instance (IndexMapABI k v, X86DeferCont r) => X86DeferCont (IndexMap LinuxX86_64C k v -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unIndexMap IndexMap

type instance LinuxX86_64CEval (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) = IO (IndexMap Identity k v)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) = IndexMap Identity k v

instance (IndexMapABI k v) => LinuxX86_64Param (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) where
  x86param = mkPtrHandleX86RetParam unIndexMap

instance (IndexMapABI k v) => LinuxX86_64Evaluate (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) where
  x86eval = mkPtrHandleX86RetEval IndexMap

instance (IndexMapABI k v) => X86DeferCall (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg IndexMap

instance (IndexMapABI k v) => X86DeferCont (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont IndexMap

instance (IndexMapABI k v, LinuxX86_64Param r, JanusCTH r) => JanusCTH (IndexMap LinuxX86_64C k v -> r) where
  type JanusCTHEval (IndexMap LinuxX86_64C k v -> r) = IndexMap Identity k v -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(IndexMap Identity k v)) : ps, retSpec)

instance (IndexMapABI k v) => JanusCTH (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) where
  type JanusCTHEval (LinuxX86_64CM (IndexMap LinuxX86_64C k v)) = IO (IndexMap Identity k v)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(IndexMap Identity k v)) (Proxy @(CStruct (IndexMapSpine k v))))
