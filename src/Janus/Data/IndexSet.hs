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

module Janus.Data.IndexSet
  ( -- * Key-Only Dense Insertion-Ordered SwissTable IndexSet Spine & Handle
    IndexSetSpine (..),
    IndexSet (..),
    IndexSetCtx,

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

    -- * Membership, Positional Lookup, Insertion, & Removal
    member,
    lookupIndex,
    getAtIndex,
    insert,
    popBack,
    withPopBack,
    swapRemove,
    shiftRemove,
    isSubsetOf,

    -- * In-Place Zero-Allocation Sorting
    sort,

    -- * Unit-Stride Dense Streams, SPMD Kernels, Set-Algebra Operations, & Drains
    stream,
    reverseStream,
    traverseSPMD_,
    foldSPMD,
    memberBatchSPMD,
    intersectionS,
    differenceS,
    unionInto,
    drainToIndexSet,
    fromStream,
    drainAppendIndexSet,
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
    freeBytes,
    freeStruct,
    freeTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
    janusSIMDAlignment,
  )
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainAll, drainFind, drainFirst, drainForEach, drain_, runDrain)
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
import Janus.Data.Sort (sortBufBy)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (defaultHashSalt)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    uniform,
    uniformAlignedPtr,
  )
import Janus.Typed (JanusTyped)

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
-- IndexSet Spine & Handle
--------------------------------------------------------------------------------

-- | Dedicated key-only dense-packed insertion-ordered SwissTable spine.
data IndexSetSpine k f = IndexSetSpine
  { issCtrl :: !(f (Ptr Word8)),
    issSparse :: !(f (Ptr Int64)),
    issSparseCap :: !(f Int64),
    issGrowthLeft :: !(f Int64),
    issSeed :: !(f Word64),
    issKeys :: !(f (CStruct (Buf k))),
    issDenseLen :: !(f Int64),
    issDenseCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, JanusStruct (Buf k)) => JanusStruct (IndexSetSpine k)

newtype IndexSet (e :: K.Type -> K.Type) k = IndexSet
  { unIndexSet :: e (Ptr (CStruct (IndexSetSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (IndexSet Identity k)

deriving stock instance Show (IndexSet Identity k)

type IndexSetCtx m e k =
  ( LayoutCtx m e k,
    HashKey e k,
    Typeable k,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct (IndexSetSpine k)),
    JanusTyped e (CStruct (IndexSetSpine k))
  )

--------------------------------------------------------------------------------
-- Lifecycle, Queries, & Capacity Management
--------------------------------------------------------------------------------

new ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  m (IndexSet e k)
new alloc = do
  hdr <- allocStruct @(CStruct (IndexSetSpine k)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  poke (getPtrOf (issCtrl f)) nullPtr
  poke (getPtrOf (issSparse f)) nullPtr
  poke (getPtrOf (issSparseCap f)) 0
  poke (getPtrOf (issGrowthLeft f)) 0
  poke (getPtrOf (issSeed f)) defaultHashSalt
  pokeBufStruct (getPtrOf (issKeys f)) k0
  poke (getPtrOf (issDenseLen f)) 0
  poke (getPtrOf (issDenseCap f)) 0
  pure (IndexSet hdr)

withCapacity ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (IndexSet e k)
withCapacity alloc minElems = do
  is <- new alloc
  whenM_ (minElems `gt` 0) $
    reserve alloc is minElems
  pure is

free ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  m ()
free alloc (IndexSet hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (issSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (issCtrl f))
    sparse <- peek (getPtrOf (issSparse f))
    freeBytes alloc (toVoidPtr ctrl) (sCap + 8)
    freeTypedArray @Int64 alloc sparse sCap
  dCap <- peek (getPtrOf (issDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (issKeys f))
    freeBuf alloc kb dCap
  freeStruct alloc hdr

size :: (IndexSetCtx m e k) => IndexSet e k -> m (e Int64)
size (IndexSet hdr) = peek (getPtrOf (issDenseLen (projectPtrs hdr)))

capacity :: (IndexSetCtx m e k) => IndexSet e k -> m (e Int64)
capacity (IndexSet hdr) = peek (getPtrOf (issDenseCap (projectPtrs hdr)))

isEmpty :: (IndexSetCtx m e k) => IndexSet e k -> m (e Bool)
isEmpty is = (`eq` 0) <$> size is

clear ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  m ()
clear (IndexSet hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (issSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (issCtrl f))
    cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
    poke (getPtrOf (issGrowthLeft f)) (maxLoadForCap sCap)
  poke (getPtrOf (issDenseLen f)) 0

rebuildSparseInPlace ::
  forall k m e.
  (IndexSetCtx m e k) =>
  e (Ptr (CStruct (IndexSetSpine k))) ->
  m ()
rebuildSparseInPlace hdr = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (issSparseCap f))
  curLen <- peek (getPtrOf (issDenseLen f))
  ctrl <- peek (getPtrOf (issCtrl f))
  sparse <- peek (getPtrOf (issSparse f))
  kb <- peekBufStruct (getPtrOf (issKeys f))
  seed <- peek (getPtrOf (issSeed f))
  cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
  let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
      groupMask = (sCap `rshift` 3) - 1
  runDrain
    ( drainForEach $ \dIdx -> do
        key <- readBuf kb dIdx
        h <- letM (hashElemWithSalt @e @k seed key)
        let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
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
  poke (getPtrOf (issGrowthLeft f)) (maxLoadForCap sCap - curLen)

rehashSparseTo ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  e Int64 ->
  m ()
rehashSparseTo alloc (IndexSet hdr) newSCap = do
  let f = projectPtrs hdr
  oldSCap <- peek (getPtrOf (issSparseCap f))
  ifThenElseM_
    ((newSCap `eq` oldSCap) `land` (oldSCap `gt` 0))
    (rebuildSparseInPlace @k hdr)
    ( do
        whenM_ (oldSCap `gt` 0) $ do
          oldCtrl <- peek (getPtrOf (issCtrl f))
          oldSparse <- peek (getPtrOf (issSparse f))
          freeBytes alloc (toVoidPtr oldCtrl) (oldSCap + 8)
          freeTypedArray @Int64 alloc oldSparse oldSCap
        newCtrlVP <- allocBytesAligned alloc (newSCap + 8) (fromIntegral janusSIMDAlignment)
        let newCtrl = fromVoidPtr newCtrlVP :: e (Ptr Word8)
        newSparse <- allocTypedArray @Int64 alloc newSCap
        poke (getPtrOf (issCtrl f)) newCtrl
        poke (getPtrOf (issSparse f)) newSparse
        poke (getPtrOf (issSparseCap f)) newSCap
        rebuildSparseInPlace @k hdr
    )

ensureDenseCap ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  e Int64 ->
  m ()
ensureDenseCap alloc (IndexSet hdr) needed = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (issDenseLen f))
  curDCap <- peek (getPtrOf (issDenseCap f))
  whenM_ (needed `gt` curDCap) $ do
    (_, newDCap) <-
      runDrain
        (drainFind 8 (`ge` needed))
        (iterateS (ifThenElse (curDCap `lt` 8) 8 (curDCap * 2)) (* 2))
    oldK <- peekBufStruct (getPtrOf (issKeys f))
    newK <- reallocBuf alloc oldK curDCap newDCap curLen
    pokeBufStruct (getPtrOf (issKeys f)) newK
    poke (getPtrOf (issDenseCap f)) newDCap

reserve ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  e Int64 ->
  m ()
reserve alloc is@(IndexSet hdr) additional =
  whenM_ (additional `gt` 0) $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (issDenseLen f))
    needed <- letM (curLen + additional)
    ensureDenseCap alloc is needed
    curSCap <- peek (getPtrOf (issSparseCap f))
    gl <- peek (getPtrOf (issGrowthLeft f))
    whenM_ (additional `gt` gl) $ do
      (_, targetSCap) <-
        runDrain
          (drainFind 16 (\cand -> maxLoadForCap cand `ge` needed))
          (iterateS (ifThenElse (curSCap `lt` 16) 16 curSCap) (`lshift` 1))
      rehashSparseTo alloc is targetSCap

--------------------------------------------------------------------------------
-- Probing, Positional Access, Insertion, Removal, & Sorting
--------------------------------------------------------------------------------

findSlotAndDense ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Elem e k ->
  m (e Bool, e Int64, e Int64)
findSlotAndDense (IndexSet hdr) key = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (issSparseCap f))
  foundRef <- newRef false
  slotRef <- newRef (0 :: e Int64)
  denseRef <- newRef (0 :: e Int64)
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (issCtrl f))
    sparse <- peek (getPtrOf (issSparse f))
    kb <- peekBufStruct (getPtrOf (issKeys f))
    seed <- peek (getPtrOf (issSeed f))
    h <- letM (hashElemWithSalt @e @k seed key)
    let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
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
  let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
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

member ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Elem e k ->
  m (e Bool)
member is key = (\(ok, _, _) -> ok) <$> findSlotAndDense is key

lookupIndex ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Elem e k ->
  m (e Bool, e Int64)
lookupIndex is key = do
  (ok, _, dIdx) <- findSlotAndDense is key
  pure (ok, dIdx)

getAtIndex ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  e Int64 ->
  m (Elem e k)
getAtIndex (IndexSet hdr) idx = do
  kb <- peekBufStruct (getPtrOf (issKeys (projectPtrs hdr)))
  readBuf kb idx

-- | Insert @key@ at the back if absent, returning @true@ iff @key@ was newly inserted.
insert ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  Elem e k ->
  m (e Bool)
insert alloc is@(IndexSet hdr) key = do
  (exists, _, _) <- findSlotAndDense is key
  insertedRef <- newRef false
  whenM_ (lnot exists) $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (issDenseLen f))
    curDCap <- peek (getPtrOf (issDenseCap f))
    whenM_ (curLen `eq` curDCap) $
      ensureDenseCap alloc is (curLen + 1)

    gl <- peek (getPtrOf (issGrowthLeft f))
    whenM_ (gl `le` 0) $ do
      curSCap <- peek (getPtrOf (issSparseCap f))
      let nextSCap =
            ifThenElse
              (curSCap `eq` 0)
              16
              (ifThenElse ((curLen * 2) `lt` maxLoadForCap curSCap) curSCap (curSCap `lshift` 1))
      rehashSparseTo alloc is nextSCap

    kb <- peekBufStruct (getPtrOf (issKeys f))
    writeBuf kb curLen key
    poke (getPtrOf (issDenseLen f)) (curLen + 1)

    sCap <- peek (getPtrOf (issSparseCap f))
    ctrl <- peek (getPtrOf (issCtrl f))
    sparse <- peek (getPtrOf (issSparse f))
    seed <- peek (getPtrOf (issSeed f))
    h <- letM (hashElemWithSalt @e @k seed key)
    let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
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
      glNow <- peek (getPtrOf (issGrowthLeft f))
      poke (getPtrOf (issGrowthLeft f)) (glNow - 1)
    writeRef insertedRef true
  readRef insertedRef

popBack ::
  forall k ks m e.
  (IndexSetCtx m e k, Elem e k ~ e ks, JanusTyped e ks) =>
  e ks ->
  IndexSet e k ->
  m (e Bool, e ks)
popBack defK is = do
  okRef <- newRef false
  kRef <- newRef defK
  withPopBack
    is
    (pure ())
    ( \kOut -> do
        writeRef okRef true
        writeRef kRef kOut
    )
  (,) <$> readRef okRef <*> readRef kRef

withPopBack ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withPopBack (IndexSet hdr) onEmpty onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (issDenseLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let lastIdx = curLen - 1
        kb <- peekBufStruct (getPtrOf (issKeys f))
        lastK <- readBuf kb lastIdx
        ctrl <- peek (getPtrOf (issCtrl f))
        sCap <- peek (getPtrOf (issSparseCap f))
        sparse <- peek (getPtrOf (issSparse f))
        seed <- peek (getPtrOf (issSeed f))
        lastSlot <- findSparseSlotForDense @k ctrl sCap sparse seed lastK lastIdx
        deleteSparseSlot ctrl sCap (getPtrOf (issGrowthLeft f)) lastSlot
        poke (getPtrOf (issDenseLen f)) lastIdx
        onHit lastK
    )

swapRemove ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Elem e k ->
  m (e Bool)
swapRemove is@(IndexSet hdr) key = do
  (found, targetSlot, denseIdx) <- findSlotAndDense is key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (issDenseLen f))
    let lastIdx = curLen - 1
    ctrl <- peek (getPtrOf (issCtrl f))
    sCap <- peek (getPtrOf (issSparseCap f))
    sparse <- peek (getPtrOf (issSparse f))
    deleteSparseSlot ctrl sCap (getPtrOf (issGrowthLeft f)) targetSlot
    whenM_ (denseIdx `lt` lastIdx) $ do
      kb <- peekBufStruct (getPtrOf (issKeys f))
      seed <- peek (getPtrOf (issSeed f))
      movedK <- readBuf kb lastIdx
      movedSlot <- findSparseSlotForDense @k ctrl sCap sparse seed movedK lastIdx
      pokeElemOff sparse denseIdx movedSlot
      writeBuf kb denseIdx movedK
    poke (getPtrOf (issDenseLen f)) lastIdx
  pure found

shiftRemove ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Elem e k ->
  m (e Bool)
shiftRemove is@(IndexSet hdr) key = do
  (found, targetSlot, denseIdx) <- findSlotAndDense is key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (issDenseLen f))
    let lastIdx = curLen - 1
        tailCnt = lastIdx - denseIdx
    ctrl <- peek (getPtrOf (issCtrl f))
    sCap <- peek (getPtrOf (issSparseCap f))
    sparse <- peek (getPtrOf (issSparse f))
    deleteSparseSlot ctrl sCap (getPtrOf (issGrowthLeft f)) targetSlot
    whenM_ (tailCnt `gt` 0) $ do
      kb <- peekBufStruct (getPtrOf (issKeys f))
      moveBuf kb denseIdx (denseIdx + 1) tailCnt
      let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
          numGroups = sCap `rshift` 3
      runDrain
        ( drainForEach $ \sl -> do
            dIdx <- peekElemOff sparse sl
            whenM_ (dIdx `gt` denseIdx) $
              pokeElemOff sparse (dIdx - 1) sl
        )
        (swarOccupiedSlotsS ctrlWords numGroups)
    poke (getPtrOf (issDenseLen f)) lastIdx
  pure found

-- | Sort dense keys ascending in-place via 'sortBufBy' and rebuild the SwissTable
-- sparse index in-place with ZERO heap allocations.
sort ::
  forall k m e.
  (IndexSetCtx m e k, OrdKey e k) =>
  IndexSet e k ->
  m ()
sort (IndexSet hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (issDenseLen f))
  whenM_ (curLen `gt` 1) $ do
    kb <- peekBufStruct (getPtrOf (issKeys f))
    sortBufBy (ltKey @e @k) kb curLen
    rebuildSparseInPlace @k hdr

--------------------------------------------------------------------------------
-- Unit-Stride Dense Streams, Set-Algebra Operations, & Drains
--------------------------------------------------------------------------------

stream ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Stream m (Elem e k)
stream (IndexSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (issDenseLen f)) <*> peekBufStruct (getPtrOf (issKeys f)))
        (\(n, kb) -> streamM (rangeS 0 n) (readBuf kb))

reverseStream ::
  forall k m e.
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  Stream m (Elem e k)
reverseStream (IndexSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (issDenseLen f)) <*> peekBufStruct (getPtrOf (issKeys f)))
        (\(n, kb) -> streamM (reverseRangeS 0 n) (readBuf kb))

-- | Layout-polymorphic read-only unit-stride SIMD traversal over contiguous insertion-ordered keys @[0, size)@.
traverseSPMD_ ::
  forall w k m e.
  (IndexSetCtx m e k, LayoutUnitSPMD w m e k) =>
  Proxy w ->
  IndexSet e k ->
  (SPMD w e Int64 -> Elem (SPMD w e) k -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (IndexSet hdr) body = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (issDenseLen f))
  kb <- peekBufStruct (getPtrOf (issKeys f))
  traverseBufSPMD_ pw True kb n body

-- | Layout-polymorphic masked-safe unit-stride SIMD fold over contiguous insertion-ordered keys @[0, size)@.
foldSPMD ::
  forall w k s m e.
  (IndexSetCtx m e k, LayoutUnitSPMD w m e k, SPMDTyped w e s) =>
  Proxy w ->
  IndexSet e k ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) k -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw (IndexSet hdr) s0 step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (issDenseLen f))
  kb <- peekBufStruct (getPtrOf (issKeys f))
  foldBufSPMD pw True kb n s0 step

-- | Lane-parallel SIMD batch membership query probing @w@ keys simultaneously.
memberBatchSPMD ::
  forall w k m e.
  (IndexSetCtx m e k, SPMDGang w m e, HashKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  Proxy w ->
  IndexSet e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
memberBatchSPMD _ (IndexSet hdr) qKeys = do
  let f = projectPtrs hdr
  sCap <- liftSPMD (peek (getPtrOf (issSparseCap f)))
  foundRef <- newRef false
  whenM_ (uniform sCap `gt` 0) $ do
    ctrl <- liftSPMD (peek (getPtrOf (issCtrl f)))
    sparse <- liftSPMD (peek (getPtrOf (issSparse f)))
    kb <- liftSPMD (peekBufStruct (getPtrOf (issKeys f)))
    seed <- liftSPMD (peek (getPtrOf (issSeed f)))
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
            whenM_ hit (writeRef foundRef true)
            pure (ifThenElse hit 0 (curM `iand` (curM - 1)), ())
        fnd <- readRef foundRef
        let actNext = lnot fnd `land` (empMask `eq` 0)
            stNext = st + 1
            gNext = (g + stNext) `iand` uniform groupMask
        pure ((gNext, stNext, actNext), ())
  readRef foundRef

isSubsetOf ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  IndexSet e k ->
  m (e Bool)
isSubsetOf sub sup = do
  nSub <- size sub
  nSup <- size sup
  resRef <- newRef false
  whenM_ (nSub `le` nSup) $ do
    ok <- runDrain (drainAll id) (streamM (stream sub) (member sup))
    writeRef resRef ok
  readRef resRef

-- | Yield @a ∩ b@ preserving @a@'s exact insertion order.
intersectionS ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  IndexSet e k ->
  Stream m (Elem e k)
intersectionS a b =
  filterMapS
    (\x -> (\inB -> (inB, x)) <$> member b x)
    (stream a)

-- | Yield @a \ b@ preserving @a@'s exact insertion order.
differenceS ::
  (IndexSetCtx m e k) =>
  IndexSet e k ->
  IndexSet e k ->
  Stream m (Elem e k)
differenceS a b =
  filterMapS
    (\x -> (\inB -> (lnot inB, x)) <$> member b x)
    (stream a)

unionInto ::
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  IndexSet e k ->
  m ()
unionInto alloc dst src =
  runDrain (drainAppendIndexSet alloc dst) (stream src)

drainToIndexSet ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (IndexSet e k)
drainToIndexSet alloc = Drain $ \sIn -> do
  is <- new alloc
  runDrain (drainAppendIndexSet alloc is) sIn
  pure is

fromStream ::
  forall k m e.
  (IndexSetCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (IndexSet e k)
fromStream alloc = runDrain (drainToIndexSet alloc)

drainAppendIndexSet ::
  (IndexSetCtx m e k) =>
  Alloc e ->
  IndexSet e k ->
  Drain m (Elem e k) ()
drainAppendIndexSet alloc is = drainForEach (\x -> insert alloc is x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type IndexSetABI k = (Typeable k, JanusStruct (Buf k))

type instance JanusCHost (IndexSet (JanusCT _) k) = IndexSet Identity k

deriving via
  ViaPtrHandle t (CStruct (IndexSetSpine k)) (IndexSet Identity k)
  instance
    (IndexSetABI k) => JanusCArg t (IndexSet (JanusCT t) k)

deriving via
  ViaPtrHandle t (CStruct (IndexSetSpine k)) (IndexSet Identity k)
  instance
    (IndexSetABI k) => JanusCValRet t (IndexSet (JanusCT t) k)
