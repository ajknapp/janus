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

module Janus.Data.Pool
  ( -- * Address-Stable Segmented Object Pool (64-Slot Bitmasked Segments)
    PoolSegSlots,
    PoolHandle (..),
    packPoolHandle,
    unpackPoolHandle,
    PoolSeg (..),
    PoolSpine (..),
    Pool (..),
    PoolCtx,

    -- * Lifecycle
    new,
    free,
    clear,

    -- * Queries
    size,
    capacity,
    isEmpty,
    isLive,

    -- * O(1) Acquire, Release, Read, Write, & Stable Element Pointers
    acquire,
    acquireSlot,
    release,
    readPool,
    writePool,
    modifyPool,
    handlePtrScalar,
    handlePtrAoS,

    -- * 64-Bit Occupancy-Bitmask Word-Skipping Streams & Drains
    stream,
    handlesS,
    toStream,
    ptrStreamScalar,
    drainToPool,
    fromStream,
    drainAppendPool,

    -- * First-Class Occupancy-Masked SPMD / SIMD Kernels
    foreachOccupiedSPMD,
    foldOccupiedSPMD,
  )
where

import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFirst, drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream (..), filterMapS, rangeS)
import Janus.Data.DynVec (DynVec)
import qualified Janus.Data.DynVec as DV
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDM,
    SPMDTyped,
    uniform,
    withSPMD,
  )

type PoolSegSlots = 64

segSlots :: Int64
segSlots = 64

-- | Handle addressing @(segmentIdx, slotInSegment)@ in an address-stable 'Pool'.
data PoolHandle f = PoolHandle
  { phSeg :: !(f Word32),
    phSlot :: !(f Word32)
  }
  deriving stock (Generic)

deriving stock instance Eq (PoolHandle Identity)

deriving stock instance Ord (PoolHandle Identity)

deriving stock instance Show (PoolHandle Identity)

instance JanusStruct PoolHandle

instance JanusHKD PoolHandle

packPoolHandle ::
  (ExpBits e Word64, ExpIntegralCast e Word32 Word64, Num (e Word64)) =>
  PoolHandle e ->
  e Word64
packPoolHandle (PoolHandle seg slot) =
  (toIntegral seg `lshift` 32) `ior` toIntegral slot

unpackPoolHandle ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Word32, Num (e Word64)) =>
  e Word64 ->
  PoolHandle e
unpackPoolHandle w =
  PoolHandle
    { phSeg = toIntegral (w `rshift` 32),
      phSlot = toIntegral (w `iand` 0xFFFFFFFF)
    }

-- | 64-slot pool segment descriptor:
-- * @psBuf@: 64-element fixed chunk that NEVER moves or reallocates.
-- * @psOcc@: 64-bit occupancy bitmask (bit @i == 1@ iff slot @i@ is live).
-- * @psBumped@: lazy high-water mark @[0 .. 64]@.
-- * @psNextFree@: embedded free-list links (@(nextSeg << 6) | nextSlot@, or @-1@).
data PoolSeg a f = PoolSeg
  { psBuf :: !(f (CStruct (Buf a))),
    psOcc :: !(f Word64),
    psBumped :: !(f Int64),
    psNextFree :: !(f (FixedArray PoolSegSlots Int64))
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (PoolSeg a)

data PoolSpine a f = PoolSpine
  { plsSegs :: !(f (Ptr (CStruct (PoolSeg a)))),
    plsNumSegs :: !(f Int64),
    plsDirCap :: !(f Int64),
    plsCurSeg :: !(f Int64),
    plsFreeHead :: !(f Int64),
    plsLiveCount :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (PoolSpine a)

newtype Pool (e :: K.Type -> K.Type) a = Pool
  { unPool :: e (Ptr (CStruct (PoolSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (Pool Identity a)

deriving stock instance Show (Pool Identity a)

type PoolCtx m e a = LayoutCtx m e a

-- | Branchless 6-step count-trailing-zeros for a non-zero @Word64@.
ctzWord64 ::
  forall e.
  (ExpBits e Word64, ExpCond e, KnownJanusType e Int64, Num (e Word64), Num (e Int64)) =>
  e Word64 ->
  e Int64
ctzWord64 m =
  let lsb = m `iand` (0 - m)
      b5 = ifThenElse ((lsb `iand` 0x00000000FFFFFFFF) `eq` 0) 32 0
      b4 = ifThenElse ((lsb `iand` 0x0000FFFF0000FFFF) `eq` 0) 16 0
      b3 = ifThenElse ((lsb `iand` 0x00FF00FF00FF00FF) `eq` 0) 8 0
      b2 = ifThenElse ((lsb `iand` 0x0F0F0F0F0F0F0F0F) `eq` 0) 4 0
      b1 = ifThenElse ((lsb `iand` 0x3333333333333333) `eq` 0) 2 0
      b0 = ifThenElse ((lsb `iand` 0x5555555555555555) `eq` 0) 1 0
   in b5 + b4 + b3 + b2 + b1 + b0

new ::
  forall a m e.
  (PoolCtx m e a) =>
  Alloc e ->
  m (Pool e a)
new alloc = do
  hdr <- allocStruct @(CStruct (PoolSpine a)) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (plsSegs f)) nullPtr
  poke (getPtrOf (plsNumSegs f)) 0
  poke (getPtrOf (plsDirCap f)) 0
  poke (getPtrOf (plsCurSeg f)) 0
  poke (getPtrOf (plsFreeHead f)) (-1)
  poke (getPtrOf (plsLiveCount f)) 0
  pure (Pool hdr)

free ::
  forall a m e.
  (PoolCtx m e a) =>
  Alloc e ->
  Pool e a ->
  m ()
free alloc (Pool hdr) = do
  let f = projectPtrs hdr
  segs <- peek (getPtrOf (plsSegs f))
  numS <- peek (getPtrOf (plsNumSegs f))
  dirCap <- peek (getPtrOf (plsDirCap f))
  runDrain
    ( drainForEach $ \si -> do
        let sf = projectPtrs (ptrIndex segs si)
        sb <- peekBufStruct (getPtrOf (psBuf sf))
        freeBuf alloc sb (fromIntegral segSlots)
    )
    (rangeS 0 numS)
  whenM_ (dirCap `gt` 0) $
    freeTypedArray @(CStruct (PoolSeg a)) alloc segs dirCap
  freeStruct alloc hdr

-- | O(numSegments) warm reset: clears occupancy bitmasks, bump cursors, and free-list
-- while keeping all allocated 64-slot segments warm for reuse.
clear ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  m ()
clear (Pool hdr) = do
  let f = projectPtrs hdr
  segs <- peek (getPtrOf (plsSegs f))
  numS <- peek (getPtrOf (plsNumSegs f))
  runDrain
    ( drainForEach $ \si -> do
        let sf = projectPtrs (ptrIndex segs si)
        poke (getPtrOf (psOcc sf)) 0
        poke (getPtrOf (psBumped sf)) 0
    )
    (rangeS 0 numS)
  poke (getPtrOf (plsCurSeg f)) 0
  poke (getPtrOf (plsFreeHead f)) (-1)
  poke (getPtrOf (plsLiveCount f)) 0

size :: (PoolCtx m e a) => Pool e a -> m (e Int64)
size (Pool hdr) = peek (getPtrOf (plsLiveCount (projectPtrs hdr)))

capacity :: (PoolCtx m e a) => Pool e a -> m (e Int64)
capacity (Pool hdr) = do
  numS <- peek (getPtrOf (plsNumSegs (projectPtrs hdr)))
  pure (numS * fromIntegral segSlots)

isEmpty :: (PoolCtx m e a) => Pool e a -> m (e Bool)
isEmpty p = (`eq` 0) <$> size p

isLive ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  PoolHandle e ->
  m (e Bool)
isLive (Pool hdr) (PoolHandle segW slotW) = do
  let f = projectPtrs hdr
      seg = toIntegral segW :: e Int64
      slot = toIntegral slotW :: e Int64
  numS <- peek (getPtrOf (plsNumSegs f))
  okRef <- newRef false
  whenM_ ((seg `lt` numS) `land` (slot `lt` fromIntegral segSlots)) $ do
    segs <- peek (getPtrOf (plsSegs f))
    occ <- peek (getPtrOf (psOcc (projectPtrs (ptrIndex segs seg))))
    let bit = (occ `rshift` toIntegral slot) `iand` 1
    writeRef okRef (bit `eq` 1)
  readRef okRef

-- | Acquire an uninitialized live slot in O(1), returning its address-stable 'PoolHandle'.
acquireSlot ::
  forall a m e.
  (PoolCtx m e a) =>
  Alloc e ->
  Pool e a ->
  m (PoolHandle e)
acquireSlot alloc (Pool hdr) = do
  let f = projectPtrs hdr
  fh <- peek (getPtrOf (plsFreeHead f))
  outSegRef <- newRef (0 :: e Int64)
  outSlotRef <- newRef (0 :: e Int64)

  ifThenElseM_
    (fh `ge` 0)
    -- 1. Pop recycled slot from free-list
    ( do
        let seg = fh `rshift` 6
            slot = fh `iand` 63
        segs <- peek (getPtrOf (plsSegs f))
        let sf = projectPtrs (ptrIndex segs seg)
            nfArr = fixedArrayElemPtr (getPtrOf (psNextFree sf))
        nxtFh <- peekElemOff nfArr slot
        occ <- peek (getPtrOf (psOcc sf))
        poke (getPtrOf (psOcc sf)) (occ `ior` ((1 :: e Word64) `lshift` toIntegral slot))
        poke (getPtrOf (plsFreeHead f)) nxtFh
        writeRef outSegRef seg
        writeRef outSlotRef slot
    )
    -- 2. Lazy bump in warm segment, or allocate a new 64-slot segment
    ( do
        numS <- peek (getPtrOf (plsNumSegs f))
        curS0 <- peek (getPtrOf (plsCurSeg f))
        segs0 <- peek (getPtrOf (plsSegs f))
        let warmSegS =
              filterMapS
                ( \cs -> do
                    let sf = projectPtrs (ptrIndex segs0 cs)
                    bumped <- peek (getPtrOf (psBumped sf))
                    pure (bumped `lt` fromIntegral segSlots, (cs, bumped))
                )
                (rangeS curS0 numS)
        (placedAfter, (foundCs, foundBumped)) <- runDrain (drainFirst (0, 0)) warmSegS
        ifThenElseM_
          placedAfter
          ( do
              let sf = projectPtrs (ptrIndex segs0 foundCs)
              occ <- peek (getPtrOf (psOcc sf))
              poke (getPtrOf (psBumped sf)) (foundBumped + 1)
              poke (getPtrOf (psOcc sf)) (occ `ior` ((1 :: e Word64) `lshift` toIntegral foundBumped))
              poke (getPtrOf (plsCurSeg f)) foundCs
              writeRef outSegRef foundCs
              writeRef outSlotRef foundBumped
          )
          ( do
              dirCap <- peek (getPtrOf (plsDirCap f))
              whenM_ (numS `ge` dirCap) $ do
                let newDirCap = ifThenElse (dirCap `lt` 8) 8 (dirCap * 2)
                oldDir <- peek (getPtrOf (plsSegs f))
                newDir <- reallocTypedArray @(CStruct (PoolSeg a)) alloc oldDir dirCap newDirCap
                poke (getPtrOf (plsSegs f)) newDir
                poke (getPtrOf (plsDirCap f)) newDirCap
              segs <- peek (getPtrOf (plsSegs f))
              newSegBuf <- allocBuf @a alloc (fromIntegral segSlots)
              let sf = projectPtrs (ptrIndex segs numS)
              pokeBufStruct (getPtrOf (psBuf sf)) newSegBuf
              poke (getPtrOf (psOcc sf)) 1 -- slot 0 occupied
              poke (getPtrOf (psBumped sf)) 1
              poke (getPtrOf (plsNumSegs f)) (numS + 1)
              poke (getPtrOf (plsCurSeg f)) numS
              writeRef outSegRef numS
              writeRef outSlotRef 0
          )
    )

  live <- peek (getPtrOf (plsLiveCount f))
  poke (getPtrOf (plsLiveCount f)) (live + 1)
  segOut <- readRef outSegRef
  slotOut <- readRef outSlotRef
  pure (PoolHandle (toIntegral segOut) (toIntegral slotOut))

-- | Acquire a slot and initialize it with @val@.
acquire ::
  forall a m e.
  (PoolCtx m e a) =>
  Alloc e ->
  Pool e a ->
  Elem e a ->
  m (PoolHandle e)
acquire alloc p val = do
  h <- acquireSlot alloc p
  writePool p h val
  pure h

-- | O(1) release of a live handle back to the pool's free-list. Returns @true@ iff @h@ was live.
release ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  PoolHandle e ->
  m (e Bool)
release p@(Pool hdr) h@(PoolHandle segW slotW) = do
  liveOk <- isLive p h
  whenM_ liveOk $ do
    let f = projectPtrs hdr
        seg = toIntegral segW :: e Int64
        slot = toIntegral slotW :: e Int64
        packed = (seg `lshift` 6) `ior` slot
    segs <- peek (getPtrOf (plsSegs f))
    let sf = projectPtrs (ptrIndex segs seg)
        nfArr = fixedArrayElemPtr (getPtrOf (psNextFree sf))
    occ <- peek (getPtrOf (psOcc sf))
    fh <- peek (getPtrOf (plsFreeHead f))
    live <- peek (getPtrOf (plsLiveCount f))
    poke (getPtrOf (psOcc sf)) (occ `iand` inot ((1 :: e Word64) `lshift` toIntegral slot))
    pokeElemOff nfArr fh slot
    poke (getPtrOf (plsFreeHead f)) packed
    poke (getPtrOf (plsLiveCount f)) (live - 1)
  pure liveOk

readPool ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  PoolHandle e ->
  m (Elem e a)
readPool (Pool hdr) (PoolHandle segW slotW) = do
  segs <- peek (getPtrOf (plsSegs (projectPtrs hdr)))
  sb <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs (toIntegral segW)))))
  readBuf sb (toIntegral slotW)

writePool ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  PoolHandle e ->
  Elem e a ->
  m ()
writePool (Pool hdr) (PoolHandle segW slotW) val = do
  segs <- peek (getPtrOf (plsSegs (projectPtrs hdr)))
  sb <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs (toIntegral segW)))))
  writeBuf sb (toIntegral slotW) val

modifyPool ::
  (PoolCtx m e a) =>
  Pool e a ->
  PoolHandle e ->
  (Elem e a -> Elem e a) ->
  m ()
modifyPool p h fn = readPool p h >>= writePool p h . fn

-- | Obtain an address-stable raw pointer to @h@ (never invalidated by subsequent 'acquire' or 'release' of other slots).
handlePtrScalar ::
  forall a m e.
  (PoolCtx m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  Pool e a ->
  PoolHandle e ->
  m (e (Ptr a))
handlePtrScalar (Pool hdr) (PoolHandle segW slotW) = do
  segs <- peek (getPtrOf (plsSegs (projectPtrs hdr)))
  ScalarBuf pSeg <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs (toIntegral segW)))))
  pure (ptrIndex pSeg (toIntegral slotW))

handlePtrAoS ::
  forall s m e.
  (PoolCtx m e (AoS s), ExpPtr e (CStruct s)) =>
  Pool e (AoS s) ->
  PoolHandle e ->
  m (e (Ptr (CStruct s)))
handlePtrAoS (Pool hdr) (PoolHandle segW slotW) = do
  segs <- peek (getPtrOf (plsSegs (projectPtrs hdr)))
  AoSBuf pSeg <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs (toIntegral segW)))))
  pure (ptrIndex pSeg (toIntegral slotW))

--------------------------------------------------------------------------------
-- 64-Bit Bitmask Word-Skipping Streams & Drains
--------------------------------------------------------------------------------

toStream ::
  forall a m e.
  (PoolCtx m e a) =>
  Pool e a ->
  Stream m (PoolHandle e, Elem e a)
toStream (Pool hdr) = Stream $ Codensity $ \kOuter -> do
  let f = projectPtrs hdr
  numS <- peek (getPtrOf (plsNumSegs f))
  segs <- peek (getPtrOf (plsSegs f))
  segRef <- newRef (0 :: e Int64)
  maskRef <- newRef (0 :: e Word64)
  kOuter $ ContT $ \kStep -> do
    curMask <- readRef maskRef
    ifThenElseM_
      (curMask `ne` 0)
      ( do
          sNext <- readRef segRef
          let seg = sNext - 1
              slot = ctzWord64 curMask
          writeRef maskRef (curMask `iand` (curMask - 1))
          sb <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs seg))))
          val <- readBuf sb slot
          kStep (Yield (PoolHandle (toIntegral seg) (toIntegral slot), val))
      )
      ( do
          seg <- readRef segRef
          ifThenElseM_
            (seg `ge` numS)
            (kStep Stop)
            ( do
                occ <- peek (getPtrOf (psOcc (projectPtrs (ptrIndex segs seg))))
                writeRef segRef (seg + 1)
                writeRef maskRef occ
                kStep Skip
            )
      )

stream :: (PoolCtx m e a) => Pool e a -> Stream m (Elem e a)
stream p = snd <$> toStream p

handlesS :: (PoolCtx m e a) => Pool e a -> Stream m (PoolHandle e)
handlesS p = fst <$> toStream p

ptrStreamScalar ::
  forall a m e.
  (PoolCtx m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  Pool e a ->
  Stream m (e (Ptr a))
ptrStreamScalar (Pool hdr) = Stream $ Codensity $ \kOuter -> do
  let f = projectPtrs hdr
  numS <- peek (getPtrOf (plsNumSegs f))
  segs <- peek (getPtrOf (plsSegs f))
  segRef <- newRef (0 :: e Int64)
  maskRef <- newRef (0 :: e Word64)
  kOuter $ ContT $ \kStep -> do
    curMask <- readRef maskRef
    ifThenElseM_
      (curMask `ne` 0)
      ( do
          sNext <- readRef segRef
          let seg = sNext - 1
              slot = ctzWord64 curMask
          writeRef maskRef (curMask `iand` (curMask - 1))
          ScalarBuf pSeg <- peekBufStruct (getPtrOf (psBuf (projectPtrs (ptrIndex segs seg))))
          kStep (Yield (ptrIndex pSeg slot))
      )
      ( do
          seg <- readRef segRef
          ifThenElseM_
            (seg `ge` numS)
            (kStep Stop)
            ( do
                occ <- peek (getPtrOf (psOcc (projectPtrs (ptrIndex segs seg))))
                writeRef segRef (seg + 1)
                writeRef maskRef occ
                kStep Skip
            )
      )

drainToPool ::
  forall a m e.
  (PoolCtx m e a) =>
  Alloc e ->
  Drain m (Elem e a) (Pool e a)
drainToPool alloc = Drain $ \s -> do
  p <- new alloc
  runDrain (drainForEach (\x -> acquire alloc p x >> pure ())) s
  pure p

fromStream ::
  (PoolCtx m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (Pool e a)
fromStream alloc = runDrain (drainToPool alloc)

drainAppendPool ::
  forall a m e.
  (PoolCtx m e a, DV.DynVecCtx m e (AoS PoolHandle)) =>
  Alloc e ->
  Pool e a ->
  Drain m (Elem e a) (DynVec e (AoS PoolHandle))
drainAppendPool alloc p = Drain $ \s -> do
  hv <- DV.new @(AoS PoolHandle) alloc
  runDrain
    ( drainForEach $ \x -> do
        h <- acquire alloc p x
        DV.push alloc hv h
    )
    s
  pure hv

--------------------------------------------------------------------------------
-- First-Class Occupancy-Masked SPMD / SIMD Kernels
--------------------------------------------------------------------------------

-- | Strip-mine all occupied slots across all 64-slot segments in-place at SIMD gang width @w@,
-- skipping completely empty 64-slot segments in O(1) and empty @w@-lane gangs via @mmovemask == 0@.
-- Passes the global slot index @(segIdx << 6) + localIdx@ to @step@.
foreachOccupiedSPMD ::
  forall w a m e.
  (PoolCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  Pool e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachOccupiedSPMD pw (Pool hdr) step = do
  let f = projectPtrs hdr
  numS <- peek (getPtrOf (plsNumSegs f))
  segs <- peek (getPtrOf (plsSegs f))
  runDrain
    ( drainForEach $ \segIdx -> do
        let sf = projectPtrs (ptrIndex segs segIdx)
        occMask <- peek (getPtrOf (psOcc sf))
        whenM_ (occMask `ne` 0) $ do
          segBuf <- peekBufStruct (getPtrOf (psBuf sf))
          let isOcc localIdx =
                pure (((uniform occMask `rshift` toIntegral (localIdx `iand` 63)) `iand` 1) `ne` 0)
              baseIdx = uniform (segIdx `lshift` 6)
          foreachMaskedBufSPMD
            pw
            True
            segBuf
            (fromIntegral segSlots)
            isOcc
            (\localIdx val -> step (baseIdx + localIdx) val)
    )
    (rangeS 0 numS)

-- | Masked-safe SIMD fold over all occupied slots across all 64-slot segments at gang width @w@,
-- passing the global slot index @(segIdx << 6) + localIdx@ to @step@.
foldOccupiedSPMD ::
  forall w a s m e.
  (PoolCtx m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  Pool e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldOccupiedSPMD pw (Pool hdr) s0 step = do
  let f = projectPtrs hdr
  numS <- peek (getPtrOf (plsNumSegs f))
  segs <- peek (getPtrOf (plsSegs f))
  accRef <- withSPMD pw (newRef s0)
  runDrain
    ( drainForEach $ \segIdx -> do
        let sf = projectPtrs (ptrIndex segs segIdx)
        occMask <- peek (getPtrOf (psOcc sf))
        whenM_ (occMask `ne` 0) $ do
          segBuf <- peekBufStruct (getPtrOf (psBuf sf))
          curAcc <- withSPMD pw (readRef accRef)
          let isOcc localIdx =
                pure (((uniform occMask `rshift` toIntegral (localIdx `iand` 63)) `iand` 1) `ne` 0)
              baseIdx = uniform (segIdx `lshift` 6)
          nxtAcc <-
            foldMaskedBufSPMD
              pw
              True
              segBuf
              (fromIntegral segSlots)
              isOcc
              curAcc
              (\acc localIdx val -> step acc (baseIdx + localIdx) val)
          withSPMD pw (writeRef accRef nxtAcc)
    )
    (rangeS 0 numS)
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type PoolABI a = (Typeable a, JanusStruct (Buf a))

type instance JanusCHost (Pool (JanusCT _) a) = Pool Identity a

deriving via
  ViaPtrHandle t (CStruct (PoolSpine a)) (Pool Identity a)
  instance
    (PoolABI a) => JanusCArg t (Pool (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (PoolSpine a)) (Pool Identity a)
  instance
    (PoolABI a) => JanusCValRet t (Pool (JanusCT t) a)
