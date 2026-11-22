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

module Janus.Data.SlotMap
  ( -- * Generational Slot Handle & Internal Entry Struct
    SlotKey (..),
    packSlotKey,
    unpackSlotKey,
    SlotEntry (..),
    SlotMapSpine (..),
    SlotMap (..),
    SlotMapCtx,

    -- * Lifecycle
    new,
    withCapacity,
    free,

    -- * Queries & O(N_live) Generation-Bumping Clear
    size,
    capacity,
    isEmpty,
    clear,
    reserve,

    -- * O(1) Generational Insert, Lookup, Update, & Swap-Delete
    insert,
    containsKey,
    lookup,
    withLookup,
    modifyKey,
    delete,

    -- * Dense Packed Streams, Drains, & SPMD Kernels
    valuesS,
    keysS,
    toStream,
    containsKeysSPMD,
    foreachValuesSPMD,
    foldValuesSPMD,
    foreachScalarSPMD,
    foreachSoASlotMapSPMD,
    drainToSlotMap,
    fromStream,
    drainAppendSlotMap,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import GHC.TypeNats (natVal)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainFoldM, drainForEach, runDrain)
import Janus.Control.Stream (Stream, iterateS, rangeS, streamM, withStreamSetup)
import Janus.Data.DynVec (DynVec)
import qualified Janus.Data.DynVec as DV
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
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
    uniform,
    uniformAlignedPtr,
    withSPMD,
    withSPMDMasked,
  )
import Janus.Typed (JanusTyped)
import Prelude hiding (lookup)

freeListEnd :: Word32
freeListEnd = 0xFFFFFFFF

-- | 64-bit generational key: @skGen@ is ALWAYS odd for valid issued keys.
data SlotKey f = SlotKey
  { skIndex :: !(f Word32),
    skGen :: !(f Word32)
  }
  deriving stock (Generic)

deriving stock instance Eq (SlotKey Identity)

deriving stock instance Ord (SlotKey Identity)

deriving stock instance Show (SlotKey Identity)

instance JanusStruct SlotKey

instance JanusHKD SlotKey

packSlotKey ::
  (ExpBits e Word64, ExpIntegralCast e Word32 Word64, Num (e Word64)) =>
  SlotKey e ->
  e Word64
packSlotKey (SlotKey idx gen) =
  (toIntegral gen `lshift` 32) `ior` toIntegral idx

unpackSlotKey ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Word32, Num (e Word64)) =>
  e Word64 ->
  SlotKey e
unpackSlotKey packed =
  SlotKey
    { skIndex = toIntegral (packed `iand` 0xFFFFFFFF),
      skGen = toIntegral (packed `rshift` 32)
    }

-- | Sparse indirection slot:
-- * When @seGen@ is odd (alive): @seDenseOrNext@ is the index into @smsDenseVals@.
-- * When @seGen@ is even (free): @seDenseOrNext@ is the next free slot index (or @0xFFFFFFFF@).
data SlotEntry f = SlotEntry
  { seDenseOrNext :: !(f Word32),
    seGen :: !(f Word32)
  }
  deriving stock (Generic)

deriving stock instance Eq (SlotEntry Identity)

deriving stock instance Show (SlotEntry Identity)

instance JanusStruct SlotEntry

data SlotMapSpine a f = SlotMapSpine
  { smsSlots :: !(f (Ptr (CStruct SlotEntry))),
    smsSlotCap :: !(f Int64),
    smsSlotBumped :: !(f Int64),
    smsFreeHead :: !(f Word32),
    smsDenseVals :: !(f (CStruct (Buf a))),
    smsDenseToSlot :: !(f (Ptr Word32)),
    smsDenseLen :: !(f Int64),
    smsDenseCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (SlotMapSpine a)

newtype SlotMap (e :: K.Type -> K.Type) a = SlotMap
  { unSlotMap :: e (Ptr (CStruct (SlotMapSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (SlotMap Identity a)

deriving stock instance Show (SlotMap Identity a)

type SlotMapCtx m e a =
  ( LayoutCtx m e a,
    Typeable a,
    ExpPtr e Word32,
    JanusTyped e (Ptr Word32),
    CmdStorable m e (Ptr Word32),
    ExpPtr e (CStruct SlotEntry),
    ExpSized e (CStruct SlotEntry),
    JanusTyped e (CStruct SlotEntry),
    JanusTyped e (Ptr (CStruct SlotEntry)),
    CmdStorable m e (Ptr (CStruct SlotEntry)),
    ExpSized e (CStruct (SlotMapSpine a)),
    JanusTyped e (CStruct (SlotMapSpine a))
  )

new ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  m (SlotMap e a)
new alloc = do
  hdr <- allocStruct @(CStruct (SlotMapSpine a)) alloc
  let f = projectPtrs hdr
  b0 <- allocBuf @a alloc 0
  poke (getPtrOf (smsSlots f)) nullPtr
  poke (getPtrOf (smsSlotCap f)) 0
  poke (getPtrOf (smsSlotBumped f)) 0
  poke (getPtrOf (smsFreeHead f)) (fromIntegral freeListEnd)
  pokeBufStruct (getPtrOf (smsDenseVals f)) b0
  poke (getPtrOf (smsDenseToSlot f)) nullPtr
  poke (getPtrOf (smsDenseLen f)) 0
  poke (getPtrOf (smsDenseCap f)) 0
  pure (SlotMap hdr)

withCapacity ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (SlotMap e a)
withCapacity alloc initCap = do
  sm <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc sm initCap
  pure sm

free ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  SlotMap e a ->
  m ()
free alloc (SlotMap hdr) = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (smsSlotCap f))
  whenM_ (sCap `gt` 0) $ do
    slots <- peek (getPtrOf (smsSlots f))
    freeTypedArray @(CStruct SlotEntry) alloc slots sCap
  dCap <- peek (getPtrOf (smsDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    dv <- peekBufStruct (getPtrOf (smsDenseVals f))
    d2s <- peek (getPtrOf (smsDenseToSlot f))
    freeBuf alloc dv dCap
    freeTypedArray @Word32 alloc d2s dCap
  freeStruct alloc hdr

size :: (SlotMapCtx m e a) => SlotMap e a -> m (e Int64)
size (SlotMap hdr) = peek (getPtrOf (smsDenseLen (projectPtrs hdr)))

capacity :: (SlotMapCtx m e a) => SlotMap e a -> m (e Int64)
capacity (SlotMap hdr) = peek (getPtrOf (smsDenseCap (projectPtrs hdr)))

isEmpty :: (SlotMapCtx m e a) => SlotMap e a -> m (e Bool)
isEmpty sm = (`eq` 0) <$> size sm

-- | Invalidate all currently live keys by bumping their slot generations to even numbers
-- and returning their slots to the free-list, then setting @denseLen = 0@.
clear ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  m ()
clear (SlotMap hdr) = do
  let f = projectPtrs hdr
  dLen <- peek (getPtrOf (smsDenseLen f))
  whenM_ (dLen `gt` 0) $ do
    slots <- peek (getPtrOf (smsSlots f))
    d2s <- peek (getPtrOf (smsDenseToSlot f))
    fh0 <- peek (getPtrOf (smsFreeHead f))
    fhEnd <-
      runDrain
        ( drainFoldM fh0 $ \fh di -> do
            sIdxW32 <- peekElemOff d2s di
            let sPtr = ptrIndex slots (toIntegral sIdxW32)
                sf = projectPtrs sPtr
            curGen <- peek (getPtrOf (seGen sf))
            poke (getPtrOf (seGen sf)) (curGen + 1)
            poke (getPtrOf (seDenseOrNext sf)) fh
            pure sIdxW32
        )
        (rangeS 0 dLen)
    poke (getPtrOf (smsFreeHead f)) fhEnd
    poke (getPtrOf (smsDenseLen f)) 0

reserve ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  SlotMap e a ->
  e Int64 ->
  m ()
reserve alloc (SlotMap hdr) additional = do
  let f = projectPtrs hdr
  dLen <- peek (getPtrOf (smsDenseLen f))
  dCap <- peek (getPtrOf (smsDenseCap f))
  sCap <- peek (getPtrOf (smsSlotCap f))
  needed <- letM (dLen + additional)

  whenM_ (needed `gt` dCap) $ do
    let c0 = ifThenElse (dCap `lt` 8) 8 (dCap * 2)
    (_, newDCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldDV <- peekBufStruct (getPtrOf (smsDenseVals f))
    oldD2S <- peek (getPtrOf (smsDenseToSlot f))
    newDV <- reallocBuf alloc oldDV dCap newDCap dLen
    newD2S <- reallocTypedArray @Word32 alloc oldD2S dCap newDCap
    pokeBufStruct (getPtrOf (smsDenseVals f)) newDV
    poke (getPtrOf (smsDenseToSlot f)) newD2S
    poke (getPtrOf (smsDenseCap f)) newDCap

  -- Ensure sparse slot capacity is also at least needed (when free-list is empty we bump smsSlotBumped)
  whenM_ (needed `gt` sCap) $ do
    let sc0 = ifThenElse (sCap `lt` 8) 8 (sCap * 2)
    (_, newSCap) <- runDrain (drainFind sc0 (`ge` needed)) (iterateS sc0 (* 2))
    oldSlots <- peek (getPtrOf (smsSlots f))
    newSlots <- reallocTypedArray @(CStruct SlotEntry) alloc oldSlots sCap newSCap
    poke (getPtrOf (smsSlots f)) newSlots
    poke (getPtrOf (smsSlotCap f)) newSCap

-- | Insert @val@ in O(1), returning a fresh odd-generation 'SlotKey'.
insert ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  SlotMap e a ->
  Elem e a ->
  m (SlotKey e)
insert alloc sm@(SlotMap hdr) val = do
  let f = projectPtrs hdr
  dLen <- peek (getPtrOf (smsDenseLen f))
  dCap <- peek (getPtrOf (smsDenseCap f))
  whenM_ (dLen `eq` dCap) $
    reserve alloc sm 1

  fh <- peek (getPtrOf (smsFreeHead f))
  slotIdxRef <- newRef (0 :: e Word32)
  slotGenRef <- newRef (1 :: e Word32)

  ifThenElseM_
    (fh `ne` fromIntegral freeListEnd)
    ( do
        slots <- peek (getPtrOf (smsSlots f))
        let sf = projectPtrs (ptrIndex slots (toIntegral fh))
        nxtFree <- peek (getPtrOf (seDenseOrNext sf))
        evenGen <- peek (getPtrOf (seGen sf))
        let newOddGen = evenGen + 1
        poke (getPtrOf (seDenseOrNext sf)) (toIntegral dLen)
        poke (getPtrOf (seGen sf)) newOddGen
        poke (getPtrOf (smsFreeHead f)) nxtFree
        writeRef slotIdxRef fh
        writeRef slotGenRef newOddGen
    )
    ( do
        sBumped <- peek (getPtrOf (smsSlotBumped f))
        sCap <- peek (getPtrOf (smsSlotCap f))
        whenM_ (sBumped `ge` sCap) $ do
          let newSCap = ifThenElse (sCap `lt` 8) 8 (sCap * 2)
          oldSlots <- peek (getPtrOf (smsSlots f))
          newSlots <- reallocTypedArray @(CStruct SlotEntry) alloc oldSlots sCap newSCap
          poke (getPtrOf (smsSlots f)) newSlots
          poke (getPtrOf (smsSlotCap f)) newSCap
        slots <- peek (getPtrOf (smsSlots f))
        let sf = projectPtrs (ptrIndex slots sBumped)
            sW32 = toIntegral sBumped :: e Word32
        poke (getPtrOf (seDenseOrNext sf)) (toIntegral dLen)
        poke (getPtrOf (seGen sf)) 1
        poke (getPtrOf (smsSlotBumped f)) (sBumped + 1)
        writeRef slotIdxRef sW32
        writeRef slotGenRef 1
    )

  sIdx <- readRef slotIdxRef
  sGen <- readRef slotGenRef
  dv <- peekBufStruct (getPtrOf (smsDenseVals f))
  d2s <- peek (getPtrOf (smsDenseToSlot f))
  writeBuf dv dLen val
  pokeElemOff d2s sIdx dLen
  poke (getPtrOf (smsDenseLen f)) (dLen + 1)
  pure (SlotKey sIdx sGen)

-- | Resolve @key@ to @(isValid, denseIdx)@.
resolveKey ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  SlotKey e ->
  m (e Bool, e Int64)
resolveKey (SlotMap hdr) (SlotKey sIdxW32 kGen) = do
  let f = projectPtrs hdr
      sIdx = toIntegral sIdxW32 :: e Int64
  sBumped <- peek (getPtrOf (smsSlotBumped f))
  okRef <- newRef false
  dRef <- newRef (0 :: e Int64)
  whenM_ (((kGen `iand` 1) `eq` 1) `land` (sIdx `lt` sBumped)) $ do
    slots <- peek (getPtrOf (smsSlots f))
    let sf = projectPtrs (ptrIndex slots sIdx)
    curGen <- peek (getPtrOf (seGen sf))
    whenM_ (curGen `eq` kGen) $ do
      dW32 <- peek (getPtrOf (seDenseOrNext sf))
      writeRef okRef true
      writeRef dRef (toIntegral dW32)
  (,) <$> readRef okRef <*> readRef dRef

containsKey ::
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  SlotKey e ->
  m (e Bool)
containsKey sm k = fst <$> resolveKey sm k

lookup ::
  forall a s m e.
  (SlotMapCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  SlotMap e a ->
  SlotKey e ->
  m (e Bool, e s)
lookup defVal sm@(SlotMap hdr) k = do
  (ok, dIdx) <- resolveKey sm k
  valRef <- newRef defVal
  whenM_ ok $ do
    dv <- peekBufStruct (getPtrOf (smsDenseVals (projectPtrs hdr)))
    readBuf dv dIdx >>= writeRef valRef
  val <- readRef valRef
  pure (ok, val)

withLookup ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  SlotKey e ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withLookup sm@(SlotMap hdr) k onMiss onHit = do
  (ok, dIdx) <- resolveKey sm k
  ifThenElseM_
    ok
    ( do
        dv <- peekBufStruct (getPtrOf (smsDenseVals (projectPtrs hdr)))
        readBuf dv dIdx >>= onHit
    )
    onMiss

modifyKey ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  SlotKey e ->
  (Elem e a -> Elem e a) ->
  m (e Bool)
modifyKey sm@(SlotMap hdr) k fn = do
  (ok, dIdx) <- resolveKey sm k
  whenM_ ok $ do
    dv <- peekBufStruct (getPtrOf (smsDenseVals (projectPtrs hdr)))
    cur <- readBuf dv dIdx
    writeBuf dv dIdx (fn cur)
  pure ok

-- | O(1) generational delete: bumps slot generation to even, pushes onto free-list,
-- and swap-removes from the packed dense array while updating the moved element's slot entry!
delete ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  SlotKey e ->
  m (e Bool)
delete sm@(SlotMap hdr) k@(SlotKey sIdxW32 kGen) = do
  (ok, dIdx) <- resolveKey sm k
  whenM_ ok $ do
    let f = projectPtrs hdr
        sIdx = toIntegral sIdxW32 :: e Int64
    slots <- peek (getPtrOf (smsSlots f))
    fh <- peek (getPtrOf (smsFreeHead f))
    dLen <- peek (getPtrOf (smsDenseLen f))
    dv <- peekBufStruct (getPtrOf (smsDenseVals f))
    d2s <- peek (getPtrOf (smsDenseToSlot f))

    -- Retire sIdx: bump gen to even and push onto freeHead
    let sf = projectPtrs (ptrIndex slots sIdx)
    poke (getPtrOf (seGen sf)) (kGen + 1)
    poke (getPtrOf (seDenseOrNext sf)) fh
    poke (getPtrOf (smsFreeHead f)) sIdxW32

    -- Dense swap-remove
    let lastDense = dLen - 1
    whenM_ (dIdx `lt` lastDense) $ do
      movedVal <- readBuf dv lastDense
      movedSlot <- peekElemOff d2s lastDense
      writeBuf dv dIdx movedVal
      pokeElemOff d2s movedSlot dIdx
      let msf = projectPtrs (ptrIndex slots (toIntegral movedSlot))
      poke (getPtrOf (seDenseOrNext msf)) (toIntegral dIdx)

    poke (getPtrOf (smsDenseLen f)) lastDense
  pure ok

-- | Contiguous unit-stride 'Stream' over the packed @denseVals@ buffer (zero holes, zero branch mispredictions).
valuesS ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  Stream m (Elem e a)
valuesS (SlotMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (smsDenseLen f)) <*> peekBufStruct (getPtrOf (smsDenseVals f)))
        (\(n, dv) -> streamM (rangeS 0 n) (readBuf dv))

keysS ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  Stream m (SlotKey e)
keysS (SlotMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (smsDenseLen f)) <*> peek (getPtrOf (smsDenseToSlot f)) <*> peek (getPtrOf (smsSlots f)))
        $ \(n, d2s, slots) ->
          streamM (rangeS 0 n) $ \di -> do
            sW32 <- peekElemOff d2s di
            gen <- peek (getPtrOf (seGen (projectPtrs (ptrIndex slots (toIntegral sW32)))))
            pure (SlotKey sW32 gen)

toStream ::
  forall a m e.
  (SlotMapCtx m e a) =>
  SlotMap e a ->
  Stream m (SlotKey e, Elem e a)
toStream (SlotMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,,) <$> peek (getPtrOf (smsDenseLen f)) <*> peekBufStruct (getPtrOf (smsDenseVals f)) <*> peek (getPtrOf (smsDenseToSlot f)) <*> peek (getPtrOf (smsSlots f)))
        $ \(n, dv, d2s, slots) ->
          streamM (rangeS 0 n) $ \di -> do
            sW32 <- peekElemOff d2s di
            gen <- peek (getPtrOf (seGen (projectPtrs (ptrIndex slots (toIntegral sW32)))))
            val <- readBuf dv di
            pure (SlotKey sW32 gen, val)

-- | 64B-aligned SPMD strip-mined loop directly over the packed @denseVals@ array!
foreachScalarSPMD ::
  forall w a m e.
  ( SlotMapCtx m e a,
    Buf a ~ ScalarBuf a,
    SPMDGang w m e,
    SPMDLayout w (Ptr a) ~ 'LPtr a,
    CmdStorable (SPMDM w m e) (SPMD w e) a
  ) =>
  Proxy w ->
  SlotMap e a ->
  (SPMD w e Int64 -> SPMD w e a -> SPMDM w m e (SPMD w e a)) ->
  m ()
foreachScalarSPMD pw (SlotMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (smsDenseLen f))
  ScalarBuf rawPtr <- peekBufStruct (getPtrOf (smsDenseVals f))
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

foreachSoASlotMapSPMD ::
  forall w s m e.
  ( SlotMapCtx m e (SoA s),
    SPMDGang w m e,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  SlotMap e (SoA s) ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachSoASlotMapSPMD pw (SlotMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (smsDenseLen f))
  b <- peekBufStruct (getPtrOf (smsDenseVals f))
  foreachSoASPMD pw b n step

-- | Lane-parallel generational key validity check across @w@ SIMD lanes via a single
-- bounds-masked @Word32@ gather into the 8-byte @SlotEntry@ array.
containsKeysSPMD ::
  forall w a m e.
  (SlotMapCtx m e a, SPMDGang w m e) =>
  SlotMap e a ->
  SPMD w e Word32 ->
  SPMD w e Word32 ->
  SPMDM w m e (SPMD w e Bool)
containsKeysSPMD (SlotMap hdr) sIdxW32 kGen = do
  let f = projectPtrs hdr
      sIdx64 = toIntegral sIdxW32 :: SPMD w e Int64
      genWordOff = fromIntegral (fieldByteOffset @SlotEntry seGen `div` 4) :: SPMD w e Int64
  sBumped <- liftSPMD (peek (getPtrOf (smsSlotBumped f)))
  okRef <- newRef (false :: SPMD w e Bool)
  let valid = ((kGen `iand` 1) `eq` 1) `land` (sIdx64 `ge` 0) `land` (sIdx64 `lt` uniform sBumped)
  whenM_ valid $ do
    slots <- liftSPMD (peek (getPtrOf (smsSlots f)))
    let slotsW32 = fromVoidPtr (toVoidPtr slots) :: e (Ptr Word32)
    curGen <- peekElemOff (uniformAlignedPtr slotsW32) ((sIdx64 `lshift` 1) + genWordOff)
    writeRef okRef (curGen `eq` kGen)
  readRef okRef

-- | Unified layout-polymorphic SPMD strip-mined loop over the packed @denseVals@ buffer
-- across @Scalar@, @SoA@, @AoSoA w@, and @SOAOA w@.
foreachValuesSPMD ::
  forall w a m e.
  (SlotMapCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  SlotMap e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachValuesSPMD pw (SlotMap hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (smsDenseLen f))
  dv <- peekBufStruct (getPtrOf (smsDenseVals f))
  foreachBufSPMD pw True dv n step

-- | Masked-safe layout-polymorphic SIMD fold over the packed @denseVals@ buffer.
foldValuesSPMD ::
  forall w a s m e.
  (SlotMapCtx m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  SlotMap e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (SlotMap hdr) s0 step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (smsDenseLen f))
  dv <- peekBufStruct (getPtrOf (smsDenseVals f))
  foldBufSPMD pw True dv n s0 step

drainToSlotMap ::
  forall a m e.
  (SlotMapCtx m e a) =>
  Alloc e ->
  Drain m (Elem e a) (SlotMap e a)
drainToSlotMap alloc = Drain $ \s -> do
  sm <- new alloc
  runDrain (drainForEach (\x -> insert alloc sm x >> pure ())) s
  pure sm

fromStream ::
  (SlotMapCtx m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (SlotMap e a)
fromStream alloc = runDrain (drainToSlotMap alloc)

-- | Append every element from a 'Stream' into an existing @sm@, returning a 'DynVec' of the issued @(AoS SlotKey)@ handles.
drainAppendSlotMap ::
  forall a m e.
  (SlotMapCtx m e a, DV.DynVecCtx m e (AoS SlotKey)) =>
  Alloc e ->
  SlotMap e a ->
  Drain m (Elem e a) (DynVec e (AoS SlotKey))
drainAppendSlotMap alloc sm = Drain $ \s -> do
  keysVec <- DV.new @(AoS SlotKey) alloc
  runDrain
    ( drainForEach $ \x -> do
        k <- insert alloc sm x
        DV.push alloc keysVec k
    )
    s
  pure keysVec

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type SlotMapABI a = (Typeable a, JanusStruct (Buf a))

type instance JanusCEval (SlotMap JanusC a -> r) = SlotMap Identity a -> JanusCEval r

type instance JanusCRetType (SlotMap JanusC a -> r) = JanusCRetType r

instance (SlotMapABI a, JanusCParam r) => JanusCParam (SlotMap JanusC a -> r) where
  jcparam = mkPtrHandleJCParam SlotMap

instance (SlotMapABI a, JanusCEvaluate r) => JanusCEvaluate (SlotMap JanusC a -> r) where
  jceval = mkPtrHandleJCEval unSlotMap SlotMap

type instance JanusCEval (JanusCM (SlotMap JanusC a)) = IO (SlotMap Identity a)

type instance JanusCRetType (JanusCM (SlotMap JanusC a)) = SlotMap Identity a

instance (SlotMapABI a) => JanusCParam (JanusCM (SlotMap JanusC a)) where
  jcparam = mkPtrHandleJCRetParam unSlotMap

instance (SlotMapABI a) => JanusCEvaluate (JanusCM (SlotMap JanusC a)) where
  jceval = mkPtrHandleJCRetEval SlotMap

type instance LinuxX86_64CEval (SlotMap LinuxX86_64C a -> r) = SlotMap Identity a -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (SlotMap LinuxX86_64C a -> r) = LinuxX86_64BaseRet r

instance (SlotMapABI a, LinuxX86_64Param r) => LinuxX86_64Param (SlotMap LinuxX86_64C a -> r) where
  x86param = mkPtrHandleX86Param SlotMap

instance (SlotMapABI a, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (SlotMap LinuxX86_64C a -> r) where
  x86eval = mkPtrHandleX86EvalParam unSlotMap SlotMap

instance (SlotMapABI a, X86DeferCont r) => X86DeferCall (SlotMap LinuxX86_64C a -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unSlotMap SlotMap

instance (SlotMapABI a, X86DeferCont r) => X86DeferCont (SlotMap LinuxX86_64C a -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unSlotMap SlotMap

type instance LinuxX86_64CEval (LinuxX86_64CM (SlotMap LinuxX86_64C a)) = IO (SlotMap Identity a)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (SlotMap LinuxX86_64C a)) = SlotMap Identity a

instance (SlotMapABI a) => LinuxX86_64Param (LinuxX86_64CM (SlotMap LinuxX86_64C a)) where
  x86param = mkPtrHandleX86RetParam unSlotMap

instance (SlotMapABI a) => LinuxX86_64Evaluate (LinuxX86_64CM (SlotMap LinuxX86_64C a)) where
  x86eval = mkPtrHandleX86RetEval SlotMap

instance (SlotMapABI a) => X86DeferCall (LinuxX86_64CM (SlotMap LinuxX86_64C a)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg SlotMap

instance (SlotMapABI a) => X86DeferCont (LinuxX86_64CM (SlotMap LinuxX86_64C a)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont SlotMap

instance (SlotMapABI a, LinuxX86_64Param r, JanusCTH r) => JanusCTH (SlotMap LinuxX86_64C a -> r) where
  type JanusCTHEval (SlotMap LinuxX86_64C a -> r) = SlotMap Identity a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(SlotMap Identity a)) : ps, retSpec)

instance (SlotMapABI a) => JanusCTH (LinuxX86_64CM (SlotMap LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (SlotMap LinuxX86_64C a)) = IO (SlotMap Identity a)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(SlotMap Identity a)) (Proxy @(CStruct (SlotMapSpine a))))
