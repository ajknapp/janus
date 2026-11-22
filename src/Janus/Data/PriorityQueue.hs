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

module Janus.Data.PriorityQueue
  ( -- * 4-ary Cache-Line Min-Priority Queue (`PriorityQueue e k v`)
    PriorityQueueSpine (..),
    PriorityQueue (..),
    PriorityQueueCtx,
    new,
    withCapacity,
    free,
    size,
    capacity,
    isEmpty,
    clear,
    reserve,
    push,
    peekMin,
    withPeekMin,
    popMin,
    withPopMin,
    replaceMin,
    pushPopMin,
    unorderedStream,
    popAllS,
    drainToPriorityQueue,
    fromStream,
    drainAppendPriorityQueue,

    -- * Zero-Value-Overhead 4-ary Key-Only Min-Heap (`MinHeap e k`)
    MinHeapSpine (..),
    MinHeap (..),
    MinHeapCtx,
    newMinHeap,
    withMinHeapCapacity,
    freeMinHeap,
    minHeapSize,
    minHeapCapacity,
    isMinHeapEmpty,
    clearMinHeap,
    reserveMinHeap,
    pushMinHeap,
    peekMinHeap,
    withPeekMinHeap,
    popMinHeap,
    withPopMinHeap,
    unorderedMinHeapStream,
    popAllMinHeapS,
    drainToMinHeap,
    minHeapFromStream,
    drainAppendMinHeap,
  )
where

import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream
  ( Stream (..),
    iterateS,
    rangeS,
    reverseRangeS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import Janus.Data.FlatMap (OrdKey (..))
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 4-ary Key-Value Min-Priority Queue
--------------------------------------------------------------------------------

-- | C-ABI spine for a 4-ary implicit min-heap.
-- Node @i@ has parent @(i - 1) >> 2@ and up to 4 children @(i << 2) + 1 .. (i << 2) + 4@,
-- placing all 4 sibling keys contiguously inside a single 64-byte cache line!
data PriorityQueueSpine k v f = PriorityQueueSpine
  { pqsKeys :: !(f (CStruct (Buf k))),
    pqsVals :: !(f (CStruct (Buf v))),
    pqsLen :: !(f Int64),
    pqsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) => JanusStruct (PriorityQueueSpine k v)

newtype PriorityQueue (e :: K.Type -> K.Type) k v = PriorityQueue
  { unPriorityQueue :: e (Ptr (CStruct (PriorityQueueSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (PriorityQueue Identity k v)

deriving stock instance Show (PriorityQueue Identity k v)

type PriorityQueueCtx m e k v =
  ( LayoutCtx m e k,
    LayoutCtx m e v,
    OrdKey e k,
    Typeable k,
    Typeable v,
    ExpSized e (CStruct (PriorityQueueSpine k v)),
    JanusTyped e (CStruct (PriorityQueueSpine k v))
  )

new ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  m (PriorityQueue e k v)
new alloc = do
  hdr <- allocStruct @(CStruct (PriorityQueueSpine k v)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  v0 <- allocBuf @v alloc 0
  pokeBufStruct (getPtrOf (pqsKeys f)) k0
  pokeBufStruct (getPtrOf (pqsVals f)) v0
  poke (getPtrOf (pqsLen f)) 0
  poke (getPtrOf (pqsCap f)) 0
  pure (PriorityQueue hdr)

withCapacity ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (PriorityQueue e k v)
withCapacity alloc initCap = do
  pq <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc pq initCap
  pure pq

free ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  PriorityQueue e k v ->
  m ()
free alloc (PriorityQueue hdr) = do
  let f = projectPtrs hdr
  curCap <- peek (getPtrOf (pqsCap f))
  whenM_ (curCap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (pqsKeys f))
    vb <- peekBufStruct (getPtrOf (pqsVals f))
    freeBuf alloc kb curCap
    freeBuf alloc vb curCap
  freeStruct alloc hdr

size :: (PriorityQueueCtx m e k v) => PriorityQueue e k v -> m (e Int64)
size (PriorityQueue hdr) = peek (getPtrOf (pqsLen (projectPtrs hdr)))

capacity :: (PriorityQueueCtx m e k v) => PriorityQueue e k v -> m (e Int64)
capacity (PriorityQueue hdr) = peek (getPtrOf (pqsCap (projectPtrs hdr)))

isEmpty :: (PriorityQueueCtx m e k v) => PriorityQueue e k v -> m (e Bool)
isEmpty pq = (`eq` 0) <$> size pq

clear :: (PriorityQueueCtx m e k v) => PriorityQueue e k v -> m ()
clear (PriorityQueue hdr) = poke (getPtrOf (pqsLen (projectPtrs hdr))) 0

reserve ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  PriorityQueue e k v ->
  e Int64 ->
  m ()
reserve alloc (PriorityQueue hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  curCap <- peek (getPtrOf (pqsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldK <- peekBufStruct (getPtrOf (pqsKeys f))
    oldV <- peekBufStruct (getPtrOf (pqsVals f))
    newK <- reallocBuf alloc oldK curCap newCap curLen
    newV <- reallocBuf alloc oldV curCap newCap curLen
    pokeBufStruct (getPtrOf (pqsKeys f)) newK
    pokeBufStruct (getPtrOf (pqsVals f)) newV
    poke (getPtrOf (pqsCap f)) newCap

-- | Hole-based 4-ary sift-up: shifts ancestors down and writes @(itemK, itemV)@ once.
siftUpKV ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Buf k e ->
  Buf v e ->
  e Int64 ->
  Elem e k ->
  Elem e v ->
  m ()
siftUpKV kb vb idx0 itemK itemV = do
  posRef <- newRef idx0
  runDrain drain_ $
    unfoldWhileM (idx0 `gt` 0) pure $ \_ -> do
      cur <- readRef posRef
      let p = (cur - 1) `rshift` 2
      pk <- readBuf kb p
      whenM_ (ltKey @e @k itemK pk) $ do
        pv <- readBuf vb p
        writeBuf kb cur pk
        writeBuf vb cur pv
        writeRef posRef p
      curAfter <- readRef posRef
      pure ((curAfter `eq` p) `land` (p `gt` 0), ())
  finalPos <- readRef posRef
  writeBuf kb finalPos itemK
  writeBuf vb finalPos itemV

-- | Hole-based 4-ary sift-down: inspects up to 4 contiguous children per level.
siftDownKV ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Buf k e ->
  Buf v e ->
  e Int64 ->
  e Int64 ->
  Elem e k ->
  Elem e v ->
  m ()
siftDownKV kb vb heapLen idx0 itemK itemV = do
  posRef <- newRef idx0
  let hasChild i = ((i `lshift` 2) + 1) `lt` heapLen
  runDrain drain_ $
    unfoldWhileM (hasChild idx0) pure $ \_ -> do
      cur <- readRef posRef
      let c0 = (cur `lshift` 2) + 1
      bestRef <- newRef c0
      whenM_ ((c0 + 1) `lt` heapLen) $ do
        b1 <- readRef bestRef
        kb1 <- readBuf kb b1
        kc1 <- readBuf kb (c0 + 1)
        whenM_ (ltKey @e @k kc1 kb1) (writeRef bestRef (c0 + 1))
      whenM_ ((c0 + 2) `lt` heapLen) $ do
        b2 <- readRef bestRef
        kb2 <- readBuf kb b2
        kc2 <- readBuf kb (c0 + 2)
        whenM_ (ltKey @e @k kc2 kb2) (writeRef bestRef (c0 + 2))
      whenM_ ((c0 + 3) `lt` heapLen) $ do
        b3 <- readRef bestRef
        kb3 <- readBuf kb b3
        kc3 <- readBuf kb (c0 + 3)
        whenM_ (ltKey @e @k kc3 kb3) (writeRef bestRef (c0 + 3))
      bestC <- readRef bestRef
      bestCK <- readBuf kb bestC
      whenM_ (ltKey @e @k bestCK itemK) $ do
        bestCV <- readBuf vb bestC
        writeBuf kb cur bestCK
        writeBuf vb cur bestCV
        writeRef posRef bestC
      curAfter <- readRef posRef
      pure ((curAfter `eq` bestC) `land` hasChild bestC, ())
  finalPos <- readRef posRef
  writeBuf kb finalPos itemK
  writeBuf vb finalPos itemV

push ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  PriorityQueue e k v ->
  Elem e k ->
  Elem e v ->
  m ()
push alloc pq@(PriorityQueue hdr) key val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  curCap <- peek (getPtrOf (pqsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc pq 1
  kb <- peekBufStruct (getPtrOf (pqsKeys f))
  vb <- peekBufStruct (getPtrOf (pqsVals f))
  siftUpKV kb vb curLen key val
  poke (getPtrOf (pqsLen f)) (curLen + 1)

peekMin ::
  forall k v sk sv m e.
  ( PriorityQueueCtx m e k v,
    Elem e k ~ e sk,
    Elem e v ~ e sv,
    JanusTyped e sk,
    JanusTyped e sv
  ) =>
  e sk ->
  e sv ->
  PriorityQueue e k v ->
  m (e Bool, e sk, e sv)
peekMin defK defV (PriorityQueue hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  whenM_ (curLen `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (pqsKeys f))
    vb <- peekBufStruct (getPtrOf (pqsVals f))
    readBuf kb 0 >>= writeRef kRef
    readBuf vb 0 >>= writeRef vRef
    writeRef okRef true
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

withPeekMin ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  PriorityQueue e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withPeekMin (PriorityQueue hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        kb <- peekBufStruct (getPtrOf (pqsKeys f))
        vb <- peekBufStruct (getPtrOf (pqsVals f))
        k0 <- readBuf kb 0
        v0 <- readBuf vb 0
        onMin k0 v0
    )

popMin ::
  forall k v sk sv m e.
  ( PriorityQueueCtx m e k v,
    Elem e k ~ e sk,
    Elem e v ~ e sv,
    JanusTyped e sk,
    JanusTyped e sv
  ) =>
  e sk ->
  e sv ->
  PriorityQueue e k v ->
  m (e Bool, e sk, e sv)
popMin defK defV pq = do
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  withPopMin pq (pure ()) $ \k v -> do
    writeRef okRef true
    writeRef kRef k
    writeRef vRef v
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

withPopMin ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  PriorityQueue e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withPopMin (PriorityQueue hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        kb <- peekBufStruct (getPtrOf (pqsKeys f))
        vb <- peekBufStruct (getPtrOf (pqsVals f))
        minK <- readBuf kb 0
        minV <- readBuf vb 0
        let newLen = curLen - 1
        poke (getPtrOf (pqsLen f)) newLen
        whenM_ (newLen `gt` 0) $ do
          lastK <- readBuf kb newLen
          lastV <- readBuf vb newLen
          siftDownKV kb vb newLen 0 lastK lastV
        onMin minK minV
    )

-- | Replace the minimum root with @(newK, newV)@ in a single 4-ary sift-down (no pop+push!).
-- Requires @size > 0@; returns the displaced @(oldMinK, oldMinV)@.
replaceMin ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  PriorityQueue e k v ->
  Elem e k ->
  Elem e v ->
  m (Elem e k, Elem e v)
replaceMin (PriorityQueue hdr) newK newV = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (pqsLen f))
  kb <- peekBufStruct (getPtrOf (pqsKeys f))
  vb <- peekBufStruct (getPtrOf (pqsVals f))
  oldK <- readBuf kb 0
  oldV <- readBuf vb 0
  siftDownKV kb vb curLen 0 newK newV
  pure (oldK, oldV)

-- | Push @(k, v)@ and immediately pop the minimum in at most a single sift-down (zero resize!).
pushPopMin ::
  forall k v sk sv m e.
  ( PriorityQueueCtx m e k v,
    Elem e k ~ e sk,
    Elem e v ~ e sv,
    JanusTyped e sk,
    JanusTyped e sv
  ) =>
  PriorityQueue e k v ->
  e sk ->
  e sv ->
  m (e sk, e sv)
pushPopMin pq@(PriorityQueue hdr) k v = do
  curLen <- peek (getPtrOf (pqsLen (projectPtrs hdr)))
  outKRef <- newRef k
  outVRef <- newRef v
  whenM_ (curLen `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (pqsKeys (projectPtrs hdr)))
    rootK <- readBuf kb 0
    whenM_ (ltKey @e @k rootK k) $ do
      (oldK, oldV) <- replaceMin pq k v
      writeRef outKRef oldK
      writeRef outVRef oldV
  (,) <$> readRef outKRef <*> readRef outVRef

-- | Fast non-destructive contiguous scan over heap entries (in heap array order).
unorderedStream ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  PriorityQueue e k v ->
  Stream m (Elem e k, Elem e v)
unorderedStream (PriorityQueue hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (pqsLen f)) <*> peekBufStruct (getPtrOf (pqsKeys f)) <*> peekBufStruct (getPtrOf (pqsVals f)))
        (\(n, kb, vb) -> streamM (rangeS 0 n) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

-- | Destructive ascending-priority stream popping the minimum on each step.
popAllS ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  PriorityQueue e k v ->
  Stream m (Elem e k, Elem e v)
popAllS pq =
  Stream $ pure $ ContT $ \kStep ->
    withPopMin pq (kStep Stop) (\k v -> kStep (Yield (k, v)))

-- | O(N) Floyd bottom-up 4-ary heap construction from an arbitrary 'Stream'.
drainToPriorityQueue ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e v) (PriorityQueue e k v)
drainToPriorityQueue alloc = Drain $ \s -> do
  pq@(PriorityQueue hdr) <- new alloc
  let f = projectPtrs hdr
  -- 1. Append raw elements without per-element siftUp
  runDrain
    ( drainForEach $ \(k, v) -> do
        curLen <- peek (getPtrOf (pqsLen f))
        curCap <- peek (getPtrOf (pqsCap f))
        whenM_ (curLen `eq` curCap) $
          reserve alloc pq 1
        kb <- peekBufStruct (getPtrOf (pqsKeys f))
        vb <- peekBufStruct (getPtrOf (pqsVals f))
        writeBuf kb curLen k
        writeBuf vb curLen v
        poke (getPtrOf (pqsLen f)) (curLen + 1)
    )
    s
  -- 2. O(N) bottom-up Floyd heapify over internal nodes [(n - 2) >> 2 .. 0]
  n <- peek (getPtrOf (pqsLen f))
  whenM_ (n `ge` 2) $ do
    kb <- peekBufStruct (getPtrOf (pqsKeys f))
    vb <- peekBufStruct (getPtrOf (pqsVals f))
    let lastParentPlus1 = ((n - 2) `rshift` 2) + 1
    runDrain
      ( drainForEach $ \i -> do
          ki <- readBuf kb i
          vi <- readBuf vb i
          siftDownKV kb vb n i ki vi
      )
      (reverseRangeS 0 lastParentPlus1)
  pure pq

fromStream ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e v) ->
  m (PriorityQueue e k v)
fromStream alloc = runDrain (drainToPriorityQueue alloc)

drainAppendPriorityQueue ::
  forall k v m e.
  (PriorityQueueCtx m e k v) =>
  Alloc e ->
  PriorityQueue e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendPriorityQueue alloc pq =
  drainForEach (\(k, v) -> push alloc pq k v)

--------------------------------------------------------------------------------
-- Zero-Value-Overhead Key-Only 4-ary MinHeap
--------------------------------------------------------------------------------

data MinHeapSpine k f = MinHeapSpine
  { mhsKeys :: !(f (CStruct (Buf k))),
    mhsLen :: !(f Int64),
    mhsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, JanusStruct (Buf k)) => JanusStruct (MinHeapSpine k)

newtype MinHeap (e :: K.Type -> K.Type) k = MinHeap
  { unMinHeap :: e (Ptr (CStruct (MinHeapSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (MinHeap Identity k)

deriving stock instance Show (MinHeap Identity k)

type MinHeapCtx m e k =
  ( LayoutCtx m e k,
    OrdKey e k,
    Typeable k,
    ExpSized e (CStruct (MinHeapSpine k)),
    JanusTyped e (CStruct (MinHeapSpine k))
  )

newMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  m (MinHeap e k)
newMinHeap alloc = do
  hdr <- allocStruct @(CStruct (MinHeapSpine k)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  pokeBufStruct (getPtrOf (mhsKeys f)) k0
  poke (getPtrOf (mhsLen f)) 0
  poke (getPtrOf (mhsCap f)) 0
  pure (MinHeap hdr)

withMinHeapCapacity ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (MinHeap e k)
withMinHeapCapacity alloc initCap = do
  mh <- newMinHeap alloc
  whenM_ (initCap `gt` 0) $
    reserveMinHeap alloc mh initCap
  pure mh

freeMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  MinHeap e k ->
  m ()
freeMinHeap alloc (MinHeap hdr) = do
  let f = projectPtrs hdr
  curCap <- peek (getPtrOf (mhsCap f))
  whenM_ (curCap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (mhsKeys f))
    freeBuf alloc kb curCap
  freeStruct alloc hdr

minHeapSize :: (MinHeapCtx m e k) => MinHeap e k -> m (e Int64)
minHeapSize (MinHeap hdr) = peek (getPtrOf (mhsLen (projectPtrs hdr)))

minHeapCapacity :: (MinHeapCtx m e k) => MinHeap e k -> m (e Int64)
minHeapCapacity (MinHeap hdr) = peek (getPtrOf (mhsCap (projectPtrs hdr)))

isMinHeapEmpty :: (MinHeapCtx m e k) => MinHeap e k -> m (e Bool)
isMinHeapEmpty mh = (`eq` 0) <$> minHeapSize mh

clearMinHeap :: (MinHeapCtx m e k) => MinHeap e k -> m ()
clearMinHeap (MinHeap hdr) = poke (getPtrOf (mhsLen (projectPtrs hdr))) 0

reserveMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  MinHeap e k ->
  e Int64 ->
  m ()
reserveMinHeap alloc (MinHeap hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (mhsLen f))
  curCap <- peek (getPtrOf (mhsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldK <- peekBufStruct (getPtrOf (mhsKeys f))
    newK <- reallocBuf alloc oldK curCap newCap curLen
    pokeBufStruct (getPtrOf (mhsKeys f)) newK
    poke (getPtrOf (mhsCap f)) newCap

siftUpKey ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Buf k e ->
  e Int64 ->
  Elem e k ->
  m ()
siftUpKey kb idx0 itemK = do
  posRef <- newRef idx0
  runDrain drain_ $
    unfoldWhileM (idx0 `gt` 0) pure $ \_ -> do
      cur <- readRef posRef
      let p = (cur - 1) `rshift` 2
      pk <- readBuf kb p
      whenM_ (ltKey @e @k itemK pk) $ do
        writeBuf kb cur pk
        writeRef posRef p
      curAfter <- readRef posRef
      pure ((curAfter `eq` p) `land` (p `gt` 0), ())
  finalPos <- readRef posRef
  writeBuf kb finalPos itemK

siftDownKey ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Buf k e ->
  e Int64 ->
  e Int64 ->
  Elem e k ->
  m ()
siftDownKey kb heapLen idx0 itemK = do
  posRef <- newRef idx0
  let hasChild i = ((i `lshift` 2) + 1) `lt` heapLen
  runDrain drain_ $
    unfoldWhileM (hasChild idx0) pure $ \_ -> do
      cur <- readRef posRef
      let c0 = (cur `lshift` 2) + 1
      bestRef <- newRef c0
      whenM_ ((c0 + 1) `lt` heapLen) $ do
        b1 <- readRef bestRef
        kb1 <- readBuf kb b1
        kc1 <- readBuf kb (c0 + 1)
        whenM_ (ltKey @e @k kc1 kb1) (writeRef bestRef (c0 + 1))
      whenM_ ((c0 + 2) `lt` heapLen) $ do
        b2 <- readRef bestRef
        kb2 <- readBuf kb b2
        kc2 <- readBuf kb (c0 + 2)
        whenM_ (ltKey @e @k kc2 kb2) (writeRef bestRef (c0 + 2))
      whenM_ ((c0 + 3) `lt` heapLen) $ do
        b3 <- readRef bestRef
        kb3 <- readBuf kb b3
        kc3 <- readBuf kb (c0 + 3)
        whenM_ (ltKey @e @k kc3 kb3) (writeRef bestRef (c0 + 3))
      bestC <- readRef bestRef
      bestCK <- readBuf kb bestC
      whenM_ (ltKey @e @k bestCK itemK) $ do
        writeBuf kb cur bestCK
        writeRef posRef bestC
      curAfter <- readRef posRef
      pure ((curAfter `eq` bestC) `land` hasChild bestC, ())
  finalPos <- readRef posRef
  writeBuf kb finalPos itemK

pushMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  MinHeap e k ->
  Elem e k ->
  m ()
pushMinHeap alloc mh@(MinHeap hdr) key = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (mhsLen f))
  curCap <- peek (getPtrOf (mhsCap f))
  whenM_ (curLen `eq` curCap) $
    reserveMinHeap alloc mh 1
  kb <- peekBufStruct (getPtrOf (mhsKeys f))
  siftUpKey kb curLen key
  poke (getPtrOf (mhsLen f)) (curLen + 1)

peekMinHeap ::
  forall k sk m e.
  (MinHeapCtx m e k, Elem e k ~ e sk, JanusTyped e sk) =>
  e sk ->
  MinHeap e k ->
  m (e Bool, e sk)
peekMinHeap defK (MinHeap hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (mhsLen f))
  okRef <- newRef false
  kRef <- newRef defK
  whenM_ (curLen `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (mhsKeys f))
    readBuf kb 0 >>= writeRef kRef
    writeRef okRef true
  (,) <$> readRef okRef <*> readRef kRef

withPeekMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  MinHeap e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withPeekMinHeap (MinHeap hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (mhsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    (peekBufStruct (getPtrOf (mhsKeys f)) >>= (`readBuf` 0) >>= onMin)

popMinHeap ::
  forall k sk m e.
  (MinHeapCtx m e k, Elem e k ~ e sk, JanusTyped e sk) =>
  e sk ->
  MinHeap e k ->
  m (e Bool, e sk)
popMinHeap defK mh = do
  okRef <- newRef false
  kRef <- newRef defK
  withPopMinHeap mh (pure ()) $ \k -> do
    writeRef okRef true
    writeRef kRef k
  (,) <$> readRef okRef <*> readRef kRef

withPopMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  MinHeap e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withPopMinHeap (MinHeap hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (mhsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        kb <- peekBufStruct (getPtrOf (mhsKeys f))
        minK <- readBuf kb 0
        let newLen = curLen - 1
        poke (getPtrOf (mhsLen f)) newLen
        whenM_ (newLen `gt` 0) $ do
          lastK <- readBuf kb newLen
          siftDownKey kb newLen 0 lastK
        onMin minK
    )

unorderedMinHeapStream ::
  forall k m e.
  (MinHeapCtx m e k) =>
  MinHeap e k ->
  Stream m (Elem e k)
unorderedMinHeapStream (MinHeap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (mhsLen f)) <*> peekBufStruct (getPtrOf (mhsKeys f)))
        (\(n, kb) -> streamM (rangeS 0 n) (readBuf kb))

popAllMinHeapS ::
  forall k m e.
  (MinHeapCtx m e k) =>
  MinHeap e k ->
  Stream m (Elem e k)
popAllMinHeapS mh =
  Stream $ pure $ ContT $ \kStep ->
    withPopMinHeap mh (kStep Stop) (kStep . Yield)

drainToMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (MinHeap e k)
drainToMinHeap alloc = Drain $ \s -> do
  mh@(MinHeap hdr) <- newMinHeap alloc
  let f = projectPtrs hdr
  runDrain
    ( drainForEach $ \k -> do
        curLen <- peek (getPtrOf (mhsLen f))
        curCap <- peek (getPtrOf (mhsCap f))
        whenM_ (curLen `eq` curCap) $
          reserveMinHeap alloc mh 1
        kb <- peekBufStruct (getPtrOf (mhsKeys f))
        writeBuf kb curLen k
        poke (getPtrOf (mhsLen f)) (curLen + 1)
    )
    s
  n <- peek (getPtrOf (mhsLen f))
  whenM_ (n `ge` 2) $ do
    kb <- peekBufStruct (getPtrOf (mhsKeys f))
    let lastParentPlus1 = ((n - 2) `rshift` 2) + 1
    runDrain
      (drainForEach (\i -> readBuf kb i >>= siftDownKey kb n i))
      (reverseRangeS 0 lastParentPlus1)
  pure mh

minHeapFromStream ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (MinHeap e k)
minHeapFromStream alloc = runDrain (drainToMinHeap alloc)

drainAppendMinHeap ::
  forall k m e.
  (MinHeapCtx m e k) =>
  Alloc e ->
  MinHeap e k ->
  Drain m (Elem e k) ()
drainAppendMinHeap alloc mh = drainForEach (pushMinHeap alloc mh)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type PriorityQueueABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCEval (PriorityQueue JanusC k v -> r) = PriorityQueue Identity k v -> JanusCEval r

type instance JanusCRetType (PriorityQueue JanusC k v -> r) = JanusCRetType r

instance (PriorityQueueABI k v, JanusCParam r) => JanusCParam (PriorityQueue JanusC k v -> r) where
  jcparam = mkPtrHandleJCParam PriorityQueue

instance (PriorityQueueABI k v, JanusCEvaluate r) => JanusCEvaluate (PriorityQueue JanusC k v -> r) where
  jceval = mkPtrHandleJCEval unPriorityQueue PriorityQueue

type instance JanusCEval (JanusCM (PriorityQueue JanusC k v)) = IO (PriorityQueue Identity k v)

type instance JanusCRetType (JanusCM (PriorityQueue JanusC k v)) = PriorityQueue Identity k v

instance (PriorityQueueABI k v) => JanusCParam (JanusCM (PriorityQueue JanusC k v)) where
  jcparam = mkPtrHandleJCRetParam unPriorityQueue

instance (PriorityQueueABI k v) => JanusCEvaluate (JanusCM (PriorityQueue JanusC k v)) where
  jceval = mkPtrHandleJCRetEval PriorityQueue

type instance LinuxX86_64CEval (PriorityQueue LinuxX86_64C k v -> r) = PriorityQueue Identity k v -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (PriorityQueue LinuxX86_64C k v -> r) = LinuxX86_64BaseRet r

instance (PriorityQueueABI k v, LinuxX86_64Param r) => LinuxX86_64Param (PriorityQueue LinuxX86_64C k v -> r) where
  x86param = mkPtrHandleX86Param PriorityQueue

instance (PriorityQueueABI k v, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (PriorityQueue LinuxX86_64C k v -> r) where
  x86eval = mkPtrHandleX86EvalParam unPriorityQueue PriorityQueue

instance (PriorityQueueABI k v, X86DeferCont r) => X86DeferCall (PriorityQueue LinuxX86_64C k v -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unPriorityQueue PriorityQueue

instance (PriorityQueueABI k v, X86DeferCont r) => X86DeferCont (PriorityQueue LinuxX86_64C k v -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unPriorityQueue PriorityQueue

type instance LinuxX86_64CEval (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) = IO (PriorityQueue Identity k v)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) = PriorityQueue Identity k v

instance (PriorityQueueABI k v) => LinuxX86_64Param (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) where
  x86param = mkPtrHandleX86RetParam unPriorityQueue

instance (PriorityQueueABI k v) => LinuxX86_64Evaluate (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) where
  x86eval = mkPtrHandleX86RetEval PriorityQueue

instance (PriorityQueueABI k v) => X86DeferCall (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg PriorityQueue

instance (PriorityQueueABI k v) => X86DeferCont (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont PriorityQueue

instance (PriorityQueueABI k v, LinuxX86_64Param r, JanusCTH r) => JanusCTH (PriorityQueue LinuxX86_64C k v -> r) where
  type JanusCTHEval (PriorityQueue LinuxX86_64C k v -> r) = PriorityQueue Identity k v -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(PriorityQueue Identity k v)) : ps, retSpec)

instance (PriorityQueueABI k v) => JanusCTH (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) where
  type JanusCTHEval (LinuxX86_64CM (PriorityQueue LinuxX86_64C k v)) = IO (PriorityQueue Identity k v)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(PriorityQueue Identity k v)) (Proxy @(CStruct (PriorityQueueSpine k v))))

type MinHeapABI k = (Typeable k, JanusStruct (Buf k))

type instance JanusCEval (MinHeap JanusC k -> r) = MinHeap Identity k -> JanusCEval r

type instance JanusCRetType (MinHeap JanusC k -> r) = JanusCRetType r

instance (MinHeapABI k, JanusCParam r) => JanusCParam (MinHeap JanusC k -> r) where
  jcparam = mkPtrHandleJCParam MinHeap

instance (MinHeapABI k, JanusCEvaluate r) => JanusCEvaluate (MinHeap JanusC k -> r) where
  jceval = mkPtrHandleJCEval unMinHeap MinHeap

type instance JanusCEval (JanusCM (MinHeap JanusC k)) = IO (MinHeap Identity k)

type instance JanusCRetType (JanusCM (MinHeap JanusC k)) = MinHeap Identity k

instance (MinHeapABI k) => JanusCParam (JanusCM (MinHeap JanusC k)) where
  jcparam = mkPtrHandleJCRetParam unMinHeap

instance (MinHeapABI k) => JanusCEvaluate (JanusCM (MinHeap JanusC k)) where
  jceval = mkPtrHandleJCRetEval MinHeap

type instance LinuxX86_64CEval (MinHeap LinuxX86_64C k -> r) = MinHeap Identity k -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (MinHeap LinuxX86_64C k -> r) = LinuxX86_64BaseRet r

instance (MinHeapABI k, LinuxX86_64Param r) => LinuxX86_64Param (MinHeap LinuxX86_64C k -> r) where
  x86param = mkPtrHandleX86Param MinHeap

instance (MinHeapABI k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (MinHeap LinuxX86_64C k -> r) where
  x86eval = mkPtrHandleX86EvalParam unMinHeap MinHeap

instance (MinHeapABI k, X86DeferCont r) => X86DeferCall (MinHeap LinuxX86_64C k -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unMinHeap MinHeap

instance (MinHeapABI k, X86DeferCont r) => X86DeferCont (MinHeap LinuxX86_64C k -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unMinHeap MinHeap

type instance LinuxX86_64CEval (LinuxX86_64CM (MinHeap LinuxX86_64C k)) = IO (MinHeap Identity k)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (MinHeap LinuxX86_64C k)) = MinHeap Identity k

instance (MinHeapABI k) => LinuxX86_64Param (LinuxX86_64CM (MinHeap LinuxX86_64C k)) where
  x86param = mkPtrHandleX86RetParam unMinHeap

instance (MinHeapABI k) => LinuxX86_64Evaluate (LinuxX86_64CM (MinHeap LinuxX86_64C k)) where
  x86eval = mkPtrHandleX86RetEval MinHeap

instance (MinHeapABI k) => X86DeferCall (LinuxX86_64CM (MinHeap LinuxX86_64C k)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg MinHeap

instance (MinHeapABI k) => X86DeferCont (LinuxX86_64CM (MinHeap LinuxX86_64C k)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont MinHeap

instance (MinHeapABI k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (MinHeap LinuxX86_64C k -> r) where
  type JanusCTHEval (MinHeap LinuxX86_64C k -> r) = MinHeap Identity k -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(MinHeap Identity k)) : ps, retSpec)

instance (MinHeapABI k) => JanusCTH (LinuxX86_64CM (MinHeap LinuxX86_64C k)) where
  type JanusCTHEval (LinuxX86_64CM (MinHeap LinuxX86_64C k)) = IO (MinHeap Identity k)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(MinHeap Identity k)) (Proxy @(CStruct (MinHeapSpine k))))
