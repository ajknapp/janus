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

module Janus.Data.IndexedPQ
  ( -- * Handle-Indexed 4-ary Min-Priority Queue (O(1) Uninitialized-Safe Clear + O(log4 N) DecreaseKey!)
    IndexedPQSpine (..),
    IndexedPQ (..),
    IndexedPQCtx,

    -- * Lifecycle
    new,
    withUniverse,
    free,

    -- * Queries & O(1) Clear
    size,
    universeCapacity,
    isEmpty,
    clear,
    ensureUniverse,

    -- * O(1) Lookup & O(log4 N) Handle-Addressed Updates
    member,
    lookup,
    withLookup,
    insertOrUpdate,
    decreasePriority,
    deleteById,

    -- * Min Inspection & Extraction
    peekMin,
    withPeekMin,
    popMin,
    withPopMin,

    -- * Streams & Drains
    toStream,
    popAllS,
    drainToIndexedPQ,
    fromStream,
    drainAppendIndexedPQ,

    -- * SPMD / SIMD Gang Kernels & Batch Handle Membership
    foreachValuesSPMD,
    foldPrioritiesSPMD,
    containsIdsSPMD,
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
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream (..), iterateS, rangeS, streamM, unfoldWhileM, withStreamSetup)
import Janus.Data.FlatMap (OrdKey (..), OrdLayoutCtx)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD (SPMD, SPMDGang, SPMDM, SPMDTyped, liftSPMD, uniform, uniformAlignedPtr)
import Prelude hiding (lookup)

-- | Spine struct for 'IndexedPQ':
-- Combines a 4-ary min-heap (@ipqsHeapIds@, @ipqsPrios@, @ipqsVals@) with a Briggs-Torczon
-- uninitialized-safe sparse handle map @ipqsSparsePos@ (@id@ is live iff
-- @0 <= ipqsSparsePos[id] < ipqsLen && ipqsHeapIds[ipqsSparsePos[id]] == id@).
data IndexedPQSpine p v f = IndexedPQSpine
  { ipqsSparsePos :: !(f (Ptr Int64)),
    ipqsHeapIds :: !(f (Ptr Int64)),
    ipqsPrios :: !(f (CStruct (Buf p))),
    ipqsVals :: !(f (CStruct (Buf v))),
    ipqsLen :: !(f Int64),
    ipqsUniverseCap :: !(f Int64),
    ipqsHeapCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable p, Typeable v, JanusStruct (Buf p), JanusStruct (Buf v)) => JanusStruct (IndexedPQSpine p v)

newtype IndexedPQ (e :: K.Type -> K.Type) p v = IndexedPQ
  { unIndexedPQ :: e (Ptr (CStruct (IndexedPQSpine p v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (IndexedPQ Identity p v)

deriving stock instance Show (IndexedPQ Identity p v)

type IndexedPQCtx m e p v = (OrdLayoutCtx m e p, LayoutCtx m e v)

new ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  m (IndexedPQ e p v)
new alloc = do
  hdr <- allocStruct @(CStruct (IndexedPQSpine p v)) alloc
  let f = projectPtrs hdr
  p0 <- allocBuf @p alloc 0
  v0 <- allocBuf @v alloc 0
  poke (getPtrOf (ipqsSparsePos f)) nullPtr
  poke (getPtrOf (ipqsHeapIds f)) nullPtr
  pokeBufStruct (getPtrOf (ipqsPrios f)) p0
  pokeBufStruct (getPtrOf (ipqsVals f)) v0
  poke (getPtrOf (ipqsLen f)) 0
  poke (getPtrOf (ipqsUniverseCap f)) 0
  poke (getPtrOf (ipqsHeapCap f)) 0
  pure (IndexedPQ hdr)

withUniverse ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  e Int64 ->
  m (IndexedPQ e p v)
withUniverse alloc uCap = do
  pq <- new alloc
  whenM_ (uCap `gt` 0) $ do
    ensureUniverse alloc pq (uCap - 1)
    ensureHeapCap alloc pq uCap
  pure pq

free ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  m ()
free alloc (IndexedPQ hdr) = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (ipqsUniverseCap f))
  whenM_ (uCap `gt` 0) $ do
    sp <- peek (getPtrOf (ipqsSparsePos f))
    freeTypedArray @Int64 alloc sp uCap
  hCap <- peek (getPtrOf (ipqsHeapCap f))
  whenM_ (hCap `gt` 0) $ do
    hi <- peek (getPtrOf (ipqsHeapIds f))
    pb <- peekBufStruct (getPtrOf (ipqsPrios f))
    vb <- peekBufStruct (getPtrOf (ipqsVals f))
    freeTypedArray @Int64 alloc hi hCap
    freeBuf alloc pb hCap
    freeBuf alloc vb hCap
  freeStruct alloc hdr

size :: (IndexedPQCtx m e p v) => IndexedPQ e p v -> m (e Int64)
size (IndexedPQ hdr) = peek (getPtrOf (ipqsLen (projectPtrs hdr)))

universeCapacity :: (IndexedPQCtx m e p v) => IndexedPQ e p v -> m (e Int64)
universeCapacity (IndexedPQ hdr) = peek (getPtrOf (ipqsUniverseCap (projectPtrs hdr)))

isEmpty :: (IndexedPQCtx m e p v) => IndexedPQ e p v -> m (e Bool)
isEmpty pq = (`eq` 0) <$> size pq

-- | O(1) clear without zeroing @ipqsSparsePos@!
clear :: (IndexedPQCtx m e p v) => IndexedPQ e p v -> m ()
clear (IndexedPQ hdr) = poke (getPtrOf (ipqsLen (projectPtrs hdr))) 0

ensureUniverse ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  e Int64 ->
  m ()
ensureUniverse alloc (IndexedPQ hdr) maxId = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (ipqsUniverseCap f))
  whenM_ (maxId `ge` uCap) $ do
    let c0 = ifThenElse (uCap `lt` 64) 64 (uCap * 2)
    (_, newUCap) <- runDrain (drainFind c0 (`gt` maxId)) (iterateS c0 (* 2))
    oldSp <- peek (getPtrOf (ipqsSparsePos f))
    newSp <- reallocTypedArray @Int64 alloc oldSp uCap newUCap
    poke (getPtrOf (ipqsSparsePos f)) newSp
    poke (getPtrOf (ipqsUniverseCap f)) newUCap

ensureHeapCap ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  e Int64 ->
  m ()
ensureHeapCap alloc (IndexedPQ hdr) reqCap = do
  let f = projectPtrs hdr
  hCap <- peek (getPtrOf (ipqsHeapCap f))
  curLen <- peek (getPtrOf (ipqsLen f))
  whenM_ (reqCap `gt` hCap) $ do
    let c0 = ifThenElse (hCap `lt` 16) 16 (hCap * 2)
    (_, newHCap) <- runDrain (drainFind c0 (`ge` reqCap)) (iterateS c0 (* 2))
    oldIds <- peek (getPtrOf (ipqsHeapIds f))
    oldP <- peekBufStruct (getPtrOf (ipqsPrios f))
    oldV <- peekBufStruct (getPtrOf (ipqsVals f))
    newIds <- reallocTypedArray @Int64 alloc oldIds hCap newHCap
    newP <- reallocBuf alloc oldP hCap newHCap curLen
    newV <- reallocBuf alloc oldV hCap newHCap curLen
    poke (getPtrOf (ipqsHeapIds f)) newIds
    pokeBufStruct (getPtrOf (ipqsPrios f)) newP
    pokeBufStruct (getPtrOf (ipqsVals f)) newV
    poke (getPtrOf (ipqsHeapCap f)) newHCap

findHeapSlot ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  e Int64 ->
  m (e Bool, e Int64)
findHeapSlot (IndexedPQ hdr) itemId = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (ipqsUniverseCap f))
  okRef <- newRef false
  posRef <- newRef (0 :: e Int64)
  whenM_ ((itemId `ge` 0) `land` (itemId `lt` uCap)) $ do
    curLen <- peek (getPtrOf (ipqsLen f))
    sp <- peek (getPtrOf (ipqsSparsePos f))
    pos <- peekElemOff sp itemId
    whenM_ ((pos `ge` 0) `land` (pos `lt` curLen)) $ do
      hi <- peek (getPtrOf (ipqsHeapIds f))
      backId <- peekElemOff hi pos
      whenM_ (backId `eq` itemId) $ do
        writeRef okRef true
        writeRef posRef pos
  (,) <$> readRef okRef <*> readRef posRef

member :: (IndexedPQCtx m e p v) => IndexedPQ e p v -> e Int64 -> m (e Bool)
member pq itemId = fst <$> findHeapSlot pq itemId

lookup ::
  forall p v sp sv m e.
  ( IndexedPQCtx m e p v,
    Elem e p ~ e sp,
    Elem e v ~ e sv,
    KnownJanusType e sp,
    KnownJanusType e sv
  ) =>
  e sp ->
  e sv ->
  IndexedPQ e p v ->
  e Int64 ->
  m (e Bool, e sp, e sv)
lookup defP defV pq@(IndexedPQ hdr) itemId = do
  (ok, pos) <- findHeapSlot pq itemId
  pRef <- newRef defP
  vRef <- newRef defV
  whenM_ ok $ do
    let f = projectPtrs hdr
    pb <- peekBufStruct (getPtrOf (ipqsPrios f))
    vb <- peekBufStruct (getPtrOf (ipqsVals f))
    readBuf pb pos >>= writeRef pRef
    readBuf vb pos >>= writeRef vRef
  (,,) <$> pure ok <*> readRef pRef <*> readRef vRef

withLookup ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  e Int64 ->
  m () ->
  (Elem e p -> Elem e v -> m ()) ->
  m ()
withLookup pq@(IndexedPQ hdr) itemId onMiss onHit = do
  (ok, pos) <- findHeapSlot pq itemId
  ifThenElseM_
    ok
    ( do
        let f = projectPtrs hdr
        pb <- peekBufStruct (getPtrOf (ipqsPrios f))
        vb <- peekBufStruct (getPtrOf (ipqsVals f))
        p <- readBuf pb pos
        v <- readBuf vb pos
        onHit p v
    )
    onMiss

siftUpIndexed ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  e (Ptr Int64) ->
  e (Ptr Int64) ->
  Buf p e ->
  Buf v e ->
  e Int64 ->
  e Int64 ->
  Elem e p ->
  Elem e v ->
  m ()
siftUpIndexed sp hi pb vb idx0 itemId itemP itemV = do
  posRef <- newRef idx0
  runDrain drain_ $
    unfoldWhileM (idx0 `gt` 0) pure $ \_ -> do
      cur <- readRef posRef
      let par = (cur - 1) `rshift` 2
      parP <- readBuf pb par
      whenM_ (ltKey @e @p itemP parP) $ do
        parId <- peekElemOff hi par
        parV <- readBuf vb par
        pokeElemOff hi parId cur
        pokeElemOff sp cur parId
        writeBuf pb cur parP
        writeBuf vb cur parV
        writeRef posRef par
      curAfter <- readRef posRef
      pure ((curAfter `eq` par) `land` (par `gt` 0), ())
  finalPos <- readRef posRef
  pokeElemOff hi itemId finalPos
  pokeElemOff sp finalPos itemId
  writeBuf pb finalPos itemP
  writeBuf vb finalPos itemV

siftDownIndexed ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  e (Ptr Int64) ->
  e (Ptr Int64) ->
  Buf p e ->
  Buf v e ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  Elem e p ->
  Elem e v ->
  m ()
siftDownIndexed sp hi pb vb heapLen idx0 itemId itemP itemV = do
  posRef <- newRef idx0
  let hasChild i = ((i `lshift` 2) + 1) `lt` heapLen
  runDrain drain_ $
    unfoldWhileM (hasChild idx0) pure $ \_ -> do
      cur <- readRef posRef
      let c0 = (cur `lshift` 2) + 1
      bestRef <- newRef c0
      whenM_ ((c0 + 1) `lt` heapLen) $ do
        b1 <- readRef bestRef
        pb1 <- readBuf pb b1
        pc1 <- readBuf pb (c0 + 1)
        whenM_ (ltKey @e @p pc1 pb1) (writeRef bestRef (c0 + 1))
      whenM_ ((c0 + 2) `lt` heapLen) $ do
        b2 <- readRef bestRef
        pb2 <- readBuf pb b2
        pc2 <- readBuf pb (c0 + 2)
        whenM_ (ltKey @e @p pc2 pb2) (writeRef bestRef (c0 + 2))
      whenM_ ((c0 + 3) `lt` heapLen) $ do
        b3 <- readRef bestRef
        pb3 <- readBuf pb b3
        pc3 <- readBuf pb (c0 + 3)
        whenM_ (ltKey @e @p pc3 pb3) (writeRef bestRef (c0 + 3))
      bestC <- readRef bestRef
      bestCP <- readBuf pb bestC
      whenM_ (ltKey @e @p bestCP itemP) $ do
        bestId <- peekElemOff hi bestC
        bestV <- readBuf vb bestC
        pokeElemOff hi bestId cur
        pokeElemOff sp cur bestId
        writeBuf pb cur bestCP
        writeBuf vb cur bestV
        writeRef posRef bestC
      curAfter <- readRef posRef
      pure ((curAfter `eq` bestC) `land` hasChild bestC, ())
  finalPos <- readRef posRef
  pokeElemOff hi itemId finalPos
  pokeElemOff sp finalPos itemId
  writeBuf pb finalPos itemP
  writeBuf vb finalPos itemV

-- | Insert @(itemId, prio, val)@ if absent, or update its priority/payload in O(log4 N)
-- (sifting up if @prio@ decreased or sifting down if @prio@ increased).
insertOrUpdate ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  e Int64 ->
  Elem e p ->
  Elem e v ->
  m ()
insertOrUpdate alloc pq@(IndexedPQ hdr) itemId prio val =
  whenM_ (itemId `ge` 0) $ do
    let f = projectPtrs hdr
    (ok, pos) <- findHeapSlot pq itemId
    ifThenElseM_
      ok
      ( do
          curLen <- peek (getPtrOf (ipqsLen f))
          sp <- peek (getPtrOf (ipqsSparsePos f))
          hi <- peek (getPtrOf (ipqsHeapIds f))
          pb <- peekBufStruct (getPtrOf (ipqsPrios f))
          vb <- peekBufStruct (getPtrOf (ipqsVals f))
          oldP <- readBuf pb pos
          ifThenElseM_
            (ltKey @e @p prio oldP)
            (siftUpIndexed sp hi pb vb pos itemId prio val)
            (siftDownIndexed sp hi pb vb curLen pos itemId prio val)
      )
      ( do
          ensureUniverse alloc pq itemId
          curLen <- peek (getPtrOf (ipqsLen f))
          ensureHeapCap alloc pq (curLen + 1)
          sp <- peek (getPtrOf (ipqsSparsePos f))
          hi <- peek (getPtrOf (ipqsHeapIds f))
          pb <- peekBufStruct (getPtrOf (ipqsPrios f))
          vb <- peekBufStruct (getPtrOf (ipqsVals f))
          siftUpIndexed sp hi pb vb curLen itemId prio val
          poke (getPtrOf (ipqsLen f)) (curLen + 1)
      )

-- | Dijkstra / A* relaxation primitive: inserts @(itemId, prio, val)@ if absent,
-- or updates & sifts up ONLY if @prio@ is strictly smaller than the current priority.
-- Returns @true@ iff inserted or relaxed.
decreasePriority ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  e Int64 ->
  Elem e p ->
  Elem e v ->
  m (e Bool)
decreasePriority alloc pq@(IndexedPQ hdr) itemId prio val = do
  relaxedRef <- newRef false
  whenM_ (itemId `ge` 0) $ do
    let f = projectPtrs hdr
    (ok, pos) <- findHeapSlot pq itemId
    ifThenElseM_
      ok
      ( do
          pb <- peekBufStruct (getPtrOf (ipqsPrios f))
          oldP <- readBuf pb pos
          whenM_ (ltKey @e @p prio oldP) $ do
            sp <- peek (getPtrOf (ipqsSparsePos f))
            hi <- peek (getPtrOf (ipqsHeapIds f))
            vb <- peekBufStruct (getPtrOf (ipqsVals f))
            siftUpIndexed sp hi pb vb pos itemId prio val
            writeRef relaxedRef true
      )
      ( do
          ensureUniverse alloc pq itemId
          curLen <- peek (getPtrOf (ipqsLen f))
          ensureHeapCap alloc pq (curLen + 1)
          sp <- peek (getPtrOf (ipqsSparsePos f))
          hi <- peek (getPtrOf (ipqsHeapIds f))
          pb <- peekBufStruct (getPtrOf (ipqsPrios f))
          vb <- peekBufStruct (getPtrOf (ipqsVals f))
          siftUpIndexed sp hi pb vb curLen itemId prio val
          poke (getPtrOf (ipqsLen f)) (curLen + 1)
          writeRef relaxedRef true
      )
  readRef relaxedRef

-- | O(log4 N) arbitrary handle removal. Returns @true@ iff @itemId@ was present and removed.
deleteById ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  e Int64 ->
  m (e Bool)
deleteById pq@(IndexedPQ hdr) itemId = do
  (ok, pos) <- findHeapSlot pq itemId
  whenM_ ok $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (ipqsLen f))
    sp <- peek (getPtrOf (ipqsSparsePos f))
    hi <- peek (getPtrOf (ipqsHeapIds f))
    pb <- peekBufStruct (getPtrOf (ipqsPrios f))
    vb <- peekBufStruct (getPtrOf (ipqsVals f))
    let newLen = curLen - 1
    poke (getPtrOf (ipqsLen f)) newLen
    pokeElemOff sp (-1) itemId
    whenM_ (pos `lt` newLen) $ do
      removedP <- readBuf pb pos
      movedId <- peekElemOff hi newLen
      movedP <- readBuf pb newLen
      movedV <- readBuf vb newLen
      ifThenElseM_
        (ltKey @e @p movedP removedP)
        (siftUpIndexed sp hi pb vb pos movedId movedP movedV)
        (siftDownIndexed sp hi pb vb newLen pos movedId movedP movedV)
  pure ok

peekMin ::
  forall p v sp sv m e.
  ( IndexedPQCtx m e p v,
    Elem e p ~ e sp,
    Elem e v ~ e sv,
    KnownJanusType e sp,
    KnownJanusType e sv
  ) =>
  e sp ->
  e sv ->
  IndexedPQ e p v ->
  m (e Bool, e Int64, e sp, e sv)
peekMin defP defV (IndexedPQ hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (ipqsLen f))
  okRef <- newRef false
  idRef <- newRef (0 :: e Int64)
  pRef <- newRef defP
  vRef <- newRef defV
  whenM_ (curLen `gt` 0) $ do
    hi <- peek (getPtrOf (ipqsHeapIds f))
    pb <- peekBufStruct (getPtrOf (ipqsPrios f))
    vb <- peekBufStruct (getPtrOf (ipqsVals f))
    peekElemOff hi 0 >>= writeRef idRef
    readBuf pb 0 >>= writeRef pRef
    readBuf vb 0 >>= writeRef vRef
    writeRef okRef true
  (,,,) <$> readRef okRef <*> readRef idRef <*> readRef pRef <*> readRef vRef

withPeekMin ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  m () ->
  (e Int64 -> Elem e p -> Elem e v -> m ()) ->
  m ()
withPeekMin (IndexedPQ hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (ipqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        hi <- peek (getPtrOf (ipqsHeapIds f))
        pb <- peekBufStruct (getPtrOf (ipqsPrios f))
        vb <- peekBufStruct (getPtrOf (ipqsVals f))
        i0 <- peekElemOff hi 0
        p0 <- readBuf pb 0
        v0 <- readBuf vb 0
        onMin i0 p0 v0
    )

popMin ::
  forall p v sp sv m e.
  ( IndexedPQCtx m e p v,
    Elem e p ~ e sp,
    Elem e v ~ e sv,
    KnownJanusType e sp,
    KnownJanusType e sv
  ) =>
  e sp ->
  e sv ->
  IndexedPQ e p v ->
  m (e Bool, e Int64, e sp, e sv)
popMin defP defV pq = do
  okRef <- newRef false
  idRef <- newRef (0 :: e Int64)
  pRef <- newRef defP
  vRef <- newRef defV
  withPopMin pq (pure ()) $ \i p v -> do
    writeRef okRef true
    writeRef idRef i
    writeRef pRef p
    writeRef vRef v
  (,,,) <$> readRef okRef <*> readRef idRef <*> readRef pRef <*> readRef vRef

withPopMin ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  m () ->
  (e Int64 -> Elem e p -> Elem e v -> m ()) ->
  m ()
withPopMin (IndexedPQ hdr) onEmpty onMin = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (ipqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        sp <- peek (getPtrOf (ipqsSparsePos f))
        hi <- peek (getPtrOf (ipqsHeapIds f))
        pb <- peekBufStruct (getPtrOf (ipqsPrios f))
        vb <- peekBufStruct (getPtrOf (ipqsVals f))
        minId <- peekElemOff hi 0
        minP <- readBuf pb 0
        minV <- readBuf vb 0
        let newLen = curLen - 1
        poke (getPtrOf (ipqsLen f)) newLen
        pokeElemOff sp (-1) minId
        whenM_ (newLen `gt` 0) $ do
          lastId <- peekElemOff hi newLen
          lastP <- readBuf pb newLen
          lastV <- readBuf vb newLen
          siftDownIndexed sp hi pb vb newLen 0 lastId lastP lastV
        onMin minId minP minV
    )

-- | Contiguous non-destructive stream over packed heap entries @(id, prio, val)@.
toStream ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  Stream m (e Int64, Elem e p, Elem e v)
toStream (IndexedPQ hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (ipqsLen f))
            <*> peek (getPtrOf (ipqsHeapIds f))
            <*> peekBufStruct (getPtrOf (ipqsPrios f))
            <*> peekBufStruct (getPtrOf (ipqsVals f))
        )
        $ \(n, hi, pb, vb) ->
          streamM (rangeS 0 n) $ \d ->
            (,,) <$> peekElemOff hi d <*> readBuf pb d <*> readBuf vb d

-- | Destructive ascending-priority stream popping @(id, prio, val)@ until empty.
popAllS ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  IndexedPQ e p v ->
  Stream m (e Int64, Elem e p, Elem e v)
popAllS pq =
  Stream $ pure $ ContT $ \kStep ->
    withPopMin pq (kStep Stop) (\i p v -> kStep (Yield (i, p, v)))

drainToIndexedPQ ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  Drain m (e Int64, Elem e p, Elem e v) (IndexedPQ e p v)
drainToIndexedPQ alloc = Drain $ \s -> do
  pq <- new alloc
  runDrain (drainAppendIndexedPQ alloc pq) s
  pure pq

fromStream ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  Stream m (e Int64, Elem e p, Elem e v) ->
  m (IndexedPQ e p v)
fromStream alloc = runDrain (drainToIndexedPQ alloc)

drainAppendIndexedPQ ::
  forall p v m e.
  (IndexedPQCtx m e p v) =>
  Alloc e ->
  IndexedPQ e p v ->
  Drain m (e Int64, Elem e p, Elem e v) ()
drainAppendIndexedPQ alloc pq =
  drainForEach (\(i, p, v) -> insertOrUpdate alloc pq i p v)

--------------------------------------------------------------------------------
-- SPMD / SIMD Gang Kernels & Batch Handle Membership Over IndexedPQ
--------------------------------------------------------------------------------

-- | Mutate payload values in-place across @[0 .. size - 1]@ in SIMD without disturbing heap priorities or handle indices.
foreachValuesSPMD ::
  forall w p v m e.
  (IndexedPQCtx m e p v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  IndexedPQ e p v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (IndexedPQ hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (ipqsLen f))
  vb <- peekBufStruct (getPtrOf (ipqsVals f))
  foreachBufSPMD pw True vb n step

-- | Masked-safe 64B-aligned SIMD fold over packed heap priorities @[0 .. size - 1]@.
foldPrioritiesSPMD ::
  forall w p v s m e.
  (IndexedPQCtx m e p v, LayoutUnitSPMD w m e p, SPMDTyped w e s) =>
  Proxy w ->
  IndexedPQ e p v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) p -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldPrioritiesSPMD pw (IndexedPQ hdr) s0 step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (ipqsLen f))
  pb <- peekBufStruct (getPtrOf (ipqsPrios f))
  foldBufSPMD pw True pb n s0 step

-- | Bounds-masked Briggs-Torczon SIMD handle membership query across a gang of @ids@.
containsIdsSPMD ::
  forall w p v m e.
  (IndexedPQCtx m e p v, SPMDGang w m e) =>
  IndexedPQ e p v ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Bool)
containsIdsSPMD (IndexedPQ hdr) ids = do
  let f = projectPtrs hdr
  uCap <- liftSPMD (peek (getPtrOf (ipqsUniverseCap f)))
  curLen <- liftSPMD (peek (getPtrOf (ipqsLen f)))
  okRef <- newRef false
  whenM_ ((uniform curLen `gt` 0) `land` (ids `ge` 0) `land` (ids `lt` uniform uCap)) $ do
    sp <- liftSPMD (peek (getPtrOf (ipqsSparsePos f)))
    pos <- peekElemOff (uniformAlignedPtr sp) ids
    whenM_ ((pos `ge` 0) `land` (pos `lt` uniform curLen)) $ do
      hi <- liftSPMD (peek (getPtrOf (ipqsHeapIds f)))
      backId <- peekElemOff (uniformAlignedPtr hi) pos
      writeRef okRef (backId `eq` ids)
  readRef okRef

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type IndexedPQABI p v = (Typeable p, Typeable v, JanusStruct (Buf p), JanusStruct (Buf v))

type instance JanusCHost (IndexedPQ (JanusCT _) p v) = IndexedPQ Identity p v

deriving via
  ViaPtrHandle t (CStruct (IndexedPQSpine p v)) (IndexedPQ Identity p v)
  instance
    (IndexedPQABI p v) => JanusCArg t (IndexedPQ (JanusCT t) p v)

deriving via
  ViaPtrHandle t (CStruct (IndexedPQSpine p v)) (IndexedPQ Identity p v)
  instance
    (IndexedPQABI p v) => JanusCValRet t (IndexedPQ (JanusCT t) p v)
