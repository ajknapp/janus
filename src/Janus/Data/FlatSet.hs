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

module Janus.Data.FlatSet
  ( -- * Contiguous Sorted FlatSet Spine & Handle
    FlatSetSpine (..),
    FlatSet (..),
    FlatSetCtx,

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

    -- * Ordered Operations & Neighbour Queries
    insert,
    member,
    delete,
    lookupGE,
    withLookupGE,
    lookupLE,
    withLookupLE,
    minKey,
    maxKey,
    keyAt,

    -- * Sorted Streams, SPMD Kernels, Range Slices, Linear Merge Set-Algebra, & Bulk Drains
    stream,
    reverseStream,
    traverseSPMD_,
    foldSPMD,
    lowerBoundBatchSPMD,
    memberBatchSPMD,
    rangeStream,
    unionS,
    intersectionS,
    differenceS,
    drainToFlatSet,
    fromStream,
    drainAppendFlatSet,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Ref (RefCell, letM, newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFind, drainFoldM, drainForEach, drain_, runDrain)
import Janus.Control.Stream
  ( Stream,
    differenceSortedByS,
    intersectSortedByS,
    iterateS,
    rangeS,
    reverseRangeS,
    streamM,
    unfoldWhileM,
    unionSortedByS,
    withStreamSetup,
  )
import Janus.Data.FlatMap (OrdKey (..), OrdLayoutCtx)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (lowerBoundBuf, sortBufBy, upperBoundBuf)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    uniform,
  )

data FlatSetSpine k f = FlatSetSpine
  { fssKeys :: !(f (CStruct (Buf k))),
    fssLen :: !(f Int64),
    fssCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, JanusStruct (Buf k)) => JanusStruct (FlatSetSpine k)

newtype FlatSet (e :: K.Type -> K.Type) k = FlatSet
  { unFlatSet :: e (Ptr (CStruct (FlatSetSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (FlatSet Identity k)

deriving stock instance Show (FlatSet Identity k)

type FlatSetCtx m e k = OrdLayoutCtx m e k

new ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  m (FlatSet e k)
new alloc = do
  hdr <- allocStruct @(CStruct (FlatSetSpine k)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  pokeBufStruct (getPtrOf (fssKeys f)) k0
  poke (getPtrOf (fssLen f)) 0
  poke (getPtrOf (fssCap f)) 0
  pure (FlatSet hdr)

withCapacity ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (FlatSet e k)
withCapacity alloc initCap = do
  fs <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc fs initCap
  pure fs

free ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  FlatSet e k ->
  m ()
free alloc (FlatSet hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (fssCap f))
  whenM_ (cap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (fssKeys f))
    freeBuf alloc kb cap
  freeStruct alloc hdr

size :: (FlatSetCtx m e k) => FlatSet e k -> m (e Int64)
size (FlatSet hdr) = peek (getPtrOf (fssLen (projectPtrs hdr)))

capacity :: (FlatSetCtx m e k) => FlatSet e k -> m (e Int64)
capacity (FlatSet hdr) = peek (getPtrOf (fssCap (projectPtrs hdr)))

isEmpty :: (FlatSetCtx m e k) => FlatSet e k -> m (e Bool)
isEmpty fs = (`eq` 0) <$> size fs

clear :: (FlatSetCtx m e k) => FlatSet e k -> m ()
clear (FlatSet hdr) = poke (getPtrOf (fssLen (projectPtrs hdr))) 0

reserve ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  FlatSet e k ->
  e Int64 ->
  m ()
reserve alloc (FlatSet hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  curCap <- peek (getPtrOf (fssCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    (_, newCap) <-
      runDrain
        (drainFind 8 (`ge` needed))
        (iterateS (ifThenElse (curCap `lt` 8) 8 (curCap * 2)) (* 2))
    oldK <- peekBufStruct (getPtrOf (fssKeys f))
    newK <- reallocBuf alloc oldK curCap newCap curLen
    pokeBufStruct (getPtrOf (fssKeys f)) newK
    poke (getPtrOf (fssCap f)) newCap

findPos ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Elem e k ->
  m (e Bool, e Int64)
findPos (FlatSet hdr) key = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen key
  foundRef <- newRef false
  whenM_ (pos `lt` curLen) $ do
    cand <- readBuf kb pos
    writeRef foundRef (eqKey @e @k cand key)
  found <- readRef foundRef
  pure (found, pos)

-- | Insert @key@ maintaining sorted order. Returns @true@ iff @key@ was newly inserted.
insert ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  FlatSet e k ->
  Elem e k ->
  m (e Bool)
insert alloc fs@(FlatSet hdr) key = do
  (found, pos) <- findPos fs key
  insertedRef <- newRef false
  whenM_ (lnot found) $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (fssLen f))
    curCap <- peek (getPtrOf (fssCap f))
    whenM_ (curLen `eq` curCap) $
      reserve alloc fs 1
    kb <- peekBufStruct (getPtrOf (fssKeys f))
    let tailCnt = curLen - pos
    whenM_ (tailCnt `gt` 0) $
      moveBuf kb (pos + 1) pos tailCnt
    writeBuf kb pos key
    poke (getPtrOf (fssLen f)) (curLen + 1)
    writeRef insertedRef true
  readRef insertedRef

member :: (FlatSetCtx m e k) => FlatSet e k -> Elem e k -> m (e Bool)
member fs key = fst <$> findPos fs key

delete ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Elem e k ->
  m (e Bool)
delete fs@(FlatSet hdr) key = do
  (found, pos) <- findPos fs key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (fssLen f))
    kb <- peekBufStruct (getPtrOf (fssKeys f))
    let tailCnt = curLen - pos - 1
    whenM_ (tailCnt `gt` 0) $
      moveBuf kb pos (pos + 1) tailCnt
    poke (getPtrOf (fssLen f)) (curLen - 1)
  pure found

keyAt :: (FlatSetCtx m e k) => FlatSet e k -> e Int64 -> m (Elem e k)
keyAt (FlatSet hdr) idx = do
  kb <- peekBufStruct (getPtrOf (fssKeys (projectPtrs hdr)))
  readBuf kb idx

lookupGE ::
  forall k ks m e.
  (FlatSetCtx m e k, Elem e k ~ e ks, KnownJanusType e ks) =>
  e ks ->
  FlatSet e k ->
  e ks ->
  m (e Bool, e ks)
lookupGE defK (FlatSet hdr) target = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen target
  okRef <- newRef false
  kRef <- newRef defK
  whenM_ (pos `lt` curLen) $ do
    readBuf kb pos >>= writeRef kRef
    writeRef okRef true
  (,) <$> readRef okRef <*> readRef kRef

withLookupGE ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Elem e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withLookupGE (FlatSet hdr) target onMiss onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen target
  ifThenElseM_
    (pos `lt` curLen)
    (readBuf kb pos >>= onHit)
    onMiss

lookupLE ::
  forall k ks m e.
  (FlatSetCtx m e k, Elem e k ~ e ks, KnownJanusType e ks) =>
  e ks ->
  FlatSet e k ->
  e ks ->
  m (e Bool, e ks)
lookupLE defK (FlatSet hdr) target = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  ub <- upperBoundBuf (ltKey @e @k) kb curLen target
  okRef <- newRef false
  kRef <- newRef defK
  whenM_ (ub `gt` 0) $ do
    readBuf kb (ub - 1) >>= writeRef kRef
    writeRef okRef true
  (,) <$> readRef okRef <*> readRef kRef

withLookupLE ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Elem e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withLookupLE (FlatSet hdr) target onMiss onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  ub <- upperBoundBuf (ltKey @e @k) kb curLen target
  ifThenElseM_
    (ub `gt` 0)
    (readBuf kb (ub - 1) >>= onHit)
    onMiss

minKey ::
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
minKey fs onEmpty onHit = do
  n <- size fs
  ifThenElseM_ (n `le` 0) onEmpty (keyAt fs 0 >>= onHit)

maxKey ::
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
maxKey fs onEmpty onHit = do
  n <- size fs
  ifThenElseM_ (n `le` 0) onEmpty (keyAt fs (n - 1) >>= onHit)

stream ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Stream m (Elem e k)
stream (FlatSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (fssLen f)) <*> peekBufStruct (getPtrOf (fssKeys f)))
        (\(n, kb) -> streamM (rangeS 0 n) (readBuf kb))

reverseStream ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Stream m (Elem e k)
reverseStream (FlatSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (fssLen f)) <*> peekBufStruct (getPtrOf (fssKeys f)))
        (\(n, kb) -> streamM (reverseRangeS 0 n) (readBuf kb))

-- | Layout-polymorphic read-only unit-stride SIMD traversal over sorted keys @[0, size)@.
traverseSPMD_ ::
  forall w k m e.
  (FlatSetCtx m e k, LayoutUnitSPMD w m e k) =>
  Proxy w ->
  FlatSet e k ->
  (SPMD w e Int64 -> Elem (SPMD w e) k -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (FlatSet hdr) body = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  traverseBufSPMD_ pw True kb n body

-- | Layout-polymorphic masked-safe unit-stride SIMD fold over sorted keys @[0, size)@.
foldSPMD ::
  forall w k s m e.
  (FlatSetCtx m e k, LayoutUnitSPMD w m e k, SPMDTyped w e s) =>
  Proxy w ->
  FlatSet e k ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) k -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw (FlatSet hdr) s0 step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (fssLen f))
  kb <- peekBufStruct (getPtrOf (fssKeys f))
  foldBufSPMD pw True kb n s0 step

-- | Branchless lane-parallel monobound SIMD binary search returning the lower-bound index
-- in @[0, size]@ for each of @w@ query keys in lockstep.
lowerBoundBatchSPMD ::
  forall w k m e.
  (FlatSetCtx m e k, SPMDGang w m e, OrdKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  Proxy w ->
  FlatSet e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Int64)
lowerBoundBatchSPMD _ (FlatSet hdr) qKeys = do
  let f = projectPtrs hdr
  n <- liftSPMD (peek (getPtrOf (fssLen f)))
  resRef <- newRef (0 :: SPMD w e Int64)
  whenM_ (uniform n `gt` 0) $ do
    kb <- liftSPMD (peekBufStruct (getPtrOf (fssKeys f)))
    baseRef <- newRef (0 :: SPMD w e Int64)
    runDrain drain_ $
      unfoldWhileM (uniform n) (\remL -> pure (remL `gt` 1)) $ \remL -> do
        base <- readRef baseRef
        let half = remL `rshift` 1
            mid = base + half
        mk <- gatherBufSPMD kb mid
        writeRef baseRef (ifThenElse (ltKey @(SPMD w e) @k mk qKeys) mid base)
        pure (remL - half, ())
    finalBase <- readRef baseRef
    vk <- gatherBufSPMD kb finalBase
    writeRef resRef (ifThenElse (ltKey @(SPMD w e) @k vk qKeys) (finalBase + 1) finalBase)
  readRef resRef

-- | Lane-parallel SIMD batch membership query via branchless binary search over @w@ keys.
memberBatchSPMD ::
  forall w k m e.
  (FlatSetCtx m e k, SPMDGang w m e, OrdKey (SPMD w e) k, LayoutGatherSPMD w m e k) =>
  Proxy w ->
  FlatSet e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
memberBatchSPMD pw fs@(FlatSet hdr) qKeys = do
  let f = projectPtrs hdr
  n <- liftSPMD (peek (getPtrOf (fssLen f)))
  pos <- lowerBoundBatchSPMD pw fs qKeys
  foundRef <- newRef false
  whenM_ (pos `lt` uniform n) $ do
    kb <- liftSPMD (peekBufStruct (getPtrOf (fssKeys f)))
    ck <- gatherBufSPMD kb pos
    writeRef foundRef (eqKey @(SPMD w e) @k ck qKeys)
  readRef foundRef

rangeStream ::
  forall k m e.
  (FlatSetCtx m e k) =>
  FlatSet e k ->
  Elem e k ->
  Elem e k ->
  Stream m (Elem e k)
rangeStream (FlatSet hdr) lo hi =
  let f = projectPtrs hdr
   in withStreamSetup
        ( do
            n <- peek (getPtrOf (fssLen f))
            kb <- peekBufStruct (getPtrOf (fssKeys f))
            startIdx <- lowerBoundBuf (ltKey @e @k) kb n lo
            endIdx <- upperBoundBuf (ltKey @e @k) kb n hi
            pure (startIdx, endIdx, kb)
        )
        (\(startIdx, endIdx, kb) -> streamM (rangeS startIdx endIdx) (readBuf kb))

unionS ::
  forall k m e.
  (FlatSetCtx m e k, RefCell m e (Elem e k)) =>
  FlatSet e k ->
  FlatSet e k ->
  Stream m (Elem e k)
unionS a b =
  unionSortedByS (ltKey @e @k) (eqKey @e @k) const (stream a) (stream b)

intersectionS ::
  forall k m e.
  (FlatSetCtx m e k, RefCell m e (Elem e k)) =>
  FlatSet e k ->
  FlatSet e k ->
  Stream m (Elem e k)
intersectionS a b =
  intersectSortedByS (ltKey @e @k) (eqKey @e @k) const (stream a) (stream b)

differenceS ::
  forall k m e.
  (FlatSetCtx m e k, RefCell m e (Elem e k)) =>
  FlatSet e k ->
  FlatSet e k ->
  Stream m (Elem e k)
differenceS a b =
  differenceSortedByS (ltKey @e @k) (eqKey @e @k) (stream a) (stream b)

-- | O(N log N) bulk construction: append unsorted stream, sort in place, and deduplicate in O(N).
drainToFlatSet ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (FlatSet e k)
drainToFlatSet alloc = Drain $ \s -> do
  fs@(FlatSet hdr) <- new alloc
  let f = projectPtrs hdr
  runDrain
    ( drainForEach $ \kIn -> do
        curLen <- peek (getPtrOf (fssLen f))
        curCap <- peek (getPtrOf (fssCap f))
        whenM_ (curLen `eq` curCap) $
          reserve alloc fs 1
        kb <- peekBufStruct (getPtrOf (fssKeys f))
        writeBuf kb curLen kIn
        poke (getPtrOf (fssLen f)) (curLen + 1)
    )
    s
  rawLen <- peek (getPtrOf (fssLen f))
  whenM_ (rawLen `gt` 1) $ do
    kb <- peekBufStruct (getPtrOf (fssKeys f))
    sortBufBy (ltKey @e @k) kb rawLen
    finalLast <-
      runDrain
        ( drainFoldM (0 :: e Int64) $ \wIdx readIdx -> do
            prevK <- readBuf kb wIdx
            curK <- readBuf kb readIdx
            outRef <- newRef wIdx
            whenM_ (lnot (eqKey @e @k prevK curK)) $ do
              let wNext = wIdx + 1
              whenM_ (wNext `ne` readIdx) $
                writeBuf kb wNext curK
              writeRef outRef wNext
            readRef outRef
        )
        (rangeS 1 rawLen)
    poke (getPtrOf (fssLen f)) (finalLast + 1)
  pure fs

fromStream ::
  forall k m e.
  (FlatSetCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (FlatSet e k)
fromStream alloc = runDrain (drainToFlatSet alloc)

-- | Append/insert all elements yielded by a 'Stream' into an existing 'FlatSet'.
drainAppendFlatSet ::
  (FlatSetCtx m e k) =>
  Alloc e ->
  FlatSet e k ->
  Drain m (Elem e k) ()
drainAppendFlatSet alloc fs = drainForEach (\x -> insert alloc fs x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type FlatSetABI k = (Typeable k, JanusStruct (Buf k))

type instance JanusCHost (FlatSet (JanusCT _) k) = FlatSet Identity k

deriving via
  ViaPtrHandle t (CStruct (FlatSetSpine k)) (FlatSet Identity k)
  instance
    (FlatSetABI k) => JanusCArg t (FlatSet (JanusCT t) k)

deriving via
  ViaPtrHandle t (CStruct (FlatSetSpine k)) (FlatSet Identity k)
  instance
    (FlatSetABI k) => JanusCValRet t (FlatSet (JanusCT t) k)
