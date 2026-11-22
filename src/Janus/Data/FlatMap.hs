{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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

module Janus.Data.FlatMap
  ( -- * Ordered Key Class (Supporting Scalars & Lexicographic HKD Records)
    OrdKey (..),
    GOrdStruct (..),

    -- * Contiguous SoA Sorted FlatMap Spine & Handle
    FlatMapSpine (..),
    FlatMap (..),
    FlatMapCtx,

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

    -- * Ordered Lookup, Insertion, & Deletion
    insert,
    insertWith,
    lookup,
    withLookup,
    lookupGE,
    withLookupGE,
    lookupLE,
    withLookupLE,
    minEntry,
    maxEntry,
    member,
    delete,
    entryAt,

    -- * Contiguous Sorted Streams, Range Queries, Set-Algebra Merges, & Bulk Drains
    toStream,
    reverseStream,
    keysS,
    valuesS,
    rangeStream,
    unionWithS,
    intersectWithS,
    differenceS,
    drainToFlatMap,
    fromStream,
    drainAppendFlatMap,
    drainAppendFlatMapWith,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpSized (..))
import Janus.Command.Ref (RefCell, letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainFoldM, drainForEach, runDrain)
import Janus.Control.Stream
  ( Stream,
    differenceSortedByS,
    intersectSortedByS,
    iterateS,
    rangeS,
    reverseRangeS,
    streamM,
    unionSortedByS,
    withStreamSetup,
  )
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (lowerBoundBuf, stableSortKeyValBufsBy, upperBoundBuf)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- OrdKey Class (Branchless Strict-Weak Ordering Over Scalars & HKD Records)
--------------------------------------------------------------------------------

class OrdKey (e :: K.Type -> K.Type) k where
  ltKey :: Elem e k -> Elem e k -> e Bool
  eqKey :: Elem e k -> Elem e k -> e Bool

#define INST_ORD_SCALAR(TY) \
instance (ExpOrd e (TY)) => OrdKey e (TY) where { \
  ltKey = lt; \
  eqKey = eq \
}

INST_ORD_SCALAR(Bool)
INST_ORD_SCALAR(Int8)
INST_ORD_SCALAR(Int16)
INST_ORD_SCALAR(Int32)
INST_ORD_SCALAR(Int64)
INST_ORD_SCALAR(Word8)
INST_ORD_SCALAR(Word16)
INST_ORD_SCALAR(Word32)
INST_ORD_SCALAR(Word64)
INST_ORD_SCALAR(Float)
INST_ORD_SCALAR(Double)
INST_ORD_SCALAR(CChar)
INST_ORD_SCALAR(CSChar)
INST_ORD_SCALAR(CUChar)
INST_ORD_SCALAR(CShort)
INST_ORD_SCALAR(CUShort)
INST_ORD_SCALAR(CInt)
INST_ORD_SCALAR(CUInt)
INST_ORD_SCALAR(CLong)
INST_ORD_SCALAR(CULong)
INST_ORD_SCALAR(CLLong)
INST_ORD_SCALAR(CULLong)
INST_ORD_SCALAR(CSize)
INST_ORD_SCALAR(CFloat)
INST_ORD_SCALAR(CDouble)

class GOrdStruct (e :: K.Type -> K.Type) (rep :: K.Type -> K.Type) where
  gLtStruct :: rep p -> rep p -> e Bool
  gEqStruct :: rep p -> rep p -> e Bool

instance (GOrdStruct e f) => GOrdStruct e (M1 i c f) where
  gLtStruct (M1 x) (M1 y) = gLtStruct @e x y
  gEqStruct (M1 x) (M1 y) = gEqStruct @e x y

instance (ExpBool e, GOrdStruct e f, GOrdStruct e g) => GOrdStruct e (f :*: g) where
  gLtStruct (fx :*: gx) (fy :*: gy) =
    gLtStruct @e fx fy `lor` (gEqStruct @e fx fy `land` gLtStruct @e gx gy)
  gEqStruct (fx :*: gx) (fy :*: gy) =
    gEqStruct @e fx fy `land` gEqStruct @e gx gy

instance (ExpOrd e a) => GOrdStruct e (K1 i (e a)) where
  gLtStruct (K1 x) (K1 y) = x `lt` y
  gEqStruct (K1 x) (K1 y) = x `eq` y

instance (Generic (sub e), GOrdStruct e (Rep (sub e))) => GOrdStruct e (K1 i (sub e)) where
  gLtStruct (K1 x) (K1 y) = gLtStruct @e (from x) (from y)
  gEqStruct (K1 x) (K1 y) = gEqStruct @e (from x) (from y)

instance (Generic (s e), GOrdStruct e (Rep (s e))) => OrdKey e (AoS s) where
  ltKey x y = gLtStruct @e (from x) (from y)
  eqKey x y = gEqStruct @e (from x) (from y)

instance (Generic (s e), GOrdStruct e (Rep (s e))) => OrdKey e (SoA s) where
  ltKey x y = gLtStruct @e (from x) (from y)
  eqKey x y = gEqStruct @e (from x) (from y)

instance (Generic (s e), GOrdStruct e (Rep (s e))) => OrdKey e (AoSoA w s) where
  ltKey x y = gLtStruct @e (from x) (from y)
  eqKey x y = gEqStruct @e (from x) (from y)

instance (Generic (outer e), GOrdStruct e (Rep (outer e))) => OrdKey e (SOAOA w outer) where
  ltKey x y = gLtStruct @e (from x) (from y)
  eqKey x y = gEqStruct @e (from x) (from y)

--------------------------------------------------------------------------------
-- FlatMap Spine & Handle
--------------------------------------------------------------------------------

-- | Separate sorted @keys@ and @vals@ buffers so binary search touches ONLY @keys@
-- (maximizing keys per 64-byte cache line without value pollution).
data FlatMapSpine k v f = FlatMapSpine
  { fmsKeys :: !(f (CStruct (Buf k))),
    fmsVals :: !(f (CStruct (Buf v))),
    fmsLen :: !(f Int64),
    fmsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) => JanusStruct (FlatMapSpine k v)

newtype FlatMap (e :: K.Type -> K.Type) k v = FlatMap
  { unFlatMap :: e (Ptr (CStruct (FlatMapSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (FlatMap Identity k v)

deriving stock instance Show (FlatMap Identity k v)

type FlatMapCtx m e k v =
  ( LayoutCtx m e k,
    LayoutCtx m e v,
    OrdKey e k,
    Typeable k,
    Typeable v,
    ExpSized e (CStruct (FlatMapSpine k v)),
    JanusTyped e (CStruct (FlatMapSpine k v))
  )

new ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  m (FlatMap e k v)
new alloc = do
  hdr <- allocStruct @(CStruct (FlatMapSpine k v)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  v0 <- allocBuf @v alloc 0
  pokeBufStruct (getPtrOf (fmsKeys f)) k0
  pokeBufStruct (getPtrOf (fmsVals f)) v0
  poke (getPtrOf (fmsLen f)) 0
  poke (getPtrOf (fmsCap f)) 0
  pure (FlatMap hdr)

withCapacity ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  e Int64 ->
  m (FlatMap e k v)
withCapacity alloc initCap = do
  fm <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc fm initCap
  pure fm

free ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  FlatMap e k v ->
  m ()
free alloc (FlatMap hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (fmsCap f))
  whenM_ (cap `gt` 0) $ do
    kb <- peekBufStruct (getPtrOf (fmsKeys f))
    vb <- peekBufStruct (getPtrOf (fmsVals f))
    freeBuf alloc kb cap
    freeBuf alloc vb cap
  freeStruct alloc hdr

size :: (FlatMapCtx m e k v) => FlatMap e k v -> m (e Int64)
size (FlatMap hdr) = peek (getPtrOf (fmsLen (projectPtrs hdr)))

capacity :: (FlatMapCtx m e k v) => FlatMap e k v -> m (e Int64)
capacity (FlatMap hdr) = peek (getPtrOf (fmsCap (projectPtrs hdr)))

isEmpty :: (FlatMapCtx m e k v) => FlatMap e k v -> m (e Bool)
isEmpty fm = (`eq` 0) <$> size fm

clear :: (FlatMapCtx m e k v) => FlatMap e k v -> m ()
clear (FlatMap hdr) = poke (getPtrOf (fmsLen (projectPtrs hdr))) 0

reserve ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  FlatMap e k v ->
  e Int64 ->
  m ()
reserve alloc (FlatMap hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  curCap <- peek (getPtrOf (fmsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    (_, newCap) <-
      runDrain
        (drainFind 8 (`ge` needed))
        (iterateS (ifThenElse (curCap `lt` 8) 8 (curCap * 2)) (* 2))
    oldK <- peekBufStruct (getPtrOf (fmsKeys f))
    oldV <- peekBufStruct (getPtrOf (fmsVals f))
    newK <- reallocBuf alloc oldK curCap newCap curLen
    newV <- reallocBuf alloc oldV curCap newCap curLen
    pokeBufStruct (getPtrOf (fmsKeys f)) newK
    pokeBufStruct (getPtrOf (fmsVals f)) newV
    poke (getPtrOf (fmsCap f)) newCap

findPos ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  m (e Bool, e Int64)
findPos (FlatMap hdr) key = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen key
  foundRef <- newRef false
  whenM_ (pos `lt` curLen) $ do
    cand <- readBuf kb pos
    writeRef foundRef (eqKey @e @k cand key)
  found <- readRef foundRef
  pure (found, pos)

insert ::
  (FlatMapCtx m e k v) =>
  Alloc e ->
  FlatMap e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert alloc fm key val = insertWith alloc fm (\_ newV -> newV) key val

insertWith ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  FlatMap e k v ->
  (Elem e v -> Elem e v -> Elem e v) ->
  Elem e k ->
  Elem e v ->
  m ()
insertWith alloc fm@(FlatMap hdr) combine key val = do
  (found, pos) <- findPos fm key
  let f = projectPtrs hdr
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (fmsVals f))
        oldV <- readBuf vb pos
        writeBuf vb pos (combine oldV val)
    )
    ( do
        curLen <- peek (getPtrOf (fmsLen f))
        curCap <- peek (getPtrOf (fmsCap f))
        whenM_ (curLen `eq` curCap) $
          reserve alloc fm 1
        kb <- peekBufStruct (getPtrOf (fmsKeys f))
        vb <- peekBufStruct (getPtrOf (fmsVals f))
        let tailCnt = curLen - pos
        whenM_ (tailCnt `gt` 0) $ do
          moveBuf kb (pos + 1) pos tailCnt
          moveBuf vb (pos + 1) pos tailCnt
        writeBuf kb pos key
        writeBuf vb pos val
        poke (getPtrOf (fmsLen f)) (curLen + 1)
    )

member :: (FlatMapCtx m e k v) => FlatMap e k v -> Elem e k -> m (e Bool)
member fm key = fst <$> findPos fm key

lookup ::
  forall k v s m e.
  (FlatMapCtx m e k v, Elem e v ~ e s, JanusTyped e s) =>
  e s ->
  FlatMap e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal fm@(FlatMap hdr) key = do
  (found, pos) <- findPos fm key
  valRef <- newRef defVal
  whenM_ found $ do
    vb <- peekBufStruct (getPtrOf (fmsVals (projectPtrs hdr)))
    readBuf vb pos >>= writeRef valRef
  val <- readRef valRef
  pure (found, val)

withLookup ::
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup fm@(FlatMap hdr) key onMiss onHit = do
  (found, pos) <- findPos fm key
  ifThenElseM_
    found
    ( do
        vb <- peekBufStruct (getPtrOf (fmsVals (projectPtrs hdr)))
        readBuf vb pos >>= onHit
    )
    onMiss

lookupGE ::
  forall k v ks vs m e.
  (FlatMapCtx m e k v, Elem e k ~ e ks, Elem e v ~ e vs, JanusTyped e ks, JanusTyped e vs) =>
  e ks ->
  e vs ->
  FlatMap e k v ->
  e ks ->
  m (e Bool, e ks, e vs)
lookupGE defK defV (FlatMap hdr) target = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen target
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  whenM_ (pos `lt` curLen) $ do
    vb <- peekBufStruct (getPtrOf (fmsVals f))
    readBuf kb pos >>= writeRef kRef
    readBuf vb pos >>= writeRef vRef
    writeRef okRef true
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

withLookupGE ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withLookupGE (FlatMap hdr) target onMiss onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  pos <- lowerBoundBuf (ltKey @e @k) kb curLen target
  ifThenElseM_
    (pos `lt` curLen)
    ( do
        vb <- peekBufStruct (getPtrOf (fmsVals f))
        kOut <- readBuf kb pos
        vOut <- readBuf vb pos
        onHit kOut vOut
    )
    onMiss

lookupLE ::
  forall k v ks vs m e.
  (FlatMapCtx m e k v, Elem e k ~ e ks, Elem e v ~ e vs, JanusTyped e ks, JanusTyped e vs) =>
  e ks ->
  e vs ->
  FlatMap e k v ->
  e ks ->
  m (e Bool, e ks, e vs)
lookupLE defK defV (FlatMap hdr) target = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  ub <- upperBoundBuf (ltKey @e @k) kb curLen target
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  whenM_ (ub `gt` 0) $ do
    let idx = ub - 1
    vb <- peekBufStruct (getPtrOf (fmsVals f))
    readBuf kb idx >>= writeRef kRef
    readBuf vb idx >>= writeRef vRef
    writeRef okRef true
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

withLookupLE ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withLookupLE (FlatMap hdr) target onMiss onHit = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (fmsLen f))
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  ub <- upperBoundBuf (ltKey @e @k) kb curLen target
  ifThenElseM_
    (ub `gt` 0)
    ( do
        let idx = ub - 1
        vb <- peekBufStruct (getPtrOf (fmsVals f))
        kOut <- readBuf kb idx
        vOut <- readBuf vb idx
        onHit kOut vOut
    )
    onMiss

entryAt ::
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  e Int64 ->
  m (Elem e k, Elem e v)
entryAt (FlatMap hdr) idx = do
  let f = projectPtrs hdr
  kb <- peekBufStruct (getPtrOf (fmsKeys f))
  vb <- peekBufStruct (getPtrOf (fmsVals f))
  (,) <$> readBuf kb idx <*> readBuf vb idx

minEntry ::
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
minEntry fm onEmpty onHit = do
  n <- size fm
  ifThenElseM_
    (n `le` 0)
    onEmpty
    (entryAt fm 0 >>= uncurry onHit)

maxEntry ::
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
maxEntry fm onEmpty onHit = do
  n <- size fm
  ifThenElseM_
    (n `le` 0)
    onEmpty
    (entryAt fm (n - 1) >>= uncurry onHit)

delete ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  m (e Bool)
delete fm@(FlatMap hdr) key = do
  (found, pos) <- findPos fm key
  whenM_ found $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (fmsLen f))
    kb <- peekBufStruct (getPtrOf (fmsKeys f))
    vb <- peekBufStruct (getPtrOf (fmsVals f))
    let tailCnt = curLen - pos - 1
    whenM_ (tailCnt `gt` 0) $ do
      moveBuf kb pos (pos + 1) tailCnt
      moveBuf vb pos (pos + 1) tailCnt
    poke (getPtrOf (fmsLen f)) (curLen - 1)
  pure found

toStream ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Stream m (Elem e k, Elem e v)
toStream (FlatMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (fmsLen f)) <*> peekBufStruct (getPtrOf (fmsKeys f)) <*> peekBufStruct (getPtrOf (fmsVals f)))
        (\(n, kb, vb) -> streamM (rangeS 0 n) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

reverseStream ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Stream m (Elem e k, Elem e v)
reverseStream (FlatMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (fmsLen f)) <*> peekBufStruct (getPtrOf (fmsKeys f)) <*> peekBufStruct (getPtrOf (fmsVals f)))
        (\(n, kb, vb) -> streamM (reverseRangeS 0 n) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

keysS :: (FlatMapCtx m e k v) => FlatMap e k v -> Stream m (Elem e k)
keysS (FlatMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (fmsLen f)) <*> peekBufStruct (getPtrOf (fmsKeys f)))
        (\(n, kb) -> streamM (rangeS 0 n) (readBuf kb))

valuesS :: (FlatMapCtx m e k v) => FlatMap e k v -> Stream m (Elem e v)
valuesS (FlatMap hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (fmsLen f)) <*> peekBufStruct (getPtrOf (fmsVals f)))
        (\(n, vb) -> streamM (rangeS 0 n) (readBuf vb))

-- | Contiguous range slice over @[lo, hi]@ (inclusive) using two O(log N) branchless searches.
rangeStream ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  FlatMap e k v ->
  Elem e k ->
  Elem e k ->
  Stream m (Elem e k, Elem e v)
rangeStream (FlatMap hdr) lo hi =
  let f = projectPtrs hdr
   in withStreamSetup
        ( do
            n <- peek (getPtrOf (fmsLen f))
            kb <- peekBufStruct (getPtrOf (fmsKeys f))
            vb <- peekBufStruct (getPtrOf (fmsVals f))
            startIdx <- lowerBoundBuf (ltKey @e @k) kb n lo
            endIdx <- upperBoundBuf (ltKey @e @k) kb n hi
            pure (startIdx, endIdx, kb, vb)
        )
        (\(startIdx, endIdx, kb, vb) -> streamM (rangeS startIdx endIdx) (\i -> (,) <$> readBuf kb i <*> readBuf vb i))

unionWithS ::
  forall k v m e.
  (FlatMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  (Elem e v -> Elem e v -> Elem e v) ->
  FlatMap e k v ->
  FlatMap e k v ->
  Stream m (Elem e k, Elem e v)
unionWithS combine a b =
  unionSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (\(ka, va) (_, vb) -> (ka, combine va vb))
    (toStream a)
    (toStream b)

intersectWithS ::
  forall k v m e.
  (FlatMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  (Elem e v -> Elem e v -> Elem e v) ->
  FlatMap e k v ->
  FlatMap e k v ->
  Stream m (Elem e k, Elem e v)
intersectWithS combine a b =
  intersectSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (\(ka, va) (_, vb) -> (ka, combine va vb))
    (toStream a)
    (toStream b)

differenceS ::
  forall k v m e.
  (FlatMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  FlatMap e k v ->
  FlatMap e k v ->
  Stream m (Elem e k, Elem e v)
differenceS a b =
  differenceSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (toStream a)
    (toStream b)

-- | O(N log N) bulk construction from an unsorted 'Stream': appends sequentially, sorts
-- @(keys, vals)@ in place via zero-allocation introsort, and deduplicates keeping the last value.
drainToFlatMap ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e v) (FlatMap e k v)
drainToFlatMap alloc = Drain $ \s -> do
  fm@(FlatMap hdr) <- new alloc
  let f = projectPtrs hdr
  runDrain
    ( drainForEach $ \(kIn, vIn) -> do
        curLen <- peek (getPtrOf (fmsLen f))
        curCap <- peek (getPtrOf (fmsCap f))
        whenM_ (curLen `eq` curCap) $
          reserve alloc fm 1
        kb <- peekBufStruct (getPtrOf (fmsKeys f))
        vb <- peekBufStruct (getPtrOf (fmsVals f))
        writeBuf kb curLen kIn
        writeBuf vb curLen vIn
        poke (getPtrOf (fmsLen f)) (curLen + 1)
    )
    s
  rawLen <- peek (getPtrOf (fmsLen f))
  whenM_ (rawLen `gt` 1) $ do
    kb <- peekBufStruct (getPtrOf (fmsKeys f))
    vb <- peekBufStruct (getPtrOf (fmsVals f))
    stableSortKeyValBufsBy alloc (ltKey @e @k) (eqKey @e @k) kb vb rawLen
    -- Linear-scan deduplication keeping last value per duplicate key run
    finalLast <-
      runDrain
        ( drainFoldM (0 :: e Int64) $ \wIdx readIdx -> do
            prevK <- readBuf kb wIdx
            curK <- readBuf kb readIdx
            curV <- readBuf vb readIdx
            outRef <- newRef wIdx
            ifThenElseM_
              (eqKey @e @k prevK curK)
              (writeBuf vb wIdx curV)
              ( do
                  let wNext = wIdx + 1
                  whenM_ (wNext `ne` readIdx) $ do
                    writeBuf kb wNext curK
                    writeBuf vb wNext curV
                  writeRef outRef wNext
              )
            readRef outRef
        )
        (rangeS 1 rawLen)
    poke (getPtrOf (fmsLen f)) (finalLast + 1)
  pure fm

fromStream ::
  forall k v m e.
  (FlatMapCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e v) ->
  m (FlatMap e k v)
fromStream alloc = runDrain (drainToFlatMap alloc)

-- | Append/insert all @(key, val)@ pairs yielded by a 'Stream' into an existing 'FlatMap'
-- (overwriting on duplicate keys).
drainAppendFlatMap ::
  (FlatMapCtx m e k v) =>
  Alloc e ->
  FlatMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendFlatMap alloc fm = drainForEach (\(k, v) -> insert alloc fm k v)

-- | Append/insert all @(key, val)@ pairs yielded by a 'Stream' into an existing 'FlatMap',
-- combining duplicate keys via @combine oldVal newVal@.
drainAppendFlatMapWith ::
  (FlatMapCtx m e k v) =>
  Alloc e ->
  (Elem e v -> Elem e v -> Elem e v) ->
  FlatMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendFlatMapWith alloc combine fm =
  drainForEach (\(k, v) -> insertWith alloc fm combine k v)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type FlatMapABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCEval (FlatMap JanusC k v -> r) = FlatMap Identity k v -> JanusCEval r

type instance JanusCRetType (FlatMap JanusC k v -> r) = JanusCRetType r

instance (FlatMapABI k v, JanusCParam r) => JanusCParam (FlatMap JanusC k v -> r) where
  jcparam = mkPtrHandleJCParam FlatMap

instance (FlatMapABI k v, JanusCEvaluate r) => JanusCEvaluate (FlatMap JanusC k v -> r) where
  jceval = mkPtrHandleJCEval unFlatMap FlatMap

type instance JanusCEval (JanusCM (FlatMap JanusC k v)) = IO (FlatMap Identity k v)

type instance JanusCRetType (JanusCM (FlatMap JanusC k v)) = FlatMap Identity k v

instance (FlatMapABI k v) => JanusCParam (JanusCM (FlatMap JanusC k v)) where
  jcparam = mkPtrHandleJCRetParam unFlatMap

instance (FlatMapABI k v) => JanusCEvaluate (JanusCM (FlatMap JanusC k v)) where
  jceval = mkPtrHandleJCRetEval FlatMap

type instance LinuxX86_64CEval (FlatMap LinuxX86_64C k v -> r) = FlatMap Identity k v -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (FlatMap LinuxX86_64C k v -> r) = LinuxX86_64BaseRet r

instance (FlatMapABI k v, LinuxX86_64Param r) => LinuxX86_64Param (FlatMap LinuxX86_64C k v -> r) where
  x86param = mkPtrHandleX86Param FlatMap

instance (FlatMapABI k v, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (FlatMap LinuxX86_64C k v -> r) where
  x86eval = mkPtrHandleX86EvalParam unFlatMap FlatMap

instance (FlatMapABI k v, X86DeferCont r) => X86DeferCall (FlatMap LinuxX86_64C k v -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unFlatMap FlatMap

instance (FlatMapABI k v, X86DeferCont r) => X86DeferCont (FlatMap LinuxX86_64C k v -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unFlatMap FlatMap

type instance LinuxX86_64CEval (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) = IO (FlatMap Identity k v)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) = FlatMap Identity k v

instance (FlatMapABI k v) => LinuxX86_64Param (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) where
  x86param = mkPtrHandleX86RetParam unFlatMap

instance (FlatMapABI k v) => LinuxX86_64Evaluate (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) where
  x86eval = mkPtrHandleX86RetEval FlatMap

instance (FlatMapABI k v) => X86DeferCall (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg FlatMap

instance (FlatMapABI k v) => X86DeferCont (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont FlatMap

instance (FlatMapABI k v, LinuxX86_64Param r, JanusCTH r) => JanusCTH (FlatMap LinuxX86_64C k v -> r) where
  type JanusCTHEval (FlatMap LinuxX86_64C k v -> r) = FlatMap Identity k v -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(FlatMap Identity k v)) : ps, retSpec)

instance (FlatMapABI k v) => JanusCTH (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) where
  type JanusCTHEval (LinuxX86_64CM (FlatMap LinuxX86_64C k v)) = IO (FlatMap Identity k v)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(FlatMap Identity k v)) (Proxy @(CStruct (FlatMapSpine k v))))
