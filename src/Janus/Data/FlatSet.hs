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

    -- * Sorted Streams, Range Slices, Linear Merge Set-Algebra, & Bulk Drains
    stream,
    reverseStream,
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
import Janus.Data.FlatMap (OrdKey (..))
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (lowerBoundBuf, sortBufBy, upperBoundBuf)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

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

type FlatSetCtx m e k =
  ( LayoutCtx m e k,
    OrdKey e k,
    Typeable k,
    ExpSized e (CStruct (FlatSetSpine k)),
    JanusTyped e (CStruct (FlatSetSpine k))
  )

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
  (FlatSetCtx m e k, Elem e k ~ e ks, JanusTyped e ks) =>
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
  (FlatSetCtx m e k, Elem e k ~ e ks, JanusTyped e ks) =>
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

type instance JanusCEval (FlatSet JanusC k -> r) = FlatSet Identity k -> JanusCEval r

type instance JanusCRetType (FlatSet JanusC k -> r) = JanusCRetType r

instance (FlatSetABI k, JanusCParam r) => JanusCParam (FlatSet JanusC k -> r) where
  jcparam = mkPtrHandleJCParam FlatSet

instance (FlatSetABI k, JanusCEvaluate r) => JanusCEvaluate (FlatSet JanusC k -> r) where
  jceval = mkPtrHandleJCEval unFlatSet FlatSet

type instance JanusCEval (JanusCM (FlatSet JanusC k)) = IO (FlatSet Identity k)

type instance JanusCRetType (JanusCM (FlatSet JanusC k)) = FlatSet Identity k

instance (FlatSetABI k) => JanusCParam (JanusCM (FlatSet JanusC k)) where
  jcparam = mkPtrHandleJCRetParam unFlatSet

instance (FlatSetABI k) => JanusCEvaluate (JanusCM (FlatSet JanusC k)) where
  jceval = mkPtrHandleJCRetEval FlatSet

type instance LinuxX86_64CEval (FlatSet LinuxX86_64C k -> r) = FlatSet Identity k -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (FlatSet LinuxX86_64C k -> r) = LinuxX86_64BaseRet r

instance (FlatSetABI k, LinuxX86_64Param r) => LinuxX86_64Param (FlatSet LinuxX86_64C k -> r) where
  x86param = mkPtrHandleX86Param FlatSet

instance (FlatSetABI k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (FlatSet LinuxX86_64C k -> r) where
  x86eval = mkPtrHandleX86EvalParam unFlatSet FlatSet

instance (FlatSetABI k, X86DeferCont r) => X86DeferCall (FlatSet LinuxX86_64C k -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unFlatSet FlatSet

instance (FlatSetABI k, X86DeferCont r) => X86DeferCont (FlatSet LinuxX86_64C k -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unFlatSet FlatSet

type instance LinuxX86_64CEval (LinuxX86_64CM (FlatSet LinuxX86_64C k)) = IO (FlatSet Identity k)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (FlatSet LinuxX86_64C k)) = FlatSet Identity k

instance (FlatSetABI k) => LinuxX86_64Param (LinuxX86_64CM (FlatSet LinuxX86_64C k)) where
  x86param = mkPtrHandleX86RetParam unFlatSet

instance (FlatSetABI k) => LinuxX86_64Evaluate (LinuxX86_64CM (FlatSet LinuxX86_64C k)) where
  x86eval = mkPtrHandleX86RetEval FlatSet

instance (FlatSetABI k) => X86DeferCall (LinuxX86_64CM (FlatSet LinuxX86_64C k)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg FlatSet

instance (FlatSetABI k) => X86DeferCont (LinuxX86_64CM (FlatSet LinuxX86_64C k)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont FlatSet

instance (FlatSetABI k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (FlatSet LinuxX86_64C k -> r) where
  type JanusCTHEval (FlatSet LinuxX86_64C k -> r) = FlatSet Identity k -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(FlatSet Identity k)) : ps, retSpec)

instance (FlatSetABI k) => JanusCTH (LinuxX86_64CM (FlatSet LinuxX86_64C k)) where
  type JanusCTHEval (LinuxX86_64CM (FlatSet LinuxX86_64C k)) = IO (FlatSet Identity k)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(FlatSet Identity k)) (Proxy @(CStruct (FlatSetSpine k))))
