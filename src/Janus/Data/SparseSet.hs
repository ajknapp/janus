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

module Janus.Data.SparseSet
  ( -- * Briggs-Torczon O(1) Sparse Integer Set (O(1) Uninitialized-Memory-Safe Clear!)
    SparseSetSpine (..),
    SparseSet (..),
    SparseSetCtx,

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

    -- * O(1) Membership, Insertion, & Swap-Deletion
    member,
    insert,
    delete,
    denseAt,

    -- * Packed Dense Streams, Fast Set Operations, & Drains
    stream,
    reverseStream,
    intersectionS,
    differenceS,
    unionInto,
    drainToSparseSet,
    fromStream,
    drainAppendSparseSet,

    -- * First-Class SPMD / SIMD Dense Scans & Briggs-Torczon Gather Membership
    traverseDenseSPMD_,
    foldDenseSPMD,
    memberSPMD,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, filterMapS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout (ScalarBuf (..), foldBufSPMD, traverseBufSPMD_)
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
    uniformAlignedPtr,
  )
import Janus.Typed (JanusTyped)

-- | Spine struct for Briggs-Torczon 'SparseSet'.
-- Note: @spsSparse@ NEVER needs zero-initialization; @x@ is live iff
-- @0 <= spsSparse[x] < spsLen@ AND @spsDense[spsSparse[x]] == x@.
data SparseSetSpine f = SparseSetSpine
  { spsSparse :: !(f (Ptr Int64)),
    spsDense :: !(f (Ptr Int64)),
    spsLen :: !(f Int64),
    spsUniverseCap :: !(f Int64),
    spsDenseCap :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (SparseSetSpine Identity)

deriving stock instance Show (SparseSetSpine Identity)

instance JanusStruct SparseSetSpine

newtype SparseSet (e :: K.Type -> K.Type) = SparseSet
  { unSparseSet :: e (Ptr (CStruct SparseSetSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (SparseSet Identity)

deriving stock instance Show (SparseSet Identity)

instance JanusHKD SparseSet

type SparseSetCtx m e =
  ( CmdAlloc m e,
    ExpPtr e Int64,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct SparseSetSpine),
    JanusTyped e (CStruct SparseSetSpine)
  )

new ::
  forall m e.
  (SparseSetCtx m e) =>
  Alloc e ->
  m (SparseSet e)
new alloc = do
  hdr <- allocStruct @(CStruct SparseSetSpine) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (spsSparse f)) nullPtr
  poke (getPtrOf (spsDense f)) nullPtr
  poke (getPtrOf (spsLen f)) 0
  poke (getPtrOf (spsUniverseCap f)) 0
  poke (getPtrOf (spsDenseCap f)) 0
  pure (SparseSet hdr)

-- | Create a 'SparseSet' with pre-allocated @sparse@ and @dense@ capacity @uCap@
-- (WITHOUT zeroing @sparse@ — O(1) setup!).
withUniverse ::
  forall m e.
  (SparseSetCtx m e) =>
  Alloc e ->
  e Int64 ->
  m (SparseSet e)
withUniverse alloc uCap = do
  ss <- new alloc
  whenM_ (uCap `gt` 0) $ do
    ensureUniverse alloc ss (uCap - 1)
    ensureDenseCap alloc ss uCap
  pure ss

free ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  m ()
free alloc (SparseSet hdr) = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (spsUniverseCap f))
  whenM_ (uCap `gt` 0) $ do
    sp <- peek (getPtrOf (spsSparse f))
    freeTypedArray @Int64 alloc sp uCap
  dCap <- peek (getPtrOf (spsDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    dn <- peek (getPtrOf (spsDense f))
    freeTypedArray @Int64 alloc dn dCap
  freeStruct alloc hdr

size :: (SparseSetCtx m e) => SparseSet e -> m (e Int64)
size (SparseSet hdr) = peek (getPtrOf (spsLen (projectPtrs hdr)))

universeCapacity :: (SparseSetCtx m e) => SparseSet e -> m (e Int64)
universeCapacity (SparseSet hdr) = peek (getPtrOf (spsUniverseCap (projectPtrs hdr)))

isEmpty :: (SparseSetCtx m e) => SparseSet e -> m (e Bool)
isEmpty ss = (`eq` 0) <$> size ss

-- | O(1) clear: sets @spsLen = 0@ without touching @spsSparse@ or @spsDense@!
clear :: (SparseSetCtx m e) => SparseSet e -> m ()
clear (SparseSet hdr) = poke (getPtrOf (spsLen (projectPtrs hdr))) 0

-- | Ensure @spsSparse@ can index @maxKey@ (growing geometrically without zeroing!).
ensureUniverse ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  e Int64 ->
  m ()
ensureUniverse alloc (SparseSet hdr) maxKey = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (spsUniverseCap f))
  whenM_ (maxKey `ge` uCap) $ do
    let c0 = ifThenElse (uCap `lt` 64) 64 (uCap * 2)
    (_, newUCap) <- runDrain (drainFind c0 (`gt` maxKey)) (iterateS c0 (* 2))
    oldSp <- peek (getPtrOf (spsSparse f))
    newSp <- reallocTypedArray @Int64 alloc oldSp uCap newUCap
    poke (getPtrOf (spsSparse f)) newSp
    poke (getPtrOf (spsUniverseCap f)) newUCap

ensureDenseCap ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  e Int64 ->
  m ()
ensureDenseCap alloc (SparseSet hdr) reqCap = do
  let f = projectPtrs hdr
  dCap <- peek (getPtrOf (spsDenseCap f))
  whenM_ (reqCap `gt` dCap) $ do
    let c0 = ifThenElse (dCap `lt` 16) 16 (dCap * 2)
    (_, newDCap) <- runDrain (drainFind c0 (`ge` reqCap)) (iterateS c0 (* 2))
    oldDn <- peek (getPtrOf (spsDense f))
    newDn <- reallocTypedArray @Int64 alloc oldDn dCap newDCap
    poke (getPtrOf (spsDense f)) newDn
    poke (getPtrOf (spsDenseCap f)) newDCap

-- | O(1) Briggs-Torczon membership check.
member ::
  (SparseSetCtx m e) =>
  SparseSet e ->
  e Int64 ->
  m (e Bool)
member (SparseSet hdr) x = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (spsUniverseCap f))
  okRef <- newRef false
  whenM_ ((x `ge` 0) `land` (x `lt` uCap)) $ do
    curLen <- peek (getPtrOf (spsLen f))
    sp <- peek (getPtrOf (spsSparse f))
    idx <- peekElemOff sp x
    whenM_ ((idx `ge` 0) `land` (idx `lt` curLen)) $ do
      dn <- peek (getPtrOf (spsDense f))
      dx <- peekElemOff dn idx
      writeRef okRef (dx `eq` x)
  readRef okRef

-- | O(1) amortized insert of @x >= 0@. Returns @true@ iff @x@ was newly inserted.
insert ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  e Int64 ->
  m (e Bool)
insert alloc ss@(SparseSet hdr) x = do
  already <- member ss x
  insertedRef <- newRef false
  whenM_ ((x `ge` 0) `land` lnot already) $ do
    let f = projectPtrs hdr
    ensureUniverse alloc ss x
    curLen <- peek (getPtrOf (spsLen f))
    ensureDenseCap alloc ss (curLen + 1)
    sp <- peek (getPtrOf (spsSparse f))
    dn <- peek (getPtrOf (spsDense f))
    pokeElemOff sp curLen x
    pokeElemOff dn x curLen
    poke (getPtrOf (spsLen f)) (curLen + 1)
    writeRef insertedRef true
  readRef insertedRef

-- | O(1) swap-delete of @x@. Returns @true@ iff @x@ was present and removed.
delete ::
  (SparseSetCtx m e) =>
  SparseSet e ->
  e Int64 ->
  m (e Bool)
delete ss@(SparseSet hdr) x = do
  present <- member ss x
  whenM_ present $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (spsLen f))
    sp <- peek (getPtrOf (spsSparse f))
    dn <- peek (getPtrOf (spsDense f))
    idx <- peekElemOff sp x
    let lastIdx = curLen - 1
    whenM_ (idx `lt` lastIdx) $ do
      lastVal <- peekElemOff dn lastIdx
      pokeElemOff dn lastVal idx
      pokeElemOff sp idx lastVal
    poke (getPtrOf (spsLen f)) lastIdx
  pure present

denseAt :: (SparseSetCtx m e) => SparseSet e -> e Int64 -> m (e Int64)
denseAt (SparseSet hdr) idx = do
  dn <- peek (getPtrOf (spsDense (projectPtrs hdr)))
  peekElemOff dn idx

-- | Unit-stride contiguous 'Stream' over @dense[0 .. len - 1]@.
stream ::
  (SparseSetCtx m e) =>
  SparseSet e ->
  Stream m (e Int64)
stream (SparseSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (spsLen f)) <*> peek (getPtrOf (spsDense f)))
        (\(n, dn) -> streamM (rangeS 0 n) (peekElemOff dn))

reverseStream ::
  (SparseSetCtx m e) =>
  SparseSet e ->
  Stream m (e Int64)
reverseStream (SparseSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (spsLen f)) <*> peek (getPtrOf (spsDense f)))
        (\(n, dn) -> streamM (reverseRangeS 0 n) (peekElemOff dn))

-- | O(min(|A|, |B|)) fused intersection 'Stream': iterates the smaller set's packed @dense@
-- array and O(1)-probes the larger set!
intersectionS ::
  forall m e.
  (SparseSetCtx m e) =>
  SparseSet e ->
  SparseSet e ->
  Stream m (e Int64)
intersectionS a@(SparseSet pa) b@(SparseSet pb) =
  withStreamSetup
    ((,) <$> size a <*> size b)
    $ \(na, nb) ->
      let wa = ptrToWord64Expr pa
          wb = ptrToWord64Expr pb
          smaller = SparseSet (word64ToPtrExpr (ifThenElse (na `le` nb) wa wb))
          larger = SparseSet (word64ToPtrExpr (ifThenElse (na `le` nb) wb wa))
       in filterMapS (\x -> (\ok -> (ok, x)) <$> member larger x) (stream smaller)

differenceS ::
  (SparseSetCtx m e) =>
  SparseSet e ->
  SparseSet e ->
  Stream m (e Int64)
differenceS a b =
  filterMapS (\x -> (\inB -> (lnot inB, x)) <$> member b x) (stream a)

unionInto ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  SparseSet e ->
  m ()
unionInto alloc dst src =
  runDrain (drainAppendSparseSet alloc dst) (stream src)

drainToSparseSet ::
  (SparseSetCtx m e) =>
  Alloc e ->
  Drain m (e Int64) (SparseSet e)
drainToSparseSet alloc = Drain $ \s -> do
  ss <- new alloc
  runDrain (drainAppendSparseSet alloc ss) s
  pure ss

fromStream ::
  (SparseSetCtx m e) =>
  Alloc e ->
  Stream m (e Int64) ->
  m (SparseSet e)
fromStream alloc = runDrain (drainToSparseSet alloc)

drainAppendSparseSet ::
  (SparseSetCtx m e) =>
  Alloc e ->
  SparseSet e ->
  Drain m (e Int64) ()
drainAppendSparseSet alloc ss = drainForEach (\x -> insert alloc ss x >> pure ())

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Dense Scans & Briggs-Torczon Gather Membership
--------------------------------------------------------------------------------

-- | Read-only 64B-aligned SIMD scan over @spsDense[0 .. size - 1]@ at gang width @w@.
traverseDenseSPMD_ ::
  forall w m e.
  (SparseSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  SparseSet e ->
  (SPMD w e Int64 -> SPMD w e Int64 -> SPMDM w m e ()) ->
  m ()
traverseDenseSPMD_ pw (SparseSet hdr) body = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (spsLen f))
  dn <- peek (getPtrOf (spsDense f))
  traverseBufSPMD_ pw True (ScalarBuf dn) curLen body

-- | Masked-safe 64B-aligned SIMD fold over @spsDense[0 .. size - 1]@ at gang width @w@.
foldDenseSPMD ::
  forall w s m e.
  (SparseSetCtx m e, SPMDGang w m e, SPMDTyped w e s) =>
  Proxy w ->
  SparseSet e ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> SPMD w e Int64 -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldDenseSPMD pw (SparseSet hdr) s0 step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (spsLen f))
  dn <- peek (getPtrOf (spsDense f))
  foldBufSPMD pw True (ScalarBuf dn) curLen s0 step

-- | Lane-parallel Briggs-Torczon O(1) membership test across @w@ SIMD lanes
-- (uninitialized-memory-safe and bounds-masked).
memberSPMD ::
  forall w m e.
  (SparseSetCtx m e, SPMDGang w m e) =>
  SparseSet e ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Bool)
memberSPMD (SparseSet hdr) xs = do
  let f = projectPtrs hdr
  uCap <- liftSPMD (peek (getPtrOf (spsUniverseCap f)))
  okRef <- newRef (false :: SPMD w e Bool)
  whenM_ ((xs `ge` 0) `land` (xs `lt` uniform uCap)) $ do
    curLen <- liftSPMD (peek (getPtrOf (spsLen f)))
    sp <- liftSPMD (peek (getPtrOf (spsSparse f)))
    d <- peekElemOff (uniformAlignedPtr sp) xs
    whenM_ ((d `ge` 0) `land` (d `lt` uniform curLen)) $ do
      dn <- liftSPMD (peek (getPtrOf (spsDense f)))
      dk <- peekElemOff (uniformAlignedPtr dn) d
      writeRef okRef (dk `eq` xs)
  readRef okRef

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (SparseSet (JanusCT _)) = SparseSet Identity

deriving via
  ViaPtrHandle t (CStruct SparseSetSpine) (SparseSet Identity)
  instance
    JanusCArg t (SparseSet (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct SparseSetSpine) (SparseSet Identity)
  instance
    JanusCValRet t (SparseSet (JanusCT t))
