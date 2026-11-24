{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Janus.Alloc.Policy
  ( -- * Composable Allocation Policy Datatype (Never a Typeclass)
    Alloc (..),
    CmdAlloc,

    -- * Convenience Policy Builders & Combinators
    dynamicAlloc,
    fixedAlloc,
    monotonicAlloc,
    arenaAlloc,
    poolAlloc,
    recyclingAlloc,
    fallbackAlloc,
    segregateAlloc,
    statsAlloc,

    -- * Core Byte-Level Allocation Operations
    allocBytes,
    allocBytesAligned,
    allocZeroed,
    allocZeroedAligned,
    reallocBytes,
    reallocBytesAligned,
    freeBytes,
    ownsPtr,
    resetAlloc,

    -- * Typed Struct / Array Allocation Helpers
    allocStruct,
    freeStruct,
    allocTypedArray,
    reallocTypedArray,
    freeTypedArray,

    -- * Derived Allocator Lifecycle Constructors & Operations
    newFixedBuf,
    newFixedBufAligned,
    destroyFixedBuf,
    newMonotonic,
    newMonotonicAligned,
    resetMonotonic,
    destroyMonotonic,
    newArena,
    newArenaAligned,
    rewindArena,
    withArenaScope,
    resetArena,
    destroyArena,
    newPool,
    newPoolAligned,
    resetPool,
    destroyPool,
    newRecycler,
    newRecyclerAligned,
    flushRecycler,
    destroyRecycler,
  )
where

import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Janus.Alloc.Arena
import Janus.Alloc.Dynamic
import Janus.Alloc.Fixed
import Janus.Alloc.Monotonic
import Janus.Alloc.Pool
import Janus.Alloc.Recycling
import Janus.Alloc.Stats
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
    janusSIMDAlignment,
  )
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Data.HKD (CStruct, FixedArray, getPtrOf, projectPtrs)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

-- | Unified constraint bundle supporting all composable 'Alloc' policies across
-- @(IO, Identity)@ and @(JanusCMT t, JanusCT t)@ across any target @t@.
type CmdAlloc m e =
  ( CmdAllocBase m e,
    ExpSized e (CStruct FixedBufSpine),
    JanusTyped e (CStruct FixedBufSpine),
    ExpSized e (CStruct MonoSlabHdr),
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr)),
    ExpSized e (CStruct MonotonicSpine),
    JanusTyped e (CStruct MonotonicSpine),
    ExpSized e (CStruct ArenaSlabHdr),
    JanusTyped e (CStruct ArenaSlabHdr),
    ExpPtr e (CStruct ArenaSlabHdr),
    CmdStorable m e (Ptr (CStruct ArenaSlabHdr)),
    ExpSized e (CStruct ArenaLargeHdr),
    JanusTyped e (CStruct ArenaLargeHdr),
    ExpPtr e (CStruct ArenaLargeHdr),
    CmdStorable m e (Ptr (CStruct ArenaLargeHdr)),
    ExpSized e (CStruct ArenaSpine),
    JanusTyped e (CStruct ArenaSpine),
    ExpSized e (CStruct PoolSlabHdr),
    JanusTyped e (CStruct PoolSlabHdr),
    ExpPtr e (CStruct PoolSlabHdr),
    CmdStorable m e (Ptr (CStruct PoolSlabHdr)),
    ExpSized e (CStruct MemPoolSpine),
    JanusTyped e (CStruct MemPoolSpine),
    ExpSized e (CStruct RecyclerSpine),
    JanusTyped e (CStruct RecyclerSpine),
    JanusTyped e (FixedArray RecyclerBuckets (Ptr Word8)),
    JanusTyped e (FixedArray RecyclerBuckets Int64),
    ExpSized e (CStruct AllocTrackerSpine),
    JanusTyped e (CStruct AllocTrackerSpine)
  )

-- | First-class composable memory allocation policy datatype.
data Alloc (e :: K.Type -> K.Type)
  = AllocDynamic
  | AllocFixed !(FixedBuf e)
  | AllocMonotonic !(Monotonic e) !(Alloc e)
  | AllocArena !(Arena e) !(Alloc e)
  | AllocPool !(MemPool e) !(Alloc e)
  | AllocRecycling !(Recycler e) !(Alloc e)
  | AllocFallback !(Alloc e) !(Alloc e)
  | AllocSegregate !(e Int64) !(Alloc e) !(Alloc e)
  | AllocStats !(AllocTracker e) !(Alloc e)

dynamicAlloc :: Alloc e
dynamicAlloc = AllocDynamic

fixedAlloc :: FixedBuf e -> Alloc e
fixedAlloc = AllocFixed

monotonicAlloc :: Monotonic e -> Alloc e -> Alloc e
monotonicAlloc = AllocMonotonic

arenaAlloc :: Arena e -> Alloc e -> Alloc e
arenaAlloc = AllocArena

poolAlloc :: MemPool e -> Alloc e -> Alloc e
poolAlloc = AllocPool

recyclingAlloc :: Recycler e -> Alloc e -> Alloc e
recyclingAlloc = AllocRecycling

fallbackAlloc :: Alloc e -> Alloc e -> Alloc e
fallbackAlloc = AllocFallback

segregateAlloc :: e Int64 -> Alloc e -> Alloc e -> Alloc e
segregateAlloc = AllocSegregate

statsAlloc :: AllocTracker e -> Alloc e -> Alloc e
statsAlloc = AllocStats

-- | Allocate @sz@ bytes using the policy's configured default alignment
-- (64 bytes for 'AllocDynamic' and default-constructed derived allocators;
-- @minAlign@ for custom-aligned derived allocators).
allocBytes :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (e (Ptr ()))
allocBytes alloc sz = allocBytesAligned alloc sz 1

-- | Allocate @sz@ bytes with alignment @max policyMinAlign reqAlign@.
allocBytesAligned ::
  forall m e.
  (CmdAlloc m e) =>
  Alloc e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
allocBytesAligned policy sz reqAlign = case policy of
  AllocDynamic ->
    dynamicAllocAligned sz reqAlign
  AllocFixed fb ->
    fixedAllocImpl fb sz reqAlign
  AllocMonotonic mono parent ->
    monotonicAllocImpl (allocBytesAligned parent) mono sz reqAlign
  AllocArena ar parent ->
    arenaAllocImpl (allocBytesAligned parent) ar sz reqAlign
  AllocPool mp parent ->
    poolAllocImpl (allocBytesAligned parent) mp sz reqAlign
  AllocRecycling rec parent ->
    recyclerAllocImpl (allocBytesAligned parent) rec sz reqAlign
  AllocFallback primary secondary -> do
    p1 <- allocBytesAligned primary sz reqAlign
    outRef <- newRef (fromVoidPtr p1 :: e (Ptr Word8))
    whenM_ (isNullVoidPtr p1) $
      allocBytesAligned secondary sz reqAlign >>= writeRef outRef . fromVoidPtr
    toVoidPtr <$> readRef outRef
  AllocSegregate threshold smallA largeA -> do
    outRef <- newRef (nullPtr @e @Word8)
    ifThenElseM_
      (sz `le` threshold)
      (allocBytesAligned smallA sz reqAlign >>= writeRef outRef . fromVoidPtr)
      (allocBytesAligned largeA sz reqAlign >>= writeRef outRef . fromVoidPtr)
    toVoidPtr <$> readRef outRef
  AllocStats tracker inner -> do
    vp <- allocBytesAligned inner sz reqAlign
    statsRecordAlloc tracker vp sz
    pure vp

allocZeroed :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (e (Ptr ()))
allocZeroed alloc sz = allocZeroedAligned alloc sz 1

allocZeroedAligned ::
  forall m e.
  (CmdAlloc m e) =>
  Alloc e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
allocZeroedAligned AllocDynamic sz reqAlign = dynamicAllocZeroedAligned sz reqAlign
allocZeroedAligned policy sz reqAlign = do
  vp <- allocBytesAligned policy sz reqAlign
  whenM_ (lnot (isNullVoidPtr vp) `land` (sz `gt` 0)) $
    cmdMemset vp 0 sz
  pure vp

reallocBytes ::
  (CmdAlloc m e) =>
  Alloc e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
reallocBytes alloc oldPtr oldSz newSz =
  reallocBytesAligned alloc oldPtr oldSz newSz 1

reallocBytesAligned ::
  forall m e.
  (CmdAlloc m e) =>
  Alloc e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
reallocBytesAligned policy oldPtr oldSz newSz reqAlign = case policy of
  AllocDynamic ->
    dynamicReallocAligned oldPtr oldSz newSz reqAlign
  AllocFixed fb ->
    fixedReallocImpl fb oldPtr oldSz newSz reqAlign
  AllocMonotonic mono parent ->
    monotonicReallocImpl (allocBytesAligned parent) mono oldPtr oldSz newSz reqAlign
  AllocArena ar parent ->
    arenaReallocImpl (allocBytesAligned parent) ar oldPtr oldSz newSz reqAlign
  AllocPool mp parent ->
    poolReallocImpl (allocBytesAligned parent) mp oldPtr oldSz newSz reqAlign
  AllocRecycling rec parent ->
    recyclerReallocImpl (allocBytesAligned parent) (freeBytes parent) rec oldPtr oldSz newSz reqAlign
  AllocFallback primary secondary -> do
    outRef <- newRef (nullPtr @e @Word8)
    ifThenElseM_
      (isNullVoidPtr oldPtr)
      (allocBytesAligned policy newSz reqAlign >>= writeRef outRef . fromVoidPtr)
      ( ifThenElseM_
          (newSz `le` 0)
          (freeBytes policy oldPtr oldSz >> writeRef outRef (nullPtr @e @Word8))
          ( do
              inPrim <- ownsPtr primary oldPtr
              ifThenElseM_
                inPrim
                ( do
                    np <- reallocBytesAligned primary oldPtr oldSz newSz reqAlign
                    ifThenElseM_
                      (lnot (isNullVoidPtr np))
                      (writeRef outRef (fromVoidPtr np))
                      -- Spill over from exhausted primary to secondary!
                      ( do
                          np2 <- allocBytesAligned secondary newSz reqAlign
                          whenM_ (lnot (isNullVoidPtr np2)) $ do
                            let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                            cmdMemcpy np2 oldPtr copyLen
                            freeBytes primary oldPtr oldSz
                          writeRef outRef (fromVoidPtr np2)
                      )
                )
                (reallocBytesAligned secondary oldPtr oldSz newSz reqAlign >>= writeRef outRef . fromVoidPtr)
          )
      )
    toVoidPtr <$> readRef outRef
  AllocSegregate threshold smallA largeA -> do
    outRef <- newRef (nullPtr @e @Word8)
    ifThenElseM_
      (isNullVoidPtr oldPtr)
      (allocBytesAligned policy newSz reqAlign >>= writeRef outRef . fromVoidPtr)
      ( ifThenElseM_
          (newSz `le` 0)
          (freeBytes policy oldPtr oldSz >> writeRef outRef (nullPtr @e @Word8))
          ( do
              let wasSmall = oldSz `le` threshold
                  nowSmall = newSz `le` threshold
              ifThenElseM_
                (wasSmall `land` nowSmall)
                (reallocBytesAligned smallA oldPtr oldSz newSz reqAlign >>= writeRef outRef . fromVoidPtr)
                ( ifThenElseM_
                    (lnot wasSmall `land` lnot nowSmall)
                    (reallocBytesAligned largeA oldPtr oldSz newSz reqAlign >>= writeRef outRef . fromVoidPtr)
                    -- Crossed threshold: migrate between smallA and largeA!
                    ( ifThenElseM_
                        wasSmall
                        ( do
                            np <- allocBytesAligned largeA newSz reqAlign
                            whenM_ (lnot (isNullVoidPtr np)) $ do
                              let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                              cmdMemcpy np oldPtr copyLen
                              freeBytes smallA oldPtr oldSz
                            writeRef outRef (fromVoidPtr np)
                        )
                        ( do
                            np <- allocBytesAligned smallA newSz reqAlign
                            whenM_ (lnot (isNullVoidPtr np)) $ do
                              let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                              cmdMemcpy np oldPtr copyLen
                              freeBytes largeA oldPtr oldSz
                            writeRef outRef (fromVoidPtr np)
                        )
                    )
                )
          )
      )
    toVoidPtr <$> readRef outRef
  AllocStats tracker inner -> do
    outRef <- newRef (nullPtr @e @Word8)
    ifThenElseM_
      (isNullVoidPtr oldPtr)
      (allocBytesAligned policy newSz reqAlign >>= writeRef outRef . fromVoidPtr)
      ( ifThenElseM_
          (newSz `le` 0)
          (freeBytes policy oldPtr oldSz >> writeRef outRef (nullPtr @e @Word8))
          ( do
              np <- reallocBytesAligned inner oldPtr oldSz newSz reqAlign
              whenM_ (lnot (isNullVoidPtr np)) $ do
                statsRecordFree tracker oldPtr oldSz
                statsRecordAlloc tracker np newSz
              writeRef outRef (fromVoidPtr np)
          )
      )
    toVoidPtr <$> readRef outRef

freeBytes ::
  forall m e.
  (CmdAlloc m e) =>
  Alloc e ->
  e (Ptr ()) ->
  e Int64 ->
  m ()
freeBytes policy ptr oldSz =
  whenM_ (lnot (isNullVoidPtr ptr)) $ case policy of
    AllocDynamic ->
      dynamicFree ptr
    AllocFixed fb ->
      fixedFreeImpl fb ptr oldSz
    AllocMonotonic _ _ ->
      pure ()
    AllocArena _ _ ->
      pure ()
    AllocPool mp _ ->
      poolFreeImpl mp ptr
    AllocRecycling rec parent ->
      recyclerFreeImpl (freeBytes parent) rec ptr oldSz
    AllocFallback primary secondary -> do
      inPrim <- ownsPtr primary ptr
      ifThenElseM_
        inPrim
        (freeBytes primary ptr oldSz)
        (freeBytes secondary ptr oldSz)
    AllocSegregate threshold smallA largeA ->
      ifThenElseM_
        (oldSz `le` threshold)
        (freeBytes smallA ptr oldSz)
        (freeBytes largeA ptr oldSz)
    AllocStats tracker inner -> do
      statsRecordFree tracker ptr oldSz
      freeBytes inner ptr oldSz

ownsPtr ::
  forall m e.
  (CmdAlloc m e) =>
  Alloc e ->
  e (Ptr ()) ->
  m (e Bool)
ownsPtr policy ptr = case policy of
  AllocDynamic ->
    pure (lnot (isNullVoidPtr ptr))
  AllocFixed fb ->
    fixedOwnsPtr fb ptr
  AllocMonotonic mono _ ->
    monotonicOwnsPtr mono ptr
  AllocArena ar _ ->
    arenaOwnsPtr ar ptr
  AllocPool mp _ ->
    poolOwnsPtr mp ptr
  AllocRecycling _ parent ->
    ownsPtr parent ptr
  AllocFallback primary secondary -> do
    p1 <- ownsPtr primary ptr
    resRef <- newRef p1
    whenM_ (lnot p1) $
      ownsPtr secondary ptr >>= writeRef resRef
    readRef resRef
  AllocSegregate _ smallA largeA -> do
    p1 <- ownsPtr smallA ptr
    resRef <- newRef p1
    whenM_ (lnot p1) $
      ownsPtr largeA ptr >>= writeRef resRef
    readRef resRef
  AllocStats _ inner ->
    ownsPtr inner ptr

resetAlloc ::
  (CmdAlloc m e) =>
  Alloc e ->
  m ()
resetAlloc policy = case policy of
  AllocDynamic -> pure ()
  AllocFixed fb -> resetFixedBuf fb
  AllocMonotonic mono parent -> monotonicResetImpl (freeBytes parent) mono
  AllocArena ar parent -> arenaResetImpl (freeBytes parent) ar
  AllocPool mp _ -> poolResetImpl mp
  AllocRecycling rec parent -> recyclerFlushImpl (freeBytes parent) rec >> resetAlloc parent
  AllocFallback primary secondary -> resetAlloc primary >> resetAlloc secondary
  AllocSegregate _ smallA largeA -> resetAlloc smallA >> resetAlloc largeA
  AllocStats tracker inner -> resetAlloc inner >> resetAllocTracker tracker

--------------------------------------------------------------------------------
-- Typed Allocation Helpers
--------------------------------------------------------------------------------

allocStruct ::
  forall a m e.
  (CmdAlloc m e, ExpSized e a, JanusTyped e a) =>
  Alloc e ->
  m (e (Ptr a))
allocStruct alloc =
  fromVoidPtr <$> allocBytesAligned alloc (sizeOf (Proxy @a)) (alignOf (Proxy @a))

freeStruct ::
  forall a m e.
  (CmdAlloc m e, ExpSized e a, JanusTyped e a) =>
  Alloc e ->
  e (Ptr a) ->
  m ()
freeStruct alloc p =
  freeBytes alloc (toVoidPtr p) (sizeOf (Proxy @a))

-- | Allocate a 64-byte SIMD-aligned array of @n@ elements of type @a@
-- (returns @NULL@ without invoking the allocator when @n <= 0@).
allocTypedArray ::
  forall a m e.
  (CmdAlloc m e, ExpSized e a, JanusTyped e a) =>
  Alloc e ->
  e Int64 ->
  m (e (Ptr a))
allocTypedArray alloc n = do
  outRef <- newRef (nullPtr @e @Word8)
  whenM_ (n `gt` 0) $ do
    let elemAlign = alignOf (Proxy @a)
        simdAl = fromIntegral janusSIMDAlignment
        effAl = ifThenElse (elemAlign `gt` simdAl) elemAlign simdAl
    vp <- allocBytesAligned alloc (n * sizeOf (Proxy @a)) effAl
    writeRef outRef (fromVoidPtr vp)
  fromVoidPtr . toVoidPtr <$> readRef outRef

reallocTypedArray ::
  forall a m e.
  (CmdAlloc m e, ExpSized e a, JanusTyped e a) =>
  Alloc e ->
  e (Ptr a) ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr a))
reallocTypedArray alloc oldP oldN newN = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (newN `le` 0)
    (freeTypedArray @a alloc oldP oldN)
    ( ifThenElseM_
        (isNullVoidPtr (toVoidPtr oldP) `lor` (oldN `le` 0))
        (allocTypedArray @a alloc newN >>= writeRef outRef . fromVoidPtr . toVoidPtr)
        ( ifThenElseM_
            (oldN `eq` newN)
            (writeRef outRef (fromVoidPtr (toVoidPtr oldP)))
            ( do
                let sz = sizeOf (Proxy @a)
                    elemAlign = alignOf (Proxy @a)
                    simdAl = fromIntegral janusSIMDAlignment
                    effAl = ifThenElse (elemAlign `gt` simdAl) elemAlign simdAl
                vp <- reallocBytesAligned alloc (toVoidPtr oldP) (oldN * sz) (newN * sz) effAl
                writeRef outRef (fromVoidPtr vp)
            )
        )
    )
  fromVoidPtr . toVoidPtr <$> readRef outRef

freeTypedArray ::
  forall a m e.
  (CmdAlloc m e, ExpSized e a, JanusTyped e a) =>
  Alloc e ->
  e (Ptr a) ->
  e Int64 ->
  m ()
freeTypedArray alloc p n =
  whenM_ (n `gt` 0) $
    freeBytes alloc (toVoidPtr p) (n * sizeOf (Proxy @a))

--------------------------------------------------------------------------------
-- Derived Allocator Lifecycle Constructors & Operations
--------------------------------------------------------------------------------

newFixedBuf :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (FixedBuf e)
newFixedBuf parent cap = newFixedBufAligned parent cap (fromIntegral janusSIMDAlignment)

newFixedBufAligned :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> m (FixedBuf e)
newFixedBufAligned parent cap minAl = do
  hdr <- allocStruct @(CStruct FixedBufSpine) parent
  let bufAl = ifThenElse (minAl `gt` fromIntegral janusSIMDAlignment) minAl (fromIntegral janusSIMDAlignment)
  buf <- allocBytesAligned parent cap bufAl
  initFixedBufRaw hdr (fromVoidPtr buf) cap minAl

destroyFixedBuf :: (CmdAlloc m e) => Alloc e -> FixedBuf e -> m ()
destroyFixedBuf parent (FixedBuf hdr) = do
  let f = projectPtrs hdr
  base <- peek (getPtrOf (fbsBase f))
  cap <- peek (getPtrOf (fbsCap f))
  freeBytes parent (toVoidPtr base) cap
  freeStruct parent hdr

newMonotonic :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (Monotonic e)
newMonotonic parent initSlabCap =
  newMonotonicAligned parent initSlabCap (fromIntegral janusSIMDAlignment)

newMonotonicAligned :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> m (Monotonic e)
newMonotonicAligned parent initSlabCap minAl = do
  hdr <- allocStruct @(CStruct MonotonicSpine) parent
  initMonotonicRaw hdr initSlabCap minAl

resetMonotonic :: (CmdAlloc m e) => Alloc e -> Monotonic e -> m ()
resetMonotonic parent = monotonicResetImpl (freeBytes parent)

destroyMonotonic :: (CmdAlloc m e) => Alloc e -> Monotonic e -> m ()
destroyMonotonic parent =
  monotonicDestroyImpl (freeBytes parent) (sizeOf (Proxy @(CStruct MonotonicSpine)))

newArena :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (Arena e)
newArena parent slabCap =
  newArenaAligned parent slabCap (fromIntegral janusSIMDAlignment)

newArenaAligned :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> m (Arena e)
newArenaAligned parent slabCap minAl = do
  hdr <- allocStruct @(CStruct ArenaSpine) parent
  initArenaRaw hdr slabCap minAl

rewindArena :: (CmdAlloc m e) => Alloc e -> Arena e -> ArenaMark e -> m ()
rewindArena parent = arenaRewindImpl (freeBytes parent)

-- | Execute an action inside a temporary arena scope, automatically rewinding
-- all slab & large-block allocations performed inside @body@ upon completion.
withArenaScope ::
  (CmdAlloc m e) =>
  Alloc e ->
  Arena e ->
  (Alloc e -> m r) ->
  m r
withArenaScope parent ar body = do
  m <- markArena ar
  res <- body (AllocArena ar parent)
  rewindArena parent ar m
  pure res

resetArena :: (CmdAlloc m e) => Alloc e -> Arena e -> m ()
resetArena parent = arenaResetImpl (freeBytes parent)

destroyArena :: (CmdAlloc m e) => Alloc e -> Arena e -> m ()
destroyArena parent =
  arenaDestroyImpl (freeBytes parent) (sizeOf (Proxy @(CStruct ArenaSpine)))

newPool :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> m (MemPool e)
newPool parent slotSz slotsPerSlab =
  newPoolAligned parent slotSz slotsPerSlab (fromIntegral janusSIMDAlignment)

newPoolAligned :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> e Int64 -> m (MemPool e)
newPoolAligned parent slotSz slotsPerSlab slotAl = do
  hdr <- allocStruct @(CStruct MemPoolSpine) parent
  initMemPoolRaw hdr slotSz slotsPerSlab slotAl

resetPool :: (CmdAlloc m e) => MemPool e -> m ()
resetPool = poolResetImpl

destroyPool :: (CmdAlloc m e) => Alloc e -> MemPool e -> m ()
destroyPool parent =
  poolDestroyImpl (freeBytes parent) (sizeOf (Proxy @(CStruct MemPoolSpine)))

newRecycler :: (CmdAlloc m e) => Alloc e -> e Int64 -> m (Recycler e)
newRecycler parent maxPerBucket =
  newRecyclerAligned parent maxPerBucket (fromIntegral janusSIMDAlignment)

newRecyclerAligned :: (CmdAlloc m e) => Alloc e -> e Int64 -> e Int64 -> m (Recycler e)
newRecyclerAligned parent maxPerBucket minAl = do
  hdr <- allocStruct @(CStruct RecyclerSpine) parent
  initRecyclerRaw hdr maxPerBucket minAl

flushRecycler :: (CmdAlloc m e) => Alloc e -> Recycler e -> m ()
flushRecycler parent = recyclerFlushImpl (freeBytes parent)

destroyRecycler :: (CmdAlloc m e) => Alloc e -> Recycler e -> m ()
destroyRecycler parent =
  recyclerDestroyImpl (freeBytes parent) (sizeOf (Proxy @(CStruct RecyclerSpine)))
