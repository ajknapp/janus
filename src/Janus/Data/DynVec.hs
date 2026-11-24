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

module Janus.Data.DynVec
  ( -- * Dynamically Sized Vector Spine & Handle
    DynVecSpine (..),
    DynVec (..),
    DynVecCtx,

    -- * Lifecycle (Allocation Policy Explicit)
    new,
    withCapacity,
    free,

    -- * Capacity & Length Queries
    len,
    capacity,
    isEmpty,
    buf,

    -- * Capacity Management
    clear,
    truncateVec,
    reserve,
    shrinkToFit,

    -- * Element Mutation & Access
    push,
    pop,
    withPop,
    peekBack,
    withPeekBack,
    readVec,
    writeVec,
    modifyVec,
    swapRemove,
    insertAt,
    removeAt,
    elemPtrScalar,
    elemPtrAoS,

    -- * Sorting
    sortBy,
    sort,

    -- * Stream Producers & Drain Consumers (`drainToVect`)
    stream,
    reverseStream,
    indexedStream,
    ptrStreamScalar,
    drainToVect,
    fromStream,
    drainAppendDynVec,
    appendStream,

    -- * SPMD / SIMD Gang Kernels Over Vector Storage
    foreachSPMD,
    traverseSPMD_,
    foldSPMD,
    gatherUnsafeSPMD,
    gatherScalarSPMD,
    scatterSPMD,
    foreachScalarSPMD,
    foreachSoAVecSPMD,
    foreachAoSoAVecSPMD,
    foreachSOAOAVecSPMD,
  )
where

import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import GHC.TypeNats (natVal)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, indexedS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (sortBuf, sortBufBy)
import Janus.Expression.Bool (ExpBool (..))
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

-- | C-ABI spine struct for 'DynVec'.
data DynVecSpine a f = DynVecSpine
  { dvsBuf :: !(f (CStruct (Buf a))),
    dvsLen :: !(f Int64),
    dvsCap :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance (Eq (CStruct (Buf a))) => Eq (DynVecSpine a Identity)

deriving stock instance (Show (CStruct (Buf a))) => Show (DynVecSpine a Identity)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (DynVecSpine a)

-- | Mutable, single-threaded, dynamically sized vector supporting all 'StorageLayout's
-- (@Scalar@, @AoS@, @SoA@, @AoSoA w@, @SOAOA w@).
newtype DynVec (e :: K.Type -> K.Type) a = DynVec
  { unDynVec :: e (Ptr (CStruct (DynVecSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (DynVec Identity a)

deriving stock instance Show (DynVec Identity a)

type DynVecCtx m e a =
  ( LayoutCtx m e a,
    Typeable a,
    ExpSized e (CStruct (DynVecSpine a)),
    JanusTyped e (CStruct (DynVecSpine a))
  )

-- | Create an empty 'DynVec' (allocates the spine via @alloc@; buffer allocation is deferred until first push/reserve).
new ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  m (DynVec e a)
new alloc = do
  hdr <- allocStruct @(CStruct (DynVecSpine a)) alloc
  let f = projectPtrs hdr
  b0 <- allocBuf @a alloc 0
  pokeBufStruct (getPtrOf (dvsBuf f)) b0
  poke (getPtrOf (dvsLen f)) 0
  poke (getPtrOf (dvsCap f)) 0
  pure (DynVec hdr)

-- | Create an empty 'DynVec' with at least @initCap@ pre-allocated capacity.
withCapacity ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (DynVec e a)
withCapacity alloc initCap = do
  hdr <- allocStruct @(CStruct (DynVecSpine a)) alloc
  let f = projectPtrs hdr
      capPos = ifThenElse (initCap `le` 0) 0 initCap
  b0 <- allocBuf @a alloc capPos
  pokeBufStruct (getPtrOf (dvsBuf f)) b0
  poke (getPtrOf (dvsLen f)) 0
  poke (getPtrOf (dvsCap f)) capPos
  pure (DynVec hdr)

-- | Release both the backing element buffer (if non-empty capacity) and the spine struct via @alloc@.
free ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  m ()
free alloc (DynVec hdr) = do
  let f = projectPtrs hdr
  curCap <- peek (getPtrOf (dvsCap f))
  whenM_ (curCap `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (dvsBuf f))
    freeBuf alloc b curCap
  freeStruct alloc hdr

len :: (DynVecCtx m e a) => DynVec e a -> m (e Int64)
len (DynVec hdr) = peek (getPtrOf (dvsLen (projectPtrs hdr)))

capacity :: (DynVecCtx m e a) => DynVec e a -> m (e Int64)
capacity (DynVec hdr) = peek (getPtrOf (dvsCap (projectPtrs hdr)))

isEmpty :: (DynVecCtx m e a) => DynVec e a -> m (e Bool)
isEmpty v = (`eq` 0) <$> len v

buf :: (DynVecCtx m e a) => DynVec e a -> m (Buf a e)
buf (DynVec hdr) = peekBufStruct (getPtrOf (dvsBuf (projectPtrs hdr)))

-- | O(1) reset of length to 0 while keeping allocated capacity warm.
clear :: (DynVecCtx m e a) => DynVec e a -> m ()
clear (DynVec hdr) = poke (getPtrOf (dvsLen (projectPtrs hdr))) 0

-- | Truncate vector length to @min curLen newLen@ (if @newLen >= 0@).
truncateVec :: (DynVecCtx m e a) => DynVec e a -> e Int64 -> m ()
truncateVec (DynVec hdr) newLen = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  let clamped = ifThenElse (newLen `le` 0) 0 newLen
  whenM_ (clamped `lt` curLen) $
    poke (getPtrOf (dvsLen f)) clamped

-- | Ensure capacity is at least @curLen + additional@ using geometric 2x growth (starting at 8).
reserve ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  e Int64 ->
  m ()
reserve alloc (DynVec hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  curCap <- peek (getPtrOf (dvsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    (_, newCap) <-
      runDrain
        (drainFind 8 (`ge` needed))
        (iterateS (ifThenElse (curCap `lt` 8) 8 (curCap * 2)) (* 2))
    oldBuf <- peekBufStruct (getPtrOf (dvsBuf f))
    newBuf <- reallocBuf alloc oldBuf curCap newCap curLen
    pokeBufStruct (getPtrOf (dvsBuf f)) newBuf
    poke (getPtrOf (dvsCap f)) newCap

-- | Reallocate backing storage down to exact @len@ (freeing the buffer if @len == 0@).
shrinkToFit ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  m ()
shrinkToFit alloc (DynVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  curCap <- peek (getPtrOf (dvsCap f))
  whenM_ (curCap `gt` curLen) $ do
    oldBuf <- peekBufStruct (getPtrOf (dvsBuf f))
    ifThenElseM_
      (curLen `eq` 0)
      ( do
          freeBuf alloc oldBuf curCap
          b0 <- allocBuf @a alloc 0
          pokeBufStruct (getPtrOf (dvsBuf f)) b0
          poke (getPtrOf (dvsCap f)) 0
      )
      ( do
          newBuf <- reallocBuf alloc oldBuf curCap curLen curLen
          pokeBufStruct (getPtrOf (dvsBuf f)) newBuf
          poke (getPtrOf (dvsCap f)) curLen
      )

-- | Append an element at the back in amortized O(1) time.
push ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  Elem e a ->
  m ()
push alloc v@(DynVec hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  curCap <- peek (getPtrOf (dvsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc v 1
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  writeBuf b curLen val
  poke (getPtrOf (dvsLen f)) (curLen + 1)

-- | Pop the last scalar element, returning @(poppedOk, valueOrDefault)@.
pop ::
  forall a s m e.
  (DynVecCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  DynVec e a ->
  m (e Bool, e s)
pop defVal (DynVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    let newLen = curLen - 1
    b <- peekBufStruct (getPtrOf (dvsBuf f))
    v <- readBuf b newLen
    poke (getPtrOf (dvsLen f)) newLen
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

-- | Effectful pop supporting any element type (including HKD structs) via callback.
withPop ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (DynVec hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let newLen = curLen - 1
        b <- peekBufStruct (getPtrOf (dvsBuf f))
        v <- readBuf b newLen
        poke (getPtrOf (dvsLen f)) newLen
        onElem v
    )

peekBack ::
  forall a s m e.
  (DynVecCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  DynVec e a ->
  m (e Bool, e s)
peekBack defVal (DynVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (dvsBuf f))
    v <- readBuf b (curLen - 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekBack ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekBack (DynVec hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        b <- peekBufStruct (getPtrOf (dvsBuf f))
        v <- readBuf b (curLen - 1)
        onElem v
    )

readVec :: (DynVecCtx m e a) => DynVec e a -> e Int64 -> m (Elem e a)
readVec (DynVec hdr) idx = do
  b <- peekBufStruct (getPtrOf (dvsBuf (projectPtrs hdr)))
  readBuf b idx

writeVec :: (DynVecCtx m e a) => DynVec e a -> e Int64 -> Elem e a -> m ()
writeVec (DynVec hdr) idx val = do
  b <- peekBufStruct (getPtrOf (dvsBuf (projectPtrs hdr)))
  writeBuf b idx val

modifyVec :: (DynVecCtx m e a) => DynVec e a -> e Int64 -> (Elem e a -> Elem e a) -> m ()
modifyVec v idx f = do
  cur <- readVec v idx
  writeVec v idx (f cur)

-- | O(1) unordered removal at @idx@: swaps @vec[len - 1]@ into @idx@, decrements @len@,
-- and returns the removed element.
swapRemove ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  e Int64 ->
  m (Elem e a)
swapRemove (DynVec hdr) idx = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  removed <- readBuf b idx
  let lastIdx = curLen - 1
  whenM_ (idx `lt` lastIdx) $ do
    lastVal <- readBuf b lastIdx
    writeBuf b idx lastVal
  poke (getPtrOf (dvsLen f)) lastIdx
  pure removed

-- | Insert @val@ at @idx@ (@0 <= idx <= len@), shifting @[idx .. len - 1]@ right via 'moveBuf'.
insertAt ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  e Int64 ->
  Elem e a ->
  m ()
insertAt alloc v@(DynVec hdr) idx val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  curCap <- peek (getPtrOf (dvsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc v 1
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  let tailCnt = curLen - idx
  whenM_ (tailCnt `gt` 0) $
    moveBuf b (idx + 1) idx tailCnt
  writeBuf b idx val
  poke (getPtrOf (dvsLen f)) (curLen + 1)

-- | Remove and return element at @idx@ (@0 <= idx < len@), shifting @[idx + 1 .. len - 1]@ left via 'moveBuf'.
removeAt ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  e Int64 ->
  m (Elem e a)
removeAt (DynVec hdr) idx = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dvsLen f))
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  removed <- readBuf b idx
  let tailCnt = curLen - idx - 1
  whenM_ (tailCnt `gt` 0) $
    moveBuf b idx (idx + 1) tailCnt
  poke (getPtrOf (dvsLen f)) (curLen - 1)
  pure removed

elemPtrScalar ::
  (DynVecCtx m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  DynVec e a ->
  e Int64 ->
  m (e (Ptr a))
elemPtrScalar v idx = do
  ScalarBuf p <- buf v
  pure (ptrIndex p idx)

elemPtrAoS ::
  (DynVecCtx m e (AoS s), ExpPtr e (CStruct s)) =>
  DynVec e (AoS s) ->
  e Int64 ->
  m (e (Ptr (CStruct s)))
elemPtrAoS v idx = do
  AoSBuf p <- buf v
  pure (ptrIndex p idx)

-- | Zero-allocation in-place introsort over the vector's live elements.
sortBy ::
  (DynVecCtx m e a) =>
  (Elem e a -> Elem e a -> e Bool) ->
  DynVec e a ->
  m ()
sortBy isLt (DynVec hdr) = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (dvsLen f))
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  sortBufBy isLt b n

sort ::
  (DynVecCtx m e a, Elem e a ~ e s, ExpOrd e s) =>
  DynVec e a ->
  m ()
sort (DynVec hdr) = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (dvsLen f))
  b <- peekBufStruct (getPtrOf (dvsBuf f))
  sortBuf b n

-- | Sequential forward 'Stream' over @[0 .. len - 1]@. Hoists 'len' and 'buf' out of the loop!
stream ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  Stream m (Elem e a)
stream (DynVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (dvsLen f)) <*> peekBufStruct (getPtrOf (dvsBuf f)))
        (\(n, b) -> streamM (rangeS 0 n) (readBuf b))

-- | Sequential reverse 'Stream' from @len - 1@ down to @0@.
reverseStream ::
  forall a m e.
  (DynVecCtx m e a) =>
  DynVec e a ->
  Stream m (Elem e a)
reverseStream (DynVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (dvsLen f)) <*> peekBufStruct (getPtrOf (dvsBuf f)))
        (\(n, b) -> streamM (reverseRangeS 0 n) (readBuf b))

indexedStream ::
  (DynVecCtx m e a) =>
  DynVec e a ->
  Stream m (e Int64, Elem e a)
indexedStream v = indexedS (stream v)

ptrStreamScalar ::
  (DynVecCtx m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  DynVec e a ->
  Stream m (e (Ptr a))
ptrStreamScalar (DynVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (dvsLen f)) <*> peekBufStruct (getPtrOf (dvsBuf f)))
        (\(n, ScalarBuf p) -> ptrIndex p <$> rangeS 0 n)

-- | Materialize any fused 'Stream' directly into a freshly allocated 'DynVec'.
drainToVect ::
  forall a m e.
  (DynVecCtx m e a) =>
  Alloc e ->
  Drain m (Elem e a) (DynVec e a)
drainToVect alloc = Drain $ \s -> do
  v <- new alloc
  runDrain (drainAppendDynVec alloc v) s
  pure v

fromStream ::
  (DynVecCtx m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (DynVec e a)
fromStream alloc = runDrain (drainToVect alloc)

-- | Append all elements yielded by a 'Stream' to the back of an existing 'DynVec'.
drainAppendDynVec ::
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  Drain m (Elem e a) ()
drainAppendDynVec alloc v = drainForEach (push alloc v)

appendStream ::
  (DynVecCtx m e a) =>
  Alloc e ->
  DynVec e a ->
  Stream m (Elem e a) ->
  m ()
appendStream alloc v = runDrain (drainAppendDynVec alloc v)

--------------------------------------------------------------------------------
-- SPMD / SIMD Gang Kernels Over DynVec
--------------------------------------------------------------------------------

-- | Unified layout-polymorphic 64B-aligned in-place SIMD map over @[0 .. len - 1]@
-- across @Scalar@, @SoA s@, @AoSoA w s@, and @SOAOA w outer@.
foreachSPMD ::
  forall w a m e.
  (DynVecCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  DynVec e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw v step = do
  n <- len v
  b <- buf v
  foreachBufSPMD pw True b n step

-- | Unified layout-polymorphic read-only 64B-aligned SIMD scan over @[0 .. len - 1]@.
traverseSPMD_ ::
  forall w a m e.
  (DynVecCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  DynVec e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw v body = do
  n <- len v
  b <- buf v
  traverseBufSPMD_ pw True b n body

-- | Unified layout-polymorphic masked-safe 64B-aligned SIMD fold over @[0 .. len - 1]@.
foldSPMD ::
  forall w a s m e.
  (DynVecCtx m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  DynVec e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw v s0 step = do
  n <- len v
  b <- buf v
  foldBufSPMD pw True b n s0 step

-- | Unchecked varying-index SIMD gather across @Scalar@ and @SoA s@ (@0 <= idx < len@).
gatherUnsafeSPMD ::
  forall w a m e.
  (DynVecCtx m e a, LayoutGatherSPMD w m e a) =>
  DynVec e a ->
  SPMD w e Int64 ->
  SPMDM w m e (Elem (SPMD w e) a)
gatherUnsafeSPMD v idx = do
  b <- liftSPMD (buf v)
  gatherBufSPMD b idx

-- | Bounds-masked varying-index scalar SIMD gather returning @defVal@ on out-of-bounds lanes.
gatherScalarSPMD ::
  forall w a s m e.
  ( DynVecCtx m e a,
    LayoutGatherSPMD w m e a,
    Elem (SPMD w e) a ~ SPMD w e s,
    SPMDTyped w e s
  ) =>
  SPMD w e s ->
  DynVec e a ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e s)
gatherScalarSPMD defVal v idx = do
  n <- liftSPMD (len v)
  resRef <- newRef defVal
  whenM_ ((idx `ge` 0) `land` (idx `lt` uniform n)) $ do
    b <- liftSPMD (buf v)
    gathered <- gatherBufSPMD b idx
    writeRef resRef gathered
  readRef resRef

-- | Bounds-masked varying-index SIMD scatter across @Scalar@ and @SoA s@ (lanes outside @[0, len)@ are ignored).
scatterSPMD ::
  forall w a m e.
  (DynVecCtx m e a, LayoutGatherSPMD w m e a) =>
  DynVec e a ->
  SPMD w e Int64 ->
  Elem (SPMD w e) a ->
  SPMDM w m e ()
scatterSPMD v idx val = do
  n <- liftSPMD (len v)
  whenM_ ((idx `ge` 0) `land` (idx `lt` uniform n)) $ do
    b <- liftSPMD (buf v)
    scatterBufSPMD b idx val

-- | Strip-mined 64B-aligned SPMD loop over a scalar 'DynVec'.
foreachScalarSPMD ::
  forall w a m e.
  ( DynVecCtx m e a,
    Buf a ~ ScalarBuf a,
    SPMDGang w m e,
    SPMDLayout w (Ptr a) ~ 'LPtr a,
    CmdStorable (SPMDM w m e) (SPMD w e) a
  ) =>
  Proxy w ->
  DynVec e a ->
  (SPMD w e Int64 -> SPMD w e a -> SPMDM w m e (SPMD w e a)) ->
  m ()
foreachScalarSPMD pw v step = do
  n <- len v
  ScalarBuf rawPtr <- buf v
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

foreachSoAVecSPMD ::
  forall w s m e.
  ( DynVecCtx m e (SoA s),
    SPMDGang w m e,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  DynVec e (SoA s) ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachSoAVecSPMD pw v step = do
  n <- len v
  b <- buf v
  foreachSoASPMD pw b n step

foreachAoSoAVecSPMD ::
  forall w s m e.
  ( DynVecCtx m e (AoSoA w s),
    SPMDGang w m e,
    ExpPtr e (CStruct (TileStruct w s)),
    Generic (TileStruct w s (PtrOf e)),
    Generic (s (TileField w (PtrOf e))),
    Generic (s (SPMD w e)),
    Coercible (TileStruct w s (PtrOf e)) (s (TileField w (PtrOf e))),
    GJanusStructProject e (Rep (TileStruct w s Identity)) (Rep (TileStruct w s (PtrOf e))),
    GTileSPMD w m e (Rep (s (TileField w (PtrOf e)))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  DynVec e (AoSoA w s) ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachAoSoAVecSPMD pw v step = do
  n <- len v
  b <- buf v
  foreachAoSoASPMD pw b n step

foreachSOAOAVecSPMD ::
  forall w outer m e.
  ( DynVecCtx m e (SOAOA w outer),
    SPMDGang w m e,
    Generic (outer (SPMD w e)),
    GSOAOASPMD w m e (Rep (outer Identity)) (Rep (outer (SPMD w e)))
  ) =>
  Proxy w ->
  DynVec e (SOAOA w outer) ->
  (SPMD w e Int64 -> outer (SPMD w e) -> SPMDM w m e (outer (SPMD w e))) ->
  m ()
foreachSOAOAVecSPMD pw v step = do
  n <- len v
  b <- buf v
  foreachSOAOASPMD pw b n step

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances (Parameters & Return Handles)
--------------------------------------------------------------------------------

type instance JanusCHost (DynVec (JanusCT _) a) = DynVec Identity a

deriving via
  ViaPtrHandle t (CStruct (DynVecSpine a)) (DynVec Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCArg t (DynVec (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (DynVecSpine a)) (DynVec Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCValRet t (DynVec (JanusCT t) a)
