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

module Janus.Data.Deque
  ( -- * Growable Power-of-2 Circular Double-Ended Queue
    DequeSpine (..),
    Deque (..),
    DequeCtx,

    -- * Lifecycle
    new,
    withCapacity,
    free,

    -- * Queries & Capacity Management
    len,
    capacity,
    isEmpty,
    clear,
    reserve,

    -- * Double-Ended O(1) Operations & Random Access
    pushBack,
    pushFront,
    popBack,
    withPopBack,
    popFront,
    withPopFront,
    peekFront,
    withPeekFront,
    peekBack,
    withPeekBack,
    readDeque,
    writeDeque,
    modifyDeque,

    -- * Two-Span Contiguous Streams & Drains
    stream,
    reverseStream,
    indexedStream,
    drainToDeque,
    fromStream,
    drainAppendDequeBack,
    drainAppendDequeFront,

    -- * Two-Span Contiguous SPMD / SIMD Gang Kernels
    foreachSPMD,
    traverseSPMD_,
    foldSPMD,
  )
where

import Control.Applicative ((<|>))
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
import Janus.Command.Memory (CmdStorable (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, indexedS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD (SPMD, SPMDM, SPMDTyped, withSPMD)
import Janus.Typed (JanusTyped)

-- | Spine struct for 'Deque'.
-- @dqsCap@ is always @0@ or a power of 2 @>= 8@, so index wrapping uses @(idx & (cap - 1))@.
data DequeSpine a f = DequeSpine
  { dqsBuf :: !(f (CStruct (Buf a))),
    dqsHead :: !(f Int64),
    dqsLen :: !(f Int64),
    dqsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (DequeSpine a)

newtype Deque (e :: K.Type -> K.Type) a = Deque
  { unDeque :: e (Ptr (CStruct (DequeSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (Deque Identity a)

deriving stock instance Show (Deque Identity a)

type DequeCtx m e a =
  ( LayoutCtx m e a,
    Typeable a,
    ExpSized e (CStruct (DequeSpine a)),
    JanusTyped e (CStruct (DequeSpine a))
  )

new ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  m (Deque e a)
new alloc = do
  hdr <- allocStruct @(CStruct (DequeSpine a)) alloc
  let f = projectPtrs hdr
  b0 <- allocBuf @a alloc 0
  pokeBufStruct (getPtrOf (dqsBuf f)) b0
  poke (getPtrOf (dqsHead f)) 0
  poke (getPtrOf (dqsLen f)) 0
  poke (getPtrOf (dqsCap f)) 0
  pure (Deque hdr)

withCapacity ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (Deque e a)
withCapacity alloc initCap = do
  dq <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc dq initCap
  pure dq

free ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  m ()
free alloc (Deque hdr) = do
  let f = projectPtrs hdr
  curCap <- peek (getPtrOf (dqsCap f))
  whenM_ (curCap `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    freeBuf alloc b curCap
  freeStruct alloc hdr

len :: (DequeCtx m e a) => Deque e a -> m (e Int64)
len (Deque hdr) = peek (getPtrOf (dqsLen (projectPtrs hdr)))

capacity :: (DequeCtx m e a) => Deque e a -> m (e Int64)
capacity (Deque hdr) = peek (getPtrOf (dqsCap (projectPtrs hdr)))

isEmpty :: (DequeCtx m e a) => Deque e a -> m (e Bool)
isEmpty dq = (`eq` 0) <$> len dq

clear :: (DequeCtx m e a) => Deque e a -> m ()
clear (Deque hdr) = do
  let f = projectPtrs hdr
  poke (getPtrOf (dqsHead f)) 0
  poke (getPtrOf (dqsLen f)) 0

-- | Grow backing buffer to at least @curLen + additional@ (power of 2 @>= 8@),
-- unwrapping any circular wrap into a contiguous @[0 .. curLen - 1]@ prefix via two bulk 'copyBuf' calls.
reserve ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  e Int64 ->
  m ()
reserve alloc (Deque hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  curCap <- peek (getPtrOf (dqsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap `lshift` 1)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (`lshift` 1))
    newBuf <- allocBuf @a alloc newCap
    whenM_ (curCap `gt` 0) $ do
      oldBuf <- peekBufStruct (getPtrOf (dqsBuf f))
      whenM_ (curLen `gt` 0) $ do
        hd <- peek (getPtrOf (dqsHead f))
        let remRight = curCap - hd
            span1 = ifThenElse (curLen `lt` remRight) curLen remRight
            span2 = curLen - span1
        copyBuf newBuf 0 oldBuf hd span1
        whenM_ (span2 `gt` 0) $
          copyBuf newBuf span1 oldBuf 0 span2
      freeBuf alloc oldBuf curCap
    pokeBufStruct (getPtrOf (dqsBuf f)) newBuf
    poke (getPtrOf (dqsHead f)) 0
    poke (getPtrOf (dqsCap f)) newCap

pushBack ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  Elem e a ->
  m ()
pushBack alloc dq@(Deque hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  curCap <- peek (getPtrOf (dqsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc dq 1
  hd <- peek (getPtrOf (dqsHead f))
  cap <- peek (getPtrOf (dqsCap f))
  b <- peekBufStruct (getPtrOf (dqsBuf f))
  let physIdx = (hd + curLen) `iand` (cap - 1)
  writeBuf b physIdx val
  poke (getPtrOf (dqsLen f)) (curLen + 1)

pushFront ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  Elem e a ->
  m ()
pushFront alloc dq@(Deque hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  curCap <- peek (getPtrOf (dqsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc dq 1
  hd <- peek (getPtrOf (dqsHead f))
  cap <- peek (getPtrOf (dqsCap f))
  b <- peekBufStruct (getPtrOf (dqsBuf f))
  let newHd = (hd - 1) `iand` (cap - 1)
  writeBuf b newHd val
  poke (getPtrOf (dqsHead f)) newHd
  poke (getPtrOf (dqsLen f)) (curLen + 1)

popFront ::
  forall a s m e.
  (DequeCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  Deque e a ->
  m (e Bool, e s)
popFront defVal (Deque hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    cap <- peek (getPtrOf (dqsCap f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    v <- readBuf b hd
    poke (getPtrOf (dqsHead f)) ((hd + 1) `iand` (cap - 1))
    poke (getPtrOf (dqsLen f)) (curLen - 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPopFront ::
  forall a m e.
  (DequeCtx m e a) =>
  Deque e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPopFront (Deque hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        hd <- peek (getPtrOf (dqsHead f))
        cap <- peek (getPtrOf (dqsCap f))
        b <- peekBufStruct (getPtrOf (dqsBuf f))
        v <- readBuf b hd
        poke (getPtrOf (dqsHead f)) ((hd + 1) `iand` (cap - 1))
        poke (getPtrOf (dqsLen f)) (curLen - 1)
        onElem v
    )

popBack ::
  forall a s m e.
  (DequeCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  Deque e a ->
  m (e Bool, e s)
popBack defVal (Deque hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    cap <- peek (getPtrOf (dqsCap f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    let newLen = curLen - 1
        physIdx = (hd + newLen) `iand` (cap - 1)
    v <- readBuf b physIdx
    poke (getPtrOf (dqsLen f)) newLen
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPopBack ::
  forall a m e.
  (DequeCtx m e a) =>
  Deque e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPopBack (Deque hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        hd <- peek (getPtrOf (dqsHead f))
        cap <- peek (getPtrOf (dqsCap f))
        b <- peekBufStruct (getPtrOf (dqsBuf f))
        let newLen = curLen - 1
            physIdx = (hd + newLen) `iand` (cap - 1)
        v <- readBuf b physIdx
        poke (getPtrOf (dqsLen f)) newLen
        onElem v
    )

peekFront ::
  forall a s m e.
  (DequeCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  Deque e a ->
  m (e Bool, e s)
peekFront defVal (Deque hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    v <- readBuf b hd
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekFront ::
  (DequeCtx m e a) =>
  Deque e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekFront (Deque hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        hd <- peek (getPtrOf (dqsHead f))
        b <- peekBufStruct (getPtrOf (dqsBuf f))
        v <- readBuf b hd
        onElem v
    )

peekBack ::
  forall a s m e.
  (DequeCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  Deque e a ->
  m (e Bool, e s)
peekBack defVal (Deque hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    cap <- peek (getPtrOf (dqsCap f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    v <- readBuf b ((hd + curLen - 1) `iand` (cap - 1))
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekBack ::
  (DequeCtx m e a) =>
  Deque e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekBack (Deque hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        hd <- peek (getPtrOf (dqsHead f))
        cap <- peek (getPtrOf (dqsCap f))
        b <- peekBufStruct (getPtrOf (dqsBuf f))
        v <- readBuf b ((hd + curLen - 1) `iand` (cap - 1))
        onElem v
    )

readDeque :: (DequeCtx m e a) => Deque e a -> e Int64 -> m (Elem e a)
readDeque (Deque hdr) idx = do
  let f = projectPtrs hdr
  hd <- peek (getPtrOf (dqsHead f))
  cap <- peek (getPtrOf (dqsCap f))
  b <- peekBufStruct (getPtrOf (dqsBuf f))
  readBuf b ((hd + idx) `iand` (cap - 1))

writeDeque :: (DequeCtx m e a) => Deque e a -> e Int64 -> Elem e a -> m ()
writeDeque (Deque hdr) idx val = do
  let f = projectPtrs hdr
  hd <- peek (getPtrOf (dqsHead f))
  cap <- peek (getPtrOf (dqsCap f))
  b <- peekBufStruct (getPtrOf (dqsBuf f))
  writeBuf b ((hd + idx) `iand` (cap - 1)) val

modifyDeque :: (DequeCtx m e a) => Deque e a -> e Int64 -> (Elem e a -> Elem e a) -> m ()
modifyDeque dq idx f = do
  cur <- readDeque dq idx
  writeDeque dq idx (f cur)

-- | Two-span contiguous 'Stream' from front to back with zero per-element wrap masking.
stream ::
  forall a m e.
  (DequeCtx m e a) =>
  Deque e a ->
  Stream m (Elem e a)
stream (Deque hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( do
            hd <- peek (getPtrOf (dqsHead f))
            curLen <- peek (getPtrOf (dqsLen f))
            cap <- peek (getPtrOf (dqsCap f))
            b <- peekBufStruct (getPtrOf (dqsBuf f))
            pure (hd, curLen, cap, b)
        )
        $ \(hd, curLen, cap, b) ->
          let remRight = cap - hd
              span1 = ifThenElse (curLen `lt` remRight) curLen remRight
              span2 = curLen - span1
              physIndices = rangeS hd (hd + span1) <|> rangeS 0 span2
           in streamM physIndices (readBuf b)

reverseStream ::
  forall a m e.
  (DequeCtx m e a) =>
  Deque e a ->
  Stream m (Elem e a)
reverseStream dq =
  withStreamSetup
    (len dq)
    (\n -> streamM (reverseRangeS 0 n) (readDeque dq))

indexedStream ::
  (DequeCtx m e a) =>
  Deque e a ->
  Stream m (e Int64, Elem e a)
indexedStream dq = indexedS (stream dq)

drainToDeque ::
  forall a m e.
  (DequeCtx m e a) =>
  Alloc e ->
  Drain m (Elem e a) (Deque e a)
drainToDeque alloc = Drain $ \s -> do
  dq <- new alloc
  runDrain (drainAppendDequeBack alloc dq) s
  pure dq

fromStream ::
  (DequeCtx m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (Deque e a)
fromStream alloc = runDrain (drainToDeque alloc)

drainAppendDequeBack ::
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  Drain m (Elem e a) ()
drainAppendDequeBack alloc dq = drainForEach (pushBack alloc dq)

drainAppendDequeFront ::
  (DequeCtx m e a) =>
  Alloc e ->
  Deque e a ->
  Drain m (Elem e a) ()
drainAppendDequeFront alloc dq = drainForEach (pushFront alloc dq)

--------------------------------------------------------------------------------
-- Two-Span Contiguous SPMD / SIMD Gang Kernels Over Deque
--------------------------------------------------------------------------------

-- | Two-span contiguous in-place SIMD map over @[0 .. len - 1]@ across @Scalar@ and @SoA s@
-- (unaligned on the head-offset first span, 64B-aligned on the wrapped @[0 .. span2Len)@ span).
foreachSPMD ::
  forall w a m e.
  (DequeCtx m e a, LayoutSliceSPMD w m e a) =>
  Proxy w ->
  Deque e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw (Deque hdr) step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    cap <- peek (getPtrOf (dqsCap f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    let remRight = cap - hd
    span1Len <- letM (ifThenElse (curLen `lt` remRight) curLen remRight)
    span2Len <- letM (curLen - span1Len)
    foreachSliceBufSPMD pw False b hd span1Len 0 step
    whenM_ (span2Len `gt` 0) $
      foreachSliceBufSPMD pw True b 0 span2Len span1Len step

-- | Two-span contiguous read-only SIMD scan over @[0 .. len - 1]@ across @Scalar@ and @SoA s@.
traverseSPMD_ ::
  forall w a m e.
  (DequeCtx m e a, LayoutSliceSPMD w m e a) =>
  Proxy w ->
  Deque e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (Deque hdr) body = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (dqsLen f))
  whenM_ (curLen `gt` 0) $ do
    hd <- peek (getPtrOf (dqsHead f))
    cap <- peek (getPtrOf (dqsCap f))
    b <- peekBufStruct (getPtrOf (dqsBuf f))
    let remRight = cap - hd
    span1Len <- letM (ifThenElse (curLen `lt` remRight) curLen remRight)
    span2Len <- letM (curLen - span1Len)
    traverseSliceBufSPMD_ pw False b hd span1Len 0 body
    whenM_ (span2Len `gt` 0) $
      traverseSliceBufSPMD_ pw True b 0 span2Len span1Len body

-- | Two-span contiguous masked-safe SIMD fold over @[0 .. len - 1]@ across @Scalar@ and @SoA s@.
foldSPMD ::
  forall w a s m e.
  (DequeCtx m e a, LayoutSliceSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  Deque e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw dq s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseSPMD_ pw dq $ \logIdx val -> do
    cur <- readRef accRef
    nxt <- step cur logIdx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (Deque (JanusCT _) a) = Deque Identity a

deriving via
  ViaPtrHandle t (CStruct (DequeSpine a)) (Deque Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCArg t (Deque (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (DequeSpine a)) (Deque Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCValRet t (Deque (JanusCT t) a)
