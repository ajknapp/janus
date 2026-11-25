{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Data.RingBuffer
  ( -- * Bounded Power-of-2 Ring Buffer Spine & Handle
    RingBufferSpine (..),
    RingBuffer (..),
    RingBufferCtx,

    -- * Lifecycle
    new,
    free,

    -- * Queries
    len,
    capacity,
    isEmpty,
    isFull,
    clear,

    -- * Bounded Enqueue / Dequeue / Overwrite
    tryPush,
    pushOverwrite,
    pushOverwrite_,
    tryPop,
    withPop,
    peekFront,
    withPeekFront,
    peekBack,
    withPeekBack,
    readRelative,
    writeRelative,

    -- * Wrap-Aware Streams & Drains
    stream,
    reverseStream,
    indexedStream,
    drainToRingBuffer,
    drainOverwriteRingBuffer,
    drainTryRingBuffer,

    -- * Two-Span Contiguous SPMD / SIMD Gang Kernels
    foreachSPMD,
    traverseSPMD_,
    foldSPMD,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Codensity (runCodensity)
import Control.Monad.Cont (runContT)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Command.While (whileM)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream (..), indexedS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD (SPMD, SPMDM, SPMDTyped, withSPMD)

-- | Spine struct for 'RingBuffer'.
-- Uses unmasked monotonically increasing 64-bit cursors @rbsReadPos@ and @rbsWritePos@
-- with @rbsMask = cap - 1@ so all @cap@ slots are usable without wasting a sentinel slot.
data RingBufferSpine a f = RingBufferSpine
  { rbsBuf :: !(f (CStruct (Buf a))),
    rbsMask :: !(f Word64),
    rbsCap :: !(f Int64),
    rbsReadPos :: !(f Word64),
    rbsWritePos :: !(f Word64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (RingBufferSpine a)

newtype RingBuffer (e :: K.Type -> K.Type) a = RingBuffer
  { unRingBuffer :: e (Ptr (CStruct (RingBufferSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (RingBuffer Identity a)

deriving stock instance Show (RingBuffer Identity a)

type RingBufferCtx m e a = LayoutCtx m e a

-- | Allocate a bounded 'RingBuffer' whose capacity is rounded up to the next power of 2 @>= max 1 minCap@.
new ::
  forall a m e.
  (RingBufferCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (RingBuffer e a)
new alloc minCap = do
  (_, cap) <- runDrain (drainFind 1 (`ge` minCap)) (iterateS (1 :: e Int64) (`lshift` 1))
  hdr <- allocStruct @(CStruct (RingBufferSpine a)) alloc
  b <- allocBuf @a alloc cap
  let f = projectPtrs hdr
      mask = toIntegral (cap - 1) :: e Word64
  pokeBufStruct (getPtrOf (rbsBuf f)) b
  poke (getPtrOf (rbsMask f)) mask
  poke (getPtrOf (rbsCap f)) cap
  poke (getPtrOf (rbsReadPos f)) 0
  poke (getPtrOf (rbsWritePos f)) 0
  pure (RingBuffer hdr)

free ::
  forall a m e.
  (RingBufferCtx m e a) =>
  Alloc e ->
  RingBuffer e a ->
  m ()
free alloc (RingBuffer hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (rbsCap f))
  b <- peekBufStruct (getPtrOf (rbsBuf f))
  freeBuf alloc b cap
  freeStruct alloc hdr

len :: (RingBufferCtx m e a) => RingBuffer e a -> m (e Int64)
len (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  pure (toIntegral (wp - rp))

capacity :: (RingBufferCtx m e a) => RingBuffer e a -> m (e Int64)
capacity (RingBuffer hdr) = peek (getPtrOf (rbsCap (projectPtrs hdr)))

isEmpty :: (RingBufferCtx m e a) => RingBuffer e a -> m (e Bool)
isEmpty (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  pure (wp `eq` rp)

isFull :: (RingBufferCtx m e a) => RingBuffer e a -> m (e Bool)
isFull (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  cap <- peek (getPtrOf (rbsCap f))
  pure (toIntegral (wp - rp) `eq` cap)

clear :: (RingBufferCtx m e a) => RingBuffer e a -> m ()
clear (RingBuffer hdr) = do
  let f = projectPtrs hdr
  poke (getPtrOf (rbsReadPos f)) 0
  poke (getPtrOf (rbsWritePos f)) 0

-- | Attempt to push @val@ without overwriting. Returns @true@ if pushed, @false@ if full.
tryPush ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Elem e a ->
  m (e Bool)
tryPush (RingBuffer hdr) val = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  cap <- peek (getPtrOf (rbsCap f))
  okRef <- newRef false
  whenM_ (toIntegral (wp - rp) `lt` cap) $ do
    mask <- peek (getPtrOf (rbsMask f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    let physIdx = toIntegral (wp `iand` mask) :: e Int64
    writeBuf b physIdx val
    poke (getPtrOf (rbsWritePos f)) (wp + 1)
    writeRef okRef true
  readRef okRef

-- | Push @val@, evicting the oldest element if full. Returns @true@ iff an element was overwritten.
pushOverwrite ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Elem e a ->
  m (e Bool)
pushOverwrite (RingBuffer hdr) val = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  cap <- peek (getPtrOf (rbsCap f))
  mask <- peek (getPtrOf (rbsMask f))
  b <- peekBufStruct (getPtrOf (rbsBuf f))
  overwroteRef <- newRef false
  whenM_ (toIntegral (wp - rp) `eq` cap) $ do
    poke (getPtrOf (rbsReadPos f)) (rp + 1)
    writeRef overwroteRef true
  let physIdx = toIntegral (wp `iand` mask) :: e Int64
  writeBuf b physIdx val
  poke (getPtrOf (rbsWritePos f)) (wp + 1)
  readRef overwroteRef

pushOverwrite_ ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Elem e a ->
  m ()
pushOverwrite_ rb val = do
  _ <- pushOverwrite rb val
  pure ()

tryPop ::
  forall a s m e.
  (RingBufferCtx m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  RingBuffer e a ->
  m (e Bool, e s)
tryPop defVal (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (wp `ne` rp) $ do
    mask <- peek (getPtrOf (rbsMask f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    let physIdx = toIntegral (rp `iand` mask) :: e Int64
    v <- readBuf b physIdx
    poke (getPtrOf (rbsReadPos f)) (rp + 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPop ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (RingBuffer hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  ifThenElseM_
    (wp `eq` rp)
    onEmpty
    ( do
        mask <- peek (getPtrOf (rbsMask f))
        b <- peekBufStruct (getPtrOf (rbsBuf f))
        let physIdx = toIntegral (rp `iand` mask) :: e Int64
        v <- readBuf b physIdx
        poke (getPtrOf (rbsReadPos f)) (rp + 1)
        onElem v
    )

peekFront ::
  forall a s m e.
  (RingBufferCtx m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  RingBuffer e a ->
  m (e Bool, e s)
peekFront defVal (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (wp `ne` rp) $ do
    mask <- peek (getPtrOf (rbsMask f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    v <- readBuf b (toIntegral (rp `iand` mask))
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekFront ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekFront (RingBuffer hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  ifThenElseM_
    (wp `eq` rp)
    onEmpty
    ( do
        mask <- peek (getPtrOf (rbsMask f))
        b <- peekBufStruct (getPtrOf (rbsBuf f))
        v <- readBuf b (toIntegral (rp `iand` mask))
        onElem v
    )

peekBack ::
  forall a s m e.
  (RingBufferCtx m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  RingBuffer e a ->
  m (e Bool, e s)
peekBack defVal (RingBuffer hdr) = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (wp `ne` rp) $ do
    mask <- peek (getPtrOf (rbsMask f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    v <- readBuf b (toIntegral ((wp - 1) `iand` mask))
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekBack ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekBack (RingBuffer hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  ifThenElseM_
    (wp `eq` rp)
    onEmpty
    ( do
        mask <- peek (getPtrOf (rbsMask f))
        b <- peekBufStruct (getPtrOf (rbsBuf f))
        v <- readBuf b (toIntegral ((wp - 1) `iand` mask))
        onElem v
    )

-- | Read element at logical offset @relIdx@ (@0 <= relIdx < len@) from the front (oldest element).
readRelative ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  e Int64 ->
  m (Elem e a)
readRelative (RingBuffer hdr) relIdx = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  mask <- peek (getPtrOf (rbsMask f))
  b <- peekBufStruct (getPtrOf (rbsBuf f))
  let physIdx = toIntegral ((rp + toIntegral relIdx) `iand` mask) :: e Int64
  readBuf b physIdx

writeRelative ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  e Int64 ->
  Elem e a ->
  m ()
writeRelative (RingBuffer hdr) relIdx val = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  mask <- peek (getPtrOf (rbsMask f))
  b <- peekBufStruct (getPtrOf (rbsBuf f))
  let physIdx = toIntegral ((rp + toIntegral relIdx) `iand` mask) :: e Int64
  writeBuf b physIdx val

-- | Two-span contiguous 'Stream' from oldest to newest element with zero per-element wrap masking.
stream ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Stream m (Elem e a)
stream (RingBuffer hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ( do
            rp <- peek (getPtrOf (rbsReadPos f))
            wp <- peek (getPtrOf (rbsWritePos f))
            mask <- peek (getPtrOf (rbsMask f))
            cap <- peek (getPtrOf (rbsCap f))
            b <- peekBufStruct (getPtrOf (rbsBuf f))
            pure (rp, wp, mask, cap, b)
        )
        $ \(rp, wp, mask, cap, b) ->
          let curLen = toIntegral (wp - rp) :: e Int64
              startIdx = toIntegral (rp `iand` mask) :: e Int64
              remToEnd = cap - startIdx
              span1 = ifThenElse (curLen `lt` remToEnd) curLen remToEnd
              span2 = curLen - span1
              physIndices = rangeS startIdx (startIdx + span1) <|> rangeS 0 span2
           in streamM physIndices (readBuf b)

reverseStream ::
  forall a m e.
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Stream m (Elem e a)
reverseStream rb =
  withStreamSetup
    (len rb)
    (\n -> streamM (reverseRangeS 0 n) (readRelative rb))

indexedStream ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Stream m (e Int64, Elem e a)
indexedStream rb = indexedS (stream rb)

-- | Drain a 'Stream' into a freshly allocated 'RingBuffer' of capacity @>= minCap@
-- using sliding-window overwrite semantics (retaining the last @cap@ elements).
drainToRingBuffer ::
  forall a m e.
  (RingBufferCtx m e a) =>
  Alloc e ->
  e Int64 ->
  Drain m (Elem e a) (RingBuffer e a)
drainToRingBuffer alloc minCap = Drain $ \s -> do
  rb <- new alloc minCap
  runDrain (drainOverwriteRingBuffer rb) s
  pure rb

drainOverwriteRingBuffer ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Drain m (Elem e a) ()
drainOverwriteRingBuffer rb = drainForEach (pushOverwrite_ rb)

-- | Drain elements into @rb@ until either the stream finishes or @rb@ becomes full,
-- returning the number of elements enqueued.
drainTryRingBuffer ::
  (RingBufferCtx m e a) =>
  RingBuffer e a ->
  Drain m (Elem e a) (e Int64)
drainTryRingBuffer rb = Drain $ \(Stream s) -> do
  cntRef <- newRef 0
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield x -> do
          ok <- tryPush rb x
          ifThenElseM_
            ok
            (readRef cntRef >>= \n -> writeRef cntRef (n + 1))
            (writeRef c false)
        Skip -> pure ()
        Stop -> writeRef c false
  readRef cntRef

--------------------------------------------------------------------------------
-- Two-Span Contiguous SPMD / SIMD Gang Kernels Over RingBuffer
--------------------------------------------------------------------------------

-- | Two-span contiguous in-place SIMD map over @[0 .. len - 1]@ across @Scalar@ and @SoA s@
-- (unaligned on the head-offset first span, 64B-aligned on the wrapped @[0 .. span2Len)@ span).
foreachSPMD ::
  forall w a m e.
  (RingBufferCtx m e a, LayoutSliceSPMD w m e a) =>
  Proxy w ->
  RingBuffer e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw (RingBuffer hdr) step = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  curLen <- letM (toIntegral (wp - rp) :: e Int64)
  whenM_ (curLen `gt` 0) $ do
    mask <- peek (getPtrOf (rbsMask f))
    cap <- peek (getPtrOf (rbsCap f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    headIdx <- letM (toIntegral (rp `iand` mask) :: e Int64)
    let remToEnd = cap - headIdx
    span1Len <- letM (ifThenElse (curLen `lt` remToEnd) curLen remToEnd)
    span2Len <- letM (curLen - span1Len)
    foreachSliceBufSPMD pw False b headIdx span1Len 0 step
    whenM_ (span2Len `gt` 0) $
      foreachSliceBufSPMD pw True b 0 span2Len span1Len step

-- | Two-span contiguous read-only SIMD scan over @[0 .. len - 1]@ across @Scalar@ and @SoA s@.
traverseSPMD_ ::
  forall w a m e.
  (RingBufferCtx m e a, LayoutSliceSPMD w m e a) =>
  Proxy w ->
  RingBuffer e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (RingBuffer hdr) body = do
  let f = projectPtrs hdr
  rp <- peek (getPtrOf (rbsReadPos f))
  wp <- peek (getPtrOf (rbsWritePos f))
  curLen <- letM (toIntegral (wp - rp) :: e Int64)
  whenM_ (curLen `gt` 0) $ do
    mask <- peek (getPtrOf (rbsMask f))
    cap <- peek (getPtrOf (rbsCap f))
    b <- peekBufStruct (getPtrOf (rbsBuf f))
    headIdx <- letM (toIntegral (rp `iand` mask) :: e Int64)
    let remToEnd = cap - headIdx
    span1Len <- letM (ifThenElse (curLen `lt` remToEnd) curLen remToEnd)
    span2Len <- letM (curLen - span1Len)
    traverseSliceBufSPMD_ pw False b headIdx span1Len 0 body
    whenM_ (span2Len `gt` 0) $
      traverseSliceBufSPMD_ pw True b 0 span2Len span1Len body

-- | Two-span contiguous masked-safe SIMD fold over @[0 .. len - 1]@ across @Scalar@ and @SoA s@.
foldSPMD ::
  forall w a s m e.
  (RingBufferCtx m e a, LayoutSliceSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  RingBuffer e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw rb s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseSPMD_ pw rb $ \logIdx val -> do
    cur <- readRef accRef
    nxt <- step cur logIdx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (RingBuffer (JanusCT _) a) = RingBuffer Identity a

deriving via
  ViaPtrHandle t (CStruct (RingBufferSpine a)) (RingBuffer Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCArg t (RingBuffer (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (RingBufferSpine a)) (RingBuffer Identity a)
  instance
    (Typeable a, JanusStruct (Buf a)) => JanusCValRet t (RingBuffer (JanusCT t) a)
