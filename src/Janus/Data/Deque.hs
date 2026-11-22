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
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
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
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCEval (Deque JanusC a -> r) = Deque Identity a -> JanusCEval r

type instance JanusCRetType (Deque JanusC a -> r) = JanusCRetType r

instance (Typeable a, JanusStruct (Buf a), JanusCParam r) => JanusCParam (Deque JanusC a -> r) where
  jcparam = mkPtrHandleJCParam Deque

instance (Typeable a, JanusStruct (Buf a), JanusCEvaluate r) => JanusCEvaluate (Deque JanusC a -> r) where
  jceval = mkPtrHandleJCEval unDeque Deque

type instance JanusCEval (JanusCM (Deque JanusC a)) = IO (Deque Identity a)

type instance JanusCRetType (JanusCM (Deque JanusC a)) = Deque Identity a

instance (Typeable a, JanusStruct (Buf a)) => JanusCParam (JanusCM (Deque JanusC a)) where
  jcparam = mkPtrHandleJCRetParam unDeque

instance (Typeable a, JanusStruct (Buf a)) => JanusCEvaluate (JanusCM (Deque JanusC a)) where
  jceval = mkPtrHandleJCRetEval Deque

type instance LinuxX86_64CEval (Deque LinuxX86_64C a -> r) = Deque Identity a -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (Deque LinuxX86_64C a -> r) = LinuxX86_64BaseRet r

instance (Typeable a, JanusStruct (Buf a), LinuxX86_64Param r) => LinuxX86_64Param (Deque LinuxX86_64C a -> r) where
  x86param = mkPtrHandleX86Param Deque

instance (Typeable a, JanusStruct (Buf a), X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (Deque LinuxX86_64C a -> r) where
  x86eval = mkPtrHandleX86EvalParam unDeque Deque

instance (Typeable a, JanusStruct (Buf a), X86DeferCont r) => X86DeferCall (Deque LinuxX86_64C a -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unDeque Deque

instance (Typeable a, JanusStruct (Buf a), X86DeferCont r) => X86DeferCont (Deque LinuxX86_64C a -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unDeque Deque

type instance LinuxX86_64CEval (LinuxX86_64CM (Deque LinuxX86_64C a)) = IO (Deque Identity a)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (Deque LinuxX86_64C a)) = Deque Identity a

instance (Typeable a, JanusStruct (Buf a)) => LinuxX86_64Param (LinuxX86_64CM (Deque LinuxX86_64C a)) where
  x86param = mkPtrHandleX86RetParam unDeque

instance (Typeable a, JanusStruct (Buf a)) => LinuxX86_64Evaluate (LinuxX86_64CM (Deque LinuxX86_64C a)) where
  x86eval = mkPtrHandleX86RetEval Deque

instance (Typeable a, JanusStruct (Buf a)) => X86DeferCall (LinuxX86_64CM (Deque LinuxX86_64C a)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg Deque

instance (Typeable a, JanusStruct (Buf a)) => X86DeferCont (LinuxX86_64CM (Deque LinuxX86_64C a)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont Deque

instance (Typeable a, JanusStruct (Buf a), LinuxX86_64Param r, JanusCTH r) => JanusCTH (Deque LinuxX86_64C a -> r) where
  type JanusCTHEval (Deque LinuxX86_64C a -> r) = Deque Identity a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(Deque Identity a)) : ps, retSpec)

instance (Typeable a, JanusStruct (Buf a)) => JanusCTH (LinuxX86_64CM (Deque LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (Deque LinuxX86_64C a)) = IO (Deque Identity a)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(Deque Identity a)) (Proxy @(CStruct (DequeSpine a))))
