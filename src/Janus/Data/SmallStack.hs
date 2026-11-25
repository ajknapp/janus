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

module Janus.Data.SmallStack
  ( -- * Inline Small-Buffer-Optimized LIFO Stack Spine & Handle
    SmallStackSpine (..),
    SmallStack (..),
    SmallStackCtx,

    -- * Lifecycle & Stack-Scoped Helper
    new,
    free,
    withSmallStack,

    -- * Queries & Frame Scoping
    len,
    capacity,
    isEmpty,
    isSpilled,
    buf,
    clear,
    reserve,
    markDepth,
    rewindToDepth,

    -- * LIFO Mutation & Inspection
    push,
    pop,
    withPop,
    peekTop,
    withPeekTop,
    readAt,

    -- * Streams (Including Dynamic Worklist `popAllS`) & Drains
    topDownStream,
    bottomUpStream,
    popAllS,
    drainToSmallStack,
    fromStream,
    drainAppendSmallStack,

    -- * SPMD / SIMD Gang Kernels Over Inline & Spilled Storage
    foreachSPMD,
    traverseSPMD_,
    foldSPMD,
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
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), allocaAligned)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream (..), iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD (SPMD, SPMDM, SPMDTyped, withSPMD)

-- | Spine struct embedding @n@ inline elements directly inside the struct.
-- While @sstsLen <= n@ (and no larger 'reserve' has been requested), @sstsIsHeap == 0@
-- and @sstsBuf@ points directly into @sstsInline@ with zero element-buffer allocations.
data SmallStackSpine (n :: Nat) a f = SmallStackSpine
  { sstsBuf :: !(f (CStruct (Buf a))),
    sstsLen :: !(f Int64),
    sstsCap :: !(f Int64),
    sstsIsHeap :: !(f Int64),
    sstsInline :: !(f (CStruct (InlineBuf n a)))
  }
  deriving stock (Generic)

instance
  ( KnownNat n,
    Typeable n,
    Typeable a,
    JanusStruct (Buf a),
    JanusStruct (InlineBuf n a)
  ) =>
  JanusStruct (SmallStackSpine n a)

newtype SmallStack (n :: Nat) (e :: K.Type -> K.Type) a = SmallStack
  { unSmallStack :: e (Ptr (CStruct (SmallStackSpine n a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (SmallStack n Identity a)

deriving stock instance Show (SmallStack n Identity a)

type SmallStackCtx n m e a = InlineLayoutCtx n m e a

initSmallStackSpine ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  e (Ptr (CStruct (SmallStackSpine n a))) ->
  m (SmallStack n e a)
initSmallStackSpine hdr = do
  let f = projectPtrs hdr
      inlineCap = fromIntegral (natVal (Proxy @n)) :: e Int64
  bInline <- inlineAsBuf (Proxy @n) (getPtrOf (sstsInline f))
  pokeBufStruct (getPtrOf (sstsBuf f)) bInline
  poke (getPtrOf (sstsLen f)) 0
  poke (getPtrOf (sstsCap f)) inlineCap
  poke (getPtrOf (sstsIsHeap f)) 0
  pure (SmallStack hdr)

-- | Allocate the 'SmallStack' spine via @alloc@. Zero element-buffer allocations occur while @len <= n@.
new ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  m (SmallStack n e a)
new alloc = do
  hdr <- allocStruct @(CStruct (SmallStackSpine n a)) alloc
  initSmallStackSpine hdr

-- | Free any spilled heap buffer and free the spine via @alloc@.
free ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  SmallStack n e a ->
  m ()
free alloc (SmallStack hdr) = do
  let f = projectPtrs hdr
  isHeap <- peek (getPtrOf (sstsIsHeap f))
  whenM_ (isHeap `ne` 0) $ do
    curCap <- peek (getPtrOf (sstsCap f))
    b <- peekBufStruct (getPtrOf (sstsBuf f))
    freeBuf alloc b curCap
  freeStruct alloc hdr

-- | Stack-scoped 'SmallStack' in 'IO': allocates the spine on a 64B-aligned C stack frame
-- (performing zero allocator calls unless depth exceeds @n@!) and cleans up any spilled buffer on exit.
withSmallStack ::
  forall n a r.
  (SmallStackCtx n IO Identity a) =>
  Alloc Identity ->
  (SmallStack n Identity a -> IO r) ->
  IO r
withSmallStack alloc body =
  allocaAligned 1 $ \(rawHdr :: Ptr (CStruct (SmallStackSpine n a))) -> do
    st <- initSmallStackSpine (Identity rawHdr)
    res <- body st
    let f = projectPtrs (Identity rawHdr)
    Identity isHeap <- peek (getPtrOf (sstsIsHeap f))
    if isHeap /= 0
      then do
        curCap <- peek (getPtrOf (sstsCap f))
        b <- peekBufStruct (getPtrOf (sstsBuf f))
        freeBuf alloc b curCap
      else pure ()
    pure res

len :: (SmallStackCtx n m e a) => SmallStack n e a -> m (e Int64)
len (SmallStack hdr) = peek (getPtrOf (sstsLen (projectPtrs hdr)))

capacity :: (SmallStackCtx n m e a) => SmallStack n e a -> m (e Int64)
capacity (SmallStack hdr) = peek (getPtrOf (sstsCap (projectPtrs hdr)))

isEmpty :: (SmallStackCtx n m e a) => SmallStack n e a -> m (e Bool)
isEmpty st = (`eq` 0) <$> len st

isSpilled :: (SmallStackCtx n m e a) => SmallStack n e a -> m (e Bool)
isSpilled (SmallStack hdr) = (`ne` 0) <$> peek (getPtrOf (sstsIsHeap (projectPtrs hdr)))

buf :: (SmallStackCtx n m e a) => SmallStack n e a -> m (Buf a e)
buf (SmallStack hdr) = peekBufStruct (getPtrOf (sstsBuf (projectPtrs hdr)))

clear :: (SmallStackCtx n m e a) => SmallStack n e a -> m ()
clear (SmallStack hdr) = poke (getPtrOf (sstsLen (projectPtrs hdr))) 0

markDepth :: (SmallStackCtx n m e a) => SmallStack n e a -> m (e Int64)
markDepth = len

rewindToDepth :: (SmallStackCtx n m e a) => SmallStack n e a -> e Int64 -> m ()
rewindToDepth (SmallStack hdr) savedDepth = do
  let lp = getPtrOf (sstsLen (projectPtrs hdr))
  curLen <- peek lp
  let clamped = ifThenElse (savedDepth `lt` 0) 0 (ifThenElse (savedDepth `lt` curLen) savedDepth curLen)
  poke lp clamped

reserve ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  SmallStack n e a ->
  e Int64 ->
  m ()
reserve alloc (SmallStack hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  curCap <- peek (getPtrOf (sstsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldBuf <- peekBufStruct (getPtrOf (sstsBuf f))
    wasHeap <- peek (getPtrOf (sstsIsHeap f))
    ifThenElseM_
      (wasHeap `eq` 0)
      ( do
          newBuf <- allocBuf @a alloc newCap
          whenM_ (curLen `gt` 0) $
            copyBuf newBuf 0 oldBuf 0 curLen
          pokeBufStruct (getPtrOf (sstsBuf f)) newBuf
          poke (getPtrOf (sstsCap f)) newCap
          poke (getPtrOf (sstsIsHeap f)) 1
      )
      ( do
          newBuf <- reallocBuf alloc oldBuf curCap newCap curLen
          pokeBufStruct (getPtrOf (sstsBuf f)) newBuf
          poke (getPtrOf (sstsCap f)) newCap
      )

push ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  SmallStack n e a ->
  Elem e a ->
  m ()
push alloc st@(SmallStack hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  curCap <- peek (getPtrOf (sstsCap f))
  whenM_ (curLen `ge` curCap) $
    reserve alloc st 1
  b <- peekBufStruct (getPtrOf (sstsBuf f))
  writeBuf b curLen val
  poke (getPtrOf (sstsLen f)) (curLen + 1)

pop ::
  forall n a s m e.
  (SmallStackCtx n m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  SmallStack n e a ->
  m (e Bool, e s)
pop defVal (SmallStack hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    let topIdx = curLen - 1
    b <- peekBufStruct (getPtrOf (sstsBuf f))
    v <- readBuf b topIdx
    poke (getPtrOf (sstsLen f)) topIdx
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPop ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (SmallStack hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let topIdx = curLen - 1
        b <- peekBufStruct (getPtrOf (sstsBuf f))
        v <- readBuf b topIdx
        poke (getPtrOf (sstsLen f)) topIdx
        onElem v
    )

peekTop ::
  forall n a s m e.
  (SmallStackCtx n m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  SmallStack n e a ->
  m (e Bool, e s)
peekTop defVal (SmallStack hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (sstsBuf f))
    v <- readBuf b (curLen - 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekTop ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekTop (SmallStack hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        b <- peekBufStruct (getPtrOf (sstsBuf f))
        readBuf b (curLen - 1) >>= onElem
    )

readAt ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  e Int64 ->
  m (Elem e a)
readAt (SmallStack hdr) idx = do
  b <- peekBufStruct (getPtrOf (sstsBuf (projectPtrs hdr)))
  readBuf b idx

--------------------------------------------------------------------------------
-- Streams & Drains
--------------------------------------------------------------------------------

topDownStream ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  Stream m (Elem e a)
topDownStream (SmallStack hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (sstsLen f)) <*> peekBufStruct (getPtrOf (sstsBuf f)))
        (\(mLen, b) -> streamM (reverseRangeS 0 mLen) (readBuf b))

bottomUpStream ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  Stream m (Elem e a)
bottomUpStream (SmallStack hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (sstsLen f)) <*> peekBufStruct (getPtrOf (sstsBuf f)))
        (\(mLen, b) -> streamM (rangeS 0 mLen) (readBuf b))

-- | Destructive LIFO worklist stream safe under interleaved 'push' (including inline-to-heap spill).
popAllS ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  SmallStack n e a ->
  Stream m (Elem e a)
popAllS st =
  Stream $ pure $ ContT $ \kStep ->
    withPop st (kStep Stop) (kStep . Yield)

drainToSmallStack ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  Drain m (Elem e a) (SmallStack n e a)
drainToSmallStack alloc = Drain $ \s -> do
  st <- new @n alloc
  runDrain (drainAppendSmallStack alloc st) s
  pure st

fromStream ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (SmallStack n e a)
fromStream alloc = runDrain (drainToSmallStack @n alloc)

drainAppendSmallStack ::
  forall n a m e.
  (SmallStackCtx n m e a) =>
  Alloc e ->
  SmallStack n e a ->
  Drain m (Elem e a) ()
drainAppendSmallStack alloc st = drainForEach (push alloc st)

--------------------------------------------------------------------------------
-- SPMD / SIMD Gang Kernels Over SmallStack
--------------------------------------------------------------------------------

-- | Layout-polymorphic in-place SIMD map over bottom-to-top @[0 .. len - 1]@,
-- dispatching 64B-aligned loads/stores when spilled to the heap and unaligned loads/stores when inline.
foreachSPMD ::
  forall w n a m e.
  (SmallStackCtx n m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  SmallStack n e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw (SmallStack hdr) step = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  b <- peekBufStruct (getPtrOf (sstsBuf f))
  isHeap <- peek (getPtrOf (sstsIsHeap f))
  ifThenElseM_
    (isHeap `ne` 0)
    (foreachBufSPMD pw True b curLen step)
    (foreachBufSPMD pw False b curLen step)

-- | Layout-polymorphic read-only SIMD scan over bottom-to-top @[0 .. len - 1]@.
traverseSPMD_ ::
  forall w n a m e.
  (SmallStackCtx n m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  SmallStack n e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (SmallStack hdr) body = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (sstsLen f))
  b <- peekBufStruct (getPtrOf (sstsBuf f))
  isHeap <- peek (getPtrOf (sstsIsHeap f))
  ifThenElseM_
    (isHeap `ne` 0)
    (traverseBufSPMD_ pw True b curLen body)
    (traverseBufSPMD_ pw False b curLen body)

-- | Layout-polymorphic masked-safe SIMD fold over bottom-to-top @[0 .. len - 1]@.
foldSPMD ::
  forall w n a s m e.
  (SmallStackCtx n m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  SmallStack n e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw st s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseSPMD_ pw st $ \idx val -> do
    cur <- readRef accRef
    nxt <- step cur idx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type SmallStackABI n a =
  ( KnownNat n,
    Typeable n,
    Typeable a,
    JanusStruct (Buf a),
    JanusStruct (InlineBuf n a)
  )

type instance JanusCHost (SmallStack n (JanusCT _) a) = SmallStack n Identity a

deriving via
  ViaPtrHandle t (CStruct (SmallStackSpine n a)) (SmallStack n Identity a)
  instance
    (SmallStackABI n a) => JanusCArg t (SmallStack n (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (SmallStackSpine n a)) (SmallStack n Identity a)
  instance
    (SmallStackABI n a) => JanusCValRet t (SmallStack n (JanusCT t) a)
