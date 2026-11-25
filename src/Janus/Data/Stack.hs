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

module Janus.Data.Stack
  ( -- * Contiguous LIFO Stack Spine & Handle (Supports All 5 StorageLayouts!)
    StackSpine (..),
    Stack (..),
    StackCtx,

    -- * Lifecycle
    new,
    withCapacity,
    free,

    -- * Queries & Frame Scoping
    len,
    capacity,
    isEmpty,
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
    drainToStack,
    fromStream,
    drainAppendStack,

    -- * SPMD / SIMD Gang Kernels Over Stack Storage
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
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
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
import Janus.SPMD (SPMD, SPMDM, SPMDTyped)

-- | C-ABI control spine for a contiguous LIFO 'Stack'.
data StackSpine a f = StackSpine
  { stsBuf :: !(f (CStruct (Buf a))),
    stsLen :: !(f Int64),
    stsCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (StackSpine a)

newtype Stack (e :: K.Type -> K.Type) a = Stack
  { unStack :: e (Ptr (CStruct (StackSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (Stack Identity a)

deriving stock instance Show (Stack Identity a)

type StackCtx m e a = LayoutCtx m e a

-- | Allocate an empty 'Stack' (zero element-buffer allocation until first 'push' or 'reserve').
new ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  m (Stack e a)
new alloc = do
  hdr <- allocStruct @(CStruct (StackSpine a)) alloc
  let f = projectPtrs hdr
  b0 <- allocBuf @a alloc 0
  pokeBufStruct (getPtrOf (stsBuf f)) b0
  poke (getPtrOf (stsLen f)) 0
  poke (getPtrOf (stsCap f)) 0
  pure (Stack hdr)

-- | Allocate a 'Stack' pre-reserved for at least @initCap@ elements.
withCapacity ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (Stack e a)
withCapacity alloc initCap = do
  st <- new alloc
  whenM_ (initCap `gt` 0) $
    reserve alloc st initCap
  pure st

-- | Free the backing element buffer (if non-empty) and spine via @alloc@.
free ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Stack e a ->
  m ()
free alloc (Stack hdr) = do
  let f = projectPtrs hdr
  curCap <- peek (getPtrOf (stsCap f))
  whenM_ (curCap `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (stsBuf f))
    freeBuf alloc b curCap
  freeStruct alloc hdr

len :: (StackCtx m e a) => Stack e a -> m (e Int64)
len (Stack hdr) = peek (getPtrOf (stsLen (projectPtrs hdr)))

capacity :: (StackCtx m e a) => Stack e a -> m (e Int64)
capacity (Stack hdr) = peek (getPtrOf (stsCap (projectPtrs hdr)))

isEmpty :: (StackCtx m e a) => Stack e a -> m (e Bool)
isEmpty st = (`eq` 0) <$> len st

buf :: (StackCtx m e a) => Stack e a -> m (Buf a e)
buf (Stack hdr) = peekBufStruct (getPtrOf (stsBuf (projectPtrs hdr)))

-- | O(1) reset of stack depth to 0 while keeping capacity warm.
clear :: (StackCtx m e a) => Stack e a -> m ()
clear (Stack hdr) = poke (getPtrOf (stsLen (projectPtrs hdr))) 0

-- | Capture the current stack depth for subsequent O(1) 'rewindToDepth'.
markDepth :: (StackCtx m e a) => Stack e a -> m (e Int64)
markDepth = len

-- | O(1) bulk frame pop: unwinds stack depth back to @savedDepth@ (clamped to @[0, len]@).
rewindToDepth :: (StackCtx m e a) => Stack e a -> e Int64 -> m ()
rewindToDepth (Stack hdr) savedDepth = do
  let lp = getPtrOf (stsLen (projectPtrs hdr))
  curLen <- peek lp
  let clamped = ifThenElse (savedDepth `lt` 0) 0 (ifThenElse (savedDepth `lt` curLen) savedDepth curLen)
  poke lp clamped

-- | Ensure capacity for at least @curLen + additional@ elements via geometric doubling.
reserve ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Stack e a ->
  e Int64 ->
  m ()
reserve alloc (Stack hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  curCap <- peek (getPtrOf (stsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldBuf <- peekBufStruct (getPtrOf (stsBuf f))
    newBuf <- reallocBuf alloc oldBuf curCap newCap curLen
    pokeBufStruct (getPtrOf (stsBuf f)) newBuf
    poke (getPtrOf (stsCap f)) newCap

-- | Push @val@ onto the top of the stack in amortized O(1).
push ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Stack e a ->
  Elem e a ->
  m ()
push alloc st@(Stack hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  curCap <- peek (getPtrOf (stsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc st 1
  b <- peekBufStruct (getPtrOf (stsBuf f))
  writeBuf b curLen val
  poke (getPtrOf (stsLen f)) (curLen + 1)

-- | Pop the top element, returning @(found, valOrDefault)@.
pop ::
  forall a s m e.
  (StackCtx m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  Stack e a ->
  m (e Bool, e s)
pop defVal (Stack hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    let topIdx = curLen - 1
    b <- peekBufStruct (getPtrOf (stsBuf f))
    v <- readBuf b topIdx
    poke (getPtrOf (stsLen f)) topIdx
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

-- | CPS pop supporting all 'StorageLayout's (including 'AoS', 'SoA', 'AoSoA', 'SOAOA').
withPop ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (Stack hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let topIdx = curLen - 1
        b <- peekBufStruct (getPtrOf (stsBuf f))
        v <- readBuf b topIdx
        poke (getPtrOf (stsLen f)) topIdx
        onElem v
    )

-- | Inspect the top element without popping, returning @(found, valOrDefault)@.
peekTop ::
  forall a s m e.
  (StackCtx m e a, Elem e a ~ e s, KnownJanusType e s) =>
  e s ->
  Stack e a ->
  m (e Bool, e s)
peekTop defVal (Stack hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (stsBuf f))
    v <- readBuf b (curLen - 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekTop ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekTop (Stack hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        b <- peekBufStruct (getPtrOf (stsBuf f))
        readBuf b (curLen - 1) >>= onElem
    )

-- | Read element at bottom-up index @idx@ (@0@ = bottom, @len - 1@ = top).
readAt ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  e Int64 ->
  m (Elem e a)
readAt (Stack hdr) idx = do
  b <- peekBufStruct (getPtrOf (stsBuf (projectPtrs hdr)))
  readBuf b idx

--------------------------------------------------------------------------------
-- Streams & Drains
--------------------------------------------------------------------------------

-- | Non-destructive LIFO stream from top (@len - 1@) down to bottom (@0@).
topDownStream ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  Stream m (Elem e a)
topDownStream (Stack hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (stsLen f)) <*> peekBufStruct (getPtrOf (stsBuf f)))
        (\(n, b) -> streamM (reverseRangeS 0 n) (readBuf b))

-- | Non-destructive bottom-to-top stream (@0 .. len - 1@).
bottomUpStream ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  Stream m (Elem e a)
bottomUpStream (Stack hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (stsLen f)) <*> peekBufStruct (getPtrOf (stsBuf f)))
        (\(n, b) -> streamM (rangeS 0 n) (readBuf b))

-- | Destructive LIFO worklist stream: pops and yields the top element on each step.
-- Re-reads the spine on every pull so consumers may safely call 'push' (including resizing)
-- during DFS, flood-fill, or iterative tree/graph traversals!
popAllS ::
  forall a m e.
  (StackCtx m e a) =>
  Stack e a ->
  Stream m (Elem e a)
popAllS st =
  Stream $ pure $ ContT $ \kStep ->
    withPop st (kStep Stop) (kStep . Yield)

drainToStack ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Drain m (Elem e a) (Stack e a)
drainToStack alloc = Drain $ \s -> do
  st <- new alloc
  runDrain (drainAppendStack alloc st) s
  pure st

fromStream ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (Stack e a)
fromStream alloc = runDrain (drainToStack alloc)

drainAppendStack ::
  forall a m e.
  (StackCtx m e a) =>
  Alloc e ->
  Stack e a ->
  Drain m (Elem e a) ()
drainAppendStack alloc st = drainForEach (push alloc st)

--------------------------------------------------------------------------------
-- SPMD / SIMD Gang Kernels Over Stack
--------------------------------------------------------------------------------

-- | Layout-polymorphic 64B-aligned in-place SIMD map over bottom-to-top @[0 .. len - 1]@.
foreachSPMD ::
  forall w a m e.
  (StackCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  Stack e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw st step = do
  n <- len st
  b <- buf st
  foreachBufSPMD pw True b n step

-- | Layout-polymorphic 64B-aligned read-only SIMD scan over bottom-to-top @[0 .. len - 1]@.
traverseSPMD_ ::
  forall w a m e.
  (StackCtx m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  Stack e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw st body = do
  n <- len st
  b <- buf st
  traverseBufSPMD_ pw True b n body

-- | Layout-polymorphic 64B-aligned masked-safe SIMD fold over bottom-to-top @[0 .. len - 1]@.
foldSPMD ::
  forall w a s m e.
  (StackCtx m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  Stack e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw st s0 step = do
  n <- len st
  b <- buf st
  foldBufSPMD pw True b n s0 step

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type StackABI a = (Typeable a, JanusStruct (Buf a))

type instance JanusCHost (Stack (JanusCT _) a) = Stack Identity a

deriving via
  ViaPtrHandle t (CStruct (StackSpine a)) (Stack Identity a)
  instance
    (StackABI a) => JanusCArg t (Stack (JanusCT t) a)

deriving via
  ViaPtrHandle t (CStruct (StackSpine a)) (Stack Identity a)
  instance
    (StackABI a) => JanusCValRet t (Stack (JanusCT t) a)
