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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Data.IntrusiveList
  ( -- * Embedded Intrusive Hook & Multi-Hook `container_of` Selector
    RawListHook (..),
    HookSpec (..),
    mkHookSpec,
    IntrusiveListSpine (..),
    IntrusiveList (..),
    IntrusiveListCtx,

    -- * Allocating Spine & Node Helpers (Explicit `Alloc e`)
    new,
    freeListSpine,
    freeAllNodesAndSpine,
    allocNode,
    freeNode,
    allocAndPushBack,
    allocAndPushFront,

    -- * Zero-Allocation Intrusive Link Operations (No `Alloc e` Required!)
    len,
    isEmpty,
    initNodeHook,
    isNodeLinked,
    pushFrontNode,
    pushBackNode,
    insertBeforeNode,
    insertAfterNode,
    unlinkNode,
    popFrontNode,
    popBackNode,
    frontNode,
    backNode,
    spliceBack,
    spliceFront,
    clearNodes,

    -- * Prefetch-Safe Unlink-Safe Streams & Drains
    nodePtrStream,
    reverseNodePtrStream,
    elemStream,
    reverseElemStream,
    drainAppendNodesList,
    drainToList,
    fromStream,
    drainAllocAppendList,
  )
where

import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import Janus.Alloc.Dynamic (CmdAllocBase)
import Janus.Alloc.Policy (Alloc, CmdAlloc, allocStruct, freeStruct)
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream (..), streamM)
import Janus.Data.HKD
import Janus.Data.Layout (StructCtx)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Eq (ExpEq (..))

-- | 16-byte doubly-linked intrusive hook embedded directly inside user HKD node structs:
-- @myHook :: !(f (CStruct RawListHook))@.
-- A single node struct may embed multiple 'RawListHook' fields to belong to multiple
-- 'IntrusiveList's simultaneously!
data RawListHook f = RawListHook
  { rlhPrev :: !(f (Ptr Word8)),
    rlhNext :: !(f (Ptr Word8))
  }
  deriving stock (Generic)

deriving stock instance Eq (RawListHook Identity)

deriving stock instance Show (RawListHook Identity)

instance JanusStruct RawListHook

-- | Compile-time/host C-ABI byte offset of a chosen 'RawListHook' field inside @CStruct s@.
newtype HookSpec (s :: (K.Type -> K.Type) -> K.Type) = HookSpec
  { hookByteOffset :: Int
  }
  deriving stock (Eq, Show)

-- | Build a 'HookSpec' from a projected field selector (e.g. @mkHookSpec taskQueueHook@).
mkHookSpec ::
  forall s.
  ( JanusStruct s,
    Generic (s (PtrOf Identity)),
    GJanusStructProject Identity (Rep (s Identity)) (Rep (s (PtrOf Identity)))
  ) =>
  (s (PtrOf Identity) -> PtrOf Identity (CStruct RawListHook)) ->
  HookSpec s
mkHookSpec sel = HookSpec (fieldByteOffset @s sel)

-- | Circular sentinel-headed doubly-linked list spine:
-- * Empty list has @sentinel.prev == &sentinel@ and @sentinel.next == &sentinel@ (zero NULL checks!).
-- * Stores @ilsHookOffset@ in the spine so all link/iteration functions work uniformly across Host & C FFI!
data IntrusiveListSpine f = IntrusiveListSpine
  { ilsSentinel :: !(f (CStruct RawListHook)),
    ilsHookOffset :: !(f Int64),
    ilsLen :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (IntrusiveListSpine Identity)

deriving stock instance Show (IntrusiveListSpine Identity)

instance JanusStruct IntrusiveListSpine

newtype IntrusiveList (s :: (K.Type -> K.Type) -> K.Type) (e :: K.Type -> K.Type) = IntrusiveList
  { unIntrusiveList :: e (Ptr (CStruct IntrusiveListSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (IntrusiveList s Identity)

deriving stock instance Show (IntrusiveList s Identity)

type IntrusiveListCtx m e s = StructCtx m e s

asHookPtr ::
  (ExpPtrCast e, KnownJanusType e Word8, KnownJanusType e (CStruct RawListHook)) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct RawListHook))
asHookPtr p = fromVoidPtr (toVoidPtr p)

toRawHookPtr ::
  (ExpPtrCast e, KnownJanusType e (CStruct RawListHook), KnownJanusType e Word8) =>
  e (Ptr (CStruct RawListHook)) ->
  e (Ptr Word8)
toRawHookPtr p = fromVoidPtr (toVoidPtr p)

sentinelHookPtr ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct RawListHook))
sentinelHookPtr (IntrusiveList hdr) =
  getPtrOf (ilsSentinel (projectPtrs hdr))

nodeToHookRaw ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  e Int64 ->
  e (Ptr (CStruct s)) ->
  e (Ptr Word8)
nodeToHookRaw hookOff pNode =
  ptrAdd (fromVoidPtr (toVoidPtr pNode) :: e (Ptr Word8)) hookOff

hookRawToNode ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  e Int64 ->
  e (Ptr Word8) ->
  e (Ptr (CStruct s))
hookRawToNode hookOff pHookRaw =
  fromVoidPtr (toVoidPtr (ptrAdd pHookRaw (negate hookOff)))

--------------------------------------------------------------------------------
-- Allocating Lifecycle & Node Helpers (Explicit `Alloc e`)
--------------------------------------------------------------------------------

-- | Allocate and initialize an empty circular sentinel 'IntrusiveList' for the given 'HookSpec'.
new ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  Alloc e ->
  HookSpec s ->
  m (IntrusiveList s e)
new alloc (HookSpec off) = do
  hdr <- allocStruct @(CStruct IntrusiveListSpine) alloc
  let f = projectPtrs hdr
      pSent = getPtrOf (ilsSentinel f)
      rawSent = toRawHookPtr pSent
      sf = projectPtrs pSent
  poke (getPtrOf (rlhPrev sf)) rawSent
  poke (getPtrOf (rlhNext sf)) rawSent
  poke (getPtrOf (ilsHookOffset f)) (fromIntegral off)
  poke (getPtrOf (ilsLen f)) 0
  pure (IntrusiveList hdr)

-- | Free ONLY the list spine (leaving any nodes untouched).
freeListSpine ::
  (IntrusiveListCtx m e s) =>
  Alloc e ->
  IntrusiveList s e ->
  m ()
freeListSpine alloc (IntrusiveList hdr) = freeStruct alloc hdr

-- | Prefetch-safe walk freeing every currently linked node via @alloc@, then freeing the list spine.
freeAllNodesAndSpine ::
  (IntrusiveListCtx m e s) =>
  Alloc e ->
  IntrusiveList s e ->
  m ()
freeAllNodesAndSpine alloc lst = do
  runDrain (drainForEach (freeStruct alloc)) (nodePtrStream lst)
  freeListSpine alloc lst

-- | Allocate and initialize a node struct via @alloc@ from an HKD value @s e@.
allocNode ::
  forall s m e.
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocNode alloc val = do
  p <- allocStruct @(CStruct s) alloc
  pokeStruct p val
  pure p

freeNode ::
  (IntrusiveListCtx m e s, CmdAlloc m e) =>
  Alloc e ->
  e (Ptr (CStruct s)) ->
  m ()
freeNode = freeStruct

allocAndPushBack ::
  forall s m e.
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  IntrusiveList s e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocAndPushBack alloc lst val = do
  pNode <- allocNode alloc val
  initNodeHook lst pNode
  pushBackNode lst pNode
  pure pNode

allocAndPushFront ::
  forall s m e.
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  IntrusiveList s e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocAndPushFront alloc lst val = do
  pNode <- allocNode alloc val
  initNodeHook lst pNode
  pushFrontNode lst pNode
  pure pNode

--------------------------------------------------------------------------------
-- Zero-Allocation Intrusive Link Operations (No `Alloc e` Required!)
--------------------------------------------------------------------------------

len :: (IntrusiveListCtx m e s) => IntrusiveList s e -> m (e Int64)
len (IntrusiveList hdr) = peek (getPtrOf (ilsLen (projectPtrs hdr)))

isEmpty :: (IntrusiveListCtx m e s) => IntrusiveList s e -> m (e Bool)
isEmpty lst = do
  let pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  nxt <- peek (getPtrOf (rlhNext (projectPtrs pSent)))
  pure (ptrToWord64Expr nxt `eq` ptrToWord64Expr rawSent)

initNodeHook ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  m ()
initNodeHook (IntrusiveList hdr) pNode = do
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asHookPtr (nodeToHookRaw off pNode))
  poke (getPtrOf (rlhPrev hf)) nullPtr
  poke (getPtrOf (rlhNext hf)) nullPtr

isNodeLinked ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  m (e Bool)
isNodeLinked (IntrusiveList hdr) pNode = do
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asHookPtr (nodeToHookRaw off pNode))
  nxt <- peek (getPtrOf (rlhNext hf))
  pure (ptrToWord64Expr nxt `ne` 0)

linkHookBetween ::
  (CmdAllocBase m e) =>
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  m ()
linkHookBetween prevRaw nextRaw nodeHookRaw = do
  let pf = projectPtrs (asHookPtr prevRaw)
      nf = projectPtrs (asHookPtr nextRaw)
      hf = projectPtrs (asHookPtr nodeHookRaw)
  poke (getPtrOf (rlhPrev hf)) prevRaw
  poke (getPtrOf (rlhNext hf)) nextRaw
  poke (getPtrOf (rlhNext pf)) nodeHookRaw
  poke (getPtrOf (rlhPrev nf)) nodeHookRaw

pushFrontNode ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  m ()
pushFrontNode lst@(IntrusiveList hdr) pNode = do
  let f = projectPtrs hdr
      pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset f))
  curLen <- peek (getPtrOf (ilsLen f))
  firstRaw <- peek (getPtrOf (rlhNext (projectPtrs pSent)))
  linkHookBetween rawSent firstRaw (nodeToHookRaw off pNode)
  poke (getPtrOf (ilsLen f)) (curLen + 1)

pushBackNode ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  m ()
pushBackNode lst@(IntrusiveList hdr) pNode = do
  let f = projectPtrs hdr
      pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset f))
  curLen <- peek (getPtrOf (ilsLen f))
  lastRaw <- peek (getPtrOf (rlhPrev (projectPtrs pSent)))
  linkHookBetween lastRaw rawSent (nodeToHookRaw off pNode)
  poke (getPtrOf (ilsLen f)) (curLen + 1)

insertBeforeNode ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  e (Ptr (CStruct s)) ->
  m ()
insertBeforeNode (IntrusiveList hdr) anchorNode newNode = do
  let f = projectPtrs hdr
  off <- peek (getPtrOf (ilsHookOffset f))
  curLen <- peek (getPtrOf (ilsLen f))
  let anchorRaw = nodeToHookRaw off anchorNode
      newRaw = nodeToHookRaw off newNode
  prevRaw <- peek (getPtrOf (rlhPrev (projectPtrs (asHookPtr anchorRaw))))
  linkHookBetween prevRaw anchorRaw newRaw
  poke (getPtrOf (ilsLen f)) (curLen + 1)

insertAfterNode ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  e (Ptr (CStruct s)) ->
  m ()
insertAfterNode (IntrusiveList hdr) anchorNode newNode = do
  let f = projectPtrs hdr
  off <- peek (getPtrOf (ilsHookOffset f))
  curLen <- peek (getPtrOf (ilsLen f))
  let anchorRaw = nodeToHookRaw off anchorNode
      newRaw = nodeToHookRaw off newNode
  nextRaw <- peek (getPtrOf (rlhNext (projectPtrs (asHookPtr anchorRaw))))
  linkHookBetween anchorRaw nextRaw newRaw
  poke (getPtrOf (ilsLen f)) (curLen + 1)

-- | O(1) branchless circular unlink (also zeroes the unlinked node's hook pointers).
unlinkNode ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  e (Ptr (CStruct s)) ->
  m ()
unlinkNode (IntrusiveList hdr) pNode = do
  let f = projectPtrs hdr
  off <- peek (getPtrOf (ilsHookOffset f))
  let hf = projectPtrs (asHookPtr (nodeToHookRaw off pNode))
  nxtRaw <- peek (getPtrOf (rlhNext hf))
  whenM_ (ptrToWord64Expr nxtRaw `ne` 0) $ do
    prvRaw <- peek (getPtrOf (rlhPrev hf))
    poke (getPtrOf (rlhNext (projectPtrs (asHookPtr prvRaw)))) nxtRaw
    poke (getPtrOf (rlhPrev (projectPtrs (asHookPtr nxtRaw)))) prvRaw
    poke (getPtrOf (rlhPrev hf)) nullPtr
    poke (getPtrOf (rlhNext hf)) nullPtr
    curLen <- peek (getPtrOf (ilsLen f))
    poke (getPtrOf (ilsLen f)) (curLen - 1)

frontNode ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  m (e (Ptr (CStruct s)))
frontNode lst@(IntrusiveList hdr) = do
  let pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  firstRaw <- peek (getPtrOf (rlhNext (projectPtrs pSent)))
  outRef <- newRef (nullPtr @e @(CStruct s))
  whenM_ (ptrToWord64Expr firstRaw `ne` ptrToWord64Expr rawSent) $
    writeRef outRef (hookRawToNode off firstRaw)
  readRef outRef

backNode ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  m (e (Ptr (CStruct s)))
backNode lst@(IntrusiveList hdr) = do
  let pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  lastRaw <- peek (getPtrOf (rlhPrev (projectPtrs pSent)))
  outRef <- newRef (nullPtr @e @(CStruct s))
  whenM_ (ptrToWord64Expr lastRaw `ne` ptrToWord64Expr rawSent) $
    writeRef outRef (hookRawToNode off lastRaw)
  readRef outRef

popFrontNode ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  m (e (Ptr (CStruct s)))
popFrontNode lst = do
  p <- frontNode lst
  whenM_ (ptrToWord64Expr p `ne` 0) $
    unlinkNode lst p
  pure p

popBackNode ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  m (e (Ptr (CStruct s)))
popBackNode lst = do
  p <- backNode lst
  whenM_ (ptrToWord64Expr p `ne` 0) $
    unlinkNode lst p
  pure p

-- | O(1) zero-allocation splice: transfers all nodes of @src@ onto the back of @dst@, leaving @src@ empty!
spliceBack ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  IntrusiveList s e ->
  m ()
spliceBack dst@(IntrusiveList hdrDst) src@(IntrusiveList hdrSrc) = do
  srcEmpty <- isEmpty src
  whenM_ (lnot srcEmpty `land` (ptrToWord64Expr hdrDst `ne` ptrToWord64Expr hdrSrc)) $ do
    let dSent = sentinelHookPtr dst
        sSent = sentinelHookPtr src
        rawDSent = toRawHookPtr dSent
        rawSSent = toRawHookPtr sSent
        df = projectPtrs dSent
        sf = projectPtrs sSent
    srcFirst <- peek (getPtrOf (rlhNext sf))
    srcLast <- peek (getPtrOf (rlhPrev sf))
    dstLast <- peek (getPtrOf (rlhPrev df))

    -- Attach [srcFirst .. srcLast] between dstLast and rawDSent
    poke (getPtrOf (rlhNext (projectPtrs (asHookPtr dstLast)))) srcFirst
    poke (getPtrOf (rlhPrev (projectPtrs (asHookPtr srcFirst)))) dstLast
    poke (getPtrOf (rlhNext (projectPtrs (asHookPtr srcLast)))) rawDSent
    poke (getPtrOf (rlhPrev df)) srcLast

    -- Reset src to empty sentinel loop
    poke (getPtrOf (rlhPrev sf)) rawSSent
    poke (getPtrOf (rlhNext sf)) rawSSent

    dLen <- peek (getPtrOf (ilsLen (projectPtrs hdrDst)))
    sLen <- peek (getPtrOf (ilsLen (projectPtrs hdrSrc)))
    poke (getPtrOf (ilsLen (projectPtrs hdrDst))) (dLen + sLen)
    poke (getPtrOf (ilsLen (projectPtrs hdrSrc))) 0

-- | O(1) zero-allocation splice: transfers all nodes of @src@ onto the front of @dst@, leaving @src@ empty!
spliceFront ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  IntrusiveList s e ->
  m ()
spliceFront dst@(IntrusiveList hdrDst) src@(IntrusiveList hdrSrc) = do
  srcEmpty <- isEmpty src
  whenM_ (lnot srcEmpty `land` (ptrToWord64Expr hdrDst `ne` ptrToWord64Expr hdrSrc)) $ do
    let dSent = sentinelHookPtr dst
        sSent = sentinelHookPtr src
        rawDSent = toRawHookPtr dSent
        rawSSent = toRawHookPtr sSent
        df = projectPtrs dSent
        sf = projectPtrs sSent
    srcFirst <- peek (getPtrOf (rlhNext sf))
    srcLast <- peek (getPtrOf (rlhPrev sf))
    dstFirst <- peek (getPtrOf (rlhNext df))

    poke (getPtrOf (rlhNext df)) srcFirst
    poke (getPtrOf (rlhPrev (projectPtrs (asHookPtr srcFirst)))) rawDSent
    poke (getPtrOf (rlhNext (projectPtrs (asHookPtr srcLast)))) dstFirst
    poke (getPtrOf (rlhPrev (projectPtrs (asHookPtr dstFirst)))) srcLast

    poke (getPtrOf (rlhPrev sf)) rawSSent
    poke (getPtrOf (rlhNext sf)) rawSSent

    dLen <- peek (getPtrOf (ilsLen (projectPtrs hdrDst)))
    sLen <- peek (getPtrOf (ilsLen (projectPtrs hdrSrc)))
    poke (getPtrOf (ilsLen (projectPtrs hdrDst))) (dLen + sLen)
    poke (getPtrOf (ilsLen (projectPtrs hdrSrc))) 0

-- | Unlink every node in the list without freeing node memory.
clearNodes ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  m ()
clearNodes lst =
  runDrain (drainForEach (unlinkNode lst)) (nodePtrStream lst)

--------------------------------------------------------------------------------
-- Prefetch-Safe Unlink-Safe Streams & Drains
--------------------------------------------------------------------------------

-- | Forward 'Stream' of @e (Ptr (CStruct s))@ node pointers.
-- Prefetches @curHook.next@ BEFORE yielding @curNode@, so unlinking or freeing the
-- yielded node inside the stream consumer is 100% safe!
nodePtrStream ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  Stream m (e (Ptr (CStruct s)))
nodePtrStream lst@(IntrusiveList hdr) = Stream $ Codensity $ \kOuter -> do
  let pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  firstRaw <- peek (getPtrOf (rlhNext (projectPtrs pSent)))
  curRef <- newRef firstRaw
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef curRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` ptrToWord64Expr rawSent)
      (kStep Stop)
      ( do
          nxtRaw <- peek (getPtrOf (rlhNext (projectPtrs (asHookPtr curRaw))))
          writeRef curRef nxtRaw
          kStep (Yield (hookRawToNode off curRaw))
      )

reverseNodePtrStream ::
  forall s m e.
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  Stream m (e (Ptr (CStruct s)))
reverseNodePtrStream lst@(IntrusiveList hdr) = Stream $ Codensity $ \kOuter -> do
  let pSent = sentinelHookPtr lst
      rawSent = toRawHookPtr pSent
  off <- peek (getPtrOf (ilsHookOffset (projectPtrs hdr)))
  lastRaw <- peek (getPtrOf (rlhPrev (projectPtrs pSent)))
  curRef <- newRef lastRaw
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef curRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` ptrToWord64Expr rawSent)
      (kStep Stop)
      ( do
          prvRaw <- peek (getPtrOf (rlhPrev (projectPtrs (asHookPtr curRaw))))
          writeRef curRef prvRaw
          kStep (Yield (hookRawToNode off curRaw))
      )

elemStream ::
  ( IntrusiveListCtx m e s,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  IntrusiveList s e ->
  Stream m (s e)
elemStream lst = streamM (nodePtrStream lst) peekStruct

reverseElemStream ::
  ( IntrusiveListCtx m e s,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  IntrusiveList s e ->
  Stream m (s e)
reverseElemStream lst = streamM (reverseNodePtrStream lst) peekStruct

-- | Non-allocating drain appending pre-allocated @e (Ptr (CStruct s))@ nodes onto the back of @lst@.
drainAppendNodesList ::
  (IntrusiveListCtx m e s) =>
  IntrusiveList s e ->
  Drain m (e (Ptr (CStruct s))) ()
drainAppendNodesList lst = drainForEach (pushBackNode lst)

-- | Allocate a new 'IntrusiveList' and allocate+link a node for every HKD value yielded by a 'Stream'.
drainToList ::
  forall s m e.
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  HookSpec s ->
  Drain m (s e) (IntrusiveList s e)
drainToList alloc spec = Drain $ \st -> do
  lst <- new alloc spec
  runDrain (drainAllocAppendList alloc lst) st
  pure lst

fromStream ::
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  HookSpec s ->
  Stream m (s e) ->
  m (IntrusiveList s e)
fromStream alloc spec = runDrain (drainToList alloc spec)

-- | Allocate and append a new node onto the back of @lst@ for each value yielded by a 'Stream'.
drainAllocAppendList ::
  ( IntrusiveListCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  IntrusiveList s e ->
  Drain m (s e) ()
drainAllocAppendList alloc lst =
  drainForEach (\v -> allocAndPushBack alloc lst v >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (IntrusiveList s (JanusCT _)) = IntrusiveList s Identity

deriving via
  ViaPtrHandle t (CStruct IntrusiveListSpine) (IntrusiveList s Identity)
  instance
    (Typeable s) => JanusCArg t (IntrusiveList s (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct IntrusiveListSpine) (IntrusiveList s Identity)
  instance
    (Typeable s) => JanusCValRet t (IntrusiveList s (JanusCT t))
