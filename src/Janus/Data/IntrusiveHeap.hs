{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Janus.Data.IntrusiveHeap
  ( -- * Embedded 3-Pointer Pairing-Heap Hook & Multi-Hook `container_of` Spec
    RawHeapHook (..),
    emptyHeapHook,
    HeapSpec (..),
    mkHeapSpec,
    IntrusiveHeapSpine (..),
    IntrusiveHeap (..),
    IntrusiveHeapCtx,

    -- * Allocating Spine & Node Helpers (Explicit `Alloc e`)
    new,
    freeHeapSpine,
    freeAllNodesAndSpine,
    allocNode,
    freeNode,
    allocAndPush,

    -- * Zero-Allocation Intrusive Pairing Min-Heap Operations (No `Alloc e` Required!)
    len,
    isEmpty,
    initNodeHook,
    isNodeLinked,
    peekMinNode,
    pushNode,
    mergeHeaps,
    popMinNode,
    decreaseKeyNode,
    removeNode,
    clearNodes,

    -- * Unlink-Safe Priority Streams & Drains
    popAllNodesS,
    drainAppendNodesHeap,
    drainToIntrusiveHeap,
    fromStream,
    drainAllocAppendHeap,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import Janus.Alloc.Dynamic (CmdAllocBase)
import Janus.Alloc.Policy (Alloc, CmdAlloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFoldM, drainForEach, drain_, runDrain)
import Janus.Control.Stream (Stream, unfoldWhileM)
import Janus.Data.FlatMap (OrdKey (..))
import Janus.Data.HKD
import Janus.Data.Layout (Elem, StorageLayout)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Typed (JanusTyped)

-- | 24-byte 3-pointer intrusive pairing-heap hook embedded inside user HKD node structs:
-- @myHeapHook :: !(f (CStruct RawHeapHook))@.
--
-- Invariant across linked vs unlinked nodes:
-- * Every unlinked node has @rhhChild == NULL@, @rhhNextSib == NULL@, @rhhPrevOrParent == NULL@.
-- * The unique root node of a non-empty heap has @rhhPrevOrParent == spineMarkerRaw hdr@ and @rhhNextSib == NULL@.
-- * Every non-root child node has @rhhPrevOrParent@ pointing to its parent (if first child)
--   or its immediate left sibling, giving O(1) arbitrary-node detach without heap allocations!
data RawHeapHook f = RawHeapHook
  { rhhChild :: !(f (Ptr Word8)),
    rhhNextSib :: !(f (Ptr Word8)),
    rhhPrevOrParent :: !(f (Ptr Word8))
  }
  deriving stock (Generic)

deriving stock instance Eq (RawHeapHook Identity)

deriving stock instance Show (RawHeapHook Identity)

instance JanusStruct RawHeapHook

-- | Construct an unlinked 'RawHeapHook' with all three pointers set to @nullPtr@.
emptyHeapHook :: (ExpPtr e Word8) => RawHeapHook e
emptyHeapHook =
  RawHeapHook
    { rhhChild = nullPtr,
      rhhNextSib = nullPtr,
      rhhPrevOrParent = nullPtr
    }

-- | Compile-time/host C-ABI byte offsets of a chosen 'RawHeapHook' field and scalar priority
-- key field @f k@ inside @CStruct s@.
data HeapSpec (s :: (K.Type -> K.Type) -> K.Type) k = HeapSpec
  { heapHookByteOffset :: !Int,
    heapKeyByteOffset :: !Int
  }
  deriving stock (Eq, Show)

-- | Build a 'HeapSpec' from projected HKD field selectors
-- (e.g. @mkHeapSpec taskHeapHook taskPriority@).
mkHeapSpec ::
  forall s k.
  ( JanusStruct s,
    Generic (s (PtrOf Identity)),
    GJanusStructProject Identity (Rep (s Identity)) (Rep (s (PtrOf Identity)))
  ) =>
  (s (PtrOf Identity) -> PtrOf Identity (CStruct RawHeapHook)) ->
  (s (PtrOf Identity) -> PtrOf Identity k) ->
  HeapSpec s k
mkHeapSpec hookSel keySel =
  HeapSpec
    { heapHookByteOffset = fieldByteOffset @s hookSel,
      heapKeyByteOffset = fieldByteOffset @s keySel
    }

-- | Spine struct for 'IntrusiveHeap'. Stores both @ihsHookOffset@ and @ihsKeyOffset@ in the
-- spine so all zero-allocation priority operations work uniformly across Host & C FFI!
data IntrusiveHeapSpine f = IntrusiveHeapSpine
  { ihsRoot :: !(f (Ptr Word8)),
    ihsHookOffset :: !(f Int64),
    ihsKeyOffset :: !(f Int64),
    ihsLen :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (IntrusiveHeapSpine Identity)

deriving stock instance Show (IntrusiveHeapSpine Identity)

instance JanusStruct IntrusiveHeapSpine

newtype IntrusiveHeap (s :: (K.Type -> K.Type) -> K.Type) k (e :: K.Type -> K.Type) = IntrusiveHeap
  { unIntrusiveHeap :: e (Ptr (CStruct IntrusiveHeapSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (IntrusiveHeap s k Identity)

deriving stock instance Show (IntrusiveHeap s k Identity)

instance JanusHKD (IntrusiveHeap s k)

type IntrusiveHeapCtx m e s k =
  ( CmdAllocBase m e,
    Typeable s,
    Typeable k,
    JanusStruct s,
    StorageLayout k,
    Elem e k ~ e k,
    OrdKey e k,
    CmdStorable m e k,
    ExpPtr e k,
    JanusTyped e k,
    ExpPtr e (CStruct s),
    ExpSized e (CStruct s),
    JanusTyped e (CStruct s),
    JanusTyped e (Ptr (CStruct s)),
    CmdStorable m e (Ptr (CStruct s)),
    JanusTyped e (CStruct RawHeapHook),
    JanusTyped e (CStruct IntrusiveHeapSpine)
  )

asHeapHookPtr ::
  (ExpPtrCast e, JanusTyped e Word8, JanusTyped e (CStruct RawHeapHook)) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct RawHeapHook))
asHeapHookPtr p = fromVoidPtr (toVoidPtr p)

spineMarkerRaw ::
  (ExpPtrCast e, JanusTyped e (CStruct IntrusiveHeapSpine), JanusTyped e Word8) =>
  e (Ptr (CStruct IntrusiveHeapSpine)) ->
  e (Ptr Word8)
spineMarkerRaw hdr = fromVoidPtr (toVoidPtr hdr)

nodeToHookRaw ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  e Int64 ->
  e (Ptr (CStruct s)) ->
  e (Ptr Word8)
nodeToHookRaw hookOff pNode =
  ptrAdd (fromVoidPtr (toVoidPtr pNode) :: e (Ptr Word8)) hookOff

hookRawToNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  e Int64 ->
  e (Ptr Word8) ->
  e (Ptr (CStruct s))
hookRawToNode hookOff pHookRaw =
  fromVoidPtr (toVoidPtr (ptrAdd pHookRaw (negate hookOff)))

peekHookKey ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Word8) ->
  m (e k)
peekHookKey hookOff keyOff pHookRaw = do
  let pNodeBytes = ptrAdd pHookRaw (negate hookOff)
      pKey = fromVoidPtr (toVoidPtr (ptrAdd pNodeBytes keyOff)) :: e (Ptr k)
  peek pKey

--------------------------------------------------------------------------------
-- Allocating Lifecycle & Node Helpers (Explicit `Alloc e`)
--------------------------------------------------------------------------------

-- | Allocate and initialize an empty 'IntrusiveHeap' spine for the given 'HeapSpec'.
new ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k, CmdAlloc m e, ExpSized e (CStruct IntrusiveHeapSpine)) =>
  Alloc e ->
  HeapSpec s k ->
  m (IntrusiveHeap s k e)
new alloc (HeapSpec hookOff keyOff) = do
  hdr <- allocStruct @(CStruct IntrusiveHeapSpine) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (ihsRoot f)) nullPtr
  poke (getPtrOf (ihsHookOffset f)) (fromIntegral hookOff)
  poke (getPtrOf (ihsKeyOffset f)) (fromIntegral keyOff)
  poke (getPtrOf (ihsLen f)) 0
  pure (IntrusiveHeap hdr)

-- | Free ONLY the heap spine (leaving any nodes untouched).
freeHeapSpine ::
  (IntrusiveHeapCtx m e s k, CmdAlloc m e, ExpSized e (CStruct IntrusiveHeapSpine)) =>
  Alloc e ->
  IntrusiveHeap s k e ->
  m ()
freeHeapSpine alloc (IntrusiveHeap hdr) = freeStruct alloc hdr

-- | Pop and free every currently linked node via @alloc@, then free the heap spine.
freeAllNodesAndSpine ::
  (IntrusiveHeapCtx m e s k, CmdAlloc m e, ExpSized e (CStruct IntrusiveHeapSpine)) =>
  Alloc e ->
  IntrusiveHeap s k e ->
  m ()
freeAllNodesAndSpine alloc hp = do
  runDrain (drainForEach (freeStruct alloc)) (popAllNodesS hp)
  freeHeapSpine alloc hp

-- | Allocate and initialize a node struct via @alloc@ from an HKD value @s e@.
allocNode ::
  forall s m e.
  ( CmdAlloc m e,
    JanusStruct s,
    ExpSized e (CStruct s),
    JanusTyped e (CStruct s),
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
  (CmdAlloc m e, ExpSized e (CStruct s), JanusTyped e (CStruct s)) =>
  Alloc e ->
  e (Ptr (CStruct s)) ->
  m ()
freeNode = freeStruct

-- | Allocate a node, initialize its heap hook, and push it into @hp@.
allocAndPush ::
  forall s k m e.
  ( IntrusiveHeapCtx m e s k,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  IntrusiveHeap s k e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocAndPush alloc hp val = do
  pNode <- allocNode alloc val
  initNodeHook hp pNode
  pushNode hp pNode
  pure pNode

--------------------------------------------------------------------------------
-- Internal Pairing-Heap Tree Primitives (Zero Allocations!)
--------------------------------------------------------------------------------

attachChild ::
  (CmdAllocBase m e, JanusTyped e (CStruct RawHeapHook)) =>
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  m ()
attachChild winnerRaw loserRaw = do
  let wf = projectPtrs (asHeapHookPtr winnerRaw)
      lf = projectPtrs (asHeapHookPtr loserRaw)
  oldChild <- peek (getPtrOf (rhhChild wf))
  poke (getPtrOf (rhhPrevOrParent lf)) winnerRaw
  poke (getPtrOf (rhhNextSib lf)) oldChild
  whenM_ (ptrToWord64Expr oldChild `ne` 0) $
    poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr oldChild)))) loserRaw
  poke (getPtrOf (rhhChild wf)) loserRaw

-- | O(1) pairing meld of two non-null detached trees @aRaw@ and @bRaw@.
meldHooks ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  m (e (Ptr Word8))
meldHooks hookOff keyOff aRaw bRaw = do
  keyA <- peekHookKey @s @k hookOff keyOff aRaw
  keyB <- peekHookKey @s @k hookOff keyOff bRaw
  outRef <- newRef aRaw
  ifThenElseM_
    (ltKey @e @k keyB keyA)
    ( do
        attachChild bRaw aRaw
        let bf = projectPtrs (asHeapHookPtr bRaw)
        poke (getPtrOf (rhhPrevOrParent bf)) nullPtr
        poke (getPtrOf (rhhNextSib bf)) nullPtr
        writeRef outRef bRaw
    )
    ( do
        attachChild aRaw bRaw
        let af = projectPtrs (asHeapHookPtr aRaw)
        poke (getPtrOf (rhhPrevOrParent af)) nullPtr
        poke (getPtrOf (rhhNextSib af)) nullPtr
        writeRef outRef aRaw
    )
  readRef outRef

-- | O(1) detach of a non-root linked node from its parent or left sibling
-- (preserving @nodeRaw.rhhChild@ intact).
cutNode ::
  (CmdAllocBase m e, JanusTyped e (CStruct RawHeapHook)) =>
  e (Ptr Word8) ->
  m ()
cutNode nodeRaw = do
  let hf = projectPtrs (asHeapHookPtr nodeRaw)
  popRaw <- peek (getPtrOf (rhhPrevOrParent hf))
  nxtRaw <- peek (getPtrOf (rhhNextSib hf))
  let pf = projectPtrs (asHeapHookPtr popRaw)
  popChild <- peek (getPtrOf (rhhChild pf))
  ifThenElseM_
    (ptrToWord64Expr popChild `eq` ptrToWord64Expr nodeRaw)
    ( do
        poke (getPtrOf (rhhChild pf)) nxtRaw
        whenM_ (ptrToWord64Expr nxtRaw `ne` 0) $
          poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr nxtRaw)))) popRaw
    )
    ( do
        poke (getPtrOf (rhhNextSib pf)) nxtRaw
        whenM_ (ptrToWord64Expr nxtRaw `ne` 0) $
          poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr nxtRaw)))) popRaw
    )
  poke (getPtrOf (rhhPrevOrParent hf)) nullPtr
  poke (getPtrOf (rhhNextSib hf)) nullPtr

-- | Iterative two-pass pairing combine over a detached @firstChildRaw@ sibling chain:
-- * Pass 1 pairs adjacent siblings left-to-right, threading paired roots onto a temporary
--   reversed stack via @rhhNextSib@ (zero heap allocations!).
-- * Pass 2 melds that stack right-to-left into a single new subtree root.
twoPassCombineChildren ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Word8) ->
  m (e (Ptr Word8))
twoPassCombineChildren hookOff keyOff firstChildRaw = do
  outRef <- newRef (nullPtr @e @Word8)
  whenM_ (ptrToWord64Expr firstChildRaw `ne` 0) $ do
    let pairStream :: Stream m (e (Ptr Word8))
        pairStream =
          unfoldWhileM firstChildRaw (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \aRaw -> do
            let af = projectPtrs (asHeapHookPtr aRaw)
            bRaw <- peek (getPtrOf (rhhNextSib af))
            poke (getPtrOf (rhhPrevOrParent af)) nullPtr
            poke (getPtrOf (rhhNextSib af)) nullPtr
            nxtCurRef <- newRef (nullPtr @e @Word8)
            pairedRef <- newRef aRaw
            whenM_ (ptrToWord64Expr bRaw `ne` 0) $ do
              let bf = projectPtrs (asHeapHookPtr bRaw)
              afterBRaw <- peek (getPtrOf (rhhNextSib bf))
              poke (getPtrOf (rhhPrevOrParent bf)) nullPtr
              poke (getPtrOf (rhhNextSib bf)) nullPtr
              merged <- meldHooks @s @k hookOff keyOff aRaw bRaw
              writeRef nxtCurRef afterBRaw
              writeRef pairedRef merged
            (,) <$> readRef nxtCurRef <*> readRef pairedRef

    stackHead <-
      runDrain
        ( drainFoldM (nullPtr @e @Word8) $ \accHead treeRaw -> do
            poke (getPtrOf (rhhNextSib (projectPtrs (asHeapHookPtr treeRaw)))) accHead
            pure treeRaw
        )
        pairStream

    let sf0 = projectPtrs (asHeapHookPtr stackHead)
    rest0 <- peek (getPtrOf (rhhNextSib sf0))
    poke (getPtrOf (rhhNextSib sf0)) nullPtr
    poke (getPtrOf (rhhPrevOrParent sf0)) nullPtr

    let revStream :: Stream m (e (Ptr Word8))
        revStream =
          unfoldWhileM rest0 (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \curRaw -> do
            let cf = projectPtrs (asHeapHookPtr curRaw)
            nxt <- peek (getPtrOf (rhhNextSib cf))
            poke (getPtrOf (rhhNextSib cf)) nullPtr
            poke (getPtrOf (rhhPrevOrParent cf)) nullPtr
            pure (nxt, curRaw)

    finalRoot <-
      runDrain
        (drainFoldM stackHead (\acc cur -> meldHooks @s @k hookOff keyOff cur acc))
        revStream
    writeRef outRef finalRoot
  readRef outRef

--------------------------------------------------------------------------------
-- Zero-Allocation Intrusive Pairing-Heap Operations (No `Alloc e` Required!)
--------------------------------------------------------------------------------

len :: (IntrusiveHeapCtx m e s k) => IntrusiveHeap s k e -> m (e Int64)
len (IntrusiveHeap hdr) = peek (getPtrOf (ihsLen (projectPtrs hdr)))

isEmpty :: (IntrusiveHeapCtx m e s k) => IntrusiveHeap s k e -> m (e Bool)
isEmpty (IntrusiveHeap hdr) = do
  r <- peek (getPtrOf (ihsRoot (projectPtrs hdr)))
  pure (ptrToWord64Expr r `eq` 0)

initNodeHook ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  e (Ptr (CStruct s)) ->
  m ()
initNodeHook (IntrusiveHeap hdr) pNode = do
  hookOff <- peek (getPtrOf (ihsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asHeapHookPtr (nodeToHookRaw @s @k hookOff pNode))
  poke (getPtrOf (rhhChild hf)) nullPtr
  poke (getPtrOf (rhhNextSib hf)) nullPtr
  poke (getPtrOf (rhhPrevOrParent hf)) nullPtr

isNodeLinked ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  e (Ptr (CStruct s)) ->
  m (e Bool)
isNodeLinked (IntrusiveHeap hdr) pNode = do
  hookOff <- peek (getPtrOf (ihsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asHeapHookPtr (nodeToHookRaw @s @k hookOff pNode))
  popRaw <- peek (getPtrOf (rhhPrevOrParent hf))
  pure (ptrToWord64Expr popRaw `ne` 0)

-- | O(1) peek at the minimum-priority node pointer (returns @nullPtr@ if empty).
peekMinNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  m (e (Ptr (CStruct s)))
peekMinNode (IntrusiveHeap hdr) = do
  let f = projectPtrs hdr
  rootRaw <- peek (getPtrOf (ihsRoot f))
  outRef <- newRef (nullPtr @e @(CStruct s))
  whenM_ (ptrToWord64Expr rootRaw `ne` 0) $ do
    hookOff <- peek (getPtrOf (ihsHookOffset f))
    writeRef outRef (hookRawToNode @s @k hookOff rootRaw)
  readRef outRef

-- | O(1) zero-allocation push of an unlinked node @pNode@ into @hp@.
pushNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  e (Ptr (CStruct s)) ->
  m ()
pushNode (IntrusiveHeap hdr) pNode = do
  let f = projectPtrs hdr
      marker = spineMarkerRaw hdr
  hookOff <- peek (getPtrOf (ihsHookOffset f))
  let nodeRaw = nodeToHookRaw @s @k hookOff pNode
      hf = projectPtrs (asHeapHookPtr nodeRaw)
  prevPop <- peek (getPtrOf (rhhPrevOrParent hf))
  whenM_ (ptrToWord64Expr prevPop `eq` 0) $ do
    poke (getPtrOf (rhhChild hf)) nullPtr
    poke (getPtrOf (rhhNextSib hf)) nullPtr
    curRoot <- peek (getPtrOf (ihsRoot f))
    ifThenElseM_
      (ptrToWord64Expr curRoot `eq` 0)
      ( do
          poke (getPtrOf (rhhPrevOrParent hf)) marker
          poke (getPtrOf (ihsRoot f)) nodeRaw
      )
      ( do
          keyOff <- peek (getPtrOf (ihsKeyOffset f))
          poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr curRoot)))) nullPtr
          newRoot <- meldHooks @s @k hookOff keyOff curRoot nodeRaw
          let nrf = projectPtrs (asHeapHookPtr newRoot)
          poke (getPtrOf (rhhPrevOrParent nrf)) marker
          poke (getPtrOf (rhhNextSib nrf)) nullPtr
          poke (getPtrOf (ihsRoot f)) newRoot
      )
    curLen <- peek (getPtrOf (ihsLen f))
    poke (getPtrOf (ihsLen f)) (curLen + 1)

-- | O(1) zero-allocation meld of all nodes from @src@ into @dst@, leaving @src@ empty!
mergeHeaps ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  IntrusiveHeap s k e ->
  m ()
mergeHeaps (IntrusiveHeap hdrDst) (IntrusiveHeap hdrSrc) = do
  let fSrc = projectPtrs hdrSrc
  srcRoot <- peek (getPtrOf (ihsRoot fSrc))
  whenM_ ((ptrToWord64Expr srcRoot `ne` 0) `land` (ptrToWord64Expr hdrDst `ne` ptrToWord64Expr hdrSrc)) $ do
    let fDst = projectPtrs hdrDst
        dstMarker = spineMarkerRaw hdrDst
        srf = projectPtrs (asHeapHookPtr srcRoot)
    poke (getPtrOf (rhhPrevOrParent srf)) nullPtr
    poke (getPtrOf (rhhNextSib srf)) nullPtr
    dstRoot <- peek (getPtrOf (ihsRoot fDst))
    ifThenElseM_
      (ptrToWord64Expr dstRoot `eq` 0)
      ( do
          poke (getPtrOf (rhhPrevOrParent srf)) dstMarker
          poke (getPtrOf (ihsRoot fDst)) srcRoot
      )
      ( do
          hookOff <- peek (getPtrOf (ihsHookOffset fDst))
          keyOff <- peek (getPtrOf (ihsKeyOffset fDst))
          poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr dstRoot)))) nullPtr
          newRoot <- meldHooks @s @k hookOff keyOff dstRoot srcRoot
          let nrf = projectPtrs (asHeapHookPtr newRoot)
          poke (getPtrOf (rhhPrevOrParent nrf)) dstMarker
          poke (getPtrOf (rhhNextSib nrf)) nullPtr
          poke (getPtrOf (ihsRoot fDst)) newRoot
      )
    dLen <- peek (getPtrOf (ihsLen fDst))
    sLen <- peek (getPtrOf (ihsLen fSrc))
    poke (getPtrOf (ihsLen fDst)) (dLen + sLen)
    poke (getPtrOf (ihsRoot fSrc)) nullPtr
    poke (getPtrOf (ihsLen fSrc)) 0

-- | O(log N) amortized extract-min via iterative two-pass pairing (returns @nullPtr@ if empty).
popMinNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  m (e (Ptr (CStruct s)))
popMinNode (IntrusiveHeap hdr) = do
  let f = projectPtrs hdr
      marker = spineMarkerRaw hdr
  rootRaw <- peek (getPtrOf (ihsRoot f))
  outRef <- newRef (nullPtr @e @(CStruct s))
  whenM_ (ptrToWord64Expr rootRaw `ne` 0) $ do
    hookOff <- peek (getPtrOf (ihsHookOffset f))
    keyOff <- peek (getPtrOf (ihsKeyOffset f))
    curLen <- peek (getPtrOf (ihsLen f))
    let rf = projectPtrs (asHeapHookPtr rootRaw)
    firstChild <- peek (getPtrOf (rhhChild rf))
    poke (getPtrOf (rhhChild rf)) nullPtr
    poke (getPtrOf (rhhNextSib rf)) nullPtr
    poke (getPtrOf (rhhPrevOrParent rf)) nullPtr
    newRoot <- twoPassCombineChildren @s @k hookOff keyOff firstChild
    whenM_ (ptrToWord64Expr newRoot `ne` 0) $
      poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr newRoot)))) marker
    poke (getPtrOf (ihsRoot f)) newRoot
    poke (getPtrOf (ihsLen f)) (curLen - 1)
    writeRef outRef (hookRawToNode @s @k hookOff rootRaw)
  readRef outRef

-- | O(1) decrease-key: after the caller lowers @pNode@'s priority field in-place,
-- cuts @pNode@ from its parent/sibling (if non-root) and melds it with the root.
decreaseKeyNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  e (Ptr (CStruct s)) ->
  m ()
decreaseKeyNode (IntrusiveHeap hdr) pNode = do
  let f = projectPtrs hdr
      marker = spineMarkerRaw hdr
  hookOff <- peek (getPtrOf (ihsHookOffset f))
  let nodeRaw = nodeToHookRaw @s @k hookOff pNode
      hf = projectPtrs (asHeapHookPtr nodeRaw)
  popRaw <- peek (getPtrOf (rhhPrevOrParent hf))
  whenM_ ((ptrToWord64Expr popRaw `ne` 0) `land` (ptrToWord64Expr popRaw `ne` ptrToWord64Expr marker)) $ do
    keyOff <- peek (getPtrOf (ihsKeyOffset f))
    cutNode nodeRaw
    curRoot <- peek (getPtrOf (ihsRoot f))
    poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr curRoot)))) nullPtr
    newRoot <- meldHooks @s @k hookOff keyOff curRoot nodeRaw
    let nrf = projectPtrs (asHeapHookPtr newRoot)
    poke (getPtrOf (rhhPrevOrParent nrf)) marker
    poke (getPtrOf (rhhNextSib nrf)) nullPtr
    poke (getPtrOf (ihsRoot f)) newRoot

-- | O(log N) amortized arbitrary-node removal: detaches @pNode@ from anywhere in the heap,
-- combines its children via two-pass pairing, melds them back into the root, and zeroes @pNode@'s hook.
removeNode ::
  forall s k m e.
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  e (Ptr (CStruct s)) ->
  m ()
removeNode hp@(IntrusiveHeap hdr) pNode = do
  let f = projectPtrs hdr
      marker = spineMarkerRaw hdr
  hookOff <- peek (getPtrOf (ihsHookOffset f))
  let nodeRaw = nodeToHookRaw @s @k hookOff pNode
      hf = projectPtrs (asHeapHookPtr nodeRaw)
  popRaw <- peek (getPtrOf (rhhPrevOrParent hf))
  whenM_ (ptrToWord64Expr popRaw `ne` 0) $
    ifThenElseM_
      (ptrToWord64Expr popRaw `eq` ptrToWord64Expr marker)
      (popMinNode hp >> pure ())
      ( do
          keyOff <- peek (getPtrOf (ihsKeyOffset f))
          cutNode nodeRaw
          firstChild <- peek (getPtrOf (rhhChild hf))
          poke (getPtrOf (rhhChild hf)) nullPtr
          whenM_ (ptrToWord64Expr firstChild `ne` 0) $ do
            subRoot <- twoPassCombineChildren @s @k hookOff keyOff firstChild
            curRoot <- peek (getPtrOf (ihsRoot f))
            poke (getPtrOf (rhhPrevOrParent (projectPtrs (asHeapHookPtr curRoot)))) nullPtr
            newRoot <- meldHooks @s @k hookOff keyOff curRoot subRoot
            let nrf = projectPtrs (asHeapHookPtr newRoot)
            poke (getPtrOf (rhhPrevOrParent nrf)) marker
            poke (getPtrOf (rhhNextSib nrf)) nullPtr
            poke (getPtrOf (ihsRoot f)) newRoot
          curLen <- peek (getPtrOf (ihsLen f))
          poke (getPtrOf (ihsLen f)) (curLen - 1)
      )

-- | Unlink and zero every node's hook in the heap without freeing node memory.
clearNodes ::
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  m ()
clearNodes hp = runDrain drain_ (popAllNodesS hp)

--------------------------------------------------------------------------------
-- Unlink-Safe Priority Streams & Drains
--------------------------------------------------------------------------------

-- | Destructive ascending-priority 'Stream' popping and yielding @e (Ptr (CStruct s))@
-- until @hp@ is empty. Each yielded node is already unlinked before being passed to
-- downstream consumers, so freeing or re-linking inside the consumer is 100% safe!
popAllNodesS ::
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  Stream m (e (Ptr (CStruct s)))
popAllNodesS hp =
  unfoldWhileM () (\_ -> lnot <$> isEmpty hp) (\_ -> (,) () <$> popMinNode hp)

-- | Non-allocating drain pushing pre-allocated @e (Ptr (CStruct s))@ nodes into @hp@.
drainAppendNodesHeap ::
  (IntrusiveHeapCtx m e s k) =>
  IntrusiveHeap s k e ->
  Drain m (e (Ptr (CStruct s))) ()
drainAppendNodesHeap hp = drainForEach (pushNode hp)

-- | Allocate a new 'IntrusiveHeap' and allocate+push a node for every HKD value yielded by a 'Stream'.
drainToIntrusiveHeap ::
  forall s k m e.
  ( IntrusiveHeapCtx m e s k,
    CmdAlloc m e,
    ExpSized e (CStruct IntrusiveHeapSpine),
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  HeapSpec s k ->
  Drain m (s e) (IntrusiveHeap s k e)
drainToIntrusiveHeap alloc spec = Drain $ \st -> do
  hp <- new alloc spec
  runDrain (drainAllocAppendHeap alloc hp) st
  pure hp

fromStream ::
  ( IntrusiveHeapCtx m e s k,
    CmdAlloc m e,
    ExpSized e (CStruct IntrusiveHeapSpine),
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  HeapSpec s k ->
  Stream m (s e) ->
  m (IntrusiveHeap s k e)
fromStream alloc spec = runDrain (drainToIntrusiveHeap alloc spec)

-- | Allocate and push a new node into @hp@ for each HKD value yielded by a 'Stream'.
drainAllocAppendHeap ::
  ( IntrusiveHeapCtx m e s k,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  IntrusiveHeap s k e ->
  Drain m (s e) ()
drainAllocAppendHeap alloc hp =
  drainForEach (\v -> allocAndPush alloc hp v >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCEval (IntrusiveHeap s k JanusC -> r) = IntrusiveHeap s k Identity -> JanusCEval r

type instance JanusCRetType (IntrusiveHeap s k JanusC -> r) = JanusCRetType r

instance (Typeable s, Typeable k, JanusCParam r) => JanusCParam (IntrusiveHeap s k JanusC -> r) where
  jcparam = mkPtrHandleJCParam IntrusiveHeap

instance (Typeable s, Typeable k, JanusCEvaluate r) => JanusCEvaluate (IntrusiveHeap s k JanusC -> r) where
  jceval = mkPtrHandleJCEval unIntrusiveHeap IntrusiveHeap

type instance JanusCEval (JanusCM (IntrusiveHeap s k JanusC)) = IO (IntrusiveHeap s k Identity)

type instance JanusCRetType (JanusCM (IntrusiveHeap s k JanusC)) = IntrusiveHeap s k Identity

instance (Typeable s, Typeable k) => JanusCParam (JanusCM (IntrusiveHeap s k JanusC)) where
  jcparam = mkPtrHandleJCRetParam unIntrusiveHeap

instance (Typeable s, Typeable k) => JanusCEvaluate (JanusCM (IntrusiveHeap s k JanusC)) where
  jceval = mkPtrHandleJCRetEval IntrusiveHeap

type instance LinuxX86_64CEval (IntrusiveHeap s k LinuxX86_64C -> r) = IntrusiveHeap s k Identity -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (IntrusiveHeap s k LinuxX86_64C -> r) = LinuxX86_64BaseRet r

instance (Typeable s, Typeable k, LinuxX86_64Param r) => LinuxX86_64Param (IntrusiveHeap s k LinuxX86_64C -> r) where
  x86param = mkPtrHandleX86Param IntrusiveHeap

instance (Typeable s, Typeable k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (IntrusiveHeap s k LinuxX86_64C -> r) where
  x86eval = mkPtrHandleX86EvalParam unIntrusiveHeap IntrusiveHeap

instance (Typeable s, Typeable k, X86DeferCont r) => X86DeferCall (IntrusiveHeap s k LinuxX86_64C -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unIntrusiveHeap IntrusiveHeap

instance (Typeable s, Typeable k, X86DeferCont r) => X86DeferCont (IntrusiveHeap s k LinuxX86_64C -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unIntrusiveHeap IntrusiveHeap

type instance LinuxX86_64CEval (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) = IO (IntrusiveHeap s k Identity)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) = IntrusiveHeap s k Identity

instance (Typeable s, Typeable k) => LinuxX86_64Param (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) where
  x86param = mkPtrHandleX86RetParam unIntrusiveHeap

instance (Typeable s, Typeable k) => LinuxX86_64Evaluate (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) where
  x86eval = mkPtrHandleX86RetEval IntrusiveHeap

instance (Typeable s, Typeable k) => X86DeferCall (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg IntrusiveHeap

instance (Typeable s, Typeable k) => X86DeferCont (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont IntrusiveHeap

instance (Typeable s, Typeable k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (IntrusiveHeap s k LinuxX86_64C -> r) where
  type JanusCTHEval (IntrusiveHeap s k LinuxX86_64C -> r) = IntrusiveHeap s k Identity -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(IntrusiveHeap s k Identity)) : ps, retSpec)

instance (Typeable s, Typeable k) => JanusCTH (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) where
  type JanusCTHEval (LinuxX86_64CM (IntrusiveHeap s k LinuxX86_64C)) = IO (IntrusiveHeap s k Identity)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(IntrusiveHeap s k Identity)) (Proxy @(CStruct IntrusiveHeapSpine)))
