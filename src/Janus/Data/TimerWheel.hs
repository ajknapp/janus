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

module Janus.Data.TimerWheel
  ( -- * Embedded Intrusive Hook & `container_of` Timer Spec
    RawListHook (..),
    emptyTimerHook,
    TimerWheelLevels,
    TimerWheelSlots,
    TimerWheelBuckets,
    TimerSpec (..),
    mkTimerSpec,
    TimerWheelSpine (..),
    TimerWheel (..),
    TimerWheelCtx,

    -- * Allocating Spine & Timer Node Helpers (Explicit `Alloc e`)
    new,
    freeWheelSpine,
    freeAllTimersAndSpine,
    allocTimerNode,
    freeTimerNode,
    allocAndSchedule,

    -- * Zero-Allocation O(1) Intrusive Timer Operations (No `Alloc e` Required!)
    initTimerHook,
    isTimerScheduled,
    pendingCount,
    currentTick,
    scheduleNode,
    cancelNode,
    clearTimers,

    -- * Hierarchical Bitmask Tick-Skipping Expiration Streams & Drains
    advanceExpiredStream,
    drainAdvanceExpired,
    drainAppendNodesWheel,
    drainAllocAppendWheel,
  )
where

import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import Janus.Alloc.Dynamic (CmdAllocBase)
import Janus.Alloc.Policy (Alloc, CmdAlloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (ExpJanus, KnownJanusType, withJanusType)
import Janus.Control.Drain (Drain, drainFoldM, drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, rangeS, statefulS, unfoldWhileM)
import Janus.Data.HKD
import Janus.Data.IntrusiveList (RawListHook (..))
import Janus.Data.Layout (StructCtx)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))

type TimerWheelLevels = 4

type TimerWheelSlots = 64

type TimerWheelBuckets = 256

-- | Construct an unscheduled 'RawListHook' with both pointers set to @nullPtr@.
emptyTimerHook :: (ExpPtr e Word8) => RawListHook e
emptyTimerHook = RawListHook {rlhPrev = nullPtr, rlhNext = nullPtr}

-- | Compile-time/host C-ABI byte offsets of a chosen 'RawListHook' field and
-- @Word64@ deadline field inside @CStruct s@.
data TimerSpec (s :: (K.Type -> K.Type) -> K.Type) = TimerSpec
  { timerHookByteOffset :: !Int,
    timerDeadlineByteOffset :: !Int
  }
  deriving stock (Eq, Show)

-- | Build a 'TimerSpec' from projected HKD field selectors
-- (e.g. @mkTimerSpec timerHook timerDeadline@).
mkTimerSpec ::
  forall s.
  ( JanusStruct s,
    Generic (s (PtrOf Identity)),
    GJanusStructProject Identity (Rep (s Identity)) (Rep (s (PtrOf Identity)))
  ) =>
  (s (PtrOf Identity) -> PtrOf Identity (CStruct RawListHook)) ->
  (s (PtrOf Identity) -> PtrOf Identity Word64) ->
  TimerSpec s
mkTimerSpec hookSel deadlineSel =
  TimerSpec
    { timerHookByteOffset = fieldByteOffset @s hookSel,
      timerDeadlineByteOffset = fieldByteOffset @s deadlineSel
    }

-- | 4-level x 64-slot hierarchical bitmask timer wheel spine:
-- * @twsBuckets@: 256 circular doubly-linked 'RawListHook' sentinels (@lvl * 64 + slot@).
-- * @twsOverflow@: circular sentinel holding timers with @(deadline ^ curTick) >= 2^24@.
-- * @twsOcc@: four 64-bit occupancy bitmasks (bit @slot@ in @twsOcc[lvl]@ is @1@ iff bucket
--   @(lvl, slot)@ is non-empty), enabling O(1) @ctz@ skipping over millions of idle ticks!
data TimerWheelSpine f = TimerWheelSpine
  { twsBuckets :: !(f (FixedArray TimerWheelBuckets (CStruct RawListHook))),
    twsOverflow :: !(f (CStruct RawListHook)),
    twsOcc :: !(f (FixedArray TimerWheelLevels Word64)),
    twsCurrentTick :: !(f Word64),
    twsHookOffset :: !(f Int64),
    twsDeadlineOffset :: !(f Int64),
    twsPendingCount :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (TimerWheelSpine Identity)

deriving stock instance Show (TimerWheelSpine Identity)

instance JanusStruct TimerWheelSpine

newtype TimerWheel (s :: (K.Type -> K.Type) -> K.Type) (e :: K.Type -> K.Type) = TimerWheel
  { unTimerWheel :: e (Ptr (CStruct TimerWheelSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (TimerWheel s Identity)

deriving stock instance Show (TimerWheel s Identity)

instance JanusHKD (TimerWheel s)

type TimerWheelSpineCtx e = ExpJanus e

type TimerWheelCtx m e s = StructCtx m e s

--------------------------------------------------------------------------------
-- Internal Pointer, Bitmask, & Circular-Sentinel Helpers
--------------------------------------------------------------------------------

-- | Branchless 6-step count-trailing-zeros for a non-zero @Word64@ (zero bit-shifts!).
ctzWord64 ::
  (ExpBits e Word64, ExpCond e, ExpEq e Word64, KnownJanusType e Word64, Num (e Word64)) =>
  e Word64 ->
  e Word64
ctzWord64 m =
  let lsb = m `iand` (0 - m)
      b5 = ifThenElse ((lsb `iand` 0x00000000FFFFFFFF) `eq` 0) 32 0
      b4 = ifThenElse ((lsb `iand` 0x0000FFFF0000FFFF) `eq` 0) 16 0
      b3 = ifThenElse ((lsb `iand` 0x00FF00FF00FF00FF) `eq` 0) 8 0
      b2 = ifThenElse ((lsb `iand` 0x0F0F0F0F0F0F0F0F) `eq` 0) 4 0
      b1 = ifThenElse ((lsb `iand` 0x3333333333333333) `eq` 0) 2 0
      b0 = ifThenElse ((lsb `iand` 0x5555555555555555) `eq` 0) 1 0
   in b5 + b4 + b3 + b2 + b1 + b0

asListHookPtr ::
  (ExpPtrCast e, KnownJanusType e Word8, KnownJanusType e (CStruct RawListHook)) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct RawListHook))
asListHookPtr p = fromVoidPtr (toVoidPtr p)

bucketsBaseRaw ::
  forall e.
  (TimerWheelSpineCtx e) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e (Ptr Word8)
bucketsBaseRaw hdr =
  withJanusType @(FixedArray TimerWheelBuckets (CStruct RawListHook)) @e $
    fromVoidPtr (toVoidPtr (getPtrOf (twsBuckets (projectPtrs hdr))))

bucketRawPtr ::
  (TimerWheelSpineCtx e) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Int64 ->
  e (Ptr Word8)
bucketRawPtr hdr linearIdx =
  ptrAdd (bucketsBaseRaw hdr) (linearIdx * sizeOf (Proxy @(CStruct RawListHook)))

overflowRawPtr ::
  forall e.
  (TimerWheelSpineCtx e) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e (Ptr Word8)
overflowRawPtr hdr =
  withJanusType @(CStruct RawListHook) @e $
    fromVoidPtr (toVoidPtr (getPtrOf (twsOverflow (projectPtrs hdr))))

occElemPtr ::
  (TimerWheelSpineCtx e) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e (Ptr Word64)
occElemPtr hdr =
  fixedArrayElemPtr (getPtrOf (twsOcc (projectPtrs hdr)))

nodeToHookRaw ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e Int64 ->
  e (Ptr (CStruct s)) ->
  e (Ptr Word8)
nodeToHookRaw hookOff pNode =
  ptrAdd (fromVoidPtr (toVoidPtr pNode) :: e (Ptr Word8)) hookOff

hookRawToNode ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e Int64 ->
  e (Ptr Word8) ->
  e (Ptr (CStruct s))
hookRawToNode hookOff pHookRaw =
  fromVoidPtr (toVoidPtr (ptrAdd pHookRaw (negate hookOff)))

peekHookDeadline ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Word8) ->
  m (e Word64)
peekHookDeadline hookOff deadlineOff pHookRaw = do
  let pNodeBytes = ptrAdd pHookRaw (negate hookOff)
      pDeadline = fromVoidPtr (toVoidPtr (ptrAdd pNodeBytes deadlineOff)) :: e (Ptr Word64)
  peek pDeadline

initSelfSentinel ::
  (CmdAllocBase m e) =>
  e (Ptr Word8) ->
  m ()
initSelfSentinel sentRaw = do
  let sf = projectPtrs (asListHookPtr sentRaw)
  poke (getPtrOf (rlhPrev sf)) sentRaw
  poke (getPtrOf (rlhNext sf)) sentRaw

linkListHookBetween ::
  (CmdAllocBase m e) =>
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  m ()
linkListHookBetween prevRaw nextRaw nodeHookRaw = do
  let pf = projectPtrs (asListHookPtr prevRaw)
      nf = projectPtrs (asListHookPtr nextRaw)
      hf = projectPtrs (asListHookPtr nodeHookRaw)
  poke (getPtrOf (rlhPrev hf)) prevRaw
  poke (getPtrOf (rlhNext hf)) nextRaw
  poke (getPtrOf (rlhNext pf)) nodeHookRaw
  poke (getPtrOf (rlhPrev nf)) nodeHookRaw

pushBackBucket ::
  forall m e.
  (CmdAllocBase m e) =>
  e (Ptr Word64) ->
  e Int64 ->
  e Word64 ->
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  m ()
pushBackBucket occPtr lvl slotW sentRaw nodeHookRaw = do
  lastRaw <- peek (getPtrOf (rlhPrev (projectPtrs (asListHookPtr sentRaw))))
  linkListHookBetween lastRaw sentRaw nodeHookRaw
  oldMask <- peekElemOff occPtr lvl
  pokeElemOff occPtr (oldMask `ior` ((1 :: e Word64) `lshift` slotW)) lvl

-- | Place @nodeHookRaw@ into the exact XOR-hierarchical bucket (or overflow list) relative to @curTick@.
placeHookAtTick ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Word64 ->
  e Int64 ->
  e Int64 ->
  e (Ptr Word8) ->
  m ()
placeHookAtTick hdr curTick hookOff deadlineOff nodeHookRaw = do
  deadline <- peekHookDeadline @s hookOff deadlineOff nodeHookRaw
  let eff = ifThenElse (deadline `le` curTick) curTick deadline
      xorVal = eff `ieor` curTick
      occPtr = occElemPtr hdr
  ifThenElseM_
    (xorVal `lt` 64)
    ( do
        let slotW = eff `iand` 63
            idx = toIntegral slotW :: e Int64
        pushBackBucket occPtr 0 slotW (bucketRawPtr hdr idx) nodeHookRaw
    )
    ( ifThenElseM_
        (xorVal `lt` 4096)
        ( do
            let slotW = (eff `rshift` 6) `iand` 63
                idx = 64 + (toIntegral slotW :: e Int64)
            pushBackBucket occPtr 1 slotW (bucketRawPtr hdr idx) nodeHookRaw
        )
        ( ifThenElseM_
            (xorVal `lt` 262144)
            ( do
                let slotW = (eff `rshift` 12) `iand` 63
                    idx = 128 + (toIntegral slotW :: e Int64)
                pushBackBucket occPtr 2 slotW (bucketRawPtr hdr idx) nodeHookRaw
            )
            ( ifThenElseM_
                (xorVal `lt` 16777216)
                ( do
                    let slotW = (eff `rshift` 18) `iand` 63
                        idx = 192 + (toIntegral slotW :: e Int64)
                    pushBackBucket occPtr 3 slotW (bucketRawPtr hdr idx) nodeHookRaw
                )
                ( do
                    let ovRaw = overflowRawPtr hdr
                    lastRaw <- peek (getPtrOf (rlhPrev (projectPtrs (asListHookPtr ovRaw))))
                    linkListHookBetween lastRaw ovRaw nodeHookRaw
                )
            )
        )
    )

-- | Detach all nodes from @sentRaw@ into a null-terminated singly-linked list via @rlhNext@
-- and reset @sentRaw@ to an empty circular self-loop BEFORE returning the detached head!
detachSentinelChain ::
  forall m e.
  (CmdAllocBase m e) =>
  e (Ptr Word8) ->
  m (e (Ptr Word8))
detachSentinelChain sentRaw = do
  let sf = projectPtrs (asListHookPtr sentRaw)
  firstRaw <- peek (getPtrOf (rlhNext sf))
  outRef <- newRef (nullPtr @e @Word8)
  whenM_ (ptrToWord64Expr firstRaw `ne` ptrToWord64Expr sentRaw) $ do
    lastRaw <- peek (getPtrOf (rlhPrev sf))
    poke (getPtrOf (rlhNext (projectPtrs (asListHookPtr lastRaw)))) nullPtr
    poke (getPtrOf (rlhPrev sf)) sentRaw
    poke (getPtrOf (rlhNext sf)) sentRaw
    writeRef outRef firstRaw
  readRef outRef

walkDetachedHooksS ::
  (CmdAllocBase m e) =>
  e (Ptr Word8) ->
  Stream m (e (Ptr Word8))
walkDetachedHooksS headRaw =
  unfoldWhileM headRaw (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \curRaw -> do
    nxt <- peek (getPtrOf (rlhNext (projectPtrs (asListHookPtr curRaw))))
    pure (nxt, curRaw)

cascadeBucket ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Word64 ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  e Word64 ->
  m ()
cascadeBucket hdr newTick hookOff deadlineOff lvl slotW = do
  let occPtr = occElemPtr hdr
      bit = (1 :: e Word64) `lshift` slotW
  oldMask <- peekElemOff occPtr lvl
  whenM_ ((oldMask `iand` bit) `ne` 0) $ do
    pokeElemOff occPtr (oldMask `iand` inot bit) lvl
    let idx = (lvl `lshift` 6) + (toIntegral slotW :: e Int64)
    detached <- detachSentinelChain (bucketRawPtr hdr idx)
    runDrain
      (drainForEach (placeHookAtTick @s hdr newTick hookOff deadlineOff))
      (walkDetachedHooksS detached)

cascadeOverflow ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Word64 ->
  e Int64 ->
  e Int64 ->
  m ()
cascadeOverflow hdr newTick hookOff deadlineOff = do
  detached <- detachSentinelChain (overflowRawPtr hdr)
  runDrain
    (drainForEach (placeHookAtTick @s hdr newTick hookOff deadlineOff))
    (walkDetachedHooksS detached)

cascadeAtTick ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Word64 ->
  e Int64 ->
  e Int64 ->
  m ()
cascadeAtTick hdr t hookOff deadlineOff =
  whenM_ ((t `iand` 63) `eq` 0) $ do
    whenM_ ((t `iand` 4095) `eq` 0) $ do
      whenM_ ((t `iand` 262143) `eq` 0) $ do
        whenM_ ((t `iand` 16777215) `eq` 0) $
          cascadeOverflow @s hdr t hookOff deadlineOff
        cascadeBucket @s hdr t hookOff deadlineOff 3 ((t `rshift` 18) `iand` 63)
      cascadeBucket @s hdr t hookOff deadlineOff 2 ((t `rshift` 12) `iand` 63)
    cascadeBucket @s hdr t hookOff deadlineOff 1 ((t `rshift` 6) `iand` 63)

-- | Compute the earliest future tick @> curTick@ that has either an occupied Level 0 slot
-- or a non-empty cascade bucket across Levels 1..3 / Overflow.
nextWakeOrCascadeTick ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  e (Ptr (CStruct TimerWheelSpine)) ->
  e Word64 ->
  m (e Bool, e Word64)
nextWakeOrCascadeTick hdr curTick = do
  let occPtr = occElemPtr hdr
  occ0 <- peekElemOff occPtr 0
  hasRef <- newRef false
  tickRef <- newRef curTick
  ifThenElseM_
    (occ0 `ne` 0)
    ( do
        writeRef hasRef true
        writeRef tickRef ((curTick `iand` inot 63) + ctzWord64 occ0)
    )
    ( do
        occ1 <- peekElemOff occPtr 1
        ifThenElseM_
          (occ1 `ne` 0)
          ( do
              writeRef hasRef true
              writeRef tickRef ((curTick `iand` inot 4095) + (ctzWord64 occ1 `lshift` 6))
          )
          ( do
              occ2 <- peekElemOff occPtr 2
              ifThenElseM_
                (occ2 `ne` 0)
                ( do
                    writeRef hasRef true
                    writeRef tickRef ((curTick `iand` inot 262143) + (ctzWord64 occ2 `lshift` 12))
                )
                ( do
                    occ3 <- peekElemOff occPtr 3
                    ifThenElseM_
                      (occ3 `ne` 0)
                      ( do
                          writeRef hasRef true
                          writeRef tickRef ((curTick `iand` inot 16777215) + (ctzWord64 occ3 `lshift` 18))
                      )
                      ( do
                          let ovRaw = overflowRawPtr hdr
                          ovFirst <- peek (getPtrOf (rlhNext (projectPtrs (asListHookPtr ovRaw))))
                          whenM_ (ptrToWord64Expr ovFirst `ne` ptrToWord64Expr ovRaw) $ do
                            writeRef hasRef true
                            writeRef tickRef (((curTick `rshift` 24) + 1) `lshift` 24)
                      )
                )
          )
    )
  (,) <$> readRef hasRef <*> readRef tickRef

--------------------------------------------------------------------------------
-- Allocating Lifecycle & Timer Node Helpers (Explicit `Alloc e`)
--------------------------------------------------------------------------------

-- | Allocate and initialize a 4-level x 64-slot 'TimerWheel' starting at @startTick@.
new ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  Alloc e ->
  TimerSpec s ->
  e Word64 ->
  m (TimerWheel s e)
new alloc (TimerSpec hookOff deadlineOff) startTick = do
  hdr <- allocStruct @(CStruct TimerWheelSpine) alloc
  let f = projectPtrs hdr
      occPtr = occElemPtr hdr
  runDrain
    (drainForEach (\i -> initSelfSentinel (bucketRawPtr hdr i)))
    (rangeS 0 256)
  initSelfSentinel (overflowRawPtr hdr)
  runDrain
    (drainForEach (\lvl -> pokeElemOff occPtr 0 lvl))
    (rangeS 0 4)
  poke (getPtrOf (twsCurrentTick f)) startTick
  poke (getPtrOf (twsHookOffset f)) (fromIntegral hookOff)
  poke (getPtrOf (twsDeadlineOffset f)) (fromIntegral deadlineOff)
  poke (getPtrOf (twsPendingCount f)) 0
  pure (TimerWheel hdr)

-- | Free ONLY the timer wheel spine (leaving any timer nodes untouched).
freeWheelSpine ::
  (TimerWheelCtx m e s) =>
  Alloc e ->
  TimerWheel s e ->
  m ()
freeWheelSpine alloc (TimerWheel hdr) = freeStruct alloc hdr

-- | Unlink and free every currently scheduled timer node via @alloc@, then free the wheel spine.
freeAllTimersAndSpine ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  Alloc e ->
  TimerWheel s e ->
  m ()
freeAllTimersAndSpine alloc wheel@(TimerWheel hdr) = do
  hookOff <- peek (getPtrOf (twsHookOffset (projectPtrs hdr)))
  let freeDetached sentRaw = do
        detached <- detachSentinelChain sentRaw
        runDrain
          (drainForEach (\hRaw -> freeStruct alloc (hookRawToNode @s hookOff hRaw)))
          (walkDetachedHooksS detached)
  runDrain
    (drainForEach (\i -> freeDetached (bucketRawPtr hdr i)))
    (rangeS 0 256)
  freeDetached (overflowRawPtr hdr)
  freeWheelSpine alloc wheel

-- | Allocate and initialize a timer node struct via @alloc@ from an HKD value @s e@.
allocTimerNode ::
  forall s m e.
  ( CmdAlloc m e,
    JanusStruct s,
    ExpSized e (CStruct s),
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocTimerNode alloc val = do
  p <- allocStruct @(CStruct s) alloc
  pokeStruct p val
  pure p

freeTimerNode ::
  (CmdAlloc m e, ExpSized e (CStruct s)) =>
  Alloc e ->
  e (Ptr (CStruct s)) ->
  m ()
freeTimerNode = freeStruct

-- | Allocate a timer node, initialize its hook, and schedule it into @wheel@.
allocAndSchedule ::
  forall s m e.
  ( TimerWheelCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  TimerWheel s e ->
  s e ->
  m (e (Ptr (CStruct s)))
allocAndSchedule alloc wheel val = do
  pNode <- allocTimerNode alloc val
  initTimerHook wheel pNode
  scheduleNode wheel pNode
  pure pNode

--------------------------------------------------------------------------------
-- Zero-Allocation Intrusive Timer Operations (No `Alloc e` Required!)
--------------------------------------------------------------------------------

initTimerHook ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e (Ptr (CStruct s)) ->
  m ()
initTimerHook (TimerWheel hdr) pNode = do
  hookOff <- peek (getPtrOf (twsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asListHookPtr (nodeToHookRaw @s hookOff pNode))
  poke (getPtrOf (rlhPrev hf)) nullPtr
  poke (getPtrOf (rlhNext hf)) nullPtr

isTimerScheduled ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e (Ptr (CStruct s)) ->
  m (e Bool)
isTimerScheduled (TimerWheel hdr) pNode = do
  hookOff <- peek (getPtrOf (twsHookOffset (projectPtrs hdr)))
  let hf = projectPtrs (asListHookPtr (nodeToHookRaw @s hookOff pNode))
  nxt <- peek (getPtrOf (rlhNext hf))
  pure (ptrToWord64Expr nxt `ne` 0)

pendingCount :: (TimerWheelCtx m e s) => TimerWheel s e -> m (e Int64)
pendingCount (TimerWheel hdr) = peek (getPtrOf (twsPendingCount (projectPtrs hdr)))

currentTick :: (TimerWheelCtx m e s) => TimerWheel s e -> m (e Word64)
currentTick (TimerWheel hdr) = peek (getPtrOf (twsCurrentTick (projectPtrs hdr)))

-- | O(1) zero-allocation timer scheduling (reschedule-safe: if @pNode@ is already scheduled
-- in @wheel@, cancels it first before placing at its current deadline field).
scheduleNode ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e (Ptr (CStruct s)) ->
  m ()
scheduleNode wheel@(TimerWheel hdr) pNode = do
  _ <- cancelNode wheel pNode
  let f = projectPtrs hdr
  curTick <- peek (getPtrOf (twsCurrentTick f))
  hookOff <- peek (getPtrOf (twsHookOffset f))
  deadlineOff <- peek (getPtrOf (twsDeadlineOffset f))
  cnt <- peek (getPtrOf (twsPendingCount f))
  placeHookAtTick @s hdr curTick hookOff deadlineOff (nodeToHookRaw @s hookOff pNode)
  poke (getPtrOf (twsPendingCount f)) (cnt + 1)

-- | O(1) zero-allocation timer cancellation!
-- When unlinking the LAST node in a bucket (@prvRaw == nxtRaw@), @prvRaw@ is the exact
-- address of the bucket sentinel inside @twsBuckets[0..255]@, allowing O(1) pointer-offset
-- computation of @(lvl, slot)@ to clear @twsOcc[lvl]@ without storing any extra field in @CStruct s@!
cancelNode ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e (Ptr (CStruct s)) ->
  m (e Bool)
cancelNode (TimerWheel hdr) pNode = do
  let f = projectPtrs hdr
  hookOff <- peek (getPtrOf (twsHookOffset f))
  let hf = projectPtrs (asListHookPtr (nodeToHookRaw @s hookOff pNode))
  nxtRaw <- peek (getPtrOf (rlhNext hf))
  let wasScheduled = ptrToWord64Expr nxtRaw `ne` 0
  whenM_ wasScheduled $ do
    prvRaw <- peek (getPtrOf (rlhPrev hf))
    poke (getPtrOf (rlhNext (projectPtrs (asListHookPtr prvRaw)))) nxtRaw
    poke (getPtrOf (rlhPrev (projectPtrs (asListHookPtr nxtRaw)))) prvRaw
    poke (getPtrOf (rlhPrev hf)) nullPtr
    poke (getPtrOf (rlhNext hf)) nullPtr
    cnt <- peek (getPtrOf (twsPendingCount f))
    poke (getPtrOf (twsPendingCount f)) (cnt - 1)

    let prvW = ptrToWord64Expr prvRaw
        nxtW = ptrToWord64Expr nxtRaw
        ovW = ptrToWord64Expr (overflowRawPtr hdr)
    whenM_ ((prvW `eq` nxtW) `land` (prvW `ne` ovW)) $ do
      let baseW = ptrToWord64Expr (bucketsBaseRaw hdr)
          hookSzW = toIntegral (sizeOf (Proxy @(CStruct RawListHook))) :: e Word64
          linearIdx = (prvW - baseW) `quotient` hookSzW
          lvl = toIntegral (linearIdx `rshift` 6) :: e Int64
          slotW = linearIdx `iand` 63
          occPtr = occElemPtr hdr
      oldMask <- peekElemOff occPtr lvl
      pokeElemOff occPtr (oldMask `iand` inot ((1 :: e Word64) `lshift` slotW)) lvl
  pure wasScheduled

-- | Unlink and zero every scheduled timer's hook without freeing node memory.
clearTimers ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  m ()
clearTimers (TimerWheel hdr) = do
  let f = projectPtrs hdr
      occPtr = occElemPtr hdr
      zeroDetached sentRaw = do
        detached <- detachSentinelChain sentRaw
        runDrain
          ( drainForEach $ \hRaw -> do
              let hf = projectPtrs (asListHookPtr hRaw)
              poke (getPtrOf (rlhPrev hf)) nullPtr
              poke (getPtrOf (rlhNext hf)) nullPtr
          )
          (walkDetachedHooksS detached)
  runDrain
    (drainForEach (\i -> zeroDetached (bucketRawPtr hdr i)))
    (rangeS 0 256)
  zeroDetached (overflowRawPtr hdr)
  runDrain
    (drainForEach (\lvl -> pokeElemOff occPtr 0 lvl))
    (rangeS 0 4)
  poke (getPtrOf (twsPendingCount f)) 0

--------------------------------------------------------------------------------
-- Hierarchical Bitmask Tick-Skipping Expiration Streams & Drains
--------------------------------------------------------------------------------

-- | Advance @wheel@ monotonically up to @targetTick@, yielding every expired timer
-- (@deadline <= targetTick@) in non-decreasing effective-tick order while jumping directly
-- over empty tick ranges in O(1) via @twsOcc@ bitmasks!
--
-- Each yielded timer is already unlinked before being passed to downstream consumers, so
-- freeing, cancelling other timers, or rescheduling periodic timers inside the consumer is 100% safe!
advanceExpiredStream ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e Word64 ->
  Stream m (e (Ptr (CStruct s)))
advanceExpiredStream (TimerWheel hdr) targetTick =
  let f = projectPtrs hdr
   in statefulS
        ((,) <$> peek (getPtrOf (twsHookOffset f)) <*> peek (getPtrOf (twsDeadlineOffset f)))
        $ \(hookOff, deadlineOff) -> ContT $ \kStep -> do
          curTick <- peek (getPtrOf (twsCurrentTick f))
          ifThenElseM_
            (curTick `gt` targetTick)
            (kStep Stop)
            ( do
                let slot0W = curTick `iand` 63
                    slot0I = toIntegral slot0W :: e Int64
                    b0Raw = bucketRawPtr hdr slot0I
                    b0f = projectPtrs (asListHookPtr b0Raw)
                firstRaw <- peek (getPtrOf (rlhNext b0f))
                ifThenElseM_
                  (ptrToWord64Expr firstRaw `ne` ptrToWord64Expr b0Raw)
                  ( do
                      let hf = projectPtrs (asListHookPtr firstRaw)
                      nxtRaw <- peek (getPtrOf (rlhNext hf))
                      poke (getPtrOf (rlhNext b0f)) nxtRaw
                      poke (getPtrOf (rlhPrev (projectPtrs (asListHookPtr nxtRaw)))) b0Raw
                      poke (getPtrOf (rlhPrev hf)) nullPtr
                      poke (getPtrOf (rlhNext hf)) nullPtr
                      cnt <- peek (getPtrOf (twsPendingCount f))
                      poke (getPtrOf (twsPendingCount f)) (cnt - 1)
                      whenM_ (ptrToWord64Expr nxtRaw `eq` ptrToWord64Expr b0Raw) $ do
                        let occPtr = occElemPtr hdr
                        m0 <- peekElemOff occPtr 0
                        pokeElemOff occPtr (m0 `iand` inot ((1 :: e Word64) `lshift` slot0W)) 0
                      kStep (Yield (hookRawToNode @s hookOff firstRaw))
                  )
                  ( ifThenElseM_
                      (curTick `eq` targetTick)
                      (kStep Stop)
                      ( do
                          (hasWake, candTick) <- nextWakeOrCascadeTick @s hdr curTick
                          ifThenElseM_
                            (hasWake `land` (candTick `le` targetTick))
                            ( do
                                poke (getPtrOf (twsCurrentTick f)) candTick
                                cascadeAtTick @s hdr candTick hookOff deadlineOff
                                kStep Skip
                            )
                            ( do
                                poke (getPtrOf (twsCurrentTick f)) targetTick
                                kStep Stop
                            )
                      )
                  )
            )

-- | Advance @wheel@ to @targetTick@, invoking @onExpire@ for each expired timer node
-- and returning the total count of fired timers.
drainAdvanceExpired ::
  forall s m e.
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  e Word64 ->
  (e (Ptr (CStruct s)) -> m ()) ->
  m (e Int64)
drainAdvanceExpired wheel targetTick onExpire =
  runDrain
    (drainFoldM (0 :: e Int64) (\cnt p -> onExpire p >> pure (cnt + 1)))
    (advanceExpiredStream wheel targetTick)

-- | Non-allocating drain scheduling pre-allocated @e (Ptr (CStruct s))@ timer nodes into @wheel@.
drainAppendNodesWheel ::
  (TimerWheelCtx m e s) =>
  TimerWheel s e ->
  Drain m (e (Ptr (CStruct s))) ()
drainAppendNodesWheel wheel = drainForEach (scheduleNode wheel)

-- | Allocate and schedule a new timer node into @wheel@ for each HKD value yielded by a 'Stream'.
drainAllocAppendWheel ::
  ( TimerWheelCtx m e s,
    CmdAlloc m e,
    Generic (s (PtrOf e)),
    Generic (s e),
    GJanusStructProject e (Rep (s Identity)) (Rep (s (PtrOf e))),
    GJanusStructVal m e (Rep (s (PtrOf e))) (Rep (s e))
  ) =>
  Alloc e ->
  TimerWheel s e ->
  Drain m (s e) ()
drainAllocAppendWheel alloc wheel =
  drainForEach (\v -> allocAndSchedule alloc wheel v >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (TimerWheel s (JanusCT _)) = TimerWheel s Identity

deriving via
  ViaPtrHandle t (CStruct TimerWheelSpine) (TimerWheel s Identity)
  instance
    (Typeable s) => JanusCArg t (TimerWheel s (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct TimerWheelSpine) (TimerWheel s Identity)
  instance
    (Typeable s) => JanusCValRet t (TimerWheel s (JanusCT t))
