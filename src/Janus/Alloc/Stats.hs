{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Alloc.Stats
  ( AllocTrackerSpine (..),
    AllocTracker (..),
    AllocSnapshot (..),
    newAllocTracker,
    freeAllocTracker,
    withAllocTracker,
    resetAllocTracker,
    readAllocSnapshot,
    statsRecordAlloc,
    statsRecordFree,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, isNullVoidPtr)
import Janus.Backend.C.Raw
import Janus.Command.Memory (CmdMem (..), CmdStorable (..), ExpSized (..), allocaAligned)
import Janus.Command.Cond (whenM_)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Constraints (KnownJanusType, withJanusType)
import Janus.Data.HKD

data AllocTrackerSpine f = AllocTrackerSpine
  { atsLiveBlocks :: !(f Int64),
    atsLiveBytes :: !(f Int64),
    atsPeakBytes :: !(f Int64),
    atsTotalAllocs :: !(f Int64),
    atsTotalFrees :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (AllocTrackerSpine Identity)

deriving stock instance Show (AllocTrackerSpine Identity)

instance JanusStruct AllocTrackerSpine

newtype AllocTracker e = AllocTracker
  { unAllocTracker :: e (Ptr (CStruct AllocTrackerSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (AllocTracker Identity)

deriving stock instance Show (AllocTracker Identity)

instance JanusHKD AllocTracker

data AllocSnapshot e = AllocSnapshot
  { snapLiveBlocks :: !(e Int64),
    snapLiveBytes :: !(e Int64),
    snapPeakBytes :: !(e Int64),
    snapTotalAllocs :: !(e Int64),
    snapTotalFrees :: !(e Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (AllocSnapshot Identity)

deriving stock instance Show (AllocSnapshot Identity)

newAllocTracker ::
  forall m e.
  (CmdAllocBase m e) =>
  m (AllocTracker e)
newAllocTracker =
  withJanusType @(CStruct AllocTrackerSpine) @e $ do
    vp <- calloc 1 (sizeOf (Proxy @(CStruct AllocTrackerSpine)))
    pure (AllocTracker (fromVoidPtr vp))

freeAllocTracker ::
  forall m e.
  (CmdMem m e, ExpPtrCast e, KnownJanusType e (CStruct AllocTrackerSpine)) =>
  AllocTracker e ->
  m ()
freeAllocTracker (AllocTracker p) =
  withJanusType @(CStruct AllocTrackerSpine) @e $
    free (toVoidPtr p)

withAllocTracker :: (AllocTracker Identity -> IO a) -> IO a
withAllocTracker k =
  allocaAligned 1 $ \(p :: Ptr (CStruct AllocTrackerSpine)) ->
    k (AllocTracker (Identity p))

resetAllocTracker ::
  (CmdAllocBase m e) =>
  AllocTracker e ->
  m ()
resetAllocTracker (AllocTracker p) = do
  let f = projectPtrs p
  poke (getPtrOf (atsLiveBlocks f)) 0
  poke (getPtrOf (atsLiveBytes f)) 0
  poke (getPtrOf (atsPeakBytes f)) 0
  poke (getPtrOf (atsTotalAllocs f)) 0
  poke (getPtrOf (atsTotalFrees f)) 0

readAllocSnapshot ::
  (CmdAllocBase m e) =>
  AllocTracker e ->
  m (AllocSnapshot e)
readAllocSnapshot (AllocTracker p) = do
  let f = projectPtrs p
  AllocSnapshot
    <$> peek (getPtrOf (atsLiveBlocks f))
    <*> peek (getPtrOf (atsLiveBytes f))
    <*> peek (getPtrOf (atsPeakBytes f))
    <*> peek (getPtrOf (atsTotalAllocs f))
    <*> peek (getPtrOf (atsTotalFrees f))

statsRecordAlloc ::
  (CmdAllocBase m e) =>
  AllocTracker e ->
  e (Ptr ()) ->
  e Int64 ->
  m ()
statsRecordAlloc (AllocTracker p) resPtr sz =
  whenM_ (lnot (isNullVoidPtr resPtr)) $ do
    let f = projectPtrs p
    lb <- peek (getPtrOf (atsLiveBlocks f))
    lbytes <- peek (getPtrOf (atsLiveBytes f))
    peak <- peek (getPtrOf (atsPeakBytes f))
    totA <- peek (getPtrOf (atsTotalAllocs f))
    let lbytes' = lbytes + sz
        peak' = ifThenElse (lbytes' `gt` peak) lbytes' peak
    poke (getPtrOf (atsLiveBlocks f)) (lb + 1)
    poke (getPtrOf (atsLiveBytes f)) lbytes'
    poke (getPtrOf (atsPeakBytes f)) peak'
    poke (getPtrOf (atsTotalAllocs f)) (totA + 1)

statsRecordFree ::
  (CmdAllocBase m e) =>
  AllocTracker e ->
  e (Ptr ()) ->
  e Int64 ->
  m ()
statsRecordFree (AllocTracker p) ptr sz =
  whenM_ (lnot (isNullVoidPtr ptr)) $ do
    let f = projectPtrs p
    lb <- peek (getPtrOf (atsLiveBlocks f))
    lbytes <- peek (getPtrOf (atsLiveBytes f))
    totF <- peek (getPtrOf (atsTotalFrees f))
    poke (getPtrOf (atsLiveBlocks f)) (lb - 1)
    poke (getPtrOf (atsLiveBytes f)) (lbytes - sz)
    poke (getPtrOf (atsTotalFrees f)) (totF + 1)

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (AllocTracker (JanusCT _)) = AllocTracker Identity

deriving via
  ViaPtrHandle t (CStruct AllocTrackerSpine) (AllocTracker Identity)
  instance
    JanusCArg t (AllocTracker (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct AllocTrackerSpine) (AllocTracker Identity)
  instance
    JanusCValRet t (AllocTracker (JanusCT t))
