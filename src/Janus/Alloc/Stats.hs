{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Memory (CmdMem (..), CmdStorable (..), ExpSized (..), allocaAligned)
import Janus.Command.Cond (whenM_)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Data.HKD
import Janus.Typed (JanusTyped)

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
  (CmdAllocBase m e, ExpSized e (CStruct AllocTrackerSpine), JanusCTyped (CStruct AllocTrackerSpine)) =>
  m (AllocTracker e)
newAllocTracker = do
  vp <- calloc 1 (sizeOf (Proxy @(CStruct AllocTrackerSpine)))
  pure (AllocTracker (fromVoidPtr vp))

freeAllocTracker ::
  (CmdMem m e, ExpPtrCast e, JanusTyped e (CStruct AllocTrackerSpine)) =>
  AllocTracker e ->
  m ()
freeAllocTracker (AllocTracker p) = free (toVoidPtr p)

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
type instance JanusCEval (AllocTracker JanusC -> r) = AllocTracker Identity -> JanusCEval r

type instance JanusCRetType (AllocTracker JanusC -> r) = JanusCRetType r

instance (JanusCParam r) => JanusCParam (AllocTracker JanusC -> r) where
  jcparam = mkPtrHandleJCParam AllocTracker

instance (JanusCEvaluate r) => JanusCEvaluate (AllocTracker JanusC -> r) where
  jceval = mkPtrHandleJCEval unAllocTracker AllocTracker

type instance LinuxX86_64CEval (AllocTracker LinuxX86_64C -> r) = AllocTracker Identity -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (AllocTracker LinuxX86_64C -> r) = LinuxX86_64BaseRet r

instance (LinuxX86_64Param r) => LinuxX86_64Param (AllocTracker LinuxX86_64C -> r) where
  x86param = mkPtrHandleX86Param AllocTracker

instance (X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (AllocTracker LinuxX86_64C -> r) where
  x86eval = mkPtrHandleX86EvalParam unAllocTracker AllocTracker

instance (X86DeferCont r) => X86DeferCall (AllocTracker LinuxX86_64C -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unAllocTracker AllocTracker

instance (X86DeferCont r) => X86DeferCont (AllocTracker LinuxX86_64C -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unAllocTracker AllocTracker

instance (LinuxX86_64Param r, JanusCTH r) => JanusCTH (AllocTracker LinuxX86_64C -> r) where
  type JanusCTHEval (AllocTracker LinuxX86_64C -> r) = AllocTracker Identity -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(AllocTracker Identity)) : ps, retSpec)
