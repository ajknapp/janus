{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.IntrusiveSchedAndIntern where

import Control.Lens ((&), (.~))
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (minimumBy, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Word (Word64, Word8)
import Foreign.Ptr (nullPtr)
import GHC.Generics (Generic)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream)
import Janus.Data.HKD
import Janus.Data.Interner (Interner)
import qualified Janus.Data.Interner as INTR
import Janus.Data.IntrusiveHeap (HeapSpec, IntrusiveHeap, RawHeapHook (..), emptyHeapHook, mkHeapSpec)
import qualified Janus.Data.IntrusiveHeap as IH
import Janus.Data.TimerWheel (RawListHook (..), TimerSpec, TimerWheel, emptyTimerHook, mkTimerSpec)
import qualified Janus.Data.TimerWheel as TW
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getIntrusiveSchedTestConfig :: IO CBuildConfig
getIntrusiveSchedTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

--------------------------------------------------------------------------------
-- Shared Multi-Hook HKD Node Struct (`RawHeapHook` + `RawListHook`!)
--------------------------------------------------------------------------------

data SchedNode f = SchedNode
  { snHeapHook :: !(f (CStruct RawHeapHook)),
    snTimerHook :: !(f (CStruct RawListHook)),
    snPriority :: !(f Int64),
    snDeadline :: !(f Word64),
    snId :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (SchedNode Identity)

deriving stock instance Show (SchedNode Identity)

instance JanusStruct SchedNode

schedHeapSpec :: HeapSpec SchedNode Int64
schedHeapSpec = mkHeapSpec snHeapHook snPriority

schedTimerSpec :: TimerSpec SchedNode
schedTimerSpec = mkTimerSpec snTimerHook snDeadline

--------------------------------------------------------------------------------
-- 1. IntrusiveHeap Zero-Allocation Pairing Heap Property Test
--------------------------------------------------------------------------------

data HeapOp
  = HOPush !Int !Int64
  | HODecrease !Int !Int64
  | HORemove !Int
  | HOPopMin
  | HOMergeAux !Int !Int64
  deriving stock (Show)

genHeapOp :: Int -> Gen HeapOp
genHeapOp numNodes =
  Gen.frequency
    [ (5, HOPush <$> Gen.int (Range.linear 0 (numNodes - 1)) <*> Gen.int64 (Range.linear (-200) 200)),
      (4, HODecrease <$> Gen.int (Range.linear 0 (numNodes - 1)) <*> Gen.int64 (Range.linear 1 80)),
      (3, HORemove <$> Gen.int (Range.linear 0 (numNodes - 1))),
      (3, pure HOPopMin),
      (2, HOMergeAux <$> Gen.int (Range.linear 0 (numNodes - 1)) <*> Gen.int64 (Range.linear (-200) 200))
    ]

check_intrusive_heap_zero_alloc_churn :: Property
check_intrusive_heap_zero_alloc_churn = property $ do
  let numNodes = 32
  ops <- forAll $ Gen.list (Range.linear 1 220) (genHeapOp numNodes)

  (zeroChurnAllocs, remPoppedPrios, refRemPrios, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      hpMain <- IH.new alloc schedHeapSpec
      hpAux <- IH.new alloc schedHeapSpec

      -- Pre-allocate all `numNodes` nodes once before churn
      nodes <-
        forM [0 .. numNodes - 1] $ \idx ->
          IH.allocNode
            alloc
            SchedNode
              { snHeapHook = Identity (CStruct (emptyHeapHook @Identity)),
                snTimerHook = Identity (CStruct (emptyTimerHook @Identity)),
                snPriority = Identity 0,
                snDeadline = Identity 0,
                snId = Identity (fromIntegral idx)
              }
      let nodeAt i = nodes !! i

      snapBefore <- readAllocSnapshot tracker

      let step model op = case op of
            HOPush idx prio ->
              if Map.member idx model
                then pure model
                else do
                  let pNode = nodeAt idx
                  poke (getPtrOf (snPriority (projectPtrs pNode))) (Identity prio)
                  IH.pushNode hpMain pNode
                  pure (Map.insert idx prio model)
            HODecrease idx delta ->
              case Map.lookup idx model of
                Nothing -> pure model
                Just oldP -> do
                  let newP = oldP - delta
                      pNode = nodeAt idx
                  poke (getPtrOf (snPriority (projectPtrs pNode))) (Identity newP)
                  IH.decreaseKeyNode hpMain pNode
                  pure (Map.insert idx newP model)
            HORemove idx ->
              case Map.lookup idx model of
                Nothing -> pure model
                Just _ -> do
                  IH.removeNode hpMain (nodeAt idx)
                  pure (Map.delete idx model)
            HOPopMin ->
              if Map.null model
                then do
                  Identity pOut <- IH.popMinNode hpMain
                  Identity isEmptyNow <- IH.isEmpty hpMain
                  if isEmptyNow && pOut == nullPtr
                    then pure model
                    else error "IntrusiveHeap popMinNode non-null on empty"
                else do
                  pMin <- IH.popMinNode hpMain
                  let nf = projectPtrs pMin
                  Identity gotId <- peek (getPtrOf (snId nf))
                  Identity gotP <- peek (getPtrOf (snPriority nf))
                  let minP = minimum (Map.elems model)
                      idx = fromIntegral gotId :: Int
                  case Map.lookup idx model of
                    Just mp | gotP == minP && mp == minP -> pure (Map.delete idx model)
                    _ -> error "IntrusiveHeap popMinNode priority mismatch"
            HOMergeAux idx prio ->
              if Map.member idx model
                then pure model
                else do
                  let pNode = nodeAt idx
                  poke (getPtrOf (snPriority (projectPtrs pNode))) (Identity prio)
                  IH.pushNode hpAux pNode
                  IH.mergeHeaps hpMain hpAux
                  pure (Map.insert idx prio model)

      modelEnd <- foldlM' step Map.empty ops
      remNodes <- collectStreamHost (IH.popAllNodesS hpMain)
      remPrios <- mapM (\p -> runIdentity <$> peek (getPtrOf (snPriority (projectPtrs p)))) remNodes

      snapAfter <- readAllocSnapshot tracker
      let churnAllocs = runIdentity (snapTotalAllocs snapAfter) - runIdentity (snapTotalAllocs snapBefore)

      forM_ nodes (IH.freeNode alloc)
      IH.freeHeapSpine alloc hpAux
      IH.freeHeapSpine alloc hpMain
      snapEnd <- readAllocSnapshot tracker
      pure
        ( churnAllocs,
          remPrios,
          sort (Map.elems modelEnd),
          (runIdentity (snapLiveBlocks snapEnd), runIdentity (snapLiveBytes snapEnd))
        )

  zeroChurnAllocs === 0
  remPoppedPrios === refRemPrios
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 2. TimerWheel 4-Level + Overflow Bitmask Tick-Skipping Property Test
--------------------------------------------------------------------------------

genDeadlineOffset :: Gen Word64
genDeadlineOffset =
  Gen.frequency
    [ (4, Gen.word64 (Range.linear 0 63)),
      (4, Gen.word64 (Range.linear 64 4095)),
      (3, Gen.word64 (Range.linear 4096 262143)),
      (2, Gen.word64 (Range.linear 262144 16777215)),
      (2, Gen.word64 (Range.linear 16777216 22000000))
    ]

check_timer_wheel_multilevel_and_overflow :: Property
check_timer_wheel_multilevel_and_overflow = property $ do
  startTick <- forAll $ Gen.word64 (Range.linear 0 500)
  rawDeadlines <- forAll $ Gen.list (Range.linear 1 60) genDeadlineOffset
  cancelFlags <- forAll $ Gen.list (Range.singleton (length rawDeadlines)) Gen.bool
  stepJumps <- forAll $ Gen.list (Range.linear 1 12) (Gen.word64 (Range.linear 1 3000000))

  (zeroChurnAllocs, firedPairs, expectedFiredIds, exactDeadlineOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      wheel <- TW.new alloc schedTimerSpec (Identity startTick)

      let specs = zip3 [0 :: Int64 ..] (map (+ startTick) rawDeadlines) cancelFlags
      nodes <-
        forM specs $ \(tid, dl, _) ->
          TW.allocTimerNode
            alloc
            SchedNode
              { snHeapHook = Identity (CStruct (emptyHeapHook @Identity)),
                snTimerHook = Identity (CStruct (emptyTimerHook @Identity)),
                snPriority = Identity 0,
                snDeadline = Identity dl,
                snId = Identity tid
              }

      snapBefore <- readAllocSnapshot tracker

      -- Schedule all timers
      forM_ nodes (TW.scheduleNode wheel)

      -- Cancel flagged timers in O(1)
      forM_ (zip nodes specs) $ \(pNode, (_, _, doCancel)) ->
        when doCancel $ do
          Identity cancelled <- TW.cancelNode wheel pNode
          when (not cancelled) $ error "Expected scheduled timer to cancel cleanly"

      -- Advance through step jumps, then finally advance past max deadline (startTick + 25,000,000)
      let scanTargets = drop 1 (scanl (+) startTick stepJumps) ++ [startTick + 25000000]
      firedRef <- newIORef ([] :: [(Int64, Word64, Word64)])

      forM_ scanTargets $ \target -> do
        runDrain
          ( drainForEach $ \pExpired -> do
              Identity fireTick <- TW.currentTick wheel
              let nf = projectPtrs pExpired
              Identity tid <- peek (getPtrOf (snId nf))
              Identity dl <- peek (getPtrOf (snDeadline nf))
              modifyIORef' firedRef ((tid, dl, fireTick) :)
          )
          (TW.advanceExpiredStream wheel (Identity target))

      snapAfter <- readAllocSnapshot tracker
      let churnAllocs = runIdentity (snapTotalAllocs snapAfter) - runIdentity (snapTotalAllocs snapBefore)

      fired <- reverse <$> readIORef firedRef
      let timingExact = all (\(_, dl, fireTick) -> fireTick == max startTick dl) fired
          firedIdsSorted = sort [tid | (tid, _, _) <- fired]
          expectedIds = sort [tid | (tid, _, doCancel) <- specs, not doCancel]

      forM_ nodes (TW.freeTimerNode alloc)
      TW.freeWheelSpine alloc wheel
      snapEnd <- readAllocSnapshot tracker
      pure
        ( churnAllocs,
          firedIdsSorted,
          expectedIds,
          timingExact,
          (runIdentity (snapLiveBlocks snapEnd), runIdentity (snapLiveBytes snapEnd))
        )

  zeroChurnAllocs === 0
  firedPairs === expectedFiredIds
  assert exactDeadlineOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. Interner Bijective Symbol Assignment, Raw Bytes, & Zero Leaks
--------------------------------------------------------------------------------

genArbitraryBytes :: Gen [Word8]
genArbitraryBytes =
  Gen.frequency
    [ (1, pure []),
      (4, Gen.list (Range.linear 1 12) (Gen.word8 Range.constantBounded)),
      (2, Gen.list (Range.linear 64 320) (Gen.word8 Range.constantBounded))
    ]

check_interner_bijection_and_resolution :: Property
check_interner_bijection_and_resolution = property $ do
  slices <- forAll $ Gen.list (Range.linear 1 120) genArbitraryBytes
  extraProbes <- forAll $ Gen.list (Range.linear 0 40) genArbitraryBytes

  (bijectionOk, roundTripOk, probeOk, finalCount, expectedUniqueCount, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      intr <- INTR.new alloc

      let step (fwdMap, revMap, okAcc) bs = do
            sym <- INTR.internByteList alloc intr bs
            case Map.lookup bs fwdMap of
              Just prevSym ->
                pure (fwdMap, revMap, okAcc && sym == prevSym)
              Nothing ->
                let freshOk = Map.notMember sym revMap
                 in pure (Map.insert bs sym fwdMap, Map.insert sym bs revMap, okAcc && freshOk)

      (fwdMap, revMap, bijOk) <- foldlM' step (Map.empty, Map.empty, True) (slices ++ take 30 slices)

      -- Verify O(1) resolveByteList matches every interned byte list bit-for-bit
      rtChecks <-
        forM (Map.toList revMap) $ \(sym, origBs) -> do
          mGot <- INTR.resolveByteList intr sym
          pure (mGot == Just origBs)

      -- Verify lookupByteList hits iff slice is in `fwdMap`
      prChecks <-
        forM (slices ++ extraProbes) $ \bs -> do
          (hit, sym) <- INTR.lookupByteList intr bs
          case Map.lookup bs fwdMap of
            Nothing -> pure (not hit)
            Just expSym -> pure (hit && sym == expSym)

      Identity cnt <- INTR.count intr

      -- Exercise `clear` + re-interning
      INTR.clear alloc intr
      sA <- INTR.internString alloc intr "alpha\0beta"
      sB <- INTR.internString alloc intr "alpha\0beta"
      mStr <- INTR.resolveString intr sA

      INTR.free alloc intr
      snapEnd <- readAllocSnapshot tracker
      pure
        ( bijOk && sA == sB && mStr == Just "alpha\0beta",
          and rtChecks,
          and prChecks,
          cnt,
          fromIntegral (length (nub slices)) :: Int64,
          (runIdentity (snapLiveBlocks snapEnd), runIdentity (snapLiveBytes snapEnd))
        )

  assert bijectionOk
  assert roundTripOk
  assert probeOk
  finalCount === expectedUniqueCount
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 4. Host <-> Compiled C FFI Round-Trip (`IntrusiveHeap`, `TimerWheel`, `Interner`)
--------------------------------------------------------------------------------

cSchedAndInternKernel ::
  IntrusiveHeap SchedNode Int64 LinuxX86_64C ->
  TimerWheel SchedNode LinuxX86_64C ->
  Interner LinuxX86_64C ->
  LinuxX86_64C Word64 ->
  LinuxX86_64CM (LinuxX86_64C Int64)
cSchedAndInternKernel hp wheel intr targetTick = do
  pMin <- IH.popMinNode hp
  minPrio <- peek (getPtrOf (snPriority (projectPtrs pMin)))
  firedCnt <- TW.drainAdvanceExpired wheel targetTick (\_ -> pure ())
  lenSum <- runDrain drainSum ((\(_, _, l) -> l) <$> INTR.entriesS intr)
  pure (minPrio + firedCnt * 100 + lenSum)

check_intrusive_sched_and_intern_c_ffi ::
  IO
    ( IntrusiveHeap SchedNode Int64 Identity ->
      TimerWheel SchedNode Identity ->
      Interner Identity ->
      Word64 ->
      IO Int64
    ) ->
  Property
check_intrusive_sched_and_intern_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  prios <- forAll $ Gen.list (Range.linear 1 20) (Gen.int64 (Range.linear (-50) 50))
  deadlines <- forAll $ Gen.list (Range.linear 1 20) (Gen.word64 (Range.linear 1 50000))
  cutoff <- forAll $ Gen.word64 (Range.linear 1 50000)
  strs <- forAll $ Gen.list (Range.linear 1 20) (Gen.string (Range.linear 0 25) Gen.alphaNum)

  (cRes, expectedRes) <- liftIO $ do
    hp <- IH.new dynamicAlloc schedHeapSpec
    wheel <- TW.new dynamicAlloc schedTimerSpec 0
    intr <- INTR.new dynamicAlloc

    hNodes <-
      forM (zip [0 :: Int64 ..] prios) $ \(idx, p) -> do
        pn <-
          IH.allocNode
            dynamicAlloc
            SchedNode
              { snHeapHook = Identity (CStruct (emptyHeapHook @Identity)),
                snTimerHook = Identity (CStruct (emptyTimerHook @Identity)),
                snPriority = Identity p,
                snDeadline = Identity 0,
                snId = Identity idx
              }
        IH.pushNode hp pn
        pure pn

    tNodes <-
      forM (zip [0 :: Int64 ..] deadlines) $ \(idx, dl) -> do
        tn <-
          TW.allocTimerNode
            dynamicAlloc
            SchedNode
              { snHeapHook = Identity (CStruct (emptyHeapHook @Identity)),
                snTimerHook = Identity (CStruct (emptyTimerHook @Identity)),
                snPriority = Identity 0,
                snDeadline = Identity dl,
                snId = Identity idx
              }
        TW.scheduleNode wheel tn
        pure tn

    forM_ strs (INTR.internString dynamicAlloc intr)
    Identity totInternBytes <- INTR.totalBytes intr

    res <- cFn hp wheel intr cutoff

    forM_ hNodes (IH.freeNode dynamicAlloc)
    forM_ tNodes (TW.freeTimerNode dynamicAlloc)
    IH.freeHeapSpine dynamicAlloc hp
    TW.freeWheelSpine dynamicAlloc wheel
    INTR.free dynamicAlloc intr

    let minP = minimumBy compare prios
        expFired = fromIntegral (length [dl | dl <- deadlines, dl <= cutoff]) :: Int64
    pure (res, minP + expFired * 100 + totInternBytes)

  cRes === expectedRes

test_intrusive_sched_and_intern :: TestTree
test_intrusive_sched_and_intern =
  let acq = getIntrusiveSchedTestConfig >>= flip acquireJanusC cSchedAndInternKernel
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "Zero-Alloc Intrusive Schedulers (IntrusiveHeap, TimerWheel) & Byte Interner"
          [ testProperty "IntrusiveHeap pairing heap O(1) push/merge/decreaseKey & O(log N) remove/popMin with 0 churn allocs" check_intrusive_heap_zero_alloc_churn,
            testProperty "TimerWheel 4-level x 64-slot + overflow bitmask tick-skipping & O(1) cancel with 0 churn allocs" check_timer_wheel_multilevel_and_overflow,
            testProperty "Interner bijective Symbol deduplication, embedded nulls, & O(1) resolution" check_interner_bijection_and_resolution,
            testProperty "Host <-> Compiled C FFI round-trip across IntrusiveHeap, TimerWheel, & Interner" (check_intrusive_sched_and_intern_c_ffi (fst <$> rFn))
          ]
