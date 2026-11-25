{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.StackAndHeap where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (minimumBy, sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Command.Cond (whenM_)
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream, rangeS)
import Janus.Data.HKD
import Janus.Data.IndexedPQ (IndexedPQ)
import qualified Janus.Data.IndexedPQ as IPQ
import Janus.Data.Layout
import Janus.Data.PriorityQueue (PriorityQueue)
import qualified Janus.Data.PriorityQueue as PQ
import qualified Janus.Data.SmallStack as SST
import Janus.Data.Stack (Stack)
import qualified Janus.Data.Stack as ST
import Janus.Expression.Ord (ExpOrd (..))
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getStackHeapTestConfig :: IO CBuildConfig
getStackHeapTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

listToStreamHost :: [a] -> Stream IO a
listToStreamHost xs = do
  let n = fromIntegral (length xs) :: Int64
      arr = Map.fromAscList (zip [0 ..] xs)
  (\(Identity i) -> arr Map.! i) <$> rangeS 0 (Identity n)

--------------------------------------------------------------------------------
-- 1. Stack & SmallStack Stateful Trace + Frame Rewind + Dynamic Worklist `popAllS`
--------------------------------------------------------------------------------

data StackCmd
  = SPush !Int64
  | SPop
  | SPeek
  | SRewind !Int64
  | SClear
  deriving stock (Show)

genStackCmd :: Gen StackCmd
genStackCmd =
  Gen.frequency
    [ (5, SPush <$> Gen.int64 (Range.linear (-500) 500)),
      (3, pure SPop),
      (2, pure SPeek),
      (2, SRewind <$> Gen.int64 (Range.linear 0 12)),
      (1, pure SClear)
    ]

check_stack_and_smallstack :: Property
check_stack_and_smallstack = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 160) genStackCmd
  inlineVals <- forAll $ Gen.list (Range.linear 0 8) (Gen.int64 (Range.linear (-100) 100))
  treeRoot <- forAll $ Gen.int64 (Range.linear 1 25)

  (stTop, sstTop, stBot, refModel, inlineAllocs, workSum, refWorkSum, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc

      -- Verify zero allocator invocations when `withSmallStack @8` stays within inline capacity 8
      inlineOut <- SST.withSmallStack @8 @Int64 alloc $ \sstInline -> do
        forM_ inlineVals (SST.push alloc sstInline . Identity)
        Identity sp <- SST.isSpilled sstInline
        if sp then error "SmallStack spilled within inline capacity 8" else pure ()
        map runIdentity <$> collectStreamHost (SST.topDownStream sstInline)
      snapInline <- readAllocSnapshot tracker
      let allocsUsedInline = runIdentity (snapTotalAllocs snapInline)

      st <- ST.new @Int64 alloc
      sst <- SST.new @6 @Int64 alloc

      let step model cmd = case cmd of
            SPush x -> do
              ST.push alloc st (Identity x)
              SST.push alloc sst (Identity x)
              pure (x : model)
            SPop -> do
              (Identity ok1, Identity v1) <- ST.pop 0 st
              (Identity ok2, Identity v2) <- SST.pop 0 sst
              case model of
                [] ->
                  if not ok1 && not ok2
                    then pure []
                    else error "Stack pop succeeded on empty"
                (m : ms) ->
                  if ok1 && ok2 && v1 == m && v2 == m
                    then pure ms
                    else error "Stack pop mismatch"
            SPeek -> do
              (Identity ok1, Identity v1) <- ST.peekTop 0 st
              (Identity ok2, Identity v2) <- SST.peekTop 0 sst
              case model of
                [] ->
                  if not ok1 && not ok2
                    then pure []
                    else error "Stack peek succeeded on empty"
                (m : _) ->
                  if ok1 && ok2 && v1 == m && v2 == m
                    then pure model
                    else error "Stack peek mismatch"
            SRewind targetDepth -> do
              ST.rewindToDepth st (Identity targetDepth)
              SST.rewindToDepth sst (Identity targetDepth)
              let curLen = length model
                  clamped = max 0 (min curLen (fromIntegral targetDepth))
              pure (drop (curLen - clamped) model)
            SClear -> do
              ST.clear st
              SST.clear sst
              pure []

      modelEnd <- foldlM' step [] cmds
      top1 <- map runIdentity <$> collectStreamHost (ST.topDownStream st)
      top2 <- map runIdentity <$> collectStreamHost (SST.topDownStream sst)
      bot1 <- map runIdentity <$> collectStreamHost (ST.bottomUpStream st)

      -- Dynamic DFS worklist test on `popAllS`:
      -- Expand node `k > 1` into children `k `quot` 2` and `k `quot` 3`, summing all visited nodes!
      workSt <- SST.new @4 @Int64 alloc
      SST.push alloc workSt (Identity treeRoot)
      visitedRef <- newIORef (0 :: Int64)
      runDrain
        ( drainForEach $ \(Identity k) -> do
            modifyIORef' visitedRef (+ k)
            whenM_ (Identity k `gt` 1) $ do
              SST.push alloc workSt (Identity (k `quot` 2))
              SST.push alloc workSt (Identity (k `quot` 3))
        )
        (SST.popAllS workSt)
      wSum <- readIORef visitedRef
      let pureExpand k
            | k <= 0 = 0
            | k == 1 = 1
            | otherwise = k + pureExpand (k `quot` 2) + pureExpand (k `quot` 3)

      SST.free alloc workSt
      ST.free alloc st
      SST.free alloc sst
      snapEnd <- readAllocSnapshot tracker
      if inlineOut == reverse inlineVals
        then
          pure
            ( top1,
              top2,
              bot1,
              modelEnd,
              allocsUsedInline,
              wSum,
              pureExpand treeRoot,
              (runIdentity (snapLiveBlocks snapEnd), runIdentity (snapLiveBytes snapEnd))
            )
        else error "withSmallStack inline output mismatch"

  inlineAllocs === 0
  stTop === refModel
  sstTop === refModel
  stBot === reverse refModel
  workSum === refWorkSum
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 2. PriorityQueue & MinHeap (4-ary Heap + Floyd O(N) Build + SoA Payload)
--------------------------------------------------------------------------------

data JobPayload f = JobPayload
  { jpJobId :: !(f Int64),
    jpWeight :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (JobPayload Identity)

deriving stock instance Show (JobPayload Identity)

instance JanusStruct JobPayload

instance JanusHKD JobPayload

data PQCmd
  = PQPush !Int64 !Int64
  | PQPop
  | PQReplaceMin !Int64 !Int64
  | PQPushPop !Int64 !Int64
  | PQClear
  deriving stock (Show)

genPQCmd :: Gen PQCmd
genPQCmd =
  Gen.frequency
    [ (6, PQPush <$> Gen.int64 (Range.linear (-80) 80) <*> Gen.int64 (Range.linear 1 1000)),
      (3, pure PQPop),
      (2, PQReplaceMin <$> Gen.int64 (Range.linear (-80) 80) <*> Gen.int64 (Range.linear 1 1000)),
      (2, PQPushPop <$> Gen.int64 (Range.linear (-80) 80) <*> Gen.int64 (Range.linear 1 1000)),
      (1, pure PQClear)
    ]

check_priority_queue_and_minheap :: Property
check_priority_queue_and_minheap = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 180) genPQCmd
  bulkItems <-
    forAll $
      Gen.list
        (Range.linear 0 150)
        ((,) <$> Gen.int64 (Range.linear (-100) 100) <*> Gen.int64 (Range.linear 1 500))

  (poppedKeys, refSortedKeys, floydKeys, minHeapKeys, expectedBulkKeys, soaSortedKeys, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      pq <- PQ.new @Int64 @Int64 alloc

      let step model cmd = case cmd of
            PQPush k v -> do
              PQ.push alloc pq (Identity k) (Identity v)
              pure ((k, v) : model)
            PQPop -> do
              (Identity ok, Identity k, _) <- PQ.popMin 0 0 pq
              case sortBy (comparing fst) model of
                [] ->
                  if not ok then pure [] else error "PriorityQueue popMin hit on empty"
                ((mk, _) : _) ->
                  if ok && k == mk
                    then pure (deleteFirstKey k model)
                    else error "PriorityQueue popMin key mismatch"
            PQReplaceMin k v ->
              case sortBy (comparing fst) model of
                [] -> pure []
                ((mk, _) : _) -> do
                  (Identity oldK, _) <- PQ.replaceMin pq (Identity k) (Identity v)
                  if oldK == mk
                    then pure ((k, v) : deleteFirstKey oldK model)
                    else error "PriorityQueue replaceMin mismatch"
            PQPushPop k v -> do
              (Identity outK, _) <- PQ.pushPopMin pq (Identity k) (Identity v)
              let minK = minimum (k : map fst model)
              if outK == minK
                then pure (deleteFirstKey outK ((k, v) : model))
                else error "PriorityQueue pushPopMin mismatch"
            PQClear -> do
              PQ.clear pq
              pure []

      modelFinal <- foldlM' step [] cmds
      remPairs <- collectStreamHost (PQ.popAllS pq)
      let remKeys = map (runIdentity . fst) remPairs

      -- Test O(N) Floyd bottom-up construction on PriorityQueue, MinHeap, and SoA payloads!
      pqFloyd <-
        PQ.fromStream @Int64 @Int64
          alloc
          (listToStreamHost [(Identity k, Identity v) | (k, v) <- bulkItems])
      floydOut <- map (runIdentity . fst) <$> collectStreamHost (PQ.popAllS pqFloyd)

      mh <-
        PQ.minHeapFromStream @Int64
          alloc
          (listToStreamHost [Identity k | (k, _) <- bulkItems])
      mhOut <- map runIdentity <$> collectStreamHost (PQ.popAllMinHeapS mh)

      pqSoA <-
        PQ.fromStream @Int64 @(SoA JobPayload)
          alloc
          (listToStreamHost [(Identity k, JobPayload (Identity v) (Identity (k + v))) | (k, v) <- bulkItems])
      soaOut <- collectStreamHost (PQ.popAllS pqSoA)
      let soaKeys =
            [ if runIdentity (jpWeight p) == k + runIdentity (jpJobId p) then k else error "Corrupt SoA payload"
              | (Identity k, p) <- soaOut
            ]

      PQ.free alloc pqSoA
      PQ.freeMinHeap alloc mh
      PQ.free alloc pqFloyd
      PQ.free alloc pq
      snap <- readAllocSnapshot tracker
      pure
        ( remKeys,
          sort (map fst modelFinal),
          floydOut,
          mhOut,
          sort (map fst bulkItems),
          soaKeys,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  poppedKeys === refSortedKeys
  floydKeys === expectedBulkKeys
  minHeapKeys === expectedBulkKeys
  soaSortedKeys === expectedBulkKeys
  liveEnd === (0, 0)
  where
    deleteFirstKey _ [] = []
    deleteFirstKey target ((k, v) : rest)
      | k == target = rest
      | otherwise = (k, v) : deleteFirstKey target rest

    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 3. IndexedPQ Stateful Trace + Dijkstra Shortest Paths
--------------------------------------------------------------------------------

data IPQCmd
  = IPQUpsert !Int64 !Int64 !Int64
  | IPQDecrease !Int64 !Int64 !Int64
  | IPQDelete !Int64
  | IPQPopMin
  | IPQLookup !Int64
  | IPQClear
  deriving stock (Show)

genIPQCmd :: Gen IPQCmd
genIPQCmd =
  Gen.frequency
    [ (5, IPQUpsert <$> Gen.int64 (Range.linear 0 60) <*> Gen.int64 (Range.linear (-100) 100) <*> Gen.int64 (Range.linear 1 999)),
      (4, IPQDecrease <$> Gen.int64 (Range.linear 0 60) <*> Gen.int64 (Range.linear (-120) 100) <*> Gen.int64 (Range.linear 1 999)),
      (3, IPQDelete <$> Gen.int64 (Range.linear 0 70)),
      (3, pure IPQPopMin),
      (3, IPQLookup <$> Gen.int64 (Range.linear 0 70)),
      (1, pure IPQClear)
    ]

pureDijkstra :: Int -> [(Int64, Int64, Int64)] -> Map.Map Int64 Int64
pureDijkstra nVerts edges = go (Set.singleton 0) (Map.singleton 0 0)
  where
    adj =
      foldr
        (\(u, v, w) m -> Map.insertWith (++) u [(v, w)] m)
        Map.empty
        [(u, v, w) | (u, v, w) <- edges, u >= 0, u < fromIntegral nVerts, v >= 0, v < fromIntegral nVerts]
    go open dist
      | Set.null open = dist
      | otherwise =
          let u = minimumBy (comparing (\x -> (dist Map.! x, x))) (Set.toList open)
              du = dist Map.! u
              open' = Set.delete u open
              nbrs = Map.findWithDefault [] u adj
              (open'', dist'') =
                foldr
                  ( \(v, w) (oAcc, dAcc) ->
                      let cand = du + w
                       in case Map.lookup v dAcc of
                            Just old | cand >= old -> (oAcc, dAcc)
                            _ -> (Set.insert v oAcc, Map.insert v cand dAcc)
                  )
                  (open', dist)
                  nbrs
           in go open'' dist''

check_indexed_pq_and_dijkstra :: Property
check_indexed_pq_and_dijkstra = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 200) genIPQCmd
  nVerts <- forAll $ Gen.int (Range.linear 2 24)
  edges <-
    forAll $
      Gen.list
        (Range.linear 0 80)
        ( (,,)
            <$> Gen.int64 (Range.linear 0 (fromIntegral (nVerts - 1)))
            <*> Gen.int64 (Range.linear 0 (fromIntegral (nVerts - 1)))
            <*> Gen.int64 (Range.linear 1 50)
        )

  (poppedPrios, refPrios, dijkstraOut, refDijkstra, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      ipq <- IPQ.new @Int64 @Int64 alloc

      let step m cmd = case cmd of
            IPQUpsert i p v -> do
              IPQ.insertOrUpdate alloc ipq (Identity i) (Identity p) (Identity v)
              pure (Map.insert i (p, v) m)
            IPQDecrease i p v -> do
              Identity relaxed <- IPQ.decreasePriority alloc ipq (Identity i) (Identity p) (Identity v)
              case Map.lookup i m of
                Nothing ->
                  if relaxed then pure (Map.insert i (p, v) m) else error "IndexedPQ failed to insert on decreasePriority"
                Just (oldP, _) ->
                  if p < oldP
                    then if relaxed then pure (Map.insert i (p, v) m) else error "IndexedPQ missed decreasePriority"
                    else if not relaxed then pure m else error "IndexedPQ relaxed when p >= oldP"
            IPQDelete i -> do
              Identity r <- IPQ.deleteById ipq (Identity i)
              if r == Map.member i m
                then pure (Map.delete i m)
                else error "IndexedPQ deleteById mismatch"
            IPQPopMin -> do
              (Identity ok, Identity outId, Identity outP, _) <- IPQ.popMin 0 0 ipq
              if Map.null m
                then if not ok then pure m else error "IndexedPQ popMin hit on empty"
                else do
                  let minP = minimum (map fst (Map.elems m))
                  case Map.lookup outId m of
                    Just (mp, _) | ok && outP == minP && mp == minP -> pure (Map.delete outId m)
                    _ -> error "IndexedPQ popMin priority mismatch"
            IPQLookup i -> do
              (Identity ok, Identity p, Identity v) <- IPQ.lookup 0 0 ipq (Identity i)
              case Map.lookup i m of
                Nothing -> if not ok then pure m else error "IndexedPQ false lookup hit"
                Just (mp, mv) -> if ok && p == mp && v == mv then pure m else error "IndexedPQ lookup value mismatch"
            IPQClear -> do
              IPQ.clear ipq
              pure Map.empty

      mEnd <- foldlM' step Map.empty cmds
      remTriples <- collectStreamHost (IPQ.popAllS ipq)
      let remPrios = [p | (_, Identity p, _) <- remTriples]

      -- Run Dijkstra using `IndexedPQ` + `decreasePriority`!
      let adj =
            foldr
              (\(u, v, w) acc -> Map.insertWith (++) u [(v, w)] acc)
              Map.empty
              edges
      dpq <- IPQ.withUniverse @Int64 @Int64 alloc (Identity (fromIntegral nVerts))
      distRef <- newIORef (Map.singleton 0 0 :: Map.Map Int64 Int64)
      _ <- IPQ.decreasePriority alloc dpq 0 0 (-1)
      runDrain
        ( drainForEach $ \(Identity u, Identity du, _) -> do
            let nbrs = Map.findWithDefault [] u adj
            forM_ nbrs $ \(v, w) -> do
              curMap <- readIORef distRef
              let cand = du + w
              case Map.lookup v curMap of
                Just old | cand >= old -> pure ()
                _ -> do
                  modifyIORef' distRef (Map.insert v cand)
                  _ <- IPQ.decreasePriority alloc dpq (Identity v) (Identity cand) (Identity u)
                  pure ()
        )
        (IPQ.popAllS dpq)
      dMap <- readIORef distRef

      IPQ.free alloc dpq
      IPQ.free alloc ipq
      snap <- readAllocSnapshot tracker
      pure
        ( remPrios,
          sort (map fst (Map.elems mEnd)),
          dMap,
          pureDijkstra nVerts edges,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  poppedPrios === refPrios
  dijkstraOut === refDijkstra
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 4. Host <-> Compiled C FFI Round-Trip (Stack, PriorityQueue, IndexedPQ)
--------------------------------------------------------------------------------

cMutateStackAndHeaps ::
  Stack LinuxX86_64CCSE Int64 ->
  PriorityQueue LinuxX86_64CCSE Int64 Int64 ->
  IndexedPQ LinuxX86_64CCSE Int64 Int64 ->
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMutateStackAndHeaps st pq ipq n = do
  runDrain
    ( drainForEach $ \i -> do
        ST.push dynamicAlloc st (i + 10)
        PQ.push dynamicAlloc pq (100 - i) (i * 2)
        _ <- IPQ.decreasePriority dynamicAlloc ipq i (200 - i * 3) i
        pure ()
    )
    (rangeS 0 n)
  s1 <- runDrain drainSum (ST.topDownStream st)
  s2 <- runDrain drainSum (fst <$> PQ.unorderedStream pq)
  (_, _, minP, _) <- IPQ.peekMin 0 0 ipq
  pure (s1 + s2 + minP)

check_stack_heap_c_ffi ::
  IO (Stack Identity Int64 -> PriorityQueue Identity Int64 Int64 -> IndexedPQ Identity Int64 Int64 -> Int64 -> IO Int64) ->
  Property
check_stack_heap_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  n <- forAll $ Gen.int64 (Range.linear 1 35)
  (cTotal, stOut, pqOut, ipqOut) <- liftIO $ do
    st <- ST.new @Int64 dynamicAlloc
    pq <- PQ.new @Int64 @Int64 dynamicAlloc
    ipq <- IPQ.new @Int64 @Int64 dynamicAlloc
    ST.push dynamicAlloc st 99
    PQ.push dynamicAlloc pq 50 1

    res <- cFn st pq ipq n
    stList <- map runIdentity <$> collectStreamHost (ST.topDownStream st)
    pqList <- map (runIdentity . fst) <$> collectStreamHost (PQ.popAllS pq)
    ipqPopped <- collectStreamHost (IPQ.popAllS ipq)
    let ipqPrios = [p | (_, Identity p, _) <- ipqPopped]
    ST.free dynamicAlloc st
    PQ.free dynamicAlloc pq
    IPQ.free dynamicAlloc ipq
    pure (res, stList, pqList, ipqPrios)

  let expSt = reverse (99 : [i + 10 | i <- [0 .. n - 1]])
      expPQ = sort (50 : [100 - i | i <- [0 .. n - 1]])
      expIPQ = sort [200 - i * 3 | i <- [0 .. n - 1]]
  case expIPQ of
    [] -> assert False
    (minIPQ : _) -> do
      stOut === expSt
      pqOut === expPQ
      ipqOut === expIPQ
      cTotal === sum expSt + sum expPQ + minIPQ

test_stack_and_heap :: TestTree
test_stack_and_heap =
  let acq = getStackHeapTestConfig >>= flip acquireJanusC cMutateStackAndHeaps
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "Stacks & 4-ary Cache-Line Priority Queues (Stack, SmallStack, PriorityQueue, MinHeap, IndexedPQ)"
          [ testProperty "Stack & SmallStack stateful trace, zero-alloc inline fast path, rewind, & worklist popAllS" check_stack_and_smallstack,
            testProperty "PriorityQueue & MinHeap 4-ary heap, Floyd O(N) build, replaceMin, & SoA payloads" check_priority_queue_and_minheap,
            testProperty "IndexedPQ stateful trace + Dijkstra shortest paths" check_indexed_pq_and_dijkstra,
            testProperty "Host <-> Compiled C FFI round-trip" (check_stack_heap_c_ffi (fst <$> rFn))
          ]
