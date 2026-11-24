{-# LANGUAGE DataKinds #-}
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

module Test.Janus.Data.IndexedAndIntrusive where

import Control.Lens ((&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr, nullPtr)
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM)
import Janus.Command.Memory (CmdStorable (peek))
import Janus.Command.Range (rangeM)
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream)
import Janus.Data
import qualified Janus.Data.IntrusiveList as IL
import qualified Janus.Data.Pool as PL
import qualified Janus.Data.SlotMap as SM
import qualified Janus.Data.SparseSet as SPS
import qualified Janus.Data.SparseVec as SPV
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getIdxTestConfig :: IO CBuildConfig
getIdxTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

--------------------------------------------------------------------------------
-- 1. SlotMap Generational Stale-Handle Rejection + Packed Dense SPMD
--------------------------------------------------------------------------------

data SlotCmd
  = SInsert !Int64
  | SDeleteLive !Int
  | SCheckStale
  | SModifyLive !Int !Int64
  | SClear
  deriving (Eq, Show)

check_slotmap_generational_safety :: Property
check_slotmap_generational_safety = property $ do
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 180)
        ( Gen.frequency
            [ (6, SInsert <$> Gen.int64 (Range.linear (-500) 500)),
              (4, SDeleteLive <$> Gen.int (Range.linear 0 100)),
              (3, pure SCheckStale),
              (3, SModifyLive <$> Gen.int (Range.linear 0 100) <*> Gen.int64 (Range.linear 1 50)),
              (1, pure SClear)
            ]
        )

  (denseStreamVals, refSortedVals, staleAllRejected, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      sm <- SM.new @Int64 alloc
      let step (liveMap, staleKeys) cmd = case cmd of
            SInsert v -> do
              k <- SM.insert alloc sm (Identity v)
              pure (Map.insert k v liveMap, staleKeys)
            SDeleteLive idx
              | Map.null liveMap -> pure (liveMap, staleKeys)
              | otherwise -> do
                  let (k, _) = Map.elemAt (idx `mod` Map.size liveMap) liveMap
                  Identity ok <- SM.delete sm k
                  -- Deleting the same key a SECOND time immediately must return False!
                  Identity ok2 <- SM.delete sm k
                  if ok && not ok2
                    then pure (Map.delete k liveMap, k : staleKeys)
                    else error "SlotMap delete generation check failed"
            SCheckStale -> do
              -- Every single retired key (even after its slot index was recycled!) MUST fail lookup & containsKey!
              forM_ staleKeys $ \sk -> do
                Identity has <- SM.containsKey sm sk
                (Identity lOk, _) <- SM.lookup 0 sm sk
                when (has || lOk) $
                  error "Stale SlotKey accepted after slot reuse!"
              pure (liveMap, staleKeys)
            SModifyLive idx delta
              | Map.null liveMap -> pure (liveMap, staleKeys)
              | otherwise -> do
                  let (k, oldV) = Map.elemAt (idx `mod` Map.size liveMap) liveMap
                  Identity ok <- SM.modifyKey sm k (+ Identity delta)
                  if ok
                    then pure (Map.insert k (oldV + delta) liveMap, staleKeys)
                    else error "SlotMap modifyKey failed on live key"
            SClear -> do
              let retired = Map.keys liveMap ++ staleKeys
              SM.clear sm
              pure (Map.empty, retired)

      (liveEndMap, allStale) <- foldlM' step (Map.empty, []) cmds
      staleCheck <-
        and
          <$> mapM
            ( \sk -> do
                Identity has <- SM.containsKey sm sk
                pure (not has)
            )
            allStale

      -- Run 8-lane SPMD +10 over packed dense values
      SM.foreachScalarSPMD (Proxy @8) sm (\_ x -> pure (x + 10))
      vals <- sort . map runIdentity <$> collectStreamHost (SM.valuesS sm)
      SM.free alloc sm
      snap <- readAllocSnapshot tracker
      pure
        ( vals,
          sort (map (+ 10) (Map.elems liveEndMap)),
          staleCheck,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  assert staleAllRejected
  denseStreamVals === refSortedVals
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 2. Briggs-Torczon SparseSet & SparseVec (`clear`, `sparseDot`, `sparseAxpy`)
--------------------------------------------------------------------------------

check_sparseset_and_sparsevec :: Property
check_sparseset_and_sparsevec = property $ do
  setOps <-
    forAll $
      Gen.list
        (Range.linear 1 160)
        ( (,)
            <$> Gen.int (Range.linear 0 3) -- 0=ins, 1=del, 2=member, 3=O(1) clear
            <*> Gen.int64 (Range.linear 0 250)
        )
  vecA <-
    forAll $
      Gen.list
        (Range.linear 0 50)
        ((,) <$> Gen.int64 (Range.linear 0 120) <*> Gen.int64 (Range.linear (-40) 40))
  vecB <-
    forAll $
      Gen.list
        (Range.linear 0 50)
        ((,) <$> Gen.int64 (Range.linear 0 120) <*> Gen.int64 (Range.linear (-40) 40))
  alpha <- forAll $ Gen.int64 (Range.linear (-5) 5)

  (ssElems, refSetElems, dotGot, axpyPairs, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      ss <- SPS.withUniverse alloc 32
      let step s (op, k) = case op of
            0 -> do
              Identity ins <- SPS.insert alloc ss (Identity k)
              if ins == not (IntSet.member (fromIntegral k) s)
                then pure (IntSet.insert (fromIntegral k) s)
                else error "SparseSet insert mismatch"
            1 -> do
              Identity remOk <- SPS.delete ss (Identity k)
              if remOk == IntSet.member (fromIntegral k) s
                then pure (IntSet.delete (fromIntegral k) s)
                else error "SparseSet delete mismatch"
            2 -> do
              Identity m <- SPS.member ss (Identity k)
              if m == IntSet.member (fromIntegral k) s
                then pure s
                else error "SparseSet member mismatch"
            _ -> SPS.clear ss >> pure IntSet.empty

      sEnd <- foldlM' step IntSet.empty setOps
      ssList <- sort . map runIdentity <$> collectStreamHost (SPS.stream ss)

      -- SparseVec dot & axpy
      vx <- SPV.new @Int64 alloc
      vy <- SPV.new @Int64 alloc
      forM_ vecA $ \(idx, v) -> SPV.insert alloc vx (Identity idx) (Identity v)
      forM_ vecB $ \(idx, v) -> SPV.insert alloc vy (Identity idx) (Identity v)

      Identity dVal <- SPV.sparseDot vx vy
      SPV.sparseAxpy alloc (Identity alpha) vx vy
      yList <- sort . map (\(Identity i, Identity v) -> (fromIntegral i :: Int, v)) <$> collectStreamHost (SPV.toStream vy)

      SPV.free alloc vx
      SPV.free alloc vy
      SPS.free alloc ss
      snap <- readAllocSnapshot tracker
      pure
        ( map fromIntegral ssList,
          IntSet.toAscList sEnd,
          dVal,
          yList,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  let mA = IntMap.fromList [(fromIntegral i, v) | (i, v) <- vecA]
      mB = IntMap.fromList [(fromIntegral i, v) | (i, v) <- vecB]
      expectedDot = sum (IntMap.elems (IntMap.intersectionWith (*) mA mB))
      expectedAxpy = IntMap.toAscList (IntMap.unionWith (+) mB (IntMap.map (* alpha) mA))

  ssElems === refSetElems
  dotGot === expectedDot
  axpyPairs === expectedAxpy
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 3. Segmented Object Pool Address Stability & Multi-Word Occupancy Bitmask Stream
--------------------------------------------------------------------------------

check_pool_address_stability_and_bitmask_stream :: Property
check_pool_address_stability_and_bitmask_stream = property $ do
  -- > 150 elements spans 3 x 64-bit occupancy bitmask segments!
  vals <- forAll $ Gen.list (Range.linear 10 160) (Gen.int64 (Range.linear 1 10000))
  dropIndices <- forAll $ Gen.list (Range.linear 0 60) (Gen.int (Range.linear 0 200))

  (streamedLive, expectedLive, ptrStabilityOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      pool <- PL.new @Int64 alloc
      handlesAndPtrs <-
        mapM
          ( \v -> do
              h <- PL.acquire alloc pool (Identity v)
              Identity p <- PL.handlePtrScalar pool h
              pure (h, p :: Ptr Int64, v)
          )
          vals

      -- Release a random subset of handles (creating holes across 64-bit bitmask boundaries!)
      let step m idx
            | Map.null m = pure m
            | otherwise = do
                let (h, _) = Map.elemAt (idx `mod` Map.size m) m
                Identity ok <- PL.release pool h
                if ok then pure (Map.delete h m) else error "Pool release failed"

      let initMap = Map.fromList [(h, (p, v)) | (h, p, v) <- handlesAndPtrs]
      survivingMap <- foldlM' step initMap dropIndices

      -- Acquire 40 more items (recycling freed slots AND potentially allocating new segments)
      extraHandles <- mapM (\i -> PL.acquire alloc pool (Identity (50000 + i))) [1 .. 40 :: Int64]

      -- Verify every surviving original handle's SAVED raw pointer still holds its exact value!
      ptrsOk <-
        and
          <$> mapM
            ( \(h, (savedP, expectedV)) -> do
                Identity curP <- PL.handlePtrScalar pool h
                peeked <- Storable.peek savedP
                pure (curP == savedP && peeked == expectedV)
            )
            (Map.toList survivingMap)

      liveOut <- sort . map runIdentity <$> collectStreamHost (PL.stream pool)
      let expectedAll = sort ([v | (_, v) <- Map.elems survivingMap] ++ [50000 + i | i <- [1 .. 40]])
          _ = extraHandles

      PL.free alloc pool
      snap <- readAllocSnapshot tracker
      pure (liveOut, expectedAll, ptrsOk, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  assert ptrStabilityOk
  streamedLive === expectedLive
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 4. Multi-Hook IntrusiveList (`container_of`, Mid-Stream Unlink, & O(1) Splice)
--------------------------------------------------------------------------------

data DualNode f = DualNode
  { dnPayload :: !(f Int64),
    dnHookA :: !(f (CStruct RawListHook)),
    dnHookB :: !(f (CStruct RawListHook))
  }
  deriving stock (Generic)

instance JanusStruct DualNode

hookSpecA :: HookSpec DualNode
hookSpecA = mkHookSpec dnHookA

hookSpecB :: HookSpec DualNode
hookSpecB = mkHookSpec dnHookB

check_intrusive_list_multihook_splice_and_unlink :: Property
check_intrusive_list_multihook_splice_and_unlink = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 60) (Gen.int64 (Range.linear (-200) 200))
  ys <- forAll $ Gen.list (Range.linear 0 40) (Gen.int64 (Range.linear (-200) 200))

  (listABefore, listBBefore, listAAfterEvensRemoved, listBAfterUnchanged, splicedOut, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let root = statsAlloc tracker dynamicAlloc
      -- Back node allocations with an AllocPool (16B alignment) to verify composable allocator integration!
      mp <- newPoolAligned root 64 32 16
      let nodeAlloc = poolAlloc mp root
          zeroHook = CStruct (RawListHook (Identity nullPtr) (Identity nullPtr))

      listA <- IL.new root hookSpecA
      listB <- IL.new root hookSpecB

      -- Allocate each node ONCE, linking it onto back of listA AND front of listB simultaneously!
      forM_ xs $ \x -> do
        pNode <- IL.allocNode nodeAlloc (DualNode (Identity x) (Identity zeroHook) (Identity zeroHook))
        IL.initNodeHook listA pNode
        IL.initNodeHook listB pNode
        IL.pushBackNode listA pNode
        IL.pushFrontNode listB pNode

      let readPayloads lst =
            mapM (\p -> runIdentity <$> peek (getPtrOf (dnPayload (projectPtrs p)))) =<< collectStreamHost (IL.nodePtrStream lst)

      a0 <- readPayloads listA
      b0 <- readPayloads listB

      -- Mid-stream `unlinkNode` from listA ONLY for even payloads (must NOT disturb listB or stream cursor!)
      runDrain
        ( drainForEach $ \pNode -> do
            Identity v <- peek (getPtrOf (dnPayload (projectPtrs pNode)))
            when (even v) $
              IL.unlinkNode listA pNode
        )
        (IL.nodePtrStream listA)

      a1 <- readPayloads listA
      b1 <- readPayloads listB

      -- Test O(1) zero-allocation `spliceBack` from a third listC (using hookA) onto listA
      listC <- IL.new root hookSpecA
      extraPtrs <-
        mapM
          ( \y -> do
              pNode <- IL.allocNode nodeAlloc (DualNode (Identity y) (Identity zeroHook) (Identity zeroHook))
              IL.initNodeHook listC pNode
              IL.pushBackNode listC pNode
              pure pNode
          )
          ys
      IL.spliceBack listA listC
      aSpliced <- readPayloads listA

      -- Free all nodes (via listB for initial nodes + extraPtrs for ys) and spines
      runDrain (drainForEach (IL.freeNode nodeAlloc)) (IL.nodePtrStream listB)
      forM_ extraPtrs (IL.freeNode nodeAlloc)
      IL.freeListSpine root listC
      IL.freeListSpine root listB
      IL.freeListSpine root listA
      destroyPool root mp
      snap <- readAllocSnapshot tracker
      pure (a0, b0, a1, b1, aSpliced, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  listABefore === xs
  listBBefore === reverse xs
  listAAfterEvensRemoved === filter odd xs
  listBAfterUnchanged === reverse xs
  splicedOut === (filter odd xs ++ ys)
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 5. Host <-> Compiled C FFI Round-Trip (`SlotMap`, `SparseSet`, `Pool`, `IntrusiveList`)
--------------------------------------------------------------------------------

cMutateIndexedAndIntrusive ::
  SlotMap LinuxX86_64C Int64 ->
  SparseSet LinuxX86_64C ->
  Pool LinuxX86_64C Int64 ->
  IntrusiveList DualNode LinuxX86_64C ->
  LinuxX86_64C Int64 ->
  LinuxX86_64CM (LinuxX86_64C Int64)
cMutateIndexedAndIntrusive sm sps pool ilst n = do
  rangeM 0 n $ \i -> do
    _ <- SM.insert dynamicAlloc sm (i * 5)
    _ <- SPS.insert dynamicAlloc sps (i * 3)
    _ <- PL.acquire dynamicAlloc pool (i * 7)
    pure ()
  s1 <- runDrain drainSum (SM.valuesS sm)
  s2 <- runDrain drainSum (SPS.stream sps)
  s3 <- runDrain drainSum (PL.stream pool)
  s4 <- runDrain drainSum (dnPayload <$> IL.elemStream ilst)
  pure (s1 + s2 + s3 + s4)

check_indexed_intrusive_host_c_ffi ::
  IO
    ( SlotMap Identity Int64 ->
      SparseSet Identity ->
      Pool Identity Int64 ->
      IntrusiveList DualNode Identity ->
      Int64 ->
      IO Int64
    ) ->
  Property
check_indexed_intrusive_host_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  n <- forAll $ Gen.int64 (Range.linear 0 50)
  listVals <- forAll $ Gen.list (Range.linear 0 25) (Gen.int64 (Range.linear (-50) 50))

  (cTot, smLen, spsLen, poolLen) <- liftIO $ do
    sm <- SM.new @Int64 dynamicAlloc
    sps <- SPS.new dynamicAlloc
    pool <- PL.new @Int64 dynamicAlloc
    let zeroHook = CStruct (RawListHook (Identity nullPtr) (Identity nullPtr))
    ilst <- IL.new dynamicAlloc hookSpecA
    forM_ listVals $ \v ->
      IL.allocAndPushBack dynamicAlloc ilst (DualNode (Identity v) (Identity zeroHook) (Identity zeroHook))

    tot <- cFn sm sps pool ilst n
    Identity l1 <- SM.size sm
    Identity l2 <- SPS.size sps
    Identity l3 <- PL.size pool

    IL.freeAllNodesAndSpine dynamicAlloc ilst
    PL.free dynamicAlloc pool
    SPS.free dynamicAlloc sps
    SM.free dynamicAlloc sm
    pure (tot, l1, l2, l3)

  smLen === n
  spsLen === n
  poolLen === n
  let expected =
        sum [i * 5 | i <- [0 .. n - 1]]
          + sum [i * 3 | i <- [0 .. n - 1]]
          + sum [i * 7 | i <- [0 .. n - 1]]
          + sum listVals
  cTot === expected

test_indexed_and_intrusive_containers :: TestTree
test_indexed_and_intrusive_containers =
  let acq = getIdxTestConfig >>= flip acquireJanusC cMutateIndexedAndIntrusive
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "SlotMap, SparseSet, SparseVec, Pool, & IntrusiveList"
          [ testProperty "SlotMap generational stale-key rejection & SPMD over denseVals" check_slotmap_generational_safety,
            testProperty "SparseSet & SparseVec O(1) clear, sparseDot, & sparseAxpy" check_sparseset_and_sparsevec,
            testProperty "Pool address stability & multi-word 64-bit occupancy bitmask stream" check_pool_address_stability_and_bitmask_stream,
            testProperty "IntrusiveList multi-hook simultaneous membership, mid-stream unlink, & O(1) splice" check_intrusive_list_multihook_splice_and_unlink,
            testProperty "Host <-> Compiled C SlotMap, SparseSet, Pool, & IntrusiveList FFI" (check_indexed_intrusive_host_c_ffi (fst <$> rFn))
          ]
