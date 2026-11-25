{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.Ordered where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Command.Range (rangeM)
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream)
import Janus.Data
import qualified Janus.Data.BTreeMap as BTM
import qualified Janus.Data.BTreeSet as BTS
import qualified Janus.Data.FlatMap as FM
import qualified Janus.Data.FlatSet as FS
import Janus.Fix (fun)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getOrdTestConfig :: IO CBuildConfig
getOrdTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

listToStreamHost :: [a] -> Stream IO a
listToStreamHost xs0 = asum (map pure xs0)

data OrdCmd
  = OInsert !Int64 !Int64
  | ODelete !Int64
  | OLookup !Int64
  | OLookupGE !Int64
  | OLookupLE !Int64
  | ORange !Int64 !Int64
  | OClear
  deriving (Eq, Show)

genOrdCmd :: Int64 -> Gen OrdCmd
genOrdCmd bound =
  Gen.frequency
    [ (8, OInsert <$> Gen.int64 (Range.linear (-bound) bound) <*> Gen.int64 (Range.linear (-999) 999)),
      (5, ODelete <$> Gen.int64 (Range.linear (-bound) bound)),
      (3, OLookup <$> Gen.int64 (Range.linear (-bound) bound)),
      (3, OLookupGE <$> Gen.int64 (Range.linear (-bound) bound)),
      (3, OLookupLE <$> Gen.int64 (Range.linear (-bound) bound)),
      (2, ORange <$> Gen.int64 (Range.linear (-bound) bound) <*> Gen.int64 (Range.linear (-bound) bound)),
      (1, pure OClear)
    ]

--------------------------------------------------------------------------------
-- 1. FlatMap & FlatSet Differential Traces, Bulk Sort-Deduplicate Drains, & Merges
--------------------------------------------------------------------------------

check_flatmap_and_flatset :: Property
check_flatmap_and_flatset = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 150) (genOrdCmd 40)
  bulkPairs <-
    forAll $
      Gen.list
        (Range.linear 0 80)
        ((,) <$> Gen.int64 (Range.linear (-25) 25) <*> Gen.int64 (Range.linear (-500) 500))

  (fmAsc, fmDesc, refMapAsc, bulkOut, unionOut, interOut, diffOut, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      fm <- FM.new @Int64 @Int64 alloc
      let step m cmd = case cmd of
            OInsert k v -> FM.insert alloc fm (Identity k) (Identity v) >> pure (Map.insert k v m)
            ODelete k -> do
              Identity r <- FM.delete fm (Identity k)
              if r == Map.member k m then pure (Map.delete k m) else error "FlatMap delete mismatch"
            OLookup k -> do
              (Identity ok, Identity v) <- FM.lookup 0 fm (Identity k)
              case Map.lookup k m of
                Nothing -> if not ok then pure m else error "FlatMap false hit"
                Just mv -> if ok && v == mv then pure m else error "FlatMap lookup mismatch"
            OLookupGE k -> do
              (Identity ok, Identity rk, Identity rv) <- FM.lookupGE 0 0 fm (Identity k)
              case Map.lookupGE k m of
                Nothing -> if not ok then pure m else error "FlatMap GE false hit"
                Just (mk, mv) -> if ok && rk == mk && rv == mv then pure m else error "FlatMap GE mismatch"
            OLookupLE k -> do
              (Identity ok, Identity rk, Identity rv) <- FM.lookupLE 0 0 fm (Identity k)
              case Map.lookupLE k m of
                Nothing -> if not ok then pure m else error "FlatMap LE false hit"
                Just (mk, mv) -> if ok && rk == mk && rv == mv then pure m else error "FlatMap LE mismatch"
            ORange lo hi -> do
              rng <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (FM.rangeStream fm (Identity lo) (Identity hi))
              let expected = [(k, v) | (k, v) <- Map.toAscList m, k >= lo && k <= hi]
              if rng == expected then pure m else error "FlatMap rangeStream mismatch"
            OClear -> FM.clear fm >> pure Map.empty

      mEnd <- foldlM' step Map.empty cmds
      asc <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (FM.toStream fm)
      desc <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (FM.reverseStream fm)

      -- Bulk construction via `drainToFlatMap` (O(N log N) sort + last-wins deduplication)
      fmBulk <-
        FM.fromStream @Int64 @Int64
          alloc
          (listToStreamHost [(Identity k, Identity v) | (k, v) <- bulkPairs])
      bOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (FM.toStream fmBulk)

      -- Fused linear-scan set-algebra streams over FlatSet
      fsA <- FS.fromStream @Int64 alloc (listToStreamHost [Identity k | (k, _) <- asc])
      fsB <- FS.fromStream @Int64 alloc (listToStreamHost [Identity k | (k, _) <- bulkPairs])
      uList <- map runIdentity <$> collectStreamHost (FS.unionS fsA fsB)
      iList <- map runIdentity <$> collectStreamHost (FS.intersectionS fsA fsB)
      dList <- map runIdentity <$> collectStreamHost (FS.differenceS fsA fsB)

      FS.free alloc fsA
      FS.free alloc fsB
      FM.free alloc fmBulk
      FM.free alloc fm
      snap <- readAllocSnapshot tracker
      pure (asc, desc, Map.toAscList mEnd, bOut, uList, iList, dList, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  let refBulk = Map.fromList bulkPairs
      setA = Map.keysSet (Map.fromAscList refMapAsc)
      setB = Map.keysSet refBulk
  fmAsc === refMapAsc
  fmDesc === reverse refMapAsc
  bulkOut === Map.toAscList refBulk
  unionOut === Set.toAscList (Set.union setA setB)
  interOut === Set.toAscList (Set.intersection setA setB)
  diffOut === Set.toAscList (Set.difference setA setB)
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 2. BTreeMap & BTreeSet Multi-Level Splits/Borrows/Merges + Per-Step Invariant Check
--------------------------------------------------------------------------------

check_btreemap_invariants_and_operations :: Property
check_btreemap_invariants_and_operations = property $ do
  -- 280 operations over [-90 .. 90] forces height 1 -> 2 -> 3 splits and 3 -> 2 -> 1 -> 0 merges!
  cmds <- forAll $ Gen.list (Range.linear 1 280) (genOrdCmd 90)
  -- Also drain down to empty at the end half the time to exercise full root collapse!
  drainAllAtEnd <- forAll Gen.bool

  (ascOut, descOut, refAsc, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      bm <- BTM.new @Int64 @Int64 alloc
      let verifyStep tag = do
            ok <- BTM.checkInvariants bm
            if ok then pure () else error ("BTreeMap invariant violated after " ++ tag)

          step m cmd = case cmd of
            OInsert k v -> do
              BTM.insert alloc bm (Identity k) (Identity v)
              verifyStep "insert"
              pure (Map.insert k v m)
            ODelete k -> do
              Identity r <- BTM.delete alloc bm (Identity k)
              verifyStep "delete"
              if r == Map.member k m then pure (Map.delete k m) else error "BTreeMap delete mismatch"
            OLookup k -> do
              (Identity ok, Identity v) <- BTM.lookup 0 bm (Identity k)
              case Map.lookup k m of
                Nothing -> if not ok then pure m else error "BTreeMap false hit"
                Just mv -> if ok && v == mv then pure m else error "BTreeMap lookup mismatch"
            OLookupGE k -> do
              (Identity ok, Identity rk, Identity rv) <- BTM.lookupGE 0 0 bm (Identity k)
              case Map.lookupGE k m of
                Nothing -> if not ok then pure m else error "BTreeMap GE false hit"
                Just (mk, mv) -> if ok && rk == mk && rv == mv then pure m else error "BTreeMap GE mismatch"
            OLookupLE k -> do
              (Identity ok, Identity rk, Identity rv) <- BTM.lookupLE 0 0 bm (Identity k)
              case Map.lookupLE k m of
                Nothing -> if not ok then pure m else error "BTreeMap LE false hit"
                Just (mk, mv) -> if ok && rk == mk && rv == mv then pure m else error "BTreeMap LE mismatch"
            ORange lo hi -> do
              rng <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (BTM.rangeStream bm (Identity lo) (Identity hi))
              let expected = [(k, v) | (k, v) <- Map.toAscList m, k >= lo && k <= hi]
              if rng == expected then pure m else error "BTreeMap rangeStream mismatch"
            OClear -> do
              BTM.clear alloc bm
              verifyStep "clear"
              pure Map.empty

      mMid <- foldlM' step Map.empty cmds
      mFinal <-
        if drainAllAtEnd
          then
            foldlM'
              ( \acc k -> do
                  Identity r <- BTM.delete alloc bm (Identity k)
                  verifyStep "final-delete"
                  if r then pure (Map.delete k acc) else error "Expected key during drain-to-empty"
              )
              mMid
              (Map.keys mMid)
          else pure mMid

      asc <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (BTM.toStream bm)
      desc <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (BTM.reverseStream bm)
      BTM.free alloc bm
      snap <- readAllocSnapshot tracker
      pure (asc, desc, Map.toAscList mFinal, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  ascOut === refAsc
  descOut === reverse refAsc
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

check_btreeset_invariants_and_set_algebra :: Property
check_btreeset_invariants_and_set_algebra = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 240) (genOrdCmd 80)
  extraB <- forAll $ Gen.list (Range.linear 0 70) (Gen.int64 (Range.linear (-80) 80))

  (ascA, descA, refA, uOut, iOut, dOut, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      bs <- BTS.new @Int64 alloc
      let step s cmd = case cmd of
            OInsert k _ -> do
              Identity ins <- BTS.insert alloc bs (Identity k)
              invOk <- BTS.checkInvariants bs
              if invOk && ins == not (Set.member k s)
                then pure (Set.insert k s)
                else error "BTreeSet insert/invariant failure"
            ODelete k -> do
              Identity remOk <- BTS.delete alloc bs (Identity k)
              invOk <- BTS.checkInvariants bs
              if invOk && remOk == Set.member k s
                then pure (Set.delete k s)
                else error "BTreeSet delete/invariant failure"
            OLookup k -> do
              Identity m <- BTS.member bs (Identity k)
              if m == Set.member k s then pure s else error "BTreeSet member mismatch"
            OLookupGE k -> do
              (Identity ok, Identity rk) <- BTS.lookupGE 0 bs (Identity k)
              case Set.lookupGE k s of
                Nothing -> if not ok then pure s else error "BTreeSet GE false hit"
                Just mk -> if ok && rk == mk then pure s else error "BTreeSet GE mismatch"
            OLookupLE k -> do
              (Identity ok, Identity rk) <- BTS.lookupLE 0 bs (Identity k)
              case Set.lookupLE k s of
                Nothing -> if not ok then pure s else error "BTreeSet LE false hit"
                Just mk -> if ok && rk == mk then pure s else error "BTreeSet LE mismatch"
            ORange lo hi -> do
              rng <- map runIdentity <$> collectStreamHost (BTS.rangeStream bs (Identity lo) (Identity hi))
              let expected = [k | k <- Set.toAscList s, k >= lo && k <= hi]
              if rng == expected then pure s else error "BTreeSet rangeStream mismatch"
            OClear -> do
              BTS.clear alloc bs
              invOk <- BTS.checkInvariants bs
              if invOk then pure Set.empty else error "BTreeSet clear invariant failure"

      sEnd <- foldlM' step Set.empty cmds
      aAsc <- map runIdentity <$> collectStreamHost (BTS.stream bs)
      aDesc <- map runIdentity <$> collectStreamHost (BTS.reverseStream bs)

      bsB <- BTS.fromStream @Int64 alloc (listToStreamHost (map Identity extraB))
      uList <- map runIdentity <$> collectStreamHost (BTS.unionS bs bsB)
      iList <- map runIdentity <$> collectStreamHost (BTS.intersectionS bs bsB)
      dList <- map runIdentity <$> collectStreamHost (BTS.differenceS bs bsB)

      BTS.free alloc bsB
      BTS.free alloc bs
      snap <- readAllocSnapshot tracker
      pure (aAsc, aDesc, Set.toAscList sEnd, uList, iList, dList, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  let setA = Set.fromAscList refA
      setB = Set.fromList extraB
  ascA === refA
  descA === reverse refA
  uOut === Set.toAscList (Set.union setA setB)
  iOut === Set.toAscList (Set.intersection setA setB)
  dOut === Set.toAscList (Set.difference setA setB)
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 3. Host <-> Compiled C BTreeMap & FlatMap Round-Trip
--------------------------------------------------------------------------------

cMutateOrderedContainers ::
  BTreeMap LinuxX86_64CCSE Int64 Int64 ->
  FlatMap LinuxX86_64CCSE Int64 Int64 ->
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMutateOrderedContainers btm fm n = do
  let btmPhase ::
        BTreeMap LinuxX86_64CCSE Int64 Int64 ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      btmPhase =
        fun (Proxy @LinuxX86_64CCSE) "ord_btm_phase" $ \_ btm' n' -> do
          -- Insert `n'` entries in C (forcing B+Tree leaf & internal splits inside compiled C!)
          rangeM 0 n' $ \i -> do
            let k = (i * 17) - 50
            BTM.insert dynamicAlloc btm' k (i + 1)
          -- Exercise B+Tree delete / miss path in C
          rangeM 0 n' $ \i -> do
            let k = (i * 17) - 50
            _ <- BTM.delete dynamicAlloc btm' (k - 100000)
            pure ()
          runDrain drainSum (snd <$> BTM.rangeStream btm' (-20) 500)

      fmPhase ::
        FlatMap LinuxX86_64CCSE Int64 Int64 ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      fmPhase =
        fun (Proxy @LinuxX86_64CCSE) "ord_fm_phase" $ \_ fm' n' -> do
          rangeM 0 n' $ \i -> do
            let k = (i * 17) - 50
            FM.insert dynamicAlloc fm' k (i + 1)
          runDrain drainSum (snd <$> FM.rangeStream fm' (-20) 500)

  s1 <- btmPhase btm n
  s2 <- fmPhase fm n
  pure (s1 - s2)

check_ordered_host_c_ffi ::
  IO (BTreeMap Identity Int64 Int64 -> FlatMap Identity Int64 Int64 -> Int64 -> IO Int64) ->
  Property
check_ordered_host_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  initKVs <-
    forAll $
      Gen.list
        (Range.linear 0 30)
        ((,) <$> Gen.int64 (Range.linear (-100) 100) <*> Gen.int64 (Range.linear 1 200))
  n <- forAll $ Gen.int64 (Range.linear 0 45)

  (diffSum, invOk, btmAsc, fmAsc) <- liftIO $ do
    btm <- BTM.new @Int64 @Int64 dynamicAlloc
    fm <- FM.new @Int64 @Int64 dynamicAlloc
    forM_ initKVs $ \(k, v) -> do
      BTM.insert dynamicAlloc btm (Identity k) (Identity v)
      FM.insert dynamicAlloc fm (Identity k) (Identity v)
    d <- cFn btm fm n
    ok <- BTM.checkInvariants btm
    bList <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (BTM.toStream btm)
    fList <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (FM.toStream fm)
    BTM.free dynamicAlloc btm
    FM.free dynamicAlloc fm
    pure (d, ok, bList, fList)

  assert invOk
  diffSum === 0
  btmAsc === fmAsc

test_ordered_containers :: TestTree
test_ordered_containers =
  let acq = getOrdTestConfig >>= flip acquireJanusC cMutateOrderedContainers
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "Ordered Data Structures (FlatMap, FlatSet, BTreeMap, BTreeSet)"
          [ testProperty "FlatMap & FlatSet stateful trace, rangeStream, set-algebra merges, & bulk drain" check_flatmap_and_flatset,
            testProperty "BTreeMap multi-level splits/borrows/merges + per-step structural invariants" check_btreemap_invariants_and_operations,
            testProperty "BTreeSet multi-level splits/borrows/merges + set-algebra streams" check_btreeset_invariants_and_set_algebra,
            testProperty "Host <-> Compiled C BTreeMap & FlatMap FFI round-trip" (check_ordered_host_c_ffi (fst <$> rFn))
          ]
