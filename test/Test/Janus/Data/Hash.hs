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

module Test.Janus.Data.Hash where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Property, assert, forAll, property, (===))
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
import qualified Janus.Data.HashMap as HM
import qualified Janus.Data.HashSet as HS
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getHashTestConfig :: IO CBuildConfig
getHashTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

--------------------------------------------------------------------------------
-- HKD 2D Key Struct Testing Generic MurmurHash Over SoA Keys
--------------------------------------------------------------------------------

data Key2D f = Key2D
  { k2X :: !(f Int64),
    k2Y :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (Key2D Identity)

deriving stock instance Ord (Key2D Identity)

deriving stock instance Show (Key2D Identity)

instance JanusStruct Key2D

--------------------------------------------------------------------------------
-- 1. SwissTable HashMap Differential Trace + Tombstone Churn + HKD Keys
--------------------------------------------------------------------------------

data MapCmd
  = MInsert !Int64 !Int64
  | MInsertWithAdd !Int64 !Int64
  | MDelete !Int64
  | MLookup !Int64
  | MClear
  deriving (Eq, Show)

check_hashmap_stateful_and_tombstone_churn :: Property
check_hashmap_stateful_and_tombstone_churn = property $ do
  -- Small key range [-12 .. 12] forces intense collisions, tombstone reuse, and tombstone-sweep rehashes!
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 250)
        ( Gen.frequency
            [ (6, MInsert <$> Gen.int64 (Range.linear (-12) 12) <*> Gen.int64 (Range.linear (-1000) 1000)),
              (4, MInsertWithAdd <$> Gen.int64 (Range.linear (-12) 12) <*> Gen.int64 (Range.linear 1 50)),
              (5, MDelete <$> Gen.int64 (Range.linear (-15) 15)),
              (5, MLookup <$> Gen.int64 (Range.linear (-15) 15)),
              (1, pure MClear)
            ]
        )

  (finalPairs, modelPairs, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      hm <- HM.new @Int64 @Int64 alloc
      let step m cmd = case cmd of
            MInsert k v -> do
              HM.insert alloc hm (Identity k) (Identity v)
              pure (Map.insert k v m)
            MInsertWithAdd k v -> do
              HM.insertWith alloc hm (+) (Identity k) (Identity v)
              pure (Map.insertWith (+) k v m)
            MDelete k -> do
              Identity removed <- HM.delete hm (Identity k)
              if removed /= Map.member k m
                then error "HashMap delete return mismatch"
                else pure (Map.delete k m)
            MLookup k -> do
              (Identity found, Identity val) <- HM.lookup 0 hm (Identity k)
              case Map.lookup k m of
                Nothing -> if not found then pure m else error "HashMap false hit"
                Just mv -> if found && val == mv then pure m else error "HashMap lookup mismatch"
            MClear -> HM.clear hm >> pure Map.empty

      mEnd <- foldlM' step Map.empty cmds
      rawPairs <- collectStreamHost (HM.toStream hm)
      let unPairs = sort [(k, v) | (Identity k, Identity v) <- rawPairs]
      HM.free alloc hm
      snap <- readAllocSnapshot tracker
      pure (unPairs, Map.toAscList mEnd, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  finalPairs === modelPairs
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

check_hashmap_hkd_soa_keys :: Property
check_hashmap_hkd_soa_keys = property $ do
  entries <-
    forAll $
      Gen.list
        (Range.linear 0 80)
        ( (,,)
            <$> Gen.int64 (Range.linear (-8) 8)
            <*> Gen.int64 (Range.linear (-8) 8)
            <*> Gen.int64 (Range.linear (-500) 500)
        )
  (hmPairs, refPairs, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      hm <- HM.new @(SoA Key2D) @Int64 alloc
      forM_ entries $ \(x, y, v) ->
        HM.insert alloc hm (Key2D (Identity x) (Identity y)) (Identity v)
      raw <- collectStreamHost (HM.toStream hm)
      HM.free alloc hm
      snap <- readAllocSnapshot tracker
      let got = sort [(k, v) | (k, Identity v) <- raw]
          ref = Map.toAscList (Map.fromList [(Key2D (Identity x) (Identity y), v) | (x, y, v) <- entries])
      pure (got, ref, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  hmPairs === refPairs
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 2. SwissTable HashSet Set-Algebra Streams & Stateful Trace
--------------------------------------------------------------------------------

check_hashset_algebra_and_trace :: Property
check_hashset_algebra_and_trace = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 80) (Gen.int64 (Range.linear (-30) 30))
  ys <- forAll $ Gen.list (Range.linear 0 80) (Gen.int64 (Range.linear (-30) 30))
  dels <- forAll $ Gen.list (Range.linear 0 30) (Gen.int64 (Range.linear (-30) 30))

  (aElems, interElems, diffElems, subsetSelf, subsetUnion, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      sa <- HS.new @Int64 alloc
      sb <- HS.new @Int64 alloc
      forM_ xs (HS.insert alloc sa . Identity)
      forM_ dels (HS.delete sa . Identity)
      forM_ ys (HS.insert alloc sb . Identity)

      la <- sort . map runIdentity <$> collectStreamHost (HS.stream sa)
      li <- sort . map runIdentity <$> collectStreamHost (HS.intersectionS sa sb)
      ld <- sort . map runIdentity <$> collectStreamHost (HS.differenceS sa sb)
      Identity subSelf <- HS.isSubsetOf sa sa

      su <- HS.fromStream @Int64 alloc (HS.stream sa)
      HS.unionInto alloc su sb
      Identity subU <- HS.isSubsetOf sa su

      HS.free alloc su
      HS.free alloc sa
      HS.free alloc sb
      snap <- readAllocSnapshot tracker
      pure (la, li, ld, subSelf, subU, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  let refA = foldr Set.delete (Set.fromList xs) dels
      refB = Set.fromList ys
  aElems === Set.toAscList refA
  interElems === Set.toAscList (Set.intersection refA refB)
  diffElems === Set.toAscList (Set.difference refA refB)
  assert subsetSelf
  assert subsetUnion
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. Host <-> Compiled C SwissTable HashMap & HashSet Round-Trip
--------------------------------------------------------------------------------

cMutateHashMapAndSet ::
  HashMap LinuxX86_64CCSE Int64 Int64 ->
  HashSet LinuxX86_64CCSE Int64 ->
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMutateHashMapAndSet hm hs n = do
  rangeM 0 n $ \i -> do
    HM.insertWith dynamicAlloc hm (+) i (i * 3 + 1)
    _ <- HS.insert dynamicAlloc hs (i * 2)
    pure ()
  sMap <- runDrain drainSum (HM.valuesS hm)
  sSet <- runDrain drainSum (HS.stream hs)
  pure (sMap + sSet)

check_hash_host_c_ffi ::
  IO (HashMap Identity Int64 Int64 -> HashSet Identity Int64 -> Int64 -> IO Int64) ->
  Property
check_hash_host_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  initPairs <-
    forAll $
      Gen.list
        (Range.linear 0 30)
        ((,) <$> Gen.int64 (Range.linear 0 25) <*> Gen.int64 (Range.linear 1 100))
  n <- forAll $ Gen.int64 (Range.linear 0 40)

  (cTotal, hsPairs, hsSetList) <- liftIO $ do
    hm <- HM.new @Int64 @Int64 dynamicAlloc
    hs <- HS.new @Int64 dynamicAlloc
    forM_ initPairs $ \(k, v) -> do
      HM.insert dynamicAlloc hm (Identity k) (Identity v)
      _ <- HS.insert dynamicAlloc hs (Identity k)
      pure ()
    tot <- cFn hm hs n
    mpOut <- sort . map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (HM.toStream hm)
    stOut <- sort . map runIdentity <$> collectStreamHost (HS.stream hs)
    HM.free dynamicAlloc hm
    HS.free dynamicAlloc hs
    pure (tot, mpOut, stOut)

  let m0 = Map.fromList initPairs
      m1 = foldl (\acc i -> Map.insertWith (+) i (i * 3 + 1) acc) m0 [0 .. n - 1]
      s0 = Set.fromList (map fst initPairs)
      s1 = foldl (\acc i -> Set.insert (i * 2) acc) s0 [0 .. n - 1]
  hsPairs === Map.toAscList m1
  hsSetList === Set.toAscList s1
  cTotal === sum (Map.elems m1) + sum (Set.toList s1)

test_hash_containers :: TestTree
test_hash_containers =
  let acq = getHashTestConfig >>= flip acquireJanusC cMutateHashMapAndSet
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "SwissTable HashMap & HashSet (MurmurHash + SWAR)"
          [ testProperty "HashMap stateful trace + collisions + tombstone churn + zero leaks" check_hashmap_stateful_and_tombstone_churn,
            testProperty "HashMap with HKD SoA 2D struct keys via generic MurmurHash" check_hashmap_hkd_soa_keys,
            testProperty "HashSet set-algebra streams (intersectionS, differenceS, unionInto)" check_hashset_algebra_and_trace,
            testProperty "Host <-> Compiled C HashMap & HashSet FFI round-trip" (check_hash_host_c_ffi (fst <$> rFn))
          ]
