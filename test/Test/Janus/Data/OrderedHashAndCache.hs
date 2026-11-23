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

module Test.Janus.Data.OrderedHashAndCache where

import Control.Lens ((&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (findIndex, nub, sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM)
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream, rangeS)
import Janus.Data.ClockCache (ClockCache)
import qualified Janus.Data.ClockCache as CC
import Janus.Data.HKD (JanusStruct)
import Janus.Data.IndexMap (IndexMap)
import qualified Janus.Data.IndexMap as IM
import Janus.Data.IndexSet (IndexSet)
import qualified Janus.Data.IndexSet as IS
import Janus.Data.Layout (SoA)
import Janus.Data.LruCache (LruCache)
import qualified Janus.Data.LruCache as LRU
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getOrderedCacheTestConfig :: IO CBuildConfig
getOrderedCacheTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost sIn = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) sIn
  reverse <$> readIORef ref

listToStreamHost :: [a] -> Stream IO a
listToStreamHost xs0 = asum (map pure xs0)

foldlM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldlM' _ acc [] = pure acc
foldlM' stepFn acc (x : xs) = do
  acc' <- stepFn acc x
  acc' `seq` foldlM' stepFn acc' xs

--------------------------------------------------------------------------------
-- HKD Key & Value Structs for SoA + SPMD IndexMap Testing
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

data Vec2I64 f = Vec2I64
  { v2A :: !(f Int64),
    v2B :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (Vec2I64 Identity)

deriving stock instance Show (Vec2I64 Identity)

instance JanusStruct Vec2I64

--------------------------------------------------------------------------------
-- 1. IndexMap Exact Insertion Order, Removals, Positional Access, & sortByKey
--------------------------------------------------------------------------------

data IndexMapCmd
  = IMInsert !Int64 !Int64
  | IMInsertWithAdd !Int64 !Int64
  | IMShiftRemove !Int64
  | IMSwapRemove !Int64
  | IMPopBack
  | IMLookupCheck !Int64
  | IMWriteAt !Int !Int64
  | IMSortByKey
  | IMClear
  deriving (Eq, Show)

genIndexMapCmd :: Gen IndexMapCmd
genIndexMapCmd =
  Gen.frequency
    [ (6, IMInsert <$> Gen.int64 (Range.linear (-14) 14) <*> Gen.int64 (Range.linear (-500) 500)),
      (4, IMInsertWithAdd <$> Gen.int64 (Range.linear (-14) 14) <*> Gen.int64 (Range.linear 1 50)),
      (4, IMShiftRemove <$> Gen.int64 (Range.linear (-16) 16)),
      (4, IMSwapRemove <$> Gen.int64 (Range.linear (-16) 16)),
      (2, pure IMPopBack),
      (4, IMLookupCheck <$> Gen.int64 (Range.linear (-16) 16)),
      (2, IMWriteAt <$> Gen.int (Range.linear 0 50) <*> Gen.int64 (Range.linear (-999) 999)),
      (2, pure IMSortByKey),
      (1, pure IMClear)
    ]

imModelInsertWith :: (Int64 -> Int64 -> Int64) -> Int64 -> Int64 -> [(Int64, Int64)] -> [(Int64, Int64)]
imModelInsertWith combine k v model =
  case break ((== k) . fst) model of
    (_, []) -> model ++ [(k, v)]
    (before, (_, oldV) : after) -> before ++ [(k, combine oldV v)] ++ after

imModelShiftRemove :: Int64 -> [(Int64, Int64)] -> (Bool, [(Int64, Int64)])
imModelShiftRemove k model =
  case break ((== k) . fst) model of
    (_, []) -> (False, model)
    (before, _ : after) -> (True, before ++ after)

imModelSwapRemove :: Int64 -> [(Int64, Int64)] -> (Bool, [(Int64, Int64)])
imModelSwapRemove k model =
  case findIndex ((== k) . fst) model of
    Nothing -> (False, model)
    Just idx
      | idx == length model - 1 -> (True, init model)
      | otherwise ->
          let lastPair = last model
              initPairs = init model
           in (True, take idx initPairs ++ [lastPair] ++ drop (idx + 1) initPairs)

check_indexmap_order_and_ops :: Property
check_indexmap_order_and_ops = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 240) genIndexMapCmd
  (fwdOut, revOut, keysOut, valsOut, clonedOut, modelEnd, sortZeroAllocOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      im <- IM.new @Int64 @Int64 alloc
      sortAllocOkRef <- newIORef True

      let verifyIndices curModel =
            forM_ (zip [0 :: Int64 ..] curModel) $ \(expectedIdx, (mk, mv)) -> do
              (Identity foundIdx, Identity gotIdx) <- IM.lookupIndex im (Identity mk)
              (Identity gk, Identity gv) <- IM.getAtIndex im (Identity expectedIdx)
              when (not foundIdx || gotIdx /= expectedIdx || gk /= mk || gv /= mv) $
                error "IndexMap dense/sparse index invariant violated"

          step model cmd = case cmd of
            IMInsert k v -> do
              IM.insert alloc im (Identity k) (Identity v)
              pure (imModelInsertWith (\_ newV -> newV) k v model)
            IMInsertWithAdd k dv -> do
              IM.insertWith alloc im (+) (Identity k) (Identity dv)
              pure (imModelInsertWith (+) k dv model)
            IMShiftRemove k -> do
              Identity removed <- IM.shiftRemove im (Identity k)
              let (expRem, model') = imModelShiftRemove k model
              if removed /= expRem
                then error "IndexMap shiftRemove mismatch"
                else verifyIndices model' >> pure model'
            IMSwapRemove k -> do
              Identity removed <- IM.swapRemove im (Identity k)
              let (expRem, model') = imModelSwapRemove k model
              if removed /= expRem
                then error "IndexMap swapRemove mismatch"
                else verifyIndices model' >> pure model'
            IMPopBack -> do
              (Identity ok, Identity pk, Identity pv) <- IM.popBack 0 0 im
              case reverse model of
                [] ->
                  if not ok then pure [] else error "IndexMap popBack succeeded on empty"
                ((ek, ev) : restRev) ->
                  if ok && pk == ek && pv == ev
                    then pure (reverse restRev)
                    else error "IndexMap popBack mismatch"
            IMLookupCheck k -> do
              Identity hasK <- IM.containsKey im (Identity k)
              (Identity foundV, Identity gotV) <- IM.lookup 0 im (Identity k)
              (Identity foundI, Identity gotI) <- IM.lookupIndex im (Identity k)
              case findIndex ((== k) . fst) model of
                Nothing ->
                  if not hasK && not foundV && not foundI
                    then pure model
                    else error "IndexMap false positive lookup"
                Just pos -> do
                  let (_, ev) = model !! pos
                  Identity kAt <- IM.getKeyAtIndex im (Identity (fromIntegral pos))
                  Identity vAt <- IM.getValAtIndex im (Identity (fromIntegral pos))
                  if hasK && foundV && foundI && gotV == ev && gotI == fromIntegral pos && kAt == k && vAt == ev
                    then pure model
                    else error "IndexMap lookup/index mismatch"
            IMWriteAt rawIdx newV
              | null model -> pure []
              | otherwise -> do
                  let pos = rawIdx `mod` length model
                      (ek, _) = model !! pos
                  IM.writeValAtIndex im (Identity (fromIntegral pos)) (Identity newV)
                  pure (take pos model ++ [(ek, newV)] ++ drop (pos + 1) model)
            IMSortByKey -> do
              snapBefore <- readAllocSnapshot tracker
              IM.sortByKey im
              snapAfter <- readAllocSnapshot tracker
              when (snapTotalAllocs snapAfter /= snapTotalAllocs snapBefore) $
                modifyIORef' sortAllocOkRef (const False)
              let sortedModel = sortOn fst model
              verifyIndices sortedModel
              pure sortedModel
            IMClear -> do
              IM.clear im
              pure []

      mEnd <- foldlM' step [] cmds
      verifyIndices mEnd

      fwd <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (IM.toStream im)
      rev <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (IM.reverseStream im)
      ks <- map runIdentity <$> collectStreamHost (IM.keysS im)
      vs <- map runIdentity <$> collectStreamHost (IM.valuesS im)

      imClone <- IM.fromStream @Int64 @Int64 alloc (IM.toStream im)
      cloned <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (IM.toStream imClone)

      IM.free alloc imClone
      IM.free alloc im
      sortZeroOk <- readIORef sortAllocOkRef
      snap <- readAllocSnapshot tracker
      pure
        ( fwd,
          rev,
          ks,
          vs,
          cloned,
          mEnd,
          sortZeroOk,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  assert sortZeroAllocOk
  fwdOut === modelEnd
  revOut === reverse modelEnd
  keysOut === map fst modelEnd
  valsOut === map snd modelEnd
  clonedOut === modelEnd
  liveEnd === (0, 0)

check_indexmap_soa_spmd :: Property
check_indexmap_soa_spmd = property $ do
  rawEntries <-
    forAll $
      Gen.list
        (Range.linear 0 75)
        ( (,,,)
            <$> Gen.int64 (Range.linear (-6) 6)
            <*> Gen.int64 (Range.linear (-6) 6)
            <*> Gen.int64 (Range.linear (-200) 200)
            <*> Gen.int64 (Range.linear (-200) 200)
        )
  remKeys <-
    forAll $
      Gen.list
        (Range.linear 0 15)
        ((,) <$> Gen.int64 (Range.linear (-6) 6) <*> Gen.int64 (Range.linear (-6) 6))

  (gotEntries, expectedEntries, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      im <- IM.withCapacity @(SoA Key2D) @(SoA Vec2I64) alloc 8
      let insertStep acc (kx, ky, va, vb) = do
            let kRec = Key2D (Identity kx) (Identity ky)
                vRec = Vec2I64 (Identity va) (Identity vb)
            IM.insert alloc im kRec vRec
            pure $
              case break ((== kRec) . fst) acc of
                (_, []) -> acc ++ [(kRec, (va, vb))]
                (before, _ : after) -> before ++ [(kRec, (va, vb))] ++ after

          removeStep acc (kx, ky) = do
            let kRec = Key2D (Identity kx) (Identity ky)
            _ <- IM.shiftRemove im kRec
            pure (filter ((/= kRec) . fst) acc)

      m1 <- foldlM' insertStep [] rawEntries
      m2 <- foldlM' removeStep m1 remKeys

      IM.foreachSoAValuesSPMD (Proxy @4) im $ \idx v ->
        pure
          Vec2I64
            { v2A = v2A v * 3 + idx,
              v2B = v2B v - idx * 2
            }

      outPairs <- collectStreamHost (IM.toStream im)
      IM.free alloc im
      snap <- readAllocSnapshot tracker
      let expected =
            [ (kRec, Vec2I64 (Identity (va * 3 + i)) (Identity (vb - i * 2)))
            | (i, (kRec, (va, vb))) <- zip [0 :: Int64 ..] m2
            ]
      pure (outPairs, expected, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  gotEntries === expectedEntries
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 2. IndexSet Exact Order Preservation, Sorting, & Set-Algebra Streams
--------------------------------------------------------------------------------

data IndexSetCmd
  = ISInsert !Int64
  | ISShiftRemove !Int64
  | ISSwapRemove !Int64
  | ISPopBack
  | ISSort
  | ISClear
  deriving (Eq, Show)

check_indexset_order_and_algebra :: Property
check_indexset_order_and_algebra = property $ do
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 200)
        ( Gen.frequency
            [ (7, ISInsert <$> Gen.int64 (Range.linear (-15) 15)),
              (4, ISShiftRemove <$> Gen.int64 (Range.linear (-18) 18)),
              (4, ISSwapRemove <$> Gen.int64 (Range.linear (-18) 18)),
              (2, pure ISPopBack),
              (2, pure ISSort),
              (1, pure ISClear)
            ]
        )
  otherXs <- forAll $ Gen.list (Range.linear 0 40) (Gen.int64 (Range.linear (-15) 15))

  (fwdOut, revOut, interOut, diffOut, unionOut, subOk, sortZeroOk, modelEnd, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      sa <- IS.new @Int64 alloc
      sortAllocOkRef <- newIORef True

      let step model cmd = case cmd of
            ISInsert x -> do
              Identity newly <- IS.insert alloc sa (Identity x)
              if elem x model
                then if not newly then pure model else error "IndexSet duplicate insert returned True"
                else if newly then pure (model ++ [x]) else error "IndexSet fresh insert returned False"
            ISShiftRemove x -> do
              Identity remOk <- IS.shiftRemove sa (Identity x)
              if remOk /= elem x model
                then error "IndexSet shiftRemove mismatch"
                else pure (filter (/= x) model)
            ISSwapRemove x -> do
              Identity remOk <- IS.swapRemove sa (Identity x)
              case findIndex (== x) model of
                Nothing ->
                  if not remOk then pure model else error "IndexSet swapRemove false hit"
                Just idx ->
                  if not remOk
                    then error "IndexSet swapRemove missed live key"
                    else
                      if idx == length model - 1
                        then pure (init model)
                        else
                          let lx = last model
                              rest = init model
                           in pure (take idx rest ++ [lx] ++ drop (idx + 1) rest)
            ISPopBack -> do
              (Identity ok, Identity px) <- IS.popBack 0 sa
              case reverse model of
                [] -> if not ok then pure [] else error "IndexSet empty popBack"
                (lx : restRev) ->
                  if ok && px == lx then pure (reverse restRev) else error "IndexSet popBack mismatch"
            ISSort -> do
              snap0 <- readAllocSnapshot tracker
              IS.sort sa
              snap1 <- readAllocSnapshot tracker
              when (snapTotalAllocs snap1 /= snapTotalAllocs snap0) $
                modifyIORef' sortAllocOkRef (const False)
              pure (List.sort model)
            ISClear -> IS.clear sa >> pure []

      mEnd <- foldlM' step [] cmds
      forM_ (zip [0 :: Int64 ..] mEnd) $ \(idx, x) -> do
        (Identity ok, Identity gotIdx) <- IS.lookupIndex sa (Identity x)
        Identity atK <- IS.getAtIndex sa (Identity idx)
        when (not ok || gotIdx /= idx || atK /= x) $
          error "IndexSet lookupIndex / getAtIndex mismatch"

      sb <- IS.fromStream @Int64 alloc (listToStreamHost (map Identity otherXs))
      fwd <- map runIdentity <$> collectStreamHost (IS.stream sa)
      rev <- map runIdentity <$> collectStreamHost (IS.reverseStream sa)
      inter <- map runIdentity <$> collectStreamHost (IS.intersectionS sa sb)
      diff <- map runIdentity <$> collectStreamHost (IS.differenceS sa sb)

      su <- IS.fromStream @Int64 alloc (IS.stream sa)
      IS.unionInto alloc su sb
      uList <- map runIdentity <$> collectStreamHost (IS.stream su)
      Identity isSub <- IS.isSubsetOf sa su

      IS.free alloc su
      IS.free alloc sb
      IS.free alloc sa
      sOk <- readIORef sortAllocOkRef
      snap <- readAllocSnapshot tracker
      pure
        ( fwd,
          rev,
          inter,
          diff,
          uList,
          isSub,
          sOk,
          mEnd,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  let setB = Set.fromList otherXs
      expectedUnion = nub (modelEnd ++ nub otherXs)
  assert sortZeroOk
  assert subOk
  fwdOut === modelEnd
  revOut === reverse modelEnd
  interOut === filter (`Set.member` setB) modelEnd
  diffOut === filter (`Set.notMember` setB) modelEnd
  unionOut === expectedUnion
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. LruCache Exact MRU->LRU Differential Trace & Zero Steady-State Allocations
--------------------------------------------------------------------------------

data LruCmd
  = LInsert !Int64 !Int64
  | LLookup !Int64
  | LPeek !Int64
  | LDelete !Int64
  | LClear
  deriving (Eq, Show)

lruModelInsert :: Int -> Int64 -> Int64 -> [(Int64, Int64)] -> [(Int64, Int64)]
lruModelInsert cap k v model =
  case break ((== k) . fst) model of
    (before, _ : after) -> (k, v) : (before ++ after)
    (_, []) -> take cap ((k, v) : model)

lruModelLookup :: Int64 -> [(Int64, Int64)] -> (Bool, Int64, [(Int64, Int64)])
lruModelLookup k model =
  case break ((== k) . fst) model of
    (_, []) -> (False, 0, model)
    (before, (_, v) : after) -> (True, v, (k, v) : (before ++ after))

lruModelDelete :: Int64 -> [(Int64, Int64)] -> (Bool, [(Int64, Int64)])
lruModelDelete k model =
  case break ((== k) . fst) model of
    (_, []) -> (False, model)
    (before, _ : after) -> (True, before ++ after)

check_lru_cache_exact_order_and_zero_steady_allocs :: Property
check_lru_cache_exact_order_and_zero_steady_allocs = property $ do
  cap <- forAll $ Gen.int (Range.linear 1 14)
  -- 250..340 operations over key space [0 .. 24] forces heavy eviction & multiple tombstone compactions!
  cmds <-
    forAll $
      Gen.list
        (Range.linear 250 340)
        ( Gen.frequency
            [ (8, LInsert <$> Gen.int64 (Range.linear 0 24) <*> Gen.int64 (Range.linear (-500) 500)),
              (6, LLookup <$> Gen.int64 (Range.linear 0 28)),
              (4, LPeek <$> Gen.int64 (Range.linear 0 28)),
              (3, LDelete <$> Gen.int64 (Range.linear 0 28)),
              (1, pure LClear)
            ]
        )

  (finalMru, finalLru, modelEnd, zeroSteadyAllocs, stepOrderOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      lru <- LRU.new @Int64 @Int64 alloc (Identity (fromIntegral cap))
      snapAfterNew <- readAllocSnapshot tracker
      orderOkRef <- newIORef True

      let step (stepIdx, model) cmd = do
            model' <- case cmd of
              LInsert k v -> do
                LRU.insert lru (Identity k) (Identity v)
                pure (lruModelInsert cap k v model)
              LLookup k -> do
                (Identity found, Identity gotV) <- LRU.lookup 0 lru (Identity k)
                let (expFound, expV, promoted) = lruModelLookup k model
                if found == expFound && (not found || gotV == expV)
                  then pure promoted
                  else error "LruCache lookup mismatch"
              LPeek k -> do
                Identity hasK <- LRU.containsKey lru (Identity k)
                (Identity found, Identity gotV) <- LRU.peekKey 0 lru (Identity k)
                case List.lookup k model of
                  Nothing ->
                    if not hasK && not found then pure model else error "LruCache peek false hit"
                  Just expV ->
                    if hasK && found && gotV == expV then pure model else error "LruCache peek mismatch"
              LDelete k -> do
                Identity remOk <- LRU.delete lru (Identity k)
                let (expRem, nextM) = lruModelDelete k model
                if remOk == expRem then pure nextM else error "LruCache delete mismatch"
              LClear -> do
                LRU.clear lru
                pure []

            when (stepIdx `mod` 25 == (0 :: Int)) $ do
              curMru <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (LRU.mruStream lru)
              Identity curSz <- LRU.size lru
              when (curMru /= model' || curSz /= fromIntegral (length model')) $
                modifyIORef' orderOkRef (const False)
            pure (stepIdx + 1, model')

      (_, mEnd) <- foldlM' step (1 :: Int, []) cmds
      snapAfterChurn <- readAllocSnapshot tracker
      let steadyAllocs = runIdentity (snapTotalAllocs snapAfterChurn) - runIdentity (snapTotalAllocs snapAfterNew)

      mruOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (LRU.mruStream lru)
      lruOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (LRU.lruStream lru)
      LRU.free alloc lru
      okSteps <- readIORef orderOkRef
      snapFinal <- readAllocSnapshot tracker
      pure
        ( mruOut,
          lruOut,
          mEnd,
          steadyAllocs == 0,
          okSteps,
          (runIdentity (snapLiveBlocks snapFinal), runIdentity (snapLiveBytes snapFinal))
        )

  assert zeroSteadyAllocs
  assert stepOrderOk
  finalMru === modelEnd
  finalLru === reverse modelEnd
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 4. ClockCache Second-Chance Retention & Zero Steady-State Allocations
--------------------------------------------------------------------------------

data ClockCmd
  = CInsert !Int64 !Int64
  | CLookup !Int64
  | CPeek !Int64
  | CDelete !Int64
  | CClear
  deriving (Eq, Show)

check_clock_cache_second_chance_and_zero_steady_allocs :: Property
check_clock_cache_second_chance_and_zero_steady_allocs = property $ do
  scCap <- forAll $ Gen.int64 (Range.linear 4 16)
  churnCap <- forAll $ Gen.int64 (Range.linear 1 12)
  cmds <-
    forAll $
      Gen.list
        (Range.linear 260 360)
        ( Gen.frequency
            [ (8, CInsert <$> Gen.int64 (Range.linear 0 36) <*> Gen.int64 (Range.linear (-500) 500)),
              (6, CLookup <$> Gen.int64 (Range.linear 0 40)),
              (4, CPeek <$> Gen.int64 (Range.linear 0 40)),
              (3, CDelete <$> Gen.int64 (Range.linear 0 40)),
              (1, pure CClear)
            ]
        )

  (secondChanceOk, zeroSteadyAllocs, invariantsOk, streamCloneOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc

      -- Phase A: Exact second-chance retention verification
      ccSC <- CC.new @Int64 @Int64 alloc (Identity scCap)
      forM_ [0 .. scCap - 1] $ \k ->
        CC.insert ccSC (Identity k) (Identity (k * 10))
      -- Inserting `scCap` sweeps slots `0 .. scCap - 1` clearing all ref-bits to 0, evicts slot 0 (`hand` = 1)
      CC.insert ccSC (Identity scCap) (Identity 999)
      -- `peekKey` on key 1 MUST leave its ref-bit at 0; `lookup` on key 2 MUST set its ref-bit to 1!
      (Identity p1Ok, _) <- CC.peekKey 0 ccSC (Identity 1)
      (Identity l2Ok, _) <- CC.lookup 0 ccSC (Identity 2)
      -- Next insert evicts key 1 (`refBit == 0`, `hand` advances to 2)
      CC.insert ccSC (Identity (scCap + 1)) (Identity 1001)
      -- Next insert visits key 2 (`refBit == 1`), spares key 2 (clearing its ref-bit), and evicts key 3!
      CC.insert ccSC (Identity (scCap + 2)) (Identity 1002)
      Identity has1 <- CC.containsKey ccSC (Identity 1)
      Identity has2 <- CC.containsKey ccSC (Identity 2)
      Identity has3 <- CC.containsKey ccSC (Identity 3)
      let scOk = p1Ok && l2Ok && not has1 && has2 && not has3
      CC.free alloc ccSC

      -- Phase B: Heavy eviction & tombstone-compaction churn with ZERO steady-state allocations
      cc <- CC.new @Int64 @Int64 alloc (Identity churnCap)
      snapAfterNew <- readAllocSnapshot tracker
      invOkRef <- newIORef True

      -- Track latest authoritative value written per key (any resident key MUST hold this exact value!)
      let step latestVals cmd = case cmd of
            CInsert k v -> do
              CC.insert cc (Identity k) (Identity v)
              Identity hasNow <- CC.containsKey cc (Identity k)
              Identity szNow <- CC.size cc
              when (not hasNow || szNow > churnCap) $
                modifyIORef' invOkRef (const False)
              pure (Map.insert k v latestVals)
            CLookup k -> do
              (Identity found, Identity gotV) <- CC.lookup 0 cc (Identity k)
              when found $
                case Map.lookup k latestVals of
                  Nothing -> modifyIORef' invOkRef (const False)
                  Just expV -> when (gotV /= expV) $ modifyIORef' invOkRef (const False)
              pure latestVals
            CPeek k -> do
              (Identity found, Identity gotV) <- CC.peekKey 0 cc (Identity k)
              when found $
                case Map.lookup k latestVals of
                  Nothing -> modifyIORef' invOkRef (const False)
                  Just expV -> when (gotV /= expV) $ modifyIORef' invOkRef (const False)
              pure latestVals
            CDelete k -> do
              Identity hadBefore <- CC.containsKey cc (Identity k)
              Identity remOk <- CC.delete cc (Identity k)
              Identity hasAfter <- CC.containsKey cc (Identity k)
              when (remOk /= hadBefore || hasAfter) $
                modifyIORef' invOkRef (const False)
              pure (Map.delete k latestVals)
            CClear -> do
              CC.clear cc
              Identity emptyNow <- CC.isEmpty cc
              when (not emptyNow) $
                modifyIORef' invOkRef (const False)
              pure Map.empty

      finalLatest <- foldlM' step Map.empty cmds
      snapAfterChurn <- readAllocSnapshot tracker
      let steadyAllocs = runIdentity (snapTotalAllocs snapAfterChurn) - runIdentity (snapTotalAllocs snapAfterNew)

      streamPairs <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (CC.toStream cc)
      Identity finalSz <- CC.size cc
      let allMatchLatest =
            fromIntegral (length streamPairs) == finalSz
              && finalSz <= churnCap
              && all (\(k, v) -> Map.lookup k finalLatest == Just v) streamPairs

      ccClone <- CC.fromStream @Int64 @Int64 alloc (Identity churnCap) (CC.toStream cc)
      clonePairs <- List.sort . map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (CC.toStream ccClone)
      CC.free alloc ccClone
      CC.free alloc cc

      invOk <- readIORef invOkRef
      snapFinal <- readAllocSnapshot tracker
      pure
        ( scOk,
          steadyAllocs == 0,
          invOk && allMatchLatest,
          clonePairs == List.sort streamPairs,
          (runIdentity (snapLiveBlocks snapFinal), runIdentity (snapLiveBytes snapFinal))
        )

  assert secondChanceOk
  assert zeroSteadyAllocs
  assert invariantsOk
  assert streamCloneOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 5. Host <-> Compiled C FFI Round-Trip (`compileJanusC`)
--------------------------------------------------------------------------------

cMutateOrderedAndCaches ::
  IndexMap LinuxX86_64C Int64 Int64 ->
  IndexSet LinuxX86_64C Int64 ->
  ClockCache LinuxX86_64C Int64 Int64 ->
  LruCache LinuxX86_64C Int64 Int64 ->
  LinuxX86_64C Int64 ->
  LinuxX86_64CM (LinuxX86_64C Int64)
cMutateOrderedAndCaches im is cc lru n = do
  runDrain
    ( drainForEach $ \i -> do
        IM.insertWith dynamicAlloc im (+) i (i * 10 + 1)
        _ <- IS.insert dynamicAlloc is (i * 2)
        CC.insert cc i (i * 5)
        LRU.insert lru i (i * 7)
    )
    (rangeS 0 n)
  IM.foreachScalarValuesSPMD (Proxy @4) im (\_ v -> pure (v * 2))
  s1 <- runDrain drainSum (IM.valuesS im)
  s2 <- runDrain drainSum (IS.stream is)
  s3 <- runDrain drainSum (CC.valuesS cc)
  s4 <- runDrain drainSum (LRU.mruValuesS lru)
  pure (s1 + s2 + s3 + s4)

check_ordered_and_cache_host_c_ffi ::
  IO
    ( IndexMap Identity Int64 Int64 ->
      IndexSet Identity Int64 ->
      ClockCache Identity Int64 Int64 ->
      LruCache Identity Int64 Int64 ->
      Int64 ->
      IO Int64
    ) ->
  Property
check_ordered_and_cache_host_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  initPairs <-
    forAll $
      Gen.list
        (Range.linear 0 20)
        ((,) <$> Gen.int64 (Range.linear 0 15) <*> Gen.int64 (Range.linear 1 100))
  n <- forAll $ Gen.int64 (Range.linear 0 35)
  let cacheCap = 8 :: Int64

  (cTotal, hsImPairs, hsIsList, hsLruMru, hsCcPairs, expectedIm, expectedIs, expectedLru) <- liftIO $ do
    im <- IM.new @Int64 @Int64 dynamicAlloc
    is <- IS.new @Int64 dynamicAlloc
    cc <- CC.new @Int64 @Int64 dynamicAlloc (Identity cacheCap)
    lru <- LRU.new @Int64 @Int64 dynamicAlloc (Identity cacheCap)

    let initStep (mAcc, sAcc, lAcc) (k, v) = do
          IM.insert dynamicAlloc im (Identity k) (Identity v)
          _ <- IS.insert dynamicAlloc is (Identity k)
          CC.insert cc (Identity k) (Identity v)
          LRU.insert lru (Identity k) (Identity v)
          let mAcc' = imModelInsertWith (\_ nv -> nv) k v mAcc
              sAcc' = if elem k sAcc then sAcc else sAcc ++ [k]
              lAcc' = lruModelInsert (fromIntegral cacheCap) k v lAcc
          pure (mAcc', sAcc', lAcc')

    (m0, s0, l0) <- foldlM' initStep ([], [], []) initPairs
    tot <- cFn im is cc lru n

    imOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (IM.toStream im)
    isOut <- map runIdentity <$> collectStreamHost (IS.stream is)
    lruOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (LRU.mruStream lru)
    ccOut <- map (\(Identity k, Identity v) -> (k, v)) <$> collectStreamHost (CC.toStream cc)

    IM.free dynamicAlloc im
    IS.free dynamicAlloc is
    CC.free dynamicAlloc cc
    LRU.free dynamicAlloc lru

    let m1 =
          map (\(k, v) -> (k, v * 2)) $
            foldl (\acc i -> imModelInsertWith (+) i (i * 10 + 1) acc) m0 [0 .. n - 1]
        s1 =
          foldl (\acc i -> let x = i * 2 in if elem x acc then acc else acc ++ [x]) s0 [0 .. n - 1]
        l1 =
          foldl (\acc i -> lruModelInsert (fromIntegral cacheCap) i (i * 7) acc) l0 [0 .. n - 1]

    pure (tot, imOut, isOut, lruOut, ccOut, m1, s1, l1)

  hsImPairs === expectedIm
  hsIsList === expectedIs
  hsLruMru === expectedLru
  assert (length hsCcPairs <= fromIntegral cacheCap)
  cTotal
    === sum (map snd expectedIm)
      + sum expectedIs
      + sum (map snd hsCcPairs)
      + sum (map snd expectedLru)

test_ordered_hash_and_cache :: TestTree
test_ordered_hash_and_cache =
  let acq = getOrderedCacheTestConfig >>= flip acquireJanusC cMutateOrderedAndCaches
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "Insertion-Ordered SwissTables (IndexMap, IndexSet) & Bounded Caches (ClockCache, LruCache)"
          [ testProperty "IndexMap exact insertion order, shiftRemove, swapRemove, popBack, & zero-alloc sortByKey" check_indexmap_order_and_ops,
            testProperty "IndexMap HKD SoA 2D keys & SoA 4-lane SPMD values transformation" check_indexmap_soa_spmd,
            testProperty "IndexSet exact order preservation, zero-alloc sort, & set-algebra streams" check_indexset_order_and_algebra,
            testProperty "LruCache exact MRU->LRU order across 250+ ops & zero steady-state allocations" check_lru_cache_exact_order_and_zero_steady_allocs,
            testProperty "ClockCache second-chance retention & zero steady-state allocations under heavy churn" check_clock_cache_second_chance_and_zero_steady_allocs,
            testProperty "Host <-> Compiled C IndexMap, IndexSet, ClockCache, & LruCache FFI round-trip" (check_ordered_and_cache_host_c_ffi (fst <$> rFn))
          ]
