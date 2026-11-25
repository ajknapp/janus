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

module Test.Janus.Data.SPMD where

import Control.Lens ((&), (.~))
import Control.Monad (forM, forM_)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Foreign.Marshal.Array (peekArray)
import GHC.Generics (Generic)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Command.Memory (withAlignedArray)
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, rangeS, statefulS, streamM)
import Janus.Data
import Janus.Expression.Cast (ExpBoolCast (..))
import Janus.Expression.Cond (ExpCond (..))
import qualified Janus.Data.BTreeMap as BTM
import qualified Janus.Data.BTreeSet as BTS
import qualified Janus.Data.BitSet as BS
import qualified Janus.Data.BitTree as BT
import qualified Janus.Data.BloomFilter as BF
import qualified Janus.Data.ClockCache as CC
import qualified Janus.Data.CsrGraph as CSR
import qualified Janus.Data.Deque as DQ
import qualified Janus.Data.DisjointSet as DS
import qualified Janus.Data.DynVec as DV
import qualified Janus.Data.EytzingerVec as EV
import qualified Janus.Data.FlatIntervalMap as FIM
import qualified Janus.Data.FlatMap as FM
import qualified Janus.Data.FlatSet as FS
import qualified Janus.Data.HashMap as HM
import qualified Janus.Data.HashSet as HS
import qualified Janus.Data.IndexMap as IM
import qualified Janus.Data.IndexSet as IS
import qualified Janus.Data.IndexedPQ as IPQ
import qualified Janus.Data.Interner as INTR
import qualified Janus.Data.LruCache as LRU
import qualified Janus.Data.Pool as POOL
import qualified Janus.Data.PriorityQueue as PQ
import qualified Janus.Data.RingBuffer as RB
import qualified Janus.Data.SlotMap as SM
import qualified Janus.Data.SmallStack as SSTK
import qualified Janus.Data.SmallVec as SV
import qualified Janus.Data.SparseSet as SS
import qualified Janus.Data.SparseVec as SVEC
import qualified Janus.Data.StableVec as STV
import qualified Janus.Data.Stack as STK
import qualified Janus.Data.XorFilter as XF
import Janus.SPMD
  ( HostSPMDValue (..),
    SPMD,
    gangReduceAdd,
    spmdAlignedUnitStride,
    uniform,
    withSPMD,
  )
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getSPMDDataTestConfig :: IO CBuildConfig
getSPMDDataTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

collectStreamHost :: Stream IO a -> IO [a]
collectStreamHost s = do
  ref <- newIORef []
  runDrain (drainForEach (\x -> modifyIORef' ref (x :))) s
  reverse <$> readIORef ref

listToStreamHost :: [a] -> Stream IO a
listToStreamHost xs0 =
  statefulS (newIORef xs0) $ \ref -> ContT $ \kStep -> do
    xs <- readIORef ref
    case xs of
      [] -> kStep Stop
      (y : ys) -> writeIORef ref ys >> kStep (Yield y)

--------------------------------------------------------------------------------
-- HKD Test Structs (`SoA`, `AoSoA 4`, `SOAOA 4`)
--------------------------------------------------------------------------------

data Vec2 f = Vec2
  { v2X :: !(f Int64),
    v2Y :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (Vec2 Identity)

deriving stock instance Show (Vec2 Identity)

instance JanusStruct Vec2

instance JanusHKD Vec2

newtype HotX f = HotX {hxVal :: f Int64}
  deriving stock (Generic)

deriving stock instance Eq (HotX Identity)

deriving stock instance Show (HotX Identity)

instance JanusStruct HotX

newtype ColdY f = ColdY {cyVal :: f Int64}
  deriving stock (Generic)

deriving stock instance Eq (ColdY Identity)

deriving stock instance Show (ColdY Identity)

instance JanusStruct ColdY

data SplitVec2 f = SplitVec2
  { svHot :: !(HotX f),
    svCold :: !(ColdY f)
  }
  deriving stock (Generic)

deriving stock instance Eq (SplitVec2 Identity)

deriving stock instance Show (SplitVec2 Identity)

instance JanusStruct SplitVec2

--------------------------------------------------------------------------------
-- 1. BitSet, BitTree, BloomFilter, & XorFilter SPMD / SIMD Entrypoints
--------------------------------------------------------------------------------

check_spmd_bitwise_and_filters :: Property
check_spmd_bitwise_and_filters = property $ do
  xsA <- forAll $ Gen.list (Range.linear 0 140) (Gen.int (Range.linear 0 800))
  xsB <- forAll $ Gen.list (Range.linear 0 140) (Gen.int (Range.linear 0 800))
  q4 <- forAll $ Gen.list (Range.singleton 4) (Gen.int64 (Range.linear (-10) 900))
  btItems <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 50000))
  btQ4 <- forAll $ Gen.list (Range.singleton 4) (Gen.int64 (Range.linear (-5) 60000))
  bfKeys <- forAll $ Gen.list (Range.linear 10 120) (Gen.int64 (Range.linear 1 50000))

  (diffList, xorList, pc4, pc8, disjOk, subOk, bsGatherOut, btGatherOut, bfHitOk, xfHitOk, liveEnd) <-
    liftIO $
      withAllocTracker $ \tracker -> do
        let alloc = statsAlloc tracker dynamicAlloc
            mkBS xs = BS.fromStream alloc (listToStreamHost [Identity (fromIntegral x) | x <- xs])
            toBS b = map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream b)

        bsA <- mkBS xsA
        bsB <- mkBS xsB
        bsD <- mkBS xsA
        BS.spmdDifferenceInto (Proxy @4) bsD bsB
        dOut <- toBS bsD

        bsX <- mkBS xsA
        BS.spmdXorInto (Proxy @8) alloc bsX bsB
        xOut <- toBS bsX

        Identity cnt4 <- BS.spmdPopcount (Proxy @4) bsA
        Identity cnt8 <- BS.spmdPopcount (Proxy @8) bsA
        Identity disjDB <- BS.spmdIsDisjointFrom (Proxy @4) bsD bsB
        Identity subDA <- BS.spmdIsSubsetOf (Proxy @8) bsD bsA
        bsQRes <- spmdHostToList <$> withSPMD (Proxy @4) (BS.testBitsSPMD bsA (spmdHostFromList q4))

        bt <- BT.fromStream alloc 65536 (listToStreamHost [Identity (fromIntegral x) | x <- btItems])
        btQRes <- spmdHostToList <$> withSPMD (Proxy @4) (BT.memberSPMD bt (spmdHostFromList btQ4))

        bf <- BF.newForCapacity @Int64 alloc 200
        forM_ bfKeys (BF.spmdInsert bf . Identity)
        bfSingleHits <- mapM (fmap runIdentity . BF.spmdMightContain bf . Identity) bfKeys
        bfU <- BF.newForCapacity @Int64 alloc 200
        BF.spmdUnionInto (Proxy @8) bfU bf
        Identity bfEst <- BF.spmdEstimatedPopcount (Proxy @8) bfU
        let bfProbe4 = take 4 (bfKeys ++ repeat 1)
        bfBatch4 <- spmdHostToList <$> withSPMD (Proxy @4) (BF.mightContainBatchSPMD (Proxy @4) bfU (spmdHostFromList bfProbe4))

        xf <- XF.fromStream @Int64 alloc (listToStreamHost (map Identity bfKeys))
        xfBatch4 <- spmdHostToList <$> withSPMD (Proxy @4) (XF.mightContainBatchSPMD (Proxy @4) xf (spmdHostFromList bfProbe4))

        XF.free alloc xf
        BF.free alloc bfU
        BF.free alloc bf
        BT.free alloc bt
        BS.free alloc bsX
        BS.free alloc bsD
        BS.free alloc bsB
        BS.free alloc bsA
        snap <- readAllocSnapshot tracker
        pure
          ( dOut,
            xOut,
            fromIntegral cnt4 :: Int,
            fromIntegral cnt8 :: Int,
            disjDB,
            subDA,
            bsQRes,
            btQRes,
            and bfSingleHits && and bfBatch4 && bfEst > 0,
            and xfBatch4,
            (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
          )

  let setA = IntSet.fromList xsA
      setB = IntSet.fromList xsB
      setBT = IntSet.fromList btItems
  diffList === IntSet.toAscList (IntSet.difference setA setB)
  xorList === IntSet.toAscList (IntSet.union (IntSet.difference setA setB) (IntSet.difference setB setA))
  pc4 === IntSet.size setA
  pc8 === IntSet.size setA
  assert disjOk
  assert subOk
  bsGatherOut === [q >= 0 && IntSet.member (fromIntegral q) setA | q <- q4]
  btGatherOut === [q >= 0 && q < 65536 && IntSet.member (fromIntegral q) setBT | q <- btQ4]
  assert bfHitOk
  assert xfHitOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 2. Sequential, Stack, & Priority Queue SPMD Entrypoints (All 4 SIMD Layouts!)
--------------------------------------------------------------------------------

check_spmd_sequential_stacks_and_heaps :: Property
check_spmd_sequential_stacks_and_heaps = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 70) (Gen.int64 (Range.linear (-100) 100))

  (dvLayoutsOk, svInlineOk, svHeapOk, stvOk, rbOk, dqOk, stkOk, pqRebuildOk, ipqOk, liveEnd) <-
    liftIO $
      withAllocTracker $ \tracker -> do
        let alloc = statsAlloc tracker dynamicAlloc
            expectedXform = [x * 3 + i | (i, x) <- zip [0 ..] xs]
            expectedSum = sum expectedXform

        -- 1. DynVec across Scalar, SoA, AoSoA 4, & SOAOA 4 + gatherScalarSPMD + scatterSPMD
        (dvSc :: DV.DynVec Identity Int64) <- DV.fromStream alloc (listToStreamHost (map Identity xs))
        DV.foreachSPMD (Proxy @4) dvSc (\idx v -> pure (v * 3 + idx))
        scSum <- gangReduceAdd <$> DV.foldSPMD (Proxy @4) dvSc 0 (\acc _ v -> pure (acc + v))

        (dvSoA :: DV.DynVec Identity (SoA Vec2)) <- DV.fromStream alloc (listToStreamHost [Vec2 (Identity x) (Identity (x + 1)) | x <- xs])
        DV.foreachSPMD (Proxy @4) dvSoA (\idx (Vec2 x y) -> pure (Vec2 (x * 3 + idx) (y + 2)))
        soaSum <- gangReduceAdd <$> DV.foldSPMD (Proxy @4) dvSoA 0 (\acc _ (Vec2 x _) -> pure (acc + x))

        (dvAoSoA :: DV.DynVec Identity (AoSoA 4 Vec2)) <- DV.fromStream alloc (listToStreamHost [Vec2 (Identity x) (Identity (x + 1)) | x <- xs])
        DV.foreachSPMD (Proxy @4) dvAoSoA (\idx (Vec2 x y) -> pure (Vec2 (x * 3 + idx) (y + 2)))
        aosoaSum <- gangReduceAdd <$> DV.foldSPMD (Proxy @4) dvAoSoA 0 (\acc _ (Vec2 x _) -> pure (acc + x))

        (dvSOAOA :: DV.DynVec Identity (SOAOA 4 SplitVec2)) <-
          DV.fromStream
            alloc
            (listToStreamHost [SplitVec2 (HotX (Identity x)) (ColdY (Identity (x + 1))) | x <- xs])
        DV.foreachSPMD (Proxy @4) dvSOAOA (\idx (SplitVec2 (HotX x) (ColdY y)) -> pure (SplitVec2 (HotX (x * 3 + idx)) (ColdY (y + 2))))
        soaoaSum <- gangReduceAdd <$> DV.foldSPMD (Proxy @4) dvSOAOA 0 (\acc _ (SplitVec2 (HotX x) _) -> pure (acc + x))

        gathOut <- spmdHostToList <$> withSPMD (Proxy @4) (DV.gatherScalarSPMD (-999) dvSc (spmdHostFromList [-1, 0, fromIntegral (length xs), fromIntegral (length xs - 1)]))

        -- 2. SmallVec (inline <= 8 & spilled > 8)
        let xsInline = take 5 xs
        (svIn :: SV.SmallVec 8 Identity Int64) <- SV.fromStream alloc (listToStreamHost (map Identity xsInline))
        SV.foreachSPMD (Proxy @4) svIn (\idx v -> pure (v * 2 + idx))
        Identity svInSum <- gangReduceAdd <$> SV.foldSPMD (Proxy @4) svIn 0 (\acc _ v -> pure (acc + v))

        let xsSpill = xs ++ [1 .. 10]
        (svSp :: SV.SmallVec 8 Identity Int64) <- SV.fromStream alloc (listToStreamHost (map Identity xsSpill))
        SV.foreachSPMD (Proxy @4) svSp (\idx v -> pure (v * 2 + idx))
        Identity svSpSum <- gangReduceAdd <$> SV.foldSPMD (Proxy @4) svSp 0 (\acc _ v -> pure (acc + v))

        -- 3. StableVec (chunk size 16 -> multiple chunks!)
        (stv :: STV.StableVec 4 Identity Int64) <- STV.fromStream alloc (listToStreamHost (map Identity xsSpill))
        STV.foreachSPMD (Proxy @4) stv (\idx v -> pure (v * 2 + idx))
        Identity stvSum <- gangReduceAdd <$> STV.foldSPMD (Proxy @4) stv 0 (\acc _ v -> pure (acc + v))

        -- 4. Wrapped RingBuffer & Deque
        rb <- RB.new @Int64 alloc 128
        forM_ [1 .. 50 :: Int64] (RB.pushOverwrite_ rb . Identity)
        forM_ [1 .. 35 :: Int] (\_ -> RB.tryPop 0 rb)
        forM_ xs (RB.pushOverwrite_ rb . Identity)
        rbBefore <- map runIdentity <$> collectStreamHost (RB.stream rb)
        RB.foreachSPMD (Proxy @4) rb (\idx v -> pure (v + idx))
        Identity rbSum <- gangReduceAdd <$> RB.foldSPMD (Proxy @4) rb 0 (\acc _ v -> pure (acc + v))

        dq <- DQ.new @Int64 alloc
        forM_ (take 10 xs) (DQ.pushFront alloc dq . Identity)
        forM_ (drop 10 xs) (DQ.pushBack alloc dq . Identity)
        dqBefore <- map runIdentity <$> collectStreamHost (DQ.stream dq)
        DQ.foreachSPMD (Proxy @4) dq (\idx v -> pure (v + idx))
        Identity dqSum <- gangReduceAdd <$> DQ.foldSPMD (Proxy @4) dq 0 (\acc _ v -> pure (acc + v))

        -- 5. Stack & SmallStack
        (stk :: STK.Stack Identity Int64) <- STK.fromStream alloc (listToStreamHost (map Identity xs))
        STK.foreachSPMD (Proxy @4) stk (\_ v -> pure (v + 5))
        Identity stkSum <- gangReduceAdd <$> STK.foldSPMD (Proxy @4) stk 0 (\acc _ v -> pure (acc + v))

        (sstk :: SSTK.SmallStack 8 Identity Int64) <- SSTK.fromStream alloc (listToStreamHost (map Identity xsInline))
        SSTK.foreachSPMD (Proxy @4) sstk (\_ v -> pure (v + 5))
        Identity sstkSum <- gangReduceAdd <$> SSTK.foldSPMD (Proxy @4) sstk 0 (\acc _ v -> pure (acc + v))

        -- 6. PriorityQueue (mapKeysAndRebuildSPMD) & IndexedPQ (containsIdsSPMD)
        (pq :: PQ.PriorityQueue Identity Int64 Int64) <- PQ.fromStream alloc (listToStreamHost [(Identity x, Identity i) | (i, x) <- zip [0 ..] xs])
        PQ.foreachValuesSPMD (Proxy @4) pq (\_ v -> pure (v + 10))
        -- Negate every key in SIMD and rebuild 4-ary heap order in O(N)!
        PQ.mapKeysAndRebuildSPMD (Proxy @4) pq (\_ k -> pure (0 - k))
        Identity pqKeySum <- gangReduceAdd <$> PQ.foldKeysSPMD (Proxy @4) pq 0 (\acc _ k -> pure (acc + k))
        pqPopped <- map (runIdentity . fst) <$> collectStreamHost (PQ.popAllS pq)

        ipq <- IPQ.new @Int64 @Int64 alloc
        IPQ.insertOrUpdate alloc ipq 2 10 100
        IPQ.insertOrUpdate alloc ipq 7 20 200
        IPQ.foreachValuesSPMD (Proxy @4) ipq (\_ v -> pure (v * 2))
        Identity ipqPSum <- gangReduceAdd <$> IPQ.foldPrioritiesSPMD (Proxy @4) ipq 0 (\acc _ p -> pure (acc + p))
        ipqHas <- spmdHostToList <$> withSPMD (Proxy @4) (IPQ.containsIdsSPMD ipq (spmdHostFromList [-1, 2, 5, 7]))

        IPQ.free alloc ipq
        PQ.free alloc pq
        SSTK.free alloc sstk
        STK.free alloc stk
        DQ.free alloc dq
        RB.free alloc rb
        STV.free alloc stv
        SV.free alloc svSp
        SV.free alloc svIn
        DV.free alloc dvSOAOA
        DV.free alloc dvAoSoA
        DV.free alloc dvSoA
        DV.free alloc dvSc
        snap <- readAllocSnapshot tracker

        let expInSum = sum [x * 2 + i | (i, x) <- zip [0 ..] xsInline]
            expSpSum = sum [x * 2 + i | (i, x) <- zip [0 ..] xsSpill]
            expRbSum = sum [x + i | (i, x) <- zip [0 ..] rbBefore]
            expDqSum = sum [x + i | (i, x) <- zip [0 ..] dqBefore]
            lastXform = expectedXform !! (length xs - 1)
            firstXform = case expectedXform of (h : _) -> h; [] -> 0
        pure
          ( runIdentity scSum == expectedSum
              && runIdentity soaSum == expectedSum
              && runIdentity aosoaSum == expectedSum
              && runIdentity soaoaSum == expectedSum
              && gathOut == [-999, firstXform, -999, lastXform],
            svInSum == expInSum,
            svSpSum == expSpSum,
            stvSum == expSpSum,
            rbSum == expRbSum,
            dqSum == expDqSum,
            stkSum == sum [x + 5 | x <- xs] && sstkSum == sum [x + 5 | x <- xsInline],
            pqPopped == sort (map negate xs) && pqKeySum == sum (map negate xs),
            ipqPSum == 30 && ipqHas == [False, True, False, True],
            (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
          )

  assert dvLayoutsOk
  assert svInlineOk
  assert svHeapOk
  assert stvOk
  assert rbOk
  assert dqOk
  assert stkOk
  assert pqRebuildOk
  assert ipqOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. Hash, Insertion-Ordered, Eviction-Cache, & Ordered SPMD Entrypoints
--------------------------------------------------------------------------------

check_spmd_hash_cache_and_ordered :: Property
check_spmd_hash_cache_and_ordered = property $ do
  kvs <- forAll $ Gen.list (Range.linear 4 65) ((,) <$> Gen.int64 (Range.linear 0 120) <*> Gen.int64 (Range.linear (-50) 50))
  q4 <- forAll $ Gen.list (Range.singleton 4) (Gen.int64 (Range.linear (-5) 130))

  (hmOk, hsOk, imOk, isOk, cacheOk, fmOk, fsOk, btreeOk, liveEnd) <-
    liftIO $
      withAllocTracker $ \tracker -> do
        let alloc = statsAlloc tracker dynamicAlloc
            refMap = Map.fromList kvs
            delKeys = take (Map.size refMap `div` 3) (Map.keys refMap)
            liveMap = foldr Map.delete refMap delKeys
            expectedValSumAfterMul2 = sum [v * 2 | v <- Map.elems liveMap]
            expectedKeySum = sum (Map.keys liveMap)

        -- 1. HashMap & HashSet (including tombstones from deletes!)
        (hm :: HM.HashMap Identity Int64 Int64) <- HM.fromStream alloc (listToStreamHost [(Identity k, Identity v) | (k, v) <- kvs])
        forM_ delKeys (HM.delete hm . Identity)
        HM.foreachValuesSPMD (Proxy @4) hm (\_ v -> pure (v * 2))
        Identity hmSum <- gangReduceAdd <$> HM.foldValuesSPMD (Proxy @4) hm 0 (\acc _ v -> pure (acc + v))
        (hmFnd, hmVals) <- withSPMD (Proxy @4) (HM.lookupScalarBatchSPMD (Proxy @4) (-777) hm (spmdHostFromList q4))

        (hs :: HS.HashSet Identity Int64) <- HS.fromStream alloc (listToStreamHost [Identity k | (k, _) <- kvs])
        forM_ delKeys (HS.delete hs . Identity)
        Identity hsSum <- gangReduceAdd <$> HS.foldOccupiedSPMD (Proxy @4) hs 0 (\acc _ k -> pure (acc + k))
        hsMem <- spmdHostToList <$> withSPMD (Proxy @4) (HS.memberBatchSPMD (Proxy @4) hs (spmdHostFromList q4))

        -- 2. IndexMap & IndexSet
        (im :: IM.IndexMap Identity Int64 Int64) <- IM.fromStream alloc (listToStreamHost [(Identity k, Identity v) | (k, v) <- Map.toList liveMap])
        IM.foreachValuesSPMD (Proxy @4) im (\_ v -> pure (v * 2))
        Identity imSum <- gangReduceAdd <$> IM.foldValuesSPMD (Proxy @4) im 0 (\acc _ v -> pure (acc + v))
        (imFnd, imVals) <- withSPMD (Proxy @4) (IM.lookupScalarBatchSPMD (Proxy @4) (-777) im (spmdHostFromList q4))

        (iset :: IS.IndexSet Identity Int64) <- IS.fromStream alloc (listToStreamHost [Identity k | k <- Map.keys liveMap])
        Identity isSum <- gangReduceAdd <$> IS.foldSPMD (Proxy @4) iset 0 (\acc _ k -> pure (acc + k))
        isMem <- spmdHostToList <$> withSPMD (Proxy @4) (IS.memberBatchSPMD (Proxy @4) iset (spmdHostFromList q4))

        -- 3. ClockCache & LruCache (occupancy-masked SPMD!)
        cc <- CC.new @Int64 @Int64 alloc 16
        lru <- LRU.new @Int64 @Int64 alloc 16
        forM_ (take 8 (Map.toList liveMap)) $ \(k, v) -> do
          CC.insert cc (Identity k) (Identity v)
          LRU.insert lru (Identity k) (Identity v)
        let cachedSum0 = sum (map snd (take 8 (Map.toList liveMap)))
        CC.foreachValuesSPMD (Proxy @4) cc (\_ v -> pure (v * 3))
        LRU.foreachValuesSPMD (Proxy @4) lru (\_ v -> pure (v * 3))
        Identity ccSum <- gangReduceAdd <$> CC.foldValuesSPMD (Proxy @4) cc 0 (\acc _ v -> pure (acc + v))
        Identity lruSum <- gangReduceAdd <$> LRU.foldValuesSPMD (Proxy @4) lru 0 (\acc _ v -> pure (acc + v))

        -- 4. FlatMap & FlatSet (branchless lane-parallel binary search!)
        (fm :: FM.FlatMap Identity Int64 Int64) <- FM.fromStream alloc (listToStreamHost [(Identity k, Identity v) | (k, v) <- Map.toList liveMap])
        FM.foreachValuesSPMD (Proxy @4) fm (\_ v -> pure (v * 2))
        Identity fmSum <- gangReduceAdd <$> FM.foldValuesSPMD (Proxy @4) fm 0 (\acc _ v -> pure (acc + v))
        (fmFnd, fmVals) <- withSPMD (Proxy @4) (FM.lookupScalarBatchSPMD (Proxy @4) (-777) fm (spmdHostFromList q4))

        (fs :: FS.FlatSet Identity Int64) <- FS.fromStream alloc (listToStreamHost [Identity k | k <- Map.keys liveMap])
        Identity fsSum <- gangReduceAdd <$> FS.foldSPMD (Proxy @4) fs 0 (\acc _ k -> pure (acc + k))
        fsMem <- spmdHostToList <$> withSPMD (Proxy @4) (FS.memberBatchSPMD (Proxy @4) fs (spmdHostFromList q4))

        -- 5. BTreeMap & BTreeSet (multi-leaf 16-slot chunked SIMD!)
        (btm :: BTM.BTreeMap Identity Int64 Int64) <- BTM.fromStream alloc (listToStreamHost [(Identity k, Identity v) | (k, v) <- Map.toList liveMap])
        BTM.foreachValuesSPMD (Proxy @4) btm (\_ v -> pure (v * 2))
        Identity btmSum <- gangReduceAdd <$> BTM.foldValuesSPMD (Proxy @4) btm 0 (\acc _ v -> pure (acc + v))

        (bts :: BTS.BTreeSet Identity Int64) <- BTS.fromStream alloc (listToStreamHost [Identity k | k <- Map.keys liveMap])
        Identity btsSum <- gangReduceAdd <$> BTS.foldSPMD (Proxy @4) bts 0 (\acc _ k -> pure (acc + k))

        BTS.free alloc bts
        BTM.free alloc btm
        FS.free alloc fs
        FM.free alloc fm
        LRU.free alloc lru
        CC.free alloc cc
        IS.free alloc iset
        IM.free alloc im
        HS.free alloc hs
        HM.free alloc hm
        snap <- readAllocSnapshot tracker

        let expFnd = [Map.member q liveMap | q <- q4]
            expVals = [maybe (-777) (* 2) (Map.lookup q liveMap) | q <- q4]
        pure
          ( hmSum == expectedValSumAfterMul2 && spmdHostToList hmFnd == expFnd && spmdHostToList hmVals == expVals,
            hsSum == expectedKeySum && hsMem == expFnd,
            imSum == expectedValSumAfterMul2 && spmdHostToList imFnd == expFnd && spmdHostToList imVals == expVals,
            isSum == expectedKeySum && isMem == expFnd,
            ccSum == cachedSum0 * 3 && lruSum == cachedSum0 * 3,
            fmSum == expectedValSumAfterMul2 && spmdHostToList fmFnd == expFnd && spmdHostToList fmVals == expVals,
            fsSum == expectedKeySum && fsMem == expFnd,
            btmSum == expectedValSumAfterMul2 && btsSum == expectedKeySum,
            (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
          )

  assert hmOk
  assert hsOk
  assert imOk
  assert isOk
  assert cacheOk
  assert fmOk
  assert fsOk
  assert btreeOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 4. Partition, Graph, Spatial, Sparse, Pool, & Interner SPMD Entrypoints
--------------------------------------------------------------------------------

check_spmd_graph_spatial_sparse_pool_interner :: Property
check_spmd_graph_spatial_sparse_pool_interner = property $ do
  evKeysRaw <- forAll $ Gen.list (Range.linear 0 100) (Gen.int64 (Range.linear (-80) 80))
  q4 <- forAll $ Gen.list (Range.singleton 4) (Gen.int64 (Range.linear (-90) 90))
  intervals <-
    forAll $
      Gen.list
        (Range.linear 0 60)
        ( do
            a <- Gen.int64 (Range.linear (-50) 50)
            b <- Gen.int64 (Range.linear (-50) 50)
            pure (min a b, max a b, a + b)
        )

  (dsOk, csrOk, evOk, fimOk, smOk, sparseOk, poolOk, intrOk, liveEnd) <-
    liftIO $
      withAllocTracker $ \tracker -> do
        let alloc = statsAlloc tracker dynamicAlloc

        -- 1. DisjointSet
        ds <- DS.new alloc 12
        _ <- DS.union ds 0 1
        _ <- DS.union ds 1 2
        _ <- DS.union ds 5 6
        Identity spmdComps <- DS.spmdCountComponents (Proxy @4) ds
        Identity scalarComps <- DS.numSets ds
        conn4 <- spmdHostToList <$> withSPMD (Proxy @4) (DS.connectedSPMD ds (spmdHostFromList [0, 0, 5, -1]) (spmdHostFromList [2, 5, 6, 2]))
        roots4 <- spmdHostToList <$> withSPMD (Proxy @4) (DS.isRootSPMD ds (spmdHostFromList [3, 4, 7, 20]))

        -- 2. CsrGraph (spmdCsrSpMVVec & outDegreeSPMD)
        let nV = 8 :: Int64
            edges = [(0, 1, 2 :: Int64), (0, 3, 5), (2, 0, 4), (2, 4, 3), (2, 7, 1), (5, 2, 6)]
        (cg :: CSR.CsrGraph Identity Int64) <-
          CSR.fromEdgeStream
            alloc
            (Identity nV)
            (listToStreamHost [(Identity u, Identity v, Identity w) | (u, v, w) <- edges])
        CSR.foreachEdgeValuesSPMD (Proxy @4) cg (\_ w -> pure (w * 2))
        (xVec :: DV.DynVec Identity Int64) <- DV.fromStream alloc (listToStreamHost [Identity (i + 1) | i <- [0 .. nV - 1]])
        (ySpmd :: DV.DynVec Identity Int64) <- DV.fromStream alloc (listToStreamHost [0 | _ <- [0 .. nV - 1]])
        (yRef :: DV.DynVec Identity Int64) <- DV.fromStream alloc (listToStreamHost [0 | _ <- [0 .. nV - 1]])
        CSR.spmdCsrSpMVVec (Proxy @4) cg xVec ySpmd
        ScalarBuf xp <- DV.buf xVec
        ScalarBuf ypRef <- DV.buf yRef
        CSR.csrSpMV cg xp ypRef
        ySpmdList <- map runIdentity <$> collectStreamHost (DV.stream ySpmd)
        yRefList <- map runIdentity <$> collectStreamHost (DV.stream yRef)
        degs4 <- spmdHostToList <$> withSPMD (Proxy @4) (CSR.outDegreeSPMD cg (spmdHostFromList [0, 1, 2, 99]))

        -- 3. EytzingerVec (lockstep lane-parallel lookupScalarGESPMD & memberSPMD)
        (ev :: EV.EytzingerVec Identity Int64) <- EV.fromStream alloc (listToStreamHost (map Identity evKeysRaw))
        let evSet = Set.fromList evKeysRaw
        (evHasGe, evGeVals) <- withSPMD (Proxy @4) (EV.lookupScalarGESPMD (Proxy @4) (-999) ev (spmdHostFromList q4))
        evMem <- spmdHostToList <$> withSPMD (Proxy @4) (EV.memberSPMD (Proxy @4) ev (spmdHostFromList q4))

        -- 4. FlatIntervalMap (spmdCountOverlapping & spmdCountStabbed)
        (fim :: FIM.FlatIntervalMap Identity Int64 Int64) <-
          FIM.fromStream
            alloc
            (listToStreamHost [(Identity l, Identity r, Identity v) | (l, r, v) <- intervals])
        FIM.foreachValuesSPMD (Proxy @4) fim (\_ v -> pure (v + 1))
        Identity fimCntOv <- FIM.spmdCountOverlapping (Proxy @4) fim (uniform (-10)) (uniform 15)
        Identity fimRefOv <- FIM.countOverlapping fim (-10) 15
        Identity fimCntSt <- FIM.spmdCountStabbed (Proxy @4) fim (uniform 3)
        fimRefSt <- fromIntegral . length <$> collectStreamHost (FIM.pointStabStream fim 3)

        -- 5. SlotMap (SoA foreachValuesSPMD + containsKeysSPMD)
        sm <- SM.new @(SoA Vec2) alloc
        k0 <- SM.insert alloc sm (Vec2 10 20)
        k1 <- SM.insert alloc sm (Vec2 30 40)
        k2 <- SM.insert alloc sm (Vec2 50 60)
        _ <- SM.delete sm k1 -- k1 is now stale!
        SM.foreachValuesSPMD (Proxy @4) sm (\_ (Vec2 x y) -> pure (Vec2 (x + 5) (y + 5)))
        Identity smXSum <- gangReduceAdd <$> SM.foldValuesSPMD (Proxy @4) sm 0 (\acc _ (Vec2 x _) -> pure (acc + x))
        let idxList4 = [runIdentity (SM.skIndex k0), runIdentity (SM.skIndex k1), runIdentity (SM.skIndex k2), 999]
            genList4 = [runIdentity (SM.skGen k0), runIdentity (SM.skGen k1), runIdentity (SM.skGen k2), 1]
        smLive4 <- spmdHostToList <$> withSPMD (Proxy @4) (SM.containsKeysSPMD sm (spmdHostFromList idxList4) (spmdHostFromList genList4))

        -- 6. SparseSet & SparseVec (spmdDenseAxpyInto)
        ss <- SS.fromStream alloc (listToStreamHost [Identity i | i <- [2, 5, 9, 14, 21]])
        ssMem4 <- spmdHostToList <$> withSPMD (Proxy @4) (SS.memberSPMD ss (spmdHostFromList [2, 3, 14, -1]))
        (svec :: SVEC.SparseVec Identity Int64) <- SVEC.fromStream alloc (listToStreamHost [(1, 10), (3, 20), (6, 30)])
        SVEC.foreachValuesSPMD (Proxy @4) svec (\_ v -> pure (v * 2))
        axpyOut <-
          withAlignedArray [100, 100, 100, 100, 100, 100, 100, 100 :: Int64] $ \dstRaw -> do
            SVEC.spmdDenseAxpyInto (Proxy @4) 3 svec (Identity dstRaw)
            peekArray 8 dstRaw

        -- 7. Pool (multi-segment 64-slot occupancy bitmask SPMD with released holes!)
        pool <- POOL.new @Int64 alloc
        pHandles <- forM [1 .. 80 :: Int64] (POOL.acquire alloc pool . Identity)
        forM_ (take 15 (drop 10 pHandles)) (POOL.release pool)
        POOL.foreachOccupiedSPMD (Proxy @4) pool (\_ v -> pure (v * 10))
        Identity poolSpmdSum <- gangReduceAdd <$> POOL.foldOccupiedSPMD (Proxy @4) pool 0 (\acc _ v -> pure (acc + v))
        poolStreamVals <- map runIdentity <$> collectStreamHost (POOL.stream pool)

        -- 8. Interner
        intr <- INTR.new alloc
        s0 <- INTR.internString alloc intr "hello"
        s1 <- INTR.internString alloc intr ""
        s2 <- INTR.internString alloc intr "mechanically-sympathetic"
        let symW4 = [runIdentity (unSymbol s0), runIdentity (unSymbol s1), runIdentity (unSymbol s2), 999]
        intrLens4 <- spmdHostToList <$> withSPMD (Proxy @4) (INTR.symbolLengthsSPMD intr (spmdHostFromList symW4))
        Identity intrTotLen <- gangReduceAdd <$> INTR.foldSymbolLengthsSPMD (Proxy @4) intr 0 (\acc _ l -> pure (acc + l))

        INTR.free alloc intr
        POOL.free alloc pool
        SVEC.free alloc svec
        SS.free alloc ss
        SM.free alloc sm
        FIM.free alloc fim
        EV.free alloc ev
        DV.free alloc yRef
        DV.free alloc ySpmd
        DV.free alloc xVec
        CSR.free alloc cg
        DS.free alloc ds
        snap <- readAllocSnapshot tracker

        let expEvGe = [maybe (False, -999) (\v -> (True, v)) (Set.lookupGE q evSet) | q <- q4]
            expEvMem = [Set.member q evSet | q <- q4]
        pure
          ( spmdComps == scalarComps && conn4 == [True, False, True, False] && roots4 == [True, True, True, False],
            ySpmdList == yRefList && degs4 == [2, 0, 3, 0],
            zip (spmdHostToList evHasGe) (spmdHostToList evGeVals) == expEvGe && evMem == expEvMem,
            fimCntOv == fimRefOv && fimCntSt == fimRefSt,
            smXSum == (10 + 5) + (50 + 5) && smLive4 == [True, False, True, False],
            ssMem4 == [True, False, True, False] && axpyOut == [100, 160, 100, 220, 100, 100, 280, 100],
            poolSpmdSum == sum poolStreamVals,
            intrLens4 == [5, 0, 24, 0] && intrTotLen == 29,
            (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
          )

  assert dsOk
  assert csrOk
  assert evOk
  assert fimOk
  assert smOk
  assert sparseOk
  assert poolOk
  assert intrOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 5. Host <-> Compiled x86-64 C SPMD Kernel Across Multiple Containers
--------------------------------------------------------------------------------

cMultiContainerSPMDKernel ::
  BS.BitSet LinuxX86_64CCSE ->
  DV.DynVec LinuxX86_64CCSE (SoA Vec2) ->
  HM.HashMap LinuxX86_64CCSE Int64 Int64 ->
  EV.EytzingerVec LinuxX86_64CCSE Int64 ->
  POOL.Pool LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMultiContainerSPMDKernel bs dv hm ev pool = do
  -- 1. In-place SoA DynVec update gated by lane-gather BitSet membership (`testBitsSPMD`)!
  DV.foreachSPMD (Proxy @4) dv $ \idx (Vec2 x y) -> do
    active <- BS.testBitsSPMD bs idx
    pure (Vec2 (ifThenElse active (x + y) x) (y * 2))
  -- 2. Masked HashMap value doubling in SIMD
  HM.foreachValuesSPMD (Proxy @4) hm (\_ v -> pure (v * 2))
  -- 3. Bitmask-gated Pool increment in SIMD
  POOL.foreachOccupiedSPMD (Proxy @4) pool (\_ v -> pure (v + 7))
  -- 4. Vectorized BitSet popcount + EytzingerVec batch membership count in SIMD
  pc <- BS.spmdPopcount (Proxy @4) bs
  evHits <-
    runDrain drainSum $
      streamM (rangeS (0 :: LinuxX86_64CCSE Int64) 4) $ \g -> do
        let qGang = spmdAlignedUnitStride (g * 4)
        m <- withSPMD (Proxy @4) (EV.memberSPMD (Proxy @4) ev qGang)
        pure (gangReduceAdd (fromBool m :: SPMD 4 LinuxX86_64CCSE Int64))
  hmSum <- gangReduceAdd <$> HM.foldValuesSPMD (Proxy @4) hm 0 (\acc _ v -> pure (acc + v))
  poolSum <- gangReduceAdd <$> POOL.foldOccupiedSPMD (Proxy @4) pool 0 (\acc _ v -> pure (acc + v))
  pure (pc + evHits * 100 + hmSum + poolSum)

check_spmd_containers_compiled_c_ffi ::
  IO
    ( BS.BitSet Identity ->
      DV.DynVec Identity (SoA Vec2) ->
      HM.HashMap Identity Int64 Int64 ->
      EV.EytzingerVec Identity Int64 ->
      POOL.Pool Identity Int64 ->
      IO Int64
    ) ->
  Property
check_spmd_containers_compiled_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  bits <- forAll $ Gen.list (Range.linear 1 30) (Gen.int (Range.linear 0 120))
  hmPairs <- forAll $ Gen.list (Range.linear 1 30) ((,) <$> Gen.int64 (Range.linear 0 80) <*> Gen.int64 (Range.linear 1 50))
  poolItems <- forAll $ Gen.list (Range.linear 1 30) (Gen.int64 (Range.linear 1 50))

  (cRes, expectedRes, dvXOut, expectedDvX) <- liftIO $ do
    bs <- BS.fromStream dynamicAlloc (listToStreamHost [Identity (fromIntegral b) | b <- bits])
    (dv :: DV.DynVec Identity (SoA Vec2)) <- DV.fromStream dynamicAlloc (listToStreamHost [Vec2 (Identity i) (Identity (i * 3)) | i <- [0 .. 18 :: Int64]])
    (hm :: HM.HashMap Identity Int64 Int64) <- HM.fromStream dynamicAlloc (listToStreamHost [(Identity k, Identity v) | (k, v) <- hmPairs])
    (ev :: EV.EytzingerVec Identity Int64) <- EV.fromStream dynamicAlloc (listToStreamHost [Identity (i * 2) | i <- [0 .. 15 :: Int64]])
    (pool :: POOL.Pool Identity Int64) <- POOL.fromStream dynamicAlloc (listToStreamHost (map Identity poolItems))

    tot <- cFn bs dv hm ev pool
    dvAfter <- collectStreamHost (DV.stream dv)

    POOL.free dynamicAlloc pool
    EV.free dynamicAlloc ev
    HM.free dynamicAlloc hm
    DV.free dynamicAlloc dv
    BS.free dynamicAlloc bs

    let bitSetRef = IntSet.fromList bits
        expPc = fromIntegral (IntSet.size bitSetRef) :: Int64
        -- Queries `0 .. 15`, while `ev` holds even numbers `0, 2, ..., 30` -> 8 hits (`0, 2, 4, 6, 8, 10, 12, 14`)!
        expEvHits = 8 :: Int64
        expHm = sum [v * 2 | v <- Map.elems (Map.fromList hmPairs)]
        expPool = sum [p + 7 | p <- poolItems]
    pure
      ( tot,
        expPc + expEvHits * 100 + expHm + expPool,
        [runIdentity x | Vec2 x _ <- dvAfter],
        [if IntSet.member (fromIntegral i) bitSetRef then i + i * 3 else i | i <- [0 .. 18 :: Int64]]
      )

  cRes === expectedRes
  dvXOut === expectedDvX

test_data_spmd :: TestTree
test_data_spmd =
  let acq = getSPMDDataTestConfig >>= flip acquireJanusC cMultiContainerSPMDKernel
   in withResource acq releaseJanusC $ \rFn ->
        testGroup
          "First-Class SPMD / SIMD Support Across Janus Data Structures"
          [ testProperty "BitSet, BitTree, BloomFilter (@8 cache-line), & XorFilter SPMD" check_spmd_bitwise_and_filters,
            testProperty "DynVec (Scalar/SoA/AoSoA/SOAOA), SmallVec, StableVec, RingBuffer, Deque, Stack, & Heaps SPMD" check_spmd_sequential_stacks_and_heaps,
            testProperty "HashMap, HashSet, IndexMap, IndexSet, ClockCache, LruCache, FlatMap/Set, & BTreeMap/Set SPMD" check_spmd_hash_cache_and_ordered,
            testProperty "DisjointSet, CsrGraph SpMV, EytzingerVec, FlatIntervalMap, SlotMap, SparseSet/Vec, Pool, & Interner SPMD" check_spmd_graph_spatial_sparse_pool_interner,
            testProperty "Host <-> Compiled x86-64 C Multi-Container SPMD Kernel" (check_spmd_containers_compiled_c_ffi (fst <$> rFn))
          ]
