{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.BitAndFilter where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.IntSet as IntSet
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Control.Drain (drainCountWhere, drainForEach, drainSum, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, rangeS, statefulS, streamM)
import Janus.Data (JanusStruct, SoA)
import Janus.Data.BitSet (BitSet)
import qualified Janus.Data.BitSet as BS
import Janus.Data.BitTree (BitTree)
import qualified Janus.Data.BitTree as BT
import Janus.Data.BloomFilter (BloomFilter)
import qualified Janus.Data.BloomFilter as BF
import Janus.Data.XorFilter (XorFilter)
import qualified Janus.Data.XorFilter as XF
import Janus.Fix (fun)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getBitFilterTestConfig :: IO CBuildConfig
getBitFilterTestConfig = do
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

foldlM' :: (Monad m) => (s -> a -> m s) -> s -> [a] -> m s
foldlM' _ acc [] = pure acc
foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 1. BitSet Stateful Trace, Range Masks, SWAR Scans, & Word-Skipping Streams
--------------------------------------------------------------------------------

data BitCmd
  = BSet !Int
  | BClearBit !Int
  | BToggle !Int
  | BSetRange !Int !Int
  | BQuery !Int
  | BClearAll
  deriving (Eq, Show)

check_bitset_stateful_and_algebra :: Property
check_bitset_stateful_and_algebra = property $ do
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 180)
        ( Gen.frequency
            [ (6, BSet <$> Gen.int (Range.linear 0 520)),
              (5, BClearBit <$> Gen.int (Range.linear 0 520)),
              (4, BToggle <$> Gen.int (Range.linear 0 520)),
              ( 3,
                BSetRange
                  <$> Gen.int (Range.linear 0 400)
                  <*> Gen.int (Range.linear 0 460)
              ),
              (4, BQuery <$> Gen.int (Range.linear (-5) 540)),
              (1, pure BClearAll)
            ]
        )
  extraB <- forAll $ Gen.list (Range.linear 0 90) (Gen.int (Range.linear 0 600))

  (ascOut, descOut, refAsc, uStream, iStream, dStream, uMut, iMut, dMut, xMut, relOk, liveEnd) <-
    liftIO $
      withAllocTracker $ \tracker -> do
        let alloc = statsAlloc tracker dynamicAlloc
        bs <- BS.new alloc
        -- Explicitly exercise exact 64-bit word boundary ranges first (`[0, 64)`, `[64, 128)`, `[64, 64)`)
        BS.setRange alloc bs 0 64
        BS.setRange alloc bs 64 128
        BS.setRange alloc bs 64 64
        BS.clear bs

        let step s cmd = case cmd of
              BSet b -> do
                Identity prev <- BS.setBit alloc bs (Identity (fromIntegral b))
                if prev == IntSet.member b s
                  then pure (IntSet.insert b s)
                  else error "BitSet setBit previous Bool mismatch"
              BClearBit b -> do
                Identity prev <- BS.clearBit bs (Identity (fromIntegral b))
                if prev == IntSet.member b s
                  then pure (IntSet.delete b s)
                  else error "BitSet clearBit previous Bool mismatch"
              BToggle b -> do
                BS.toggleBit alloc bs (Identity (fromIntegral b))
                let s' = if IntSet.member b s then IntSet.delete b s else IntSet.insert b s
                pure s'
              BSetRange lo hi -> do
                BS.setRange alloc bs (Identity (fromIntegral lo)) (Identity (fromIntegral hi))
                let s' =
                      if lo < hi
                        then IntSet.union s (IntSet.fromAscList [max 0 lo .. hi - 1])
                        else s
                pure s'
              BQuery q -> do
                Identity tb <- BS.testBit bs (Identity (fromIntegral q))
                let expectedTb = q >= 0 && IntSet.member q s
                (Identity hasFst, Identity fstV) <- BS.findFirstSet bs
                (Identity hasLst, Identity lstV) <- BS.findLastSet bs
                (Identity hasGe, Identity geV) <- BS.findNextSetGe bs (Identity (fromIntegral q))
                Identity pc <- BS.popcount bs
                Identity emp <- BS.isEmpty bs
                let minOk = case fst <$> IntSet.minView s of
                      Nothing -> not hasFst
                      Just m -> hasFst && fromIntegral fstV == m
                    maxOk = case fst <$> IntSet.maxView s of
                      Nothing -> not hasLst
                      Just m -> hasLst && fromIntegral lstV == m
                    geOk = case IntSet.lookupGE (max 0 q) s of
                      Nothing -> not hasGe
                      Just g -> hasGe && fromIntegral geV == g
                if tb == expectedTb
                  && minOk
                  && maxOk
                  && geOk
                  && fromIntegral pc == IntSet.size s
                  && emp == IntSet.null s
                  then pure s
                  else error "BitSet query/scan mismatch"
              BClearAll -> BS.clear bs >> pure IntSet.empty

        sEnd <- foldlM' step IntSet.empty cmds
        asc <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bs)
        desc <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.reverseStream bs)

        bsB <- BS.fromStream alloc (listToStreamHost [Identity (fromIntegral x) | x <- extraB])
        uS <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.unionS bs bsB)
        iS <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.intersectionS bs bsB)
        dS <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.differenceS bs bsB)

        bsU <- BS.fromStream alloc (BS.stream bs)
        BS.unionInto alloc bsU bsB
        uM <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bsU)

        bsI <- BS.fromStream alloc (BS.stream bs)
        BS.intersectInto bsI bsB
        iM <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bsI)

        bsD <- BS.fromStream alloc (BS.stream bs)
        BS.differenceInto bsD bsB
        dM <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bsD)

        bsX <- BS.fromStream alloc (BS.stream bs)
        BS.xorInto alloc bsX bsB
        xM <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bsX)

        Identity eqSelf <- BS.equals bs bs
        Identity subU <- BS.isSubsetOf bs bsU
        Identity subI <- BS.isSubsetOf bsI bs
        Identity disjDI <- BS.isDisjointFrom bsD bsB

        BS.free alloc bsX
        BS.free alloc bsD
        BS.free alloc bsI
        BS.free alloc bsU
        BS.free alloc bsB
        BS.free alloc bs
        snap <- readAllocSnapshot tracker
        pure
          ( asc,
            desc,
            IntSet.toAscList sEnd,
            uS,
            iS,
            dS,
            uM,
            iM,
            dM,
            xM,
            eqSelf && subU && subI && disjDI,
            (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
          )

  let setA = IntSet.fromAscList refAsc
      setB = IntSet.fromList extraB
      refU = IntSet.toAscList (IntSet.union setA setB)
      refI = IntSet.toAscList (IntSet.intersection setA setB)
      refD = IntSet.toAscList (IntSet.difference setA setB)
      refX = IntSet.toAscList (IntSet.union (IntSet.difference setA setB) (IntSet.difference setB setA))
  ascOut === refAsc
  descOut === reverse refAsc
  uStream === refU
  iStream === refI
  dStream === refD
  uMut === refU
  iMut === refI
  dMut === refD
  xMut === refX
  assert relOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 2. BitSet SPMD Gang-Accelerated `spmdUnionInto` & `spmdIntersectInto` (@4 & @8)
--------------------------------------------------------------------------------

check_bitset_spmd_union_and_intersect :: Property
check_bitset_spmd_union_and_intersect = property $ do
  maxA <- forAll $ Gen.int (Range.linear 10 900)
  maxB <- forAll $ Gen.int (Range.linear 10 900)
  xsA <- forAll $ Gen.list (Range.linear 0 120) (Gen.int (Range.linear 0 maxA))
  xsB <- forAll $ Gen.list (Range.linear 0 120) (Gen.int (Range.linear 0 maxB))

  (u4, u8, i4, i8, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
          mkBS xs = BS.fromStream alloc (listToStreamHost [Identity (fromIntegral x) | x <- xs])
          toListBS b = map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream b)

      bsA <- mkBS xsA
      bsB <- mkBS xsB

      bsU4 <- mkBS xsA
      BS.spmdUnionInto (Proxy @4) alloc bsU4 bsB
      outU4 <- toListBS bsU4

      bsU8 <- mkBS xsA
      BS.spmdUnionInto (Proxy @8) alloc bsU8 bsB
      outU8 <- toListBS bsU8

      bsI4 <- mkBS xsA
      BS.spmdIntersectInto (Proxy @4) alloc bsI4 bsB
      outI4 <- toListBS bsI4

      bsI8 <- mkBS xsA
      BS.spmdIntersectInto (Proxy @8) alloc bsI8 bsB
      outI8 <- toListBS bsI8

      BS.free alloc bsI8
      BS.free alloc bsI4
      BS.free alloc bsU8
      BS.free alloc bsU4
      BS.free alloc bsB
      BS.free alloc bsA
      snap <- readAllocSnapshot tracker
      pure
        ( outU4,
          outU8,
          outI4,
          outI8,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  let refA = IntSet.fromList xsA
      refB = IntSet.fromList xsB
      expectedU = IntSet.toAscList (IntSet.union refA refB)
      expectedI = IntSet.toAscList (IntSet.intersection refA refB)
  u4 === expectedU
  u8 === expectedU
  i4 === expectedI
  i8 === expectedI
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. BitTree 3-Level 64-Ary Pyramid Up to 262,144 (`findNextSetGe` & `popMin`)
--------------------------------------------------------------------------------

check_bittree_stateful_full_universe_and_popmin :: Property
check_bittree_stateful_full_universe_and_popmin = property $ do
  ops <-
    forAll $
      Gen.list
        (Range.linear 1 220)
        ( (,)
            <$> Gen.int (Range.linear 0 4)
            <*> Gen.choice
              [ Gen.int (Range.linear (-4) 262148),
                Gen.element [0, 1, 63, 64, 4032, 4095, 4096, 258048, 262142, 262143, 262144]
              ]
        )

  (streamed, poppedAll, refAsc, emptyAfterPop, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
          uBound = 262144 :: Int
      bt <- BT.new alloc (Identity (fromIntegral uBound))
      let step s (op, x) = case op of
            0 -> do
              Identity ins <- BT.insert bt (Identity (fromIntegral x))
              let inBounds = x >= 0 && x < uBound
                  expectedIns = inBounds && not (IntSet.member x s)
                  s' = if inBounds then IntSet.insert x s else s
              if ins == expectedIns
                then pure s'
                else error "BitTree insert mismatch"
            1 -> do
              Identity remOk <- BT.delete bt (Identity (fromIntegral x))
              let inBounds = x >= 0 && x < uBound
                  expectedRem = inBounds && IntSet.member x s
                  s' = if inBounds then IntSet.delete x s else s
              if remOk == expectedRem
                then pure s'
                else error "BitTree delete mismatch"
            2 -> do
              Identity m <- BT.member bt (Identity (fromIntegral x))
              let expectedM = x >= 0 && x < uBound && IntSet.member x s
              if m == expectedM
                then pure s
                else error "BitTree member mismatch"
            3 -> do
              (Identity hasGe, Identity geV) <- BT.findNextSetGe bt (Identity (fromIntegral x))
              (Identity hasMin, Identity minV) <- BT.findFirstSet bt
              (Identity hasMax, Identity maxV) <- BT.findLastSet bt
              Identity sz <- BT.size bt
              Identity emp <- BT.isEmpty bt
              let geOk = case IntSet.lookupGE (max 0 x) s of
                    Nothing -> not hasGe
                    Just g -> hasGe && fromIntegral geV == g
                  minOk = case fst <$> IntSet.minView s of
                    Nothing -> not hasMin
                    Just mn -> hasMin && fromIntegral minV == mn
                  maxOk = case fst <$> IntSet.maxView s of
                    Nothing -> not hasMax
                    Just mx -> hasMax && fromIntegral maxV == mx
              if geOk && minOk && maxOk && fromIntegral sz == IntSet.size s && emp == IntSet.null s
                then pure s
                else error "BitTree successor/min/max/size mismatch"
            _ -> BT.clear bt >> pure IntSet.empty

      sEnd <- foldlM' step IntSet.empty ops
      stOut <- map (fromIntegral . runIdentity) <$> collectStreamHost (BT.stream bt)

      -- Clone via `fromStream` and drain completely via O(1) `popMin`!
      btClone <- BT.fromStream alloc 262144 (BT.stream bt)
      let drainPopMin acc = do
            (Identity ok, Identity v) <- BT.popMin btClone
            if ok
              then drainPopMin (fromIntegral v : acc)
              else pure (reverse acc)
      popped <- drainPopMin []
      Identity empEnd <- BT.isEmpty btClone
      Identity szEnd <- BT.size btClone

      BT.free alloc btClone
      BT.free alloc bt
      snap <- readAllocSnapshot tracker
      pure
        ( stOut,
          popped,
          IntSet.toAscList sEnd,
          empEnd && szEnd == 0,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  streamed === refAsc
  poppedAll === refAsc
  assert emptyAfterPop
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 4. Split-Block BloomFilter Zero False Negatives, Empirical FPR < 0.05, & Union
--------------------------------------------------------------------------------

check_bloomfilter_zero_fn_fpr_and_union :: Property
check_bloomfilter_zero_fn_fpr_and_union = property $ do
  keysA <- forAll $ Gen.list (Range.linear 40 320) (Gen.int64 (Range.linear 0 100000))
  keysB <- forAll $ Gen.list (Range.linear 40 320) (Gen.int64 (Range.linear 0 100000))

  (allPresentA, fprA, estBeforeClear, estAfterClear, allPresentUnion, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      bfA <- BF.fromStream @Int64 alloc 400 (listToStreamHost (map Identity keysA))
      bfB <- BF.newForCapacity @Int64 alloc 400
      forM_ keysB (BF.insert bfB . Identity)

      hitsA <- mapM (fmap runIdentity . BF.mightContain bfA . Identity) keysA
      let probeRange = [200000 .. 201999 :: Int64]
      falsePosA <-
        length . filter id
          <$> mapM (fmap runIdentity . BF.mightContain bfA . Identity) probeRange

      Identity estPre <- BF.estimatedPopcount bfA

      bfUnion <- BF.newForCapacity @Int64 alloc 400
      BF.unionInto bfUnion bfA
      BF.unionInto bfUnion bfB
      hitsU <- mapM (fmap runIdentity . BF.mightContain bfUnion . Identity) (keysA ++ keysB)

      BF.clear bfA
      Identity estPost <- BF.estimatedPopcount bfA

      BF.free alloc bfUnion
      BF.free alloc bfB
      BF.free alloc bfA
      snap <- readAllocSnapshot tracker
      let rate = fromIntegral falsePosA / (2000.0 :: Double)
      pure
        ( and hitsA,
          rate,
          estPre,
          estPost,
          and hitsU,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  assert allPresentA
  assert (fprA < 0.05)
  assert (estBeforeClear > 0)
  estAfterClear === 0
  assert allPresentUnion
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 5. Static 8-Bit XorFilter Zero False Negatives, FPR < 0.02, Duplicates, & Empty
--------------------------------------------------------------------------------

data FilterKey2D f = FilterKey2D
  { fkX :: !(f Int64),
    fkY :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (FilterKey2D Identity)

deriving stock instance Show (FilterKey2D Identity)

instance JanusStruct FilterKey2D

check_xorfilter_zero_fn_fpr_duplicates_empty_and_leaks :: Property
check_xorfilter_zero_fn_fpr_duplicates_empty_and_leaks = property $ do
  rawKeys <- forAll $ Gen.list (Range.linear 1 420) (Gen.int64 (Range.linear (-100000) 100000))
  dupTake <- forAll $ Gen.int (Range.linear 0 80)

  (emptyOk, kcGot, allPresent, hkdAllPresent, fprXor, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc

      -- Phase A: Empty stream
      xfEmpty <- XF.fromStream @Int64 alloc (listToStreamHost [])
      Identity empCnt <- XF.keyCount xfEmpty
      Identity empBytes <- XF.byteSize xfEmpty
      empHits <- mapM (fmap runIdentity . XF.mightContain xfEmpty . Identity) [0 .. 31 :: Int64]
      XF.free alloc xfEmpty

      -- Phase B: Non-empty stream with deliberate duplicates
      let streamInput = rawKeys ++ take dupTake rawKeys
      xf <- XF.fromStream @Int64 alloc (listToStreamHost (map Identity streamInput))
      Identity kc <- XF.keyCount xf
      memberHits <- mapM (fmap runIdentity . XF.mightContain xf . Identity) rawKeys

      let nonMembers = [500000 .. 502499 :: Int64]
      fpCnt <-
        length . filter id
          <$> mapM (fmap runIdentity . XF.mightContain xf . Identity) nonMembers
      XF.free alloc xf

      -- Phase C: HKD SoA 2D struct keys in both BloomFilter & XorFilter
      let hkdKeys = [FilterKey2D (Identity k) (Identity (k * 3 + 7)) | k <- take 60 rawKeys]
      bfHkd <- BF.fromStream @(SoA FilterKey2D) alloc 64 (listToStreamHost hkdKeys)
      xfHkd <- XF.fromStream @(SoA FilterKey2D) alloc (listToStreamHost hkdKeys)
      bfHkdHits <- mapM (fmap runIdentity . BF.mightContain bfHkd) hkdKeys
      xfHkdHits <- mapM (fmap runIdentity . XF.mightContain xfHkd) hkdKeys
      XF.free alloc xfHkd
      BF.free alloc bfHkd

      snap <- readAllocSnapshot tracker
      let empCheck = empCnt == 0 && empBytes == 0 && not (or empHits)
          rate = fromIntegral fpCnt / (2500.0 :: Double)
      pure
        ( empCheck,
          kc,
          and memberHits,
          and bfHkdHits && and xfHkdHits,
          rate,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  let uniqueCnt = fromIntegral (Set.size (Set.fromList rawKeys)) :: Int64
  assert emptyOk
  kcGot === uniqueCnt
  assert allPresent
  assert hkdAllPresent
  assert (fprXor < 0.02)
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 6. Host <-> Compiled C FFI Round-Trip (`BitSet`, `BitTree`, `BloomFilter`, `XorFilter`)
--------------------------------------------------------------------------------

cMutateAndProbeBitFilter ::
  BitSet LinuxX86_64CCSE ->
  BitTree LinuxX86_64CCSE ->
  BloomFilter LinuxX86_64CCSE Int64 ->
  XorFilter LinuxX86_64CCSE Int64 ->
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMutateAndProbeBitFilter bs bt bf xf n = do
  let bsPhase ::
        BitSet LinuxX86_64CCSE ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      bsPhase =
        fun (Proxy @LinuxX86_64CCSE) "bf_bs_phase" $ \_ bs' n' -> do
          tmpBs <- BS.new dynamicAlloc
          runDrain
            (drainForEach (\i -> BS.setBit dynamicAlloc tmpBs (i * 7) >> pure ()))
            (rangeS (0 :: LinuxX86_64CCSE Int64) n')
          BS.spmdUnionInto (Proxy @8) dynamicAlloc bs' tmpBs
          BS.free dynamicAlloc tmpBs
          BS.popcount bs'

      btPhase ::
        BitTree LinuxX86_64CCSE ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      btPhase =
        fun (Proxy @LinuxX86_64CCSE) "bf_bt_phase" $ \_ bt' n' -> do
          runDrain
            (drainForEach (\i -> BT.insert bt' (i * 13) >> pure ()))
            (rangeS (0 :: LinuxX86_64CCSE Int64) n')
          runDrain drainSum (BT.stream bt')

      bloomPhase ::
        BloomFilter LinuxX86_64CCSE Int64 ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      bloomPhase =
        fun (Proxy @LinuxX86_64CCSE) "bf_bloom_phase" $ \_ bf' n' -> do
          runDrain
            (drainForEach (\i -> BF.insert bf' (i * 19 + 3)))
            (rangeS (0 :: LinuxX86_64CCSE Int64) n')
          runDrain (drainCountWhere id) $
            streamM (rangeS (0 :: LinuxX86_64CCSE Int64) n') (\i -> BF.mightContain bf' (i * 19 + 3))

      xorPhase ::
        XorFilter LinuxX86_64CCSE Int64 ->
        LinuxX86_64CCSE Int64 ->
        LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
      xorPhase =
        fun (Proxy @LinuxX86_64CCSE) "bf_xor_phase" $ \_ xf' n' ->
          runDrain (drainCountWhere id) $
            streamM (rangeS (0 :: LinuxX86_64CCSE Int64) n') (\i -> XF.mightContain xf' (i * 5 + 1))

  bsCnt <- bsPhase bs n
  btSum <- btPhase bt n
  bfHits <- bloomPhase bf n
  xfHits <- xorPhase xf n
  pure (bsCnt + btSum + bfHits * 1000 + xfHits * 10000)

cBuildXorFilterInC ::
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (XorFilter LinuxX86_64CCSE Int64)
cBuildXorFilterInC n =
  XF.fromStream @Int64 dynamicAlloc ((\i -> i * 11 + 7) <$> rangeS (0 :: LinuxX86_64CCSE Int64) n)

check_bit_and_filter_host_c_ffi ::
  IO
    ( BitSet Identity ->
      BitTree Identity ->
      BloomFilter Identity Int64 ->
      XorFilter Identity Int64 ->
      Int64 ->
      IO Int64
    ) ->
  IO (Int64 -> IO (XorFilter Identity Int64)) ->
  Property
check_bit_and_filter_host_c_ffi getMutFn getBuildXfFn = property $ do
  cMutFn <- liftIO getMutFn
  cBuildXfFn <- liftIO getBuildXfFn
  initBits <- forAll $ Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 350))
  n <- forAll $ Gen.int64 (Range.linear 0 60)

  (cRes, hsBsList, hsBtList, hsBfAllHit, cBuiltXfAllHit, cBuiltXfCnt) <- liftIO $ do
    bs <- BS.new dynamicAlloc
    forM_ initBits (BS.setBit dynamicAlloc bs . Identity . fromIntegral)
    bt <- BT.new dynamicAlloc 4096
    bf <- BF.newForCapacity @Int64 dynamicAlloc 256
    xf <- XF.fromStream @Int64 dynamicAlloc (listToStreamHost [Identity (i * 5 + 1) | i <- [0 .. n - 1]])

    tot <- cMutFn bs bt bf xf n
    bsOut <- map (fromIntegral . runIdentity) <$> collectStreamHost (BS.stream bs)
    btOut <- map runIdentity <$> collectStreamHost (BT.stream bt)
    bfOk <- and <$> mapM (\i -> runIdentity <$> BF.mightContain bf (Identity (i * 19 + 3))) [0 .. n - 1]

    XF.free dynamicAlloc xf
    BF.free dynamicAlloc bf
    BT.free dynamicAlloc bt
    BS.free dynamicAlloc bs

    xfFromC <- cBuildXfFn n
    Identity kcC <- XF.keyCount xfFromC
    cXfOk <- and <$> mapM (\i -> runIdentity <$> XF.mightContain xfFromC (Identity (i * 11 + 7))) [0 .. n - 1]
    XF.free dynamicAlloc xfFromC

    pure (tot, bsOut, btOut, bfOk, cXfOk, kcC)

  let expectedBs = IntSet.toAscList (IntSet.union (IntSet.fromList initBits) (IntSet.fromList [fromIntegral (i * 7) | i <- [0 .. n - 1]]))
      expectedBt = [i * 13 | i <- [0 .. n - 1]]
      expectedTot =
        fromIntegral (length expectedBs)
          + sum expectedBt
          + n * 1000
          + n * 10000
  hsBsList === expectedBs
  hsBtList === expectedBt
  assert hsBfAllHit
  assert cBuiltXfAllHit
  cBuiltXfCnt === n
  cRes === expectedTot

test_bit_and_filter :: TestTree
test_bit_and_filter =
  let acqMut = getBitFilterTestConfig >>= flip acquireJanusC cMutateAndProbeBitFilter
      acqBuild = getBitFilterTestConfig >>= flip acquireJanusC cBuildXorFilterInC
   in withResource acqMut releaseJanusC $ \rMut ->
        withResource acqBuild releaseJanusC $ \rBuild ->
          testGroup
            "BitSet, BitTree, Split-Block BloomFilter, & Lemire-Graf XorFilter"
            [ testProperty "BitSet stateful trace, range masks, SWAR scans, & word-level streams" check_bitset_stateful_and_algebra,
              testProperty "BitSet SPMD gang-accelerated spmdUnionInto & spmdIntersectInto (@4 & @8)" check_bitset_spmd_union_and_intersect,
              testProperty "BitTree O(1) 3-level 64-ary pyramid up to 262,144 (findNextSetGe & popMin)" check_bittree_stateful_full_universe_and_popmin,
              testProperty "BloomFilter split-block 0 false negatives, empirical FPR < 0.05, & unionInto" check_bloomfilter_zero_fn_fpr_and_union,
              testProperty "XorFilter 8-bit peeling 0 false negatives, FPR < 0.02, duplicates, empty, & zero leaks" check_xorfilter_zero_fn_fpr_duplicates_empty_and_leaks,
              testProperty "Host <-> Compiled C FFI round-trip + C-built XorFilter return" $
                check_bit_and_filter_host_c_ffi (fst <$> rMut) (fst <$> rBuild)
            ]
