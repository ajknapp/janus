{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.Sequential where

import Control.Lens ((&), (.~))
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Foreign.Ptr (Ptr)
import qualified Foreign.Storable as Storable
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (acquireJanusC, releaseJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Command.Range (rangeM)
import Janus.Control.Drain
  ( drainAll,
    drainAny,
    drainCountWhere,
    drainFind,
    drainFirst,
    drainLast,
    drainMeanVariance,
    drainMinMax,
    drainNone,
    drainNth,
    drainSum,
    runDrain,
  )
import Janus.Control.Stream (Stream, rangeS)
import Janus.Data
import qualified Janus.Data.Deque as DQ
import qualified Janus.Data.DynVec as DV
import qualified Janus.Data.RingBuffer as RB
import qualified Janus.Data.SmallVec as SMV
import qualified Janus.Data.StableVec as STV
import Janus.Expression.Cast (ExpFloatingCast (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getSeqTestConfig :: IO CBuildConfig
getSeqTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

streamToListInt64 :: Stream IO (Identity Int64) -> IO [Int64]
streamToListInt64 s = do
  v <- runDrain (DV.drainToVect @Int64 dynamicAlloc) s
  Identity n <- DV.len v
  xs <- mapM (fmap runIdentity . DV.readVec v . Identity) [0 .. n - 1]
  DV.free dynamicAlloc v
  pure xs

--------------------------------------------------------------------------------
-- 1. DynVec + `drainToVect` + Expanded `Drain` Suite Property Tests
--------------------------------------------------------------------------------

data VecCmd
  = VPush !Int64
  | VPop
  | VWrite !Int !Int64
  | VInsertAt !Int !Int64
  | VRemoveAt !Int
  | VSwapRemove !Int
  | VSort
  | VShrinkToFit
  | VClear
  deriving (Eq, Show)

genVecCmd :: Gen VecCmd
genVecCmd =
  Gen.frequency
    [ (6, VPush <$> Gen.int64 (Range.linear (-500) 500)),
      (2, pure VPop),
      (3, VWrite <$> Gen.int (Range.linear 0 100) <*> Gen.int64 (Range.linear (-500) 500)),
      (3, VInsertAt <$> Gen.int (Range.linear 0 100) <*> Gen.int64 (Range.linear (-500) 500)),
      (2, VRemoveAt <$> Gen.int (Range.linear 0 100)),
      (2, VSwapRemove <$> Gen.int (Range.linear 0 100)),
      (1, pure VSort),
      (1, pure VShrinkToFit),
      (1, pure VClear)
    ]

check_dynvec_stateful_and_drains :: Property
check_dynvec_stateful_and_drains = property $ do
  cmds <- forAll $ Gen.list (Range.linear 1 120) genVecCmd
  (finalVec, finalRev, modelFinal, drainChecksOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      v <- DV.new @Int64 alloc
      let step model cmd = case cmd of
            VPush x -> do
              DV.push alloc v (Identity x)
              pure (model ++ [x])
            VPop -> do
              (Identity ok, Identity popped) <- DV.pop (-999) v
              case reverse model of
                [] -> if not ok then pure [] else error "Popped from empty DynVec"
                (lastX : restRev) ->
                  if ok && popped == lastX
                    then pure (reverse restRev)
                    else error "DynVec pop mismatch"
            VWrite idx x
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                  DV.writeVec v (Identity (fromIntegral i)) (Identity x)
                  pure (take i model ++ [x] ++ drop (i + 1) model)
            VInsertAt idx x -> do
              let i = idx `mod` (length model + 1)
              DV.insertAt alloc v (Identity (fromIntegral i)) (Identity x)
              pure (take i model ++ [x] ++ drop i model)
            VRemoveAt idx
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                  Identity remV <- DV.removeAt v (Identity (fromIntegral i))
                  if remV /= model !! i
                    then error "DynVec removeAt mismatch"
                    else pure (take i model ++ drop (i + 1) model)
            VSwapRemove idx
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                      lastX = last model
                  Identity remV <- DV.swapRemove v (Identity (fromIntegral i))
                  let nextModel
                        | i == length model - 1 = init model
                        | otherwise = take i (init model) ++ [lastX] ++ drop (i + 1) (init model)
                  if remV /= model !! i
                    then error "DynVec swapRemove mismatch"
                    else pure nextModel
            VSort -> do
              DV.sort v
              pure (Data.List.sort model)
            VShrinkToFit -> do
              DV.shrinkToFit alloc v
              pure model
            VClear -> do
              DV.clear v
              pure []

      modelEnd <- foldlM' step [] cmds
      -- Materialize clone via `drainToVect`!
      vClone <- runDrain (DV.drainToVect @Int64 alloc) (DV.stream v)
      fwd <- streamToListInt64 (DV.stream vClone)
      rev <- streamToListInt64 (DV.reverseStream vClone)

      -- Exercise expanded Drain operations over DV.stream
      (Identity mn, Identity mx) <- runDrain drainMinMax (DV.stream v)
      Identity cntPos <- runDrain (drainCountWhere (`gt` 0)) (DV.stream v)
      Identity anyEq0 <- runDrain (drainAny (`eq` 0)) (DV.stream v)
      Identity allGeMin <- runDrain (drainAll (`ge` Identity mn)) (DV.stream v)
      Identity noneLtMin <- runDrain (drainNone (`lt` Identity mn)) (DV.stream v)
      (Identity hasFst, Identity fstV) <- runDrain (drainFirst 0) (DV.stream v)
      (Identity hasLst, Identity lstV) <- runDrain (drainLast 0) (DV.stream v)
      (Identity has2, Identity nth2) <- runDrain (drainNth 2 0) (DV.stream v)
      (Identity foundPos, _) <- runDrain (drainFind 0 (`gt` 0)) (DV.stream v)
      (Identity mu, _) <- runDrain drainMeanVariance (toFloating @Identity @Int64 @Double <$> DV.stream v)

      let drainsOk =
            case modelEnd of
              [] -> not hasFst && not hasLst && not has2 && not anyEq0 && cntPos == 0 && mu == 0
              (m0 : _) ->
                mn == minimum modelEnd
                  && mx == maximum modelEnd
                  && cntPos == fromIntegral (length (filter (> 0) modelEnd))
                  && anyEq0 == elem 0 modelEnd
                  && allGeMin
                  && noneLtMin
                  && hasFst
                  && fstV == m0
                  && hasLst
                  && lstV == last modelEnd
                  && (if length modelEnd > 2 then has2 && nth2 == modelEnd !! 2 else not has2)
                  && foundPos == any (> 0) modelEnd

      DV.free alloc vClone
      DV.free alloc v
      snap <- readAllocSnapshot tracker
      pure (fwd, rev, modelEnd, drainsOk, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  finalVec === modelFinal
  finalRev === reverse modelFinal
  assert drainChecksOk
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 2. SmallVec Inline Zero-Alloc Fast Path + Spill Property Tests
--------------------------------------------------------------------------------

check_smallvec_inline_and_spill :: Property
check_smallvec_inline_and_spill = property $ do
  xsSmall <- forAll $ Gen.list (Range.linear 0 8) (Gen.int64 (Range.linear (-100) 100))
  cmds <- forAll $ Gen.list (Range.linear 1 90) genVecCmd

  (zeroAllocsOk, smallRoundTrip, spilledList, modelEnd, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc

      -- Phase A: <= 8 elements inside `withSmallVec @8` must trigger ZERO heap allocations!
      smallRes <- SMV.withSmallVec @8 @Int64 alloc $ \sv -> do
        forM_ xsSmall (SMV.push alloc sv . Identity)
        Identity sp <- SMV.isSpilled sv
        snapMid <- readAllocSnapshot tracker
        listOut <- streamToListInt64 (SMV.stream sv)
        pure (not sp && runIdentity (snapTotalAllocs snapMid) == 0, listOut)

      resetAllocTracker tracker

      -- Phase B: Full command trace spilling well past inline capacity 4 (`SmallVec 4`)
      sv <- SMV.new @4 @Int64 alloc
      let step model cmd = case cmd of
            VPush x -> SMV.push alloc sv (Identity x) >> pure (model ++ [x])
            VPop -> do
              (Identity ok, Identity v) <- SMV.pop 0 sv
              case reverse model of
                [] -> if not ok then pure [] else error "SmallVec empty pop"
                (y : ys) -> if ok && v == y then pure (reverse ys) else error "SmallVec pop mismatch"
            VWrite idx x
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                  SMV.writeVec sv (Identity (fromIntegral i)) (Identity x)
                  pure (take i model ++ [x] ++ drop (i + 1) model)
            VInsertAt idx x -> do
              let i = idx `mod` (length model + 1)
              SMV.insertAt alloc sv (Identity (fromIntegral i)) (Identity x)
              pure (take i model ++ [x] ++ drop i model)
            VRemoveAt idx
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                  Identity r <- SMV.removeAt sv (Identity (fromIntegral i))
                  if r == model !! i then pure (take i model ++ drop (i + 1) model) else error "SmallVec removeAt"
            VSwapRemove idx
              | null model -> pure []
              | otherwise -> do
                  let i = idx `mod` length model
                      lx = last model
                  Identity r <- SMV.swapRemove sv (Identity (fromIntegral i))
                  let nm
                        | i == length model - 1 = init model
                        | otherwise = take i (init model) ++ [lx] ++ drop (i + 1) (init model)
                  if r == model !! i then pure nm else error "SmallVec swapRemove"
            VSort -> SMV.sort sv >> pure (Data.List.sort model)
            VShrinkToFit -> pure model
            VClear -> SMV.clear sv >> pure []

      mOut <- foldlM' step [] cmds
      sOut <- streamToListInt64 (SMV.stream sv)
      SMV.free alloc sv
      snap <- readAllocSnapshot tracker
      pure (fst smallRes, snd smallRes, sOut, mOut, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  assert zeroAllocsOk
  smallRoundTrip === xsSmall
  spilledList === modelEnd
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 3. StableVec Pointer Stability Across Arbitrary Growth Property
--------------------------------------------------------------------------------

check_stablevec_pointer_stability :: Property
check_stablevec_pointer_stability = property $ do
  initial <- forAll $ Gen.list (Range.linear 1 20) (Gen.int64 (Range.linear 1 1000))
  extra <- forAll $ Gen.list (Range.linear 50 300) (Gen.int64 (Range.linear 1001 5000))

  (streamedAll, ptrReadsAfterGrowth, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      -- Use small chunk shift `logChunk = 3` (8 elements/chunk) to force many directory reallocations!
      sv <- STV.new @3 @Int64 alloc
      savedPtrs <-
        mapM
          ( \(idx, x) -> do
              STV.push alloc sv (Identity x)
              Identity p <- STV.elemPtrScalar sv (Identity idx)
              pure (p :: Ptr Int64, x)
          )
          (zip [0 :: Int64 ..] initial)

      -- Push up to 300 additional elements, growing chunk directory repeatedly
      forM_ extra (STV.push alloc sv . Identity)

      -- Mutate initial elements directly through the SAVED pointers from before growth!
      forM_ savedPtrs $ \(p, oldX) ->
        Storable.poke p (oldX + 777)

      ptrVals <- mapM (\(p, _) -> Storable.peek p) savedPtrs
      allElems <- streamToListInt64 (STV.stream sv)
      STV.free alloc sv
      snap <- readAllocSnapshot tracker
      pure (allElems, ptrVals, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  ptrReadsAfterGrowth === map (+ 777) initial
  streamedAll === (map (+ 777) initial ++ extra)
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 4. RingBuffer & Deque Stateful Differential Properties
--------------------------------------------------------------------------------

data RingCmd
  = RTryPush !Int64
  | RPushOverwrite !Int64
  | RPop
  | RClear
  deriving (Eq, Show)

check_ringbuffer_stateful :: Property
check_ringbuffer_stateful = property $ do
  capExp <- forAll $ Gen.int (Range.linear 1 5) -- cap = 2, 4, 8, 16, 32
  let cap = (2 :: Int) ^ capExp
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 150)
        ( Gen.frequency
            [ (4, RTryPush <$> Gen.int64 (Range.linear (-200) 200)),
              (4, RPushOverwrite <$> Gen.int64 (Range.linear (-200) 200)),
              (4, pure RPop),
              (1, pure RClear)
            ]
        )

  (rbFwd, rbRev, modelOut, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      rb <- RB.new @Int64 alloc (Identity (fromIntegral cap))
      let step q cmd = case cmd of
            RTryPush x -> do
              Identity ok <- RB.tryPush rb (Identity x)
              if Seq.length q < cap
                then if ok then pure (q Seq.|> x) else error "RingBuffer tryPush failed when not full"
                else if not ok then pure q else error "RingBuffer tryPush succeeded when full"
            RPushOverwrite x -> do
              Identity ov <- RB.pushOverwrite rb (Identity x)
              if Seq.length q == cap
                then case Seq.viewl q of
                  (_ Seq.:< rest) -> if ov then pure (rest Seq.|> x) else error "Expected overwrite=true"
                  Seq.EmptyL -> error "impossible"
                else if not ov then pure (q Seq.|> x) else error "Expected overwrite=false"
            RPop -> do
              (Identity ok, Identity v) <- RB.tryPop 0 rb
              case Seq.viewl q of
                Seq.EmptyL -> if not ok then pure Seq.empty else error "Popped empty RingBuffer"
                (front Seq.:< rest) -> if ok && v == front then pure rest else error "RingBuffer pop mismatch"
            RClear -> RB.clear rb >> pure Seq.empty

      qEnd <- foldlM' step Seq.empty cmds
      fwd <- streamToListInt64 (RB.stream rb)
      rev <- streamToListInt64 (RB.reverseStream rb)
      RB.free alloc rb
      snap <- readAllocSnapshot tracker
      pure (fwd, rev, toList qEnd, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  rbFwd === modelOut
  rbRev === reverse modelOut
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

data DequeCmd
  = DPushFront !Int64
  | DPushBack !Int64
  | DPopFront
  | DPopBack
  | DWrite !Int !Int64
  | DClear
  deriving (Eq, Show)

check_deque_stateful :: Property
check_deque_stateful = property $ do
  cmds <-
    forAll $
      Gen.list
        (Range.linear 1 160)
        ( Gen.frequency
            [ (5, DPushFront <$> Gen.int64 (Range.linear (-300) 300)),
              (5, DPushBack <$> Gen.int64 (Range.linear (-300) 300)),
              (3, pure DPopFront),
              (3, pure DPopBack),
              (2, DWrite <$> Gen.int (Range.linear 0 100) <*> Gen.int64 (Range.linear (-300) 300)),
              (1, pure DClear)
            ]
        )

  (dqFwd, dqRev, modelOut, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      dq <- DQ.new @Int64 alloc
      let step q cmd = case cmd of
            DPushFront x -> DQ.pushFront alloc dq (Identity x) >> pure (x Seq.<| q)
            DPushBack x -> DQ.pushBack alloc dq (Identity x) >> pure (q Seq.|> x)
            DPopFront -> do
              (Identity ok, Identity v) <- DQ.popFront 0 dq
              case Seq.viewl q of
                Seq.EmptyL -> if not ok then pure Seq.empty else error "Deque empty popFront"
                (h Seq.:< rest) -> if ok && v == h then pure rest else error "Deque popFront mismatch"
            DPopBack -> do
              (Identity ok, Identity v) <- DQ.popBack 0 dq
              case Seq.viewr q of
                Seq.EmptyR -> if not ok then pure Seq.empty else error "Deque empty popBack"
                (rest Seq.:> l) -> if ok && v == l then pure rest else error "Deque popBack mismatch"
            DWrite idx x
              | Seq.null q -> pure Seq.empty
              | otherwise -> do
                  let i = idx `mod` Seq.length q
                  DQ.writeDeque dq (Identity (fromIntegral i)) (Identity x)
                  pure (Seq.update i x q)
            DClear -> DQ.clear dq >> pure Seq.empty

      qEnd <- foldlM' step Seq.empty cmds
      fwd <- streamToListInt64 (DQ.stream dq)
      rev <- streamToListInt64 (DQ.reverseStream dq)
      DQ.free alloc dq
      snap <- readAllocSnapshot tracker
      pure (fwd, rev, toList qEnd, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  dqFwd === modelOut
  dqRev === reverse modelOut
  liveEnd === (0, 0)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = f acc x >>= \a' -> a' `seq` foldlM' f a' xs

--------------------------------------------------------------------------------
-- 5. Host <-> Compiled C Bidirectional Mutation & Return-Handle Round-Trips
--------------------------------------------------------------------------------

cMutateHostDynVecAndDeque ::
  DynVec LinuxX86_64CCSE Int64 ->
  Deque LinuxX86_64CCSE Int64 ->
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
cMutateHostDynVecAndDeque vec dq count = do
  -- Push `count` items into BOTH containers in C (triggering C-side reallocations visible to Haskell!)
  rangeM 0 count $ \i -> do
    DV.push dynamicAlloc vec (i * 10 + 1)
    DQ.pushFront dynamicAlloc dq (i + 100)
  -- Run 8-lane SPMD doubling over `vec`
  DV.foreachScalarSPMD (Proxy @8) vec (\_ x -> pure (x * 2))
  s1 <- runDrain drainSum (DV.stream vec)
  s2 <- runDrain drainSum (DQ.stream dq)
  pure (s1 + s2)

cBuildVecViaDrainInC ::
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CMCSE (DynVec LinuxX86_64CCSE Int64)
cBuildVecViaDrainInC n =
  runDrain (DV.drainToVect @Int64 dynamicAlloc) ((+ 5) <$> rangeS 0 n)

check_sequential_host_c_ffi ::
  IO (DynVec Identity Int64 -> Deque Identity Int64 -> Int64 -> IO Int64) ->
  IO (Int64 -> IO (DynVec Identity Int64)) ->
  Property
check_sequential_host_c_ffi getMutFn getBuildFn = property $ do
  mutFn <- liftIO getMutFn
  buildFn <- liftIO getBuildFn
  initVec <- forAll $ Gen.list (Range.linear 0 25) (Gen.int64 (Range.linear (-50) 50))
  initDq <- forAll $ Gen.list (Range.linear 0 25) (Gen.int64 (Range.linear (-50) 50))
  extra <- forAll $ Gen.int64 (Range.linear 0 40)

  (cSum, hsVecAfter, hsDqAfter, cConstructedVec) <- liftIO $ do
    v <- DV.new @Int64 dynamicAlloc
    dq <- DQ.new @Int64 dynamicAlloc
    forM_ initVec (DV.push dynamicAlloc v . Identity)
    forM_ initDq (DQ.pushBack dynamicAlloc dq . Identity)

    s <- mutFn v dq extra
    vList <- streamToListInt64 (DV.stream v)
    dqList <- streamToListInt64 (DQ.stream dq)
    DV.free dynamicAlloc v
    DQ.free dynamicAlloc dq

    vFromC <- buildFn extra
    fromCList <- streamToListInt64 (DV.stream vFromC)
    DV.free dynamicAlloc vFromC
    pure (s, vList, dqList, fromCList)

  let expectedVec = map (* 2) (initVec ++ [i * 10 + 1 | i <- [0 .. extra - 1]])
      expectedDq = reverse [i + 100 | i <- [0 .. extra - 1]] ++ initDq
  hsVecAfter === expectedVec
  hsDqAfter === expectedDq
  cSum === sum expectedVec + sum expectedDq
  cConstructedVec === [i + 5 | i <- [0 .. extra - 1]]
  where
    _ = replicateM @IO

test_sequential_containers :: TestTree
test_sequential_containers =
  let acqMut = getSeqTestConfig >>= flip acquireJanusC cMutateHostDynVecAndDeque
      acqBuild = getSeqTestConfig >>= flip acquireJanusC cBuildVecViaDrainInC
   in withResource acqMut releaseJanusC $ \rMut ->
        withResource acqBuild releaseJanusC $ \rBuild ->
          testGroup
            "Sequential & Positional Data Structures"
            [ testProperty "DynVec stateful trace + drainToVect + expanded Drain suite" check_dynvec_stateful_and_drains,
              testProperty "SmallVec zero-allocation inline fast path + heap spill" check_smallvec_inline_and_spill,
              testProperty "StableVec pointer stability across directory growth" check_stablevec_pointer_stability,
              testProperty "RingBuffer wrap-around tryPush / pushOverwrite / stream" check_ringbuffer_stateful,
              testProperty "Deque double-ended push/pop/resize unwrap" check_deque_stateful,
              testProperty "Host <-> Compiled C FFI round-trip + C-side drainToVect return" $
                check_sequential_host_c_ffi (fst <$> rMut) (fst <$> rBuild)
            ]
