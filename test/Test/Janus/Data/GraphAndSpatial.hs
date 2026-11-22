{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Data.GraphAndSpatial
  ( test_graph_and_spatial,
  )
where

import Control.Lens ((&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Sequence as Seq
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM, acquireLinuxX86_64C, releaseLinuxX86_64C)
import Janus.Command.Cond (whenM_)
import Janus.Command.Memory (CmdStorable (peekElemOff, pokeElemOff))
import Janus.Control.Drain (drainForEach, drainSum, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, statefulS, takeS)
import Janus.Data.CsrGraph (CsrGraph (..))
import qualified Janus.Data.CsrGraph as CSR
import Janus.Data.DisjointSet (DisjointSet (..))
import qualified Janus.Data.DisjointSet as DS
import qualified Janus.Data.DynVec as DV
import Janus.Data.EytzingerVec (EytzingerVec (..))
import qualified Janus.Data.EytzingerVec as EV
import Janus.Data.FlatIntervalMap (FlatIntervalMap (..))
import qualified Janus.Data.FlatIntervalMap as FIM
import Janus.Expression.Ord (ExpOrd (ge))
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

getGraphSpatialTestConfig :: IO CBuildConfig
getGraphSpatialTestConfig = do
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
  statefulS (newIORef xs0) $ \ref -> ContT $ \stepK -> do
    cur <- readIORef ref
    case cur of
      [] -> stepK Stop
      (y : ys) -> writeIORef ref ys >> stepK (Yield y)

foldlM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldlM' _ acc [] = pure acc
foldlM' f acc (x : xs) = do
  acc' <- f acc x
  acc' `seq` foldlM' f acc' xs

--------------------------------------------------------------------------------
-- 1. DisjointSet Differential Partition Trace, Path-Halving, & Bulk Unions
--------------------------------------------------------------------------------

data DSCmd
  = DSUnion !Int64 !Int64
  | DSSameSet !Int64 !Int64
  | DSCompSize !Int64
  | DSCheckPartition !Int64
  | DSGrow !Int64
  | DSBulkUnions ![(Int64, Int64)]
  | DSReset
  deriving (Eq, Show)

genDSCmd :: Gen DSCmd
genDSCmd =
  Gen.frequency
    [ (7, DSUnion <$> Gen.int64 (Range.linear (-2) 95) <*> Gen.int64 (Range.linear (-2) 95)),
      (4, DSSameSet <$> Gen.int64 (Range.linear (-2) 95) <*> Gen.int64 (Range.linear (-2) 95)),
      (4, DSCompSize <$> Gen.int64 (Range.linear (-2) 95)),
      (3, DSCheckPartition <$> Gen.int64 (Range.linear (-2) 95)),
      (2, DSGrow <$> Gen.int64 (Range.linear 0 12)),
      ( 2,
        DSBulkUnions
          <$> Gen.list
            (Range.linear 0 10)
            ((,) <$> Gen.int64 (Range.linear (-1) 95) <*> Gen.int64 (Range.linear (-1) 95))
      ),
      (1, pure DSReset)
    ]

modelValid :: IntMap.IntMap Int -> Int64 -> Bool
modelValid m x =
  let xi = fromIntegral x :: Int
   in xi >= 0 && xi < IntMap.size m

modelUnion :: IntMap.IntMap Int -> Int64 -> Int64 -> (Bool, IntMap.IntMap Int)
modelUnion m x y
  | not (modelValid m x && modelValid m y) = (False, m)
  | otherwise =
      let cx = m IntMap.! fromIntegral x
          cy = m IntMap.! fromIntegral y
       in if cx == cy
            then (False, m)
            else (True, IntMap.map (\c -> if c == cy then cx else c) m)

modelNumSets :: IntMap.IntMap Int -> Int64
modelNumSets m = fromIntegral (IntSet.size (IntSet.fromList (IntMap.elems m)))

modelCompSize :: IntMap.IntMap Int -> Int64 -> Int64
modelCompSize m x
  | not (modelValid m x) = 0
  | otherwise =
      let cx = m IntMap.! fromIntegral x
       in fromIntegral (IntMap.size (IntMap.filter (== cx) m))

modelMembers :: IntMap.IntMap Int -> Int64 -> [Int64]
modelMembers m x
  | not (modelValid m x) = []
  | otherwise =
      let cx = m IntMap.! fromIntegral x
       in [fromIntegral k | (k, c) <- IntMap.toAscList m, c == cx]

check_disjoint_set_partition_model :: Property
check_disjoint_set_partition_model = property $ do
  initN <- forAll $ Gen.int64 (Range.linear 0 50)
  cmds <- forAll $ Gen.list (Range.linear 1 150) genDSCmd

  (finalNumElems, finalNumSets, expectedElems, expectedSets, rootsPartitionOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      ds <- DS.new alloc (Identity initN)
      let initModel = IntMap.fromList [(i, i) | i <- [0 .. fromIntegral initN - 1]]
          step m cmd = case cmd of
            DSUnion x y -> do
              (Identity didMerge, Identity cRoot) <- DS.union ds (Identity x) (Identity y)
              let (expMerge, m') = modelUnion m x y
              when (didMerge /= expMerge) $
                error "DisjointSet union didMerge mismatch"
              when (modelValid m' x && modelValid m' y) $ do
                Identity rx <- DS.find ds (Identity x)
                Identity ry <- DS.find ds (Identity y)
                when (rx /= cRoot || ry /= cRoot) $
                  error "DisjointSet union canonicalRoot mismatch"
              pure m'
            DSSameSet x y -> do
              Identity got <- DS.sameSet ds (Identity x) (Identity y)
              let expSame =
                    modelValid m x
                      && modelValid m y
                      && (m IntMap.! fromIntegral x) == (m IntMap.! fromIntegral y)
              if got == expSame then pure m else error "DisjointSet sameSet mismatch"
            DSCompSize x -> do
              Identity gotSz <- DS.componentSize ds (Identity x)
              if gotSz == modelCompSize m x then pure m else error "DisjointSet componentSize mismatch"
            DSCheckPartition x -> do
              Identity ns <- DS.numSets ds
              roots <- map runIdentity <$> collectStreamHost (DS.rootsS ds)
              members <- map runIdentity <$> collectStreamHost (DS.componentMembersS ds (Identity x))
              if ns == modelNumSets m && fromIntegral (length roots) == ns && members == modelMembers m x
                then pure m
                else error "DisjointSet partition/stream mismatch"
            DSGrow add -> do
              DS.grow alloc ds (Identity add)
              let base = IntMap.size m
                  extra = [(base + i, base + i) | i <- [0 .. fromIntegral (max 0 add) - 1]]
              pure (IntMap.union m (IntMap.fromList extra))
            DSBulkUnions pairs -> do
              runDrain
                (DS.drainAppendUnions ds)
                (listToStreamHost [(Identity u, Identity v) | (u, v) <- pairs])
              pure (foldl (\acc (u, v) -> snd (modelUnion acc u v)) m pairs)
            DSReset -> do
              DS.reset ds
              let sz = IntMap.size m
              pure (IntMap.fromList [(i, i) | i <- [0 .. sz - 1]])

      mEnd <- foldlM' step initModel cmds
      Identity endElems <- DS.numElements ds
      Identity endSets <- DS.numSets ds
      roots <- map runIdentity <$> collectStreamHost (DS.rootsS ds)
      compLists <- mapM (fmap (map runIdentity) . collectStreamHost . DS.componentMembersS ds . Identity) roots
      let allCovered = sort (concat compLists) == [0 .. endElems - 1]
          rootsOk = fromIntegral (length roots) == endSets && allCovered

      DS.free alloc ds
      snap <- readAllocSnapshot tracker
      pure
        ( endElems,
          endSets,
          fromIntegral (IntMap.size mEnd),
          modelNumSets mEnd,
          rootsOk,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  finalNumElems === expectedElems
  finalNumSets === expectedSets
  assert rootsPartitionOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 2. CsrGraph Bulk Construction, Adjacency Streams, BFS Distances, & csrSpMV
--------------------------------------------------------------------------------

pureBfs :: Int64 -> IntMap.IntMap [(Int64, Int64)] -> Int64 -> [Int64]
pureBfs nv adj src
  | src < 0 || src >= nv = replicate (fromIntegral nv) (-1)
  | otherwise =
      let s0 = fromIntegral src :: Int
          go :: Seq.Seq Int -> IntMap.IntMap Int64 -> IntMap.IntMap Int64
          go q dists = case Seq.viewl q of
            Seq.EmptyL -> dists
            u Seq.:< rest ->
              let du = dists IntMap.! u
                  nbrs = map (fromIntegral . fst) (IntMap.findWithDefault [] u adj)
                  (q', dists') =
                    foldl
                      ( \( cq, cd ) v ->
                          if IntMap.member v cd
                            then (cq, cd)
                            else (cq Seq.|> v, IntMap.insert v (du + 1) cd)
                      )
                      (rest, dists)
                      nbrs
               in go q' dists'
          finalDists = go (Seq.singleton s0) (IntMap.singleton s0 0)
       in [IntMap.findWithDefault (-1) v finalDists | v <- [0 .. fromIntegral nv - 1]]

check_csr_graph_bfs_and_spmv :: Property
check_csr_graph_bfs_and_spmv = property $ do
  nv <-
    forAll $
      Gen.choice
        [ Gen.element [0, 1, 2, 5, 16],
          Gen.int64 (Range.linear 0 55)
        ]
  rawEdges <-
    forAll $
      Gen.list
        (Range.linear 0 180)
        ( (,,)
            <$> Gen.int64 (Range.linear (-2) (nv + 2))
            <*> Gen.int64 (Range.linear (-2) (nv + 2))
            <*> Gen.int64 (Range.linear (-50) 50)
        )
  xVals <- forAll $ Gen.list (Range.singleton (fromIntegral nv)) (Gen.int64 (Range.linear (-30) 30))

  let validEdges = [(u, v, w) | (u, v, w) <- rawEdges, u >= 0, u < nv, v >= 0, v < nv]
      adjMap =
        IntMap.fromListWith
          (flip (++))
          [(fromIntegral u, [(v, w)]) | (u, v, w) <- validEdges]
      srcCandidates = [-1, 0, nv `div` 2, nv - 1, nv]

  (gotNV, gotNE, allEdgesGot, adjChecksOk, bfsResults, yGot, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      g <-
        CSR.fromEdgeStream @Int64
          alloc
          (Identity nv)
          (listToStreamHost [(Identity u, Identity v, Identity w) | (u, v, w) <- rawEdges])

      Identity gNV <- CSR.numVertices g
      Identity gNE <- CSR.numEdges g
      allE <-
        map (\(Identity u, Identity v, Identity w) -> (u, v, w))
          <$> collectStreamHost (CSR.allEdgesS g)

      perVertOk <-
        fmap and $
          mapM
            ( \u -> do
                Identity deg <- CSR.outDegree g (Identity u)
                nbrs <- map runIdentity <$> collectStreamHost (CSR.neighborsS g (Identity u))
                oes <- map (\(Identity v, Identity w) -> (v, w)) <$> collectStreamHost (CSR.outEdgesS g (Identity u))
                let expOes =
                      if u >= 0 && u < nv
                        then IntMap.findWithDefault [] (fromIntegral u) adjMap
                        else []
                pure (deg == fromIntegral (length expOes) && oes == expOes && nbrs == map fst expOes)
            )
            ([-1 .. nv])

      bfsOut <-
        mapM
          ( \s -> do
              dv <- CSR.bfsDistances alloc g (Identity s)
              ds <- map runIdentity <$> collectStreamHost (DV.stream dv)
              DV.free alloc dv
              pure ds
          )
          srcCandidates

      -- Test csrSpMV with Alloc-managed input/output vectors
      xPtr <- allocTypedArray @Int64 alloc (Identity nv)
      yPtr <- allocTypedArray @Int64 alloc (Identity nv)
      forM_ (zip [0 :: Int64 ..] xVals) $ \(i, xv) ->
        pokeElemOff xPtr (Identity xv) (Identity i)
      CSR.csrSpMV g xPtr yPtr
      yList <- mapM (\i -> runIdentity <$> peekElemOff yPtr (Identity i)) [0 .. nv - 1]
      freeTypedArray @Int64 alloc yPtr (Identity nv)
      freeTypedArray @Int64 alloc xPtr (Identity nv)

      CSR.free alloc g
      snap <- readAllocSnapshot tracker
      pure
        ( gNV,
          gNE,
          allE,
          perVertOk,
          bfsOut,
          yList,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  let expectedAllEdges =
        [ (u, v, w)
        | u <- [0 .. nv - 1],
          (v, w) <- IntMap.findWithDefault [] (fromIntegral u) adjMap
        ]
      expectedBfs = map (pureBfs nv adjMap) srcCandidates
      xMap = IntMap.fromList (zip [0 :: Int ..] xVals)
      expectedY =
        [ sum [w * (xMap IntMap.! fromIntegral v) | (v, w) <- IntMap.findWithDefault [] (fromIntegral u) adjMap]
        | u <- [0 .. nv - 1]
        ]

  gotNV === nv
  gotNE === fromIntegral (length validEdges)
  allEdgesGot === expectedAllEdges
  assert adjChecksOk
  bfsResults === expectedBfs
  yGot === expectedY
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 3. EytzingerVec Branchless Search & Stackless In-Order Stream (Sizes 0..300)
--------------------------------------------------------------------------------

check_eytzinger_vec_search_and_inorder :: Property
check_eytzinger_vec_search_and_inorder = property $ do
  sz <-
    forAll $
      Gen.choice
        [ Gen.element [0, 1, 2, 3, 4, 5, 6, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128, 255, 256, 300],
          Gen.int (Range.linear 0 300)
        ]
  xs <- forAll $ Gen.list (Range.singleton sz) (Gen.int64 (Range.linear (-70) 70))
  extraQs <- forAll $ Gen.list (Range.linear 5 40) (Gen.int64 (Range.linear (-85) 85))
  let sortedRef = sort xs
      queries = [-80, -71, -70, -35, -1, 0, 1, 35, 70, 71, 80] ++ extraQs

  (gotSz, gotEmpty, inOrderGot, early3Got, bfsSortedGot, queriesOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      ev <- EV.fromStream @Int64 alloc (listToStreamHost (map Identity xs))
      Identity n <- EV.size ev
      Identity emp <- EV.isEmpty ev
      inOrd <- map runIdentity <$> collectStreamHost (EV.inOrderStream ev)
      early3 <- map runIdentity <$> collectStreamHost (takeS 3 (EV.inOrderStream ev))
      bfsRaw <- map runIdentity <$> collectStreamHost (EV.rawBfsStream ev)

      qOk <-
        fmap and $
          mapM
            ( \q -> do
                Identity mem <- EV.member ev (Identity q)
                (Identity geOk, Identity geVal) <- EV.lookupGE (-99999) ev (Identity q)
                cbRef <- newIORef (False, -99999 :: Int64)
                EV.withLookupGE
                  ev
                  (Identity q)
                  (writeIORef cbRef (False, -99999))
                  (\(Identity hit) -> writeIORef cbRef (True, hit))
                (cbOk, cbVal) <- readIORef cbRef
                let expMem = elem q sortedRef
                    (expGeOk, expGeVal) = case dropWhile (< q) sortedRef of
                      [] -> (False, -99999)
                      (h : _) -> (True, h)
                pure
                  ( mem == expMem
                      && geOk == expGeOk
                      && geVal == expGeVal
                      && cbOk == expGeOk
                      && cbVal == expGeVal
                  )
            )
            queries

      EV.free alloc ev
      snap <- readAllocSnapshot tracker
      pure
        ( n,
          emp,
          inOrd,
          early3,
          sort bfsRaw,
          qOk,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  gotSz === fromIntegral sz
  gotEmpty === null xs
  inOrderGot === sortedRef
  early3Got === take 3 sortedRef
  bfsSortedGot === sortedRef
  assert queriesOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 4. FlatIntervalMap Overlap / Point-Stab / Early-Stop Leak Checks (Sizes 0..250)
--------------------------------------------------------------------------------

genIntervalTriple :: Gen (Int64, Int64, Int64)
genIntervalTriple = do
  lo <- Gen.int64 (Range.linear (-100) 100)
  spanKind <- Gen.int (Range.linear 0 9)
  hi <- case spanKind of
    0 -> pure lo -- point interval [lo, lo]
    1 -> Gen.int64 (Range.linear lo (lo + 150)) -- wide spanning interval
    2 -> pure (lo - 3) -- invalid empty interval (hi < lo) to test filtering
    _ -> Gen.int64 (Range.linear lo (lo + 25)) -- typical interval
  val <- Gen.int64 (Range.linear (-500) 500)
  pure (lo, hi, val)

check_flat_interval_map_queries_and_early_stop :: Property
check_flat_interval_map_queries_and_early_stop = property $ do
  sz <-
    forAll $
      Gen.choice
        [ Gen.element [0, 1, 2, 3, 4, 5, 6, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128, 250],
          Gen.int (Range.linear 0 250)
        ]
  rawTriples <- forAll $ Gen.list (Range.singleton sz) genIntervalTriple
  qRanges <-
    forAll $
      Gen.list
        (Range.linear 8 30)
        ((,) <$> Gen.int64 (Range.linear (-120) 120) <*> Gen.int64 (Range.linear (-120) 120))
  qPoints <- forAll $ Gen.list (Range.linear 8 25) (Gen.int64 (Range.linear (-120) 120))

  let refSorted =
        [ (lo, hi, val)
        | (lo, hi, _, val) <-
            sort [(lo, hi, idx, val) | (idx, (lo, hi, val)) <- zip [0 :: Int ..] rawTriples, lo <= hi]
        ]

  (gotSz, gotEmpty, seqAllGot, rangeChecksOk, pointChecksOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      fim <-
        FIM.fromStream @Int64 @Int64
          alloc
          (listToStreamHost [(Identity lo, Identity hi, Identity val) | (lo, hi, val) <- rawTriples])

      Identity n <- FIM.size fim
      Identity emp <- FIM.isEmpty fim
      seqAll <-
        map (\(Identity lo, Identity hi, Identity v) -> (lo, hi, v))
          <$> collectStreamHost (FIM.toStream fim)

      rOk <-
        fmap and $
          mapM
            ( \(qLo, qHi) -> do
                ov <-
                  map (\(Identity lo, Identity hi, Identity v) -> (lo, hi, v))
                    <$> collectStreamHost (FIM.overlappingStream fim (Identity qLo) (Identity qHi))
                early2 <-
                  map (\(Identity lo, Identity hi, Identity v) -> (lo, hi, v))
                    <$> collectStreamHost (takeS 2 (FIM.overlappingStream fim (Identity qLo) (Identity qHi)))
                Identity cnt <- FIM.countOverlapping fim (Identity qLo) (Identity qHi)
                Identity anyHit <- FIM.anyOverlapping fim (Identity qLo) (Identity qHi)
                let expected =
                      [ t
                      | t@(lo, hi, _) <- refSorted,
                        qLo <= qHi,
                        lo <= qHi && hi >= qLo
                      ]
                pure
                  ( ov == expected
                      && early2 == take 2 expected
                      && cnt == fromIntegral (length expected)
                      && anyHit == not (null expected)
                  )
            )
            (((-150, -140) : (140, 150) : (-200, 200) : (10, 5) : qRanges))

      pOk <-
        fmap and $
          mapM
            ( \pt -> do
                stab <-
                  map (\(Identity lo, Identity hi, Identity v) -> (lo, hi, v))
                    <$> collectStreamHost (FIM.pointStabStream fim (Identity pt))
                let expected = [t | t@(lo, hi, _) <- refSorted, lo <= pt && hi >= pt]
                pure (stab == expected)
            )
            qPoints

      FIM.free alloc fim
      snap <- readAllocSnapshot tracker
      pure
        ( n,
          emp,
          seqAll,
          rOk,
          pOk,
          (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap))
        )

  gotSz === fromIntegral (length refSorted)
  gotEmpty === null refSorted
  seqAllGot === refSorted
  assert rangeChecksOk
  assert pointChecksOk
  liveEnd === (0, 0)

--------------------------------------------------------------------------------
-- 5. Host <-> Compiled C FFI Round-Trip Across All 4 Containers
--------------------------------------------------------------------------------

cGraphAndSpatialKernel ::
  DisjointSet LinuxX86_64C ->
  CsrGraph LinuxX86_64C Int64 ->
  EytzingerVec LinuxX86_64C Int64 ->
  FlatIntervalMap LinuxX86_64C Int64 Int64 ->
  LinuxX86_64C Int64 ->
  LinuxX86_64C Int64 ->
  LinuxX86_64C Int64 ->
  LinuxX86_64CM (LinuxX86_64C Int64)
cGraphAndSpatialKernel ds g ev fim target qLo qHi = do
  nDs <- DS.numElements ds
  whenM_ (nDs `ge` 2) $ do
    _ <- DS.union ds 0 (nDs - 1)
    pure ()
  setsAfter <- DS.numSets ds

  dists <- CSR.bfsDistances dynamicAlloc g 0
  distSum <- runDrain drainSum (DV.stream dists)
  DV.free dynamicAlloc dists

  (_, geVal) <- EV.lookupGE (-1000) ev target
  evSum <- runDrain drainSum (EV.inOrderStream ev)

  ovValSum <- runDrain drainSum ((\(_, _, v) -> v) <$> FIM.overlappingStream fim qLo qHi)
  pure (setsAfter + distSum + geVal + evSum + ovValSum)

check_graph_spatial_host_c_ffi ::
  IO
    ( DisjointSet Identity ->
      CsrGraph Identity Int64 ->
      EytzingerVec Identity Int64 ->
      FlatIntervalMap Identity Int64 Int64 ->
      Int64 ->
      Int64 ->
      Int64 ->
      IO Int64
    ) ->
  Property
check_graph_spatial_host_c_ffi getCFn = property $ do
  cFn <- liftIO getCFn
  nDs <- forAll $ Gen.int64 (Range.linear 0 30)
  nv <- forAll $ Gen.int64 (Range.linear 0 25)
  edges <-
    forAll $
      Gen.list
        (Range.linear 0 50)
        ( (,,)
            <$> Gen.int64 (Range.linear 0 (max 0 (nv - 1)))
            <*> Gen.int64 (Range.linear 0 (max 0 (nv - 1)))
            <*> Gen.int64 (Range.linear 1 20)
        )
  evKeys <- forAll $ Gen.list (Range.linear 0 40) (Gen.int64 (Range.linear (-50) 50))
  intervals <- forAll $ Gen.list (Range.linear 0 40) genIntervalTriple
  target <- forAll $ Gen.int64 (Range.linear (-60) 60)
  qLo <- forAll $ Gen.int64 (Range.linear (-80) 80)
  qHi <- forAll $ Gen.int64 (Range.linear (-80) 80)

  let validEdges = [(u, v, w) | (u, v, w) <- edges, u >= 0, u < nv, v >= 0, v < nv]
      adjMap = IntMap.fromListWith (flip (++)) [(fromIntegral u, [(v, w)]) | (u, v, w) <- validEdges]
      sortedKeys = sort evKeys
      validIntervals = [(lo, hi, val) | (lo, hi, val) <- intervals, lo <= hi]

  (cRes, hostMergedOk, liveEnd) <- liftIO $
    withAllocTracker $ \tracker -> do
      let alloc = statsAlloc tracker dynamicAlloc
      ds <- DS.new alloc (Identity nDs)
      g <-
        CSR.fromEdgeStream @Int64
          alloc
          (Identity nv)
          (listToStreamHost [(Identity u, Identity v, Identity w) | (u, v, w) <- validEdges])
      ev <- EV.fromStream @Int64 alloc (listToStreamHost (map Identity evKeys))
      fim <-
        FIM.fromStream @Int64 @Int64
          alloc
          (listToStreamHost [(Identity lo, Identity hi, Identity val) | (lo, hi, val) <- validIntervals])

      tot <- cFn ds g ev fim target qLo qHi
      mergedOk <-
        if nDs >= 2
          then runIdentity <$> DS.sameSet ds 0 (Identity (nDs - 1))
          else pure True

      FIM.free alloc fim
      EV.free alloc ev
      CSR.free alloc g
      DS.free alloc ds
      snap <- readAllocSnapshot tracker
      pure (tot, mergedOk, (runIdentity (snapLiveBlocks snap), runIdentity (snapLiveBytes snap)))

  let expSets = if nDs >= 2 then nDs - 1 else nDs
      expDistSum = sum (pureBfs nv adjMap 0)
      expGeVal = case dropWhile (< target) sortedKeys of
        [] -> -1000
        (h : _) -> h
      expEvSum = sum sortedKeys
      expOvSum = sum [val | (lo, hi, val) <- validIntervals, qLo <= qHi, lo <= qHi && hi >= qLo]

  assert hostMergedOk
  cRes === (expSets + expDistSum + expGeVal + expEvSum + expOvSum)
  liveEnd === (0, 0)

test_graph_and_spatial :: TestTree
test_graph_and_spatial =
  let acq = getGraphSpatialTestConfig >>= flip acquireLinuxX86_64C cGraphAndSpatialKernel
   in withResource acq releaseLinuxX86_64C $ \rFn ->
        testGroup
          "Partition, Graph, & Spatial Search Data Structures"
          [ testProperty "DisjointSet equivalence-class partition model, path-halving, & drainAppendUnions" check_disjoint_set_partition_model,
            testProperty "CsrGraph bulk build, adjacency streams, BFS distances, & csrSpMV" check_csr_graph_bfs_and_spmv,
            testProperty "EytzingerVec branchless lookupGE, member, & stackless inOrderStream (sizes 0..300)" check_eytzinger_vec_search_and_inorder,
            testProperty "FlatIntervalMap overlappingStream, pointStabStream, & early-stop zero-leak (sizes 0..250)" check_flat_interval_map_queries_and_early_stop,
            testProperty "Host <-> Compiled C FFI round-trip across DisjointSet, CsrGraph, EytzingerVec, & FlatIntervalMap" (check_graph_spatial_host_c_ffi (fst <$> rFn))
          ]
