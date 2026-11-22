{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Alloc where

import Control.Lens ((&), (.~))
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, nullPtr, ptrToWordPtr)
import qualified Foreign.Storable as Storable
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc
import Janus.Backend.C (JanusC, JanusCM, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Command.Array (CmdStorable (peek, poke))
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Control.Stream (rangeS, streamM)
import Janus.Expression.Cast (ExpPtrCast (..))
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.Hedgehog (testProperty)

getAllocTestConfig :: IO CBuildConfig
getAllocTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

data AllocOp
  = OpAlloc !Int64 !Int64 !Word8 -- size, reqAlign, fillTag
  | OpRealloc !Int !Int64 !Int64 -- slotIndexMod, newSize, reqAlign
  | OpFree !Int
  deriving (Eq, Show)

genAlign :: Gen Int64
genAlign = Gen.element [1, 8, 16, 32, 64, 128]

genAllocOp :: Gen AllocOp
genAllocOp =
  Gen.frequency
    [ (5, OpAlloc <$> Gen.int64 (Range.linear 1 512) <*> genAlign <*> Gen.word8 (Range.linear 1 250)),
      (3, OpRealloc <$> Gen.int (Range.linear 0 50) <*> Gen.int64 (Range.linear 1 768) <*> genAlign),
      (3, OpFree <$> Gen.int (Range.linear 0 50))
    ]

-- | Verify alignment, disjointness (zero overlap corruption of payload bytes),
-- and leak-freedom across an arbitrary sequence of alloc/realloc/free operations!
runDisjointnessTrace ::
  Alloc Identity ->
  Int64 ->
  [AllocOp] ->
  IO ()
runDisjointnessTrace policy minPolicyAlign ops = do
  let checkLive :: [(Ptr (), Int64, Int64, Word8)] -> IO Bool
      checkLive blocks = do
        -- Check alignment & byte integrity
        alignAndPayloadOk <- forM blocks $ \(p, sz, reqAl, tag) -> do
          let expectedAl = fromIntegral (max minPolicyAlign reqAl)
              addr = ptrToWordPtr p
              alOk = addr /= 0 && (addr `mod` expectedAl == 0)
          bytes <- mapM (Storable.peekByteOff p . fromIntegral) [0 .. sz - 1]
          pure (alOk && all (== tag) (bytes :: [Word8]))
        -- Check pairwise interval non-overlap
        let nonOverlap =
              and
                [ let a0 = ptrToWordPtr p1
                      a1 = a0 + fromIntegral s1
                      b0 = ptrToWordPtr p2
                      b1 = b0 + fromIntegral s2
                   in a1 <= b0 || b1 <= a0
                | (i, (p1, s1, _, _)) <- zip [0 :: Int ..] blocks,
                  (j, (p2, s2, _, _)) <- zip [0 :: Int ..] blocks,
                  i < j
                ]
        pure (and alignAndPayloadOk && nonOverlap)

      fillBytes :: Ptr () -> Int64 -> Int64 -> Word8 -> IO ()
      fillBytes p start end tag =
        forM_ [start .. end - 1] $ \off ->
          Storable.pokeByteOff p (fromIntegral off) tag

      step :: [(Ptr (), Int64, Int64, Word8)] -> AllocOp -> IO [(Ptr (), Int64, Int64, Word8)]
      step live op = case op of
        OpAlloc sz al tag -> do
          Identity p <- allocBytesAligned policy (Identity sz) (Identity al)
          if p == nullPtr
            then pure live
            else do
              fillBytes p 0 sz tag
              let live' = (p, sz, al, tag) : live
              ok <- checkLive live'
              if ok then pure live' else error "Disjointness/alignment violation after allocBytesAligned"
        OpRealloc idx newSz al
          | null live -> pure []
          | otherwise -> do
              let k = idx `mod` length live
                  (oldP, oldSz, oldAl, tag) = live !! k
                  rest = take k live ++ drop (k + 1) live
              Identity newP <- reallocBytesAligned policy (Identity oldP) (Identity oldSz) (Identity newSz) (Identity al)
              if newP == nullPtr
                then pure live
                else do
                  -- Prefix [0 .. min oldSz newSz - 1] must still contain `tag`!
                  let common = min oldSz newSz
                  prefix <- mapM (Storable.peekByteOff newP . fromIntegral) [0 .. common - 1]
                  if any (/= tag) (prefix :: [Word8])
                    then error "Prefix corrupted during reallocBytesAligned"
                    else do
                      whenGrowFill newP oldSz newSz tag
                      let _ = oldAl
                          live' = (newP, newSz, al, tag) : rest
                      ok <- checkLive live'
                      if ok then pure live' else error "Disjointness/alignment violation after reallocBytesAligned"
        OpFree idx
          | null live -> pure []
          | otherwise -> do
              let k = idx `mod` length live
                  (oldP, oldSz, _, _) = live !! k
                  rest = take k live ++ drop (k + 1) live
              freeBytes policy (Identity oldP) (Identity oldSz)
              pure rest

      whenGrowFill p oldSz newSz tag
        | newSz > oldSz = fillBytes p oldSz newSz tag
        | otherwise = pure ()

  remaining <- foldlM' step [] ops
  forM_ remaining $ \(p, sz, _, _) ->
    freeBytes policy (Identity p) (Identity sz)
  where
    foldlM' _ acc [] = pure acc
    foldlM' f acc (x : xs) = do
      acc' <- f acc x
      acc' `seq` foldlM' f acc' xs

--------------------------------------------------------------------------------
-- Hedgehog Properties Across All Policies & Configurable Alignments
--------------------------------------------------------------------------------

check_dynamic_and_recycling_disjoint_and_leak_free :: Property
check_dynamic_and_recycling_disjoint_and_leak_free = property $ do
  minAl <- forAll $ Gen.element [8, 16, 32, 64]
  ops <- forAll $ Gen.list (Range.linear 1 80) genAllocOp
  snapEnd <- liftIO $
    withAllocTracker $ \rootTracker -> do
      let rootAlloc = statsAlloc rootTracker dynamicAlloc
      rec <- newRecyclerAligned rootAlloc 64 (Identity minAl)
      withAllocTracker $ \innerTracker -> do
        let policy = statsAlloc innerTracker (recyclingAlloc rec rootAlloc)
        runDisjointnessTrace policy minAl ops
        sInner <- readAllocSnapshot innerTracker
        if runIdentity (snapLiveBlocks sInner) /= 0 || runIdentity (snapLiveBytes sInner) /= 0
          then error "Inner AllocStats leaked"
          else pure ()
      destroyRecycler rootAlloc rec
      readAllocSnapshot rootTracker
  runIdentity (snapLiveBlocks snapEnd) === 0
  runIdentity (snapLiveBytes snapEnd) === 0

check_arena_mark_rewind_and_custom_alignment :: Property
check_arena_mark_rewind_and_custom_alignment = property $ do
  minAl <- forAll $ Gen.element [8, 16, 32, 64, 128]
  slabCap <- forAll $ Gen.int64 (Range.linear 512 2048)
  outerSizes <- forAll $ Gen.list (Range.linear 1 20) (Gen.int64 (Range.linear 1 600))
  innerSizes <- forAll $ Gen.list (Range.linear 1 30) (Gen.int64 (Range.linear 1 3000)) -- includes > slabCap large blocks!
  snapEnd <- liftIO $
    withAllocTracker $ \tracker -> do
      let parent = statsAlloc tracker dynamicAlloc
      ar <- newArenaAligned parent (Identity slabCap) (Identity minAl)
      let policy = arenaAlloc ar parent

      outerBlocks <- forM outerSizes $ \sz -> do
        Identity p <- allocBytes policy (Identity sz)
        let addr = ptrToWordPtr p
        if addr `mod` fromIntegral minAl /= 0
          then error "Arena custom minAlign violated"
          else pure ()
        forM_ [0 .. sz - 1] $ \i -> Storable.pokeByteOff p (fromIntegral i) (0xAA :: Word8)
        pure (p, sz)

      -- Enter scoped savepoint, allocate small & dedicated large blocks, then rewind!
      withArenaScope parent ar $ \scopedPolicy ->
        forM_ innerSizes $ \sz -> do
          Identity p <- allocBytes scopedPolicy (Identity sz)
          forM_ [0 .. sz - 1] $ \i -> Storable.pokeByteOff p (fromIntegral i) (0x55 :: Word8)

      -- Outer blocks before mark must remain 100% intact!
      forM_ outerBlocks $ \(p, sz) -> do
        bs <- mapM (Storable.peekByteOff p . fromIntegral) [0 .. sz - 1]
        if any (/= (0xAA :: Word8)) bs
          then error "Arena rewind corrupted pre-mark block"
          else pure ()

      resetArena parent ar
      destroyArena parent ar
      readAllocSnapshot tracker

  runIdentity (snapLiveBlocks snapEnd) === 0
  runIdentity (snapLiveBytes snapEnd) === 0

check_composable_fallback_segregate_pool_monotonic :: Property
check_composable_fallback_segregate_pool_monotonic = property $ do
  ops <- forAll $ Gen.list (Range.linear 1 100) genAllocOp
  snapEnd <- liftIO $
    withAllocTracker $ \tracker -> do
      let root = statsAlloc tracker dynamicAlloc
      -- Fixed 1KB primary buffer -> spills over to Segregate(<= 64B -> Pool(64B, align 16B), > 64B -> Monotonic(align 16B))!
      fb <- newFixedBufAligned root 1024 16
      mp <- newPoolAligned root 64 16 16
      mono <- newMonotonicAligned root 1024 16
      let composite =
            fallbackAlloc
              (fixedAlloc fb)
              (segregateAlloc 64 (poolAlloc mp root) (monotonicAlloc mono root))
      runDisjointnessTrace composite 16 ops
      resetAlloc composite
      destroyFixedBuf root fb
      destroyPool root mp
      destroyMonotonic root mono
      readAllocSnapshot tracker

  runIdentity (snapLiveBlocks snapEnd) === 0
  runIdentity (snapLiveBytes snapEnd) === 0

--------------------------------------------------------------------------------
-- Compiled C Execution of Composable Allocator Hierarchy
--------------------------------------------------------------------------------

cCompositeAllocKernel :: JanusC Int64 -> JanusCM (JanusC Int64)
cCompositeAllocKernel n = do
  tracker <- newAllocTracker
  let root = statsAlloc tracker dynamicAlloc
  fb <- newFixedBufAligned root 256 16
  ar <- newArenaAligned root 1024 16
  mp <- newPoolAligned root 64 32 16
  let policy =
        fallbackAlloc
          (fixedAlloc fb)
          (segregateAlloc 64 (poolAlloc mp root) (arenaAlloc ar root))

  total <-
    runDrain drainSum $
      streamM (rangeS 0 n) $ \i -> do
        vpSmall <- allocBytes policy 32
        vpLarge <- allocBytesAligned policy 160 64
        let pSmall = fromVoidPtr vpSmall :: JanusC (Ptr Int64)
            pLarge = fromVoidPtr vpLarge :: JanusC (Ptr Int64)
        poke pSmall (i + 1)
        poke pLarge (i * 2)
        v1 <- peek pSmall
        v2 <- peek pLarge
        freeBytes policy vpSmall 32
        freeBytes policy vpLarge 160
        pure (v1 + v2)

  destroyFixedBuf root fb
  destroyPool root mp
  destroyArena root ar
  snap <- readAllocSnapshot tracker
  freeAllocTracker tracker
  pure (snapLiveBytes snap * 1000000 + total)

unit_compiled_c_composite_allocator :: IO ()
unit_compiled_c_allocator_helper :: IO ()
unit_compiled_c_allocator_helper = pure ()

unit_compiled_c_composite_allocator = do
  cfg <- getAllocTestConfig
  res <- withJanusC cfg cCompositeAllocKernel (\fn -> fn 50)
  let expected = sum [(i + 1) + (i * 2) | i <- [0 .. 49 :: Int64]]
  res @?= expected

test_composable_allocators :: TestTree
test_composable_allocators =
  testGroup
    "Composable Memory Allocation Policies"
    [ testProperty "Dynamic & Recycling disjointness, configurable alignment, and zero leaks" check_dynamic_and_recycling_disjoint_and_leak_free,
      testProperty "Arena mark/rewind scopes, dedicated large blocks, and custom minAlign" check_arena_mark_rewind_and_custom_alignment,
      testProperty "Composite Fallback + Segregate + Pool + Monotonic disjointness and zero leaks" check_composable_fallback_segregate_pool_monotonic
    ]
