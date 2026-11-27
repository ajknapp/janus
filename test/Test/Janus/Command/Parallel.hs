{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Command.Parallel
  ( test_parallel_argobots,
  )
where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, peekElemOff, poke, sizeOf)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (JanusCCSE, JanusCMCSE, withJanusC)
import Janus.Backend.C.Build (defaultCBuildConfig)
import Janus.Backend.C.CUDA.Atomic (CmdAtomicAdd (..))
import qualified Janus.Command.Memory as M
import Janus.Command.Parallel
  ( CmdParallel (..),
    ParallelDejaFu (..),
    ParallelDejaFuM,
    runParallelDejaFuM,
    withArgobotsXstreams,
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Constraints (LocalArgs (..))
import Janus.Expression.Inject (ExpInject (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

-- | Parallel vector fill + atomic sum, polymorphic across IO, ParallelDejaFuM,
-- and compiled Argobots C!
parFillAndSum ::
  (CmdParallel m e) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Int64) ->
  e (Ptr Int64) ->
  m ()
parFillAndSum n grain outBuf sumPtr =
  parallelForRangeM "fill_sum" 0 n grain (LACons outBuf (LACons sumPtr LANil)) $
    \_wid lo hi (LACons out (LACons sPtr LANil)) ->
      rangeM lo hi $ \i -> do
        let v = i * 3 + 1
        M.pokeElemOff out v i
        _ <- atomicAddPtr sPtr v
        pure ()

-- | Nested parallelForRangeM test (validates deadlock-free work-stealing in Argobots!).
nestedParMatrix ::
  (CmdParallel m e) =>
  e Int64 ->
  e Int64 ->
  e (Ptr Int64) ->
  m ()
nestedParMatrix rows cols outBuf =
  parallelForRangeM "outer_rows" 0 rows 1 (LACons cols (LACons outBuf LANil)) $
    \_widR rLo rHi (LACons cN (LACons buf LANil)) ->
      rangeM rLo rHi $ \r ->
        parallelForRangeM "inner_cols" 0 cN 2 (LACons r (LACons cN (LACons buf LANil))) $
          \_widC cLo cHi (LACons rVal (LACons colsVal (LACons out LANil))) ->
            rangeM cLo cHi $ \c ->
              M.pokeElemOff out (rVal * 100 + c) (rVal * colsVal + c)

test_parallel_argobots :: TestTree
test_parallel_argobots =
  testGroup
    "CmdParallel (Argobots & DejaFu)"
    [ testCase "Argobots compiled C parallelForRangeM + atomicAdd matches serial reference" $
        withArgobotsXstreams 4 $ do
          let n = 1024 :: Int
              nbytes = n * sizeOf (undefined :: Int64)
          allocaBytes nbytes $ \pOut ->
            allocaBytes (sizeOf (undefined :: Int64)) $ \pSum -> do
              poke pSum (0 :: Int64)
              withJanusC
                defaultCBuildConfig
                (\nn gr out s -> parFillAndSum @JanusCMCSE @JanusCCSE nn gr out s)
                (\fn -> fn (fromIntegral n) 32 pOut pSum)
              expectedSum <- pure $ sum [fromIntegral i * 3 + 1 :: Int64 | i <- [0 .. n - 1]]
              actualSum <- peek pSum
              actualSum @?= expectedSum
              forM_ [0 .. n - 1] $ \i -> do
                v <- peekElemOff pOut i
                v @?= fromIntegral i * 3 + 1,
      testCase "Argobots nested parallelForRangeM executes cleanly without deadlock" $
        withArgobotsXstreams 4 $ do
          let rows = 16 :: Int
              cols = 32 :: Int
              total = rows * cols
          allocaBytes (total * sizeOf (undefined :: Int64)) $ \pOut -> do
            withJanusC
              defaultCBuildConfig
              (\r c out -> nestedParMatrix @JanusCMCSE @JanusCCSE r c out)
              (\fn -> fn (fromIntegral rows) (fromIntegral cols) pOut)
            forM_ [0 .. rows - 1] $ \r ->
              forM_ [0 .. cols - 1] $ \c -> do
                v <- peekElemOff pOut (r * cols + c)
                v @?= fromIntegral (r * 100 + c),
      testProperty "DejaFu verifies race-free CmdParallel + CmdAtomicAdd across random sizes/workers" $
        withTests 20 $
          property $ do
            n <- forAll $ Gen.int (Range.linear 1 32)
            grain <- forAll $ Gen.int (Range.linear 1 8)
            workers <- forAll $ Gen.int (Range.linear 1 4)
            (res, actualSum, vals) <- evalIO $
              allocaBytes (n * sizeOf (undefined :: Int64)) $ \pOut ->
                allocaBytes (sizeOf (undefined :: Int64)) $ \pSum -> do
                  poke pSum (0 :: Int64)
                  (r, _tr) <-
                    runParallelDejaFuM workers $
                      parFillAndSum @ParallelDejaFuM @ParallelDejaFu
                        (fromIntegral n)
                        (fromIntegral grain)
                        (inject pOut)
                        (inject pSum)
                  s <- peek pSum
                  vs <- mapM (peekElemOff pOut) [0 .. n - 1]
                  pure (r, s, vs)
            res === Right ()
            let expected = [fromIntegral i * 3 + 1 :: Int64 | i <- [0 .. n - 1]]
            vals === expected
            actualSum === sum expected,
      testProperty "DejaFu verifies lock-free atomicCAS64 counter under concurrent workers" $
        withTests 15 $
          property $ do
            n <- forAll $ Gen.int (Range.linear 1 20)
            workers <- forAll $ Gen.int (Range.linear 2 4)
            (res, finalVal) <- evalIO $
              allocaBytes (sizeOf (undefined :: Word64)) $ \pCnt -> do
                poke pCnt (0 :: Word64)
                (r, _tr) <-
                  runParallelDejaFuM workers $
                    parallelForRangeM @ParallelDejaFuM @ParallelDejaFu
                      "cas_inc"
                      0
                      (fromIntegral n)
                      1
                      (LACons (inject pCnt) LANil)
                      $ \_wid lo hi (LACons p LANil) ->
                        rangeM lo hi $ \_ -> do
                          _ <- atomicAddPtr p 1
                          pure ()
                v <- peek pCnt
                pure (r, v)
            res === Right ()
            finalVal === fromIntegral n
    ]
