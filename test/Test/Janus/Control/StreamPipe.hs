{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Control.StreamPipe where

import Control.Category ((>>>))
import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Data.Functor.Identity (Identity (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Janus.Backend.C (JanusC, JanusCM, withJanusC)
import Janus.Backend.C.Build (cBuildConfigCacheDir, defaultCBuildConfig)
import qualified Janus.Command.Array as A
import Janus.Command.Cond (CmdCond)
import Janus.Command.Ref (CmdRef, modifyRef, newRef, readRef)
import Janus.Command.While (CmdWhile)
import Janus.Control.Pipe (delayP, dropP, filterP, pipeM, runDrain, runPipe, takeP, drainLength, drainSum)
import Janus.Control.Stream (bracketS, dropS, filterS, rangeS, runStream, scanS, streamM, takeS, zipWithS)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Eq (eq)
import Janus.Expression.Integral (ExpIntegral, remainder)
import Janus.Expression.Ord (ExpOrd)
import Janus.Typed (JanusTyped)
import System.Environment (getEnv)
import Test.Tasty.HUnit ((@?=))

pipelineSum ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    ExpBool e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  e Int64 ->
  m (e Int64)
pipelineSum n =
  let evens = filterP (\x -> (x `remainder` 2) `eq` 0)
      window = dropP 2 >>> takeP 4
      doubled = pipeM (\x -> pure (x * 2))
   in runDrain drainSum $ runPipe (evens >>> window >>> doubled) (rangeS 0 n)

unit_pipe_stream_io_and_c :: IO ()
unit_pipe_stream_io_and_c = do
  -- Evens in [0..19]: 0,2,4,6,8,10,12,14,16,18 -> drop 2: 4,6,8,10 -> take 4: 4,6,8,10 -> *2: 8+12+16+20 = 56
  Identity resIO <- pipelineSum @IO @Identity 20
  resIO @?= 56

  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
  resC <- withJanusC cfg (pipelineSum @JanusCM @JanusC) (\f -> f 20)
  resC @?= 56

cartesianWeightedSum ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    ExpBool e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  e Int64 ->
  m (e Int64)
cartesianWeightedSum n =
  runDrain drainSum $ do
    i <- rangeS 0 n
    j <- rangeS 0 i
    pure (10 * i + j)

unit_stream_monad_bind :: IO ()
unit_stream_monad_bind = do
  -- i = 0: empty inner stream (rangeS 0 0) -> must not stop outer stream!
  -- i = 1: j = 0 -> 10
  -- i = 2: j = 0, 1 -> 20 + 21 = 41
  -- i = 3: j = 0, 1, 2 -> 30 + 31 + 32 = 93
  -- Total = 144
  Identity resIO <- cartesianWeightedSum @IO @Identity 4
  resIO @?= 144

  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
  resC <- withJanusC cfg (cartesianWeightedSum @JanusCM @JanusC) (\f -> f 4)
  resC @?= 144

unit_stream_combinators_and_bracket :: IO ()
unit_stream_combinators_and_bracket = do
  -- dropS over filtered stream (must skip Skips and stop cleanly when n > remaining elements)
  let evens = filterS (\x -> (x `remainder` 2) `eq` 0) (rangeS (0 :: Identity Int64) 10)
  -- evens: 0, 2, 4, 6, 8 -> dropS 2: 4, 6, 8 -> takeS 2: 4, 6 -> sum = 10
  Identity s1 <- runDrain drainSum (takeS 2 (dropS 2 evens))
  s1 @?= 10

  -- dropS past stream end -> 0 elements
  Identity lenEmpty <- runDrain drainLength (dropS 10 evens)
  lenEmpty @?= 0

  -- zipWithS + scanS + delayP
  let zipped = zipWithS (+) (rangeS (1 :: Identity Int64) 5) (scanS 0 (+) (rangeS 1 5))
  -- range: 1,2,3,4; scan: 1,3,6,10; zipped: 2,5,9,14 -> delayP 100: 100,2,5,9 -> sum = 116
  Identity s2 <- runDrain drainSum (runPipe (delayP 100) zipped)
  s2 @?= 116

  -- bracketS setup & teardown verification
  released <- newIORef False
  acc <- newRef (0 :: Identity Int64)
  let sBracket =
        bracketS
          (pure (10 :: Int64))
          (\_ -> writeIORef released True)
          (\base -> streamM (rangeS (0 :: Identity Int64) 3) (\x -> modifyRef acc (+ (x + Identity base))))
  runStream sBracket
  Identity total <- readRef acc
  wasReleased <- readIORef released
  total @?= 33
  wasReleased @?= True

unit_array_inner_product_and_norm2 :: IO ()
unit_array_inner_product_and_norm2 = do
  let bds = A.tensorBound (Proxy @'[4])
  A.withTensor bds $ \(vx :: A.Vector 4 Identity Float) ->
    A.withTensor bds $ \(vy :: A.Vector 4 Identity Float) -> do
      forM_ [0 .. 3 :: Int64] $ \i -> do
        A.writeTensor vx (Identity i A.:. A.Z) (Identity (fromIntegral (i + 1)))
        A.writeTensor vy (Identity i A.:. A.Z) (Identity (fromIntegral (2 * (i + 1))))
      -- vx = [1,2,3,4], vy = [2,4,6,8]
      -- innerProduct = 2 + 8 + 18 + 32 = 60
      -- vectorNorm2 vx = 1 + 4 + 9 + 16 = 30
      Identity ip <- A.innerProduct vx vy
      Identity n2 <- A.vectorNorm2 vx
      ip @?= 60.0
      n2 @?= 30.0
