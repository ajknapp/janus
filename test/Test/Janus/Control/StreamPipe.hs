{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Control.StreamPipe where

import Control.Applicative (Alternative (..))
import Control.Category ((>>>))
import Control.Lens ((&), (.~))
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Janus.Backend.C (JanusC, JanusCM, withJanusC)
import Janus.Backend.C.Build (cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Command.Cond (CmdCond)
-- import qualified Janus.Command.Memory as M
import Janus.Command.Ref (CmdRef, modifyRef, newRef, readRef)
import Janus.Command.While (CmdWhile)
import Janus.Control.Drain (drainFold, drainLength, drainSum, runDrain)
import Janus.Control.Pipe (delayP, dropP, filterP, pipeM, runPipe, takeP)
import Janus.Control.Stream (Stream, bracketS, dropS, filterS, rangeS, runStream, scanS, streamM, takeS, zipWithS)
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

alternativeStreamDigest ::
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
alternativeStreamDigest n =
  let evens = filterS (\x -> (x `remainder` 2) `eq` 0) (rangeS 1 n)
      combined = empty <|> evens <|> pure 7 <|> rangeS 8 10 <|> empty
   in runDrain (drainFold 0 (\acc x -> acc * 10 + x)) combined

alternativeBindDigest ::
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
alternativeBindDigest n =
  runDrain (drainFold 0 (\acc x -> acc * 10 + x)) $ do
    i <- rangeS 1 3 <|> pure n
    pure i <|> pure (i + 1)

alternativeZipSum ::
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
alternativeZipSum n =
  let lhs = empty <|> rangeS 1 3 <|> empty <|> rangeS 3 n <|> empty
      rhs = rangeS 10 20
   in runDrain drainSum $ zipWithS (\a b -> 100 * a + b) lhs rhs

unit_stream_alternative :: IO ()
unit_stream_alternative = do
  -- empty yields 0 elements
  Identity lenEmpty <- runDrain drainLength (empty :: Stream IO (Identity Int64))
  lenEmpty @?= 0

  -- Sequential concatenation with empty, filtered stream (Skips), pure, and rangeS in IO and C
  -- n = 6 -> evens in [1..5]: 2, 4; then 7; then 8, 9 -> 24789
  Identity digestIO <- alternativeStreamDigest @IO @Identity 6
  digestIO @?= 24789

  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
  digestC <- withJanusC cfg (alternativeStreamDigest @JanusCM @JanusC) (\f -> f 6)
  digestC @?= 24789

  -- Interaction with Monad (>>=): i in [1, 2, 5] >>= [i, i + 1] -> [1, 2, 2, 3, 5, 6] -> 122356
  Identity bindIO <- alternativeBindDigest @IO @Identity 5
  bindIO @?= 122356

  bindC <- withJanusC cfg (alternativeBindDigest @JanusCM @JanusC) (\f -> f 5)
  bindC @?= 122356

  -- Lockstep zipWithS across (<|>) and empty transitions (no spurious Skip ticks) in IO and C:
  -- n = 5 -> (1,10), (2,11), (3,12), (4,13) -> 110 + 211 + 312 + 413 = 1046
  Identity zipIO <- alternativeZipSum @IO @Identity 5
  zipIO @?= 1046

  zipC <- withJanusC cfg (alternativeZipSum @JanusCM @JanusC) (\f -> f 5)
  zipC @?= 1046

  -- Step-level Alternative laws under lockstep zipWithS
  let s1 = rangeS (1 :: Identity Int64) 3 -- 1, 2
      s2 = rangeS 3 6 -- 3, 4, 5
      s3 = rangeS 6 8 -- 6, 7
      sRef = rangeS (10 :: Identity Int64) 30
      foldDigits = runDrain (drainFold 0 (\acc x -> acc * 10 + x))
  Identity baseZip <- foldDigits (zipWithS (+) s1 sRef)
  Identity leftIdZip <- foldDigits (zipWithS (+) (empty <|> s1) sRef)
  Identity rightIdZip <- foldDigits (zipWithS (+) (s1 <|> empty) sRef)
  leftIdZip @?= baseZip
  rightIdZip @?= baseZip

  Identity assocL <- foldDigits (zipWithS (+) ((s1 <|> s2) <|> s3) sRef)
  Identity assocR <- foldDigits (zipWithS (+) (s1 <|> (s2 <|> s3)) sRef)
  assocL @?= assocR

  -- dropS 1 >>> takeS 3 across s1 (1, 2) <|> s2 (3, 4, 5) -> 2, 3, 4 -> 234
  Identity sliced <- foldDigits (takeS 3 (dropS 1 (s1 <|> s2)))
  sliced @?= 234

  stepsLeft <- newIORef (0 :: Int)
  let effLeft = takeS 1 (streamM (rangeS (1 :: Identity Int64) 10) (\x -> modifyIORef' stepsLeft (+ 1) >> pure x))
  Identity effRes <- foldDigits (effLeft <|> rangeS 2 6)
  effRes @?= 12345
  leftCount <- readIORef stepsLeft
  leftCount @?= 2

  -- Early termination via takeS still runs bracketS teardown for both branches
  relLeft <- newIORef False
  relRight <- newIORef False
  let leftB = bracketS (pure ()) (\_ -> writeIORef relLeft True) (\_ -> rangeS (1 :: Identity Int64) 4)
      rightB = bracketS (pure ()) (\_ -> writeIORef relRight True) (\_ -> rangeS 4 7)
  Identity early <- foldDigits (takeS 2 (leftB <|> rightB))
  early @?= 12
  wasRelLeft <- readIORef relLeft
  wasRelRight <- readIORef relRight
  wasRelLeft @?= True
  wasRelRight @?= True
