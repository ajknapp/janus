{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Fix where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Expression.Cond
import Janus.Expression.Ord
import Janus.Fix
import Janus.Typed
import System.Environment
import System.Mem (performGC)
import Test.Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.Hedgehog

fac ::
  forall e a.
  ( Num (e a),
    ExpCond e,
    ExpOrd e a,
    ExpFix e (e a -> e a),
    JanusTyped e a
  ) =>
  e a ->
  e a
fac = fun (Proxy @e) "fac" $ \self n -> ifThenElse (n `le` 0) 1 (n * self (n - 1))

compiledFacEqualsInterpretedFac :: IO (Int64 -> Int64) -> Property
compiledFacEqualsInterpretedFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = runIdentity $ fac (Identity n)
      m = f' n
  k === m

compiledFacEqualsSpecFac :: IO (Int64 -> Int64) -> Property
compiledFacEqualsSpecFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = product [1 .. n]
      m = f' n
  k === m

unit_withJanusC_lazy_dl_lifetime :: IO ()
unit_withJanusC_lazy_dl_lifetime = do
  tmpdir <- getEnv "TMPDIR"
  let cfg = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
  lazyConst <- withJanusC cfg (fac @JanusC @Int64 10) pure
  lazyApp <- withJanusC cfg (fac @JanusC @Int64) (\f -> pure (f 10))
  escapedFn <- withJanusC cfg (fac @JanusC @Int64) pure
  performGC
  threadDelay 10000
  lazyConst @?= 3628800
  lazyApp @?= 3628800
  escapedFn 10 @?= 3628800

test_fix :: TestTree
test_fix = do
  let acquire f = do
        tmpdir <- getEnv "TMPDIR"
        acquireJanusC JC (defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir) f
  withResource (acquire (fac @JanusC @Int64)) releaseJanusC $ \k ->
    let k' = fst <$> k
     in testGroup
        "fix"
        [ testProperty "compiled fac = interpreted fac" (compiledFacEqualsInterpretedFac k'),
          testProperty "compiled fac = spec fac" (compiledFacEqualsSpecFac k')
        ]
