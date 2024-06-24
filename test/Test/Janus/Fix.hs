{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Fix where

import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Int
import Data.Proxy
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C
import Janus.Expression.Cond
import Janus.Expression.Ord
import Janus.Fix
import Janus.Typed
import Test.Tasty
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

compiledFacEqualsInterpretedFac :: IO (Int64 -> IO Int64) -> Property
compiledFacEqualsInterpretedFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = runIdentity $ fac (Identity n)
  m <- liftIO $ f' n
  k === m

compiledFacEqualsSpecFac :: IO (Int64 -> IO Int64) -> Property
compiledFacEqualsSpecFac f = property $ do
  f' <- liftIO f
  n <- forAll $ Gen.int64 (Range.constant 0 20)
  let k = product [1 .. n]
  m <- liftIO $ f' n
  k === m

test_fix :: TestTree
test_fix = withResource (acquireJanusC (fac @JanusC @Int64)) releaseJanusC $ \k ->
  let k' = fst <$> k
   in testGroup
        "fix"
        [ testProperty "compiled fac = interpreted fac" (compiledFacEqualsInterpretedFac k'),
          testProperty "compiled fac = spec fac" (compiledFacEqualsSpecFac k')
        ]
