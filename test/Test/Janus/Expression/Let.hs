{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Expression.Let where

import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Int
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C
import Janus.Expression.Let
import Janus.Typed
import Test.Tasty
import Test.Tasty.Hedgehog

pow :: (Num (e a), JanusTyped e a, ExpLet e) => e a -> Int -> e a
pow _ 0 = 1
pow x 1 = x
pow x n = let_ (pow x (n `div` 2)) $ \y -> if even n then y * y else x * y * y

pow5 :: forall e a. (Num (e a), JanusTyped e a, ExpLet e) => e a -> e a
pow5 x = pow x 5

pow6 :: forall e a. (Num (e a), JanusTyped e a, ExpLet e) => e a -> e a
pow6 x = pow x 6

pownEqualsStandard :: IO (Int64 -> IO Int64) -> Int -> Property
pownEqualsStandard p n = property $ do
  p' <- liftIO p
  x <- forAll (Gen.int64 $ Range.linear 0 1000)
  y <- liftIO (p' x)
  let z = x ^ n
  y === z

pownCompiledEqualsInterpreted :: IO (Int64 -> IO Int64) -> Int -> Property
pownCompiledEqualsInterpreted p n = property $ do
  p' <- liftIO p
  x <- forAll (Gen.int64 $ Range.linear 0 1000)
  y <- liftIO (p' x)
  let z = runIdentity $ pow (Identity x) n
  y === z

test_let :: TestTree
test_let = withResource (acquireJanusC JC (pow5 @JanusC @Int64)) releaseJanusC $ \cpow5 ->
  withResource (acquireJanusC JC (pow6 @JanusC @Int64)) releaseJanusC $ \cpow6 ->
    let cpow5' = fst <$> cpow5
        cpow6' = fst <$> cpow6
     in testGroup
          "let"
          [ testProperty "compile pow5 = interpret pow5" (pownCompiledEqualsInterpreted cpow5' 5),
            testProperty "compile pow6 = interpret pow6" (pownCompiledEqualsInterpreted cpow6' 6),
            testProperty "compile pow5 = standard pow5" (pownEqualsStandard cpow5' 5),
            testProperty "compile pow6 = standard pow6" (pownEqualsStandard cpow6' 6)
          ]
