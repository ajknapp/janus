{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Expression.SpecialMath
  ( test_special_math,
  )
where

import Data.Functor.Identity (Identity (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (JanusCCSE, withJanusC)
import Janus.Backend.C.Build (defaultCBuildConfig)
import Janus.Backend.C.CUDA (CUDAIO (..), CUDADejaFu (..))
import Janus.Expression.SpecialMath (ExpSpecialMath (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

approxEq :: Double -> Double -> Double -> Double -> Bool
approxEq relTol absTol a b =
  abs (a - b) <= absTol + relTol * max (abs a) (abs b)

test_special_math :: TestTree
test_special_math =
  testGroup
    "ExpSpecialMath"
    [ testCase "digamma and trigamma match central finite differences of lgamma" $ do
        let xs = [0.25, 0.5, 1.0, 1.5, 2.5, 5.0, 10.0, 25.0 :: Double]
            h = 1e-6 :: Double
        forM_ xs $ \x -> do
          let lgPlus = runIdentity (lgammaE (Identity (x + h)))
              lgMinus = runIdentity (lgammaE (Identity (x - h)))
              fdPsi = (lgPlus - lgMinus) / (2 * h)
              psiVal = runIdentity (digammaE (Identity x))
              psiPlus = runIdentity (digammaE (Identity (x + h)))
              psiMinus = runIdentity (digammaE (Identity (x - h)))
              fdTri = (psiPlus - psiMinus) / (2 * h)
              triVal = runIdentity (trigammaE (Identity x))
          assertBool
            ("digamma mismatch at x=" <> show x <> ": got " <> show psiVal <> ", fd=" <> show fdPsi)
            (approxEq 1e-6 1e-7 psiVal fdPsi)
          assertBool
            ("trigamma mismatch at x=" <> show x <> ": got " <> show triVal <> ", fd=" <> show fdTri)
            (approxEq 1e-5 1e-6 triVal fdTri),
      testCase "erf/erfinv round-trip and erfc complement" $ do
        let xs = [-0.99, -0.75, -0.2, 0.0, 0.35, 0.85, 0.999 :: Double]
        forM_ xs $ \x -> do
          let y = runIdentity (erfinvE (Identity x))
              x' = runIdentity (erfE (Identity y))
              ec = runIdentity (erfcE (Identity y))
          assertBool
            ("erf(erfinv(x)) mismatch at x=" <> show x <> ": got " <> show x')
            (approxEq 1e-12 1e-12 x x')
          assertBool
            ("erf + erfc /= 1 at y=" <> show y)
            (approxEq 1e-12 1e-12 (x' + ec) 1.0),
      testCase "CUDAIO and CUDADejaFu emulate ExpSpecialMath identically to Identity" $ do
        let x = 2.75 :: Double
        getCUDAIO (lgammaE (CUDAIO x)) @?= runIdentity (lgammaE (Identity x))
        getCUDAIO (digammaE (CUDAIO x)) @?= runIdentity (digammaE (Identity x))
        getCUDAIO (trigammaE (CUDAIO x)) @?= runIdentity (trigammaE (Identity x))
        getCUDADejaFu (erfinvE (CUDADejaFu (0.5 :: Double))) @?= runIdentity (erfinvE (Identity 0.5))
        getCUDADejaFu (rsqrtE (CUDADejaFu (4.0 :: Double))) @?= runIdentity (rsqrtE (Identity 4.0)),
      testProperty "compiled C CSE matches Identity across special functions" $
        withTests 25 $
          property $ do
            x <- forAll $ Gen.double (Range.linearFrac 0.15 20.0)
            u <- forAll $ Gen.double (Range.linearFrac (-0.95) 0.95)
            let expr :: (ExpSpecialMath e Double) => e Double -> e Double -> e Double
                expr a b = lgammaE a + digammaE a + trigammaE a + erfE b + erfcE b + erfinvE b + log1pE a + expm1E (b * 0.5) + rsqrtE a
                expected = runIdentity (expr (Identity x) (Identity u))
            actual <- evalIO $ withJanusC defaultCBuildConfig (\a b -> expr @JanusCCSE a b) $ \fn -> pure (fn x u)
            Hedgehog.assert (approxEq 1e-11 1e-11 actual expected)
    ]
  where
    forM_ = flip mapM_
