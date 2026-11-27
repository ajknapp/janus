{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Special mathematical functions for numerical computing and Bayesian
-- log-densities ('lgammaE', 'digammaE', 'trigammaE', 'erfE', 'erfcE',
-- 'erfinvE', 'log1pE', 'expm1E', 'rsqrtE'), with instances across 'Identity',
-- 'JanusCT', 'JanusCUDA', 'CUDAIO', and 'CUDADejaFu'.
module Janus.Expression.SpecialMath
  ( ExpSpecialMath (..),
    digammaScalar,
    trigammaScalar,
    erfinvScalar,
  )
where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import Data.Loc (noLoc)
import Foreign.C.Types (CDouble (..), CFloat (..))
import GHC.Float (expm1, log1p)
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..), JanusCUDA (..))
import Janus.Backend.C.Raw (JanusCT (..), RVal (..), addHeader)
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)
import Language.C.Quote (Exp (..), Id (..))

foreign import ccall unsafe "lgamma" c_lgamma :: CDouble -> CDouble
foreign import ccall unsafe "lgammaf" c_lgammaf :: CFloat -> CFloat
foreign import ccall unsafe "erf" c_erf :: CDouble -> CDouble
foreign import ccall unsafe "erff" c_erff :: CFloat -> CFloat
foreign import ccall unsafe "erfc" c_erfc :: CDouble -> CDouble
foreign import ccall unsafe "erfcf" c_erfcf :: CFloat -> CFloat

-- | Special mathematical functions closed under differentiation
-- (@d/dx lgamma = digamma@, @d/dx digamma = trigamma@, @d/dx erf = 2/sqrt(pi) * exp(-x^2)@).
class (Floating (e a)) => ExpSpecialMath e a where
  lgammaE :: e a -> e a
  digammaE :: e a -> e a
  trigammaE :: e a -> e a
  erfE :: e a -> e a
  erfcE :: e a -> e a
  erfinvE :: e a -> e a
  log1pE :: e a -> e a
  expm1E :: e a -> e a
  rsqrtE :: e a -> e a
  rsqrtE x = 1 / sqrt x

--------------------------------------------------------------------------------
-- Portable Embedded Implementations of Digamma, Trigamma, & Erfinv
--------------------------------------------------------------------------------

-- | Embedded branch-free unrolled recurrence + Bernoulli asymptotic series for
-- the digamma function @psi(x)@ for @x > 0@ (accurate to @< 1e-14@ in Double
-- and @< 1e-6@ in Float, works identically on CPU and CUDA GPU).
digammaScalar ::
  (Floating (e a), ExpOrd e a, ExpCond e, ExpLet e, JanusTyped e a, JanusTyped e Bool) =>
  e a ->
  e a
digammaScalar x0 = go (8 :: Int) x0 0
  where
    go 0 x acc =
      let_ (1 / x) $ \invX ->
        let_ (invX * invX) $ \invX2 ->
          acc
            + log x
            - 0.5 * invX
            - invX2
              * ( (1 / 12)
                    - invX2
                      * ( (1 / 120)
                            - invX2
                              * ( (1 / 252)
                                    - invX2 * ((1 / 240) - invX2 * (1 / 132))
                                )
                        )
                )
    go k x acc =
      let_ (x `lt` 8) $ \small ->
        let_ (ifThenElse small (x + 1) x) $ \x' ->
          let_ (ifThenElse small (acc - 1 / x) acc) $ \acc' ->
            go (k - 1) x' acc'

-- | Embedded branch-free unrolled recurrence + Bernoulli asymptotic series for
-- the trigamma function @psi_1(x)@ for @x > 0@ (accurate to @< 1e-14@ in Double).
trigammaScalar ::
  (Floating (e a), ExpOrd e a, ExpCond e, ExpLet e, JanusTyped e a, JanusTyped e Bool) =>
  e a ->
  e a
trigammaScalar x0 = go (8 :: Int) x0 0
  where
    go 0 x acc =
      let_ (1 / x) $ \invX ->
        let_ (invX * invX) $ \invX2 ->
          acc
            + invX
            + 0.5 * invX2
            + (invX * invX2)
              * ( (1 / 6)
                    - invX2
                      * ( (1 / 30)
                            - invX2
                              * ( (1 / 42)
                                    - invX2 * ((1 / 30) - invX2 * (5 / 66))
                                )
                        )
                )
    go k x acc =
      let_ (x `lt` 8) $ \small ->
        let_ (ifThenElse small (x + 1) x) $ \x' ->
          let_ (ifThenElse small (acc + 1 / (x * x)) acc) $ \acc' ->
            go (k - 1) x' acc'

-- | Giles (2010) rational approximation for @erfinv(x)@ on @(-1, 1)@ followed by
-- two Halley refinement steps using @erfFn@, achieving full IEEE-754 Double
-- precision across CPU and portable C backends.
erfinvScalar ::
  (Floating (e a), ExpOrd e a, ExpCond e, ExpLet e, JanusTyped e a, JanusTyped e Bool) =>
  (e a -> e a) ->
  e a ->
  e a
erfinvScalar erfFn x =
  let_ (negate (log ((1 - x) * (1 + x)))) $ \w ->
    let_ (w `lt` 5.0) $ \central ->
      let_ (ifThenElse central (w - 2.5) (sqrt w - 3.0)) $ \u ->
        let_ (ifThenElse central (polyCentral u) (polyTail u)) $ \p ->
          let_ (p * x) $ \y0 ->
            halleyStep (halleyStep y0)
  where
    sqrtPiOver2 = 0.886226925452758013649
    halleyStep y =
      let_ (erfFn y - x) $ \err ->
        let_ (err * sqrtPiOver2 * exp (y * y)) $ \d ->
          y - d / (1 + y * d)
    polyCentral u =
      (((((((2.81022636e-08 * u + 3.43273939e-07) * u - 3.5233877e-06) * u - 4.39150654e-06) * u + 0.00021858087) * u - 0.00125372503) * u - 0.00417768164) * u + 0.246640727) * u + 1.50140941
    polyTail u =
      ((((((((-0.000200214257 * u + 0.000100950558) * u + 0.00134934322) * u - 0.00367342844) * u + 0.00573950773) * u - 0.0076224613) * u + 0.00943887047) * u + 1.00167406) * u + 2.83297682)

--------------------------------------------------------------------------------
-- Identity Instances
--------------------------------------------------------------------------------

instance ExpSpecialMath Identity Double where
  lgammaE (Identity x) = Identity (coerce (c_lgamma (coerce x)))
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE (Identity x) = Identity (coerce (c_erf (coerce x)))
  erfcE (Identity x) = Identity (coerce (c_erfc (coerce x)))
  erfinvE = erfinvScalar erfE
  log1pE (Identity x) = Identity (log1p x)
  expm1E (Identity x) = Identity (expm1 x)

instance ExpSpecialMath Identity Float where
  lgammaE (Identity x) = Identity (coerce (c_lgammaf (coerce x)))
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE (Identity x) = Identity (coerce (c_erff (coerce x)))
  erfcE (Identity x) = Identity (coerce (c_erfcf (coerce x)))
  erfinvE = erfinvScalar erfE
  log1pE (Identity x) = Identity (log1p x)
  expm1E (Identity x) = Identity (expm1 x)

instance ExpSpecialMath Identity CDouble where
  lgammaE (Identity x) = Identity (c_lgamma x)
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE (Identity x) = Identity (c_erf x)
  erfcE (Identity x) = Identity (c_erfc x)
  erfinvE = erfinvScalar erfE
  log1pE (Identity (CDouble x)) = Identity (CDouble (log1p x))
  expm1E (Identity (CDouble x)) = Identity (CDouble (expm1 x))

instance ExpSpecialMath Identity CFloat where
  lgammaE (Identity x) = Identity (c_lgammaf x)
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE (Identity x) = Identity (c_erff x)
  erfcE (Identity x) = Identity (c_erfcf x)
  erfinvE = erfinvScalar erfE
  log1pE (Identity (CFloat x)) = Identity (CFloat (log1p x))
  expm1E (Identity (CFloat x)) = Identity (CFloat (expm1 x))

--------------------------------------------------------------------------------
-- JanusCT t (C99 <math.h>) Instances
--------------------------------------------------------------------------------

cMathCall1 :: String -> JanusCT t a -> JanusCT t a
cMathCall1 fn (JanusCT ma) = JanusCT $ do
  addHeader "math.h"
  RVal a <- ma
  pure (RVal (FnCall (Var (Id fn noLoc) noLoc) [a] noLoc))

instance ExpSpecialMath (JanusCT t) Double where
  lgammaE = cMathCall1 "lgamma"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cMathCall1 "erf"
  erfcE = cMathCall1 "erfc"
  erfinvE = erfinvScalar erfE
  log1pE = cMathCall1 "log1p"
  expm1E = cMathCall1 "expm1"

instance ExpSpecialMath (JanusCT t) Float where
  lgammaE = cMathCall1 "lgammaf"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cMathCall1 "erff"
  erfcE = cMathCall1 "erfcf"
  erfinvE = erfinvScalar erfE
  log1pE = cMathCall1 "log1pf"
  expm1E = cMathCall1 "expm1f"

instance ExpSpecialMath (JanusCT t) CDouble where
  lgammaE = cMathCall1 "lgamma"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cMathCall1 "erf"
  erfcE = cMathCall1 "erfc"
  erfinvE = erfinvScalar erfE
  log1pE = cMathCall1 "log1p"
  expm1E = cMathCall1 "expm1"

instance ExpSpecialMath (JanusCT t) CFloat where
  lgammaE = cMathCall1 "lgammaf"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cMathCall1 "erff"
  erfcE = cMathCall1 "erfcf"
  erfinvE = erfinvScalar erfE
  log1pE = cMathCall1 "log1pf"
  expm1E = cMathCall1 "expm1f"

--------------------------------------------------------------------------------
-- JanusCUDA (CUDA Device Math Intrinsics) & CPU Emulation Instances
--------------------------------------------------------------------------------

cudaMathCall1 :: String -> JanusCUDA a -> JanusCUDA a
cudaMathCall1 fn (JanusCUDA (JanusCT ma)) = JanusCUDA $ JanusCT $ do
  RVal a <- ma
  pure (RVal (FnCall (Var (Id fn noLoc) noLoc) [a] noLoc))

instance ExpSpecialMath JanusCUDA Double where
  lgammaE = cudaMathCall1 "lgamma"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cudaMathCall1 "erf"
  erfcE = cudaMathCall1 "erfc"
  erfinvE = cudaMathCall1 "erfinv"
  log1pE = cudaMathCall1 "log1p"
  expm1E = cudaMathCall1 "expm1"
  rsqrtE = cudaMathCall1 "rsqrt"

instance ExpSpecialMath JanusCUDA Float where
  lgammaE = cudaMathCall1 "lgammaf"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cudaMathCall1 "erff"
  erfcE = cudaMathCall1 "erfcf"
  erfinvE = cudaMathCall1 "erfinvf"
  log1pE = cudaMathCall1 "log1pf"
  expm1E = cudaMathCall1 "expm1f"
  rsqrtE = cudaMathCall1 "rsqrtf"

instance ExpSpecialMath JanusCUDA CDouble where
  lgammaE = cudaMathCall1 "lgamma"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cudaMathCall1 "erf"
  erfcE = cudaMathCall1 "erfc"
  erfinvE = cudaMathCall1 "erfinv"
  log1pE = cudaMathCall1 "log1p"
  expm1E = cudaMathCall1 "expm1"
  rsqrtE = cudaMathCall1 "rsqrt"

instance ExpSpecialMath JanusCUDA CFloat where
  lgammaE = cudaMathCall1 "lgammaf"
  digammaE = digammaScalar
  trigammaE = trigammaScalar
  erfE = cudaMathCall1 "erff"
  erfcE = cudaMathCall1 "erfcf"
  erfinvE = cudaMathCall1 "erfinvf"
  log1pE = cudaMathCall1 "log1pf"
  expm1E = cudaMathCall1 "expm1f"
  rsqrtE = cudaMathCall1 "rsqrtf"

instance (ExpSpecialMath Identity a, Floating (CUDAIO a)) => ExpSpecialMath CUDAIO a where
  lgammaE = coerce (lgammaE @Identity @a)
  digammaE = coerce (digammaE @Identity @a)
  trigammaE = coerce (trigammaE @Identity @a)
  erfE = coerce (erfE @Identity @a)
  erfcE = coerce (erfcE @Identity @a)
  erfinvE = coerce (erfinvE @Identity @a)
  log1pE = coerce (log1pE @Identity @a)
  expm1E = coerce (expm1E @Identity @a)
  rsqrtE = coerce (rsqrtE @Identity @a)

instance (ExpSpecialMath Identity a, Floating (CUDADejaFu a)) => ExpSpecialMath CUDADejaFu a where
  lgammaE = coerce (lgammaE @Identity @a)
  digammaE = coerce (digammaE @Identity @a)
  trigammaE = coerce (trigammaE @Identity @a)
  erfE = coerce (erfE @Identity @a)
  erfcE = coerce (erfcE @Identity @a)
  erfinvE = coerce (erfinvE @Identity @a)
  log1pE = coerce (log1pE @Identity @a)
  expm1E = coerce (expm1E @Identity @a)
  rsqrtE = coerce (rsqrtE @Identity @a)
