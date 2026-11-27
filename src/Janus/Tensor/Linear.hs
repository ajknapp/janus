{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Integration of @Janus.Tensor@ with Edward Kmett's @adjunctions@,
-- @distributive@, and @linear@ libraries:
--
-- * 'Applicative', 'Distributive', and 'Representable' instances on @'Tab' i@
--   with @'Rep' ('Tab' i) = 'IxExpr' i@ (Dex's @for@/@!@ = Kmett's @tabulate@/@index@!).
-- * 'Linear.Vector.Additive', 'Linear.Metric.Metric', and 'Linear.Trace.Trace'
--   instances directly on @'Tab' i@, plus 'Linear.Conjugate.Conjugate' on @'Expr' a@.
-- * Zero-boilerplate converters between @'Tab' ('IxFin' n) ('Expr' a)@ and
--   @linear@'s 'V2', 'V3', 'V4', 'M22', 'M33', 'M44', and 'Linear.V.V'.
-- * Contraction-backed matrix/vector operators ('dotT', '(!*!)', '(!*)',
--   '(*!)', 'outerT', 'traceT') that never unroll large dimensions in Haskell.
module Janus.Tensor.Linear
  ( -- * Re-exported Kmett Algebra & `linear` Classes
    Distributive (..),
    Representable (..),
    Additive (..),
    Metric (..),
    Trace (..),
    Conjugate (..),
    (*^),
    (^*),
    (^/),
    negated,
    sumV,
    V2 (..),
    V3 (..),
    V4 (..),
    M22,
    M33,
    M44,
    Quaternion (..),

    -- * Zero-Unroll Tensor Linear Algebra Operators
    dotT,
    quadranceT,
    normT,
    traceT,
    outerT,
    matmulT,
    matvecT,
    vecmatT,
    curryTab,
    uncurryTab,

    -- * Interop Bridges: `Tab` <-> `linear` Fixed-Size Vectors & Matrices
    v2ToTab,
    tabToV2,
    v3ToTab,
    tabToV3,
    v4ToTab,
    tabToV4,
    m22ToTab,
    tabToM22,
    m33ToTab,
    tabToM33,
    m44ToTab,
    tabToM44,
    linearVToTab,
    tabToLinearV,
  )
where

import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.TypeNats (KnownNat, natVal)
import Janus.Tensor.Algebra (AddOp, TensorMonoid, TensorScalar)
import Janus.Tensor.DSL (forIx, ixConst, ixFst, ixOrd, ixPair, ixSnd, select, sumIx, (!), (.==))
import Janus.Tensor.IR (Expr, Tab (..))
import Janus.Tensor.Index (IxKind (..), IxSet (..))
import Linear.Conjugate (Conjugate (..))
import Linear.Matrix (M22, M33, M44)
import Linear.Metric (Metric (..))
import Linear.Quaternion (Quaternion (..))
import Linear.Trace (Trace (..))
import qualified Linear.V as LV
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Linear.Vector (Additive (..), negated, sumV, (*^), (^*), (^/))

--------------------------------------------------------------------------------
-- 1. O(1)-IR Zero-Unroll Tensor Linear Algebra Operators
-- (Non-orphan Applicative, Distributive, Representable, Additive, Metric,
-- Trace, and Conjugate instances for `Tab` and `Expr` live in `Janus.Tensor.IR`.)
--------------------------------------------------------------------------------

-- | Embedded inner product lowering directly to a single 'Reduce AddOp' node
-- (automatically recognized as a contraction by the contraction planner!).
dotT ::
  (IxSet i, TensorMonoid AddOp a, Num (Expr a)) =>
  Tab i (Expr a) ->
  Tab i (Expr a) ->
  Expr a
dotT u v = sumIx (\ix -> u ! ix * v ! ix)

quadranceT ::
  (IxSet i, TensorMonoid AddOp a, Num (Expr a)) =>
  Tab i (Expr a) ->
  Expr a
quadranceT u = dotT u u

normT ::
  (IxSet i, TensorMonoid AddOp a, Floating (Expr a)) =>
  Tab i (Expr a) ->
  Expr a
normT = sqrt . quadranceT

traceT ::
  (IxSet i, TensorMonoid AddOp a) =>
  Tab ('IxProd i i) (Expr a) ->
  Expr a
traceT m = sumIx (\ix -> m ! ixPair ix ix)

outerT ::
  (IxSet i, IxSet j, Num (Expr a)) =>
  Tab i (Expr a) ->
  Tab j (Expr a) ->
  Tab ('IxProd i j) (Expr a)
outerT u v = forIx (\p -> u ! ixFst p * v ! ixSnd p)

matmulT ::
  (IxSet m, IxSet k, IxSet n, TensorMonoid AddOp a, Num (Expr a)) =>
  Tab ('IxProd m k) (Expr a) ->
  Tab ('IxProd k n) (Expr a) ->
  Tab ('IxProd m n) (Expr a)
matmulT a b =
  forIx $ \p ->
    let i = ixFst p
        j = ixSnd p
     in sumIx $ \k -> a ! ixPair i k * b ! ixPair k j

matvecT ::
  (IxSet m, IxSet k, TensorMonoid AddOp a, Num (Expr a)) =>
  Tab ('IxProd m k) (Expr a) ->
  Tab k (Expr a) ->
  Tab m (Expr a)
matvecT a x = forIx $ \i -> sumIx $ \k -> a ! ixPair i k * x ! k

vecmatT ::
  (IxSet k, IxSet n, TensorMonoid AddOp a, Num (Expr a)) =>
  Tab k (Expr a) ->
  Tab ('IxProd k n) (Expr a) ->
  Tab n (Expr a)
vecmatT x a = forIx $ \j -> sumIx $ \k -> x ! k * a ! ixPair k j

curryTab :: (IxSet i, IxSet j) => Tab ('IxProd i j) a -> Tab i (Tab j a)
curryTab t = forIx (\i -> forIx (\j -> t ! ixPair i j))

uncurryTab :: (IxSet i, IxSet j) => Tab i (Tab j a) -> Tab ('IxProd i j) a
uncurryTab t = forIx (\p -> (t ! ixFst p) ! ixSnd p)

--------------------------------------------------------------------------------
-- 4. Interop Bridges: `Tab` <-> `linear` V2 / V3 / V4 / M22 / M33 / M44 / V n
--------------------------------------------------------------------------------

v2ToTab :: (TensorScalar a) => V2 (Expr a) -> Tab ('IxFin 2) (Expr a)
v2ToTab (V2 x y) = forIx (\ix -> select (ixOrd ix .== 0) x y)

tabToV2 :: Tab ('IxFin 2) a -> V2 a
tabToV2 t = V2 (t ! ixConst 0) (t ! ixConst 1)

v3ToTab :: (TensorScalar a) => V3 (Expr a) -> Tab ('IxFin 3) (Expr a)
v3ToTab (V3 x y z) =
  forIx $ \ix ->
    let o = ixOrd ix
     in select (o .== 0) x (select (o .== 1) y z)

tabToV3 :: Tab ('IxFin 3) a -> V3 a
tabToV3 t = V3 (t ! ixConst 0) (t ! ixConst 1) (t ! ixConst 2)

v4ToTab :: (TensorScalar a) => V4 (Expr a) -> Tab ('IxFin 4) (Expr a)
v4ToTab (V4 x y z w) =
  forIx $ \ix ->
    let o = ixOrd ix
     in select (o .== 0) x (select (o .== 1) y (select (o .== 2) z w))

tabToV4 :: Tab ('IxFin 4) a -> V4 a
tabToV4 t = V4 (t ! ixConst 0) (t ! ixConst 1) (t ! ixConst 2) (t ! ixConst 3)

m22ToTab :: (TensorScalar a) => M22 (Expr a) -> Tab ('IxProd ('IxFin 2) ('IxFin 2)) (Expr a)
m22ToTab (V2 r0 r1) = uncurryTab (forIx (\i -> select (ixOrd i .== 0) <$> v2ToTab r0 <*> v2ToTab r1))

tabToM22 :: Tab ('IxProd ('IxFin 2) ('IxFin 2)) a -> M22 a
tabToM22 t = tabToV2 (fmap tabToV2 (curryTab t))

m33ToTab :: (TensorScalar a) => M33 (Expr a) -> Tab ('IxProd ('IxFin 3) ('IxFin 3)) (Expr a)
m33ToTab (V3 r0 r1 r2) =
  let t0 = v3ToTab r0
      t1 = v3ToTab r1
      t2 = v3ToTab r2
   in forIx $ \p ->
        let r = ixOrd (ixFst p)
            c = ixSnd p
         in select (r .== 0) (t0 ! c) (select (r .== 1) (t1 ! c) (t2 ! c))

tabToM33 :: Tab ('IxProd ('IxFin 3) ('IxFin 3)) a -> M33 a
tabToM33 t = tabToV3 (fmap tabToV3 (curryTab t))

m44ToTab :: (TensorScalar a) => M44 (Expr a) -> Tab ('IxProd ('IxFin 4) ('IxFin 4)) (Expr a)
m44ToTab (V4 r0 r1 r2 r3) =
  let t0 = v4ToTab r0
      t1 = v4ToTab r1
      t2 = v4ToTab r2
      t3 = v4ToTab r3
   in forIx $ \p ->
        let r = ixOrd (ixFst p)
            c = ixSnd p
         in select (r .== 0) (t0 ! c) (select (r .== 1) (t1 ! c) (select (r .== 2) (t2 ! c) (t3 ! c)))

tabToM44 :: Tab ('IxProd ('IxFin 4) ('IxFin 4)) a -> M44 a
tabToM44 t = tabToV4 (fmap tabToV4 (curryTab t))

-- | Convert a statically-sized @Linear.V.V n (Expr a)@ into a @Tab ('IxFin n) (Expr a)@.
linearVToTab :: forall n a. (KnownNat n, TensorScalar a) => LV.V n (Expr a) -> Tab ('IxFin n) (Expr a)
linearVToTab (LV.V vec) =
  let n = fromIntegral (natVal (Proxy @n)) :: Int
   in forIx $ \ix ->
        let o = ixOrd ix
            go k
              | k >= n - 1 = vec V.! max 0 (n - 1)
              | otherwise = select (o .== fromIntegral (fromIntegral k :: Int64)) (vec V.! k) (go (k + 1))
         in go 0

-- | Convert a @Tab ('IxFin n) a@ into a @Linear.V.V n a@.
tabToLinearV :: forall n a. (KnownNat n) => Tab ('IxFin n) a -> LV.V n a
tabToLinearV t =
  let n = fromIntegral (natVal (Proxy @n)) :: Int
   in LV.V (V.generate n (\k -> t ! ixConst k))
