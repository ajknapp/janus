{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Open-world algebraic hierarchy for 'Janus.Tensor':
--
-- 1. 'TensorMonoid': associative monoids (not required to be commutative),
--    powering parallel 'Reduce' and work-efficient Blelloch 'AssocScan'
--    (including stateful accumulators like single-pass online 'LogSumExpOp'
--    and non-commutative 'AffineOp' for SSM/Mamba recurrences).
-- 2. 'TensorCommMonoid': commutative monoids, powering order-independent
--    Dex-style 'Accum' and generalized 'ScatterMonoid', with native hardware
--    atomics for built-ins and a generic lock-free 32/64-bit @atomicCAS@
--    fallback for arbitrary user-defined commutative monoids.
-- 3. 'TensorSemiring': commutative semirings @(⊕, ⊗)@ built on
--    'TensorCommMonoid', powering optimal N-ary Einstein tensor contractions
--    ('StandardSR', 'LogSR', 'TropicalMaxSR', 'TropicalMinSR', 'BooleanSR',
--    and any user-defined semiring).
module Janus.Tensor.Algebra
  ( -- * Scalar & Atomic Capability Bundles
    TensorScalar (..),
    CmdAtomicAll,
    genericAtomicCAS32Update,
    genericAtomicCAS64Update,

    -- * Open-World Associative Monoids
    TensorMonoid (..),
    SomeMonoid (..),
    someMonoid,
    monoidDigest,

    -- * Open-World Commutative Monoids
    TensorCommMonoid (..),
    SomeCommMonoid (..),
    someCommMonoid,
    commMonoidDigest,

    -- * Open-World Commutative Semirings
    TensorSemiring (..),
    SomeSemiring (..),
    someSemiring,
    semiringDigest,

    -- * Built-in Monoid & Semiring Tag Types
    AddOp,
    MulOp,
    MaxOp,
    MinOp,
    LogSumExpOp,
    LogAddExpOp,
    AndOp,
    OrOp,
    XorOp,
    AffineOp,
    StandardSR,
    LogSR,
    TropicalMaxSR,
    TropicalMinSR,
    BooleanSR,

    -- * Embedded Numerically Stable Helpers
    logAddExpE,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (sizeOf))
import Janus.Backend.C.CUDA.Atomic
  ( CmdAtomic (..),
    CmdAtomicAdd (..),
    CmdAtomicBits (..),
    CmdAtomicMinMax (..),
  )
import Janus.Backend.C.Raw (JanusCTyped)
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Memory (CmdRefPtr (..), CmdStorable (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Constraints (CmdJanus, ExpJanus, withJanusType)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (FMA (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD.Types (ExpPair (..), SIMDPair)

--------------------------------------------------------------------------------
-- 1. Scalar Element Witnesses & Atomic Capability Bundle
--------------------------------------------------------------------------------

-- | Uniform atomic capability bundle satisfied by 'JanusCUDAM', 'CUDAIOM',
-- 'CUDADejaFuM', 'JanusCMT', 'IO', 'ParallelDejaFuM', and 'CSEM'.
type CmdAtomicAll m e =
  ( CmdJanus m e,
    ExpPair e,
    CmdAtomic m e,
    CmdAtomicAdd m e Int32,
    CmdAtomicAdd m e Word32,
    CmdAtomicAdd m e Int64,
    CmdAtomicAdd m e Word64,
    CmdAtomicAdd m e Float,
    CmdAtomicAdd m e Double,
    CmdAtomicMinMax m e Int32,
    CmdAtomicMinMax m e Word32,
    CmdAtomicMinMax m e Int64,
    CmdAtomicMinMax m e Word64,
    CmdAtomicMinMax m e Float,
    CmdAtomicMinMax m e Double,
    CmdAtomicBits m e Int32,
    CmdAtomicBits m e Word32,
    CmdAtomicBits m e Int64,
    CmdAtomicBits m e Word64
  )

-- | Scalar types supported as tensor storage elements across all Janus backends.
class (Typeable a, Storable a, JanusCTyped a, Eq a) => TensorScalar a where
  scalarName :: Proxy a -> Text.Text
  scalarZeroE :: (ExpJanus e) => Proxy a -> e a
  scalarOneE :: (ExpJanus e) => Proxy a -> e a
  scalarDefaultAtomic ::
    (CmdAtomicAll m e) =>
    (forall e0. (ExpJanus e0) => e0 a -> e0 a -> e0 a) ->
    e (Ptr a) ->
    e a ->
    m ()

-- | Generic lock-free 32-bit @atomicCAS@ update loop for any 4-byte scalar
-- type @a@ and arbitrary binary combine function @f@.
genericAtomicCAS32Update ::
  forall m e a.
  (CmdAtomicAll m e, Storable a, JanusCTyped a) =>
  (e a -> e a -> e a) ->
  e (Ptr a) ->
  e a ->
  m ()
genericAtomicCAS32Update combine ptrA valA =
  withJanusType @a @e $
    withJanusType @Word32 @e $
      withJanusType @(Ptr Word32) @e $
        withJanusType @Bool @e $ do
          let ptrU = fromVoidPtr (toVoidPtr ptrA) :: e (Ptr Word32)
          rOldU <- newRef =<< peek ptrU
          rOldA <- newRef' @m @e @a
          rNewA <- newRef' @m @e @a
          rNewU <- newRef' @m @e @Word32
          whileM true $ \cont -> do
            expectedU <- readRef rOldU
            -- Reinterpret expectedU bits as `a` via stack ref pointer
            writeRef rNewU expectedU
            curA <- withRefPtr rNewU $ \pU -> peek (fromVoidPtr (toVoidPtr pU) :: e (Ptr a))
            writeRef rOldA curA
            nextA <- letM (combine curA valA)
            writeRef rNewA nextA
            desiredU <- withRefPtr rNewA $ \pA -> peek (fromVoidPtr (toVoidPtr pA) :: e (Ptr Word32))
            observedU <- atomicCAS32Ptr ptrU expectedU desiredU
            ifThenElseM_
              (observedU `eq` expectedU)
              (writeRef cont false)
              (writeRef rOldU observedU)

-- | Generic lock-free 64-bit @atomicCAS@ update loop for any 8-byte scalar
-- type @a@ and arbitrary binary combine function @f@.
genericAtomicCAS64Update ::
  forall m e a.
  (CmdAtomicAll m e, Storable a, JanusCTyped a) =>
  (e a -> e a -> e a) ->
  e (Ptr a) ->
  e a ->
  m ()
genericAtomicCAS64Update combine ptrA valA =
  withJanusType @a @e $
    withJanusType @Word64 @e $
      withJanusType @(Ptr Word64) @e $
        withJanusType @Bool @e $ do
          let ptrU = fromVoidPtr (toVoidPtr ptrA) :: e (Ptr Word64)
          rOldU <- newRef =<< peek ptrU
          rNewA <- newRef' @m @e @a
          rNewU <- newRef' @m @e @Word64
          whileM true $ \cont -> do
            expectedU <- readRef rOldU
            writeRef rNewU expectedU
            curA <- withRefPtr rNewU $ \pU -> peek (fromVoidPtr (toVoidPtr pU) :: e (Ptr a))
            nextA <- letM (combine curA valA)
            writeRef rNewA nextA
            desiredU <- withRefPtr rNewA $ \pA -> peek (fromVoidPtr (toVoidPtr pA) :: e (Ptr Word64))
            observedU <- atomicCAS64Ptr ptrU expectedU desiredU
            ifThenElseM_
              (observedU `eq` expectedU)
              (writeRef cont false)
              (writeRef rOldU observedU)

-- Helper needed for qualified import above; we'll import CmdCond cleanly below.

instance TensorScalar Float where
  scalarName _ = "f32"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS32Update combine p v

instance TensorScalar Double where
  scalarName _ = "f64"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS64Update combine p v

instance TensorScalar Int32 where
  scalarName _ = "i32"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS32Update combine p v

instance TensorScalar Int64 where
  scalarName _ = "i64"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS64Update combine p v

instance TensorScalar Word32 where
  scalarName _ = "u32"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS32Update combine p v

instance TensorScalar Word64 where
  scalarName _ = "u64"
  scalarZeroE _ = 0
  scalarOneE _ = 1
  scalarDefaultAtomic combine p v = genericAtomicCAS64Update combine p v

instance TensorScalar Bool where
  scalarName _ = "bool"
  scalarZeroE _ = false
  scalarOneE _ = true
  scalarDefaultAtomic combine p v = do
    -- Bool is 1 byte; combine directly (OR/AND overrides use atomicOr/atomicAnd on word or CAS)
    cur <- peek p
    poke p (combine cur v)

--------------------------------------------------------------------------------
-- 2. Embedded Numerically Stable Helpers
--------------------------------------------------------------------------------

-- | Numerically stable @log(exp(a) + exp(b))@ handling @-inf@ identities safely.
logAddExpE ::
  forall e a.
  (ExpJanus e, Floating (e a), ExpOrd e a, ExpEq e a, Storable a, JanusCTyped a) =>
  e a ->
  e a ->
  e a
logAddExpE a b =
  withJanusType @a @e $
    withJanusType @Bool @e $
      let_ (ifThenElse (a `gt` b) a b) $ \hi ->
        let_ (ifThenElse (a `gt` b) b a) $ \lo ->
          ifThenElse
            (lo `eq` negate (1 / 0))
            hi
            (hi + log (1 + exp (lo - hi)))

--------------------------------------------------------------------------------
-- 3. Open-World Associative Monoids (`TensorMonoid`)
--------------------------------------------------------------------------------

-- | Open-world associative monoid for parallel reductions and associative scans.
--
-- Not required to be commutative (e.g., 'AffineOp' for SSM/Mamba scans).
-- Supports an optional richer accumulator type @MonoidAcc op a@ (defaulting to
-- @a@), allowing single-pass numerically stable parallel reductions such as
-- 'LogSumExpOp' carrying @SIMDPair a a = (runningMax, sumExp)@.
class
  ( Typeable op,
    TensorScalar a,
    Typeable (MonoidAcc op a),
    Storable (MonoidAcc op a),
    JanusCTyped (MonoidAcc op a)
  ) =>
  TensorMonoid (op :: K.Type) (a :: K.Type)
  where
  type MonoidAcc op a :: K.Type
  type MonoidAcc op a = a

  monoidTag :: Proxy op -> Proxy a -> Text.Text
  monoidEmpty :: (ExpJanus e, ExpPair e) => Proxy op -> Proxy a -> e (MonoidAcc op a)
  monoidInject :: (ExpJanus e, ExpPair e) => Proxy op -> e a -> e (MonoidAcc op a)
  monoidCombine :: (ExpJanus e, ExpPair e) => Proxy op -> e (MonoidAcc op a) -> e (MonoidAcc op a) -> e (MonoidAcc op a)
  monoidFinalize :: (ExpJanus e, ExpPair e) => Proxy op -> e (MonoidAcc op a) -> e a

  default monoidInject :: (MonoidAcc op a ~ a) => Proxy op -> e a -> e (MonoidAcc op a)
  monoidInject _ = id

  default monoidFinalize :: (MonoidAcc op a ~ a) => Proxy op -> e (MonoidAcc op a) -> e a
  monoidFinalize _ = id

-- | Existential IR witness for any open-world 'TensorMonoid'.
data SomeMonoid (a :: K.Type) where
  SomeMonoid :: (TensorMonoid op a) => !(Proxy op) -> SomeMonoid a

someMonoid :: forall op a. (TensorMonoid op a) => SomeMonoid a
someMonoid = SomeMonoid (Proxy @op)

monoidDigest :: forall a. SomeMonoid a -> BS.ByteString
monoidDigest (SomeMonoid (p :: Proxy op)) =
  SHA256.hash (Text.encodeUtf8 ("Monoid:" <> monoidTag p (Proxy @a)))

--------------------------------------------------------------------------------
-- 4. Open-World Commutative Monoids (`TensorCommMonoid`)
--------------------------------------------------------------------------------

-- | Open-world commutative monoid unlocking order-independent GPU-parallel
-- 'Accum' and 'ScatterMonoid'.
--
-- Custom user commutative monoids automatically work on both GPU and
-- multi-threaded CPU via the default lock-free 32/64-bit @atomicCAS@ loop!
class (TensorMonoid op a) => TensorCommMonoid (op :: K.Type) (a :: K.Type) where
  -- | Combine two direct @a@ values (used for atomic scatter & fallback).
  commMonoidCombineVal :: (ExpJanus e, ExpPair e) => Proxy op -> e a -> e a -> e a
  default commMonoidCombineVal ::
    (ExpJanus e, ExpPair e) =>
    Proxy op ->
    e a ->
    e a ->
    e a
  commMonoidCombineVal p x y =
    monoidFinalize @op @a p (monoidCombine @op @a p (monoidInject @op @a p x) (monoidInject @op @a p y))

  -- | Direct identity element of type @a@ (for initializing scatter/accum buffers).
  commMonoidZeroVal :: (ExpJanus e, ExpPair e) => Proxy op -> Proxy a -> e a
  default commMonoidZeroVal ::
    (ExpJanus e, ExpPair e) =>
    Proxy op ->
    Proxy a ->
    e a
  commMonoidZeroVal p pa = monoidFinalize @op @a p (monoidEmpty @op @a p pa)

  -- | Thread-safe atomic accumulation @*ptr = *ptr <> val@ on GPU and CPU.
  monoidAtomicUpdate ::
    forall m e.
    (CmdAtomicAll m e) =>
    Proxy op ->
    e (Ptr a) ->
    e a ->
    m ()
  default monoidAtomicUpdate ::
    forall m e.
    (CmdAtomicAll m e) =>
    Proxy op ->
    e (Ptr a) ->
    e a ->
    m ()
  monoidAtomicUpdate p ptr val =
    if sizeOf (undefined :: a) == 4
      then genericAtomicCAS32Update (commMonoidCombineVal p) ptr val
      else
        if sizeOf (undefined :: a) == 8
          then genericAtomicCAS64Update (commMonoidCombineVal p) ptr val
          else do
            cur <- peek ptr
            poke ptr (commMonoidCombineVal p cur val)

data SomeCommMonoid (a :: K.Type) where
  SomeCommMonoid :: (TensorCommMonoid op a) => !(Proxy op) -> SomeCommMonoid a

someCommMonoid :: forall op a. (TensorCommMonoid op a) => SomeCommMonoid a
someCommMonoid = SomeCommMonoid (Proxy @op)

commMonoidDigest :: forall a. SomeCommMonoid a -> BS.ByteString
commMonoidDigest (SomeCommMonoid (p :: Proxy op)) =
  SHA256.hash (Text.encodeUtf8 ("CommMonoid:" <> monoidTag p (Proxy @a)))

--------------------------------------------------------------------------------
-- 5. Open-World Commutative Semirings (`TensorSemiring`)
--------------------------------------------------------------------------------

-- | Open-world commutative semiring @(⊕, ⊗)@ for N-ary Einstein tensor
-- contractions. Its additive structure @SemiringAdd sr@ is a
-- 'TensorCommMonoid', so split-K reductions and atomic accumulation work
-- automatically for any user-defined semiring.
class (Typeable sr, TensorCommMonoid (SemiringAdd sr) a) => TensorSemiring (sr :: K.Type) (a :: K.Type) where
  type SemiringAdd sr :: K.Type
  semiringTag :: Proxy sr -> Proxy a -> Text.Text
  semiringZero :: (ExpJanus e, ExpPair e) => Proxy sr -> Proxy a -> e a
  semiringZero _ = commMonoidZeroVal (Proxy @(SemiringAdd sr))
  semiringOne :: (ExpJanus e, ExpPair e) => Proxy sr -> Proxy a -> e a
  semiringAdd :: (ExpJanus e, ExpPair e) => Proxy sr -> e a -> e a -> e a
  semiringAdd _ = commMonoidCombineVal (Proxy @(SemiringAdd sr))
  semiringMul :: (ExpJanus e, ExpPair e) => Proxy sr -> e a -> e a -> e a
  semiringFMA :: (ExpJanus e, ExpPair e) => Proxy sr -> e a -> e a -> e a -> e a
  semiringFMA p acc x y = semiringAdd p acc (semiringMul p x y)

data SomeSemiring (a :: K.Type) where
  SomeSemiring :: (TensorSemiring sr a) => !(Proxy sr) -> SomeSemiring a

someSemiring :: forall sr a. (TensorSemiring sr a) => SomeSemiring a
someSemiring = SomeSemiring (Proxy @sr)

semiringDigest :: forall a. SomeSemiring a -> BS.ByteString
semiringDigest (SomeSemiring (p :: Proxy sr)) =
  SHA256.hash (Text.encodeUtf8 ("Semiring:" <> semiringTag p (Proxy @a)))

--------------------------------------------------------------------------------
-- 6. Built-In Open Monoid & Semiring Tag Types and Instances
--------------------------------------------------------------------------------

data AddOp
data MulOp
data MaxOp
data MinOp
data LogSumExpOp
data LogAddExpOp
data AndOp
data OrOp
data XorOp
data AffineOp

data StandardSR
data LogSR
data TropicalMaxSR
data TropicalMinSR
data BooleanSR

-- AddOp
instance TensorMonoid AddOp Float where
  monoidTag _ _ = "Add:f32"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Float where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

instance TensorMonoid AddOp Double where
  monoidTag _ _ = "Add:f64"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Double where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

instance TensorMonoid AddOp Int32 where
  monoidTag _ _ = "Add:i32"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Int32 where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

instance TensorMonoid AddOp Int64 where
  monoidTag _ _ = "Add:i64"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Int64 where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

instance TensorMonoid AddOp Word32 where
  monoidTag _ _ = "Add:u32"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Word32 where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

instance TensorMonoid AddOp Word64 where
  monoidTag _ _ = "Add:u64"
  monoidEmpty _ _ = 0
  monoidCombine _ = (+)
instance TensorCommMonoid AddOp Word64 where
  monoidAtomicUpdate _ p v = atomicAddPtr p v >> pure ()

-- MulOp (uses generic lock-free CAS fallback for monoidAtomicUpdate!)
instance TensorMonoid MulOp Float where
  monoidTag _ _ = "Mul:f32"
  monoidEmpty _ _ = 1
  monoidCombine _ = (*)
instance TensorCommMonoid MulOp Float

instance TensorMonoid MulOp Double where
  monoidTag _ _ = "Mul:f64"
  monoidEmpty _ _ = 1
  monoidCombine _ = (*)
instance TensorCommMonoid MulOp Double

instance TensorMonoid MulOp Int32 where
  monoidTag _ _ = "Mul:i32"
  monoidEmpty _ _ = 1
  monoidCombine _ = (*)
instance TensorCommMonoid MulOp Int32

instance TensorMonoid MulOp Int64 where
  monoidTag _ _ = "Mul:i64"
  monoidEmpty _ _ = 1
  monoidCombine _ = (*)
instance TensorCommMonoid MulOp Int64

maxE ::
  forall e a.
  (ExpJanus e, ExpOrd e a, Storable a, JanusCTyped a) =>
  e a ->
  e a ->
  e a
maxE a b = withJanusType @a @e (withJanusType @Bool @e (ifThenElse (a `gt` b) a b))

minE ::
  forall e a.
  (ExpJanus e, ExpOrd e a, Storable a, JanusCTyped a) =>
  e a ->
  e a ->
  e a
minE a b = withJanusType @a @e (withJanusType @Bool @e (ifThenElse (a `lt` b) a b))

-- MaxOp
instance TensorMonoid MaxOp Float where
  monoidTag _ _ = "Max:f32"
  monoidEmpty _ _ = negate (1 / 0)
  monoidCombine _ = maxE
instance TensorCommMonoid MaxOp Float where
  monoidAtomicUpdate _ p v = atomicMaxPtr p v >> pure ()

instance TensorMonoid MaxOp Double where
  monoidTag _ _ = "Max:f64"
  monoidEmpty _ _ = negate (1 / 0)
  monoidCombine _ = maxE
instance TensorCommMonoid MaxOp Double where
  monoidAtomicUpdate _ p v = atomicMaxPtr p v >> pure ()

instance TensorMonoid MaxOp Int32 where
  monoidTag _ _ = "Max:i32"
  monoidEmpty _ _ = minBound
  monoidCombine _ = maxE
instance TensorCommMonoid MaxOp Int32 where
  monoidAtomicUpdate _ p v = atomicMaxPtr p v >> pure ()

instance TensorMonoid MaxOp Int64 where
  monoidTag _ _ = "Max:i64"
  monoidEmpty _ _ = minBound
  monoidCombine _ = maxE
instance TensorCommMonoid MaxOp Int64 where
  monoidAtomicUpdate _ p v = atomicMaxPtr p v >> pure ()

-- MinOp
instance TensorMonoid MinOp Float where
  monoidTag _ _ = "Min:f32"
  monoidEmpty _ _ = 1 / 0
  monoidCombine _ = minE
instance TensorCommMonoid MinOp Float where
  monoidAtomicUpdate _ p v = atomicMinPtr p v >> pure ()

instance TensorMonoid MinOp Double where
  monoidTag _ _ = "Min:f64"
  monoidEmpty _ _ = 1 / 0
  monoidCombine _ = minE
instance TensorCommMonoid MinOp Double where
  monoidAtomicUpdate _ p v = atomicMinPtr p v >> pure ()

instance TensorMonoid MinOp Int32 where
  monoidTag _ _ = "Min:i32"
  monoidEmpty _ _ = maxBound
  monoidCombine _ = minE
instance TensorCommMonoid MinOp Int32 where
  monoidAtomicUpdate _ p v = atomicMinPtr p v >> pure ()

instance TensorMonoid MinOp Int64 where
  monoidTag _ _ = "Min:i64"
  monoidEmpty _ _ = maxBound
  monoidCombine _ = minE
instance TensorCommMonoid MinOp Int64 where
  monoidAtomicUpdate _ p v = atomicMinPtr p v >> pure ()

-- LogSumExpOp (single-pass online numerically stable (max, sumExp) monoid!)
onlineLSEEmpty :: forall e a. (ExpJanus e, ExpPair e, Floating (e a), Storable a, JanusCTyped a) => e (SIMDPair a a)
onlineLSEEmpty = withJanusType @a @e (mkPair (negate (1 / 0)) 0)

onlineLSEInject :: forall e a. (ExpJanus e, ExpPair e, Num (e a), Storable a, JanusCTyped a) => e a -> e (SIMDPair a a)
onlineLSEInject x = withJanusType @a @e (mkPair x 1)

onlineLSECombine ::
  forall e a.
  (ExpJanus e, ExpPair e, Floating (e a), ExpOrd e a, ExpEq e a, Storable a, JanusCTyped a) =>
  e (SIMDPair a a) ->
  e (SIMDPair a a) ->
  e (SIMDPair a a)
onlineLSECombine p1 p2 =
  withJanusType @a @e $
    withJanusType @Bool @e $
      let_ (fstPair p1) $ \m1 ->
        let_ (sndPair p1) $ \s1 ->
          let_ (fstPair p2) $ \m2 ->
            let_ (sndPair p2) $ \s2 ->
              let_ (ifThenElse (m1 `gt` m2) m1 m2) $ \m ->
                let_ (ifThenElse (s1 `eq` 0) 0 (s1 * exp (m1 - m))) $ \s1' ->
                  let_ (ifThenElse (s2 `eq` 0) 0 (s2 * exp (m2 - m))) $ \s2' ->
                    mkPair m (s1' + s2')

onlineLSEFinalize ::
  forall e a.
  (ExpJanus e, ExpPair e, Floating (e a), ExpEq e a, Storable a, JanusCTyped a) =>
  e (SIMDPair a a) ->
  e a
onlineLSEFinalize p =
  withJanusType @a @e $
    withJanusType @Bool @e $
      let_ (fstPair p) $ \m ->
        let_ (sndPair p) $ \s ->
          ifThenElse (s `eq` 0) (negate (1 / 0)) (m + log s)

instance TensorMonoid LogSumExpOp Float where
  type MonoidAcc LogSumExpOp Float = SIMDPair Float Float
  monoidTag _ _ = "LogSumExp:f32"
  monoidEmpty _ _ = onlineLSEEmpty
  monoidInject _ = onlineLSEInject
  monoidCombine _ = onlineLSECombine
  monoidFinalize _ = onlineLSEFinalize
instance TensorCommMonoid LogSumExpOp Float where
  commMonoidCombineVal _ = logAddExpE
  commMonoidZeroVal _ _ = negate (1 / 0)

instance TensorMonoid LogSumExpOp Double where
  type MonoidAcc LogSumExpOp Double = SIMDPair Double Double
  monoidTag _ _ = "LogSumExp:f64"
  monoidEmpty _ _ = onlineLSEEmpty
  monoidInject _ = onlineLSEInject
  monoidCombine _ = onlineLSECombine
  monoidFinalize _ = onlineLSEFinalize
instance TensorCommMonoid LogSumExpOp Double where
  commMonoidCombineVal _ = logAddExpE
  commMonoidZeroVal _ _ = negate (1 / 0)

-- LogAddExpOp (direct scalar logaddexp monoid, used by LogSR contractions)
instance TensorMonoid LogAddExpOp Float where
  monoidTag _ _ = "LogAddExp:f32"
  monoidEmpty _ _ = negate (1 / 0)
  monoidCombine _ = logAddExpE
instance TensorCommMonoid LogAddExpOp Float

instance TensorMonoid LogAddExpOp Double where
  monoidTag _ _ = "LogAddExp:f64"
  monoidEmpty _ _ = negate (1 / 0)
  monoidCombine _ = logAddExpE
instance TensorCommMonoid LogAddExpOp Double

-- Boolean monoids
instance TensorMonoid AndOp Bool where
  monoidTag _ _ = "And:bool"
  monoidEmpty _ _ = true
  monoidCombine _ = land
instance TensorCommMonoid AndOp Bool

instance TensorMonoid OrOp Bool where
  monoidTag _ _ = "Or:bool"
  monoidEmpty _ _ = false
  monoidCombine _ = lor
instance TensorCommMonoid OrOp Bool

instance TensorMonoid XorOp Bool where
  monoidTag _ _ = "Xor:bool"
  monoidEmpty _ _ = false
  monoidCombine _ a b = (a `lor` b) `land` lnot (a `land` b)
instance TensorCommMonoid XorOp Bool

-- StandardSR (+, *, fma)
instance TensorSemiring StandardSR Float where
  type SemiringAdd StandardSR = AddOp
  semiringTag _ _ = "StandardSR:f32"
  semiringOne _ _ = 1
  semiringMul _ = (*)
  semiringFMA _ acc x y = fma x y acc

instance TensorSemiring StandardSR Double where
  type SemiringAdd StandardSR = AddOp
  semiringTag _ _ = "StandardSR:f64"
  semiringOne _ _ = 1
  semiringMul _ = (*)
  semiringFMA _ acc x y = fma x y acc

instance TensorSemiring StandardSR Int32 where
  type SemiringAdd StandardSR = AddOp
  semiringTag _ _ = "StandardSR:i32"
  semiringOne _ _ = 1
  semiringMul _ = (*)

instance TensorSemiring StandardSR Int64 where
  type SemiringAdd StandardSR = AddOp
  semiringTag _ _ = "StandardSR:i64"
  semiringOne _ _ = 1
  semiringMul _ = (*)

-- LogSR (logaddexp, +) — Bayesian log-space marginalization / factor graphs!
instance TensorSemiring LogSR Float where
  type SemiringAdd LogSR = LogAddExpOp
  semiringTag _ _ = "LogSR:f32"
  semiringOne _ _ = 0
  semiringMul _ = (+)

instance TensorSemiring LogSR Double where
  type SemiringAdd LogSR = LogAddExpOp
  semiringTag _ _ = "LogSR:f64"
  semiringOne _ _ = 0
  semiringMul _ = (+)

-- TropicalMaxSR (max, +) — Viterbi / MAP inference!
instance TensorSemiring TropicalMaxSR Float where
  type SemiringAdd TropicalMaxSR = MaxOp
  semiringTag _ _ = "TropicalMaxSR:f32"
  semiringOne _ _ = 0
  semiringMul _ = (+)

instance TensorSemiring TropicalMaxSR Double where
  type SemiringAdd TropicalMaxSR = MaxOp
  semiringTag _ _ = "TropicalMaxSR:f64"
  semiringOne _ _ = 0
  semiringMul _ = (+)

-- TropicalMinSR (min, +) — shortest-path / dynamic programming!
instance TensorSemiring TropicalMinSR Float where
  type SemiringAdd TropicalMinSR = MinOp
  semiringTag _ _ = "TropicalMinSR:f32"
  semiringOne _ _ = 0
  semiringMul _ = (+)

instance TensorSemiring TropicalMinSR Double where
  type SemiringAdd TropicalMinSR = MinOp
  semiringTag _ _ = "TropicalMinSR:f64"
  semiringOne _ _ = 0
  semiringMul _ = (+)

-- BooleanSR (||, &&) — support & reachability masks!
instance TensorSemiring BooleanSR Bool where
  type SemiringAdd BooleanSR = OrOp
  semiringTag _ _ = "BooleanSR:bool"
  semiringOne _ _ = true
  semiringMul _ = land
