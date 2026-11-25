{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Data.Tensor
  ( Tensor(..),
    TensorBounds(..),
    TensorIndex(..),
    ReifyTensorBounds(..),
    tensorBounds,
    Vector,
    Matrix,
    Tensor3,
    Tensor4,
    newTensor,
    freeTensor,
    withTensor,
    readTensor,
    writeTensor,
    tensorSize,
    iterateTensorBounds,
  )
where

import Data.Foldable
import Data.Int
import Data.Kind as K
import Data.Proxy
import Data.Typeable
import Foreign.Ptr as Ptr
import qualified Foreign.Storable as Storable
import GHC.TypeLits
import Janus.Backend.C.Raw
import Janus.Command.Memory
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding (id, (.))

data Tensor (ds :: [Nat]) e a where
  Tensor :: TensorBounds ds e -> e (Ptr a) -> Tensor ds e a

instance (Typeable t, JanusCTyped a, Typeable ds) => JanusCTyped (Tensor ds (JanusCT t) a) where
  jctype _ = jctype (Proxy @(Ptr a))

data TensorBounds (ds :: [Nat]) (e :: K.Type -> K.Type) where
  TensorBoundsNil :: TensorBounds '[] e
  TensorBoundsCons :: (KnownNat n) => Proxy n -> TensorBounds ns e -> TensorBounds (n ': ns) e

deriving instance Show (TensorBounds ds e)

data TensorIndex (ds :: [Nat]) e where
  Z :: TensorIndex '[] e
  (:.) :: e Int64 -> TensorIndex ds e -> TensorIndex (d ': ds) e

infixr 5 :.

deriving instance (Show (e Int64)) => Show (TensorIndex ds e)

class ReifyTensorBounds (ds :: [Nat]) where
  reifyTensorBounds :: Proxy ds -> TensorBounds ds e

instance ReifyTensorBounds '[] where
  reifyTensorBounds _ = TensorBoundsNil

instance (KnownNat d, ReifyTensorBounds ds) => ReifyTensorBounds (d ': ds) where
  reifyTensorBounds _ = TensorBoundsCons (Proxy @d) (reifyTensorBounds (Proxy @ds))

tensorBoundSize :: TensorBounds ds e -> Int
tensorBoundSize = go 1
  where
    go :: Int -> TensorBounds ds e -> Int
    go s TensorBoundsNil = s
    go s (TensorBoundsCons p ts) = go (fromIntegral (natVal p) * s) ts

tensorSize :: Tensor ds e a -> Int
tensorSize (Tensor tb _) = tensorBoundSize tb

withTensor ::
  forall ds e a b.
  (Storable.Storable a, Num (e Int64), ExpInject e (Ptr a)) =>
  TensorBounds ds e ->
  (Tensor ds e a -> IO b) ->
  IO b
withTensor ti f =
  allocaAligned (tensorBoundSize ti) $ \(p :: Ptr a) ->
    f (Tensor ti (inject p))

tbToList :: (Num (e Int64)) => TensorBounds ds e -> [e Int64]
tbToList TensorBoundsNil = []
tbToList (TensorBoundsCons p bs) = fromIntegral (natVal p) : tbToList bs

tiToList :: TensorIndex ds e -> [e Int64]
tiToList Z = []
tiToList (i :. is) = i : tiToList is

rowIndex :: (Num (e Int64)) => [e Int64] -> [e Int64] -> e Int64
rowIndex ti (_:tb) = foldl' (\s (i, b) -> i + b * s) 0 $ zip ti (1 : tb)
rowIndex _ _ = error "rowIndex: the impossible happened"

readTensor :: (Num (e Int64), CmdStorable m e a) => Tensor ds e a -> TensorIndex ds e -> m (e a)
readTensor (Tensor tb p) ti = peekElemOff p (rowIndex ti' tb')
  where
    tb' = tbToList tb
    ti' = tiToList ti

writeTensor :: (Num (e Int64), CmdStorable m e a) => Tensor ds e a -> TensorIndex ds e -> e a -> m ()
writeTensor (Tensor tb p) ti a = pokeElemOff p a (rowIndex ti' tb')
  where
    tb' = tbToList tb
    ti' = tiToList ti

newTensor ::
  forall m e a ds.
  ( Monad m,
    CmdMem m e,
    ExpSized e a,
    ExpPtrCast e,
    JanusTyped e a,
    Num (e Int64)
  ) =>
  TensorBounds ds e ->
  m (Tensor ds e a)
newTensor tb = do
  let n = tensorBoundSize tb
  p <- calloc (fromIntegral n) (sizeOf (Proxy @a))
  pure $ Tensor tb (fromVoidPtr p)

freeTensor :: (CmdMem m e, ExpPtrCast e, JanusTyped e a) => Tensor ds e a -> m ()
freeTensor (Tensor _ p) = free (toVoidPtr p)

type Vector n e a = Tensor '[n] e a

type Matrix n m e a = Tensor '[n, m] e a

type Tensor3 n m p e a = Tensor '[n, m, p] e a

type Tensor4 n m p q e a = Tensor '[n, m, p, q] e a

iterateTensorBounds ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  TensorBounds sh e ->
  (TensorIndex sh e -> m ()) ->
  m ()
iterateTensorBounds TensorBoundsNil k = k Z
iterateTensorBounds (TensorBoundsCons p ts) k = rangeM 0 (fromIntegral $ natVal p) $ \i ->
  iterateTensorBounds ts $ \idx -> k (i :. idx)

tensorBounds :: Tensor sh e a -> TensorBounds sh e
tensorBounds (Tensor sh _) = sh
