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

module Janus.Command.Array
  ( module Janus.Command.Array,
    module Janus.Command.Memory,
  )
where

import Control.Category
import Control.Monad.Cont
import Data.Foldable
import Data.Int
import Data.Kind as K
import Data.Proxy
import Data.Typeable
import Foreign.Ptr as Ptr
import qualified Foreign.Storable as Storable
import GHC.TypeLits
import Janus.Backend.C
import Janus.Command.Memory
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.While
import Janus.Control.Drain
import Janus.Control.Pipe
import Janus.Control.Step
import Janus.Control.Stream
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding (id, (.))

data Tensor (ds :: [Nat]) e a where
  Tensor :: TensorBound ds e -> e (Ptr a) -> Tensor ds e a

instance (JanusCTyped a, Typeable ds) => JanusCTyped (Tensor ds JanusC a) where
  jctype _ = jctype (Proxy @(Ptr a))

data TensorBound (ds :: [Nat]) (e :: K.Type -> K.Type) where
  TensorBoundNil :: TensorBound '[] e
  TensorBoundCons :: (KnownNat n) => Proxy n -> TensorBound ns e -> TensorBound (n ': ns) e

deriving instance Show (TensorBound ds e)

data TensorIndex (ds :: [Nat]) e where
  Z :: TensorIndex '[] e
  (:.) :: e Int64 -> TensorIndex ds e -> TensorIndex (d ': ds) e

infixr 5 :.

deriving instance (Show (e Int64)) => Show (TensorIndex ds e)

class ReifyTensorBound (ds :: [Nat]) where
  tensorBound :: Proxy ds -> TensorBound ds e

instance ReifyTensorBound '[] where
  tensorBound _ = TensorBoundNil

instance (KnownNat d, ReifyTensorBound ds) => ReifyTensorBound (d ': ds) where
  tensorBound _ = TensorBoundCons (Proxy @d) (tensorBound (Proxy @ds))

tensorBoundSize :: TensorBound ds e -> Int
tensorBoundSize = go 1
  where
    go :: Int -> TensorBound ds e -> Int
    go s TensorBoundNil = s
    go s (TensorBoundCons p ts) = go (fromIntegral (natVal p) * s) ts

tensorSize :: Tensor ds e a -> Int
tensorSize (Tensor tb _) = tensorBoundSize tb

withTensor ::
  forall ds e a b.
  (Storable.Storable a, Num (e Int64), ExpInject e (Ptr a)) =>
  TensorBound ds e ->
  (Tensor ds e a -> IO b) ->
  IO b
withTensor ti f =
  allocaAligned (tensorBoundSize ti) $ \(p :: Ptr a) ->
    f (Tensor ti (inject p))

tbToList :: (Num (e Int64)) => TensorBound ds e -> [e Int64]
tbToList TensorBoundNil = []
tbToList (TensorBoundCons p bs) = fromIntegral (natVal p) : tbToList bs

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
  TensorBound ds e ->
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

tensorIndices ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (TensorIndex ds e)
tensorIndices (Tensor bds _) = Stream $ pure $ ContT $ \k -> do
  iterateTensorBounds bds (k . Yield)
  k Stop

iterateTensorBounds ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  TensorBound sh e ->
  (TensorIndex sh e -> m ()) ->
  m ()
iterateTensorBounds TensorBoundNil k = k Z
iterateTensorBounds (TensorBoundCons p ts) k = rangeM 0 (fromIntegral $ natVal p) $ \i ->
  iterateTensorBounds ts $ \idx -> k (i :. idx)

tensorValuesS ::
  ( Monad m,
    CmdRef m e,
    CmdStorable m e a,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (e a)
tensorValuesS t = streamM (tensorIndices t) (readTensor t)

tensorIndicesValuesS ::
  ( Monad m,
    CmdRef m e,
    CmdStorable m e a,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (TensorIndex ds e, e a)
tensorIndicesValuesS t = streamM (tensorIndices t) $ \idx -> do
  ti <- readTensor t idx
  pure (idx, ti)

innerProduct ::
  ( CmdRange m e,
    CmdRef m e,
    CmdWhile m e,
    CmdStorable m e a,
    JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    ExpOrd e Int64,
    Num (e Int64),
    Num (e a)
  ) =>
  Vector n e a ->
  Vector n e a ->
  m (e a)
innerProduct x y = runDrain drainSum $ runPipe (pipeM $ \idx -> do
  xi <- readTensor x idx
  yi <- readTensor y idx
  letM (xi * yi)) (tensorIndices x)

vectorNorm2 ::
  ( JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    Floating (e a),
    CmdRange m e,
    CmdWhile m e,
    CmdRef m e,
    CmdStorable m e a,
    ExpOrd e Int64,
    Num (e Int64)
  ) =>
  Vector n e a ->
  m (e a)
vectorNorm2 x = runDrain drainSum $ runPipe (pipeM $ \idx -> do
  xi <- readTensor x idx
  letM (xi * xi)) (tensorIndices x)

vectorNorm ::
  ( JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    Floating (e a),
    CmdRange m e,
    CmdWhile m e,
    CmdRef m e,
    CmdStorable m e a,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Bool
  ) =>
  Vector n e a ->
  m (e a)
vectorNorm x = vectorNorm2 x >>= letM . sqrt
