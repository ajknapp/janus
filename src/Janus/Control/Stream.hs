{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Janus.Control.Stream where

-- import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Trans
import Data.Int
import Janus.Control.Step
import Janus.Command.Cond
import Janus.Command.Ref
import Janus.Command.Range
import Janus.Expression.Ord
import Janus.Typed

newtype Stream m a = Stream {getStream :: ContT () m (Step a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Stream m) where
  pure a = Stream $ pure (Yield a)
  Stream f <*> Stream x = Stream $ (<*>) <$> f <*> x

instance (Monad m) => Monad (Stream m) where
  return = pure
  Stream f >>= m = Stream $ do
    f' <- f
    case f' of
      Yield a -> getStream (m a)
      Skip -> pure Skip
      Stop -> pure Stop

rangeS ::
  ( CmdRef m e,
    CmdRange m e,
    JanusTyped e Int64,
    JanusTyped e Bool,
    ExpOrd e Int64,
    Num (e Int64)
  ) =>
  e Int64 ->
  e Int64 ->
  Stream m (e Int64)
rangeS l u = Stream $ ContT $ \k -> do
  rangeM l u (k . Yield)
  k Stop

filterS :: (CmdCond m e) => (a -> e Bool) -> Stream m a -> Stream m a
filterS f (Stream s) = Stream $ do
  s' <- s
  ContT $ \k -> case s' of
    Yield a -> ifThenElseM_ (f a) (k (Yield a)) (k Skip)
    Skip -> k Skip
    Stop -> k Stop

streamM :: Monad m => Stream m a -> (a -> m b) -> Stream m b
streamM (Stream s) f = Stream $ s >>= \case
  Yield a -> Yield <$> lift (f a)
  Skip -> pure Skip
  Stop -> pure Stop
