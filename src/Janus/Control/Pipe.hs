{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Janus.Control.Pipe where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Int
import Data.Profunctor
import GHC.Float
import Janus.Control.Step
import Janus.Control.Stream
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Bool
import Janus.Expression.Cond
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding (id, (.))

newtype Pipe m a b = Pipe {getPipe :: Codensity m (Kleisli (ContT () m) (Step a) (Step b))}
  deriving (Functor)

type Source m a = Pipe m () a

type Sink m a = Pipe m a ()

type Effect m = Pipe m () ()

instance Applicative (Pipe m a) where
  pure a = Pipe $ pure $ Kleisli $ const (pure (Yield a))
  Pipe f <*> Pipe x = Pipe $ do
    f' <- f
    x' <- x
    pure $ Kleisli $ \case
      Yield a -> (<*>) <$> runKleisli f' (Yield a) <*> runKleisli x' (Yield a)
      Skip -> pure Skip
      Stop -> pure Stop

instance Category (Pipe m) where
  id = Pipe $ pure id
  Pipe f . Pipe g = Pipe $ (.) <$> f <*> g

instance Profunctor (Pipe m) where
  dimap f g (Pipe p) = Pipe $ fmap (dimap (fmap f) (fmap g)) p

instance Strong (Pipe m) where
  first' (Pipe f) = Pipe $ do
    f' <- f
    pure $ Kleisli $ \case
      Yield (b, d) -> fmap (,d) <$> runKleisli f' (Yield b)
      Skip -> pure Skip
      Stop -> pure Stop

instance Arrow (Pipe m) where
  arr f = Pipe $ pure (arr (fmap f))
  first = first'

instance Choice (Pipe m) where
  left' = left
  right' = right

instance ArrowChoice (Pipe m) where
  Pipe f +++ Pipe g = Pipe $ do
    f' <- f
    g' <- g
    pure $ Kleisli $ \case
      Yield ebb' -> case ebb' of
        Left b -> fmap Left <$> runKleisli f' (Yield b)
        Right b' -> fmap Right <$> runKleisli g' (Yield b')
      Skip -> pure Skip
      Stop -> pure Stop

instance (Semigroup b) => Semigroup (Pipe m a b) where
  (<>) = liftA2 (<>)

instance (Monoid b) => Monoid (Pipe m a b) where
  mempty = pure mempty

instance (Num b) => Num (Pipe m a b) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum

instance (Fractional b) => Fractional (Pipe m a b) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)
  recip = fmap recip

instance (Floating b) => Floating (Pipe m a b) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  log1p = fmap log1p
  expm1 = fmap expm1
  log1pexp = fmap log1pexp
  log1mexp = fmap log1mexp

rangeP :: (JanusTyped e a, CmdRef m e, CmdCond m e, ExpOrd e a, Num (e a)) => e a -> e a -> Source m (e a)
rangeP l u = Pipe $ do
  r <- lift $ newRef l
  pure $ Kleisli $ \case
    Yield _ -> ContT $ \k -> do
      r' <- readRef r
      ifThenElseM_ (r' `lt` u) (writeRef r (r' + 1) >> k (Yield r')) (k Stop)
    Skip -> pure Skip
    Stop -> pure Stop

filterP :: (CmdCond m e) => (a -> e Bool) -> Pipe m a a
filterP f = Pipe $ pure $ Kleisli $ \case
  Yield a -> ContT $ \k -> ifThenElseM_ (f a) (k (Yield a)) (k Skip)
  Skip -> pure Skip
  Stop -> pure Stop

takeP :: (CmdCond m e, CmdRef m e, ExpOrd e Int64, Num (e Int64), JanusTyped e Int64) => e Int64 -> Pipe m a a
takeP n = Pipe $ do
  r <- lift $ newRef 0
  pure $ Kleisli $ \case
    Yield a -> ContT $ \k -> do
      r' <- readRef r
      ifThenElseM_ (r' `lt` n) (writeRef r (r' + 1) >> k (Yield a)) (k Stop)
    Skip -> pure Skip
    Stop -> pure Stop

dropP :: (CmdCond m e, CmdRef m e, ExpOrd e Int64, Num (e Int64), JanusTyped e Int64) => e Int64 -> Pipe m a a
dropP n = Pipe $ do
  r <- lift (newRef 0)
  pure $ Kleisli $ \case
    Yield a -> ContT $ \k -> do
      r' <- readRef r
      ifThenElseM_ (r' `lt` n) (writeRef r (r'+1) >> k Skip) (k (Yield a))
    Skip -> pure Skip
    Stop -> pure Stop

delayP :: (CmdRef m e, JanusTyped e a) => e a -> Pipe m (e a) (e a)
delayP x0 = Pipe $ do
  r <- lift $ newRef x0
  pure $ Kleisli $ \case
    Yield a -> ContT $ \k -> do
      a' <- readRef r
      writeRef r a
      k (Yield a')
    x -> pure x

delayP' :: (CmdCond m e, CmdRef m e, JanusTyped e a, JanusTyped e Bool) => Pipe m (e a) (e a)
delayP' = Pipe $ do
  r <- lift newRef'
  firstIter <- lift (newRef true)
  pure $ Kleisli $ \case
    Yield a -> ContT $ \k -> do
      fi <- readRef firstIter
      ifThenElseM_ fi (writeRef firstIter false >> writeRef r a >> k Skip) $ do
        a' <- readRef r
        writeRef r a
        k (Yield a')
    x -> pure x

-- only use this at the beginning of a pipeline
source :: Stream m a -> Source m a
source (Stream s) = Pipe $ pure $ Kleisli $ \case
  Yield _ -> s
  Skip -> pure Skip
  Stop -> pure Stop

-- for debugging purposes only
traceShowP :: (CmdPutString m e, Monad m, Show a) => Pipe m a a
traceShowP = Pipe $ pure $ Kleisli $ \case
  Yield a -> ContT $ \k -> withString (show a <> "\n") putString >> k (Yield a)
  Skip -> pure Skip
  Stop -> pure Stop

traceFormatP :: (Monad m, CmdFormat m e a) => Pipe m (e a) (e a)
traceFormatP = Pipe $ pure $ Kleisli $ \case
  Yield a -> ContT $ \k -> format a >> k (Yield a)
  Skip -> pure Skip
  Stop -> pure Stop

formatP :: (Monad m, CmdFormat m e a) => Sink m (e a)
formatP = Pipe $ pure $ Kleisli $ \case
  Yield a -> ContT $ \k -> format a >> k (Yield ())
  Skip -> pure Skip
  Stop -> pure Stop

pipe' :: (Monad m) => m c -> (c -> m d) -> (Step a -> c -> m (Step b)) -> Pipe m a b
pipe' acq rel step = Pipe $ do
  c <- lift acq
  r <- shift $ \iter -> do
    lift $ iter $ Kleisli $ lift . flip step c
  _ <- lift (rel c)
  pure r

pipe :: (Monad m) => (Step a -> m (Step b)) -> Pipe m a b
pipe step = pipe' (pure ()) (const (pure ())) (const . step)

pipeM :: (Monad m) => (a -> m b) -> Pipe m a b
pipeM k = Pipe $ pure $ Kleisli $ \case
  Yield a -> lift $ Yield <$> k a
  Skip -> pure Skip
  Stop -> pure Stop

joinP :: Monad m => Pipe m (m a) a
joinP = Pipe $ pure $ Kleisli $ \case
  Yield ma -> ContT $ \k -> ma >>= k . Yield
  Skip -> pure Skip
  Stop -> pure Stop

newtype Drain m a r = Drain {getDrain :: Source m a -> m r}
  deriving (Functor, Applicative, Monad, MonadFix) via ReaderT (Source m a) m

drain_ :: (CmdRef m e, CmdWhile m e, ExpBool e, JanusTyped e Bool) => Drain m a ()
drain_ = Drain $ \(Pipe p) -> runCodensity p $ \k -> do
  whileM true $ \c -> do
    runContT (runKleisli k (Yield ())) $ \case
      Yield _ -> pure ()
      Skip -> pure ()
      Stop -> writeRef c false

drainFold ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b
  ) =>
  e b ->
  (e b -> a -> e b) ->
  Drain m a (e b)
drainFold b0 f = Drain $ \(Pipe p) -> do
  r <- newRef b0
  runCodensity p $ \k -> do
    whileM true $ \c -> do
      runContT (runKleisli k (Yield ())) $ \case
        Yield a -> modifyRef r (`f` a)
        Skip -> pure ()
        Stop -> writeRef c false
  readRef r

drainSum ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Num (e b)
  ) =>
  Drain m (e b) (e b)
drainSum = drainFold 0 (+)

drainProduct ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Num (e b)
  ) =>
  Drain m (e b) (e b)
drainProduct = drainFold 1 (*)

drainMin ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Num (e b),
    Bounded (e b),
    ExpCond e,
    ExpOrd e b
  ) =>
  Drain m (e b) (e b)
drainMin = drainFold maxBound min'

drainMax ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Num (e b),
    Bounded (e b),
    ExpCond e,
    ExpOrd e b
  ) =>
  Drain m (e b) (e b)
drainMax = drainFold minBound max'

drainMonoid ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Monoid (e b)
  ) =>
  Drain m (e b) (e b)
drainMonoid = drainFold mempty (<>)

drainLength ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  Drain m a (e Int64)
drainLength = drainFold 0 $ \s _ -> s + 1

runPipe :: Drain m a r -> Source m a -> m r
runPipe (Drain d) = d
