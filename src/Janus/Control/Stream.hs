{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Control.Stream where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Trans
import Data.Functor.Identity ()
import Data.Int
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Command.While
import Janus.Control.Step
import Janus.Expression.Bool
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding ((.), id)

-- | A stream that produces elements of type @a@ in monad @m@.
--
-- The outer 'Codensity' layer supports setup/teardown (resource acquisition
-- and release around the loop body): any effects performed in 'Codensity'
-- before the inner continuation is invoked act as setup, and the
-- continuation-capture mechanism of 'Codensity' allows teardown to run
-- after the loop completes (bracket-style).
--
-- The inner @ContT () m (Step a)@ is the per-iteration loop body that
-- produces 'Step' values: 'Yield', 'Skip', or 'Stop'.
newtype Stream m a = Stream {getStream :: Codensity m (ContT () m (Step a))}
  deriving (Functor)

-- | 'Applicative' instance consistent with 'Monad' (i.e. @('<*>') = 'ap'@).
--
-- @'pure' a@ produces a singleton stream that yields @a@ once and then
-- stops.  @fs '<*>' xs@ binds over @fs@, and for each function yielded,
-- binds over @xs@ to apply it — standard monadic product semantics.
-- Use 'repeatS' and 'zipS' when you need infinite streams or lockstep zipping.
instance (Monad m, CmdCond m e, CmdWhile m e, JanusTyped e Bool) => Applicative (Stream m) where
  pure a = Stream $ do
    first <- lift (newRef true)
    pure $ ContT $ \k -> do
      isFirst <- readRef first
      ifThenElseM_ isFirst (writeRef first false >> k (Yield a)) (k Stop)
  (<*>) = ap

-- | Produce an infinite stream that yields @a@ on every iteration.
repeatS :: (Applicative m) => a -> Stream m a
repeatS a = Stream $ pure (pure (Yield a))

-- | 'Monad' instance that flatMaps (concatMaps) streams.
--
-- For each element yielded by the outer stream, the inner stream is run
-- to completion (repeatedly stepping until 'Stop'), giving Cartesian product
-- semantics across nested binds.
instance (Monad m, CmdCond m e, CmdWhile m e, JanusTyped e Bool) => Monad (Stream m) where
  return = pure
  Stream outer >>= f = Stream $ do
    outerBody <- outer
    pure $ ContT $ \k ->
      runContT outerBody $ \case
        Yield a ->
          runCodensity (getStream $ f a) $ \innerBody ->
            whileM true $ \c ->
              runContT innerBody $ \case
                Yield b -> k (Yield b)
                Skip -> pure ()
                Stop -> writeRef c false
        Skip -> k Skip
        Stop -> k Stop

-- | Produce a stream from a range @[l, u)@.
rangeS ::
  ( CmdRef m e,
    CmdCond m e,
    JanusTyped e a,
    JanusTyped e Bool,
    ExpOrd e a,
    Num (e a)
  ) =>
  e a ->
  e a ->
  Stream m (e a)
rangeS l u = Stream $ do
  r <- lift $ newRef l
  pure $ ContT $ \k -> do
    r' <- readRef r
    ifThenElseM_ (r' `lt` u) (writeRef r (r' + 1) >> k (Yield r')) (k Stop)

-- | Filter elements of a stream, turning rejected elements into 'Skip'.
filterS :: (CmdCond m e) => (a -> e Bool) -> Stream m a -> Stream m a
filterS f (Stream s) = Stream $ do
  body <- s
  pure $ do
    s' <- body
    ContT $ \k -> case s' of
      Yield a -> ifThenElseM_ (f a) (k (Yield a)) (k Skip)
      Skip -> k Skip
      Stop -> k Stop

-- | Map an effectful function over each yielded element.
streamM :: (Monad m) => Stream m a -> (a -> m b) -> Stream m b
streamM (Stream s) f = Stream $ do
  body <- s
  pure $
    body >>= \case
      Yield a -> Yield <$> lift (f a)
      Skip -> pure Skip
      Stop -> pure Stop

-- | Zip two streams in lockstep.
--
-- Both streams' 'Codensity' layers are sequenced (setting up and
-- tearing down both), then their inner 'ContT' bodies are run on each
-- iteration and the 'Step' results are combined via 'Step''s
-- 'Applicative' (which threads 'Skip' and 'Stop').
zipWithS :: (Monad m) => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWithS f (Stream sa) (Stream sb) = Stream $ do
  a <- sa
  b <- sb
  pure $ liftA2 f <$> a <*> b

zipS :: (Monad m) => Stream m a -> Stream m b -> Stream m (a, b)
zipS = zipWithS (,)

-- | Construct a stream with explicit setup and teardown.
--
-- @bracketS acquire release body@ runs @acquire@ once before the loop,
-- passes the resource to @body@ to build the per-iteration stream, and
-- runs @release@ after the loop finishes (even if the stream is stopped
-- early by a downstream consumer).
bracketS :: (Monad m) => m r -> (r -> m ()) -> (r -> Stream m a) -> Stream m a
bracketS acquire release body = Stream $ Codensity $ \k -> do
  r <- acquire
  res <- runCodensity (getStream (body r)) k
  release r
  pure res

-- | Run a @Stream m ()@ to completion using a 'whileM' loop.
--
-- The stream's 'Codensity' layer is lowered first (executing any
-- setup/teardown around the loop), then 'whileM' drives the inner
-- 'ContT' body repeatedly until a 'Stop' step is produced.
runStream ::
  ( CmdWhile m e,
    JanusTyped e Bool
  ) =>
  Stream m () ->
  m ()
runStream (Stream s) = runCodensity s $ \body ->
  whileM true $ \c ->
    runContT body $ \case
      Yield _ -> pure ()
      Skip -> pure ()
      Stop -> writeRef c false

scanS :: (JanusTyped e a, CmdRef m e) => e a -> (e a -> e a -> e a) -> Stream m (e a) -> Stream m (e a)
scanS x0 f (Stream s) = Stream $ do
  s' <- s
  x <- lift $ newRef x0
  pure $
    s' >>= \case
      Yield y' -> lift $ do
        x' <- readRef x
        z <- letM $ f x' y'
        writeRef x z
        pure $ Yield z
      Skip -> pure Skip
      Stop -> pure Stop

-- | Take at most @n@ elements from a stream.
takeS ::
  ( CmdCond m e,
    CmdRef m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  Stream m a ->
  Stream m a
takeS n (Stream s) = Stream $ do
  r <- lift $ newRef 0
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        r' <- readRef r
        ifThenElseM_ (r' `lt` n) (writeRef r (r' + 1) >> k (Yield a)) (k Stop)
      Skip -> k Skip
      Stop -> k Stop

-- | Drop the first @n@ elements from a stream.
dropS ::
  ( CmdCond m e,
    CmdRef m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  Stream m a ->
  Stream m a
dropS n (Stream s) = Stream $ do
  r <- lift $ newRef 0
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        r' <- readRef r
        ifThenElseM_ (r' `lt` n) (writeRef r (r' + 1) >> k Skip) (k (Yield a))
      Skip -> k Skip
      Stop -> k Stop

-- | Delay elements by one step, emitting the given initial element first.
delayS ::
  ( CmdRef m e,
    JanusTyped e a
  ) =>
  e a ->
  Stream m (e a) ->
  Stream m (e a)
delayS x0 (Stream s) = Stream $ do
  r <- lift $ newRef x0
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        a' <- readRef r
        writeRef r a
        k (Yield a')
      x -> k x

-- | Delay elements by one step, skipping the first iteration and retaining subsequent values.
delayS' ::
  ( CmdCond m e,
    CmdRef m e,
    JanusTyped e a,
    JanusTyped e Bool
  ) =>
  Stream m (e a) ->
  Stream m (e a)
delayS' (Stream s) = Stream $ do
  r <- lift newRef'
  firstIter <- lift (newRef true)
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        fi <- readRef firstIter
        ifThenElseM_ fi (writeRef firstIter false >> writeRef r a >> k Skip) $ do
          a' <- readRef r
          writeRef r a
          k (Yield a')
      x -> k x

-- | Format and print each yielded element of a stream.
formatS ::
  ( Monad m,
    CmdFormat m e a
  ) =>
  Stream m (e a) ->
  Stream m ()
formatS (Stream s) = Stream $ do
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> format a >> k (Yield ())
      Skip -> k Skip
      Stop -> k Stop
