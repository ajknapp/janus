{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Control.Stream where

import Control.Applicative
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Trans
import Data.Functor.Alt
import Data.Functor.Identity ()
import Data.Int
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Command.While
import Janus.Control.Step
import Janus.Expression.Bool
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding (id, (.))

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

-- | 'Alt' instance that concatenates streams sequentially.
--
-- 'empty' produces an empty stream that immediately stops.
-- @xs '<|>' ys@ behaves like @xs@ until it emits 'Stop', immediately
-- switching to @ys@ on that same step (without an extra 'Skip') and
-- running @ys@ until it emits 'Stop', which stops the combined stream.
instance (Monad m, CmdCond m e, CmdWhile m e, JanusTyped e Bool) => Alt (Stream m) where
  Stream s1 <!> Stream s2 = Stream $ do
    inLeft <- lift (newRef true)
    b1 <- s1
    b2 <- s2
    pure $ ContT $ \k -> do
      isLeft <- readRef inLeft
      whenM_ isLeft $
        runContT b1 $ \case
          Yield a -> k (Yield a)
          Skip -> k Skip
          Stop -> writeRef inLeft false
      isLeft' <- readRef inLeft
      unlessM_ isLeft' $
        runContT b2 k

-- | Duplicates the 'Alt' instance.
instance (Monad m, CmdCond m e, CmdWhile m e, JanusTyped e Bool) => Alternative (Stream m) where
  empty = Stream $ pure (pure Stop)
  (<|>) = (<!>)

-- | Duplicates the 'Alt' instance.
instance (Monad m, CmdCond m e, CmdWhile m e, JanusTyped e Bool) => MonadPlus (Stream m) where
  mzero = empty
  mplus = (<|>)

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

-- | Execute a monadic setup action @m s@ once during stream initialization
-- (in the outer 'Codensity' layer) without introducing a nested '(>>=)' loop,
-- preserving single-step pull semantics for 'zipS', 'unionSortedByS', and
-- short-circuiting 'Drain's.
withStreamSetup :: (Monad m) => m s -> (s -> Stream m a) -> Stream m a
withStreamSetup setup mkStream = Stream $ do
  s <- lift setup
  getStream (mkStream s)

-- | Construct a stream with setup state @m s@ and a per-iteration step.
statefulS :: (Monad m) => m s -> (s -> ContT () m (Step a)) -> Stream m a
statefulS mkState stepFn = Stream $ do
  st <- lift mkState
  pure (stepFn st)

-- | Stateful stream unfold backed by a 'RefCell' state @s@.
-- Each step returns @(hasValue, nextState, value)@; when @hasValue@ is false,
-- the stream terminates with 'Stop'.
unfoldSM ::
  ( Monad m,
    RefCell m e s,
    CmdCond m e
  ) =>
  s ->
  (s -> m (e Bool, s, a)) ->
  Stream m a
unfoldSM s0 stepFn = Stream $ do
  stCell <- lift (newCell s0)
  pure $ ContT $ \k -> do
    cur <- readCell stCell
    (keepGoing, nxt, val) <- stepFn cur
    ifThenElseM_ keepGoing (writeCell stCell nxt >> k (Yield val)) (k Stop)

-- | Stateful stream unfold that checks @cond cur@ BEFORE running @stepFn cur@,
-- making pointer-chain walks null-safe and mutation/free-safe (since @stepFn@
-- can prefetch @next@ before yielding @cur@).
unfoldWhileM ::
  ( Monad m,
    RefCell m e s,
    CmdCond m e
  ) =>
  s ->
  (s -> m (e Bool)) ->
  (s -> m (s, a)) ->
  Stream m a
unfoldWhileM s0 cond stepFn = Stream $ do
  stCell <- lift (newCell s0)
  pure $ ContT $ \k -> do
    cur <- readCell stCell
    ok <- cond cur
    ifThenElseM_
      ok
      ( do
          (nxt, val) <- stepFn cur
          writeCell stCell nxt
          k (Yield val)
      )
      (k Stop)

-- | Infinite stream @x0, step x0, step (step x0), ...@ backed by a mutable cell.
iterateS ::
  ( Monad m,
    RefCell m e s
  ) =>
  s ->
  (s -> s) ->
  Stream m s
iterateS s0 step = Stream $ do
  stCell <- lift (newCell s0)
  pure $ ContT $ \k -> do
    cur <- readCell stCell
    writeCell stCell (step cur)
    k (Yield cur)

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

-- | Produce a stream from a range @[l, u)@ in reverse order: @u - 1, u - 2, ..., l@.
reverseRangeS ::
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
reverseRangeS l u = Stream $ do
  r <- lift $ newRef u
  pure $ ContT $ \k -> do
    r' <- readRef r
    ifThenElseM_
      (r' `gt` l)
      ( do
          prev <- letM (r' - 1)
          writeRef r prev
          k (Yield prev)
      )
      (k Stop)

-- | Pair each yielded element with its 0-based index @0, 1, 2, ...@.
indexedS ::
  ( CmdRef m e,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  Stream m a ->
  Stream m (e Int64, a)
indexedS (Stream s) = Stream $ do
  idxRef <- lift (newRef 0)
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        i <- readRef idxRef
        writeRef idxRef (i + 1)
        k (Yield (i, a))
      Skip -> k Skip
      Stop -> k Stop

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

-- | Map an effectful predicate+value producer over a stream, emitting 'Yield'
-- when the returned boolean is true and 'Skip' when false.
filterMapS :: (Monad m, CmdCond m e) => (a -> m (e Bool, b)) -> Stream m a -> Stream m b
filterMapS f (Stream s) = Stream $ do
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        (keep, b) <- f a
        ifThenElseM_ keep (k (Yield b)) (k Skip)
      Skip -> k Skip
      Stop -> k Stop

-- | Yield elements while predicate holds, stopping immediately on the first failure.
takeWhileS ::
  ( CmdCond m e,
    CmdRef m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> e Bool) ->
  Stream m a ->
  Stream m a
takeWhileS p (Stream s) = Stream $ do
  doneRef <- lift (newRef false)
  body <- s
  pure $ ContT $ \k -> do
    done <- readRef doneRef
    ifThenElseM_
      done
      (k Stop)
      ( runContT body $ \case
          Yield a ->
            ifThenElseM_
              (p a)
              (k (Yield a))
              (writeRef doneRef true >> k Stop)
          Skip -> k Skip
          Stop -> writeRef doneRef true >> k Stop
      )

-- | Drop leading elements while predicate holds, yielding that failing element and all subsequent elements.
dropWhileS ::
  ( CmdCond m e,
    CmdRef m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> e Bool) ->
  Stream m a ->
  Stream m a
dropWhileS p (Stream s) = Stream $ do
  droppingRef <- lift (newRef true)
  body <- s
  pure $ do
    step <- body
    ContT $ \k -> case step of
      Yield a -> do
        dropping <- readRef droppingRef
        ifThenElseM_
          dropping
          (ifThenElseM_ (p a) (k Skip) (writeRef droppingRef false >> k (Yield a)))
          (k (Yield a))
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

--------------------------------------------------------------------------------
-- Sorted Stream Relational Combinators (Union, Intersection, Difference)
--------------------------------------------------------------------------------

-- | Merge-union two strictly ascending sorted streams in O(N + M), combining
-- equal elements via @combineEq@.
--
-- State encoding for each input side: @0@ = need pull, @1@ = buffered item ready, @2@ = exhausted.
unionSortedByS ::
  forall m e a.
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdCase m e,
    RefCell m e a,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (a -> a -> e Bool) ->
  (a -> a -> e Bool) ->
  (a -> a -> a) ->
  Stream m a ->
  Stream m a ->
  Stream m a
unionSortedByS isLt isEq combineEq (Stream sA) (Stream sB) = Stream $ do
  stA <- lift (newRef (0 :: e Int64))
  stB <- lift (newRef (0 :: e Int64))
  bufA <- lift newCell'
  bufB <- lift newCell'
  bodyA <- sA
  bodyB <- sB
  pure $ ContT $ \k -> do
    aState <- readRef stA
    bState <- readRef stB
    switchCase_
      aState
      [ ( 0,
          runContT bodyA $ \case
            Yield a -> writeCell bufA a >> writeRef stA 1 >> k Skip
            Skip -> k Skip
            Stop -> writeRef stA 2 >> k Skip
        )
      ]
      $ switchCase_
        bState
        [ ( 0,
            runContT bodyB $ \case
              Yield b -> writeCell bufB b >> writeRef stB 1 >> k Skip
              Skip -> k Skip
              Stop -> writeRef stB 2 >> k Skip
          )
        ]
        $ switchCase_
          (aState * 3 + bState)
          [ ( 4, -- aState=1, bState=1
              do
                a <- readCell bufA
                b <- readCell bufB
                ifThenElseM_
                  (isLt a b)
                  (writeRef stA 0 >> k (Yield a))
                  ( ifThenElseM_
                      (isEq a b)
                      (writeRef stA 0 >> writeRef stB 0 >> k (Yield (combineEq a b)))
                      (writeRef stB 0 >> k (Yield b))
                  )
            ),
            ( 5, -- aState=1, bState=2
              do
                a <- readCell bufA
                writeRef stA 0
                k (Yield a)
            ),
            ( 7, -- aState=2, bState=1
              do
                b <- readCell bufB
                writeRef stB 0
                k (Yield b)
            )
          ]
          (k Stop)

-- | Merge-intersect two strictly ascending sorted streams in O(N + M).
intersectSortedByS ::
  forall m e a b c.
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdCase m e,
    RefCell m e a,
    RefCell m e b,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (a -> b -> e Bool) ->
  (a -> b -> e Bool) ->
  (a -> b -> c) ->
  Stream m a ->
  Stream m b ->
  Stream m c
intersectSortedByS isLt isEq combineEq (Stream sA) (Stream sB) = Stream $ do
  stA <- lift (newRef (0 :: e Int64))
  stB <- lift (newRef (0 :: e Int64))
  bufA <- lift newCell'
  bufB <- lift newCell'
  bodyA <- sA
  bodyB <- sB
  pure $ ContT $ \k -> do
    aState <- readRef stA
    bState <- readRef stB
    switchCase_
      aState
      [ (2, k Stop),
        ( 0,
          runContT bodyA $ \case
            Yield a -> writeCell bufA a >> writeRef stA 1 >> k Skip
            Skip -> k Skip
            Stop -> writeRef stA 2 >> k Stop
        )
      ]
      $ switchCase_
        bState
        [ (2, k Stop),
          ( 0,
            runContT bodyB $ \case
              Yield b -> writeCell bufB b >> writeRef stB 1 >> k Skip
              Skip -> k Skip
              Stop -> writeRef stB 2 >> k Stop
          )
        ]
        $ do
          a <- readCell bufA
          b <- readCell bufB
          ifThenElseM_
            (isLt a b)
            (writeRef stA 0 >> k Skip)
            ( ifThenElseM_
                (isEq a b)
                (writeRef stA 0 >> writeRef stB 0 >> k (Yield (combineEq a b)))
                (writeRef stB 0 >> k Skip)
            )

-- | Sorted set/map difference @A \ B@ over strictly ascending sorted streams in O(N + M).
differenceSortedByS ::
  forall m e a b.
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdCase m e,
    RefCell m e a,
    RefCell m e b,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (a -> b -> e Bool) ->
  (a -> b -> e Bool) ->
  Stream m a ->
  Stream m b ->
  Stream m a
differenceSortedByS isLt isEq (Stream sA) (Stream sB) = Stream $ do
  stA <- lift (newRef (0 :: e Int64))
  stB <- lift (newRef (0 :: e Int64))
  bufA <- lift newCell'
  bufB <- lift newCell'
  bodyA <- sA
  bodyB <- sB
  pure $ ContT $ \k -> do
    aState <- readRef stA
    bState <- readRef stB
    switchCase_
      aState
      [ (2, k Stop),
        ( 0,
          runContT bodyA $ \case
            Yield a -> writeCell bufA a >> writeRef stA 1 >> k Skip
            Skip -> k Skip
            Stop -> writeRef stA 2 >> k Stop
        )
      ]
      $ switchCase_
        bState
        [ ( 2,
            do
              a <- readCell bufA
              writeRef stA 0
              k (Yield a)
          ),
          ( 0,
            runContT bodyB $ \case
              Yield b -> writeCell bufB b >> writeRef stB 1 >> k Skip
              Skip -> k Skip
              Stop -> writeRef stB 2 >> k Skip
          )
        ]
        $ do
          a <- readCell bufA
          b <- readCell bufB
          ifThenElseM_
            (isLt a b)
            (writeRef stA 0 >> k (Yield a))
            ( ifThenElseM_
                (isEq a b)
                (writeRef stA 0 >> writeRef stB 0 >> k Skip)
                (writeRef stB 0 >> k Skip)
            )
