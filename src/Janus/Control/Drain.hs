{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Janus.Control.Drain where

import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Int
import Foreign.Ptr (Ptr)
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Ref
import Janus.Command.While
import Janus.Control.Step
import Janus.Control.Stream
import Janus.Expression.Bool
import Janus.Expression.Cond
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord
import Janus.Typed

newtype Drain m a r = Drain {getDrain :: Stream m a -> m r}
  deriving (Functor, Applicative, Monad, MonadFix) via ReaderT (Stream m a) m

drain_ :: (CmdRef m e, CmdWhile m e, ExpBool e, JanusTyped e Bool) => Drain m a ()
drain_ = Drain $ \(Stream s) -> runCodensity s $ \body -> do
  whileM true $ \c -> do
    runContT body $ \case
      Yield _ -> pure ()
      Skip -> pure ()
      Stop -> writeRef c false

-- | Execute an effectful sink action for every yielded element.
drainForEach ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> m ()) ->
  Drain m a ()
drainForEach act = Drain $ \(Stream s) -> runCodensity s $ \body -> do
  whileM true $ \c -> do
    runContT body $ \case
      Yield a -> act a
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
drainFold b0 f = Drain $ \(Stream s) -> do
  r <- newRef b0
  runCodensity s $ \body -> do
    whileM true $ \c -> do
      runContT body $ \case
        Yield a -> modifyRef r (`f` a)
        Skip -> pure ()
        Stop -> writeRef c false
  readRef r

-- | Effectful fold over any 'RefCell'-storable state @s@ (scalars, tuples, or HKD values).
drainFoldM ::
  ( CmdRef m e,
    CmdWhile m e,
    RefCell m e s,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  s ->
  (s -> a -> m s) ->
  Drain m a s
drainFoldM s0 step = Drain $ \(Stream s) -> do
  cell <- newCell s0
  runCodensity s $ \body -> do
    whileM true $ \c -> do
      runContT body $ \case
        Yield a -> do
          cur <- readCell cell
          nxt <- step cur a
          writeCell cell nxt
        Skip -> pure ()
        Stop -> writeRef c false
  readCell cell

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

-- | Single-pass simultaneous minimum and maximum reduction.
drainMinMax ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e b,
    Bounded (e b),
    ExpCond e,
    ExpOrd e b
  ) =>
  Drain m (e b) (e b, e b)
drainMinMax = Drain $ \(Stream s) -> do
  rMin <- newRef maxBound
  rMax <- newRef minBound
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield x -> do
          modifyRef rMin (`min'` x)
          modifyRef rMax (`max'` x)
        Skip -> pure ()
        Stop -> writeRef c false
  (,) <$> readRef rMin <*> readRef rMax

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

-- | Count yielded elements satisfying a predicate.
drainCountWhere ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (a -> e Bool) ->
  Drain m a (e Int64)
drainCountWhere p = Drain $ \(Stream s) -> do
  cnt <- newRef 0
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a -> whenM_ (p a) (modifyRef cnt (+ 1))
        Skip -> pure ()
        Stop -> writeRef c false
  readRef cnt

-- | Short-circuiting existential predicate drain (stops on first @true@).
drainAny ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> e Bool) ->
  Drain m a (e Bool)
drainAny p = Drain $ \(Stream s) -> do
  found <- newRef false
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a ->
          whenM_ (p a) $ do
            writeRef found true
            writeRef c false
        Skip -> pure ()
        Stop -> writeRef c false
  readRef found

-- | Short-circuiting universal predicate drain (stops on first @false@).
drainAll ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> e Bool) ->
  Drain m a (e Bool)
drainAll p = Drain $ \(Stream s) -> do
  ok <- newRef true
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a ->
          ifThenElseM_
            (p a)
            (pure ())
            (writeRef ok false >> writeRef c false)
        Skip -> pure ()
        Stop -> writeRef c false
  readRef ok

-- | Short-circuiting negated existential predicate drain (stops on first @true@).
drainNone ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  (a -> e Bool) ->
  Drain m a (e Bool)
drainNone p = lnot <$> drainAny p

-- | Short-circuiting search returning @(found, matchedOrDefault)@.
drainFind ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    RefCell m e a,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  a ->
  (a -> e Bool) ->
  Drain m a (e Bool, a)
drainFind defVal p = Drain $ \(Stream s) -> do
  found <- newRef false
  resCell <- newCell defVal
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a -> do
          already <- readRef found
          whenM_ (lnot already `land` p a) $ do
            writeRef found true
            writeCell resCell a
            writeRef c false
        Skip -> pure ()
        Stop -> writeRef c false
  (,) <$> readRef found <*> readCell resCell

-- | Short-circuiting drain taking the first yielded element (if any).
drainFirst ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    RefCell m e a,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  a ->
  Drain m a (e Bool, a)
drainFirst defVal = drainFind defVal (const true)

-- | Drain returning the last yielded element (if any).
drainLast ::
  ( CmdRef m e,
    CmdWhile m e,
    RefCell m e a,
    ExpBool e,
    JanusTyped e Bool
  ) =>
  a ->
  Drain m a (e Bool, a)
drainLast defVal = Drain $ \(Stream s) -> do
  found <- newRef false
  resCell <- newCell defVal
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a -> writeRef found true >> writeCell resCell a
        Skip -> pure ()
        Stop -> writeRef c false
  (,) <$> readRef found <*> readCell resCell

-- | Short-circuiting drain returning the 0-based @n@-th yielded element.
drainNth ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdCond m e,
    RefCell m e a,
    ExpBool e,
    ExpEq e Int64,
    JanusTyped e Bool,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  e Int64 ->
  a ->
  Drain m a (e Bool, a)
drainNth n defVal = Drain $ \(Stream s) -> do
  found <- newRef false
  idx <- newRef 0
  resCell <- newCell defVal
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a -> do
          already <- readRef found
          whenM_ (lnot already) $ do
            i <- readRef idx
            writeRef idx (i + 1)
            whenM_ (i `eq` n) $ do
              writeRef found true
              writeCell resCell a
              writeRef c false
        Skip -> pure ()
        Stop -> writeRef c false
  (,) <$> readRef found <*> readCell resCell

-- | Welford single-pass online mean reduction.
drainMean ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e a,
    Fractional (e a)
  ) =>
  Drain m (e a) (e a)
drainMean = Drain $ \(Stream s) -> do
  rMean <- newRef 0
  rCnt <- newRef 0
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield x -> do
          mean <- readRef rMean
          cnt <- readRef rCnt
          n' <- letM (cnt + 1)
          delta <- letM (x - mean)
          mean' <- letM (mean + delta / n')
          writeRef rMean mean'
          writeRef rCnt n'
        Skip -> pure ()
        Stop -> writeRef c false
  readRef rMean

-- | Welford single-pass online @(mean, sampleVariance)@ reduction.
drainMeanVariance ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    ExpCond e,
    ExpOrd e a,
    JanusTyped e Bool,
    JanusTyped e a,
    Fractional (e a)
  ) =>
  Drain m (e a) (e a, e a)
drainMeanVariance = Drain $ \(Stream s) -> do
  rMean <- newRef 0
  rM2 <- newRef 0
  rCnt <- newRef 0
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield x -> do
          mu <- readRef rMean
          s2 <- readRef rM2
          cnt <- readRef rCnt
          n' <- letM (cnt + 1)
          d1 <- letM (x - mu)
          mu' <- letM (mu + d1 / n')
          d2 <- letM (x - mu')
          s2' <- letM (s2 + d1 * d2)
          writeRef rMean mu'
          writeRef rM2 s2'
          writeRef rCnt n'
        Skip -> pure ()
        Stop -> writeRef c false
  mean <- readRef rMean
  m2 <- readRef rM2
  count <- readRef rCnt
  pure (mean, ifThenElse (count `gt` 1) (m2 / (count - 1)) 0)

-- | Write stream elements sequentially into memory via a write callback @(idx -> a -> m ())@,
-- returning the total number of elements written.
drainIndexed ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (e Int64 -> a -> m ()) ->
  Drain m a (e Int64)
drainIndexed writeAt = Drain $ \(Stream s) -> do
  idx <- newRef 0
  runCodensity s $ \body ->
    whileM true $ \c ->
      runContT body $ \case
        Yield a -> do
          i <- readRef idx
          writeAt i a
          writeRef idx (i + 1)
        Skip -> pure ()
        Stop -> writeRef c false
  readRef idx

-- | Write scalar stream elements sequentially into a pointer @dst@, returning count written.
drainToPtr ::
  ( CmdRef m e,
    CmdWhile m e,
    ExpBool e,
    JanusTyped e Bool,
    JanusTyped e Int64,
    Num (e Int64)
  ) =>
  (e (Ptr a) -> e a -> e Int64 -> m ()) ->
  e (Ptr a) ->
  Drain m (e a) (e Int64)
drainToPtr pokeOff dst = drainIndexed (\i x -> pokeOff dst x i)

runDrain :: Drain m a r -> Stream m a -> m r
runDrain (Drain d) = d
