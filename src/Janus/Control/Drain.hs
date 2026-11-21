{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module Janus.Control.Drain where

import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Int
import Janus.Control.Step
import Janus.Control.Stream
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Cond
import Janus.Expression.Bool
import Janus.Typed
import Janus.Expression.Ord

newtype Drain m a r = Drain {getDrain :: Stream m a -> m r}
  deriving (Functor, Applicative, Monad, MonadFix) via ReaderT (Stream m a) m

drain_ :: (CmdRef m e, CmdWhile m e, ExpBool e, JanusTyped e Bool) => Drain m a ()
drain_ = Drain $ \(Stream s) -> runCodensity s $ \body -> do
  whileM true $ \c -> do
    runContT body $ \case
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
drainFold b0 f = Drain $ \(Stream s) -> do
  r <- newRef b0
  runCodensity s $ \body -> do
    whileM true $ \c -> do
      runContT body $ \case
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

runDrain :: Drain m a r -> Stream m a -> m r
runDrain (Drain d) = d
