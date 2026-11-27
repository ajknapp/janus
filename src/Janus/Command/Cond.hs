{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}

module Janus.Command.Cond where

import Data.Functor.Identity (Identity (..))
import Janus.Backend.C.Quote (captureBlock_, jcStms)
import Janus.Backend.C.Raw
import Janus.Command.Ref
import Janus.Expression.Bool
import Janus.Typed

class (ExpBool e, Monad m) => CmdCond m e | e -> m, m -> e where
  ifThenElseM_ :: e Bool -> m () -> m () -> m ()

instance CmdCond IO Identity where
  ifThenElseM_ (Identity p) a b = if p then a else b

instance CmdCond (JanusCMT t) (JanusCT t) where
  ifThenElseM_ (JanusC c) t f = do
    RVal c' <- liftJanusCM c
    tItems <- captureBlock_ t
    fItems <- captureBlock_ f
    if null fItems
      then [jcStms| if ($exp:c') { $items:tItems } |]
      else [jcStms| if ($exp:c') { $items:tItems } else { $items:fItems } |]

ifThenElseM ::
  (CmdCond m e, CmdRef m e, JanusTyped e a) =>
  e Bool ->
  m (e a) ->
  m (e a) ->
  m (e a)
ifThenElseM c t f = do
  r <- newRef'
  ifThenElseM_ c (t >>= writeRef r) (f >>= writeRef r)
  readRef r

whenM_ :: (CmdCond m e) => e Bool -> m () -> m ()
whenM_ p m = ifThenElseM_ p m (pure ())

unlessM_ :: (CmdCond m e) => e Bool -> m () -> m ()
unlessM_ p = ifThenElseM_ p (pure ())
