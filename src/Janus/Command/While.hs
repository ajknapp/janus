{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}

module Janus.Command.While where

import Control.Monad
import Data.Functor.Identity (Identity (..))
import Janus.Backend.C.Quote (captureBlock_, jcStms)
import Janus.Backend.C.Raw
import Janus.Command.Ref
import Janus.Expression.Bool

class (Monad m, ExpBool e, CmdRef m e) => CmdWhile m e | e -> m, m -> e where
  whileM :: e Bool -> (Ref m e Bool -> m ()) -> m ()

instance CmdWhile IO Identity where
  whileM a body = do
    r <- newRef a
    let go = do
          r' <- readRef r
          when (runIdentity r') $ body r >> go
    go

instance CmdWhile (JanusCMT t) (JanusCT t) where
  whileM c body = do
    ref <- newRef c
    bodyItems <- captureBlock_ (body ref)
    [jcStms| while ($exp:ref) { $items:bodyItems } |]
