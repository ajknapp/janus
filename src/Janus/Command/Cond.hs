{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Janus.Command.Cond where

import Control.Lens
import Data.Loc
import Data.Monoid
import Janus.Backend.C
import Janus.Command.Ref
import Janus.Expression.Bool
import Janus.Typed
import Language.C.Quote

class (ExpBool e, Monad m) => CmdCond m e | e -> m, m -> e where
  ifThenElseM_ :: e Bool -> m () -> m () -> m ()

instance CmdCond IO Identity where
  ifThenElseM_ (Identity p) a b = if p then a else b

instance CmdCond JanusCM JanusC where
  ifThenElseM_ (JanusC c) t f = do
    RVal c' <- c
    fn <- getFunction
    modifyFunction $ \jfn -> jfn & jcfBlock .~ mempty
    t
    fn' <- getFunction
    let tblock = fn' ^. jcfBlock
    modifyFunction $ \jfn -> jfn & jcfBlock .~ mempty
    f
    fn'' <- getFunction
    let fblock = fn'' ^. jcfBlock
        fblock' = appEndo fblock []
        ifteStm =
          BlockStm $
            If
              c'
              (Block (appEndo tblock []) noLoc)
              (if null fblock' then Nothing else Just (Block fblock' noLoc))
              noLoc
    modifyFunction $ \jfn -> jfn & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ifteStm]

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
