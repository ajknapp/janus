{-# LANGUAGE FunctionalDependencies #-}

module Janus.Command.While where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Loc
import Data.Monoid
import Janus.Backend.C
import Janus.Command.Ref
import Janus.Expression.Bool
import Language.C.Quote

class (Monad m, ExpBool e, CmdRef m e) => CmdWhile m e | e -> m, m -> e where
  whileM :: e Bool -> (Ref m e Bool -> m ()) -> m ()

instance CmdWhile IO Identity where
  whileM a body = do
    r <- newRef a
    let go = do
          r' <- readRef r
          when (runIdentity r') $ body r >> go
    go

instance CmdWhile JanusCM JanusC where
  whileM c body = do
    ref@(JanusCRef (LVal r)) <- newRef c
    fn <- getFunction
    fname <- askFuncName
    let block = fn ^. jcfBlock
        fn' = fn & jcfBlock .~ mempty
    modify $ \s -> s & ix fname .~ fn'
    body ref
    fn'' <- getFunction
    let block' = fn'' ^. jcfBlock
        whileStm = BlockStm $ While r (Block (appEndo block' []) noLoc) noLoc
        fn''' = fn'' & jcfBlock .~ appendBlock block [whileStm]
    modify $ \s -> s & ix fname .~ fn'''
