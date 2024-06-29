{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Janus.Command.Range where

import Control.Lens
import Control.Monad.State
import Data.Int
import Data.Loc
import Data.Monoid
import Data.Proxy
import Janus.Backend.C
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Ord
import Janus.Typed
import Language.C.Quote

class CmdRange m e | m -> e, e -> m where
  rangeM :: (JanusTyped e Int64, ExpOrd e Int64, Num (e Int64)) => e Int64 -> e Int64 -> (e Int64 -> m ()) -> m ()
  default rangeM ::
    (JanusTyped e Bool, JanusTyped e Int64, CmdRef m e, CmdWhile m e, ExpOrd e Int64, Num (e Int64)) =>
    e Int64 ->
    e Int64 ->
    (e Int64 -> m ()) ->
    m ()
  rangeM l u body = do
    r <- newRef l
    whileM (l `lt` u) $ \c -> do
      r' <- readRef r
      r'' <- letM $ r' + 1
      body r'
      writeRef r r''
      writeRef c (r'' `lt` u)

instance CmdRange IO Identity where

instance CmdRange JanusCM JanusC where
  rangeM :: forall a. JanusCTyped a =>
    JanusC a ->
    JanusC a ->
    (JanusC a -> JanusCM ()) ->
    JanusCM ()
  rangeM (JanusC l) (JanusC u) body = do
    RVal l' <- l
    RVal u' <- u
    JCType spec dec <- getJanusCType (Proxy @a)
    case spec of
      DeclSpec _ _ tspec _ -> do
        fn <- getFunction
        let block = fn ^. jcfBlock
            c = fn ^. jcfVarCounter
            idxId = Id ("i_" <> show c) noLoc
            idx = Var idxId noLoc
            fn' = fn & jcfBlock .~ mempty
                     & jcfVarCounter +~ 1
        fname <- askFuncName
        modify $ \s -> s & ix fname .~ fn'
        body $ JanusC $ pure $ RVal idx
        fn'' <- getFunction
        let igroup = InitGroup (DeclSpec [] [] tspec noLoc) [] idxInit noLoc
            idxInit = [Init idxId dec Nothing (Just $ ExpInitializer l' noLoc) [] noLoc]
            idxCond = BinOp Lt idx u' noLoc
            idxAdv = PostInc idx noLoc
            forBlock = Block (appEndo (fn'' ^. jcfBlock) []) noLoc
            forloop = [BlockStm $ For (Left igroup) (Just idxCond) (Just idxAdv) forBlock noLoc]
            fn''' = fn'' & jcfBlock .~ appendBlock block forloop
        modify $ \s -> s & ix fname .~ fn'''
      _ -> error "JanusC.rangeM: the impossible happened"
