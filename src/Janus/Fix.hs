{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Janus.Fix where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.Kind as K
import Data.Loc
import Data.Proxy
import qualified Data.Set as Set
import Janus.Backend.C
import Language.C.Quote

class JanusCFix r where
  jcfun :: String -> JanusCM [Exp] -> JanusCM [Param] -> Int -> r -> r
  jcfake :: String -> JanusCM [Exp] -> r -> r

instance (JanusCTyped a) => JanusCFix (JanusCM (JanusC a)) where
  jcfun name args params _ body = do
    params' <- local (const name) params
    let params'' = Params (reverse params') False noLoc
    local (const name) $ do
      modify $ \s -> s & ix name . jcfParams .~ params''
      body' <- body
      finishFunction body'
    JCType spec dec <- getJanusCType (Proxy @a)
    let proto = InitGroup spec [] [Init (Id name noLoc) (Proto dec params'' noLoc) Nothing Nothing [] noLoc] noLoc
    modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
    args' <- args
    pure $ JanusC $ pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
  jcfake name args _ = do
    args' <- args
    pure $ JanusC $ pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (JanusCTyped a) => JanusCFix (JanusC a) where
  jcfun name args params _ body = JanusC $ do
    params' <- local (const name) params
    let params'' = Params (reverse params') False noLoc
    local (const name) $ do
      modify $ \s -> s & ix name . jcfParams .~ params''
      finishFunction body
    JCType spec dec <- getJanusCType (Proxy @a)
    let proto = InitGroup spec [] [Init (Id name noLoc) (Proto dec params'' noLoc) Nothing Nothing [] noLoc] noLoc
    modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
    args' <- args
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
  jcfake name args _ = JanusC $ do
    args' <- args
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (JanusCTyped a, JanusCFix r) => JanusCFix (JanusC a -> r) where
  jcfun name args parms n body x = jcfun name args' parms' (n + 1) (body x')
    where
      args' = do
        RVal y <- getJanusC x
        ys <- args
        pure (y : ys)
      parms' = local (const name) $ do
        ys <- parms
        JCType spec dec <- getJanusCType (Proxy @a)
        let xid = Id ("arg_" <> show n) noLoc
        pure (Param (Just xid) spec dec noLoc : ys)
      x' = JanusC $ do
        let xid = Id ("arg_" <> show n) noLoc
            v = Var xid noLoc
        pure $ RVal v
  jcfake name args f x = flip (jcfake name) (f x) $ do
    RVal x' <- getJanusC x
    args' <- args
    pure (x' : args')

jfix ::
  (JanusCFix r) => String -> (r -> r) -> r
jfix name f = jcfun name (pure []) (pure []) 0 (f $ jcfake name (pure []) (jfix name f))

-- TODO find a way to get rid of the Proxy argument
class ExpFix (e :: K.Type -> K.Type) r where
  fun :: Proxy e -> String -> (r -> r) -> r

instance ExpFix Identity r where
  fun _ _ = fix
  {-# INLINE fun #-}

instance (JanusCFix r) => ExpFix JanusC r where
  fun _ = jfix
