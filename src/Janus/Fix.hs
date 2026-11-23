{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-cse #-}

module Janus.Fix where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (unpack)
import Data.Char (isAlphaNum)
import Data.Function
import Data.Kind as K
import Data.Loc
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable, typeRep)
import Janus.Backend.C
import Language.C.Quote

class (Typeable r) => JanusCFix r where
  jcfun :: String -> JanusCM [Exp] -> JanusCM [Param] -> Int -> r -> r
  jcfake :: String -> JanusCM [Exp] -> r -> r

localFun :: JCFuncInfo -> JanusCMT t a -> JanusCMT t a
localFun x = local (const x)

instance (Typeable t, JanusCTyped a) => JanusCFix (JanusCMT t (JanusCT t a)) where
  jcfun name args params _ body = do
    fi <- ask
    let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
    alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup name)
    params' <- liftJanusCM (localFun funInfo params)
    let params'' = Params (reverse params') False noLoc
    unless alreadyDefined $
      localFun funInfo $ do
        modify $ \s -> s & ix name . jcfParams .~ params''
        body' <- body
        finishFunction body'
    JCType spec dec <- getJanusCType (Proxy @a)
    let proto = InitGroup (setTypeQualifiers (jcTypeQuals funInfo) spec) [] [Init (Id name noLoc) (Proto dec params'' noLoc) Nothing Nothing [] noLoc] noLoc
    modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
    args' <- liftJanusCM args
    pure $ JanusC $ pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
  jcfake name args _ = do
    args' <- liftJanusCM args
    pure $ JanusC $ pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (Typeable t, JanusCTyped a) => JanusCFix (JanusCT t a) where
  jcfun name args params _ body = JanusC $ do
    fi <- ask
    let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
    alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup name)
    params' <- localFun funInfo params
    let params'' = Params (reverse params') False noLoc
    unless alreadyDefined $
      localFun funInfo $ do
        modify $ \s -> s & ix name . jcfParams .~ params''
        finishFunction body
    JCType spec dec <- getJanusCType (Proxy @a)
    let proto = InitGroup (setTypeQualifiers (jcTypeQuals funInfo) spec) [] [Init (Id name noLoc) (Proto dec params'' noLoc) Nothing Nothing [] noLoc] noLoc
    modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
    args' <- args
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
  jcfake name args _ = JanusC $ do
    args' <- args
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (Typeable t, JanusCTyped a, JanusCFix r) => JanusCFix (JanusCT t a -> r) where
  jcfun name args parms n body x = jcfun name args' parms' (n + 1) (body x')
    where
      args' = do
        RVal y <- getJanusC x
        ys <- args
        pure (y : ys)
      parms' = do
        fi <- ask
        let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
        localFun funInfo $ do
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

sanitizeLocalIdent :: String -> String
sanitizeLocalIdent = map (\c -> if isAlphaNum c || c == '_' then c else '_')

jfix ::
  forall r.
  (JanusCFix r) =>
  String ->
  (r -> r) ->
  r
jfix name f =
  let tySha = SHA256.hash (Text.encodeUtf8 (Text.pack (show (typeRep (Proxy @r)))))
      mangled =
        "janus_local_"
          <> sanitizeLocalIdent name
          <> "_"
          <> take 12 (unpack (B16.encode tySha))
   in jcfun mangled (pure []) (pure []) 0 (f $ jcfake mangled (pure []) (jfix name f))

-- TODO find a way to get rid of the Proxy argument
--
-- only use fun directly beneath a toplevel, pointfree definition like so:
-- foo :: e a -> e b -> e c -> e d
-- foo = fun (Proxy @e) "foo" $ \x y z -> ... x ... y ... (bar z)
-- to prevent scope extrusion problems
class ExpFix (e :: K.Type -> K.Type) r where
  fun :: Proxy e -> String -> (r -> r) -> r

instance ExpFix Identity r where
  fun _ _ = fix
  {-# INLINE fun #-}

instance (JanusCFix r) => ExpFix (JanusCT t) r where
  fun _ = jfix
