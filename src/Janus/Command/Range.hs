{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Command.Range where

import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.Proxy
import Janus.Backend.C.Quote (captureBlock_, freshCVar, getCType, jcStms)
import Janus.Backend.C.Raw
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Ord
import Janus.Typed

class (JanusTyped e Int64, ExpOrd e Int64, Num (e Int64)) => CmdRange m e | m -> e, e -> m where
  rangeM :: e Int64 -> e Int64 -> (e Int64 -> m ()) -> m ()
  default rangeM ::
    (JanusTyped e Bool, CmdRef m e, CmdWhile m e) =>
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

instance CmdRange (JanusCMT t) (JanusCT t) where
  rangeM ::
    forall a.
    (JanusCTyped a) =>
    JanusCT t a ->
    JanusCT t a ->
    (JanusCT t a -> JanusCMT t ()) ->
    JanusCMT t ()
  rangeM (JanusC l) (JanusC u) body = do
    RVal l' <- liftJanusCM l
    RVal u' <- liftJanusCM u
    idxTy <- getCType (Proxy @a)
    (idxId, idxExp) <- freshCVar "i"
    bodyItems <- captureBlock_ (body (JanusC (pure (RVal idxExp))))
    [jcStms| for ($ty:idxTy $id:idxId = $exp:l'; $id:idxId < $exp:u'; $id:idxId++) { $items:bodyItems } |]
