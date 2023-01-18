{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Janus.Command.Ref where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity
import Data.IORef
import Data.Proxy
import Language.C.Quote
import Janus.Backend.C
import Janus.Typed
import Control.Lens
import Data.Loc

class Monad m => CmdRef m e | m -> e, e -> m where
  type Ref m e a = r | r -> m e a
  newRef :: JanusTyped e a => e a -> m (Ref m e a)
  newRef' :: JanusTyped e a => m (Ref m e a)
  readRef :: JanusTyped e a => Ref m e a -> m (e a)
  writeRef :: JanusTyped e a => Ref m e a -> e a -> m ()

instance CmdRef IO Identity where
  type Ref IO Identity a = IORef a
  newRef (Identity a) = newIORef a
  newRef' = newIORef undefined
  readRef ref = Identity <$> readIORef ref
  writeRef ref (Identity a) = writeIORef ref a

letM :: (CmdRef m e, JanusTyped e a) => e a -> m (e a)
letM e = newRef e >>= readRef

modifyRef :: (CmdRef m e, JanusTyped e a) => Ref m e a -> (e a -> e a) -> m ()
modifyRef r f = readRef r >>= writeRef r . f

newtype JanusCRef a = JanusCRef { getJanusCRef :: LVal a }

instance CmdRef JanusCM JanusC where
  type Ref JanusCM JanusC a = JanusCRef a

  newRef :: forall a. JanusTyped JanusC a => JanusC a -> JanusCM (Ref JanusCM JanusC a)
  newRef (JanusC a) = do
    RVal a' <- a
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- ask
    let c = fn ^. jcfVarCounter
        var1 = Id ("ref_" <> show c) noLoc
        ini = Init var1 dec Nothing (Just $ ExpInitializer a' noLoc) [] noLoc
        ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' = fn & jcfVarCounter +~ 1
                 & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
    modify $ \s -> s & ix fname .~ fn'
    pure $ JanusCRef $ LVal $ Var var1 noLoc

  newRef' :: forall a. JanusTyped JanusC a => JanusCM (Ref JanusCM JanusC a)
  newRef' =  do
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- ask
    let c = fn ^. jcfVarCounter
        var1 = Id ("ref_" <> show c) noLoc
        ini = Init var1 dec Nothing Nothing [] noLoc
        ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' = fn & jcfVarCounter +~ 1
                 & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
    modify $ \s -> s & ix fname .~ fn'
    pure $ JanusCRef $ LVal $ Var var1 noLoc

  readRef :: forall a. JanusTyped JanusC a => Ref JanusCM JanusC a -> JanusCM (JanusC a)
  readRef (JanusCRef (LVal ref)) = do
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- ask
    let c = fn ^. jcfVarCounter
        val1 = Id ("x_" <> show c) noLoc
        ini = Init val1 dec Nothing (Just $ ExpInitializer ref noLoc) [] noLoc
        ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' = fn & jcfVarCounter +~ 1
                 & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
    modify $ \s -> s & ix fname .~ fn'
    pure $ JanusC $ JanusCM $ pure $ RVal $ Var val1 noLoc

  writeRef :: forall a. JanusTyped JanusC a => Ref JanusCM JanusC a -> JanusC a -> JanusCM ()
  writeRef (JanusCRef (LVal ref)) (JanusC a) = do
    RVal a' <- a
    modifyFunction $ \fn ->
      let c = fn ^. jcfVarCounter
          ref1 = BlockStm $ Exp (Just $ Assign ref JustAssign a' noLoc) noLoc
      in fn & jcfVarCounter .~ (c + 1)
            & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
