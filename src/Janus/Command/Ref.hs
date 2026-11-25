{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Command.Ref where

import Control.Lens
import Control.Monad.State
import Data.IORef
import qualified Data.Kind as K
import Data.Loc
import Data.Proxy
import Janus.Backend.C.Raw
import Janus.Typed
import Language.C.Quote

class (Monad m) => CmdRef m e | m -> e, e -> m where
  type Ref m e a = r | r -> m e a
  letM :: (JanusTyped e a) => e a -> m (e a)
  newRef :: (JanusTyped e a) => e a -> m (Ref m e a)
  newRef' :: (JanusTyped e a) => m (Ref m e a)
  readRef :: (JanusTyped e a) => Ref m e a -> m (e a)
  writeRef :: (JanusTyped e a) => Ref m e a -> e a -> m ()

instance CmdRef IO Identity where
  type Ref IO Identity a = IORef a
  letM = pure
  newRef (Identity a) = newIORef a
  newRef' = newIORef undefined
  readRef ref = Identity <$> readIORef ref
  writeRef ref (Identity a) = writeIORef ref a

newRefBody :: forall t a. (JanusCTyped a) => String -> JanusCT t a -> JanusCMT t Exp
newRefBody name (JanusC a) = do
  RVal a' <- liftJanusCM a
  JCType spec dec <- getJanusCType (Proxy @a)
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      var1 = Id (name <> show c) noLoc
      ini = Init var1 dec Nothing (Just $ ExpInitializer a' noLoc) [] noLoc
      ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
      fn' =
        fn
          & jcfVarCounter +~ 1
          & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
  modify $ \s -> s & ix fname .~ fn'
  pure $ Var var1 noLoc

modifyRef :: (CmdRef m e, JanusTyped e a) => Ref m e a -> (e a -> e a) -> m ()
modifyRef r f = readRef r >>= writeRef r . f

newtype JanusCRefT (t :: K.Type) a = JanusCRefT {getJanusCRefT :: LVal a}

pattern JanusCRef :: LVal a -> JanusCRefT t a
pattern JanusCRef lv = JanusCRefT lv

{-# COMPLETE JanusCRef #-}

getJanusCRef :: JanusCRefT t a -> LVal a
getJanusCRef = getJanusCRefT

type JanusCRef = JanusCRefT PortableC

instance CmdRef (JanusCMT t) (JanusCT t) where
  type Ref (JanusCMT t) (JanusCT t) a = JanusCRefT t a

  letM x = JanusC . pure . RVal <$> newRefBody "x_" x

  newRef x = JanusCRef . LVal <$> newRefBody "ref_" x

  newRef' :: forall a. (JanusTyped (JanusCT t) a) => JanusCMT t (Ref (JanusCMT t) (JanusCT t) a)
  newRef' = do
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- askFuncName
    let c = fn ^. jcfVarCounter
        var1 = Id ("ref_" <> show c) noLoc
        ini = Init var1 dec Nothing Nothing [] noLoc
        ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' =
          fn
            & jcfVarCounter +~ 1
            & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
    modify $ \s -> s & ix fname .~ fn'
    pure $ JanusCRef $ LVal $ Var var1 noLoc

  readRef :: forall a. (JanusTyped (JanusCT t) a) => Ref (JanusCMT t) (JanusCT t) a -> JanusCMT t (JanusCT t a)
  readRef (JanusCRef (LVal ref)) = do
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- askFuncName
    let c = fn ^. jcfVarCounter
        val1 = Id ("x_" <> show c) noLoc
        ini = Init val1 dec Nothing (Just $ ExpInitializer ref noLoc) [] noLoc
        ref1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' =
          fn
            & jcfVarCounter +~ 1
            & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]
    modify $ \s -> s & ix fname .~ fn'
    pure $ JanusC $ pure $ RVal $ Var val1 noLoc

  writeRef :: forall a. (JanusTyped (JanusCT t) a) => Ref (JanusCMT t) (JanusCT t) a -> JanusCT t a -> JanusCMT t ()
  writeRef (JanusCRef (LVal ref)) (JanusC a) = do
    RVal a' <- liftJanusCM a
    modifyFunction $ \fn ->
      let c = fn ^. jcfVarCounter
          ref1 = BlockStm $ Exp (Just $ Assign ref JustAssign a' noLoc) noLoc
       in fn
            & jcfVarCounter .~ (c + 1)
            & jcfBlock .~ appendBlock (fn ^. jcfBlock) [ref1]

-- | Generalized mutable local cell supporting scalar @e a@ expressions, unit,
-- tuples, and composite HKD values across all Janus backends.
data Cell m a = Cell
  { readCell :: !(m a),
    writeCell :: !(a -> m ())
  }

class (CmdRef m e) => RefCell (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a | m -> e, e -> m where
  newCell :: a -> m (Cell m a)
  newCell' :: m (Cell m a)

instance (CmdRef m e) => RefCell m e () where
  newCell _ = pure (Cell (pure ()) (const (pure ())))
  newCell' = pure (Cell (pure ()) (const (pure ())))

instance RefCell IO Identity (Identity a) where
  newCell x = do
    r <- newRef x
    pure (Cell (readRef r) (writeRef r))
  newCell' = do
    r <- newRef'
    pure (Cell (readRef r) (writeRef r))

instance (JanusCTyped a) => RefCell (JanusCMT t) (JanusCT t) (JanusCT t a) where
  newCell x = do
    r <- newRef x
    pure (Cell (readRef r) (writeRef r))
  newCell' = do
    r <- newRef'
    pure (Cell (readRef r) (writeRef r))

instance (CmdRef m e, RefCell m e a, RefCell m e b) => RefCell m e (a, b) where
  newCell (a, b) = do
    ca <- newCell a
    cb <- newCell b
    pure (Cell ((,) <$> readCell ca <*> readCell cb) (\(a', b') -> writeCell ca a' >> writeCell cb b'))
  newCell' = do
    ca <- newCell'
    cb <- newCell'
    pure (Cell ((,) <$> readCell ca <*> readCell cb) (\(a', b') -> writeCell ca a' >> writeCell cb b'))

instance (CmdRef m e, RefCell m e a, RefCell m e b, RefCell m e c) => RefCell m e (a, b, c) where
  newCell (a, b, c) = do
    ca <- newCell a
    cb <- newCell b
    cc <- newCell c
    pure (Cell ((,,) <$> readCell ca <*> readCell cb <*> readCell cc) (\(a', b', c') -> writeCell ca a' >> writeCell cb b' >> writeCell cc c'))
  newCell' = do
    ca <- newCell'
    cb <- newCell'
    cc <- newCell'
    pure (Cell ((,,) <$> readCell ca <*> readCell cb <*> readCell cc) (\(a', b', c') -> writeCell ca a' >> writeCell cb b' >> writeCell cc c'))

modifyCell :: (Monad m) => Cell m a -> (a -> a) -> m ()
modifyCell c f = readCell c >>= writeCell c . f
