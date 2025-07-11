{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Expression.Inject where

import Control.Lens
import Data.Loc
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Language.C.Quote

class ExpInject e a where
  inject :: a -> e a

instance ExpInject Identity a where
  inject = pure

instance (JanusLitC a) => ExpInject JanusC a where
  inject a = JanusC $ do
    a' <- jlitc @a a
    pure $ RVal a'

class GJanusCInject f where
  gjcinject :: f a -> String -> JanusCM [(Maybe Designation, Initializer)]

instance (JanusCTyped f, Generic f, GJanusCInject (Rep f)) => GJanusCInject (K1 i f) where
  gjcinject (K1 f) s = do
    ini <- gjcinject (GHC.Generics.from f) s
    JCType spec dec <- getJanusCType (Proxy @f)
    let e = CompoundLit (Type spec dec noLoc) ini noLoc
    pure [(Just (Designation [MemberDesignator (Id s noLoc) noLoc] noLoc), ExpInitializer e noLoc)]

instance {-# OVERLAPS #-} GJanusCInject (K1 i (JanusC a)) where
  gjcinject (K1 (JanusC a)) s = do
    RVal a' <- a
    pure [(Just (Designation [MemberDesignator (Id s noLoc) noLoc] noLoc), ExpInitializer a' noLoc)]

instance (GJanusCInject f, GJanusCInject g) => GJanusCInject (f :*: g) where
  gjcinject (f :*: g) s = (<>) <$> gjcinject f s <*> gjcinject g s

instance (GJanusCInject f, KnownSymbol name) => GJanusCInject (S1 ('MetaSel ('Just name) p s l) f) where
  gjcinject (M1 a) _ = gjcinject a (symbolVal (Proxy @name))

instance (GJanusCInject f) => GJanusCInject (C1 i f) where
  gjcinject (M1 a) = gjcinject a

instance (GJanusCInject f) => GJanusCInject (D1 i f) where
  gjcinject (M1 a) = gjcinject a

class GJanusCFields f where
  gjcfields :: Proxy (f a) -> String -> JanusCM [FieldGroup]

instance {-# OVERLAPS #-} (JanusCTyped f) => GJanusCFields (K1 i (JanusC f)) where
  gjcfields _ s = do
    JCType spec _ <- getJanusCType (Proxy @f)
    pure [FieldGroup spec [Field (Just $ Id s noLoc) (Just $ DeclRoot noLoc) Nothing noLoc] noLoc]

instance (JanusCTyped f) => GJanusCFields (K1 i f) where
  gjcfields _ s = do
    JCType spec _ <- getJanusCType (Proxy @f)
    pure [FieldGroup spec [Field (Just $ Id s noLoc) (Just $ DeclRoot noLoc) Nothing noLoc] noLoc]

instance (GJanusCFields f, KnownSymbol name) => GJanusCFields (S1 ('MetaSel ('Just name) p s l) f) where
  gjcfields :: forall a. Proxy (S1 ('MetaSel ('Just name) p s l) f a) -> String -> JanusCM [FieldGroup]
  gjcfields _ _ = gjcfields (Proxy @(f a)) (symbolVal (Proxy @name))

instance (GJanusCFields f) => GJanusCFields (C1 i f) where
  gjcfields :: forall a. Proxy (C1 i f a) -> String -> JanusCM [FieldGroup]
  gjcfields _ = gjcfields (Proxy @(f a))

instance (GJanusCFields f) => GJanusCFields (D1 i f) where
  gjcfields :: forall a. Proxy (D1 i f a) -> String -> JanusCM [FieldGroup]
  gjcfields _ = gjcfields (Proxy @(f a))

instance (GJanusCFields f, GJanusCFields g) => GJanusCFields (f :*: g) where
  gjcfields :: forall a. Proxy ((f :*: g) a) -> String -> JanusCM [FieldGroup]
  gjcfields _ s = (<>) <$> gjcfields (Proxy @(f a)) s <*> gjcfields (Proxy @(g a)) s

defaultJanusCType :: forall f. (GJanusCFields (Rep f)) => Proxy f -> JanusCM JCType
defaultJanusCType _ = do
  fgs <- gjcfields (Proxy @(Rep f ())) ""
  pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)

defaultJanusCInject :: forall f. (Generic f, GJanusCInject (Rep f), JanusCTyped f) => f -> JanusC f
defaultJanusCInject a = JanusC $ do
  ini <- gjcinject (GHC.Generics.from a) ""
  JCType spec dec <- getJanusCType (Proxy @f)
  pure $ RVal $ CompoundLit (Type spec dec noLoc) ini noLoc

