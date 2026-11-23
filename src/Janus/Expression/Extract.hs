{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Janus.Expression.Extract where

import Data.Functor.Identity
import Data.Loc
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Janus.Expression.Cast
import Language.C.Quote

class ExpExtract e a where
  extract :: e a -> a

instance ExpExtract Identity a where
  extract (Identity x) = x

class GJanusCExtract t f where
  gjanusCExtract :: JanusCT t (f a) -> String -> f a

instance (ExpExtract (JanusCT t) f) => GJanusCExtract t (K1 i f) where
  gjanusCExtract (JanusC a) s = K1 (extract f)
    where
      f :: JanusCT t f
      f = JanusC $ do
        RVal a' <- a
        pure $ RVal $ Member a' (Id s noLoc) noLoc

instance {-# OVERLAPS #-} GJanusCExtract t (K1 i (JanusCT t a)) where
  gjanusCExtract (JanusC a) name = K1 f
    where
      f = JanusC $ do
        RVal a' <- a
        pure $ RVal $ Member a' (Id name noLoc) noLoc

instance (GJanusCExtract t f, GJanusCExtract t g) => GJanusCExtract t (f :*: g) where
  gjanusCExtract fg s = gjanusCExtract (janusCReallyUnsafeCast fg) s :*: gjanusCExtract (janusCReallyUnsafeCast fg) s

instance (GJanusCExtract t f, KnownSymbol name) => GJanusCExtract t (S1 ('MetaSel ('Just name) p s l) f) where
  gjanusCExtract f _ = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) (symbolVal $ Proxy @name)

instance (GJanusCExtract t f) => GJanusCExtract t (C1 i f) where
  gjanusCExtract f s = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) s

instance (GJanusCExtract t f) => GJanusCExtract t (D1 i f) where
  gjanusCExtract f s = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) s

defaultJanusCExtract :: forall t a. (Generic a, GJanusCExtract t (Rep a)) => JanusCT t a -> a
defaultJanusCExtract f = to $ gjanusCExtract (janusCReallyUnsafeCast f :: JanusCT t (Rep a ())) ""
