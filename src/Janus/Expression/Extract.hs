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

class GJanusCExtract f where
  gjanusCExtract :: JanusC (f a) -> String -> f a

instance (ExpExtract JanusC f) => GJanusCExtract (K1 i f) where
  gjanusCExtract (JanusC a) s = K1 (extract f)
    where
      f = JanusC $ do
        RVal a' <- a
        pure $ RVal $ Member a' (Id s noLoc) noLoc

instance {-# OVERLAPS #-} GJanusCExtract (K1 i (JanusC a)) where
  gjanusCExtract (JanusC a) name = K1 f
    where
      f = JanusC $ do
        RVal a' <- a
        pure $ RVal $ Member a' (Id name noLoc) noLoc

instance (GJanusCExtract f, GJanusCExtract g) => GJanusCExtract (f :*: g) where
  gjanusCExtract fg s = gjanusCExtract (janusCReallyUnsafeCast fg) s :*: gjanusCExtract (janusCReallyUnsafeCast fg) s

instance (GJanusCExtract f, KnownSymbol name) => GJanusCExtract (S1 ('MetaSel ('Just name) p s l) f) where
  gjanusCExtract f _ = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) (symbolVal $ Proxy @name)

instance (GJanusCExtract f) => GJanusCExtract (C1 i f) where
  gjanusCExtract f s = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) s

instance (GJanusCExtract f) => GJanusCExtract (D1 i f) where
  gjanusCExtract f s = M1 $ gjanusCExtract (janusCReallyUnsafeCast f) s

defaultJanusCExtract :: forall a. (Generic a, GJanusCExtract (Rep a)) => JanusC a -> a
defaultJanusCExtract f = to $ gjanusCExtract (janusCReallyUnsafeCast f :: JanusC (Rep a ())) ""
