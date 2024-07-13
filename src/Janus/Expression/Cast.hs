{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -lcuda -lnvJitLink #-}

module Janus.Expression.Cast where

import Control.Arrow ((***))
import Data.Functor.Identity
import Data.Loc
import qualified Data.Set as Set
import Data.Typeable
import Foreign.Ptr
import Janus.Backend.C
import Janus.Typed
import Language.C.Quote
import Language.Haskell.TH

class ExpBoolCast e a where
  fromBool :: e Bool -> e a
  toBool :: e a -> e Bool

instance (Num a, Eq a) => ExpBoolCast Identity a where
  fromBool (Identity a) = Identity (fromInteger . toInteger $ fromEnum a)
  toBool (Identity a) = Identity (a /= 0)

janusCUnsafeCast :: forall a b. (JanusCTyped b) => JanusC a -> JanusC b
janusCUnsafeCast (JanusC a) = JanusC $ do
  RVal a' <- a
  JCType spec dec <- getJanusCType (Proxy @b)
  pure $ RVal $ Cast (Type spec dec noLoc) a' noLoc

janusCReallyUnsafeCast :: JanusC a -> JanusC b
janusCReallyUnsafeCast (JanusC a) = JanusC $ do
  RVal a' <- a
  pure $ RVal a'

$( let inst t =
         [d|
           instance ExpBoolCast JanusC $t where
             fromBool = janusCUnsafeCast
             toBool = janusCUnsafeCast
           |]
       tys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes, janusFloatTypes])
    in traverse (fmap head . inst . pure . ConT) tys
 )

class ExpFloatingCast e a b where
  toFloating :: e a -> e b

$( let inst ta tb =
         [d|
           instance ExpFloatingCast JanusC $ta $tb where
             toFloating = janusCUnsafeCast
           |]
       sourcetys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes, janusFloatTypes])
       ftys = Set.toList janusFloatTypes
       tys = [(s, f) | s <- sourcetys, f <- ftys]
       mkTy = pure . ConT
    in traverse (fmap head . uncurry inst . (mkTy *** mkTy)) tys
 )

class ExpIntegralCast e a b where
  toIntegral :: e a -> e b

instance (Integral a, Num b) => ExpIntegralCast Identity a b where
  toIntegral (Identity a) = Identity (fromIntegral a)

$( let inst ta tb =
         [d|
           instance ExpIntegralCast JanusC $ta $tb where
             toIntegral = janusCUnsafeCast
           |]
       sourcetys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes, janusFloatTypes])
       itys = Set.toList (Set.union janusSignedIntTypes janusUnsignedIntTypes)
       tys = [(s, i) | s <- sourcetys, i <- itys]
       mkTy = pure . ConT
    in traverse (fmap head . uncurry inst . (mkTy *** mkTy)) tys
 )

-- to/fromVoidPtr are required to avoid overlapping instances for JanusCTyped (Ptr ())/JanusCTyped (Ptr a)
-- this also stops you from peeking/poking a void pointer
class ExpPtrCast e where
  ptrCast :: (JanusTyped e a, JanusTyped e b) => e (Ptr a) -> e (Ptr b)
  toVoidPtr :: JanusTyped e a => e (Ptr a) -> e (Ptr ())
  fromVoidPtr :: JanusTyped e a => e (Ptr ()) -> e (Ptr a)

instance ExpPtrCast Identity where
  ptrCast = fmap castPtr
  toVoidPtr = fmap castPtr
  fromVoidPtr = fmap castPtr

instance ExpPtrCast JanusC where
  ptrCast ::
    forall a b.
    (JanusTyped JanusC b) =>
    JanusC (Ptr a) ->
    JanusC (Ptr b)
  ptrCast (JanusC a) = JanusC $ do
    RVal a' <- a
    JCType spec dec <- jctype (Proxy @(Ptr b))
    pure $ RVal $ Cast (Type spec dec noLoc) a' noLoc
  toVoidPtr (JanusC a) = JanusC $ do
    RVal a' <- a
    let JCType spec dec = voidPtrType
    pure $ RVal $ Cast (Type spec dec noLoc) a' noLoc
  fromVoidPtr :: forall a. JanusTyped JanusC a => JanusC (Ptr ()) -> JanusC (Ptr a)
  fromVoidPtr (JanusC a) = JanusC $ do
    RVal a' <- a
    JCType spec dec <- getJanusCType (Proxy @a)
    pure $ RVal $ Cast (Type spec (Ptr [] dec noLoc) noLoc) a' noLoc
