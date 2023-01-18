{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Janus.Expression.Eq where

import Data.Functor.Identity
import qualified Data.Set as Set
import Language.C.Quote
import Language.Haskell.TH

import Janus.Backend.C
import Janus.Typed

class ExpEq e a where
  eq :: e a -> e a -> e Bool
  ne :: e a -> e a -> e Bool

infix 4 `eq`, `ne`

instance Eq a => ExpEq Identity a where
  eq a b = (==) <$> a <*> b
  ne a b = (/=) <$> a <*> b

instance ExpEq JanusC Bool where
  eq = janusCBoolBinOp Eq
  ne = janusCBoolBinOp Ne

$( let inst t =
         [d|
           instance ExpEq JanusC $t where
             eq = janusCBoolBinOp Eq
             ne = janusCBoolBinOp Ne
           |]
       tys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes, janusFloatTypes])
    in traverse (fmap head . inst . pure . ConT) tys
 )
