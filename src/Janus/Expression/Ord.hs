{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Janus.Expression.Ord where

import Data.Functor.Identity
import qualified Data.Set as Set
import Language.C.Quote
import Language.Haskell.TH

import Janus.Backend.C
import Janus.Expression.Eq
import Janus.Expression.Cond (ExpCond(ifThenElse))
import Janus.Typed

class ExpEq e a => ExpOrd e a where
  lt :: e a -> e a -> e Bool
  le :: e a -> e a -> e Bool
  gt :: e a -> e a -> e Bool
  ge :: e a -> e a -> e Bool

infix 4 `lt`, `le`, `gt`, `ge`

instance Ord a => ExpOrd Identity a where
  lt a b = (<) <$> a <*> b
  le a b = (<=) <$> a <*> b
  gt a b = (>) <$> a <*> b
  ge a b = (>=) <$> a <*> b

$( let inst t =
         [d|
           instance ExpOrd JanusC $t where
             lt = janusCBoolBinOp Lt
             le = janusCBoolBinOp Le
             gt = janusCBoolBinOp Gt
             ge = janusCBoolBinOp Ge
           |]
       tys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes, janusFloatTypes])
    in traverse (fmap head . inst . pure . ConT) tys
 )

min' :: (ExpCond e, ExpOrd e a, JanusTyped e a) => e a -> e a -> e a
min' a b = ifThenElse (a `lt` b) a b

max' :: (ExpCond e, ExpOrd e a, JanusTyped e a) => e a -> e a -> e a
max' a b = ifThenElse (a `lt` b) b a
