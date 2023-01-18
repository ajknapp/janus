{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Janus.Expression.Integral where

import Data.Functor.Identity
import qualified Data.Set as Set
import Janus.Backend.C
import Janus.Typed
import Language.C.Quote
import Language.Haskell.TH

class (Num (e a)) => ExpIntegral e a where
  quotient :: e a -> e a -> e a
  remainder :: e a -> e a -> e a

infixl 7 `quotient`, `remainder`

instance (Integral a) => ExpIntegral Identity a where
  quotient = quot
  remainder = rem

$( let inst t =
         [d|
           instance ExpIntegral JanusC $t where
             quotient = janusCBinOp Div
             remainder = janusCBinOp Mod
           |]
       tys = Set.toList (Set.union janusSignedIntTypes janusUnsignedIntTypes)
    in traverse (fmap head . inst . pure . ConT) tys
 )
