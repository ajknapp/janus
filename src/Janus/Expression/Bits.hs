{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Janus.Expression.Bits where

import Data.Bits (Bits (..))
import Data.Functor.Identity
import qualified Data.Set as Set
import Janus.Backend.C
import Janus.Expression.Eq
import Janus.Typed
import Language.C.Quote
import Language.Haskell.TH

class (ExpEq e a) => ExpBits e a where
  iand :: e a -> e a -> e a
  ior :: e a -> e a -> e a
  ieor :: e a -> e a -> e a
  inot :: e a -> e a
  lshift :: e a -> e a -> e a
  rshift :: e a -> e a -> e a

infixl 8 `lshift`, `rshift`

infixl 7 `iand`, `ior`

infixl 6 `ieor`

instance (Bits a, Integral a) => ExpBits Identity a where
  iand a b = a .&. b
  ior a b = a .|. b
  ieor = xor
  inot = complement
  lshift a i = shiftL a (fromIntegral $ runIdentity i)
  rshift a i = shiftR a (fromIntegral $ runIdentity i)

$( let inst t =
         [d|
           instance ExpBits JanusC $t where
             iand = janusCBinOp And
             ior = janusCBinOp Or
             ieor = janusCBinOp Xor
             inot = janusCUnOp Not
             lshift = janusCBinOp Lsh
             rshift = janusCBinOp Rsh
           |]
       tys = Set.toList (Set.union janusSignedIntTypes janusUnsignedIntTypes)
    in traverse (fmap head . inst . pure . ConT) tys
 )
