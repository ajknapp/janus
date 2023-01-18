{-# LANGUAGE FlexibleInstances #-}

module Janus.Expression.Bool where

import Control.Lens
import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper
import Janus.Backend.C
import Language.C.Quote

class ExpBool e where
  false :: e Bool
  true :: e Bool
  land :: e Bool -> e Bool -> e Bool
  lor :: e Bool -> e Bool -> e Bool
  lnot :: e Bool -> e Bool

infixr 3 `land`

infixr 2 `lor`

instance ExpBool Identity where
  true = Identity True
  false = Identity False
  land (Identity a) (Identity b) = Identity $ a && b
  lor (Identity a) (Identity b) = Identity $ a || b
  lnot (Identity a) = Identity (not a)

instance ExpBool JanusC where
  false = lowerBound
  true = upperBound
  land = (/\)
  lor = (\/)
  lnot = janusCUnOp Lnot
