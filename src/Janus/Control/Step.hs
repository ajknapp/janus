{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Janus.Control.Step where

import GHC.Generics

data Step a = Yield a | Skip | Stop
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Applicative Step where
  pure = Yield
  Yield f <*> Yield x = Yield (f x)
  Stop <*> _ = Stop
  _ <*> Stop = Stop
  Skip <*> _ = Skip
  _ <*> Skip = Skip
