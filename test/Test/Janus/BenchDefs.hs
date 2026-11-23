{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Janus.BenchDefs
  ( AlgoStrategy (..),
    UnrollMode (..),
    KernelChoice (..),
  )
where

import Janus.Bench (deriveChoiceKey)

data AlgoStrategy
  = AlgoFastSPMD
  | AlgoSlowRedundant
  deriving (Eq, Ord, Show)

data UnrollMode
  = UnrollWide4
  | UnrollScalar1
  deriving (Eq, Ord, Show)

data KernelChoice a where
  AlgoPiece :: KernelChoice AlgoStrategy
  UnrollPiece :: KernelChoice UnrollMode

deriveChoiceKey ''KernelChoice
