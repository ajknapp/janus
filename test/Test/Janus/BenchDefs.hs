{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Janus.BenchDefs
  ( AlgoStrategy (..),
    UnrollMode (..),
    KernelChoice (..),
    algoPiece,
    unrollPiece,
  )
where

import Control.Lens (makeLenses)
import qualified Data.Kind as K
import GHC.Generics (Generic)
import Janus.Bench
  ( ChoiceColumns (..),
    Const (..),
    FFoldable (..),
    FFunctor (..),
    FTraversable (..),
    FZip (..),
    Identity (..),
    ffmapDefault,
    ffoldMapDefault,
    gftraverse,
    gfzipWith,
  )

data AlgoStrategy
  = AlgoFastSPMD
  | AlgoSlowRedundant
  deriving (Eq, Ord, Show)

data UnrollMode
  = UnrollWide4
  | UnrollScalar1
  deriving (Eq, Ord, Show)

data KernelChoice (f :: K.Type -> K.Type) = KernelChoice
  { _algoPiece :: f AlgoStrategy,
    _unrollPiece :: f UnrollMode
  }
  deriving (Generic)

deriving instance Eq (KernelChoice Identity)

deriving instance Show (KernelChoice Identity)

deriving instance Eq (KernelChoice (Const String))

deriving instance Show (KernelChoice (Const String))

makeLenses ''KernelChoice

instance FFunctor KernelChoice where
  ffmap = ffmapDefault

instance FFoldable KernelChoice where
  ffoldMap = ffoldMapDefault

instance FTraversable KernelChoice where
  ftraverse = gftraverse

instance FZip KernelChoice where
  fzipWith = gfzipWith

instance ChoiceColumns KernelChoice where
  choiceColumns =
    KernelChoice
      { _algoPiece = Const "algo_piece",
        _unrollPiece = Const "unroll_piece"
      }
