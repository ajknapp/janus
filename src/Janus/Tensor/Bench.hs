{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Integration of @Janus.Tensor@ top-@K@ contraction planning with
-- "Janus.Bench" for empirical benchmarking, CSV reporting, selection
-- persistence ('saveSelection', 'loadSelection', 'loadSelectionTH'), and
-- zero-overhead production deployment ('runProduction').
module Janus.Tensor.Bench
  ( -- * Kernel Tile & Execution Tuning Configuration
    TileConfig (..),
    defaultTileConfigs,

    -- * Higher-Kinded Choice Record (`TensorTuneChoices`)
    TensorTuneChoices (..),
    tcContractionPlan,
    tcTileConfig,
    tcCpuParallel,
    tcVectorize,

    -- * Top-K Contraction Candidate Generation for `Janus.Bench`
    topKContractionChoices,
    topKSimpleChoiceSpace,
    tensorChoiceSpace,
    defaultTensorChoiceSpace,

    -- * Re-exported `Janus.Bench` Persistence & Production API
    ChoiceSpace,
    ChoiceSelection (..),
    SelectionSpec,
    BenchConfig (..),
    defaultBenchConfig,
    saveSelection,
    loadSelection,
    loadSelectionSpec,
    loadSelectionTH,
    resolveSelection,
    resolveSelectionOrFail,
    extractSelectionSpec,
    runProduction,
    choose,
    chooseDescription,
    simpleImpl,
  )
where

import Control.Lens (Lens')
import Data.Functor.Const (Const (..))
import Data.HKD
  ( FFoldable (..),
    FFunctor (..),
    FTraversable (..),
    FZip (..),
  )
import qualified Data.Kind as K
import GHC.Generics (Generic)
import Janus.Bench
  ( BenchConfig (..),
    Candidates (..),
    ChoiceColumns (..),
    ChoiceSelection (..),
    ChoiceSpace,
    SelectionSpec,
    SimpleChoice (..),
    candidates,
    choose,
    chooseDescription,
    defaultBenchConfig,
    extractSelectionSpec,
    implWithId,
    loadSelection,
    loadSelectionSpec,
    loadSelectionTH,
    resolveSelection,
    resolveSelectionOrFail,
    runProduction,
    saveSelection,
    simpleImpl,
  )
import Janus.Tensor.Contract.Hypergraph (ContractionTree, Hypergraph)
import Janus.Tensor.Contract.Plan (PlanCandidate (..), PlanConfig (..), planTopK)

--------------------------------------------------------------------------------
-- 1. Tile Configuration & HKD Choice Record (`TensorTuneChoices`)
--------------------------------------------------------------------------------

data TileConfig = TileConfig
  { tileM :: !Int,
    tileN :: !Int,
    tileK :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)

defaultTileConfigs :: [TileConfig]
defaultTileConfigs =
  [ TileConfig 16 16 16,
    TileConfig 32 32 16,
    TileConfig 64 64 16
  ]

-- | Higher-kinded choice record plugging directly into "Janus.Bench"
-- ('benchmarkAndSelectCWithSetup', 'benchmarkAndSelectIOWithSetup',
-- 'saveSelection', 'loadSelectionTH', 'runProduction').
data TensorTuneChoices (f :: K.Type -> K.Type) = TensorTuneChoices
  { _tcContractionPlan :: !(f ContractionTree),
    _tcTileConfig :: !(f TileConfig),
    _tcCpuParallel :: !(f Bool),
    _tcVectorize :: !(f Bool)
  }
  deriving stock (Generic)

deriving stock instance
  (Eq (f ContractionTree), Eq (f TileConfig), Eq (f Bool)) =>
  Eq (TensorTuneChoices f)

deriving stock instance
  (Ord (f ContractionTree), Ord (f TileConfig), Ord (f Bool)) =>
  Ord (TensorTuneChoices f)

deriving stock instance
  (Show (f ContractionTree), Show (f TileConfig), Show (f Bool)) =>
  Show (TensorTuneChoices f)

tcContractionPlan :: Lens' (TensorTuneChoices f) (f ContractionTree)
tcContractionPlan k s = (\x -> s {_tcContractionPlan = x}) <$> k (_tcContractionPlan s)

tcTileConfig :: Lens' (TensorTuneChoices f) (f TileConfig)
tcTileConfig k s = (\x -> s {_tcTileConfig = x}) <$> k (_tcTileConfig s)

tcCpuParallel :: Lens' (TensorTuneChoices f) (f Bool)
tcCpuParallel k s = (\x -> s {_tcCpuParallel = x}) <$> k (_tcCpuParallel s)

tcVectorize :: Lens' (TensorTuneChoices f) (f Bool)
tcVectorize k s = (\x -> s {_tcVectorize = x}) <$> k (_tcVectorize s)

instance FFunctor TensorTuneChoices where
  ffmap f (TensorTuneChoices p t par vec) =
    TensorTuneChoices (f p) (f t) (f par) (f vec)

instance FFoldable TensorTuneChoices where
  ffoldMap f (TensorTuneChoices p t par vec) =
    f p <> f t <> f par <> f vec

instance FTraversable TensorTuneChoices where
  ftraverse f (TensorTuneChoices p t par vec) =
    TensorTuneChoices <$> f p <*> f t <*> f par <*> f vec

instance FZip TensorTuneChoices where
  fzipWith f (TensorTuneChoices p1 t1 par1 vec1) (TensorTuneChoices p2 t2 par2 vec2) =
    TensorTuneChoices (f p1 p2) (f t1 t2) (f par1 par2) (f vec1 vec2)

instance ChoiceColumns TensorTuneChoices where
  choiceColumns =
    TensorTuneChoices
      { _tcContractionPlan = Const "contraction_plan",
        _tcTileConfig = Const "tile_config",
        _tcCpuParallel = Const "cpu_parallel",
        _tcVectorize = Const "vectorize"
      }

--------------------------------------------------------------------------------
-- 2. Top-K Contraction Candidate Generators for `Janus.Bench`
--------------------------------------------------------------------------------

-- | Convert the top-@K@ contraction trees produced by 'planTopK' into a
-- "Janus.Bench" 'Candidates' list with stable programmatic IDs and readable
-- parenthesization/FLOP descriptions.
topKContractionChoices :: PlanConfig -> Hypergraph -> Candidates ContractionTree
topKContractionChoices cfg hg =
  candidates
    [ implWithId (pcId c) (pcDescription c) (pcTree c)
      | c <- planTopK cfg hg
    ]

-- | Single-dimension 'ChoiceSpace' containing just the top-@K@ contraction
-- trees (for fast contraction-order-only empirical selection).
topKSimpleChoiceSpace :: PlanConfig -> Hypergraph -> ChoiceSpace (SimpleChoice ContractionTree)
topKSimpleChoiceSpace cfg hg =
  SimpleChoice (topKContractionChoices cfg hg)

-- | Full multi-dimensional tensor tuning 'ChoiceSpace' combining the top-@K@
-- contraction trees with tile sizes, Argobots CPU parallelism, and SIMD
-- vectorization flags.
tensorChoiceSpace ::
  PlanConfig ->
  Hypergraph ->
  [TileConfig] ->
  [Bool] ->
  [Bool] ->
  ChoiceSpace TensorTuneChoices
tensorChoiceSpace cfg hg tiles parFlags vecFlags =
  TensorTuneChoices
    { _tcContractionPlan = topKContractionChoices cfg hg,
      _tcTileConfig =
        candidates
          [ implWithId
              ("tile_" <> show (tileM t) <> "x" <> show (tileN t) <> "x" <> show (tileK t))
              ("tile(" <> show (tileM t) <> "," <> show (tileN t) <> "," <> show (tileK t) <> ")")
              t
            | t <- tiles
          ],
      _tcCpuParallel =
        candidates
          [ implWithId (if b then "par_argobots" else "serial") (if b then "Argobots parallel" else "Serial") b
            | b <- parFlags
          ],
      _tcVectorize =
        candidates
          [ implWithId (if b then "simd_on" else "simd_off") (if b then "SIMD enabled" else "Scalar") b
            | b <- vecFlags
          ]
    }

defaultTensorChoiceSpace :: PlanConfig -> Hypergraph -> ChoiceSpace TensorTuneChoices
defaultTensorChoiceSpace cfg hg =
  tensorChoiceSpace cfg hg defaultTileConfigs [True, False] [True, False]
