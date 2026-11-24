-- | Polymorphic Janus benchmarking monad transformer ('BenchT' / 'BenchM'),
-- expression wrapper ('Bench'), higher-kinded choice records ('HasChoices',
-- 'HasChoiceDescriptions', 'choose', 'chooseDescription', 'ChoiceColumns',
-- 'ChoiceSpace', 'ChoiceSelection'), named @tic@/@toc@ timers, pure-Janus
-- Bayesian statistical engine, ascending CSV report generation, and
-- zero-overhead library deployment helpers.
--
-- All EDSL typeclasses on @('BenchT' c m e, 'Bench' c e)@ are derived from the
-- general @('Control.Monad.Reader.ReaderT', 'Janus.Control.Reader.ReaderExp')@
-- instances in "Janus.Control.Reader", and 'choose' / 'chooseDescription' are
-- plain functions built on 'Control.Monad.Reader.ask' and lenses.
module Janus.Bench
  ( -- * Benchmarking Monad Transformer & Expression Wrapper
    Bench (..),
    getBench,
    liftBench,
    BenchRef (..),
    getBenchRef,
    BenchEnv (..),
    BenchT (..),
    BenchM,
    runBenchT,
    liftBenchM,
    MonadReader (..),

    -- * Higher-Kinded Choice Combinators, Lenses, & Spaces
    HasChoices (..),
    HasChoiceDescriptions (..),
    choose,
    chooseDescription,
    Impl (..),
    impl,
    implWithId,
    Candidates (..),
    candidates,
    ChoiceColumns (..),
    ChoiceSpace,
    ChoiceSelection (..),
    fromChoiceImpl,
    NoChoice (..),
    SimpleChoice (..),
    simpleImpl,
    elementL,
    emptyChoiceSpace,
    emptyChoiceSelection,
    simpleChoiceSpace,
    enumerateChoiceImpls,
    enumerateChoices,
    choiceHeaderColumns,
    choiceDescriptionFields,

    -- * Re-exported "Data.HKD" & Functor Wrappers
    Identity (..),
    Const (..),
    Element (..),
    FFunctor (..),
    FFoldable (..),
    FTraversable (..),
    FZip (..),
    FRepeat (..),
    ffmapDefault,
    ffoldMapDefault,
    gftraverse,
    gfzipWith,
    gfrepeat,

    -- * Named @tic@ / @toc@ Timers
    CmdNamedTimer,
    namedTic,
    namedToc,
    withNamedTimer,
    sanitizeTimerFileStem,

    -- * Zero-Overhead Library Execution & Persistence
    SelectionSpec,
    extractSelectionSpec,
    resolveSelection,
    resolveSelectionOrFail,
    saveSelection,
    loadSelectionSpec,
    loadSelection,
    loadSelectionTH,
    runWithDescriptions,
    runWithChoices,
    runProduction,

    -- * Pure-Janus Statistical Engine & Configuration
    BenchConfig (..),
    defaultBenchConfig,
    TimingEstimate (..),
    CmdBenchStats,
    nextSplitMix64,
    nextUniform01,
    calibrateTimerOverhead,
    calibrateBatchSize,
    estimateTimingPosterior,

    -- * Pure-Janus, Compiled-C, & IO Benchmark Runners
    CmdBenchRunner,
    benchmarkChoicesJanusWithSetup,
    benchmarkChoicesJanus,
    benchmarkCandidatesJanus,
    benchmarkSimpleJanus,
    benchmarkAndSelectCWithSetup,
    benchmarkAndSelectCWith,
    benchmarkAndSelectC,
    benchmarkCandidatesC,
    benchmarkSimpleC,
    benchmarkAndSelectIOWithSetup,
    benchmarkAndSelectIO,
    benchmarkCandidatesIO,
    benchmarkSimpleIO,

    -- * Scoped Buffer Management (Re-exported from "Janus.Data.Layout")
    ScopedBuf (..),
    withBuf,
    scopedBuf,
    scopedBufCap,
    readScopedBuf,
    writeScopedBuf,
    reallocScopedBuf,

    -- * Re-exported Associated SIMD Wrappers
    SIMDVector (..),
    SIMDMask (..),
  )
where

import Control.Monad.Reader (MonadReader (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.HKD
  ( Element (..),
    FFoldable (..),
    FFunctor (..),
    FRepeat (..),
    FTraversable (..),
    FZip (..),
    ffmapDefault,
    ffoldMapDefault,
    gfrepeat,
    gftraverse,
    gfzipWith,
  )
import Janus.Bench.Choice
import Janus.Bench.Gang ()
import Janus.Bench.Instances
import Janus.Bench.Runner
import Janus.Bench.Stats
import Janus.Bench.Types
import Janus.Data.Layout
  ( ScopedBuf (..),
    readScopedBuf,
    reallocScopedBuf,
    scopedBuf,
    scopedBufCap,
    withBuf,
    writeScopedBuf,
  )
