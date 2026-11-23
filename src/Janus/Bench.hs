-- | Polymorphic Janus benchmarking monad transformer ('BenchT' / 'BenchM'),
-- expression wrapper ('Bench'), implementation choice framework ('HasChoices',
-- 'choose', 'ChoiceKey', 'ChoiceSpace', 'ChoiceSelection'), named @tic@/@toc@
-- timers, pure-Janus Bayesian statistical engine, ascending CSV report
-- generation, and zero-overhead library deployment helpers.
--
-- All EDSL typeclasses on @('BenchT' c m e, 'Bench' c e)@ are derived from the
-- general @('Control.Monad.Reader.ReaderT', 'Janus.Control.Reader.ReaderExp')@
-- instances in "Janus.Control.Reader", and 'choose' is a function built on
-- 'Control.Monad.Reader.ask'.
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

    -- * Implementation Choice Combinators & Choice Spaces
    HasChoices (..),
    choose,
    askChoiceImpl,
    Impl (..),
    impl,
    implWithId,
    Candidates (..),
    candidates,
    DSum ((:=>)),
    Some (..),
    GEq (..),
    Hashable (..),
    ChoiceKey (..),
    ChoiceSelection,
    ChoiceSpace,
    choiceSpace,
    choiceSelection,
    emptyChoiceSpace,
    emptyChoiceSelection,
    simpleChoiceSpace,
    NoChoice,
    SimpleChoice (..),
    enumerateChoices,
    choiceHeaderColumns,
    choiceDescriptionFields,
    camelToSnake,
    deriveChoiceKey,

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

    -- * Re-exported Associated SIMD Wrappers
    SIMDVector (..),
    SIMDMask (..),
  )
where

import Control.Monad.Reader (MonadReader (..))
import Data.GADT.Compare (GEq (..))
import Data.Hashable (Hashable (..))
import Data.Some (Some (..))
import Janus.Bench.Choice
import Janus.Bench.Gang ()
import Janus.Bench.Instances
import Janus.Bench.Runner
import Janus.Bench.Stats
import Janus.Bench.Types
