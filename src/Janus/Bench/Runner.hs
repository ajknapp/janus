{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure-Janus multi-implementation benchmark runner, ascending CSV report
-- generator, and compiled-C / @IO@ convenience entry points.
module Janus.Bench.Runner
  ( -- * Runner Constraint Bundle
    CmdBenchRunner,

    -- * Pure-Janus Benchmark Kernels (Zero Per-Call FFI)
    benchmarkChoicesJanusWithSetup,
    benchmarkChoicesJanus,
    benchmarkCandidatesJanus,
    benchmarkSimpleJanus,

    -- * Compiled-C Host Runners (Selects Fastest Choice & Emits CSVs)
    benchmarkAndSelectCWithSetup,
    benchmarkAndSelectCWith,
    benchmarkAndSelectC,
    benchmarkCandidatesC,
    benchmarkSimpleC,

    -- * IO Interpreter Runners
    benchmarkAndSelectIOWithSetup,
    benchmarkAndSelectIO,
    benchmarkCandidatesIO,
    benchmarkSimpleIO,
  )
where

import Control.Monad (unless, void)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Word (Word64, Word8)
import Foreign.C (CString)
import Foreign.Ptr (Ptr)
import Janus.Alloc.Policy (dynamicAlloc)
import Janus.Backend.C
  ( JanusCEval,
    JanusCEvaluate,
    JanusCMT,
    JanusCParam,
    JanusCT,
    JanusCTarget,
    withJanusC,
  )
import Janus.Backend.C.Build (CBuildConfig, defaultCBuildConfig)
import Janus.Bench.Choice
  ( SelectionSpec,
    choiceDescriptionFields,
    choiceHeaderColumns,
    emptyChoiceSpace,
    enumerateChoices,
    extractSelectionSpec,
    simpleChoiceSpace,
  )
import Janus.Bench.Instances ()
import Janus.Bench.Stats
  ( BenchConfig (..),
    CmdBenchStats,
    TimingEstimate (..),
    calibrateBatchSize,
    calibrateTimerOverhead,
    estimateTimingPosterior,
  )
import Janus.Bench.Types
  ( BenchEnv (..),
    BenchT,
    ChoiceColumns,
    ChoiceSelection (..),
    ChoiceSpace,
    Impl,
    NoChoice,
    SimpleChoice,
    TimerCollector (..),
    choose,
    resetTimerIterAccumulators,
    runBenchT,
    simpleImpl,
    withTimerCollector,
  )
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Format (CmdPutString (..), CmdString (..))
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..))
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (whileM)
import Janus.Control.CSV (CSVRow (..), csvEscapeString, csvField, drainCSVFileCString)
import Janus.Control.Drain (runDrain)
import Janus.Control.Stream (rangeS)
import Janus.Data.Layout (ScalarBuf (..), readBuf, scopedBuf, withBuf, writeBuf)
import Janus.Data.Sort (sortKeyValBufs)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpFloatingCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..), max')
import Janus.Typed (JanusTyped)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addTrailingPathSeparator)

--------------------------------------------------------------------------------
-- Runner Constraint Bundle
--------------------------------------------------------------------------------

-- | Constraint bundle required to execute benchmarks, run Bayesian bootstrap
-- inference, sort results, and stream CSV reports purely in Janus.
type CmdBenchRunner m e =
  ( CmdBenchStats m e,
    CmdPutString m e,
    ExpOrd e Word8,
    JanusTyped e Int64
  )

--------------------------------------------------------------------------------
-- Internal Pure-Janus Path Construction Helper
--------------------------------------------------------------------------------

withTimerCSVPathCString ::
  forall m e a.
  (CmdBenchRunner m e) =>
  FilePath ->
  e CString ->
  (e CString -> m a) ->
  m a
withTimerCSVPathCString outDir timerNameCStr k = do
  let dirPrefix = if null outDir then "" else addTrailingPathSeparator outDir
  withString dirPrefix $ \prefixCStr -> do
    let pref8 :: e (Ptr Word8)
        pref8 = ptrCast prefixCStr
        name8 :: e (Ptr Word8)
        name8 = ptrCast timerNameCStr
    prefLenRef <- newRef (0 :: e Int64)
    whileM true $ \cont -> do
      i <- readRef prefLenRef
      b <- peekElemOff pref8 i
      ifThenElseM_ (b `eq` 0) (writeRef cont false) (writeRef prefLenRef (i + 1))
    nameLenRef <- newRef (0 :: e Int64)
    whileM true $ \cont -> do
      i <- readRef nameLenRef
      b <- peekElemOff name8 i
      ifThenElseM_ (b `eq` 0) (writeRef cont false) (writeRef nameLenRef (i + 1))

    prefLen <- readRef prefLenRef
    nameLen <- readRef nameLenRef
    withBuf @Word8 dynamicAlloc (prefLen + nameLen + 6) $ \pathSB -> do
      ScalarBuf dst8 <- scopedBuf pathSB
      whenM_ (prefLen `gt` 0) $
        cmdMemcpy (toVoidPtr dst8) (toVoidPtr prefixCStr) prefLen
      rangeM 0 nameLen $ \i -> do
        b <- peekElemOff name8 i
        let isDigitB = (b `ge` 48) `land` (b `le` 57)
            isUpperB = (b `ge` 65) `land` (b `le` 90)
            isLowerB = (b `ge` 97) `land` (b `le` 122)
            isSafePunct = (b `eq` 95) `lor` (b `eq` 45)
            cleanB = ifThenElse (isDigitB `lor` isUpperB `lor` isLowerB `lor` isSafePunct) b 95
        pokeElemOff dst8 cleanB (prefLen + i)
      let extBase = prefLen + nameLen
      pokeElemOff dst8 46 extBase
      pokeElemOff dst8 99 (extBase + 1)
      pokeElemOff dst8 115 (extBase + 2)
      pokeElemOff dst8 118 (extBase + 3)
      pokeElemOff dst8 0 (extBase + 4)

      k (ptrCast dst8)

--------------------------------------------------------------------------------
-- Pure-Janus Multi-Implementation Benchmark Kernel
--------------------------------------------------------------------------------

-- | Benchmark every combination in a 'ChoiceSpace' purely inside Janus @(m, e)@,
-- supporting per-choice untimed @setup@ and @teardown@ actions.
--
-- Writes @<outDir>/overall.csv@ plus @<outDir>/<timer>.csv@ for every active
-- named @tic@/@toc@ pair, with rows sorted in **ascending** order of @timing@
-- (fastest implementation first), and returns the 0-based index of the fastest
-- implementation in @'enumerateChoices' space@.
benchmarkChoicesJanusWithSetup ::
  forall c m e res b.
  (ChoiceColumns c, CmdBenchRunner m e) =>
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c m e res ->
  (res -> BenchT c m e ()) ->
  (res -> BenchT c m e b) ->
  m (e Int64)
benchmarkChoicesJanusWithSetup cfg outDir space setup teardown benchBody = do
  let selections = enumerateChoices space
      numImpls = length selections
  if numImpls <= 0
    then error "Janus.Bench: ChoiceSpace produced zero implementation combinations"
    else withTimerCollector (benchMaxNamedTimers cfg) $ \tc -> do
      let maxSlots = tcMaxSlots tc
          maxSlotsE = fromIntegral maxSlots :: e Int64
          numImplsE = fromIntegral numImpls :: e Int64
          nSamples = max 4 (benchSamples cfg)
          nSamplesE = fromIntegral nSamples :: e Int64
          nWarmup = max 0 (benchWarmupIters cfg)
          nWarmupE = fromIntegral nWarmup :: e Int64
          totalCellsE = fromIntegral (numImpls * maxSlots) :: e Int64
          curSamplesCapE = fromIntegral (maxSlots * nSamples) :: e Int64
          choiceCols = choiceHeaderColumns @c
          hasChoiceCols = not (null choiceCols)
          headerCols = choiceCols <> ["timing", "ci_lower", "ci_upper"]
          rowPrefixes =
            [ intercalate "," (map csvEscapeString (choiceDescriptionFields (selDescriptions sel)))
            | sel <- selections
            ]

      rngRef <- newRef (fromIntegral (benchSeed cfg) :: e Word64)
      timerOverhead <- calibrateTimerOverhead cfg

      withBuf @Word64 dynamicAlloc 1 $ \startScratchSB ->
        withBuf @Double dynamicAlloc curSamplesCapE $ \curSamplesSB ->
          withBuf @Int64 dynamicAlloc maxSlotsE $ \slotSampleCountsSB ->
            withBuf @Double dynamicAlloc totalCellsE $ \estTimingsSB ->
              withBuf @Double dynamicAlloc totalCellsE $ \estLowersSB ->
                withBuf @Double dynamicAlloc totalCellsE $ \estUppersSB ->
                  withBuf @Int64 dynamicAlloc totalCellsE $ \slotRecordedSB ->
                    withBuf @Double dynamicAlloc numImplsE $ \sortKeysSB ->
                      withBuf @Int64 dynamicAlloc numImplsE $ \sortIdxsSB -> do
                        ScalarBuf startScratchPtr <- scopedBuf startScratchSB
                        curSamplesBuf@(ScalarBuf curSamplesPtr) <- scopedBuf curSamplesSB
                        slotSampleCounts <- scopedBuf slotSampleCountsSB
                        estTimings <- scopedBuf estTimingsSB
                        estLowers <- scopedBuf estLowersSB
                        estUppers <- scopedBuf estUppersSB
                        slotRecorded <- scopedBuf slotRecordedSB
                        sortKeys <- scopedBuf sortKeysSB
                        sortIdxs <- scopedBuf sortIdxsSB

                        rangeM 0 totalCellsE $ \idx -> do
                          writeBuf estTimings idx 0.0
                          writeBuf estLowers idx 0.0
                          writeBuf estUppers idx 0.0
                          writeBuf slotRecorded idx 0

                        -- Execute warmup, batched timing, and Bayesian bootstrap for each combination k
                        let runOneImpl :: Int -> ChoiceSelection c -> m ()
                            runOneImpl kIdx sel = do
                              let kE = fromIntegral kIdx :: e Int64
                                  env =
                                    BenchEnv
                                      { _benchChoices = selChoices sel,
                                        _benchDescriptions = selDescriptions sel,
                                        _benchTimers = Just tc
                                      }
                              res <- runBenchT env setup
                              let stepOnce = void (runBenchT env (benchBody res))

                              batchB <- calibrateBatchSize cfg stepOnce
                              batchBF <- letM (toFloating batchB :: e Double)

                              -- Warmup batches (populates named timer slots and warms caches/predictors)
                              rangeM 0 nWarmupE $ \_ -> do
                                resetTimerIterAccumulators tc
                                rangeM 0 batchB $ \_ -> stepOnce

                              rangeM 0 maxSlotsE $ \s ->
                                writeBuf slotSampleCounts s 0

                              -- Sample collection batches
                              rangeM 0 nSamplesE $ \sIdx -> do
                                resetTimerIterAccumulators tc
                                Tic t0 <- tic
                                poke startScratchPtr t0
                                rangeM 0 batchB $ \_ -> stepOnce
                                Toc t1 <- toc
                                t0Loaded <- peek startScratchPtr
                                let rawDelta = ifThenElse (t1 `ge` t0Loaded) (t1 - t0Loaded) 0
                                    netOverall = max' 0.0 (toFloating rawDelta - timerOverhead)
                                    perCallOverall = netOverall / batchBF
                                writeBuf curSamplesBuf sIdx perCallOverall
                                c0 <- readBuf slotSampleCounts 0
                                writeBuf slotSampleCounts 0 (c0 + 1)

                                numUsed <- peek (tcCountPtr tc)
                                rangeM 1 numUsed $ \slot -> do
                                  cnt <- peekElemOff (tcIterCounts tc) slot
                                  whenM_ (cnt `gt` 0) $ do
                                    del <- peekElemOff (tcIterDeltas tc) slot
                                    let netSlot =
                                          max'
                                            0.0
                                            (toFloating del - toFloating cnt * timerOverhead)
                                        perCallSlot = netSlot / batchBF
                                    curCnt <- readBuf slotSampleCounts slot
                                    writeBuf curSamplesBuf (slot * nSamplesE + curCnt) perCallSlot
                                    writeBuf slotSampleCounts slot (curCnt + 1)

                              -- Compute Bayesian posterior summaries for "overall" and active named timers
                              numUsedAfter <- peek (tcCountPtr tc)
                              rangeM 0 numUsedAfter $ \slot -> do
                                cnt <- readBuf slotSampleCounts slot
                                whenM_ (cnt `gt` 0) $ do
                                  let slotSubBuf = ScalarBuf (ptrIndex curSamplesPtr (slot * nSamplesE))
                                      cellIdx = kE * maxSlotsE + slot
                                  est <- estimateTimingPosterior cfg rngRef slotSubBuf cnt
                                  writeBuf estTimings cellIdx (estTiming est)
                                  writeBuf estLowers cellIdx (estCiLower est)
                                  writeBuf estUppers cellIdx (estCiUpper est)
                                  writeBuf slotRecorded cellIdx 1

                              runBenchT env (teardown res)

                        sequence_ [runOneImpl k sel | (k, sel) <- zip [0 ..] selections]

                        -- Sort active implementations ascending (fastest first) and emit CSV per timer
                        bestOverallIdxRef <- newRef (0 :: e Int64)

                        totalSlots <- peek (tcCountPtr tc)
                        rangeM 0 totalSlots $ \slot -> do
                          activeCountRef <- newRef (0 :: e Int64)
                          rangeM 0 numImplsE $ \kE -> do
                            let cellIdx = kE * maxSlotsE + slot
                            recFlag <- readBuf slotRecorded cellIdx
                            whenM_ (recFlag `eq` 1) $ do
                              ac <- readRef activeCountRef
                              tVal <- readBuf estTimings cellIdx
                              writeBuf sortKeys ac tVal
                              writeBuf sortIdxs ac kE
                              writeRef activeCountRef (ac + 1)

                          activeCount <- readRef activeCountRef
                          whenM_ (activeCount `gt` 0) $ do
                            sortKeyValBufs sortKeys sortIdxs activeCount
                            whenM_ (slot `eq` 0) $ do
                              fastest <- readBuf sortIdxs 0
                              writeRef bestOverallIdxRef fastest

                            timerNameCStr <- peekElemOff (tcSlotNames tc) slot
                            withTimerCSVPathCString outDir timerNameCStr $ \csvPathCStr -> do
                              let choicePrefixRow :: e Int64 -> CSVRow m e
                                  choicePrefixRow implIdx =
                                    CSVRow $ \h ->
                                      switchCase_
                                        implIdx
                                        [ (k, withString pref (hputString h))
                                        | (k, pref) <- zip [0 ..] rowPrefixes
                                        ]
                                        (pure ())
                                  rowStream =
                                    fmap
                                      ( \pos ->
                                          CSVRow $ \h -> do
                                            implIdx <- readBuf sortIdxs pos
                                            let cellIdx = implIdx * maxSlotsE + slot
                                            tVal <- readBuf estTimings cellIdx
                                            loVal <- readBuf estLowers cellIdx
                                            hiVal <- readBuf estUppers cellIdx
                                            let statsCols =
                                                  csvField tVal <> csvField loVal <> csvField hiVal
                                                fullRow =
                                                  if hasChoiceCols
                                                    then choicePrefixRow implIdx <> statsCols
                                                    else statsCols
                                            emitCSVRow fullRow h
                                      )
                                      (rangeS 0 activeCount)
                              runDrain (drainCSVFileCString csvPathCStr (Just headerCols)) rowStream

                        readRef bestOverallIdxRef

-- | Benchmark every combination in a 'ChoiceSpace' purely inside Janus @(m, e)@
-- without separate setup/teardown actions.
benchmarkChoicesJanus ::
  (ChoiceColumns c, CmdBenchRunner m e) =>
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c m e a ->
  m (e Int64)
benchmarkChoicesJanus cfg outDir space act =
  benchmarkChoicesJanusWithSetup cfg outDir space (pure ()) (const (pure ())) (const act)

-- | Benchmark a flat list of named 'Impl' candidates via 'SimpleChoice' purely
-- inside Janus @(m, e)@.
benchmarkCandidatesJanus ::
  (CmdBenchRunner m e) =>
  BenchConfig ->
  FilePath ->
  [Impl a] ->
  (a -> BenchT (SimpleChoice a) m e b) ->
  m (e Int64)
benchmarkCandidatesJanus cfg outDir cands fn =
  benchmarkChoicesJanus cfg outDir (simpleChoiceSpace cands) $ do
    chosen <- choose simpleImpl
    fn chosen

-- | Benchmark a single implementation (with optional named @tic@/@toc@ pairs)
-- purely inside Janus @(m, e)@, emitting @overall.csv@ and per-timer CSV files.
benchmarkSimpleJanus ::
  (CmdBenchRunner m e) =>
  BenchConfig ->
  FilePath ->
  BenchT NoChoice m e a ->
  m ()
benchmarkSimpleJanus cfg outDir act =
  void (benchmarkChoicesJanus cfg outDir emptyChoiceSpace act)

--------------------------------------------------------------------------------
-- Compiled-C Host Runners
--------------------------------------------------------------------------------

ensureOutDir :: FilePath -> IO ()
ensureOutDir outDir =
  unless (null outDir || outDir == ".") $
    createDirectoryIfMissing True outDir

-- | Compile a multi-choice Janus benchmark (with per-choice @setup@/@teardown@)
-- to native target C (@t@), execute the entire timing + Bayesian bootstrap +
-- ascending CSV generation pipeline in C with zero per-call FFI overhead, and
-- return the winning 'ChoiceSelection' and serializable 'SelectionSpec'.
benchmarkAndSelectCWithSetup ::
  forall t c res a.
  ( JanusCTarget t,
    ChoiceColumns c,
    CmdBenchRunner (JanusCMT t) (JanusCT t),
    JanusCParam t (JanusCMT t (JanusCT t Int64)),
    JanusCEvaluate t (JanusCMT t (JanusCT t Int64)),
    JanusCEval (JanusCMT t (JanusCT t Int64)) ~ IO Int64
  ) =>
  CBuildConfig ->
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c (JanusCMT t) (JanusCT t) res ->
  (res -> BenchT c (JanusCMT t) (JanusCT t) ()) ->
  (res -> BenchT c (JanusCMT t) (JanusCT t) a) ->
  IO (ChoiceSelection c, SelectionSpec)
benchmarkAndSelectCWithSetup buildCfg benchCfg outDir space setup teardown body = do
  let selections = enumerateChoices space
  if null selections
    then ioError (userError "benchmarkAndSelectCWithSetup: empty ChoiceSpace")
    else do
      ensureOutDir outDir
      bestIdx64 <-
        withJanusC @t
          buildCfg
          (benchmarkChoicesJanusWithSetup benchCfg outDir space setup teardown body)
          id
      let bestIdx = max 0 (min (length selections - 1) (fromIntegral bestIdx64))
          winner = selections !! bestIdx
      pure (winner, extractSelectionSpec winner)

-- | Compile and run a multi-choice Janus benchmark on target @t@ with an
-- explicit 'CBuildConfig'.
benchmarkAndSelectCWith ::
  forall t c a.
  ( JanusCTarget t,
    ChoiceColumns c,
    CmdBenchRunner (JanusCMT t) (JanusCT t),
    JanusCParam t (JanusCMT t (JanusCT t Int64)),
    JanusCEvaluate t (JanusCMT t (JanusCT t Int64)),
    JanusCEval (JanusCMT t (JanusCT t Int64)) ~ IO Int64
  ) =>
  CBuildConfig ->
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c (JanusCMT t) (JanusCT t) a ->
  IO (ChoiceSelection c, SelectionSpec)
benchmarkAndSelectCWith buildCfg benchCfg outDir space body =
  benchmarkAndSelectCWithSetup
    @t
    buildCfg
    benchCfg
    outDir
    space
    (pure ())
    (const (pure ()))
    (const body)

-- | Compile and run a multi-choice Janus benchmark on target @t@ using
-- 'defaultCBuildConfig', writing ascending-sorted CSV reports to @outDir@ and
-- returning the fastest 'ChoiceSelection' and its 'SelectionSpec'.
benchmarkAndSelectC ::
  forall t c a.
  ( JanusCTarget t,
    ChoiceColumns c,
    CmdBenchRunner (JanusCMT t) (JanusCT t),
    JanusCParam t (JanusCMT t (JanusCT t Int64)),
    JanusCEvaluate t (JanusCMT t (JanusCT t Int64)),
    JanusCEval (JanusCMT t (JanusCT t Int64)) ~ IO Int64
  ) =>
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c (JanusCMT t) (JanusCT t) a ->
  IO (ChoiceSelection c, SelectionSpec)
benchmarkAndSelectC = benchmarkAndSelectCWith @t defaultCBuildConfig

-- | Compile and benchmark a flat list of candidate 'Impl's on target @t@,
-- returning the fastest 'Impl'.
benchmarkCandidatesC ::
  forall t implTy b.
  ( JanusCTarget t,
    CmdBenchRunner (JanusCMT t) (JanusCT t),
    JanusCParam t (JanusCMT t (JanusCT t Int64)),
    JanusCEvaluate t (JanusCMT t (JanusCT t Int64)),
    JanusCEval (JanusCMT t (JanusCT t Int64)) ~ IO Int64
  ) =>
  BenchConfig ->
  FilePath ->
  [Impl implTy] ->
  (implTy -> BenchT (SimpleChoice implTy) (JanusCMT t) (JanusCT t) b) ->
  IO (Impl implTy)
benchmarkCandidatesC benchCfg outDir cands fn = do
  if null cands
    then ioError (userError "benchmarkCandidatesC: empty candidate list")
    else do
      ensureOutDir outDir
      bestIdx64 <-
        withJanusC @t
          defaultCBuildConfig
          (benchmarkCandidatesJanus benchCfg outDir cands fn)
          id
      let bestIdx = max 0 (min (length cands - 1) (fromIntegral bestIdx64))
      pure (cands !! bestIdx)

-- | Compile and execute a single-implementation Janus benchmark on target @t@,
-- writing @overall.csv@ and per-named-timer CSV files into @outDir@.
benchmarkSimpleC ::
  forall t a.
  ( JanusCTarget t,
    CmdBenchRunner (JanusCMT t) (JanusCT t),
    JanusCParam t (JanusCMT t ()),
    JanusCEvaluate t (JanusCMT t ()),
    JanusCEval (JanusCMT t ()) ~ IO ()
  ) =>
  BenchConfig ->
  FilePath ->
  BenchT NoChoice (JanusCMT t) (JanusCT t) a ->
  IO ()
benchmarkSimpleC benchCfg outDir body = do
  ensureOutDir outDir
  withJanusC @t defaultCBuildConfig (benchmarkSimpleJanus benchCfg outDir body) id

--------------------------------------------------------------------------------
-- IO Interpreter Runners
--------------------------------------------------------------------------------

-- | Run a multi-choice Janus benchmark (with per-choice @setup@/@teardown@) in
-- the @(IO, Identity)@ interpreter, writing ascending-sorted CSV reports to
-- @outDir@ and returning the fastest 'ChoiceSelection' and 'SelectionSpec'.
benchmarkAndSelectIOWithSetup ::
  (ChoiceColumns c) =>
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c IO Identity res ->
  (res -> BenchT c IO Identity ()) ->
  (res -> BenchT c IO Identity a) ->
  IO (ChoiceSelection c, SelectionSpec)
benchmarkAndSelectIOWithSetup benchCfg outDir space setup teardown body = do
  let selections = enumerateChoices space
  if null selections
    then ioError (userError "benchmarkAndSelectIOWithSetup: empty ChoiceSpace")
    else do
      ensureOutDir outDir
      Identity bestIdx64 <-
        benchmarkChoicesJanusWithSetup benchCfg outDir space setup teardown body
      let bestIdx = max 0 (min (length selections - 1) (fromIntegral bestIdx64))
          winner = selections !! bestIdx
      pure (winner, extractSelectionSpec winner)

-- | Run a multi-choice Janus benchmark in @(IO, Identity)@.
benchmarkAndSelectIO ::
  (ChoiceColumns c) =>
  BenchConfig ->
  FilePath ->
  ChoiceSpace c ->
  BenchT c IO Identity a ->
  IO (ChoiceSelection c, SelectionSpec)
benchmarkAndSelectIO benchCfg outDir space body =
  benchmarkAndSelectIOWithSetup
    benchCfg
    outDir
    space
    (pure ())
    (const (pure ()))
    (const body)

-- | Benchmark a flat list of candidate 'Impl's in @(IO, Identity)@, returning
-- the fastest 'Impl'.
benchmarkCandidatesIO ::
  BenchConfig ->
  FilePath ->
  [Impl implTy] ->
  (implTy -> BenchT (SimpleChoice implTy) IO Identity b) ->
  IO (Impl implTy)
benchmarkCandidatesIO benchCfg outDir cands fn = do
  if null cands
    then ioError (userError "benchmarkCandidatesIO: empty candidate list")
    else do
      ensureOutDir outDir
      Identity bestIdx64 <- benchmarkCandidatesJanus benchCfg outDir cands fn
      let bestIdx = max 0 (min (length cands - 1) (fromIntegral bestIdx64))
      pure (cands !! bestIdx)

-- | Run a single-implementation Janus benchmark in @(IO, Identity)@.
benchmarkSimpleIO ::
  BenchConfig ->
  FilePath ->
  BenchT NoChoice IO Identity a ->
  IO ()
benchmarkSimpleIO benchCfg outDir body = do
  ensureOutDir outDir
  benchmarkSimpleJanus benchCfg outDir body
