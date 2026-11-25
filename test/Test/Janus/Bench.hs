{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Janus.Bench where

import Control.Applicative (empty, (<|>))
import Control.Exception (bracket)
import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (..))
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64)
import Foreign.C (CChar)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc.Policy
  ( CmdAlloc,
    destroyFixedBuf,
    destroyRecycler,
    dynamicAlloc,
    fallbackAlloc,
    fixedAlloc,
    newFixedBuf,
    newRecycler,
    recyclingAlloc,
    segregateAlloc,
    statsAlloc,
  )
import Janus.Alloc.Stats
  ( AllocSnapshot (..),
    freeAllocTracker,
    newAllocTracker,
    readAllocSnapshot,
  )
import Janus.Backend.C (JanusCCSE, JanusCMCSE, PortableC, showJanusC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64, LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Bench
import Janus.Command.Cond (whenM_)
import Janus.Command.Format (CmdFormat, CmdPutString, CmdString (..))
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Control.CSV
  ( CSVRow,
    csvEscapeString,
    csvField,
    drainCSVFile,
  )
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Control.Reader (ReaderExp (..))
import Janus.Control.Stream (Stream)
import qualified Janus.Data.DynVec as DV
import Janus.Data.Layout (ScalarBuf (..), allocBuf, freeBuf, isNullBuf, readBuf, writeBuf)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Fix (ExpFix (..))
import Janus.SPMD
  ( CmdOptimalGang,
    foldForeachOptimal0M,
    gangReduceAdd,
    uniformAlignedPtr,
  )
import Janus.Typed (JanusTyped)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Janus.BenchDefs
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------
-- Helpers: Temp Directory & RFC 4180 Parser for Test Verification
--------------------------------------------------------------------------------

getBenchTestBuildConfig :: IO CBuildConfig
getBenchTestBuildConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

withTempBenchDir :: String -> (FilePath -> IO a) -> IO a
withTempBenchDir tag action = do
  baseTmp <- getTemporaryDirectory
  uq <- hashUnique <$> newUnique
  let dir = baseTmp </> ("janus_bench_test_" <> tag <> "_" <> show uq)
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    action

-- | Strict RFC 4180 CSV parser returning rows of unescaped fields (supporting
-- quoted fields with embedded commas, doubled quotes, and newlines).
parseRFC4180 :: String -> [[String]]
parseRFC4180 = parseRows
  where
    parseRows [] = []
    parseRows s =
      let (row, rest) = parseRow s
       in row : parseRows rest

    parseRow s =
      let (field, rest) = parseField s
       in case rest of
            (',' : more) ->
              let (fields, afterRow) = parseRow more
               in (field : fields, afterRow)
            ('\r' : '\n' : more) -> ([field], more)
            ('\n' : more) -> ([field], more)
            ('\r' : more) -> ([field], more)
            [] -> ([field], [])
            _ -> ([field], drop 1 rest)

    parseField ('"' : cs) = parseQuoted cs
    parseField cs =
      let (plain, rest) = break (\c -> c == ',' || c == '\n' || c == '\r') cs
       in (plain, rest)

    parseQuoted ('"' : '"' : cs) =
      let (restField, rest) = parseQuoted cs
       in ('"' : restField, rest)
    parseQuoted ('"' : cs) = ("", cs)
    parseQuoted (c : cs) =
      let (restField, rest) = parseQuoted cs
       in (c : restField, rest)
    parseQuoted [] = ("", [])

--------------------------------------------------------------------------------
-- 1. Reusable CSV Drain Tests (RFC 4180 Escaping & IO vs C Parity)
--------------------------------------------------------------------------------

test_csv_properties :: TestTree
test_csv_properties =
  testGroup
    "CSV RFC 4180 Escaping Properties"
    [ testProperty "arbitrary cell strings round-trip through csvEscapeString and parseRFC4180" csvRfc4180RoundtripProp
    ]

csvRfc4180RoundtripProp :: Property
csvRfc4180RoundtripProp = property $ do
  let genCell =
        Gen.string
          (Range.linear 0 32)
          (Gen.element ['a', 'b', '0', ' ', ',', '"', '\n'])
  rows <-
    forAll $
      Gen.list
        (Range.linear 1 12)
        (Gen.list (Range.singleton 3) genCell)
  let formatThree [a, b, c] =
        csvEscapeString a <> "," <> csvEscapeString b <> "," <> csvEscapeString c <> "\n"
      formatThree other = error ("unexpected row length: " <> show (length other))
      rendered = concatMap formatThree rows
  parseRFC4180 rendered === rows

emitTestCSV ::
  forall m e.
  ( CmdAlloc m e,
    CmdPutString m e,
    CmdFormat m e Int64,
    JanusTyped e CChar
  ) =>
  FilePath ->
  FilePath ->
  m ()
emitTestCSV tuplePath dynPath = do
  let fromListS :: [row] -> Stream m row
      fromListS = foldr ((<|>) . pure) empty
      items :: [(String, e Int64, e Bool)]
      items =
        [ ("alpha, \"quoted\"", 42, true),
          ("plain_row", -7, false)
        ]
  runDrain
    (drainCSVFile tuplePath (Just ["label", "count", "active"]))
    (fromListS items)

  withString "dynamic, \"escaped\" cell" $ \dynSpecial ->
    withString "clean_cell" $ \dynClean -> do
      let rows :: [CSVRow m e]
          rows =
            [ csvField ("row" :: String) <> csvField dynSpecial <> csvField (100 :: e Int64),
              csvField ("row" :: String) <> csvField dynClean <> csvField (101 :: e Int64)
            ]
      runDrain
        (drainCSVFile dynPath (Just ["kind", "note", "score"]))
        (fromListS rows)

unit_csv_drain_io_and_compiled_c :: Assertion
unit_csv_drain_io_and_compiled_c =
  withTempBenchDir "csv" $ \dir -> do
    buildCfg <- getBenchTestBuildConfig
    let ioTuple = dir </> "io_tuple.csv"
        ioDyn = dir </> "io_dyn.csv"
        cTuple = dir </> "c_tuple.csv"
        cDyn = dir </> "c_dyn.csv"

    emitTestCSV @IO @Identity ioTuple ioDyn
    withJanusC @PortableC buildCfg (emitTestCSV @JanusCMCSE @JanusCCSE cTuple cDyn) id

    ioTupleText <- readFile ioTuple
    cTupleText <- readFile cTuple
    ioTupleText @?= cTupleText
    parseRFC4180 cTupleText
      @?= [ ["label", "count", "active"],
            ["alpha, \"quoted\"", "42", "true"],
            ["plain_row", "-7", "false"]
          ]

    ioDynText <- readFile ioDyn
    cDynText <- readFile cDyn
    ioDynText @?= cDynText
    parseRFC4180 cDynText
      @?= [ ["kind", "note", "score"],
            ["row", "dynamic, \"escaped\" cell", "100"],
            ["row", "clean_cell", "101"]
          ]

--------------------------------------------------------------------------------
-- 2. Pure-Janus Bayesian Bootstrap & Outlier Immunity Tests
--------------------------------------------------------------------------------

syntheticPosteriorKernel ::
  forall m e.
  (CmdBenchStats m e) =>
  m (e Double, e Double, e Double)
syntheticPosteriorKernel = do
  let n = 100 :: e Int64
      cfg =
        defaultBenchConfig
          { benchBootstrapDraws = 400,
            benchMadThreshold = 5.0,
            benchCredibleMass = 0.95,
            benchSeed = 0x123456789abcdef0
          }
  withBuf @Double dynamicAlloc n $ \bufSB -> do
    buf <- scopedBuf bufSB
    -- 90 clean samples tightly clustered around 500.0 ticks (499.0 .. 501.0)
    -- plus 10 extreme 100x OS-interrupt spikes at 50000.0 ticks!
    rangeM 0 90 $ \i -> do
      let offset = ifThenElse ((i `iand` 1) `eq` 0) (-1.0) 1.0
      writeBuf buf i (500.0 + offset)
    rangeM 90 100 $ \i ->
      writeBuf buf i 50000.0

    rngRef <- newRef (fromIntegral (benchSeed cfg) :: e Word64)
    est <- estimateTimingPosterior cfg rngRef buf n
    pure (estTiming est, estCiLower est, estCiUpper est)

unit_bayesian_bootstrap_outlier_immunity :: Assertion
unit_bayesian_bootstrap_outlier_immunity = do
  buildCfg <- getBenchTestBuildConfig
  (Identity ioMean, Identity ioLo, Identity ioHi) <- syntheticPosteriorKernel @IO @Identity

  -- Run identical estimator in compiled LinuxX86_64 C
  cMean <-
    withJanusC @LinuxX86_64
      buildCfg
      ( do
          (mVal, _loVal, _hiVal) <- syntheticPosteriorKernel @LinuxX86_64CMCSE @LinuxX86_64CCSE
          pure mVal
      )
      id

  -- Both IO and C recover ~500.0 despite 10% 50000.0 spikes (naive mean = 5450.0!)
  assertBool ("expected ioLo <= ioMean: " <> show (ioLo, ioMean)) (ioLo <= ioMean)
  assertBool ("expected ioMean <= ioHi: " <> show (ioMean, ioHi)) (ioMean <= ioHi)
  assertBool ("expected ioMean near 500.0, got " <> show ioMean) (abs (ioMean - 500.0) < 2.0)
  assertBool ("expected CI upper well below spike-contaminated mean, got " <> show ioHi) (ioHi < 505.0)
  assertBool ("expected IO and compiled C posterior mean to match: " <> show (ioMean, cMean)) (abs (ioMean - cMean) < 1.0e-9)

--------------------------------------------------------------------------------
-- 3. Multi-Choice + Named Timers + Optimal SPMD + DynVec + Fixpoint Inside BenchT
--------------------------------------------------------------------------------

kernelChoiceSpace :: ChoiceSpace KernelChoice
kernelChoiceSpace =
  KernelChoice
    { _algoPiece =
        candidates
          [ implWithId "fast_spmd" "SIMD gang reduction (fast)" AlgoFastSPMD,
            implWithId "slow_redundant" "Redundant multi-pass scalar, \"baseline\"" AlgoSlowRedundant
          ],
      _unrollPiece =
        candidates
          [ implWithId "wide_4" "Optimal SIMD gang, \"vectorized\"" UnrollWide4,
            implWithId "scalar_1" "1-lane scalar" UnrollScalar1
          ]
    }

-- | Polymorphic benchmarked algorithm exercising:
--   * 'choose' & 'chooseDescription' over HKD lenses ('algoPiece', 'unrollPiece')
--   * 'namedTic' / 'namedToc' and 'withNamedTimer'
--   * Inner 'foldForeachOptimal0M' ('CmdOptimalGang') SIMD execution inside '(BenchT c m e, Bench c e)'
--   * 'DynVec' & 'dynamicAlloc' inside 'BenchT'
--   * Recursive local function via 'ExpFix' ('fun') on 'Bench c e'
polymorphicBenchKernel ::
  forall m e.
  ( CmdAlloc (BenchT KernelChoice m e) (Bench KernelChoice e),
    DV.DynVecCtx (BenchT KernelChoice m e) (Bench KernelChoice e) Int64,
    CmdNamedTimer m e,
    CmdOptimalGang (BenchT KernelChoice m e) (Bench KernelChoice e),
    ExpFix (Bench KernelChoice e) (Bench KernelChoice e Int64 -> Bench KernelChoice e Int64)
  ) =>
  (ScalarBuf Int64 (Bench KernelChoice e), ScalarBuf Int64 (Bench KernelChoice e)) ->
  Bench KernelChoice e Int64 ->
  BenchT KernelChoice m e (Bench KernelChoice e Int64)
polymorphicBenchKernel (inputBuf@(ScalarBuf rawPtr), sinkBuf) len = do
  algo <- choose algoPiece
  unroll <- choose unrollPiece

  -- Exercise recursive `fun` over `Bench c e`
  let mixStep :: Bench KernelChoice e Int64 -> Bench KernelChoice e Int64
      mixStep =
        fun (Proxy @(Bench KernelChoice e)) "bench_mix" $ \self k ->
          ifThenElse (k `le` 0) 1 (k + self (k - 1))

  -- Phase 1: "prepare" named timer using DynVec inside BenchT
  namedTic "prepare"
  dv <- DV.withCapacity @Int64 dynamicAlloc 8
  DV.push dynamicAlloc dv (mixStep 3)
  DV.push dynamicAlloc dv 10
  prepSum <- runDrain drainSum (DV.stream dv)
  DV.free dynamicAlloc dv
  namedToc "prepare"

  -- Phase 2: "compute" named timer (fast single-pass SPMD vs serial data-dependent chains)
  let runSerialChain :: Int64 -> BenchT KernelChoice m e (Bench KernelChoice e Int64)
      runSerialChain passes = do
        sumRef <- newRef (0 :: Bench KernelChoice e Int64)
        rangeM 0 len $ \i -> do
          v <- readBuf inputBuf i
          cur <- readRef sumRef
          writeRef sumRef (cur + v)
        chainRef <- newRef (1 :: Bench KernelChoice e Int64)
        rangeM 0 (fromIntegral passes) $ \p ->
          rangeM 0 len $ \i -> do
            v <- readBuf inputBuf i
            st <- readRef chainRef
            writeRef chainRef ((st * 2862933555777941757) + (v `ieor` p) + 3037000493)
        baseSum <- readRef sumRef
        sink <- readRef chainRef
        -- Keep `sink` live so `-O3 -flto` must execute the full serial dependency chain
        pure (baseSum + ifThenElse (sink `eq` 0x7fffffffffffffff) 1 0)

  totalRef <- newRef prepSum
  withNamedTimer "compute" $
    case (algo, unroll) of
      (AlgoFastSPMD, UnrollWide4) -> do
        s <-
          foldForeachOptimal0M
            (Proxy @Int64)
            len
            0
            ( \acc laneIdx -> do
                v <- peekElemOff (uniformAlignedPtr rawPtr) laneIdx
                pure (acc + v)
            )
            gangReduceAdd
        prev <- readRef totalRef
        writeRef totalRef (prev + s)
      (AlgoFastSPMD, UnrollScalar1) -> do
        s <- runSerialChain 4
        prev <- readRef totalRef
        writeRef totalRef (prev + s)
      (AlgoSlowRedundant, UnrollWide4) -> do
        s <- runSerialChain 16
        prev <- readRef totalRef
        writeRef totalRef (prev + s)
      (AlgoSlowRedundant, UnrollScalar1) -> do
        s <- runSerialChain 64
        prev <- readRef totalRef
        writeRef totalRef (prev + s)

  finalTot <- readRef totalRef
  writeBuf sinkBuf 0 finalTot
  pure finalTot

-- Compile-time TH selection splice loaded from `test/fixtures/bench_selection.sel`
fixtureCompiledSpec :: SelectionSpec
fixtureCompiledSpec = $(loadSelectionTH "test/fixtures/bench_selection.sel")

verifyBenchmarkCSV :: FilePath -> Int -> IO [[String]]
verifyBenchmarkCSV csvPath expectedDataRows = do
  exists <- doesFileExist csvPath
  assertBool ("expected CSV file to exist: " <> csvPath) exists
  raw <- readFile csvPath
  let rows = parseRFC4180 raw
  case rows of
    [] -> fail ("empty CSV file: " <> csvPath)
    (hdr : body) -> do
      hdr @?= ["algo_piece", "unroll_piece", "timing", "ci_lower", "ci_upper"]
      length body @?= expectedDataRows
      let timings = map (\r -> read (r !! 2) :: Double) body
      forM_ body $ \r -> do
        length r @?= 5
        let t = read (r !! 2) :: Double
            lo = read (r !! 3) :: Double
            hi = read (r !! 4) :: Double
        assertBool ("timing >= 0 in " <> csvPath <> ": " <> show r) (t >= 0.0)
        assertBool ("ci_lower <= timing in " <> csvPath <> ": " <> show r) (lo <= t + 1.0e-9)
        assertBool ("timing <= ci_upper in " <> csvPath <> ": " <> show r) (t <= hi + 1.0e-9)
      -- Verify ascending sort order (fastest first: t_0 <= t_1 <= ...)
      forM_ (zip timings (drop 1 timings)) $ \(tPrev, tNext) ->
        assertBool
          ("expected ascending timing order (fastest first) in " <> csvPath <> ", got " <> show timings)
          (tPrev <= tNext + 1.0e-9)
      pure body

unit_multi_choice_benchmark_c_io_and_library_promotion :: Assertion
unit_multi_choice_benchmark_c_io_and_library_promotion =
  withTempBenchDir "multi" $ \dir -> do
    buildCfg <- getBenchTestBuildConfig
    let cOutDir = dir </> "c_csv"
        ioOutDir = dir </> "io_csv"
        selFile = dir </> "saved" </> "winner.sel"
        fastCfg =
          defaultBenchConfig
            { benchWarmupIters = 6,
              benchSamples = 20,
              benchBootstrapDraws = 150,
              benchCalibrationIters = 64,
              benchMinBatchTicks = 4000,
              benchMaxBatchSize = 4096
            }
        arrLen :: forall f. (Num (f Int64)) => f Int64
        arrLen = 256

        setupArr ::
          forall m e.
          (Monad m, DV.DynVecCtx (BenchT KernelChoice m e) (Bench KernelChoice e) Int64) =>
          BenchT KernelChoice m e (ScalarBuf Int64 (Bench KernelChoice e), ScalarBuf Int64 (Bench KernelChoice e))
        setupArr = do
          buf <- allocBuf @Int64 dynamicAlloc arrLen
          sink <- allocBuf @Int64 dynamicAlloc 1
          rangeM 0 arrLen $ \i -> writeBuf buf i (i + 1)
          writeBuf sink 0 0
          pure (buf, sink)

        teardownArr ::
          forall m e.
          (Monad m, DV.DynVecCtx (BenchT KernelChoice m e) (Bench KernelChoice e) Int64) =>
          (ScalarBuf Int64 (Bench KernelChoice e), ScalarBuf Int64 (Bench KernelChoice e)) ->
          BenchT KernelChoice m e ()
        teardownArr (buf, sink) = do
          freeBuf @Int64 dynamicAlloc buf arrLen
          freeBuf @Int64 dynamicAlloc sink 1

        benchKernelC :: LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
        benchKernelC =
          benchmarkChoicesJanusWithSetup
            fastCfg
            cOutDir
            kernelChoiceSpace
            setupArr
            teardownArr
            (`polymorphicBenchKernel` arrLen)

    -- 1. Verify generated benchmark C contains inline rdtsc/rdtscp + fopen (zero FFI callbacks)
    benchCSrc <- showJanusC @LinuxX86_64 "bench_all" benchKernelC
    assertBool "benchmark C should contain rdtsc" ("rdtsc" `isInfixOf` benchCSrc)
    assertBool "benchmark C should contain rdtscp" ("rdtscp" `isInfixOf` benchCSrc)
    assertBool "benchmark C should contain fopen" ("fopen" `isInfixOf` benchCSrc)

    -- 2. Run compiled LinuxX86_64 C multi-choice benchmark end-to-end
    (cWinner, cSpec) <-
      benchmarkAndSelectCWithSetup @LinuxX86_64
        buildCfg
        fastCfg
        cOutDir
        kernelChoiceSpace
        setupArr
        teardownArr
        (`polymorphicBenchKernel` arrLen)

    overallRows <- verifyBenchmarkCSV (cOutDir </> "overall.csv") 4
    _ <- verifyBenchmarkCSV (cOutDir </> "prepare.csv") 4
    computeRows <- verifyBenchmarkCSV (cOutDir </> "compute.csv") 4

    -- Verify human-readable descriptions (including commas and quotes) and fastest-first ordering
    case (overallRows, computeRows) of
      (fastestOverallRow : _, fastestComputeRow : _) -> do
        let fastestOverallAlgo = fastestOverallRow !! 0
            slowestOverallAlgo = last overallRows !! 0
            slowestComputeRow = last computeRows
        fastestOverallAlgo @?= "SIMD gang reduction (fast)"
        slowestOverallAlgo @?= "Redundant multi-pass scalar, \"baseline\""
        take 2 fastestComputeRow @?= ["SIMD gang reduction (fast)", "Optimal SIMD gang, \"vectorized\""]
        take 2 slowestComputeRow @?= ["Redundant multi-pass scalar, \"baseline\"", "1-lane scalar"]
      _ -> fail "expected non-empty overallRows and computeRows"
    cSpec @?= extractSelectionSpec cWinner

    -- 3. Run IO interpreter benchmark on the identical polymorphic kernel
    let ioFastCfg =
          fastCfg
            { benchWarmupIters = 2,
              benchSamples = 8,
              benchBootstrapDraws = 80,
              benchFixedBatchSize = Just 1
            }
    (_ioWinner, _ioSpec) <-
      benchmarkAndSelectIOWithSetup
        ioFastCfg
        ioOutDir
        kernelChoiceSpace
        setupArr
        teardownArr
        (`polymorphicBenchKernel` arrLen)
    _ <- verifyBenchmarkCSV (ioOutDir </> "overall.csv") 4
    _ <- verifyBenchmarkCSV (ioOutDir </> "prepare.csv") 4
    _ <- verifyBenchmarkCSV (ioOutDir </> "compute.csv") 4

    -- 4. Library promotion: saveSelection -> loadSelection + loadSelectionTH -> runProduction
    saveSelection selFile cWinner
    reloadedWinner <- loadSelection kernelChoiceSpace selFile
    extractSelectionSpec reloadedWinner @?= cSpec

    let thWinner = resolveSelectionOrFail kernelChoiceSpace fixtureCompiledSpec
        prodKernelC :: LinuxX86_64CMCSE (LinuxX86_64CCSE Int64)
        prodKernelC =
          runProduction thWinner $ do
            buf <- setupArr
            res <- polymorphicBenchKernel buf arrLen
            teardownArr buf
            pure res

    -- Verify zero timer instructions (rdtsc/rdtscp) in production mode C code!
    prodCSrc <- showJanusC @LinuxX86_64 "prod_kernel" prodKernelC
    assertBool "production C must NOT contain rdtsc" (not ("rdtsc" `isInfixOf` prodCSrc))
    assertBool "production C must NOT contain rdtscp" (not ("rdtscp" `isInfixOf` prodCSrc))

    -- Verify production C and IO produce the exact expected sum:
    -- mixStep 3 = (3 + 2 + 1 + 1) = 7; prepSum = 7 + 10 = 17; sum(1..256) = 32896 -> 32913
    cProdVal <- withJanusC @LinuxX86_64 buildCfg prodKernelC id
    Identity ioProdVal <-
      runProduction reloadedWinner $ do
        buf <- setupArr
        res <- polymorphicBenchKernel buf arrLen
        teardownArr buf
        pure res
    cProdVal @?= 32913
    ioProdVal @?= 32913

    -- 5. Verify zero-choice `benchmarkSimpleC` produces header `timing,ci_lower,ci_upper`
    let simpleDir = dir </> "simple_csv"
    benchmarkSimpleC @LinuxX86_64
      (fastCfg {benchWarmupIters = 2, benchSamples = 10, benchBootstrapDraws = 80, benchFixedBatchSize = Just 4})
      simpleDir
      $ do
        acc <- newRef (0 :: Bench NoChoice LinuxX86_64CCSE Int64)
        withNamedTimer "loop" $
          rangeM 0 64 $ \i -> do
            cur <- readRef acc
            writeRef acc (cur + i)
        readRef acc

    simpleOverall <- parseRFC4180 <$> readFile (simpleDir </> "overall.csv")
    simpleLoop <- parseRFC4180 <$> readFile (simpleDir </> "loop.csv")
    case simpleOverall of
      [hdr, row] -> do
        hdr @?= ["timing", "ci_lower", "ci_upper"]
        length row @?= 3
      other -> fail ("unexpected simple overall.csv: " <> show other)
    case simpleLoop of
      [hdr, row] -> do
        hdr @?= ["timing", "ci_lower", "ci_upper"]
        length row @?= 3
      other -> fail ("unexpected simple loop.csv: " <> show other)

--------------------------------------------------------------------------------
-- 4. Direct `(ReaderT r m, ReaderExp r e)` & HKD Lenses (`choose` / `chooseDescription`)
--------------------------------------------------------------------------------

rawReaderKernel ::
  forall r m e.
  ( HasChoices KernelChoice r,
    Monad m,
    DV.DynVecCtx (ReaderT r m) (ReaderExp r e) Int64,
    CmdOptimalGang (ReaderT r m) (ReaderExp r e)
  ) =>
  ReaderT r m (ReaderExp r e Int64)
rawReaderKernel = do
  algo <- choose algoPiece
  unroll <- choose unrollPiece
  let len = 64 :: ReaderExp r e Int64
  withBuf @Int64 dynamicAlloc len $ \bufSB -> do
    buf@(ScalarBuf rawPtr) <- scopedBuf bufSB
    rangeM 0 len $ \i -> writeBuf buf i (i + 1)
    case (algo, unroll) of
      (AlgoFastSPMD, UnrollWide4) ->
        foldForeachOptimal0M
          (Proxy @Int64)
          len
          0
          ( \acc laneIdx -> do
              v <- peekElemOff (uniformAlignedPtr rawPtr) laneIdx
              pure (acc + v)
          )
          gangReduceAdd
      _ -> do
        accRef <- newRef (1000 :: ReaderExp r e Int64)
        rangeM 0 len $ \i -> do
          v <- readBuf buf i
          cur <- readRef accRef
          writeRef accRef (cur + v)
        readRef accRef

unit_reader_t_direct_and_choose_via_ask :: Assertion
unit_reader_t_direct_and_choose_via_ask = do
  buildCfg <- getBenchTestBuildConfig
  let fastSel = resolveSelectionOrFail kernelChoiceSpace fixtureCompiledSpec
      slowChoices :: KernelChoice Identity
      slowChoices =
        KernelChoice
          { _algoPiece = Identity AlgoSlowRedundant,
            _unrollPiece = Identity UnrollScalar1
          }
      slowDescriptions :: KernelChoice (Const String)
      slowDescriptions =
        KernelChoice
          { _algoPiece = Const "slow-desc",
            _unrollPiece = Const "scalar-desc"
          }
      slowPair = (slowChoices, slowDescriptions)

  -- Verify both `c Identity` and `c (Const String)` in `ReaderT` using the same lenses
  (algoDesc, unrollDesc, Identity ioSlow) <-
    flip runReaderT slowPair $ do
      d1 <- chooseDescription algoPiece
      d2 <- chooseDescription unrollPiece
      v <- getReaderExp <$> rawReaderKernel @(KernelChoice Identity, KernelChoice (Const String)) @IO @Identity
      pure (d1, d2, v)
  algoDesc @?= "slow-desc"
  unrollDesc @?= "scalar-desc"

  -- Also verify `ChoiceSelection KernelChoice` and bare `KernelChoice Identity` directly in `ReaderT`
  Identity ioFast <-
    runReaderT (getReaderExp <$> rawReaderKernel @(ChoiceSelection KernelChoice) @IO @Identity) fastSel
  cFast <-
    withJanusC @LinuxX86_64
      buildCfg
      (runReaderT (getReaderExp <$> rawReaderKernel @(KernelChoice Identity) @LinuxX86_64CMCSE @LinuxX86_64CCSE) (selChoices fastSel))
      id
  cSlow <-
    withJanusC @LinuxX86_64
      buildCfg
      (runReaderT (getReaderExp <$> rawReaderKernel @(KernelChoice Identity, KernelChoice (Const String)) @LinuxX86_64CMCSE @LinuxX86_64CCSE) slowPair)
      id

  -- sum(1..64) = 2080; slow branch starts at 1000 -> 3080
  ioFast @?= 2080
  cFast @?= 2080
  ioSlow @?= 3080
  cSlow @?= 3080

--------------------------------------------------------------------------------
-- 5. `withBuf` + `reallocScopedBuf` Across Composite Allocation Policies
--------------------------------------------------------------------------------

withBufReallocKernel ::
  forall m e.
  (CmdBenchStats m e) =>
  m (e Int64)
withBufReallocKernel = do
  tracker <- newAllocTracker
  fb <- newFixedBuf dynamicAlloc 192
  rec <- newRecycler dynamicAlloc 8
  let composite =
        statsAlloc
          tracker
          ( segregateAlloc
              256
              (fallbackAlloc (fixedAlloc fb) (recyclingAlloc rec dynamicAlloc))
              dynamicAlloc
          )

  -- 1. Multi-stage `reallocScopedBuf`:
  --    cap 8 (64B, FixedBuf) -> 16 (128B, FixedBuf in-place)
  --    -> 32 (256B, spills to Recycling) -> 128 (1024B, crosses Segregate to largeA)
  --    -> 256 (2048B inside rangeM conditional)
  sumVal <-
    withBuf @Int64 composite 8 $ \sb -> do
      rangeM 0 8 $ \i -> writeScopedBuf sb i (i + 1)
      _ <- reallocScopedBuf sb 16 8
      rangeM 8 16 $ \i -> writeScopedBuf sb i (i + 1)
      _ <- reallocScopedBuf sb 32 16
      rangeM 16 32 $ \i -> writeScopedBuf sb i (i + 1)
      _ <- reallocScopedBuf sb 128 32
      rangeM 32 128 $ \i -> writeScopedBuf sb i (i + 1)
      rangeM 128 256 $ \i -> do
        curCap <- scopedBufCap sb
        whenM_ (i `ge` curCap) $ do
          _ <- reallocScopedBuf sb 256 128
          pure ()
        writeScopedBuf sb i (i + 1)
      accRef <- newRef (0 :: e Int64)
      rangeM 0 256 $ \i -> do
        v <- readScopedBuf sb i
        cur <- readRef accRef
        writeRef accRef (cur + v)
      readRef accRef

  -- 2. Shrink-to-zero (`reallocScopedBuf sb 0 0`) inside `withBuf` must not double-free
  withBuf @Int64 composite 16 $ \sb2 -> do
    writeScopedBuf sb2 0 42
    _ <- reallocScopedBuf sb2 0 0
    pure ()

  snap1 <- readAllocSnapshot tracker
  destroyRecycler dynamicAlloc rec
  destroyFixedBuf dynamicAlloc fb
  freeAllocTracker tracker

  -- 3. Failed `reallocScopedBuf` (exhausted FixedBuf without fallback) must
  --    preserve original `(oldBuf, oldCap)` and still free it on `withBuf` exit
  tracker2 <- newAllocTracker
  tinyFb <- newFixedBuf dynamicAlloc 128
  let tinyPolicy = statsAlloc tracker2 (fixedAlloc tinyFb)
  oomCheckVal <-
    withBuf @Int64 tinyPolicy 4 $ \sb3 -> do
      writeScopedBuf sb3 0 777
      failedBuf <- reallocScopedBuf sb3 10000 4
      v0 <- readScopedBuf sb3 0
      capAfterFail <- scopedBufCap sb3
      pure $
        ifThenElse
          (isNullBuf @Int64 (Proxy @m) failedBuf `land` (capAfterFail `eq` 4))
          v0
          (-999999)
  snap2 <- readAllocSnapshot tracker2
  destroyFixedBuf dynamicAlloc tinyFb
  freeAllocTracker tracker2

  pure $
    sumVal
      + oomCheckVal
      + snapLiveBlocks snap1 * 1000000
      + snapLiveBytes snap1 * 1000
      + snapLiveBlocks snap2 * 1000000
      + snapLiveBytes snap2 * 1000

unit_with_buf_realloc_across_policies_io_and_c :: Assertion
unit_with_buf_realloc_across_policies_io_and_c = do
  buildCfg <- getBenchTestBuildConfig
  Identity ioRes <- withBufReallocKernel @IO @Identity
  cRes <- withJanusC @LinuxX86_64 buildCfg (withBufReallocKernel @LinuxX86_64CMCSE @LinuxX86_64CCSE) id
  -- sum(1..256) = 32896, oomCheckVal = 777, all liveBlocks == 0 and liveBytes == 0 -> 33673
  ioRes @?= 33673
  cRes @?= 33673
