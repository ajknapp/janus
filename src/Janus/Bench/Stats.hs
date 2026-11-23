{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pure-Janus statistical benchmarking engine: timer-overhead calibration,
-- adaptive batch sizing, SplitMix64 PRNG, MAD spike gating, and Rubin's (1981)
-- Nonparametric Bayesian Bootstrap over Dirichlet-weighted Huber M-estimators.
--
-- Every operation in this module executes 100% inside Janus @(m, e)@, compiling
-- to self-contained C loops with zero per-sample FFI callbacks.
module Janus.Bench.Stats
  ( -- * Benchmark Configuration & Posterior Summary
    BenchConfig (..),
    defaultBenchConfig,
    TimingEstimate (..),
    CmdBenchStats,

    -- * Pure-Janus SplitMix64 PRNG
    nextSplitMix64,
    nextUniform01,

    -- * Calibration Primitives
    calibrateTimerOverhead,
    calibrateBatchSize,

    -- * Robust Bayesian Bootstrap Location & Credible Interval
    estimateTimingPosterior,
  )
where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import Janus.Alloc.Policy (CmdAlloc, dynamicAlloc)
import Janus.Bench.Types (CmdNamedTimer)
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Format (CmdFormat)
import Janus.Command.Memory (CmdMem (..), CmdStorable (..))
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (Ref, letM, newRef, readRef, writeRef)
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (whileM)
import Janus.Data.Layout (LayoutElemCtx, ScalarBuf (..), allocBuf, freeBuf, readBuf, writeBuf)
import Janus.Data.Sort (sortBuf)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpFloatingCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..), max', min')
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- Configuration & Result Types
--------------------------------------------------------------------------------

-- | Configuration governing warmup, batching, outlier gating, and Bayesian
-- bootstrap credible-interval estimation.
data BenchConfig = BenchConfig
  { -- | Number of unrecorded warmup batches before sample collection (default: @32@).
    benchWarmupIters :: !Int,
    -- | Number of batched timing samples collected per implementation (default: @100@).
    benchSamples :: !Int,
    -- | Number of @Dirichlet(1,...,1)@ Bayesian bootstrap draws (default: @500@).
    benchBootstrapDraws :: !Int,
    -- | Number of empty @tic@/@toc@ pairs used to calibrate baseline timer overhead (default: @256@).
    benchCalibrationIters :: !Int,
    -- | Target minimum ticks per batch during adaptive batch sizing (default: @20000@).
    benchMinBatchTicks :: !Word64,
    -- | Upper cap on iterations per batch during adaptive batch sizing (default: @65536@).
    benchMaxBatchSize :: !Int,
    -- | Optional fixed batch size overriding adaptive calibration (default: 'Nothing').
    benchFixedBatchSize :: !(Maybe Int),
    -- | Upper MAD multiplier for gating extreme OS interrupt / page-fault spikes (default: @5.0@).
    benchMadThreshold :: !Double,
    -- | Posterior credible interval mass in @(0, 1)@ (default: @0.95@ for a 95% CI).
    benchCredibleMass :: !Double,
    -- | Maximum number of distinct named @tic@/@toc@ pairs tracked per benchmark (default: @32@).
    benchMaxNamedTimers :: !Int,
    -- | Deterministic SplitMix64 seed for reproducible Bayesian bootstrap draws.
    benchSeed :: !Word64
  }
  deriving (Eq, Ord, Show)

-- | Sensible production defaults for cycle-level micro- and meso-benchmarks.
defaultBenchConfig :: BenchConfig
defaultBenchConfig =
  BenchConfig
    { benchWarmupIters = 32,
      benchSamples = 100,
      benchBootstrapDraws = 500,
      benchCalibrationIters = 256,
      benchMinBatchTicks = 20000,
      benchMaxBatchSize = 65536,
      benchFixedBatchSize = Nothing,
      benchMadThreshold = 5.0,
      benchCredibleMass = 0.95,
      benchMaxNamedTimers = 32,
      benchSeed = 0x853c49e6748fea9b
    }

-- | Posterior location estimate and equal-tailed credible interval in raw
-- 'CmdTicToc' ticks per call.
data TimingEstimate e = TimingEstimate
  { estTiming :: !(e Double),
    estCiLower :: !(e Double),
    estCiUpper :: !(e Double)
  }

-- | Unified constraint bundle for pure-Janus statistical benchmarking.
type CmdBenchStats m e =
  ( CmdAlloc m e,
    CmdNamedTimer m e,
    CmdFormat m e Double,
    LayoutElemCtx m e Double,
    LayoutElemCtx m e Int64,
    CmdStorable m e (Ptr Word64),
    ExpFloatingCast e Int64 Double,
    ExpFloatingCast e Word64 Double,
    ExpOrd e Double,
    ExpEq e Double,
    Floating (e Double),
    Fractional (e Double),
    Num (e Double),
    JanusTyped e Double,
    JanusTyped e (Ptr Double),
    JanusTyped e (Ptr Int64),
    JanusTyped e (Ptr Word64)
  )

--------------------------------------------------------------------------------
-- Pure-Janus SplitMix64 PRNG
--------------------------------------------------------------------------------

-- | Advance a pure-Janus 64-bit SplitMix64 PRNG state and return the next
-- pseudorandom 'Word64'.
nextSplitMix64 ::
  (CmdNamedTimer m e) =>
  Ref m e Word64 ->
  m (e Word64)
nextSplitMix64 stateRef = do
  s0 <- readRef stateRef
  s1 <- letM (s0 + 0x9e3779b97f4a7c15)
  writeRef stateRef s1
  z1 <- letM ((s1 `ieor` (s1 `rshift` 30)) * 0xbf58476d1ce4e5b9)
  z2 <- letM ((z1 `ieor` (z1 `rshift` 27)) * 0x94d049bb133111eb)
  letM (z2 `ieor` (z2 `rshift` 31))

-- | Draw an open-interval uniform variate @u in (0, 1)@ strictly bounded away
-- from @0@ and @1@ (@u = ((w >> 11) + 0.5) * 2^-53@) so @-log u@ is always
-- finite and strictly positive.
nextUniform01 ::
  (CmdBenchStats m e) =>
  Ref m e Word64 ->
  m (e Double)
nextUniform01 stateRef = do
  w <- nextSplitMix64 stateRef
  top53 <- letM (w `rshift` 11)
  let inv2Pow53 = 1.0 / 9007199254740992.0
  letM ((toFloating top53 + 0.5) * inv2Pow53)

--------------------------------------------------------------------------------
-- Timer-Overhead & Adaptive Batch-Size Calibration
--------------------------------------------------------------------------------

doubleLit :: (Fractional (e Double)) => Double -> e Double
doubleLit x = fromRational (toRational x)

-- | Measure @benchCalibrationIters@ empty @tic@ -> store -> @toc@ pairs in pure
-- Janus and return the 10th-percentile baseline overhead in ticks.
calibrateTimerOverhead ::
  forall m e.
  (CmdBenchStats m e) =>
  BenchConfig ->
  m (e Double)
calibrateTimerOverhead cfg = do
  let nCal = max 16 (benchCalibrationIters cfg)
      nCalE = fromIntegral nCal :: e Int64
      p10Idx = fromIntegral (max 0 (min (nCal - 1) ((nCal * 10) `div` 100))) :: e Int64
  calBuf <- allocBuf @Double dynamicAlloc nCalE
  rawScratch <- calloc 1 8
  let scratchPtr = fromVoidPtr rawScratch :: e (Ptr Word64)
  -- Brief warmup of the hardware cycle counter
  rangeM 0 16 $ \_ -> do
    Tic t0 <- tic
    poke scratchPtr t0
    Toc _ <- toc
    pure ()
  rangeM 0 nCalE $ \i -> do
    Tic t0 <- tic
    poke scratchPtr t0
    Toc t1 <- toc
    t0Loaded <- peek scratchPtr
    let delta = ifThenElse (t1 `ge` t0Loaded) (t1 - t0Loaded) 0
    writeBuf calBuf i (toFloating delta)
  sortBuf calBuf nCalE
  overhead <- readBuf calBuf p10Idx
  free rawScratch
  freeBuf @Double dynamicAlloc calBuf nCalE
  pure overhead

-- | Determine the per-sample batch size @B >= 1@ so each measured batch spans
-- at least 'benchMinBatchTicks' ticks (or return 'benchFixedBatchSize' when set).
calibrateBatchSize ::
  forall m e.
  (CmdBenchStats m e) =>
  BenchConfig ->
  m () ->
  m (e Int64)
calibrateBatchSize cfg stepOnce =
  case benchFixedBatchSize cfg of
    Just fixedB -> pure (fromIntegral (max 1 fixedB))
    Nothing -> do
      let targetTicks = fromIntegral (benchMinBatchTicks cfg) :: e Word64
          maxB = fromIntegral (max 1 (benchMaxBatchSize cfg)) :: e Int64
      -- Single warmup call before probing batch latency
      stepOnce
      bRef <- newRef (1 :: e Int64)
      triesRef <- newRef (0 :: e Int64)
      whileM true $ \cont -> do
        tries <- readRef triesRef
        curB <- readRef bRef
        ifThenElseM_
          ((tries `ge` 20) `lor` (curB `ge` maxB))
          (writeRef cont false)
          ( do
              Tic t0 <- tic
              rangeM 0 curB $ \_ -> stepOnce
              Toc t1 <- toc
              let elapsed = ifThenElse (t1 `ge` t0) (t1 - t0) 0
              ifThenElseM_
                (elapsed `ge` targetTicks)
                (writeRef cont false)
                ( do
                    let nextB =
                          ifThenElse
                            ((elapsed * 8 `lt` targetTicks) `land` (curB * 4 `le` maxB))
                            (curB * 4)
                            (min' maxB (curB * 2))
                    writeRef bRef nextB
                    writeRef triesRef (tries + 1)
                )
          )
      readRef bRef

--------------------------------------------------------------------------------
-- Robust Bayesian Bootstrap Over MAD-Gated Huber M-Estimators
--------------------------------------------------------------------------------

-- | Compute the posterior location ('estTiming') and equal-tailed credible
-- interval ('estCiLower', 'estCiUpper') from @nSamples@ per-call tick
-- measurements in @rawBuf@ using MAD spike gating followed by Rubin's (1981)
-- Nonparametric Bayesian Bootstrap over a Dirichlet-weighted Huber M-estimator.
estimateTimingPosterior ::
  forall m e.
  (CmdBenchStats m e) =>
  BenchConfig ->
  Ref m e Word64 ->
  ScalarBuf Double e ->
  e Int64 ->
  m (TimingEstimate e)
estimateTimingPosterior cfg rngRef rawBuf nSamples = do
  let nClamped = max' 1 nSamples
      bDraws = max 20 (benchBootstrapDraws cfg)
      bDrawsE = fromIntegral bDraws :: e Int64
      madMult = doubleLit (max 1.0 (benchMadThreshold cfg))
      credMass = max 0.01 (min 0.9999 (benchCredibleMass cfg))
      alpha = (1.0 - credMass) * 0.5
      posLo = alpha * fromIntegral (bDraws - 1)
      loIdx0 = max 0 (min (bDraws - 1) (floor posLo)) :: Int
      loIdx1 = max 0 (min (bDraws - 1) (loIdx0 + 1)) :: Int
      fracLo = doubleLit (posLo - fromIntegral loIdx0)
      posHi = (1.0 - alpha) * fromIntegral (bDraws - 1)
      hiIdx0 = max 0 (min (bDraws - 1) (floor posHi)) :: Int
      hiIdx1 = max 0 (min (bDraws - 1) (hiIdx0 + 1)) :: Int
      fracHi = doubleLit (posHi - fromIntegral hiIdx0)

  sortedBuf <- allocBuf @Double dynamicAlloc nClamped
  devBuf <- allocBuf @Double dynamicAlloc nClamped
  weightsBuf <- allocBuf @Double dynamicAlloc nClamped
  drawsBuf <- allocBuf @Double dynamicAlloc bDrawsE

  -- 1. Copy and sort raw per-call tick samples ascending.
  rangeM 0 nClamped $ \i -> do
    v <- readBuf rawBuf i
    writeBuf sortedBuf i v
  sortBuf sortedBuf nClamped

  -- 2. Compute sample median and normalized MAD (1.4826 * MAD).
  loMid <- letM ((nClamped - 1) `rshift` 1)
  hiMid <- letM (nClamped `rshift` 1)
  medLo <- readBuf sortedBuf loMid
  medHi <- readBuf sortedBuf hiMid
  med <- letM (0.5 * (medLo + medHi))

  rangeM 0 nClamped $ \i -> do
    xi <- readBuf sortedBuf i
    writeBuf devBuf i (abs (xi - med))
  sortBuf devBuf nClamped
  madLo <- readBuf devBuf loMid
  madHi <- readBuf devBuf hiMid
  sigmaMad <- letM (doubleLit 1.4826 * 0.5 * (madLo + madHi))
  scaleFloor <- letM (max' (doubleLit 1.0e-9) (doubleLit 1.0e-4 * abs med))
  robustScale <- letM (max' sigmaMad scaleFloor)
  upperFence <- letM (med + madMult * robustScale)

  -- 3. Gate extreme right-tail OS interrupt spikes while retaining >= ceil(0.8 * N) samples.
  keptCountRef <- newRef (0 :: e Int64)
  rangeM 0 nClamped $ \i -> do
    xi <- readBuf sortedBuf i
    whenM_ (xi `le` upperFence) $ do
      k <- readRef keptCountRef
      writeRef keptCountRef (k + 1)
  rawKept <- readRef keptCountRef
  minRetained <- letM (max' 1 ((nClamped * 8 + 9) `quotient` 10))
  mClean <- letM (min' nClamped (max' minRetained rawKept))
  cHuber <- letM (doubleLit 1.345 * robustScale)

  -- 4. Rubin (1981) Bayesian Bootstrap draws with Dirichlet(1,...,1) weights.
  rangeM 0 bDrawsE $ \b -> do
    wSumRef <- newRef (0.0 :: e Double)
    rangeM 0 mClean $ \i -> do
      u <- nextUniform01 rngRef
      eVal <- letM (negate (log u))
      writeBuf weightsBuf i eVal
      ws <- readRef wSumRef
      writeRef wSumRef (ws + eVal)
    wSum <- readRef wSumRef
    invWSum <- letM (1.0 / wSum)
    rangeM 0 mClean $ \i -> do
      ei <- readBuf weightsBuf i
      writeBuf weightsBuf i (ei * invWSum)

    -- Exact O(M) Dirichlet-weighted median on sorted clean prefix
    cumWRef <- newRef (0.0 :: e Double)
    foundMedRef <- newRef false
    thetaRef <- newRef med
    rangeM 0 mClean $ \i -> do
      wi <- readBuf weightsBuf i
      cw <- readRef cumWRef
      cw' <- letM (cw + wi)
      writeRef cumWRef cw'
      already <- readRef foundMedRef
      whenM_ (lnot already `land` (cw' `ge` 0.5)) $ do
        xi <- readBuf sortedBuf i
        writeRef thetaRef xi
        writeRef foundMedRef true

    -- 3-step IRLS refinement for the Dirichlet-weighted Huber M-estimator
    rangeM 0 3 $ \_ -> do
      curTheta <- readRef thetaRef
      numRef <- newRef (0.0 :: e Double)
      denRef <- newRef (0.0 :: e Double)
      rangeM 0 mClean $ \i -> do
        xi <- readBuf sortedBuf i
        wi <- readBuf weightsBuf i
        r <- letM (abs (xi - curTheta))
        let huberW = ifThenElse (r `le` cHuber) 1.0 (cHuber / r)
        effW <- letM (wi * huberW)
        num <- readRef numRef
        den <- readRef denRef
        writeRef numRef (num + effW * xi)
        writeRef denRef (den + effW)
      numFinal <- readRef numRef
      denFinal <- readRef denRef
      whenM_ (denFinal `gt` 0.0) $
        writeRef thetaRef (numFinal / denFinal)

    thetaDraw <- readRef thetaRef
    writeBuf drawsBuf b thetaDraw

  -- 5. Sort posterior draws and extract posterior mean + equal-tailed CI.
  sortBuf drawsBuf bDrawsE
  drawSumRef <- newRef (0.0 :: e Double)
  rangeM 0 bDrawsE $ \b -> do
    db <- readBuf drawsBuf b
    s <- readRef drawSumRef
    writeRef drawSumRef (s + db)
  drawSum <- readRef drawSumRef
  postMean <- letM (drawSum / toFloating bDrawsE)

  dLo0 <- readBuf drawsBuf (fromIntegral loIdx0)
  dLo1 <- readBuf drawsBuf (fromIntegral loIdx1)
  dHi0 <- readBuf drawsBuf (fromIntegral hiIdx0)
  dHi1 <- readBuf drawsBuf (fromIntegral hiIdx1)
  rawCiLo <- letM (dLo0 + fracLo * (dLo1 - dLo0))
  rawCiHi <- letM (dHi0 + fracHi * (dHi1 - dHi0))
  ciLo <- letM (min' rawCiLo postMean)
  ciHi <- letM (max' rawCiHi postMean)

  freeBuf @Double dynamicAlloc sortedBuf nClamped
  freeBuf @Double dynamicAlloc devBuf nClamped
  freeBuf @Double dynamicAlloc weightsBuf nClamped
  freeBuf @Double dynamicAlloc drawsBuf bDrawsE

  pure
    TimingEstimate
      { estTiming = postMean,
        estCiLower = ciLo,
        estCiUpper = ciHi
      }
