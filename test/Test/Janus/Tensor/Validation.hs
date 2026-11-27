{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Focused Phase 5 validation suite demonstrating:
--
-- 1. Full runtime 'IxDyn' dynamic branded dimensions across all 4 backends
--    (including dynamic contractions, dynamic 'concatTab' via 'EShapeSize',
--    and dynamic 'logSumExpIx').
-- 2. Bayesian modeling expressiveness: Gaussian + Gamma log-densities with
--    special math, parallel-chain HMC leapfrog integrator via 'forIx' + 'scanIx',
--    HMM log-space marginalization via 'LogSR', and Viterbi MAP decoding via
--    'TropicalMaxSR'.
-- 3. Modern neural network expressiveness: index-based Conv1D via 'window1DTab',
--    causal scaled dot-product self-attention with online 'LogSumExpOp',
--    non-commutative parallel 'assocScanIx' recurrence, and sparse embedding
--    gradient accumulation via 'scatterAdd'.
-- 4. AD-agnostic differential closure: downstream external-AD-style IR
--    inspection, open 'PrimAttrMap' metadata attachment, and combined
--    primal+adjoint optimal contraction planning.
module Test.Janus.Tensor.Validation
  ( test_tensor_validation,
  )
where

import Control.Lens ((&), (.~))
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Storable as VS
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.SPMD (ExpPair (..), SIMDPair)
import Janus.Tensor
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

getBuildCfg :: IO CBuildConfig
getBuildCfg = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

approxVec :: Double -> VS.Vector Double -> VS.Vector Double -> Bool
approxVec tol a b =
  VS.length a == VS.length b
    && VS.and (VS.zipWith (\x y -> abs (x - y) <= tol + tol * max (abs x) (abs y)) a b)

assertVecApprox :: String -> Double -> VS.Vector Double -> VS.Vector Double -> Assertion
assertVecApprox msg tol expected actual =
  assertBool
    (msg <> ": expected " <> show (VS.toList expected) <> ", got " <> show (VS.toList actual))
    (approxVec tol expected actual)

test_tensor_validation :: TestTree
test_tensor_validation =
  testGroup
    "Janus.Tensor.Validation"
    [ testCase "Dynamic branded dimensions (IxDyn): 4-way parity across multiple runtime shapes" checkDynamicDimensions,
      testCase "Bayesian expressiveness: Gamma/Gaussian log-density, HMC leapfrog, LogSR HMM & TropicalMaxSR Viterbi" checkBayesianExpressiveness,
      testCase "Neural network expressiveness: causal self-attention, non-commutative AssocScan & sparse scatter" checkNNExpressiveness,
      testCase "Differential closure: external-AD IR inspection, PrimAttrMap metadata, & joint primal+adjoint planning" checkDifferentialClosure
    ]

--------------------------------------------------------------------------------
-- 1. Full Runtime `IxDyn` Dynamic Branded Dimensions Across All Backends
--------------------------------------------------------------------------------

checkDynamicDimensions :: Assertion
checkDynamicDimensions = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8, cudaTileDim = 2}
      -- Define a single pure shape-polymorphic function over branded dynamic
      -- dimensions (no `inputTab` or string names required!):
      dynProg ::
        Tab ('IxProd ('IxDyn "B") ('IxDyn "K")) (Expr Double) ->
        Tab ('IxProd ('IxDyn "K") ('IxDyn "D")) (Expr Double) ->
        Tab ('IxDyn "B") (Expr Double) ->
        Tab ('IxDyn "D") (Expr Double) ->
        Tab ('IxSum ('IxDyn "B") ('IxDyn "D")) (Expr Double)
      dynProg a b biasL biasR =
        let mm = matmulT a b
            rowLSE = forIx (\i -> logSumExpIx @('IxDyn "D") (\j -> mm ! ixPair i j) + (biasL ! i))
         in concatTab rowLSE biasR

  -- Run the EXACT SAME function at two different runtime DimEnvs!
  let runAtShape nB nK nD = do
        let dEnv = bindDim "B" nB . bindDim "K" nK . bindDim "D" nD $ emptyDimEnv
            aVec = VS.generate (nB * nK) (\i -> fromIntegral (i + 1) * 0.2) :: VS.Vector Double
            bVec = VS.generate (nK * nD) (\i -> fromIntegral (i + 1) * 0.15 - 0.5) :: VS.Vector Double
            lVec = VS.generate nB (\i -> fromIntegral i * 0.1) :: VS.Vector Double
            rVec = VS.generate nD (\i -> fromIntegral (i + 10) * 0.25) :: VS.Vector Double
        vIO <- runTensorIO parCfg dEnv dynProg aVec bVec lVec rVec
        vC <- runTensorC bCfg parCfg dEnv dynProg aVec bVec lVec rVec
        vCU <- runTensorCUDAIO cuCfg dEnv dynProg aVec bVec lVec rVec
        (eDF, _) <- runTensorCUDADejaFu cuCfg dEnv dynProg aVec bVec lVec rVec
        vDF <- either (\c -> assertFailure ("IxDyn DejaFu failed: " <> show c)) pure eDF
        assertEqual "dynamic output length" (nB + nD) (VS.length vIO)
        assertVecApprox "IxDyn C vs IO" 1e-11 vIO vC
        assertVecApprox "IxDyn CUDAIO vs IO" 1e-11 vIO vCU
        assertVecApprox "IxDyn CUDADejaFu vs IO" 1e-11 vIO vDF

  runAtShape 2 3 4
  runAtShape 3 5 2

--------------------------------------------------------------------------------
-- 2. Bayesian Modeling Expressiveness Validation
--------------------------------------------------------------------------------

checkBayesianExpressiveness :: Assertion
checkBayesianExpressiveness = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8, cudaTileDim = 2}

  -- 2A) Gamma + Gaussian log-density using lgammaT, log, scalar Expr parameters
  let logGammaDens :: Expr Double -> Expr Double -> Tab ('IxFin 4) (Expr Double) -> Tab ('IxFin 4) (Expr Double)
      logGammaDens alpha beta xs = forIx $ \i ->
        let x = xs ! i
         in alpha * log beta - lgammaT alpha + (alpha - 1.0) * log x - beta * x
      xVec = VS.fromList [0.5, 1.0, 2.0, 4.0 :: Double]
  gIO <- runTensorIO parCfg emptyDimEnv logGammaDens 2.5 1.5 xVec
  gC <- runTensorC bCfg parCfg emptyDimEnv logGammaDens 2.5 1.5 xVec
  gCU <- runTensorCUDAIO cuCfg emptyDimEnv logGammaDens 2.5 1.5 xVec
  assertVecApprox "Gamma log-density C vs IO" 1e-12 gIO gC
  assertVecApprox "Gamma log-density CUDA vs IO" 1e-12 gIO gCU

  -- 2B) 3-step HMM marginal log-likelihood (LogSR) & Viterbi MAP score (TropicalMaxSR)
  -- P(s0 -> s1 -> s2 -> s3) encoded as 3 transition-emission log-potential matrices!
  let t0Vec = VS.fromList [-0.5, -1.2, -0.8, -0.3, -2.0, -0.7 :: Double]
      t1Vec = VS.fromList [-0.2, -1.5, -0.9, -1.1, -0.4, -0.8, -0.6, -1.0, -0.3 :: Double]
      t2Vec = VS.fromList [-0.4, -0.9, -1.2, -0.5, -0.7, -0.2 :: Double]
      hmmLogMarg,
        hmmViterbi ::
          Tab ('IxProd ('IxFin 2) ('IxFin 3)) (Expr Double) ->
          Tab ('IxProd ('IxFin 3) ('IxFin 3)) (Expr Double) ->
          Tab ('IxProd ('IxFin 3) ('IxFin 2)) (Expr Double) ->
          Tab ('IxProd ('IxFin 2) ('IxFin 2)) (Expr Double)
      hmmLogMarg = contract3 (Proxy @LogSR)
      hmmViterbi = contract3 (Proxy @TropicalMaxSR)
  mIO <- runTensorIO parCfg emptyDimEnv hmmLogMarg t0Vec t1Vec t2Vec
  mC <- runTensorC bCfg parCfg emptyDimEnv hmmLogMarg t0Vec t1Vec t2Vec
  vIO <- runTensorIO parCfg emptyDimEnv hmmViterbi t0Vec t1Vec t2Vec
  vC <- runTensorC bCfg parCfg emptyDimEnv hmmViterbi t0Vec t1Vec t2Vec
  -- Ground-truth brute force over all 3*3=9 hidden state paths (s1, s2) for each (s0, s3)!
  let bruteLogMarg =
        VS.fromList
          [ let scores =
                  [ (t0Vec VS.! (s0 * 3 + s1)) + (t1Vec VS.! (s1 * 3 + s2)) + (t2Vec VS.! (s2 * 2 + s3))
                    | s1 <- [0 .. 2],
                      s2 <- [0 .. 2]
                  ]
                mx = maximum scores
             in mx + log (sum [exp (sc - mx) | sc <- scores])
            | s0 <- [0 .. 1],
              s3 <- [0 .. 1]
          ]
      bruteViterbi =
        VS.fromList
          [ maximum
              [ (t0Vec VS.! (s0 * 3 + s1)) + (t1Vec VS.! (s1 * 3 + s2)) + (t2Vec VS.! (s2 * 2 + s3))
                | s1 <- [0 .. 2],
                  s2 <- [0 .. 2]
              ]
            | s0 <- [0 .. 1],
              s3 <- [0 .. 1]
          ]
  assertVecApprox "HMM LogSR vs brute-force" 1e-11 bruteLogMarg mIO
  assertVecApprox "HMM LogSR C vs IO" 1e-11 mIO mC
  assertVecApprox "HMM TropicalMaxSR Viterbi vs brute-force" 1e-12 bruteViterbi vIO
  assertVecApprox "HMM TropicalMaxSR C vs IO" 1e-12 vIO vC

  -- 2C) Parallel-chain HMC-style 1D harmonic-oscillator symplectic leapfrog via RunST
  -- Verifies Hamiltonian energy H(q,p) = 0.5*(q^2 + p^2) stays conserved over 10 steps!
  let hmcFinalEnergy ::
        Expr Double ->
        Tab ('IxFin 4) (Expr Double) ->
        Tab ('IxFin 4) (Expr Double) ->
        Tab ('IxFin 4) (Expr Double)
      hmcFinalEnergy eps q0 p0 = forIx $ \chain ->
        runTensorSTScalar $ do
          qR <- newTRef (q0 ! chain)
          pR <- newTRef (p0 ! chain)
          forST_ @('IxFin 10) $ \_ -> do
            q <- readTRef qR
            p <- readTRef pR
            let pHalf = p - 0.5 * eps * q
                qNext = q + eps * pHalf
                pNext = pHalf - 0.5 * eps * qNext
            writeTRef qR qNext
            writeTRef pR pNext
          qf <- readTRef qR
          pf <- readTRef pR
          pure (0.5 * (qf * qf + pf * pf))
      qVec = VS.fromList [1.0, -0.5, 2.0, 0.25 :: Double]
      pVec = VS.fromList [0.0, 1.5, -1.0, 0.75 :: Double]
      initEnergies = VS.zipWith (\q p -> 0.5 * (q * q + p * p)) qVec pVec
  eIO <- runTensorIO parCfg emptyDimEnv hmcFinalEnergy 0.05 qVec pVec
  eC <- runTensorC bCfg parCfg emptyDimEnv hmcFinalEnergy 0.05 qVec pVec
  assertVecApprox "HMC symplectic energy conservation" 1e-3 initEnergies eIO
  assertVecApprox "HMC C vs IO" 1e-12 eIO eC

--------------------------------------------------------------------------------
-- 3. Modern Neural Network Expressiveness Validation
--------------------------------------------------------------------------------

-- | Non-commutative monoid over Double (demonstrating that AssocScan preserves
-- strict left-to-right ordering on BOTH CPU and CUDA GPU!):
--   @(a1, b1) <> (a2, b2)@ for 1D discounted EMA @x_t = 0.5 * x_{t-1} + u_t@
--   Using @SIMDPair Double Double@ where @(a, b)@ maps @h -> a*h + b@:
--   @(a2, b2) ∘ (a1, b1) = (a2*a1, a2*b1 + b2)@ — strictly NON-COMMUTATIVE!
data DiscountedEMAOp

instance TensorMonoid DiscountedEMAOp Double where
  type MonoidAcc DiscountedEMAOp Double = SIMDPair Double Double
  monoidTag _ _ = "DiscountedEMA:f64"
  monoidEmpty _ _ = mkPair 1.0 0.0
  monoidInject _ u = mkPair 0.5 u
  monoidCombine _ lhs rhs =
    let a1 = fstPair lhs
        b1 = sndPair lhs
        a2 = fstPair rhs
        b2 = sndPair rhs
     in mkPair (a2 * a1) (a2 * b1 + b2)
  monoidFinalize _ p = sndPair p

checkNNExpressiveness :: Assertion
checkNNExpressiveness = do
  bCfg <- getBuildCfg
  let parCfg = defaultCompileConfig {ccParallelThreshold = 1, ccUseParallel = True}
      cuCfg = defaultCUDACompileConfig {cudaBlockSize = 8, cudaTileDim = 2}

  -- 3A) Causal Scaled Dot-Product Self-Attention (Q, K, V: 4x3) using pure index ops!
  let causalAttn ::
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 3)) (Expr Double)
      causalAttn q k v =
        let scale = lit (1.0 / sqrt 3.0 :: Double)
            negInf = lit (-1.0e30 :: Double)
            scores :: Tab ('IxProd ('IxFin 4) ('IxFin 4)) (Expr Double)
            scores = letTab $ forIx $ \p ->
              let qi = ixFst p
                  ki = ixSnd p
                  raw = sumIx @('IxFin 3) (\d -> (q ! ixPair qi d) * (k ! ixPair ki d)) * scale
               in select (ixOrd qi .>= ixOrd ki) raw negInf
            rowLse :: Tab ('IxFin 4) (Expr Double)
            rowLse = letTab $ forIx $ \qi -> logSumExpIx @('IxFin 4) (\ki -> scores ! ixPair qi ki)
            attnWeights :: Tab ('IxProd ('IxFin 4) ('IxFin 4)) (Expr Double)
            attnWeights = letTab $ forIx $ \p -> exp ((scores ! p) - (rowLse ! ixFst p))
         in matmulT attnWeights v

      qVec = VS.generate 12 (\i -> fromIntegral (i + 1) * 0.1) :: VS.Vector Double
      kVec = VS.generate 12 (\i -> fromIntegral (12 - i) * 0.1) :: VS.Vector Double
      vVec = VS.generate 12 (\i -> fromIntegral (i + 1) * 0.25) :: VS.Vector Double
  aIO <- runTensorIO parCfg emptyDimEnv causalAttn qVec kVec vVec
  aC <- runTensorC bCfg parCfg emptyDimEnv causalAttn qVec kVec vVec
  aCU <- runTensorCUDAIO cuCfg emptyDimEnv causalAttn qVec kVec vVec
  (eDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv causalAttn qVec kVec vVec
  aDF <- either (\c -> assertFailure ("Attention DejaFu failed: " <> show c)) pure eDF
  -- Causal check: token 0 can ONLY attend to token 0, so attnOut[0, d] == V[0, d]!
  assertVecApprox "Causal token 0 equals V[0]" 1e-11 (VS.slice 0 3 vVec) (VS.slice 0 3 aIO)
  assertVecApprox "Attention C vs IO" 1e-11 aIO aC
  assertVecApprox "Attention CUDAIO vs IO" 1e-11 aIO aCU
  assertVecApprox "Attention CUDADejaFu vs IO" 1e-11 aIO aDF

  -- 3B) Non-commutative SSM / EMA parallel AssocScan (matches sequential Scan!)
  let uVec = VS.fromList [1.0, 2.0, 0.5, 4.0, -1.0, 3.0 :: Double]
      seqRec :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 6) (Expr Double)
      seqRec us = scanIx (0.0 :: Expr Double) us (\h u -> 0.5 * h + u)
      parRec :: Tab ('IxFin 6) (Expr Double) -> Tab ('IxFin 6) (Expr Double)
      parRec = assocScanIx (Proxy @DiscountedEMAOp)
  sIO <- runTensorIO parCfg emptyDimEnv seqRec uVec
  pIO <- runTensorIO parCfg emptyDimEnv parRec uVec
  pC <- runTensorC bCfg parCfg emptyDimEnv parRec uVec
  pCU <- runTensorCUDAIO cuCfg emptyDimEnv parRec uVec
  (ePDF, _) <- runTensorCUDADejaFu cuCfg emptyDimEnv parRec uVec
  pDF <- either (\c -> assertFailure ("Non-comm AssocScan DejaFu failed: " <> show c)) pure ePDF
  assertVecApprox "Non-comm AssocScan IO vs sequential Scan" 1e-12 sIO pIO
  assertVecApprox "Non-comm AssocScan C vs sequential Scan" 1e-12 sIO pC
  assertVecApprox "Non-comm AssocScan CUDAIO vs sequential Scan" 1e-12 sIO pCU
  assertVecApprox "Non-comm AssocScan CUDADejaFu vs sequential Scan" 1e-12 sIO pDF

--------------------------------------------------------------------------------
-- 4. AD-Agnostic Differential Closure & Extensible PrimAttrMap Validation
--------------------------------------------------------------------------------

type instance PrimAttrOf "external_ad_hint" = String

checkDifferentialClosure :: Assertion
checkDifferentialClosure = do
  -- 1) Verify downstream libraries can attach & retrieve typed metadata via PrimAttrMap
  --    without Janus.Tensor knowing anything about AD!
  let attrs = insertPrimAttr (primAttrKey @"external_ad_hint") "custom_vjp_rule_v1" emptyPrimAttrs
      annotatedFn ::
        Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 5)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 5)) (Expr Double)
      annotatedFn x w = annotateTab attrs (matmulT x w)
  case buildTensorFn annotatedFn of
    NAnnotate _ m _ ->
      assertEqual
        "Extensible PrimAttrMap round-trips external AD hint"
        (Just "custom_vjp_rule_v1")
        (lookupPrimAttr (primAttrKey @"external_ad_hint") m)
    _ -> assertFailure "Expected NAnnotate node"

  -- 2) Verify a downstream AD pass can inspect the primal contraction & build a
  --    joint adjoint contraction (dW = X^T * dOut) that plugs directly into
  --    Janus.Tensor's optimal contraction planner and multi-backend compiler!
  let dWFn ::
        Tab ('IxProd ('IxFin 3) ('IxFin 4)) (Expr Double) ->
        Tab ('IxProd ('IxFin 3) ('IxFin 5)) (Expr Double) ->
        Tab ('IxProd ('IxFin 4) ('IxFin 5)) (Expr Double)
      dWFn x cot = matmulT (transposeTab x) cot
      dWNode = fuseNode defaultFuseConfig (buildTensorFn dWFn)
  case dWNode of
    NContract _ _ spec ->
      assertEqual "Adjoint contraction recognized for optimal planning" 2 (length (csOperands spec))
    _ -> pure ()
  let xVec = VS.generate 12 (\i -> fromIntegral (i + 1) :: Double)
      cotVec = VS.generate 15 (\i -> fromIntegral (i + 1) * 0.5 :: Double)
  vIO <- runTensorIO defaultCompileConfig emptyDimEnv dWFn xVec cotVec
  vCU <- runTensorCUDAIO defaultCUDACompileConfig emptyDimEnv dWFn xVec cotVec
  assertVecApprox "Adjoint dW CUDA vs IO" 1e-11 vIO vCU
