{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Janus.Tensor.Core
  ( test_tensor_core,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Tensor.Algebra (AddOp)
import Janus.Tensor.Compile (buildTensorFn)
import Janus.Tensor.DSL
import Janus.Tensor.IR
import Janus.Tensor.Index
import Janus.Tensor.Linear
import Janus.Tensor.ST
import qualified Linear.V as LV
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

test_tensor_core :: TestTree
test_tensor_core =
  testGroup
    "Janus.Tensor.Core"
    [ testProperty "IxIso bijection & host/embedded parity laws" law_ixIso,
      testProperty "IxPrism partial-inverse & host/embedded parity laws" law_ixPrism,
      testProperty "IxAffine broadcast & window1D host/embedded parity" law_ixAffine,
      testCase "Merkle digest determinism, beta-reduction & eta-contraction" checkMerkleAndBetaEta,
      testCase "Rank-indexed static table synonyms (ScalarTab..Tensor9Tab & TensorTab)" checkRankTabSynonyms,
      testCase "Kmett linear Representable/Additive/Metric/Trace & V2..V4/M33/V interop" checkLinearInterop,
      testCase "TensorST and AccumM construct well-typed Merkle-hashed IR nodes" checkStAndAccumIR
    ]

checkIsoLaws :: forall i j. (IxSet i, IxSet j) => IxIso i j -> PropertyT IO ()
checkIsoLaws iso = do
  let env = emptyDimEnv
      nSrc = sixEvalSize env (isoSrc iso)
      nDst = sixEvalSize env (isoDst iso)
  nSrc === nDst
  k <- forAll $ Gen.int (Range.linear 0 (nSrc - 1))
  let fwd = evalIsoOrd env iso k
      bwd = evalIsoOrd env (invIso iso) fwd
      fwdE = fromIntegral (runIdentity (evalIsoOrdE env iso (Identity (fromIntegral k :: Int64)))) :: Int
      bwdE = fromIntegral (runIdentity (evalIsoOrdE env (invIso iso) (Identity (fromIntegral fwd :: Int64)))) :: Int
  assert (fwd >= 0 && fwd < nDst)
  bwd === k
  fwdE === fwd
  bwdE === k

law_ixIso :: Property
law_ixIso = property $ do
  -- ixSwap on (3, 5)
  checkIsoLaws (ixSwap :: IxIso ('IxProd ('IxFin 3) ('IxFin 5)) ('IxProd ('IxFin 5) ('IxFin 3)))
  -- ixAssocL / ixAssocR on ((2, 3), 4)
  checkIsoLaws
    ( ixAssocR ::
        IxIso
          ('IxProd ('IxProd ('IxFin 2) ('IxFin 3)) ('IxFin 4))
          ('IxProd ('IxFin 2) ('IxProd ('IxFin 3) ('IxFin 4)))
    )
  checkIsoLaws
    ( ixAssocL ::
        IxIso
          ('IxProd ('IxFin 2) ('IxProd ('IxFin 3) ('IxFin 4)))
          ('IxProd ('IxProd ('IxFin 2) ('IxFin 3)) ('IxFin 4))
    )
  -- ixRev on Fin 7
  checkIsoLaws (ixRev :: IxIso ('IxFin 7) ('IxFin 7))
  -- ixShift on Fin 11
  sh <- forAll $ Gen.int (Range.linear (-25) 25)
  checkIsoLaws (ixShift sh :: IxIso ('IxFin 11) ('IxFin 11))
  -- ixReshape (3, 4) <-> (2, 6)
  checkIsoLaws
    ( ixReshape ::
        IxIso
          ('IxProd ('IxFin 3) ('IxFin 4))
          ('IxProd ('IxFin 2) ('IxFin 6))
    )
  -- Composed iso: swap >>> rev >>> shift
  let compIso =
        ixCompIso
          (ixCompIso (ixSwap :: IxIso ('IxProd ('IxFin 3) ('IxFin 4)) ('IxProd ('IxFin 4) ('IxFin 3))) ixRev)
          (ixShift sh)
  checkIsoLaws compIso

checkPrismLaws :: forall i j. (IxSet i, IxSet j) => IxPrism i j -> PropertyT IO ()
checkPrismLaws p = do
  let env = emptyDimEnv
      nSrc = sixEvalSize env (prismSrc p)
      nDst = sixEvalSize env (prismDst p)
  -- Forward then backward is identity
  k <- forAll $ Gen.int (Range.linear 0 (nSrc - 1))
  let dst = evalPrismForwardOrd env p k
      dstE = fromIntegral (runIdentity (evalPrismForwardOrdE env p (Identity (fromIntegral k :: Int64)))) :: Int
  assert (dst >= 0 && dst < nDst)
  dstE === dst
  evalPrismBackwardOrd env p dst === Just k
  let (okE, backE) = evalPrismBackwardOrdE env p (Identity (fromIntegral dst :: Int64))
  runIdentity okE === True
  fromIntegral (runIdentity backE) === k
  -- Backward on arbitrary dst agrees with embedded evaluation and round-trips when Just
  j <- forAll $ Gen.int (Range.linear 0 (nDst - 1))
  let mBack = evalPrismBackwardOrd env p j
      (okJ, idxJ) = evalPrismBackwardOrdE env p (Identity (fromIntegral j :: Int64))
  case mBack of
    Nothing -> runIdentity okJ === False
    Just k' -> do
      runIdentity okJ === True
      fromIntegral (runIdentity idxJ) === k'
      evalPrismForwardOrd env p k' === j

law_ixPrism :: Property
law_ixPrism = property $ do
  -- ixSlice
  off <- forAll $ Gen.int (Range.linear 0 6)
  checkPrismLaws (ixSlice off :: IxPrism ('IxFin 4) ('IxFin 10))
  -- ixStride
  checkPrismLaws (ixStride 1 2 :: IxPrism ('IxFin 5) ('IxFin 11))
  -- ixInL / ixInR
  checkPrismLaws (ixInL :: IxPrism ('IxFin 3) ('IxSum ('IxFin 3) ('IxFin 5)))
  checkPrismLaws (ixInR :: IxPrism ('IxFin 5) ('IxSum ('IxFin 3) ('IxFin 5)))
  -- ixDiag
  checkPrismLaws (ixDiag :: IxPrism ('IxFin 6) ('IxProd ('IxFin 6) ('IxFin 6)))
  -- Composed prism: slice then diag (standard categorical composition order)
  checkPrismLaws
    ( ixCompPrism
        (ixDiag :: IxPrism ('IxFin 6) ('IxProd ('IxFin 6) ('IxFin 6)))
        (ixSlice 1 :: IxPrism ('IxFin 4) ('IxFin 6))
    )

law_ixAffine :: Property
law_ixAffine = property $ do
  let env = emptyDimEnv
      bcast = ixBroadcast :: IxAffine ('IxProd ('IxFin 4) ('IxFin 5)) 'IxUnit
  kB <- forAll $ Gen.int (Range.linear 0 19)
  let (okH, dH) = evalAffineForwardOrd env bcast kB
      (okB, dstB) = evalAffineForwardOrdE env bcast (Identity (fromIntegral kB :: Int64))
  runIdentity okB === okH
  okH === True
  dH === 0
  fromIntegral (runIdentity dstB) === dH
  -- 1D sliding window with stride=2, dilation=1, pad=1 on input length 8, kernel 3, output 4
  let win = ixWindow1D 2 1 1 :: IxAffine ('IxProd ('IxFin 4) ('IxFin 3)) ('IxFin 8)
  kW <- forAll $ Gen.int (Range.linear 0 11)
  let (okWH, dWH) = evalAffineForwardOrd env win kW
      (okW, dstW) = evalAffineForwardOrdE env win (Identity (fromIntegral kW :: Int64))
  runIdentity okW === okWH
  if okWH
    then do
      fromIntegral (runIdentity dstW) === dWH
      assert (dWH >= 0 && dWH < 8)
    else pure ()

checkMerkleAndBetaEta :: Assertion
checkMerkleAndBetaEta = do
  let idTab :: Tab ('IxFin 8) (Expr Double) -> Tab ('IxFin 8) (Expr Double)
      idTab x = x
      -- Eta-contraction: forIx (\i -> x ! i) collapses to the exact same NInput digest
      etaTab :: Tab ('IxFin 8) (Expr Double) -> Tab ('IxFin 8) (Expr Double)
      etaTab x = forIx (\i -> x ! i)
      dX = rawNodeDigest (buildTensorFn idTab)
      dEta = rawNodeDigest (buildTensorFn etaTab)
  assertEqual "eta-contraction preserves exact input node digest" dX dEta
  -- Alpha-equivalence: structurally identical expressions with different Haskell names hash identically
  let t1, t2, t3 :: Tab ('IxFin 8) (Expr Double) -> Tab ('IxFin 8) (Expr Double)
      t1 x = forIx @('IxFin 8) (\i -> (x ! i) * 2 + 1)
      t2 y = forIx @('IxFin 8) (\j -> (y ! j) * 2 + 1)
      t3 y = forIx @('IxFin 8) (\j -> (y ! j) * 3 + 1)
  assertEqual "alpha-equivalent tables have identical Merkle digests"
    (rawNodeDigest (buildTensorFn t1))
    (rawNodeDigest (buildTensorFn t2))
  assertBool "distinct tables have distinct Merkle digests"
    (rawNodeDigest (buildTensorFn t1) /= rawNodeDigest (buildTensorFn t3))
  -- Beta-reduction: (forIx f) ! ix directly applies f without creating an intermediate NFor node
  let fusedFn :: Tab ('IxFin 8) (Expr Double) -> Expr Double
      fusedFn x = (forIx @('IxFin 8) (\i -> (x ! i) + 5)) ! ixConst 3
  case buildTensorFn fusedFn of
    NFor _ SIxUnit _ (EBinOp _ BAdd (EIndex _ (NInput _ _ _) (IxConst 3 _)) (ELit _ 5.0)) -> pure ()
    _ -> assertFailure "Expected beta-reduced EBinOp over NInput without intermediate NFor"

checkRankTabSynonyms :: Assertion
checkRankTabSynonyms = do
  -- Verify every rank 0..9 synonym is definitionally identical to TensorTab / ShapeToIx
  let _r0 :: ScalarTab Double :~: TensorTab '[] Double
      _r0 = Refl
      _r1 :: VectorTab 2 Double :~: TensorTab '[2] Double
      _r1 = Refl
      _r2 :: MatrixTab 2 3 Double :~: TensorTab '[2, 3] Double
      _r2 = Refl
      _r3 :: Tensor3Tab 2 3 4 Double :~: TensorTab '[2, 3, 4] Double
      _r3 = Refl
      _r4 :: Tensor4Tab 1 2 3 4 Double :~: TensorTab '[1, 2, 3, 4] Double
      _r4 = Refl
      _r5 :: Tensor5Tab 1 2 3 4 5 Double :~: TensorTab '[1, 2, 3, 4, 5] Double
      _r5 = Refl
      _r6 :: Tensor6Tab 1 2 3 4 5 6 Double :~: TensorTab '[1, 2, 3, 4, 5, 6] Double
      _r6 = Refl
      _r7 :: Tensor7Tab 1 2 3 4 5 6 7 Double :~: TensorTab '[1, 2, 3, 4, 5, 6, 7] Double
      _r7 = Refl
      _r8 :: Tensor8Tab 1 2 3 4 5 6 7 8 Double :~: TensorTab '[1, 2, 3, 4, 5, 6, 7, 8] Double
      _r8 = Refl
      _r9 :: Tensor9Tab 1 2 3 4 5 6 7 8 9 Double :~: TensorTab '[1, 2, 3, 4, 5, 6, 7, 8, 9] Double
      _r9 = Refl
      t9Fn ::
        Tensor9Tab 1 2 1 2 1 2 1 2 3 (Expr Double) ->
        Tensor9Tab 1 2 1 2 1 2 1 2 3 (Expr Double)
      t9Fn t = forIx (\i -> (t ! i) + 1.0)
      n9 = buildTensorFn t9Fn
  assertEqual "Tensor9Tab static element count" (Just 48) (sixStaticSize (rawNodeShape n9))

checkLinearInterop :: Assertion
checkLinearInterop = do
  -- Representable + Additive on Tab ('IxFin 3)
  let u = v3ToTab (V3 (1 :: Expr Double) 2 3)
      v = v3ToTab (V3 (4 :: Expr Double) 5 6)
      w = (2 :: Expr Double) *^ u ^+^ v ^-^ zero
      -- Metric.dot on host Tab ('IxFin 3) Double
      uHost = tabulate (\ix -> case ix of IxConst k _ -> fromIntegral (k + 1 :: Int); _ -> 0) :: Tab ('IxFin 3) Double
      vHost = tabulate (\ix -> case ix of IxConst k _ -> fromIntegral (k + 4 :: Int); _ -> 0) :: Tab ('IxFin 3) Double
  assertEqual "Linear.Metric.dot on host Tab" (1 * 4 + 2 * 5 + 3 * 6 :: Double) (dot uHost vHost)
  -- Trace on host Tab ('IxFin 3) (Tab ('IxFin 3) Double)
  let mHost = tabulate (\i -> tabulate (\j -> case (i, j) of (IxConst r _, IxConst c _) -> fromIntegral (r * 3 + c + 1 :: Int); _ -> 0)) :: Tab ('IxFin 3) (Tab ('IxFin 3) Double)
  assertEqual "Linear.Trace.trace on host Tab" (1 + 5 + 9 :: Double) (trace mHost)
  -- Embedded dotT / matmulT / traceT produce O(1) Reduce nodes
  let dNode = buildExpr (dotT u v)
  case dNode of
    EReduce {} -> pure ()
    _ -> assertFailure "Expected dotT to emit a single EReduce node"
  let m33 = m33ToTab (V3 (V3 1 0 0) (V3 0 2 0) (V3 0 0 3) :: M33 (Expr Double))
      trNode = buildExpr (traceT m33)
  case trNode of
    EReduce {} -> pure ()
    _ -> assertFailure "Expected traceT to emit a single EReduce node"
  -- Linear.V.V interop round-trip shape check
  let lv = LV.V (V.fromList [1 :: Expr Double, 2, 3, 4, 5]) :: LV.V 5 (Expr Double)
      t5 = linearVToTab lv
      LV.V vecBack = tabToLinearV t5
  assertEqual "Linear.V round-trip vector length" 5 (V.length vecBack)
  -- Ensure w materializes cleanly
  assertEqual "w shape" (Just 3) (sixStaticSize (rawNodeShape (buildNode (materialize w))))

checkStAndAccumIR :: Assertion
checkStAndAccumIR = do
  let stFn :: Tab ('IxFin 4) (Expr Double) -> Tab ('IxFin 4) (Expr Double)
      stFn x = runTensorST $ do
        buf <- newMTab x
        acc <- newTRef (0 :: Expr Double)
        forST_ @('IxFin 4) $ \i -> do
          v <- readMTab buf i
          modifyTRef acc (+ v)
          s <- readTRef acc
          writeMTab buf i s
        freezeMTab buf
      accFn :: Tab ('IxFin 4) (Expr Double) -> Tab ('IxFin 4) (Expr Double)
      accFn x = runAccum (Proxy @AddOp) $ \ref ->
        parForAccum @('IxFin 4) $ \i ->
          accumWrite ref i (x ! i)
  case buildTensorFn stFn of
    NRunST _ _ prog -> assertEqual "TensorST emitted expected top-level stmts" 3 (length (stStmts prog))
    _ -> assertFailure "Expected NRunST node"
  case buildTensorFn accFn of
    NRunAccum _ _ _ prog -> assertEqual "AccumM emitted expected top-level stmts" 1 (length (accumStmts prog))
    _ -> assertFailure "Expected NRunAccum node"
