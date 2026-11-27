{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hypergraph representation, binary contraction trees, axis classification
-- (@Batch@ / @M@ / @N@ / @K@), and analytical / Roofline cost models for
-- optimal Einstein tensor contractions.
module Janus.Tensor.Contract.Hypergraph
  ( -- * Hypergraph Types
    IndexId,
    HyperEdge (..),
    Hypergraph (..),
    fromContractSpec,
    dimProduct,

    -- * Binary Contraction Trees & Axis Classification
    BinaryStepMeta (..),
    ContractionTree (..),
    classifyBinaryStep,
    treeLabels,
    treeLeaves,
    treeSignature,
    treePretty,

    -- * Analytical & GPU Roofline Cost Models
    CostModel (..),
    PlanMetrics (..),
    flopCostModel,
    cpuCostModel,
    gpuRooflineCostModel,
    evalTreeMetrics,
  )
where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Foreign.Storable (Storable (sizeOf))
import Janus.Tensor.IR (ContractOperand (..), ContractSpec (..))
import Janus.Tensor.Index (DimEnv, DimSpec (..), lookupDim)

type IndexId = Int

-- | A single tensor operand in the contraction hypergraph.
data HyperEdge = HyperEdge
  { heId :: !Int,
    heLabels :: ![IndexId]
  }
  deriving stock (Eq, Ord, Show)

-- | Contraction hypergraph with concrete (static or 'DimEnv'-resolved) axis sizes.
data Hypergraph = Hypergraph
  { hgVerts :: !(IM.IntMap Int),
    hgEdges :: !(IM.IntMap HyperEdge),
    hgOutLabels :: ![IndexId],
    hgElemBytes :: !Int
  }
  deriving stock (Eq, Show)

resolveDim :: DimEnv -> DimSpec -> Int
resolveDim _ (DimStatic n) = n
resolveDim env (DimDyn nm) = lookupDim nm env

-- | Build a concrete 'Hypergraph' from an IR 'ContractSpec' and runtime 'DimEnv'.
fromContractSpec :: forall i a. (Storable a) => DimEnv -> ContractSpec i a -> Hypergraph
fromContractSpec env spec =
  let verts =
        IM.fromList
          [ (vid, resolveDim env ds)
            | (vid, ds) <- Map.toList (csVertices spec)
          ]
      edges =
        IM.fromList
          [ (eid, HyperEdge eid ls)
            | (eid, ContractOperand _ ls) <- zip [0 ..] (csOperands spec)
          ]
   in Hypergraph
        { hgVerts = verts,
          hgEdges = edges,
          hgOutLabels = csOutLabels spec,
          hgElemBytes = sizeOf (undefined :: a)
        }

dimProduct :: IM.IntMap Int -> [IndexId] -> Double
dimProduct verts =
  foldr (\v acc -> acc * fromIntegral (IM.findWithDefault 1 v verts)) 1.0

--------------------------------------------------------------------------------
-- Binary Contraction Trees & Batch/M/N/K Classification
--------------------------------------------------------------------------------

-- | Metadata for a single binary contraction step @C = A ⋆ B@, classified into
-- standard GEMM/BMM roles so both CPU and CUDA tiled kernels lower directly.
data BinaryStepMeta = BinaryStepMeta
  { bsBatch :: ![IndexId],
    bsM :: ![IndexId],
    bsN :: ![IndexId],
    bsK :: ![IndexId],
    bsLhsLabels :: ![IndexId],
    bsRhsLabels :: ![IndexId],
    bsOutLabels :: ![IndexId],
    bsFlops :: !Double,
    bsIoBytes :: !Double,
    bsOutBytes :: !Double
  }
  deriving stock (Eq, Ord, Show)

-- | Binary contraction tree: leaves reference operand indices in 'csOperands'
-- along with their post-local-trace surviving labels.
data ContractionTree
  = CLeaf !Int ![IndexId]
  | CBinary !BinaryStepMeta !ContractionTree !ContractionTree
  deriving stock (Eq, Ord, Show)

treeLabels :: ContractionTree -> [IndexId]
treeLabels (CLeaf _ ls) = ls
treeLabels (CBinary m _ _) = bsOutLabels m

treeLeaves :: ContractionTree -> [Int]
treeLeaves (CLeaf i _) = [i]
treeLeaves (CBinary _ l r) = treeLeaves l <> treeLeaves r

-- | Canonical parenthesization signature (commutativity-normalized so @(A,B)@
-- and @(B,A)@ deduplicate cleanly when enumerating top-K plans).
treeSignature :: ContractionTree -> String
treeSignature (CLeaf i _) = "T" <> show i
treeSignature (CBinary _ l r) =
  let sl = treeSignature l
      sr = treeSignature r
   in if sl <= sr
        then "(" <> sl <> "," <> sr <> ")"
        else "(" <> sr <> "," <> sl <> ")"

treePretty :: ContractionTree -> String
treePretty (CLeaf i ls) = "T" <> show i <> "[" <> intercalate "," (map show ls) <> "]"
treePretty (CBinary m l r) =
  "("
    <> treePretty l
    <> " *"
    <> (if null (bsK m) then "outer" else "K=" <> intercalate "," (map show (bsK m)))
    <> "* "
    <> treePretty r
    <> ")"

-- | Classify the axes of a binary contraction step given operand label lists
-- and the set of surviving output labels for this step.
classifyBinaryStep ::
  IM.IntMap Int ->
  Int ->
  [IndexId] ->
  [IndexId] ->
  [IndexId] ->
  BinaryStepMeta
classifyBinaryStep verts elemBytes lhsLs rhsLs targetOutLs =
  let lhsU = nub lhsLs
      rhsU = nub rhsLs
      lhsSet = IS.fromList lhsU
      rhsSet = IS.fromList rhsU
      outSet = IS.fromList targetOutLs
      batchLs = [v | v <- targetOutLs, IS.member v lhsSet && IS.member v rhsSet]
      mLs = [v | v <- targetOutLs, IS.member v lhsSet && not (IS.member v rhsSet)]
      nLs = [v | v <- targetOutLs, not (IS.member v lhsSet) && IS.member v rhsSet]
      kLs = [v | v <- lhsU, IS.member v rhsSet && not (IS.member v outSet)]
      -- Also include any axis present only in one operand that is summed out here
      lhsOnlyContract = [v | v <- lhsU, not (IS.member v rhsSet) && not (IS.member v outSet)]
      rhsOnlyContract = [v | v <- rhsU, not (IS.member v lhsSet) && not (IS.member v outSet)]
      allK = nub (kLs <> lhsOnlyContract <> rhsOnlyContract)
      pBatch = dimProduct verts batchLs
      pM = dimProduct verts mLs
      pN = dimProduct verts nLs
      pK = dimProduct verts allK
      flops =
        if null allK
          then pBatch * pM * pN
          else 2.0 * pBatch * pM * pN * pK
      lhsBytes = dimProduct verts lhsU * fromIntegral elemBytes
      rhsBytes = dimProduct verts rhsU * fromIntegral elemBytes
      outBytes = dimProduct verts targetOutLs * fromIntegral elemBytes
   in BinaryStepMeta
        { bsBatch = batchLs,
          bsM = mLs,
          bsN = nLs,
          bsK = allK,
          bsLhsLabels = lhsU,
          bsRhsLabels = rhsU,
          bsOutLabels = targetOutLs,
          bsFlops = flops,
          bsIoBytes = lhsBytes + rhsBytes + outBytes,
          bsOutBytes = outBytes
        }

--------------------------------------------------------------------------------
-- Cost Models & Tree Evaluation
--------------------------------------------------------------------------------

data CostModel = CostModel
  { cmName :: !Text.Text,
    cmStepScore :: !(BinaryStepMeta -> Double),
    cmMemoryWeight :: !Double
  }

data PlanMetrics = PlanMetrics
  { pmFlops :: !Double,
    pmBytes :: !Double,
    pmPeakBytes :: !Double,
    pmScore :: !Double
  }
  deriving stock (Eq, Ord, Show)

-- | Pure FLOP minimization model (standard baseline for exact optimality proofs).
flopCostModel :: CostModel
flopCostModel =
  CostModel
    { cmName = "FLOP",
      cmStepScore = bsFlops,
      cmMemoryWeight = 0.0
    }

-- | CPU roofline cost model (models ~200 GFLOP/s compute vs ~50 GB/s DRAM).
cpuCostModel :: CostModel
cpuCostModel =
  CostModel
    { cmName = "CPU-Roofline",
      cmStepScore = \m ->
        let computeTime = bsFlops m / 2.0e11
            memTime = bsIoBytes m / 5.0e10
         in max computeTime memTime + 1.0e-7,
      cmMemoryWeight = 1.0e-12
    }

-- | GPU roofline cost model (models ~20 TFLOP/s compute vs ~900 GB/s HBM + launch overhead).
gpuRooflineCostModel :: CostModel
gpuRooflineCostModel =
  CostModel
    { cmName = "GPU-Roofline",
      cmStepScore = \m ->
        let computeTime = bsFlops m / 2.0e13
            memTime = bsIoBytes m / 9.0e11
         in max computeTime memTime + 3.0e-6,
      cmMemoryWeight = 1.0e-12
    }

-- | Compute exact FLOPs, total I/O bytes, peak live intermediate bytes, and
-- total cost-model score for a 'ContractionTree'.
evalTreeMetrics :: CostModel -> Hypergraph -> ContractionTree -> PlanMetrics
evalTreeMetrics cm hg tree =
  let (flops, ioB, peakB, stepScore, _) = go tree
   in PlanMetrics
        { pmFlops = flops,
          pmBytes = ioB,
          pmPeakBytes = peakB,
          pmScore = stepScore + cmMemoryWeight cm * peakB
        }
  where
    go (CLeaf _ ls) =
      let sz = dimProduct (hgVerts hg) ls * fromIntegral (hgElemBytes hg)
       in (0.0, 0.0, sz, 0.0, sz)
    go (CBinary m l r) =
      let (fL, bL, pL, sL, liveL) = go l
          (fR, bR, pR, sR, liveR) = go r
          outB = bsOutBytes m
          peakHere = maximum [pL, liveL + pR, liveL + liveR + outB]
          sc = sL + sR + cmStepScore cm m
       in (fL + fR + bsFlops m, bL + bR + bsIoBytes m, peakHere, sc, outB)
