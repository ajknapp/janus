module Janus.SPMD
  ( -- * Core SPMD Expression, Command, Mask, & Reference Types
    SPMD (..),
    SPMDM (..),
    SPMDRef (..),
    SPMDMask (..),
    SPMDMaskElem,
    Layout (..),
    SPMDRawLayout,
    SPMDLayout,
    SPMDRep,
    SPMDExpr,
    SPMDInt64Expr (..),
    SPMDPtrExpr (..),
    SPMDVoidPtrExpr (..),
    PtrTree,
    PtrTreeOps (..),
    SIMDPair (..),
    VoidPtr (..),
    SPMDUnit (..),
    ExpPair (..),
    SPMDTyped (..),

    -- * Mask Conversion & Scoping Combinators
    toSIMDMask,
    fromSIMDMask,
    askSIMDMask,
    askMaskBool,
    withMask,
    unmasked,
    withSPMD,
    withSPMDMasked,
    runSPMD,
    runSPMDWithMask,
    runSPMDM,
    runSPMDMWithMask,
    liftSPMD,

    -- * ISPC-Style Lane Queries, Broadcasts, & Array / Tensor Lifting
    programCount,
    programIndex,
    uniform,
    spmdSplat,
    spmdAlignedIndex,
    spmdAlignedUnitStride,
    spmdUnitStride,
    uniformPtr,
    uniformAlignedPtr,
    liftTensor,
    liftTensorUnaligned,
    batchTensors,
    spmdFromLanes,
    spmdToLanes,
    spmdFromSIMD,
    spmdToSIMD,
    SPMDLanesByLayout (..),
    HostSPMDValue (..),

    -- * Horizontal Gang Reductions (Unmasked & Mask-Aware)
    SPMDReduceByLayout (..),
    gangAny,
    gangAll,
    gangNone,
    gangReduceAdd,
    gangReduceMin,
    gangReduceMax,
    spmdAny,
    spmdAll,
    spmdNone,
    spmdReduceAdd,
    spmdReduceMin,
    spmdReduceMax,

    -- * Optimal Non-Splitting Gang Size Selection & Strip-Mining Loops
    GangSize (..),
    MaxSingleRegGang,
    optimalX86GangSize,
    optimalCUDAGangSize,
    SPMDGang,
    CmdOptimalGang (..),
    foreach0M,
    foreachM,
    foreachAlignedM,
    foreachTensorM,
    foldForeach0M,
    foldForeachTensorM,
    foreachOptimal0M,
    foreachOptimalM,
    foreachOptimalTensorM,
    foldForeachOptimal0M,
    foldForeachOptimalTensorM,
  )
where

import Janus.SPMD.Cast ()
import Janus.SPMD.Command ()
import Janus.SPMD.Expression ()
import Janus.SPMD.Gang
import Janus.SPMD.Host
import Janus.SPMD.Lane
import Janus.SPMD.Raw ()
import Janus.SPMD.Typed
import Janus.SPMD.Types
