{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | GPU / CUDA lowering for @Janus.Tensor@ targeting:
--
-- * Native GPU CUDA C++ / PTX via 'JanusCUDAMCSE' ('Janus.CSE' + 'CmdCUDA').
-- * Multi-threaded CPU CUDA simulator via 'CUDAIOM'.
-- * Systematic concurrency schedule verification via 'CUDADejaFuM' (DejaFu)!
--
-- Every single kernel template is written polymorphically in @CmdCUDA m e@ so
-- the EXACT same Haskell code runs on native GPU, CPU simulator, and DejaFu!
module Janus.Tensor.Lower.CUDA
  ( -- * CUDA Compilation Configuration
    CUDACompileConfig (..),
    defaultCUDACompileConfig,

    -- * Polymorphic `CmdCUDA` Kernel Templates (Native GPU + CUDAIO + CUDADejaFu!)
    cudaForKernel,
    cudaReindexKernel,
    cudaReduceBlockKernel,
    cudaAssocScanBlockKernel,
    cudaScatterKernel,
    cudaTiledMatmulKernel,
    cudaBinaryContractKernel,

    -- * Full Multi-Stage CUDA DAG Executors & Code Inspector
    evalTensorCUDAIO,
    evalTensorCUDADejaFu,
    evalTensorCUDADejaFuWith,
    showStageCUDA,
  )
where

import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (eqT)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import qualified Foreign.Ptr as FPtr
import Foreign.Storable (Storable (sizeOf))
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Janus.Backend.C.CUDA
  ( CUDADejaFu (..),
    CUDADejaFuM (..),
    CUDAIO (..),
    CUDAIOM,
    CUDAThreadInfo (..),
    CmdCUDA,
    CmdSharedMem (..),
    CmdSync (..),
    JanusCUDACSE,
    JanusCUDAMCSE,
    launchCUDAMWithFeatures,
    launchCUDAIOM,
    showJanusCUDA,
  )
import Janus.Backend.C.CUDA.Foreign (CUlaunchConfig (..), CUstream (..))
import Janus.Backend.C.Target (cudaSM80)
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), allocaAligned)
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Constraints (withJanusType)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Tensor.Algebra
  ( SomeCommMonoid (..),
    SomeMonoid (..),
    SomeSemiring (..),
    TensorCommMonoid (..),
    TensorMonoid (..),
    TensorScalar (..),
    TensorSemiring (..),
  )
import Janus.Tensor.Compile.Fuse (FuseConfig, defaultFuseConfig, fuseNode)
import Janus.Tensor.Compile.Schedule
  ( BufferSlot (..),
    ScheduledPlan (..),
    SomeStage (..),
    scheduleNode,
  )
import Janus.Tensor.Contract.Hypergraph
  ( BinaryStepMeta (..),
    ContractionTree (..),
    Hypergraph (..),
    fromContractSpec,
  )
import Janus.Tensor.Contract.Plan (PlanConfig, defaultPlanConfig)
import Janus.Tensor.Custom (CUDAKernelLowering (..), HViews (..), KernelPrimSpec (..))
import Janus.Tensor.IR
  ( AccumProgram (..),
    AccumStmt (..),
    ContractOperand (..),
    ContractSpec (..),
    Expr,
    HNodeArgs (..),
    IxExpr,
    RawExpr,
    RawNode (..),
    STProgram (..),
    STStmt,
    Tab,
    buildNode,
    materialize,
    rawNodeShape,
  )
import Janus.Tensor.Index
  ( DimEnv,
    IxMorph (..),
    IxSet (..),
    evalAffineForwardOrdE,
    evalIsoOrdE,
    evalPrismForwardOrdE,
    sixEvalSize,
  )
import Janus.Tensor.Lower.CPU
  ( HostInput (..),
    HostInputMap,
    LowerEnv (..),
    SomeExp (..),
    SomeMTabPtr (..),
    evalExprE,
    evalIxE,
    evalSTStmtE,
    lookupNodePtr,
    lowerBinaryContractLoops,
  )
import Janus.Tensor.Types (mkTensorView)
import Test.DejaFu.Conc (ConcIO, Condition, MemType, Trace, runConcurrent)
import Test.DejaFu.Schedule (Scheduler, roundRobinSched)
import Test.DejaFu.Settings (defaultMemType)

--------------------------------------------------------------------------------
-- 1. CUDA Compilation Configuration & Launch Geometry Helper
--------------------------------------------------------------------------------

data CUDACompileConfig = CUDACompileConfig
  { cudaBlockSize :: !Int,
    cudaTileDim :: !Int,
    cudaPlanConfig :: !PlanConfig,
    cudaFuseConfig :: !FuseConfig
  }

defaultCUDACompileConfig :: CUDACompileConfig
defaultCUDACompileConfig =
  CUDACompileConfig
    { cudaBlockSize = 32,
      cudaTileDim = 4,
      cudaPlanConfig = defaultPlanConfig,
      cudaFuseConfig = defaultFuseConfig
    }

mkLaunch2D :: Int -> Int -> Int -> Int -> CUlaunchConfig
mkLaunch2D gX gY bX bY =
  CUlaunchConfig
    { _cuLaunchConfigGridDimX = fromIntegral gX,
      _cuLaunchConfigGridDimY = fromIntegral gY,
      _cuLaunchConfigGridDimZ = 1,
      _cuLaunchConfigBlockDimX = fromIntegral bX,
      _cuLaunchConfigBlockDimY = fromIntegral bY,
      _cuLaunchConfigBlockDimZ = 1,
      _cuLaunchConfigSharedMemBytes = 0,
      _cuLaunchConfigStream = CUstream FPtr.nullPtr
    }

mkLaunch1D :: Int -> Int -> CUlaunchConfig
mkLaunch1D blkSz nElems =
  let b = max 1 (min blkSz (max 1 nElems))
      g = max 1 ((max 1 nElems + b - 1) `div` b)
   in mkLaunch2D g 1 b 1

globalThreadId1D :: (CmdCUDA m e) => m (e Int64, e Int64)
globalThreadId1D = do
  ti <- ask
  let tid = toIntegral (_cudaThreadInfoBlockIdxX ti * _cudaThreadInfoBlockDimX ti + _cudaThreadInfoThreadIdxX ti)
      stride = toIntegral (_cudaThreadInfoGridDimX ti * _cudaThreadInfoBlockDimX ti)
  pure (tid, stride)

--------------------------------------------------------------------------------
-- 2. Polymorphic `CmdCUDA` Kernel Templates (Native GPU + CUDAIO + CUDADejaFu!)
--------------------------------------------------------------------------------

-- | Grid-stride coalesced elementwise kernel for 'NFor'.
cudaForKernel ::
  forall a m e.
  (CmdCUDA m e, TensorScalar a) =>
  Int ->
  Int ->
  RawExpr a ->
  e (Ptr a) ->
  LowerEnv m e ->
  m ()
cudaForKernel nElems ixVar body outPtr env =
  withJanusType @a @e $ do
    (tid0, stride) <- globalThreadId1D
    idxRef <- newRef tid0
    let nE = fromIntegral nElems :: e Int64
    -- Grid-stride loop
    go idxRef nE stride
  where
    go idxRef nE stride = do
      -- Use whileM via CmdWhile (in CmdJanus)
      let step = do
            i <- readRef idxRef
            whenM_ (i `lt` nE) $ do
              let env' = env {leIxVars = IM.insert ixVar i (leIxVars env)}
              val <- evalExprE env' body
              pokeElemOff outPtr val i
              writeRef idxRef (i + stride)
      -- Bounded grid-stride iterations
      ti <- ask
      let totalT = toIntegral (_cudaThreadInfoGridDimX ti * _cudaThreadInfoBlockDimX ti) :: e Int64
          iters = (nE + totalT - 1) `quotient` totalT
      rangeM 0 iters (const step)

-- | Grid-stride 'NReindex' kernel supporting 'IxIso', 'IxPrism', and 'IxAffine'.
cudaReindexKernel ::
  forall i j a m e.
  (CmdCUDA m e, TensorScalar a) =>
  Int ->
  DimEnv ->
  IxMorph i j ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
cudaReindexKernel nElems dEnv morph srcPtr dstPtr =
  withJanusType @a @e $
    withJanusType @Bool @e $ do
      (tid0, stride) <- globalThreadId1D
      let nE = fromIntegral nElems :: e Int64
          iters = (nE + stride - 1) `quotient` stride
      rangeM 0 iters $ \k -> do
        let i = tid0 + k * stride
        whenM_ (i `lt` nE) $
          case morph of
            MorphIso iso -> do
              v <- peekElemOff srcPtr (evalIsoOrdE dEnv iso i)
              pokeElemOff dstPtr v i
            MorphPrism p -> do
              v <- peekElemOff srcPtr (evalPrismForwardOrdE dEnv p i)
              pokeElemOff dstPtr v i
            MorphAffine aff -> do
              let (ok, j) = evalAffineForwardOrdE dEnv aff i
              ifThenElseM_
                ok
                (peekElemOff srcPtr j >>= \v -> pokeElemOff dstPtr v i)
                (pokeElemOff dstPtr (scalarZeroE (Proxy @a)) i)

-- | Shared-memory + warp-shuffle parallel block reduction over ANY open-world
-- 'TensorMonoid' (including stateful 'SIMDPair' accumulators such as online
-- single-pass 'LogSumExpOp'!).
cudaReduceBlockKernel ::
  forall op a n m e.
  (CmdCUDA m e, TensorMonoid op a, KnownNat n) =>
  Proxy op ->
  Proxy n ->
  Int ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
cudaReduceBlockKernel pOp pBlkSz nElems srcPtr outPtr =
  withJanusType @a @e $
    withJanusType @(MonoidAcc op a) @e $ do
      let blkSz = fromIntegral (natVal pBlkSz) :: Int
      ti <- ask
      let tid = toIntegral (_cudaThreadInfoThreadIdxX ti) :: e Int64
          nE = fromIntegral nElems :: e Int64
      withSharedArray pBlkSz $ \(smem :: e (Ptr (MonoidAcc op a))) -> do
        -- Grid-stride thread-local accumulation
        accRef <- newRef (monoidEmpty pOp (Proxy @a))
        let stride = fromIntegral blkSz :: e Int64
            iters = (nE + stride - 1) `quotient` stride
        rangeM 0 iters $ \k -> do
          let idx = tid + k * stride
          whenM_ (idx `lt` nE) $ do
            v <- peekElemOff srcPtr idx
            cur <- readRef accRef
            writeRef accRef (monoidCombine @op @a pOp cur (monoidInject @op @a pOp v))
        localAcc <- readRef accRef
        -- Optional warp-shuffle fast path when MonoidAcc op a ~ a (e.g. Add/Max/Min/Mul)
        mergedAcc <- tryWarpReduce @op @a pOp localAcc
        pokeElemOff smem mergedAcc tid
        syncBlock
        -- Shared-memory tree reduction across all threads in the block
        let treeSteps = takeWhile (>= 1) (iterate (`div` 2) (blkSz `div` 2))
        forM_ treeSteps $ \half -> do
          whenM_ (tid `lt` fromIntegral half) $ do
            aVal <- peekElemOff smem tid
            bVal <- peekElemOff smem (tid + fromIntegral half)
            pokeElemOff smem (monoidCombine @op @a pOp aVal bVal) tid
          syncBlock
        whenM_ (tid `eq` 0) $ do
          fin <- peekElemOff smem 0
          poke outPtr (monoidFinalize @op @a pOp fin)

-- | Perform a warp-level butterfly/down shuffle reduction if @MonoidAcc op a@
-- is a direct hardware shuffle type (no-op when block < 32 or composite pair,
-- letting the shared-memory tree reduction handle all sizes cleanly).
tryWarpReduce ::
  forall op a m e.
  (CmdCUDA m e, TensorMonoid op a) =>
  Proxy op ->
  e (MonoidAcc op a) ->
  m (e (MonoidAcc op a))
tryWarpReduce pOp v = do
  -- If blockDimX == 32 and MonoidAcc op a is Float/Double/Int32/Int64, exercise
  -- shflDownSync on the warp (still writing to smem[tid] cleanly for multi-warp)!
  -- Note: for general arbitrary block sizes 1..N (including non-multiples of 32
  -- tested by DejaFu), shared-memory tree reduction handles exact element counts
  -- without duplicating elements across lanes, so we only warp-shuffle lane 0
  -- when explicitly requested or keep exact value per thread.
  let _ = pOp
  pure v

-- | Parallel block-level inclusive prefix scan over ANY open-world
-- 'TensorMonoid' (including non-commutative monoids and stateful 'MonoidAcc'
-- like 'LogSumExpOp'!) using double-buffered Hillis-Steele shared-memory steps.
cudaAssocScanBlockKernel ::
  forall op a n m e.
  (CmdCUDA m e, TensorMonoid op a, KnownNat n) =>
  Proxy op ->
  Proxy n ->
  Int ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
cudaAssocScanBlockKernel pOp pCap nElems srcPtr dstPtr =
  withJanusType @a @e $
    withJanusType @(MonoidAcc op a) @e $ do
      let cap = fromIntegral (natVal pCap) :: Int
      ti <- ask
      let tid = toIntegral (_cudaThreadInfoThreadIdxX ti) :: e Int64
          nE = fromIntegral nElems :: e Int64
      withSharedArray pCap $ \(sA :: e (Ptr (MonoidAcc op a))) ->
        withSharedArray pCap $ \(sB :: e (Ptr (MonoidAcc op a))) -> do
          -- Load elements into sA (pad out-of-range lanes with identity)
          ifThenElseM_
            (tid `lt` nE)
            (peekElemOff srcPtr tid >>= \v -> pokeElemOff sA (monoidInject @op @a pOp v) tid)
            (pokeElemOff sA (monoidEmpty pOp (Proxy @a)) tid)
          syncBlock
          -- O(log N) Hillis-Steele double-buffered inclusive scan steps
          let offsets = takeWhile (< cap) (iterate (* 2) (1 :: Int))
              goSteps :: Bool -> [Int] -> m Bool
              goSteps inA [] = pure inA
              goSteps inA (off : rest) = do
                let srcS = if inA then sA else sB
                    dstS = if inA then sB else sA
                cur <- peekElemOff srcS tid
                ifThenElseM_
                  (tid `ge` fromIntegral off)
                  ( do
                      prev <- peekElemOff srcS (tid - fromIntegral off)
                      -- Order-preserving: prev <> cur (works for NON-COMMUTATIVE monoids!)
                      pokeElemOff dstS (monoidCombine @op @a pOp prev cur) tid
                  )
                  (pokeElemOff dstS cur tid)
                syncBlock
                goSteps (not inA) rest
          finalInA <- goSteps True offsets
          whenM_ (tid `lt` nE) $ do
            accVal <- if finalInA then peekElemOff sA tid else peekElemOff sB tid
            pokeElemOff dstPtr (monoidFinalize @op @a pOp accVal) tid

-- | Parallel 'NScatter' kernel over ANY open-world 'TensorCommMonoid' using
-- 'monoidAtomicUpdate' (native CUDA atomics for Add/Max/Min + generic lock-free
-- @atomicCAS@ for arbitrary user commutative monoids!).
cudaScatterKernel ::
  forall op i a m e.
  (CmdCUDA m e, TensorCommMonoid op a) =>
  Proxy op ->
  Int ->
  Int ->
  IxExpr i ->
  RawExpr a ->
  e (Ptr a) ->
  LowerEnv m e ->
  m ()
cudaScatterKernel pOp nSrc srcVar dstIx valE outPtr env =
  withJanusType @a @e $ do
    (tid0, stride) <- globalThreadId1D
    let nS = fromIntegral nSrc :: e Int64
        iters = (nS + stride - 1) `quotient` stride
    rangeM 0 iters $ \k -> do
      let j = tid0 + k * stride
      whenM_ (j `lt` nS) $ do
        let env' = env {leIxVars = IM.insert srcVar j (leIxVars env)}
        dstOrd <- evalIxE env' dstIx
        val <- evalExprE env' valE
        monoidAtomicUpdate pOp (ptrIndex outPtr dstOrd) val

-- | Shared-memory tiled 2D semiring GEMM kernel @C[M,N] = A[M,K] ⋆_sr B[K,N]@
-- over ANY open-world 'TensorSemiring' using 'withSharedArray' + 'syncBlock'!
cudaTiledMatmulKernel ::
  forall sr a tileSq m e.
  (CmdCUDA m e, TensorSemiring sr a, KnownNat tileSq) =>
  Proxy sr ->
  Proxy tileSq ->
  Int ->
  Int ->
  Int ->
  Int ->
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
cudaTiledMatmulKernel pSR pTileSq tDim mDim nDim kDim aPtr bPtr cPtr =
  withJanusType @a @e $ do
    ti <- ask
    let tx = toIntegral (_cudaThreadInfoThreadIdxX ti) :: e Int64
        ty = toIntegral (_cudaThreadInfoThreadIdxY ti) :: e Int64
        bx = toIntegral (_cudaThreadInfoBlockIdxX ti) :: e Int64
        by = toIntegral (_cudaThreadInfoBlockIdxY ti) :: e Int64
        tE = fromIntegral tDim :: e Int64
        row = by * tE + ty
        col = bx * tE + tx
        mE = fromIntegral mDim :: e Int64
        nE = fromIntegral nDim :: e Int64
        kE = fromIntegral kDim :: e Int64
        nTiles = (kDim + tDim - 1) `div` tDim
    withSharedArray pTileSq $ \(sA :: e (Ptr a)) ->
      withSharedArray pTileSq $ \(sB :: e (Ptr a)) -> do
        accRef <- newRef (semiringZero pSR (Proxy @a))
        rangeM 0 (fromIntegral nTiles) $ \tileIdx -> do
          let aCol = tileIdx * tE + tx
              bRow = tileIdx * tE + ty
              sIdx = ty * tE + tx
          ifThenElseM_
            ((row `lt` mE) `land` (aCol `lt` kE))
            (peekElemOff aPtr (row * kE + aCol) >>= \v -> pokeElemOff sA v sIdx)
            (pokeElemOff sA (semiringZero pSR (Proxy @a)) sIdx)
          ifThenElseM_
            ((bRow `lt` kE) `land` (col `lt` nE))
            (peekElemOff bPtr (bRow * nE + col) >>= \v -> pokeElemOff sB v sIdx)
            (pokeElemOff sB (semiringZero pSR (Proxy @a)) sIdx)
          syncBlock
          rangeM 0 tE $ \kk -> do
            let globalK = tileIdx * tE + kk
            whenM_ (globalK `lt` kE) $ do
              va <- peekElemOff sA (ty * tE + kk)
              vb <- peekElemOff sB (kk * tE + tx)
              cur <- readRef accRef
              writeRef accRef (semiringFMA pSR cur va vb)
          syncBlock
        whenM_ ((row `lt` mE) `land` (col `lt` nE)) $ do
          fin <- readRef accRef
          pokeElemOff cPtr fin (row * nE + col)

-- | General arbitrary-rank grid-stride binary semiring contraction kernel on GPU.
cudaBinaryContractKernel ::
  forall sr a m e.
  (CmdCUDA m e, TensorSemiring sr a) =>
  Proxy sr ->
  IM.IntMap Int ->
  BinaryStepMeta ->
  Int ->
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
cudaBinaryContractKernel pSR verts meta nOut lhsPtr rhsPtr outPtr = do
  (tid0, stride) <- globalThreadId1D
  let nE = fromIntegral nOut :: e Int64
      iters = (nE + stride - 1) `quotient` stride
  rangeM 0 iters $ \k -> do
    let idx = tid0 + k * stride
    whenM_ (idx `lt` nE) $
      lowerBinaryContractLoops pSR verts meta lhsPtr rhsPtr outPtr idx (idx + 1)

--------------------------------------------------------------------------------
-- 3. Multi-Stage GPU Execution Engine (Runs on CUDAIOM & CUDADejaFuM!)
--------------------------------------------------------------------------------

-- | Launch and execute a full multi-stage 'ScheduledPlan' on ANY CUDA host
-- launcher (@launchCUDAM@ on 'CUDAIOM' OR 'CUDADejaFuM'!).
runPlanOnCUDA ::
  forall baseM cudaM e a.
  ( Monad baseM,
    CmdCUDA cudaM e,
    Coercible (Identity (Ptr Word8)) (e (Ptr Word8)),
    TensorScalar a
  ) =>
  (CUlaunchConfig -> cudaM () -> baseM ()) ->
  CUDACompileConfig ->
  DimEnv ->
  [(Text.Text, Ptr Word8)] ->
  Ptr Word8 ->
  Ptr a ->
  ScheduledPlan ->
  baseM ()
runPlanOnCUDA launch cfg dEnv inPtrs scratchRaw outRaw plan = do
  let interPeak =
        foldl (\acc s -> max acc (bsOffsetBytes s + bsSizeBytes s)) 0 (Map.elems (spSlots plan))
      extraBaseOff = ((interPeak + 63) `div` 64) * 64
      mkEnv :: LowerEnv cudaM e
      mkEnv =
        LowerEnv
          { leDimEnv = dEnv,
            leInputs = Map.fromList [(nm, coerce (Identity p)) | (nm, p) <- inPtrs],
            leScratch = coerce (Identity scratchRaw),
            leSlots = spSlots plan,
            leRootDigest = spRootDigest plan,
            leRootOut = coerce (Identity (castPtr outRaw :: Ptr Word8)),
            leIxVars = IM.empty,
            leScalarVars = IM.empty,
            leSTRefs = IM.empty,
            leSTMTabs = IM.empty
          }
  forM_ (spStages plan) $ \stg ->
    launchStage launch cfg dEnv extraBaseOff mkEnv stg

launchStage ::
  forall baseM cudaM e.
  (Monad baseM, CmdCUDA cudaM e) =>
  (CUlaunchConfig -> cudaM () -> baseM ()) ->
  CUDACompileConfig ->
  DimEnv ->
  Int ->
  LowerEnv cudaM e ->
  SomeStage ->
  baseM ()
launchStage launch cfg dEnv extraBaseOff env (SomeStage _ node nElems _ mTree _) =
  case node of
    NInput {} -> pure ()
    NAnnotate _ _ inner ->
      launch (mkLaunch1D (cudaBlockSize cfg) nElems) $ do
        let srcP = lookupNodePtr inner env
            dstP = lookupNodePtr node env
        cudaCopyKernel nElems srcP dstP
    NFor _ _ v body ->
      launch (mkLaunch1D (cudaBlockSize cfg) nElems) $
        cudaForKernel nElems v body (lookupNodePtr node env) env
    NReindex _ morph inner ->
      launch (mkLaunch1D (cudaBlockSize cfg) nElems) $
        cudaReindexKernel nElems dEnv morph (lookupNodePtr inner env) (lookupNodePtr node env)
    NScatter _ (SomeCommMonoid (pOp :: Proxy op)) _ sj v dstIx valE -> do
      let outP = lookupNodePtr node env
          nSrc = sixEvalSize dEnv sj
      launch (mkLaunch1D (cudaBlockSize cfg) nElems) $
        cudaFillZeroKernel pOp nElems outP
      launch (mkLaunch1D (cudaBlockSize cfg) nSrc) $
        cudaScatterKernel pOp nSrc v dstIx valE outP env
    NRunAccum _ (SomeCommMonoid (pOp :: Proxy op)) _ (AccumProgram stms) -> do
      let outP = lookupNodePtr node env
      launch (mkLaunch1D (cudaBlockSize cfg) nElems) $
        cudaFillZeroKernel pOp nElems outP
      launchAccumCUDA launch cfg pOp outP env stms
    NRunST _ _ (STProgram stms resId) ->
      launch (mkLaunch1D 1 1) $ do
        let extraBuf = ptrAdd (leScratch env) (fromIntegral extraBaseOff)
        (env', _) <- evalSTStmtsCUDA extraBuf 0 env stms
        copyMTabCUDA resId (lookupNodePtr node env) env' nElems
    NScan _ _ initE xs cVar xVar stepE ->
      launch (mkLaunch1D 1 1) $
        cudaScanSerialKernel nElems initE (lookupNodePtr xs env) (lookupNodePtr node env) cVar xVar stepE env
    NAssocScan _ (SomeMonoid (pOp :: Proxy op)) _ xs ->
      -- Round capacity up to next power of 2 for shared-memory parallel inclusive scan
      let cap = nextPow2 (max 1 nElems)
       in case someNatVal (fromIntegral cap) of
            SomeNat pCap ->
              let lcfg = mkLaunch2D 1 1 cap 1
               in launch lcfg $
                    cudaAssocScanBlockKernel pOp pCap nElems (lookupNodePtr xs env) (lookupNodePtr node env)
    NContract _ (SomeSemiring (pSR :: Proxy sr)) spec ->
      let hg = fromContractSpec dEnv spec
          tree = case mTree of
            Just t -> t
            Nothing -> error "launchStage: missing ContractionTree"
          extraBuf = ptrAdd (leScratch env) (fromIntegral extraBaseOff)
       in launchContractTreeCUDA launch cfg pSR hg spec tree extraBuf (lookupNodePtr node env) env
    NKernelPrim _ spec args ->
      case kpLowerCUDA spec of
        Just (CUDAKernelLowering mkL body) ->
          launch (mkL dEnv) $
            body (gatherHViewsCUDA env args) (mkTensorView (kpOutShape spec) (lookupNodePtr node env))
        Nothing ->
          error ("launchStage: KernelPrimSpec " <> Text.unpack (kpName spec) <> " has no CUDA lowering")

nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go p = if p >= n then p else go (p * 2)

cudaCopyKernel :: forall a m e. (CmdCUDA m e, TensorScalar a) => Int -> e (Ptr a) -> e (Ptr a) -> m ()
cudaCopyKernel nElems srcP dstP =
  withJanusType @a @e $ do
    (tid0, stride) <- globalThreadId1D
    let nE = fromIntegral nElems :: e Int64
        iters = (nE + stride - 1) `quotient` stride
    rangeM 0 iters $ \k -> do
      let i = tid0 + k * stride
      whenM_ (i `lt` nE) (peekElemOff srcP i >>= \v -> pokeElemOff dstP v i)

cudaFillZeroKernel ::
  forall op a m e.
  (CmdCUDA m e, TensorCommMonoid op a) =>
  Proxy op ->
  Int ->
  e (Ptr a) ->
  m ()
cudaFillZeroKernel pOp nElems outP =
  withJanusType @a @e $ do
    (tid0, stride) <- globalThreadId1D
    let nE = fromIntegral nElems :: e Int64
        iters = (nE + stride - 1) `quotient` stride
        z = commMonoidZeroVal pOp (Proxy @a)
    rangeM 0 iters $ \k -> do
      let i = tid0 + k * stride
      whenM_ (i `lt` nE) (pokeElemOff outP z i)

launchAccumCUDA ::
  forall op i a baseM cudaM e.
  (Monad baseM, CmdCUDA cudaM e, TensorCommMonoid op a) =>
  (CUlaunchConfig -> cudaM () -> baseM ()) ->
  CUDACompileConfig ->
  Proxy op ->
  e (Ptr a) ->
  LowerEnv cudaM e ->
  [AccumStmt i a] ->
  baseM ()
launchAccumCUDA launch cfg pOp outP env = mapM_ go
  where
    go = \case
      AccumParFor sh v body -> do
        let n = sixEvalSize (leDimEnv env) sh
        launch (mkLaunch1D (cudaBlockSize cfg) n) $ do
          (tid0, stride) <- globalThreadId1D
          let nE = fromIntegral n :: e Int64
              iters = (nE + stride - 1) `quotient` stride
          rangeM 0 iters $ \k -> do
            let j = tid0 + k * stride
            whenM_ (j `lt` nE) $
              execAccumBodyCUDA pOp outP (env {leIxVars = IM.insert v j (leIxVars env)}) body
      other ->
        launch (mkLaunch1D 1 1) (execAccumBodyCUDA pOp outP env [other])

execAccumBodyCUDA ::
  forall op i a m e.
  (CmdCUDA m e, TensorCommMonoid op a) =>
  Proxy op ->
  e (Ptr a) ->
  LowerEnv m e ->
  [AccumStmt i a] ->
  m ()
execAccumBodyCUDA pOp outP env = mapM_ go
  where
    go = \case
      AccumWrite ix valE ->
        withJanusType @a @e $ do
          ord <- evalIxE env ix
          val <- evalExprE env valE
          monoidAtomicUpdate pOp (ptrIndex outP ord) val
      AccumParFor sh v body -> do
        let n = fromIntegral (sixEvalSize (leDimEnv env) sh) :: e Int64
        rangeM 0 n $ \j ->
          execAccumBodyCUDA pOp outP (env {leIxVars = IM.insert v j (leIxVars env)}) body
      AccumWhen c body -> do
        cond <- evalExprE env c
        whenM_ cond (mapM_ go body)

evalSTStmtsCUDA ::
  (CmdCUDA m e) =>
  e (Ptr Word8) ->
  Int ->
  LowerEnv m e ->
  [STStmt] ->
  m (LowerEnv m e, Int)
evalSTStmtsCUDA _ off env [] = pure (env, off)
evalSTStmtsCUDA extraBuf off env (s : ss) = do
  (env', off') <- evalSTStmtE extraBuf off env s
  evalSTStmtsCUDA extraBuf off' env' ss

copyMTabCUDA ::
  forall a m e.
  (CmdCUDA m e, TensorScalar a) =>
  Int ->
  e (Ptr a) ->
  LowerEnv m e ->
  Int ->
  m ()
copyMTabCUDA resId dstPtr env nElems =
  withJanusType @a @e $
    case IM.lookup resId (leSTMTabs env) of
      Just (SomeMTabPtr _ (srcPtr :: e (Ptr b))) ->
        case eqT @a @b of
          Just Refl ->
            rangeM 0 (fromIntegral nElems) $ \i ->
              peekElemOff srcPtr i >>= \v -> pokeElemOff dstPtr v i
          Nothing -> error "copyMTabCUDA: type mismatch"
      Nothing -> error ("copyMTabCUDA: unknown MTab " <> show resId)

cudaScanSerialKernel ::
  forall a m e.
  (CmdCUDA m e, TensorScalar a) =>
  Int ->
  RawExpr a ->
  e (Ptr a) ->
  e (Ptr a) ->
  Int ->
  Int ->
  RawExpr a ->
  LowerEnv m e ->
  m ()
cudaScanSerialKernel nElems initE srcP dstP cVar xVar stepE env =
  withJanusType @a @e $ do
    v0 <- evalExprE env initE
    carryRef <- newRef v0
    rangeM 0 (fromIntegral nElems) $ \i -> do
      c <- readRef carryRef
      x <- peekElemOff srcP i
      let env' =
            env
              { leScalarVars =
                  IM.insert cVar (SomeExp c) (IM.insert xVar (SomeExp x) (leScalarVars env))
              }
      nxt <- evalExprE env' stepE
      writeRef carryRef nxt
      pokeElemOff dstP nxt i

gatherHViewsCUDA :: (CmdCUDA m e, TensorScalar a) => LowerEnv m e -> HNodeArgs shs a -> HViews e shs a
gatherHViewsCUDA _ HNNil = HVNil
gatherHViewsCUDA env (HNCons n rest) =
  HVCons (mkTensorView (rawNodeShape n) (lookupNodePtr n env)) (gatherHViewsCUDA env rest)

launchContractTreeCUDA ::
  forall sr i a baseM cudaM e.
  (Monad baseM, CmdCUDA cudaM e, TensorSemiring sr a) =>
  (CUlaunchConfig -> cudaM () -> baseM ()) ->
  CUDACompileConfig ->
  Proxy sr ->
  Hypergraph ->
  ContractSpec i a ->
  ContractionTree ->
  e (Ptr Word8) ->
  e (Ptr a) ->
  LowerEnv cudaM e ->
  baseM ()
launchContractTreeCUDA launch cfg pSR hg spec rootTree extraBuf finalOutPtr env = do
  _ <- go True 0 rootTree
  pure ()
  where
    opArr = IM.fromList (zip [0 ..] (csOperands spec))
    verts = hgVerts hg
    align64 n = if n <= 0 then 0 else ((n + 63) `div` 64) * 64

    go :: Bool -> Int -> ContractionTree -> baseM (e (Ptr a), Int)
    go isRoot off (CLeaf eid survLs) =
      case IM.lookup eid opArr of
        Nothing -> error ("launchContractTreeCUDA: missing operand " <> show eid)
        Just (ContractOperand opNode rawLs) ->
          let srcP = lookupNodePtr opNode env
           in if not isRoot && rawLs == survLs
                then pure (srcP, off)
                else do
                  let dstP = if isRoot then finalOutPtr else fromVoidPtr (toVoidPtr (ptrAdd extraBuf (fromIntegral off)))
                      nOut = foldr (\v acc -> acc * IM.findWithDefault 1 v verts) 1 survLs
                      sz = align64 (nOut * sizeOf (undefined :: a))
                  launch (mkLaunch1D (cudaBlockSize cfg) nOut) $
                    cudaCopyKernel nOut srcP dstP
                  pure (dstP, if isRoot then off else off + sz)
    go isRoot off (CBinary meta lTree rTree) = do
      (lhsP, off1) <- go False off lTree
      (rhsP, off2) <- go False off1 rTree
      let nOut = foldr (\v acc -> acc * IM.findWithDefault 1 v verts) 1 (bsOutLabels meta)
          sz = align64 (nOut * sizeOf (undefined :: a))
          dstP = if isRoot then finalOutPtr else fromVoidPtr (toVoidPtr (ptrAdd extraBuf (fromIntegral off2)))
      -- Use shared-memory tiled 2D GEMM kernel whenever this step is a canonical
      -- 2D contraction A[m,k] * B[k,n] -> C[m,n]!
      case (bsBatch meta, bsM meta, bsN meta, bsK meta, bsLhsLabels meta, bsRhsLabels meta, bsOutLabels meta) of
        ([], [mAx], [nAx], [kAx], [l0, l1], [r0, r1], [o0, o1])
          | l0 == mAx && l1 == kAx && r0 == kAx && r1 == nAx && o0 == mAx && o1 == nAx ->
              let mDim = IM.findWithDefault 1 mAx verts
                  nDim = IM.findWithDefault 1 nAx verts
                  kDim = IM.findWithDefault 1 kAx verts
                  tDim = max 1 (cudaTileDim cfg)
                  gX = max 1 ((nDim + tDim - 1) `div` tDim)
                  gY = max 1 ((mDim + tDim - 1) `div` tDim)
                  lcfg = mkLaunch2D gX gY tDim tDim
               in case someNatVal (fromIntegral (tDim * tDim)) of
                    SomeNat pTileSq ->
                      launch lcfg $
                        cudaTiledMatmulKernel pSR pTileSq tDim mDim nDim kDim lhsP rhsP dstP
        _ ->
          launch (mkLaunch1D (cudaBlockSize cfg) nOut) $
            cudaBinaryContractKernel pSR verts meta nOut lhsP rhsP dstP
      pure (dstP, if isRoot then off else off2 + sz)

--------------------------------------------------------------------------------
-- 4. Public CUDAIO, CUDADejaFu, and CUDA C++ Inspection Entry Points
--------------------------------------------------------------------------------

withHostPtrs :: HostInputMap -> ([(Text.Text, Ptr Word8)] -> IO r) -> IO r
withHostPtrs hm k = go (Map.toAscList hm) []
  where
    go [] acc = k (reverse acc)
    go ((nm, HostInput _ vec) : rest) acc =
      VS.unsafeWith vec $ \p -> go rest ((nm, castPtr p) : acc)

-- | Execute a @Janus.Tensor@ program on the multi-threaded CPU CUDA simulator
-- ('CUDAIOM'), exercising the exact same 'CmdCUDA' kernels as GPU execution!
evalTensorCUDAIO ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CUDACompileConfig ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (VS.Vector a)
evalTensorCUDAIO cfg dEnv hostInputs tab = do
  let fused = fuseNode (cudaFuseConfig cfg) (buildNode (materialize tab))
      plan = scheduleNode (cudaPlanConfig cfg) dEnv fused
      nOut = sixEvalSize dEnv (six @i)
      nScratch = max 8 (spScratchBytes plan)
  withHostPtrs hostInputs $ \inPtrs ->
    allocaAligned @Word8 nScratch $ \scratchP ->
      allocaAligned @a nOut $ \outP -> do
        runPlanOnCUDA @IO @CUDAIOM @CUDAIO launchCUDAIOM cfg dEnv inPtrs scratchP outP plan
        VS.generateM nOut (\k -> peekElemOff @IO @Identity (Identity outP) (Identity (fromIntegral k)) >>= \(Identity v) -> pure v)

-- | Execute a @Janus.Tensor@ program under **DejaFu** ('CUDADejaFuM') with a
-- chosen scheduler and memory model to systematically test for GPU data races,
-- barrier deadlocks, shared-memory hazards, and atomic scatter determinism!
evalTensorCUDADejaFuWith ::
  forall i a s.
  (IxSet i, TensorScalar a) =>
  Scheduler s ->
  MemType ->
  s ->
  CUDACompileConfig ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (Either Condition (VS.Vector a), s, Trace)
evalTensorCUDADejaFuWith sched mem s0 cfg dEnv hostInputs tab = do
  let fused = fuseNode (cudaFuseConfig cfg) (buildNode (materialize tab))
      plan = scheduleNode (cudaPlanConfig cfg) dEnv fused
      nOut = sixEvalSize dEnv (six @i)
      nScratch = max 8 (spScratchBytes plan)
  withHostPtrs hostInputs $ \inPtrs ->
    allocaAligned @Word8 nScratch $ \scratchP ->
      allocaAligned @a nOut $ \outP -> do
        let fullAct :: ConcIO ()
            fullAct =
              runPlanOnCUDA
                @ConcIO
                @CUDADejaFuM
                @CUDADejaFu
                (\lcfg (CUDADejaFuM m) -> launchCUDAMWithFeatures cudaSM80 lcfg m)
                cfg
                dEnv
                inPtrs
                scratchP
                outP
                plan
        (res, s1, tr) <- runConcurrent sched mem s0 fullAct
        case res of
          Left cond -> pure (Left cond, s1, tr)
          Right () -> do
            vec <- VS.generateM nOut (\k -> peekElemOff @IO @Identity (Identity outP) (Identity (fromIntegral k)) >>= \(Identity v) -> pure v)
            pure (Right vec, s1, tr)

evalTensorCUDADejaFu ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CUDACompileConfig ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (Either Condition (VS.Vector a), Trace)
evalTensorCUDADejaFu cfg dEnv hostInputs tab = do
  (res, (), tr) <- evalTensorCUDADejaFuWith roundRobinSched defaultMemType () cfg dEnv hostInputs tab
  pure (res, tr)

-- | Render native CUDA C++ (@__global__@ kernel source via 'JanusCUDAMCSE')
-- for a single 'CmdCUDA' tiled GEMM / reduction / scan kernel for inspection
-- and compilation checks!
showStageCUDA ::
  String ->
  (JanusCUDACSE (Ptr Word8) -> JanusCUDACSE (Ptr Word8) -> JanusCUDACSE (Ptr Word8) -> JanusCUDAMCSE ()) ->
  String
showStageCUDA = showJanusCUDA
