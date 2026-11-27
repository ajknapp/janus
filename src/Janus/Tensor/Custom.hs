{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Open-world "back door" constructors allowing users to inject custom
-- primitives written directly in Janus without modifying @Janus.Tensor@:
--
-- * 'ScalarPrimSpec': pure pointwise Janus expressions that fuse directly
--   inside @For@/@Reduce@/@Scan@/@TensorST@ loops on both CPU and GPU.
-- * 'KernelPrimSpec': whole-tensor Janus commands operating on materialized
--   'TensorView's, with Argobots multi-threaded CPU lowering and optional
--   CUDA kernel lowering (automatically testable on CPU via 'CUDAIO' and
--   'CUDADejaFu').
--
-- Both carry an open-world 'PrimAttrMap' so external libraries (such as a
-- separate AD package) can attach derivative rules or hints without coupling
-- @Janus.Tensor@ to AD.
module Janus.Tensor.Custom
  ( -- * Purity & Heterogeneous Argument Lists
    PrimPurity (..),
    HExprs (..),
    HViews (..),

    -- * Scalar Backdoor Primitive
    ScalarPrimSpec (..),
    mkScalarPrim,

    -- * Whole-Tensor Kernel Backdoor Primitive
    CUDAKernelLowering (..),
    KernelPrimSpec (..),
    mkKernelPrimCPU,
    mkKernelPrim,
  )
where

import qualified Data.Kind as K
import qualified Data.Text as Text
import Janus.Backend.C.CUDA (CmdCUDA)
import Janus.Backend.C.CUDA.Foreign (CUlaunchConfig)
import Janus.Command.Parallel (CmdParWorker)
import Janus.Constraints (ExpJanus)
import Janus.Tensor.Algebra (TensorScalar)
import Janus.Tensor.Index (DimEnv, IxKind, SIx)
import Janus.Tensor.Types (PrimAttrMap, TensorView, emptyPrimAttrs)

data PrimPurity
  = PrimPure
  | PrimEffectful
  deriving (Eq, Ord, Show)

-- | Heterogeneous list of Janus scalar expressions for 'ScalarPrimSpec'.
data HExprs (e :: K.Type -> K.Type) (ts :: [K.Type]) where
  HENil :: HExprs e '[]
  HECons :: (TensorScalar a) => !(e a) -> !(HExprs e ts) -> HExprs e (a ': ts)

-- | Heterogeneous list of runtime 'TensorView's for 'KernelPrimSpec'.
data HViews (e :: K.Type -> K.Type) (shapes :: [IxKind]) (a :: K.Type) where
  HVNil :: HViews e '[] a
  HVCons :: !(TensorView e sh a) -> !(HViews e shs a) -> HViews e (sh ': shs) a

-- | Open-world pointwise scalar primitive written in Janus ('ExpJanus').
-- Fuses cleanly inside @For@, @Reduce@, @Scan@, and @TensorST@ loops.
data ScalarPrimSpec (args :: [K.Type]) (ret :: K.Type) = ScalarPrimSpec
  { spName :: !Text.Text,
    spLower :: forall e. (ExpJanus e) => HExprs e args -> e ret,
    spAttrs :: !PrimAttrMap
  }

mkScalarPrim ::
  Text.Text ->
  (forall e. (ExpJanus e) => HExprs e args -> e ret) ->
  ScalarPrimSpec args ret
mkScalarPrim name lower =
  ScalarPrimSpec
    { spName = name,
      spLower = lower,
      spAttrs = emptyPrimAttrs
    }

-- | CUDA kernel lowering wrapper (avoids impredicative polymorphism inside 'Maybe').
data CUDAKernelLowering (inShapes :: [IxKind]) (outShape :: IxKind) (a :: K.Type) = CUDAKernelLowering
  { cklLaunch :: !(DimEnv -> CUlaunchConfig),
    cklBody ::
      forall m e.
      (CmdCUDA m e) =>
      HViews e inShapes a ->
      TensorView e outShape a ->
      m ()
  }

-- | Open-world whole-tensor kernel primitive written in Janus.
--
-- * 'kpLowerCPU' has full 'CmdParWorker' access (including Argobots
--   'Janus.Command.Parallel.parallelForRangeM' and thread-safe atomics).
-- * 'kpLowerCUDA' (optional) provides a launch-config calculator and a
--   'CmdCUDA' kernel that executes on native GPU ('JanusCUDAMCSE') AND CPU
--   simulators ('CUDAIOM' / 'CUDADejaFuM').
data KernelPrimSpec (inShapes :: [IxKind]) (outShape :: IxKind) (a :: K.Type) = KernelPrimSpec
  { kpName :: !Text.Text,
    kpPurity :: !PrimPurity,
    kpOutShape :: !(SIx outShape),
    kpLowerCPU ::
      forall m e.
      (CmdParWorker m e) =>
      HViews e inShapes a ->
      TensorView e outShape a ->
      m (),
    kpLowerCUDA :: !(Maybe (CUDAKernelLowering inShapes outShape a)),
    kpAttrs :: !PrimAttrMap
  }

mkKernelPrimCPU ::
  Text.Text ->
  SIx outShape ->
  (forall m e. (CmdParWorker m e) => HViews e inShapes a -> TensorView e outShape a -> m ()) ->
  KernelPrimSpec inShapes outShape a
mkKernelPrimCPU name outSh cpuLower =
  KernelPrimSpec
    { kpName = name,
      kpPurity = PrimPure,
      kpOutShape = outSh,
      kpLowerCPU = cpuLower,
      kpLowerCUDA = Nothing,
      kpAttrs = emptyPrimAttrs
    }

mkKernelPrim ::
  Text.Text ->
  SIx outShape ->
  (forall m e. (CmdParWorker m e) => HViews e inShapes a -> TensorView e outShape a -> m ()) ->
  (DimEnv -> CUlaunchConfig) ->
  (forall m e. (CmdCUDA m e) => HViews e inShapes a -> TensorView e outShape a -> m ()) ->
  KernelPrimSpec inShapes outShape a
mkKernelPrim name outSh cpuLower mkLaunch cudaLower =
  KernelPrimSpec
    { kpName = name,
      kpPurity = PrimPure,
      kpOutShape = outSh,
      kpLowerCPU = cpuLower,
      kpLowerCUDA = Just (CUDAKernelLowering mkLaunch cudaLower),
      kpAttrs = emptyPrimAttrs
    }
