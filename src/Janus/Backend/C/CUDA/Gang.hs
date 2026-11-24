{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O0 -Wno-orphans #-}

module Janus.Backend.C.CUDA.Gang where

import Control.Monad.Trans.Class (lift)
import Janus.Backend.C.CUDA.CPU ()
import Janus.Backend.C.CUDA.CPUSIMD ()
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.Runtime
import Janus.Backend.C.CUDA.SIMD.Arith ()
import Janus.Backend.C.CUDA.SIMD.Base ()
import Janus.Backend.C.CUDA.SIMD.Bits ()
import Janus.Backend.C.CUDA.SIMD.Cast ()
import Janus.Backend.C.CUDA.SIMD.Float ()
import Janus.Backend.C.CUDA.SIMD.MaskCmp ()
import Janus.Backend.C.CUDA.SIMD.Mem ()
import Janus.Backend.C.CUDA.SIMD.ReduceDot ()
import Janus.Backend.C.CUDA.SIMD.Shuffle ()
import Janus.Backend.C.CUDA.Types
import Janus.SPMD.Gang
  ( CmdOptimalGang (..),
    JanusTypedChain (..),
    OptimalGangCaps (..),
    dispatchOptimalGang,
    optimalCUDAGangSize,
  )

--------------------------------------------------------------------------------
-- Optimal Non-Splitting Gang Dispatch for CUDA Backends
--------------------------------------------------------------------------------

instance CmdOptimalGang JanusCUDAM JanusCUDA where
  optimalGangCaps = OptimalGangCaps JTChainC
  withOptimalGang pa k = do
    fs <- JanusCUDAM (lift askCUDAFeatures)
    dispatchOptimalGang optimalGangCaps (optimalCUDAGangSize fs pa) pa k

instance CmdOptimalGang CUDAIOM CUDAIO where
  optimalGangCaps = OptimalGangCaps JTChainId
  withOptimalGang pa k = do
    fs <- askCUDAIOFeatures
    dispatchOptimalGang optimalGangCaps (optimalCUDAGangSize fs pa) pa k

instance CmdOptimalGang CUDADejaFuM CUDADejaFu where
  optimalGangCaps = OptimalGangCaps JTChainId
  withOptimalGang pa k = do
    fs <- askCUDADejaFuFeatures
    dispatchOptimalGang optimalGangCaps (optimalCUDAGangSize fs pa) pa k
