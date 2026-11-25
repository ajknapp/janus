{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Backend.C.CUDA
  ( module Janus.Backend.C.CUDA.Types,
    module Janus.Backend.C.CUDA.Kernel,
    module Janus.Backend.C.CUDA.Runtime,
    module Janus.Backend.C.CUDA.CPUSIMD,
    module Janus.Backend.C.CUDA.SIMD.Base,
    module Janus.Backend.C.CUDA.SIMD.MaskCmp,
    module Janus.Backend.C.CUDA.SIMD.Float,
    module Janus.Backend.C.CUDA.SIMD.Bits,
    module Janus.Backend.C.CUDA.SIMD.ReduceDot,

    -- * CSE-Backed CUDA Expression, Command, & Reference Type Aliases
    JanusCUDACSE,
    JanusCUDAMCSE,
    JanusCUDARefCSE,

    -- * CSE-to-CUDA Compilation & Evaluation Classes
    JanusCUDACSEParam,
    JanusCUDACSEEvaluate,
    JanusCUDACSEEval,
    mkCUDAFeatureMap,

    -- * CSE-Backed CUDA Inspection & Kernel Launch Entrypoints
    showJanusCUDAWith,
    showJanusCUDA,
    printJanusCUDAWith,
    printJanusCUDA,
    withJanusCUDAFnWith,
    withJanusCUDAWith,
    withJanusCUDAFn,
    withJanusCUDA,
  )
where

import qualified Data.Dependent.HashMap as DHM
import Data.Functor.Identity (Identity (..))
import Janus.Backend.C.Build (CBuildConfig)
import Janus.Backend.C.CUDA.CPU ()
import Janus.Backend.C.CUDA.CPUSIMD
import Janus.Backend.C.CUDA.Foreign (CUctx, CUdevice, CUfunction, CUlaunchConfig)
import Janus.Backend.C.CUDA.Gang ()
import Janus.Backend.C.CUDA.Kernel
import Janus.Backend.C.CUDA.Runtime
import Janus.Backend.C.CUDA.SIMD.Arith ()
import Janus.Backend.C.CUDA.SIMD.Base
import Janus.Backend.C.CUDA.SIMD.Bits
import Janus.Backend.C.CUDA.SIMD.Cast ()
import Janus.Backend.C.CUDA.SIMD.Float
import Janus.Backend.C.CUDA.SIMD.MaskCmp
import Janus.Backend.C.CUDA.SIMD.Mem ()
import Janus.Backend.C.CUDA.SIMD.ReduceDot
import Janus.Backend.C.CUDA.SIMD.Shuffle ()
import Janus.Backend.C.CUDA.Types
import Janus.Backend.C.Target (CUDAFeatures, TargetFeatureMap, cudaFeatureKey, cudaSM80, observeCUDAFeatures)
import Janus.CSE
  ( CSEExp,
    CSEInterpret,
    CSEM,
    CSEOptimize,
    CSERef,
    CSETarget,
    withCSEWithFeatures,
  )

type JanusCUDACSE = CSEExp JanusCUDA

type JanusCUDAMCSE = CSEM JanusCUDAM JanusCUDA

type JanusCUDARefCSE = CSERef JanusCUDAM JanusCUDA

class
  (CSEOptimize r, CSEInterpret r, JanusCUDAParam (CSETarget r)) =>
  JanusCUDACSEParam r

instance
  (CSEOptimize r, CSEInterpret r, JanusCUDAParam (CSETarget r)) =>
  JanusCUDACSEParam r

class
  (JanusCUDACSEParam r, JanusCUDAEvaluate (CSETarget r)) =>
  JanusCUDACSEEvaluate r

instance
  (JanusCUDACSEParam r, JanusCUDAEvaluate (CSETarget r)) =>
  JanusCUDACSEEvaluate r

type JanusCUDACSEEval r = JanusCUDAEval (CSETarget r)

mkCUDAFeatureMap :: CUDAFeatures -> TargetFeatureMap
mkCUDAFeatureMap fs = DHM.singleton cudaFeatureKey (Identity fs)

showJanusCUDAWith :: forall r. (JanusCUDACSEParam r) => CUDAFeatures -> String -> r -> String
showJanusCUDAWith fs name a =
  showRawJanusCUDAWith fs name (withCSEWithFeatures (mkCUDAFeatureMap fs) a)

showJanusCUDA :: forall r. (JanusCUDACSEParam r) => String -> r -> String
showJanusCUDA = showJanusCUDAWith cudaSM80

printJanusCUDAWith :: (JanusCUDACSEParam r) => CUDAFeatures -> r -> IO ()
printJanusCUDAWith fs = putStrLn . showJanusCUDAWith fs "janus_main"

printJanusCUDA :: (JanusCUDACSEParam r) => r -> IO ()
printJanusCUDA = putStrLn . showJanusCUDA "janus_main"

withJanusCUDAFnWith ::
  forall r a.
  (JanusCUDACSEEvaluate r) =>
  CUDAFeatures ->
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDACSEEval r) -> IO a) ->
  IO a
withJanusCUDAFnWith reqFeatures dev ctx config f k =
  withRawJanusCUDAFnWith
    reqFeatures
    dev
    ctx
    config
    (withCSEWithFeatures (mkCUDAFeatureMap reqFeatures) f)
    k

withJanusCUDAWith ::
  (JanusCUDACSEEvaluate r) =>
  CUDAFeatures ->
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  ((CUlaunchConfig -> JanusCUDACSEEval r) -> IO a) ->
  IO a
withJanusCUDAWith fs dev ctx config f k = withJanusCUDAFnWith fs dev ctx config f (const k)

withJanusCUDAFn ::
  (JanusCUDACSEEvaluate r) =>
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDACSEEval r) -> IO a) ->
  IO a
withJanusCUDAFn dev ctx config f k = do
  fs <- observeCUDAFeatures dev
  withJanusCUDAFnWith fs dev ctx config f k

withJanusCUDA ::
  (JanusCUDACSEEvaluate r) =>
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  ((CUlaunchConfig -> JanusCUDACSEEval r) -> IO a) ->
  IO a
withJanusCUDA dev ctx config f k = withJanusCUDAFn dev ctx config f (const k)
