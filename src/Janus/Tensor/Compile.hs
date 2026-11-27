{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level function-compilation API for @Janus.Tensor@ (modeled after
-- 'Janus.Backend.C.withJanusC'): users write plain, purely functional Haskell
-- functions over 'Tab' and 'Expr' arguments and compile them directly—never
-- needing to touch 'inputTab', 'HostInputMap', or string tensor names.
--
-- * 'compileTensor' / 'compileTensorWithScratch': compiles a Haskell tensor
--   function directly into a curried Janus monadic action taking 'TensorView's
--   (and scalar @e a@ arguments) in ANY 'CmdParWorker' monad ('IO',
--   'ParallelDejaFuM', 'JanusCMCSE', 'LinuxX86_64CMCSE').
-- * 'compileTensorPtrs': same, taking raw @e (Ptr a)@ pointers—plugs directly
--   into 'Janus.Backend.C.withJanusC' and 'Janus.Backend.C.showJanusC'!
-- * 'runTensorIO', 'runTensorC', 'runTensorCUDAIO', 'runTensorCUDADejaFu',
--   'runTensorDejaFu': direct multi-backend host runners taking 'VS.Vector's
--   (and scalar @a@s) as ordinary curried Haskell arguments.
module Janus.Tensor.Compile
  ( -- * Function Compilation Typeclass (`TensorFn`)
    TensorFn (..),

    -- * Direct In-Janus Monadic Compilation (Zero `inputTab` Boilerplate!)
    compileTensor,
    compileTensorWithTree,
    compileTensorWithScratch,
    compileTensorPtrs,
    compileTensorPtrsWithTree,

    -- * Curried Multi-Backend Runners & Inspection
    runTensorIO,
    runTensorC,
    runTensorCWithTree,
    runTensorDejaFu,
    runTensorCUDAIO,
    runTensorCUDADejaFu,
    runTensorCUDADejaFuWith,
    showTensorFnC,
    planFnScratchBytes,
    buildTensorFn,
  )
where

import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Janus.Backend.C.Build (CBuildConfig)
import Janus.Command.Memory (CmdRefPtr (..))
import Janus.Command.Parallel (CmdParWorker)
import Janus.Command.Ref (CmdRef (..))
import Janus.Constraints (withJanusType)
import Janus.Tensor.Algebra (TensorScalar (..))
import Janus.Tensor.Compile.Fuse (defaultFuseConfig, fuseNode)
import Janus.Tensor.Compile.Schedule (ScratchPolicy (..), planScratchBytes)
import Janus.Tensor.Contract.Hypergraph (ContractionTree)
import Janus.Tensor.Contract.Plan (PlanConfig)
import Janus.Tensor.DSL (forIx, inputTab, ixConst, (!))
import Janus.Tensor.IR (Expr, RawNode, Tab, buildNode, materialize)
import Janus.Tensor.Index (DimEnv, IxKind (..), IxSet (..), SIx (..))
import Janus.Tensor.Lower.CPU
  ( CompileConfig (..),
    HostInputMap,
    InputMap,
    SomeExp (..),
    SomeSIxSpec (..),
    bindHostInput,
    bindInputView,
    emptyHostInputs,
    emptyInputs,
    evalTensorCWithTree,
    evalTensorDejaFu,
    evalTensorIO,
    lowerNodeCPUWithOverride,
    showTensorC,
  )
import Janus.Tensor.Lower.CUDA
  ( CUDACompileConfig,
    evalTensorCUDADejaFu,
    evalTensorCUDADejaFuWith,
    evalTensorCUDAIO,
  )
import Janus.Tensor.Types (TensorView, mkTensorView)
import Test.DejaFu.Conc (Condition, MemType, Trace)
import Test.DejaFu.Schedule (Scheduler)

argName :: Int -> Text.Text
argName k = "__arg_" <> Text.pack (show k)

-- | Bind accumulated scalar arguments as 1-element stack-resident 'TensorView's
-- inside the Janus monad (zero heap allocation via 'newRef' + 'withRefPtr').
withScalarInputs ::
  forall m e r.
  (CmdParWorker m e) =>
  [(Text.Text, SomeExp e)] ->
  InputMap e ->
  (InputMap e -> m r) ->
  m r
withScalarInputs [] inMap k = k inMap
withScalarInputs ((nm, SomeExp (val :: e b)) : rest) inMap k =
  withJanusType @b @e $ do
    r <- newRef val
    withRefPtr r $ \p ->
      withScalarInputs rest (bindInputView nm (mkTensorView SIxUnit p) inMap) k

--------------------------------------------------------------------------------
-- `TensorFn`: Curried Function Compilation Class
--------------------------------------------------------------------------------

-- | Class of purely functional Haskell tensor functions over any number of
-- @Tab i (Expr a)@ and scalar @Expr a@ parameters, returning either a tensor
-- @Tab o (Expr b)@ or a scalar @Expr b@.
class TensorFn (f :: K.Type) where
  -- | Type of the compiled function operating on 'TensorView's inside a Janus monad @(m, e)@.
  type TensorJanusFn (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) f :: K.Type

  -- | Type of the compiled function operating on raw @e (Ptr a)@ pointers inside @(m, e)@
  -- (directly compatible with 'Janus.Backend.C.withJanusC' and 'Janus.Backend.C.showJanusC').
  type TensorPtrFn (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) f :: K.Type

  -- | Type of host runners ('runTensorIO', 'runTensorC', 'runTensorCUDAIO').
  type TensorHostFn (m :: K.Type -> K.Type) f :: K.Type

  -- | Type of DejaFu runners ('runTensorDejaFu', 'runTensorCUDADejaFu').
  type TensorDejaFuFn f :: K.Type

  -- | Type of parameterized DejaFu runner ('runTensorCUDADejaFuWith').
  type TensorDejaFuWithFn s f :: K.Type

  -- | Output index shape of the tensor function (@'IxUnit@ for scalar-returning functions).
  type TensorOutShape f :: IxKind

  -- | Output scalar element type of the tensor function.
  type TensorOutElem f :: K.Type

  compileTensorGo ::
    (CmdParWorker m e) =>
    Int ->
    CompileConfig ->
    Maybe ContractionTree ->
    DimEnv ->
    InputMap e ->
    [(Text.Text, SomeExp e)] ->
    Maybe (e (Ptr Word8)) ->
    f ->
    TensorJanusFn m e f

  compileTensorPtrsGo ::
    (CmdParWorker m e) =>
    Int ->
    CompileConfig ->
    Maybe ContractionTree ->
    DimEnv ->
    InputMap e ->
    [(Text.Text, SomeExp e)] ->
    Maybe (e (Ptr Word8)) ->
    f ->
    TensorPtrFn m e f

  runTensorIOGo ::
    Int ->
    CompileConfig ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorHostFn IO f

  runTensorCGo ::
    Int ->
    CBuildConfig ->
    CompileConfig ->
    Maybe ContractionTree ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorHostFn IO f

  runTensorDejaFuGo ::
    Int ->
    CompileConfig ->
    Int ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorDejaFuFn f

  runTensorCUDAIOGo ::
    Int ->
    CUDACompileConfig ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorHostFn IO f

  runTensorCUDADejaFuGo ::
    Int ->
    CUDACompileConfig ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorDejaFuFn f

  runTensorCUDADejaFuWithGo ::
    Int ->
    Scheduler s ->
    MemType ->
    s ->
    CUDACompileConfig ->
    DimEnv ->
    HostInputMap ->
    f ->
    TensorDejaFuWithFn s f

  showTensorFnCGo ::
    Int ->
    CompileConfig ->
    DimEnv ->
    [(Text.Text, SomeSIxSpec)] ->
    String ->
    f ->
    IO String

  planFnScratchBytesGo ::
    Int ->
    PlanConfig ->
    DimEnv ->
    f ->
    Int

  buildTensorFnGo ::
    Int ->
    f ->
    RawNode (TensorOutShape f) (TensorOutElem f)

-- | Base case: tensor output @Tab i (Expr a)@.
instance (IxSet i, TensorScalar a) => TensorFn (Tab i (Expr a)) where
  type TensorJanusFn m e (Tab i (Expr a)) = TensorView e i a -> m ()
  type TensorPtrFn m e (Tab i (Expr a)) = e (Ptr a) -> m ()
  type TensorHostFn m (Tab i (Expr a)) = m (VS.Vector a)
  type TensorDejaFuFn (Tab i (Expr a)) = IO (Either Condition (VS.Vector a), Trace)
  type TensorDejaFuWithFn s (Tab i (Expr a)) = IO (Either Condition (VS.Vector a), s, Trace)
  type TensorOutShape (Tab i (Expr a)) = i
  type TensorOutElem (Tab i (Expr a)) = a

  compileTensorGo _ cfg mTree dEnv inMap scalars mScratch tab outView =
    withScalarInputs scalars inMap $ \fullIn ->
      lowerNodeCPUWithOverride cfg mTree dEnv fullIn mScratch (buildNode (materialize tab)) outView

  compileTensorPtrsGo _ cfg mTree dEnv inMap scalars mScratch tab outPtr =
    withScalarInputs scalars inMap $ \fullIn ->
      lowerNodeCPUWithOverride cfg mTree dEnv fullIn mScratch (buildNode (materialize tab)) (mkTensorView (six @i) outPtr)

  runTensorIOGo _ cfg dEnv hIn tab =
    evalTensorIO cfg dEnv hIn tab

  runTensorCGo _ bCfg cfg mTree dEnv hIn tab =
    evalTensorCWithTree bCfg cfg mTree dEnv hIn tab

  runTensorDejaFuGo _ cfg nWorkers dEnv hIn tab =
    evalTensorDejaFu cfg nWorkers dEnv hIn tab

  runTensorCUDAIOGo _ cfg dEnv hIn tab =
    evalTensorCUDAIO cfg dEnv hIn tab

  runTensorCUDADejaFuGo _ cfg dEnv hIn tab =
    evalTensorCUDADejaFu cfg dEnv hIn tab

  runTensorCUDADejaFuWithGo _ sched mem s0 cfg dEnv hIn tab =
    evalTensorCUDADejaFuWith sched mem s0 cfg dEnv hIn tab

  showTensorFnCGo _ cfg dEnv revSpecs fnName tab =
    showTensorC cfg dEnv (reverse revSpecs) fnName tab

  planFnScratchBytesGo _ pCfg dEnv tab =
    planScratchBytes pCfg dEnv (fuseNode defaultFuseConfig (buildNode (materialize tab)))

  buildTensorFnGo _ tab =
    buildNode (materialize tab)

-- | Base case: scalar output @Expr a@ (writes to a stack ref in Janus — zero allocation!).
instance (TensorScalar a) => TensorFn (Expr a) where
  type TensorJanusFn m e (Expr a) = m (e a)
  type TensorPtrFn m e (Expr a) = m (e a)
  type TensorHostFn m (Expr a) = m a
  type TensorDejaFuFn (Expr a) = IO (Either Condition a, Trace)
  type TensorDejaFuWithFn s (Expr a) = IO (Either Condition a, s, Trace)
  type TensorOutShape (Expr a) = 'IxUnit
  type TensorOutElem (Expr a) = a

  compileTensorGo ::
    forall m e.
    (CmdParWorker m e) =>
    Int ->
    CompileConfig ->
    Maybe ContractionTree ->
    DimEnv ->
    InputMap e ->
    [(Text.Text, SomeExp e)] ->
    Maybe (e (Ptr Word8)) ->
    Expr a ->
    m (e a)
  compileTensorGo _ cfg mTree dEnv inMap scalars mScratch expr =
    withJanusType @a @e $
      withScalarInputs scalars inMap $ \fullIn -> do
        outRef <- newRef (scalarZeroE (Proxy @a))
        withRefPtr outRef $ \outP ->
          lowerNodeCPUWithOverride
            cfg
            mTree
            dEnv
            fullIn
            mScratch
            (buildNode (materialize (forIx @'IxUnit (\_ -> expr))))
            (mkTensorView SIxUnit outP)
        readRef outRef

  compileTensorPtrsGo = compileTensorGo

  runTensorIOGo _ cfg dEnv hIn expr = do
    v <- evalTensorIO cfg dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (v VS.! 0)

  runTensorCGo _ bCfg cfg mTree dEnv hIn expr = do
    v <- evalTensorCWithTree bCfg cfg mTree dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (v VS.! 0)

  runTensorDejaFuGo _ cfg nWorkers dEnv hIn expr = do
    (eRes, tr) <- evalTensorDejaFu cfg nWorkers dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (fmap (VS.! 0) eRes, tr)

  runTensorCUDAIOGo _ cfg dEnv hIn expr = do
    v <- evalTensorCUDAIO cfg dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (v VS.! 0)

  runTensorCUDADejaFuGo _ cfg dEnv hIn expr = do
    (eRes, tr) <- evalTensorCUDADejaFu cfg dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (fmap (VS.! 0) eRes, tr)

  runTensorCUDADejaFuWithGo _ sched mem s0 cfg dEnv hIn expr = do
    (eRes, s1, tr) <- evalTensorCUDADejaFuWith sched mem s0 cfg dEnv hIn (forIx @'IxUnit (\_ -> expr))
    pure (fmap (VS.! 0) eRes, s1, tr)

  showTensorFnCGo _ cfg dEnv revSpecs fnName expr =
    showTensorC cfg dEnv (reverse revSpecs) fnName (forIx @'IxUnit (\_ -> expr))

  planFnScratchBytesGo _ pCfg dEnv expr =
    planScratchBytes pCfg dEnv (buildNode (materialize (forIx @'IxUnit (\_ -> expr))))

  buildTensorFnGo _ expr =
    buildNode (materialize (forIx @'IxUnit (\_ -> expr)))

-- | Recursive case: tensor parameter @Tab i (Expr a) -> r@.
instance (IxSet i, TensorScalar a, TensorFn r) => TensorFn (Tab i (Expr a) -> r) where
  type TensorJanusFn m e (Tab i (Expr a) -> r) = TensorView e i a -> TensorJanusFn m e r
  type TensorPtrFn m e (Tab i (Expr a) -> r) = e (Ptr a) -> TensorPtrFn m e r
  type TensorHostFn m (Tab i (Expr a) -> r) = VS.Vector a -> TensorHostFn m r
  type TensorDejaFuFn (Tab i (Expr a) -> r) = VS.Vector a -> TensorDejaFuFn r
  type TensorDejaFuWithFn s (Tab i (Expr a) -> r) = VS.Vector a -> TensorDejaFuWithFn s r
  type TensorOutShape (Tab i (Expr a) -> r) = TensorOutShape r
  type TensorOutElem (Tab i (Expr a) -> r) = TensorOutElem r

  compileTensorGo idx cfg mTree dEnv inMap scalars mScratch fn inView =
    let nm = argName idx
     in compileTensorGo (idx + 1) cfg mTree dEnv (bindInputView nm inView inMap) scalars mScratch (fn (inputTab @i @a nm))

  compileTensorPtrsGo idx cfg mTree dEnv inMap scalars mScratch fn inPtr =
    let nm = argName idx
     in compileTensorPtrsGo (idx + 1) cfg mTree dEnv (bindInputView nm (mkTensorView (six @i) inPtr) inMap) scalars mScratch (fn (inputTab @i @a nm))

  runTensorIOGo idx cfg dEnv hIn fn vec =
    let nm = argName idx
     in runTensorIOGo (idx + 1) cfg dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  runTensorCGo idx bCfg cfg mTree dEnv hIn fn vec =
    let nm = argName idx
     in runTensorCGo (idx + 1) bCfg cfg mTree dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  runTensorDejaFuGo idx cfg nWorkers dEnv hIn fn vec =
    let nm = argName idx
     in runTensorDejaFuGo (idx + 1) cfg nWorkers dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  runTensorCUDAIOGo idx cfg dEnv hIn fn vec =
    let nm = argName idx
     in runTensorCUDAIOGo (idx + 1) cfg dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  runTensorCUDADejaFuGo idx cfg dEnv hIn fn vec =
    let nm = argName idx
     in runTensorCUDADejaFuGo (idx + 1) cfg dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  runTensorCUDADejaFuWithGo idx sched mem s0 cfg dEnv hIn fn vec =
    let nm = argName idx
     in runTensorCUDADejaFuWithGo (idx + 1) sched mem s0 cfg dEnv (bindHostInput @i nm vec hIn) (fn (inputTab @i @a nm))

  showTensorFnCGo idx cfg dEnv revSpecs fnName fn =
    let nm = argName idx
        spec = (nm, SomeSIxSpec (six @i) (Proxy @a))
     in showTensorFnCGo (idx + 1) cfg dEnv (spec : revSpecs) fnName (fn (inputTab @i @a nm))

  planFnScratchBytesGo idx pCfg dEnv fn =
    let nm = argName idx
     in planFnScratchBytesGo (idx + 1) pCfg dEnv (fn (inputTab @i @a nm))

  buildTensorFnGo idx fn =
    let nm = argName idx
     in buildTensorFnGo (idx + 1) (fn (inputTab @i @a nm))

-- | Recursive case: scalar parameter @Expr a -> r@.
instance (TensorScalar a, TensorFn r) => TensorFn (Expr a -> r) where
  type TensorJanusFn m e (Expr a -> r) = e a -> TensorJanusFn m e r
  type TensorPtrFn m e (Expr a -> r) = e a -> TensorPtrFn m e r
  type TensorHostFn m (Expr a -> r) = a -> TensorHostFn m r
  type TensorDejaFuFn (Expr a -> r) = a -> TensorDejaFuFn r
  type TensorDejaFuWithFn s (Expr a -> r) = a -> TensorDejaFuWithFn s r
  type TensorOutShape (Expr a -> r) = TensorOutShape r
  type TensorOutElem (Expr a -> r) = TensorOutElem r

  compileTensorGo idx cfg mTree dEnv inMap scalars mScratch fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in compileTensorGo (idx + 1) cfg mTree dEnv inMap ((nm, SomeExp scalarVal) : scalars) mScratch (fn argExpr)

  compileTensorPtrsGo idx cfg mTree dEnv inMap scalars mScratch fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in compileTensorPtrsGo (idx + 1) cfg mTree dEnv inMap ((nm, SomeExp scalarVal) : scalars) mScratch (fn argExpr)

  runTensorIOGo idx cfg dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorIOGo (idx + 1) cfg dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  runTensorCGo idx bCfg cfg mTree dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorCGo (idx + 1) bCfg cfg mTree dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  runTensorDejaFuGo idx cfg nWorkers dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorDejaFuGo (idx + 1) cfg nWorkers dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  runTensorCUDAIOGo idx cfg dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorCUDAIOGo (idx + 1) cfg dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  runTensorCUDADejaFuGo idx cfg dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorCUDADejaFuGo (idx + 1) cfg dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  runTensorCUDADejaFuWithGo idx sched mem s0 cfg dEnv hIn fn scalarVal =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in runTensorCUDADejaFuWithGo (idx + 1) sched mem s0 cfg dEnv (bindHostInput @'IxUnit nm (VS.singleton scalarVal) hIn) (fn argExpr)

  showTensorFnCGo idx cfg dEnv revSpecs fnName fn =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
        spec = (nm, SomeSIxSpec SIxUnit (Proxy @a))
     in showTensorFnCGo (idx + 1) cfg dEnv (spec : revSpecs) fnName (fn argExpr)

  planFnScratchBytesGo idx pCfg dEnv fn =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in planFnScratchBytesGo (idx + 1) pCfg dEnv (fn argExpr)

  buildTensorFnGo idx fn =
    let nm = argName idx
        argExpr = inputTab @'IxUnit @a nm ! ixConst 0
     in buildTensorFnGo (idx + 1) (fn argExpr)

--------------------------------------------------------------------------------
-- Public Top-Level Entry Points
--------------------------------------------------------------------------------

-- | Compile a purely functional Haskell tensor function directly into a curried
-- Janus monadic command accepting 'TensorView' (and scalar @e a@) arguments!
compileTensor ::
  forall f m e.
  (TensorFn f, CmdParWorker m e) =>
  CompileConfig ->
  DimEnv ->
  f ->
  TensorJanusFn m e f
compileTensor cfg dEnv = compileTensorGo @f @m @e 0 cfg Nothing dEnv emptyInputs [] Nothing

-- | Like 'compileTensor', with an explicit 'ContractionTree' override (e.g.,
-- chosen empirically via 'Janus.Tensor.Bench').
compileTensorWithTree ::
  forall f m e.
  (TensorFn f, CmdParWorker m e) =>
  CompileConfig ->
  Maybe ContractionTree ->
  DimEnv ->
  f ->
  TensorJanusFn m e f
compileTensorWithTree cfg mTree dEnv = compileTensorGo @f @m @e 0 cfg mTree dEnv emptyInputs [] Nothing

-- | Zero-heap-allocation variant of 'compileTensor' using a caller-supplied
-- scratchpad buffer of size @planFnScratchBytes@.
compileTensorWithScratch ::
  forall f m e.
  (TensorFn f, CmdParWorker m e) =>
  CompileConfig ->
  DimEnv ->
  e (Ptr Word8) ->
  f ->
  TensorJanusFn m e f
compileTensorWithScratch cfg dEnv scratchBuf =
  compileTensorGo @f @m @e 0 (cfg {ccScratchPolicy = ScratchCallerBuf}) Nothing dEnv emptyInputs [] (Just scratchBuf)

-- | Compile a purely functional Haskell tensor function into a curried Janus
-- command taking raw @e (Ptr a)@ pointers—plugs directly into
-- 'Janus.Backend.C.withJanusC' and 'Janus.Backend.C.showJanusC'!
compileTensorPtrs ::
  forall f m e.
  (TensorFn f, CmdParWorker m e) =>
  CompileConfig ->
  DimEnv ->
  f ->
  TensorPtrFn m e f
compileTensorPtrs cfg dEnv = compileTensorPtrsGo @f @m @e 0 cfg Nothing dEnv emptyInputs [] Nothing

compileTensorPtrsWithTree ::
  forall f m e.
  (TensorFn f, CmdParWorker m e) =>
  CompileConfig ->
  Maybe ContractionTree ->
  DimEnv ->
  f ->
  TensorPtrFn m e f
compileTensorPtrsWithTree cfg mTree dEnv = compileTensorPtrsGo @f @m @e 0 cfg mTree dEnv emptyInputs [] Nothing

runTensorIO :: (TensorFn f) => CompileConfig -> DimEnv -> f -> TensorHostFn IO f
runTensorIO cfg dEnv = runTensorIOGo 0 cfg dEnv emptyHostInputs

runTensorC :: (TensorFn f) => CBuildConfig -> CompileConfig -> DimEnv -> f -> TensorHostFn IO f
runTensorC bCfg cfg dEnv = runTensorCGo 0 bCfg cfg Nothing dEnv emptyHostInputs

runTensorCWithTree ::
  (TensorFn f) =>
  CBuildConfig ->
  CompileConfig ->
  Maybe ContractionTree ->
  DimEnv ->
  f ->
  TensorHostFn IO f
runTensorCWithTree bCfg cfg mTree dEnv = runTensorCGo 0 bCfg cfg mTree dEnv emptyHostInputs

runTensorDejaFu :: (TensorFn f) => CompileConfig -> Int -> DimEnv -> f -> TensorDejaFuFn f
runTensorDejaFu cfg nWorkers dEnv = runTensorDejaFuGo 0 cfg nWorkers dEnv emptyHostInputs

runTensorCUDAIO :: (TensorFn f) => CUDACompileConfig -> DimEnv -> f -> TensorHostFn IO f
runTensorCUDAIO cfg dEnv = runTensorCUDAIOGo 0 cfg dEnv emptyHostInputs

runTensorCUDADejaFuWith ::
  (TensorFn f) =>
  Scheduler s ->
  MemType ->
  s ->
  CUDACompileConfig ->
  DimEnv ->
  f ->
  TensorDejaFuWithFn s f
runTensorCUDADejaFuWith sched mem s0 cfg dEnv =
  runTensorCUDADejaFuWithGo 0 sched mem s0 cfg dEnv emptyHostInputs

-- | Convenience 'runTensorCUDADejaFu' using 'roundRobinSched' and 'defaultMemType'.
runTensorCUDADejaFu ::
  (TensorFn f) =>
  CUDACompileConfig ->
  DimEnv ->
  f ->
  TensorDejaFuFn f
runTensorCUDADejaFu cfg dEnv = runTensorCUDADejaFuGo 0 cfg dEnv emptyHostInputs

showTensorFnC :: (TensorFn f) => CompileConfig -> DimEnv -> String -> f -> IO String
showTensorFnC cfg dEnv = showTensorFnCGo 0 cfg dEnv []

planFnScratchBytes :: (TensorFn f) => PlanConfig -> DimEnv -> f -> Int
planFnScratchBytes pCfg dEnv = planFnScratchBytesGo 0 pCfg dEnv

-- | Reify a purely functional Haskell tensor function into its Merkle-hashed
-- 'RawNode' IR graph without needing manual 'inputTab' calls.
buildTensorFn :: (TensorFn f) => f -> RawNode (TensorOutShape f) (TensorOutElem f)
buildTensorFn = buildTensorFnGo 0
