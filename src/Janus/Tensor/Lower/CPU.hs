{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Multi-threaded CPU lowering for @Janus.Tensor@ targeting:
--
-- * Native compiled C ('JanusCMCSE' / 'LinuxX86_64CMCSE') with Argobots
--   'parallelForRangeM', thread-safe 'CmdAtomic', 'Janus.CSE', and **zero heap
--   allocation** for statically-sized graphs within budget ('withStackBytes'
--   or caller-provided scratchpad).
-- * Direct Haskell 'IO' / 'Identity' oracle execution.
-- * 'ParallelDejaFuM' concurrency verification via DejaFu!
-- * Seamless embedding inside larger Janus 'CmdJanus' programs operating on
--   'TensorView's (including zero-copy 'Janus.Data.Tensor.Tensor' adapters).
module Janus.Tensor.Lower.CPU
  ( -- * Compilation & Execution Configuration
    CompileConfig (..),
    defaultCompileConfig,

    -- * Runtime Input Maps
    SomeTensorView (..),
    InputMap,
    emptyInputs,
    bindInput,
    bindInputView,
    HostInput (..),
    HostInputMap,
    emptyHostInputs,
    bindHostInput,

    -- * Shared Expression & Index Evaluators (Reused by CUDA Lowering)
    SomeExp (..),
    SomeMTabPtr (..),
    LowerEnv (..),
    lookupInputPtr,
    lookupNodePtr,
    evalIxE,
    evalExprE,
    evalSTStmtE,
    evalSTProgScalarE,
    lowerBinaryContractLoops,
    withPackedPtrs,

    -- * Direct In-Janus Lowering (CPU / Argobots / DejaFu / Embedded)
    lowerNodeCPU,
    lowerNodeCPUWithOverride,
    lowerTensorCPU,
    lowerTensorCPUWithScratch,

    -- * Convenience Runners & C Code Inspection
    SomeSIxSpec (..),
    evalTensorIO,
    evalTensorDejaFu,
    evalTensorC,
    evalTensorCWithTree,
    showTensorC,
  )
where

import Control.Monad (forM_)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (eqT)
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (sizeOf))
import Janus.Backend.C (JanusCCSE, JanusCMCSE, PortableC, showJanusC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig)
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Memory
  ( CmdDynamicAligned (..),
    CmdMem (..),
    CmdStackArray (..),
    CmdStorable (..),
    ExpPtr (..),
    allocaAligned,
  )
import Janus.Command.Parallel
  ( CmdParWorker,
    CmdParallel (..),
    ParallelDejaFu (..),
    runParallelDejaFuM,
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..), Ref)
import Janus.Command.While (CmdWhile (..))
import Janus.Constraints (CmdJanus, ExpJanus, LocalArgs (..), withJanusType)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpBoolCast (..), ExpFloatingCast (..), ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SpecialMath (ExpSpecialMath (..))
import Janus.SPMD.Types (ExpPair (..))
import Janus.Tensor.Algebra
  ( SomeCommMonoid (..),
    SomeMonoid (..),
    SomeSemiring (..),
    TensorCommMonoid (..),
    TensorMonoid (..),
    TensorScalar (..),
    TensorSemiring (..),
  )
import Janus.Tensor.Compile.Fuse (FuseConfig (..), defaultFuseConfig, fuseNode)
import Janus.Tensor.Compile.Schedule
  ( BufferSlot (..),
    ScheduledPlan (..),
    ScratchPolicy (..),
    SomeStage (..),
    defaultScratchPolicy,
    scheduleNode,
  )
import Janus.Tensor.Contract.Hypergraph
  ( BinaryStepMeta (..),
    ContractionTree (..),
    Hypergraph (..),
    IndexId,
    fromContractSpec,
  )
import Janus.Tensor.Contract.Plan (PlanConfig, defaultPlanConfig)
import Janus.Tensor.Custom (HExprs (..), HViews (..), KernelPrimSpec (..), ScalarPrimSpec (..))
import Janus.Tensor.IR
  ( AccumProgram (..),
    AccumStmt (..),
    ContractOperand (..),
    ContractSpec (..),
    Digest,
    Expr,
    HExprArgs (..),
    HNodeArgs (..),
    IxExpr (..),
    RawExpr (..),
    RawNode (..),
    STProgram (..),
    STStmt (..),
    ScalarBinOp (..),
    ScalarCmpOp (..),
    ScalarUnOp (..),
    Tab (..),
    buildNode,
    ixExprShape,
    materialize,
    rawNodeDigest,
    rawNodeShape,
  )
import Janus.Tensor.Index
  ( DimEnv,
    IxMorph (..),
    IxSet (..),
    SIx (..),
    evalAffineForwardOrdE,
    evalIsoOrdE,
    evalPrismForwardOrdE,
    sixEvalSize,
  )
import Janus.Tensor.Types (TensorView (..), mkTensorView)
import Test.DejaFu.Conc (Condition, Trace)

--------------------------------------------------------------------------------
-- 1. Compilation Configuration & Runtime Input Maps
--------------------------------------------------------------------------------

data CompileConfig = CompileConfig
  { ccPlanConfig :: !PlanConfig,
    ccScratchPolicy :: !ScratchPolicy,
    ccParallelThreshold :: !Int,
    ccUseParallel :: !Bool,
    ccFuseConfig :: !FuseConfig
  }

defaultCompileConfig :: CompileConfig
defaultCompileConfig =
  CompileConfig
    { ccPlanConfig = defaultPlanConfig,
      ccScratchPolicy = defaultScratchPolicy,
      ccParallelThreshold = 256,
      ccUseParallel = True,
      ccFuseConfig = defaultFuseConfig
    }

data SomeTensorView (e :: K.Type -> K.Type) where
  SomeTensorView :: (TensorScalar a) => !(TensorView e i a) -> SomeTensorView e

type InputMap e = Map.Map Text.Text (SomeTensorView e)

emptyInputs :: InputMap e
emptyInputs = Map.empty

bindInputView :: (TensorScalar a) => Text.Text -> TensorView e i a -> InputMap e -> InputMap e
bindInputView nm tv = Map.insert nm (SomeTensorView tv)

bindInput :: forall i a e. (IxSet i, TensorScalar a) => Text.Text -> e (Ptr a) -> InputMap e -> InputMap e
bindInput nm ptr = bindInputView nm (mkTensorView (six @i) ptr)

-- | Host-side typed vector input for 'evalTensorIO' / 'evalTensorC' / 'evalTensorDejaFu'.
data HostInput where
  HostInput :: (TensorScalar a) => !(SIx i) -> !(VS.Vector a) -> HostInput

type HostInputMap = Map.Map Text.Text HostInput

emptyHostInputs :: HostInputMap
emptyHostInputs = Map.empty

bindHostInput :: forall i a. (IxSet i, TensorScalar a) => Text.Text -> VS.Vector a -> HostInputMap -> HostInputMap
bindHostInput nm vec = Map.insert nm (HostInput (six @i) vec)

--------------------------------------------------------------------------------
-- 2. Runtime Lowering Environment & Packed Pointer Transport for Parallel Workers
--------------------------------------------------------------------------------

data SomeRef (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  SomeRef :: (TensorScalar a) => !(Ref m e a) -> SomeRef m e

data SomeMTabPtr (e :: K.Type -> K.Type) where
  SomeMTabPtr :: (TensorScalar a) => !(SIx i) -> !(e (Ptr a)) -> SomeMTabPtr e

-- | Read-only environment threaded through expression and command lowering.
data LowerEnv (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) = LowerEnv
  { leDimEnv :: !DimEnv,
    leInputs :: !(Map.Map Text.Text (e (Ptr Word8))),
    leScratch :: !(e (Ptr Word8)),
    leSlots :: !(Map.Map Digest BufferSlot),
    leRootDigest :: !Digest,
    leRootOut :: !(e (Ptr Word8)),
    leIxVars :: !(IM.IntMap (e Int64)),
    leScalarVars :: !(IM.IntMap (SomeExp e)),
    leSTRefs :: !(IM.IntMap (SomeRef m e)),
    leSTMTabs :: !(IM.IntMap (SomeMTabPtr e))
  }

data SomeExp (e :: K.Type -> K.Type) where
  SomeExp :: (TensorScalar a) => !(e a) -> SomeExp e

lookupInputPtr ::
  forall a m e.
  (ExpJanus e, TensorScalar a) =>
  Text.Text ->
  LowerEnv m e ->
  e (Ptr a)
lookupInputPtr nm env =
  withJanusType @a @e $
    case Map.lookup nm (leInputs env) of
      Just p -> fromVoidPtr (toVoidPtr p)
      Nothing -> error ("Janus.Tensor.Lower: missing runtime input tensor " <> Text.unpack nm)

lookupNodePtr ::
  forall i a m e.
  (ExpJanus e, TensorScalar a) =>
  RawNode i a ->
  LowerEnv m e ->
  e (Ptr a)
lookupNodePtr node env =
  withJanusType @a @e $
    case node of
      NInput _ nm _ -> lookupInputPtr nm env
      _ ->
        let d = rawNodeDigest node
         in if d == leRootDigest env
              then fromVoidPtr (toVoidPtr (leRootOut env))
              else case Map.lookup d (leSlots env) of
                Just slot ->
                  fromVoidPtr (toVoidPtr (ptrAdd (leScratch env) (fromIntegral (bsOffsetBytes slot))))
                Nothing ->
                  error ("Janus.Tensor.Lower: unscheduled node buffer " <> show d)

-- | Pack an arbitrary list of @e (Ptr Word8)@ into a strongly-typed 'LocalArgs'
-- tuple for zero-overhead passing into Argobots 'parallelForRangeM' workers!
withPackedPtrs ::
  forall e r.
  (ExpJanus e) =>
  [e (Ptr Word8)] ->
  ( forall ts.
    LocalArgs e ts ->
    (forall e0. LocalArgs e0 ts -> [e0 (Ptr Word8)]) ->
    r
  ) ->
  r
withPackedPtrs [] k = k LANil (const [])
withPackedPtrs (p : ps) k =
  withJanusType @(Ptr Word8) @e $
    withPackedPtrs ps $ \rest unpackRest ->
      k (LACons p rest) (\(LACons p0 r0) -> p0 : unpackRest r0)

--------------------------------------------------------------------------------
-- 3. Shared Polymorphic Expression & Index Evaluators (CPU & CUDA!)
--------------------------------------------------------------------------------

-- | Evaluate a structured 'IxExpr' into its flat row-major @e Int64@ ordinal.
evalIxE ::
  forall m e i.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double) =>
  LowerEnv m e ->
  IxExpr i ->
  m (e Int64)
evalIxE env = go
  where
    dEnv = leDimEnv env
    go :: forall k. IxExpr k -> m (e Int64)
    go = \case
      IxVar v _ ->
        case IM.lookup v (leIxVars env) of
          Just x -> pure x
          Nothing -> error ("evalIxE: unbound index variable " <> show v)
      IxConst c _ -> pure (fromIntegral c)
      IxPair a b -> do
        oa <- go a
        ob <- go b
        let sb = fromIntegral (sixEvalSize dEnv (ixExprShape b)) :: e Int64
        pure (oa * sb + ob)
      IxFst p -> do
        op <- go p
        case ixExprShape p of
          SIxProd _ sb ->
            let nb = fromIntegral (sixEvalSize dEnv sb) :: e Int64
             in pure (op `quotient` nb)
      IxSnd p -> do
        op <- go p
        case ixExprShape p of
          SIxProd _ sb ->
            let nb = fromIntegral (sixEvalSize dEnv sb) :: e Int64
             in pure (op `remainder` nb)
      IxInL _ a -> go a
      IxInR sa b -> do
        ob <- go b
        pure (fromIntegral (sixEvalSize dEnv sa) + ob)
      IxTagE _ a -> go a
      IxUntagE a -> go a
      IxApplyIso iso a -> do
        oa <- go a
        pure (evalIsoOrdE dEnv iso oa)
      IxFromOrd _ expr -> evalExprE env expr

-- | Evaluate a 'RawExpr a' into a Janus expression @e a@ inside any 'CmdJanus'
-- backend (CPU C, IO, ParallelDejaFu, CUDA C++, CUDAIO, CUDADejaFu!).
evalExprE ::
  forall m e a.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double) =>
  LowerEnv m e ->
  RawExpr a ->
  m (e a)
evalExprE env = \case
  ELit _ v -> pure (evalLitE v)
  EScalarVar _ vid _ ->
    case IM.lookup vid (leScalarVars env) of
      Just (SomeExp (v :: e b)) ->
        case eqT @a @b of
          Just Refl -> pure v
          Nothing -> error "evalExprE: scalar variable type mismatch"
      Nothing -> error ("evalExprE: unbound scalar variable " <> show vid)
  EOrd _ ix -> evalIxE env ix
  EShapeSize _ sh -> pure (fromIntegral (sixEvalSize (leDimEnv env) sh))
  EUnOp _ op x -> do
    vx <- evalExprE env x
    pure (evalUnOpE op vx)
  EBinOp _ op x y -> do
    vx <- evalExprE env x
    vy <- evalExprE env y
    pure (evalBinOpE op vx vy)
  ECmp _ op x y -> do
    vx <- evalExprE env x
    vy <- evalExprE env y
    pure (evalCmpOpE op vx vy)
  ESelect _ c t f ->
    withJanusType @a @e $
      withJanusType @Bool @e $ do
        vc <- evalExprE env c
        vt <- evalExprE env t
        vf <- evalExprE env f
        pure (ifThenElse vc vt vf)
  ECast _ x -> do
    vx <- evalExprE env x
    pure (evalCastE vx)
  EIndex _ node ix ->
    withJanusType @a @e $ do
      ord <- evalIxE env ix
      let ptr = lookupNodePtr node env
      peekElemOff ptr ord
  EReduce _ (SomeMonoid (pOp :: Proxy op)) s v body ->
    withJanusType @(MonoidAcc op a) @e $ do
      let n = fromIntegral (sixEvalSize (leDimEnv env) s) :: e Int64
      accRef <- newRef (monoidEmpty pOp (Proxy @a))
      rangeM 0 n $ \i -> do
        let env' = env {leIxVars = IM.insert v i (leIxVars env)}
        val <- evalExprE env' body
        cur <- readRef accRef
        writeRef accRef (monoidCombine @op @a pOp cur (monoidInject @op @a pOp val))
      fin <- readRef accRef
      pure (monoidFinalize @op @a pOp fin)
  EScalarPrim _ spec args -> do
    hArgs <- evalHArgs args
    pure (spLower spec hArgs)
  EAnnotate _ _ x -> evalExprE env x
  EReadSTRef _ rid ->
    withJanusType @a @e $
      case IM.lookup rid (leSTRefs env) of
        Just (SomeRef (r :: Ref m e b)) ->
          case eqT @a @b of
            Just Refl -> readRef r
            Nothing -> error "evalExprE: STRef type mismatch"
        Nothing -> error ("evalExprE: unbound STRef " <> show rid)
  EReadMTab _ tid _ ix ->
    withJanusType @a @e $
      case IM.lookup tid (leSTMTabs env) of
        Just (SomeMTabPtr _ (p :: e (Ptr b))) ->
          case eqT @a @b of
            Just Refl -> do
              ord <- evalIxE env ix
              peekElemOff p ord
            Nothing -> error "evalExprE: MTab type mismatch"
        Nothing -> error ("evalExprE: unbound MTab " <> show tid)
  ERunSTScalar _ prog -> evalSTProgScalarE env prog
  where
    evalHArgs :: HExprArgs ts -> m (HExprs e ts)
    evalHArgs HANil = pure HENil
    evalHArgs (HACons e rest) = HECons <$> evalExprE env e <*> evalHArgs rest

evalLitE :: forall e a. (ExpJanus e, TensorScalar a) => a -> e a
evalLitE v
  | Just Refl <- eqT @a @Float = realToFrac v
  | Just Refl <- eqT @a @Double = realToFrac v
  | Just Refl <- eqT @a @Int32 = fromIntegral v
  | Just Refl <- eqT @a @Int64 = fromIntegral v
  | Just Refl <- eqT @a @Word32 = fromIntegral v
  | Just Refl <- eqT @a @Word64 = fromIntegral v
  | Just Refl <- eqT @a @Bool = if v then true else false
  | otherwise = error "evalLitE: unsupported TensorScalar"

evalUnOpE ::
  forall e a.
  (ExpJanus e, ExpSpecialMath e Float, ExpSpecialMath e Double, TensorScalar a) =>
  ScalarUnOp a ->
  e a ->
  e a
evalUnOpE op x = case op of
  UNeg -> withNum negate
  UAbs -> withNum abs
  USignum -> withNum signum
  URecip -> withFrac recip
  UExp -> withFloat exp
  ULog -> withFloat log
  USqrt -> withFloat sqrt
  USin -> withFloat sin
  UCos -> withFloat cos
  UTan -> withFloat tan
  UAsin -> withFloat asin
  UAcos -> withFloat acos
  UAtan -> withFloat atan
  USinh -> withFloat sinh
  UCosh -> withFloat cosh
  UTanh -> withFloat tanh
  UAsinh -> withFloat asinh
  UAcosh -> withFloat acosh
  UAtanh -> withFloat atanh
  ULGamma -> withSpec lgammaE
  UDigamma -> withSpec digammaE
  UTrigamma -> withSpec trigammaE
  UErf -> withSpec erfE
  UErfc -> withSpec erfcE
  UErfInv -> withSpec erfinvE
  ULog1p -> withSpec log1pE
  UExpm1 -> withSpec expm1E
  URsqrt -> withSpec rsqrtE
  UNot -> lnot x
  where
    withNum :: (forall b. (Num (e b)) => e b -> e b) -> e a
    withNum f
      | Just Refl <- eqT @a @Float = f x
      | Just Refl <- eqT @a @Double = f x
      | Just Refl <- eqT @a @Int32 = f x
      | Just Refl <- eqT @a @Int64 = f x
      | Just Refl <- eqT @a @Word32 = f x
      | Just Refl <- eqT @a @Word64 = f x
      | otherwise = error "evalUnOpE: non-Num scalar"
    withFrac :: (forall b. (Fractional (e b)) => e b -> e b) -> e a
    withFrac f
      | Just Refl <- eqT @a @Float = f x
      | Just Refl <- eqT @a @Double = f x
      | otherwise = error "evalUnOpE: non-Fractional scalar"
    withFloat :: (forall b. (Floating (e b)) => e b -> e b) -> e a
    withFloat f
      | Just Refl <- eqT @a @Float = f x
      | Just Refl <- eqT @a @Double = f x
      | otherwise = error "evalUnOpE: non-Floating scalar"
    withSpec :: (forall b. (ExpSpecialMath e b) => e b -> e b) -> e a
    withSpec f
      | Just Refl <- eqT @a @Float = f x
      | Just Refl <- eqT @a @Double = f x
      | otherwise = error "evalUnOpE: non-SpecialMath scalar"

evalBinOpE :: forall e a. (ExpJanus e, TensorScalar a) => ScalarBinOp a -> e a -> e a -> e a
evalBinOpE op x y = case op of
  BAdd -> withNum (+)
  BSub -> withNum (-)
  BMul -> withNum (*)
  BDiv -> withFrac (/)
  BPow -> withFloat (**)
  BQuot -> withInt quotient
  BRem -> withInt remainder
  BAnd -> x `land` y
  BOr -> x `lor` y
  where
    withNum :: (forall b. (Num (e b)) => e b -> e b -> e b) -> e a
    withNum f
      | Just Refl <- eqT @a @Float = f x y
      | Just Refl <- eqT @a @Double = f x y
      | Just Refl <- eqT @a @Int32 = f x y
      | Just Refl <- eqT @a @Int64 = f x y
      | Just Refl <- eqT @a @Word32 = f x y
      | Just Refl <- eqT @a @Word64 = f x y
      | otherwise = error "evalBinOpE: non-Num scalar"
    withFrac :: (forall b. (Fractional (e b)) => e b -> e b -> e b) -> e a
    withFrac f
      | Just Refl <- eqT @a @Float = f x y
      | Just Refl <- eqT @a @Double = f x y
      | otherwise = error "evalBinOpE: non-Fractional scalar"
    withFloat :: (forall b. (Floating (e b)) => e b -> e b -> e b) -> e a
    withFloat f
      | Just Refl <- eqT @a @Float = f x y
      | Just Refl <- eqT @a @Double = f x y
      | otherwise = error "evalBinOpE: non-Floating scalar"
    withInt :: (forall b. (ExpIntegral e b) => e b -> e b -> e b) -> e a
    withInt f
      | Just Refl <- eqT @a @Int32 = f x y
      | Just Refl <- eqT @a @Int64 = f x y
      | Just Refl <- eqT @a @Word32 = f x y
      | Just Refl <- eqT @a @Word64 = f x y
      | otherwise = error "evalBinOpE: non-Integral scalar"

evalCmpOpE :: forall e a. (ExpJanus e, TensorScalar a) => ScalarCmpOp -> e a -> e a -> e Bool
evalCmpOpE op x y = withCmp go
  where
    go :: forall b. (ExpEq e b, ExpOrd e b) => e b -> e b -> e Bool
    go a b = case op of
      CEq -> a `eq` b
      CNe -> a `ne` b
      CLt -> a `lt` b
      CLe -> a `le` b
      CGt -> a `gt` b
      CGe -> a `ge` b
    withCmp :: (forall b. (ExpEq e b, ExpOrd e b) => e b -> e b -> e Bool) -> e Bool
    withCmp f
      | Just Refl <- eqT @a @Float = f x y
      | Just Refl <- eqT @a @Double = f x y
      | Just Refl <- eqT @a @Int32 = f x y
      | Just Refl <- eqT @a @Int64 = f x y
      | Just Refl <- eqT @a @Word32 = f x y
      | Just Refl <- eqT @a @Word64 = f x y
      | Just Refl <- eqT @a @Bool = case op of
          CEq -> x `eq` y
          CNe -> x `ne` y
          CLt -> lnot x `land` y
          CLe -> lnot x `lor` y
          CGt -> x `land` lnot y
          CGe -> x `lor` lnot y
      | otherwise = error "evalCmpOpE: unsupported scalar"

evalCastE :: forall e a b. (ExpJanus e, TensorScalar a, TensorScalar b) => e a -> e b
evalCastE x
  | Just Refl <- eqT @a @b = x
  -- Bool -> numeric
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Float = fromBool x
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Double = fromBool x
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Int32 = fromBool x
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Int64 = fromBool x
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Word32 = fromBool x
  | Just Refl <- eqT @a @Bool, Just Refl <- eqT @b @Word64 = fromBool x
  -- numeric -> Bool
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Float = toBool x
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Double = toBool x
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Int32 = toBool x
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Int64 = toBool x
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Word32 = toBool x
  | Just Refl <- eqT @b @Bool, Just Refl <- eqT @a @Word64 = toBool x
  -- Float <-> Double
  | Just Refl <- eqT @a @Float, Just Refl <- eqT @b @Double = toFloating x
  | Just Refl <- eqT @a @Double, Just Refl <- eqT @b @Float = toFloating x
  -- Int/Word -> Float/Double
  | Just Refl <- eqT @a @Int32, Just Refl <- eqT @b @Float = toFloating x
  | Just Refl <- eqT @a @Int32, Just Refl <- eqT @b @Double = toFloating x
  | Just Refl <- eqT @a @Int64, Just Refl <- eqT @b @Float = toFloating x
  | Just Refl <- eqT @a @Int64, Just Refl <- eqT @b @Double = toFloating x
  | Just Refl <- eqT @a @Word32, Just Refl <- eqT @b @Float = toFloating x
  | Just Refl <- eqT @a @Word32, Just Refl <- eqT @b @Double = toFloating x
  | Just Refl <- eqT @a @Word64, Just Refl <- eqT @b @Float = toFloating x
  | Just Refl <- eqT @a @Word64, Just Refl <- eqT @b @Double = toFloating x
  -- Int/Word <-> Int/Word
  | Just Refl <- eqT @a @Int32, Just Refl <- eqT @b @Int64 = toIntegral x
  | Just Refl <- eqT @a @Int64, Just Refl <- eqT @b @Int32 = toIntegral x
  | Just Refl <- eqT @a @Int32, Just Refl <- eqT @b @Word32 = toIntegral x
  | Just Refl <- eqT @a @Word32, Just Refl <- eqT @b @Int32 = toIntegral x
  | Just Refl <- eqT @a @Int64, Just Refl <- eqT @b @Word64 = toIntegral x
  | Just Refl <- eqT @a @Word64, Just Refl <- eqT @b @Int64 = toIntegral x
  | Just Refl <- eqT @a @Word32, Just Refl <- eqT @b @Word64 = toIntegral x
  | Just Refl <- eqT @a @Word64, Just Refl <- eqT @b @Word32 = toIntegral x
  | Just Refl <- eqT @a @Int32, Just Refl <- eqT @b @Word64 = toIntegral x
  | Just Refl <- eqT @a @Word64, Just Refl <- eqT @b @Int32 = toIntegral x
  | Just Refl <- eqT @a @Int64, Just Refl <- eqT @b @Word32 = toIntegral x
  | Just Refl <- eqT @a @Word32, Just Refl <- eqT @b @Int64 = toIntegral x
  | otherwise = error "evalCastE: unsupported numeric cast pair"

--------------------------------------------------------------------------------
-- 4. Local-State `TensorST` Lowering (CPU & Single-Thread CUDA)
--------------------------------------------------------------------------------

evalSTStmtE ::
  forall m e.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double) =>
  e (Ptr Word8) ->
  Int ->
  LowerEnv m e ->
  STStmt ->
  m (LowerEnv m e, Int)
evalSTStmtE extraBuf extraOff env = \case
  STNewRef rid (initE :: RawExpr b) ->
    withJanusType @b @e $ do
      v <- evalExprE env initE
      r <- newRef v
      pure (env {leSTRefs = IM.insert rid (SomeRef r) (leSTRefs env)}, extraOff)
  STWriteRef rid (valE :: RawExpr b) ->
    withJanusType @b @e $ do
      v <- evalExprE env valE
      case IM.lookup rid (leSTRefs env) of
        Just (SomeRef (r :: Ref m e c)) ->
          case eqT @b @c of
            Just Refl -> writeRef r v >> pure (env, extraOff)
            Nothing -> error "evalSTStmtE: STWriteRef type mismatch"
        Nothing -> error ("evalSTStmtE: unbound STRef " <> show rid)
  STNewMTab tid sh (initNode :: RawNode j b) ->
    withJanusType @b @e $ do
      let n = sixEvalSize (leDimEnv env) sh
          szAligned = ((n * sizeOf (undefined :: b) + 63) `div` 64) * 64
          srcPtr = lookupNodePtr initNode env
          dstPtr = fromVoidPtr (toVoidPtr (ptrAdd extraBuf (fromIntegral extraOff))) :: e (Ptr b)
      rangeM 0 (fromIntegral n) $ \i -> do
        v <- peekElemOff srcPtr i
        pokeElemOff dstPtr v i
      pure (env {leSTMTabs = IM.insert tid (SomeMTabPtr sh dstPtr) (leSTMTabs env)}, extraOff + szAligned)
  STWriteMTab tid _ ix (valE :: RawExpr b) ->
    withJanusType @b @e $ do
      ord <- evalIxE env ix
      v <- evalExprE env valE
      case IM.lookup tid (leSTMTabs env) of
        Just (SomeMTabPtr _ (p :: e (Ptr c))) ->
          case eqT @b @c of
            Just Refl -> pokeElemOff p v ord >> pure (env, extraOff)
            Nothing -> error "evalSTStmtE: STWriteMTab type mismatch"
        Nothing -> error ("evalSTStmtE: unbound MTab " <> show tid)
  STFor sh v body -> do
    let n = fromIntegral (sixEvalSize (leDimEnv env) sh) :: e Int64
    rangeM 0 n $ \i -> do
      let env' = env {leIxVars = IM.insert v i (leIxVars env)}
      _ <- evalSTStmtsE extraBuf extraOff env' body
      pure ()
    pure (env, extraOff)
  STWhile cond body -> do
    whileM true $ \cont -> do
      c <- evalExprE env cond
      ifThenElseM_
        c
        (evalSTStmtsE extraBuf extraOff env body >> pure ())
        (writeRef cont false)
    pure (env, extraOff)
  STWhen cond body -> do
    c <- evalExprE env cond
    whenM_ c (evalSTStmtsE extraBuf extraOff env body >> pure ())
    pure (env, extraOff)

evalSTStmtsE ::
  forall m e.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double) =>
  e (Ptr Word8) ->
  Int ->
  LowerEnv m e ->
  [STStmt] ->
  m (LowerEnv m e, Int)
evalSTStmtsE _ off env [] = pure (env, off)
evalSTStmtsE extraBuf off env (s : ss) = do
  (env', off') <- evalSTStmtE extraBuf off env s
  evalSTStmtsE extraBuf off' env' ss

evalSTProgScalarE ::
  forall m e a.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double) =>
  LowerEnv m e ->
  STProgram (RawExpr a) ->
  m (e a)
evalSTProgScalarE env (STProgram stms resE) = do
  (env', _) <- evalSTStmtsE (leScratch env) 0 env stms
  evalExprE env' resE

--------------------------------------------------------------------------------
-- 5. General Binary Semiring Contraction Kernel (Arbitrary Rank & Semiring)
--------------------------------------------------------------------------------

-- | Compute row-major strides for an ordered list of axis labels.
axisStrides :: IM.IntMap Int -> [IndexId] -> IM.IntMap Int
axisStrides verts labels =
  let revDims = reverse [(v, IM.findWithDefault 1 v verts) | v <- labels]
      go _ [] = []
      go stride ((v, d) : rest) = (v, stride) : go (stride * d) rest
   in IM.fromListWith (+) (go 1 revDims)

-- | Lower a single binary contraction step @C = A ⋆_sr B@ for an arbitrary
-- range @[loOut, hiOut)@ of flat output elements (used by BOTH serial and
-- Argobots parallel CPU loops AND CUDA grid-stride kernels!).
lowerBinaryContractLoops ::
  forall sr a m e.
  (CmdJanus m e, ExpPair e, TensorSemiring sr a) =>
  Proxy sr ->
  IM.IntMap Int ->
  BinaryStepMeta ->
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  e Int64 ->
  e Int64 ->
  m ()
lowerBinaryContractLoops pSR verts meta lhsPtr rhsPtr outPtr loOut hiOut =
  withJanusType @a @e $ do
    let outLabels = bsOutLabels meta
        kLabels = bsK meta
        lhsStrideMap = axisStrides verts (bsLhsLabels meta)
        rhsStrideMap = axisStrides verts (bsRhsLabels meta)
        outDimList = [(v, IM.findWithDefault 1 v verts) | v <- outLabels]
        kDimList = [(v, IM.findWithDefault 1 v verts) | v <- kLabels]
        outStrides = axisStrides verts outLabels
        kStrides = axisStrides verts kLabels
        totalK = foldr (\(_, d) acc -> acc * d) 1 kDimList
        -- Decompose flat output ordinal -> (lhsBaseOff, rhsBaseOff)
        decomposeOut :: e Int64 -> (e Int64, e Int64)
        decomposeOut flatIdx =
          foldr
            ( \(v, d) (lAcc, rAcc) ->
                let oS = fromIntegral (IM.findWithDefault 1 v outStrides) :: e Int64
                    coord = (flatIdx `quotient` oS) `remainder` fromIntegral d
                    lS = fromIntegral (IM.findWithDefault 0 v lhsStrideMap) :: e Int64
                    rS = fromIntegral (IM.findWithDefault 0 v rhsStrideMap) :: e Int64
                 in (lAcc + coord * lS, rAcc + coord * rS)
            )
            (0, 0)
            outDimList
        decomposeK :: e Int64 -> (e Int64, e Int64)
        decomposeK flatK =
          foldr
            ( \(v, d) (lAcc, rAcc) ->
                let kS = fromIntegral (IM.findWithDefault 1 v kStrides) :: e Int64
                    coord = (flatK `quotient` kS) `remainder` fromIntegral d
                    lS = fromIntegral (IM.findWithDefault 0 v lhsStrideMap) :: e Int64
                    rS = fromIntegral (IM.findWithDefault 0 v rhsStrideMap) :: e Int64
                 in (lAcc + coord * lS, rAcc + coord * rS)
            )
            (0, 0)
            kDimList
    rangeM loOut hiOut $ \outIdx -> do
      let (lBase, rBase) = decomposeOut outIdx
      if null kLabels
        then do
          va <- peekElemOff lhsPtr lBase
          vb <- peekElemOff rhsPtr rBase
          pokeElemOff outPtr (semiringMul pSR va vb) outIdx
        else do
          accRef <- newRef (semiringZero pSR (Proxy @a))
          rangeM 0 (fromIntegral totalK) $ \kIdx -> do
            let (lK, rK) = decomposeK kIdx
            va <- peekElemOff lhsPtr (lBase + lK)
            vb <- peekElemOff rhsPtr (rBase + rK)
            cur <- readRef accRef
            writeRef accRef (semiringFMA pSR cur va vb)
          res <- readRef accRef
          pokeElemOff outPtr res outIdx

--------------------------------------------------------------------------------
-- 6. Stage-by-Stage CPU Lowering (Argobots Parallel + Zero-Alloc Stack Scratch)
--------------------------------------------------------------------------------

-- | Allocate/acquire the scratchpad buffer according to 'ScratchPolicy' and
-- invoke the continuation. Produces ZERO heap allocations when
-- @spScratchBytes == 0@, under 'ScratchCallerBuf', or under 'ScratchAuto' when
-- static scratch fits within budget!
withScratchPlan ::
  (CmdParWorker m e) =>
  ScratchPolicy ->
  ScheduledPlan ->
  Maybe (e (Ptr Word8)) ->
  (e (Ptr Word8) -> m r) ->
  m r
withScratchPlan policy plan mCallerBuf k
  | spScratchBytes plan <= 0 = k nullPtr
  | otherwise = case policy of
      ScratchCallerBuf ->
        case mCallerBuf of
          Just buf -> k buf
          Nothing -> error "withScratchPlan: ScratchCallerBuf requires caller scratch buffer"
      ScratchAuto budget
        | spIsStatic plan && spScratchBytes plan <= budget ->
            withStackBytes (spScratchBytes plan) k
        | otherwise -> allocArena
      ScratchAlloc -> allocArena
  where
    allocArena = do
      raw <- alignedMalloc (fromIntegral (spScratchBytes plan)) 64
      res <- k (fromVoidPtr raw)
      free raw
      pure res

-- | Helper dispatching either Argobots 'parallelForRangeM' or serial 'rangeM'
-- depending on 'CompileConfig' and problem size.
dispatchRangeCPU ::
  forall m e.
  (CmdParWorker m e) =>
  CompileConfig ->
  String ->
  Int ->
  LowerEnv m e ->
  (forall m0 e0. (CmdParWorker m0 e0) => LowerEnv m0 e0 -> e0 Int64 -> e0 Int64 -> m0 ()) ->
  m ()
dispatchRangeCPU cfg label totalElems env chunkBody =
  if ccUseParallel cfg && totalElems >= ccParallelThreshold cfg
    then do
      let inputPairs = Map.toAscList (leInputs env)
          inputNames = map fst inputPairs
          allPtrs = leScratch env : leRootOut env : map snd inputPairs
          grain = max 1 (totalElems `div` 16)
      withPackedPtrs allPtrs $ \packedArgs unpackArgs ->
        parallelForRangeM label 0 (fromIntegral totalElems) (fromIntegral grain) packedArgs $
          \_ lo hi wArgs ->
            case unpackArgs wArgs of
              (wScratch : wRootOut : wInPtrs) ->
                let wEnv =
                      LowerEnv
                        { leDimEnv = leDimEnv env,
                          leInputs = Map.fromList (zip inputNames wInPtrs),
                          leScratch = wScratch,
                          leSlots = leSlots env,
                          leRootDigest = leRootDigest env,
                          leRootOut = wRootOut,
                          leIxVars = IM.empty,
                          leScalarVars = IM.empty,
                          leSTRefs = IM.empty,
                          leSTMTabs = IM.empty
                        }
                 in chunkBody wEnv lo hi
              _ -> error "dispatchRangeCPU: packed pointer unpack mismatch"
    else chunkBody env 0 (fromIntegral totalElems)

-- | Lower a single scheduled stage onto CPU (serial or Argobots parallel).
lowerStageCPU ::
  forall m e.
  (CmdParWorker m e) =>
  CompileConfig ->
  Maybe ContractionTree ->
  Int ->
  LowerEnv m e ->
  SomeStage ->
  m ()
lowerStageCPU cfg mTreeOverride extraBaseOff env (SomeStage dig node nElems _ mPlannedTree _) =
  case node of
    NInput {} -> pure ()
    NAnnotate _ _ inner -> do
      let srcPtr = lookupNodePtr inner env
          dstPtr = lookupNodePtr node env
      dispatchRangeCPU cfg "tensor_annot" nElems env $ \wEnv lo hi ->
        let sP = lookupNodePtr inner wEnv
            dP = lookupNodePtr node wEnv
         in copySlice sP dP lo hi
      let _ = (srcPtr, dstPtr, dig)
      pure ()
    NFor _ _ v body ->
      dispatchRangeCPU cfg "tensor_for" nElems env $ \wEnv lo hi ->
        let outPtr = lookupNodePtr node wEnv
         in lowerForSlice outPtr v body wEnv lo hi
    NReindex _ morph inner ->
      dispatchRangeCPU cfg "tensor_reindex" nElems env $ \wEnv lo hi ->
        let srcPtr = lookupNodePtr inner wEnv
            dstPtr = lookupNodePtr node wEnv
         in lowerReindexSlice (leDimEnv wEnv) morph srcPtr dstPtr lo hi
    NScatter _ (SomeCommMonoid (pOp :: Proxy op)) _ sj v dstIx valE -> do
      -- Zero-initialize output with commMonoidZeroVal, then parallel atomic scatter!
      let initOut :: forall m0 e0. (CmdParWorker m0 e0) => LowerEnv m0 e0 -> e0 Int64 -> e0 Int64 -> m0 ()
          initOut wEnv lo hi = lowerFillZero pOp (lookupNodePtr node wEnv) lo hi
      dispatchRangeCPU cfg "tensor_scat_init" nElems env initOut
      let nSrc = sixEvalSize (leDimEnv env) sj
      dispatchRangeCPU cfg "tensor_scat" nSrc env $ \wEnv lo hi ->
        let outPtr = lookupNodePtr node wEnv
         in rangeM lo hi $ \j -> do
              let wEnv' = wEnv {leIxVars = IM.insert v j (leIxVars wEnv)}
              dstOrd <- evalIxE wEnv' dstIx
              val <- evalExprE wEnv' valE
              monoidAtomicUpdate pOp (ptrIndex outPtr dstOrd) val
    NRunAccum _ (SomeCommMonoid (pOp :: Proxy op)) _ (AccumProgram stms) -> do
      dispatchRangeCPU cfg "tensor_acc_init" nElems env $ \wEnv lo hi ->
        lowerFillZero pOp (lookupNodePtr node wEnv) lo hi
      lowerAccumStmtsCPU cfg pOp (lookupNodePtr node env) env stms
    NRunST _ _ (STProgram stms resId) -> do
      let extraBuf = ptrAdd (leScratch env) (fromIntegral extraBaseOff)
      (env', _) <- evalSTStmtsE extraBuf 0 env stms
      copyFrozenMTab resId (lookupNodePtr node env) env' nElems
    NScan _ _ initE xs cVar xVar stepE -> do
      let srcPtr = lookupNodePtr xs env
          dstPtr = lookupNodePtr node env
      lowerScanSerial initE srcPtr dstPtr cVar xVar stepE env nElems
    NAssocScan _ (SomeMonoid (pOp :: Proxy op)) _ xs -> do
      let srcPtr = lookupNodePtr xs env
          dstPtr = lookupNodePtr node env
      lowerAssocScanCPU pOp srcPtr dstPtr nElems
    NContract _ (SomeSemiring (pSR :: Proxy sr)) spec -> do
      let hg = fromContractSpec (leDimEnv env) spec
          tree = case mTreeOverride of
            Just t -> t
            Nothing -> case mPlannedTree of
              Just t -> t
              Nothing -> error "lowerStageCPU: missing contraction tree"
          extraBuf = ptrAdd (leScratch env) (fromIntegral extraBaseOff)
      lowerContractTreeCPU cfg pSR hg spec tree extraBuf (lookupNodePtr node env) env
    NKernelPrim _ spec args ->
      let hViews = gatherHViews env args
          outView = mkTensorView (kpOutShape spec) (lookupNodePtr node env)
       in kpLowerCPU spec hViews outView

copySlice :: forall a m e. (CmdJanus m e, TensorScalar a) => e (Ptr a) -> e (Ptr a) -> e Int64 -> e Int64 -> m ()
copySlice src dst lo hi =
  withJanusType @a @e $
    rangeM lo hi $ \i -> do
      v <- peekElemOff src i
      pokeElemOff dst v i

lowerForSlice ::
  forall a m e.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double, TensorScalar a) =>
  e (Ptr a) ->
  Int ->
  RawExpr a ->
  LowerEnv m e ->
  e Int64 ->
  e Int64 ->
  m ()
lowerForSlice outPtr v body env lo hi =
  withJanusType @a @e $
    rangeM lo hi $ \i -> do
      let env' = env {leIxVars = IM.insert v i (leIxVars env)}
      val <- evalExprE env' body
      pokeElemOff outPtr val i

lowerReindexSlice ::
  forall i j a m e.
  (CmdJanus m e, TensorScalar a) =>
  DimEnv ->
  IxMorph i j ->
  e (Ptr a) ->
  e (Ptr a) ->
  e Int64 ->
  e Int64 ->
  m ()
lowerReindexSlice dEnv morph srcPtr dstPtr lo hi =
  withJanusType @a @e $
    withJanusType @Bool @e $
      rangeM lo hi $ \i ->
        case morph of
          MorphIso iso -> do
            let j = evalIsoOrdE dEnv iso i
            v <- peekElemOff srcPtr j
            pokeElemOff dstPtr v i
          MorphPrism p -> do
            let j = evalPrismForwardOrdE dEnv p i
            v <- peekElemOff srcPtr j
            pokeElemOff dstPtr v i
          MorphAffine aff -> do
            let (inB, j) = evalAffineForwardOrdE dEnv aff i
            ifThenElseM_
              inB
              (peekElemOff srcPtr j >>= \v -> pokeElemOff dstPtr v i)
              (pokeElemOff dstPtr (scalarZeroE (Proxy @a)) i)

lowerFillZero ::
  forall op a m e.
  (CmdJanus m e, ExpPair e, TensorCommMonoid op a) =>
  Proxy op ->
  e (Ptr a) ->
  e Int64 ->
  e Int64 ->
  m ()
lowerFillZero pOp outPtr lo hi =
  withJanusType @a @e $
    let z = commMonoidZeroVal pOp (Proxy @a)
     in rangeM lo hi $ \i -> pokeElemOff outPtr z i

lowerAccumStmtsCPU ::
  forall op i a m e.
  (CmdParWorker m e, TensorCommMonoid op a) =>
  CompileConfig ->
  Proxy op ->
  e (Ptr a) ->
  LowerEnv m e ->
  [AccumStmt i a] ->
  m ()
lowerAccumStmtsCPU cfg pOp outPtr env = mapM_ go
  where
    go :: AccumStmt i a -> m ()
    go = \case
      AccumWrite ix valE ->
        withJanusType @a @e $ do
          ord <- evalIxE env ix
          val <- evalExprE env valE
          monoidAtomicUpdate pOp (ptrIndex outPtr ord) val
      AccumParFor sh v body -> do
        let n = sixEvalSize (leDimEnv env) sh
        if IM.null (leIxVars env) && ccUseParallel cfg && n >= ccParallelThreshold cfg
          then dispatchRangeCPU cfg "tensor_accum_par" n env $ \(wEnv :: LowerEnv m0 e0) lo hi ->
            withJanusType @a @e0 $
              let wOut = fromVoidPtr (toVoidPtr (leRootOut wEnv))
               in rangeM lo hi $ \j ->
                    let wEnv' = wEnv {leIxVars = IM.insert v j (leIxVars wEnv)}
                     in lowerAccumSerial pOp wOut wEnv' body
          else rangeM 0 (fromIntegral n) $ \j ->
            let env' = env {leIxVars = IM.insert v j (leIxVars env)}
             in lowerAccumSerial pOp outPtr env' body
      AccumWhen c body -> do
        cond <- evalExprE env c
        whenM_ cond (mapM_ go body)

lowerAccumSerial ::
  forall op i a m e.
  (CmdParWorker m e, TensorCommMonoid op a) =>
  Proxy op ->
  e (Ptr a) ->
  LowerEnv m e ->
  [AccumStmt i a] ->
  m ()
lowerAccumSerial pOp outPtr env = mapM_ go
  where
    go = \case
      AccumWrite ix valE ->
        withJanusType @a @e $ do
          ord <- evalIxE env ix
          val <- evalExprE env valE
          monoidAtomicUpdate pOp (ptrIndex outPtr ord) val
      AccumParFor sh v body -> do
        let n = fromIntegral (sixEvalSize (leDimEnv env) sh) :: e Int64
        rangeM 0 n $ \j ->
          lowerAccumSerial pOp outPtr (env {leIxVars = IM.insert v j (leIxVars env)}) body
      AccumWhen c body -> do
        cond <- evalExprE env c
        whenM_ cond (mapM_ go body)

copyFrozenMTab ::
  forall a m e.
  (CmdJanus m e, TensorScalar a) =>
  Int ->
  e (Ptr a) ->
  LowerEnv m e ->
  Int ->
  m ()
copyFrozenMTab resId dstPtr env nElems =
  withJanusType @a @e $
    case IM.lookup resId (leSTMTabs env) of
      Just (SomeMTabPtr _ (srcPtr :: e (Ptr b))) ->
        case eqT @a @b of
          Just Refl ->
            rangeM 0 (fromIntegral nElems) $ \i -> do
              v <- peekElemOff srcPtr i
              pokeElemOff dstPtr v i
          Nothing -> error "copyFrozenMTab: result MTab element type mismatch"
      Nothing -> error ("copyFrozenMTab: unknown frozen MTab id " <> show resId)

lowerScanSerial ::
  forall a m e.
  (CmdJanus m e, ExpPair e, ExpSpecialMath e Float, ExpSpecialMath e Double, TensorScalar a) =>
  RawExpr a ->
  e (Ptr a) ->
  e (Ptr a) ->
  Int ->
  Int ->
  RawExpr a ->
  LowerEnv m e ->
  Int ->
  m ()
lowerScanSerial initE srcPtr dstPtr cVar xVar stepE env nElems =
  withJanusType @a @e $ do
    v0 <- evalExprE env initE
    carryRef <- newRef v0
    rangeM 0 (fromIntegral nElems) $ \i -> do
      c <- readRef carryRef
      x <- peekElemOff srcPtr i
      let env' =
            env
              { leScalarVars =
                  IM.insert cVar (SomeExp c) (IM.insert xVar (SomeExp x) (leScalarVars env))
              }
      nxt <- evalExprE env' stepE
      writeRef carryRef nxt
      pokeElemOff dstPtr nxt i

lowerAssocScanCPU ::
  forall op a m e.
  (CmdJanus m e, ExpPair e, TensorMonoid op a) =>
  Proxy op ->
  e (Ptr a) ->
  e (Ptr a) ->
  Int ->
  m ()
lowerAssocScanCPU pOp srcPtr dstPtr nElems =
  withJanusType @a @e $
    withJanusType @(MonoidAcc op a) @e $ do
      accRef <- newRef (monoidEmpty pOp (Proxy @a))
      rangeM 0 (fromIntegral nElems) $ \i -> do
        x <- peekElemOff srcPtr i
        cur <- readRef accRef
        let nxt = monoidCombine @op @a pOp cur (monoidInject @op @a pOp x)
        writeRef accRef nxt
        pokeElemOff dstPtr (monoidFinalize @op @a pOp nxt) i

gatherHViews ::
  forall shs a m e.
  (ExpJanus e, TensorScalar a) =>
  LowerEnv m e ->
  HNodeArgs shs a ->
  HViews e shs a
gatherHViews _ HNNil = HVNil
gatherHViews env (HNCons n rest) =
  HVCons (mkTensorView (rawNodeShape n) (lookupNodePtr n env)) (gatherHViews env rest)

-- | Execute a planned binary 'ContractionTree' using scratchpad buffers for
-- internal nodes and writing the final result directly to @finalOutPtr@.
lowerContractTreeCPU ::
  forall sr i a m e.
  (CmdParWorker m e, TensorSemiring sr a) =>
  CompileConfig ->
  Proxy sr ->
  Hypergraph ->
  ContractSpec i a ->
  ContractionTree ->
  e (Ptr Word8) ->
  e (Ptr a) ->
  LowerEnv m e ->
  m ()
lowerContractTreeCPU cfg pSR hg spec rootTree extraBuf finalOutPtr env = do
  _ <- go True 0 rootTree
  pure ()
  where
    opArr = IM.fromList (zip [0 ..] (csOperands spec))
    verts = hgVerts hg
    align64 n = if n <= 0 then 0 else ((n + 63) `div` 64) * 64

    go :: Bool -> Int -> ContractionTree -> m (e (Ptr a), Int)
    go isRoot off (CLeaf eid survLs) =
      case IM.lookup eid opArr of
        Nothing -> error ("lowerContractTreeCPU: missing operand " <> show eid)
        Just (ContractOperand opNode rawLs) ->
          let srcPtr = lookupNodePtr opNode env
           in if not isRoot && rawLs == survLs
                then pure (srcPtr, off)
                else do
                  let dstPtr = if isRoot then finalOutPtr else fromVoidPtr (toVoidPtr (ptrAdd extraBuf (fromIntegral off)))
                      nOut = foldr (\v acc -> acc * IM.findWithDefault 1 v verts) 1 survLs
                      sz = align64 (nOut * sizeOf (undefined :: a))
                  -- Local trace / transpose / copy when single operand
                  let rawStrides = axisStrides verts rawLs
                      outStrides = axisStrides verts survLs
                      contracted = [v | v <- rawLs, v `notElem` survLs]
                      cStrides = axisStrides verts contracted
                      totalC = foldr (\v acc -> acc * IM.findWithDefault 1 v verts) 1 contracted
                  () <- withJanusType @a @e $
                    rangeM 0 (fromIntegral nOut) $ \outIdx -> do
                        let baseSrc =
                              foldr
                                ( \v acc ->
                                    let d = IM.findWithDefault 1 v verts
                                        oS = fromIntegral (IM.findWithDefault 1 v outStrides) :: e Int64
                                        rS = fromIntegral (IM.findWithDefault 0 v rawStrides) :: e Int64
                                     in acc + ((outIdx `quotient` oS) `remainder` fromIntegral d) * rS
                                )
                                (0 :: e Int64)
                                survLs
                        if null contracted
                          then peekElemOff srcPtr baseSrc >>= \val -> pokeElemOff dstPtr val outIdx
                          else do
                            accRef <- newRef (semiringZero pSR (Proxy @a))
                            rangeM 0 (fromIntegral totalC) $ \cIdx -> do
                              let cOff =
                                    foldr
                                      ( \v acc ->
                                          let d = IM.findWithDefault 1 v verts
                                              cS = fromIntegral (IM.findWithDefault 1 v cStrides) :: e Int64
                                              rS = fromIntegral (IM.findWithDefault 0 v rawStrides) :: e Int64
                                           in acc + ((cIdx `quotient` cS) `remainder` fromIntegral d) * rS
                                      )
                                      (0 :: e Int64)
                                      contracted
                              val <- peekElemOff srcPtr (baseSrc + cOff)
                              cur <- readRef accRef
                              writeRef accRef (semiringAdd pSR cur val)
                            fin <- readRef accRef
                            pokeElemOff dstPtr fin outIdx
                  pure (dstPtr, if isRoot then off else off + sz)
    go isRoot off (CBinary meta lTree rTree) = do
      (lhsPtr, off1) <- go False off lTree
      (rhsPtr, off2) <- go False off1 rTree
      let nOut = foldr (\v acc -> acc * IM.findWithDefault 1 v verts) 1 (bsOutLabels meta)
          sz = align64 (nOut * sizeOf (undefined :: a))
          dstPtr =
            withJanusType @a @e $
              if isRoot then finalOutPtr else fromVoidPtr (toVoidPtr (ptrAdd extraBuf (fromIntegral off2)))
      if ccUseParallel cfg && nOut >= ccParallelThreshold cfg
        then do
          let grain = max 1 (nOut `div` 16)
              ptrs =
                withJanusType @a @e $
                  [ fromVoidPtr (toVoidPtr lhsPtr),
                    fromVoidPtr (toVoidPtr rhsPtr),
                    fromVoidPtr (toVoidPtr dstPtr)
                  ]
          withPackedPtrs ptrs $ \packed unpack ->
            parallelForRangeM "tensor_contract" 0 (fromIntegral nOut) (fromIntegral grain) packed $
              \(_ :: e0 Int32) lo hi wArgs ->
                withJanusType @a @e0 $
                  case unpack wArgs of
                    [wL, wR, wO] ->
                      lowerBinaryContractLoops
                        @sr
                        @a
                        pSR
                        verts
                        meta
                        (fromVoidPtr (toVoidPtr wL))
                        (fromVoidPtr (toVoidPtr wR))
                        (fromVoidPtr (toVoidPtr wO))
                        lo
                        hi
                    _ -> error "lowerContractTreeCPU: unpack mismatch"
        else lowerBinaryContractLoops @sr @a pSR verts meta lhsPtr rhsPtr dstPtr 0 (fromIntegral nOut)
      pure (dstPtr, if isRoot then off else off2 + sz)

--------------------------------------------------------------------------------
-- 7. Top-Level Direct In-Janus Lowering & Convenience Runners
--------------------------------------------------------------------------------

lowerNodeCPUWithOverride ::
  forall i a m e.
  (CmdParWorker m e, TensorScalar a) =>
  CompileConfig ->
  Maybe ContractionTree ->
  DimEnv ->
  InputMap e ->
  Maybe (e (Ptr Word8)) ->
  RawNode i a ->
  TensorView e i a ->
  m ()
lowerNodeCPUWithOverride cfg mTreeOverride dEnv inputs mCallerScratch rawRoot outView = do
  let fused = fuseNode (ccFuseConfig cfg) rawRoot
      plan = scheduleNode (ccPlanConfig cfg) dEnv fused
      interPeak =
        foldl (\acc s -> max acc (bsOffsetBytes s + bsSizeBytes s)) 0 (Map.elems (spSlots plan))
      extraBaseOff = ((interPeak + 63) `div` 64) * 64
      rawInputs =
        Map.map
          (\(SomeTensorView (tv :: TensorView e s b)) -> withJanusType @b @e (fromVoidPtr (toVoidPtr (tvPtr tv))))
          inputs
  withScratchPlan (ccScratchPolicy cfg) plan mCallerScratch $ \scratchPtr -> do
    let env =
          LowerEnv
            { leDimEnv = dEnv,
              leInputs = rawInputs,
              leScratch = scratchPtr,
              leSlots = spSlots plan,
              leRootDigest = spRootDigest plan,
              leRootOut = withJanusType @a @e (fromVoidPtr (toVoidPtr (tvPtr outView))),
              leIxVars = IM.empty,
              leScalarVars = IM.empty,
              leSTRefs = IM.empty,
              leSTMTabs = IM.empty
            }
    forM_ (spStages plan) $ \stg ->
      let ov = if stDigest stg == spRootDigest plan then mTreeOverride else Nothing
       in lowerStageCPU cfg ov extraBaseOff env stg

lowerNodeCPU ::
  forall i a m e.
  (CmdParWorker m e, TensorScalar a) =>
  CompileConfig ->
  DimEnv ->
  InputMap e ->
  RawNode i a ->
  TensorView e i a ->
  m ()
lowerNodeCPU cfg dEnv inputs = lowerNodeCPUWithOverride cfg Nothing dEnv inputs Nothing

-- | Embed and execute any @Tab i (Expr a)@ directly inside a larger Janus
-- program ('IO', 'ParallelDejaFuM', 'JanusCMCSE', 'LinuxX86_64CMCSE').
lowerTensorCPU ::
  forall i a m e.
  (CmdParWorker m e, IxSet i, TensorScalar a) =>
  CompileConfig ->
  DimEnv ->
  InputMap e ->
  Tab i (Expr a) ->
  TensorView e i a ->
  m ()
lowerTensorCPU cfg dEnv inputs tab =
  lowerNodeCPU cfg dEnv inputs (buildNode (materialize tab))

-- | Zero-heap-allocation lowering using a caller-provided scratch buffer of
-- size @planScratchBytes@.
lowerTensorCPUWithScratch ::
  forall i a m e.
  (CmdParWorker m e, IxSet i, TensorScalar a) =>
  CompileConfig ->
  DimEnv ->
  InputMap e ->
  e (Ptr Word8) ->
  Tab i (Expr a) ->
  TensorView e i a ->
  m ()
lowerTensorCPUWithScratch cfg dEnv inputs scratchBuf tab =
  lowerNodeCPUWithOverride
    (cfg {ccScratchPolicy = ScratchCallerBuf})
    Nothing
    dEnv
    inputs
    (Just scratchBuf)
    (buildNode (materialize tab))

-- | Bracket host 'VS.Vector' inputs into pinned C pointers.
withHostInputPtrs :: HostInputMap -> ([(Text.Text, Ptr ())] -> IO r) -> IO r
withHostInputPtrs hm k = go (Map.toAscList hm) []
  where
    go [] acc = k (reverse acc)
    go ((nm, HostInput _ vec) : rest) acc =
      VS.unsafeWith vec $ \p -> go rest ((nm, castPtr p) : acc)

-- | Direct Haskell 'IO' / 'Identity' oracle evaluation returning a 'VS.Vector a'.
evalTensorIO ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CompileConfig ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (VS.Vector a)
evalTensorIO cfg dEnv hostInputs tab = do
  let nOut = sixEvalSize dEnv (six @i)
  withHostInputPtrs hostInputs $ \inPtrs ->
    allocaAligned @a nOut $ \outP -> do
      let inMap =
            Map.fromList
              [ ( nm,
                  case Map.lookup nm hostInputs of
                    Just (HostInput sh (_ :: VS.Vector b)) ->
                      SomeTensorView (mkTensorView sh (Identity (castPtr p :: Ptr b)))
                    Nothing -> error "evalTensorIO: impossible"
                )
                | (nm, p) <- inPtrs
              ]
          outView = mkTensorView (six @i) (Identity outP)
      lowerTensorCPU cfg dEnv inMap tab outView
      VS.generateM nOut (\k -> peekElemOff @IO @Identity (Identity outP) (Identity (fromIntegral k)) >>= \(Identity v) -> pure v)

-- | Execute a @Janus.Tensor@ program under DejaFu ('ParallelDejaFuM') to
-- systematically verify multi-threaded CPU concurrency safety!
evalTensorDejaFu ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CompileConfig ->
  Int ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (Either Condition (VS.Vector a), Trace)
evalTensorDejaFu cfg nWorkers dEnv hostInputs tab = do
  let nOut = sixEvalSize dEnv (six @i)
  withHostInputPtrs hostInputs $ \inPtrs ->
    allocaAligned @a nOut $ \outP -> do
      let inMap =
            Map.fromList
              [ ( nm,
                  case Map.lookup nm hostInputs of
                    Just (HostInput sh (_ :: VS.Vector b)) ->
                      SomeTensorView (mkTensorView sh (ParallelDejaFu (castPtr p :: Ptr b)))
                    Nothing -> error "evalTensorDejaFu: impossible"
                )
                | (nm, p) <- inPtrs
              ]
          outView = mkTensorView (six @i) (ParallelDejaFu outP)
      (res, tr) <- runParallelDejaFuM nWorkers (lowerTensorCPU cfg dEnv inMap tab outView)
      case res of
        Left cond -> pure (Left cond, tr)
        Right () -> do
          vec <- VS.generateM nOut (\k -> peekElemOff @IO @Identity (Identity outP) (Identity (fromIntegral k)) >>= \(Identity v) -> pure v)
          pure (Right vec, tr)

-- | Render the generated C source code for a @Janus.Tensor@ program (going
-- through 'Janus.CSE' + Argobots + zero-alloc stack scratchpad!).
-- Accepts an array of input @void*@ pointers + output @a*@ pointer.
showTensorC ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CompileConfig ->
  DimEnv ->
  [(Text.Text, SomeSIxSpec)] ->
  String ->
  Tab i (Expr a) ->
  IO String
showTensorC cfg dEnv inputSpecs fnName tab =
  showJanusC @PortableC fnName $ \(inPtrArr :: JanusCCSE (Ptr (Ptr Word8))) (outRaw :: JanusCCSE (Ptr Word8)) -> do
    inMap <- buildCInputMap inputSpecs inPtrArr
    let outP = withJanusType @a @JanusCCSE (fromVoidPtr (toVoidPtr outRaw))
    lowerTensorCPU cfg dEnv inMap tab (mkTensorView (six @i) outP)

data SomeSIxSpec where
  SomeSIxSpec :: (TensorScalar a) => !(SIx i) -> Proxy a -> SomeSIxSpec

buildCInputMap ::
  forall m e.
  (CmdJanus m e) =>
  [(Text.Text, SomeSIxSpec)] ->
  e (Ptr (Ptr Word8)) ->
  m (InputMap e)
buildCInputMap specs ptrArr = do
  pairs <-
    mapM
      ( \(idx, (nm, SomeSIxSpec sh (_ :: Proxy b))) ->
          withJanusType @b @e $ do
            rawP <- peekElemOff ptrArr (fromIntegral idx)
            pure (nm, SomeTensorView (mkTensorView sh (fromVoidPtr (toVoidPtr rawP) :: e (Ptr b))))
      )
      (zip [0 :: Int ..] specs)
  pure (Map.fromList pairs)

-- | Compile and execute a @Janus.Tensor@ program via native C ('JanusCMCSE' +
-- Argobots + 'Janus.CSE') with an optional explicit 'ContractionTree' override!
evalTensorCWithTree ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CBuildConfig ->
  CompileConfig ->
  Maybe ContractionTree ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (VS.Vector a)
evalTensorCWithTree buildCfg cfg mTreeOverride dEnv hostInputs tab = do
  let nOut = sixEvalSize dEnv (six @i)
      inputList = Map.toAscList hostInputs
      inputSpecs =
        [ (nm, SomeSIxSpec sh (Proxy @b))
          | (nm, HostInput sh (_ :: VS.Vector b)) <- inputList
        ]
  withHostInputPtrs hostInputs $ \inPtrs ->
    allocaAligned @(Ptr Word8) (max 1 (length inPtrs)) $ \ptrArr -> do
      forM_ (zip [0 :: Int ..] (map snd inPtrs)) $ \(k, p) ->
        pokeElemOff @IO @Identity (Identity ptrArr) (Identity (castPtr p)) (Identity (fromIntegral k))
      allocaAligned @a nOut $ \outP -> do
        let cKernel :: JanusCCSE (Ptr (Ptr Word8)) -> JanusCCSE (Ptr Word8) -> JanusCMCSE ()
            cKernel pArr oRaw = do
              inMap <- buildCInputMap inputSpecs pArr
              let oP = withJanusType @a @JanusCCSE (fromVoidPtr (toVoidPtr oRaw))
              lowerNodeCPUWithOverride cfg mTreeOverride dEnv inMap Nothing (buildNode (materialize tab)) (mkTensorView (six @i) oP)
        withJanusC @PortableC buildCfg cKernel (\fn -> fn ptrArr (castPtr outP))
        VS.generateM nOut (\k -> peekElemOff @IO @Identity (Identity outP) (Identity (fromIntegral k)) >>= \(Identity v) -> pure v)

-- | Compile and execute a @Janus.Tensor@ program via native C ('JanusCMCSE' +
-- Argobots + 'Janus.CSE').
evalTensorC ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  CBuildConfig ->
  CompileConfig ->
  DimEnv ->
  HostInputMap ->
  Tab i (Expr a) ->
  IO (VS.Vector a)
evalTensorC buildCfg cfg = evalTensorCWithTree buildCfg cfg Nothing
