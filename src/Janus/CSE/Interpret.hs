{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Polymorphic interpreter lowering CSE-optimized 'CSEExp' and 'CSEM'
-- programs into any backend @(m, e)@ implementing the Janus typeclasses.
module Janus.CSE.Interpret
  ( -- * Interpreter Environment
    CSEEnv,
    emptyCSEEnv,

    -- * Direct Expression & Monadic Interpreters
    interpretCSEExp,
    interpretCSEExpWithEnv,
    interpretCSEM,
    interpretCSEMWithFeatures,
    interpretCSEMWithEnv,
    interpretCSEMWithEnvAndFeatures,

    -- * Curried Function Interpreter & Combined `withCSE` Helpers
    CSEBodyMonad,
    CSEInterpretFrom (..),
    CSEInterpret (..),
    withCSE,
    withCSEWithFeatures,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Dependent.HashMap as DHM
import Data.Foldable (foldlM, toList)
import Data.Functor.Identity (Identity)
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl), testEquality)
import GHC.TypeNats (natVal)
import Janus.Backend.C.CUDA.Types (CUDAThreadInfo (..))
import Janus.Backend.C.Target (TargetFeatureMap)
import Janus.CSE.Optimize
import Janus.CSE.Types
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Format (CmdString (..))
import Janus.Command.Memory (CmdRefPtr (..))
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile (..))
import Janus.SPMD.Gang (CmdOptimalGang (..), clampGangWidth)
import Janus.SPMD.Typed (SPMDTyped (..))
import Janus.SPMD.Types (SPMD (..), SPMDM, SPMDRep, liftSPMD, runSPMD, runSPMDM)
import qualified Type.Reflection as TR

--------------------------------------------------------------------------------
-- Type-Safe Parameter Signature Witness for Recursive Functions
--------------------------------------------------------------------------------

data HParamTypes (params :: [K.Type]) where
  HPTNil :: HParamTypes '[]
  HPTCons :: !(TR.TypeRep p) -> !(HParamTypes ps) -> HParamTypes (p ': ps)

hargsParamTypes :: HArgs (VarId e) params -> HParamTypes params
hargsParamTypes HNil = HPTNil
hargsParamTypes (HCons (VarId _ tr _) rest) = HPTCons tr (hargsParamTypes rest)

eqHParamTypes :: HParamTypes as -> HParamTypes bs -> Maybe (as :~: bs)
eqHParamTypes HPTNil HPTNil = Just Refl
eqHParamTypes (HPTCons t1 ts1) (HPTCons t2 ts2) = do
  Refl <- testEquality t1 t2
  Refl <- eqHParamTypes ts1 ts2
  pure Refl
eqHParamTypes _ _ = Nothing

data SomePureTargetFun (e :: K.Type -> K.Type) where
  SomePureTargetFun ::
    !(HParamTypes params) ->
    !(TR.TypeRep ret) ->
    !(HArgs e params -> e ret) ->
    SomePureTargetFun e

data SomeMonadicTargetFun (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  SomeMonadicTargetFun ::
    !(HParamTypes params) ->
    !(TR.TypeRep ret) ->
    !(HArgs e params -> m (e ret)) ->
    SomeMonadicTargetFun m e

--------------------------------------------------------------------------------
-- Typed Interpreter Environment
--------------------------------------------------------------------------------

data CSEEnv (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) = CSEEnv
  { envVars :: !(Map.Map VarKey (SomeTargetVal e)),
    envRefs :: !(Map.Map RefKey (SomeTargetRef m e)),
    envPureFuns :: !(Map.Map Digest (SomePureTargetFun e)),
    envMonadicFuns :: !(Map.Map Digest (SomeMonadicTargetFun m e))
  }

emptyCSEEnv :: CSEEnv m e
emptyCSEEnv =
  CSEEnv
    { envVars = Map.empty,
      envRefs = Map.empty,
      envPureFuns = Map.empty,
      envMonadicFuns = Map.empty
    }

insertEnvVar :: VarId e a -> e a -> CSEEnv m e -> CSEEnv m e
insertEnvVar (VarId vk tr _) val env =
  env {envVars = Map.insert vk (SomeTargetVal tr val) (envVars env)}

deleteEnvVar :: VarId e a -> CSEEnv m e -> CSEEnv m e
deleteEnvVar (VarId vk _ _) env =
  env {envVars = Map.delete vk (envVars env)}

lookupEnvVar :: CSEEnv m e -> VarId e a -> e a
lookupEnvVar env (VarId vk tr _) =
  case Map.lookup vk (envVars env) of
    Just (SomeTargetVal trFound val)
      | Just Refl <- testEquality tr trFound -> val
    _ -> error ("Janus.CSE.Interpret: unbound variable " <> show vk)

insertEnvRef :: CSERef m e a -> Ref m e a -> CSEEnv m e -> CSEEnv m e
insertEnvRef (CSERef rk tr _) ref env =
  env {envRefs = Map.insert rk (SomeTargetRef tr ref) (envRefs env)}

lookupEnvRef :: CSEEnv m e -> CSERef m e a -> Ref m e a
lookupEnvRef env (CSERef rk tr _) =
  case Map.lookup rk (envRefs env) of
    Just (SomeTargetRef trFound ref)
      | Just Refl <- testEquality tr trFound -> ref
    _ -> error ("Janus.CSE.Interpret: unbound reference " <> show rk)

extendCSEEnv ::
  [(VarKey, SomeTargetVal e)] ->
  [(RefKey, SomeTargetRef m e)] ->
  CSEEnv m e ->
  CSEEnv m e
extendCSEEnv vs rs env =
  env
    { envVars = foldl (\mp (k, v) -> Map.insert k v mp) (envVars env) vs,
      envRefs = foldl (\mp (k, r) -> Map.insert k r mp) (envRefs env) rs
    }

bindFormalHArgs ::
  HArgs (VarId e) params ->
  HArgs e params ->
  CSEEnv m e ->
  CSEEnv m e
bindFormalHArgs HNil HNil env = env
bindFormalHArgs (HCons vid vids) (HCons val vals) env =
  bindFormalHArgs vids vals (insertEnvVar vid val env)

--------------------------------------------------------------------------------
-- Pure Expression Interpreter (`interpretCSEExp`)
--------------------------------------------------------------------------------

-- | Interpret a closed 'CSEExp' into target expression functor @e@.
interpretCSEExp :: (CSETargetExp e) => CSEExp e a -> e a
interpretCSEExp = interpretCSEExpWithEnv (emptyCSEEnv @Identity)

-- | Interpret a 'CSEExp' under an existing 'CSEEnv'.
interpretCSEExpWithEnv ::
  forall m e a.
  (CSETargetExp e) =>
  CSEEnv m e ->
  CSEExp e a ->
  e a
interpretCSEExpWithEnv env = go
  where
    go :: forall x. CSEExp e x -> e x
    go = \case
      ExpVar _ vid -> lookupEnvVar env vid
      ExpLit _ val -> val
      ExpOp _ _ _ _ _ args eval ->
        eval (evalHArgs args)
      ExpListOp _ _ _ xs eval ->
        eval (map go xs)
      ExpShortCircuit _ _ lhs rhs eval ->
        eval (go lhs) (go rhs)
      ExpCondNode _ c t f eval ->
        eval (go c) (go t) (go f)
      ExpLetNode _ vid capA capB rhs body ->
        let rhsVal = go rhs
         in cseLetWithCap capA capB rhsVal $ \boundVal ->
              interpretCSEExpWithEnv (insertEnvVar vid boundVal env) body
      ExpFixCall _ pfd callArgs ->
        let targetArgs = evalHArgs callArgs
            funKey = pfdKey pfd
            pts = hargsParamTypes (pfdParams pfd)
            retTr = pfdRetType pfd
         in case Map.lookup funKey (envPureFuns env) of
              Just (SomePureTargetFun ptsFound retFound activeSelf)
                | Just Refl <- eqHParamTypes pts ptsFound,
                  Just Refl <- testEquality retTr retFound ->
                    activeSelf targetArgs
              _ ->
                let step selfFn formalVals =
                      let localEnv =
                            bindFormalHArgs
                              (pfdParams pfd)
                              formalVals
                              ( (emptyCSEEnv @Identity @e)
                                  { envPureFuns =
                                      Map.insert
                                        funKey
                                        (SomePureTargetFun pts retTr selfFn)
                                        (envPureFuns env)
                                  }
                              )
                       in interpretCSEExpWithEnv localEnv (pfdBody pfd)
                 in pfdEmitTarget pfd step targetArgs

    evalHArgs :: HArgs (CSEExp e) ts -> HArgs e ts
    evalHArgs HNil = HNil
    evalHArgs (HCons x xs) = HCons (go x) (evalHArgs xs)

--------------------------------------------------------------------------------
-- Monadic Block & Program Interpreter (`interpretCSEM`)
--------------------------------------------------------------------------------

-- | Interpret a closed 'CSEM' computation into target @(m, e)@.
interpretCSEM ::
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  CSEM m e res ->
  m (CSETargetVal e res)
interpretCSEM = interpretCSEMWithFeatures DHM.empty

-- | Interpret a closed 'CSEM' computation under a 'TargetFeatureMap'.
interpretCSEMWithFeatures ::
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  TargetFeatureMap ->
  CSEM m e res ->
  m (CSETargetVal e res)
interpretCSEMWithFeatures tfm = interpretCSEMWithEnvAndFeatures tfm emptyCSEEnv

-- | Interpret a 'CSEM' computation under an existing 'CSEEnv'.
interpretCSEMWithEnv ::
  forall m e res.
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  CSEEnv m e ->
  CSEM m e res ->
  m (CSETargetVal e res)
interpretCSEMWithEnv = interpretCSEMWithEnvAndFeatures DHM.empty

-- | Interpret a 'CSEM' computation under a 'TargetFeatureMap' and 'CSEEnv'.
interpretCSEMWithEnvAndFeatures ::
  forall m e res.
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  TargetFeatureMap ->
  CSEEnv m e ->
  CSEM m e res ->
  m (CSETargetVal e res)
interpretCSEMWithEnvAndFeatures tfm env0 mAct =
  interpCSEMWith
    tfm
    (\envFinal ret -> interpretCSEReturn @e @res (interpretCSEExpWithEnv envFinal) ret)
    env0
    mAct

interpCSEMWith ::
  forall m e res out.
  (Monad m, CmdRef m e, CSETargetExp e) =>
  TargetFeatureMap ->
  (CSEEnv m e -> res -> out) ->
  CSEEnv m e ->
  CSEM m e res ->
  m out
interpCSEMWith tfm finish env0 (CSEM buildFn) =
  let (_, tree) = buildFn tfm 0
   in interpTree env0 tree
  where
    interpTree :: CSEEnv m e -> ProgTree m e res -> m out
    interpTree env = \case
      ProgLinear blk ret -> do
        envFinal <- interpBlock tfm env blk
        pure (finish envFinal ret)
      ProgOptimalGang (_ :: TR.TypeRep elemTy) b2 b4 b8 b16 b32 ->
        withOptimalGang @m @e (Proxy @elemTy) $ \pn ->
          case clampGangWidth (fromIntegral (natVal pn)) of
            2 -> interpTree env b2
            4 -> interpTree env b4
            8 -> interpTree env b8
            16 -> interpTree env b16
            _ -> interpTree env b32

interpBlock ::
  forall m e.
  (Monad m, CmdRef m e, CSETargetExp e) =>
  TargetFeatureMap ->
  CSEEnv m e ->
  Block m e ->
  m (CSEEnv m e)
interpBlock tfm env0 blk = foldlM (interpItem tfm) env0 (toList blk)

interpItem ::
  forall m e.
  (Monad m, CmdRef m e, CSETargetExp e) =>
  TargetFeatureMap ->
  CSEEnv m e ->
  ScopeItem m e ->
  m (CSEEnv m e)
interpItem tfm env = \case
  ItemStm stm -> interpStm tfm env stm
  ItemWithRefPtr cseRef vid innerBlk -> do
    let tgtRef = lookupEnvRef env cseRef
    envInner <- withRefPtr tgtRef $ \tgtPtr ->
      interpBlock tfm (insertEnvVar vid tgtPtr env) innerBlk
    pure (deleteEnvVar vid envInner)
  ItemWithString str vid innerBlk -> do
    envInner <- withString str $ \tgtCStr ->
      interpBlock tfm (insertEnvVar vid tgtCStr env) innerBlk
    pure (deleteEnvVar vid envInner)
  ItemOptimalGang (_ :: TR.TypeRep elemTy) b2 b4 b8 b16 b32 ->
    withOptimalGang @m @e (Proxy @elemTy) $ \pn ->
      env <$ case clampGangWidth (fromIntegral (natVal pn)) of
        2 -> interpBlock tfm env b2
        4 -> interpBlock tfm env b4
        8 -> interpBlock tfm env b8
        16 -> interpBlock tfm env b16
        _ -> interpBlock tfm env b32

interpStm ::
  forall m e.
  (Monad m, CmdRef m e, CSETargetExp e) =>
  TargetFeatureMap ->
  CSEEnv m e ->
  Stm m e ->
  m (CSEEnv m e)
interpStm tfm env = \case
  StmLetM vid cap rhs -> do
    let rhsVal = interpretCSEExpWithEnv env rhs
    boundVal <- cseLetMWithCap cap rhsVal
    pure (insertEnvVar vid boundVal env)
  StmEffectVal vid args eval -> do
    let tgtArgs = evalHArgsWith env args
    resVal <- eval tgtArgs
    pure (insertEnvVar vid resVal env)
  StmEffectUnit args eval -> do
    let tgtArgs = evalHArgsWith env args
    eval tgtArgs
    pure env
  StmNewRef cseRef mbInit -> do
    tgtRef <- case mbInit of
      Just initExp -> newRef (interpretCSEExpWithEnv env initExp)
      Nothing -> newRef'
    pure (insertEnvRef cseRef tgtRef env)
  StmReadRef vid cseRef -> do
    let tgtRef = lookupEnvRef env cseRef
    val <- readRef tgtRef
    pure (insertEnvVar vid val env)
  StmWriteRef cseRef valExp -> do
    let tgtRef = lookupEnvRef env cseRef
    writeRef tgtRef (interpretCSEExpWithEnv env valExp)
    pure env
  StmIf cond tBlk fBlk -> do
    let condVal = interpretCSEExpWithEnv env cond
    ifThenElseM_
      condVal
      (interpBlock tfm env tBlk >> pure ())
      (interpBlock tfm env fBlk >> pure ())
    pure env
  StmWhile cond cseRef bodyBlk -> do
    let condVal = interpretCSEExpWithEnv env cond
    whileM condVal $ \tgtLoopRef -> do
      _ <- interpBlock tfm (insertEnvRef cseRef tgtLoopRef env) bodyBlk
      pure ()
    pure env
  StmRange lo hi vid bodyBlk -> do
    let loVal = interpretCSEExpWithEnv env lo
        hiVal = interpretCSEExpWithEnv env hi
    rangeM loVal hiVal $ \tgtIdx -> do
      _ <- interpBlock tfm (insertEnvVar vid tgtIdx env) bodyBlk
      pure ()
    pure env
  StmSwitch tag cases defBlk -> do
    let tagVal = interpretCSEExpWithEnv env tag
        tgtCases = [(k, interpBlock tfm env b >> pure ()) | (k, b) <- cases]
    switchCase_ tagVal tgtCases (interpBlock tfm env defBlk >> pure ())
    pure env
  StmCUDAAsk tiVars -> do
    tiVals <- ask
    let env' =
          insertEnvVar (_cudaThreadInfoBlockIdxX tiVars) (_cudaThreadInfoBlockIdxX tiVals)
            . insertEnvVar (_cudaThreadInfoBlockIdxY tiVars) (_cudaThreadInfoBlockIdxY tiVals)
            . insertEnvVar (_cudaThreadInfoBlockIdxZ tiVars) (_cudaThreadInfoBlockIdxZ tiVals)
            . insertEnvVar (_cudaThreadInfoBlockDimX tiVars) (_cudaThreadInfoBlockDimX tiVals)
            . insertEnvVar (_cudaThreadInfoBlockDimY tiVars) (_cudaThreadInfoBlockDimY tiVals)
            . insertEnvVar (_cudaThreadInfoBlockDimZ tiVars) (_cudaThreadInfoBlockDimZ tiVals)
            . insertEnvVar (_cudaThreadInfoThreadIdxX tiVars) (_cudaThreadInfoThreadIdxX tiVals)
            . insertEnvVar (_cudaThreadInfoThreadIdxY tiVars) (_cudaThreadInfoThreadIdxY tiVals)
            . insertEnvVar (_cudaThreadInfoThreadIdxZ tiVars) (_cudaThreadInfoThreadIdxZ tiVals)
            . insertEnvVar (_cudaThreadInfoGridDimX tiVars) (_cudaThreadInfoGridDimX tiVals)
            . insertEnvVar (_cudaThreadInfoGridDimY tiVars) (_cudaThreadInfoGridDimY tiVals)
            . insertEnvVar (_cudaThreadInfoGridDimZ tiVars) (_cudaThreadInfoGridDimZ tiVals)
            $ env
    pure env'
  StmFixCall vid mfd callArgs -> do
    let tgtArgs = evalHArgsWith env callArgs
        funKey = mfdKey mfd
        pts = hargsParamTypes (mfdParams mfd)
        retTr = mfdRetType mfd
    resVal <- case Map.lookup funKey (envMonadicFuns env) of
      Just (SomeMonadicTargetFun ptsFound retFound activeSelf)
        | Just Refl <- eqHParamTypes pts ptsFound,
          Just Refl <- testEquality retTr retFound ->
            activeSelf tgtArgs
      _ ->
        let step selfFn formalVals =
              let localEnv =
                    bindFormalHArgs
                      (mfdParams mfd)
                      formalVals
                      ( (emptyCSEEnv @m @e)
                          { envMonadicFuns =
                              Map.insert
                                funKey
                                (SomeMonadicTargetFun pts retTr selfFn)
                                (envMonadicFuns env),
                            envPureFuns = envPureFuns env
                          }
                      )
               in interpCSEMWith tfm interpretCSEExpWithEnv localEnv (mfdBody mfd)
         in mfdEmitTarget mfd step tgtArgs
    pure (insertEnvVar vid resVal env)

evalHArgsWith ::
  (CSETargetExp e) =>
  CSEEnv m e ->
  HArgs (CSEExp e) ts ->
  HArgs e ts
evalHArgsWith _ HNil = HNil
evalHArgsWith env (HCons x xs) =
  HCons (interpretCSEExpWithEnv env x) (evalHArgsWith env xs)

--------------------------------------------------------------------------------
-- Curried Function Interpreter (`interpretCSE`) & `withCSE`
--------------------------------------------------------------------------------

type family CSEBodyMonad (r :: K.Type) :: K.Type -> K.Type where
  CSEBodyMonad (CSEExp _ _) = Identity
  CSEBodyMonad (CSEM m _ _) = m
  CSEBodyMonad (SPMD _ _ _) = Identity
  CSEBodyMonad (SPMDM _ (CSEM m _) _ _) = m
  CSEBodyMonad (_ -> r) = CSEBodyMonad r

class CSEInterpretFrom (r :: K.Type) where
  type CSETarget r :: K.Type
  type CSEInterpExp r :: K.Type -> K.Type
  buildCSEInterpreter ::
    TargetFeatureMap ->
    Int ->
    r ->
    CSEEnv (CSEBodyMonad r) (CSEInterpExp r) ->
    CSETarget r

instance (CSETargetExp e, CSEExpReturnLeaf e a) => CSEInterpretFrom (CSEExp e a) where
  type CSETarget (CSEExp e a) = e (TargetExpElem e a)
  type CSEInterpExp (CSEExp e a) = e
  buildCSEInterpreter _ _ !expr = \env -> interpretExpLeaf (interpretCSEExpWithEnv env expr)

instance
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  CSEInterpretFrom (CSEM m e res)
  where
  type CSETarget (CSEM m e res) = m (CSETargetVal e res)
  type CSEInterpExp (CSEM m e res) = e
  buildCSEInterpreter tfm _ !mAct = \env -> interpretCSEMWithEnvAndFeatures tfm env mAct

instance
  ( CSETargetExp e,
    SPMDTyped n (CSEExp e) a,
    SPMDTyped n e a,
    CSEExpReturnLeaf e (SPMDRep n (CSEExp e) a),
    TargetExpElem e (SPMDRep n (CSEExp e) a) ~ SPMDRep n e a
  ) =>
  CSEInterpretFrom (SPMD n (CSEExp e) a)
  where
  type CSETarget (SPMD n (CSEExp e) a) = SPMD n e a
  type CSEInterpExp (SPMD n (CSEExp e) a) = e
  buildCSEInterpreter _ _ !spmdExp = \env ->
    let rawExp = spmdMaterialize @n @(CSEExp e) @a (runSPMD spmdExp)
        rawTgt = interpretExpLeaf @e @(SPMDRep n (CSEExp e) a) (interpretCSEExpWithEnv env rawExp)
     in SPMD (pure (spmdDematerialize @n @e @a rawTgt))

instance
  (Monad m, CmdRef m e, CSETargetExp e, CSETargetReturn e res) =>
  CSEInterpretFrom (SPMDM n (CSEM m e) (CSEExp e) res)
  where
  type CSETarget (SPMDM n (CSEM m e) (CSEExp e) res) = SPMDM n m e (CSETargetVal e res)
  type CSEInterpExp (SPMDM n (CSEM m e) (CSEExp e) res) = e
  buildCSEInterpreter tfm _ !act = \env ->
    liftSPMD (interpretCSEMWithEnvAndFeatures tfm env (runSPMDM act))

instance
  ( CSEParam p,
    CSEInterpretFrom r,
    e ~ CSEParamExp p,
    e ~ CSEInterpExp r,
    CSEParamMonad p (CSEBodyMonad r)
  ) =>
  CSEInterpretFrom (p -> r)
  where
  type CSETarget (p -> r) = CSEParamTarget p -> CSETarget r
  type CSEInterpExp (p -> r) = CSEParamExp p
  buildCSEInterpreter tfm idx fn =
    let (idx', formalP, _) = cseAllocParam @p idx
        !restInterp = buildCSEInterpreter tfm idx' (fn formalP)
     in \env targetP ->
          let (_, vs, rs) = cseBindTargetParam @p @(CSEBodyMonad r) idx targetP
           in restInterp (extendCSEEnv vs rs env)

-- | Interpret a pure 'CSEExp', a monadic 'CSEM' action, an 'SPMD' / 'SPMDM'
-- computation, or an arbitrarily curried function over @('CSEM' m e, 'CSEExp' e)@
-- into the underlying Janus typeclasses on @(m, e)@.
class (CSEInterpretFrom r) => CSEInterpret (r :: K.Type) where
  interpretCSE :: r -> CSETarget r
  interpretCSEWithFeatures :: TargetFeatureMap -> r -> CSETarget r

instance (CSEInterpretFrom r) => CSEInterpret r where
  interpretCSE = interpretCSEWithFeatures DHM.empty
  interpretCSEWithFeatures tfm r =
    let !runner = buildCSEInterpreter tfm 0 r
     in runner emptyCSEEnv

-- | Optimize a Janus program via algorithmic Common Subexpression Elimination
-- ('cse') and immediately lower it into the target Janus typeclasses
-- ('interpretCSE').
withCSE :: (CSEOptimize r, CSEInterpret r) => r -> CSETarget r
withCSE = interpretCSE . cse

-- | Optimize and lower a Janus program under a known 'TargetFeatureMap'.
withCSEWithFeatures :: (CSEOptimize r, CSEInterpret r) => TargetFeatureMap -> r -> CSETarget r
withCSEWithFeatures tfm = interpretCSEWithFeatures tfm . cseWithFeatures tfm
