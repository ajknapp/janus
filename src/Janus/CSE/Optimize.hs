{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Pure algorithmic Common Subexpression Elimination over 'CSEExp' and 'CSEM'
-- via deduplicated Merkle DAG construction, lexical scope-tree LCA placement,
-- and reverse-topological effective-use counting.
module Janus.CSE.Optimize
  ( -- * Existential Expression, Variable, & Target Wrappers
    SomeCSEExp (..),
    SomeVarId (..),
    SomeTargetVal (..),
    SomeTargetRef (..),

    -- * Direct Expression & Monadic CSE Passes
    cseExp,
    cseProgTreeWith,
    cseM,
    cseMWithFeatures,

    -- * Return, Parameter, & Curried Function Optimization Classes
    TargetExpElem,
    CSEExpReturnLeaf (..),
    CSEExpParamLeaf (..),
    CSEHKD (..),
    GCSEHKD (..),
    GCSETraverse (..),
    GCSEMap (..),
    CSEReturn (..),
    CSETargetReturn (..),
    ParamSubst (..),
    emptyParamSubst,
    isIdentityParamSubst,
    CSEParam (..),
    CSEParamMonad,
    ReifyTensorBoundsShape (..),
    CSEApplySubst (..),
    CSEOptimizeFrom (..),
    cseFromIdx,
    CSEOptimize (..),
  )
where

import Control.Applicative (Const (..))
import Control.Monad.State.Strict (State, evalState, execState, get, gets, modify', put, runState)
import Data.Coerce (Coercible)
import qualified Data.Dependent.HashMap as DHM
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Type.Equality ((:~:) (Refl), testEquality)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics
import GHC.TypeNats (KnownNat, Nat)
import Janus.Backend.C.CUDA.Types (mapCUDAThreadInfo)
import Janus.Backend.C.Target (TargetFeatureMap)
import Janus.CSE.Types
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.TicToc (Tic (..), Toc (..))
import Janus.Control.Reader.Base (ExpNewtype (..))
import Janus.Data.HKD (HKD (..), PtrOf (..))
import Janus.Data.Tensor (Tensor (..), TensorBounds (..))
import Janus.Expression.SIMD (ExpSIMD, SIMDShape, Wide)
import Janus.SPMD.Typed (SPMDTyped (..))
import Janus.SPMD.Types (SIMDPair, SPMD (..), SPMDM, SPMDRep, liftSPMD, runSPMD, runSPMDM)
import qualified Type.Reflection as TR

--------------------------------------------------------------------------------
-- Existential Expression, Variable, & Target Wrappers
--------------------------------------------------------------------------------

data SomeCSEExp (e :: K.Type -> K.Type) where
  SomeCSEExp :: !(TR.TypeRep a) -> !(CSEExp e a) -> SomeCSEExp e

data SomeVarId (e :: K.Type -> K.Type) where
  SomeVarId :: !(VarId e a) -> SomeVarId e

data SomeTargetVal (e :: K.Type -> K.Type) where
  SomeTargetVal :: !(TR.TypeRep a) -> !(e a) -> SomeTargetVal e

data SomeTargetRef (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  SomeTargetRef :: !(TR.TypeRep a) -> !(Ref m e a) -> SomeTargetRef m e

--------------------------------------------------------------------------------
-- Pass 0: $O(|DAG|)$ Normalization of User-Written Pure `ExpLetNode`s
--------------------------------------------------------------------------------

data NormState e = NormState
  { nsMemo :: !(Map.Map Digest (SomeCSEExp e)),
    nsLetSub :: !(Map.Map VarKey (SomeCSEExp e))
  }

normalizeExp :: forall e a. CSEExp e a -> CSEExp e a
normalizeExp root =
  evalState (normGo root) (NormState Map.empty Map.empty)

normGo :: forall e a. CSEExp e a -> State (NormState e) (CSEExp e a)
normGo expr = case expr of
  ExpVar _ (VarId k tr _) -> do
    mbSub <- gets (Map.lookup k . nsLetSub)
    case mbSub of
      Just (SomeCSEExp trSub eSub)
        | Just Refl <- testEquality tr trSub -> pure eSub
      _ -> pure expr
  ExpLit {} -> pure expr
  ExpOp meta tag imms canTrap zeroCost args eval ->
    withNormMemo (emDigest meta) (fst <$> emBind meta) $ do
      args' <- mapHArgsM normGo args
      pure $
        if zeroCost
          then mkExpZeroCostOp tag imms (emBind meta) args' eval
          else mkExpOp tag imms canTrap (emBind meta) args' eval
  ExpListOp meta tag imms xs eval ->
    withNormMemo (emDigest meta) (fst <$> emBind meta) $ do
      xs' <- traverse normGo xs
      pure (mkExpListOp tag imms (emBind meta) xs' eval)
  ExpShortCircuit meta op lhs rhs eval ->
    withNormMemo (emDigest meta) (Just (TR.typeRep @Bool)) $ do
      lhs' <- normGo lhs
      rhs' <- normGo rhs
      pure (mkExpShortCircuit op lhs' rhs' eval)
  ExpCondNode meta c t f eval ->
    case emBind meta of
      Just (tr, cap) ->
        withNormMemo (emDigest meta) (Just tr) $ do
          c' <- normGo c
          t' <- normGo t
          f' <- normGo f
          pure (mkExpCond tr cap c' t' f' eval)
      Nothing -> pure expr
  ExpLetNode meta (VarId k trA _) _ _ rhs body ->
    withNormMemo (emDigest meta) (fst <$> emBind meta) $ do
      rhs' <- normGo rhs
      modify' $ \st -> st {nsLetSub = Map.insert k (SomeCSEExp trA rhs') (nsLetSub st)}
      normGo body
  ExpFixCall meta pfd args ->
    withNormMemo (emDigest meta) (Just (pfdRetType pfd)) $ do
      args' <- mapHArgsM normGo args
      pure (mkExpFixCall pfd args')

withNormMemo ::
  Digest ->
  Maybe (TR.TypeRep a) ->
  State (NormState e) (CSEExp e a) ->
  State (NormState e) (CSEExp e a)
withNormMemo _ Nothing compute = compute
withNormMemo origDig (Just tr) compute = do
  mbHit <- gets (Map.lookup origDig . nsMemo)
  case mbHit of
    Just (SomeCSEExp trHit eHit)
      | Just Refl <- testEquality tr trHit -> pure eHit
    _ -> do
      res <- compute
      modify' $ \st -> st {nsMemo = Map.insert origDig (SomeCSEExp tr res) (nsMemo st)}
      pure res

normalizeBlock :: Block m e -> Block m e
normalizeBlock = fmap normalizeItem

normalizeItem :: ScopeItem m e -> ScopeItem m e
normalizeItem = \case
  ItemStm stm -> ItemStm (normalizeStm stm)
  ItemWithRefPtr ref vid blk -> ItemWithRefPtr ref vid (normalizeBlock blk)
  ItemWithString s vid blk -> ItemWithString s vid (normalizeBlock blk)
  ItemOptimalGang tr b2 b4 b8 b16 b32 ->
    ItemOptimalGang
      tr
      (normalizeBlock b2)
      (normalizeBlock b4)
      (normalizeBlock b8)
      (normalizeBlock b16)
      (normalizeBlock b32)

normalizeStm :: Stm m e -> Stm m e
normalizeStm = \case
  StmLetM vid cap rhs -> StmLetM vid cap (normalizeExp rhs)
  StmEffectVal vid args eval -> StmEffectVal vid (runIdentity (mapHArgsM (Identity . normalizeExp) args)) eval
  StmEffectUnit args eval -> StmEffectUnit (runIdentity (mapHArgsM (Identity . normalizeExp) args)) eval
  StmNewRef ref mbInit -> StmNewRef ref (fmap normalizeExp mbInit)
  StmReadRef vid ref -> StmReadRef vid ref
  StmWriteRef ref val -> StmWriteRef ref (normalizeExp val)
  StmIf c tBlk fBlk -> StmIf (normalizeExp c) (normalizeBlock tBlk) (normalizeBlock fBlk)
  StmWhile c ref bodyBlk -> StmWhile (normalizeExp c) ref (normalizeBlock bodyBlk)
  StmRange lo hi vid bodyBlk -> StmRange (normalizeExp lo) (normalizeExp hi) vid (normalizeBlock bodyBlk)
  StmSwitch tag cases defBlk ->
    StmSwitch (normalizeExp tag) [(k, normalizeBlock b) | (k, b) <- cases] (normalizeBlock defBlk)
  StmCUDAAsk ti -> StmCUDAAsk ti
  StmFixCall vid mfd args ->
    StmFixCall vid mfd (runIdentity (mapHArgsM (Identity . normalizeExp) args))

--------------------------------------------------------------------------------
-- Scope Tree & Deduplicated Merkle DAG Construction
--------------------------------------------------------------------------------

type ScopeId = Int

type DagId = Int

data ScopeKind
  = SKBlock
  | SKPureBindable
  | SKPureNoBind
  deriving stock (Eq, Show)

data ScopeInfo = ScopeInfo
  { siId :: !ScopeId,
    siParent :: !(Maybe (ScopeId, Int)),
    siDepth :: !Int,
    siKind :: !ScopeKind,
    siExhaustiveGroups :: ![[ScopeId]],
    siUncondChildren :: ![ScopeId]
  }
  deriving stock (Show)

data UseSite = UseSite
  { usScope :: !ScopeId,
    usItemIdx :: !Int
  }
  deriving stock (Eq, Ord, Show)

data CondBranchScopes = CondBranchScopes
  { cbsThen :: !ScopeId,
    cbsElse :: !ScopeId
  }
  deriving stock (Eq, Ord, Show)

-- | Structural shape of an interned pure expression node in the deduplicated
-- Merkle DAG.
data DagNodeShape e a where
  -- | Standard eager pure operator or recursive call whose operand
  -- subexpressions are evaluated in the same scope as this node.
  ShapeRegular ::
    -- | Normalized expression template used to reconstruct the node during emission.
    !(CSEExp e a) ->
    -- | Direct child 'DagId's mapped to their syntactic occurrence multiplicities
    -- within this node's immediate operands.
    !(IntMap.IntMap Int) ->
    DagNodeShape e a
  -- | Pure conditional ('Janus.Expression.Cond.ifThenElse') whose condition is
  -- evaluated in the enclosing scope while the true and false arms live in
  -- guarded child scopes ('SKPureBindable').
  ShapeCond ::
    -- | Normalized boolean condition expression.
    !(CSEExp e Bool) ->
    -- | Interned 'DagId' of the condition expression, or 'Nothing' if trivial/unbindable.
    !(Maybe DagId) ->
    -- | Normalized true-branch expression.
    !(CSEExp e a) ->
    -- | Interned 'DagId' of the true-branch expression, or 'Nothing' if trivial/unbindable.
    !(Maybe DagId) ->
    -- | Normalized false-branch expression.
    !(CSEExp e a) ->
    -- | Interned 'DagId' of the false-branch expression, or 'Nothing' if trivial/unbindable.
    !(Maybe DagId) ->
    -- | Target evaluator lowering @(cond, thenVal, elseVal)@ into @e a@.
    !(e Bool -> e a -> e a -> e a) ->
    DagNodeShape e a
  -- | Short-circuit boolean connective ('SCLand' / 'SCLor') whose LHS is
  -- evaluated in the enclosing scope while the RHS lives in a non-bindable
  -- guarded scope ('SKPureNoBind').
  ShapeShortCircuit ::
    -- | Whether this node is logical AND ('SCLand') or logical OR ('SCLor').
    !ShortCircuitOp ->
    -- | Normalized left-hand boolean operand (evaluated unconditionally at this site).
    !(CSEExp e Bool) ->
    -- | Interned 'DagId' of the left-hand operand, or 'Nothing' if trivial/unbindable.
    !(Maybe DagId) ->
    -- | Normalized right-hand boolean operand (guarded by the left-hand operand).
    !(CSEExp e Bool) ->
    -- | Interned 'DagId' of the right-hand operand, or 'Nothing' if trivial/unbindable.
    !(Maybe DagId) ->
    -- | Target evaluator lowering @(lhs, rhs)@ into @e 'Bool'@.
    !(e Bool -> e Bool -> e Bool) ->
    DagNodeShape e Bool

data DagEntry e where
  DagEntry ::
    { deId :: !DagId,
      deDigest :: !Digest,
      deTypeRep :: !(TR.TypeRep a),
      deBindCap :: !(BindCap e a),
      deHasTrapOrFix :: !Bool,
      deFreeVars :: !(Set.Set VarKey),
      deShape :: !(DagNodeShape e a)
    } ->
    DagEntry e

data BuildState e = BuildState
  { bsNextScope :: !ScopeId,
    bsScopes :: !(IntMap.IntMap ScopeInfo),
    bsVarDefs :: !(Map.Map VarKey UseSite),
    bsNextDag :: !DagId,
    bsDigToDag :: !(Map.Map Digest DagId),
    bsDagEntries :: !(IntMap.IntMap (DagEntry e)),
    bsRootSites :: !(IntMap.IntMap (Map.Map UseSite Int)),
    -- For Cond / ShortCircuit instances at each syntactic/propagated site:
    bsCondSiteBranches :: !(Map.Map (DagId, UseSite) CondBranchScopes),
    bsSCSiteRHS :: !(Map.Map (DagId, UseSite) ScopeId)
  }

initBuildState :: ScopeKind -> BuildState e
initBuildState rootKind =
  let rootInfo =
        ScopeInfo
          { siId = 0,
            siParent = Nothing,
            siDepth = 0,
            siKind = rootKind,
            siExhaustiveGroups = [],
            siUncondChildren = []
          }
   in BuildState
        { bsNextScope = 1,
          bsScopes = IntMap.singleton 0 rootInfo,
          bsVarDefs = Map.empty,
          bsNextDag = 0,
          bsDigToDag = Map.empty,
          bsDagEntries = IntMap.empty,
          bsRootSites = IntMap.empty,
          bsCondSiteBranches = Map.empty,
          bsSCSiteRHS = Map.empty
        }

allocScope :: ScopeId -> Int -> ScopeKind -> State (BuildState e) ScopeId
allocScope parentId itemIdx kind = do
  st <- get
  let sid = bsNextScope st
      pInfo = bsScopes st IntMap.! parentId
      info =
        ScopeInfo
          { siId = sid,
            siParent = Just (parentId, itemIdx),
            siDepth = siDepth pInfo + 1,
            siKind = kind,
            siExhaustiveGroups = [],
            siUncondChildren = []
          }
  put st {bsNextScope = sid + 1, bsScopes = IntMap.insert sid info (bsScopes st)}
  pure sid

addExhaustiveGroup :: ScopeId -> [ScopeId] -> State (BuildState e) ()
addExhaustiveGroup parentId grp =
  modify' $ \st ->
    st
      { bsScopes =
          IntMap.adjust
            (\si -> si {siExhaustiveGroups = grp : siExhaustiveGroups si})
            parentId
            (bsScopes st)
      }

addUncondChild :: ScopeId -> ScopeId -> State (BuildState e) ()
addUncondChild parentId childId =
  modify' $ \st ->
    st
      { bsScopes =
          IntMap.adjust
            (\si -> si {siUncondChildren = childId : siUncondChildren si})
            parentId
            (bsScopes st)
      }

recordVarDef :: VarKey -> ScopeId -> Int -> State (BuildState e) ()
recordVarDef k sid afterIdx =
  modify' $ \st -> st {bsVarDefs = Map.insert k (UseSite sid afterIdx) (bsVarDefs st)}

addRootSite :: DagId -> UseSite -> State (BuildState e) ()
addRootSite did site =
  modify' $ \st ->
    let inner = Map.insertWith (+) site 1 (IntMap.findWithDefault Map.empty did (bsRootSites st))
     in st {bsRootSites = IntMap.insert did inner (bsRootSites st)}

-- | Intern an expression into the deduplicated Merkle DAG in $O(1)$ amortized
-- work per unique node. Returns @Just did@ if the expression itself is a
-- non-trivial bindable DAG node, or @Nothing@ if trivial (after interning any
-- non-trivial children inside zero-cost wrappers).
internExp :: forall e a. CSEExp e a -> State (BuildState e) (Maybe DagId)
internExp expr =
  let meta = expMeta expr
   in case expr of
        ExpVar {} -> pure Nothing
        ExpLit {} -> pure Nothing
        ExpOp _ _ _ _ True args _ -> do
          -- Zero-cost op (`emIsTrivial == True`): intern children directly as roots at caller
          _ <- collectHArgsEdges args
          pure Nothing
        _ ->
          case emBind meta of
            Nothing -> pure Nothing
            Just (tr, cap) -> do
              let dig = emDigest meta
              mbExisting <- gets (Map.lookup dig . bsDigToDag)
              case mbExisting of
                Just did -> pure (Just did)
                Nothing -> do
                  shape <- buildShape expr
                  st <- get
                  let did = bsNextDag st
                      entry =
                        DagEntry
                          { deId = did,
                            deDigest = dig,
                            deTypeRep = tr,
                            deBindCap = cap,
                            deHasTrapOrFix = emHasTrapOrFix meta,
                            deFreeVars = emFreeVars meta,
                            deShape = shape
                          }
                  put
                    st
                      { bsNextDag = did + 1,
                        bsDigToDag = Map.insert dig did (bsDigToDag st),
                        bsDagEntries = IntMap.insert did entry (bsDagEntries st)
                      }
                  pure (Just did)

buildShape :: forall e a. CSEExp e a -> State (BuildState e) (DagNodeShape e a)
buildShape expr = case expr of
  ExpOp _ _ _ _ _ args _ -> do
    edges <- collectHArgsEdges args
    pure (ShapeRegular expr edges)
  ExpListOp _ _ _ xs _ -> do
    edges <- collectListEdges xs
    pure (ShapeRegular expr edges)
  ExpFixCall _ _ args -> do
    edges <- collectHArgsEdges args
    pure (ShapeRegular expr edges)
  ExpCondNode _ c t f eval -> do
    mbC <- internChildRoot c
    mbT <- internChildRoot t
    mbF <- internChildRoot f
    pure (ShapeCond c mbC t mbT f mbF eval)
  ExpShortCircuit _ op lhs rhs eval -> do
    mbL <- internChildRoot lhs
    mbR <- internChildRoot rhs
    pure (ShapeShortCircuit op lhs mbL rhs mbR eval)
  _ -> pure (ShapeRegular expr IntMap.empty)

-- | Extract the non-trivial DagId(s) reachable through zero-cost wrappers.
collectDirectChildren :: CSEExp e x -> State (BuildState e) (IntMap.IntMap Int)
collectDirectChildren expr = case expr of
  ExpVar {} -> pure IntMap.empty
  ExpLit {} -> pure IntMap.empty
  ExpOp _ _ _ _ True args _ -> collectHArgsEdges args
  _ -> do
    mbD <- internExp expr
    pure $ case mbD of
      Nothing -> IntMap.empty
      Just d -> IntMap.singleton d 1

-- | For `ExpCondNode` / `ExpShortCircuit` arms: if the arm is a zero-cost wrapper
-- around a single non-trivial node, unwrap to find any child DagId, or handle via
-- `registerRootAt` when registering scopes.
internChildRoot :: CSEExp e x -> State (BuildState e) (Maybe DagId)
internChildRoot expr = case expr of
  ExpOp _ _ _ _ True (HCons inner HNil) _ -> internChildRoot inner
  _ -> internExp expr

collectHArgsEdges :: HArgs (CSEExp e) ts -> State (BuildState e) (IntMap.IntMap Int)
collectHArgsEdges HNil = pure IntMap.empty
collectHArgsEdges (HCons x xs) = do
  m1 <- collectDirectChildren x
  m2 <- collectHArgsEdges xs
  pure (IntMap.unionWith (+) m1 m2)

collectListEdges :: [CSEExp e b] -> State (BuildState e) (IntMap.IntMap Int)
collectListEdges [] = pure IntMap.empty
collectListEdges (x : xs) = do
  m1 <- collectDirectChildren x
  m2 <- collectListEdges xs
  pure (IntMap.unionWith (+) m1 m2)

-- | Register a top-level expression occurrence at `UseSite` (creating any
-- nested pure scopes for `ShapeCond` / `ShapeShortCircuit` if not yet created
-- at this `(DagId, UseSite)`).
registerExpAt :: UseSite -> CSEExp e a -> State (BuildState e) ()
registerExpAt site expr = case expr of
  ExpVar {} -> pure ()
  ExpLit {} -> pure ()
  ExpOp _ _ _ _ True args _ -> registerHArgsAt site args
  _ -> do
    mbDid <- internExp expr
    case mbDid of
      Nothing -> pure ()
      Just did -> do
        addRootSite did site
        ensureGuardedScopes did site

registerHArgsAt :: UseSite -> HArgs (CSEExp e) ts -> State (BuildState e) ()
registerHArgsAt _ HNil = pure ()
registerHArgsAt site (HCons x xs) = registerExpAt site x >> registerHArgsAt site xs

ensureGuardedScopes :: DagId -> UseSite -> State (BuildState e) ()
ensureGuardedScopes did site@(UseSite sid idx) = do
  entry <- gets ((IntMap.! did) . bsDagEntries)
  case entry of
    DagEntry {deShape = ShapeCond _ _ _ mbT _ mbF _} -> do
      already <- gets (Map.member (did, site) . bsCondSiteBranches)
      if already
        then pure ()
        else do
          sT <- allocScope sid idx SKPureBindable
          sF <- allocScope sid idx SKPureBindable
          addExhaustiveGroup sid [sT, sF]
          modify' $ \st ->
            st {bsCondSiteBranches = Map.insert (did, site) (CondBranchScopes sT sF) (bsCondSiteBranches st)}
          case mbT of
            Just dT -> ensureGuardedScopes dT (UseSite sT 0)
            Nothing -> pure ()
          case mbF of
            Just dF -> ensureGuardedScopes dF (UseSite sF 0)
            Nothing -> pure ()
    DagEntry {deShape = ShapeShortCircuit _ _ _ _ mbR _} -> do
      already <- gets (Map.member (did, site) . bsSCSiteRHS)
      if already
        then pure ()
        else do
          sR <- allocScope sid idx SKPureNoBind
          modify' $ \st ->
            st {bsSCSiteRHS = Map.insert (did, site) sR (bsSCSiteRHS st)}
          case mbR of
            Just dR -> ensureGuardedScopes dR (UseSite sR 0)
            Nothing -> pure ()
    _ -> pure ()

registerBlock :: ScopeId -> Block m e -> State (BuildState e) ()
registerBlock sid blk =
  mapM_ (uncurry (registerItem sid)) (zip [0 ..] (toList blk))

recordHArgVarDefs :: HArgs (VarId e) ts -> ScopeId -> Int -> State (BuildState e) ()
recordHArgVarDefs HNil _ _ = pure ()
recordHArgVarDefs (HCons vid rest) sid itemIdx = do
  recordVarDef (vidKey vid) sid itemIdx
  recordHArgVarDefs rest sid itemIdx

registerItem :: ScopeId -> Int -> ScopeItem m e -> State (BuildState e) ()
registerItem sid idx item =
  let site = UseSite sid idx
   in case item of
        ItemStm stm -> case stm of
          StmLetM vid _ rhs -> do
            registerExpAt site rhs
            recordVarDef (vidKey vid) sid (idx + 1)
          StmEffectVal vid args _ -> do
            registerHArgsAt site args
            recordVarDef (vidKey vid) sid (idx + 1)
          StmEffectUnit args _ ->
            registerHArgsAt site args
          StmNewRef _ mbInit ->
            mapM_ (registerExpAt site) mbInit
          StmReadRef vid _ ->
            recordVarDef (vidKey vid) sid (idx + 1)
          StmWriteRef _ val ->
            registerExpAt site val
          StmIf cond tBlk fBlk -> do
            registerExpAt site cond
            sT <- allocScope sid idx SKBlock
            sF <- allocScope sid idx SKBlock
            addExhaustiveGroup sid [sT, sF]
            registerBlock sT tBlk
            registerBlock sF fBlk
          StmWhile cond _ bodyBlk -> do
            registerExpAt site cond
            sB <- allocScope sid idx SKBlock
            registerBlock sB bodyBlk
          StmRange lo hi vid bodyBlk -> do
            registerExpAt site lo
            registerExpAt site hi
            sB <- allocScope sid idx SKBlock
            recordVarDef (vidKey vid) sB 0
            registerBlock sB bodyBlk
          StmSwitch tag cases defBlk -> do
            registerExpAt site tag
            caseScopes <- mapM (\(_, b) -> do sC <- allocScope sid idx SKBlock; registerBlock sC b; pure sC) cases
            sDef <- allocScope sid idx SKBlock
            registerBlock sDef defBlk
            addExhaustiveGroup sid (caseScopes <> [sDef])
          StmCUDAAsk _ -> pure ()
          StmFixCall vids _ args -> do
            registerHArgsAt site args
            recordHArgVarDefs vids sid (idx + 1)
        ItemWithRefPtr _ vid innerBlk -> do
          sInner <- allocScope sid idx SKBlock
          addUncondChild sid sInner
          recordVarDef (vidKey vid) sInner 0
          registerBlock sInner innerBlk
        ItemWithString _ vid innerBlk -> do
          sInner <- allocScope sid idx SKBlock
          addUncondChild sid sInner
          recordVarDef (vidKey vid) sInner 0
          registerBlock sInner innerBlk
        ItemOptimalGang _ b2 b4 b8 b16 b32 -> do
          s2 <- allocScope sid idx SKBlock
          s4 <- allocScope sid idx SKBlock
          s8 <- allocScope sid idx SKBlock
          s16 <- allocScope sid idx SKBlock
          s32 <- allocScope sid idx SKBlock
          addExhaustiveGroup sid [s2, s4, s8, s16, s32]
          registerBlock s2 b2
          registerBlock s4 b4
          registerBlock s8 b8
          registerBlock s16 b16
          registerBlock s32 b32

--------------------------------------------------------------------------------
-- Scope LCA, Dominance, & Reverse-Topological Placement Pass
--------------------------------------------------------------------------------

lcaScope :: IntMap.IntMap ScopeInfo -> ScopeId -> ScopeId -> ScopeId
lcaScope scopes = go
  where
    go s1 s2
      | s1 == s2 = s1
      | d1 > d2 = go (parentOf s1) s2
      | d2 > d1 = go s1 (parentOf s2)
      | otherwise = go (parentOf s1) (parentOf s2)
      where
        d1 = siDepth (scopes IntMap.! s1)
        d2 = siDepth (scopes IntMap.! s2)
    parentOf s = case siParent (scopes IntMap.! s) of
      Just (p, _) -> p
      Nothing -> 0

isDescendantScope :: IntMap.IntMap ScopeInfo -> ScopeId -> ScopeId -> Bool
isDescendantScope scopes cand anc =
  let dCand = siDepth (scopes IntMap.! cand)
      dAnc = siDepth (scopes IntMap.! anc)
   in dCand >= dAnc && walkUp (dCand - dAnc) cand == anc
  where
    walkUp 0 s = s
    walkUp k s = case siParent (scopes IntMap.! s) of
      Just (p, _) -> walkUp (k - 1) p
      Nothing -> s

-- | Given `anc` and a `UseSite` at or inside `anc`, return the item index in
-- `anc` containing that use site.
itemIdxInAncestor :: IntMap.IntMap ScopeInfo -> ScopeId -> UseSite -> Int
itemIdxInAncestor scopes anc (UseSite s idx)
  | s == anc = idx
  | otherwise = case siParent (scopes IntMap.! s) of
      Just (p, pIdx) -> itemIdxInAncestor scopes anc (UseSite p pIdx)
      Nothing -> 0

-- | Immediate child scope of `anc` along the path toward `desc` (precondition:
-- `desc` is a strict descendant of `anc`).
immediateChildToward :: IntMap.IntMap ScopeInfo -> ScopeId -> ScopeId -> ScopeId
immediateChildToward scopes anc desc =
  case siParent (scopes IntMap.! desc) of
    Just (p, _)
      | p == anc -> desc
      | otherwise -> immediateChildToward scopes anc p
    Nothing -> desc

-- | Walk upward out of any `SKPureNoBind` (`&&`/`||` RHS) scope to the nearest
-- bindable scope (`SKBlock` or `SKPureBindable`).
nearestBindableAncestor :: IntMap.IntMap ScopeInfo -> UseSite -> Maybe UseSite
nearestBindableAncestor scopes (UseSite s idx) =
  let info = scopes IntMap.! s
   in case siKind info of
        SKBlock -> Just (UseSite s idx)
        SKPureBindable -> Just (UseSite s 0)
        SKPureNoBind ->
          case siParent info of
            Just (p, pIdx) -> nearestBindableAncestor scopes (UseSite p pIdx)
            Nothing -> Nothing

-- | Check whether a DAG node with direct occurrence scopes `directScopes` is
-- guaranteed to be evaluated on every execution path through `targetScope`.
isUnconditionalIn :: IntMap.IntMap ScopeInfo -> IntSet.IntSet -> ScopeId -> Bool
isUnconditionalIn scopes directScopes = check
  where
    check s =
      IntSet.member s directScopes
        || any check (siUncondChildren info)
        || any (all check) (siExhaustiveGroups info)
      where
        info = scopes IntMap.! s

-- | Adjust a candidate `(sCand, earliestIdx)` if any free variable of `D` was
-- defined inside an `ItemWithRefPtr`/`ItemWithString` child scope at
-- `earliestIdx` (meaning `D` is used both inside that scoped item and after it).
refineForEscapedDefs ::
  IntMap.IntMap ScopeInfo ->
  Map.Map VarKey UseSite ->
  Set.Set VarKey ->
  Map.Map UseSite Int ->
  UseSite ->
  UseSite
refineForEscapedDefs scopes varDefs freeVars sites = go
  where
    go cand@(UseSite sCand idxCand) =
      let blockingDefs =
            [ defScope
              | vk <- Set.toList freeVars,
                Just (UseSite defScope _) <- [Map.lookup vk varDefs],
                defScope /= sCand,
                isDescendantScope scopes defScope sCand,
                itemIdxInAncestor scopes sCand (UseSite defScope 0) >= idxCand
            ]
       in case blockingDefs of
            [] -> cand
            (dScope : _) ->
              let innerIdxs =
                    [ itemIdxInAncestor scopes dScope u
                      | u <- Map.keys sites,
                        isDescendantScope scopes (usScope u) dScope
                    ]
                  newIdx = case innerIdxs of
                    [] -> 0
                    xs -> minimum xs
               in go (UseSite dScope newIdx)

data CommitBinding (e :: K.Type -> K.Type) = CommitBinding
  { cbDagId :: !DagId,
    cbSite :: !UseSite,
    cbVarKey :: !VarKey
  }
  deriving stock (Show)

data CommitState e = CommitState
  { csBuild :: !(BuildState e),
    csSiteCounts :: !(IntMap.IntMap (Map.Map UseSite Int)),
    csNextCSEVar :: !Int,
    csBindings :: !(IntMap.IntMap [CommitBinding e]) -- DagId -> committed bindings (usually 1)
  }

addPropagatedSite :: DagId -> UseSite -> Int -> State (CommitState e) ()
addPropagatedSite _ _ n | n <= 0 = pure ()
addPropagatedSite did site n = do
  st <- get
  let curMap = IntMap.findWithDefault Map.empty did (csSiteCounts st)
      curMap' = Map.insertWith (+) site n curMap
  put st {csSiteCounts = IntMap.insert did curMap' (csSiteCounts st)}
  -- Ensure nested Cond / ShortCircuit scopes exist at propagated `site`
  let ((), bSt') = runState (ensureGuardedScopes did site) (csBuild st)
  modify' $ \s -> s {csBuild = bSt'}

allocCSEVar :: State (CommitState e) VarKey
allocCSEVar = do
  st <- get
  let k = csNextCSEVar st
  put st {csNextCSEVar = k + 1}
  pure (VarCSE k)

-- | Attempt to commit bindings for `entry` given a subset of its `sites`.
decideSites ::
  DagEntry e ->
  Map.Map UseSite Int ->
  State (CommitState e) [CommitBinding e]
decideSites entry sites = do
  let totalUses = sum (Map.elems sites)
  if totalUses < 2 || Map.null sites
    then pure []
    else do
      bSt <- gets csBuild
      let scopes = bsScopes bSt
          varDefs = bsVarDefs bSt
          allScopes = map usScope (Map.keys sites)
          sLca = foldl1 (lcaScope scopes) allScopes
          rawEarliest = minimum [itemIdxInAncestor scopes sLca u | u <- Map.keys sites]
      case nearestBindableAncestor scopes (UseSite sLca rawEarliest) of
        Nothing -> pure []
        Just rawBindSite -> do
          let bindSite@(UseSite sBind _) =
                case siKind (scopes IntMap.! usScope rawBindSite) of
                  SKBlock -> refineForEscapedDefs scopes varDefs (deFreeVars entry) sites rawBindSite
                  _ -> UseSite (usScope rawBindSite) 0
              directScopes = IntSet.fromList (map usScope (Map.keys sites))
              safeHere =
                not (deHasTrapOrFix entry)
                  || isUnconditionalIn scopes directScopes sBind
          if safeHere
            then do
              vk <- allocCSEVar
              pure [CommitBinding (deId entry) bindSite vk]
            else do
              -- Partition strict-descendant occurrences by immediate child scopes of `sLca`
              let descSites =
                    Map.fromListWith
                      (Map.unionWith (+))
                      [ (immediateChildToward scopes sLca (usScope u), Map.singleton u cnt)
                        | (u, cnt) <- Map.toList sites,
                          usScope u /= sLca
                      ]
              concat <$> mapM (decideSites entry) (Map.elems descSites)

runReverseTopoCommit :: BuildState e -> CommitState e
runReverseTopoCommit bSt =
  let nDag = bsNextDag bSt
      initCS =
        CommitState
          { csBuild = bSt,
            csSiteCounts = bsRootSites bSt,
            csNextCSEVar = 0,
            csBindings = IntMap.empty
          }
   in execState (mapM_ stepDag [nDag - 1, nDag - 2 .. 0]) initCS
  where
    stepDag :: DagId -> State (CommitState e) ()
    stepDag did = do
      sites <- gets (IntMap.findWithDefault Map.empty did . csSiteCounts)
      if Map.null sites
        then pure ()
        else do
          entry <- gets ((IntMap.! did) . bsDagEntries . csBuild)
          commits <- decideSites entry sites
          if null commits
            then propagateFromSites entry (Map.toList sites)
            else do
              modify' $ \st -> st {csBindings = IntMap.insert did commits (csBindings st)}
              propagateFromSites entry [(cbSite cb, 1) | cb <- commits]

    propagateFromSites :: DagEntry e -> [(UseSite, Int)] -> State (CommitState e) ()
    propagateFromSites (DagEntry {deId = did, deShape = shape}) evalSites =
      case shape of
        ShapeRegular _ edges ->
          mapM_
            ( \(childDid, mult) ->
                mapM_ (\(u, cnt) -> addPropagatedSite childDid u (cnt * mult)) evalSites
            )
            (IntMap.toList edges)
        ShapeCond _ mbC _ mbT _ mbF _ ->
          mapM_
            ( \(u, cnt) -> do
                case mbC of
                  Just dC -> addPropagatedSite dC u cnt
                  Nothing -> pure ()
                -- Ensure CondBranchScopes exist at `u` (important when `u` is a hoisted parent bind site!)
                st0 <- get
                let ((), bSt1) = runState (ensureGuardedScopes did u) (csBuild st0)
                put st0 {csBuild = bSt1}
                CondBranchScopes sT sF <- gets ((Map.! (did, u)) . bsCondSiteBranches . csBuild)
                case mbT of
                  Just dT -> addPropagatedSite dT (UseSite sT 0) cnt
                  Nothing -> pure ()
                case mbF of
                  Just dF -> addPropagatedSite dF (UseSite sF 0) cnt
                  Nothing -> pure ()
            )
            evalSites
        ShapeShortCircuit _ _ mbL _ mbR _ ->
          mapM_
            ( \(u, cnt) -> do
                case mbL of
                  Just dL -> addPropagatedSite dL u cnt
                  Nothing -> pure ()
                st0 <- get
                let ((), bSt1) = runState (ensureGuardedScopes did u) (csBuild st0)
                put st0 {csBuild = bSt1}
                sR <- gets ((Map.! (did, u)) . bsSCSiteRHS . csBuild)
                case mbR of
                  Just dR -> addPropagatedSite dR (UseSite sR 0) cnt
                  Nothing -> pure ()
            )
            evalSites

--------------------------------------------------------------------------------
-- Forward-Topological AST Emission & Scoped Rewriting
--------------------------------------------------------------------------------

type ActiveMap (e :: K.Type -> K.Type) = Map.Map Digest (SomeVarId e)

type SiteBindIndex (e :: K.Type -> K.Type) = Map.Map UseSite [(DagId, VarKey)]

buildSiteBindIndex :: CommitState e -> SiteBindIndex e
buildSiteBindIndex cs =
  -- Forward-topological order (`did` ascending: 0 .. nDag - 1) so child bindings
  -- at the same `UseSite` precede parent bindings that depend on them.
  let pairs =
        [ (cbSite cb, [(did, cbVarKey cb)])
          | (did, cbs) <- IntMap.toAscList (csBindings cs),
            cb <- cbs
        ]
   in Map.fromListWith (flip (<>)) pairs

emitBlockCSE ::
  forall m e.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  ScopeId ->
  Block m e ->
  (ActiveMap e, Block m e)
emitBlockCSE cs siteIdx active0 sid blk =
  let items = toList blk
      (activeAfterItems, chunks) = go 0 active0 items
   in (activeAfterItems, Seq.fromList (concat chunks))
  where
    bSt = csBuild cs
    dagEntries = bsDagEntries bSt

    emitSiteLetMs :: Int -> ActiveMap e -> (ActiveMap e, [ScopeItem m e])
    emitSiteLetMs idx act0 =
      let binds = Map.findWithDefault [] (UseSite sid idx) siteIdx
       in foldl stepBind (act0, []) binds
      where
        stepBind (act, acc) (did, vk) =
          case dagEntries IntMap.! did of
            DagEntry {deId = dId, deDigest = dig, deTypeRep = tr, deBindCap = cap, deShape = shape} ->
              let vid = VarId vk tr (Just cap)
                  rhs' = rewriteDagShape cs siteIdx act (UseSite sid idx) dId tr cap shape
                  act' = Map.insert dig (SomeVarId vid) act
               in (act', acc <> [ItemStm (StmLetM vid cap rhs')])

    go :: Int -> ActiveMap e -> [ScopeItem m e] -> (ActiveMap e, [[ScopeItem m e]])
    go idx act [] =
      -- Also emit any bindings anchored at the virtual return index (`idx = length items`)
      let (act', pre) = emitSiteLetMs idx act
       in (act', [pre | not (null pre)])
    go idx act (it : rest) =
      let (act1, pre) = emitSiteLetMs idx act
          site = UseSite sid idx
          (act2, it') = rewriteItem cs siteIdx act1 site it
          (actFinal, restChunks) = go (idx + 1) act2 rest
       in (actFinal, (pre <> [it']) : restChunks)

rewriteItem ::
  forall m e.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  UseSite ->
  ScopeItem m e ->
  (ActiveMap e, ScopeItem m e)
rewriteItem cs siteIdx act site@(UseSite sid idx) item =
  let bSt = csBuild cs
      childScopesAt predicate =
        [ siId si
          | si <- IntMap.elems (bsScopes bSt),
            siParent si == Just (sid, idx),
            predicate si
        ]
      blockChildren = filter (\s -> siKind (bsScopes bSt IntMap.! s) == SKBlock) (childScopesAt (const True))
   in case item of
        ItemStm stm -> case stm of
          StmLetM vid cap rhs ->
            (act, ItemStm (StmLetM vid cap (rewriteExp cs siteIdx act site rhs)))
          StmEffectVal vid args eval ->
            (act, ItemStm (StmEffectVal vid (rewriteHArgs cs siteIdx act site args) eval))
          StmEffectUnit args eval ->
            (act, ItemStm (StmEffectUnit (rewriteHArgs cs siteIdx act site args) eval))
          StmNewRef ref mbInit ->
            (act, ItemStm (StmNewRef ref (fmap (rewriteExp cs siteIdx act site) mbInit)))
          StmReadRef vid ref ->
            (act, ItemStm (StmReadRef vid ref))
          StmWriteRef ref val ->
            (act, ItemStm (StmWriteRef ref (rewriteExp cs siteIdx act site val)))
          StmIf cond tBlk fBlk ->
            let cond' = rewriteExp cs siteIdx act site cond
                (sT, sF) = case blockChildren of
                  [a, b] -> (a, b)
                  _ -> error "Janus.CSE.Optimize: StmIf scope mismatch"
                (_, tBlk') = emitBlockCSE cs siteIdx act sT tBlk
                (_, fBlk') = emitBlockCSE cs siteIdx act sF fBlk
             in (act, ItemStm (StmIf cond' tBlk' fBlk'))
          StmWhile cond ref bodyBlk ->
            let cond' = rewriteExp cs siteIdx act site cond
                sB = case blockChildren of
                  [a] -> a
                  _ -> error "Janus.CSE.Optimize: StmWhile scope mismatch"
                (_, bodyBlk') = emitBlockCSE cs siteIdx act sB bodyBlk
             in (act, ItemStm (StmWhile cond' ref bodyBlk'))
          StmRange lo hi vid bodyBlk ->
            let lo' = rewriteExp cs siteIdx act site lo
                hi' = rewriteExp cs siteIdx act site hi
                sB = case blockChildren of
                  [a] -> a
                  _ -> error "Janus.CSE.Optimize: StmRange scope mismatch"
                (_, bodyBlk') = emitBlockCSE cs siteIdx act sB bodyBlk
             in (act, ItemStm (StmRange lo' hi' vid bodyBlk'))
          StmSwitch tag cases defBlk ->
            let tag' = rewriteExp cs siteIdx act site tag
             in case splitAt (length cases) blockChildren of
                  (caseScs, [sDef]) | length caseScs == length cases ->
                    let cases' =
                          zipWith
                            (\sC (k, b) -> (k, snd (emitBlockCSE cs siteIdx act sC b)))
                            caseScs
                            cases
                        (_, defBlk') = emitBlockCSE cs siteIdx act sDef defBlk
                     in (act, ItemStm (StmSwitch tag' cases' defBlk'))
                  _ -> error "Janus.CSE.Optimize: StmSwitch scope mismatch"
          StmCUDAAsk ti -> (act, ItemStm (StmCUDAAsk ti))
          StmFixCall vid mfd args ->
            (act, ItemStm (StmFixCall vid mfd (rewriteHArgs cs siteIdx act site args)))
        ItemWithRefPtr ref vid innerBlk ->
          let sInner = case blockChildren of
                [a] -> a
                _ -> error "Janus.CSE.Optimize: ItemWithRefPtr scope mismatch"
              -- Bindings emitted inside `ItemWithRefPtr` remain live-out in `actAfter`!
              (actAfter, innerBlk') = emitBlockCSE cs siteIdx act sInner innerBlk
           in (actAfter, ItemWithRefPtr ref vid innerBlk')
        ItemWithString s vid innerBlk ->
          let sInner = case blockChildren of
                [a] -> a
                _ -> error "Janus.CSE.Optimize: ItemWithString scope mismatch"
              (actAfter, innerBlk') = emitBlockCSE cs siteIdx act sInner innerBlk
           in (actAfter, ItemWithString s vid innerBlk')
        ItemOptimalGang tr b2 b4 b8 b16 b32 ->
          case blockChildren of
            [s2, s4, s8, s16, s32] ->
              let (_, b2') = emitBlockCSE cs siteIdx act s2 b2
                  (_, b4') = emitBlockCSE cs siteIdx act s4 b4
                  (_, b8') = emitBlockCSE cs siteIdx act s8 b8
                  (_, b16') = emitBlockCSE cs siteIdx act s16 b16
                  (_, b32') = emitBlockCSE cs siteIdx act s32 b32
               in (act, ItemOptimalGang tr b2' b4' b8' b16' b32')
            _ -> error "Janus.CSE.Optimize: ItemOptimalGang scope mismatch"

rewriteHArgs ::
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  UseSite ->
  HArgs (CSEExp e) ts ->
  HArgs (CSEExp e) ts
rewriteHArgs _ _ _ _ HNil = HNil
rewriteHArgs cs siteIdx act site (HCons x xs) =
  HCons (rewriteExp cs siteIdx act site x) (rewriteHArgs cs siteIdx act site xs)

-- | Rewrite a pure scope (`SKPureBindable`), wrapping any bindings committed to
-- `(pureScopeId, 0)` around the rewritten expression as `ExpLetNode`s.
emitPureScopeCSE ::
  forall e a.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  ScopeId ->
  CSEExp e a ->
  CSEExp e a
emitPureScopeCSE cs siteIdx act0 sid body =
  let site = UseSite sid 0
      binds = Map.findWithDefault [] site siteIdx
      dagEntries = bsDagEntries (csBuild cs)
   in case emBind (expMeta body) of
        Nothing -> rewriteExp cs siteIdx act0 site body
        Just (trBody, capBody) ->
          let step (act, wrappers) (did, vk) =
                case dagEntries IntMap.! did of
                  DagEntry {deId = dId, deDigest = dig, deTypeRep = trV, deBindCap = capV, deShape = shape} ->
                    let vid = VarId vk trV (Just capV)
                        rhs' = rewriteDagShape cs siteIdx act site dId trV capV shape
                        act' = Map.insert dig (SomeVarId vid) act
                        wrap inner = mkExpCSELet vid capV trBody capBody rhs' inner
                     in (act', wrappers . wrap)
              (actFinal, wrapAll) = foldl step (act0, id) binds
              body' = rewriteExp cs siteIdx actFinal site body
           in wrapAll body'

-- | Rewrite an expression, replacing it with an `ExpVar` if its digest is
-- already bound in `activeBindings`, or rewriting its immediate structure.
rewriteExp ::
  forall e a.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  UseSite ->
  CSEExp e a ->
  CSEExp e a
rewriteExp cs siteIdx act site expr =
  let meta = expMeta expr
   in case emBind meta of
        Just (tr, _)
          | not (emIsTrivial meta),
            Just (SomeVarId vid) <- Map.lookup (emDigest meta) act,
            Just Refl <- testEquality tr (vidTypeRep vid) ->
              mkExpVar vid
        _ -> rewriteImmediate cs siteIdx act site expr

rewriteDagShape ::
  forall e a.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  UseSite ->
  DagId ->
  TR.TypeRep a ->
  BindCap e a ->
  DagNodeShape e a ->
  CSEExp e a
rewriteDagShape cs siteIdx act site did tr cap shape =
  case shape of
    ShapeRegular origExp _ -> rewriteImmediate cs siteIdx act site origExp
    ShapeCond c _ t _ f _ eval ->
      let c' = rewriteExp cs siteIdx act site c
       in case Map.lookup (did, site) (bsCondSiteBranches (csBuild cs)) of
            Just (CondBranchScopes sT sF) ->
              let t' = emitPureScopeCSE cs siteIdx act sT t
                  f' = emitPureScopeCSE cs siteIdx act sF f
               in mkExpCond tr cap c' t' f' eval
            Nothing ->
              mkExpCond tr cap c' (rewriteExp cs siteIdx act site t) (rewriteExp cs siteIdx act site f) eval
    ShapeShortCircuit op lhs _ rhs _ eval ->
      let lhs' = rewriteExp cs siteIdx act site lhs
          rhsSite = case Map.lookup (did, site) (bsSCSiteRHS (csBuild cs)) of
            Just sR -> UseSite sR 0
            Nothing -> site
          rhs' = rewriteExp cs siteIdx act rhsSite rhs
       in mkExpShortCircuit op lhs' rhs' eval

rewriteImmediate ::
  forall e a.
  CommitState e ->
  SiteBindIndex e ->
  ActiveMap e ->
  UseSite ->
  CSEExp e a ->
  CSEExp e a
rewriteImmediate cs siteIdx act site expr =
  let bSt = csBuild cs
      meta = expMeta expr
   in case expr of
        ExpVar {} -> expr
        ExpLit {} -> expr
        ExpOp _ tag imms canTrap zeroCost args eval ->
          let args' = rewriteHArgs cs siteIdx act site args
           in if zeroCost
                then mkExpZeroCostOp tag imms (emBind meta) args' eval
                else mkExpOp tag imms canTrap (emBind meta) args' eval
        ExpListOp _ tag imms xs eval ->
          mkExpListOp tag imms (emBind meta) (map (rewriteExp cs siteIdx act site) xs) eval
        ExpShortCircuit _ op lhs rhs eval ->
          let lhs' = rewriteExp cs siteIdx act site lhs
              mbDid = Map.lookup (emDigest meta) (bsDigToDag bSt)
              rhsSite = case mbDid >>= \d -> Map.lookup (d, site) (bsSCSiteRHS bSt) of
                Just sR -> UseSite sR 0
                Nothing -> site
              rhs' = rewriteExp cs siteIdx act rhsSite rhs
           in mkExpShortCircuit op lhs' rhs' eval
        ExpCondNode _ c t f eval ->
          case emBind meta of
            Nothing -> expr
            Just (tr, cap) ->
              let c' = rewriteExp cs siteIdx act site c
                  mbDid = Map.lookup (emDigest meta) (bsDigToDag bSt)
               in case mbDid >>= \d -> Map.lookup (d, site) (bsCondSiteBranches bSt) of
                    Just (CondBranchScopes sT sF) ->
                      let t' = emitPureScopeCSE cs siteIdx act sT t
                          f' = emitPureScopeCSE cs siteIdx act sF f
                       in mkExpCond tr cap c' t' f' eval
                    Nothing ->
                      mkExpCond
                        tr
                        cap
                        c'
                        (rewriteExp cs siteIdx act site t)
                        (rewriteExp cs siteIdx act site f)
                        eval
        ExpLetNode _ vid capA capB rhs body ->
          case emBind meta of
            Just (trB, _) ->
              mkExpCSELet
                vid
                capA
                trB
                capB
                (rewriteExp cs siteIdx act site rhs)
                (rewriteExp cs siteIdx act site body)
            Nothing -> expr
        ExpFixCall _ pfd args ->
          mkExpFixCall pfd (rewriteHArgs cs siteIdx act site args)

--------------------------------------------------------------------------------
-- Post-Pass: Trivial `StmLetM` Copy Propagation & Duplicate `StmCUDAAsk` Cleanup
--------------------------------------------------------------------------------

type CopyMap e = Map.Map VarKey (SomeCSEExp e)

substExp :: forall e a. CopyMap e -> CSEExp e a -> CSEExp e a
substExp sub expr
  | Map.null sub = expr
  | Set.disjoint (emFreeVars (expMeta expr)) (Map.keysSet sub) = expr
  | otherwise =
      let meta = expMeta expr
       in case expr of
            ExpVar _ (VarId k tr _) ->
              case Map.lookup k sub of
                Just (SomeCSEExp trSub eSub)
                  | Just Refl <- testEquality tr trSub -> eSub
                _ -> expr
            ExpLit {} -> expr
            ExpOp _ tag imms canTrap zeroCost args eval ->
              let args' = runIdentity (mapHArgsM (Identity . substExp sub) args)
               in if zeroCost
                    then mkExpZeroCostOp tag imms (emBind meta) args' eval
                    else mkExpOp tag imms canTrap (emBind meta) args' eval
            ExpListOp _ tag imms xs eval ->
              mkExpListOp tag imms (emBind meta) (map (substExp sub) xs) eval
            ExpShortCircuit _ op lhs rhs eval ->
              mkExpShortCircuit op (substExp sub lhs) (substExp sub rhs) eval
            ExpCondNode _ c t f eval ->
              case emBind meta of
                Just (tr, cap) -> mkExpCond tr cap (substExp sub c) (substExp sub t) (substExp sub f) eval
                Nothing -> expr
            ExpLetNode _ vid@(VarId k _ _) capA capB rhs body ->
              case emBind meta of
                Just (trB, _) ->
                  mkExpCSELet vid capA trB capB (substExp sub rhs) (substExp (Map.delete k sub) body)
                Nothing -> expr
            ExpFixCall _ pfd args ->
              mkExpFixCall pfd (runIdentity (mapHArgsM (Identity . substExp sub) args))

copyPropBlock :: CopyMap e -> Bool -> Block m e -> (CopyMap e, Bool, Block m e)
copyPropBlock sub0 cuda0 blk =
  let (subFinal, cudaFinal, acc) = foldl step (sub0, cuda0, Seq.empty) (toList blk)
   in (subFinal, cudaFinal, acc)
  where
    step (sub, cudaSeen, out) item = case item of
      ItemStm stm -> case stm of
        StmLetM vid@(VarId k tr _) cap rhs ->
          let rhs' = substExp sub rhs
           in case rhs' of
                ExpVar {} -> (Map.insert k (SomeCSEExp tr rhs') sub, cudaSeen, out)
                ExpLit {} -> (Map.insert k (SomeCSEExp tr rhs') sub, cudaSeen, out)
                _ -> (sub, cudaSeen, out Seq.|> ItemStm (StmLetM vid cap rhs'))
        StmEffectVal vid args eval ->
          let args' = runIdentity (mapHArgsM (Identity . substExp sub) args)
           in (sub, cudaSeen, out Seq.|> ItemStm (StmEffectVal vid args' eval))
        StmEffectUnit args eval ->
          let args' = runIdentity (mapHArgsM (Identity . substExp sub) args)
           in (sub, cudaSeen, out Seq.|> ItemStm (StmEffectUnit args' eval))
        StmNewRef ref mbInit ->
          (sub, cudaSeen, out Seq.|> ItemStm (StmNewRef ref (fmap (substExp sub) mbInit)))
        StmReadRef vid ref ->
          (sub, cudaSeen, out Seq.|> ItemStm (StmReadRef vid ref))
        StmWriteRef ref val ->
          (sub, cudaSeen, out Seq.|> ItemStm (StmWriteRef ref (substExp sub val)))
        StmIf cond tBlk fBlk ->
          let (_, _, tBlk') = copyPropBlock sub cudaSeen tBlk
              (_, _, fBlk') = copyPropBlock sub cudaSeen fBlk
           in (sub, cudaSeen, out Seq.|> ItemStm (StmIf (substExp sub cond) tBlk' fBlk'))
        StmWhile cond ref bodyBlk ->
          let (_, _, bodyBlk') = copyPropBlock sub cudaSeen bodyBlk
           in (sub, cudaSeen, out Seq.|> ItemStm (StmWhile (substExp sub cond) ref bodyBlk'))
        StmRange lo hi vid bodyBlk ->
          let (_, _, bodyBlk') = copyPropBlock sub cudaSeen bodyBlk
           in (sub, cudaSeen, out Seq.|> ItemStm (StmRange (substExp sub lo) (substExp sub hi) vid bodyBlk'))
        StmSwitch tag cases defBlk ->
          let cases' = [(k, thd3 (copyPropBlock sub cudaSeen b)) | (k, b) <- cases]
              (_, _, defBlk') = copyPropBlock sub cudaSeen defBlk
           in (sub, cudaSeen, out Seq.|> ItemStm (StmSwitch (substExp sub tag) cases' defBlk'))
        StmCUDAAsk ti
          | cudaSeen -> (sub, True, out)
          | otherwise -> (sub, True, out Seq.|> ItemStm (StmCUDAAsk ti))
        StmFixCall vid mfd args ->
          let args' = runIdentity (mapHArgsM (Identity . substExp sub) args)
           in (sub, cudaSeen, out Seq.|> ItemStm (StmFixCall vid mfd args'))
      ItemWithRefPtr ref vid innerBlk ->
        let (subAfter, cudaAfter, innerBlk') = copyPropBlock sub cudaSeen innerBlk
         in (subAfter, cudaAfter, out Seq.|> ItemWithRefPtr ref vid innerBlk')
      ItemWithString s vid innerBlk ->
        let (subAfter, cudaAfter, innerBlk') = copyPropBlock sub cudaSeen innerBlk
         in (subAfter, cudaAfter, out Seq.|> ItemWithString s vid innerBlk')
      ItemOptimalGang tr b2 b4 b8 b16 b32 ->
        let b2' = thd3 (copyPropBlock sub cudaSeen b2)
            b4' = thd3 (copyPropBlock sub cudaSeen b4)
            b8' = thd3 (copyPropBlock sub cudaSeen b8)
            b16' = thd3 (copyPropBlock sub cudaSeen b16)
            b32' = thd3 (copyPropBlock sub cudaSeen b32)
         in (sub, cudaSeen, out Seq.|> ItemOptimalGang tr b2' b4' b8' b16' b32')

    thd3 (_, _, z) = z

--------------------------------------------------------------------------------
-- Public Expression & Monadic CSE Entrypoints
--------------------------------------------------------------------------------

-- | Eliminate common subexpressions inside a standalone pure 'CSEExp'
-- expression, inserting pure 'ExpLetNode' ('Janus.Expression.Let.let_') bindings
-- at dominating scopes.
cseExp :: forall e a. CSEExp e a -> CSEExp e a
cseExp raw =
  case normalizeExp raw of
    ExpOp meta tag imms False True (HCons inner HNil) eval ->
      mkExpZeroCostOp tag imms (emBind meta) (HCons (cseExp inner) HNil) eval
    norm ->
      let rootKind = case emBind (expMeta norm) of
            Just _ -> SKPureBindable
            Nothing -> SKPureNoBind
          bSt = execState (registerExpAt (UseSite 0 0) norm) (initBuildState rootKind)
          cs = runReverseTopoCommit bSt
          siteIdx = buildSiteBindIndex cs
       in emitPureScopeCSE cs siteIdx Map.empty 0 norm

-- | Eliminate common subexpressions across a 'ProgTree' whose return value @r@
-- exposes its 'CSEExp' leaves via the supplied rank-2 traversal.
cseProgTreeWith ::
  forall m e r.
  (forall f. (Applicative f) => (forall a. CSEExp e a -> f (CSEExp e a)) -> r -> f r) ->
  ProgTree m e r ->
  ProgTree m e r
cseProgTreeWith trav = goTree
  where
    goTree :: ProgTree m e r -> ProgTree m e r
    goTree = \case
      ProgLinear rawBlk rawRet ->
        let normBlk = normalizeBlock rawBlk
            normRet = runIdentity (trav (Identity . normalizeExp) rawRet)
            retIdx = Seq.length normBlk
            retSite = UseSite 0 retIdx
            registerRetRoots =
              () <$ trav (\e -> registerExpAt retSite e >> pure e) normRet
            bSt =
              execState
                (registerBlock 0 normBlk >> registerRetRoots)
                (initBuildState SKBlock)
            cs = runReverseTopoCommit bSt
            siteIdx = buildSiteBindIndex cs
            (actFinal, cseBlk) = emitBlockCSE cs siteIdx Map.empty 0 normBlk
            cseRet = runIdentity (trav (Identity . rewriteExp cs siteIdx actFinal retSite) normRet)
            (copyMap, _, cleanBlk) = copyPropBlock Map.empty False cseBlk
            cleanRet = runIdentity (trav (Identity . substExp copyMap) cseRet)
         in ProgLinear cleanBlk cleanRet
      ProgOptimalGang tr b2 b4 b8 b16 b32 ->
        ProgOptimalGang
          tr
          (goTree b2)
          (goTree b4)
          (goTree b8)
          (goTree b16)
          (goTree b32)

-- | Eliminate common subexpressions across a monadic 'CSEM' computation,
-- sharing pure expressions across effectful commands via 'StmLetM' ('letM').
cseM :: forall m e res. (CSEReturn e res) => CSEM m e res -> CSEM m e res
cseM (CSEM buildFn) =
  let (emptyNext, emptyRaw) = buildFn DHM.empty 0
      emptyOpt = cseProgTreeWith (traverseCSEReturn @e @res) emptyRaw
   in CSEM $ \tfm baseCounter ->
        if DHM.null tfm
          then
            emptyOpt `seq`
              if baseCounter == 0 && maxCSEVarInTree emptyOpt < 0
                then (emptyNext, emptyOpt)
                else instantiateOptTree emptyParamSubst baseCounter emptyNext emptyOpt
          else
            let (nextCounter, rawTree) = buildFn tfm 0
                !optTree = cseProgTreeWith (traverseCSEReturn @e @res) rawTree
             in if baseCounter == 0 && maxCSEVarInTree optTree < 0
                  then (nextCounter, optTree)
                  else instantiateOptTree emptyParamSubst baseCounter nextCounter optTree

-- | Eliminate common subexpressions across a monadic 'CSEM' computation under
-- a known 'TargetFeatureMap'.
cseMWithFeatures ::
  forall m e res.
  (CSEReturn e res) =>
  TargetFeatureMap ->
  CSEM m e res ->
  CSEM m e res
cseMWithFeatures tfm mAct
  | DHM.null tfm = cseM mAct
  | otherwise =
      case mAct of
        CSEM buildFn ->
          let (nextCounter, rawTree) = buildFn tfm 0
              !optTree = cseProgTreeWith (traverseCSEReturn @e @res) rawTree
           in CSEM $ \_ baseCounter ->
                if baseCounter == 0 && maxCSEVarInTree optTree < 0
                  then (nextCounter, optTree)
                  else instantiateOptTree emptyParamSubst baseCounter nextCounter optTree

maxCSEVarInExp :: CSEExp e a -> Int
maxCSEVarInExp expr =
  foldl
    (\acc vk -> case vk of VarCSE i -> max acc i; _ -> acc)
    (-1)
    (Set.toList (emFreeVars (expMeta expr)))

maxCSEVarInTree :: forall m e r. (CSEReturn e r) => ProgTree m e r -> Int
maxCSEVarInTree = \case
  ProgLinear blk r ->
    let mBlk = foldl max (-1) (fmap maxCSEItem (toList blk))
        mRet = getConst (traverseCSEReturn @e @r (\e -> Const [maxCSEVarInExp e]) r)
     in foldl max mBlk mRet
  ProgOptimalGang _ b2 b4 b8 b16 b32 ->
    maximum (map maxCSEVarInTree [b2, b4, b8, b16, b32])
  where
    maxCSEBlk b = foldl max (-1) (fmap maxCSEItem (toList b))
    maxCSEHArgs :: HArgs (CSEExp e) ts -> Int
    maxCSEHArgs HNil = -1
    maxCSEHArgs (HCons x xs) = max (maxCSEVarInExp x) (maxCSEHArgs xs)
    maxCSEItem = \case
      ItemStm s -> case s of
        StmLetM (VarId vk _ _) _ rhs ->
          max (case vk of VarCSE i -> i; _ -> -1) (maxCSEVarInExp rhs)
        StmEffectVal _ args _ -> maxCSEHArgs args
        StmEffectUnit args _ -> maxCSEHArgs args
        StmNewRef _ mb -> maybe (-1) maxCSEVarInExp mb
        StmReadRef _ _ -> -1
        StmWriteRef _ val -> maxCSEVarInExp val
        StmIf c t f -> maximum [maxCSEVarInExp c, maxCSEBlk t, maxCSEBlk f]
        StmWhile c _ b -> max (maxCSEVarInExp c) (maxCSEBlk b)
        StmRange l u _ b -> maximum [maxCSEVarInExp l, maxCSEVarInExp u, maxCSEBlk b]
        StmSwitch tag cs defB ->
          maximum (maxCSEVarInExp tag : maxCSEBlk defB : [maxCSEBlk b | (_, b) <- cs])
        StmCUDAAsk _ -> -1
        StmFixCall _ _ args -> maxCSEHArgs args
      ItemWithRefPtr _ _ b -> maxCSEBlk b
      ItemWithString _ _ b -> maxCSEBlk b
      ItemOptimalGang _ b2 b4 b8 b16 b32 ->
        maximum [maxCSEBlk b2, maxCSEBlk b4, maxCSEBlk b8, maxCSEBlk b16, maxCSEBlk b32]

-- | Instantiate an optimized `ProgTree` inside an outer `CSEM` context (applying
-- any parameter substitutions and remapping local `VarSSA` / `VarCSE` / `RefLocal`
-- IDs when `baseCounter > 0` or `ParamSubst` is non-identity).
instantiateOptTree ::
  forall m e r.
  (CSEReturn e r) =>
  ParamSubst e ->
  Int ->
  Int ->
  ProgTree m e r ->
  (Int, ProgTree m e r)
instantiateOptTree sub baseCounter countUsed tree
  | isIdentityParamSubst sub && baseCounter == 0 = (countUsed, tree)
  | otherwise =
      let maxCSE = maxCSEVarInTree tree
          cseCount = maxCSE + 1
          paramStart = baseCounter + countUsed + cseCount
          (afterParams, paramMap, paramItems) = buildParamBindings paramStart (psExpList sub)
          remapKey = \case
            VarSSA i -> VarSSA (i + baseCounter)
            VarCSE i -> VarSSA (baseCounter + countUsed + i)
            other -> other
          remapRefKey rk = Map.findWithDefault (shiftRK rk) rk (psRefMap sub)
          shiftRK (RefLocal i) = RefLocal (i + baseCounter)
          shiftRK other = other
          remapRef :: forall x. CSERef m e x -> CSERef m e x
          remapRef (CSERef rk tr b) = CSERef (remapRefKey rk) tr b
          remapVid :: forall x. VarId e x -> VarId e x
          remapVid (VarId k tr b) = VarId (remapKey k) tr b

          remapExp :: forall x. CSEExp e x -> CSEExp e x
          remapExp e =
            let meta = expMeta e
             in case e of
                  ExpVar _ vid@(VarId k tr _) ->
                    case Map.lookup k paramMap of
                      Just (SomeCSEExp trSub eSub)
                        | Just Refl <- testEquality tr trSub -> eSub
                      _ -> mkExpVar (remapVid vid)
                  ExpLit {} -> e
                  ExpOp _ tag imms canTrap zeroCost args eval ->
                    let args' = runIdentity (mapHArgsM (Identity . remapExp) args)
                     in if zeroCost
                          then mkExpZeroCostOp tag imms (emBind meta) args' eval
                          else mkExpOp tag imms canTrap (emBind meta) args' eval
                  ExpListOp _ tag imms xs eval ->
                    mkExpListOp tag imms (emBind meta) (map remapExp xs) eval
                  ExpShortCircuit _ op l r eval ->
                    mkExpShortCircuit op (remapExp l) (remapExp r) eval
                  ExpCondNode _ c t f eval ->
                    case emBind meta of
                      Just (tr, cap) -> mkExpCond tr cap (remapExp c) (remapExp t) (remapExp f) eval
                      Nothing -> e
                  ExpLetNode _ vid capA capB rhs body ->
                    case emBind meta of
                      Just (trB, _) ->
                        mkExpCSELet (remapVid vid) capA trB capB (remapExp rhs) (remapExp body)
                      Nothing -> e
                  ExpFixCall _ pfd args ->
                    mkExpFixCall pfd (runIdentity (mapHArgsM (Identity . remapExp) args))

          remapBlk = fmap remapItem
          remapItem = \case
            ItemStm s -> ItemStm (remapStm s)
            ItemWithRefPtr ref vid b -> ItemWithRefPtr (remapRef ref) (remapVid vid) (remapBlk b)
            ItemWithString str vid b -> ItemWithString str (remapVid vid) (remapBlk b)
            ItemOptimalGang tr b2 b4 b8 b16 b32 ->
              ItemOptimalGang tr (remapBlk b2) (remapBlk b4) (remapBlk b8) (remapBlk b16) (remapBlk b32)
          remapStm = \case
            StmLetM vid cap rhs -> StmLetM (remapVid vid) cap (remapExp rhs)
            StmEffectVal vid args eval ->
              StmEffectVal (remapVid vid) (runIdentity (mapHArgsM (Identity . remapExp) args)) eval
            StmEffectUnit args eval ->
              StmEffectUnit (runIdentity (mapHArgsM (Identity . remapExp) args)) eval
            StmNewRef ref mb -> StmNewRef (remapRef ref) (fmap remapExp mb)
            StmReadRef vid ref -> StmReadRef (remapVid vid) (remapRef ref)
            StmWriteRef ref val -> StmWriteRef (remapRef ref) (remapExp val)
            StmIf c t f -> StmIf (remapExp c) (remapBlk t) (remapBlk f)
            StmWhile c ref b -> StmWhile (remapExp c) (remapRef ref) (remapBlk b)
            StmRange l u vid b -> StmRange (remapExp l) (remapExp u) (remapVid vid) (remapBlk b)
            StmSwitch tag cs defB -> StmSwitch (remapExp tag) [(k, remapBlk b) | (k, b) <- cs] (remapBlk defB)
            StmCUDAAsk ti -> StmCUDAAsk (mapCUDAThreadInfo remapVid ti)
            StmFixCall vids mfd args ->
              StmFixCall
                (runIdentity (mapHArgsM (Identity . remapVid) vids))
                mfd
                (runIdentity (mapHArgsM (Identity . remapExp) args))

          remapTree :: ProgTree m e r -> ProgTree m e r
          remapTree = \case
            ProgLinear b r ->
              ProgLinear (remapBlk b) (runIdentity (traverseCSEReturn @e @r (Identity . remapExp) r))
            ProgOptimalGang tr b2 b4 b8 b16 b32 ->
              ProgOptimalGang tr (remapTree b2) (remapTree b4) (remapTree b8) (remapTree b16) (remapTree b32)
       in (afterParams, prependItems paramItems (remapTree tree))
  where
    buildParamBindings ::
      Int ->
      [(VarKey, ParamBindEntry e)] ->
      (Int, CopyMap e, Block m e)
    buildParamBindings n0 = foldl stepP (n0, Map.empty, Seq.empty)
      where
        stepP (n, mp, items) (vk, ParamBindEntry tr cap actExp) =
          if emIsTrivial (expMeta actExp)
            then (n, Map.insert vk (SomeCSEExp tr actExp) mp, items)
            else
              let vid = VarId (VarSSA n) tr (Just cap)
               in ( n + 1,
                    Map.insert vk (SomeCSEExp tr (mkExpVar vid)) mp,
                    items Seq.|> ItemStm (StmLetM vid cap actExp)
                  )

--------------------------------------------------------------------------------
-- Higher-Kinded Records (`CSEHKD`), `CSEReturn`, `CSEParam`, & `CSEOptimize`
--------------------------------------------------------------------------------

data ParamBindEntry e where
  ParamBindEntry ::
    !(TR.TypeRep a) ->
    !(BindCap e a) ->
    !(CSEExp e a) ->
    ParamBindEntry e

data ParamSubst e = ParamSubst
  { psExpList :: ![(VarKey, ParamBindEntry e)],
    psRefMap :: !(Map.Map RefKey RefKey)
  }

emptyParamSubst :: ParamSubst e
emptyParamSubst = ParamSubst [] Map.empty

isIdentityParamSubst :: ParamSubst e -> Bool
isIdentityParamSubst (ParamSubst es rs) = null es && Map.null rs

addExpParamSubst ::
  VarKey ->
  TR.TypeRep a ->
  BindCap e a ->
  CSEExp e a ->
  ParamSubst e ->
  ParamSubst e
addExpParamSubst vk tr cap actual sub =
  case actual of
    ExpVar _ (VarId vkActual _ _) | vkActual == vk -> sub
    _ -> sub {psExpList = psExpList sub <> [(vk, ParamBindEntry tr cap actual)]}

addRefParamSubst :: RefKey -> RefKey -> ParamSubst e -> ParamSubst e
addRefParamSubst rkFormal rkActual sub
  | rkFormal == rkActual = sub
  | otherwise = sub {psRefMap = Map.insert rkFormal rkActual (psRefMap sub)}

-- | Generic traversal, parameter allocation, and target projection for HKD
-- records @d ('CSEExp' e) <-> d e@. Automatically satisfied by any 'Generic'
-- HKD record whose fields are 'CSETyped' in @e@ (including nested HKD records).
class CSEHKD (d :: (K.Type -> K.Type) -> K.Type) (e :: K.Type -> K.Type) where
  traverseCSEHKD ::
    (Applicative f) =>
    (forall a. CSEExp e a -> f (CSEExp e a)) ->
    d (CSEExp e) ->
    f (d (CSEExp e))
  mapCSEHKDToTarget ::
    (forall a. CSEExp e a -> e a) ->
    d (CSEExp e) ->
    d e
  allocFormalCSEHKD ::
    Int ->
    (Int, d (CSEExp e), d (CSEExp e) -> ParamSubst e -> ParamSubst e)
  bindTargetCSEHKD ::
    Int ->
    d e ->
    (Int, [(VarKey, SomeTargetVal e)])

instance
  (Generic (d (CSEExp e)), Generic (d e), GCSEHKD e (Rep (d (CSEExp e))) (Rep (d e))) =>
  CSEHKD d e
  where
  traverseCSEHKD fn x = to <$> gTraverseCSEHKD @e @(Rep (d (CSEExp e))) @(Rep (d e)) fn (from x)
  mapCSEHKDToTarget fn x = to (gMapCSEHKD @e @(Rep (d (CSEExp e))) @(Rep (d e)) fn (from x))
  allocFormalCSEHKD n0 =
    let (n1, repFormal, bindRep) = gAllocFormalHKD @e @(Rep (d (CSEExp e))) @(Rep (d e)) n0
     in (n1, to repFormal, bindRep . from)
  bindTargetCSEHKD n0 x = gBindTargetHKD @e @(Rep (d (CSEExp e))) @(Rep (d e)) n0 (from x)

class GCSEHKD (e :: K.Type -> K.Type) (repCSE :: K.Type -> K.Type) (repTgt :: K.Type -> K.Type) where
  gTraverseCSEHKD :: (Applicative f) => (forall a. CSEExp e a -> f (CSEExp e a)) -> repCSE p -> f (repCSE p)
  gMapCSEHKD :: (forall a. CSEExp e a -> e a) -> repCSE p -> repTgt p
  gAllocFormalHKD :: Int -> (Int, repCSE p, repCSE p -> ParamSubst e -> ParamSubst e)
  gBindTargetHKD :: Int -> repTgt p -> (Int, [(VarKey, SomeTargetVal e)])

instance (GCSEHKD e fCSE fTgt) => GCSEHKD e (M1 i c fCSE) (M1 i c fTgt) where
  gTraverseCSEHKD fn (M1 x) = M1 <$> gTraverseCSEHKD @e @fCSE @fTgt fn x
  gMapCSEHKD fn (M1 x) = M1 (gMapCSEHKD @e @fCSE @fTgt fn x)
  gAllocFormalHKD n0 =
    let (n1, f, b) = gAllocFormalHKD @e @fCSE @fTgt n0
     in (n1, M1 f, \(M1 x) -> b x)
  gBindTargetHKD n0 (M1 x) = gBindTargetHKD @e @fCSE @fTgt n0 x

instance
  (GCSEHKD e fCSE fTgt, GCSEHKD e gCSE gTgt) =>
  GCSEHKD e (fCSE :*: gCSE) (fTgt :*: gTgt)
  where
  gTraverseCSEHKD fn (fx :*: gx) =
    (:*:) <$> gTraverseCSEHKD @e @fCSE @fTgt fn fx <*> gTraverseCSEHKD @e @gCSE @gTgt fn gx
  gMapCSEHKD fn (fx :*: gx) =
    gMapCSEHKD @e @fCSE @fTgt fn fx :*: gMapCSEHKD @e @gCSE @gTgt fn gx
  gAllocFormalHKD n0 =
    let (n1, ff, bf) = gAllocFormalHKD @e @fCSE @fTgt n0
        (n2, fg, bg) = gAllocFormalHKD @e @gCSE @gTgt n1
     in (n2, ff :*: fg, \(fx :*: gx) -> bg gx . bf fx)
  gBindTargetHKD n0 (fx :*: gx) =
    let (n1, xs) = gBindTargetHKD @e @fCSE @fTgt n0 fx
        (n2, ys) = gBindTargetHKD @e @gCSE @gTgt n1 gx
     in (n2, xs <> ys)

instance (CSETyped e a) => GCSEHKD e (K1 i (CSEExp e a)) (K1 i (e a)) where
  gTraverseCSEHKD fn (K1 x) = K1 <$> fn x
  gMapCSEHKD fn (K1 x) = K1 (fn x)
  gAllocFormalHKD n =
    let tr = TR.typeRep @a
        cap = BindDirect @e @a
        vk = VarParam n
        vid = VarId vk tr (Just cap)
     in (n + 1, K1 (mkExpVar vid), \(K1 act) -> addExpParamSubst vk tr cap act)
  gBindTargetHKD n (K1 val) =
    (n + 1, [(VarParam n, SomeTargetVal (TR.typeRep @a) val)])

instance (CSEHKD sub e) => GCSEHKD e (K1 i (sub (CSEExp e))) (K1 i (sub e)) where
  gTraverseCSEHKD fn (K1 s) = K1 <$> traverseCSEHKD fn s
  gMapCSEHKD fn (K1 s) = K1 (mapCSEHKDToTarget fn s)
  gAllocFormalHKD n0 =
    let (n1, sFormal, bindS) = allocFormalCSEHKD @sub @e n0
     in (n1, K1 sFormal, \(K1 sAct) -> bindS sAct)
  gBindTargetHKD n0 (K1 sTgt) = bindTargetCSEHKD @sub @e n0 sTgt

instance (CSETyped e (Ptr a)) => GCSEHKD e (K1 i (PtrOf (CSEExp e) a)) (K1 i (PtrOf e a)) where
  gTraverseCSEHKD fn (K1 (PtrOf p)) = K1 . PtrOf <$> fn p
  gMapCSEHKD fn (K1 (PtrOf p)) = K1 (PtrOf (fn p))
  gAllocFormalHKD n =
    let tr = TR.typeRep @(Ptr a)
        cap = BindDirect @e @(Ptr a)
        vk = VarParam n
        vid = VarId vk tr (Just cap)
     in (n + 1, K1 (PtrOf (mkExpVar vid)), \(K1 (PtrOf act)) -> addExpParamSubst vk tr cap act)
  gBindTargetHKD n (K1 (PtrOf val)) =
    (n + 1, [(VarParam n, SomeTargetVal (TR.typeRep @(Ptr a)) val)])

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEHKD e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  GCSEHKD e (K1 i (sub (PtrOf (CSEExp e)))) (K1 i (sub (PtrOf e)))
  where
  gTraverseCSEHKD fn (K1 s) =
    K1 . to <$> gTraverseCSEHKD @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) fn (from s)
  gMapCSEHKD fn (K1 s) =
    K1 (to (gMapCSEHKD @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) fn (from s)))
  gAllocFormalHKD n0 =
    let (n1, repS, bindS) = gAllocFormalHKD @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) n0
     in (n1, K1 (to repS), \(K1 sAct) -> bindS (from sAct))
  gBindTargetHKD n0 (K1 sTgt) =
    gBindTargetHKD @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) n0 (from sTgt)

-- | Map a 'CSEExp' element type to the corresponding target @e@ element type,
-- unwrapping 'SIMDVector' and 'SIMDMask' (including inside 'SIMDPair') from
-- @'CSEExp' e@ to @e@ while leaving all other types unchanged.
type family TargetExpElem (e :: K.Type -> K.Type) (a :: K.Type) :: K.Type where
  TargetExpElem e (SIMDVector (CSEExp e) n a) = SIMDVector e n a
  TargetExpElem e (SIMDMask (CSEExp e) n a) = SIMDMask e n a
  TargetExpElem e (SIMDPair x y) = SIMDPair (TargetExpElem e x) (TargetExpElem e y)
  TargetExpElem _ a = a

class (Coercible a (TargetExpElem e a)) => CSEExpReturnLeaf (e :: K.Type -> K.Type) (a :: K.Type) where
  interpretExpLeaf :: (CSETargetExp e) => e a -> e (TargetExpElem e a)

instance {-# OVERLAPPABLE #-} (TargetExpElem e a ~ a) => CSEExpReturnLeaf e a where
  interpretExpLeaf = id

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, SIMDShape n a) =>
  CSEExpReturnLeaf e (SIMDVector (CSEExp e) n a)
  where
  interpretExpLeaf = coerceExpNewtype

instance
  {-# OVERLAPPING #-}
  (ExpNewtype e, ExpSIMD e, SIMDShape n a) =>
  CSEExpReturnLeaf e (SIMDMask (CSEExp e) n a)
  where
  interpretExpLeaf = coerceExpNewtype

instance
  {-# OVERLAPPING #-}
  (CSEExpReturnLeaf e x, CSEExpReturnLeaf e y) =>
  CSEExpReturnLeaf e (SIMDPair x y)
  where
  interpretExpLeaf = coerceExpNewtype

class GCSETraverse (e :: K.Type -> K.Type) (rep :: K.Type -> K.Type) where
  gTraverseCSEReturn :: (Applicative f) => (forall a. CSEExp e a -> f (CSEExp e a)) -> rep p -> f (rep p)

instance (GCSETraverse e f) => GCSETraverse e (M1 i c f) where
  gTraverseCSEReturn fn (M1 x) = M1 <$> gTraverseCSEReturn @e @f fn x

instance (GCSETraverse e f, GCSETraverse e g) => GCSETraverse e (f :*: g) where
  gTraverseCSEReturn fn (fx :*: gx) =
    (:*:) <$> gTraverseCSEReturn @e @f fn fx <*> gTraverseCSEReturn @e @g fn gx

instance GCSETraverse e (K1 i (CSEExp e a)) where
  gTraverseCSEReturn fn (K1 x) = K1 <$> fn x

instance GCSETraverse e (K1 i (PtrOf (CSEExp e) a)) where
  gTraverseCSEReturn fn (K1 (PtrOf p)) = K1 . PtrOf <$> fn p

instance
  (Generic (sub (PtrOf (CSEExp e))), GCSETraverse e (Rep (sub (PtrOf (CSEExp e))))) =>
  GCSETraverse e (K1 i (sub (PtrOf (CSEExp e))))
  where
  gTraverseCSEReturn fn (K1 s) =
    K1 . to <$> gTraverseCSEReturn @e @(Rep (sub (PtrOf (CSEExp e)))) fn (from s)

class GCSEMap (e :: K.Type -> K.Type) (repCSE :: K.Type -> K.Type) (repTgt :: K.Type -> K.Type) where
  gMapCSEReturn :: (forall a. CSEExp e a -> e a) -> repCSE p -> repTgt p

instance (GCSEMap e fCSE fTgt) => GCSEMap e (M1 i c fCSE) (M1 i c fTgt) where
  gMapCSEReturn fn (M1 x) = M1 (gMapCSEReturn @e @fCSE @fTgt fn x)

instance (GCSEMap e fCSE fTgt, GCSEMap e gCSE gTgt) => GCSEMap e (fCSE :*: gCSE) (fTgt :*: gTgt) where
  gMapCSEReturn fn (fx :*: gx) =
    gMapCSEReturn @e @fCSE @fTgt fn fx :*: gMapCSEReturn @e @gCSE @gTgt fn gx

instance GCSEMap e (K1 i (CSEExp e a)) (K1 i (e a)) where
  gMapCSEReturn fn (K1 x) = K1 (fn x)

instance GCSEMap e (K1 i (PtrOf (CSEExp e) a)) (K1 i (PtrOf e a)) where
  gMapCSEReturn fn (K1 (PtrOf p)) = K1 (PtrOf (fn p))

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEMap e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  GCSEMap e (K1 i (sub (PtrOf (CSEExp e)))) (K1 i (sub (PtrOf e)))
  where
  gMapCSEReturn fn (K1 s) =
    K1 (to (gMapCSEReturn @e @(Rep (sub (PtrOf (CSEExp e)))) @(Rep (sub (PtrOf e))) fn (from s)))

-- | Types that can be returned from a 'CSEM' computation, exposing all
-- 'CSEExp' leaves to the enclosing block's CSE pass.
class CSEReturn (e :: K.Type -> K.Type) (res :: K.Type) where
  traverseCSEReturn ::
    (Applicative f) =>
    (forall a. CSEExp e a -> f (CSEExp e a)) ->
    res ->
    f res
  default traverseCSEReturn ::
    (Generic res, GCSETraverse e (Rep res), Applicative f) =>
    (forall a. CSEExp e a -> f (CSEExp e a)) ->
    res ->
    f res
  traverseCSEReturn fn x = to <$> gTraverseCSEReturn @e @(Rep res) fn (from x)

-- | Return types of 'CSEM' that can be lowered to target @e@ by
-- @interpretCSE@.
class (CSEReturn e res) => CSETargetReturn (e :: K.Type -> K.Type) (res :: K.Type) where
  type CSETargetVal e res :: K.Type
  interpretCSEReturn ::
    (CSETargetExp e) =>
    (forall a. CSEExp e a -> e a) ->
    res ->
    CSETargetVal e res
  default interpretCSEReturn ::
    (Generic res, Generic (CSETargetVal e res), GCSEMap e (Rep res) (Rep (CSETargetVal e res))) =>
    (forall a. CSEExp e a -> e a) ->
    res ->
    CSETargetVal e res
  interpretCSEReturn eval x =
    to (gMapCSEReturn @e @(Rep res) @(Rep (CSETargetVal e res)) eval (from x))

instance CSEReturn e (HArgs (CSEExp e) ts) where
  traverseCSEReturn = mapHArgsM

instance CSEReturn e () where
  traverseCSEReturn _ () = pure ()

instance CSETargetReturn e () where
  type CSETargetVal e () = ()
  interpretCSEReturn _ () = ()

instance CSEReturn e (CSEExp e a) where
  traverseCSEReturn fn x = fn x

instance (CSEExpReturnLeaf e a) => CSETargetReturn e (CSEExp e a) where
  type CSETargetVal e (CSEExp e a) = e (TargetExpElem e a)
  interpretCSEReturn eval x = interpretExpLeaf (eval x)

instance (CSEReturn e r1, CSEReturn e r2) => CSEReturn e (r1, r2) where
  traverseCSEReturn fn (a, b) = (,) <$> traverseCSEReturn fn a <*> traverseCSEReturn fn b

instance (CSETargetReturn e r1, CSETargetReturn e r2) => CSETargetReturn e (r1, r2) where
  type CSETargetVal e (r1, r2) = (CSETargetVal e r1, CSETargetVal e r2)
  interpretCSEReturn eval (a, b) = (interpretCSEReturn eval a, interpretCSEReturn eval b)

instance (CSEReturn e r1, CSEReturn e r2, CSEReturn e r3) => CSEReturn e (r1, r2, r3) where
  traverseCSEReturn fn (a, b, c) =
    (,,) <$> traverseCSEReturn fn a <*> traverseCSEReturn fn b <*> traverseCSEReturn fn c

instance
  (CSETargetReturn e r1, CSETargetReturn e r2, CSETargetReturn e r3) =>
  CSETargetReturn e (r1, r2, r3)
  where
  type CSETargetVal e (r1, r2, r3) = (CSETargetVal e r1, CSETargetVal e r2, CSETargetVal e r3)
  interpretCSEReturn eval (a, b, c) =
    (interpretCSEReturn eval a, interpretCSEReturn eval b, interpretCSEReturn eval c)

instance
  (CSEReturn e r1, CSEReturn e r2, CSEReturn e r3, CSEReturn e r4) =>
  CSEReturn e (r1, r2, r3, r4)
  where
  traverseCSEReturn fn (a, b, c, d) =
    (,,,)
      <$> traverseCSEReturn fn a
      <*> traverseCSEReturn fn b
      <*> traverseCSEReturn fn c
      <*> traverseCSEReturn fn d

instance
  (CSETargetReturn e r1, CSETargetReturn e r2, CSETargetReturn e r3, CSETargetReturn e r4) =>
  CSETargetReturn e (r1, r2, r3, r4)
  where
  type
    CSETargetVal e (r1, r2, r3, r4) =
      (CSETargetVal e r1, CSETargetVal e r2, CSETargetVal e r3, CSETargetVal e r4)
  interpretCSEReturn eval (a, b, c, d) =
    ( interpretCSEReturn eval a,
      interpretCSEReturn eval b,
      interpretCSEReturn eval c,
      interpretCSEReturn eval d
    )

instance (CSEReturn e r) => CSEReturn e [r] where
  traverseCSEReturn fn = traverse (traverseCSEReturn fn)

instance (CSETargetReturn e r) => CSETargetReturn e [r] where
  type CSETargetVal e [r] = [CSETargetVal e r]
  interpretCSEReturn eval = map (interpretCSEReturn eval)

instance (CSEReturn e r) => CSEReturn e (Maybe r) where
  traverseCSEReturn fn = traverse (traverseCSEReturn fn)

instance (CSETargetReturn e r) => CSETargetReturn e (Maybe r) where
  type CSETargetVal e (Maybe r) = Maybe (CSETargetVal e r)
  interpretCSEReturn eval = fmap (interpretCSEReturn eval)

instance (CSEReturn e r1, CSEReturn e r2) => CSEReturn e (Either r1 r2) where
  traverseCSEReturn fn = \case
    Left a -> Left <$> traverseCSEReturn fn a
    Right b -> Right <$> traverseCSEReturn fn b

instance (CSETargetReturn e r1, CSETargetReturn e r2) => CSETargetReturn e (Either r1 r2) where
  type CSETargetVal e (Either r1 r2) = Either (CSETargetVal e r1) (CSETargetVal e r2)
  interpretCSEReturn eval = \case
    Left a -> Left (interpretCSEReturn eval a)
    Right b -> Right (interpretCSEReturn eval b)

instance CSEReturn e (Tic (CSEExp e)) where
  traverseCSEReturn fn (Tic t) = Tic <$> fn t

instance CSETargetReturn e (Tic (CSEExp e)) where
  type CSETargetVal e (Tic (CSEExp e)) = Tic e
  interpretCSEReturn eval (Tic t) = Tic (eval t)

instance CSEReturn e (Toc (CSEExp e)) where
  traverseCSEReturn fn (Toc t) = Toc <$> fn t

instance CSETargetReturn e (Toc (CSEExp e)) where
  type CSETargetVal e (Toc (CSEExp e)) = Toc e
  interpretCSEReturn eval (Toc t) = Toc (eval t)

instance CSEReturn e (Tensor ds (CSEExp e) a) where
  traverseCSEReturn fn (Tensor tb p) = Tensor (coerceTB tb) <$> fn p
    where
      coerceTB :: TensorBounds sh (CSEExp e) -> TensorBounds sh (CSEExp e)
      coerceTB = id

instance CSETargetReturn e (Tensor ds (CSEExp e) a) where
  type CSETargetVal e (Tensor ds (CSEExp e) a) = Tensor ds e a
  interpretCSEReturn eval (Tensor tb p) = Tensor (retagTB tb) (eval p)
    where
      retagTB :: TensorBounds sh (CSEExp e) -> TensorBounds sh e
      retagTB TensorBoundsNil = TensorBoundsNil
      retagTB (TensorBoundsCons pn rest) = TensorBoundsCons pn (retagTB rest)

instance (CSEReturn e r) => CSEReturn e (Wide k r) where
  traverseCSEReturn fn = traverse (traverseCSEReturn fn)

instance (CSETargetReturn e r) => CSETargetReturn e (Wide k r) where
  type CSETargetVal e (Wide k r) = Wide k (CSETargetVal e r)
  interpretCSEReturn eval = fmap (interpretCSEReturn eval)

instance (CSEHKD d e) => CSEReturn e (HKD d (CSEExp e)) where
  traverseCSEReturn fn (HKD x) = HKD <$> traverseCSEHKD fn x

instance (CSEHKD d e) => CSETargetReturn e (HKD d (CSEExp e)) where
  type CSETargetVal e (HKD d (CSEExp e)) = HKD d e
  interpretCSEReturn eval (HKD x) = HKD (mapCSEHKDToTarget eval x)

instance (SPMDTyped n (CSEExp e) a) => CSEReturn e (SPMD n (CSEExp e) a) where
  traverseCSEReturn fn x =
    (\v -> SPMD (pure (spmdDematerialize @n @(CSEExp e) @a v)))
      <$> fn (spmdMaterialize @n @(CSEExp e) @a (runSPMD x))

instance
  ( SPMDTyped n (CSEExp e) a,
    SPMDTyped n e a,
    CSEExpReturnLeaf e (SPMDRep n (CSEExp e) a),
    TargetExpElem e (SPMDRep n (CSEExp e) a) ~ SPMDRep n e a
  ) =>
  CSETargetReturn e (SPMD n (CSEExp e) a)
  where
  type CSETargetVal e (SPMD n (CSEExp e) a) = SPMD n e a
  interpretCSEReturn eval x =
    let rawTgt :: e (SPMDRep n e a)
        rawTgt =
          interpretExpLeaf @e @(SPMDRep n (CSEExp e) a)
            (eval (spmdMaterialize @n @(CSEExp e) @a (runSPMD x)))
     in SPMD (pure (spmdDematerialize @n @e @a rawTgt))

--------------------------------------------------------------------------------
-- Function Parameter Reification & `CSEOptimize` (`cse`)
--------------------------------------------------------------------------------

class CSEParam (p :: K.Type) where
  type CSEParamExp p :: K.Type -> K.Type
  type CSEParamTarget p :: K.Type
  cseAllocParam :: Int -> (Int, p, p -> ParamSubst (CSEParamExp p) -> ParamSubst (CSEParamExp p))
  default cseAllocParam ::
    (Generic p, Generic (CSEParamTarget p), GCSEHKD (CSEParamExp p) (Rep p) (Rep (CSEParamTarget p))) =>
    Int ->
    (Int, p, p -> ParamSubst (CSEParamExp p) -> ParamSubst (CSEParamExp p))
  cseAllocParam n0 =
    let (n1, repFormal, bindRep) =
          gAllocFormalHKD @(CSEParamExp p) @(Rep p) @(Rep (CSEParamTarget p)) n0
     in (n1, to repFormal, bindRep . from)
  cseBindTargetParam ::
    forall m.
    (CSEParamMonad p m) =>
    Int ->
    CSEParamTarget p ->
    ( Int,
      [(VarKey, SomeTargetVal (CSEParamExp p))],
      [(RefKey, SomeTargetRef m (CSEParamExp p))]
    )
  default cseBindTargetParam ::
    forall m.
    (Generic p, Generic (CSEParamTarget p), GCSEHKD (CSEParamExp p) (Rep p) (Rep (CSEParamTarget p))) =>
    Int ->
    CSEParamTarget p ->
    ( Int,
      [(VarKey, SomeTargetVal (CSEParamExp p))],
      [(RefKey, SomeTargetRef m (CSEParamExp p))]
    )
  cseBindTargetParam n0 tgt =
    let (n1, vs) =
          gBindTargetHKD @(CSEParamExp p) @(Rep p) @(Rep (CSEParamTarget p)) n0 (from tgt)
     in (n1, vs, [])

type family CSEParamMonad (p :: K.Type) (m :: K.Type -> K.Type) :: K.Constraint where
  CSEParamMonad (CSERef m1 _ _) m = (m1 ~ m)
  CSEParamMonad (p1, p2) m = (CSEParamMonad p1 m, CSEParamMonad p2 m)
  CSEParamMonad (p1, p2, p3) m = (CSEParamMonad p1 m, CSEParamMonad p2 m, CSEParamMonad p3 m)
  CSEParamMonad (Wide _ p) m = CSEParamMonad p m
  CSEParamMonad _ _ = ()

class (Coercible a (TargetExpElem e a)) => CSEExpParamLeaf (e :: K.Type -> K.Type) (a :: K.Type) where
  cseLeafBindCap :: BindCap e a
  cseAllocExpParam :: Int -> (Int, CSEExp e a, CSEExp e a -> ParamSubst e -> ParamSubst e)
  cseBindTargetExpParam :: Int -> e (TargetExpElem e a) -> (Int, [(VarKey, SomeTargetVal e)])

instance {-# OVERLAPPABLE #-} (CSETyped e a, TargetExpElem e a ~ a) => CSEExpParamLeaf e a where
  cseLeafBindCap = BindDirect @e @a
  cseAllocExpParam n =
    let tr = TR.typeRep @a
        cap = BindDirect @e @a
        vk = VarParam n
        vid = VarId vk tr (Just cap)
     in (n + 1, mkExpVar vid, \actual -> addExpParamSubst vk tr cap actual)
  cseBindTargetExpParam n val =
    (n + 1, [(VarParam n, SomeTargetVal (TR.typeRep @a) val)])

instance {-# OVERLAPPING #-} CSEExpParamLeaf e (Ptr ()) where
  cseLeafBindCap = BindVoidPtr @e
  cseAllocExpParam n =
    let tr = TR.typeRep @(Ptr ())
        cap = BindVoidPtr @e
        vk = VarParam n
        vid = VarId vk tr (Just cap)
     in (n + 1, mkExpVar vid, \actual -> addExpParamSubst vk tr cap actual)
  cseBindTargetExpParam n val =
    (n + 1, [(VarParam n, SomeTargetVal (TR.typeRep @(Ptr ())) val)])

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, ExpNewtype e, SIMDShape n a, Typeable e) =>
  CSEExpParamLeaf e (SIMDVector (CSEExp e) n a)
  where
  cseLeafBindCap = BindSIMDVec @e @n @a
  cseAllocExpParam k =
    let tr = TR.typeRep @(SIMDVector (CSEExp e) n a)
        cap = BindSIMDVec @e @n @a
        vk = VarParam k
        vid = VarId vk tr (Just cap)
     in (k + 1, mkExpVar vid, \actual -> addExpParamSubst vk tr cap actual)
  cseBindTargetExpParam k val =
    let wrapped :: e (SIMDVector (CSEExp e) n a)
        wrapped = coerceExpNewtype val
     in (k + 1, [(VarParam k, SomeTargetVal (TR.typeRep @(SIMDVector (CSEExp e) n a)) wrapped)])

instance
  {-# OVERLAPPING #-}
  (CSESIMDTarget e, ExpNewtype e, SIMDShape n a, Typeable e) =>
  CSEExpParamLeaf e (SIMDMask (CSEExp e) n a)
  where
  cseLeafBindCap = BindSIMDMask @e @n @a
  cseAllocExpParam k =
    let tr = TR.typeRep @(SIMDMask (CSEExp e) n a)
        cap = BindSIMDMask @e @n @a
        vk = VarParam k
        vid = VarId vk tr (Just cap)
     in (k + 1, mkExpVar vid, \actual -> addExpParamSubst vk tr cap actual)
  cseBindTargetExpParam k val =
    let wrapped :: e (SIMDMask (CSEExp e) n a)
        wrapped = coerceExpNewtype val
     in (k + 1, [(VarParam k, SomeTargetVal (TR.typeRep @(SIMDMask (CSEExp e) n a)) wrapped)])

instance
  {-# OVERLAPPING #-}
  (CSEExpParamLeaf e x, CSEExpParamLeaf e y, ExpNewtype e, Typeable x, Typeable y) =>
  CSEExpParamLeaf e (SIMDPair x y)
  where
  cseLeafBindCap = BindPairOf (cseLeafBindCap @e @x) (cseLeafBindCap @e @y)
  cseAllocExpParam k =
    let tr = TR.typeRep @(SIMDPair x y)
        cap = cseLeafBindCap @e @(SIMDPair x y)
        vk = VarParam k
        vid = VarId vk tr (Just cap)
     in (k + 1, mkExpVar vid, \actual -> addExpParamSubst vk tr cap actual)
  cseBindTargetExpParam k val =
    let wrapped :: e (SIMDPair x y)
        wrapped = coerceExpNewtype val
     in (k + 1, [(VarParam k, SomeTargetVal (TR.typeRep @(SIMDPair x y)) wrapped)])

instance (CSEExpParamLeaf e a) => CSEParam (CSEExp e a) where
  type CSEParamExp (CSEExp e a) = e
  type CSEParamTarget (CSEExp e a) = e (TargetExpElem e a)
  cseAllocParam = cseAllocExpParam @e @a
  cseBindTargetParam n val =
    let (n', vs) = cseBindTargetExpParam @e @a n val
     in (n', vs, [])

instance
  ( SPMDTyped n (CSEExp e) a,
    SPMDTyped n e a,
    CSEExpParamLeaf e (SPMDRep n (CSEExp e) a),
    TargetExpElem e (SPMDRep n (CSEExp e) a) ~ SPMDRep n e a
  ) =>
  CSEParam (SPMD n (CSEExp e) a)
  where
  type CSEParamExp (SPMD n (CSEExp e) a) = e
  type CSEParamTarget (SPMD n (CSEExp e) a) = SPMD n e a
  cseAllocParam k0 =
    let (k1, formalExp, bindExp) = cseAllocExpParam @e @(SPMDRep n (CSEExp e) a) k0
     in ( k1,
          SPMD (pure (spmdDematerialize @n @(CSEExp e) @a formalExp)),
          \actualSPMD -> bindExp (spmdMaterialize @n @(CSEExp e) @a (runSPMD actualSPMD))
        )
  cseBindTargetParam k0 tgtSPMD =
    let rawTgt :: e (SPMDRep n e a)
        rawTgt = spmdMaterialize @n @e @a (runSPMD tgtSPMD)
        (k1, vs) = cseBindTargetExpParam @e @(SPMDRep n (CSEExp e) a) k0 rawTgt
     in (k1, vs, [])

instance (CmdRef m e, CSETyped e a) => CSEParam (CSERef m e a) where
  type CSEParamExp (CSERef m e a) = e
  type CSEParamTarget (CSERef m e a) = Ref m e a
  cseAllocParam k =
    let tr = TR.typeRep @a
        cap = BindDirect @e @a
        rk = RefParam k
        formalRef = CSERef rk tr cap
     in (k + 1, formalRef, \(CSERef rkActual _ _) -> addRefParamSubst rk rkActual)
  cseBindTargetParam k tgtRef =
    (k + 1, [], [(RefParam k, SomeTargetRef (TR.typeRep @a) tgtRef)])

instance (CSETyped e a, ReifyTensorBoundsShape ds) => CSEParam (Tensor ds (CSEExp e) a) where
  type CSEParamExp (Tensor ds (CSEExp e) a) = e
  type CSEParamTarget (Tensor ds (CSEExp e) a) = Tensor ds e a
  cseAllocParam k =
    let tr = TR.typeRep @(Ptr a)
        cap = BindPtrOf (BindDirect @e @a)
        vk = VarParam k
        vid = VarId vk tr (Just cap)
        tb = reifyTensorBoundsCSE @ds @(CSEExp e)
     in (k + 1, Tensor tb (mkExpVar vid), \(Tensor _ actP) -> addExpParamSubst vk tr cap actP)
  cseBindTargetParam k (Tensor _ tgtP) =
    (k + 1, [(VarParam k, SomeTargetVal (TR.typeRep @(Ptr a)) tgtP)], [])

class ReifyTensorBoundsShape (ds :: [Nat]) where
  reifyTensorBoundsCSE :: TensorBounds ds e

instance ReifyTensorBoundsShape '[] where
  reifyTensorBoundsCSE = TensorBoundsNil

instance (KnownNat d, ReifyTensorBoundsShape ds) => ReifyTensorBoundsShape (d ': ds) where
  reifyTensorBoundsCSE = TensorBoundsCons (Proxy @d) (reifyTensorBoundsCSE @ds)

instance (CSEHKD d e, Typeable e) => CSEParam (HKD d (CSEExp e)) where
  type CSEParamExp (HKD d (CSEExp e)) = e
  type CSEParamTarget (HKD d (CSEExp e)) = HKD d e
  cseAllocParam k0 =
    let (k1, formalD, bindD) = allocFormalCSEHKD @d @e k0
     in (k1, HKD formalD, \(HKD actD) -> bindD actD)
  cseBindTargetParam k0 (HKD tgtD) =
    let (k1, vs) = bindTargetCSEHKD @d @e k0 tgtD
     in (k1, vs, [])

instance
  (CSEParam p1, CSEParam p2, CSEParamExp p1 ~ CSEParamExp p2) =>
  CSEParam (p1, p2)
  where
  type CSEParamExp (p1, p2) = CSEParamExp p1
  type CSEParamTarget (p1, p2) = (CSEParamTarget p1, CSEParamTarget p2)
  cseAllocParam k0 =
    let (k1, f1, b1) = cseAllocParam @p1 k0
        (k2, f2, b2) = cseAllocParam @p2 k1
     in (k2, (f1, f2), \(a1, a2) -> b2 a2 . b1 a1)
  cseBindTargetParam ::
    forall m.
    (CSEParamMonad (p1, p2) m) =>
    Int ->
    (CSEParamTarget p1, CSEParamTarget p2) ->
    (Int, [(VarKey, SomeTargetVal (CSEParamExp p1))], [(RefKey, SomeTargetRef m (CSEParamExp p1))])
  cseBindTargetParam k0 (t1, t2) =
    let (k1, v1, r1) = cseBindTargetParam @p1 @m k0 t1
        (k2, v2, r2) = cseBindTargetParam @p2 @m k1 t2
     in (k2, v1 <> v2, r1 <> r2)

instance
  (CSEParam p1, CSEParam p2, CSEParam p3, CSEParamExp p1 ~ CSEParamExp p2, CSEParamExp p2 ~ CSEParamExp p3) =>
  CSEParam (p1, p2, p3)
  where
  type CSEParamExp (p1, p2, p3) = CSEParamExp p1
  type CSEParamTarget (p1, p2, p3) = (CSEParamTarget p1, CSEParamTarget p2, CSEParamTarget p3)
  cseAllocParam k0 =
    let (k1, f1, b1) = cseAllocParam @p1 k0
        (k2, f2, b2) = cseAllocParam @p2 k1
        (k3, f3, b3) = cseAllocParam @p3 k2
     in (k3, (f1, f2, f3), \(a1, a2, a3) -> b3 a3 . b2 a2 . b1 a1)
  cseBindTargetParam ::
    forall m.
    (CSEParamMonad (p1, p2, p3) m) =>
    Int ->
    (CSEParamTarget p1, CSEParamTarget p2, CSEParamTarget p3) ->
    (Int, [(VarKey, SomeTargetVal (CSEParamExp p1))], [(RefKey, SomeTargetRef m (CSEParamExp p1))])
  cseBindTargetParam k0 (t1, t2, t3) =
    let (k1, v1, r1) = cseBindTargetParam @p1 @m k0 t1
        (k2, v2, r2) = cseBindTargetParam @p2 @m k1 t2
        (k3, v3, r3) = cseBindTargetParam @p3 @m k2 t3
     in (k3, v1 <> v2 <> v3, r1 <> r2 <> r3)

class CSEApplySubst (e :: K.Type -> K.Type) (r :: K.Type) where
  applyParamSubst :: ParamSubst e -> r -> r

instance CSEApplySubst e (CSEExp e a) where
  applyParamSubst sub expr
    | isIdentityParamSubst sub = expr
    | otherwise = applyPureParamSubst (psExpList sub) expr

applyPureParamSubst :: forall e a. [(VarKey, ParamBindEntry e)] -> CSEExp e a -> CSEExp e a
applyPureParamSubst binds body0 =
  case emBind (expMeta body0) of
    Nothing -> substExp trivialCopyMap body0
    Just (trBody, capBody) ->
      let (copyMp, wrappers) = foldl step (Map.empty, id) binds
          step (mp, wrapAcc) (vk, ParamBindEntry trA capA actExp)
            | emIsTrivial (expMeta actExp) =
                (Map.insert vk (SomeCSEExp trA actExp) mp, wrapAcc)
            | otherwise =
                let letKey = VarUserLet (mkNodeDigest "ParamLet" [encodeVarKey vk] [emDigest (expMeta actExp)])
                    vid = VarId letKey trA (Just capA)
                    wrap inner = mkExpCSELet vid capA trBody capBody actExp inner
                 in (Map.insert vk (SomeCSEExp trA (mkExpVar vid)) mp, wrapAcc . wrap)
       in wrappers (substExp copyMp body0)
  where
    trivialCopyMap =
      Map.fromList
        [ (vk, SomeCSEExp trA actExp)
          | (vk, ParamBindEntry trA _ actExp) <- binds
        ]

instance (CSEReturn e res) => CSEApplySubst e (CSEM m e res) where
  applyParamSubst sub mAct@(CSEM buildFn)
    | isIdentityParamSubst sub = mAct
    | otherwise =
        let (emptyUsed, emptyTree) = buildFn DHM.empty 0
         in CSEM $ \tfm baseCounter ->
              if DHM.null tfm
                then instantiateOptTree sub baseCounter emptyUsed emptyTree
                else
                  let (countUsed, tree) = buildFn tfm 0
                   in instantiateOptTree sub baseCounter countUsed tree

instance (SPMDTyped n (CSEExp e) a) => CSEApplySubst e (SPMD n (CSEExp e) a) where
  applyParamSubst sub x
    | isIdentityParamSubst sub = x
    | otherwise =
        let raw = spmdMaterialize @n @(CSEExp e) @a (runSPMD x)
         in SPMD (pure (spmdDematerialize @n @(CSEExp e) @a (applyParamSubst sub raw)))

instance (CSEReturn e res) => CSEApplySubst e (SPMDM n (CSEM m e) (CSEExp e) res) where
  applyParamSubst sub act = liftSPMD (applyParamSubst sub (runSPMDM act))

instance (CSEApplySubst e r) => CSEApplySubst e (p -> r) where
  applyParamSubst sub fn p = applyParamSubst sub (fn p)

class CSEOptimizeFrom (r :: K.Type) where
  type CSEOptimizeExp r :: K.Type -> K.Type
  cseFromIdxWith :: TargetFeatureMap -> Int -> r -> r

cseFromIdx :: (CSEOptimizeFrom r) => Int -> r -> r
cseFromIdx = cseFromIdxWith DHM.empty

instance CSEOptimizeFrom (CSEExp e a) where
  type CSEOptimizeExp (CSEExp e a) = e
  cseFromIdxWith _ _ = cseExp

instance (CSEReturn e res) => CSEOptimizeFrom (CSEM m e res) where
  type CSEOptimizeExp (CSEM m e res) = e
  cseFromIdxWith tfm _ = cseMWithFeatures tfm

instance (SPMDTyped n (CSEExp e) a) => CSEOptimizeFrom (SPMD n (CSEExp e) a) where
  type CSEOptimizeExp (SPMD n (CSEExp e) a) = e
  cseFromIdxWith _ _ x =
    let raw = spmdMaterialize @n @(CSEExp e) @a (runSPMD x)
     in SPMD (pure (spmdDematerialize @n @(CSEExp e) @a (cseExp raw)))

instance (CSEReturn e res) => CSEOptimizeFrom (SPMDM n (CSEM m e) (CSEExp e) res) where
  type CSEOptimizeExp (SPMDM n (CSEM m e) (CSEExp e) res) = e
  cseFromIdxWith tfm _ act = liftSPMD (cseMWithFeatures tfm (runSPMDM act))

instance
  ( CSEParam p,
    CSEOptimizeFrom r,
    e ~ CSEParamExp p,
    e ~ CSEOptimizeExp r,
    CSEApplySubst e r
  ) =>
  CSEOptimizeFrom (p -> r)
  where
  type CSEOptimizeExp (p -> r) = CSEParamExp p
  cseFromIdxWith tfm idx f =
    let (idx', formalP, bindP) = cseAllocParam @p idx
        !optRest = cseFromIdxWith tfm idx' (f formalP)
     in \actualP ->
          let sub = bindP actualP emptyParamSubst
           in if isIdentityParamSubst sub
                then optRest
                else applyParamSubst @e @r sub optRest

-- | Eliminate common subexpressions across a pure 'CSEExp' expression, a
-- monadic 'CSEM' computation, an 'SPMD' / 'SPMDM' computation, or an
-- arbitrarily curried Janus function over @('CSEM' m e, 'CSEExp' e)@.
class (CSEOptimizeFrom r) => CSEOptimize (r :: K.Type) where
  cse :: r -> r
  cseWithFeatures :: TargetFeatureMap -> r -> r

instance (CSEOptimizeFrom r) => CSEOptimize r where
  cse = cseFromIdxWith DHM.empty 0
  cseWithFeatures tfm = cseFromIdxWith tfm 0

