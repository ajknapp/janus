{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Typed Einstein tensor contraction combinators (strictly index-binder /
-- hypergraph-based with NO @Symbol@ string sugar!) and automatic IR pass
-- ('recognizeContractions') that lifts nested @For@ + @Reduce@ + semiring
-- product expressions into multi-tensor 'NContract' hypergraphs.
module Janus.Tensor.Contract.Recognize
  ( -- * Typed Hypergraph Contraction Builder (No String Sugar!)
    IxAxis (..),
    AxisShape (..),
    ContractBuilder,
    freshAxis,
    useOperand,
    buildContract,
    buildContractNode,
    contract2,
    contract3,
    contract4,

    -- * Automatic IR Contraction Recognition
    recognizeContractions,
  )
where

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (eqT, (:~:) (..))
import Janus.Tensor.Algebra
  ( AddOp,
    LogAddExpOp,
    LogSR,
    MaxOp,
    MinOp,
    OrOp,
    SomeMonoid (..),
    SomeSemiring (..),
    StandardSR,
    TensorScalar,
    TensorSemiring,
    TropicalMaxSR,
    TropicalMinSR,
    BooleanSR,
    someSemiring,
  )
import Janus.Tensor.IR
  ( ContractOperand (..),
    ContractSpec (..),
    Expr (..),
    HNodeArgs (..),
    IxExpr (..),
    RawExpr (..),
    RawNode (..),
    ScalarBinOp (..),
    TNode (..),
    Tab (..),
    fromNode,
    materialize,
    mkAnnotateNode,
    mkAssocScan,
    mkContract,
    mkKernelPrimNode,
    mkReindex,
    mkScan,
  )
import Janus.Tensor.Index
  ( DimSpec,
    IxKind (..),
    IxSet (..),
    SIx (..),
    sixFlatDims,
  )

--------------------------------------------------------------------------------
-- 1. Typed Hypergraph Contraction Builder (Zero String Sugar)
--------------------------------------------------------------------------------

-- | Typed handle to a contraction axis in 'ContractBuilder'.
data IxAxis (i :: IxKind) = IxAxis
  { axIds :: ![Int],
    axShape :: !(SIx i)
  }

-- | Type-safe shape witness matching a tensor's 'IxKind' to its 'IxAxis' handles.
data AxisShape (i :: IxKind) where
  AxisUnit :: AxisShape 'IxUnit
  Axis1 :: !(IxAxis i) -> AxisShape i
  AxisProd :: !(AxisShape i) -> !(AxisShape j) -> AxisShape ('IxProd i j)
  AxisTag :: !(Proxy s) -> !(AxisShape i) -> AxisShape ('IxTagged s i)

axisShapeLabels :: AxisShape i -> [Int]
axisShapeLabels AxisUnit = []
axisShapeLabels (Axis1 ax) = axIds ax
axisShapeLabels (AxisProd a b) = axisShapeLabels a <> axisShapeLabels b
axisShapeLabels (AxisTag _ a) = axisShapeLabels a

data CBState a = CBState
  { cbNextId :: !Int,
    cbVerts :: !(Map.Map Int DimSpec),
    cbRevOperands :: ![ContractOperand a],
    cbLevel :: !Int
  }

newtype ContractBuilder (a :: K.Type) r = ContractBuilder {unCB :: State (CBState a) r}
  deriving newtype (Functor, Applicative, Monad)

-- | Allocate a fresh typed contraction axis (for either an output or contracted index).
freshAxis :: forall i a. (IxSet i) => ContractBuilder a (IxAxis i)
freshAxis = ContractBuilder $ do
  let sh = six @i
      dims = sixFlatDims sh
  start <- gets cbNextId
  let ids = [start .. start + length dims - 1]
      newVerts = Map.fromList (zip ids dims)
  modify' (\st -> st {cbNextId = start + length dims, cbVerts = Map.union newVerts (cbVerts st)})
  pure (IxAxis ids sh)

-- | Register a tensor operand with its typed axis assignments.
useOperand :: (TensorScalar a) => Tab i (Expr a) -> AxisShape i -> ContractBuilder a ()
useOperand tab axes = ContractBuilder $ do
  lvl <- gets cbLevel
  let node = unTNode (materialize tab) lvl
      op = ContractOperand node (axisShapeLabels axes)
  modify' (\st -> st {cbRevOperands = op : cbRevOperands st})

buildContractNode ::
  forall sr i a.
  (IxSet i, TensorSemiring sr a) =>
  Proxy sr ->
  ContractBuilder a (AxisShape i) ->
  TNode i a
buildContractNode _ builder = TNode $ \lvl ->
  let initSt = CBState {cbNextId = 0, cbVerts = Map.empty, cbRevOperands = [], cbLevel = lvl}
      (outAxes, finalSt) = runState (unCB builder) initSt
      spec =
        ContractSpec
          { csOutShape = six @i,
            csOutLabels = axisShapeLabels outAxes,
            csVertices = cbVerts finalSt,
            csOperands = reverse (cbRevOperands finalSt)
          }
   in mkContract (someSemiring @sr @a) spec

-- | Build an unplanned N-ary Einstein tensor contraction over ANY open-world
-- 'TensorSemiring' using strictly typed 'IxAxis' binders.
buildContract ::
  forall sr i a.
  (IxSet i, TensorSemiring sr a) =>
  Proxy sr ->
  ContractBuilder a (AxisShape i) ->
  Tab i (Expr a)
buildContract p = fromNode . buildContractNode p

-- | Ergonomic binary Einstein contraction over any 'TensorSemiring'.
contract2 ::
  forall sr i a.
  (IxSet i, TensorSemiring sr a) =>
  Proxy sr ->
  ContractBuilder a (AxisShape i) ->
  Tab i (Expr a)
contract2 = buildContract

-- | Ergonomic 3-way matrix-chain / tensor contraction builder:
-- @contract3 (Proxy \@StandardSR) (\\i j k l -> ...)@
contract3 ::
  forall sr i j k l a.
  (IxSet i, IxSet j, IxSet k, IxSet l, TensorSemiring sr a) =>
  Proxy sr ->
  Tab ('IxProd i j) (Expr a) ->
  Tab ('IxProd j k) (Expr a) ->
  Tab ('IxProd k l) (Expr a) ->
  Tab ('IxProd i l) (Expr a)
contract3 p a b c = buildContract p $ do
  i <- freshAxis @i
  j <- freshAxis @j
  k <- freshAxis @k
  l <- freshAxis @l
  useOperand a (AxisProd (Axis1 i) (Axis1 j))
  useOperand b (AxisProd (Axis1 j) (Axis1 k))
  useOperand c (AxisProd (Axis1 k) (Axis1 l))
  pure (AxisProd (Axis1 i) (Axis1 l))

-- | Ergonomic 4-way matrix-chain contraction builder @(i,j),(j,k),(k,l),(l,m) -> (i,m)@.
contract4 ::
  forall sr i j k l m a.
  (IxSet i, IxSet j, IxSet k, IxSet l, IxSet m, TensorSemiring sr a) =>
  Proxy sr ->
  Tab ('IxProd i j) (Expr a) ->
  Tab ('IxProd j k) (Expr a) ->
  Tab ('IxProd k l) (Expr a) ->
  Tab ('IxProd l m) (Expr a) ->
  Tab ('IxProd i m) (Expr a)
contract4 p a b c d = buildContract p $ do
  i <- freshAxis @i
  j <- freshAxis @j
  k <- freshAxis @k
  l <- freshAxis @l
  m <- freshAxis @m
  useOperand a (AxisProd (Axis1 i) (Axis1 j))
  useOperand b (AxisProd (Axis1 j) (Axis1 k))
  useOperand c (AxisProd (Axis1 k) (Axis1 l))
  useOperand d (AxisProd (Axis1 l) (Axis1 m))
  pure (AxisProd (Axis1 i) (Axis1 m))

--------------------------------------------------------------------------------
-- 2. Automatic IR Contraction Recognition Pass
--------------------------------------------------------------------------------

-- | Walk a 'RawNode' DAG and automatically lift nested @NFor@ + @EReduce@ +
-- semiring product trees over indexed tensors into multi-tensor 'NContract'
-- hypergraph nodes so the contraction planner optimizes Dex-style @forIx@ /
-- @sumIx@ expressions automatically!
recognizeContractions :: RawNode i a -> RawNode i a
recognizeContractions = \case
  n@(NInput {}) -> n
  n@(NFor _ outSh outVar body) ->
    case tryRecognizeFor outSh outVar body of
      Just contracted -> contracted
      Nothing -> n
  NReindex _ m inner -> mkReindex m (recognizeContractions inner)
  n@(NScatter {}) -> n
  n@(NRunAccum {}) -> n
  n@(NRunST {}) -> n
  NScan _ s initE xs cv xv step -> mkScan s initE (recognizeContractions xs) cv xv step
  NAssocScan _ m s xs -> mkAssocScan m s (recognizeContractions xs)
  NContract _ sr spec ->
    let ops' = [ContractOperand (recognizeContractions opN) ls | ContractOperand opN ls <- csOperands spec]
     in mkContract sr (spec {csOperands = ops'})
  NKernelPrim _ spec args -> mkKernelPrimNode spec (mapHNodes recognizeContractions args)
  NAnnotate _ attrs inner -> mkAnnotateNode attrs (recognizeContractions inner)

mapHNodes :: (forall s. RawNode s a -> RawNode s a) -> HNodeArgs shs a -> HNodeArgs shs a
mapHNodes _ HNNil = HNNil
mapHNodes f (HNCons n rest) = HNCons (f n) (mapHNodes f rest)

-- | Peel nested 'EReduce' binders over the same monoid and check if the inner
-- scalar expression is a pure semiring product of at least 2 indexed tensors.
tryRecognizeFor ::
  forall i a.
  (TensorScalar a) =>
  SIx i ->
  Int ->
  RawExpr a ->
  Maybe (RawNode i a)
tryRecognizeFor outSh outVar body =
  let (redBinders, innerExpr) = peelReduces body
   in case redBinders of
        [] -> Nothing
        ((firstMonoid, _, _) : _) -> do
          -- Ensure all nested reductions use the exact same monoid
          sr <- monoidToSemiring firstMonoid innerExpr
          -- Ensure every binder in redBinders matches the first monoid
          if all (\(m, _, _) -> sameMonoidTag m firstMonoid) redBinders
            then do
              -- Assign flat vertex IDs to outer variable and each reduction variable
              let ((outLabels, varLabelMap), vertMap) =
                    runState
                      ( do
                          oLs <- allocVarAxes outSh
                          let m0 = IM.singleton outVar (SomeSIxBox outSh, oLs)
                          mFinal <-
                            foldr
                              ( \(_, rSh, rVar) accM -> do
                                  acc <- accM
                                  rLs <- allocVarAxesBox rSh
                                  pure (IM.insert rVar (rSh, rLs) acc)
                              )
                              (pure m0)
                              redBinders
                          pure (oLs, mFinal)
                      )
                      (0 :: Int, Map.empty :: Map.Map Int DimSpec)
              operands <- extractSemiringFactors sr varLabelMap innerExpr
              if length operands >= 2
                then
                  let spec =
                        ContractSpec
                          { csOutShape = outSh,
                            csOutLabels = outLabels,
                            csVertices = snd vertMap,
                            csOperands =
                              [ ContractOperand (recognizeContractions n) ls
                                | ContractOperand n ls <- operands
                              ]
                          }
                   in Just (mkContract sr spec)
                else Nothing
            else Nothing

peelReduces :: RawExpr a -> ([(SomeMonoid a, SomeSIxBox, Int)], RawExpr a)
peelReduces (EReduce _ m s v inner) =
  let (rest, core) = peelReduces inner
   in ((m, SomeSIxBox s, v) : rest, core)
peelReduces e = ([], e)

data SomeSIxBox where
  SomeSIxBox :: !(SIx j) -> SomeSIxBox

allocVarAxes :: SIx j -> State (Int, Map.Map Int DimSpec) [Int]
allocVarAxes sh = do
  let dims = sixFlatDims sh
  (nextId, vMap) <- gets id
  let ids = [nextId .. nextId + length dims - 1]
      vMap' = Map.union (Map.fromList (zip ids dims)) vMap
  modify' (const (nextId + length dims, vMap'))
  pure ids

allocVarAxesBox :: SomeSIxBox -> State (Int, Map.Map Int DimSpec) [Int]
allocVarAxesBox (SomeSIxBox s) = allocVarAxes s

sameMonoidTag :: SomeMonoid a -> SomeMonoid a -> Bool
sameMonoidTag (SomeMonoid (_ :: Proxy op1)) (SomeMonoid (_ :: Proxy op2)) =
  case eqT @op1 @op2 of
    Just Refl -> True
    Nothing -> False

-- | Infer the semiring from the reduction monoid and the top-level product op.
monoidToSemiring :: forall a. (TensorScalar a) => SomeMonoid a -> RawExpr a -> Maybe (SomeSemiring a)
monoidToSemiring (SomeMonoid (_ :: Proxy op)) expr =
  case expr of
    EBinOp _ BMul _ _ -> do
      Refl <- eqT @op @AddOp
      tryStandardSR @a
    EBinOp _ BAdd _ _ ->
      case eqT @op @LogAddExpOp of
        Just Refl -> tryLogSR @a
        Nothing ->
          case eqT @op @MaxOp of
            Just Refl -> tryTropMaxSR @a
            Nothing ->
              case eqT @op @MinOp of
                Just Refl -> tryTropMinSR @a
                Nothing -> Nothing
    EBinOp _ BAnd _ _ -> do
      Refl <- eqT @op @OrOp
      Refl <- eqT @a @Bool
      Just (someSemiring @BooleanSR @Bool)
    _ -> Nothing

tryStandardSR :: forall a. (TensorScalar a) => Maybe (SomeSemiring a)
tryStandardSR =
  case eqT @a @Float of
    Just Refl -> Just (someSemiring @StandardSR @Float)
    Nothing -> case eqT @a @Double of
      Just Refl -> Just (someSemiring @StandardSR @Double)
      Nothing -> case eqT @a @Int32 of
        Just Refl -> Just (someSemiring @StandardSR @Int32)
        Nothing -> case eqT @a @Int64 of
          Just Refl -> Just (someSemiring @StandardSR @Int64)
          Nothing -> Nothing

tryLogSR :: forall a. (TensorScalar a) => Maybe (SomeSemiring a)
tryLogSR =
  case eqT @a @Float of
    Just Refl -> Just (someSemiring @LogSR @Float)
    Nothing -> case eqT @a @Double of
      Just Refl -> Just (someSemiring @LogSR @Double)
      Nothing -> Nothing

tryTropMaxSR :: forall a. (TensorScalar a) => Maybe (SomeSemiring a)
tryTropMaxSR =
  case eqT @a @Float of
    Just Refl -> Just (someSemiring @TropicalMaxSR @Float)
    Nothing -> case eqT @a @Double of
      Just Refl -> Just (someSemiring @TropicalMaxSR @Double)
      Nothing -> Nothing

tryTropMinSR :: forall a. (TensorScalar a) => Maybe (SomeSemiring a)
tryTropMinSR =
  case eqT @a @Float of
    Just Refl -> Just (someSemiring @TropicalMinSR @Float)
    Nothing -> case eqT @a @Double of
      Just Refl -> Just (someSemiring @TropicalMinSR @Double)
      Nothing -> Nothing

-- | Extract a flat list of indexed tensor operands from a semiring product tree.
extractSemiringFactors ::
  SomeSemiring a ->
  IM.IntMap (SomeSIxBox, [Int]) ->
  RawExpr a ->
  Maybe [ContractOperand a]
extractSemiringFactors (SomeSemiring (_ :: Proxy srTy)) varMap = go
  where
    isMulOp :: ScalarBinOp a -> Bool
    isMulOp op = case op of
      BMul -> maybe False (const True) (eqT @srTy @StandardSR)
      BAdd ->
        maybe False (const True) (eqT @srTy @LogSR)
          || maybe False (const True) (eqT @srTy @TropicalMaxSR)
          || maybe False (const True) (eqT @srTy @TropicalMinSR)
      BAnd -> maybe False (const True) (eqT @srTy @BooleanSR)
      _ -> False

    go (EBinOp _ op l r) | isMulOp op = do
      ls <- go l
      rs <- go r
      pure (ls <> rs)
    go (EIndex _ node ix) = do
      labels <- resolveIxLabels varMap ix
      pure [ContractOperand node labels]
    go _ = Nothing

-- | Map an 'IxExpr' composed of bound variables, projections ('IxFst'/'IxSnd'),
-- pairs ('IxPair'), and tags to its flat list of hypergraph vertex IDs.
resolveIxLabels :: IM.IntMap (SomeSIxBox, [Int]) -> IxExpr j -> Maybe [Int]
resolveIxLabels varMap = go
  where
    go :: IxExpr k -> Maybe [Int]
    go (IxVar v _) = snd <$> IM.lookup v varMap
    go (IxPair a b) = (<>) <$> go a <*> go b
    go (IxFst p) = do
      (shL, ls) <- goWithSplit p
      pure (take (length (sixFlatDims shL)) ls)
    go (IxSnd p) = do
      (shL, ls) <- goWithSplit p
      pure (drop (length (sixFlatDims shL)) ls)
    go (IxTagE _ a) = go a
    go (IxUntagE a) = go a
    go _ = Nothing

    goWithSplit :: IxExpr ('IxProd a b) -> Maybe (SIx a, [Int])
    goWithSplit (IxVar v (SIxProd shL _)) = do
      (_, ls) <- IM.lookup v varMap
      pure (shL, ls)
    goWithSplit (IxPair a b) = do
      lsA <- go a
      lsB <- go b
      pure (ixExprShape' a, lsA <> lsB)
    goWithSplit _ = Nothing

    ixExprShape' :: IxExpr k -> SIx k
    ixExprShape' (IxVar _ s) = s
    ixExprShape' (IxConst _ s) = s
    ixExprShape' (IxPair a b) = SIxProd (ixExprShape' a) (ixExprShape' b)
    ixExprShape' (IxFst p) = case ixExprShape' p of SIxProd a _ -> a
    ixExprShape' (IxSnd p) = case ixExprShape' p of SIxProd _ b -> b
    ixExprShape' (IxInL sb a) = SIxSum (ixExprShape' a) sb
    ixExprShape' (IxInR sa b) = SIxSum sa (ixExprShape' b)
    ixExprShape' (IxTagE p a) = SIxTagged p (ixExprShape' a)
    ixExprShape' (IxUntagE a) = case ixExprShape' a of SIxTagged _ s -> s
    ixExprShape' (IxApplyIso _ _) = error "resolveIxLabels: IxApplyIso not directly contracted"
    ixExprShape' (IxFromOrd s _) = s
