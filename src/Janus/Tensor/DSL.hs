{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Ergonomic purely functional, Dex-inspired, index-first surface DSL for
-- @Janus.Tensor@.
module Janus.Tensor.DSL
  ( -- * Scalar Literals, Arithmetic, Special Math, Comparisons & Conditionals
    lit,
    trueT,
    falseT,
    bnot,
    (.&&),
    (.||),
    (.==),
    (./=),
    (.<),
    (.<=),
    (.>),
    (.>=),
    select,
    castT,
    quotT,
    remT,
    lgammaT,
    digammaT,
    trigammaT,
    erfT,
    erfcT,
    erfinvT,
    log1pT,
    expm1T,
    rsqrtT,

    -- * Index Expressions & Dex-Style Tables (`forIx`, `(!)`)
    forIx,
    (!),
    ixOrd,
    ixConst,
    ixPair,
    ixFst,
    ixSnd,
    ixInLE,
    ixInRE,
    ixTagE,
    ixUntagE,
    ixFromOrd,
    ixCase,
    inputNode,
    inputTab,
    letTab,

    -- * Structured Optic Views (`Reindex`)
    reindex,
    reindexIso,
    reindexPrism,
    reindexAffine,
    transposeTab,
    reshapeTab,
    reverseTab,
    sliceTab,
    strideTab,
    diagTab,
    window1DTab,
    concatTab,

    -- * Open-World Reductions (`TensorMonoid`)
    reduceIx,
    sumIx,
    prodIx,
    maxIx,
    minIx,
    logSumExpIx,
    anyIx,
    allIx,

    -- * Open-World Commutative-Monoid Scatter (`TensorCommMonoid`)
    scatterM,
    scatterAdd,

    -- * Sequential & Parallel Associative Scans
    scanIx,
    assocScanIx,

    -- * Open-World Backdoor Primitives & Metadata Annotations
    HExprsDSL (..),
    HNodesDSL (..),
    callScalarPrim,
    callKernelPrim,
    annotateExpr,
    annotateTab,
  )
where

import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol)
import Janus.Tensor.Algebra
  ( AddOp,
    AndOp,
    LogSumExpOp,
    MaxOp,
    MinOp,
    MulOp,
    OrOp,
    TensorCommMonoid,
    TensorMonoid,
    TensorScalar,
    someCommMonoid,
    someMonoid,
  )
import Janus.Tensor.Custom (KernelPrimSpec, ScalarPrimSpec)
import Janus.Tensor.IR
  ( Expr (..),
    HExprArgs (..),
    HNodeArgs (..),
    IxExpr (..),
    ScalarBinOp (..),
    ScalarCmpOp (..),
    ScalarUnOp (..),
    TNode (..),
    Tab (..),
    fromNode,
    materialize,
    mkAnnotateExpr,
    mkAnnotateNode,
    mkBinOp,
    mkCast,
    mkCmp,
    mkInput,
    mkKernelPrimNode,
    mkLit,
    mkOrd,
    mkReduce,
    mkReindex,
    mkScalarPrim,
    mkScalarVar,
    mkScan,
    mkScatter,
    mkSelect,
    mkShapeSize,
    mkUnOp,
    mkAssocScan,
  )
import Janus.Tensor.Index
  ( IxAffine,
    IxIso,
    IxKind (..),
    IxMorph (..),
    IxPrism,
    IxSet (..),
    SIx (..),
    ixDiag,
    ixRev,
    ixReshape,
    ixSlice,
    ixStride,
    ixSwap,
    ixWindow1D,
  )
import Janus.Tensor.Types (PrimAttrMap)

--------------------------------------------------------------------------------
-- 1. Scalar Literals (Num / Fractional / Floating instances live in Janus.Tensor.IR)
--------------------------------------------------------------------------------

lit :: (TensorScalar a, Show a) => a -> Expr a
lit x = Expr (\_ -> mkLit x)

--------------------------------------------------------------------------------
-- 2. Special Math, Booleans, Comparisons, Casts, & Conditionals
--------------------------------------------------------------------------------

lgammaT, digammaT, trigammaT, erfT, erfcT, erfinvT, log1pT, expm1T, rsqrtT ::
  (TensorScalar a, Floating a) => Expr a -> Expr a
lgammaT (Expr f) = Expr (mkUnOp ULGamma . f)
digammaT (Expr f) = Expr (mkUnOp UDigamma . f)
trigammaT (Expr f) = Expr (mkUnOp UTrigamma . f)
erfT (Expr f) = Expr (mkUnOp UErf . f)
erfcT (Expr f) = Expr (mkUnOp UErfc . f)
erfinvT (Expr f) = Expr (mkUnOp UErfInv . f)
log1pT (Expr f) = Expr (mkUnOp ULog1p . f)
expm1T (Expr f) = Expr (mkUnOp UExpm1 . f)
rsqrtT (Expr f) = Expr (mkUnOp URsqrt . f)

trueT, falseT :: Expr Bool
trueT = lit True
falseT = lit False

bnot :: Expr Bool -> Expr Bool
bnot (Expr f) = Expr (mkUnOp UNot . f)

(.&&), (.||) :: Expr Bool -> Expr Bool -> Expr Bool
Expr f .&& Expr g = Expr (\l -> mkBinOp BAnd (f l) (g l))
Expr f .|| Expr g = Expr (\l -> mkBinOp BOr (f l) (g l))
infixr 3 .&&
infixr 2 .||

(.==), (./=), (.<), (.<=), (.>), (.>=) :: (TensorScalar a, Ord a) => Expr a -> Expr a -> Expr Bool
Expr f .== Expr g = Expr (\l -> mkCmp CEq (f l) (g l))
Expr f ./= Expr g = Expr (\l -> mkCmp CNe (f l) (g l))
Expr f .< Expr g = Expr (\l -> mkCmp CLt (f l) (g l))
Expr f .<= Expr g = Expr (\l -> mkCmp CLe (f l) (g l))
Expr f .> Expr g = Expr (\l -> mkCmp CGt (f l) (g l))
Expr f .>= Expr g = Expr (\l -> mkCmp CGe (f l) (g l))
infix 4 .==, ./=, .<, .<=, .>, .>=

select :: (TensorScalar a) => Expr Bool -> Expr a -> Expr a -> Expr a
select (Expr c) (Expr t) (Expr f) = Expr (\l -> mkSelect (c l) (t l) (f l))

castT :: (TensorScalar a, TensorScalar b) => Expr a -> Expr b
castT (Expr f) = Expr (mkCast . f)

quotT, remT :: (TensorScalar a, Integral a) => Expr a -> Expr a -> Expr a
quotT (Expr f) (Expr g) = Expr (\l -> mkBinOp BQuot (f l) (g l))
remT (Expr f) (Expr g) = Expr (\l -> mkBinOp BRem (f l) (g l))

--------------------------------------------------------------------------------
-- 3. Index Expressions & Dex-Style Tables
--------------------------------------------------------------------------------

-- | Dex-style table builder: @forIx (\\i -> body i)@.
forIx :: forall i a. (IxSet i) => (IxExpr i -> a) -> Tab i a
forIx = Tab (six @i)

-- | Dex-style table indexing: @t ! i@ (automatically β-fuses with 'forIx'!).
(!) :: Tab i a -> IxExpr i -> a
(!) = tabIndex
infixl 9 !

ixOrd :: IxExpr i -> Expr Int64
ixOrd ix = Expr (\_ -> mkOrd ix)

ixConst :: forall i. (IxSet i) => Int -> IxExpr i
ixConst c = IxConst c (six @i)

ixPair :: IxExpr i -> IxExpr j -> IxExpr ('IxProd i j)
ixPair = IxPair

ixFst :: IxExpr ('IxProd i j) -> IxExpr i
ixFst = IxFst

ixSnd :: IxExpr ('IxProd i j) -> IxExpr j
ixSnd = IxSnd

ixInLE :: forall i j. (IxSet j) => IxExpr i -> IxExpr ('IxSum i j)
ixInLE = IxInL (six @j)

ixInRE :: forall i j. (IxSet i) => IxExpr j -> IxExpr ('IxSum i j)
ixInRE = IxInR (six @i)

ixTagE :: forall s i. (KnownSymbol s) => IxExpr i -> IxExpr ('IxTagged s i)
ixTagE = IxTagE (Proxy @s)

ixUntagE :: IxExpr ('IxTagged s i) -> IxExpr i
ixUntagE = IxUntagE

ixFromOrd :: forall i. (IxSet i) => Expr Int64 -> IxExpr i
ixFromOrd (Expr f) = IxFromOrd (six @i) (f 0)

-- | Eliminator for coproduct index sets @IxSum i j@ (enables pure block concat
-- and piecewise padding without collective primitives!).
ixCase ::
  forall i j a.
  (IxSet i, IxSet j, TensorScalar a) =>
  IxExpr ('IxSum i j) ->
  (IxExpr i -> Expr a) ->
  (IxExpr j -> Expr a) ->
  Expr a
ixCase ix fL fR = Expr $ \lvl ->
  let sa = six @i
      sb = six @j
      o = ixOrd ix
      nA = ixSizeExpr sa
      isL = o .< nA
      ixL = IxFromOrd sa (unExpr (select isL o 0) lvl)
      ixR = IxFromOrd sb (unExpr (select isL 0 (o - nA)) lvl)
   in unExpr (select isL (fL ixL) (fR ixR)) lvl

ixSizeExpr :: SIx i -> Expr Int64
ixSizeExpr sh = Expr (\_ -> mkShapeSize sh)

inputNode :: forall i a. (IxSet i, TensorScalar a) => Text.Text -> TNode i a
inputNode nm = TNode (\_ -> mkInput nm (six @i))

inputTab :: forall i a. (IxSet i, TensorScalar a) => Text.Text -> Tab i (Expr a)
inputTab = fromNode . inputNode

-- | Explicitly share/materialize a 'Tab' into a named DAG node to prevent
-- recomputation when indexed multiple times.
letTab :: (TensorScalar a) => Tab i (Expr a) -> Tab i (Expr a)
letTab = fromNode . materialize

--------------------------------------------------------------------------------
-- 4. Structured Optic Views (`Reindex`)
--------------------------------------------------------------------------------

reindex :: (TensorScalar a) => IxMorph i j -> Tab j (Expr a) -> Tab i (Expr a)
reindex morph tab = fromNode (TNode (\l -> mkReindex morph (unTNode (materialize tab) l)))

reindexIso :: (TensorScalar a) => IxIso i j -> Tab j (Expr a) -> Tab i (Expr a)
reindexIso = reindex . MorphIso

reindexPrism :: (TensorScalar a) => IxPrism i j -> Tab j (Expr a) -> Tab i (Expr a)
reindexPrism = reindex . MorphPrism

reindexAffine :: (TensorScalar a) => IxAffine i j -> Tab j (Expr a) -> Tab i (Expr a)
reindexAffine = reindex . MorphAffine

transposeTab :: (IxSet i, IxSet j, TensorScalar a) => Tab ('IxProd i j) (Expr a) -> Tab ('IxProd j i) (Expr a)
transposeTab = reindexIso ixSwap

reshapeTab :: (IxSet i, IxSet j, TensorScalar a) => Tab j (Expr a) -> Tab i (Expr a)
reshapeTab = reindexIso ixReshape

reverseTab :: (IxSet i, TensorScalar a) => Tab i (Expr a) -> Tab i (Expr a)
reverseTab = reindexIso ixRev

sliceTab :: (IxSet i, IxSet j, TensorScalar a) => Int -> Tab j (Expr a) -> Tab i (Expr a)
sliceTab off = reindexPrism (ixSlice off)

strideTab :: (IxSet i, IxSet j, TensorScalar a) => Int -> Int -> Tab j (Expr a) -> Tab i (Expr a)
strideTab off st = reindexPrism (ixStride off st)

diagTab :: (IxSet i, TensorScalar a) => Tab ('IxProd i i) (Expr a) -> Tab i (Expr a)
diagTab = reindexPrism ixDiag

window1DTab ::
  (IxSet o, IxSet k, IxSet inp, TensorScalar a) =>
  Int ->
  Int ->
  Int ->
  Tab inp (Expr a) ->
  Tab ('IxProd o k) (Expr a)
window1DTab st dil pad = reindexAffine (ixWindow1D st dil pad)

concatTab ::
  (IxSet i, IxSet j, TensorScalar a) =>
  Tab i (Expr a) ->
  Tab j (Expr a) ->
  Tab ('IxSum i j) (Expr a)
concatTab tL tR = forIx $ \ix -> ixCase ix (tL !) (tR !)

--------------------------------------------------------------------------------
-- 5. Open-World Reductions (`TensorMonoid`)
--------------------------------------------------------------------------------

reduceIx ::
  forall op i a.
  (IxSet i, TensorMonoid op a) =>
  Proxy op ->
  (IxExpr i -> Expr a) ->
  Expr a
reduceIx _ bodyFn = Expr $ \lvl ->
  let sh = six @i
      body = unExpr (bodyFn (IxVar lvl sh)) (lvl + 1)
   in mkReduce (someMonoid @op @a) sh lvl body

sumIx :: (IxSet i, TensorMonoid AddOp a) => (IxExpr i -> Expr a) -> Expr a
sumIx = reduceIx (Proxy @AddOp)

prodIx :: (IxSet i, TensorMonoid MulOp a) => (IxExpr i -> Expr a) -> Expr a
prodIx = reduceIx (Proxy @MulOp)

maxIx :: (IxSet i, TensorMonoid MaxOp a) => (IxExpr i -> Expr a) -> Expr a
maxIx = reduceIx (Proxy @MaxOp)

minIx :: (IxSet i, TensorMonoid MinOp a) => (IxExpr i -> Expr a) -> Expr a
minIx = reduceIx (Proxy @MinOp)

logSumExpIx :: (IxSet i, TensorMonoid LogSumExpOp a) => (IxExpr i -> Expr a) -> Expr a
logSumExpIx = reduceIx (Proxy @LogSumExpOp)

anyIx :: (IxSet i) => (IxExpr i -> Expr Bool) -> Expr Bool
anyIx = reduceIx (Proxy @OrOp)

allIx :: (IxSet i) => (IxExpr i -> Expr Bool) -> Expr Bool
allIx = reduceIx (Proxy @AndOp)

--------------------------------------------------------------------------------
-- 6. Open-World Commutative-Monoid Scatter (`TensorCommMonoid`)
--------------------------------------------------------------------------------

-- | Generalized parallel scatter parameterized by ANY open-world
-- 'TensorCommMonoid'.
scatterM ::
  forall op i j a.
  (IxSet i, IxSet j, TensorCommMonoid op a) =>
  Proxy op ->
  (IxExpr j -> IxExpr i) ->
  (IxExpr j -> Expr a) ->
  Tab i (Expr a)
scatterM _ dstIxFn valFn = fromNode $ TNode $ \lvl ->
  let si = six @i
      sj = six @j
      ixVar = IxVar lvl sj
      dstIx = dstIxFn ixVar
      valE = unExpr (valFn ixVar) (lvl + 1)
   in mkScatter (someCommMonoid @op @a) si sj lvl dstIx valE

scatterAdd ::
  forall i j a.
  (IxSet i, IxSet j, TensorCommMonoid AddOp a) =>
  (IxExpr j -> IxExpr i) ->
  (IxExpr j -> Expr a) ->
  Tab i (Expr a)
scatterAdd = scatterM (Proxy @AddOp)

--------------------------------------------------------------------------------
-- 7. Sequential & Parallel Associative Scans
--------------------------------------------------------------------------------

-- | Sequential scan over @i@: @out[t] = step(carry_{t-1}, xs ! t)@ with
-- @carry_{-1} = initCarry@.
scanIx ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  Expr a ->
  Tab i (Expr a) ->
  (Expr a -> Expr a -> Expr a) ->
  Tab i (Expr a)
scanIx initE xs stepFn = fromNode $ TNode $ \lvl ->
  let sh = six @i
      cVar = lvl
      xVar = lvl + 1
      cExpr = Expr (\_ -> mkScalarVar cVar "scan_c")
      xExpr = Expr (\_ -> mkScalarVar xVar "scan_x")
      stepBody = unExpr (stepFn cExpr xExpr) (lvl + 2)
   in mkScan sh (unExpr initE lvl) (unTNode (materialize xs) lvl) cVar xVar stepBody

-- | Work-efficient parallel associative inclusive prefix scan over any
-- open-world 'TensorMonoid' (Blelloch tree scan on GPU, 3-phase Argobots
-- parallel scan on CPU).
assocScanIx ::
  forall op i a.
  (IxSet i, TensorMonoid op a) =>
  Proxy op ->
  Tab i (Expr a) ->
  Tab i (Expr a)
assocScanIx _ xs = fromNode $ TNode $ \lvl ->
  mkAssocScan (someMonoid @op @a) (six @i) (unTNode (materialize xs) lvl)

--------------------------------------------------------------------------------
-- 8. Open-World Backdoor Primitives & Metadata Annotations
--------------------------------------------------------------------------------

data HExprsDSL (ts :: [K.Type]) where
  HDNil :: HExprsDSL '[]
  HDCons :: (TensorScalar a) => !(Expr a) -> !(HExprsDSL ts) -> HExprsDSL (a ': ts)

evalHExprsDSL :: Int -> HExprsDSL ts -> HExprArgs ts
evalHExprsDSL _ HDNil = HANil
evalHExprsDSL l (HDCons (Expr f) rest) = HACons (f l) (evalHExprsDSL l rest)

data HNodesDSL (shapes :: [IxKind]) a where
  HTNil :: HNodesDSL '[] a
  HTCons :: !(Tab sh (Expr a)) -> !(HNodesDSL shs a) -> HNodesDSL (sh ': shs) a

evalHNodesDSL :: (TensorScalar a) => Int -> HNodesDSL shs a -> HNodeArgs shs a
evalHNodesDSL _ HTNil = HNNil
evalHNodesDSL l (HTCons t rest) = HNCons (unTNode (materialize t) l) (evalHNodesDSL l rest)

callScalarPrim ::
  (TensorScalar ret) =>
  ScalarPrimSpec args ret ->
  HExprsDSL args ->
  Expr ret
callScalarPrim spec args = Expr (\l -> mkScalarPrim spec (evalHExprsDSL l args))

callKernelPrim ::
  (TensorScalar a) =>
  KernelPrimSpec inShapes outShape a ->
  HNodesDSL inShapes a ->
  Tab outShape (Expr a)
callKernelPrim spec args = fromNode $ TNode (\l -> mkKernelPrimNode spec (evalHNodesDSL l args))

annotateExpr :: (TensorScalar a) => PrimAttrMap -> Expr a -> Expr a
annotateExpr attrs (Expr f) = Expr (mkAnnotateExpr attrs . f)

annotateTab :: (TensorScalar a) => PrimAttrMap -> Tab i (Expr a) -> Tab i (Expr a)
annotateTab attrs tab = fromNode $ TNode (\l -> mkAnnotateNode attrs (unTNode (materialize tab) l))
