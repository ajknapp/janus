{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Two algebraically stratified effectful builders for @Janus.Tensor@:
--
-- 1. 'TensorST': Haskell @ST@-style rank-2 locally stateful computations
--    ('runTensorST', 'runTensorSTScalar', 'TRef', 'MTab', 'forST_', 'whileST').
--    Pure and referentially transparent outside; supports arbitrary in-place
--    algorithms inside.
-- 2. 'AccumM': Dex-style write-only commutative-monoid accumulators
--    ('runAccum', 'accumWrite', 'parForAccum') parameterized by any open-world
--    'TensorCommMonoid'. Order-independent and 100% GPU/Argobots parallel!
module Janus.Tensor.ST
  ( -- * Rank-2 Locally Stateful `TensorST`
    TRef (..),
    MTab (..),
    TensorST,
    runTensorST,
    runTensorSTScalar,
    newTRef,
    readTRef,
    writeTRef,
    modifyTRef,
    newMTab,
    readMTab,
    writeMTab,
    freezeMTab,
    forST_,
    whileST,
    whenST,

    -- * Dex-Style Commutative-Monoid Accumulator `AccumM`
    AccumRef (..),
    AccumM,
    runAccum,
    runAccumNode,
    accumWrite,
    parForAccum,
    whenAccum,
  )
where

import Control.Monad.State.Strict (State, execState, gets, modify', runState)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Janus.Tensor.Algebra (TensorCommMonoid, TensorScalar, someCommMonoid)
import Janus.Tensor.IR
  ( AccumProgram (..),
    AccumStmt (..),
    Expr (..),
    IxExpr (..),
    RawExpr (..),
    STProgram (..),
    STStmt (..),
    TNode (..),
    Tab (..),
    fromNode,
    materialize,
    mkIRDigest,
    mkRunAccum,
    mkRunST,
    mkRunSTScalar,
    ixExprDigest,
  )
import Janus.Tensor.Index (IxKind, IxSet (..), SIx, sixDigest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB

encI :: Int -> BS.ByteString
encI n = BS.toStrict (BSB.toLazyByteString (BSB.int64LE (fromIntegral n)))

--------------------------------------------------------------------------------
-- 1. Rank-2 Locally Stateful `TensorST`
--------------------------------------------------------------------------------

newtype TRef s (a :: K.Type) = TRef {unTRef :: Int}

data MTab s (i :: IxKind) (a :: K.Type) = MTab
  { mtabId :: !Int,
    mtabShape :: !(SIx i)
  }

data STBuildState = STBuildState
  { sbNextId :: !Int,
    sbNextVar :: !Int,
    sbRevStmts :: ![STStmt]
  }

newtype TensorST s a = TensorST {unTensorST :: State STBuildState a}
  deriving newtype (Functor, Applicative, Monad)

emitST :: STStmt -> TensorST s ()
emitST stm = TensorST $ modify' (\st -> st {sbRevStmts = stm : sbRevStmts st})

allocSTId :: TensorST s Int
allocSTId = TensorST $ do
  i <- gets sbNextId
  modify' (\st -> st {sbNextId = i + 1})
  pure i

allocSTVar :: TensorST s Int
allocSTVar = TensorST $ do
  v <- gets sbNextVar
  modify' (\st -> st {sbNextVar = v + 1})
  pure v

currentSTVar :: TensorST s Int
currentSTVar = TensorST (gets sbNextVar)

newTRef :: (TensorScalar a) => Expr a -> TensorST s (TRef s a)
newTRef e = do
  rid <- allocSTId
  lvl <- currentSTVar
  emitST (STNewRef rid (unExpr e lvl))
  pure (TRef rid)

readTRef :: forall a s. (TensorScalar a) => TRef s a -> TensorST s (Expr a)
readTRef (TRef rid) = do
  snapId <- allocSTId
  emitST (STNewRef snapId (EReadSTRef (mkIRDigest "RdRef" [encI rid] []) rid :: RawExpr a))
  pure $ Expr $ \_ -> EReadSTRef (mkIRDigest "RdRef" [encI snapId] []) snapId

writeTRef :: (TensorScalar a) => TRef s a -> Expr a -> TensorST s ()
writeTRef (TRef rid) e = do
  lvl <- currentSTVar
  emitST (STWriteRef rid (unExpr e lvl))

modifyTRef :: (TensorScalar a) => TRef s a -> (Expr a -> Expr a) -> TensorST s ()
modifyTRef ref f = do
  v <- readTRef ref
  writeTRef ref (f v)

newMTab :: (TensorScalar a) => Tab i (Expr a) -> TensorST s (MTab s i a)
newMTab tab = do
  tid <- allocSTId
  lvl <- currentSTVar
  let node = unTNode (materialize tab) lvl
  emitST (STNewMTab tid (tabShape tab) node)
  pure (MTab tid (tabShape tab))

readMTab :: forall a s i. (TensorScalar a) => MTab s i a -> IxExpr i -> TensorST s (Expr a)
readMTab (MTab tid sh) ix = do
  snapId <- allocSTId
  emitST (STNewRef snapId (EReadMTab (mkIRDigest "RdMT" [encI tid, sixDigest sh] [ixExprDigest ix]) tid sh ix :: RawExpr a))
  pure $ Expr $ \_ -> EReadSTRef (mkIRDigest "RdRef" [encI snapId] []) snapId

writeMTab :: (TensorScalar a) => MTab s i a -> IxExpr i -> Expr a -> TensorST s ()
writeMTab (MTab tid sh) ix val = do
  lvl <- currentSTVar
  emitST (STWriteMTab tid sh ix (unExpr val lvl))

-- | Capture a handle to the final frozen table returned by 'runTensorST'.
freezeMTab :: MTab s i a -> TensorST s (MTab s i a)
freezeMTab = pure

forST_ :: forall i s. (IxSet i) => (IxExpr i -> TensorST s ()) -> TensorST s ()
forST_ bodyFn = do
  v <- allocSTVar
  let sh = six @i
  st0 <- TensorST $ gets id
  let (_, st1) = runState (unTensorST (bodyFn (IxVar v sh))) st0 {sbRevStmts = []}
  TensorST $ modify' (\st -> st {sbNextId = sbNextId st1, sbNextVar = sbNextVar st1})
  emitST (STFor sh v (reverse (sbRevStmts st1)))

whileST :: Expr Bool -> TensorST s () -> TensorST s ()
whileST cond body = do
  lvl <- currentSTVar
  st0 <- TensorST $ gets id
  let (_, st1) = runState (unTensorST body) st0 {sbRevStmts = []}
  TensorST $ modify' (\st -> st {sbNextId = sbNextId st1, sbNextVar = sbNextVar st1})
  emitST (STWhile (unExpr cond lvl) (reverse (sbRevStmts st1)))

whenST :: Expr Bool -> TensorST s () -> TensorST s ()
whenST cond body = do
  lvl <- currentSTVar
  st0 <- TensorST $ gets id
  let (_, st1) = runState (unTensorST body) st0 {sbRevStmts = []}
  TensorST $ modify' (\st -> st {sbNextId = sbNextId st1, sbNextVar = sbNextVar st1})
  emitST (STWhen (unExpr cond lvl) (reverse (sbRevStmts st1)))

-- | Execute a rank-2 locally stateful computation returning a pure immutable
-- table @Tab i (Expr a)@. The region skolem @s@ prevents any 'TRef' or 'MTab'
-- from escaping.
runTensorST ::
  forall i a.
  (IxSet i, TensorScalar a) =>
  (forall s. TensorST s (MTab s i a)) ->
  Tab i (Expr a)
runTensorST act =
  fromNode $ TNode $ \lvl ->
    let initSt = STBuildState {sbNextId = 0, sbNextVar = lvl, sbRevStmts = []}
        (MTab resId sh, finalSt) = runState (unTensorST act) initSt
        prog = STProgram {stStmts = reverse (sbRevStmts finalSt), stResult = resId}
     in mkRunST sh prog

-- | Execute a rank-2 locally stateful computation returning a pure scalar 'Expr a'.
runTensorSTScalar ::
  forall a.
  (TensorScalar a) =>
  (forall s. TensorST s (Expr a)) ->
  Expr a
runTensorSTScalar act =
  Expr $ \lvl ->
    let initSt = STBuildState {sbNextId = 0, sbNextVar = lvl, sbRevStmts = []}
        (resE, finalSt) = runState (unTensorST act) initSt
        prog = STProgram {stStmts = reverse (sbRevStmts finalSt), stResult = unExpr resE (sbNextVar finalSt)}
     in mkRunSTScalar prog

--------------------------------------------------------------------------------
-- 2. Dex-Style Commutative-Monoid Accumulator (`AccumM`)
--------------------------------------------------------------------------------

data AccumRef s op (i :: IxKind) (a :: K.Type) = AccumRef
  { accRefShape :: !(SIx i)
  }

data AccumBuildState i a = AccumBuildState
  { abNextVar :: !Int,
    abRevStmts :: ![AccumStmt i a]
  }

newtype AccumM s op (i :: IxKind) (a :: K.Type) r = AccumM
  { unAccumM :: State (AccumBuildState i a) r
  }
  deriving newtype (Functor, Applicative, Monad)

emitAccum :: AccumStmt i a -> AccumM s op i a ()
emitAccum stm = AccumM $ modify' (\st -> st {abRevStmts = stm : abRevStmts st})

accumWrite :: AccumRef s op i a -> IxExpr i -> Expr a -> AccumM s op i a ()
accumWrite _ ix val = do
  lvl <- AccumM (gets abNextVar)
  emitAccum (AccumWrite ix (unExpr val lvl))

parForAccum :: forall j s op i a. (IxSet j) => (IxExpr j -> AccumM s op i a ()) -> AccumM s op i a ()
parForAccum bodyFn = do
  st0 <- AccumM (gets id)
  let v = abNextVar st0
      sh = six @j
      st1 = execState (unAccumM (bodyFn (IxVar v sh))) st0 {abNextVar = v + 1, abRevStmts = []}
  AccumM $ modify' (\st -> st {abNextVar = abNextVar st1})
  emitAccum (AccumParFor sh v (reverse (abRevStmts st1)))

whenAccum :: Expr Bool -> AccumM s op i a () -> AccumM s op i a ()
whenAccum cond body = do
  st0 <- AccumM (gets id)
  let st1 = execState (unAccumM body) st0 {abRevStmts = []}
  AccumM $ modify' (\st -> st {abNextVar = abNextVar st1})
  emitAccum (AccumWhen (unExpr cond (abNextVar st0)) (reverse (abRevStmts st1)))

runAccumNode ::
  forall op i a.
  (IxSet i, TensorCommMonoid op a) =>
  Proxy op ->
  (forall s. AccumRef s op i a -> AccumM s op i a ()) ->
  TNode i a
runAccumNode _ bodyFn = TNode $ \lvl ->
  let sh = six @i
      ref = AccumRef sh
      initSt = AccumBuildState {abNextVar = lvl, abRevStmts = []}
      finalSt = execState (unAccumM (bodyFn ref)) initSt
      prog = AccumProgram (reverse (abRevStmts finalSt))
   in mkRunAccum (someCommMonoid @op @a) sh prog

runAccum ::
  forall op i a.
  (IxSet i, TensorCommMonoid op a) =>
  Proxy op ->
  (forall s. AccumRef s op i a -> AccumM s op i a ()) ->
  Tab i (Expr a)
runAccum p f = fromNode (runAccumNode p f)
