{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Data.SparseVec
  ( -- * Briggs-Torczon O(1) Dense-Packed Sparse Vector (Supports All StorageLayouts!)
    SparseVecSpine (..),
    SparseVec (..),
    SparseVecCtx,

    -- * Lifecycle
    new,
    withUniverse,
    free,

    -- * Queries & O(1) Clear
    size,
    universeCapacity,
    isEmpty,
    clear,
    ensureUniverse,

    -- * O(1) Lookup, Insert, Update, & Swap-Delete
    member,
    lookup,
    withLookup,
    insert,
    insertWith,
    delete,

    -- * Sparse Linear Algebra Kernels (`sparseDot`, `sparseAxpy`)
    sparseDot,
    sparseAxpy,

    -- * Packed Dense Streams, SPMD Gang Kernels, & Drains
    toStream,
    indicesS,
    valuesS,
    foreachScalarValuesSPMD,
    foreachSoAValuesSPMD,
    drainToSparseVec,
    fromStream,
    drainAppendSparseVec,
    drainAppendSparseVecWith,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import GHC.TypeNats (natVal)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, drainSum, runDrain)
import Janus.Control.Stream (Stream, filterMapS, iterateS, rangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( Layout (..),
    SPMD,
    SPMDGang,
    SPMDLayout,
    SPMDM,
    spmdAlignedUnitStride,
    uniform,
    uniformAlignedPtr,
    withSPMD,
    withSPMDMasked,
  )
import Janus.Typed (JanusTyped)
import Prelude hiding (lookup)

data SparseVecSpine a f = SparseVecSpine
  { svsSparse :: !(f (Ptr Int64)),
    svsDenseIdx :: !(f (Ptr Int64)),
    svsDenseVals :: !(f (CStruct (Buf a))),
    svsLen :: !(f Int64),
    svsUniverseCap :: !(f Int64),
    svsDenseCap :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable a, JanusStruct (Buf a)) => JanusStruct (SparseVecSpine a)

newtype SparseVec (e :: K.Type -> K.Type) a = SparseVec
  { unSparseVec :: e (Ptr (CStruct (SparseVecSpine a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (SparseVec Identity a)

deriving stock instance Show (SparseVec Identity a)

type SparseVecCtx m e a =
  ( LayoutCtx m e a,
    Typeable a,
    ExpPtr e Int64,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct (SparseVecSpine a)),
    JanusTyped e (CStruct (SparseVecSpine a))
  )

new ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  m (SparseVec e a)
new alloc = do
  hdr <- allocStruct @(CStruct (SparseVecSpine a)) alloc
  let f = projectPtrs hdr
  b0 <- allocBuf @a alloc 0
  poke (getPtrOf (svsSparse f)) nullPtr
  poke (getPtrOf (svsDenseIdx f)) nullPtr
  pokeBufStruct (getPtrOf (svsDenseVals f)) b0
  poke (getPtrOf (svsLen f)) 0
  poke (getPtrOf (svsUniverseCap f)) 0
  poke (getPtrOf (svsDenseCap f)) 0
  pure (SparseVec hdr)

withUniverse ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  e Int64 ->
  m (SparseVec e a)
withUniverse alloc uCap = do
  sv <- new alloc
  whenM_ (uCap `gt` 0) $
    ensureUniverse alloc sv (uCap - 1)
  pure sv

free ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  m ()
free alloc (SparseVec hdr) = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (svsUniverseCap f))
  whenM_ (uCap `gt` 0) $ do
    sp <- peek (getPtrOf (svsSparse f))
    freeTypedArray @Int64 alloc sp uCap
  dCap <- peek (getPtrOf (svsDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    di <- peek (getPtrOf (svsDenseIdx f))
    dv <- peekBufStruct (getPtrOf (svsDenseVals f))
    freeTypedArray @Int64 alloc di dCap
    freeBuf alloc dv dCap
  freeStruct alloc hdr

size :: (SparseVecCtx m e a) => SparseVec e a -> m (e Int64)
size (SparseVec hdr) = peek (getPtrOf (svsLen (projectPtrs hdr)))

universeCapacity :: (SparseVecCtx m e a) => SparseVec e a -> m (e Int64)
universeCapacity (SparseVec hdr) = peek (getPtrOf (svsUniverseCap (projectPtrs hdr)))

isEmpty :: (SparseVecCtx m e a) => SparseVec e a -> m (e Bool)
isEmpty sv = (`eq` 0) <$> size sv

-- | O(1) clear without zeroing @sparse@!
clear :: (SparseVecCtx m e a) => SparseVec e a -> m ()
clear (SparseVec hdr) = poke (getPtrOf (svsLen (projectPtrs hdr))) 0

ensureUniverse ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  e Int64 ->
  m ()
ensureUniverse alloc (SparseVec hdr) maxIdx = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (svsUniverseCap f))
  whenM_ (maxIdx `ge` uCap) $ do
    let c0 = ifThenElse (uCap `lt` 64) 64 (uCap * 2)
    (_, newUCap) <- runDrain (drainFind c0 (`gt` maxIdx)) (iterateS c0 (* 2))
    oldSp <- peek (getPtrOf (svsSparse f))
    newSp <- reallocTypedArray @Int64 alloc oldSp uCap newUCap
    poke (getPtrOf (svsSparse f)) newSp
    poke (getPtrOf (svsUniverseCap f)) newUCap

ensureDenseCap ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  e Int64 ->
  m ()
ensureDenseCap alloc (SparseVec hdr) reqCap = do
  let f = projectPtrs hdr
  dCap <- peek (getPtrOf (svsDenseCap f))
  curLen <- peek (getPtrOf (svsLen f))
  whenM_ (reqCap `gt` dCap) $ do
    let c0 = ifThenElse (dCap `lt` 16) 16 (dCap * 2)
    (_, newDCap) <- runDrain (drainFind c0 (`ge` reqCap)) (iterateS c0 (* 2))
    oldDI <- peek (getPtrOf (svsDenseIdx f))
    oldDV <- peekBufStruct (getPtrOf (svsDenseVals f))
    newDI <- reallocTypedArray @Int64 alloc oldDI dCap newDCap
    newDV <- reallocBuf alloc oldDV dCap newDCap curLen
    poke (getPtrOf (svsDenseIdx f)) newDI
    pokeBufStruct (getPtrOf (svsDenseVals f)) newDV
    poke (getPtrOf (svsDenseCap f)) newDCap

findDenseSlot ::
  forall a m e.
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  e Int64 ->
  m (e Bool, e Int64)
findDenseSlot (SparseVec hdr) idx = do
  let f = projectPtrs hdr
  uCap <- peek (getPtrOf (svsUniverseCap f))
  okRef <- newRef false
  dRef <- newRef (0 :: e Int64)
  whenM_ ((idx `ge` 0) `land` (idx `lt` uCap)) $ do
    curLen <- peek (getPtrOf (svsLen f))
    sp <- peek (getPtrOf (svsSparse f))
    d <- peekElemOff sp idx
    whenM_ ((d `ge` 0) `land` (d `lt` curLen)) $ do
      di <- peek (getPtrOf (svsDenseIdx f))
      backIdx <- peekElemOff di d
      whenM_ (backIdx `eq` idx) $ do
        writeRef okRef true
        writeRef dRef d
  (,) <$> readRef okRef <*> readRef dRef

member :: (SparseVecCtx m e a) => SparseVec e a -> e Int64 -> m (e Bool)
member sv idx = fst <$> findDenseSlot sv idx

lookup ::
  forall a s m e.
  (SparseVecCtx m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  SparseVec e a ->
  e Int64 ->
  m (e Bool, e s)
lookup defVal sv@(SparseVec hdr) idx = do
  (ok, d) <- findDenseSlot sv idx
  valRef <- newRef defVal
  whenM_ ok $ do
    dv <- peekBufStruct (getPtrOf (svsDenseVals (projectPtrs hdr)))
    readBuf dv d >>= writeRef valRef
  val <- readRef valRef
  pure (ok, val)

withLookup ::
  forall a m e.
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  e Int64 ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withLookup sv@(SparseVec hdr) idx onMiss onHit = do
  (ok, d) <- findDenseSlot sv idx
  ifThenElseM_
    ok
    ( do
        dv <- peekBufStruct (getPtrOf (svsDenseVals (projectPtrs hdr)))
        readBuf dv d >>= onHit
    )
    onMiss

insert ::
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  e Int64 ->
  Elem e a ->
  m ()
insert alloc sv idx val = insertWith alloc sv (\_ newV -> newV) idx val

insertWith ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  (Elem e a -> Elem e a -> Elem e a) ->
  e Int64 ->
  Elem e a ->
  m ()
insertWith alloc sv@(SparseVec hdr) combine idx val =
  whenM_ (idx `ge` 0) $ do
    (ok, d) <- findDenseSlot sv idx
    let f = projectPtrs hdr
    ifThenElseM_
      ok
      ( do
          dv <- peekBufStruct (getPtrOf (svsDenseVals f))
          oldV <- readBuf dv d
          writeBuf dv d (combine oldV val)
      )
      ( do
          ensureUniverse alloc sv idx
          curLen <- peek (getPtrOf (svsLen f))
          ensureDenseCap alloc sv (curLen + 1)
          sp <- peek (getPtrOf (svsSparse f))
          di <- peek (getPtrOf (svsDenseIdx f))
          dv <- peekBufStruct (getPtrOf (svsDenseVals f))
          pokeElemOff sp curLen idx
          pokeElemOff di idx curLen
          writeBuf dv curLen val
          poke (getPtrOf (svsLen f)) (curLen + 1)
      )

delete ::
  forall a m e.
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  e Int64 ->
  m (e Bool)
delete sv@(SparseVec hdr) idx = do
  (ok, d) <- findDenseSlot sv idx
  whenM_ ok $ do
    let f = projectPtrs hdr
    curLen <- peek (getPtrOf (svsLen f))
    sp <- peek (getPtrOf (svsSparse f))
    di <- peek (getPtrOf (svsDenseIdx f))
    dv <- peekBufStruct (getPtrOf (svsDenseVals f))
    let lastD = curLen - 1
    whenM_ (d `lt` lastD) $ do
      movedIdx <- peekElemOff di lastD
      movedVal <- readBuf dv lastD
      pokeElemOff di movedIdx d
      writeBuf dv d movedVal
      pokeElemOff sp d movedIdx
    poke (getPtrOf (svsLen f)) lastD
  pure ok

-- | O(min(nnz(x), nnz(y))) sparse inner product: iterates the sparser vector's packed
-- dense arrays and O(1)-probes the denser vector via Briggs-Torczon lookup!
sparseDot ::
  forall a s m e.
  (SparseVecCtx m e a, Elem e a ~ e s, JanusTyped e s, Num (e s)) =>
  SparseVec e a ->
  SparseVec e a ->
  m (e s)
sparseDot x@(SparseVec px) y@(SparseVec py) = do
  nx <- size x
  ny <- size y
  let wx = ptrToWord64Expr px
      wy = ptrToWord64Expr py
      smaller :: SparseVec e a
      smaller = SparseVec (word64ToPtrExpr (ifThenElse (nx `le` ny) wx wy))
      larger :: SparseVec e a
      larger = SparseVec (word64ToPtrExpr (ifThenElse (nx `le` ny) wy wx))
      prodS =
        filterMapS
          ( \(idx, vs) -> do
              (ok, vl) <- lookup 0 larger idx
              pure (ok, vs * vl)
          )
          (toStream smaller)
  runDrain drainSum prodS

-- | O(nnz(x)) sparse AXPY: @y[i] += alpha * x[i]@ for every non-zero entry @(i, x[i])@ of @x@.
sparseAxpy ::
  forall a s m e.
  (SparseVecCtx m e a, Elem e a ~ e s, Num (e s)) =>
  Alloc e ->
  e s ->
  SparseVec e a ->
  SparseVec e a ->
  m ()
sparseAxpy alloc alpha x y =
  runDrain
    (drainForEach (\(idx, vx) -> insertWith alloc y (+) idx (alpha * vx)))
    (toStream x)

toStream ::
  forall a m e.
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  Stream m (e Int64, Elem e a)
toStream (SparseVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (svsLen f)) <*> peek (getPtrOf (svsDenseIdx f)) <*> peekBufStruct (getPtrOf (svsDenseVals f)))
        (\(n, di, dv) -> streamM (rangeS 0 n) (\d -> (,) <$> peekElemOff di d <*> readBuf dv d))

indicesS ::
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  Stream m (e Int64)
indicesS (SparseVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (svsLen f)) <*> peek (getPtrOf (svsDenseIdx f)))
        (\(n, di) -> streamM (rangeS 0 n) (peekElemOff di))

valuesS ::
  (SparseVecCtx m e a) =>
  SparseVec e a ->
  Stream m (Elem e a)
valuesS (SparseVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (svsLen f)) <*> peekBufStruct (getPtrOf (svsDenseVals f)))
        (\(n, dv) -> streamM (rangeS 0 n) (readBuf dv))

-- | 64B-aligned SPMD strip-mined loop directly over the packed non-zero @denseVals@!
foreachScalarValuesSPMD ::
  forall w a m e.
  ( SparseVecCtx m e a,
    Buf a ~ ScalarBuf a,
    SPMDGang w m e,
    SPMDLayout w (Ptr a) ~ 'LPtr a,
    CmdStorable (SPMDM w m e) (SPMD w e) a
  ) =>
  Proxy w ->
  SparseVec e a ->
  (SPMD w e Int64 -> SPMD w e a -> SPMDM w m e (SPMD w e a)) ->
  m ()
foreachScalarValuesSPMD pw (SparseVec hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (svsLen f))
  ScalarBuf rawPtr <- peekBufStruct (getPtrOf (svsDenseVals f))
  let wE = fromIntegral (natVal pw) :: e Int64
      spPtr = uniformAlignedPtr rawPtr
  numFull <- letM (ifThenElse (n `gt` 0) (n `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = spmdAlignedUnitStride base
        withSPMD pw $ do
          cur <- peekElemOff spPtr idx
          nxt <- step idx cur
          pokeElemOff spPtr nxt idx
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` n) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform n) $ do
      cur <- peekElemOff spPtr tailIdx
      nxt <- step tailIdx cur
      pokeElemOff spPtr nxt tailIdx

foreachSoAValuesSPMD ::
  forall w s m e.
  ( SparseVecCtx m e (SoA s),
    SPMDGang w m e,
    Generic (s (PtrOf e)),
    Generic (s (SPMD w e)),
    GSoASPMD w m e (Rep (s (PtrOf e))) (Rep (s (SPMD w e)))
  ) =>
  Proxy w ->
  SparseVec e (SoA s) ->
  (SPMD w e Int64 -> s (SPMD w e) -> SPMDM w m e (s (SPMD w e))) ->
  m ()
foreachSoAValuesSPMD pw (SparseVec hdr) step = do
  let f = projectPtrs hdr
  n <- peek (getPtrOf (svsLen f))
  b <- peekBufStruct (getPtrOf (svsDenseVals f))
  foreachSoASPMD pw b n step

drainToSparseVec ::
  forall a m e.
  (SparseVecCtx m e a) =>
  Alloc e ->
  Drain m (e Int64, Elem e a) (SparseVec e a)
drainToSparseVec alloc = Drain $ \s -> do
  sv <- new alloc
  runDrain (drainAppendSparseVec alloc sv) s
  pure sv

fromStream ::
  (SparseVecCtx m e a) =>
  Alloc e ->
  Stream m (e Int64, Elem e a) ->
  m (SparseVec e a)
fromStream alloc = runDrain (drainToSparseVec alloc)

drainAppendSparseVec ::
  (SparseVecCtx m e a) =>
  Alloc e ->
  SparseVec e a ->
  Drain m (e Int64, Elem e a) ()
drainAppendSparseVec alloc sv = drainForEach (\(idx, v) -> insert alloc sv idx v)

drainAppendSparseVecWith ::
  (SparseVecCtx m e a) =>
  Alloc e ->
  (Elem e a -> Elem e a -> Elem e a) ->
  SparseVec e a ->
  Drain m (e Int64, Elem e a) ()
drainAppendSparseVecWith alloc combine sv =
  drainForEach (\(idx, v) -> insertWith alloc sv combine idx v)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type SparseVecABI a = (Typeable a, JanusStruct (Buf a))

type instance JanusCEval (SparseVec JanusC a -> r) = SparseVec Identity a -> JanusCEval r

type instance JanusCRetType (SparseVec JanusC a -> r) = JanusCRetType r

instance (SparseVecABI a, JanusCParam r) => JanusCParam (SparseVec JanusC a -> r) where
  jcparam = mkPtrHandleJCParam SparseVec

instance (SparseVecABI a, JanusCEvaluate r) => JanusCEvaluate (SparseVec JanusC a -> r) where
  jceval = mkPtrHandleJCEval unSparseVec SparseVec

type instance JanusCEval (JanusCM (SparseVec JanusC a)) = IO (SparseVec Identity a)

type instance JanusCRetType (JanusCM (SparseVec JanusC a)) = SparseVec Identity a

instance (SparseVecABI a) => JanusCParam (JanusCM (SparseVec JanusC a)) where
  jcparam = mkPtrHandleJCRetParam unSparseVec

instance (SparseVecABI a) => JanusCEvaluate (JanusCM (SparseVec JanusC a)) where
  jceval = mkPtrHandleJCRetEval SparseVec

type instance LinuxX86_64CEval (SparseVec LinuxX86_64C a -> r) = SparseVec Identity a -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (SparseVec LinuxX86_64C a -> r) = LinuxX86_64BaseRet r

instance (SparseVecABI a, LinuxX86_64Param r) => LinuxX86_64Param (SparseVec LinuxX86_64C a -> r) where
  x86param = mkPtrHandleX86Param SparseVec

instance (SparseVecABI a, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (SparseVec LinuxX86_64C a -> r) where
  x86eval = mkPtrHandleX86EvalParam unSparseVec SparseVec

instance (SparseVecABI a, X86DeferCont r) => X86DeferCall (SparseVec LinuxX86_64C a -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unSparseVec SparseVec

instance (SparseVecABI a, X86DeferCont r) => X86DeferCont (SparseVec LinuxX86_64C a -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unSparseVec SparseVec

type instance LinuxX86_64CEval (LinuxX86_64CM (SparseVec LinuxX86_64C a)) = IO (SparseVec Identity a)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (SparseVec LinuxX86_64C a)) = SparseVec Identity a

instance (SparseVecABI a) => LinuxX86_64Param (LinuxX86_64CM (SparseVec LinuxX86_64C a)) where
  x86param = mkPtrHandleX86RetParam unSparseVec

instance (SparseVecABI a) => LinuxX86_64Evaluate (LinuxX86_64CM (SparseVec LinuxX86_64C a)) where
  x86eval = mkPtrHandleX86RetEval SparseVec

instance (SparseVecABI a) => X86DeferCall (LinuxX86_64CM (SparseVec LinuxX86_64C a)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg SparseVec

instance (SparseVecABI a) => X86DeferCont (LinuxX86_64CM (SparseVec LinuxX86_64C a)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont SparseVec

instance (SparseVecABI a, LinuxX86_64Param r, JanusCTH r) => JanusCTH (SparseVec LinuxX86_64C a -> r) where
  type JanusCTHEval (SparseVec LinuxX86_64C a -> r) = SparseVec Identity a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(SparseVec Identity a)) : ps, retSpec)

instance (SparseVecABI a) => JanusCTH (LinuxX86_64CM (SparseVec LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (SparseVec LinuxX86_64C a)) = IO (SparseVec Identity a)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(SparseVec Identity a)) (Proxy @(CStruct (SparseVecSpine a))))
