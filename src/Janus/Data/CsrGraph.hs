{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

module Janus.Data.CsrGraph
  ( -- * Pointer-Free Compressed Sparse Row Graph Spine & Handle
    CsrGraphSpine (..),
    CsrGraph (..),
    CsrGraphCtx,

    -- * Bulk Stream Construction & Lifecycle
    drainToCsrGraph,
    fromEdgeStream,
    free,

    -- * Graph Queries & Adjacency Streams
    numVertices,
    numEdges,
    outDegree,
    neighborsS,
    outEdgesS,
    allEdgesS,

    -- * Graph Algorithms & Sparse Linear Algebra
    bfsDistances,
    csrSpMV,

    -- * First-Class SPMD / SIMD Edge Kernels, Queries, & SpMV
    foreachEdgeValuesSPMD,
    foldEdgeValuesSPMD,
    outDegreeSPMD,
    spmdCsrSpMV,
    spmdCsrSpMVVec,
  )
where

import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    allocTypedArray,
    freeStruct,
    freeTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFoldM, drainForEach, drainSum, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, rangeS, statefulS, streamM, unfoldWhileM, withStreamSetup)
import Janus.Data.DynVec (DynVec)
import qualified Janus.Data.DynVec as DV
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDLayout,
    SPMDM,
    SPMDReduceByLayout,
    SPMDTyped,
    gangReduceAdd,
    liftSPMD,
    uniform,
    uniformAlignedPtr,
  )
import Janus.Typed (JanusTyped)

-- | Spine struct for pointer-free Compressed Sparse Row ('CsrGraph') directed weighted graph:
-- * @cgsRowOffsets@: @cgsNumVerts + 1@ entries mapping vertex @u@ to edge slice @[ro[u], ro[u + 1])@.
-- * @cgsColTargets@: @cgsNumEdges@ destination vertex indices.
-- * @cgsWeights@: @cgsNumEdges@ edge weights stored in layout @Buf w@ (@Scalar@, @AoS@, @SoA@, etc.).
data CsrGraphSpine w f = CsrGraphSpine
  { cgsRowOffsets :: !(f (Ptr Int64)),
    cgsColTargets :: !(f (Ptr Int64)),
    cgsWeights :: !(f (CStruct (Buf w))),
    cgsNumVerts :: !(f Int64),
    cgsNumEdges :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable w, JanusStruct (Buf w)) => JanusStruct (CsrGraphSpine w)

newtype CsrGraph (e :: K.Type -> K.Type) w = CsrGraph
  { unCsrGraph :: e (Ptr (CStruct (CsrGraphSpine w)))
  }
  deriving stock (Generic)

deriving stock instance Eq (CsrGraph Identity w)

deriving stock instance Show (CsrGraph Identity w)

type CsrGraphCtx m e w =
  ( LayoutCtx m e w,
    Typeable w,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct (CsrGraphSpine w)),
    JanusTyped e (CStruct (CsrGraphSpine w)),
    DV.DynVecCtx m e Int64,
    DV.DynVecCtx m e w
  )

-- | Bulk-construct a 'CsrGraph' over @numVerts@ vertices from a 'Stream' of @(u, v, weight)@
-- directed edges (ignoring out-of-range edges outside @[0, numVerts)@).
-- All temporary edge staging buffers and @writeHeads@ scratch storage are allocated and freed via @alloc@.
drainToCsrGraph ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  Alloc e ->
  e Int64 ->
  Drain m (e Int64, e Int64, Elem e w) (CsrGraph e w)
drainToCsrGraph alloc rawNumVerts = Drain $ \edgeStream -> do
  nv <- letM (ifThenElse (rawNumVerts `le` 0) 0 rawNumVerts)
  usVec <- DV.new @Int64 alloc
  vsVec <- DV.new @Int64 alloc
  wsVec <- DV.new @w alloc

  runDrain
    ( drainForEach $ \(u, v, w) ->
        whenM_ ((u `ge` 0) `land` (u `lt` nv) `land` (v `ge` 0) `land` (v `lt` nv)) $ do
          DV.push alloc usVec u
          DV.push alloc vsVec v
          DV.push alloc wsVec w
    )
    edgeStream

  ne <- DV.len usVec
  rowOffsets <- allocTypedArray @Int64 alloc (nv + 1)
  runDrain (drainForEach (\i -> pokeElemOff rowOffsets 0 i)) (rangeS 0 (nv + 1))

  colTargets <- allocTypedArray @Int64 alloc ne
  weightsBuf <- allocBuf @w alloc ne

  whenM_ (ne `gt` 0) $ do
    usBuf <- DV.buf usVec
    vsBuf <- DV.buf vsVec
    wsBuf <- DV.buf wsVec

    -- 1. Count out-degrees per vertex
    runDrain
      ( drainForEach $ \ei -> do
          u <- readBuf usBuf ei
          deg <- peekElemOff rowOffsets u
          pokeElemOff rowOffsets (deg + 1) u
      )
      (rangeS 0 ne)

    -- 2. Exclusive prefix sum over rowOffsets[0 .. nv]
    totalEdges <-
      runDrain
        ( drainFoldM (0 :: e Int64) $ \running u -> do
            cnt <- peekElemOff rowOffsets u
            pokeElemOff rowOffsets running u
            pure (running + cnt)
        )
        (rangeS 0 nv)
    pokeElemOff rowOffsets totalEdges nv

    -- 3. Scatter (v, w) into colTargets & weightsBuf using temporary writeHeads
    writeHeads <- allocTypedArray @Int64 alloc nv
    cmdMemcpy (toVoidPtr writeHeads) (toVoidPtr rowOffsets) (nv * sizeOf (Proxy @Int64))
    runDrain
      ( drainForEach $ \ei -> do
          u <- readBuf usBuf ei
          v <- readBuf vsBuf ei
          w <- readBuf wsBuf ei
          pos <- peekElemOff writeHeads u
          pokeElemOff writeHeads (pos + 1) u
          pokeElemOff colTargets v pos
          writeBuf weightsBuf pos w
      )
      (rangeS 0 ne)
    freeTypedArray @Int64 alloc writeHeads nv

  DV.free alloc wsVec
  DV.free alloc vsVec
  DV.free alloc usVec

  hdr <- allocStruct @(CStruct (CsrGraphSpine w)) alloc
  let fields = projectPtrs hdr
  poke (getPtrOf (cgsRowOffsets fields)) rowOffsets
  poke (getPtrOf (cgsColTargets fields)) colTargets
  pokeBufStruct (getPtrOf (cgsWeights fields)) weightsBuf
  poke (getPtrOf (cgsNumVerts fields)) nv
  poke (getPtrOf (cgsNumEdges fields)) ne
  pure (CsrGraph hdr)

fromEdgeStream ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  Alloc e ->
  e Int64 ->
  Stream m (e Int64, e Int64, Elem e w) ->
  m (CsrGraph e w)
fromEdgeStream alloc nv = runDrain (drainToCsrGraph alloc nv)

-- | Free CSR row offsets, column targets, edge weights buffer, and spine struct via @alloc@.
free ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  Alloc e ->
  CsrGraph e w ->
  m ()
free alloc (CsrGraph hdr) = do
  let fields = projectPtrs hdr
  nv <- peek (getPtrOf (cgsNumVerts fields))
  ne <- peek (getPtrOf (cgsNumEdges fields))
  ro <- peek (getPtrOf (cgsRowOffsets fields))
  freeTypedArray @Int64 alloc ro (nv + 1)
  whenM_ (ne `gt` 0) $ do
    ct <- peek (getPtrOf (cgsColTargets fields))
    wb <- peekBufStruct (getPtrOf (cgsWeights fields))
    freeTypedArray @Int64 alloc ct ne
    freeBuf alloc wb ne
  freeStruct alloc hdr

numVertices :: (CsrGraphCtx m e w) => CsrGraph e w -> m (e Int64)
numVertices (CsrGraph hdr) = peek (getPtrOf (cgsNumVerts (projectPtrs hdr)))

numEdges :: (CsrGraphCtx m e w) => CsrGraph e w -> m (e Int64)
numEdges (CsrGraph hdr) = peek (getPtrOf (cgsNumEdges (projectPtrs hdr)))

-- | O(1) out-degree of vertex @u@ (returns @0@ if @u@ is out of bounds).
outDegree ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  CsrGraph e w ->
  e Int64 ->
  m (e Int64)
outDegree (CsrGraph hdr) u = do
  let fields = projectPtrs hdr
  nv <- peek (getPtrOf (cgsNumVerts fields))
  degRef <- newRef (0 :: e Int64)
  whenM_ ((u `ge` 0) `land` (u `lt` nv)) $ do
    ro <- peek (getPtrOf (cgsRowOffsets fields))
    sOff <- peekElemOff ro u
    eOff <- peekElemOff ro (u + 1)
    writeRef degRef (eOff - sOff)
  readRef degRef

-- | Contiguous unit-stride 'Stream' of outgoing neighbor vertex indices from @u@.
neighborsS ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  CsrGraph e w ->
  e Int64 ->
  Stream m (e Int64)
neighborsS (CsrGraph hdr) u =
  let fields = projectPtrs hdr
   in withStreamSetup
        ( do
            nv <- peek (getPtrOf (cgsNumVerts fields))
            ct <- peek (getPtrOf (cgsColTargets fields))
            sRef <- newRef (0 :: e Int64)
            eRef <- newRef (0 :: e Int64)
            whenM_ ((u `ge` 0) `land` (u `lt` nv)) $ do
              ro <- peek (getPtrOf (cgsRowOffsets fields))
              peekElemOff ro u >>= writeRef sRef
              peekElemOff ro (u + 1) >>= writeRef eRef
            (,,) <$> readRef sRef <*> readRef eRef <*> pure ct
        )
        (\(sOff, eOff, ct) -> streamM (rangeS sOff eOff) (peekElemOff ct))

-- | Contiguous 'Stream' of outgoing @(targetVertex, weight)@ pairs from @u@.
outEdgesS ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  CsrGraph e w ->
  e Int64 ->
  Stream m (e Int64, Elem e w)
outEdgesS (CsrGraph hdr) u =
  let fields = projectPtrs hdr
   in withStreamSetup
        ( do
            nv <- peek (getPtrOf (cgsNumVerts fields))
            ct <- peek (getPtrOf (cgsColTargets fields))
            wb <- peekBufStruct (getPtrOf (cgsWeights fields))
            sRef <- newRef (0 :: e Int64)
            eRef <- newRef (0 :: e Int64)
            whenM_ ((u `ge` 0) `land` (u `lt` nv)) $ do
              ro <- peek (getPtrOf (cgsRowOffsets fields))
              peekElemOff ro u >>= writeRef sRef
              peekElemOff ro (u + 1) >>= writeRef eRef
            (,,,) <$> readRef sRef <*> readRef eRef <*> pure ct <*> pure wb
        )
        ( \(sOff, eOff, ct, wb) ->
            streamM (rangeS sOff eOff) (\ei -> (,) <$> peekElemOff ct ei <*> readBuf wb ei)
        )

-- | Single-step pull 'Stream' of all @(u, v, weight)@ edges in CSR row order.
allEdgesS ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  CsrGraph e w ->
  Stream m (e Int64, e Int64, Elem e w)
allEdgesS (CsrGraph hdr) =
  let fields = projectPtrs hdr
   in statefulS
        ( do
            ne <- peek (getPtrOf (cgsNumEdges fields))
            ro <- peek (getPtrOf (cgsRowOffsets fields))
            ct <- peek (getPtrOf (cgsColTargets fields))
            wb <- peekBufStruct (getPtrOf (cgsWeights fields))
            uRef <- newRef (0 :: e Int64)
            eiRef <- newRef (0 :: e Int64)
            pure (ne, ro, ct, wb, uRef, eiRef)
        )
        ( \(ne, ro, ct, wb, uRef, eiRef) -> ContT $ \stepK -> do
            ei <- readRef eiRef
            ifThenElseM_
              (ei `ge` ne)
              (stepK Stop)
              ( do
                  u <- readRef uRef
                  uEnd <- peekElemOff ro (u + 1)
                  ifThenElseM_
                    (ei `ge` uEnd)
                    (writeRef uRef (u + 1) >> stepK Skip)
                    ( do
                        v <- peekElemOff ct ei
                        w <- readBuf wb ei
                        writeRef eiRef (ei + 1)
                        stepK (Yield (u, v, w))
                    )
              )
        )

-- | O(V + E) Breadth-First Search hop distances from @src@, returning a 'DynVec' of length
-- @numVertices@ with @-1@ for unreachable vertices and @>= 0@ hop counts for reachable vertices.
-- Allocates and frees its temporary FIFO queue strictly through @alloc@.
bfsDistances ::
  forall w m e.
  (CsrGraphCtx m e w) =>
  Alloc e ->
  CsrGraph e w ->
  e Int64 ->
  m (DynVec e Int64)
bfsDistances alloc g@(CsrGraph hdr) src = do
  nv <- numVertices g
  distVec <- DV.withCapacity @Int64 alloc nv
  runDrain (drainForEach (\_ -> DV.push alloc distVec (-1))) (rangeS 0 nv)
  whenM_ ((src `ge` 0) `land` (src `lt` nv)) $ do
    let fields = projectPtrs hdr
    ro <- peek (getPtrOf (cgsRowOffsets fields))
    ct <- peek (getPtrOf (cgsColTargets fields))
    distBuf <- DV.buf distVec
    queue <- allocTypedArray @Int64 alloc nv
    writeBuf distBuf src 0
    pokeElemOff queue src 0
    qTailRef <- newRef (1 :: e Int64)
    runDrain drain_ $
      unfoldWhileM
        (0 :: e Int64)
        (\qHead -> (qHead `lt`) <$> readRef qTailRef)
        ( \qHead -> do
            u <- peekElemOff queue qHead
            du <- readBuf distBuf u
            eStart <- peekElemOff ro u
            eEnd <- peekElemOff ro (u + 1)
            runDrain
              ( drainForEach $ \ei -> do
                  v <- peekElemOff ct ei
                  dv <- readBuf distBuf v
                  whenM_ (dv `lt` 0) $ do
                    writeBuf distBuf v (du + 1)
                    qt <- readRef qTailRef
                    pokeElemOff queue v qt
                    writeRef qTailRef (qt + 1)
              )
              (rangeS eStart eEnd)
            pure (qHead + 1, ())
        )
    freeTypedArray @Int64 alloc queue nv
  pure distVec

-- | Sparse matrix-vector product @y[u] = sum_{(u, v, w)} w * x[v]@ over @[0, numVertices)@.
csrSpMV ::
  forall w s m e.
  (CsrGraphCtx m e w, Elem e w ~ e s, CmdStorable m e s, JanusTyped e s, Num (e s)) =>
  CsrGraph e w ->
  e (Ptr s) ->
  e (Ptr s) ->
  m ()
csrSpMV (CsrGraph hdr) xPtr yPtr = do
  let fields = projectPtrs hdr
  nv <- peek (getPtrOf (cgsNumVerts fields))
  ro <- peek (getPtrOf (cgsRowOffsets fields))
  ct <- peek (getPtrOf (cgsColTargets fields))
  wb <- peekBufStruct (getPtrOf (cgsWeights fields))
  runDrain
    ( drainForEach $ \u -> do
        eStart <- peekElemOff ro u
        eEnd <- peekElemOff ro (u + 1)
        acc <-
          runDrain
            drainSum
            ( streamM (rangeS eStart eEnd) $ \ei -> do
                v <- peekElemOff ct ei
                w <- readBuf wb ei
                xv <- peekElemOff xPtr v
                pure (w * xv)
            )
        pokeElemOff yPtr acc u
    )
    (rangeS 0 nv)

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Edge Kernels, Queries, & SpMV
--------------------------------------------------------------------------------

-- | Strip-mine all @numEdges@ contiguous edge weights in-place at SIMD gang width @w@.
foreachEdgeValuesSPMD ::
  forall w v m e.
  (CsrGraphCtx m e v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  CsrGraph e v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachEdgeValuesSPMD pw (CsrGraph hdr) step = do
  let fields = projectPtrs hdr
  ne <- peek (getPtrOf (cgsNumEdges fields))
  wb <- peekBufStruct (getPtrOf (cgsWeights fields))
  foreachBufSPMD pw True wb ne step

-- | Masked-safe SIMD fold over all @numEdges@ contiguous edge weights at gang width @w@.
foldEdgeValuesSPMD ::
  forall w v s m e.
  (CsrGraphCtx m e v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  CsrGraph e v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldEdgeValuesSPMD pw (CsrGraph hdr) s0 step = do
  let fields = projectPtrs hdr
  ne <- peek (getPtrOf (cgsNumEdges fields))
  wb <- peekBufStruct (getPtrOf (cgsWeights fields))
  foldBufSPMD pw True wb ne s0 step

-- | Bounds-masked O(1) SIMD gather of vertex out-degrees @rowOffsets[u + 1] - rowOffsets[u]@
-- across @w@ lanes (returning @0@ for out-of-bounds vertex indices).
outDegreeSPMD ::
  forall w v m e.
  (CsrGraphCtx m e v, SPMDGang w m e) =>
  CsrGraph e v ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Int64)
outDegreeSPMD (CsrGraph hdr) u = do
  let fields = projectPtrs hdr
  nv <- liftSPMD (peek (getPtrOf (cgsNumVerts fields)))
  degRef <- newRef (0 :: SPMD w e Int64)
  let valid = (u `ge` 0) `land` (u `lt` uniform nv)
  whenM_ valid $ do
    ro <- liftSPMD (peek (getPtrOf (cgsRowOffsets fields)))
    let spRo = uniformAlignedPtr ro
    sOff <- peekElemOff spRo u
    eOff <- peekElemOff spRo (u + 1)
    writeRef degRef (eOff - sOff)
  readRef degRef

-- | SIMD strip-mined sparse matrix-vector product @y[u] = sum_{(u, v, w)} w * x[v]@
-- over @[0, numVertices)@, matching 'csrSpMV''s pointer interface.
spmdCsrSpMV ::
  forall w s m e.
  ( CsrGraphCtx m e s,
    Buf s ~ ScalarBuf s,
    LayoutSliceSPMD w m e s,
    LayoutGatherSPMD w m e s,
    Elem e s ~ e s,
    Elem (SPMD w e) s ~ SPMD w e s,
    JanusTyped e s,
    SPMDTyped w e s,
    SPMDReduceByLayout (SPMDLayout w s) w e s,
    Num (e s),
    Num (SPMD w e s)
  ) =>
  Proxy w ->
  CsrGraph e s ->
  e (Ptr s) ->
  e (Ptr s) ->
  m ()
spmdCsrSpMV pw (CsrGraph hdr) xPtr yPtr = do
  let fields = projectPtrs hdr
      xBuf = ScalarBuf xPtr :: Buf s e
      yBuf = ScalarBuf yPtr :: Buf s e
  nv <- peek (getPtrOf (cgsNumVerts fields))
  ro <- peek (getPtrOf (cgsRowOffsets fields))
  ct <- peek (getPtrOf (cgsColTargets fields))
  wb <- peekBufStruct (getPtrOf (cgsWeights fields))
  let spColIdx = uniformAlignedPtr ct
  runDrain
    ( drainForEach $ \u -> do
        rowStart <- peekElemOff ro u
        rowEnd <- peekElemOff ro (u + 1)
        let rowLen = rowEnd - rowStart
        rowAcc <-
          foldSliceBufSPMD
            pw
            False
            wb
            rowStart
            rowLen
            rowStart
            (0 :: SPMD w e s)
            ( \acc edgeIdx wVal -> do
                col <- peekElemOff spColIdx edgeIdx
                xv <- gatherBufSPMD xBuf col
                pure (acc + wVal * xv)
            )
        writeBuf yBuf u (gangReduceAdd rowAcc)
    )
    (rangeS 0 nv)

-- | 'DynVec'-backed variant of 'spmdCsrSpMV' writing @y[u] = sum_{(u, v, w)} w * x[v]@
-- over @[0, numVertices)@.
spmdCsrSpMVVec ::
  forall w s m e.
  ( CsrGraphCtx m e s,
    LayoutSliceSPMD w m e s,
    LayoutGatherSPMD w m e s,
    Elem e s ~ e s,
    Elem (SPMD w e) s ~ SPMD w e s,
    JanusTyped e s,
    SPMDTyped w e s,
    SPMDReduceByLayout (SPMDLayout w s) w e s,
    Num (e s),
    Num (SPMD w e s)
  ) =>
  Proxy w ->
  CsrGraph e s ->
  DynVec e s ->
  DynVec e s ->
  m ()
spmdCsrSpMVVec pw (CsrGraph hdr) xVec yVec = do
  let fields = projectPtrs hdr
  nv <- peek (getPtrOf (cgsNumVerts fields))
  ro <- peek (getPtrOf (cgsRowOffsets fields))
  ct <- peek (getPtrOf (cgsColTargets fields))
  wb <- peekBufStruct (getPtrOf (cgsWeights fields))
  xBuf <- DV.buf xVec
  yBuf <- DV.buf yVec
  let spColIdx = uniformAlignedPtr ct
  runDrain
    ( drainForEach $ \u -> do
        rowStart <- peekElemOff ro u
        rowEnd <- peekElemOff ro (u + 1)
        let rowLen = rowEnd - rowStart
        rowAcc <-
          foldSliceBufSPMD
            pw
            False
            wb
            rowStart
            rowLen
            rowStart
            (0 :: SPMD w e s)
            ( \acc edgeIdx wVal -> do
                col <- peekElemOff spColIdx edgeIdx
                xv <- gatherBufSPMD xBuf col
                pure (acc + wVal * xv)
            )
        writeBuf yBuf u (gangReduceAdd rowAcc)
    )
    (rangeS 0 nv)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type CsrGraphABI w = (Typeable w, JanusStruct (Buf w))

type instance JanusCHost (CsrGraph (JanusCT _) w) = CsrGraph Identity w

deriving via
  ViaPtrHandle t (CStruct (CsrGraphSpine w)) (CsrGraph Identity w)
  instance
    (CsrGraphABI w) => JanusCArg t (CsrGraph (JanusCT t) w)

deriving via
  ViaPtrHandle t (CStruct (CsrGraphSpine w)) (CsrGraph Identity w)
  instance
    (CsrGraphABI w) => JanusCValRet t (CsrGraph (JanusCT t) w)
