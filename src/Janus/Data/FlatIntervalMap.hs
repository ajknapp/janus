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

module Janus.Data.FlatIntervalMap
  ( -- * Implicit Augmented Flat Interval Map Spine & Handle
    FlatIntervalMapSpine (..),
    FlatIntervalMap (..),
    FlatIntervalMapCtx,

    -- * Bulk Stream Construction & Lifecycle
    drainToFlatIntervalMap,
    fromStream,
    free,

    -- * Structural Queries
    size,
    isEmpty,
    maxLevel,

    -- * Zero-Allocation Overlap, Point-Stabbing, & Sequential Streams
    overlappingStream,
    pointStabStream,
    countOverlapping,
    anyOverlapping,
    toStream,

    -- * First-Class SPMD / SIMD Value Kernels & Overlap Counting
    foreachValuesSPMD,
    foldValuesSPMD,
    spmdCountOverlapping,
    spmdCountStabbed,
  )
where

import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import GHC.TypeNats (natVal)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    allocTypedArray,
    freeStruct,
    freeTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain
  ( Drain (..),
    drainAny,
    drainFind,
    drainFoldM,
    drainForEach,
    drainLength,
    drain_,
    runDrain,
  )
import Janus.Control.Step (Step (..))
import Janus.Control.Stream
  ( Stream,
    rangeS,
    reverseRangeS,
    statefulS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import qualified Janus.Data.DynVec as DV
import Janus.Data.FlatMap (OrdKey (..), OrdLayoutCtx)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDM,
    SPMDTyped,
    gangReduceAdd,
    spmdAlignedUnitStride,
    uniform,
    withSPMD,
    withSPMDMasked,
  )

-- | Spine struct for pointer-free implicit augmented interval map ('FlatIntervalMap').
--
-- Stores @fimsLen@ closed intervals @[lo, hi]@ (with @lo <= hi@) sorted ascending by @(lo, hi)@
-- in separate SoA layout buffers (@fimsLo@, @fimsHi@, @fimsMaxHi@, @fimsVals@).
-- Every subrange @[l, r)@ of @[0, fimsLen)@ is rooted at its midpoint @m = (l + r) >> 1@,
-- with @fimsMaxHi[m]@ storing the maximum @hi@ across @[l, r)@ for O(log N + K) overlap pruning.
data FlatIntervalMapSpine k v f = FlatIntervalMapSpine
  { fimsLo :: !(f (CStruct (Buf k))),
    fimsHi :: !(f (CStruct (Buf k))),
    fimsMaxHi :: !(f (CStruct (Buf k))),
    fimsVals :: !(f (CStruct (Buf v))),
    fimsLen :: !(f Int64),
    fimsMaxLevel :: !(f Int64)
  }
  deriving stock (Generic)

instance
  (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v)) =>
  JanusStruct (FlatIntervalMapSpine k v)

newtype FlatIntervalMap (e :: K.Type -> K.Type) k v = FlatIntervalMap
  { unFlatIntervalMap :: e (Ptr (CStruct (FlatIntervalMapSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (FlatIntervalMap Identity k v)

deriving stock instance Show (FlatIntervalMap Identity k v)

type FlatIntervalMapCtx m e k v = (OrdLayoutCtx m e k, LayoutCtx m e v)

-- | Bulk-construct a 'FlatIntervalMap' from a 'Stream' of closed intervals @(lo, hi, val)@
-- (ignoring empty intervals with @hi < lo@). Sorts stably by @(lo, hi, arrivalIndex)@ in
-- O(N log N) and augments subtree @maxHi@ bounds bottom-up in O(N), freeing all temporary
-- buffers strictly via @alloc@.
drainToFlatIntervalMap ::
  forall k v m e.
  (FlatIntervalMapCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e k, Elem e v) (FlatIntervalMap e k v)
drainToFlatIntervalMap alloc = Drain $ \inStream -> do
  rawLoVec <- DV.new @k alloc
  rawHiVec <- DV.new @k alloc
  rawValVec <- DV.new @v alloc

  runDrain
    ( drainForEach $ \(inLo, inHi, inVal) ->
        whenM_ (lnot (ltKey @e @k inHi inLo)) $ do
          DV.push alloc rawLoVec inLo
          DV.push alloc rawHiVec inHi
          DV.push alloc rawValVec inVal
    )
    inStream

  n <- DV.len rawLoVec
  loBuf <- allocBuf @k alloc n
  hiBuf <- allocBuf @k alloc n
  maxHiBuf <- allocBuf @k alloc n
  valsBuf <- allocBuf @v alloc n

  maxLvl <-
    runDrain
      drainLength
      (unfoldWhileM n (\remN -> pure (remN `gt` 0)) (\remN -> pure (remN `rshift` 1, ())))

  whenM_ (n `gt` 0) $ do
    rawLoBuf <- DV.buf rawLoVec
    rawHiBuf <- DV.buf rawHiVec
    rawValBuf <- DV.buf rawValVec

    idxArr <- allocTypedArray @Int64 alloc n
    runDrain (drainForEach (\i -> pokeElemOff idxArr i i)) (rangeS 0 n)

    whenM_ (n `gt` 1) $ do
      let swapIdx p q =
            whenM_ (p `ne` q) $ do
              ip <- peekElemOff idxArr p
              iq <- peekElemOff idxArr q
              pokeElemOff idxArr iq p
              pokeElemOff idxArr ip q
          lessIdx p q = do
            ia <- peekElemOff idxArr p
            ib <- peekElemOff idxArr q
            loA <- readBuf rawLoBuf ia
            loB <- readBuf rawLoBuf ib
            hiA <- readBuf rawHiBuf ia
            hiB <- readBuf rawHiBuf ib
            let ltLo = ltKey @e @k loA loB
                eqLo = eqKey @e @k loA loB
                ltHi = ltKey @e @k hiA hiB
                eqHi = eqKey @e @k hiA hiB
            pure (ltLo `lor` (eqLo `land` (ltHi `lor` (eqHi `land` (ia `lt` ib)))))
          siftIdx start end =
            runDrain drain_ $
              unfoldWhileM start (\root -> pure ((root * 2 + 1) `lt` end)) $ \root -> do
                child <- letM (root * 2 + 1)
                bestRef <- newRef root
                ltLeft <- lessIdx root child
                whenM_ ltLeft $
                  writeRef bestRef child
                whenM_ ((child + 1) `lt` end) $ do
                  curBest <- readRef bestRef
                  ltRight <- lessIdx curBest (child + 1)
                  whenM_ ltRight $
                    writeRef bestRef (child + 1)
                best <- readRef bestRef
                whenM_ (best `ne` root) (swapIdx root best)
                pure (ifThenElse (best `eq` root) end best, ())
      runDrain (drainForEach (\s -> siftIdx s n)) (reverseRangeS 0 (n `rshift` 1))
      runDrain (drainForEach (\end -> swapIdx 0 end >> siftIdx 0 end)) (reverseRangeS 1 n)

    -- Permute into sorted SoA buffers & initialize maxHiBuf = hiBuf
    runDrain
      ( drainForEach $ \dstIdx -> do
          srcIdx <- peekElemOff idxArr dstIdx
          lVal <- readBuf rawLoBuf srcIdx
          hVal <- readBuf rawHiBuf srcIdx
          vVal <- readBuf rawValBuf srcIdx
          writeBuf loBuf dstIdx lVal
          writeBuf hiBuf dstIdx hVal
          writeBuf maxHiBuf dstIdx hVal
          writeBuf valsBuf dstIdx vVal
      )
      (rangeS 0 n)
    freeTypedArray @Int64 alloc idxArr n

    -- Iterative post-order augmentation of maxHi[m] over midpoint tree [0, n)
    -- Max stack frames <= 2 * maxLevel <= 126 < 192 frames (576 Int64 words).
    let stackWords = 576 :: e Int64
    stack <- allocTypedArray @Int64 alloc stackWords
    pokeElemOff stack 0 0
    pokeElemOff stack n 1
    pokeElemOff stack 0 2
    spRef <- newRef (1 :: e Int64)
    runDrain drain_ $
      unfoldWhileM
        ()
        (\_ -> (`gt` 0) <$> readRef spRef)
        ( \_ -> do
            sp <- readRef spRef
            top <- letM (sp - 1)
            base <- letM (top * 3)
            l <- peekElemOff stack base
            r <- peekElemOff stack (base + 1)
            st <- peekElemOff stack (base + 2)
            m <- letM ((l + r) `rshift` 1)
            ifThenElseM_
              (st `eq` 0)
              ( do
                  pokeElemOff stack 1 (base + 2)
                  nextSpRef <- newRef sp
                  whenM_ ((m + 1) `lt` r) $ do
                    curSp <- readRef nextSpRef
                    let cBase = curSp * 3
                    pokeElemOff stack (m + 1) cBase
                    pokeElemOff stack r (cBase + 1)
                    pokeElemOff stack 0 (cBase + 2)
                    writeRef nextSpRef (curSp + 1)
                  whenM_ (l `lt` m) $ do
                    curSp <- readRef nextSpRef
                    let cBase = curSp * 3
                    pokeElemOff stack l cBase
                    pokeElemOff stack m (cBase + 1)
                    pokeElemOff stack 0 (cBase + 2)
                    writeRef nextSpRef (curSp + 1)
                  readRef nextSpRef >>= writeRef spRef
              )
              ( do
                  writeRef spRef top
                  bestIdxRef <- newRef m
                  whenM_ (l `lt` m) $ do
                    let lm = (l + m) `rshift` 1
                    bIdx <- readRef bestIdxRef
                    curBest <- readBuf maxHiBuf bIdx
                    lMax <- readBuf maxHiBuf lm
                    whenM_ (ltKey @e @k curBest lMax) $
                      writeRef bestIdxRef lm
                  whenM_ ((m + 1) `lt` r) $ do
                    let rm = (m + 1 + r) `rshift` 1
                    bIdx <- readRef bestIdxRef
                    curBest <- readBuf maxHiBuf bIdx
                    rMax <- readBuf maxHiBuf rm
                    whenM_ (ltKey @e @k curBest rMax) $
                      writeRef bestIdxRef rm
                  finalIdx <- readRef bestIdxRef
                  whenM_ (finalIdx `ne` m) $
                    readBuf maxHiBuf finalIdx >>= writeBuf maxHiBuf m
              )
            pure ((), ())
        )
    freeTypedArray @Int64 alloc stack stackWords

  DV.free alloc rawValVec
  DV.free alloc rawHiVec
  DV.free alloc rawLoVec

  hdr <- allocStruct @(CStruct (FlatIntervalMapSpine k v)) alloc
  let fields = projectPtrs hdr
  pokeBufStruct (getPtrOf (fimsLo fields)) loBuf
  pokeBufStruct (getPtrOf (fimsHi fields)) hiBuf
  pokeBufStruct (getPtrOf (fimsMaxHi fields)) maxHiBuf
  pokeBufStruct (getPtrOf (fimsVals fields)) valsBuf
  poke (getPtrOf (fimsLen fields)) n
  poke (getPtrOf (fimsMaxLevel fields)) maxLvl
  pure (FlatIntervalMap hdr)

fromStream ::
  forall k v m e.
  (FlatIntervalMapCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e k, Elem e v) ->
  m (FlatIntervalMap e k v)
fromStream alloc = runDrain (drainToFlatIntervalMap alloc)

free ::
  forall k v m e.
  (FlatIntervalMapCtx m e k v) =>
  Alloc e ->
  FlatIntervalMap e k v ->
  m ()
free alloc (FlatIntervalMap hdr) = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (fimsLen fields))
  whenM_ (n `gt` 0) $ do
    loBuf <- peekBufStruct (getPtrOf (fimsLo fields))
    hiBuf <- peekBufStruct (getPtrOf (fimsHi fields))
    maxHiBuf <- peekBufStruct (getPtrOf (fimsMaxHi fields))
    valsBuf <- peekBufStruct (getPtrOf (fimsVals fields))
    freeBuf alloc loBuf n
    freeBuf alloc hiBuf n
    freeBuf alloc maxHiBuf n
    freeBuf alloc valsBuf n
  freeStruct alloc hdr

size :: (FlatIntervalMapCtx m e k v) => FlatIntervalMap e k v -> m (e Int64)
size (FlatIntervalMap hdr) = peek (getPtrOf (fimsLen (projectPtrs hdr)))

isEmpty :: (FlatIntervalMapCtx m e k v) => FlatIntervalMap e k v -> m (e Bool)
isEmpty fim = (`eq` 0) <$> size fim

maxLevel :: (FlatIntervalMapCtx m e k v) => FlatIntervalMap e k v -> m (e Int64)
maxLevel (FlatIntervalMap hdr) = peek (getPtrOf (fimsMaxLevel (projectPtrs hdr)))

-- | Zero-allocation, early-stop-safe 'Stream' yielding all intervals @(lo, hi, val)@
-- overlapping the closed query range @[qLo, qHi]@ in exact sorted @(lo, hi)@ order.
--
-- Uses a 64-bit path-bitmask cursor @(depth, pathBits, l, r, phase)@ over the implicit
-- midpoint interval tree: prunes any subtree with @maxHi[m] < qLo@ and prunes @m@ plus
-- its right subtree whenever @lo[m] > qHi@.
overlappingStream ::
  forall k v m e.
  (FlatIntervalMapCtx m e k v) =>
  FlatIntervalMap e k v ->
  Elem e k ->
  Elem e k ->
  Stream m (Elem e k, Elem e k, Elem e v)
overlappingStream (FlatIntervalMap hdr) qLo qHi =
  let fields = projectPtrs hdr
   in statefulS
        ( do
            n <- peek (getPtrOf (fimsLen fields))
            loBuf <- peekBufStruct (getPtrOf (fimsLo fields))
            hiBuf <- peekBufStruct (getPtrOf (fimsHi fields))
            maxHiBuf <- peekBufStruct (getPtrOf (fimsMaxHi fields))
            valsBuf <- peekBufStruct (getPtrOf (fimsVals fields))
            let invalidOrEmpty = (n `le` 0) `lor` ltKey @e @k qHi qLo
            depthRef <- newRef (0 :: e Int64)
            pathRef <- newRef (0 :: e Word64)
            lRef <- newRef (0 :: e Int64)
            rRef <- newRef n
            phaseRef <- newRef (ifThenElse invalidOrEmpty (2 :: e Int64) 0)
            pure (n, loBuf, hiBuf, maxHiBuf, valsBuf, depthRef, pathRef, lRef, rRef, phaseRef)
        )
        ( \(n, loBuf, hiBuf, maxHiBuf, valsBuf, depthRef, pathRef, lRef, rRef, phaseRef) ->
            let ascend = do
                  curDepth <- readRef depthRef
                  curPath <- readRef pathRef
                  (foundAnc, targetDepth) <-
                    runDrain
                      (drainFind (0 :: e Int64) (\i -> ((curPath `rshift` toIntegral i) `iand` 1) `eq` 0))
                      (reverseRangeS 0 curDepth)
                  ifThenElseM_
                    foundAnc
                    ( do
                        (newL, newR) <-
                          runDrain
                            ( drainFoldM (0 :: e Int64, n) $ \(cl, cr) stepIdx -> do
                                let cm = (cl + cr) `rshift` 1
                                    wentRight = ((curPath `rshift` toIntegral stepIdx) `iand` 1) `ne` 0
                                pure (ifThenElse wentRight (cm + 1) cl, ifThenElse wentRight cr cm)
                            )
                            (rangeS 0 targetDepth)
                        writeRef depthRef targetDepth
                        writeRef lRef newL
                        writeRef rRef newR
                        writeRef phaseRef 1
                    )
                    (writeRef phaseRef 2)
             in ContT $ \stepK -> do
                  ph <- readRef phaseRef
                  ifThenElseM_
                    (ph `eq` 2)
                    (stepK Stop)
                    ( do
                        l <- readRef lRef
                        r <- readRef rRef
                        m <- letM ((l + r) `rshift` 1)
                        ifThenElseM_
                          (ph `eq` 0)
                          ( do
                              subMaxHi <- readBuf maxHiBuf m
                              ifThenElseM_
                                (ltKey @e @k subMaxHi qLo)
                                (ascend >> stepK Skip)
                                ( ifThenElseM_
                                    (l `lt` m)
                                    ( do
                                        d <- readRef depthRef
                                        curPath <- readRef pathRef
                                        let bitMask = (1 :: e Word64) `lshift` toIntegral d
                                        writeRef pathRef (curPath `iand` inot bitMask)
                                        writeRef depthRef (d + 1)
                                        writeRef rRef m
                                        writeRef phaseRef 0
                                        stepK Skip
                                    )
                                    (writeRef phaseRef 1 >> stepK Skip)
                                )
                          )
                          ( do
                              loM <- readBuf loBuf m
                              ifThenElseM_
                                (ltKey @e @k qHi loM)
                                (ascend >> stepK Skip)
                                ( do
                                    ifThenElseM_
                                      ((m + 1) `lt` r)
                                      ( do
                                          d <- readRef depthRef
                                          curPath <- readRef pathRef
                                          let bitMask = (1 :: e Word64) `lshift` toIntegral d
                                          writeRef pathRef (curPath `ior` bitMask)
                                          writeRef depthRef (d + 1)
                                          writeRef lRef (m + 1)
                                          writeRef phaseRef 0
                                      )
                                      ascend
                                    hiM <- readBuf hiBuf m
                                    ifThenElseM_
                                      (lnot (ltKey @e @k hiM qLo))
                                      ( do
                                          valM <- readBuf valsBuf m
                                          stepK (Yield (loM, hiM, valM))
                                      )
                                      (stepK Skip)
                                )
                          )
                    )
        )

-- | Zero-allocation 'Stream' yielding all intervals @[lo, hi]@ containing point @pt@ (@lo <= pt <= hi@)
-- in sorted @(lo, hi)@ order.
pointStabStream ::
  (FlatIntervalMapCtx m e k v) =>
  FlatIntervalMap e k v ->
  Elem e k ->
  Stream m (Elem e k, Elem e k, Elem e v)
pointStabStream fim pt = overlappingStream fim pt pt

-- | Count all intervals overlapping @[qLo, qHi]@.
countOverlapping ::
  (FlatIntervalMapCtx m e k v) =>
  FlatIntervalMap e k v ->
  Elem e k ->
  Elem e k ->
  m (e Int64)
countOverlapping fim qLo qHi = runDrain drainLength (overlappingStream fim qLo qHi)

-- | Short-circuiting O(log N) overlap test returning @true@ on the first interval overlapping @[qLo, qHi]@.
anyOverlapping ::
  (FlatIntervalMapCtx m e k v) =>
  FlatIntervalMap e k v ->
  Elem e k ->
  Elem e k ->
  m (e Bool)
anyOverlapping fim qLo qHi = runDrain (drainAny (const true)) (overlappingStream fim qLo qHi)

-- | Contiguous unit-stride 'Stream' over all @(lo, hi, val)@ entries in sorted @(lo, hi)@ order.
toStream ::
  forall k v m e.
  (FlatIntervalMapCtx m e k v) =>
  FlatIntervalMap e k v ->
  Stream m (Elem e k, Elem e k, Elem e v)
toStream (FlatIntervalMap hdr) =
  let fields = projectPtrs hdr
   in withStreamSetup
        ( (,,,)
            <$> peek (getPtrOf (fimsLen fields))
            <*> peekBufStruct (getPtrOf (fimsLo fields))
            <*> peekBufStruct (getPtrOf (fimsHi fields))
            <*> peekBufStruct (getPtrOf (fimsVals fields))
        )
        ( \(n, loBuf, hiBuf, valsBuf) ->
            streamM
              (rangeS 0 n)
              (\i -> (,,) <$> readBuf loBuf i <*> readBuf hiBuf i <*> readBuf valsBuf i)
        )

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Value Kernels & Overlap Counting
--------------------------------------------------------------------------------

-- | Strip-mine all @size@ stored interval values in-place in sorted @(lo, hi)@ order
-- at SIMD gang width @w@.
foreachValuesSPMD ::
  forall w k v m e.
  (FlatIntervalMapCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  FlatIntervalMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (FlatIntervalMap hdr) step = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (fimsLen fields))
  valsBuf <- peekBufStruct (getPtrOf (fimsVals fields))
  foreachBufSPMD pw True valsBuf n step

-- | Masked-safe SIMD fold over all @size@ stored interval values in sorted @(lo, hi)@ order.
foldValuesSPMD ::
  forall w k v s m e.
  (FlatIntervalMapCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  FlatIntervalMap e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (FlatIntervalMap hdr) s0 step = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (fimsLen fields))
  valsBuf <- peekBufStruct (getPtrOf (fimsVals fields))
  foldBufSPMD pw True valsBuf n s0 step

-- | Branch-free 64B-aligned SIMD strip-mined overlap count over @[0, size)@ for a broadcast
-- query interval @[qLoGang, qHiGang]@ (e.g. @uniform qLo@ and @uniform qHi@ for scalars).
spmdCountOverlapping ::
  forall w k v m e.
  ( FlatIntervalMapCtx m e k v,
    LayoutUnitSPMD w m e k,
    OrdKey (SPMD w e) k
  ) =>
  Proxy w ->
  FlatIntervalMap e k v ->
  Elem (SPMD w e) k ->
  Elem (SPMD w e) k ->
  m (e Int64)
spmdCountOverlapping pw (FlatIntervalMap hdr) qLoGang qHiGang = do
  let fields = projectPtrs hdr
      wE = fromIntegral (natVal pw) :: e Int64
      validQuery = lnot (ltKey @(SPMD w e) @k qHiGang qLoGang)
  n <- peek (getPtrOf (fimsLen fields))
  loBuf <- peekBufStruct (getPtrOf (fimsLo fields))
  hiBuf <- peekBufStruct (getPtrOf (fimsHi fields))
  accRef <- withSPMD pw (newRef (0 :: SPMD w e Int64))
  numFull <- letM (ifThenElse (n `gt` 0) (n `quotient` wE) 0)
  alignedEnd <- letM (numFull * wE)
  runDrain
    ( drainForEach $ \g -> do
        base <- letM (g * wE)
        let idx = spmdAlignedUnitStride base
        withSPMD pw $ do
          sGang <- readAlignedGangBuf @w @m @e @k pw True loBuf g idx
          eGang <- readAlignedGangBuf @w @m @e @k pw True hiBuf g idx
          let hit =
                validQuery
                  `land` lnot (ltKey @(SPMD w e) @k eGang qLoGang)
                  `land` lnot (ltKey @(SPMD w e) @k qHiGang sGang)
          cur <- readRef accRef
          writeRef accRef (cur + ifThenElse hit 1 0)
    )
    (rangeS 0 numFull)
  whenM_ (alignedEnd `lt` n) $ do
    let tailIdx = spmdAlignedUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform n) $ do
      sGang <- readAlignedGangBuf @w @m @e @k pw True loBuf numFull tailIdx
      eGang <- readAlignedGangBuf @w @m @e @k pw True hiBuf numFull tailIdx
      let hit =
            validQuery
              `land` lnot (ltKey @(SPMD w e) @k eGang qLoGang)
              `land` lnot (ltKey @(SPMD w e) @k qHiGang sGang)
      cur <- readRef accRef
      writeRef accRef (cur + ifThenElse hit 1 0)
  acc <- withSPMD pw (readRef accRef)
  pure (gangReduceAdd acc)

-- | Branch-free 64B-aligned SIMD strip-mined point-stabbing count (@lo <= pt <= hi@)
-- across @[0, size)@.
spmdCountStabbed ::
  forall w k v m e.
  ( FlatIntervalMapCtx m e k v,
    LayoutUnitSPMD w m e k,
    OrdKey (SPMD w e) k
  ) =>
  Proxy w ->
  FlatIntervalMap e k v ->
  Elem (SPMD w e) k ->
  m (e Int64)
spmdCountStabbed pw fim ptGang = spmdCountOverlapping pw fim ptGang ptGang

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type FlatIntervalMapABI k v = (Typeable k, Typeable v, JanusStruct (Buf k), JanusStruct (Buf v))

type instance JanusCHost (FlatIntervalMap (JanusCT _) k v) = FlatIntervalMap Identity k v

deriving via
  ViaPtrHandle t (CStruct (FlatIntervalMapSpine k v)) (FlatIntervalMap Identity k v)
  instance
    (FlatIntervalMapABI k v) => JanusCArg t (FlatIntervalMap (JanusCT t) k v)

deriving via
  ViaPtrHandle t (CStruct (FlatIntervalMapSpine k v)) (FlatIntervalMap Identity k v)
  instance
    (FlatIntervalMapABI k v) => JanusCValRet t (FlatIntervalMap (JanusCT t) k v)
