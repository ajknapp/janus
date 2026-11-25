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

module Janus.Data.EytzingerVec
  ( -- * 1-Indexed BFS-Layout Static Search Array Spine & Handle
    EytzingerVecSpine (..),
    EytzingerVec (..),
    EytzingerVecCtx,

    -- * Bulk Stream Construction & Lifecycle
    drainToEytzingerVec,
    fromStream,
    free,

    -- * Queries & Branchless Eytzinger Search
    size,
    isEmpty,
    lookupGE,
    withLookupGE,
    member,

    -- * Zero-Allocation Stackless Sorted & Raw BFS Streams
    inOrderStream,
    rawBfsStream,

    -- * First-Class SPMD / SIMD Eytzinger Search & Fold
    lookupGESPMD,
    lookupScalarGESPMD,
    memberSPMD,
    foldSPMD,
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
import Janus.Alloc.Policy (Alloc, CmdAlloc, allocStruct, freeStruct)
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
import Janus.Control.Drain (Drain (..), drainIndexed, drainLast, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, rangeS, statefulS, streamM, unfoldWhileM, withStreamSetup)
import qualified Janus.Data.DynVec as DV
import Janus.Data.FlatMap (OrdKey (..), OrdLayoutCtx)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpBoolCast (..), ExpIntegralCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    uniform,
  )

-- | Spine struct for 1-indexed Eytzinger (BFS-layout) static search array.
--
-- Duplicate keys from the input stream are preserved (matching a sorted vector):
-- 'size' equals the exact element count, 'inOrderStream' yields all elements in
-- non-decreasing sorted order, and 'lookupGE' returns the first element @>= target@.
-- When @evsLen == n > 0@, @evsBuf@ has capacity @n + 1@ and indices @1 .. n@ hold the
-- complete binary search tree in breadth-first heap numbering.
data EytzingerVecSpine k f = EytzingerVecSpine
  { evsBuf :: !(f (CStruct (Buf k))),
    evsLen :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, JanusStruct (Buf k)) => JanusStruct (EytzingerVecSpine k)

newtype EytzingerVec (e :: K.Type -> K.Type) k = EytzingerVec
  { unEytzingerVec :: e (Ptr (CStruct (EytzingerVecSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (EytzingerVec Identity k)

deriving stock instance Show (EytzingerVec Identity k)

type EytzingerVecCtx m e k = OrdLayoutCtx m e k

-- | Branchless 6-step count-trailing-zeros over @Word64@ (returns @64@ when @m == 0@).
ctzWord64 ::
  forall e.
  (ExpBits e Word64, ExpCond e, KnownJanusType e Int64, Num (e Word64), Num (e Int64)) =>
  e Word64 ->
  e Int64
ctzWord64 m =
  let lsb = m `iand` (0 - m)
      b5 = ifThenElse ((lsb `iand` 0x00000000FFFFFFFF) `eq` 0) 32 0
      b4 = ifThenElse ((lsb `iand` 0x0000FFFF0000FFFF) `eq` 0) 16 0
      b3 = ifThenElse ((lsb `iand` 0x00FF00FF00FF00FF) `eq` 0) 8 0
      b2 = ifThenElse ((lsb `iand` 0x0F0F0F0F0F0F0F0F) `eq` 0) 4 0
      b1 = ifThenElse ((lsb `iand` 0x3333333333333333) `eq` 0) 2 0
      b0 = ifThenElse ((lsb `iand` 0x5555555555555555) `eq` 0) 1 0
   in ifThenElse (m `eq` 0) 64 (b5 + b4 + b3 + b2 + b1 + b0)

-- | Strip trailing 1-bits plus one more bit: @idx >> (ctz(~idx) + 1)@, guarding shift widths @< 64@.
-- Used both to decode the Khuong-Morin candidate node after branchless descent AND to jump
-- in O(1) to the in-order successor ancestor in stackless tree traversal.
ascendRightSpines ::
  forall e.
  ( ExpBits e Word64,
    ExpOrd e Int64,
    ExpCond e,
    ExpIntegralCast e Int64 Word64,
    ExpIntegralCast e Word64 Int64,
    KnownJanusType e Int64,
    Num (e Word64),
    Num (e Int64)
  ) =>
  e Int64 ->
  e Int64
ascendRightSpines idx =
  let w = toIntegral idx :: e Word64
      ones = ctzWord64 (inot w)
      safeOnes = ifThenElse (ones `ge` 63) 0 ones
      shifted = (w `rshift` toIntegral safeOnes) `rshift` 1
   in ifThenElse (ones `ge` 63) 0 (toIntegral shifted)

-- | Zero-allocation, stackless single-step stream yielding Eytzinger indices @1 .. n@
-- in exact sorted in-order sequence.
eytzInOrderIndicesS ::
  forall m e.
  (CmdAlloc m e) =>
  e Int64 ->
  Stream m (e Int64)
eytzInOrderIndicesS n =
  statefulS
    ( do
        curRef <- newRef (ifThenElse (n `gt` 0) 1 0)
        descRef <- newRef true
        pure (curRef, descRef)
    )
    ( \(curRef, descRef) -> ContT $ \stepK -> do
        cur <- readRef curRef
        ifThenElseM_
          (cur `le` 0)
          (stepK Stop)
          ( do
              desc <- readRef descRef
              let lc = cur * 2
              ifThenElseM_
                (desc `land` (lc `le` n))
                (writeRef curRef lc >> stepK Skip)
                ( do
                    let rc = lc + 1
                    ifThenElseM_
                      (rc `le` n)
                      (writeRef curRef rc >> writeRef descRef true)
                      (writeRef curRef (ascendRightSpines cur) >> writeRef descRef false)
                    stepK (Yield cur)
                )
          )
    )

-- | Bulk-construct an 'EytzingerVec' from any 'Stream' in O(N log N) time:
-- collects into a temporary 'DV.DynVec', sorts in-place via 'ltKey', populates the
-- 1-indexed Eytzinger BFS array in O(N) with zero auxiliary stack allocation, and frees
-- the temporary 'DV.DynVec' via @alloc@. Duplicate elements are preserved.
drainToEytzingerVec ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (EytzingerVec e k)
drainToEytzingerVec alloc = Drain $ \inStream -> do
  tmpVec <- runDrain (DV.drainToVect @k alloc) inStream
  DV.sortBy (ltKey @e @k) tmpVec
  n <- DV.len tmpVec
  let allocCap = ifThenElse (n `gt` 0) (n + 1) 0
  evBuf <- allocBuf @k alloc allocCap
  whenM_ (n `gt` 0) $ do
    sortedBuf <- DV.buf tmpVec
    v0 <- readBuf sortedBuf 0
    writeBuf evBuf 0 v0
    _ <-
      runDrain
        (drainIndexed (\srcIdx eytzIdx -> readBuf sortedBuf srcIdx >>= writeBuf evBuf eytzIdx))
        (eytzInOrderIndicesS n)
    pure ()
  DV.free alloc tmpVec

  hdr <- allocStruct @(CStruct (EytzingerVecSpine k)) alloc
  let fields = projectPtrs hdr
  pokeBufStruct (getPtrOf (evsBuf fields)) evBuf
  poke (getPtrOf (evsLen fields)) n
  pure (EytzingerVec hdr)

fromStream ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (EytzingerVec e k)
fromStream alloc = runDrain (drainToEytzingerVec alloc)

free ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  Alloc e ->
  EytzingerVec e k ->
  m ()
free alloc (EytzingerVec hdr) = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (evsLen fields))
  whenM_ (n `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (evsBuf fields))
    freeBuf alloc b (n + 1)
  freeStruct alloc hdr

size :: (EytzingerVecCtx m e k) => EytzingerVec e k -> m (e Int64)
size (EytzingerVec hdr) = peek (getPtrOf (evsLen (projectPtrs hdr)))

isEmpty :: (EytzingerVecCtx m e k) => EytzingerVec e k -> m (e Bool)
isEmpty ev = (`eq` 0) <$> size ev

-- | Internal Khuong-Morin branchless Eytzinger lower-bound index search.
-- Returns @0@ when no element is @>= target@, or @cand@ (@1 <= cand <= n@) pointing to
-- the first in-order element @>= target@.
findGEIdx ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  Buf k e ->
  e Int64 ->
  Elem e k ->
  m (e Int64)
findGEIdx b n target = do
  candRef <- newRef (0 :: e Int64)
  whenM_ (n `gt` 0) $ do
    kRef <- newRef (1 :: e Int64)
    runDrain drain_ $
      unfoldWhileM
        (1 :: e Int64)
        (\i -> pure (i `le` n))
        ( \i -> do
            v <- readBuf b i
            let nxt = i * 2 + ifThenElse (ltKey @e @k v target) 1 0
            writeRef kRef nxt
            pure (nxt, ())
        )
    kFinal <- readRef kRef
    writeRef candRef (ascendRightSpines kFinal)
  readRef candRef

-- | O(log N) branchless Eytzinger lower-bound search returning @(found, firstValGEOrDefault)@.
lookupGE ::
  forall k s m e.
  (EytzingerVecCtx m e k, Elem e k ~ e s, KnownJanusType e s) =>
  e s ->
  EytzingerVec e k ->
  Elem e k ->
  m (e Bool, e s)
lookupGE defVal (EytzingerVec hdr) target = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (evsLen fields))
  b <- peekBufStruct (getPtrOf (evsBuf fields))
  cand <- findGEIdx @k b n target
  okRef <- newRef false
  outRef <- newRef defVal
  whenM_ (cand `gt` 0) $ do
    readBuf b cand >>= writeRef outRef
    writeRef okRef true
  (,) <$> readRef okRef <*> readRef outRef

-- | Callback-based O(log N) branchless lower-bound search supporting all 'StorageLayout's
-- (including multi-field HKD @AoS@ / @SoA@ keys).
withLookupGE ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  EytzingerVec e k ->
  Elem e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withLookupGE (EytzingerVec hdr) target onMiss onHit = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (evsLen fields))
  b <- peekBufStruct (getPtrOf (evsBuf fields))
  cand <- findGEIdx @k b n target
  ifThenElseM_
    (cand `gt` 0)
    (readBuf b cand >>= onHit)
    onMiss

-- | O(log N) branchless Eytzinger membership test.
member ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  EytzingerVec e k ->
  Elem e k ->
  m (e Bool)
member (EytzingerVec hdr) target = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (evsLen fields))
  b <- peekBufStruct (getPtrOf (evsBuf fields))
  cand <- findGEIdx @k b n target
  okRef <- newRef false
  whenM_ (cand `gt` 0) $ do
    v <- readBuf b cand
    writeRef okRef (eqKey @e @k v target)
  readRef okRef

-- | Zero-allocation, stackless 'Stream' yielding all elements in sorted ascending order.
inOrderStream ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  EytzingerVec e k ->
  Stream m (Elem e k)
inOrderStream (EytzingerVec hdr) =
  let fields = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (evsLen fields)) <*> peekBufStruct (getPtrOf (evsBuf fields)))
        (\(n, b) -> streamM (eytzInOrderIndicesS n) (readBuf b))

-- | Contiguous unit-stride 'Stream' over the raw 1-indexed Eytzinger BFS array @[1 .. size]@.
rawBfsStream ::
  forall k m e.
  (EytzingerVecCtx m e k) =>
  EytzingerVec e k ->
  Stream m (Elem e k)
rawBfsStream (EytzingerVec hdr) =
  let fields = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (evsLen fields)) <*> peekBufStruct (getPtrOf (evsBuf fields)))
        (\(n, b) -> streamM (rangeS 1 (n + 1)) (readBuf b))

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Eytzinger Search & Fold
--------------------------------------------------------------------------------

-- | Lane-parallel O(log N) branchless Khuong-Morin Eytzinger lower-bound index search
-- across @w@ SIMD lanes, returning @(found, eytzIdx1Based)@ (with @eytzIdx1Based == 0@ on miss).
lookupGESPMD ::
  forall w k m e.
  ( EytzingerVecCtx m e k,
    SPMDGang w m e,
    OrdKey (SPMD w e) k,
    LayoutGatherSPMD w m e k
  ) =>
  Proxy w ->
  EytzingerVec e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool, SPMD w e Int64)
lookupGESPMD _ (EytzingerVec hdr) qKeys = do
  let fields = projectPtrs hdr
  n <- liftSPMD (peek (getPtrOf (evsLen fields)))
  kb <- liftSPMD (peekBufStruct (getPtrOf (evsBuf fields)))
  (_, curEnd) <-
    runDrain (drainLast (1 :: SPMD w e Int64)) $
      unfoldWhileM
        (1 :: SPMD w e Int64)
        (\cur -> pure (cur `le` uniform n))
        ( \cur -> do
            val <- gatherBufSPMD kb cur
            let stepRight = fromBool (ltKey @(SPMD w e) @k val qKeys) :: SPMD w e Int64
                nxt = (cur `lshift` 1) + stepRight
            pure (nxt, nxt)
        )
  let cand = ascendRightSpines @(SPMD w e) curEnd
  pure (cand `ge` 1, cand)

-- | Lane-parallel O(log N) scalar lower-bound lookup returning @(found, firstValGEOrDefault)@.
lookupScalarGESPMD ::
  forall w k s m e.
  ( EytzingerVecCtx m e k,
    SPMDGang w m e,
    OrdKey (SPMD w e) k,
    LayoutGatherSPMD w m e k,
    Elem (SPMD w e) k ~ SPMD w e s,
    SPMDTyped w e s
  ) =>
  Proxy w ->
  SPMD w e s ->
  EytzingerVec e k ->
  SPMD w e s ->
  SPMDM w m e (SPMD w e Bool, SPMD w e s)
lookupScalarGESPMD pw defVal ev@(EytzingerVec hdr) qKeys = do
  (found, eytzIdx) <- lookupGESPMD pw ev qKeys
  outRef <- newRef defVal
  whenM_ found $ do
    kb <- liftSPMD (peekBufStruct (getPtrOf (evsBuf (projectPtrs hdr))))
    matched <- gatherBufSPMD kb eytzIdx
    writeRef outRef matched
  matchedOrDef <- readRef outRef
  pure (found, matchedOrDef)

-- | Lane-parallel O(log N) Eytzinger membership test across @w@ SIMD lanes.
memberSPMD ::
  forall w k m e.
  ( EytzingerVecCtx m e k,
    SPMDGang w m e,
    OrdKey (SPMD w e) k,
    LayoutGatherSPMD w m e k
  ) =>
  Proxy w ->
  EytzingerVec e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
memberSPMD pw ev@(EytzingerVec hdr) qKeys = do
  (found, eytzIdx) <- lookupGESPMD pw ev qKeys
  okRef <- newRef (false :: SPMD w e Bool)
  whenM_ found $ do
    kb <- liftSPMD (peekBufStruct (getPtrOf (evsBuf (projectPtrs hdr))))
    matched <- gatherBufSPMD kb eytzIdx
    writeRef okRef (eqKey @(SPMD w e) @k matched qKeys)
  readRef okRef

-- | Masked-safe SIMD fold over the contiguous 1-indexed Eytzinger array @[1 .. size]@.
foldSPMD ::
  forall w k s m e.
  (EytzingerVecCtx m e k, LayoutSliceSPMD w m e k, SPMDTyped w e s) =>
  Proxy w ->
  EytzingerVec e k ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) k -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw (EytzingerVec hdr) s0 step = do
  let fields = projectPtrs hdr
  n <- peek (getPtrOf (evsLen fields))
  kb <- peekBufStruct (getPtrOf (evsBuf fields))
  foldSliceBufSPMD pw False kb 1 n 0 s0 step

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type EytzingerVecABI k = (Typeable k, JanusStruct (Buf k))

type instance JanusCHost (EytzingerVec (JanusCT _) k) = EytzingerVec Identity k

deriving via
  ViaPtrHandle t (CStruct (EytzingerVecSpine k)) (EytzingerVec Identity k)
  instance
    (EytzingerVecABI k) => JanusCArg t (EytzingerVec (JanusCT t) k)

deriving via
  ViaPtrHandle t (CStruct (EytzingerVecSpine k)) (EytzingerVec Identity k)
  instance
    (EytzingerVecABI k) => JanusCValRet t (EytzingerVec (JanusCT t) k)
