{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Alloc.Recycling
  ( RecyclerBuckets,
    RecyclerSpine (..),
    Recycler (..),
    RecyclerCtx,
    initRecyclerRaw,
    recyclerAllocImpl,
    recyclerReallocImpl,
    recyclerFreeImpl,
    recyclerFlushImpl,
    recyclerDestroyImpl,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, isNullVoidPtr)
import Janus.Backend.C
import Janus.Command.Array (CmdMemCopy (..), CmdStorable (..), ExpPtr (..))
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (drainFind, drainForEach, runDrain)
import Janus.Control.Stream (indexedS, iterateS, rangeS, unfoldWhileM)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (toIntegral), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

-- | 22 power-of-2 buckets: @2^3 = 8B@ through @2^24 = 16MB@.
type RecyclerBuckets = 22

numRecyclerBuckets :: Int64
numRecyclerBuckets = 22

maxCachedAlign :: Int64
maxCachedAlign = 4096

data RecyclerSpine f = RecyclerSpine
  { rsHeads :: !(f (FixedArray RecyclerBuckets (Ptr Word8))),
    rsCounts :: !(f (FixedArray RecyclerBuckets Int64)),
    rsMinAlign :: !(f Int64),
    rsMaxPerBucket :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (RecyclerSpine Identity)

deriving stock instance Show (RecyclerSpine Identity)

instance JanusStruct RecyclerSpine

newtype Recycler e = Recycler
  { unRecycler :: e (Ptr (CStruct RecyclerSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (Recycler Identity)

deriving stock instance Show (Recycler Identity)

instance JanusHKD Recycler

type RecyclerCtx m e =
  ( CmdAllocBase m e,
    JanusTyped e (CStruct RecyclerSpine),
    JanusTyped e (FixedArray RecyclerBuckets (Ptr Word8)),
    JanusTyped e (FixedArray RecyclerBuckets Int64)
  )

initRecyclerRaw ::
  (RecyclerCtx m e) =>
  e (Ptr (CStruct RecyclerSpine)) ->
  e Int64 ->
  e Int64 ->
  m (Recycler e)
initRecyclerRaw hdr maxPerBucket minAl = do
  let f = projectPtrs hdr
      headsArr = fixedArrayElemPtr (getPtrOf (rsHeads f))
      countsArr = fixedArrayElemPtr (getPtrOf (rsCounts f))
      effMin = ifThenElse (minAl `lt` 8) 8 minAl
      effCap = ifThenElse (maxPerBucket `le` 0) 1024 maxPerBucket
  runDrain
    ( drainForEach $ \i -> do
        pokeElemOff headsArr nullPtr i
        pokeElemOff countsArr 0 i
    )
    (rangeS 0 (fromIntegral numRecyclerBuckets))
  poke (getPtrOf (rsMinAlign f)) effMin
  poke (getPtrOf (rsMaxPerBucket f)) effCap
  pure (Recycler hdr)

-- | Compute @(bucketIdx, bucketByteSize)@ for @need >= 8@.
-- If @need > 16MB@, returns @bucketIdx = 22@ (bypass cache).
computeSizeClass ::
  forall m e.
  (CmdAllocBase m e) =>
  e Int64 ->
  m (e Int64, e Int64)
computeSizeClass rawNeed = do
  need <- letM (ifThenElse (rawNeed `lt` 8) 8 rawNeed)
  let limit = fromIntegral numRecyclerBuckets :: e Int64
  (_, res) <-
    runDrain
      (drainFind (limit, 8) (\(b, sz) -> (sz `ge` need) `lor` (b `ge` limit)))
      (indexedS (iterateS (8 :: e Int64) (`lshift` 1)))
  pure res

recyclerAllocImpl ::
  forall m e.
  (RecyclerCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  Recycler e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
recyclerAllocImpl parentAlloc (Recycler hdr) reqSz reqAlign = do
  let f = projectPtrs hdr
  minAl <- peek (getPtrOf (rsMinAlign f))
  effAl <- letM (ifThenElse (reqAlign `gt` minAl) reqAlign minAl)
  -- Size class depends solely on (reqSz, minAl) so `freeBytes(ptr, reqSz)` always maps
  -- back to the exact same bucket and `clsBytes` even when `reqAlign > clsBytes`!
  let szPos = ifThenElse (reqSz `le` 0) minAl reqSz
      need = ifThenElse (szPos `gt` minAl) szPos minAl
  (b, clsBytes) <- computeSizeClass need
  outRef <- newRef (nullPtr @e @Word8)

  ifThenElseM_
    ((b `lt` fromIntegral numRecyclerBuckets) `land` (effAl `le` fromIntegral maxCachedAlign))
    ( do
        let headsArr = fixedArrayElemPtr (getPtrOf (rsHeads f))
            countsArr = fixedArrayElemPtr (getPtrOf (rsCounts f))
            bucketAl = ifThenElse (clsBytes `lt` fromIntegral maxCachedAlign) clsBytes (fromIntegral maxCachedAlign)
            allocAl = ifThenElse (effAl `gt` bucketAl) effAl bucketAl
        hd <- peekElemOff headsArr b
        let hdAligned =
              (ptrToWord64Expr hd `ne` 0)
                `land` ((toIntegral (ptrToWord64Expr hd) `iand` (effAl - 1)) `eq` (0 :: e Int64))
        ifThenElseM_
          hdAligned
          ( do
              let cell = fromVoidPtr (toVoidPtr hd) :: e (Ptr (Ptr Word8))
              nxt <- peek cell
              cnt <- peekElemOff countsArr b
              pokeElemOff headsArr nxt b
              pokeElemOff countsArr (cnt - 1) b
              writeRef outRef hd
          )
          ( do
              vp <- parentAlloc clsBytes allocAl
              writeRef outRef (fromVoidPtr vp)
          )
    )
    (parentAlloc need effAl >>= writeRef outRef . fromVoidPtr)

  toVoidPtr <$> readRef outRef

recyclerFreeImpl ::
  forall m e.
  (RecyclerCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Recycler e ->
  e (Ptr ()) ->
  e Int64 ->
  m ()
recyclerFreeImpl parentFree (Recycler hdr) ptr oldSz =
  whenM_ (lnot (isNullVoidPtr ptr)) $ do
    let f = projectPtrs hdr
    minAl <- peek (getPtrOf (rsMinAlign f))
    maxCap <- peek (getPtrOf (rsMaxPerBucket f))
    let szPos = ifThenElse (oldSz `le` 0) minAl oldSz
        need = ifThenElse (szPos `gt` minAl) szPos minAl
    (b, clsBytes) <- computeSizeClass need
    -- Also verify pointer alignment matches bucket's guaranteed alignment before caching
    let bucketAl = ifThenElse (clsBytes `lt` fromIntegral maxCachedAlign) clsBytes (fromIntegral maxCachedAlign)
        pW = ptrToWord64Expr (fromVoidPtr ptr :: e (Ptr Word8))
        alignedOk = (toIntegral pW `iand` (bucketAl - 1)) `eq` (0 :: e Int64)
    ifThenElseM_
      ((b `lt` fromIntegral numRecyclerBuckets) `land` alignedOk)
      ( do
          let headsArr = fixedArrayElemPtr (getPtrOf (rsHeads f))
              countsArr = fixedArrayElemPtr (getPtrOf (rsCounts f))
          cnt <- peekElemOff countsArr b
          ifThenElseM_
            (cnt `lt` maxCap)
            ( do
                hd <- peekElemOff headsArr b
                let cell = fromVoidPtr ptr :: e (Ptr (Ptr Word8))
                poke cell hd
                pokeElemOff headsArr (fromVoidPtr ptr) b
                pokeElemOff countsArr (cnt + 1) b
            )
            (parentFree ptr clsBytes)
      )
      (parentFree ptr (ifThenElse (b `lt` fromIntegral numRecyclerBuckets) clsBytes need))

recyclerReallocImpl ::
  forall m e.
  (RecyclerCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Recycler e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
recyclerReallocImpl parentAlloc parentFree rec@(Recycler hdr) oldPtr oldSz newSz reqAlign = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (isNullVoidPtr oldPtr)
    (recyclerAllocImpl parentAlloc rec newSz reqAlign >>= writeRef outRef . fromVoidPtr)
    ( ifThenElseM_
        (newSz `le` 0)
        (recyclerFreeImpl parentFree rec oldPtr oldSz >> writeRef outRef (nullPtr @e @Word8))
        ( do
            let f = projectPtrs hdr
            minAl <- peek (getPtrOf (rsMinAlign f))
            let effAl = ifThenElse (reqAlign `gt` minAl) reqAlign minAl
                oldPos = ifThenElse (oldSz `le` 0) minAl oldSz
                oldNeed = ifThenElse (oldPos `gt` minAl) oldPos minAl
                newNeed = ifThenElse (newSz `gt` minAl) newSz minAl
            (bOld, clsOld) <- computeSizeClass oldNeed
            (bNew, _) <- computeSizeClass newNeed
            let pW = ptrToWord64Expr (fromVoidPtr oldPtr :: e (Ptr Word8))
                alOk = (toIntegral pW `iand` (effAl - 1)) `eq` (0 :: e Int64)
            -- If still in the same cached power-of-2 size class and aligned, reuse in O(1)!
            ifThenElseM_
              ((bOld `lt` fromIntegral numRecyclerBuckets) `land` (bOld `eq` bNew) `land` alOk `land` (newSz `le` clsOld))
              (writeRef outRef (fromVoidPtr oldPtr))
              ( do
                  np <- recyclerAllocImpl parentAlloc rec newSz reqAlign
                  whenM_ (lnot (isNullVoidPtr np)) $ do
                    let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                    cmdMemcpy np oldPtr copyLen
                    recyclerFreeImpl parentFree rec oldPtr oldSz
                  writeRef outRef (fromVoidPtr np)
              )
        )
    )
  toVoidPtr <$> readRef outRef

-- | Return all cached blocks across all 22 buckets to the parent allocator.
recyclerFlushImpl ::
  forall m e.
  (RecyclerCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Recycler e ->
  m ()
recyclerFlushImpl parentFree (Recycler hdr) = do
  let f = projectPtrs hdr
      headsArr = fixedArrayElemPtr (getPtrOf (rsHeads f))
      countsArr = fixedArrayElemPtr (getPtrOf (rsCounts f))
  runDrain
    ( drainForEach $ \b -> do
        let clsBytes = (8 :: e Int64) `lshift` b
        hd0 <- peekElemOff headsArr b
        runDrain
          (drainForEach (\cur -> parentFree (toVoidPtr cur) clsBytes))
          ( unfoldWhileM hd0 (\cur -> pure (ptrToWord64Expr cur `ne` 0)) $ \cur -> do
              let cell = fromVoidPtr (toVoidPtr cur) :: e (Ptr (Ptr Word8))
              nxt <- peek cell
              pure (nxt, cur)
          )
        pokeElemOff headsArr nullPtr b
        pokeElemOff countsArr 0 b
    )
    (rangeS 0 (fromIntegral numRecyclerBuckets))

recyclerDestroyImpl ::
  (RecyclerCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  e Int64 ->
  Recycler e ->
  m ()
recyclerDestroyImpl parentFree spineSz rec@(Recycler hdr) = do
  recyclerFlushImpl parentFree rec
  parentFree (toVoidPtr hdr) spineSz

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (Recycler (JanusCT _)) = Recycler Identity

deriving via
  ViaPtrHandle t (CStruct RecyclerSpine) (Recycler Identity)
  instance
    JanusCArg t (Recycler (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct RecyclerSpine) (Recycler Identity)
  instance
    JanusCValRet t (Recycler (JanusCT t))
