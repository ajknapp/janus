{-# LANGUAGE ConstraintKinds #-}
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

module Janus.Alloc.Pool
  ( PoolSlabHdr (..),
    MemPoolSpine (..),
    MemPool (..),
    PoolCtx,
    initMemPoolRaw,
    poolOwnsPtr,
    poolAllocImpl,
    poolReallocImpl,
    poolFreeImpl,
    poolResetImpl,
    poolDestroyImpl,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, alignUpWord64, isNullVoidPtr)
import Janus.Backend.C
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), janusSIMDAlignment)
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Control.Drain (drainAny, drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, streamM, unfoldWhileM)
import Janus.Data.HKD
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

data PoolSlabHdr f = PoolSlabHdr
  { pshNext :: !(f (Ptr Word8)),
    pshPayloadBase :: !(f (Ptr Word8)),
    pshSlotsCap :: !(f Int64),
    pshBumped :: !(f Int64),
    pshAllocBytes :: !(f Int64),
    pshPad1 :: !(f Int64),
    pshPad2 :: !(f Int64),
    pshPad3 :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (PoolSlabHdr Identity)

deriving stock instance Show (PoolSlabHdr Identity)

instance JanusStruct PoolSlabHdr

poolSlabHdrBytes :: Int64
poolSlabHdrBytes = 64

data MemPoolSpine f = MemPoolSpine
  { mpsFirstSlab :: !(f (Ptr (CStruct PoolSlabHdr))),
    mpsCurSlab :: !(f (Ptr (CStruct PoolSlabHdr))),
    mpsFreeHead :: !(f (Ptr Word8)),
    mpsSlotSize :: !(f Int64),
    mpsSlotsPerSlab :: !(f Int64),
    mpsSlotAlign :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (MemPoolSpine Identity)

deriving stock instance Show (MemPoolSpine Identity)

instance JanusStruct MemPoolSpine

newtype MemPool e = MemPool
  { unMemPool :: e (Ptr (CStruct MemPoolSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (MemPool Identity)

deriving stock instance Show (MemPool Identity)

instance JanusHKD MemPool

type PoolCtx m e =
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MemPoolSpine),
    JanusTyped e (CStruct PoolSlabHdr),
    ExpPtr e (CStruct PoolSlabHdr),
    CmdStorable m e (Ptr (CStruct PoolSlabHdr))
  )

initMemPoolRaw ::
  (PoolCtx m e) =>
  e (Ptr (CStruct MemPoolSpine)) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (MemPool e)
initMemPoolRaw hdr rawSlotSz slotsPerSlab rawAlign = do
  let f = projectPtrs hdr
      effAlign = ifThenElse (rawAlign `lt` 8) 8 rawAlign
      minRawSz = ifThenElse (rawSlotSz `lt` 8) 8 rawSlotSz
      slotSz = alignUpExpr minRawSz effAlign
      effCount = ifThenElse (slotsPerSlab `lt` 4) 4 slotsPerSlab
  poke (getPtrOf (mpsFirstSlab f)) nullPtr
  poke (getPtrOf (mpsCurSlab f)) nullPtr
  poke (getPtrOf (mpsFreeHead f)) nullPtr
  poke (getPtrOf (mpsSlotSize f)) slotSz
  poke (getPtrOf (mpsSlotsPerSlab f)) effCount
  poke (getPtrOf (mpsSlotAlign f)) effAlign
  pure (MemPool hdr)

poolSlabChainFrom ::
  forall m e.
  (PoolCtx m e) =>
  e (Ptr Word8) ->
  Stream m (e (Ptr (CStruct PoolSlabHdr)))
poolSlabChainFrom startRaw =
  unfoldWhileM startRaw (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \p -> do
    let s = fromVoidPtr (toVoidPtr p) :: e (Ptr (CStruct PoolSlabHdr))
    nxt <- peek (getPtrOf (pshNext (projectPtrs s)))
    pure (nxt, s)

poolOwnsPtr ::
  forall m e.
  (PoolCtx m e) =>
  MemPool e ->
  e (Ptr ()) ->
  m (e Bool)
poolOwnsPtr (MemPool hdr) vp = do
  let f = projectPtrs hdr
      pW = ptrToWord64Expr (fromVoidPtr vp :: e (Ptr Word8))
  slotSz <- peek (getPtrOf (mpsSlotSize f))
  firstSlab <- peek (getPtrOf (mpsFirstSlab f))
  runDrain (drainAny id) $
    streamM (poolSlabChainFrom (fromVoidPtr (toVoidPtr firstSlab))) $ \s -> do
      let sf = projectPtrs s
      base <- peek (getPtrOf (pshPayloadBase sf))
      cnt <- peek (getPtrOf (pshSlotsCap sf))
      let baseW = ptrToWord64Expr base
          endW = baseW + toIntegral (cnt * slotSz)
      pure ((pW `ge` baseW) `land` (pW `lt` endW))

poolAllocImpl ::
  forall m e.
  (PoolCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  MemPool e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
poolAllocImpl parentAlloc (MemPool hdr) reqSz reqAlign = do
  let f = projectPtrs hdr
  slotSz <- peek (getPtrOf (mpsSlotSize f))
  slotAl <- peek (getPtrOf (mpsSlotAlign f))
  outRef <- newRef (nullPtr @e @Word8)

  -- Return NULL if requested size or alignment exceeds pool's configured slot geometry
  whenM_ ((reqSz `le` slotSz) `land` (reqAlign `le` slotAl)) $ do
    freeHd <- peek (getPtrOf (mpsFreeHead f))
    ifThenElseM_
      (ptrToWord64Expr freeHd `ne` 0)
      -- 1. O(1) pop from embedded intrusive free-list
      ( do
          let nextCell = fromVoidPtr (toVoidPtr freeHd) :: e (Ptr (Ptr Word8))
          nxtFree <- peek nextCell
          poke (getPtrOf (mpsFreeHead f)) nxtFree
          writeRef outRef freeHd
      )
      -- 2. Lazy bump in active slab or warm pre-allocated slab, else allocate new slab
      ( do
          cur0 <- peek (getPtrOf (mpsCurSlab f))
          placedRef <- newRef false
          whenM_ (ptrToWord64Expr cur0 `ne` 0) $ do
            let sf = projectPtrs cur0
            bumped <- peek (getPtrOf (pshBumped sf))
            cap <- peek (getPtrOf (pshSlotsCap sf))
            whenM_ (bumped `lt` cap) $ do
              base <- peek (getPtrOf (pshPayloadBase sf))
              poke (getPtrOf (pshBumped sf)) (bumped + 1)
              writeRef outRef (ptrAdd base (bumped * slotSz))
              writeRef placedRef true

          walkRef <- newRef cur0
          placed0 <- readRef placedRef
          whenM_ (lnot placed0 `land` (ptrToWord64Expr cur0 `ne` 0)) $ do
            nxt0 <- peek (getPtrOf (pshNext (projectPtrs cur0)))
            (foundWarm, (_, warmBase)) <-
              runDrain (drainFind (false, nullPtr @e @Word8) fst) $
                streamM (poolSlabChainFrom nxt0) $ \nxt -> do
                  writeRef walkRef nxt
                  let nsf = projectPtrs nxt
                  cap <- peek (getPtrOf (pshSlotsCap nsf))
                  base <- peek (getPtrOf (pshPayloadBase nsf))
                  whenM_ (cap `gt` 0) $ do
                    poke (getPtrOf (pshBumped nsf)) 1
                    poke (getPtrOf (mpsCurSlab f)) nxt
                  pure (cap `gt` 0, base)
            whenM_ foundWarm $ do
              writeRef outRef warmBase
              writeRef placedRef true

          placedAfter <- readRef placedRef
          whenM_ (lnot placedAfter) $ do
            slotsPerSlab <- peek (getPtrOf (mpsSlotsPerSlab f))
            let allocAl = ifThenElse (slotAl `gt` fromIntegral janusSIMDAlignment) slotAl (fromIntegral janusSIMDAlignment)
                totalBytes = fromIntegral poolSlabHdrBytes + slotsPerSlab * slotSz + slotAl
            rawSlab <- parentAlloc totalBytes allocAl
            whenM_ (lnot (isNullVoidPtr rawSlab)) $ do
              let newSlab = fromVoidPtr rawSlab :: e (Ptr (CStruct PoolSlabHdr))
                  sf = projectPtrs newSlab
                  afterHdr = ptrAdd (fromVoidPtr rawSlab :: e (Ptr Word8)) (fromIntegral poolSlabHdrBytes)
                  alignedW = alignUpWord64 (ptrToWord64Expr afterHdr) (toIntegral slotAl :: e Word64)
                  base = word64ToPtrExpr alignedW :: e (Ptr Word8)
              poke (getPtrOf (pshNext sf)) nullPtr
              poke (getPtrOf (pshPayloadBase sf)) base
              poke (getPtrOf (pshSlotsCap sf)) slotsPerSlab
              poke (getPtrOf (pshBumped sf)) 1
              poke (getPtrOf (pshAllocBytes sf)) totalBytes

              tailSlab <- readRef walkRef
              ifThenElseM_
                (ptrToWord64Expr tailSlab `eq` 0)
                (poke (getPtrOf (mpsFirstSlab f)) newSlab)
                (poke (getPtrOf (pshNext (projectPtrs tailSlab))) (fromVoidPtr (toVoidPtr newSlab)))
              poke (getPtrOf (mpsCurSlab f)) newSlab
              writeRef outRef base
      )

  toVoidPtr <$> readRef outRef

poolReallocImpl ::
  forall m e.
  (PoolCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  MemPool e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
poolReallocImpl parentAlloc mp@(MemPool hdr) oldPtr oldSz newSz reqAlign = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (isNullVoidPtr oldPtr)
    (poolAllocImpl parentAlloc mp newSz reqAlign >>= writeRef outRef . fromVoidPtr)
    ( ifThenElseM_
        (newSz `le` 0)
        (poolFreeImpl mp oldPtr >> writeRef outRef (nullPtr @e @Word8))
        ( do
            let f = projectPtrs hdr
            slotSz <- peek (getPtrOf (mpsSlotSize f))
            slotAl <- peek (getPtrOf (mpsSlotAlign f))
            -- If newSz & reqAlign still fit in the same fixed-size slot, resize is O(1) no-op!
            whenM_ ((newSz `le` slotSz) `land` (reqAlign `le` slotAl)) $
              writeRef outRef (fromVoidPtr oldPtr)
        )
    )
  let _ = oldSz
  toVoidPtr <$> readRef outRef

poolFreeImpl ::
  forall m e.
  (PoolCtx m e) =>
  MemPool e ->
  e (Ptr ()) ->
  m ()
poolFreeImpl (MemPool hdr) ptr =
  whenM_ (lnot (isNullVoidPtr ptr)) $ do
    let f = projectPtrs hdr
        cell = fromVoidPtr ptr :: e (Ptr (Ptr Word8))
    curFree <- peek (getPtrOf (mpsFreeHead f))
    poke cell curFree
    poke (getPtrOf (mpsFreeHead f)) (fromVoidPtr ptr)

-- | O(numSlabs) warm reset: clears free-list and resets bump cursors across all retained slabs.
poolResetImpl ::
  forall m e.
  (PoolCtx m e) =>
  MemPool e ->
  m ()
poolResetImpl (MemPool hdr) = do
  let f = projectPtrs hdr
  firstSlab <- peek (getPtrOf (mpsFirstSlab f))
  poke (getPtrOf (mpsFreeHead f)) nullPtr
  poke (getPtrOf (mpsCurSlab f)) firstSlab
  runDrain
    (drainForEach (\s -> poke (getPtrOf (pshBumped (projectPtrs s))) 0))
    (poolSlabChainFrom (fromVoidPtr (toVoidPtr firstSlab)))

poolDestroyImpl ::
  forall m e.
  (PoolCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  e Int64 ->
  MemPool e ->
  m ()
poolDestroyImpl parentFree spineSz (MemPool hdr) = do
  let f = projectPtrs hdr
  firstSlab <- peek (getPtrOf (mpsFirstSlab f))
  runDrain
    ( drainForEach $ \s -> do
        tot <- peek (getPtrOf (pshAllocBytes (projectPtrs s)))
        parentFree (toVoidPtr s) tot
    )
    (poolSlabChainFrom (fromVoidPtr (toVoidPtr firstSlab)))
  parentFree (toVoidPtr hdr) spineSz

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (MemPool (JanusCT _)) = MemPool Identity

deriving via
  ViaPtrHandle t (CStruct MemPoolSpine) (MemPool Identity)
  instance
    JanusCArg t (MemPool (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct MemPoolSpine) (MemPool Identity)
  instance
    JanusCValRet t (MemPool (JanusCT t))
