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

module Janus.Alloc.Monotonic
  ( MonoSlabHdr (..),
    MonotonicSpine (..),
    Monotonic (..),
    initMonotonicRaw,
    monotonicOwnsPtr,
    monotonicAllocImpl,
    monotonicReallocImpl,
    monotonicResetImpl,
    monotonicDestroyImpl,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, alignUpWord64, isNullVoidPtr, sameVoidPtr)
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (drainAny, drainForEach, runDrain)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), janusSIMDAlignment)
import Janus.Control.Stream (Stream, streamM, unfoldWhileM)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

-- | 64-byte padded slab header (4 * 8B = 32B + 4 * 8B padding = 64B so payload starts 64B aligned).
data MonoSlabHdr f = MonoSlabHdr
  { mshNext :: !(f (Ptr Word8)),
    mshPayloadCap :: !(f Int64),
    mshOffset :: !(f Int64),
    mshAllocBytes :: !(f Int64),
    mshLastPtr :: !(f (Ptr Word8)),
    mshPad1 :: !(f Int64),
    mshPad2 :: !(f Int64),
    mshPad3 :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (MonoSlabHdr Identity)

deriving stock instance Show (MonoSlabHdr Identity)

instance JanusStruct MonoSlabHdr

monoSlabHeaderBytes :: Int64
monoSlabHeaderBytes = 64

data MonotonicSpine f = MonotonicSpine
  { msHeadSlab :: !(f (Ptr (CStruct MonoSlabHdr))),
    msNextSlabCap :: !(f Int64),
    msMinAlign :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (MonotonicSpine Identity)

deriving stock instance Show (MonotonicSpine Identity)

instance JanusStruct MonotonicSpine

newtype Monotonic e = Monotonic
  { unMonotonic :: e (Ptr (CStruct MonotonicSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (Monotonic Identity)

deriving stock instance Show (Monotonic Identity)

instance JanusHKD Monotonic

initMonotonicRaw ::
  (CmdAllocBase m e, JanusTyped e (CStruct MonoSlabHdr), ExpPtr e (CStruct MonoSlabHdr), CmdStorable m e (Ptr (CStruct MonoSlabHdr))) =>
  e (Ptr (CStruct MonotonicSpine)) ->
  e Int64 ->
  e Int64 ->
  m (Monotonic e)
initMonotonicRaw hdr initCap minAl = do
  let f = projectPtrs hdr
      cap0 = ifThenElse (initCap `lt` 256) 256 initCap
      al0 = ifThenElse (minAl `lt` 1) 1 minAl
  poke (getPtrOf (msHeadSlab f)) nullPtr
  poke (getPtrOf (msNextSlabCap f)) cap0
  poke (getPtrOf (msMinAlign f)) al0
  pure (Monotonic hdr)

slabPayloadBase ::
  forall e.
  (ExpPtr e Word8, ExpPtrCast e, JanusTyped e Word8, JanusTyped e (CStruct MonoSlabHdr), Num (e Int64)) =>
  e (Ptr (CStruct MonoSlabHdr)) ->
  e (Ptr Word8)
slabPayloadBase slab =
  ptrAdd (fromVoidPtr (toVoidPtr slab) :: e (Ptr Word8)) (fromIntegral monoSlabHeaderBytes)

monoSlabChainFrom ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr)
  ) =>
  e (Ptr Word8) ->
  Stream m (e (Ptr (CStruct MonoSlabHdr)))
monoSlabChainFrom startRaw =
  unfoldWhileM startRaw (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \p -> do
    let s = fromVoidPtr (toVoidPtr p) :: e (Ptr (CStruct MonoSlabHdr))
    nxt <- peek (getPtrOf (mshNext (projectPtrs s)))
    pure (nxt, s)

monotonicOwnsPtr ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr))
  ) =>
  Monotonic e ->
  e (Ptr ()) ->
  m (e Bool)
monotonicOwnsPtr (Monotonic hdr) vp = do
  let f = projectPtrs hdr
      pW = ptrToWord64Expr (fromVoidPtr vp :: e (Ptr Word8))
  headSlab <- peek (getPtrOf (msHeadSlab f))
  runDrain (drainAny id) $
    streamM (monoSlabChainFrom (fromVoidPtr (toVoidPtr headSlab))) $ \cur -> do
      cap <- peek (getPtrOf (mshPayloadCap (projectPtrs cur)))
      let baseW = ptrToWord64Expr (slabPayloadBase cur)
          endW = baseW + toIntegral cap
      pure ((pW `ge` baseW) `land` (pW `lt` endW))

monotonicAllocImpl ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr))
  ) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  Monotonic e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
monotonicAllocImpl parentAlloc (Monotonic hdr) reqSz reqAlign = do
  let f = projectPtrs hdr
  minAl <- peek (getPtrOf (msMinAlign f))
  effAlI <- letM (ifThenElse (reqAlign `gt` minAl) reqAlign minAl)
  let effAl = ifThenElse (effAlI `lt` 1) 1 effAlI
      effAlW = toIntegral effAl :: e Word64
      szPos = ifThenElse (reqSz `le` 0) effAl reqSz
  headSlab <- peek (getPtrOf (msHeadSlab f))
  outRef <- newRef (nullPtr @e @Word8)
  placedRef <- newRef false

  whenM_ (ptrToWord64Expr headSlab `ne` 0) $ do
    let sf = projectPtrs headSlab
    curOff <- peek (getPtrOf (mshOffset sf))
    cap <- peek (getPtrOf (mshPayloadCap sf))
    let base = slabPayloadBase headSlab
        baseW = ptrToWord64Expr base
        alignedW = alignUpWord64 (baseW + toIntegral curOff) effAlW
        relStart = toIntegral (alignedW - baseW) :: e Int64
    newOff <- letM (relStart + szPos)
    whenM_ (newOff `le` cap) $ do
      let outP = word64ToPtrExpr alignedW :: e (Ptr Word8)
      poke (getPtrOf (mshOffset sf)) newOff
      poke (getPtrOf (mshLastPtr sf)) outP
      writeRef outRef outP
      writeRef placedRef true

  placed <- readRef placedRef
  whenM_ (lnot placed) $ do
    nxtCap <- peek (getPtrOf (msNextSlabCap f))
    let neededPayload = szPos + effAl
        chosenPayload = ifThenElse (neededPayload `gt` nxtCap) neededPayload nxtCap
        slabAllocAl = ifThenElse (effAl `gt` fromIntegral janusSIMDAlignment) effAl (fromIntegral janusSIMDAlignment)
        totalBytes = chosenPayload + fromIntegral monoSlabHeaderBytes
    rawSlab <- parentAlloc totalBytes slabAllocAl
    whenM_ (lnot (isNullVoidPtr rawSlab)) $ do
      let newSlab = fromVoidPtr rawSlab :: e (Ptr (CStruct MonoSlabHdr))
          sf = projectPtrs newSlab
          base = slabPayloadBase newSlab
          baseW = ptrToWord64Expr base
          alignedW = alignUpWord64 baseW effAlW
          relStart = toIntegral (alignedW - baseW) :: e Int64
          newOff = relStart + szPos
          outP = word64ToPtrExpr alignedW :: e (Ptr Word8)
      poke (getPtrOf (mshNext sf)) (fromVoidPtr (toVoidPtr headSlab))
      poke (getPtrOf (mshPayloadCap sf)) chosenPayload
      poke (getPtrOf (mshOffset sf)) newOff
      poke (getPtrOf (mshAllocBytes sf)) totalBytes
      poke (getPtrOf (mshLastPtr sf)) outP
      poke (getPtrOf (msHeadSlab f)) newSlab
      poke (getPtrOf (msNextSlabCap f)) (chosenPayload * 2)
      writeRef outRef outP

  toVoidPtr <$> readRef outRef

monotonicReallocImpl ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr))
  ) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  Monotonic e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
monotonicReallocImpl parentAlloc mono@(Monotonic hdr) oldPtr oldSz newSz reqAlign = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (isNullVoidPtr oldPtr)
    (monotonicAllocImpl parentAlloc mono newSz reqAlign >>= writeRef outRef . fromVoidPtr)
    ( ifThenElseM_
        (newSz `le` 0)
        (writeRef outRef (nullPtr @e @Word8))
        ( do
            let f = projectPtrs hdr
            headSlab <- peek (getPtrOf (msHeadSlab f))
            minAl <- peek (getPtrOf (msMinAlign f))
            let effAl = ifThenElse (reqAlign `gt` minAl) reqAlign minAl
            extendedRef <- newRef false
            whenM_ (ptrToWord64Expr headSlab `ne` 0) $ do
              let sf = projectPtrs headSlab
              lastP <- peek (getPtrOf (mshLastPtr sf))
              cap <- peek (getPtrOf (mshPayloadCap sf))
              let oldW = ptrToWord64Expr (fromVoidPtr oldPtr :: e (Ptr Word8))
                  isAligned = (oldW `iand` toIntegral (effAl - 1)) `eq` 0
                  baseW = ptrToWord64Expr (slabPayloadBase headSlab)
                  inPlaceEnd = toIntegral (oldW - baseW) + newSz
              whenM_ (sameVoidPtr oldPtr (toVoidPtr lastP) `land` isAligned `land` (inPlaceEnd `le` cap)) $ do
                poke (getPtrOf (mshOffset sf)) inPlaceEnd
                writeRef outRef (fromVoidPtr oldPtr)
                writeRef extendedRef true
            ext <- readRef extendedRef
            whenM_ (lnot ext) $ do
              np <- monotonicAllocImpl parentAlloc mono newSz reqAlign
              whenM_ (lnot (isNullVoidPtr np)) $ do
                let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                cmdMemcpy np oldPtr copyLen
              writeRef outRef (fromVoidPtr np)
        )
    )
  toVoidPtr <$> readRef outRef

-- | Reset monotonic allocator, keeping the largest head slab warm and freeing
-- older slabs back to the parent allocator.
monotonicResetImpl ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr))
  ) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Monotonic e ->
  m ()
monotonicResetImpl parentFree (Monotonic hdr) = do
  let f = projectPtrs hdr
  headSlab <- peek (getPtrOf (msHeadSlab f))
  whenM_ (ptrToWord64Expr headSlab `ne` 0) $ do
    let hsf = projectPtrs headSlab
    restRaw <- peek (getPtrOf (mshNext hsf))
    poke (getPtrOf (mshNext hsf)) nullPtr
    poke (getPtrOf (mshOffset hsf)) 0
    poke (getPtrOf (mshLastPtr hsf)) nullPtr
    runDrain
      ( drainForEach $ \cur -> do
          totSz <- peek (getPtrOf (mshAllocBytes (projectPtrs cur)))
          parentFree (toVoidPtr cur) totSz
      )
      (monoSlabChainFrom restRaw)

-- | Free all slabs and the spine struct back to the parent allocator.
monotonicDestroyImpl ::
  forall m e.
  ( CmdAllocBase m e,
    JanusTyped e (CStruct MonotonicSpine),
    JanusTyped e (CStruct MonoSlabHdr),
    ExpPtr e (CStruct MonoSlabHdr),
    CmdStorable m e (Ptr (CStruct MonoSlabHdr))
  ) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  e Int64 ->
  Monotonic e ->
  m ()
monotonicDestroyImpl parentFree spineSz (Monotonic hdr) = do
  let f = projectPtrs hdr
  headSlab <- peek (getPtrOf (msHeadSlab f))
  runDrain
    ( drainForEach $ \cur -> do
        totSz <- peek (getPtrOf (mshAllocBytes (projectPtrs cur)))
        parentFree (toVoidPtr cur) totSz
    )
    (monoSlabChainFrom (fromVoidPtr (toVoidPtr headSlab)))
  parentFree (toVoidPtr hdr) spineSz

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (Monotonic (JanusCT _)) = Monotonic Identity

deriving via
  ViaPtrHandle t (CStruct MonotonicSpine) (Monotonic Identity)
  instance
    JanusCArg t (Monotonic (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct MonotonicSpine) (Monotonic Identity)
  instance
    JanusCValRet t (Monotonic (JanusCT t))
