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

module Janus.Alloc.Arena
  ( ArenaSlabHdr (..),
    ArenaLargeHdr (..),
    ArenaSpine (..),
    Arena (..),
    ArenaMark (..),
    initArenaRaw,
    markArena,
    arenaOwnsPtr,
    arenaAllocImpl,
    arenaReallocImpl,
    arenaRewindImpl,
    arenaResetImpl,
    arenaDestroyImpl,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, alignUpWord64, isNullVoidPtr, sameVoidPtr)
import Janus.Backend.C.Raw
import Control.Applicative ((<|>))
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), janusSIMDAlignment)
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (ExpJanus, withJanusType)
import Janus.Control.Drain (drainAny, drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, streamM, unfoldWhileM)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))

-- | 64-byte padded slab header so payload begins on a 64-byte boundary.
data ArenaSlabHdr f = ArenaSlabHdr
  { ashNext :: !(f (Ptr Word8)),
    ashPayloadCap :: !(f Int64),
    ashOffset :: !(f Int64),
    ashAllocBytes :: !(f Int64),
    ashLastPtr :: !(f (Ptr Word8)),
    ashPad1 :: !(f Int64),
    ashPad2 :: !(f Int64),
    ashPad3 :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (ArenaSlabHdr Identity)

deriving stock instance Show (ArenaSlabHdr Identity)

instance JanusStruct ArenaSlabHdr

-- | 64-byte padded dedicated large-block header.
data ArenaLargeHdr f = ArenaLargeHdr
  { alhNext :: !(f (Ptr Word8)),
    alhPayloadCap :: !(f Int64),
    alhAllocBytes :: !(f Int64),
    alhPayloadPtr :: !(f (Ptr Word8)),
    alhPad1 :: !(f Int64),
    alhPad2 :: !(f Int64),
    alhPad3 :: !(f Int64),
    alhPad4 :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (ArenaLargeHdr Identity)

deriving stock instance Show (ArenaLargeHdr Identity)

instance JanusStruct ArenaLargeHdr

arenaHdrBytes :: Int64
arenaHdrBytes = 64

data ArenaSpine f = ArenaSpine
  { asFirstSlab :: !(f (Ptr (CStruct ArenaSlabHdr))),
    asCurSlab :: !(f (Ptr (CStruct ArenaSlabHdr))),
    asLargeHead :: !(f (Ptr (CStruct ArenaLargeHdr))),
    asSlabCap :: !(f Int64),
    asMinAlign :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (ArenaSpine Identity)

deriving stock instance Show (ArenaSpine Identity)

instance JanusStruct ArenaSpine

newtype Arena e = Arena
  { unArena :: e (Ptr (CStruct ArenaSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (Arena Identity)

deriving stock instance Show (Arena Identity)

instance JanusHKD Arena

-- | Lightweight savepoint capturing current slab cursor and large-block head.
data ArenaMark e = ArenaMark
  { amCurSlab :: !(e (Ptr Word8)),
    amSlabOffset :: !(e Int64),
    amLargeHead :: !(e (Ptr Word8))
  }
  deriving stock (Generic)

deriving stock instance Eq (ArenaMark Identity)

deriving stock instance Show (ArenaMark Identity)

type ArenaCtx m e = CmdAllocBase m e

initArenaRaw ::
  (ArenaCtx m e) =>
  e (Ptr (CStruct ArenaSpine)) ->
  e Int64 ->
  e Int64 ->
  m (Arena e)
initArenaRaw hdr slabCap minAl = do
  let f = projectPtrs hdr
      cap0 = ifThenElse (slabCap `lt` 512) 512 slabCap
      al0 = ifThenElse (minAl `lt` 1) 1 minAl
  poke (getPtrOf (asFirstSlab f)) nullPtr
  poke (getPtrOf (asCurSlab f)) nullPtr
  poke (getPtrOf (asLargeHead f)) nullPtr
  poke (getPtrOf (asSlabCap f)) cap0
  poke (getPtrOf (asMinAlign f)) al0
  pure (Arena hdr)

arenaSlabPayload ::
  forall e.
  (ExpJanus e) =>
  e (Ptr (CStruct ArenaSlabHdr)) ->
  e (Ptr Word8)
arenaSlabPayload slab =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    ptrAdd (fromVoidPtr (toVoidPtr slab) :: e (Ptr Word8)) (fromIntegral arenaHdrBytes)

markArena :: forall m e. (ArenaCtx m e) => Arena e -> m (ArenaMark e)
markArena (Arena hdr) =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    withJanusType @(CStruct ArenaLargeHdr) @e $ do
      let f = projectPtrs hdr
      curSlab <- peek (getPtrOf (asCurSlab f))
      largeHead <- peek (getPtrOf (asLargeHead f))
      offRef <- newRef 0
      whenM_ (ptrToWord64Expr curSlab `ne` 0) $ do
        let sf = projectPtrs curSlab
        peek (getPtrOf (ashOffset sf)) >>= writeRef offRef
      off <- readRef offRef
      pure
        ArenaMark
          { amCurSlab = fromVoidPtr (toVoidPtr curSlab),
            amSlabOffset = off,
            amLargeHead = fromVoidPtr (toVoidPtr largeHead)
          }

slabChainFrom ::
  forall m e.
  (ArenaCtx m e) =>
  e (Ptr Word8) ->
  Stream m (e (Ptr (CStruct ArenaSlabHdr)))
slabChainFrom startRaw =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    unfoldWhileM startRaw (\p -> pure (ptrToWord64Expr p `ne` 0)) $ \p -> do
      let s = fromVoidPtr (toVoidPtr p) :: e (Ptr (CStruct ArenaSlabHdr))
      nxt <- peek (getPtrOf (ashNext (projectPtrs s)))
      pure (nxt, s)

largeChainUntil ::
  forall m e.
  (ArenaCtx m e) =>
  e (Ptr Word8) ->
  e Word64 ->
  Stream m (e (Ptr (CStruct ArenaLargeHdr)))
largeChainUntil startRaw stopW =
  withJanusType @(CStruct ArenaLargeHdr) @e $
    unfoldWhileM
      startRaw
      (\p -> let w = ptrToWord64Expr p in pure ((w `ne` 0) `land` (w `ne` stopW)))
      $ \p -> do
        let lg = fromVoidPtr (toVoidPtr p) :: e (Ptr (CStruct ArenaLargeHdr))
        nxt <- peek (getPtrOf (alhNext (projectPtrs lg)))
        pure (nxt, lg)

arenaOwnsPtr ::
  forall m e.
  (ArenaCtx m e) =>
  Arena e ->
  e (Ptr ()) ->
  m (e Bool)
arenaOwnsPtr (Arena hdr) vp =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    withJanusType @(CStruct ArenaLargeHdr) @e $ do
      let f = projectPtrs hdr
          pW = ptrToWord64Expr (fromVoidPtr vp :: e (Ptr Word8))
      firstSlab <- peek (getPtrOf (asFirstSlab f))
      firstLg <- peek (getPtrOf (asLargeHead f))
      let slabHits =
            streamM (slabChainFrom (fromVoidPtr (toVoidPtr firstSlab))) $ \cur -> do
              cap <- peek (getPtrOf (ashPayloadCap (projectPtrs cur)))
              let baseW = ptrToWord64Expr (arenaSlabPayload cur)
                  endW = baseW + toIntegral cap
              pure ((pW `ge` baseW) `land` (pW `lt` endW))
          largeHits =
            streamM (largeChainUntil (fromVoidPtr (toVoidPtr firstLg)) 0) $ \lg -> do
              let lf = projectPtrs lg
              payP <- peek (getPtrOf (alhPayloadPtr lf))
              cap <- peek (getPtrOf (alhPayloadCap lf))
              let baseW = ptrToWord64Expr payP
                  endW = baseW + toIntegral cap
              pure ((pW `ge` baseW) `land` (pW `lt` endW))
      runDrain (drainAny id) (slabHits <|> largeHits)

tryBumpInSlab ::
  forall m e.
  (ArenaCtx m e) =>
  e (Ptr (CStruct ArenaSlabHdr)) ->
  e Int64 ->
  e Word64 ->
  m (e Bool, e (Ptr Word8))
tryBumpInSlab slab szPos effAlW = do
  let sf = projectPtrs slab
  curOff <- peek (getPtrOf (ashOffset sf))
  cap <- peek (getPtrOf (ashPayloadCap sf))
  let base = arenaSlabPayload slab
      baseW = ptrToWord64Expr base
      alignedW = alignUpWord64 (baseW + toIntegral curOff) effAlW
      relStart = toIntegral (alignedW - baseW) :: e Int64
  newOff <- letM (relStart + szPos)
  okRef <- newRef false
  outRef <- newRef (nullPtr @e @Word8)
  whenM_ (newOff `le` cap) $ do
    let outP = word64ToPtrExpr alignedW :: e (Ptr Word8)
    poke (getPtrOf (ashOffset sf)) newOff
    poke (getPtrOf (ashLastPtr sf)) outP
    writeRef okRef true
    writeRef outRef outP
  (,) <$> readRef okRef <*> readRef outRef

arenaAllocImpl ::
  forall m e.
  (ArenaCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  Arena e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
arenaAllocImpl parentAlloc (Arena hdr) reqSz reqAlign =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    withJanusType @(CStruct ArenaLargeHdr) @e $ do
  let f = projectPtrs hdr
  minAl <- peek (getPtrOf (asMinAlign f))
  slabCap <- peek (getPtrOf (asSlabCap f))
  effAlI <- letM (ifThenElse (reqAlign `gt` minAl) reqAlign minAl)
  let effAl = ifThenElse (effAlI `lt` 1) 1 effAlI
      effAlW = toIntegral effAl :: e Word64
      szPos = ifThenElse (reqSz `le` 0) effAl reqSz
      largeThreshold = slabCap `quotient` 2
  outRef <- newRef (nullPtr @e @Word8)

  ifThenElseM_
    (szPos `gt` largeThreshold)
    -- Dedicated large-block bypass (keeps active slab hot & unfragmented)
    ( do
        let allocAl = ifThenElse (effAl `gt` fromIntegral janusSIMDAlignment) effAl (fromIntegral janusSIMDAlignment)
            totalBytes = fromIntegral arenaHdrBytes + szPos + effAl
        rawBlk <- parentAlloc totalBytes allocAl
        whenM_ (lnot (isNullVoidPtr rawBlk)) $ do
          let lg = fromVoidPtr rawBlk :: e (Ptr (CStruct ArenaLargeHdr))
              lf = projectPtrs lg
              rawAfterHdr = ptrAdd (fromVoidPtr rawBlk :: e (Ptr Word8)) (fromIntegral arenaHdrBytes)
              alignedW = alignUpWord64 (ptrToWord64Expr rawAfterHdr) effAlW
              payP = word64ToPtrExpr alignedW :: e (Ptr Word8)
          oldLg <- peek (getPtrOf (asLargeHead f))
          poke (getPtrOf (alhNext lf)) (fromVoidPtr (toVoidPtr oldLg))
          poke (getPtrOf (alhPayloadCap lf)) szPos
          poke (getPtrOf (alhAllocBytes lf)) totalBytes
          poke (getPtrOf (alhPayloadPtr lf)) payP
          poke (getPtrOf (asLargeHead f)) lg
          writeRef outRef payP
    )
    -- Normal slab bump allocation (reusing retained warm slabs if present)
    ( do
        curSlab0 <- peek (getPtrOf (asCurSlab f))
        placedRef <- newRef false
        whenM_ (ptrToWord64Expr curSlab0 `ne` 0) $ do
          (ok, p) <- tryBumpInSlab curSlab0 szPos effAlW
          whenM_ ok $ writeRef outRef p >> writeRef placedRef true

        -- Walk forward along any warm pre-allocated slabs from a prior reset/rewind
        walkRef <- newRef curSlab0
        placed0 <- readRef placedRef
        whenM_ (lnot placed0 `land` (ptrToWord64Expr curSlab0 `ne` 0)) $ do
          nxt0 <- peek (getPtrOf (ashNext (projectPtrs curSlab0)))
          (foundWarm, (_, warmP)) <-
            runDrain (drainFind (false, nullPtr @e @Word8) fst) $
              streamM (slabChainFrom nxt0) $ \nxt -> do
                writeRef walkRef nxt
                poke (getPtrOf (ashOffset (projectPtrs nxt))) 0
                poke (getPtrOf (asCurSlab f)) nxt
                tryBumpInSlab nxt szPos effAlW
          whenM_ foundWarm $ do
            writeRef outRef warmP
            writeRef placedRef true

        placedAfterWalk <- readRef placedRef
        whenM_ (lnot placedAfterWalk) $ do
          let payloadCap = ifThenElse ((szPos + effAl) `gt` slabCap) (szPos + effAl) slabCap
              allocAl = ifThenElse (effAl `gt` fromIntegral janusSIMDAlignment) effAl (fromIntegral janusSIMDAlignment)
              totalBytes = fromIntegral arenaHdrBytes + payloadCap
          rawSlab <- parentAlloc totalBytes allocAl
          whenM_ (lnot (isNullVoidPtr rawSlab)) $ do
            let newSlab = fromVoidPtr rawSlab :: e (Ptr (CStruct ArenaSlabHdr))
                sf = projectPtrs newSlab
                base = arenaSlabPayload newSlab
                baseW = ptrToWord64Expr base
                alignedW = alignUpWord64 baseW effAlW
                relStart = toIntegral (alignedW - baseW) :: e Int64
                outP = word64ToPtrExpr alignedW :: e (Ptr Word8)
            poke (getPtrOf (ashNext sf)) nullPtr
            poke (getPtrOf (ashPayloadCap sf)) payloadCap
            poke (getPtrOf (ashOffset sf)) (relStart + szPos)
            poke (getPtrOf (ashAllocBytes sf)) totalBytes
            poke (getPtrOf (ashLastPtr sf)) outP

            tailSlab <- readRef walkRef
            ifThenElseM_
              (ptrToWord64Expr tailSlab `eq` 0)
              (poke (getPtrOf (asFirstSlab f)) newSlab)
              (poke (getPtrOf (ashNext (projectPtrs tailSlab))) (fromVoidPtr (toVoidPtr newSlab)))
            poke (getPtrOf (asCurSlab f)) newSlab
            writeRef outRef outP
    )

  toVoidPtr <$> readRef outRef

arenaReallocImpl ::
  forall m e.
  (ArenaCtx m e) =>
  (e Int64 -> e Int64 -> m (e (Ptr ()))) ->
  Arena e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
arenaReallocImpl parentAlloc ar@(Arena hdr) oldPtr oldSz newSz reqAlign = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (isNullVoidPtr oldPtr)
    (arenaAllocImpl parentAlloc ar newSz reqAlign >>= writeRef outRef . fromVoidPtr)
    ( ifThenElseM_
        (newSz `le` 0)
        (writeRef outRef (nullPtr @e @Word8))
        ( do
            let f = projectPtrs hdr
            curSlab <- peek (getPtrOf (asCurSlab f))
            minAl <- peek (getPtrOf (asMinAlign f))
            let effAl = ifThenElse (reqAlign `gt` minAl) reqAlign minAl
            extRef <- newRef false
            whenM_ (ptrToWord64Expr curSlab `ne` 0) $ do
              let sf = projectPtrs curSlab
              lastP <- peek (getPtrOf (ashLastPtr sf))
              cap <- peek (getPtrOf (ashPayloadCap sf))
              let oldW = ptrToWord64Expr (fromVoidPtr oldPtr :: e (Ptr Word8))
                  isAligned = (oldW `iand` toIntegral (effAl - 1)) `eq` 0
                  baseW = ptrToWord64Expr (arenaSlabPayload curSlab)
                  inPlaceEnd = toIntegral (oldW - baseW) + newSz
              whenM_ (sameVoidPtr oldPtr (toVoidPtr lastP) `land` isAligned `land` (inPlaceEnd `le` cap)) $ do
                poke (getPtrOf (ashOffset sf)) inPlaceEnd
                writeRef outRef (fromVoidPtr oldPtr)
                writeRef extRef true
            ext <- readRef extRef
            whenM_ (lnot ext) $ do
              np <- arenaAllocImpl parentAlloc ar newSz reqAlign
              whenM_ (lnot (isNullVoidPtr np)) $ do
                let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                cmdMemcpy np oldPtr copyLen
              writeRef outRef (fromVoidPtr np)
        )
    )
  toVoidPtr <$> readRef outRef

-- | Rewind an 'Arena' back to a saved 'ArenaMark', freeing any dedicated large
-- blocks allocated after the mark and rewinding slab offsets for warm reuse.
arenaRewindImpl ::
  forall m e.
  (ArenaCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Arena e ->
  ArenaMark e ->
  m ()
arenaRewindImpl parentFree (Arena hdr) mark =
  withJanusType @(CStruct ArenaSlabHdr) @e $
    withJanusType @(CStruct ArenaLargeHdr) @e $ do
  let f = projectPtrs hdr
      targetLgW = ptrToWord64Expr (amLargeHead mark)
  -- 1. Pop & free dedicated large blocks newer than amLargeHead
  lgHead0 <- peek (getPtrOf (asLargeHead f))
  runDrain
    ( drainForEach $ \lg -> do
        tot <- peek (getPtrOf (alhAllocBytes (projectPtrs lg)))
        parentFree (toVoidPtr lg) tot
    )
    (largeChainUntil (fromVoidPtr (toVoidPtr lgHead0)) targetLgW)
  poke (getPtrOf (asLargeHead f)) (fromVoidPtr (toVoidPtr (amLargeHead mark)))

  -- 2. Rewind slab cursor and clear trailing slabs' offsets
  let clearSlab s = do
        let sf = projectPtrs s
        poke (getPtrOf (ashOffset sf)) 0
        poke (getPtrOf (ashLastPtr sf)) nullPtr
      targetSlabW = ptrToWord64Expr (amCurSlab mark)
  ifThenElseM_
    (targetSlabW `eq` 0)
    ( do
        firstSlab <- peek (getPtrOf (asFirstSlab f))
        poke (getPtrOf (asCurSlab f)) firstSlab
        runDrain (drainForEach clearSlab) (slabChainFrom (fromVoidPtr (toVoidPtr firstSlab)))
    )
    ( do
        let targetSlab = fromVoidPtr (toVoidPtr (amCurSlab mark)) :: e (Ptr (CStruct ArenaSlabHdr))
            tsf = projectPtrs targetSlab
        poke (getPtrOf (ashOffset tsf)) (amSlabOffset mark)
        poke (getPtrOf (ashLastPtr tsf)) nullPtr
        poke (getPtrOf (asCurSlab f)) targetSlab
        nxt0 <- peek (getPtrOf (ashNext tsf))
        runDrain (drainForEach clearSlab) (slabChainFrom nxt0)
    )

arenaResetImpl ::
  (ArenaCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  Arena e ->
  m ()
arenaResetImpl parentFree ar =
  arenaRewindImpl parentFree ar (ArenaMark nullPtr 0 nullPtr)

arenaDestroyImpl ::
  forall m e.
  (ArenaCtx m e) =>
  (e (Ptr ()) -> e Int64 -> m ()) ->
  e Int64 ->
  Arena e ->
  m ()
arenaDestroyImpl parentFree spineSz (Arena hdr) =
  withJanusType @(CStruct ArenaSpine) @e $
    withJanusType @(CStruct ArenaSlabHdr) @e $
      withJanusType @(CStruct ArenaLargeHdr) @e $ do
  let f = projectPtrs hdr
  lgHead0 <- peek (getPtrOf (asLargeHead f))
  runDrain
    ( drainForEach $ \lg -> do
        tot <- peek (getPtrOf (alhAllocBytes (projectPtrs lg)))
        parentFree (toVoidPtr lg) tot
    )
    (largeChainUntil (fromVoidPtr (toVoidPtr lgHead0)) 0)
  firstSlab <- peek (getPtrOf (asFirstSlab f))
  runDrain
    ( drainForEach $ \s -> do
        tot <- peek (getPtrOf (ashAllocBytes (projectPtrs s)))
        parentFree (toVoidPtr s) tot
    )
    (slabChainFrom (fromVoidPtr (toVoidPtr firstSlab)))
  parentFree (toVoidPtr hdr) spineSz

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (Arena (JanusCT _)) = Arena Identity

deriving via
  ViaPtrHandle t (CStruct ArenaSpine) (Arena Identity)
  instance
    JanusCArg t (Arena (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct ArenaSpine) (Arena Identity)
  instance
    JanusCValRet t (Arena (JanusCT t))
