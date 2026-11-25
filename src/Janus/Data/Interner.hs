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

module Janus.Data.Interner
  ( -- * Compact 32-Bit Interned Symbol Handle ($O(1)$ Integer Equality & Hashing!)
    Symbol (..),
    symbolEq,

    -- * Contiguous Slab-Backed SwissTable Byte/String Interner
    InternerSlabHdr (..),
    InternerSpine (..),
    Interner (..),
    InternerCtx,

    -- * Lifecycle & Queries
    new,
    free,
    clear,
    count,
    totalBytes,
    isEmpty,

    -- * Byte-Span Interning, Lookup, & Address-Stable O(1) Resolution
    internBytes,
    lookupBytes,
    resolveSymbol,

    -- * Host `IO` String & Byte-List Helpers
    internByteList,
    lookupByteList,
    resolveByteList,
    internString,
    lookupString,
    resolveString,

    -- * Streams & Drains
    symbolsS,
    entriesS,
    drainAppendInterner,

    -- * First-Class SPMD / SIMD Symbol Metadata Gathers & Folds
    symbolLengthsSPMD,
    symbolHashesSPMD,
    foldSymbolLengthsSPMD,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Char (chr, ord)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    allocBytesAligned,
    allocStruct,
    allocTypedArray,
    freeBytes,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory
  ( CmdMemCopy (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
    janusSIMDAlignment,
    withAlignedArray,
  )
import Janus.Command.Ref (newRef, readRef, writeRef)
import Janus.Constraints (CmdJanus, KnownJanusType)
import Janus.Control.Drain (Drain, drainAll, drainFind, drainFirst, drainForEach, runDrain)
import Janus.Control.Stream
  ( Stream,
    filterMapS,
    iterateS,
    rangeS,
    streamM,
    unfoldWhileM,
    withStreamSetup,
  )
import Janus.Data.HKD
import Janus.Data.HashMap (probeGroupsS, swarBitsS)
import Janus.Data.Layout (ScalarBuf (..), foldBufSPMD)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (defaultHashSalt, hashBytesWithSalt)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    liftSPMD,
    uniform,
    uniformAlignedPtr,
    withSPMD,
  )

--------------------------------------------------------------------------------
-- Compact 32-Bit Symbol Handle
--------------------------------------------------------------------------------

newtype Symbol f = Symbol
  { unSymbol :: f Word32
  }
  deriving stock (Generic)

deriving stock instance Eq (Symbol Identity)

deriving stock instance Ord (Symbol Identity)

deriving stock instance Show (Symbol Identity)

instance JanusStruct Symbol

instance JanusHKD Symbol

symbolEq :: (ExpEq e Word32) => Symbol e -> Symbol e -> e Bool
symbolEq (Symbol a) (Symbol b) = a `eq` b

--------------------------------------------------------------------------------
-- Chained Byte Slab Header & Interner Spine
--------------------------------------------------------------------------------

data InternerSlabHdr f = InternerSlabHdr
  { ishNext :: !(f (Ptr Word8)),
    ishTotalBytes :: !(f Int64),
    ishUsed :: !(f Int64),
    ishCap :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (InternerSlabHdr Identity)

deriving stock instance Show (InternerSlabHdr Identity)

instance JanusStruct InternerSlabHdr

data InternerSpine f = InternerSpine
  { insPtrs :: !(f (Ptr (Ptr Word8))),
    insLens :: !(f (Ptr Int64)),
    insHashes :: !(f (Ptr Word64)),
    insCount :: !(f Int64),
    insDenseCap :: !(f Int64),
    insTotalBytes :: !(f Int64),
    insHeadSlab :: !(f (Ptr Word8)),
    insNextSlabCap :: !(f Int64),
    insCtrl :: !(f (Ptr Word8)),
    insSlots :: !(f (Ptr Word32)),
    insSparseCap :: !(f Int64),
    insGrowthLeft :: !(f Int64),
    insSeed :: !(f Word64)
  }
  deriving stock (Generic)

deriving stock instance Eq (InternerSpine Identity)

deriving stock instance Show (InternerSpine Identity)

instance JanusStruct InternerSpine

newtype Interner (e :: K.Type -> K.Type) = Interner
  { unInterner :: e (Ptr (CStruct InternerSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (Interner Identity)

deriving stock instance Show (Interner Identity)

instance JanusHKD Interner

type InternerCtx m e = CmdJanus m e

ctrlEmptyByte :: Word8
ctrlEmptyByte = 0x80

swarMatchH2 ::
  (ExpBits e Word64, ExpIntegralCast e Word8 Word64, Num (e Word64)) =>
  e Word64 ->
  e Word8 ->
  e Word64
swarMatchH2 w h2 =
  let splat = toIntegral h2 * 0x0101010101010101
      x = w `ieor` splat
      low7 = x `iand` 0x7F7F7F7F7F7F7F7F
      hasZeroLow7 = inot (low7 + 0x7F7F7F7F7F7F7F7F)
   in hasZeroLow7 `iand` inot x `iand` 0x8080808080808080

swarMatchEmpty :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmpty w = w `iand` inot (w `lshift` 6) `iand` 0x8080808080808080

swarLowestByteIdx ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Int64, Num (e Word64)) =>
  e Word64 ->
  e Int64
swarLowestByteIdx m =
  let lsb = (m `iand` (0 - m)) `rshift` 7
   in toIntegral ((lsb * 0x0001020304050607) `rshift` 56)

splitHash ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Int64, ExpIntegralCast e Word64 Word8, Num (e Word64)) =>
  e Word64 ->
  (e Int64, e Word8)
splitHash h =
  (toIntegral (h `rshift` 7), toIntegral (h `iand` 0x7F))

maxLoadForCap :: (ExpBits e Int64, Num (e Int64)) => e Int64 -> e Int64
maxLoadForCap cap = cap - (cap `rshift` 3)

asSlabHdr ::
  (ExpPtrCast e, KnownJanusType e Word8, KnownJanusType e (CStruct InternerSlabHdr)) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct InternerSlabHdr))
asSlabHdr p = fromVoidPtr (toVoidPtr p)

--------------------------------------------------------------------------------
-- Lifecycle & Queries
--------------------------------------------------------------------------------

new ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  m (Interner e)
new alloc = do
  hdr <- allocStruct @(CStruct InternerSpine) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (insPtrs f)) nullPtr
  poke (getPtrOf (insLens f)) nullPtr
  poke (getPtrOf (insHashes f)) nullPtr
  poke (getPtrOf (insCount f)) 0
  poke (getPtrOf (insDenseCap f)) 0
  poke (getPtrOf (insTotalBytes f)) 0
  poke (getPtrOf (insHeadSlab f)) nullPtr
  poke (getPtrOf (insNextSlabCap f)) 4096
  poke (getPtrOf (insCtrl f)) nullPtr
  poke (getPtrOf (insSlots f)) nullPtr
  poke (getPtrOf (insSparseCap f)) 0
  poke (getPtrOf (insGrowthLeft f)) 0
  poke (getPtrOf (insSeed f)) defaultHashSalt
  pure (Interner hdr)

freeAllSlabs ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  e (Ptr Word8) ->
  m ()
freeAllSlabs alloc headRaw = do
  let slabsS =
        unfoldWhileM headRaw (\cur -> pure (ptrToWord64Expr cur `ne` 0)) $ \cur -> do
          let sf = projectPtrs (asSlabHdr cur)
          nxt <- peek (getPtrOf (ishNext sf))
          tot <- peek (getPtrOf (ishTotalBytes sf))
          pure (nxt, (cur, tot))
  runDrain
    (drainForEach (\(cur, tot) -> freeBytes alloc (toVoidPtr cur) tot))
    slabsS

free ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  m ()
free alloc (Interner hdr) = do
  let f = projectPtrs hdr
  headRaw <- peek (getPtrOf (insHeadSlab f))
  freeAllSlabs alloc headRaw

  dCap <- peek (getPtrOf (insDenseCap f))
  whenM_ (dCap `gt` 0) $ do
    ptrs <- peek (getPtrOf (insPtrs f))
    lens <- peek (getPtrOf (insLens f))
    hashes <- peek (getPtrOf (insHashes f))
    freeTypedArray @(Ptr Word8) alloc ptrs dCap
    freeTypedArray @Int64 alloc lens dCap
    freeTypedArray @Word64 alloc hashes dCap

  sCap <- peek (getPtrOf (insSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (insCtrl f))
    slots <- peek (getPtrOf (insSlots f))
    freeBytes alloc (toVoidPtr ctrl) (sCap + 8)
    freeTypedArray @Word32 alloc slots sCap

  freeStruct alloc hdr

-- | Clear all interned symbols, keeping the current head byte slab and hash arrays warm.
clear ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  m ()
clear alloc (Interner hdr) = do
  let f = projectPtrs hdr
  headRaw <- peek (getPtrOf (insHeadSlab f))
  whenM_ (ptrToWord64Expr headRaw `ne` 0) $ do
    let sf = projectPtrs (asSlabHdr headRaw)
    olderRaw <- peek (getPtrOf (ishNext sf))
    freeAllSlabs alloc olderRaw
    poke (getPtrOf (ishNext sf)) nullPtr
    poke (getPtrOf (ishUsed sf)) 0

  sCap <- peek (getPtrOf (insSparseCap f))
  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (insCtrl f))
    cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (sCap + 8)
    poke (getPtrOf (insGrowthLeft f)) (maxLoadForCap sCap)

  poke (getPtrOf (insCount f)) 0
  poke (getPtrOf (insTotalBytes f)) 0

count :: (InternerCtx m e) => Interner e -> m (e Int64)
count (Interner hdr) = peek (getPtrOf (insCount (projectPtrs hdr)))

totalBytes :: (InternerCtx m e) => Interner e -> m (e Int64)
totalBytes (Interner hdr) = peek (getPtrOf (insTotalBytes (projectPtrs hdr)))

isEmpty :: (InternerCtx m e) => Interner e -> m (e Bool)
isEmpty intr = (`eq` 0) <$> count intr

--------------------------------------------------------------------------------
-- Internal Slab Bump, Dense Resize, & SwissTable Rehash
--------------------------------------------------------------------------------

allocSliceInSlabs ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  e (Ptr Word8) ->
  e Int64 ->
  m (e (Ptr Word8))
allocSliceInSlabs alloc (Interner hdr) srcPtr byteLen = do
  outPtrRef <- newRef (nullPtr @e @Word8)
  whenM_ (byteLen `gt` 0) $ do
    let f = projectPtrs hdr
        hdrSz = sizeOf (Proxy @(CStruct InternerSlabHdr))
    headRaw <- peek (getPtrOf (insHeadSlab f))
    fitsHeadRef <- newRef false
    whenM_ (ptrToWord64Expr headRaw `ne` 0) $ do
      let sf = projectPtrs (asSlabHdr headRaw)
      used <- peek (getPtrOf (ishUsed sf))
      cap <- peek (getPtrOf (ishCap sf))
      whenM_ ((used + byteLen) `le` cap) $ do
        let dst = ptrAdd headRaw (hdrSz + used)
        cmdMemcpy (toVoidPtr dst) (toVoidPtr srcPtr) byteLen
        poke (getPtrOf (ishUsed sf)) (used + byteLen)
        writeRef outPtrRef dst
        writeRef fitsHeadRef true

    fitsHead <- readRef fitsHeadRef
    whenM_ (lnot fitsHead) $ do
      nextCapHint <- peek (getPtrOf (insNextSlabCap f))
      let payloadCap = ifThenElse (byteLen `gt` nextCapHint) byteLen nextCapHint
          totBytes = hdrSz + payloadCap
      vp <- allocBytesAligned alloc totBytes (fromIntegral janusSIMDAlignment)
      let newSlabRaw = fromVoidPtr vp :: e (Ptr Word8)
          nsf = projectPtrs (asSlabHdr newSlabRaw)
          dst = ptrAdd newSlabRaw hdrSz
      poke (getPtrOf (ishNext nsf)) headRaw
      poke (getPtrOf (ishTotalBytes nsf)) totBytes
      poke (getPtrOf (ishUsed nsf)) byteLen
      poke (getPtrOf (ishCap nsf)) payloadCap
      cmdMemcpy (toVoidPtr dst) (toVoidPtr srcPtr) byteLen
      poke (getPtrOf (insHeadSlab f)) newSlabRaw
      let grownHint = ifThenElse (nextCapHint `lt` 1048576) (nextCapHint * 2) nextCapHint
      poke (getPtrOf (insNextSlabCap f)) grownHint
      writeRef outPtrRef dst
  readRef outPtrRef

ensureDenseSymbolCap ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  e Int64 ->
  m ()
ensureDenseSymbolCap alloc (Interner hdr) reqCap = do
  let f = projectPtrs hdr
  dCap <- peek (getPtrOf (insDenseCap f))
  whenM_ (reqCap `gt` dCap) $ do
    let c0 = ifThenElse (dCap `lt` 16) 16 (dCap * 2)
    (_, newDCap) <- runDrain (drainFind c0 (`ge` reqCap)) (iterateS c0 (* 2))
    oldPtrs <- peek (getPtrOf (insPtrs f))
    oldLens <- peek (getPtrOf (insLens f))
    oldHashes <- peek (getPtrOf (insHashes f))
    newPtrs <- reallocTypedArray @(Ptr Word8) alloc oldPtrs dCap newDCap
    newLens <- reallocTypedArray @Int64 alloc oldLens dCap newDCap
    newHashes <- reallocTypedArray @Word64 alloc oldHashes dCap newDCap
    poke (getPtrOf (insPtrs f)) newPtrs
    poke (getPtrOf (insLens f)) newLens
    poke (getPtrOf (insHashes f)) newHashes
    poke (getPtrOf (insDenseCap f)) newDCap

rehashSparseTo ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  e Int64 ->
  m ()
rehashSparseTo alloc (Interner hdr) newSCap = do
  let f = projectPtrs hdr
  oldSCap <- peek (getPtrOf (insSparseCap f))
  oldCtrl <- peek (getPtrOf (insCtrl f))
  oldSlots <- peek (getPtrOf (insSlots f))
  curCnt <- peek (getPtrOf (insCount f))
  hashes <- peek (getPtrOf (insHashes f))

  newCtrlVP <- allocBytesAligned alloc (newSCap + 8) (fromIntegral janusSIMDAlignment)
  let newCtrl = fromVoidPtr newCtrlVP :: e (Ptr Word8)
      newCtrlWords = fromVoidPtr newCtrlVP :: e (Ptr Word64)
  cmdMemset newCtrlVP (fromIntegral ctrlEmptyByte) (newSCap + 8)
  newSlots <- allocTypedArray @Word32 alloc newSCap

  let groupMask = (newSCap `rshift` 3) - 1
      insertSym idx = do
        h <- peekElemOff hashes idx
        let (h1, h2) = splitHash h
            g0 = h1 `iand` groupMask
            emptySlotS =
              filterMapS
                ( \g -> do
                    w <- peekElemOff newCtrlWords g
                    let em = swarMatchEmpty w
                    pure (em `ne` 0, (g `lshift` 3) + swarLowestByteIdx em)
                )
                (probeGroupsS g0 groupMask)
        (_, dstSlot) <- runDrain (drainFirst 0) emptySlotS
        pokeElemOff newCtrl h2 dstSlot
        whenM_ (dstSlot `lt` 8) $
          pokeElemOff newCtrl h2 (newSCap + dstSlot)
        pokeElemOff newSlots (toIntegral idx) dstSlot

  runDrain (drainForEach insertSym) (rangeS 0 curCnt)

  whenM_ (oldSCap `gt` 0) $ do
    freeBytes alloc (toVoidPtr oldCtrl) (oldSCap + 8)
    freeTypedArray @Word32 alloc oldSlots oldSCap

  poke (getPtrOf (insCtrl f)) newCtrl
  poke (getPtrOf (insSlots f)) newSlots
  poke (getPtrOf (insSparseCap f)) newSCap
  poke (getPtrOf (insGrowthLeft f)) (maxLoadForCap newSCap - curCnt)

bytesEqual ::
  forall m e.
  (InternerCtx m e) =>
  e (Ptr Word8) ->
  e (Ptr Word8) ->
  e Int64 ->
  m (e Bool)
bytesEqual pa pb n =
  runDrain
    (drainAll id)
    (streamM (rangeS 0 n) (\i -> eq <$> peekElemOff pa i <*> peekElemOff pb i))

-- | Probe for a byte slice @(ptr, effLen)@ with precomputed @h@.
-- Returns @(found, symIdIfFound, insertSlotIfMiss)@.
probeSlice ::
  forall m e.
  (InternerCtx m e) =>
  Interner e ->
  e (Ptr Word8) ->
  e Int64 ->
  e Word64 ->
  m (e Bool, e Word32, e Int64)
probeSlice (Interner hdr) qPtr effLen h = do
  let f = projectPtrs hdr
  sCap <- peek (getPtrOf (insSparseCap f))
  foundRef <- newRef false
  symRef <- newRef (0 :: e Word32)
  slotRef <- newRef (0 :: e Int64)

  whenM_ (sCap `gt` 0) $ do
    ctrl <- peek (getPtrOf (insCtrl f))
    slots <- peek (getPtrOf (insSlots f))
    ptrs <- peek (getPtrOf (insPtrs f))
    lens <- peek (getPtrOf (insLens f))
    hashes <- peek (getPtrOf (insHashes f))
    let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (sCap `rshift` 3) - 1
        (h1, h2) = splitHash h
        g0 = h1 `iand` groupMask
        groupStepS =
          filterMapS
            ( \g -> do
                w <- peekElemOff ctrlWords g
                let matchMask = swarMatchH2 w h2
                    hitSlotS =
                      filterMapS
                        ( \lane -> do
                            let sl = (g `lshift` 3) + lane
                            candSym <- peekElemOff slots sl
                            let idx = toIntegral candSym :: e Int64
                            ch <- peekElemOff hashes idx
                            cl <- peekElemOff lens idx
                            okRef <- newRef false
                            whenM_ ((ch `eq` h) `land` (cl `eq` effLen)) $ do
                              cp <- peekElemOff ptrs idx
                              bytesEqual cp qPtr effLen >>= writeRef okRef
                            ok <- readRef okRef
                            pure (ok, candSym)
                        )
                        (swarBitsS matchMask)
                (hitFound, hitSym) <- runDrain (drainFirst 0) hitSlotS
                stopGrpRef <- newRef false
                ifThenElseM_
                  hitFound
                  ( do
                      writeRef foundRef true
                      writeRef symRef hitSym
                      writeRef stopGrpRef true
                  )
                  ( do
                      let em = swarMatchEmpty w
                      whenM_ (em `ne` 0) $ do
                        writeRef slotRef ((g `lshift` 3) + swarLowestByteIdx em)
                        writeRef stopGrpRef true
                  )
                stopGrp <- readRef stopGrpRef
                pure (stopGrp, ())
            )
            (probeGroupsS g0 groupMask)
    _ <- runDrain (drainFirst ()) groupStepS
    pure ()

  (,,) <$> readRef foundRef <*> readRef symRef <*> readRef slotRef

--------------------------------------------------------------------------------
-- Public Byte-Span Interning, Lookup, & O(1) Resolution
--------------------------------------------------------------------------------

-- | Look up @(ptr, byteLen)@ without interning, returning @(found, symbolOrZero)@.
lookupBytes ::
  forall m e.
  (InternerCtx m e) =>
  Interner e ->
  e (Ptr Word8) ->
  e Int64 ->
  m (e Bool, Symbol e)
lookupBytes intr@(Interner hdr) ptr byteLen = do
  let effLen = ifThenElse (byteLen `lt` 0) 0 byteLen
  seed <- peek (getPtrOf (insSeed (projectPtrs hdr)))
  h <- hashBytesWithSalt seed ptr effLen
  (found, symW, _) <- probeSlice intr ptr effLen h
  pure (found, Symbol symW)

-- | Intern byte slice @[ptr .. ptr + byteLen - 1]@ via @alloc@:
-- * On hit: returns the existing 'Symbol' with zero allocations!
-- * On miss: copies bytes into an address-stable slab and assigns the next sequential 'Symbol'.
internBytes ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  e (Ptr Word8) ->
  e Int64 ->
  m (Symbol e)
internBytes alloc intr@(Interner hdr) ptr byteLen = do
  let f = projectPtrs hdr
      effLen = ifThenElse (byteLen `lt` 0) 0 byteLen
  seed <- peek (getPtrOf (insSeed f))
  h <- hashBytesWithSalt seed ptr effLen
  (found, existingSym, insSlot0) <- probeSlice intr ptr effLen h
  outSymRef <- newRef existingSym

  whenM_ (lnot found) $ do
    insSlotRef <- newRef insSlot0
    gl0 <- peek (getPtrOf (insGrowthLeft f))
    whenM_ (gl0 `le` 0) $ do
      sCap0 <- peek (getPtrOf (insSparseCap f))
      let newSCap = ifThenElse (sCap0 `lt` 16) 16 (sCap0 * 2)
      rehashSparseTo alloc intr newSCap
      (_, _, insSlotAfter) <- probeSlice intr ptr effLen h
      writeRef insSlotRef insSlotAfter

    insSlot <- readRef insSlotRef
    curCnt <- peek (getPtrOf (insCount f))
    ensureDenseSymbolCap alloc intr (curCnt + 1)
    storedPtr <- allocSliceInSlabs alloc intr ptr effLen

    ptrs <- peek (getPtrOf (insPtrs f))
    lens <- peek (getPtrOf (insLens f))
    hashes <- peek (getPtrOf (insHashes f))
    ctrl <- peek (getPtrOf (insCtrl f))
    slots <- peek (getPtrOf (insSlots f))
    sCap <- peek (getPtrOf (insSparseCap f))
    gl <- peek (getPtrOf (insGrowthLeft f))
    totB <- peek (getPtrOf (insTotalBytes f))

    let newSymW = toIntegral curCnt :: e Word32
        (_, h2) = splitHash h
    pokeElemOff ptrs storedPtr curCnt
    pokeElemOff lens effLen curCnt
    pokeElemOff hashes h curCnt

    pokeElemOff ctrl h2 insSlot
    whenM_ (insSlot `lt` 8) $
      pokeElemOff ctrl h2 (sCap + insSlot)
    pokeElemOff slots newSymW insSlot

    poke (getPtrOf (insCount f)) (curCnt + 1)
    poke (getPtrOf (insGrowthLeft f)) (gl - 1)
    poke (getPtrOf (insTotalBytes f)) (totB + effLen)
    writeRef outSymRef newSymW

  Symbol <$> readRef outSymRef

-- | O(1) resolution of a 'Symbol' to @(isValid, stableSlicePtr, byteLen)@.
resolveSymbol ::
  forall m e.
  (InternerCtx m e) =>
  Interner e ->
  Symbol e ->
  m (e Bool, e (Ptr Word8), e Int64)
resolveSymbol (Interner hdr) (Symbol symW) = do
  let f = projectPtrs hdr
      idx = toIntegral symW :: e Int64
  curCnt <- peek (getPtrOf (insCount f))
  okRef <- newRef false
  pRef <- newRef (nullPtr @e @Word8)
  lRef <- newRef (0 :: e Int64)
  whenM_ (idx `lt` curCnt) $ do
    ptrs <- peek (getPtrOf (insPtrs f))
    lens <- peek (getPtrOf (insLens f))
    peekElemOff ptrs idx >>= writeRef pRef
    peekElemOff lens idx >>= writeRef lRef
    writeRef okRef true
  (,,) <$> readRef okRef <*> readRef pRef <*> readRef lRef

--------------------------------------------------------------------------------
-- Host `IO` Convenience Helpers (`[Word8]` & UTF-8 `String`)
--------------------------------------------------------------------------------

encodeUtf8Bytes :: String -> [Word8]
encodeUtf8Bytes = concatMap go
  where
    go ch
      | cp <= 0x7F = [fromIntegral cp]
      | cp <= 0x7FF =
          [ fromIntegral (0xC0 + (cp `shiftR` 6)),
            fromIntegral (0x80 + (cp .&. 0x3F))
          ]
      | cp <= 0xFFFF =
          [ fromIntegral (0xE0 + (cp `shiftR` 12)),
            fromIntegral (0x80 + ((cp `shiftR` 6) .&. 0x3F)),
            fromIntegral (0x80 + (cp .&. 0x3F))
          ]
      | otherwise =
          [ fromIntegral (0xF0 + (cp `shiftR` 18)),
            fromIntegral (0x80 + ((cp `shiftR` 12) .&. 0x3F)),
            fromIntegral (0x80 + ((cp `shiftR` 6) .&. 0x3F)),
            fromIntegral (0x80 + (cp .&. 0x3F))
          ]
      where
        cp = ord ch

decodeUtf8Bytes :: [Word8] -> String
decodeUtf8Bytes [] = []
decodeUtf8Bytes (b0 : rest)
  | b0 < 0x80 = chr (fromIntegral b0) : decodeUtf8Bytes rest
  | b0 < 0xE0,
    (b1 : r1) <- rest =
      let cp = ((fromIntegral (b0 .&. 0x1F) :: Int) * 64) + fromIntegral (b1 .&. 0x3F)
       in chr cp : decodeUtf8Bytes r1
  | b0 < 0xF0,
    (b1 : b2 : r2) <- rest =
      let cp =
            ((fromIntegral (b0 .&. 0x0F) :: Int) * 4096)
              + ((fromIntegral (b1 .&. 0x3F) :: Int) * 64)
              + fromIntegral (b2 .&. 0x3F)
       in chr cp : decodeUtf8Bytes r2
  | (b1 : b2 : b3 : r3) <- rest =
      let cp =
            ((fromIntegral (b0 .&. 0x07) :: Int) * 262144)
              + ((fromIntegral (b1 .&. 0x3F) :: Int) * 4096)
              + ((fromIntegral (b2 .&. 0x3F) :: Int) * 64)
              + fromIntegral (b3 .&. 0x3F)
       in chr cp : decodeUtf8Bytes r3
  | otherwise = '\xFFFD' : decodeUtf8Bytes rest

internByteList :: Alloc Identity -> Interner Identity -> [Word8] -> IO (Symbol Identity)
internByteList alloc intr bs =
  withAlignedArray bs $ \rawPtr ->
    internBytes alloc intr (Identity rawPtr) (Identity (fromIntegral (length bs)))

lookupByteList :: Interner Identity -> [Word8] -> IO (Bool, Symbol Identity)
lookupByteList intr bs =
  withAlignedArray bs $ \rawPtr -> do
    (Identity ok, sym) <- lookupBytes intr (Identity rawPtr) (Identity (fromIntegral (length bs)))
    pure (ok, sym)

resolveByteList :: Interner Identity -> Symbol Identity -> IO (Maybe [Word8])
resolveByteList intr sym = do
  (Identity ok, Identity rawPtr, Identity bLen) <- resolveSymbol intr sym
  if not ok
    then pure Nothing
    else
      if bLen <= 0
        then pure (Just [])
        else Just <$> peekArray (fromIntegral bLen) rawPtr

internString :: Alloc Identity -> Interner Identity -> String -> IO (Symbol Identity)
internString alloc intr = internByteList alloc intr . encodeUtf8Bytes

lookupString :: Interner Identity -> String -> IO (Bool, Symbol Identity)
lookupString intr = lookupByteList intr . encodeUtf8Bytes

resolveString :: Interner Identity -> Symbol Identity -> IO (Maybe String)
resolveString intr sym = fmap decodeUtf8Bytes <$> resolveByteList intr sym

--------------------------------------------------------------------------------
-- Streams & Drains
--------------------------------------------------------------------------------

-- | Contiguous stream of all issued 'Symbol's in assignment order @0 .. count - 1@.
symbolsS ::
  forall m e.
  (InternerCtx m e) =>
  Interner e ->
  Stream m (Symbol e)
symbolsS (Interner hdr) =
  withStreamSetup
    (peek (getPtrOf (insCount (projectPtrs hdr))))
    (\n -> Symbol . toIntegral <$> rangeS 0 n)

-- | Contiguous stream of @(Symbol, slicePtr, byteLen)@ in assignment order @0 .. count - 1@.
entriesS ::
  forall m e.
  (InternerCtx m e) =>
  Interner e ->
  Stream m (Symbol e, e (Ptr Word8), e Int64)
entriesS (Interner hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (insCount f)) <*> peek (getPtrOf (insPtrs f)) <*> peek (getPtrOf (insLens f)))
        $ \(n, ptrs, lens) ->
          streamM (rangeS 0 n) $ \i -> do
            p <- peekElemOff ptrs i
            l <- peekElemOff lens i
            pure (Symbol (toIntegral i), p, l)

-- | Append/intern every @(ptr, byteLen)@ slice from a 'Stream' into an existing 'Interner'.
drainAppendInterner ::
  forall m e.
  (InternerCtx m e) =>
  Alloc e ->
  Interner e ->
  Drain m (e (Ptr Word8), e Int64) ()
drainAppendInterner alloc intr =
  drainForEach (\(p, l) -> internBytes alloc intr p l >> pure ())

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Symbol Metadata Gathers & Folds
--------------------------------------------------------------------------------

-- | Bounds-masked O(1) SIMD gather of interned byte lengths for @w@ 'Symbol' IDs
-- (returning @0@ for out-of-bounds symbol indices).
symbolLengthsSPMD ::
  forall w m e.
  (InternerCtx m e, SPMDGang w m e) =>
  Interner e ->
  SPMD w e Word32 ->
  SPMDM w m e (SPMD w e Int64)
symbolLengthsSPMD (Interner hdr) syms = do
  let f = projectPtrs hdr
      idx64 = toIntegral syms :: SPMD w e Int64
  cnt <- liftSPMD (peek (getPtrOf (insCount f)))
  outRef <- newRef (0 :: SPMD w e Int64)
  whenM_ ((idx64 `ge` 0) `land` (idx64 `lt` uniform cnt)) $ do
    lens <- liftSPMD (peek (getPtrOf (insLens f)))
    peekElemOff (uniformAlignedPtr lens) idx64 >>= writeRef outRef
  readRef outRef

-- | Bounds-masked O(1) SIMD gather of precomputed 64-bit hashes for @w@ 'Symbol' IDs
-- (returning @0@ for out-of-bounds symbol indices).
symbolHashesSPMD ::
  forall w m e.
  (InternerCtx m e, SPMDGang w m e) =>
  Interner e ->
  SPMD w e Word32 ->
  SPMDM w m e (SPMD w e Word64)
symbolHashesSPMD (Interner hdr) syms = do
  let f = projectPtrs hdr
      idx64 = toIntegral syms :: SPMD w e Int64
  cnt <- liftSPMD (peek (getPtrOf (insCount f)))
  outRef <- newRef (0 :: SPMD w e Word64)
  whenM_ ((idx64 `ge` 0) `land` (idx64 `lt` uniform cnt)) $ do
    hashes <- liftSPMD (peek (getPtrOf (insHashes f)))
    peekElemOff (uniformAlignedPtr hashes) idx64 >>= writeRef outRef
  readRef outRef

-- | Masked-safe 64B-aligned SIMD fold over all @count@ interned symbol byte lengths.
foldSymbolLengthsSPMD ::
  forall w s m e.
  (InternerCtx m e, SPMDGang w m e, SPMDTyped w e s) =>
  Proxy w ->
  Interner e ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> SPMD w e Int64 -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSymbolLengthsSPMD pw (Interner hdr) s0 step = do
  let f = projectPtrs hdr
  cnt <- peek (getPtrOf (insCount f))
  accRef <- withSPMD pw (newRef s0)
  whenM_ (cnt `gt` 0) $ do
    lens <- peek (getPtrOf (insLens f))
    res <- foldBufSPMD pw True (ScalarBuf lens) cnt s0 step
    withSPMD pw (writeRef accRef res)
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (Interner (JanusCT _)) = Interner Identity

deriving via
  ViaPtrHandle t (CStruct InternerSpine) (Interner Identity)
  instance
    JanusCArg t (Interner (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct InternerSpine) (Interner Identity)
  instance
    JanusCValRet t (Interner (JanusCT t))
