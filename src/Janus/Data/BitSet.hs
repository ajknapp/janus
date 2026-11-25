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

module Janus.Data.BitSet
  ( -- * Packed 64-Bit Word BitSet Spine & Handle
    BitSetSpine (..),
    BitSet (..),
    BitSetCtx,

    -- * Branchless 64-Bit SWAR Bit-Manipulation Primitives
    popcountWord64,
    ctzWord64,
    clzWord64,

    -- * Lifecycle
    new,
    withBitCapacity,
    free,

    -- * Capacity Management, Raw Pointers, & Clear
    clear,
    ensureBit,
    wordCapacity,
    bitCapacity,
    wordsPtr,
    wordsAlignedSPMD,

    -- * Single-Bit, Batch SPMD Probe, & Range Operations
    testBit,
    testBitsSPMD,
    setBit,
    clearBit,
    toggleBit,
    setRange,

    -- * Cardinality, Predicates, & Set Relations (Scalar & SPMD)
    popcount,
    spmdPopcount,
    isEmpty,
    equals,
    isSubsetOf,
    spmdIsSubsetOf,
    isDisjointFrom,
    spmdIsDisjointFrom,

    -- * SWAR Bit-Scanning Queries
    findFirstSet,
    findLastSet,
    findNextSetGe,

    -- * In-Place Word-Level & SPMD Gang-Accelerated Set Algebra & Traversals
    foreachWordsSPMD,
    foldWordsSPMD,
    unionInto,
    intersectInto,
    differenceInto,
    xorInto,
    spmdUnionInto,
    spmdIntersectInto,
    spmdDifferenceInto,
    spmdXorInto,

    -- * Word-Skipping Streams, Set-Algebra Streams, & Drains
    stream,
    reverseStream,
    unionS,
    intersectionS,
    differenceS,
    drainToBitSet,
    fromStream,
    drainAppendBitSet,
  )
where

import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import GHC.TypeNats (natVal)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Constraints (CmdJanus)
import Janus.Control.Drain (Drain (..), drainAll, drainFind, drainFirst, drainForEach, drainSum, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, filterMapS, iterateS, rangeS, reverseRangeS, statefulS, streamM)
import Janus.Data.HKD
import Janus.Data.Layout (ScalarBuf (..), foldBufSPMD, foldSliceBufSPMD, foreachBufSPMD)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Ord (ExpOrd (..), max', min')
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    SPMDTyped,
    gangAll,
    gangReduceAdd,
    liftSPMD,
    spmdAlignedUnitStride,
    uniform,
    uniformAlignedPtr,
    withSPMD,
    withSPMDMasked,
  )
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- Branchless 64-Bit SWAR Helpers (Identical Semantics in Identity & JanusC)
--------------------------------------------------------------------------------

-- | Branchless 64-bit SWAR population count returning @[0 .. 64]@.
popcountWord64 ::
  ( ExpLet e,
    ExpBits e Word64,
    ExpIntegralCast e Word64 Int64,
    Num (e Word64),
    JanusTyped e Word64,
    JanusTyped e Int64
  ) =>
  e Word64 ->
  e Int64
popcountWord64 w =
  let_ w $ \w0 ->
    let_ (w0 - ((w0 `rshift` 1) `iand` 0x5555555555555555)) $ \w1 ->
      let_ ((w1 `iand` 0x3333333333333333) + ((w1 `rshift` 2) `iand` 0x3333333333333333)) $ \w2 ->
        let_ ((w2 + (w2 `rshift` 4)) `iand` 0x0F0F0F0F0F0F0F0F) $ \w3 ->
          toIntegral ((w3 * 0x0101010101010101) `rshift` 56)

-- | Branchless 64-bit count trailing zeros (@64@ when @w == 0@, @[0 .. 63]@ otherwise).
ctzWord64 ::
  ( ExpLet e,
    ExpBits e Word64,
    ExpIntegralCast e Word64 Int64,
    Num (e Word64),
    JanusTyped e Word64,
    JanusTyped e Int64
  ) =>
  e Word64 ->
  e Int64
ctzWord64 w =
  let_ w $ \x ->
    let_ (x `iand` (0 - x)) $ \lsb ->
      popcountWord64 (lsb - 1)

-- | Branchless 64-bit count leading zeros (@64@ when @w == 0@, @[0 .. 63]@ otherwise).
clzWord64 ::
  ( ExpLet e,
    ExpBits e Word64,
    ExpIntegralCast e Word64 Int64,
    Num (e Word64),
    JanusTyped e Word64,
    JanusTyped e Int64
  ) =>
  e Word64 ->
  e Int64
clzWord64 w =
  let_ w $ \x0 ->
    let_ (x0 `ior` (x0 `rshift` 1)) $ \x1 ->
      let_ (x1 `ior` (x1 `rshift` 2)) $ \x2 ->
        let_ (x2 `ior` (x2 `rshift` 4)) $ \x3 ->
          let_ (x3 `ior` (x3 `rshift` 8)) $ \x4 ->
            let_ (x4 `ior` (x4 `rshift` 16)) $ \x5 ->
              let_ (x5 `ior` (x5 `rshift` 32)) $ \x6 ->
                popcountWord64 (inot x6)

--------------------------------------------------------------------------------
-- BitSet Spine & Handle
--------------------------------------------------------------------------------

-- | Spine struct for 64B-aligned packed 64-bit word 'BitSet'.
-- All allocated words beyond set bits (including newly grown words) are maintained zeroed.
data BitSetSpine f = BitSetSpine
  { bssWords :: !(f (Ptr Word64)),
    bssWordCap :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (BitSetSpine Identity)

deriving stock instance Show (BitSetSpine Identity)

instance JanusStruct BitSetSpine

newtype BitSet (e :: K.Type -> K.Type) = BitSet
  { unBitSet :: e (Ptr (CStruct BitSetSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (BitSet Identity)

deriving stock instance Show (BitSet Identity)

instance JanusHKD BitSet

type BitSetCtx m e = CmdJanus m e

--------------------------------------------------------------------------------
-- Lifecycle & Capacity Management
--------------------------------------------------------------------------------

new ::
  forall m e.
  (BitSetCtx m e) =>
  Alloc e ->
  m (BitSet e)
new alloc = do
  hdr <- allocStruct @(CStruct BitSetSpine) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (bssWords f)) nullPtr
  poke (getPtrOf (bssWordCap f)) 0
  pure (BitSet hdr)

withBitCapacity ::
  forall m e.
  (BitSetCtx m e) =>
  Alloc e ->
  e Int64 ->
  m (BitSet e)
withBitCapacity alloc bitCap = do
  bs <- new alloc
  whenM_ (bitCap `gt` 0) $
    ensureBit alloc bs (bitCap - 1)
  pure bs

free ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  m ()
free alloc (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  whenM_ (wCap `gt` 0) $ do
    ws <- peek (getPtrOf (bssWords f))
    freeTypedArray @Word64 alloc ws wCap
  freeStruct alloc hdr

clear ::
  (BitSetCtx m e) =>
  BitSet e ->
  m ()
clear (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  whenM_ (wCap `gt` 0) $ do
    ws <- peek (getPtrOf (bssWords f))
    cmdMemset (toVoidPtr ws) 0 (wCap `lshift` 3)

wordCapacity :: (BitSetCtx m e) => BitSet e -> m (e Int64)
wordCapacity (BitSet hdr) = peek (getPtrOf (bssWordCap (projectPtrs hdr)))

bitCapacity :: (BitSetCtx m e) => BitSet e -> m (e Int64)
bitCapacity bs = (`lshift` 6) <$> wordCapacity bs

wordsPtr :: (BitSetCtx m e) => BitSet e -> m (e (Ptr Word64))
wordsPtr (BitSet hdr) = peek (getPtrOf (bssWords (projectPtrs hdr)))

wordsAlignedSPMD :: (BitSetCtx m e) => BitSet e -> m (SPMD w e (Ptr Word64))
wordsAlignedSPMD bs = uniformAlignedPtr <$> wordsPtr bs

ensureWordCap ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  e Int64 ->
  m ()
ensureWordCap alloc (BitSet hdr) reqWords = do
  let f = projectPtrs hdr
  oldCap <- peek (getPtrOf (bssWordCap f))
  whenM_ (reqWords `gt` oldCap) $ do
    let c0 = ifThenElse (oldCap `lt` 4) 4 (oldCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` reqWords)) (iterateS c0 (* 2))
    oldWs <- peek (getPtrOf (bssWords f))
    newWs <- reallocTypedArray @Word64 alloc oldWs oldCap newCap
    cmdMemset (toVoidPtr (ptrIndex newWs oldCap)) 0 ((newCap - oldCap) `lshift` 3)
    poke (getPtrOf (bssWords f)) newWs
    poke (getPtrOf (bssWordCap f)) newCap

-- | Ensure @bs@ can index @bitIdx >= 0@, zero-initializing any newly allocated @Word64@ words.
ensureBit ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  e Int64 ->
  m ()
ensureBit alloc bs bitIdx =
  whenM_ (bitIdx `ge` 0) $
    ensureWordCap alloc bs ((bitIdx `rshift` 6) + 1)

--------------------------------------------------------------------------------
-- Single-Bit & Range Operations
--------------------------------------------------------------------------------

testBit ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  e Int64 ->
  m (e Bool)
testBit (BitSet hdr) bitIdx = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  resRef <- newRef false
  wIdx <- letM (bitIdx `rshift` 6)
  whenM_ ((bitIdx `ge` 0) `land` (wIdx `lt` wCap)) $ do
    ws <- peek (getPtrOf (bssWords f))
    w <- peekElemOff ws wIdx
    let mask = (1 :: e Word64) `lshift` toIntegral (bitIdx `iand` 63)
    writeRef resRef ((w `iand` mask) `ne` 0)
  readRef resRef

-- | Bounds-masked SIMD / SPMD batch bit-membership probe across @w@ indices simultaneously.
testBitsSPMD ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  BitSet e ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Bool)
testBitsSPMD bs bitIdxs = do
  wCap <- liftSPMD (wordCapacity bs)
  wsSp <- liftSPMD (wordsAlignedSPMD bs)
  wIdx <- letM (bitIdxs `rshift` 6)
  let inBounds = (bitIdxs `ge` 0) `land` (wIdx `lt` uniform wCap)
  resRef <- newRef false
  whenM_ inBounds $ do
    w <- peekElemOff wsSp wIdx
    let mask = (1 :: SPMD w e Word64) `lshift` toIntegral (bitIdxs `iand` 63)
    writeRef resRef ((w `iand` mask) `ne` 0)
  readRef resRef

-- | Set @bitIdx >= 0@ (growing if needed) and return its previous boolean value.
setBit ::
  forall m e.
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  e Int64 ->
  m (e Bool)
setBit alloc bs@(BitSet hdr) bitIdx = do
  prevRef <- newRef false
  whenM_ (bitIdx `ge` 0) $ do
    ensureBit alloc bs bitIdx
    let f = projectPtrs hdr
        wIdx = bitIdx `rshift` 6
        mask = (1 :: e Word64) `lshift` toIntegral (bitIdx `iand` 63)
    ws <- peek (getPtrOf (bssWords f))
    w <- peekElemOff ws wIdx
    let wasSet = (w `iand` mask) `ne` 0
    writeRef prevRef wasSet
    pokeElemOff ws (w `ior` mask) wIdx
  readRef prevRef

-- | Clear @bitIdx >= 0@ and return its previous boolean value.
clearBit ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  e Int64 ->
  m (e Bool)
clearBit (BitSet hdr) bitIdx = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  prevRef <- newRef false
  wIdx <- letM (bitIdx `rshift` 6)
  whenM_ ((bitIdx `ge` 0) `land` (wIdx `lt` wCap)) $ do
    ws <- peek (getPtrOf (bssWords f))
    w <- peekElemOff ws wIdx
    let mask = (1 :: e Word64) `lshift` toIntegral (bitIdx `iand` 63)
        wasSet = (w `iand` mask) `ne` 0
    writeRef prevRef wasSet
    whenM_ wasSet $
      pokeElemOff ws (w `iand` inot mask) wIdx
  readRef prevRef

-- | Flip bit @bitIdx >= 0@ (growing if needed).
toggleBit ::
  forall m e.
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  e Int64 ->
  m ()
toggleBit alloc bs@(BitSet hdr) bitIdx =
  whenM_ (bitIdx `ge` 0) $ do
    ensureBit alloc bs bitIdx
    let f = projectPtrs hdr
        wIdx = bitIdx `rshift` 6
        mask = (1 :: e Word64) `lshift` toIntegral (bitIdx `iand` 63)
    ws <- peek (getPtrOf (bssWords f))
    w <- peekElemOff ws wIdx
    pokeElemOff ws (w `ieor` mask) wIdx

-- | Set all bits in the half-open interval @[startBit, endBit)@ (clamped to @>= 0@).
-- Uses strictly @< 64@ shift counts to avoid C undefined behavior on @64@-bit boundaries.
setRange ::
  forall m e.
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  e Int64 ->
  e Int64 ->
  m ()
setRange alloc bs@(BitSet hdr) startBit endBit = do
  lo <- letM (max' 0 startBit)
  whenM_ (lo `lt` endBit) $ do
    lastBit <- letM (endBit - 1)
    ensureBit alloc bs lastBit
    let f = projectPtrs hdr
    ws <- peek (getPtrOf (bssWords f))
    startWord <- letM (lo `rshift` 6)
    lastWord <- letM (lastBit `rshift` 6)
    let loMask = inot (0 :: e Word64) `lshift` toIntegral (lo `iand` 63)
        hiMask = inot (0 :: e Word64) `rshift` toIntegral (63 - (lastBit `iand` 63))
    ifThenElseM_
      (startWord `eq` lastWord)
      ( do
          w <- peekElemOff ws startWord
          pokeElemOff ws (w `ior` (loMask `iand` hiMask)) startWord
      )
      ( do
          wFirst <- peekElemOff ws startWord
          pokeElemOff ws (wFirst `ior` loMask) startWord
          let midStart = startWord + 1
              midWords = lastWord - midStart
          whenM_ (midWords `gt` 0) $
            cmdMemset (toVoidPtr (ptrIndex ws midStart)) 0xFF (midWords `lshift` 3)
          wLast <- peekElemOff ws lastWord
          pokeElemOff ws (wLast `ior` hiMask) lastWord
      )

--------------------------------------------------------------------------------
-- Cardinality, Predicates, & Set Relations
--------------------------------------------------------------------------------

popcount ::
  (BitSetCtx m e) =>
  BitSet e ->
  m (e Int64)
popcount (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  ws <- peek (getPtrOf (bssWords f))
  runDrain drainSum $
    streamM (rangeS 0 wCap) $ \i ->
      popcountWord64 <$> peekElemOff ws i

-- | Strip-mined 64B-aligned SIMD / SPMD SWAR population count across all allocated words.
spmdPopcount ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  BitSet e ->
  m (e Int64)
spmdPopcount pw bs =
  gangReduceAdd <$> foldWordsSPMD pw bs (0 :: SPMD w e Int64) (\acc _ w -> pure (acc + popcountWord64 w))

isEmpty ::
  (BitSetCtx m e) =>
  BitSet e ->
  m (e Bool)
isEmpty (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  ws <- peek (getPtrOf (bssWords f))
  runDrain (drainAll (`eq` 0)) (streamM (rangeS 0 wCap) (peekElemOff ws))

equals ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  m (e Bool)
equals (BitSet hdrA) (BitSet hdrB) = do
  let fA = projectPtrs hdrA
      fB = projectPtrs hdrB
  capA <- peek (getPtrOf (bssWordCap fA))
  capB <- peek (getPtrOf (bssWordCap fB))
  wsA <- peek (getPtrOf (bssWords fA))
  wsB <- peek (getPtrOf (bssWords fB))
  common <- letM (min' capA capB)
  okCommon <-
    runDrain (drainAll id) $
      streamM (rangeS 0 common) $ \i ->
        eq <$> peekElemOff wsA i <*> peekElemOff wsB i
  resRef <- newRef okCommon
  whenM_ okCommon $ do
    whenM_ (capA `gt` common) $ do
      okTailA <- runDrain (drainAll (`eq` 0)) (streamM (rangeS common capA) (peekElemOff wsA))
      writeRef resRef okTailA
    whenM_ (capB `gt` common) $ do
      okTailB <- runDrain (drainAll (`eq` 0)) (streamM (rangeS common capB) (peekElemOff wsB))
      writeRef resRef okTailB
  readRef resRef

isSubsetOf ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  m (e Bool)
isSubsetOf (BitSet hdrSub) (BitSet hdrSup) = do
  let fSub = projectPtrs hdrSub
      fSup = projectPtrs hdrSup
  capSub <- peek (getPtrOf (bssWordCap fSub))
  capSup <- peek (getPtrOf (bssWordCap fSup))
  wsSub <- peek (getPtrOf (bssWords fSub))
  wsSup <- peek (getPtrOf (bssWords fSup))
  common <- letM (min' capSub capSup)
  okCommon <-
    runDrain (drainAll id) $
      streamM (rangeS 0 common) $ \i -> do
        wSub <- peekElemOff wsSub i
        wSup <- peekElemOff wsSup i
        pure ((wSub `iand` inot wSup) `eq` 0)
  resRef <- newRef okCommon
  whenM_ (okCommon `land` (capSub `gt` common)) $ do
    okTail <- runDrain (drainAll (`eq` 0)) (streamM (rangeS common capSub) (peekElemOff wsSub))
    writeRef resRef okTail
  readRef resRef

-- | Strip-mined branch-free SIMD / SPMD subset test @sub \subseteq sup@.
spmdIsSubsetOf ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  BitSet e ->
  BitSet e ->
  m (e Bool)
spmdIsSubsetOf pw sub sup = do
  capSub <- wordCapacity sub
  capSup <- wordCapacity sup
  common <- letM (min' capSub capSup)
  wsSubRaw <- wordsPtr sub
  wsSupSp <- wordsAlignedSPMD sup
  badCommon <-
    foldBufSPMD @w @Word64 @Word64 pw True (ScalarBuf wsSubRaw) common (0 :: SPMD w e Word64) $ \acc idx wSub -> do
      wSup <- peekElemOff wsSupSp idx
      pure (acc `ior` (wSub `iand` inot wSup))
  bad <-
    foldSliceBufSPMD @w @Word64 @Word64 pw False (ScalarBuf wsSubRaw) common (capSub - common) common badCommon $ \acc _ wSub ->
      pure (acc `ior` wSub)
  pure (gangAll (bad `eq` 0))

isDisjointFrom ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  m (e Bool)
isDisjointFrom (BitSet hdrA) (BitSet hdrB) = do
  let fA = projectPtrs hdrA
      fB = projectPtrs hdrB
  capA <- peek (getPtrOf (bssWordCap fA))
  capB <- peek (getPtrOf (bssWordCap fB))
  wsA <- peek (getPtrOf (bssWords fA))
  wsB <- peek (getPtrOf (bssWords fB))
  common <- letM (min' capA capB)
  runDrain (drainAll id) $
    streamM (rangeS 0 common) $ \i -> do
      wa <- peekElemOff wsA i
      wb <- peekElemOff wsB i
      pure ((wa `iand` wb) `eq` 0)

-- | Strip-mined branch-free SIMD / SPMD disjointness test @a \cap b = \emptyset@.
spmdIsDisjointFrom ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  BitSet e ->
  BitSet e ->
  m (e Bool)
spmdIsDisjointFrom pw a b = do
  capA <- wordCapacity a
  capB <- wordCapacity b
  common <- letM (min' capA capB)
  wsARaw <- wordsPtr a
  wsBSp <- wordsAlignedSPMD b
  overlap <-
    foldBufSPMD @w @Word64 @Word64 pw True (ScalarBuf wsARaw) common (0 :: SPMD w e Word64) $ \acc idx wa -> do
      wb <- peekElemOff wsBSp idx
      pure (acc `ior` (wa `iand` wb))
  pure (gangAll (overlap `eq` 0))

--------------------------------------------------------------------------------
-- SWAR Bit-Scanning Queries
--------------------------------------------------------------------------------

findFirstSet ::
  (BitSetCtx m e) =>
  BitSet e ->
  m (e Bool, e Int64)
findFirstSet (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  ws <- peek (getPtrOf (bssWords f))
  runDrain (drainFirst 0) $
    filterMapS
      ( \wi -> do
          w <- peekElemOff ws wi
          pure (w `ne` 0, (wi `lshift` 6) + ctzWord64 w)
      )
      (rangeS 0 wCap)

findLastSet ::
  (BitSetCtx m e) =>
  BitSet e ->
  m (e Bool, e Int64)
findLastSet (BitSet hdr) = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  ws <- peek (getPtrOf (bssWords f))
  runDrain (drainFirst 0) $
    filterMapS
      ( \wi -> do
          w <- peekElemOff ws wi
          pure (w `ne` 0, (wi `lshift` 6) + (63 - clzWord64 w))
      )
      (reverseRangeS 0 wCap)

findNextSetGe ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  e Int64 ->
  m (e Bool, e Int64)
findNextSetGe (BitSet hdr) startBit = do
  let f = projectPtrs hdr
  wCap <- peek (getPtrOf (bssWordCap f))
  lo <- letM (max' 0 startBit)
  w0 <- letM (lo `rshift` 6)
  okRef <- newRef false
  valRef <- newRef (0 :: e Int64)
  whenM_ (w0 `lt` wCap) $ do
    ws <- peek (getPtrOf (bssWords f))
    firstW <- peekElemOff ws w0
    let mask0 = inot (0 :: e Word64) `lshift` toIntegral (lo `iand` 63)
    masked0 <- letM (firstW `iand` mask0)
    ifThenElseM_
      (masked0 `ne` 0)
      ( do
          writeRef okRef true
          writeRef valRef ((w0 `lshift` 6) + ctzWord64 masked0)
      )
      ( do
          (foundRest, idxRest) <-
            runDrain (drainFirst 0) $
              filterMapS
                ( \wi -> do
                    w <- peekElemOff ws wi
                    pure (w `ne` 0, (wi `lshift` 6) + ctzWord64 w)
                )
                (rangeS (w0 + 1) wCap)
          writeRef okRef foundRest
          writeRef valRef idxRest
      )
  (,) <$> readRef okRef <*> readRef valRef

--------------------------------------------------------------------------------
-- In-Place Word-Level & SPMD Gang-Accelerated Set Algebra & Traversals
--------------------------------------------------------------------------------

-- | Strip-mine @[0, wordCapacity bs)@ in-place across 64B-aligned @Word64@ gangs.
foreachWordsSPMD ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  BitSet e ->
  (SPMD w e Int64 -> SPMD w e Word64 -> SPMDM w m e (SPMD w e Word64)) ->
  m ()
foreachWordsSPMD pw bs step = do
  wCap <- wordCapacity bs
  whenM_ (wCap `gt` 0) $ do
    wsRaw <- wordsPtr bs
    foreachBufSPMD @w @Word64 pw True (ScalarBuf wsRaw) wCap step

-- | Masked-safe 64B-aligned SIMD fold over @[0, wordCapacity bs)@.
foldWordsSPMD ::
  forall w s m e.
  (BitSetCtx m e, SPMDGang w m e, SPMDTyped w e s) =>
  Proxy w ->
  BitSet e ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> SPMD w e Word64 -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldWordsSPMD pw bs s0 step = do
  wCap <- wordCapacity bs
  wsRaw <- wordsPtr bs
  foldBufSPMD @w @Word64 @s pw True (ScalarBuf wsRaw) wCap s0 step

unionInto ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  BitSet e ->
  m ()
unionInto alloc dst src = do
  srcCap <- wordCapacity src
  whenM_ (srcCap `gt` 0) $ do
    ensureWordCap alloc dst srcCap
    dstWs <- peek (getPtrOf (bssWords (projectPtrs (unBitSet dst))))
    srcWs <- peek (getPtrOf (bssWords (projectPtrs (unBitSet src))))
    runDrain
      ( drainForEach $ \i -> do
          dw <- peekElemOff dstWs i
          sw <- peekElemOff srcWs i
          pokeElemOff dstWs (dw `ior` sw) i
      )
      (rangeS 0 srcCap)

intersectInto ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  m ()
intersectInto (BitSet hdrDst) (BitSet hdrSrc) = do
  let fDst = projectPtrs hdrDst
      fSrc = projectPtrs hdrSrc
  dstCap <- peek (getPtrOf (bssWordCap fDst))
  srcCap <- peek (getPtrOf (bssWordCap fSrc))
  common <- letM (min' dstCap srcCap)
  dstWs <- peek (getPtrOf (bssWords fDst))
  srcWs <- peek (getPtrOf (bssWords fSrc))
  runDrain
    ( drainForEach $ \i -> do
        dw <- peekElemOff dstWs i
        sw <- peekElemOff srcWs i
        pokeElemOff dstWs (dw `iand` sw) i
    )
    (rangeS 0 common)
  whenM_ (dstCap `gt` common) $
    cmdMemset (toVoidPtr (ptrIndex dstWs common)) 0 ((dstCap - common) `lshift` 3)

differenceInto ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  m ()
differenceInto (BitSet hdrDst) (BitSet hdrSrc) = do
  let fDst = projectPtrs hdrDst
      fSrc = projectPtrs hdrSrc
  dstCap <- peek (getPtrOf (bssWordCap fDst))
  srcCap <- peek (getPtrOf (bssWordCap fSrc))
  common <- letM (min' dstCap srcCap)
  dstWs <- peek (getPtrOf (bssWords fDst))
  srcWs <- peek (getPtrOf (bssWords fSrc))
  runDrain
    ( drainForEach $ \i -> do
        dw <- peekElemOff dstWs i
        sw <- peekElemOff srcWs i
        pokeElemOff dstWs (dw `iand` inot sw) i
    )
    (rangeS 0 common)

xorInto ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  BitSet e ->
  m ()
xorInto alloc dst src = do
  srcCap <- wordCapacity src
  whenM_ (srcCap `gt` 0) $ do
    ensureWordCap alloc dst srcCap
    dstWs <- peek (getPtrOf (bssWords (projectPtrs (unBitSet dst))))
    srcWs <- peek (getPtrOf (bssWords (projectPtrs (unBitSet src))))
    runDrain
      ( drainForEach $ \i -> do
          dw <- peekElemOff dstWs i
          sw <- peekElemOff srcWs i
          pokeElemOff dstWs (dw `ieor` sw) i
      )
      (rangeS 0 srcCap)

-- | Strip-mined 64B-aligned SPMD gang union @dst |= src@ over @SPMD w e Word64@.
spmdUnionInto ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  Alloc e ->
  BitSet e ->
  BitSet e ->
  m ()
spmdUnionInto pw alloc dst src = do
  srcCap <- wordCapacity src
  whenM_ (srcCap `gt` 0) $ do
    ensureWordCap alloc dst srcCap
    dstRaw <- peek (getPtrOf (bssWords (projectPtrs (unBitSet dst))))
    srcRaw <- peek (getPtrOf (bssWords (projectPtrs (unBitSet src))))
    let wE = fromIntegral (natVal pw) :: e Int64
        dstSp = uniformAlignedPtr dstRaw
        srcSp = uniformAlignedPtr srcRaw
    numFull <- letM (srcCap `quotient` wE)
    alignedEnd <- letM (numFull * wE)
    runDrain
      ( drainForEach $ \g -> do
          base <- letM (g * wE)
          let idx = spmdAlignedUnitStride base
          withSPMD pw $ do
            dw <- peekElemOff dstSp idx
            sw <- peekElemOff srcSp idx
            pokeElemOff dstSp (dw `ior` sw) idx
      )
      (rangeS 0 numFull)
    whenM_ (alignedEnd `lt` srcCap) $ do
      let tailIdx = spmdAlignedUnitStride alignedEnd
      withSPMDMasked (tailIdx `lt` uniform srcCap) $ do
        dw <- peekElemOff dstSp tailIdx
        sw <- peekElemOff srcSp tailIdx
        pokeElemOff dstSp (dw `ior` sw) tailIdx

-- | Strip-mined 64B-aligned SPMD gang intersection @dst &= src@ over @SPMD w e Word64@.
spmdIntersectInto ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  Alloc e ->
  BitSet e ->
  BitSet e ->
  m ()
spmdIntersectInto pw _alloc (BitSet hdrDst) (BitSet hdrSrc) = do
  let fDst = projectPtrs hdrDst
      fSrc = projectPtrs hdrSrc
  dstCap <- peek (getPtrOf (bssWordCap fDst))
  srcCap <- peek (getPtrOf (bssWordCap fSrc))
  common <- letM (min' dstCap srcCap)
  dstRaw <- peek (getPtrOf (bssWords fDst))
  srcRaw <- peek (getPtrOf (bssWords fSrc))
  whenM_ (common `gt` 0) $ do
    let wE = fromIntegral (natVal pw) :: e Int64
        dstSp = uniformAlignedPtr dstRaw
        srcSp = uniformAlignedPtr srcRaw
    numFull <- letM (common `quotient` wE)
    alignedEnd <- letM (numFull * wE)
    runDrain
      ( drainForEach $ \g -> do
          base <- letM (g * wE)
          let idx = spmdAlignedUnitStride base
          withSPMD pw $ do
            dw <- peekElemOff dstSp idx
            sw <- peekElemOff srcSp idx
            pokeElemOff dstSp (dw `iand` sw) idx
      )
      (rangeS 0 numFull)
    whenM_ (alignedEnd `lt` common) $ do
      let tailIdx = spmdAlignedUnitStride alignedEnd
      withSPMDMasked (tailIdx `lt` uniform common) $ do
        dw <- peekElemOff dstSp tailIdx
        sw <- peekElemOff srcSp tailIdx
        pokeElemOff dstSp (dw `iand` sw) tailIdx
  whenM_ (dstCap `gt` common) $
    cmdMemset (toVoidPtr (ptrIndex dstRaw common)) 0 ((dstCap - common) `lshift` 3)

-- | Strip-mined 64B-aligned SPMD gang difference @dst &= ~src@ over @SPMD w e Word64@.
spmdDifferenceInto ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  BitSet e ->
  BitSet e ->
  m ()
spmdDifferenceInto pw dst src = do
  dstCap <- wordCapacity dst
  srcCap <- wordCapacity src
  common <- letM (min' dstCap srcCap)
  whenM_ (common `gt` 0) $ do
    dstRaw <- wordsPtr dst
    srcSp <- wordsAlignedSPMD src
    foreachBufSPMD @w @Word64 pw True (ScalarBuf dstRaw) common $ \idx dw -> do
      sw <- peekElemOff srcSp idx
      pure (dw `iand` inot sw)

-- | Strip-mined 64B-aligned SPMD gang symmetric difference @dst ^= src@ over @SPMD w e Word64@.
spmdXorInto ::
  forall w m e.
  (BitSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  Alloc e ->
  BitSet e ->
  BitSet e ->
  m ()
spmdXorInto pw alloc dst src = do
  srcCap <- wordCapacity src
  whenM_ (srcCap `gt` 0) $ do
    ensureWordCap alloc dst srcCap
    dstRaw <- wordsPtr dst
    srcSp <- wordsAlignedSPMD src
    foreachBufSPMD @w @Word64 pw True (ScalarBuf dstRaw) srcCap $ \idx dw -> do
      sw <- peekElemOff srcSp idx
      pure (dw `ieor` sw)

--------------------------------------------------------------------------------
-- Word-Skipping Streams, Set-Algebra Streams, & Drains
--------------------------------------------------------------------------------

wordBitsStream ::
  forall m e.
  (BitSetCtx m e) =>
  m (e Int64, e Int64 -> m (e Word64)) ->
  Stream m (e Int64)
wordBitsStream mkSource =
  statefulS
    ( do
        (numWords, fetchWord) <- mkSource
        wiRef <- newRef (0 :: e Int64)
        curWordRef <- newRef (0 :: e Word64)
        pure (numWords, fetchWord, wiRef, curWordRef)
    )
    $ \(numWords, fetchWord, wiRef, curWordRef) -> ContT $ \kStep -> do
      curW <- readRef curWordRef
      ifThenElseM_
        (curW `ne` 0)
        ( do
            wi <- readRef wiRef
            let bitIdx = ((wi - 1) `lshift` 6) + ctzWord64 curW
            writeRef curWordRef (curW `iand` (curW - 1))
            kStep (Yield bitIdx)
        )
        ( do
            wi <- readRef wiRef
            ifThenElseM_
              (wi `ge` numWords)
              (kStep Stop)
              ( do
                  w <- fetchWord wi
                  writeRef wiRef (wi + 1)
                  writeRef curWordRef w
                  kStep Skip
              )
        )

-- | Ascending word-skipping 'Stream' over all set bit indices via 'ctzWord64'.
stream ::
  (BitSetCtx m e) =>
  BitSet e ->
  Stream m (e Int64)
stream (BitSet hdr) =
  let f = projectPtrs hdr
   in wordBitsStream $ do
        wCap <- peek (getPtrOf (bssWordCap f))
        ws <- peek (getPtrOf (bssWords f))
        pure (wCap, peekElemOff ws)

-- | Descending word-skipping 'Stream' over all set bit indices via 'clzWord64'.
reverseStream ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  Stream m (e Int64)
reverseStream (BitSet hdr) =
  let f = projectPtrs hdr
   in statefulS
        ( do
            wCap <- peek (getPtrOf (bssWordCap f))
            ws <- peek (getPtrOf (bssWords f))
            wiRef <- newRef wCap
            curWordRef <- newRef (0 :: e Word64)
            pure (ws, wiRef, curWordRef)
        )
        $ \(ws, wiRef, curWordRef) -> ContT $ \kStep -> do
          curW <- readRef curWordRef
          ifThenElseM_
            (curW `ne` 0)
            ( do
                wiCur <- readRef wiRef
                inWordIdx <- letM (63 - clzWord64 curW)
                let hiMask = (1 :: e Word64) `lshift` toIntegral inWordIdx
                    bitIdx = (wiCur `lshift` 6) + inWordIdx
                writeRef curWordRef (curW `ieor` hiMask)
                kStep (Yield bitIdx)
            )
            ( do
                wi <- readRef wiRef
                ifThenElseM_
                  (wi `le` 0)
                  (kStep Stop)
                  ( do
                      prevWi <- letM (wi - 1)
                      w <- peekElemOff ws prevWi
                      writeRef wiRef prevWi
                      writeRef curWordRef w
                      kStep Skip
                  )
            )

-- | Fused word-level union 'Stream' (@A \cup B@) in ascending order.
unionS ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  Stream m (e Int64)
unionS (BitSet hdrA) (BitSet hdrB) =
  let fA = projectPtrs hdrA
      fB = projectPtrs hdrB
   in wordBitsStream $ do
        capA <- peek (getPtrOf (bssWordCap fA))
        capB <- peek (getPtrOf (bssWordCap fB))
        wsA <- peek (getPtrOf (bssWords fA))
        wsB <- peek (getPtrOf (bssWords fB))
        maxCap <- letM (max' capA capB)
        let fetch wi = do
              waRef <- newRef (0 :: e Word64)
              wbRef <- newRef (0 :: e Word64)
              whenM_ (wi `lt` capA) (peekElemOff wsA wi >>= writeRef waRef)
              whenM_ (wi `lt` capB) (peekElemOff wsB wi >>= writeRef wbRef)
              ior <$> readRef waRef <*> readRef wbRef
        pure (maxCap, fetch)

-- | Fused word-level intersection 'Stream' (@A \cap B@) in ascending order.
intersectionS ::
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  Stream m (e Int64)
intersectionS (BitSet hdrA) (BitSet hdrB) =
  let fA = projectPtrs hdrA
      fB = projectPtrs hdrB
   in wordBitsStream $ do
        capA <- peek (getPtrOf (bssWordCap fA))
        capB <- peek (getPtrOf (bssWordCap fB))
        wsA <- peek (getPtrOf (bssWords fA))
        wsB <- peek (getPtrOf (bssWords fB))
        minCap <- letM (min' capA capB)
        pure (minCap, \wi -> iand <$> peekElemOff wsA wi <*> peekElemOff wsB wi)

-- | Fused word-level set-difference 'Stream' (@A \setminus B@) in ascending order.
differenceS ::
  forall m e.
  (BitSetCtx m e) =>
  BitSet e ->
  BitSet e ->
  Stream m (e Int64)
differenceS (BitSet hdrA) (BitSet hdrB) =
  let fA = projectPtrs hdrA
      fB = projectPtrs hdrB
   in wordBitsStream $ do
        capA <- peek (getPtrOf (bssWordCap fA))
        capB <- peek (getPtrOf (bssWordCap fB))
        wsA <- peek (getPtrOf (bssWords fA))
        wsB <- peek (getPtrOf (bssWords fB))
        let fetch wi = do
              wa <- peekElemOff wsA wi
              outRef <- newRef wa
              whenM_ (wi `lt` capB) $ do
                wb <- peekElemOff wsB wi
                writeRef outRef (wa `iand` inot wb)
              readRef outRef
        pure (capA, fetch)

drainToBitSet ::
  (BitSetCtx m e) =>
  Alloc e ->
  Drain m (e Int64) (BitSet e)
drainToBitSet alloc = Drain $ \s -> do
  bs <- new alloc
  runDrain (drainAppendBitSet alloc bs) s
  pure bs

fromStream ::
  (BitSetCtx m e) =>
  Alloc e ->
  Stream m (e Int64) ->
  m (BitSet e)
fromStream alloc = runDrain (drainToBitSet alloc)

drainAppendBitSet ::
  (BitSetCtx m e) =>
  Alloc e ->
  BitSet e ->
  Drain m (e Int64) ()
drainAppendBitSet alloc bs = drainForEach (\x -> setBit alloc bs x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (BitSet (JanusCT _)) = BitSet Identity

deriving via
  ViaPtrHandle t (CStruct BitSetSpine) (BitSet Identity)
  instance
    JanusCArg t (BitSet (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct BitSetSpine) (BitSet Identity)
  instance
    JanusCValRet t (BitSet (JanusCT t))
