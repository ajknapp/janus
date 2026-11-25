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

module Janus.Data.BloomFilter
  ( -- * Cache-Line Blocked 512-Bit Split-Block Bloom Filter Spine & Handle
    BloomFilterSpine (..),
    BloomFilter (..),
    BloomFilterCtx,

    -- * Lifecycle
    newWithBlocks,
    newWithBlocksSeeded,
    newForCapacity,
    free,

    -- * Queries, Raw Pointer Access, & Clear
    clear,
    numBlocks,
    bitCount,
    wordsPtr,

    -- * Single-Cache-Line Insertion, Membership Probe, Union, & Cardinality Estimate
    insert,
    mightContain,
    unionInto,
    estimatedPopcount,

    -- * SPMD / SIMD Cache-Line & Batch Operations
    spmdInsert,
    spmdMightContain,
    mightContainBatchSPMD,
    spmdUnionInto,
    spmdEstimatedPopcount,

    -- * Stream Consumers & Drains
    drainToBloomFilter,
    fromStream,
    drainAppendBloomFilter,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocStruct,
    allocZeroedAligned,
    freeBytes,
    freeStruct,
  )
import Janus.Backend.C
import Janus.Command.Cond (whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), janusSIMDAlignment)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, drainLast, drainSum, runDrain)
import Janus.Control.Stream (Stream, iterateS, rangeS, streamM, unfoldWhileM)
import Janus.Data.BitSet (popcountWord64)
import Janus.Data.HKD
import Janus.Data.HashMap (HashFilterCtx, HashKey (..))
import Janus.Data.Layout (Elem, ScalarBuf (..), foldBufSPMD, foreachBufSPMD)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpFloatingCast (..), ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (defaultHashSalt)
import Janus.Expression.Ord (ExpOrd (..), max')
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    gangAll,
    gangReduceAdd,
    liftSPMD,
    spmdAlignedUnitStride,
    spmdFromLanes,
    uniform,
    uniformAlignedPtr,
    withSPMD,
  )

--------------------------------------------------------------------------------
-- 8 Distinct Odd 64-Bit Multipliers (1 Bit Per Word64 Lane of 64B Cache Line)
--------------------------------------------------------------------------------

saltLane0, saltLane1, saltLane2, saltLane3, saltLane4, saltLane5, saltLane6, saltLane7 :: Word64
saltLane0 = 0x9E3779B97F4A7C15
saltLane1 = 0xBF58476D1CE4E5B9
saltLane2 = 0x94D049BB133111EB
saltLane3 = 0xD6E8FEB86659FD93
saltLane4 = 0xA5A35625AA3F88D5
saltLane5 = 0xC2B2AE3D27D4EB4F
saltLane6 = 0x165667B19E3779F9
saltLane7 = 0x85EBCA77C2B2AE63

laneBitMask :: forall e. (ExpBits e Word64, Num (e Word64)) => e Word64 -> Word64 -> e Word64
laneBitMask h mulC =
  (1 :: e Word64) `lshift` ((h * fromIntegral mulC) `rshift` 58)

--------------------------------------------------------------------------------
-- BloomFilter Spine & Handle
--------------------------------------------------------------------------------

-- | Spine struct for a 64-byte cache-line-aligned 512-bit split-block Bloom filter.
-- @bfsWords@ holds @bfsNumBlocks * 8@ @Word64@s (@bfsNumBlocks@ is always a power of 2 @>= 1@).
data BloomFilterSpine k f = BloomFilterSpine
  { bfsWords :: !(f (Ptr Word64)),
    bfsNumBlocks :: !(f Int64),
    bfsSeed :: !(f Word64)
  }
  deriving stock (Generic)

deriving stock instance Eq (BloomFilterSpine k Identity)

deriving stock instance Show (BloomFilterSpine k Identity)

instance (Typeable k) => JanusStruct (BloomFilterSpine k)

newtype BloomFilter (e :: K.Type -> K.Type) k = BloomFilter
  { unBloomFilter :: e (Ptr (CStruct (BloomFilterSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (BloomFilter Identity k)

deriving stock instance Show (BloomFilter Identity k)

type BloomFilterCtx m e k = HashFilterCtx m e k

--------------------------------------------------------------------------------
-- Lifecycle & Queries
--------------------------------------------------------------------------------

-- | Allocate a zero-initialized split-block 'BloomFilter' with power-of-2 @>= 1@
-- 512-bit (64-byte) blocks and explicit hash @seed@.
newWithBlocksSeeded ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  Alloc e ->
  e Int64 ->
  e Word64 ->
  m (BloomFilter e k)
newWithBlocksSeeded alloc reqBlocks seed = do
  hdr <- allocStruct @(CStruct (BloomFilterSpine k)) alloc
  let f = projectPtrs hdr
  (_, nBlocks) <- runDrain (drainFind 1 (`ge` reqBlocks)) (iterateS 1 (`lshift` 1))
  vp <- allocZeroedAligned alloc (nBlocks `lshift` 6) (fromIntegral janusSIMDAlignment)
  let ws = fromVoidPtr vp :: e (Ptr Word64)
  poke (getPtrOf (bfsWords f)) ws
  poke (getPtrOf (bfsNumBlocks f)) nBlocks
  poke (getPtrOf (bfsSeed f)) seed
  pure (BloomFilter hdr)

-- | Allocate a zero-initialized split-block 'BloomFilter' rounding @reqBlocks@ up
-- to a power of 2 @>= 1@.
newWithBlocks ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (BloomFilter e k)
newWithBlocks alloc reqBlocks = newWithBlocksSeeded @k alloc reqBlocks defaultHashSalt

-- | Allocate a split-block 'BloomFilter' sized at ~12 bits/key (rounded up to
-- power-of-2 512-bit cache-line blocks, targeting @< 0.5%@ FPR).
newForCapacity ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (BloomFilter e k)
newForCapacity alloc expectedKeys = do
  cap <- letM (max' 1 expectedKeys)
  reqBlocks <- letM (((cap * 12) + 511) `rshift` 9)
  newWithBlocks @k alloc reqBlocks

free ::
  (BloomFilterCtx m e k) =>
  Alloc e ->
  BloomFilter e k ->
  m ()
free alloc (BloomFilter hdr) = do
  let f = projectPtrs hdr
  nb <- peek (getPtrOf (bfsNumBlocks f))
  whenM_ (nb `gt` 0) $ do
    ws <- peek (getPtrOf (bfsWords f))
    freeBytes alloc (toVoidPtr ws) (nb `lshift` 6)
  freeStruct alloc hdr

clear ::
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  m ()
clear (BloomFilter hdr) = do
  let f = projectPtrs hdr
  nb <- peek (getPtrOf (bfsNumBlocks f))
  whenM_ (nb `gt` 0) $ do
    ws <- peek (getPtrOf (bfsWords f))
    cmdMemset (toVoidPtr ws) 0 (nb `lshift` 6)

numBlocks :: (BloomFilterCtx m e k) => BloomFilter e k -> m (e Int64)
numBlocks (BloomFilter hdr) = peek (getPtrOf (bfsNumBlocks (projectPtrs hdr)))

bitCount :: (BloomFilterCtx m e k) => BloomFilter e k -> m (e Int64)
bitCount bf = (`lshift` 9) <$> numBlocks bf

wordsPtr :: (BloomFilterCtx m e k) => BloomFilter e k -> m (e (Ptr Word64))
wordsPtr (BloomFilter hdr) = peek (getPtrOf (bfsWords (projectPtrs hdr)))

--------------------------------------------------------------------------------
-- Single-Cache-Line Insert, MightContain, UnionInto, & EstimatedPopcount
--------------------------------------------------------------------------------

-- | Insert @key@ by setting 8 bits inside a single 64-byte cache-line block.
insert ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  Elem e k ->
  m ()
insert (BloomFilter hdr) key = do
  let f = projectPtrs hdr
  ws <- peek (getPtrOf (bfsWords f))
  nb <- peek (getPtrOf (bfsNumBlocks f))
  seed <- peek (getPtrOf (bfsSeed f))
  h <- letM (hashElemWithSalt @e @k seed key)
  blk <- letM ((toIntegral (h `rshift` 32) :: e Int64) `iand` (nb - 1))
  base <- letM (blk `lshift` 3)
  w0 <- peekElemOff ws base
  w1 <- peekElemOff ws (base + 1)
  w2 <- peekElemOff ws (base + 2)
  w3 <- peekElemOff ws (base + 3)
  w4 <- peekElemOff ws (base + 4)
  w5 <- peekElemOff ws (base + 5)
  w6 <- peekElemOff ws (base + 6)
  w7 <- peekElemOff ws (base + 7)
  pokeElemOff ws (w0 `ior` laneBitMask h saltLane0) base
  pokeElemOff ws (w1 `ior` laneBitMask h saltLane1) (base + 1)
  pokeElemOff ws (w2 `ior` laneBitMask h saltLane2) (base + 2)
  pokeElemOff ws (w3 `ior` laneBitMask h saltLane3) (base + 3)
  pokeElemOff ws (w4 `ior` laneBitMask h saltLane4) (base + 4)
  pokeElemOff ws (w5 `ior` laneBitMask h saltLane5) (base + 5)
  pokeElemOff ws (w6 `ior` laneBitMask h saltLane6) (base + 6)
  pokeElemOff ws (w7 `ior` laneBitMask h saltLane7) (base + 7)

-- | Probe whether @key@ may be present (guaranteed 0 false negatives; single 64-byte cache-line touch).
mightContain ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  Elem e k ->
  m (e Bool)
mightContain (BloomFilter hdr) key = do
  let f = projectPtrs hdr
  ws <- peek (getPtrOf (bfsWords f))
  nb <- peek (getPtrOf (bfsNumBlocks f))
  seed <- peek (getPtrOf (bfsSeed f))
  h <- letM (hashElemWithSalt @e @k seed key)
  blk <- letM ((toIntegral (h `rshift` 32) :: e Int64) `iand` (nb - 1))
  base <- letM (blk `lshift` 3)
  w0 <- peekElemOff ws base
  w1 <- peekElemOff ws (base + 1)
  w2 <- peekElemOff ws (base + 2)
  w3 <- peekElemOff ws (base + 3)
  w4 <- peekElemOff ws (base + 4)
  w5 <- peekElemOff ws (base + 5)
  w6 <- peekElemOff ws (base + 6)
  w7 <- peekElemOff ws (base + 7)
  pure $
    ((w0 `iand` laneBitMask h saltLane0) `ne` 0)
      `land` ((w1 `iand` laneBitMask h saltLane1) `ne` 0)
      `land` ((w2 `iand` laneBitMask h saltLane2) `ne` 0)
      `land` ((w3 `iand` laneBitMask h saltLane3) `ne` 0)
      `land` ((w4 `iand` laneBitMask h saltLane4) `ne` 0)
      `land` ((w5 `iand` laneBitMask h saltLane5) `ne` 0)
      `land` ((w6 `iand` laneBitMask h saltLane6) `ne` 0)
      `land` ((w7 `iand` laneBitMask h saltLane7) `ne` 0)

-- | Bitwise-OR @src@ into @dst@ when both filters have matching block count and seed.
unionInto ::
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  BloomFilter e k ->
  m ()
unionInto (BloomFilter hdrDst) (BloomFilter hdrSrc) = do
  let fDst = projectPtrs hdrDst
      fSrc = projectPtrs hdrSrc
  nbDst <- peek (getPtrOf (bfsNumBlocks fDst))
  nbSrc <- peek (getPtrOf (bfsNumBlocks fSrc))
  seedDst <- peek (getPtrOf (bfsSeed fDst))
  seedSrc <- peek (getPtrOf (bfsSeed fSrc))
  whenM_ ((nbDst `eq` nbSrc) `land` (seedDst `eq` seedSrc)) $ do
    dstWs <- peek (getPtrOf (bfsWords fDst))
    srcWs <- peek (getPtrOf (bfsWords fSrc))
    totalWords <- letM (nbDst `lshift` 3)
    runDrain
      ( drainForEach $ \i -> do
          dw <- peekElemOff dstWs i
          sw <- peekElemOff srcWs i
          pokeElemOff dstWs (dw `ior` sw) i
      )
      (rangeS 0 totalWords)

-- | Swamidass-Baldi cardinality estimate @round(-(m / 8) * ln(1 - X / m))@ computed
-- from SWAR 64-bit population count @X@ across all blocks.
estimatedPopcount ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  m (e Int64)
estimatedPopcount (BloomFilter hdr) = do
  let f = projectPtrs hdr
  nb <- peek (getPtrOf (bfsNumBlocks f))
  ws <- peek (getPtrOf (bfsWords f))
  totalWords <- letM (nb `lshift` 3)
  setBits <-
    runDrain drainSum $
      streamM (rangeS 0 totalWords) $ \i ->
        popcountWord64 <$> peekElemOff ws i
  estimateFromSetBits nb setBits

estimateFromSetBits ::
  forall m e.
  (CmdAlloc m e) =>
  e Int64 ->
  e Int64 ->
  m (e Int64)
estimateFromSetBits nb setBits = do
  totalBits <- letM (nb `lshift` 9)
  outRef <- newRef (0 :: e Int64)
  whenM_ (setBits `gt` 0) $ do
    let mD = toFloating totalBits :: e Double
        xD = toFloating setBits :: e Double
        target =
          ifThenElse
            (setBits `lt` totalBits)
            ((0.0 - (mD / 8.0)) * log (1.0 - (xD / mD)) + 0.5)
            ((mD / 8.0) * log mD + 0.5)
        upper = totalBits `lshift` 3
    (_, rounded) <-
      runDrain
        (drainLast (0 :: e Int64))
        ( unfoldWhileM
            (0 :: e Int64, upper)
            (\(lo, hi) -> pure (lo `lt` hi))
            ( \(lo, hi) -> do
                mid <- letM ((lo + hi + 1) `rshift` 1)
                let ok = (toFloating mid :: e Double) `le` target
                    lo' = ifThenElse ok mid lo
                    hi' = ifThenElse ok hi (mid - 1)
                pure ((lo', hi'), lo')
            )
        )
    writeRef outRef rounded
  readRef outRef

--------------------------------------------------------------------------------
-- SPMD / SIMD Cache-Line & Batch Operations
--------------------------------------------------------------------------------

blockLaneMasks8 ::
  forall m e.
  (Num (e Word64), SPMDGang 8 m e) =>
  Proxy m ->
  e Word64 ->
  SPMD 8 e Word64
blockLaneMasks8 _ h =
  let salts =
        spmdFromLanes @8
          [ fromIntegral saltLane0,
            fromIntegral saltLane1,
            fromIntegral saltLane2,
            fromIntegral saltLane3,
            fromIntegral saltLane4,
            fromIntegral saltLane5,
            fromIntegral saltLane6,
            fromIntegral saltLane7
          ] ::
          SPMD 8 e Word64
   in (1 :: SPMD 8 e Word64) `lshift` ((uniform h * salts) `rshift` 58)

-- | Insert @key@ using a single 8-lane (512-bit cache-line) SPMD gang load-OR-store.
spmdInsert ::
  forall k m e.
  (BloomFilterCtx m e k, SPMDGang 8 m e) =>
  BloomFilter e k ->
  Elem e k ->
  m ()
spmdInsert (BloomFilter hdr) key = do
  let f = projectPtrs hdr
  ws <- peek (getPtrOf (bfsWords f))
  nb <- peek (getPtrOf (bfsNumBlocks f))
  seed <- peek (getPtrOf (bfsSeed f))
  h <- letM (hashElemWithSalt @e @k seed key)
  blk <- letM ((toIntegral (h `rshift` 32) :: e Int64) `iand` (nb - 1))
  base <- letM (blk `lshift` 3)
  let wsSp = uniformAlignedPtr ws
      idx = spmdAlignedUnitStride base
      masks = blockLaneMasks8 (Proxy @m) h
  withSPMD (Proxy @8) $ do
    cur <- peekElemOff wsSp idx
    pokeElemOff wsSp (cur `ior` masks) idx

-- | Probe @key@ using a single 8-lane (512-bit cache-line) SPMD gang load and 'gangAll'.
spmdMightContain ::
  forall k m e.
  (BloomFilterCtx m e k, SPMDGang 8 m e) =>
  BloomFilter e k ->
  Elem e k ->
  m (e Bool)
spmdMightContain (BloomFilter hdr) key = do
  let f = projectPtrs hdr
  ws <- peek (getPtrOf (bfsWords f))
  nb <- peek (getPtrOf (bfsNumBlocks f))
  seed <- peek (getPtrOf (bfsSeed f))
  h <- letM (hashElemWithSalt @e @k seed key)
  blk <- letM ((toIntegral (h `rshift` 32) :: e Int64) `iand` (nb - 1))
  base <- letM (blk `lshift` 3)
  let wsSp = uniformAlignedPtr ws
      idx = spmdAlignedUnitStride base
      masks = blockLaneMasks8 (Proxy @m) h
  withSPMD (Proxy @8) $ do
    cur <- peekElemOff wsSp idx
    pure (gangAll ((cur `iand` masks) `ne` 0))

-- | Batch SIMD / SPMD probe testing @w@ keys in parallel across their respective blocks.
mightContainBatchSPMD ::
  forall w k m e.
  (BloomFilterCtx m e k, SPMDGang w m e, HashKey (SPMD w e) k) =>
  Proxy w ->
  BloomFilter e k ->
  Elem (SPMD w e) k ->
  SPMDM w m e (SPMD w e Bool)
mightContainBatchSPMD _ (BloomFilter hdr) keys = do
  let f = projectPtrs hdr
  ws <- liftSPMD (peek (getPtrOf (bfsWords f)))
  nb <- liftSPMD (peek (getPtrOf (bfsNumBlocks f)))
  seed <- liftSPMD (peek (getPtrOf (bfsSeed f)))
  h <- letM (hashElemWithSalt @(SPMD w e) @k (uniform seed) keys)
  base <- letM (((toIntegral (h `rshift` 32) :: SPMD w e Int64) `iand` uniform (nb - 1)) `lshift` 3)
  let wsSp = uniformAlignedPtr ws
  w0 <- peekElemOff wsSp base
  w1 <- peekElemOff wsSp (base + 1)
  w2 <- peekElemOff wsSp (base + 2)
  w3 <- peekElemOff wsSp (base + 3)
  w4 <- peekElemOff wsSp (base + 4)
  w5 <- peekElemOff wsSp (base + 5)
  w6 <- peekElemOff wsSp (base + 6)
  w7 <- peekElemOff wsSp (base + 7)
  pure $
    ((w0 `iand` laneBitMask @(SPMD w e) h saltLane0) `ne` 0)
      `land` ((w1 `iand` laneBitMask @(SPMD w e) h saltLane1) `ne` 0)
      `land` ((w2 `iand` laneBitMask @(SPMD w e) h saltLane2) `ne` 0)
      `land` ((w3 `iand` laneBitMask @(SPMD w e) h saltLane3) `ne` 0)
      `land` ((w4 `iand` laneBitMask @(SPMD w e) h saltLane4) `ne` 0)
      `land` ((w5 `iand` laneBitMask @(SPMD w e) h saltLane5) `ne` 0)
      `land` ((w6 `iand` laneBitMask @(SPMD w e) h saltLane6) `ne` 0)
      `land` ((w7 `iand` laneBitMask @(SPMD w e) h saltLane7) `ne` 0)

-- | Strip-mined 64B-aligned SPMD gang union @dst |= src@ when block count and seed match.
spmdUnionInto ::
  forall w k m e.
  (BloomFilterCtx m e k, SPMDGang w m e) =>
  Proxy w ->
  BloomFilter e k ->
  BloomFilter e k ->
  m ()
spmdUnionInto pw (BloomFilter hdrDst) (BloomFilter hdrSrc) = do
  let fDst = projectPtrs hdrDst
      fSrc = projectPtrs hdrSrc
  nbDst <- peek (getPtrOf (bfsNumBlocks fDst))
  nbSrc <- peek (getPtrOf (bfsNumBlocks fSrc))
  seedDst <- peek (getPtrOf (bfsSeed fDst))
  seedSrc <- peek (getPtrOf (bfsSeed fSrc))
  whenM_ ((nbDst `eq` nbSrc) `land` (seedDst `eq` seedSrc)) $ do
    dstWs <- peek (getPtrOf (bfsWords fDst))
    srcWs <- peek (getPtrOf (bfsWords fSrc))
    totalWords <- letM (nbDst `lshift` 3)
    let srcSp = uniformAlignedPtr srcWs
    foreachBufSPMD @w @Word64 pw True (ScalarBuf dstWs) totalWords $ \idx dw -> do
      sw <- peekElemOff srcSp idx
      pure (dw `ior` sw)

-- | Strip-mined SIMD SWAR population count + Swamidass-Baldi cardinality estimate.
spmdEstimatedPopcount ::
  forall w k m e.
  (BloomFilterCtx m e k, SPMDGang w m e) =>
  Proxy w ->
  BloomFilter e k ->
  m (e Int64)
spmdEstimatedPopcount pw (BloomFilter hdr) = do
  let f = projectPtrs hdr
  nb <- peek (getPtrOf (bfsNumBlocks f))
  ws <- peek (getPtrOf (bfsWords f))
  totalWords <- letM (nb `lshift` 3)
  setBits <-
    gangReduceAdd
      <$> foldBufSPMD @w @Word64 @Int64 pw True (ScalarBuf ws) totalWords (0 :: SPMD w e Int64) (\acc _ w -> pure (acc + popcountWord64 w))
  estimateFromSetBits nb setBits

--------------------------------------------------------------------------------
-- Drains & Bulk Stream Construction
--------------------------------------------------------------------------------

drainToBloomFilter ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  Alloc e ->
  e Int64 ->
  Drain m (Elem e k) (BloomFilter e k)
drainToBloomFilter alloc expectedCap = Drain $ \s -> do
  bf <- newForCapacity @k alloc expectedCap
  runDrain (drainAppendBloomFilter bf) s
  pure bf

fromStream ::
  forall k m e.
  (BloomFilterCtx m e k) =>
  Alloc e ->
  e Int64 ->
  Stream m (Elem e k) ->
  m (BloomFilter e k)
fromStream alloc expectedCap = runDrain (drainToBloomFilter @k alloc expectedCap)

drainAppendBloomFilter ::
  (BloomFilterCtx m e k) =>
  BloomFilter e k ->
  Drain m (Elem e k) ()
drainAppendBloomFilter bf = drainForEach (insert bf)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type BloomFilterABI k = Typeable k

type instance JanusCHost (BloomFilter (JanusCT _) k) = BloomFilter Identity k

deriving via
  ViaPtrHandle t (CStruct (BloomFilterSpine k)) (BloomFilter Identity k)
  instance
    (BloomFilterABI k) => JanusCArg t (BloomFilter (JanusCT t) k)

deriving via
  ViaPtrHandle t (CStruct (BloomFilterSpine k)) (BloomFilter Identity k)
  instance
    (BloomFilterABI k) => JanusCValRet t (BloomFilter (JanusCT t) k)
