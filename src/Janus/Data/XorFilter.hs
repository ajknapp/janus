{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Janus.Data.XorFilter
  ( -- * Static 8-Bit Lemire-Graf 3-Partite Xor Filter Spine & Handle
    XorFilterSpine (..),
    XorFilter (..),
    XorFilterCtx,

    -- * Bulk Stream Construction & Lifecycle
    drainToXorFilter,
    fromStream,
    free,

    -- * 3-Load Zero-False-Negative Probe & Queries
    mightContain,
    keyCount,
    byteSize,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocStruct,
    allocTypedArray,
    allocZeroedAligned,
    freeBytes,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), ExpSized (..), janusSIMDAlignment)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFoldM, drainForEach, drain_, runDrain)
import Janus.Control.Stream (Stream, rangeS, reverseRangeS, unfoldWhileM)
import Janus.Data.HKD
import Janus.Data.HashMap (HashKey (..))
import Janus.Data.Layout (Elem, ScalarBuf (..), StorageLayout)
import Janus.Data.Sort (sortBuf)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.MurmurHash (defaultHashSalt, hash64, hash64WithSalt)
import Janus.Expression.Ord (ExpOrd (..), max')
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- XorFilter Spine & Handle
--------------------------------------------------------------------------------

-- | Spine struct for a static 8-bit fingerprint Xor filter (~9.84 bits/key, ~0.39% FPR).
-- Total fingerprint array length in bytes is @3 * xfsBlockLength@ (@0@ when @xfsKeyCount == 0@).
data XorFilterSpine k f = XorFilterSpine
  { xfsFingerprints :: !(f (Ptr Word8)),
    xfsBlockLength :: !(f Int64),
    xfsSeed :: !(f Word64),
    xfsKeyCount :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (XorFilterSpine k Identity)

deriving stock instance Show (XorFilterSpine k Identity)

instance (Typeable k) => JanusStruct (XorFilterSpine k)

newtype XorFilter (e :: K.Type -> K.Type) k = XorFilter
  { unXorFilter :: e (Ptr (CStruct (XorFilterSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (XorFilter Identity k)

deriving stock instance Show (XorFilter Identity k)

type XorFilterCtx m e k =
  ( CmdAlloc m e,
    StorageLayout k,
    HashKey e k,
    Typeable k,
    ExpPtr e Word64,
    JanusTyped e (Ptr Word64),
    CmdStorable m e (Ptr Word64),
    ExpSized e (CStruct (XorFilterSpine k)),
    JanusTyped e (CStruct (XorFilterSpine k))
  )

--------------------------------------------------------------------------------
-- 3-Partite Lemire FastRange Indexing + 8-Bit Fingerprint Extraction
--------------------------------------------------------------------------------

xorPositionsAndFp ::
  forall m e.
  (CmdAlloc m e) =>
  e Word64 ->
  e Int64 ->
  m (e Int64, e Int64, e Int64, e Word8)
xorPositionsAndFp h segLen = do
  segW <- letM (toIntegral segLen :: e Word64)
  hMix <- letM (hash64 (h `ieor` 0xD6E8FEB86659FD93))
  let u0 = h `iand` 0xFFFFFFFF
      u1 = h `rshift` 32
      u2 = hMix `iand` 0xFFFFFFFF
  i0 <- letM (toIntegral ((u0 * segW) `rshift` 32) :: e Int64)
  i1 <- letM (segLen + (toIntegral ((u1 * segW) `rshift` 32) :: e Int64))
  i2 <- letM ((segLen * 2) + (toIntegral ((u2 * segW) `rshift` 32) :: e Int64))
  fp <- letM (toIntegral ((hMix `rshift` 56) `iand` 0xFF) :: e Word8)
  pure (i0, i1, i2, fp)

--------------------------------------------------------------------------------
-- Lemire-Graf 3-Partite Hypergraph Peeling Bulk Construction
--------------------------------------------------------------------------------

-- | Construct a static 'XorFilter' from any 'Stream':
--
-- 1. Collects 64-bit MurmurHashes into a temporary @Alloc e@ buffer.
-- 2. Sorts in-place via 'sortBuf' and compacts unique hashes in @O(N)@.
-- 3. Runs Lemire-Graf 3-partite degree-1 hypergraph peeling (retrying seeds via 'unfoldWhileM').
-- 4. Populates 8-bit fingerprints in reverse peeling order via 'reverseRangeS' and frees all scratch buffers.
drainToXorFilter ::
  forall k m e.
  (XorFilterCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (XorFilter e k)
drainToXorFilter alloc = Drain $ \s -> do
  rawPtrRef <- newRef (nullPtr @e @Word64)
  rawLenRef <- newRef (0 :: e Int64)
  rawCapRef <- newRef (0 :: e Int64)

  runDrain
    ( drainForEach $ \key -> do
        curLen <- readRef rawLenRef
        curCap <- readRef rawCapRef
        whenM_ (curLen `eq` curCap) $ do
          let newCap = ifThenElse (curCap `eq` 0) 16 (curCap * 2)
          oldP <- readRef rawPtrRef
          newP <- reallocTypedArray @Word64 alloc oldP curCap newCap
          writeRef rawPtrRef newP
          writeRef rawCapRef newCap
        p <- readRef rawPtrRef
        h0 <- letM (hashElemWithSalt @e @k defaultHashSalt key)
        pokeElemOff p h0 curLen
        writeRef rawLenRef (curLen + 1)
    )
    s

  rawLen <- readRef rawLenRef
  rawCap <- readRef rawCapRef
  rawPtr <- readRef rawPtrRef

  hdr <- allocStruct @(CStruct (XorFilterSpine k)) alloc
  let f = projectPtrs hdr

  ifThenElseM_
    (rawLen `le` 0)
    ( do
        poke (getPtrOf (xfsFingerprints f)) nullPtr
        poke (getPtrOf (xfsBlockLength f)) 0
        poke (getPtrOf (xfsSeed f)) defaultHashSalt
        poke (getPtrOf (xfsKeyCount f)) 0
    )
    ( do
        sortBuf (ScalarBuf rawPtr) rawLen
        lastUniqueIdx <-
          runDrain
            ( drainFoldM (0 :: e Int64) $ \wIdx rIdx -> do
                prevH <- peekElemOff rawPtr wIdx
                curH <- peekElemOff rawPtr rIdx
                outRef <- newRef wIdx
                whenM_ (prevH `ne` curH) $ do
                  let wNext = wIdx + 1
                  whenM_ (wNext `ne` rIdx) $
                    pokeElemOff rawPtr curH wNext
                  writeRef outRef wNext
                readRef outRef
            )
            (rangeS 1 rawLen)
        nUnique <- letM (lastUniqueIdx + 1)
        segLen <- letM (max' 32 (((nUnique * 123) `quotient` 300) + 16))
        totalSlots <- letM (segLen * 3)

        t2hash <- allocTypedArray @Word64 alloc totalSlots
        t2count <- allocTypedArray @Int64 alloc totalSlots
        alone <- allocTypedArray @Int64 alloc totalSlots
        stackH <- allocTypedArray @Word64 alloc nUnique
        stackSlot <- allocTypedArray @Int64 alloc nUnique

        doneRef <- newRef false
        chosenSeedRef <- newRef defaultHashSalt
        attemptRef <- newRef (0 :: e Word64)

        runDrain drain_ $
          unfoldWhileM
            ()
            (\_ -> lnot <$> readRef doneRef)
            $ \_ -> do
              att <- readRef attemptRef
              writeRef attemptRef (att + 1)
              seed <- letM (hash64WithSalt defaultHashSalt att)

              cmdMemset (toVoidPtr t2hash) 0 (totalSlots `lshift` 3)
              cmdMemset (toVoidPtr t2count) 0 (totalSlots `lshift` 3)

              runDrain
                ( drainForEach $ \idx -> do
                    rawH <- peekElemOff rawPtr idx
                    h <- letM (hash64WithSalt seed rawH)
                    (i0, i1, i2, _) <- xorPositionsAndFp h segLen
                    let addIncident slot = do
                          oh <- peekElemOff t2hash slot
                          oc <- peekElemOff t2count slot
                          pokeElemOff t2hash (oh `ieor` h) slot
                          pokeElemOff t2count (oc + 1) slot
                    addIncident i0
                    addIncident i1
                    addIncident i2
                )
                (rangeS 0 nUnique)

              qHeadRef <- newRef (0 :: e Int64)
              qTailRef <- newRef (0 :: e Int64)
              stackSizeRef <- newRef (0 :: e Int64)

              runDrain
                ( drainForEach $ \v -> do
                    cnt <- peekElemOff t2count v
                    whenM_ (cnt `eq` 1) $ do
                      qt <- readRef qTailRef
                      pokeElemOff alone v qt
                      writeRef qTailRef (qt + 1)
                )
                (rangeS 0 totalSlots)

              runDrain drain_ $
                unfoldWhileM
                  ()
                  ( \_ -> do
                      qh <- readRef qHeadRef
                      qt <- readRef qTailRef
                      pure (qh `lt` qt)
                  )
                  $ \_ -> do
                    qh <- readRef qHeadRef
                    writeRef qHeadRef (qh + 1)
                    v <- peekElemOff alone qh
                    cntV <- peekElemOff t2count v
                    whenM_ (cntV `eq` 1) $ do
                      h <- peekElemOff t2hash v
                      sp <- readRef stackSizeRef
                      pokeElemOff stackH h sp
                      pokeElemOff stackSlot v sp
                      writeRef stackSizeRef (sp + 1)
                      (i0, i1, i2, _) <- xorPositionsAndFp h segLen
                      let removeIncident slot = do
                            oh <- peekElemOff t2hash slot
                            oc <- peekElemOff t2count slot
                            nc <- letM (oc - 1)
                            pokeElemOff t2hash (oh `ieor` h) slot
                            pokeElemOff t2count nc slot
                            whenM_ (nc `eq` 1) $ do
                              qt <- readRef qTailRef
                              pokeElemOff alone slot qt
                              writeRef qTailRef (qt + 1)
                      removeIncident i0
                      removeIncident i1
                      removeIncident i2
                    pure ((), ())

              peeled <- readRef stackSizeRef
              whenM_ (peeled `eq` nUnique) $ do
                writeRef chosenSeedRef seed
                writeRef doneRef true
              pure ((), ())

        fpsVP <- allocZeroedAligned alloc totalSlots (fromIntegral janusSIMDAlignment)
        let fps = fromVoidPtr fpsVP :: e (Ptr Word8)

        runDrain
          ( drainForEach $ \sp -> do
              h <- peekElemOff stackH sp
              pivot <- peekElemOff stackSlot sp
              (i0, i1, i2, fp) <- xorPositionsAndFp h segLen
              b0 <- peekElemOff fps i0
              b1 <- peekElemOff fps i1
              b2 <- peekElemOff fps i2
              pokeElemOff fps (fp `ieor` b0 `ieor` b1 `ieor` b2) pivot
          )
          (reverseRangeS 0 nUnique)

        freeTypedArray @Int64 alloc stackSlot nUnique
        freeTypedArray @Word64 alloc stackH nUnique
        freeTypedArray @Int64 alloc alone totalSlots
        freeTypedArray @Int64 alloc t2count totalSlots
        freeTypedArray @Word64 alloc t2hash totalSlots
        freeTypedArray @Word64 alloc rawPtr rawCap

        chosenSeed <- readRef chosenSeedRef
        poke (getPtrOf (xfsFingerprints f)) fps
        poke (getPtrOf (xfsBlockLength f)) segLen
        poke (getPtrOf (xfsSeed f)) chosenSeed
        poke (getPtrOf (xfsKeyCount f)) nUnique
    )

  pure (XorFilter hdr)

fromStream ::
  forall k m e.
  (XorFilterCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (XorFilter e k)
fromStream alloc = runDrain (drainToXorFilter @k alloc)

--------------------------------------------------------------------------------
-- Lifecycle & 3-Load Membership Probe
--------------------------------------------------------------------------------

free ::
  (XorFilterCtx m e k) =>
  Alloc e ->
  XorFilter e k ->
  m ()
free alloc (XorFilter hdr) = do
  let f = projectPtrs hdr
  segLen <- peek (getPtrOf (xfsBlockLength f))
  whenM_ (segLen `gt` 0) $ do
    fps <- peek (getPtrOf (xfsFingerprints f))
    freeBytes alloc (toVoidPtr fps) (segLen * 3)
  freeStruct alloc hdr

-- | @O(1)@ branchless membership probe issuing 3 independent byte loads (@b0 ^ b1 ^ b2 == fp@).
-- Guaranteed 0 false negatives (~0.39% false positive rate).
mightContain ::
  forall k m e.
  (XorFilterCtx m e k) =>
  XorFilter e k ->
  Elem e k ->
  m (e Bool)
mightContain (XorFilter hdr) key = do
  let f = projectPtrs hdr
  segLen <- peek (getPtrOf (xfsBlockLength f))
  resRef <- newRef false
  whenM_ (segLen `gt` 0) $ do
    fps <- peek (getPtrOf (xfsFingerprints f))
    seed <- peek (getPtrOf (xfsSeed f))
    rawH <- letM (hashElemWithSalt @e @k defaultHashSalt key)
    h <- letM (hash64WithSalt seed rawH)
    (i0, i1, i2, fp) <- xorPositionsAndFp h segLen
    b0 <- peekElemOff fps i0
    b1 <- peekElemOff fps i1
    b2 <- peekElemOff fps i2
    writeRef resRef (((b0 `ieor` b1) `ieor` b2) `eq` fp)
  readRef resRef

keyCount :: (XorFilterCtx m e k) => XorFilter e k -> m (e Int64)
keyCount (XorFilter hdr) = peek (getPtrOf (xfsKeyCount (projectPtrs hdr)))

byteSize :: (XorFilterCtx m e k) => XorFilter e k -> m (e Int64)
byteSize (XorFilter hdr) = (* 3) <$> peek (getPtrOf (xfsBlockLength (projectPtrs hdr)))

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type XorFilterABI k = Typeable k

type instance JanusCEval (XorFilter JanusC k -> r) = XorFilter Identity k -> JanusCEval r

type instance JanusCRetType (XorFilter JanusC k -> r) = JanusCRetType r

instance (XorFilterABI k, JanusCParam r) => JanusCParam (XorFilter JanusC k -> r) where
  jcparam = mkPtrHandleJCParam XorFilter

instance (XorFilterABI k, JanusCEvaluate r) => JanusCEvaluate (XorFilter JanusC k -> r) where
  jceval = mkPtrHandleJCEval unXorFilter XorFilter

type instance JanusCEval (JanusCM (XorFilter JanusC k)) = IO (XorFilter Identity k)

type instance JanusCRetType (JanusCM (XorFilter JanusC k)) = XorFilter Identity k

instance (XorFilterABI k) => JanusCParam (JanusCM (XorFilter JanusC k)) where
  jcparam = mkPtrHandleJCRetParam unXorFilter

instance (XorFilterABI k) => JanusCEvaluate (JanusCM (XorFilter JanusC k)) where
  jceval = mkPtrHandleJCRetEval XorFilter

type instance LinuxX86_64CEval (XorFilter LinuxX86_64C k -> r) = XorFilter Identity k -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (XorFilter LinuxX86_64C k -> r) = LinuxX86_64BaseRet r

instance (XorFilterABI k, LinuxX86_64Param r) => LinuxX86_64Param (XorFilter LinuxX86_64C k -> r) where
  x86param = mkPtrHandleX86Param XorFilter

instance (XorFilterABI k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (XorFilter LinuxX86_64C k -> r) where
  x86eval = mkPtrHandleX86EvalParam unXorFilter XorFilter

instance (XorFilterABI k, X86DeferCont r) => X86DeferCall (XorFilter LinuxX86_64C k -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unXorFilter XorFilter

instance (XorFilterABI k, X86DeferCont r) => X86DeferCont (XorFilter LinuxX86_64C k -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unXorFilter XorFilter

type instance LinuxX86_64CEval (LinuxX86_64CM (XorFilter LinuxX86_64C k)) = IO (XorFilter Identity k)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (XorFilter LinuxX86_64C k)) = XorFilter Identity k

instance (XorFilterABI k) => LinuxX86_64Param (LinuxX86_64CM (XorFilter LinuxX86_64C k)) where
  x86param = mkPtrHandleX86RetParam unXorFilter

instance (XorFilterABI k) => LinuxX86_64Evaluate (LinuxX86_64CM (XorFilter LinuxX86_64C k)) where
  x86eval = mkPtrHandleX86RetEval XorFilter

instance (XorFilterABI k) => X86DeferCall (LinuxX86_64CM (XorFilter LinuxX86_64C k)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg XorFilter

instance (XorFilterABI k) => X86DeferCont (LinuxX86_64CM (XorFilter LinuxX86_64C k)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont XorFilter

instance (XorFilterABI k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (XorFilter LinuxX86_64C k -> r) where
  type JanusCTHEval (XorFilter LinuxX86_64C k -> r) = XorFilter Identity k -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(XorFilter Identity k)) : ps, retSpec)

instance (XorFilterABI k) => JanusCTH (LinuxX86_64CM (XorFilter LinuxX86_64C k)) where
  type JanusCTHEval (LinuxX86_64CM (XorFilter LinuxX86_64C k)) = IO (XorFilter Identity k)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(XorFilter Identity k)) (Proxy @(CStruct (XorFilterSpine k))))
