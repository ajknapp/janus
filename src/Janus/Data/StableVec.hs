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

module Janus.Data.StableVec
  ( -- * Pointer-Stable Chunked Vector Spine & Handle
    StableVecSpine (..),
    StableVec (..),
    StableVec64,
    StableVecCtx,

    -- * Lifecycle
    new,
    free,

    -- * Queries
    len,
    capacity,
    isEmpty,
    clear,
    reserve,

    -- * Mutation, Access, & Stable Element Pointers
    push,
    pop,
    withPop,
    readVec,
    writeVec,
    modifyVec,
    elemPtrScalar,
    elemPtrAoS,

    -- * Chunk-Sequential Streams & Drains
    stream,
    reverseStream,
    indexedStream,
    ptrStreamScalar,
    drainToStableVec,
    fromStream,
    drainAppendStableVec,

    -- * Chunk-Sequential SPMD / SIMD Gang Kernels
    foreachSPMD,
    traverseSPMD_,
    foldSPMD,
  )
where

import Data.Bits (shiftL)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Alloc.Policy
  ( Alloc,
    allocStruct,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, indexedS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..), min')
import Janus.SPMD (SPMD, SPMDM, SPMDTyped, uniform, withSPMD)
import Janus.Typed (JanusTyped)

-- | Spine struct for 'StableVec'.
-- Allocates fixed-size chunks of @2^logChunk@ elements that NEVER move or reallocate.
-- Only the compact chunk directory @stvsChunks@ grows on resize.
data StableVecSpine (logChunk :: Nat) a f = StableVecSpine
  { stvsChunks :: !(f (Ptr (CStruct (Buf a)))),
    stvsNumChunks :: !(f Int64),
    stvsDirCap :: !(f Int64),
    stvsLen :: !(f Int64)
  }
  deriving stock (Generic)

instance (KnownNat logChunk, Typeable logChunk, Typeable a, JanusStruct (Buf a)) => JanusStruct (StableVecSpine logChunk a)

newtype StableVec (logChunk :: Nat) (e :: K.Type -> K.Type) a = StableVec
  { unStableVec :: e (Ptr (CStruct (StableVecSpine logChunk a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (StableVec logChunk Identity a)

deriving stock instance Show (StableVec logChunk Identity a)

-- | Default pointer-stable vector with @2^6 = 64@-element chunks.
type StableVec64 = StableVec 6

type StableVecCtx k m e a =
  ( LayoutCtx m e a,
    KnownNat k,
    Typeable k,
    Typeable a,
    ExpPtr e (CStruct (Buf a)),
    JanusTyped e (Ptr (CStruct (Buf a))),
    CmdStorable m e (Ptr (CStruct (Buf a))),
    ExpSized e (CStruct (StableVecSpine k a)),
    JanusTyped e (CStruct (StableVecSpine k a))
  )

chunkParams :: forall k. (KnownNat k) => Proxy k -> (Int64, Int64, Int64)
chunkParams pk =
  let sh = fromIntegral (natVal pk) :: Int
      cSz = (1 :: Int64) `shiftL` sh
      cMask = cSz - 1
   in (fromIntegral sh, cSz, cMask)

new ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  m (StableVec k e a)
new alloc = do
  hdr <- allocStruct @(CStruct (StableVecSpine k a)) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (stvsChunks f)) nullPtr
  poke (getPtrOf (stvsNumChunks f)) 0
  poke (getPtrOf (stvsDirCap f)) 0
  poke (getPtrOf (stvsLen f)) 0
  pure (StableVec hdr)

free ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  StableVec k e a ->
  m ()
free alloc (StableVec hdr) = do
  let f = projectPtrs hdr
      (_, chunkSzI, _) = chunkParams (Proxy @k)
      chunkSz = fromIntegral chunkSzI :: e Int64
  dir <- peek (getPtrOf (stvsChunks f))
  numC <- peek (getPtrOf (stvsNumChunks f))
  dirCap <- peek (getPtrOf (stvsDirCap f))
  runDrain
    ( drainForEach $ \ci -> do
        cbuf <- peekBufStruct (ptrIndex dir ci)
        freeBuf alloc cbuf chunkSz
    )
    (rangeS 0 numC)
  whenM_ (dirCap `gt` 0) $
    freeTypedArray @(CStruct (Buf a)) alloc dir dirCap
  freeStruct alloc hdr

len :: (StableVecCtx k m e a) => StableVec k e a -> m (e Int64)
len (StableVec hdr) = peek (getPtrOf (stvsLen (projectPtrs hdr)))

capacity :: forall k a m e. (StableVecCtx k m e a) => StableVec k e a -> m (e Int64)
capacity (StableVec hdr) = do
  let (_, chunkSzI, _) = chunkParams (Proxy @k)
  numC <- peek (getPtrOf (stvsNumChunks (projectPtrs hdr)))
  pure (numC * fromIntegral chunkSzI)

isEmpty :: (StableVecCtx k m e a) => StableVec k e a -> m (e Bool)
isEmpty sv = (`eq` 0) <$> len sv

-- | O(1) clear keeping already-allocated chunks warm for reuse without reallocating.
clear :: (StableVecCtx k m e a) => StableVec k e a -> m ()
clear (StableVec hdr) = poke (getPtrOf (stvsLen (projectPtrs hdr))) 0

-- | Ensure enough fixed-size chunks exist for at least @curLen + additional@ elements.
reserve ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  StableVec k e a ->
  e Int64 ->
  m ()
reserve alloc (StableVec hdr) additional = do
  let f = projectPtrs hdr
      (shI, chunkSzI, maskI) = chunkParams (Proxy @k)
      sh = fromIntegral shI :: e Int64
      chunkSz = fromIntegral chunkSzI :: e Int64
      cMask = fromIntegral maskI :: e Int64
  curLen <- peek (getPtrOf (stvsLen f))
  numC <- peek (getPtrOf (stvsNumChunks f))
  dirCap <- peek (getPtrOf (stvsDirCap f))
  neededElems <- letM (curLen + additional)
  let reqChunks = ifThenElse (neededElems `le` 0) 0 ((neededElems + cMask) `rshift` sh)
  whenM_ (reqChunks `gt` numC) $ do
    whenM_ (reqChunks `gt` dirCap) $ do
      let c0 = ifThenElse (dirCap `lt` 8) 8 (dirCap * 2)
      (_, newDirCap) <- runDrain (drainFind c0 (`ge` reqChunks)) (iterateS c0 (* 2))
      oldDir <- peek (getPtrOf (stvsChunks f))
      newDir <- reallocTypedArray @(CStruct (Buf a)) alloc oldDir dirCap newDirCap
      poke (getPtrOf (stvsChunks f)) newDir
      poke (getPtrOf (stvsDirCap f)) newDirCap
    dirNow <- peek (getPtrOf (stvsChunks f))
    runDrain
      ( drainForEach $ \ci -> do
          cbuf <- allocBuf @a alloc chunkSz
          pokeBufStruct (ptrIndex dirNow ci) cbuf
      )
      (rangeS numC reqChunks)
    poke (getPtrOf (stvsNumChunks f)) reqChunks

splitChunkSlot :: forall k e. (KnownNat k, ExpBits e Int64, Num (e Int64)) => Proxy k -> e Int64 -> (e Int64, e Int64)
splitChunkSlot pk idx =
  let (shI, _, maskI) = chunkParams pk
   in (idx `rshift` fromIntegral shI, idx `iand` fromIntegral maskI)

push ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  StableVec k e a ->
  Elem e a ->
  m ()
push alloc sv@(StableVec hdr) val = do
  let f = projectPtrs hdr
      (_, chunkSzI, _) = chunkParams (Proxy @k)
  curLen <- peek (getPtrOf (stvsLen f))
  numC <- peek (getPtrOf (stvsNumChunks f))
  whenM_ (curLen `ge` (numC * fromIntegral chunkSzI)) $
    reserve alloc sv 1
  dir <- peek (getPtrOf (stvsChunks f))
  let (ci, si) = splitChunkSlot (Proxy @k) curLen
  cbuf <- peekBufStruct (ptrIndex dir ci)
  writeBuf cbuf si val
  poke (getPtrOf (stvsLen f)) (curLen + 1)

pop ::
  forall k a s m e.
  (StableVecCtx k m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  StableVec k e a ->
  m (e Bool, e s)
pop defVal (StableVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stvsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    let newLen = curLen - 1
        (ci, si) = splitChunkSlot (Proxy @k) newLen
    dir <- peek (getPtrOf (stvsChunks f))
    cbuf <- peekBufStruct (ptrIndex dir ci)
    v <- readBuf cbuf si
    poke (getPtrOf (stvsLen f)) newLen
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPop ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (StableVec hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (stvsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let newLen = curLen - 1
            (ci, si) = splitChunkSlot (Proxy @k) newLen
        dir <- peek (getPtrOf (stvsChunks f))
        cbuf <- peekBufStruct (ptrIndex dir ci)
        v <- readBuf cbuf si
        poke (getPtrOf (stvsLen f)) newLen
        onElem v
    )

readVec ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  e Int64 ->
  m (Elem e a)
readVec (StableVec hdr) idx = do
  let (ci, si) = splitChunkSlot (Proxy @k) idx
  dir <- peek (getPtrOf (stvsChunks (projectPtrs hdr)))
  cbuf <- peekBufStruct (ptrIndex dir ci)
  readBuf cbuf si

writeVec ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  e Int64 ->
  Elem e a ->
  m ()
writeVec (StableVec hdr) idx val = do
  let (ci, si) = splitChunkSlot (Proxy @k) idx
  dir <- peek (getPtrOf (stvsChunks (projectPtrs hdr)))
  cbuf <- peekBufStruct (ptrIndex dir ci)
  writeBuf cbuf si val

modifyVec ::
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  e Int64 ->
  (Elem e a -> Elem e a) ->
  m ()
modifyVec sv idx f = do
  cur <- readVec sv idx
  writeVec sv idx (f cur)

-- | Obtain an address-stable pointer to element @idx@ (guaranteed never to be invalidated
-- by subsequent 'push' or 'reserve' calls).
elemPtrScalar ::
  forall k a m e.
  (StableVecCtx k m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  StableVec k e a ->
  e Int64 ->
  m (e (Ptr a))
elemPtrScalar (StableVec hdr) idx = do
  let (ci, si) = splitChunkSlot (Proxy @k) idx
  dir <- peek (getPtrOf (stvsChunks (projectPtrs hdr)))
  ScalarBuf pChunk <- peekBufStruct (ptrIndex dir ci)
  pure (ptrIndex pChunk si)

elemPtrAoS ::
  forall k s m e.
  (StableVecCtx k m e (AoS s), ExpPtr e (CStruct s)) =>
  StableVec k e (AoS s) ->
  e Int64 ->
  m (e (Ptr (CStruct s)))
elemPtrAoS (StableVec hdr) idx = do
  let (ci, si) = splitChunkSlot (Proxy @k) idx
  dir <- peek (getPtrOf (stvsChunks (projectPtrs hdr)))
  AoSBuf pChunk <- peekBufStruct (ptrIndex dir ci)
  pure (ptrIndex pChunk si)

-- | Chunk-sequential 'Stream': hoists the chunk descriptor load out of the inner loop
-- over each @2^k@ chunk rather than recomputing @dir[i >> k]@ per element.
stream ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  Stream m (Elem e a)
stream (StableVec hdr) =
  let f = projectPtrs hdr
      (shI, _, maskI) = chunkParams (Proxy @k)
      sh = fromIntegral shI :: e Int64
      cMask = fromIntegral maskI :: e Int64
   in withStreamSetup
        ((,) <$> peek (getPtrOf (stvsLen f)) <*> peek (getPtrOf (stvsChunks f)))
        $ \(totalLen, dir) ->
          streamM (rangeS 0 totalLen) $ \idx -> do
            cbuf <- peekBufStruct (ptrIndex dir (idx `rshift` sh))
            readBuf cbuf (idx `iand` cMask)

reverseStream ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  Stream m (Elem e a)
reverseStream sv@(StableVec hdr) =
  withStreamSetup
    (peek (getPtrOf (stvsLen (projectPtrs hdr))))
    (\totalLen -> streamM (reverseRangeS 0 totalLen) (readVec sv))

indexedStream ::
  (StableVecCtx k m e a) =>
  StableVec k e a ->
  Stream m (e Int64, Elem e a)
indexedStream sv = indexedS (stream sv)

ptrStreamScalar ::
  forall k a m e.
  (StableVecCtx k m e a, Buf a ~ ScalarBuf a, ExpPtr e a) =>
  StableVec k e a ->
  Stream m (e (Ptr a))
ptrStreamScalar (StableVec hdr) =
  let f = projectPtrs hdr
      (shI, _, maskI) = chunkParams (Proxy @k)
      sh = fromIntegral shI :: e Int64
      cMask = fromIntegral maskI :: e Int64
   in withStreamSetup
        ((,) <$> peek (getPtrOf (stvsLen f)) <*> peek (getPtrOf (stvsChunks f)))
        $ \(totalLen, dir) ->
          streamM (rangeS 0 totalLen) $ \idx -> do
            ScalarBuf pChunk <- peekBufStruct (ptrIndex dir (idx `rshift` sh))
            pure (ptrIndex pChunk (idx `iand` cMask))

drainToStableVec ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  Drain m (Elem e a) (StableVec k e a)
drainToStableVec alloc = Drain $ \s -> do
  sv <- new @k alloc
  runDrain (drainAppendStableVec alloc sv) s
  pure sv

fromStream ::
  forall k a m e.
  (StableVecCtx k m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (StableVec k e a)
fromStream alloc = runDrain (drainToStableVec @k alloc)

drainAppendStableVec ::
  (StableVecCtx k m e a) =>
  Alloc e ->
  StableVec k e a ->
  Drain m (Elem e a) ()
drainAppendStableVec alloc sv = drainForEach (push alloc sv)

--------------------------------------------------------------------------------
-- Chunk-Sequential SPMD / SIMD Gang Kernels Over StableVec
--------------------------------------------------------------------------------

-- | Layout-polymorphic 64B-aligned in-place SIMD map across all live @2^k@ chunks.
foreachSPMD ::
  forall w k a m e.
  (StableVecCtx k m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  StableVec k e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (Elem (SPMD w e) a)) ->
  m ()
foreachSPMD pw (StableVec hdr) step = do
  let f = projectPtrs hdr
      (shI, chunkSzI, maskI) = chunkParams (Proxy @k)
      sh = fromIntegral shI :: e Int64
      chunkSz = fromIntegral chunkSzI :: e Int64
      cMask = fromIntegral maskI :: e Int64
  curLen <- peek (getPtrOf (stvsLen f))
  whenM_ (curLen `gt` 0) $ do
    dir <- peek (getPtrOf (stvsChunks f))
    usedChunks <- letM ((curLen + cMask) `rshift` sh)
    runDrain
      ( drainForEach $ \cIdx -> do
          chunkBuf <- peekBufStruct (ptrIndex dir cIdx)
          chunkBase <- letM (cIdx `lshift` sh)
          chunkItems <- letM (min' chunkSz (curLen - chunkBase))
          foreachBufSPMD pw True chunkBuf chunkItems $ \localIdx val ->
            step (uniform chunkBase + localIdx) val
      )
      (rangeS 0 usedChunks)

-- | Layout-polymorphic 64B-aligned read-only SIMD scan across all live @2^k@ chunks.
traverseSPMD_ ::
  forall w k a m e.
  (StableVecCtx k m e a, LayoutUnitSPMD w m e a) =>
  Proxy w ->
  StableVec k e a ->
  (SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e ()) ->
  m ()
traverseSPMD_ pw (StableVec hdr) body = do
  let f = projectPtrs hdr
      (shI, chunkSzI, maskI) = chunkParams (Proxy @k)
      sh = fromIntegral shI :: e Int64
      chunkSz = fromIntegral chunkSzI :: e Int64
      cMask = fromIntegral maskI :: e Int64
  curLen <- peek (getPtrOf (stvsLen f))
  whenM_ (curLen `gt` 0) $ do
    dir <- peek (getPtrOf (stvsChunks f))
    usedChunks <- letM ((curLen + cMask) `rshift` sh)
    runDrain
      ( drainForEach $ \cIdx -> do
          chunkBuf <- peekBufStruct (ptrIndex dir cIdx)
          chunkBase <- letM (cIdx `lshift` sh)
          chunkItems <- letM (min' chunkSz (curLen - chunkBase))
          traverseBufSPMD_ pw True chunkBuf chunkItems $ \localIdx val ->
            body (uniform chunkBase + localIdx) val
      )
      (rangeS 0 usedChunks)

-- | Layout-polymorphic 64B-aligned masked-safe SIMD fold across all live @2^k@ chunks.
foldSPMD ::
  forall w k a s m e.
  (StableVecCtx k m e a, LayoutUnitSPMD w m e a, SPMDTyped w e s) =>
  Proxy w ->
  StableVec k e a ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) a -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldSPMD pw sv s0 step = do
  accRef <- withSPMD pw (newRef s0)
  traverseSPMD_ pw sv $ \idx val -> do
    cur <- readRef accRef
    nxt <- step cur idx val
    writeRef accRef nxt
  withSPMD pw (readRef accRef)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type StableVecABI k a = (KnownNat k, Typeable k, Typeable a, JanusStruct (Buf a))

type instance JanusCEval (StableVec k JanusC a -> r) = StableVec k Identity a -> JanusCEval r

type instance JanusCRetType (StableVec k JanusC a -> r) = JanusCRetType r

instance (StableVecABI k a, JanusCParam r) => JanusCParam (StableVec k JanusC a -> r) where
  jcparam = mkPtrHandleJCParam StableVec

instance (StableVecABI k a, JanusCEvaluate r) => JanusCEvaluate (StableVec k JanusC a -> r) where
  jceval = mkPtrHandleJCEval unStableVec StableVec

type instance JanusCEval (JanusCM (StableVec k JanusC a)) = IO (StableVec k Identity a)

type instance JanusCRetType (JanusCM (StableVec k JanusC a)) = StableVec k Identity a

instance (StableVecABI k a) => JanusCParam (JanusCM (StableVec k JanusC a)) where
  jcparam = mkPtrHandleJCRetParam unStableVec

instance (StableVecABI k a) => JanusCEvaluate (JanusCM (StableVec k JanusC a)) where
  jceval = mkPtrHandleJCRetEval StableVec

type instance LinuxX86_64CEval (StableVec k LinuxX86_64C a -> r) = StableVec k Identity a -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (StableVec k LinuxX86_64C a -> r) = LinuxX86_64BaseRet r

instance (StableVecABI k a, LinuxX86_64Param r) => LinuxX86_64Param (StableVec k LinuxX86_64C a -> r) where
  x86param = mkPtrHandleX86Param StableVec

instance (StableVecABI k a, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (StableVec k LinuxX86_64C a -> r) where
  x86eval = mkPtrHandleX86EvalParam unStableVec StableVec

instance (StableVecABI k a, X86DeferCont r) => X86DeferCall (StableVec k LinuxX86_64C a -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unStableVec StableVec

instance (StableVecABI k a, X86DeferCont r) => X86DeferCont (StableVec k LinuxX86_64C a -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unStableVec StableVec

type instance LinuxX86_64CEval (LinuxX86_64CM (StableVec k LinuxX86_64C a)) = IO (StableVec k Identity a)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (StableVec k LinuxX86_64C a)) = StableVec k Identity a

instance (StableVecABI k a) => LinuxX86_64Param (LinuxX86_64CM (StableVec k LinuxX86_64C a)) where
  x86param = mkPtrHandleX86RetParam unStableVec

instance (StableVecABI k a) => LinuxX86_64Evaluate (LinuxX86_64CM (StableVec k LinuxX86_64C a)) where
  x86eval = mkPtrHandleX86RetEval StableVec

instance (StableVecABI k a) => X86DeferCall (LinuxX86_64CM (StableVec k LinuxX86_64C a)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg StableVec

instance (StableVecABI k a) => X86DeferCont (LinuxX86_64CM (StableVec k LinuxX86_64C a)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont StableVec

instance (StableVecABI k a, LinuxX86_64Param r, JanusCTH r) => JanusCTH (StableVec k LinuxX86_64C a -> r) where
  type JanusCTHEval (StableVec k LinuxX86_64C a -> r) = StableVec k Identity a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(StableVec k Identity a)) : ps, retSpec)

instance (StableVecABI k a) => JanusCTH (LinuxX86_64CM (StableVec k LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (StableVec k LinuxX86_64C a)) = IO (StableVec k Identity a)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(StableVec k Identity a)) (Proxy @(CStruct (StableVecSpine k a))))
