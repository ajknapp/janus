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

module Janus.Data.SmallVec
  ( -- * Small Inline-First Vector Spine & Handle
    SmallVecSpine (..),
    SmallVec (..),
    SmallVecCtx,

    -- * Lifecycle (Inline Zero-Heap-Buffer Fast Path + Explicit Alloc Spill)
    new,
    free,
    withSmallVec,

    -- * Queries
    len,
    capacity,
    isEmpty,
    isSpilled,
    buf,

    -- * Capacity & Mutation
    clear,
    reserve,
    push,
    pop,
    withPop,
    peekBack,
    withPeekBack,
    readVec,
    writeVec,
    modifyVec,
    swapRemove,
    insertAt,
    removeAt,

    -- * Sorting
    sortBy,
    sort,

    -- * Stream Producers & Drain Consumers
    stream,
    reverseStream,
    indexedStream,
    drainToSmallVec,
    fromStream,
    drainAppendSmallVec,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Alloc.Policy (Alloc, allocStruct, freeStruct)
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpSized (..), allocaAligned)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFind, drainForEach, runDrain)
import Janus.Control.Stream (Stream, indexedS, iterateS, rangeS, reverseRangeS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (sortBuf, sortBufBy)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

-- | Spine struct embedding @n@ inline elements directly inside the struct.
-- While @svsLen <= n@ (and no larger 'reserve' has been requested), @svsIsHeap == 0@
-- and @svsBuf@ points directly into @svsInline@ with zero element-buffer heap allocations.
data SmallVecSpine (n :: Nat) a f = SmallVecSpine
  { svsBuf :: !(f (CStruct (Buf a))),
    svsLen :: !(f Int64),
    svsCap :: !(f Int64),
    svsIsHeap :: !(f Int64),
    svsInline :: !(f (CStruct (InlineBuf n a)))
  }
  deriving stock (Generic)

instance
  ( KnownNat n,
    Typeable n,
    Typeable a,
    JanusStruct (Buf a),
    JanusStruct (InlineBuf n a)
  ) =>
  JanusStruct (SmallVecSpine n a)

newtype SmallVec (n :: Nat) (e :: K.Type -> K.Type) a = SmallVec
  { unSmallVec :: e (Ptr (CStruct (SmallVecSpine n a)))
  }
  deriving stock (Generic)

deriving stock instance Eq (SmallVec n Identity a)

deriving stock instance Show (SmallVec n Identity a)

type SmallVecCtx n m e a =
  ( InlineLayoutCtx n m e a,
    KnownNat n,
    Typeable n,
    Typeable a,
    ExpSized e (CStruct (SmallVecSpine n a)),
    JanusTyped e (CStruct (SmallVecSpine n a))
  )

initSmallVecSpine ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  e (Ptr (CStruct (SmallVecSpine n a))) ->
  m (SmallVec n e a)
initSmallVecSpine hdr = do
  let f = projectPtrs hdr
      inlineCap = fromIntegral (natVal (Proxy @n)) :: e Int64
  bInline <- inlineAsBuf (Proxy @n) (getPtrOf (svsInline f))
  pokeBufStruct (getPtrOf (svsBuf f)) bInline
  poke (getPtrOf (svsLen f)) 0
  poke (getPtrOf (svsCap f)) inlineCap
  poke (getPtrOf (svsIsHeap f)) 0
  pure (SmallVec hdr)

-- | Allocate the 'SmallVec' spine via @alloc@. No separate element buffer is allocated
-- while @len <= n@.
new ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  m (SmallVec n e a)
new alloc = do
  hdr <- allocStruct @(CStruct (SmallVecSpine n a)) alloc
  initSmallVecSpine hdr

-- | Free any spilled heap element buffer and free the 'SmallVec' spine via @alloc@.
free ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  SmallVec n e a ->
  m ()
free alloc (SmallVec hdr) = do
  let f = projectPtrs hdr
  isHeap <- peek (getPtrOf (svsIsHeap f))
  whenM_ (isHeap `ne` 0) $ do
    curCap <- peek (getPtrOf (svsCap f))
    b <- peekBufStruct (getPtrOf (svsBuf f))
    freeBuf alloc b curCap
  freeStruct alloc hdr

-- | Stack-scoped 'SmallVec' in 'IO': allocates the spine on a 64B-aligned C stack frame
-- (zero allocator invocations unless @len@ exceeds @n@) and automatically frees any
-- spilled buffer upon exit.
withSmallVec ::
  forall n a r.
  (SmallVecCtx n IO Identity a) =>
  Alloc Identity ->
  (SmallVec n Identity a -> IO r) ->
  IO r
withSmallVec alloc body =
  allocaAligned 1 $ \(rawHdr :: Ptr (CStruct (SmallVecSpine n a))) -> do
    sv <- initSmallVecSpine (Identity rawHdr)
    res <- body sv
    let f = projectPtrs (Identity rawHdr)
    Identity isHeap <- peek (getPtrOf (svsIsHeap f))
    if isHeap /= 0
      then do
        curCap <- peek (getPtrOf (svsCap f))
        b <- peekBufStruct (getPtrOf (svsBuf f))
        freeBuf alloc b curCap
      else pure ()
    pure res

len :: (SmallVecCtx n m e a) => SmallVec n e a -> m (e Int64)
len (SmallVec hdr) = peek (getPtrOf (svsLen (projectPtrs hdr)))

capacity :: (SmallVecCtx n m e a) => SmallVec n e a -> m (e Int64)
capacity (SmallVec hdr) = peek (getPtrOf (svsCap (projectPtrs hdr)))

isEmpty :: (SmallVecCtx n m e a) => SmallVec n e a -> m (e Bool)
isEmpty sv = (`eq` 0) <$> len sv

isSpilled :: (SmallVecCtx n m e a) => SmallVec n e a -> m (e Bool)
isSpilled (SmallVec hdr) = (`ne` 0) <$> peek (getPtrOf (svsIsHeap (projectPtrs hdr)))

buf :: (SmallVecCtx n m e a) => SmallVec n e a -> m (Buf a e)
buf (SmallVec hdr) = peekBufStruct (getPtrOf (svsBuf (projectPtrs hdr)))

clear :: (SmallVecCtx n m e a) => SmallVec n e a -> m ()
clear (SmallVec hdr) = poke (getPtrOf (svsLen (projectPtrs hdr))) 0

-- | Ensure capacity for at least @curLen + additional@ elements, spilling from the inline
-- buffer to @alloc@ only when @needed > curCap@.
reserve ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  SmallVec n e a ->
  e Int64 ->
  m ()
reserve alloc (SmallVec hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  curCap <- peek (getPtrOf (svsCap f))
  needed <- letM (curLen + additional)
  whenM_ (needed `gt` curCap) $ do
    let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
    (_, newCap) <- runDrain (drainFind c0 (`ge` needed)) (iterateS c0 (* 2))
    oldBuf <- peekBufStruct (getPtrOf (svsBuf f))
    wasHeap <- peek (getPtrOf (svsIsHeap f))
    ifThenElseM_
      (wasHeap `eq` 0)
      ( do
          newBuf <- allocBuf @a alloc newCap
          whenM_ (curLen `gt` 0) $
            copyBuf newBuf 0 oldBuf 0 curLen
          pokeBufStruct (getPtrOf (svsBuf f)) newBuf
          poke (getPtrOf (svsCap f)) newCap
          poke (getPtrOf (svsIsHeap f)) 1
      )
      ( do
          newBuf <- reallocBuf alloc oldBuf curCap newCap curLen
          pokeBufStruct (getPtrOf (svsBuf f)) newBuf
          poke (getPtrOf (svsCap f)) newCap
      )

push ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  SmallVec n e a ->
  Elem e a ->
  m ()
push alloc sv@(SmallVec hdr) val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  curCap <- peek (getPtrOf (svsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc sv 1
  b <- peekBufStruct (getPtrOf (svsBuf f))
  writeBuf b curLen val
  poke (getPtrOf (svsLen f)) (curLen + 1)

pop ::
  forall n a s m e.
  (SmallVecCtx n m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  SmallVec n e a ->
  m (e Bool, e s)
pop defVal (SmallVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    let newLen = curLen - 1
    b <- peekBufStruct (getPtrOf (svsBuf f))
    v <- readBuf b newLen
    poke (getPtrOf (svsLen f)) newLen
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPop ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPop (SmallVec hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        let newLen = curLen - 1
        b <- peekBufStruct (getPtrOf (svsBuf f))
        v <- readBuf b newLen
        poke (getPtrOf (svsLen f)) newLen
        onElem v
    )

peekBack ::
  forall n a s m e.
  (SmallVecCtx n m e a, Elem e a ~ e s, JanusTyped e s) =>
  e s ->
  SmallVec n e a ->
  m (e Bool, e s)
peekBack defVal (SmallVec hdr) = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (curLen `gt` 0) $ do
    b <- peekBufStruct (getPtrOf (svsBuf f))
    v <- readBuf b (curLen - 1)
    writeRef okRef true
    writeRef valRef v
  (,) <$> readRef okRef <*> readRef valRef

withPeekBack ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  m () ->
  (Elem e a -> m ()) ->
  m ()
withPeekBack (SmallVec hdr) onEmpty onElem = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  ifThenElseM_
    (curLen `le` 0)
    onEmpty
    ( do
        b <- peekBufStruct (getPtrOf (svsBuf f))
        v <- readBuf b (curLen - 1)
        onElem v
    )

readVec :: (SmallVecCtx n m e a) => SmallVec n e a -> e Int64 -> m (Elem e a)
readVec (SmallVec hdr) idx = do
  b <- peekBufStruct (getPtrOf (svsBuf (projectPtrs hdr)))
  readBuf b idx

writeVec :: (SmallVecCtx n m e a) => SmallVec n e a -> e Int64 -> Elem e a -> m ()
writeVec (SmallVec hdr) idx val = do
  b <- peekBufStruct (getPtrOf (svsBuf (projectPtrs hdr)))
  writeBuf b idx val

modifyVec :: (SmallVecCtx n m e a) => SmallVec n e a -> e Int64 -> (Elem e a -> Elem e a) -> m ()
modifyVec sv idx f = do
  cur <- readVec sv idx
  writeVec sv idx (f cur)

swapRemove ::
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  e Int64 ->
  m (Elem e a)
swapRemove (SmallVec hdr) idx = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  b <- peekBufStruct (getPtrOf (svsBuf f))
  removed <- readBuf b idx
  let lastIdx = curLen - 1
  whenM_ (idx `lt` lastIdx) $ do
    lastVal <- readBuf b lastIdx
    writeBuf b idx lastVal
  poke (getPtrOf (svsLen f)) lastIdx
  pure removed

insertAt ::
  (SmallVecCtx n m e a) =>
  Alloc e ->
  SmallVec n e a ->
  e Int64 ->
  Elem e a ->
  m ()
insertAt alloc sv@(SmallVec hdr) idx val = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  curCap <- peek (getPtrOf (svsCap f))
  whenM_ (curLen `eq` curCap) $
    reserve alloc sv 1
  b <- peekBufStruct (getPtrOf (svsBuf f))
  let tailCnt = curLen - idx
  whenM_ (tailCnt `gt` 0) $
    moveBuf b (idx + 1) idx tailCnt
  writeBuf b idx val
  poke (getPtrOf (svsLen f)) (curLen + 1)

removeAt ::
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  e Int64 ->
  m (Elem e a)
removeAt (SmallVec hdr) idx = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (svsLen f))
  b <- peekBufStruct (getPtrOf (svsBuf f))
  removed <- readBuf b idx
  let tailCnt = curLen - idx - 1
  whenM_ (tailCnt `gt` 0) $
    moveBuf b idx (idx + 1) tailCnt
  poke (getPtrOf (svsLen f)) (curLen - 1)
  pure removed

sortBy ::
  (SmallVecCtx n m e a) =>
  (Elem e a -> Elem e a -> e Bool) ->
  SmallVec n e a ->
  m ()
sortBy isLt (SmallVec hdr) = do
  let f = projectPtrs hdr
  cnt <- peek (getPtrOf (svsLen f))
  b <- peekBufStruct (getPtrOf (svsBuf f))
  sortBufBy isLt b cnt

sort ::
  (SmallVecCtx n m e a, Elem e a ~ e s, ExpOrd e s) =>
  SmallVec n e a ->
  m ()
sort (SmallVec hdr) = do
  let f = projectPtrs hdr
  cnt <- peek (getPtrOf (svsLen f))
  b <- peekBufStruct (getPtrOf (svsBuf f))
  sortBuf b cnt

stream ::
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  Stream m (Elem e a)
stream (SmallVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (svsLen f)) <*> peekBufStruct (getPtrOf (svsBuf f)))
        (\(cnt, b) -> streamM (rangeS 0 cnt) (readBuf b))

reverseStream ::
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  Stream m (Elem e a)
reverseStream (SmallVec hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (svsLen f)) <*> peekBufStruct (getPtrOf (svsBuf f)))
        (\(cnt, b) -> streamM (reverseRangeS 0 cnt) (readBuf b))

indexedStream ::
  (SmallVecCtx n m e a) =>
  SmallVec n e a ->
  Stream m (e Int64, Elem e a)
indexedStream sv = indexedS (stream sv)

drainToSmallVec ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  Drain m (Elem e a) (SmallVec n e a)
drainToSmallVec alloc = Drain $ \s -> do
  sv <- new @n alloc
  runDrain (drainAppendSmallVec alloc sv) s
  pure sv

fromStream ::
  forall n a m e.
  (SmallVecCtx n m e a) =>
  Alloc e ->
  Stream m (Elem e a) ->
  m (SmallVec n e a)
fromStream alloc = runDrain (drainToSmallVec @n alloc)

drainAppendSmallVec ::
  (SmallVecCtx n m e a) =>
  Alloc e ->
  SmallVec n e a ->
  Drain m (Elem e a) ()
drainAppendSmallVec alloc sv = drainForEach (push alloc sv)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type SmallVecABI n a =
  ( KnownNat n,
    Typeable n,
    Typeable a,
    JanusStruct (Buf a),
    JanusStruct (InlineBuf n a)
  )

type instance JanusCEval (SmallVec n JanusC a -> r) = SmallVec n Identity a -> JanusCEval r

type instance JanusCRetType (SmallVec n JanusC a -> r) = JanusCRetType r

instance (SmallVecABI n a, JanusCParam r) => JanusCParam (SmallVec n JanusC a -> r) where
  jcparam = mkPtrHandleJCParam SmallVec

instance (SmallVecABI n a, JanusCEvaluate r) => JanusCEvaluate (SmallVec n JanusC a -> r) where
  jceval = mkPtrHandleJCEval unSmallVec SmallVec

type instance JanusCEval (JanusCM (SmallVec n JanusC a)) = IO (SmallVec n Identity a)

type instance JanusCRetType (JanusCM (SmallVec n JanusC a)) = SmallVec n Identity a

instance (SmallVecABI n a) => JanusCParam (JanusCM (SmallVec n JanusC a)) where
  jcparam = mkPtrHandleJCRetParam unSmallVec

instance (SmallVecABI n a) => JanusCEvaluate (JanusCM (SmallVec n JanusC a)) where
  jceval = mkPtrHandleJCRetEval SmallVec

type instance LinuxX86_64CEval (SmallVec n LinuxX86_64C a -> r) = SmallVec n Identity a -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (SmallVec n LinuxX86_64C a -> r) = LinuxX86_64BaseRet r

instance (SmallVecABI n a, LinuxX86_64Param r) => LinuxX86_64Param (SmallVec n LinuxX86_64C a -> r) where
  x86param = mkPtrHandleX86Param SmallVec

instance (SmallVecABI n a, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (SmallVec n LinuxX86_64C a -> r) where
  x86eval = mkPtrHandleX86EvalParam unSmallVec SmallVec

instance (SmallVecABI n a, X86DeferCont r) => X86DeferCall (SmallVec n LinuxX86_64C a -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unSmallVec SmallVec

instance (SmallVecABI n a, X86DeferCont r) => X86DeferCont (SmallVec n LinuxX86_64C a -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unSmallVec SmallVec

type instance LinuxX86_64CEval (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) = IO (SmallVec n Identity a)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) = SmallVec n Identity a

instance (SmallVecABI n a) => LinuxX86_64Param (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) where
  x86param = mkPtrHandleX86RetParam unSmallVec

instance (SmallVecABI n a) => LinuxX86_64Evaluate (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) where
  x86eval = mkPtrHandleX86RetEval SmallVec

instance (SmallVecABI n a) => X86DeferCall (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg SmallVec

instance (SmallVecABI n a) => X86DeferCont (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont SmallVec

instance (SmallVecABI n a, LinuxX86_64Param r, JanusCTH r) => JanusCTH (SmallVec n LinuxX86_64C a -> r) where
  type JanusCTHEval (SmallVec n LinuxX86_64C a -> r) = SmallVec n Identity a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(SmallVec n Identity a)) : ps, retSpec)

instance (SmallVecABI n a) => JanusCTH (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (SmallVec n LinuxX86_64C a)) = IO (SmallVec n Identity a)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(SmallVec n Identity a)) (Proxy @(CStruct (SmallVecSpine n a))))
