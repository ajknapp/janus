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

module Janus.Data.HashSet
  ( -- * Zero-Value-Buffer SwissTable HashSet Spine & Handle
    HashSetSpine (..),
    HashSet (..),
    HashSetCtx,

    -- * Lifecycle
    new,
    withCapacity,
    free,

    -- * Queries & Capacity Management
    size,
    capacity,
    isEmpty,
    clear,
    reserve,

    -- * Membership, Insertion, Deletion, & Set Relations
    insert,
    member,
    delete,
    isSubsetOf,

    -- * Fused Set-Algebra Streams & Drains
    stream,
    unionInto,
    intersectionS,
    differenceS,
    drainToHashSet,
    fromStream,
    drainAppendHashSet,
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
    allocBytesAligned,
    allocStruct,
    freeBytes,
    freeStruct,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), ExpSized (..), janusSIMDAlignment)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainAll, drainFind, drainFirst, drainForEach, drainLength, runDrain)
import Janus.Control.Stream (Stream (..), filterMapS, iterateS, streamM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.HashMap (HashKey (..), probeGroupsS, swarBitsS, swarOccupiedSlotsS)
import Janus.Data.Layout
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.MurmurHash (defaultHashSalt)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

ctrlEmptyByte :: Word8
ctrlEmptyByte = 0x80

ctrlDeletedByte :: Word8
ctrlDeletedByte = 0xFE

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

swarMatchEmptyOrDeleted :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64
swarMatchEmptyOrDeleted w = w `iand` 0x8080808080808080

swarLowestByteIdx ::
  (ExpBits e Word64, ExpIntegralCast e Word64 Int64, Num (e Word64)) =>
  e Word64 ->
  e Int64
swarLowestByteIdx m =
  let lsb = (m `iand` (0 - m)) `rshift` 7
   in toIntegral ((lsb * 0x0001020304050607) `rshift` 56)

maxLoadForCap :: (ExpBits e Int64, Num (e Int64)) => e Int64 -> e Int64
maxLoadForCap cap = cap - (cap `rshift` 3)

-- | Dedicated key-only SwissTable spine (zero value-array overhead).
data HashSetSpine k f = HashSetSpine
  { hssCtrl :: !(f (Ptr Word8)),
    hssKeys :: !(f (CStruct (Buf k))),
    hssCap :: !(f Int64),
    hssLen :: !(f Int64),
    hssGrowthLeft :: !(f Int64),
    hssSeed :: !(f Word64)
  }
  deriving stock (Generic)

instance (Typeable k, JanusStruct (Buf k)) => JanusStruct (HashSetSpine k)

newtype HashSet (e :: K.Type -> K.Type) k = HashSet
  { unHashSet :: e (Ptr (CStruct (HashSetSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (HashSet Identity k)

deriving stock instance Show (HashSet Identity k)

type HashSetCtx m e k =
  ( LayoutCtx m e k,
    HashKey e k,
    Typeable k,
    ExpSized e (CStruct (HashSetSpine k)),
    JanusTyped e (CStruct (HashSetSpine k))
  )

new ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  m (HashSet e k)
new alloc = do
  hdr <- allocStruct @(CStruct (HashSetSpine k)) alloc
  let f = projectPtrs hdr
  k0 <- allocBuf @k alloc 0
  poke (getPtrOf (hssCtrl f)) nullPtr
  pokeBufStruct (getPtrOf (hssKeys f)) k0
  poke (getPtrOf (hssCap f)) 0
  poke (getPtrOf (hssLen f)) 0
  poke (getPtrOf (hssGrowthLeft f)) 0
  poke (getPtrOf (hssSeed f)) defaultHashSalt
  pure (HashSet hdr)

withCapacity ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  e Int64 ->
  m (HashSet e k)
withCapacity alloc minElems = do
  hs <- new alloc
  whenM_ (minElems `gt` 0) $
    reserve alloc hs minElems
  pure hs

free ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  m ()
free alloc (HashSet hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hssCap f))
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hssCtrl f))
    kb <- peekBufStruct (getPtrOf (hssKeys f))
    freeBytes alloc (toVoidPtr ctrl) (cap + 8)
    freeBuf alloc kb cap
  freeStruct alloc hdr

size :: (HashSetCtx m e k) => HashSet e k -> m (e Int64)
size (HashSet hdr) = peek (getPtrOf (hssLen (projectPtrs hdr)))

capacity :: (HashSetCtx m e k) => HashSet e k -> m (e Int64)
capacity (HashSet hdr) = peek (getPtrOf (hssCap (projectPtrs hdr)))

isEmpty :: (HashSetCtx m e k) => HashSet e k -> m (e Bool)
isEmpty hs = (`eq` 0) <$> size hs

clear ::
  (HashSetCtx m e k) =>
  HashSet e k ->
  m ()
clear (HashSet hdr) = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hssCap f))
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hssCtrl f))
    cmdMemset (toVoidPtr ctrl) (fromIntegral ctrlEmptyByte) (cap + 8)
    poke (getPtrOf (hssGrowthLeft f)) (maxLoadForCap cap)
  poke (getPtrOf (hssLen f)) 0

rehashTo ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  e Int64 ->
  m ()
rehashTo alloc (HashSet hdr) newCap = do
  let f = projectPtrs hdr
  oldCap <- peek (getPtrOf (hssCap f))
  oldCtrl <- peek (getPtrOf (hssCtrl f))
  oldKeys <- peekBufStruct (getPtrOf (hssKeys f))
  seed <- peek (getPtrOf (hssSeed f))

  newCtrlVP <- allocBytesAligned alloc (newCap + 8) (fromIntegral janusSIMDAlignment)
  let newCtrl = fromVoidPtr newCtrlVP :: e (Ptr Word8)
      newCtrlWords = fromVoidPtr newCtrlVP :: e (Ptr Word64)
      newGroupMask = (newCap `rshift` 3) - 1
  cmdMemset newCtrlVP (fromIntegral ctrlEmptyByte) (newCap + 8)
  newKeys <- allocBuf @k alloc newCap
  rehashedCntRef <- newRef (0 :: e Int64)

  whenM_ (oldCap `gt` 0) $ do
    let oldCtrlWords = fromVoidPtr (toVoidPtr oldCtrl) :: e (Ptr Word64)
        oldNumGroups = oldCap `rshift` 3
    rehashed <-
      runDrain drainLength $
        streamM (swarOccupiedSlotsS oldCtrlWords oldNumGroups) $ \oldSlot -> do
          key <- readBuf oldKeys oldSlot
          h <- letM (hashElemWithSalt @e @k seed key)
          let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
              g0 = toIntegral h `iand` newGroupMask
          (_, dstSlot) <-
            runDrain (drainFirst 0) $
              filterMapS
                ( \pg -> do
                    nw <- peekElemOff newCtrlWords pg
                    let emp = swarMatchEmpty nw
                    pure (emp `ne` 0, (pg `lshift` 3) + swarLowestByteIdx emp)
                )
                (probeGroupsS g0 newGroupMask)
          pokeElemOff newCtrl h2 dstSlot
          writeBuf newKeys dstSlot key
    writeRef rehashedCntRef rehashed
    freeBytes alloc (toVoidPtr oldCtrl) (oldCap + 8)
    freeBuf alloc oldKeys oldCap

  rehashedCnt <- readRef rehashedCntRef
  poke (getPtrOf (hssCtrl f)) newCtrl
  pokeBufStruct (getPtrOf (hssKeys f)) newKeys
  poke (getPtrOf (hssCap f)) newCap
  poke (getPtrOf (hssLen f)) rehashedCnt
  poke (getPtrOf (hssGrowthLeft f)) (maxLoadForCap newCap - rehashedCnt)

reserve ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  e Int64 ->
  m ()
reserve alloc hs@(HashSet hdr) additional = do
  let f = projectPtrs hdr
  curLen <- peek (getPtrOf (hssLen f))
  curCap <- peek (getPtrOf (hssCap f))
  growthLeft <- peek (getPtrOf (hssGrowthLeft f))
  whenM_ (additional `gt` growthLeft) $ do
    neededElems <- letM (curLen + additional)
    (_, targetCap) <-
      runDrain
        (drainFind 16 (\cand -> maxLoadForCap cand `ge` neededElems))
        (iterateS (ifThenElse (curCap `lt` 16) 16 curCap) (`lshift` 1))
    rehashTo alloc hs targetCap

findSlot ::
  forall k m e.
  (HashSetCtx m e k) =>
  HashSet e k ->
  Elem e k ->
  m (e Bool, e Int64)
findSlot (HashSet hdr) key = do
  let f = projectPtrs hdr
  cap <- peek (getPtrOf (hssCap f))
  foundRef <- newRef false
  slotRef <- newRef (0 :: e Int64)
  whenM_ (cap `gt` 0) $ do
    ctrl <- peek (getPtrOf (hssCtrl f))
    kb <- peekBufStruct (getPtrOf (hssKeys f))
    seed <- peek (getPtrOf (hssSeed f))
    h <- letM (hashElemWithSalt @e @k seed key)
    let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
        ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (cap `rshift` 3) - 1
        g0 = toIntegral h `iand` groupMask
    (_, (_, hitFound, hitSlot)) <-
      runDrain
        (drainFind (true, false, 0) (\(done, _, _) -> done))
        $ streamM (probeGroupsS g0 groupMask)
        $ \g -> do
          w <- peekElemOff ctrlWords g
          (hitInGrp, (_, candSlot)) <-
            runDrain (drainFind (false, 0) fst) $
              streamM (swarBitsS (swarMatchH2 w h2)) $ \lane -> do
                let s = (g `lshift` 3) + lane
                candKey <- readBuf kb s
                pure (eqElem @e @k candKey key, s)
          let done = hitInGrp `lor` (swarMatchEmpty w `ne` 0)
          pure (done, hitInGrp, candSlot)
    writeRef foundRef hitFound
    writeRef slotRef hitSlot
  (,) <$> readRef foundRef <*> readRef slotRef

-- | Insert @key@, returning @true@ iff @key@ was newly inserted (@false@ if already present).
insert ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  Elem e k ->
  m (e Bool)
insert alloc hs@(HashSet hdr) key = do
  (exists, _) <- findSlot hs key
  insertedRef <- newRef false
  whenM_ (lnot exists) $ do
    let f = projectPtrs hdr
    growthLeft <- peek (getPtrOf (hssGrowthLeft f))
    whenM_ (growthLeft `le` 0) $ do
      curCap <- peek (getPtrOf (hssCap f))
      curLen <- peek (getPtrOf (hssLen f))
      let nextCap =
            ifThenElse
              (curCap `eq` 0)
              16
              (ifThenElse ((curLen * 2) `lt` maxLoadForCap curCap) curCap (curCap `lshift` 1))
      rehashTo alloc hs nextCap

    cap <- peek (getPtrOf (hssCap f))
    ctrl <- peek (getPtrOf (hssCtrl f))
    kb <- peekBufStruct (getPtrOf (hssKeys f))
    seed <- peek (getPtrOf (hssSeed f))
    h <- letM (hashElemWithSalt @e @k seed key)
    let h2 = toIntegral ((h `rshift` 57) `iand` 0x7F) :: e Word8
        ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        groupMask = (cap `rshift` 3) - 1
        g0 = toIntegral h `iand` groupMask
    (_, slot) <-
      runDrain (drainFirst 0) $
        filterMapS
          ( \g -> do
              w <- peekElemOff ctrlWords g
              let avail = swarMatchEmptyOrDeleted w
              pure (avail `ne` 0, (g `lshift` 3) + swarLowestByteIdx avail)
          )
          (probeGroupsS g0 groupMask)
    prevCtrl <- peekElemOff ctrl slot
    pokeElemOff ctrl h2 slot
    writeBuf kb slot key
    curLen <- peek (getPtrOf (hssLen f))
    poke (getPtrOf (hssLen f)) (curLen + 1)
    whenM_ (prevCtrl `eq` fromIntegral ctrlEmptyByte) $ do
      gl <- peek (getPtrOf (hssGrowthLeft f))
      poke (getPtrOf (hssGrowthLeft f)) (gl - 1)
    writeRef insertedRef true
  readRef insertedRef

member :: (HashSetCtx m e k) => HashSet e k -> Elem e k -> m (e Bool)
member hs key = fst <$> findSlot hs key

delete ::
  forall k m e.
  (HashSetCtx m e k) =>
  HashSet e k ->
  Elem e k ->
  m (e Bool)
delete hs@(HashSet hdr) key = do
  (found, slot) <- findSlot hs key
  whenM_ found $ do
    let f = projectPtrs hdr
    ctrl <- peek (getPtrOf (hssCtrl f))
    curLen <- peek (getPtrOf (hssLen f))
    poke (getPtrOf (hssLen f)) (curLen - 1)
    let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
        grp = slot `rshift` 3
    w <- peekElemOff ctrlWords grp
    ifThenElseM_
      (swarMatchEmpty w `ne` 0)
      ( do
          pokeElemOff ctrl (fromIntegral ctrlEmptyByte) slot
          gl <- peek (getPtrOf (hssGrowthLeft f))
          poke (getPtrOf (hssGrowthLeft f)) (gl + 1)
      )
      (pokeElemOff ctrl (fromIntegral ctrlDeletedByte) slot)
  pure found

-- | SWAR-accelerated 'Stream' over all live elements of the set.
stream ::
  forall k m e.
  (HashSetCtx m e k) =>
  HashSet e k ->
  Stream m (Elem e k)
stream (HashSet hdr) =
  let f = projectPtrs hdr
   in withStreamSetup
        ((,,) <$> peek (getPtrOf (hssCap f)) <*> peek (getPtrOf (hssCtrl f)) <*> peekBufStruct (getPtrOf (hssKeys f)))
        $ \(cap, ctrl, kb) ->
          let ctrlWords = fromVoidPtr (toVoidPtr ctrl) :: e (Ptr Word64)
           in streamM (swarOccupiedSlotsS ctrlWords (cap `rshift` 3)) (readBuf kb)

isSubsetOf ::
  (HashSetCtx m e k) =>
  HashSet e k ->
  HashSet e k ->
  m (e Bool)
isSubsetOf sub sup = do
  nSub <- size sub
  nSup <- size sup
  resRef <- newRef false
  whenM_ (nSub `le` nSup) $ do
    ok <- runDrain (drainAll id) (streamM (stream sub) (member sup))
    writeRef resRef ok
  readRef resRef

unionInto ::
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  HashSet e k ->
  m ()
unionInto alloc dst src =
  runDrain (drainAppendHashSet alloc dst) (stream src)

-- | Fused intersection 'Stream' iterating the smaller set and probing the larger set.
intersectionS ::
  forall k m e.
  (HashSetCtx m e k) =>
  HashSet e k ->
  HashSet e k ->
  Stream m (Elem e k)
intersectionS a@(HashSet pa) b@(HashSet pb) =
  withStreamSetup
    ((,) <$> size a <*> size b)
    $ \(na, nb) ->
      let wa = ptrToWord64Expr pa
          wb = ptrToWord64Expr pb
          smaller :: HashSet e k
          smaller = HashSet (word64ToPtrExpr (ifThenElse (na `le` nb) wa wb))
          larger :: HashSet e k
          larger = HashSet (word64ToPtrExpr (ifThenElse (na `le` nb) wb wa))
       in filterMapS
            (\x -> (\ok -> (ok, x)) <$> member larger x)
            (stream smaller)

differenceS ::
  (HashSetCtx m e k) =>
  HashSet e k ->
  HashSet e k ->
  Stream m (Elem e k)
differenceS a b =
  filterMapS
    (\x -> (\inB -> (lnot inB, x)) <$> member b x)
    (stream a)

drainToHashSet ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (HashSet e k)
drainToHashSet alloc = Drain $ \s -> do
  hs <- new alloc
  runDrain (drainAppendHashSet alloc hs) s
  pure hs

fromStream ::
  forall k m e.
  (HashSetCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (HashSet e k)
fromStream alloc = runDrain (drainToHashSet alloc)

-- | Append/insert all elements yielded by a 'Stream' into an existing 'HashSet'.
drainAppendHashSet ::
  (HashSetCtx m e k) =>
  Alloc e ->
  HashSet e k ->
  Drain m (Elem e k) ()
drainAppendHashSet alloc hs = drainForEach (\x -> insert alloc hs x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type HashSetABI k = (Typeable k, JanusStruct (Buf k))

type instance JanusCEval (HashSet JanusC k -> r) = HashSet Identity k -> JanusCEval r

type instance JanusCRetType (HashSet JanusC k -> r) = JanusCRetType r

instance (HashSetABI k, JanusCParam r) => JanusCParam (HashSet JanusC k -> r) where
  jcparam = mkPtrHandleJCParam HashSet

instance (HashSetABI k, JanusCEvaluate r) => JanusCEvaluate (HashSet JanusC k -> r) where
  jceval = mkPtrHandleJCEval unHashSet HashSet

type instance JanusCEval (JanusCM (HashSet JanusC k)) = IO (HashSet Identity k)

type instance JanusCRetType (JanusCM (HashSet JanusC k)) = HashSet Identity k

instance (HashSetABI k) => JanusCParam (JanusCM (HashSet JanusC k)) where
  jcparam = mkPtrHandleJCRetParam unHashSet

instance (HashSetABI k) => JanusCEvaluate (JanusCM (HashSet JanusC k)) where
  jceval = mkPtrHandleJCRetEval HashSet

type instance LinuxX86_64CEval (HashSet LinuxX86_64C k -> r) = HashSet Identity k -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (HashSet LinuxX86_64C k -> r) = LinuxX86_64BaseRet r

instance (HashSetABI k, LinuxX86_64Param r) => LinuxX86_64Param (HashSet LinuxX86_64C k -> r) where
  x86param = mkPtrHandleX86Param HashSet

instance (HashSetABI k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (HashSet LinuxX86_64C k -> r) where
  x86eval = mkPtrHandleX86EvalParam unHashSet HashSet

instance (HashSetABI k, X86DeferCont r) => X86DeferCall (HashSet LinuxX86_64C k -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unHashSet HashSet

instance (HashSetABI k, X86DeferCont r) => X86DeferCont (HashSet LinuxX86_64C k -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unHashSet HashSet

type instance LinuxX86_64CEval (LinuxX86_64CM (HashSet LinuxX86_64C k)) = IO (HashSet Identity k)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (HashSet LinuxX86_64C k)) = HashSet Identity k

instance (HashSetABI k) => LinuxX86_64Param (LinuxX86_64CM (HashSet LinuxX86_64C k)) where
  x86param = mkPtrHandleX86RetParam unHashSet

instance (HashSetABI k) => LinuxX86_64Evaluate (LinuxX86_64CM (HashSet LinuxX86_64C k)) where
  x86eval = mkPtrHandleX86RetEval HashSet

instance (HashSetABI k) => X86DeferCall (LinuxX86_64CM (HashSet LinuxX86_64C k)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg HashSet

instance (HashSetABI k) => X86DeferCont (LinuxX86_64CM (HashSet LinuxX86_64C k)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont HashSet

instance (HashSetABI k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (HashSet LinuxX86_64C k -> r) where
  type JanusCTHEval (HashSet LinuxX86_64C k -> r) = HashSet Identity k -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(HashSet Identity k)) : ps, retSpec)

instance (HashSetABI k) => JanusCTH (LinuxX86_64CM (HashSet LinuxX86_64C k)) where
  type JanusCTHEval (LinuxX86_64CM (HashSet LinuxX86_64C k)) = IO (HashSet Identity k)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(HashSet Identity k)) (Proxy @(CStruct (HashSetSpine k))))
