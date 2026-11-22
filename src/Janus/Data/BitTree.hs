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

module Janus.Data.BitTree
  ( -- * 3-Level 64-Ary Summary Bitmask Pyramid (@O(1)@ Ordered Integer Set Up to @262,144@)
    BitTreeSpine (..),
    BitTree (..),
    BitTreeCtx,
    maxBitTreeUniverse,

    -- * Lifecycle
    new,
    free,

    -- * Queries & Clear
    clear,
    size,
    universe,
    isEmpty,

    -- * Guaranteed @O(1)@ Loop-Free Membership, Insertion, & Deletion
    member,
    insert,
    delete,

    -- * Guaranteed @O(1)@ Loop-Free Min, Max, Successor, & Priority-Queue Pop
    findFirstSet,
    findLastSet,
    findNextSetGe,
    popMin,

    -- * Hierarchical Non-Zero-Branch Stream & Drains
    stream,
    drainToBitTree,
    fromStream,
    drainAppendBitTree,
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
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocStruct,
    allocZeroedAligned,
    freeBytes,
    freeStruct,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), ExpSized (..), janusSIMDAlignment)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainForEach, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream (Stream, statefulS)
import Janus.Data.BitSet (clzWord64, ctzWord64)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..), max', min')
import Janus.Typed (JanusTyped)

-- | Maximum universe size @64^3 = 262,144@ covered by the 3-level 64-ary bitmask pyramid.
maxBitTreeUniverse :: Int64
maxBitTreeUniverse = 262144

-- | Spine struct for the 3-level 64-ary summary bitmask pyramid:
--
-- * @btsL0@: 1 root @Word64@ (bit @g1@ is @1@ iff @btsL1[g1] != 0@)
-- * @btsL1@: inline 64-element @Word64@ array (bit @b1@ of @btsL1[g1]@ is @1@ iff @btsL2[(g1 << 6) + b1] != 0@)
-- * @btsL2@: leaf @Word64@ array of length @btsL2Words = (btsUniverse + 63) >> 6@
data BitTreeSpine f = BitTreeSpine
  { btsL0 :: !(f Word64),
    btsL1 :: !(f (FixedArray 64 Word64)),
    btsL2 :: !(f (Ptr Word64)),
    btsL2Words :: !(f Int64),
    btsUniverse :: !(f Int64),
    btsLen :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (BitTreeSpine Identity)

deriving stock instance Show (BitTreeSpine Identity)

instance JanusStruct BitTreeSpine

newtype BitTree (e :: K.Type -> K.Type) = BitTree
  { unBitTree :: e (Ptr (CStruct BitTreeSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (BitTree Identity)

deriving stock instance Show (BitTree Identity)

instance JanusHKD BitTree

type BitTreeCtx m e =
  ( CmdAlloc m e,
    ExpPtr e Word64,
    JanusTyped e (Ptr Word64),
    CmdStorable m e (Ptr Word64),
    JanusTyped e (FixedArray 64 Word64),
    ExpSized e (CStruct BitTreeSpine),
    JanusTyped e (CStruct BitTreeSpine)
  )

--------------------------------------------------------------------------------
-- Lifecycle, Queries, & Clear
--------------------------------------------------------------------------------

-- | Allocate an empty 'BitTree' over @[0, U)@, clamping @U@ to @[1 .. 262144]@.
new ::
  forall m e.
  (BitTreeCtx m e) =>
  Alloc e ->
  e Int64 ->
  m (BitTree e)
new alloc reqUniverse = do
  hdr <- allocStruct @(CStruct BitTreeSpine) alloc
  let f = projectPtrs hdr
      l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
  u <- letM (min' (fromIntegral maxBitTreeUniverse) (max' 1 reqUniverse))
  l2Words <- letM ((u + 63) `rshift` 6)
  vp <- allocZeroedAligned alloc (l2Words `lshift` 3) (fromIntegral janusSIMDAlignment)
  let l2 = fromVoidPtr vp :: e (Ptr Word64)
  poke (getPtrOf (btsL0 f)) 0
  cmdMemset (toVoidPtr l1) 0 (64 * 8)
  poke (getPtrOf (btsL2 f)) l2
  poke (getPtrOf (btsL2Words f)) l2Words
  poke (getPtrOf (btsUniverse f)) u
  poke (getPtrOf (btsLen f)) 0
  pure (BitTree hdr)

free ::
  (BitTreeCtx m e) =>
  Alloc e ->
  BitTree e ->
  m ()
free alloc (BitTree hdr) = do
  let f = projectPtrs hdr
  l2Words <- peek (getPtrOf (btsL2Words f))
  whenM_ (l2Words `gt` 0) $ do
    l2 <- peek (getPtrOf (btsL2 f))
    freeBytes alloc (toVoidPtr l2) (l2Words `lshift` 3)
  freeStruct alloc hdr

clear ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  m ()
clear (BitTree hdr) = do
  let f = projectPtrs hdr
      l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
  w0 <- peek (getPtrOf (btsL0 f))
  whenM_ (w0 `ne` 0) $ do
    poke (getPtrOf (btsL0 f)) 0
    cmdMemset (toVoidPtr l1) 0 (64 * 8)
    l2Words <- peek (getPtrOf (btsL2Words f))
    l2 <- peek (getPtrOf (btsL2 f))
    cmdMemset (toVoidPtr l2) 0 (l2Words `lshift` 3)
    poke (getPtrOf (btsLen f)) 0

size :: (BitTreeCtx m e) => BitTree e -> m (e Int64)
size (BitTree hdr) = peek (getPtrOf (btsLen (projectPtrs hdr)))

universe :: (BitTreeCtx m e) => BitTree e -> m (e Int64)
universe (BitTree hdr) = peek (getPtrOf (btsUniverse (projectPtrs hdr)))

isEmpty :: (BitTreeCtx m e) => BitTree e -> m (e Bool)
isEmpty (BitTree hdr) = (`eq` 0) <$> peek (getPtrOf (btsL0 (projectPtrs hdr)))

--------------------------------------------------------------------------------
-- Guaranteed O(1) Loop-Free Membership, Insertion, & Deletion
--------------------------------------------------------------------------------

member ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  e Int64 ->
  m (e Bool)
member (BitTree hdr) x = do
  let f = projectPtrs hdr
  u <- peek (getPtrOf (btsUniverse f))
  resRef <- newRef false
  whenM_ ((x `ge` 0) `land` (x `lt` u)) $ do
    l2 <- peek (getPtrOf (btsL2 f))
    w2 <- peekElemOff l2 (x `rshift` 6)
    let m2 = (1 :: e Word64) `lshift` toIntegral (x `iand` 63)
    writeRef resRef ((w2 `iand` m2) `ne` 0)
  readRef resRef

-- | Guaranteed @O(1)@ loop-free insert of @x@ (@0 <= x < universe@).
-- Returns @true@ iff @x@ was newly inserted.
insert ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  e Int64 ->
  m (e Bool)
insert (BitTree hdr) x = do
  let f = projectPtrs hdr
  u <- peek (getPtrOf (btsUniverse f))
  insRef <- newRef false
  whenM_ ((x `ge` 0) `land` (x `lt` u)) $ do
    let l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
        g2 = x `rshift` 6
        g1 = x `rshift` 12
        m2 = (1 :: e Word64) `lshift` toIntegral (x `iand` 63)
        m1 = (1 :: e Word64) `lshift` toIntegral (g2 `iand` 63)
        m0 = (1 :: e Word64) `lshift` toIntegral g1
    l2 <- peek (getPtrOf (btsL2 f))
    w2 <- peekElemOff l2 g2
    whenM_ ((w2 `iand` m2) `eq` 0) $ do
      pokeElemOff l2 (w2 `ior` m2) g2
      w1 <- peekElemOff l1 g1
      pokeElemOff l1 (w1 `ior` m1) g1
      w0 <- peek (getPtrOf (btsL0 f))
      poke (getPtrOf (btsL0 f)) (w0 `ior` m0)
      curLen <- peek (getPtrOf (btsLen f))
      poke (getPtrOf (btsLen f)) (curLen + 1)
      writeRef insRef true
  readRef insRef

-- | Guaranteed @O(1)@ loop-free delete of @x@, propagating summary bit clears upward
-- (@L1[g1]@ bit cleared iff @L2[g2] == 0@; @L0@ bit cleared iff @L1[g1] == 0@).
delete ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  e Int64 ->
  m (e Bool)
delete (BitTree hdr) x = do
  let f = projectPtrs hdr
  u <- peek (getPtrOf (btsUniverse f))
  remRef <- newRef false
  whenM_ ((x `ge` 0) `land` (x `lt` u)) $ do
    let l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
        g2 = x `rshift` 6
        g1 = x `rshift` 12
        m2 = (1 :: e Word64) `lshift` toIntegral (x `iand` 63)
        m1 = (1 :: e Word64) `lshift` toIntegral (g2 `iand` 63)
        m0 = (1 :: e Word64) `lshift` toIntegral g1
    l2 <- peek (getPtrOf (btsL2 f))
    w2 <- peekElemOff l2 g2
    whenM_ ((w2 `iand` m2) `ne` 0) $ do
      w2' <- letM (w2 `iand` inot m2)
      pokeElemOff l2 w2' g2
      whenM_ (w2' `eq` 0) $ do
        w1 <- peekElemOff l1 g1
        w1' <- letM (w1 `iand` inot m1)
        pokeElemOff l1 w1' g1
        whenM_ (w1' `eq` 0) $ do
          w0 <- peek (getPtrOf (btsL0 f))
          poke (getPtrOf (btsL0 f)) (w0 `iand` inot m0)
      curLen <- peek (getPtrOf (btsLen f))
      poke (getPtrOf (btsLen f)) (curLen - 1)
      writeRef remRef true
  readRef remRef

--------------------------------------------------------------------------------
-- Guaranteed O(1) Loop-Free Min, Max, Successor, & PopMin
--------------------------------------------------------------------------------

findFirstSet ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  m (e Bool, e Int64)
findFirstSet (BitTree hdr) = do
  let f = projectPtrs hdr
  w0 <- peek (getPtrOf (btsL0 f))
  okRef <- newRef false
  valRef <- newRef (0 :: e Int64)
  whenM_ (w0 `ne` 0) $ do
    let l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
    l2 <- peek (getPtrOf (btsL2 f))
    g1 <- letM (ctzWord64 w0)
    w1 <- peekElemOff l1 g1
    g2 <- letM ((g1 `lshift` 6) + ctzWord64 w1)
    w2 <- peekElemOff l2 g2
    writeRef okRef true
    writeRef valRef ((g2 `lshift` 6) + ctzWord64 w2)
  (,) <$> readRef okRef <*> readRef valRef

findLastSet ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  m (e Bool, e Int64)
findLastSet (BitTree hdr) = do
  let f = projectPtrs hdr
  w0 <- peek (getPtrOf (btsL0 f))
  okRef <- newRef false
  valRef <- newRef (0 :: e Int64)
  whenM_ (w0 `ne` 0) $ do
    let l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
    l2 <- peek (getPtrOf (btsL2 f))
    g1 <- letM (63 - clzWord64 w0)
    w1 <- peekElemOff l1 g1
    g2 <- letM ((g1 `lshift` 6) + (63 - clzWord64 w1))
    w2 <- peekElemOff l2 g2
    writeRef okRef true
    writeRef valRef ((g2 `lshift` 6) + (63 - clzWord64 w2))
  (,) <$> readRef okRef <*> readRef valRef

-- | Guaranteed @O(1)@ loop-free successor query returning the smallest element @>= startBit@:
-- checks masked @L2[g2]@, then masked @L1[g1]@, then masked @L0@.
findNextSetGe ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  e Int64 ->
  m (e Bool, e Int64)
findNextSetGe (BitTree hdr) startBit = do
  let f = projectPtrs hdr
  u <- peek (getPtrOf (btsUniverse f))
  x <- letM (max' 0 startBit)
  okRef <- newRef false
  valRef <- newRef (0 :: e Int64)
  whenM_ (x `lt` u) $ do
    let l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
        g2 = x `rshift` 6
        b2 = x `iand` 63
        g1 = x `rshift` 12
        b1 = g2 `iand` 63
    l2 <- peek (getPtrOf (btsL2 f))
    w2 <- peekElemOff l2 g2
    cand2 <- letM (w2 `iand` (inot (0 :: e Word64) `lshift` toIntegral b2))
    ifThenElseM_
      (cand2 `ne` 0)
      ( do
          writeRef okRef true
          writeRef valRef ((g2 `lshift` 6) + ctzWord64 cand2)
      )
      ( do
          w1 <- peekElemOff l1 g1
          let maskGtB1 = (inot (0 :: e Word64) `lshift` toIntegral b1) `lshift` 1
          cand1 <- letM (w1 `iand` maskGtB1)
          ifThenElseM_
            (cand1 `ne` 0)
            ( do
                nextG2 <- letM ((g1 `lshift` 6) + ctzWord64 cand1)
                nw2 <- peekElemOff l2 nextG2
                writeRef okRef true
                writeRef valRef ((nextG2 `lshift` 6) + ctzWord64 nw2)
            )
            ( do
                w0 <- peek (getPtrOf (btsL0 f))
                let maskGtG1 = (inot (0 :: e Word64) `lshift` toIntegral g1) `lshift` 1
                cand0 <- letM (w0 `iand` maskGtG1)
                whenM_ (cand0 `ne` 0) $ do
                  nextG1 <- letM (ctzWord64 cand0)
                  nw1 <- peekElemOff l1 nextG1
                  nextG2 <- letM ((nextG1 `lshift` 6) + ctzWord64 nw1)
                  nw2 <- peekElemOff l2 nextG2
                  writeRef okRef true
                  writeRef valRef ((nextG2 `lshift` 6) + ctzWord64 nw2)
            )
      )
  (,) <$> readRef okRef <*> readRef valRef

-- | Guaranteed @O(1)@ priority-queue extract-min ('findFirstSet' + 'delete').
popMin ::
  (BitTreeCtx m e) =>
  BitTree e ->
  m (e Bool, e Int64)
popMin bt = do
  (found, mn) <- findFirstSet bt
  whenM_ found $ do
    _ <- delete bt mn
    pure ()
  pure (found, mn)

--------------------------------------------------------------------------------
-- Hierarchical Stream & Drains
--------------------------------------------------------------------------------

-- | @O(size)@ hierarchical ascending 'Stream' descending only into non-zero @L0 -> L1 -> L2@ words.
stream ::
  forall m e.
  (BitTreeCtx m e) =>
  BitTree e ->
  Stream m (e Int64)
stream (BitTree hdr) =
  let f = projectPtrs hdr
      l1 = fixedArrayElemPtr (getPtrOf (btsL1 f)) :: e (Ptr Word64)
   in statefulS
        ( do
            w0 <- peek (getPtrOf (btsL0 f))
            l2 <- peek (getPtrOf (btsL2 f))
            rem0Ref <- newRef w0
            rem1Ref <- newRef (0 :: e Word64)
            rem2Ref <- newRef (0 :: e Word64)
            g1Ref <- newRef (0 :: e Int64)
            g2Ref <- newRef (0 :: e Int64)
            pure (l2, rem0Ref, rem1Ref, rem2Ref, g1Ref, g2Ref)
        )
        $ \(l2, rem0Ref, rem1Ref, rem2Ref, g1Ref, g2Ref) -> ContT $ \kStep -> do
          rem2 <- readRef rem2Ref
          ifThenElseM_
            (rem2 `ne` 0)
            ( do
                g2 <- readRef g2Ref
                let x = (g2 `lshift` 6) + ctzWord64 rem2
                writeRef rem2Ref (rem2 `iand` (rem2 - 1))
                kStep (Yield x)
            )
            ( do
                rem1 <- readRef rem1Ref
                ifThenElseM_
                  (rem1 `ne` 0)
                  ( do
                      g1 <- readRef g1Ref
                      g2 <- letM ((g1 `lshift` 6) + ctzWord64 rem1)
                      w2 <- peekElemOff l2 g2
                      writeRef rem1Ref (rem1 `iand` (rem1 - 1))
                      writeRef g2Ref g2
                      writeRef rem2Ref w2
                      kStep Skip
                  )
                  ( do
                      rem0 <- readRef rem0Ref
                      ifThenElseM_
                        (rem0 `ne` 0)
                        ( do
                            g1 <- letM (ctzWord64 rem0)
                            w1 <- peekElemOff l1 g1
                            writeRef rem0Ref (rem0 `iand` (rem0 - 1))
                            writeRef g1Ref g1
                            writeRef rem1Ref w1
                            kStep Skip
                        )
                        (kStep Stop)
                  )
            )

drainToBitTree ::
  (BitTreeCtx m e) =>
  Alloc e ->
  e Int64 ->
  Drain m (e Int64) (BitTree e)
drainToBitTree alloc reqUniverse = Drain $ \s -> do
  bt <- new alloc reqUniverse
  runDrain (drainAppendBitTree bt) s
  pure bt

fromStream ::
  (BitTreeCtx m e) =>
  Alloc e ->
  e Int64 ->
  Stream m (e Int64) ->
  m (BitTree e)
fromStream alloc reqUniverse = runDrain (drainToBitTree alloc reqUniverse)

drainAppendBitTree ::
  (BitTreeCtx m e) =>
  BitTree e ->
  Drain m (e Int64) ()
drainAppendBitTree bt = drainForEach (\x -> insert bt x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCEval (BitTree JanusC -> r) = BitTree Identity -> JanusCEval r

type instance JanusCRetType (BitTree JanusC -> r) = JanusCRetType r

instance (JanusCParam r) => JanusCParam (BitTree JanusC -> r) where
  jcparam = mkPtrHandleJCParam BitTree

instance (JanusCEvaluate r) => JanusCEvaluate (BitTree JanusC -> r) where
  jceval = mkPtrHandleJCEval unBitTree BitTree

type instance JanusCEval (JanusCM (BitTree JanusC)) = IO (BitTree Identity)

type instance JanusCRetType (JanusCM (BitTree JanusC)) = BitTree Identity

instance JanusCParam (JanusCM (BitTree JanusC)) where
  jcparam = mkPtrHandleJCRetParam unBitTree

instance JanusCEvaluate (JanusCM (BitTree JanusC)) where
  jceval = mkPtrHandleJCRetEval BitTree

type instance LinuxX86_64CEval (BitTree LinuxX86_64C -> r) = BitTree Identity -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (BitTree LinuxX86_64C -> r) = LinuxX86_64BaseRet r

instance (LinuxX86_64Param r) => LinuxX86_64Param (BitTree LinuxX86_64C -> r) where
  x86param = mkPtrHandleX86Param BitTree

instance (X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (BitTree LinuxX86_64C -> r) where
  x86eval = mkPtrHandleX86EvalParam unBitTree BitTree

instance (X86DeferCont r) => X86DeferCall (BitTree LinuxX86_64C -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unBitTree BitTree

instance (X86DeferCont r) => X86DeferCont (BitTree LinuxX86_64C -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unBitTree BitTree

type instance LinuxX86_64CEval (LinuxX86_64CM (BitTree LinuxX86_64C)) = IO (BitTree Identity)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (BitTree LinuxX86_64C)) = BitTree Identity

instance LinuxX86_64Param (LinuxX86_64CM (BitTree LinuxX86_64C)) where
  x86param = mkPtrHandleX86RetParam unBitTree

instance LinuxX86_64Evaluate (LinuxX86_64CM (BitTree LinuxX86_64C)) where
  x86eval = mkPtrHandleX86RetEval BitTree

instance X86DeferCall (LinuxX86_64CM (BitTree LinuxX86_64C)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg BitTree

instance X86DeferCont (LinuxX86_64CM (BitTree LinuxX86_64C)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont BitTree

instance (LinuxX86_64Param r, JanusCTH r) => JanusCTH (BitTree LinuxX86_64C -> r) where
  type JanusCTHEval (BitTree LinuxX86_64C -> r) = BitTree Identity -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(BitTree Identity)) : ps, retSpec)

instance JanusCTH (LinuxX86_64CM (BitTree LinuxX86_64C)) where
  type JanusCTHEval (LinuxX86_64CM (BitTree LinuxX86_64C)) = IO (BitTree Identity)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(BitTree Identity)) (Proxy @(CStruct BitTreeSpine)))
