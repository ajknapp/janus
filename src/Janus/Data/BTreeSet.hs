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

module Janus.Data.BTreeSet
  ( -- * Zero-Value-Buffer Cache-Line B+Tree Set (`B = 8`, Max 15 Keys)
    BTreeSetLeaf (..),
    BTreeSetSpine (..),
    BTreeSet (..),
    BTreeSetCtx,

    -- * Lifecycle
    new,
    free,
    clear,

    -- * Queries & Structural Invariant Verification
    size,
    height,
    isEmpty,
    checkInvariants,

    -- * Ordered Membership, Insertion, Deletion, & Neighbour Queries
    insert,
    member,
    delete,
    lookupGE,
    withLookupGE,
    lookupLE,
    withLookupLE,
    minKey,
    maxKey,

    -- * Zero-Stack Doubly-Linked Leaf Streams, Set-Algebra Merges, & Drains
    stream,
    reverseStream,
    rangeStream,
    unionS,
    intersectionS,
    differenceS,
    drainToBTreeSet,
    fromStream,
    drainAppendBTreeSet,
  )
where

import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Cont (ContT (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
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
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (RefCell, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain (..), drainFoldM, drainForEach, drain_, runDrain)
import Janus.Control.Step (Step (..))
import Janus.Control.Stream
  ( Stream (..),
    differenceSortedByS,
    intersectSortedByS,
    rangeS,
    reverseRangeS,
    unfoldWhileM,
    unionSortedByS,
  )
import Janus.Data.BTreeMap (BTreeInternal (..), BTreeMaxChildren, BTreeMaxKeys)
import Janus.Data.FlatMap (OrdKey (..))
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (lowerBoundBuf, upperBoundBuf)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

btMinKeys :: Int64
btMinKeys = 7

btMaxKeys :: Int64
btMaxKeys = 15

-- | Dedicated key-only leaf node (zero value storage overhead).
data BTreeSetLeaf k f = BTreeSetLeaf
  { btslNumKeys :: !(f Int64),
    btslIsLeaf :: !(f Int64),
    btslPrevLeaf :: !(f (Ptr Word8)),
    btslNextLeaf :: !(f (Ptr Word8)),
    btslKeys :: !(f (CStruct (InlineBuf BTreeMaxKeys k)))
  }
  deriving stock (Generic)

instance
  ( Typeable k,
    JanusStruct (InlineBuf BTreeMaxKeys k)
  ) =>
  JanusStruct (BTreeSetLeaf k)

data BTreeSetSpine k f = BTreeSetSpine
  { btssRoot :: !(f (Ptr Word8)),
    btssFirstLeaf :: !(f (Ptr Word8)),
    btssLastLeaf :: !(f (Ptr Word8)),
    btssLen :: !(f Int64),
    btssHeight :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k) => JanusStruct (BTreeSetSpine k)

newtype BTreeSet (e :: K.Type -> K.Type) k = BTreeSet
  { unBTreeSet :: e (Ptr (CStruct (BTreeSetSpine k)))
  }
  deriving stock (Generic)

deriving stock instance Eq (BTreeSet Identity k)

deriving stock instance Show (BTreeSet Identity k)

type BTreeSetCtx m e k =
  ( InlineLayoutCtx BTreeMaxKeys m e k,
    OrdKey e k,
    Typeable k,
    JanusTyped e (FixedArray BTreeMaxChildren (Ptr Word8)),
    ExpSized e (CStruct (BTreeInternal k)),
    JanusTyped e (CStruct (BTreeInternal k)),
    ExpSized e (CStruct (BTreeSetLeaf k)),
    JanusTyped e (CStruct (BTreeSetLeaf k)),
    ExpSized e (CStruct (BTreeSetSpine k)),
    JanusTyped e (CStruct (BTreeSetSpine k))
  )

asInternal ::
  forall k e.
  (ExpPtrCast e, JanusTyped e Word8, JanusTyped e (CStruct (BTreeInternal k))) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct (BTreeInternal k)))
asInternal p = fromVoidPtr (toVoidPtr p)

asSetLeaf ::
  forall k e.
  (ExpPtrCast e, JanusTyped e Word8, JanusTyped e (CStruct (BTreeSetLeaf k))) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct (BTreeSetLeaf k)))
asSetLeaf p = fromVoidPtr (toVoidPtr p)

toRawNode ::
  (ExpPtrCast e, JanusTyped e a, JanusTyped e Word8) =>
  e (Ptr a) ->
  e (Ptr Word8)
toRawNode p = fromVoidPtr (toVoidPtr p)

internalKeysBuf ::
  forall k m e.
  ( InlineLayoutCtx BTreeMaxKeys m e k,
    Typeable k,
    JanusTyped e (FixedArray BTreeMaxChildren (Ptr Word8))
  ) =>
  e (Ptr (CStruct (BTreeInternal k))) ->
  m (Buf k e)
internalKeysBuf pInt =
  inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btiKeys (projectPtrs pInt)))

internalChildrenPtr ::
  forall k e.
  ( ExpStructPtr e,
    ExpPtrCast e,
    Typeable k,
    JanusStruct (InlineBuf BTreeMaxKeys k),
    JanusTyped e Int64,
    JanusTyped e (CStruct (InlineBuf BTreeMaxKeys k)),
    JanusTyped e (Ptr Word8),
    JanusTyped e (FixedArray BTreeMaxChildren (Ptr Word8))
  ) =>
  e (Ptr (CStruct (BTreeInternal k))) ->
  e (Ptr (Ptr Word8))
internalChildrenPtr pInt =
  fixedArrayElemPtr (getPtrOf (btiChildren (projectPtrs pInt)))

setLeafKeysBuf ::
  forall k m e.
  (InlineLayoutCtx BTreeMaxKeys m e k, Typeable k) =>
  e (Ptr (CStruct (BTreeSetLeaf k))) ->
  m (Buf k e)
setLeafKeysBuf pLeaf =
  inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btslKeys (projectPtrs pLeaf)))

allocSetLeaf ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  m (e (Ptr (CStruct (BTreeSetLeaf k))))
allocSetLeaf alloc = do
  p <- allocStruct @(CStruct (BTreeSetLeaf k)) alloc
  let lf = projectPtrs p
  poke (getPtrOf (btslNumKeys lf)) 0
  poke (getPtrOf (btslIsLeaf lf)) 1
  poke (getPtrOf (btslPrevLeaf lf)) nullPtr
  poke (getPtrOf (btslNextLeaf lf)) nullPtr
  pure p

allocInternal ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  m (e (Ptr (CStruct (BTreeInternal k))))
allocInternal alloc = do
  p <- allocStruct @(CStruct (BTreeInternal k)) alloc
  let inf = projectPtrs p
  poke (getPtrOf (btiNumKeys inf)) 0
  poke (getPtrOf (btiIsLeaf inf)) 0
  pure p

new ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  m (BTreeSet e k)
new alloc = do
  hdr <- allocStruct @(CStruct (BTreeSetSpine k)) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (btssRoot f)) nullPtr
  poke (getPtrOf (btssFirstLeaf f)) nullPtr
  poke (getPtrOf (btssLastLeaf f)) nullPtr
  poke (getPtrOf (btssLen f)) 0
  poke (getPtrOf (btssHeight f)) 0
  pure (BTreeSet hdr)

clear ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  m ()
clear alloc (BTreeSet hdr) = do
  let f = projectPtrs hdr
  h <- peek (getPtrOf (btssHeight f))
  whenM_ (h `gt` 0) $ do
    firstL <- peek (getPtrOf (btssFirstLeaf f))
    let leavesS =
          unfoldWhileM firstL (\curL -> pure (ptrToWord64Expr curL `ne` 0)) $ \curL -> do
            let pLeaf = asSetLeaf @k curL
            nxtL <- peek (getPtrOf (btslNextLeaf (projectPtrs pLeaf)))
            pure (nxtL, pLeaf)
    runDrain (drainForEach (freeStruct alloc)) leavesS
    whenM_ (h `gt` 1) $ do
      let internalDepth = h - 1
          nsBytes = internalDepth * sizeOf (Proxy @(Ptr Word8))
          isBytes = internalDepth * sizeOf (Proxy @Int64)
      nsVP <- allocBytesAligned alloc nsBytes (alignOf (Proxy @(Ptr Word8)))
      isVP <- allocBytesAligned alloc isBytes (alignOf (Proxy @Int64))
      let nsPtr = fromVoidPtr nsVP :: e (Ptr (Ptr Word8))
          isPtr = fromVoidPtr isVP :: e (Ptr Int64)
      rootRaw <- peek (getPtrOf (btssRoot f))
      pokeElemOff nsPtr rootRaw 0
      pokeElemOff isPtr 0 0
      let postOrderStepS =
            unfoldWhileM (0 :: e Int64) (\sp -> pure (sp `ge` 0)) $ \sp -> do
              curRaw <- peekElemOff nsPtr sp
              let pInt = asInternal @k curRaw
              m <- peek (getPtrOf (btiNumKeys (projectPtrs pInt)))
              ci <- peekElemOff isPtr sp
              nextSpRef <- newRef (sp - 1)
              ifThenElseM_
                ((sp + 1 `lt` internalDepth) `land` (ci `le` m))
                ( do
                    chRaw <- peekElemOff (internalChildrenPtr pInt) ci
                    pokeElemOff isPtr (ci + 1) sp
                    pokeElemOff nsPtr chRaw (sp + 1)
                    pokeElemOff isPtr 0 (sp + 1)
                    writeRef nextSpRef (sp + 1)
                )
                (freeStruct alloc pInt)
              nextSp <- readRef nextSpRef
              pure (nextSp, ())
      runDrain drain_ postOrderStepS
      freeBytes alloc nsVP nsBytes
      freeBytes alloc isVP isBytes
    poke (getPtrOf (btssRoot f)) nullPtr
    poke (getPtrOf (btssFirstLeaf f)) nullPtr
    poke (getPtrOf (btssLastLeaf f)) nullPtr
    poke (getPtrOf (btssLen f)) 0
    poke (getPtrOf (btssHeight f)) 0

free ::
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  m ()
free alloc bs@(BTreeSet hdr) = do
  clear alloc bs
  freeStruct alloc hdr

size :: (BTreeSetCtx m e k) => BTreeSet e k -> m (e Int64)
size (BTreeSet hdr) = peek (getPtrOf (btssLen (projectPtrs hdr)))

height :: (BTreeSetCtx m e k) => BTreeSet e k -> m (e Int64)
height (BTreeSet hdr) = peek (getPtrOf (btssHeight (projectPtrs hdr)))

isEmpty :: (BTreeSetCtx m e k) => BTreeSet e k -> m (e Bool)
isEmpty bs = (`eq` 0) <$> size bs

descendToLeaf ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Elem e k ->
  m (e (Ptr (CStruct (BTreeSetLeaf k))))
descendToLeaf (BTreeSet hdr) target = do
  let f = projectPtrs hdr
  h <- peek (getPtrOf (btssHeight f))
  root0 <- peek (getPtrOf (btssRoot f))
  leafRaw <-
    runDrain
      ( drainFoldM root0 $ \curRaw _ -> do
          let pInt = asInternal @k curRaw
          m <- peek (getPtrOf (btiNumKeys (projectPtrs pInt)))
          kb <- internalKeysBuf pInt
          ci <- upperBoundBuf (ltKey @e @k) kb m target
          peekElemOff (internalChildrenPtr pInt) ci
      )
      (rangeS 0 (h - 1))
  pure (asSetLeaf @k leafRaw)

member ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Elem e k ->
  m (e Bool)
member bs@(BTreeSet hdr) key = do
  h <- peek (getPtrOf (btssHeight (projectPtrs hdr)))
  okRef <- newRef false
  whenM_ (h `gt` 0) $ do
    pLeaf <- descendToLeaf bs key
    n <- peek (getPtrOf (btslNumKeys (projectPtrs pLeaf)))
    kb <- setLeafKeysBuf pLeaf
    pos <- lowerBoundBuf (ltKey @e @k) kb n key
    whenM_ (pos `lt` n) $ do
      candK <- readBuf kb pos
      writeRef okRef (eqKey @e @k candK key)
  readRef okRef

withLookupGE ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Elem e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withLookupGE bs@(BTreeSet hdr) target onMiss onHit = do
  h <- peek (getPtrOf (btssHeight (projectPtrs hdr)))
  ifThenElseM_
    (h `le` 0)
    onMiss
    ( do
        pLeaf0 <- descendToLeaf bs target
        n0 <- peek (getPtrOf (btslNumKeys (projectPtrs pLeaf0)))
        kb0 <- setLeafKeysBuf pLeaf0
        pos0 <- lowerBoundBuf (ltKey @e @k) kb0 n0 target
        ifThenElseM_
          (pos0 `lt` n0)
          (readBuf kb0 pos0 >>= onHit)
          ( do
              nxtRaw <- peek (getPtrOf (btslNextLeaf (projectPtrs pLeaf0)))
              ifThenElseM_
                (ptrToWord64Expr nxtRaw `eq` 0)
                onMiss
                ( do
                    kb1 <- setLeafKeysBuf (asSetLeaf @k nxtRaw)
                    readBuf kb1 0 >>= onHit
                )
          )
    )

lookupGE ::
  forall k ks m e.
  (BTreeSetCtx m e k, Elem e k ~ e ks, JanusTyped e ks) =>
  e ks ->
  BTreeSet e k ->
  e ks ->
  m (e Bool, e ks)
lookupGE defK bs target = do
  okRef <- newRef false
  kRef <- newRef defK
  withLookupGE bs target (pure ()) (\ko -> writeRef okRef true >> writeRef kRef ko)
  (,) <$> readRef okRef <*> readRef kRef

withLookupLE ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Elem e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
withLookupLE bs@(BTreeSet hdr) target onMiss onHit = do
  h <- peek (getPtrOf (btssHeight (projectPtrs hdr)))
  ifThenElseM_
    (h `le` 0)
    onMiss
    ( do
        pLeaf0 <- descendToLeaf bs target
        n0 <- peek (getPtrOf (btslNumKeys (projectPtrs pLeaf0)))
        kb0 <- setLeafKeysBuf pLeaf0
        ub <- upperBoundBuf (ltKey @e @k) kb0 n0 target
        ifThenElseM_
          (ub `gt` 0)
          (readBuf kb0 (ub - 1) >>= onHit)
          ( do
              prvRaw <- peek (getPtrOf (btslPrevLeaf (projectPtrs pLeaf0)))
              ifThenElseM_
                (ptrToWord64Expr prvRaw `eq` 0)
                onMiss
                ( do
                    let pPrv = asSetLeaf @k prvRaw
                    nPrv <- peek (getPtrOf (btslNumKeys (projectPtrs pPrv)))
                    kb1 <- setLeafKeysBuf pPrv
                    readBuf kb1 (nPrv - 1) >>= onHit
                )
          )
    )

lookupLE ::
  forall k ks m e.
  (BTreeSetCtx m e k, Elem e k ~ e ks, JanusTyped e ks) =>
  e ks ->
  BTreeSet e k ->
  e ks ->
  m (e Bool, e ks)
lookupLE defK bs target = do
  okRef <- newRef false
  kRef <- newRef defK
  withLookupLE bs target (pure ()) (\ko -> writeRef okRef true >> writeRef kRef ko)
  (,) <$> readRef okRef <*> readRef kRef

minKey ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
minKey (BTreeSet hdr) onEmpty onHit = do
  firstRaw <- peek (getPtrOf (btssFirstLeaf (projectPtrs hdr)))
  ifThenElseM_
    (ptrToWord64Expr firstRaw `eq` 0)
    onEmpty
    (setLeafKeysBuf (asSetLeaf @k firstRaw) >>= \kb -> readBuf kb 0 >>= onHit)

maxKey ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  m () ->
  (Elem e k -> m ()) ->
  m ()
maxKey (BTreeSet hdr) onEmpty onHit = do
  lastRaw <- peek (getPtrOf (btssLastLeaf (projectPtrs hdr)))
  ifThenElseM_
    (ptrToWord64Expr lastRaw `eq` 0)
    onEmpty
    ( do
        let pLeaf = asSetLeaf @k lastRaw
        n <- peek (getPtrOf (btslNumKeys (projectPtrs pLeaf)))
        kb <- setLeafKeysBuf pLeaf
        readBuf kb (n - 1) >>= onHit
    )

--------------------------------------------------------------------------------
-- Top-Down Split Insertion & Rebalance Deletion
--------------------------------------------------------------------------------

splitSetLeafChild ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  e (Ptr (CStruct (BTreeSetLeaf k))) ->
  m ()
splitSetLeafChild alloc (BTreeSet hdr) pParent ci pLeft = do
  pRight <- allocSetLeaf @k alloc
  let lfL = projectPtrs pLeft
      lfR = projectPtrs pRight
      pf = projectPtrs pParent
      tf = projectPtrs hdr
  kbL <- setLeafKeysBuf pLeft
  kbR <- setLeafKeysBuf pRight
  copyBuf kbR 0 kbL 7 8
  poke (getPtrOf (btslNumKeys lfL)) 7
  poke (getPtrOf (btslNumKeys lfR)) 8

  oldNext <- peek (getPtrOf (btslNextLeaf lfL))
  let rawLeft = toRawNode pLeft
      rawRight = toRawNode pRight
  poke (getPtrOf (btslPrevLeaf lfR)) rawLeft
  poke (getPtrOf (btslNextLeaf lfR)) oldNext
  poke (getPtrOf (btslNextLeaf lfL)) rawRight
  ifThenElseM_
    (ptrToWord64Expr oldNext `ne` 0)
    (poke (getPtrOf (btslPrevLeaf (projectPtrs (asSetLeaf @k oldNext)))) rawRight)
    (poke (getPtrOf (btssLastLeaf tf)) rawRight)

  sepKey <- readBuf kbR 0
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  let pCh = internalChildrenPtr pParent
      shiftKeys = pm - ci
  whenM_ (shiftKeys `gt` 0) $ do
    moveBuf pKeys (ci + 1) ci shiftKeys
    runDrain
      ( drainForEach $ \j -> do
          ch <- peekElemOff pCh j
          pokeElemOff pCh ch (j + 1)
      )
      (reverseRangeS (ci + 1) (pm + 1))
  writeBuf pKeys ci sepKey
  pokeElemOff pCh rawRight (ci + 1)
  poke (getPtrOf (btiNumKeys pf)) (pm + 1)

splitSetInternalChild ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  m ()
splitSetInternalChild alloc pParent ci pLeft = do
  pRight <- allocInternal @k alloc
  let lf = projectPtrs pLeft
      rf = projectPtrs pRight
      pf = projectPtrs pParent
  kbL <- internalKeysBuf pLeft
  kbR <- internalKeysBuf pRight
  let chL = internalChildrenPtr pLeft
      chR = internalChildrenPtr pRight
  midKey <- readBuf kbL 7
  copyBuf kbR 0 kbL 8 7
  runDrain
    (drainForEach (\j -> peekElemOff chL (8 + j) >>= \cp -> pokeElemOff chR cp j))
    (rangeS 0 8)
  poke (getPtrOf (btiNumKeys lf)) 7
  poke (getPtrOf (btiNumKeys rf)) 7

  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  let pCh = internalChildrenPtr pParent
      shiftKeys = pm - ci
  whenM_ (shiftKeys `gt` 0) $ do
    moveBuf pKeys (ci + 1) ci shiftKeys
    runDrain
      ( drainForEach $ \j -> do
          ch <- peekElemOff pCh j
          pokeElemOff pCh ch (j + 1)
      )
      (reverseRangeS (ci + 1) (pm + 1))
  writeBuf pKeys ci midKey
  pokeElemOff pCh (toRawNode pRight) (ci + 1)
  poke (getPtrOf (btiNumKeys pf)) (pm + 1)

insertIntoNonFullSetLeaf ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  e (Ptr (CStruct (BTreeSetLeaf k))) ->
  Elem e k ->
  m (e Bool)
insertIntoNonFullSetLeaf (BTreeSet hdr) pLeaf key = do
  let lf = projectPtrs pLeaf
      tf = projectPtrs hdr
  n <- peek (getPtrOf (btslNumKeys lf))
  kb <- setLeafKeysBuf pLeaf
  pos <- lowerBoundBuf (ltKey @e @k) kb n key
  existsRef <- newRef false
  whenM_ (pos `lt` n) $ do
    candK <- readBuf kb pos
    writeRef existsRef (eqKey @e @k candK key)
  exists <- readRef existsRef
  insertedRef <- newRef false
  whenM_ (lnot exists) $ do
    let tailCnt = n - pos
    whenM_ (tailCnt `gt` 0) $
      moveBuf kb (pos + 1) pos tailCnt
    writeBuf kb pos key
    poke (getPtrOf (btslNumKeys lf)) (n + 1)
    curLen <- peek (getPtrOf (btssLen tf))
    poke (getPtrOf (btssLen tf)) (curLen + 1)
    writeRef insertedRef true
  readRef insertedRef

-- | Insert @key@, returning @true@ iff @key@ was newly inserted.
insert ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  Elem e k ->
  m (e Bool)
insert alloc bs@(BTreeSet hdr) key = do
  let tf = projectPtrs hdr
  h0 <- peek (getPtrOf (btssHeight tf))
  insertedRef <- newRef false
  ifThenElseM_
    (h0 `eq` 0)
    ( do
        pLeaf <- allocSetLeaf @k alloc
        let rawL = toRawNode pLeaf
        poke (getPtrOf (btssRoot tf)) rawL
        poke (getPtrOf (btssFirstLeaf tf)) rawL
        poke (getPtrOf (btssLastLeaf tf)) rawL
        poke (getPtrOf (btssHeight tf)) 1
        insertIntoNonFullSetLeaf bs pLeaf key >>= writeRef insertedRef
    )
    ( do
        root0 <- peek (getPtrOf (btssRoot tf))
        rKeysRef <- newRef 0
        ifThenElseM_
          (h0 `eq` 1)
          (peek (getPtrOf (btslNumKeys (projectPtrs (asSetLeaf @k root0)))) >>= writeRef rKeysRef)
          (peek (getPtrOf (btiNumKeys (projectPtrs (asInternal @k root0)))) >>= writeRef rKeysRef)
        rootKeys0 <- readRef rKeysRef
        whenM_ (rootKeys0 `eq` fromIntegral btMaxKeys) $ do
          newRoot <- allocInternal @k alloc
          pokeElemOff (internalChildrenPtr newRoot) root0 0
          poke (getPtrOf (btssRoot tf)) (toRawNode newRoot)
          poke (getPtrOf (btssHeight tf)) (h0 + 1)
          ifThenElseM_
            (h0 `eq` 1)
            (splitSetLeafChild alloc bs newRoot 0 (asSetLeaf @k root0))
            (splitSetInternalChild alloc newRoot 0 (asInternal @k root0))

        hNow <- peek (getPtrOf (btssHeight tf))
        curRef <- peek (getPtrOf (btssRoot tf)) >>= newRef
        runDrain
          ( drainForEach $ \depth -> do
              pCur <- asInternal @k <$> readRef curRef
              pm <- peek (getPtrOf (btiNumKeys (projectPtrs pCur)))
              pKeys <- internalKeysBuf pCur
              ci0 <- upperBoundBuf (ltKey @e @k) pKeys pm key
              ch0 <- peekElemOff (internalChildrenPtr pCur) ci0
              let childIsLeaf = (depth + 1) `eq` (hNow - 1)
              ifThenElseM_
                childIsLeaf
                ( do
                    let pChildL = asSetLeaf @k ch0
                    cn <- peek (getPtrOf (btslNumKeys (projectPtrs pChildL)))
                    ifThenElseM_
                      (cn `eq` fromIntegral btMaxKeys)
                      ( do
                          splitSetLeafChild alloc bs pCur ci0 pChildL
                          sepK <- readBuf pKeys ci0
                          let ciFinal = ifThenElse (ltKey @e @k key sepK) ci0 (ci0 + 1)
                          peekElemOff (internalChildrenPtr pCur) ciFinal >>= writeRef curRef
                      )
                      (writeRef curRef ch0)
                )
                ( do
                    let pChildI = asInternal @k ch0
                    cm <- peek (getPtrOf (btiNumKeys (projectPtrs pChildI)))
                    ifThenElseM_
                      (cm `eq` fromIntegral btMaxKeys)
                      ( do
                          splitSetInternalChild alloc pCur ci0 pChildI
                          sepK <- readBuf pKeys ci0
                          let ciFinal = ifThenElse (ltKey @e @k key sepK) ci0 (ci0 + 1)
                          peekElemOff (internalChildrenPtr pCur) ciFinal >>= writeRef curRef
                      )
                      (writeRef curRef ch0)
                )
          )
          (rangeS 0 (hNow - 1))
        pTargetLeaf <- asSetLeaf @k <$> readRef curRef
        insertIntoNonFullSetLeaf bs pTargetLeaf key >>= writeRef insertedRef
    )
  readRef insertedRef

removeParentSepSet ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m ()
removeParentSepSet pParent leftIdx = do
  let pf = projectPtrs pParent
      pCh = internalChildrenPtr pParent
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  let tailKeys = pm - leftIdx - 1
  whenM_ (tailKeys `gt` 0) $ do
    moveBuf pKeys leftIdx (leftIdx + 1) tailKeys
    runDrain
      (drainForEach (\j -> peekElemOff pCh (j + 1) >>= \cp -> pokeElemOff pCh cp j))
      (rangeS (leftIdx + 1) pm)
  poke (getPtrOf (btiNumKeys pf)) (pm - 1)

ensureSetLeafRich ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m (e (Ptr Word8), e Bool)
ensureSetLeafRich alloc (BTreeSet hdr) pParent ci = do
  let pf = projectPtrs pParent
      pCh = internalChildrenPtr pParent
      tf = projectPtrs hdr
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  chRaw <- peekElemOff pCh ci
  let pChild = asSetLeaf @k chRaw
  cn <- peek (getPtrOf (btslNumKeys (projectPtrs pChild)))
  outRef <- newRef chRaw
  mergedRef <- newRef false

  whenM_ (cn `le` fromIntegral btMinKeys) $ do
    doneRef <- newRef false
    whenM_ (ci `gt` 0) $ do
      lRaw <- peekElemOff pCh (ci - 1)
      let pLeft = asSetLeaf @k lRaw
      ln <- peek (getPtrOf (btslNumKeys (projectPtrs pLeft)))
      whenM_ (ln `gt` fromIntegral btMinKeys) $ do
        kbL <- setLeafKeysBuf pLeft
        kbC <- setLeafKeysBuf pChild
        lk <- readBuf kbL (ln - 1)
        moveBuf kbC 1 0 cn
        writeBuf kbC 0 lk
        poke (getPtrOf (btslNumKeys (projectPtrs pLeft))) (ln - 1)
        poke (getPtrOf (btslNumKeys (projectPtrs pChild))) (cn + 1)
        writeBuf pKeys (ci - 1) lk
        writeRef doneRef true

    done1 <- readRef doneRef
    whenM_ (lnot done1 `land` (ci `lt` pm)) $ do
      rRaw <- peekElemOff pCh (ci + 1)
      let pRight = asSetLeaf @k rRaw
      rn <- peek (getPtrOf (btslNumKeys (projectPtrs pRight)))
      whenM_ (rn `gt` fromIntegral btMinKeys) $ do
        kbC <- setLeafKeysBuf pChild
        kbR <- setLeafKeysBuf pRight
        rk0 <- readBuf kbR 0
        writeBuf kbC cn rk0
        moveBuf kbR 0 1 (rn - 1)
        poke (getPtrOf (btslNumKeys (projectPtrs pChild))) (cn + 1)
        poke (getPtrOf (btslNumKeys (projectPtrs pRight))) (rn - 1)
        readBuf kbR 0 >>= writeBuf pKeys ci
        writeRef doneRef true

    done2 <- readRef doneRef
    whenM_ (lnot done2) $ do
      let leftIdx = ifThenElse (ci `lt` pm) ci (ci - 1)
      lRaw <- peekElemOff pCh leftIdx
      rRaw <- peekElemOff pCh (leftIdx + 1)
      let pLeft = asSetLeaf @k lRaw
          pRight = asSetLeaf @k rRaw
          lfL = projectPtrs pLeft
          lfR = projectPtrs pRight
      ln <- peek (getPtrOf (btslNumKeys lfL))
      rn <- peek (getPtrOf (btslNumKeys lfR))
      kbL <- setLeafKeysBuf pLeft
      kbR <- setLeafKeysBuf pRight
      copyBuf kbL ln kbR 0 rn
      poke (getPtrOf (btslNumKeys lfL)) (ln + rn)

      nxtR <- peek (getPtrOf (btslNextLeaf lfR))
      poke (getPtrOf (btslNextLeaf lfL)) nxtR
      ifThenElseM_
        (ptrToWord64Expr nxtR `ne` 0)
        (poke (getPtrOf (btslPrevLeaf (projectPtrs (asSetLeaf @k nxtR)))) lRaw)
        (poke (getPtrOf (btssLastLeaf tf)) lRaw)

      freeStruct alloc pRight
      removeParentSepSet pParent leftIdx
      writeRef outRef lRaw
      writeRef mergedRef true

  (,) <$> readRef outRef <*> readRef mergedRef

ensureSetInternalRich ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m (e (Ptr Word8), e Bool)
ensureSetInternalRich alloc pParent ci = do
  let pf = projectPtrs pParent
      pCh = internalChildrenPtr pParent
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  chRaw <- peekElemOff pCh ci
  let pChild = asInternal @k chRaw
  cm <- peek (getPtrOf (btiNumKeys (projectPtrs pChild)))
  outRef <- newRef chRaw
  mergedRef <- newRef false

  whenM_ (cm `le` fromIntegral btMinKeys) $ do
    doneRef <- newRef false
    whenM_ (ci `gt` 0) $ do
      lRaw <- peekElemOff pCh (ci - 1)
      let pLeft = asInternal @k lRaw
      lm <- peek (getPtrOf (btiNumKeys (projectPtrs pLeft)))
      whenM_ (lm `gt` fromIntegral btMinKeys) $ do
        kbL <- internalKeysBuf pLeft
        kbC <- internalKeysBuf pChild
        let chL = internalChildrenPtr pLeft
            chC = internalChildrenPtr pChild
        parentSep <- readBuf pKeys (ci - 1)
        leftLastK <- readBuf kbL (lm - 1)
        leftLastCh <- peekElemOff chL lm
        moveBuf kbC 1 0 cm
        moveBuf kbC 1 0 cm
        runDrain
          (drainForEach (\j -> peekElemOff chC j >>= \cp -> pokeElemOff chC cp (j + 1)))
          (reverseRangeS 0 (cm + 1))
        writeBuf kbC 0 parentSep
        pokeElemOff chC leftLastCh 0
        writeBuf pKeys (ci - 1) leftLastK
        poke (getPtrOf (btiNumKeys (projectPtrs pLeft))) (lm - 1)
        poke (getPtrOf (btiNumKeys (projectPtrs pChild))) (cm + 1)
        writeRef doneRef true

    done1 <- readRef doneRef
    whenM_ (lnot done1 `land` (ci `lt` pm)) $ do
      rRaw <- peekElemOff pCh (ci + 1)
      let pRight = asInternal @k rRaw
      rm <- peek (getPtrOf (btiNumKeys (projectPtrs pRight)))
      whenM_ (rm `gt` fromIntegral btMinKeys) $ do
        kbC <- internalKeysBuf pChild
        kbR <- internalKeysBuf pRight
        let chC = internalChildrenPtr pChild
            chR = internalChildrenPtr pRight
        parentSep <- readBuf pKeys ci
        rightFirstK <- readBuf kbR 0
        rightFirstCh <- peekElemOff chR 0
        writeBuf kbC cm parentSep
        pokeElemOff chC rightFirstCh (cm + 1)
        writeBuf pKeys ci rightFirstK
        moveBuf kbR 0 1 (rm - 1)
        runDrain
          (drainForEach (\j -> peekElemOff chR (j + 1) >>= \cp -> pokeElemOff chR cp j))
          (rangeS 0 rm)
        poke (getPtrOf (btiNumKeys (projectPtrs pChild))) (cm + 1)
        poke (getPtrOf (btiNumKeys (projectPtrs pRight))) (rm - 1)
        writeRef doneRef true

    done2 <- readRef doneRef
    whenM_ (lnot done2) $ do
      let leftIdx = ifThenElse (ci `lt` pm) ci (ci - 1)
      lRaw <- peekElemOff pCh leftIdx
      rRaw <- peekElemOff pCh (leftIdx + 1)
      let pLeft = asInternal @k lRaw
          pRight = asInternal @k rRaw
          chL = internalChildrenPtr pLeft
          chR = internalChildrenPtr pRight
      lm <- peek (getPtrOf (btiNumKeys (projectPtrs pLeft)))
      rm <- peek (getPtrOf (btiNumKeys (projectPtrs pRight)))
      kbL <- internalKeysBuf pLeft
      kbR <- internalKeysBuf pRight
      pulledSep <- readBuf pKeys leftIdx
      writeBuf kbL lm pulledSep
      copyBuf kbL (lm + 1) kbR 0 rm
      runDrain
        (drainForEach (\j -> peekElemOff chR j >>= \cp -> pokeElemOff chL cp (lm + 1 + j)))
        (rangeS 0 (rm + 1))
      poke (getPtrOf (btiNumKeys (projectPtrs pLeft))) (lm + 1 + rm)
      freeStruct alloc pRight
      removeParentSepSet pParent leftIdx
      writeRef outRef lRaw
      writeRef mergedRef true

  (,) <$> readRef outRef <*> readRef mergedRef

delete ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  Elem e k ->
  m (e Bool)
delete alloc bs@(BTreeSet hdr) key = do
  let tf = projectPtrs hdr
  h0 <- peek (getPtrOf (btssHeight tf))
  removedRef <- newRef false
  whenM_ (h0 `gt` 0) $ do
    curRef <- peek (getPtrOf (btssRoot tf)) >>= newRef
    runDrain
      ( drainForEach $ \depth -> do
          curRaw <- readRef curRef
          let pCur = asInternal @k curRaw
          pm <- peek (getPtrOf (btiNumKeys (projectPtrs pCur)))
          pKeys <- internalKeysBuf pCur
          ci <- upperBoundBuf (ltKey @e @k) pKeys pm key
          let childIsLeaf = (depth + 1) `eq` (h0 - 1)
          nxtPRef <- newRef nullPtr
          mrgRef <- newRef false
          ifThenElseM_
            childIsLeaf
            (ensureSetLeafRich alloc bs pCur ci >>= \(p, b) -> writeRef nxtPRef p >> writeRef mrgRef b)
            (ensureSetInternalRich alloc pCur ci >>= \(p, b) -> writeRef nxtPRef p >> writeRef mrgRef b)
          nextRaw <- readRef nxtPRef
          didMerge <- readRef mrgRef
          whenM_ ((depth `eq` 0) `land` didMerge) $ do
            pmAfter <- peek (getPtrOf (btiNumKeys (projectPtrs pCur)))
            whenM_ (pmAfter `eq` 0) $ do
              poke (getPtrOf (btssRoot tf)) nextRaw
              curH <- peek (getPtrOf (btssHeight tf))
              poke (getPtrOf (btssHeight tf)) (curH - 1)
              freeStruct alloc pCur
          writeRef curRef nextRaw
      )
      (rangeS 0 (h0 - 1))

    pLeaf <- asSetLeaf @k <$> readRef curRef
    let lf = projectPtrs pLeaf
    n <- peek (getPtrOf (btslNumKeys lf))
    kb <- setLeafKeysBuf pLeaf
    pos <- lowerBoundBuf (ltKey @e @k) kb n key
    whenM_ (pos `lt` n) $ do
      candK <- readBuf kb pos
      whenM_ (eqKey @e @k candK key) $ do
        let tailCnt = n - pos - 1
        whenM_ (tailCnt `gt` 0) $
          moveBuf kb pos (pos + 1) tailCnt
        let newN = n - 1
        poke (getPtrOf (btslNumKeys lf)) newN
        curLen <- peek (getPtrOf (btssLen tf))
        poke (getPtrOf (btssLen tf)) (curLen - 1)
        writeRef removedRef true
        curH <- peek (getPtrOf (btssHeight tf))
        whenM_ ((curH `eq` 1) `land` (newN `eq` 0)) $ do
          freeStruct alloc pLeaf
          poke (getPtrOf (btssRoot tf)) nullPtr
          poke (getPtrOf (btssFirstLeaf tf)) nullPtr
          poke (getPtrOf (btssLastLeaf tf)) nullPtr
          poke (getPtrOf (btssHeight tf)) 0

  readRef removedRef

-- | Verify all B+Tree structural invariants on a host 'BTreeSet'.
checkInvariants ::
  forall k.
  (BTreeSetCtx IO Identity k) =>
  BTreeSet Identity k ->
  IO Bool
checkInvariants (BTreeSet (Identity rawHdr)) = do
  let tf = projectPtrs (Identity rawHdr)
  Identity rootP <- peek (getPtrOf (btssRoot tf))
  Identity firstLP <- peek (getPtrOf (btssFirstLeaf tf))
  Identity lastLP <- peek (getPtrOf (btssLastLeaf tf))
  Identity expectedLen <- peek (getPtrOf (btssLen tf))
  Identity h <- peek (getPtrOf (btssHeight tf))
  let nullW = runIdentity (nullPtr @Identity @Word8)
  if h == 0
    then pure (rootP == nullW && firstLP == nullW && lastLP == nullW && expectedLen == 0)
    else do
      (treeOk, treeLeaves, countedEntries) <- checkSubtree True 1 rootP Nothing Nothing h
      chainOk <- checkLeafChain firstLP lastLP nullW treeLeaves
      pure (treeOk && chainOk && countedEntries == expectedLen)
  where
    strictLt a b = runIdentity (ltKey @Identity @k a b)
    notLt a b = not (strictLt a b)

    checkSubtree isRoot depth rawNode loBound hiBound totalH
      | rawNode == runIdentity (nullPtr @Identity @Word8) = pure (False, [], 0)
      | depth == totalH = do
          let pLeaf = asSetLeaf @k (Identity rawNode)
              lf = projectPtrs pLeaf
          Identity n <- peek (getPtrOf (btslNumKeys lf))
          Identity isL <- peek (getPtrOf (btslIsLeaf lf))
          kb <- setLeafKeysBuf pLeaf
          ks <- mapM (readBuf kb . Identity) [0 .. n - 1]
          let countOk = if isRoot then n >= 1 && n <= btMaxKeys else n >= btMinKeys && n <= btMaxKeys
              sortedOk = and (zipWith strictLt ks (drop 1 ks))
              loOk = maybe True (\lo -> all (`notLt` lo) ks) loBound
              hiOk = maybe True (\hi -> all (`strictLt` hi) ks) hiBound
          pure (isL == 1 && countOk && sortedOk && loOk && hiOk, [rawNode], n)
      | otherwise = do
          let pInt = asInternal @k (Identity rawNode)
              inf = projectPtrs pInt
          Identity m <- peek (getPtrOf (btiNumKeys inf))
          Identity isL <- peek (getPtrOf (btiIsLeaf inf))
          kb <- internalKeysBuf pInt
          let chArr = internalChildrenPtr pInt
          seps <- mapM (readBuf kb . Identity) [0 .. m - 1]
          chs <- mapM (\i -> runIdentity <$> peekElemOff chArr (Identity i)) [0 .. m]
          let countOk = if isRoot then m >= 1 && m <= btMaxKeys else m >= btMinKeys && m <= btMaxKeys
              sortedOk = and (zipWith strictLt seps (drop 1 seps))
              childLo i = if i == 0 then loBound else Just (seps !! fromIntegral (i - 1))
              childHi i = if i == m then hiBound else Just (seps !! fromIntegral i)
          subRes <-
            mapM
              (\i -> checkSubtree False (depth + 1) (chs !! fromIntegral i) (childLo i) (childHi i) totalH)
              [0 .. m]
          let subsOk = all (\(ok, _, _) -> ok) subRes
              leaves = concatMap (\(_, ls, _) -> ls) subRes
              tot = sum [ cnt | (_, _, cnt) <- subRes ]
          pure (isL == 0 && countOk && sortedOk && subsOk, leaves, tot)

    checkLeafChain _ _ _ [] = pure False
    checkLeafChain firstP lastP nullW leaves = do
      fwdOk <- goFwd nullW firstP leaves
      pure (fwdOk && last leaves == lastP)
      where
        goFwd prevExpected cur [] = pure (cur == nullW && prevExpected == lastP)
        goFwd prevExpected cur (expected : rest)
          | cur /= expected = pure False
          | otherwise = do
              let lf = projectPtrs (asSetLeaf @k (Identity cur))
              Identity prv <- peek (getPtrOf (btslPrevLeaf lf))
              Identity nxt <- peek (getPtrOf (btslNextLeaf lf))
              if prv /= prevExpected
                then pure False
                else goFwd cur nxt rest

--------------------------------------------------------------------------------
-- Leaf-Walking Streams, Set-Algebra Merges, & Drains
--------------------------------------------------------------------------------

stream ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Stream m (Elem e k)
stream (BTreeSet hdr) = Stream $ Codensity $ \kOuter -> do
  firstRaw <- peek (getPtrOf (btssFirstLeaf (projectPtrs hdr)))
  leafRef <- newRef firstRaw
  idxRef <- newRef (0 :: e Int64)
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asSetLeaf @k curRaw
              lf = projectPtrs pLeaf
          n <- peek (getPtrOf (btslNumKeys lf))
          idx <- readRef idxRef
          ifThenElseM_
            (idx `lt` n)
            ( do
                kb <- setLeafKeysBuf pLeaf
                writeRef idxRef (idx + 1)
                readBuf kb idx >>= kStep . Yield
            )
            ( do
                nxtRaw <- peek (getPtrOf (btslNextLeaf lf))
                writeRef leafRef nxtRaw
                writeRef idxRef 0
                kStep Skip
            )
      )

reverseStream ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Stream m (Elem e k)
reverseStream (BTreeSet hdr) = Stream $ Codensity $ \kOuter -> do
  lastRaw <- peek (getPtrOf (btssLastLeaf (projectPtrs hdr)))
  leafRef <- newRef lastRaw
  idxRef <- newRef (-2 :: e Int64)
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asSetLeaf @k curRaw
              lf = projectPtrs pLeaf
          idx0 <- readRef idxRef
          ifThenElseM_
            (idx0 `eq` (-2))
            ( do
                n <- peek (getPtrOf (btslNumKeys lf))
                writeRef idxRef (n - 1)
                kStep Skip
            )
            ( ifThenElseM_
                (idx0 `ge` 0)
                ( do
                    kb <- setLeafKeysBuf pLeaf
                    writeRef idxRef (idx0 - 1)
                    readBuf kb idx0 >>= kStep . Yield
                )
                ( do
                    prvRaw <- peek (getPtrOf (btslPrevLeaf lf))
                    writeRef leafRef prvRaw
                    writeRef idxRef (-2)
                    kStep Skip
                )
            )
      )

rangeStream ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  BTreeSet e k ->
  Elem e k ->
  Elem e k ->
  Stream m (Elem e k)
rangeStream bs@(BTreeSet hdr) lo hi = Stream $ Codensity $ \kOuter -> do
  h <- peek (getPtrOf (btssHeight (projectPtrs hdr)))
  leafRef <- newRef (nullPtr @e @Word8)
  idxRef <- newRef (0 :: e Int64)
  whenM_ ((h `gt` 0) `land` lnot (ltKey @e @k hi lo)) $ do
    pStart <- descendToLeaf bs lo
    nStart <- peek (getPtrOf (btslNumKeys (projectPtrs pStart)))
    kbStart <- setLeafKeysBuf pStart
    posStart <- lowerBoundBuf (ltKey @e @k) kbStart nStart lo
    writeRef leafRef (toRawNode pStart)
    writeRef idxRef posStart
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asSetLeaf @k curRaw
              lf = projectPtrs pLeaf
          n <- peek (getPtrOf (btslNumKeys lf))
          idx <- readRef idxRef
          ifThenElseM_
            (idx `lt` n)
            ( do
                kb <- setLeafKeysBuf pLeaf
                candK <- readBuf kb idx
                ifThenElseM_
                  (ltKey @e @k hi candK)
                  (writeRef leafRef nullPtr >> kStep Stop)
                  (writeRef idxRef (idx + 1) >> kStep (Yield candK))
            )
            ( do
                nxtRaw <- peek (getPtrOf (btslNextLeaf lf))
                writeRef leafRef nxtRaw
                writeRef idxRef 0
                kStep Skip
            )
      )

unionS ::
  forall k m e.
  (BTreeSetCtx m e k, RefCell m e (Elem e k)) =>
  BTreeSet e k ->
  BTreeSet e k ->
  Stream m (Elem e k)
unionS a b =
  unionSortedByS (ltKey @e @k) (eqKey @e @k) const (stream a) (stream b)

intersectionS ::
  forall k m e.
  (BTreeSetCtx m e k, RefCell m e (Elem e k)) =>
  BTreeSet e k ->
  BTreeSet e k ->
  Stream m (Elem e k)
intersectionS a b =
  intersectSortedByS (ltKey @e @k) (eqKey @e @k) const (stream a) (stream b)

differenceS ::
  forall k m e.
  (BTreeSetCtx m e k, RefCell m e (Elem e k)) =>
  BTreeSet e k ->
  BTreeSet e k ->
  Stream m (Elem e k)
differenceS a b =
  differenceSortedByS (ltKey @e @k) (eqKey @e @k) (stream a) (stream b)

drainToBTreeSet ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  Drain m (Elem e k) (BTreeSet e k)
drainToBTreeSet alloc = Drain $ \s -> do
  bs <- new alloc
  runDrain (drainAppendBTreeSet alloc bs) s
  pure bs

fromStream ::
  forall k m e.
  (BTreeSetCtx m e k) =>
  Alloc e ->
  Stream m (Elem e k) ->
  m (BTreeSet e k)
fromStream alloc = runDrain (drainToBTreeSet alloc)

drainAppendBTreeSet ::
  (BTreeSetCtx m e k) =>
  Alloc e ->
  BTreeSet e k ->
  Drain m (Elem e k) ()
drainAppendBTreeSet alloc bs = drainForEach (\x -> insert alloc bs x >> pure ())

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCEval (BTreeSet JanusC k -> r) = BTreeSet Identity k -> JanusCEval r

type instance JanusCRetType (BTreeSet JanusC k -> r) = JanusCRetType r

instance (Typeable k, JanusCParam r) => JanusCParam (BTreeSet JanusC k -> r) where
  jcparam = mkPtrHandleJCParam BTreeSet

instance (Typeable k, JanusCEvaluate r) => JanusCEvaluate (BTreeSet JanusC k -> r) where
  jceval = mkPtrHandleJCEval unBTreeSet BTreeSet

type instance JanusCEval (JanusCM (BTreeSet JanusC k)) = IO (BTreeSet Identity k)

type instance JanusCRetType (JanusCM (BTreeSet JanusC k)) = BTreeSet Identity k

instance (Typeable k) => JanusCParam (JanusCM (BTreeSet JanusC k)) where
  jcparam = mkPtrHandleJCRetParam unBTreeSet

instance (Typeable k) => JanusCEvaluate (JanusCM (BTreeSet JanusC k)) where
  jceval = mkPtrHandleJCRetEval BTreeSet

type instance LinuxX86_64CEval (BTreeSet LinuxX86_64C k -> r) = BTreeSet Identity k -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (BTreeSet LinuxX86_64C k -> r) = LinuxX86_64BaseRet r

instance (Typeable k, LinuxX86_64Param r) => LinuxX86_64Param (BTreeSet LinuxX86_64C k -> r) where
  x86param = mkPtrHandleX86Param BTreeSet

instance (Typeable k, X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (BTreeSet LinuxX86_64C k -> r) where
  x86eval = mkPtrHandleX86EvalParam unBTreeSet BTreeSet

instance (Typeable k, X86DeferCont r) => X86DeferCall (BTreeSet LinuxX86_64C k -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unBTreeSet BTreeSet

instance (Typeable k, X86DeferCont r) => X86DeferCont (BTreeSet LinuxX86_64C k -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unBTreeSet BTreeSet

type instance LinuxX86_64CEval (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) = IO (BTreeSet Identity k)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) = BTreeSet Identity k

instance (Typeable k) => LinuxX86_64Param (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) where
  x86param = mkPtrHandleX86RetParam unBTreeSet

instance (Typeable k) => LinuxX86_64Evaluate (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) where
  x86eval = mkPtrHandleX86RetEval BTreeSet

instance (Typeable k) => X86DeferCall (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg BTreeSet

instance (Typeable k) => X86DeferCont (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont BTreeSet

instance (Typeable k, LinuxX86_64Param r, JanusCTH r) => JanusCTH (BTreeSet LinuxX86_64C k -> r) where
  type JanusCTHEval (BTreeSet LinuxX86_64C k -> r) = BTreeSet Identity k -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(BTreeSet Identity k)) : ps, retSpec)

instance (Typeable k) => JanusCTH (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) where
  type JanusCTHEval (LinuxX86_64CM (BTreeSet LinuxX86_64C k)) = IO (BTreeSet Identity k)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(BTreeSet Identity k)) (Proxy @(CStruct (BTreeSetSpine k))))
