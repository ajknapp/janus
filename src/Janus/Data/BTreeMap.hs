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

module Janus.Data.BTreeMap
  ( -- * Cache-Line B+Tree Node & Spine Types (`B = 8`, Max 15 Keys / 16 Children)
    BTreeOrder,
    BTreeMaxKeys,
    BTreeMaxChildren,
    BTreeInternal (..),
    BTreeLeaf (..),
    BTreeMapSpine (..),
    BTreeMap (..),
    BTreeMapCtx,

    -- * Lifecycle
    new,
    free,
    clear,

    -- * Queries & Structural Invariant Verification
    size,
    height,
    isEmpty,
    checkInvariants,

    -- * Ordered Lookup, Insertion, & Deletion
    insert,
    insertWith,
    lookup,
    withLookup,
    lookupGE,
    withLookupGE,
    lookupLE,
    withLookupLE,
    minEntry,
    maxEntry,
    member,
    delete,

    -- * Zero-Stack Doubly-Linked Leaf Streams, SPMD Kernels, & Drains
    toStream,
    reverseStream,
    keysS,
    valuesS,
    foreachValuesSPMD,
    foldValuesSPMD,
    rangeStream,
    unionWithS,
    intersectWithS,
    differenceS,
    drainToBTreeMap,
    fromStream,
    drainAppendBTreeMap,
    drainAppendBTreeMapWith,
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
    CmdAlloc,
    allocBytesAligned,
    allocStruct,
    freeBytes,
    freeStruct,
  )
import Janus.Backend.C.Raw
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpPtr (..), ExpSized (..))
import Janus.Command.Ref (RefCell, newRef, readRef, writeRef)
import Janus.Constraints (KnownJanusType)
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
import Janus.Data.FlatMap (InlineOrdLayoutCtx, OrdKey (..))
import Janus.Data.HKD
import Janus.Data.Layout
import Janus.Data.Sort (lowerBoundBuf, upperBoundBuf)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDM,
    SPMDTyped,
    uniform,
    withSPMD,
  )
import Prelude hiding (lookup)

-- | Branching factor @B = 8@: non-root nodes store @[7 .. 15]@ keys; internal nodes store @[8 .. 16]@ children.
type BTreeOrder = 8

type BTreeMaxKeys = 15

type BTreeMaxChildren = 16

btMinKeys :: Int64
btMinKeys = 7

btMaxKeys :: Int64
btMaxKeys = 15

-- | Internal routing node: stores @btiNumKeys@ sorted separator keys and @btiNumKeys + 1@ child pointers.
data BTreeInternal k f = BTreeInternal
  { btiNumKeys :: !(f Int64),
    btiIsLeaf :: !(f Int64),
    btiKeys :: !(f (CStruct (InlineBuf BTreeMaxKeys k))),
    btiChildren :: !(f (FixedArray BTreeMaxChildren (Ptr Word8)))
  }
  deriving stock (Generic)

instance
  ( Typeable k,
    JanusStruct (InlineBuf BTreeMaxKeys k)
  ) =>
  JanusStruct (BTreeInternal k)

-- | Leaf node: stores @btlNumKeys@ sorted keys & values inline, plus @prevLeaf@ / @nextLeaf@ links.
data BTreeLeaf k v f = BTreeLeaf
  { btlNumKeys :: !(f Int64),
    btlIsLeaf :: !(f Int64),
    btlPrevLeaf :: !(f (Ptr Word8)),
    btlNextLeaf :: !(f (Ptr Word8)),
    btlKeys :: !(f (CStruct (InlineBuf BTreeMaxKeys k))),
    btlVals :: !(f (CStruct (InlineBuf BTreeMaxKeys v)))
  }
  deriving stock (Generic)

instance
  ( Typeable k,
    Typeable v,
    JanusStruct (InlineBuf BTreeMaxKeys k),
    JanusStruct (InlineBuf BTreeMaxKeys v)
  ) =>
  JanusStruct (BTreeLeaf k v)

data BTreeMapSpine k v f = BTreeMapSpine
  { btmsRoot :: !(f (Ptr Word8)),
    btmsFirstLeaf :: !(f (Ptr Word8)),
    btmsLastLeaf :: !(f (Ptr Word8)),
    btmsLen :: !(f Int64),
    btmsHeight :: !(f Int64)
  }
  deriving stock (Generic)

instance (Typeable k, Typeable v) => JanusStruct (BTreeMapSpine k v)

newtype BTreeMap (e :: K.Type -> K.Type) k v = BTreeMap
  { unBTreeMap :: e (Ptr (CStruct (BTreeMapSpine k v)))
  }
  deriving stock (Generic)

deriving stock instance Eq (BTreeMap Identity k v)

deriving stock instance Show (BTreeMap Identity k v)

type BTreeMapCtx m e k v =
  ( InlineOrdLayoutCtx BTreeMaxKeys m e k,
    InlineLayoutCtx BTreeMaxKeys m e v
  )

asInternal ::
  forall k e.
  (ExpPtrCast e, KnownJanusType e Word8, KnownJanusType e (CStruct (BTreeInternal k))) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct (BTreeInternal k)))
asInternal p = fromVoidPtr (toVoidPtr p)

asLeaf ::
  forall k v e.
  (ExpPtrCast e, KnownJanusType e Word8, KnownJanusType e (CStruct (BTreeLeaf k v))) =>
  e (Ptr Word8) ->
  e (Ptr (CStruct (BTreeLeaf k v)))
asLeaf p = fromVoidPtr (toVoidPtr p)

toRawNode ::
  (ExpPtrCast e, KnownJanusType e a, KnownJanusType e Word8) =>
  e (Ptr a) ->
  e (Ptr Word8)
toRawNode p = fromVoidPtr (toVoidPtr p)

internalKeysBuf ::
  forall k m e.
  (InlineLayoutCtx BTreeMaxKeys m e k) =>
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
    KnownJanusType e Int64,
    KnownJanusType e (CStruct (InlineBuf BTreeMaxKeys k)),
    KnownJanusType e (Ptr Word8),
    KnownJanusType e (FixedArray BTreeMaxChildren (Ptr Word8))
  ) =>
  e (Ptr (CStruct (BTreeInternal k))) ->
  e (Ptr (Ptr Word8))
internalChildrenPtr pInt =
  fixedArrayElemPtr (getPtrOf (btiChildren (projectPtrs pInt)))

leafBufs ::
  forall k v m e.
  (InlineLayoutCtx BTreeMaxKeys m e k, InlineLayoutCtx BTreeMaxKeys m e v, Typeable k, Typeable v) =>
  e (Ptr (CStruct (BTreeLeaf k v))) ->
  m (Buf k e, Buf v e)
leafBufs pLeaf = do
  let lf = projectPtrs pLeaf
  kb <- inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btlKeys lf))
  vb <- inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btlVals lf))
  pure (kb, vb)

allocLeaf ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  m (e (Ptr (CStruct (BTreeLeaf k v))))
allocLeaf alloc = do
  p <- allocStruct @(CStruct (BTreeLeaf k v)) alloc
  let lf = projectPtrs p
  poke (getPtrOf (btlNumKeys lf)) 0
  poke (getPtrOf (btlIsLeaf lf)) 1
  poke (getPtrOf (btlPrevLeaf lf)) nullPtr
  poke (getPtrOf (btlNextLeaf lf)) nullPtr
  pure p

allocInternal ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  Proxy v ->
  m (e (Ptr (CStruct (BTreeInternal k))))
allocInternal alloc _ = do
  p <- allocStruct @(CStruct (BTreeInternal k)) alloc
  let inf = projectPtrs p
  poke (getPtrOf (btiNumKeys inf)) 0
  poke (getPtrOf (btiIsLeaf inf)) 0
  pure p

new ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  m (BTreeMap e k v)
new alloc = do
  hdr <- allocStruct @(CStruct (BTreeMapSpine k v)) alloc
  let f = projectPtrs hdr
  poke (getPtrOf (btmsRoot f)) nullPtr
  poke (getPtrOf (btmsFirstLeaf f)) nullPtr
  poke (getPtrOf (btmsLastLeaf f)) nullPtr
  poke (getPtrOf (btmsLen f)) 0
  poke (getPtrOf (btmsHeight f)) 0
  pure (BTreeMap hdr)

-- | Iteratively free all internal and leaf nodes in the tree without recursion!
-- Uses an explicit C-stack/register-free level-by-level traversal:
-- 1. Frees all leaf nodes by walking the @firstLeaf -> nextLeaf@ chain!
-- 2. Frees internal levels bottom-up or via a small dynamic/allocated work stack (or top-down path cursor)!
freeAllTreeNodes ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  m ()
freeAllTreeNodes alloc (BTreeMap hdr) = do
  let f = projectPtrs hdr
  h <- peek (getPtrOf (btmsHeight f))
  whenM_ (h `gt` 0) $ do
    -- 1. Walk and free all leaves via nextLeaf links using a prefetch-safe stream
    firstL <- peek (getPtrOf (btmsFirstLeaf f))
    let leavesS =
          unfoldWhileM firstL (\curL -> pure (ptrToWord64Expr curL `ne` 0)) $ \curL -> do
            let pLeaf = asLeaf @k @v curL
            nxtL <- peek (getPtrOf (btlNextLeaf (projectPtrs pLeaf)))
            pure (nxtL, pLeaf)
    runDrain (drainForEach (freeStruct alloc)) leavesS
    -- 2. If height > 1, iteratively free all internal nodes using an explicit post-order path stack
    -- allocated through the explicit `Alloc e` policy
    whenM_ (h `gt` 1) $ do
      let internalDepth = h - 1 -- levels 0 .. h - 2 are internal nodes
          nsBytes = internalDepth * sizeOf (Proxy @(Ptr Word8))
          isBytes = internalDepth * sizeOf (Proxy @Int64)
      nsVP <- allocBytesAligned alloc nsBytes (alignOf (Proxy @(Ptr Word8)))
      isVP <- allocBytesAligned alloc isBytes (alignOf (Proxy @Int64))
      let nsPtr = fromVoidPtr nsVP :: e (Ptr (Ptr Word8))
          isPtr = fromVoidPtr isVP :: e (Ptr Int64)
      rootRaw <- peek (getPtrOf (btmsRoot f))
      pokeElemOff nsPtr rootRaw 0
      pokeElemOff isPtr 0 0
      let postOrderStepS =
            unfoldWhileM (0 :: e Int64) (\sp -> pure (sp `ge` 0)) $ \sp -> do
              curRaw <- peekElemOff nsPtr sp
              let pInt = asInternal @k curRaw
                  inf = projectPtrs pInt
              m <- peek (getPtrOf (btiNumKeys inf))
              ci <- peekElemOff isPtr sp
              nextSpRef <- newRef (sp - 1)
              -- Only descend into children if the children are ALSO internal nodes (sp + 1 < internalDepth)
              ifThenElseM_
                ((sp + 1 `lt` internalDepth) `land` (ci `le` m))
                ( do
                    let chArr = internalChildrenPtr pInt
                    chRaw <- peekElemOff chArr ci
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
    poke (getPtrOf (btmsRoot f)) nullPtr
    poke (getPtrOf (btmsFirstLeaf f)) nullPtr
    poke (getPtrOf (btmsLastLeaf f)) nullPtr
    poke (getPtrOf (btmsLen f)) 0
    poke (getPtrOf (btmsHeight f)) 0

clear ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  m ()
clear = freeAllTreeNodes

free ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  m ()
free alloc bm@(BTreeMap hdr) = do
  freeAllTreeNodes alloc bm
  freeStruct alloc hdr

size :: (BTreeMapCtx m e k v) => BTreeMap e k v -> m (e Int64)
size (BTreeMap hdr) = peek (getPtrOf (btmsLen (projectPtrs hdr)))

height :: (BTreeMapCtx m e k v) => BTreeMap e k v -> m (e Int64)
height (BTreeMap hdr) = peek (getPtrOf (btmsHeight (projectPtrs hdr)))

isEmpty :: (BTreeMapCtx m e k v) => BTreeMap e k v -> m (e Bool)
isEmpty bm = (`eq` 0) <$> size bm

--------------------------------------------------------------------------------
-- Iterative Leaf Search
--------------------------------------------------------------------------------

-- | Descend from root to the unique leaf whose range covers @target@.
-- Caller requires @height > 0@.
descendToLeaf ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  m (e (Ptr (CStruct (BTreeLeaf k v))))
descendToLeaf (BTreeMap hdr) target = do
  let f = projectPtrs hdr
  h <- peek (getPtrOf (btmsHeight f))
  root0 <- peek (getPtrOf (btmsRoot f))
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
  pure (asLeaf @k @v leafRaw)

lookup ::
  forall k v s m e.
  (BTreeMapCtx m e k v, Elem e v ~ e s, KnownJanusType e s) =>
  e s ->
  BTreeMap e k v ->
  Elem e k ->
  m (e Bool, e s)
lookup defVal bm@(BTreeMap hdr) key = do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  okRef <- newRef false
  valRef <- newRef defVal
  whenM_ (h `gt` 0) $ do
    pLeaf <- descendToLeaf bm key
    n <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf)))
    (kb, vb) <- leafBufs pLeaf
    pos <- lowerBoundBuf (ltKey @e @k) kb n key
    whenM_ (pos `lt` n) $ do
      candK <- readBuf kb pos
      whenM_ (eqKey @e @k candK key) $ do
        readBuf vb pos >>= writeRef valRef
        writeRef okRef true
  (,) <$> readRef okRef <*> readRef valRef

withLookup ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  m () ->
  (Elem e v -> m ()) ->
  m ()
withLookup bm@(BTreeMap hdr) key onMiss onHit = do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  ifThenElseM_
    (h `le` 0)
    onMiss
    ( do
        pLeaf <- descendToLeaf bm key
        n <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf)))
        (kb, vb) <- leafBufs pLeaf
        pos <- lowerBoundBuf (ltKey @e @k) kb n key
        hitRef <- newRef false
        whenM_ (pos `lt` n) $ do
          candK <- readBuf kb pos
          whenM_ (eqKey @e @k candK key) $ do
            writeRef hitRef true
            readBuf vb pos >>= onHit
        hit <- readRef hitRef
        whenM_ (lnot hit) onMiss
    )

member ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  m (e Bool)
member bm@(BTreeMap hdr) key = do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  okRef <- newRef false
  whenM_ (h `gt` 0) $ do
    pLeaf <- descendToLeaf bm key
    n <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf)))
    (kb, _) <- leafBufs pLeaf
    pos <- lowerBoundBuf (ltKey @e @k) kb n key
    whenM_ (pos `lt` n) $ do
      candK <- readBuf kb pos
      writeRef okRef (eqKey @e @k candK key)
  readRef okRef

withLookupGE ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withLookupGE bm@(BTreeMap hdr) target onMiss onHit = do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  ifThenElseM_
    (h `le` 0)
    onMiss
    ( do
        pLeaf0 <- descendToLeaf bm target
        n0 <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf0)))
        (kb0, vb0) <- leafBufs pLeaf0
        pos0 <- lowerBoundBuf (ltKey @e @k) kb0 n0 target
        ifThenElseM_
          (pos0 `lt` n0)
          ( do
              ko <- readBuf kb0 pos0
              vo <- readBuf vb0 pos0
              onHit ko vo
          )
          ( do
              -- First >= target is entry 0 of nextLeaf (if non-null)
              nxtRaw <- peek (getPtrOf (btlNextLeaf (projectPtrs pLeaf0)))
              ifThenElseM_
                (ptrToWord64Expr nxtRaw `eq` 0)
                onMiss
                ( do
                    let pNxt = asLeaf @k @v nxtRaw
                    (kb1, vb1) <- leafBufs pNxt
                    ko <- readBuf kb1 0
                    vo <- readBuf vb1 0
                    onHit ko vo
                )
          )
    )

lookupGE ::
  forall k v ks vs m e.
  (BTreeMapCtx m e k v, Elem e k ~ e ks, Elem e v ~ e vs, KnownJanusType e ks, KnownJanusType e vs) =>
  e ks ->
  e vs ->
  BTreeMap e k v ->
  e ks ->
  m (e Bool, e ks, e vs)
lookupGE defK defV bm target = do
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  withLookupGE
    bm
    target
    (pure ())
    (\ko vo -> writeRef okRef true >> writeRef kRef ko >> writeRef vRef vo)
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

withLookupLE ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
withLookupLE bm@(BTreeMap hdr) target onMiss onHit = do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  ifThenElseM_
    (h `le` 0)
    onMiss
    ( do
        pLeaf0 <- descendToLeaf bm target
        n0 <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf0)))
        (kb0, vb0) <- leafBufs pLeaf0
        ub <- upperBoundBuf (ltKey @e @k) kb0 n0 target
        ifThenElseM_
          (ub `gt` 0)
          ( do
              let idx = ub - 1
              ko <- readBuf kb0 idx
              vo <- readBuf vb0 idx
              onHit ko vo
          )
          ( do
              prvRaw <- peek (getPtrOf (btlPrevLeaf (projectPtrs pLeaf0)))
              ifThenElseM_
                (ptrToWord64Expr prvRaw `eq` 0)
                onMiss
                ( do
                    let pPrv = asLeaf @k @v prvRaw
                    nPrv <- peek (getPtrOf (btlNumKeys (projectPtrs pPrv)))
                    (kb1, vb1) <- leafBufs pPrv
                    ko <- readBuf kb1 (nPrv - 1)
                    vo <- readBuf vb1 (nPrv - 1)
                    onHit ko vo
                )
          )
    )

lookupLE ::
  forall k v ks vs m e.
  (BTreeMapCtx m e k v, Elem e k ~ e ks, Elem e v ~ e vs, KnownJanusType e ks, KnownJanusType e vs) =>
  e ks ->
  e vs ->
  BTreeMap e k v ->
  e ks ->
  m (e Bool, e ks, e vs)
lookupLE defK defV bm target = do
  okRef <- newRef false
  kRef <- newRef defK
  vRef <- newRef defV
  withLookupLE
    bm
    target
    (pure ())
    (\ko vo -> writeRef okRef true >> writeRef kRef ko >> writeRef vRef vo)
  (,,) <$> readRef okRef <*> readRef kRef <*> readRef vRef

minEntry ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
minEntry (BTreeMap hdr) onEmpty onHit = do
  firstRaw <- peek (getPtrOf (btmsFirstLeaf (projectPtrs hdr)))
  ifThenElseM_
    (ptrToWord64Expr firstRaw `eq` 0)
    onEmpty
    ( do
        let pLeaf = asLeaf @k @v firstRaw
        (kb, vb) <- leafBufs pLeaf
        ko <- readBuf kb 0
        vo <- readBuf vb 0
        onHit ko vo
    )

maxEntry ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  m () ->
  (Elem e k -> Elem e v -> m ()) ->
  m ()
maxEntry (BTreeMap hdr) onEmpty onHit = do
  lastRaw <- peek (getPtrOf (btmsLastLeaf (projectPtrs hdr)))
  ifThenElseM_
    (ptrToWord64Expr lastRaw `eq` 0)
    onEmpty
    ( do
        let pLeaf = asLeaf @k @v lastRaw
        n <- peek (getPtrOf (btlNumKeys (projectPtrs pLeaf)))
        (kb, vb) <- leafBufs pLeaf
        ko <- readBuf kb (n - 1)
        vo <- readBuf vb (n - 1)
        onHit ko vo
    )

--------------------------------------------------------------------------------
-- Top-Down Proactive Split Insertion
--------------------------------------------------------------------------------

-- | Split full leaf child @ci@ (@15@ keys -> @7@ left + @8@ right) of non-full parent @pParent@.
splitLeafChild ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  e (Ptr (CStruct (BTreeLeaf k v))) ->
  m ()
splitLeafChild alloc (BTreeMap hdr) pParent ci pLeft = do
  pRight <- allocLeaf @k @v alloc
  let lfL = projectPtrs pLeft
      lfR = projectPtrs pRight
      pf = projectPtrs pParent
      tf = projectPtrs hdr
  (kbL, vbL) <- leafBufs pLeft
  (kbR, vbR) <- leafBufs pRight

  -- Copy [7 .. 14] (8 entries) into right leaf
  copyBuf kbR 0 kbL 7 8
  copyBuf vbR 0 vbL 7 8
  poke (getPtrOf (btlNumKeys lfL)) 7
  poke (getPtrOf (btlNumKeys lfR)) 8

  -- Splice pRight into doubly-linked leaf list after pLeft
  oldNext <- peek (getPtrOf (btlNextLeaf lfL))
  let rawLeft = toRawNode pLeft
      rawRight = toRawNode pRight
  poke (getPtrOf (btlPrevLeaf lfR)) rawLeft
  poke (getPtrOf (btlNextLeaf lfR)) oldNext
  poke (getPtrOf (btlNextLeaf lfL)) rawRight
  ifThenElseM_
    (ptrToWord64Expr oldNext `ne` 0)
    (poke (getPtrOf (btlPrevLeaf (projectPtrs (asLeaf @k @v oldNext)))) rawRight)
    (poke (getPtrOf (btmsLastLeaf tf)) rawRight)

  -- Copy-up sepKey = right.keys[0] into parent at ci
  sepKey <- readBuf kbR 0
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  let pCh = internalChildrenPtr pParent
      shiftKeys = pm - ci
  whenM_ (shiftKeys `gt` 0) $ do
    moveBuf pKeys (ci + 1) ci shiftKeys
    -- Shift children [ci + 1 .. pm] right to [ci + 2 .. pm + 1] in reverse
    runDrain
      ( drainForEach $ \j -> do
          ch <- peekElemOff pCh j
          pokeElemOff pCh ch (j + 1)
      )
      (reverseRangeS (ci + 1) (pm + 1))
  writeBuf pKeys ci sepKey
  pokeElemOff pCh rawRight (ci + 1)
  poke (getPtrOf (btiNumKeys pf)) (pm + 1)

-- | Split full internal child @ci@ (@15@ keys -> @7@ left + push-up @K[7]@ + @7@ right).
splitInternalChild ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  Proxy v ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  m ()
splitInternalChild alloc pv pParent ci pLeft = do
  pRight <- allocInternal @k @v alloc pv
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
    ( drainForEach $ \j -> do
        cPtr <- peekElemOff chL (8 + j)
        pokeElemOff chR cPtr j
    )
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

insertIntoNonFullLeaf ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  (Elem e v -> Elem e v -> Elem e v) ->
  e (Ptr (CStruct (BTreeLeaf k v))) ->
  Elem e k ->
  Elem e v ->
  m ()
insertIntoNonFullLeaf (BTreeMap hdr) combine pLeaf key val = do
  let lf = projectPtrs pLeaf
      tf = projectPtrs hdr
  n <- peek (getPtrOf (btlNumKeys lf))
  (kb, vb) <- leafBufs pLeaf
  pos <- lowerBoundBuf (ltKey @e @k) kb n key
  existsRef <- newRef false
  whenM_ (pos `lt` n) $ do
    candK <- readBuf kb pos
    writeRef existsRef (eqKey @e @k candK key)
  exists <- readRef existsRef
  ifThenElseM_
    exists
    ( do
        oldV <- readBuf vb pos
        writeBuf vb pos (combine oldV val)
    )
    ( do
        let tailCnt = n - pos
        whenM_ (tailCnt `gt` 0) $ do
          moveBuf kb (pos + 1) pos tailCnt
          moveBuf vb (pos + 1) pos tailCnt
        writeBuf kb pos key
        writeBuf vb pos val
        poke (getPtrOf (btlNumKeys lf)) (n + 1)
        curLen <- peek (getPtrOf (btmsLen tf))
        poke (getPtrOf (btmsLen tf)) (curLen + 1)
    )

insert ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  Elem e k ->
  Elem e v ->
  m ()
insert alloc bm key val = insertWith alloc bm (\_ newV -> newV) key val

-- | Single-pass top-down iterative B+Tree insertion.
insertWith ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  (Elem e v -> Elem e v -> Elem e v) ->
  Elem e k ->
  Elem e v ->
  m ()
insertWith alloc bm@(BTreeMap hdr) combine key val = do
  let tf = projectPtrs hdr
  h0 <- peek (getPtrOf (btmsHeight tf))
  ifThenElseM_
    (h0 `eq` 0)
    ( do
        pLeaf <- allocLeaf @k @v alloc
        let rawL = toRawNode pLeaf
        poke (getPtrOf (btmsRoot tf)) rawL
        poke (getPtrOf (btmsFirstLeaf tf)) rawL
        poke (getPtrOf (btmsLastLeaf tf)) rawL
        poke (getPtrOf (btmsHeight tf)) 1
        insertIntoNonFullLeaf bm combine pLeaf key val
    )
    ( do
        -- Check if root is full (15 keys); if so, grow height by 1 first!
        root0 <- peek (getPtrOf (btmsRoot tf))
        rootKeys0 <-
          ifThenElseInt64
            (h0 `eq` 1)
            (peek (getPtrOf (btlNumKeys (projectPtrs (asLeaf @k @v root0)))))
            (peek (getPtrOf (btiNumKeys (projectPtrs (asInternal @k root0)))))
        whenM_ (rootKeys0 `eq` fromIntegral btMaxKeys) $ do
          newRoot <- allocInternal @k @v alloc (Proxy @v)
          pokeElemOff (internalChildrenPtr newRoot) root0 0
          poke (getPtrOf (btmsRoot tf)) (toRawNode newRoot)
          poke (getPtrOf (btmsHeight tf)) (h0 + 1)
          ifThenElseM_
            (h0 `eq` 1)
            (splitLeafChild alloc bm newRoot 0 (asLeaf @k @v root0))
            (splitInternalChild alloc (Proxy @v) newRoot 0 (asInternal @k root0))

        -- Now root is guaranteed non-full (< 15 keys). Descend iteratively splitting any full child!
        hNow <- peek (getPtrOf (btmsHeight tf))
        curRef <- peek (getPtrOf (btmsRoot tf)) >>= newRef
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
                    let pChildL = asLeaf @k @v ch0
                    cn <- peek (getPtrOf (btlNumKeys (projectPtrs pChildL)))
                    ifThenElseM_
                      (cn `eq` fromIntegral btMaxKeys)
                      ( do
                          splitLeafChild alloc bm pCur ci0 pChildL
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
                          splitInternalChild alloc (Proxy @v) pCur ci0 pChildI
                          sepK <- readBuf pKeys ci0
                          let ciFinal = ifThenElse (ltKey @e @k key sepK) ci0 (ci0 + 1)
                          peekElemOff (internalChildrenPtr pCur) ciFinal >>= writeRef curRef
                      )
                      (writeRef curRef ch0)
                )
          )
          (rangeS 0 (hNow - 1))
        pTargetLeaf <- asLeaf @k @v <$> readRef curRef
        insertIntoNonFullLeaf bm combine pTargetLeaf key val
    )

ifThenElseInt64 ::
  (CmdAlloc m e) =>
  e Bool ->
  m (e Int64) ->
  m (e Int64) ->
  m (e Int64)
ifThenElseInt64 cond tAct fAct = do
  r <- newRef 0
  ifThenElseM_ cond (tAct >>= writeRef r) (fAct >>= writeRef r)
  readRef r

--------------------------------------------------------------------------------
-- Top-Down Proactive Rebalance Deletion
--------------------------------------------------------------------------------

removeParentSepAndRightChild ::
  forall k m e.
  (InlineLayoutCtx BTreeMaxKeys m e k) =>
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m ()
removeParentSepAndRightChild pParent leftIdx = do
  let pf = projectPtrs pParent
      pCh = internalChildrenPtr pParent
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  let tailKeys = pm - leftIdx - 1
  whenM_ (tailKeys `gt` 0) $ do
    moveBuf pKeys leftIdx (leftIdx + 1) tailKeys
    runDrain
      ( drainForEach $ \j -> do
          ch <- peekElemOff pCh (j + 1)
          pokeElemOff pCh ch j
      )
      (rangeS (leftIdx + 1) pm)
  poke (getPtrOf (btiNumKeys pf)) (pm - 1)

-- | Ensure leaf child @ci@ of @pParent@ has @>= 8@ keys; returns the raw pointer to the child leaf
-- covering @target@ (which may be the merged left leaf if merged with left sibling).
ensureLeafChildRich ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m (e (Ptr Word8), e Bool)
ensureLeafChildRich alloc (BTreeMap hdr) pParent ci = do
  let pf = projectPtrs pParent
      pCh = internalChildrenPtr pParent
      tf = projectPtrs hdr
  pm <- peek (getPtrOf (btiNumKeys pf))
  pKeys <- internalKeysBuf pParent
  chRaw <- peekElemOff pCh ci
  let pChild = asLeaf @k @v chRaw
  cn <- peek (getPtrOf (btlNumKeys (projectPtrs pChild)))
  outRef <- newRef chRaw
  mergedRef <- newRef false

  whenM_ (cn `le` fromIntegral btMinKeys) $ do
    doneRef <- newRef false
    -- Try borrow from left sibling
    whenM_ (ci `gt` 0) $ do
      lRaw <- peekElemOff pCh (ci - 1)
      let pLeft = asLeaf @k @v lRaw
      ln <- peek (getPtrOf (btlNumKeys (projectPtrs pLeft)))
      whenM_ (ln `gt` fromIntegral btMinKeys) $ do
        (kbL, vbL) <- leafBufs pLeft
        (kbC, vbC) <- leafBufs pChild
        lk <- readBuf kbL (ln - 1)
        lv <- readBuf vbL (ln - 1)
        moveBuf kbC 1 0 cn
        moveBuf vbC 1 0 cn
        writeBuf kbC 0 lk
        writeBuf vbC 0 lv
        poke (getPtrOf (btlNumKeys (projectPtrs pLeft))) (ln - 1)
        poke (getPtrOf (btlNumKeys (projectPtrs pChild))) (cn + 1)
        writeBuf pKeys (ci - 1) lk
        writeRef doneRef true

    -- Try borrow from right sibling
    done1 <- readRef doneRef
    whenM_ (lnot done1 `land` (ci `lt` pm)) $ do
      rRaw <- peekElemOff pCh (ci + 1)
      let pRight = asLeaf @k @v rRaw
      rn <- peek (getPtrOf (btlNumKeys (projectPtrs pRight)))
      whenM_ (rn `gt` fromIntegral btMinKeys) $ do
        (kbC, vbC) <- leafBufs pChild
        (kbR, vbR) <- leafBufs pRight
        rk0 <- readBuf kbR 0
        rv0 <- readBuf vbR 0
        writeBuf kbC cn rk0
        writeBuf vbC cn rv0
        moveBuf kbR 0 1 (rn - 1)
        moveBuf vbR 0 1 (rn - 1)
        poke (getPtrOf (btlNumKeys (projectPtrs pChild))) (cn + 1)
        poke (getPtrOf (btlNumKeys (projectPtrs pRight))) (rn - 1)
        newR0 <- readBuf kbR 0
        writeBuf pKeys ci newR0
        writeRef doneRef true

    -- Otherwise merge two 7-key leaves into a 14-key leaf
    done2 <- readRef doneRef
    whenM_ (lnot done2) $ do
      let leftIdx = ifThenElse (ci `lt` pm) ci (ci - 1)
      lRaw <- peekElemOff pCh leftIdx
      rRaw <- peekElemOff pCh (leftIdx + 1)
      let pLeft = asLeaf @k @v lRaw
          pRight = asLeaf @k @v rRaw
          lfL = projectPtrs pLeft
          lfR = projectPtrs pRight
      ln <- peek (getPtrOf (btlNumKeys lfL))
      rn <- peek (getPtrOf (btlNumKeys lfR))
      (kbL, vbL) <- leafBufs pLeft
      (kbR, vbR) <- leafBufs pRight
      copyBuf kbL ln kbR 0 rn
      copyBuf vbL ln vbR 0 rn
      poke (getPtrOf (btlNumKeys lfL)) (ln + rn)

      -- Unlink pRight from leaf doubly-linked list
      nxtR <- peek (getPtrOf (btlNextLeaf lfR))
      poke (getPtrOf (btlNextLeaf lfL)) nxtR
      ifThenElseM_
        (ptrToWord64Expr nxtR `ne` 0)
        (poke (getPtrOf (btlPrevLeaf (projectPtrs (asLeaf @k @v nxtR)))) lRaw)
        (poke (getPtrOf (btmsLastLeaf tf)) lRaw)

      freeStruct alloc pRight
      removeParentSepAndRightChild pParent leftIdx
      writeRef outRef lRaw
      writeRef mergedRef true

  (,) <$> readRef outRef <*> readRef mergedRef

-- | Ensure internal child @ci@ of @pParent@ has @>= 8@ keys; returns @(childRawPtr, didMerge)@.
ensureInternalChildRich ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  Proxy v ->
  e (Ptr (CStruct (BTreeInternal k))) ->
  e Int64 ->
  m (e (Ptr Word8), e Bool)
ensureInternalChildRich alloc _ pParent ci = do
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
    -- Try borrow from left internal sibling
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
        runDrain
          ( drainForEach $ \j -> do
              ptrJ <- peekElemOff chC j
              pokeElemOff chC ptrJ (j + 1)
          )
          (reverseRangeS 0 (cm + 1))
        writeBuf kbC 0 parentSep
        pokeElemOff chC leftLastCh 0
        writeBuf pKeys (ci - 1) leftLastK
        poke (getPtrOf (btiNumKeys (projectPtrs pLeft))) (lm - 1)
        poke (getPtrOf (btiNumKeys (projectPtrs pChild))) (cm + 1)
        writeRef doneRef true

    -- Try borrow from right internal sibling
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
          ( drainForEach $ \j -> do
              ptrJ <- peekElemOff chR (j + 1)
              pokeElemOff chR ptrJ j
          )
          (rangeS 0 rm)
        poke (getPtrOf (btiNumKeys (projectPtrs pChild))) (cm + 1)
        poke (getPtrOf (btiNumKeys (projectPtrs pRight))) (rm - 1)
        writeRef doneRef true

    -- Otherwise merge two 7-key internals + 1 pulled-down parent separator = 15 keys
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
        ( drainForEach $ \j -> do
            ptrJ <- peekElemOff chR j
            pokeElemOff chL ptrJ (lm + 1 + j)
        )
        (rangeS 0 (rm + 1))
      poke (getPtrOf (btiNumKeys (projectPtrs pLeft))) (lm + 1 + rm)

      freeStruct alloc pRight
      removeParentSepAndRightChild pParent leftIdx
      writeRef outRef lRaw
      writeRef mergedRef true

  (,) <$> readRef outRef <*> readRef mergedRef

-- | Single-pass top-down iterative B+Tree deletion. Returns @true@ iff @key@ was found and removed.
delete ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  Elem e k ->
  m (e Bool)
delete alloc bm@(BTreeMap hdr) key = do
  let tf = projectPtrs hdr
  h0 <- peek (getPtrOf (btmsHeight tf))
  removedRef <- newRef false
  whenM_ (h0 `gt` 0) $ do
    curRef <- peek (getPtrOf (btmsRoot tf)) >>= newRef
    -- Descend through internal levels [0 .. h0 - 2] proactively rebalancing
    runDrain
      ( drainForEach $ \depth -> do
          curRaw <- readRef curRef
          let pCur = asInternal @k curRaw
          pm <- peek (getPtrOf (btiNumKeys (projectPtrs pCur)))
          pKeys <- internalKeysBuf pCur
          ci <- upperBoundBuf (ltKey @e @k) pKeys pm key
          let childIsLeaf = (depth + 1) `eq` (h0 - 1)
          (nextRaw, didMerge) <-
            ifThenElsePair
              childIsLeaf
              (ensureLeafChildRich alloc bm pCur ci)
              (ensureInternalChildRich alloc (Proxy @v) pCur ci)
          -- If pCur was the root (depth == 0) and merging emptied pCur (numKeys == 0), collapse tree height!
          whenM_ ((depth `eq` 0) `land` didMerge) $ do
            pmAfter <- peek (getPtrOf (btiNumKeys (projectPtrs pCur)))
            whenM_ (pmAfter `eq` 0) $ do
              poke (getPtrOf (btmsRoot tf)) nextRaw
              curH <- peek (getPtrOf (btmsHeight tf))
              poke (getPtrOf (btmsHeight tf)) (curH - 1)
              freeStruct alloc pCur
          writeRef curRef nextRaw
      )
      (rangeS 0 (h0 - 1))

    -- Delete from target leaf
    pLeaf <- asLeaf @k @v <$> readRef curRef
    let lf = projectPtrs pLeaf
    n <- peek (getPtrOf (btlNumKeys lf))
    (kb, vb) <- leafBufs pLeaf
    pos <- lowerBoundBuf (ltKey @e @k) kb n key
    whenM_ (pos `lt` n) $ do
      candK <- readBuf kb pos
      whenM_ (eqKey @e @k candK key) $ do
        let tailCnt = n - pos - 1
        whenM_ (tailCnt `gt` 0) $ do
          moveBuf kb pos (pos + 1) tailCnt
          moveBuf vb pos (pos + 1) tailCnt
        let newN = n - 1
        poke (getPtrOf (btlNumKeys lf)) newN
        curLen <- peek (getPtrOf (btmsLen tf))
        poke (getPtrOf (btmsLen tf)) (curLen - 1)
        writeRef removedRef true

        -- If the root leaf itself just became empty, reset tree to empty!
        curH <- peek (getPtrOf (btmsHeight tf))
        whenM_ ((curH `eq` 1) `land` (newN `eq` 0)) $ do
          freeStruct alloc pLeaf
          poke (getPtrOf (btmsRoot tf)) nullPtr
          poke (getPtrOf (btmsFirstLeaf tf)) nullPtr
          poke (getPtrOf (btmsLastLeaf tf)) nullPtr
          poke (getPtrOf (btmsHeight tf)) 0

  readRef removedRef

ifThenElsePair ::
  (CmdAlloc m e) =>
  e Bool ->
  m (e (Ptr Word8), e Bool) ->
  m (e (Ptr Word8), e Bool) ->
  m (e (Ptr Word8), e Bool)
ifThenElsePair cond tAct fAct = do
  pRef <- newRef nullPtr
  bRef <- newRef false
  ifThenElseM_
    cond
    (tAct >>= \(p, b) -> writeRef pRef p >> writeRef bRef b)
    (fAct >>= \(p, b) -> writeRef pRef p >> writeRef bRef b)
  (,) <$> readRef pRef <*> readRef bRef

--------------------------------------------------------------------------------
-- Structural Invariant Checker (For Exhaustive Property Testing)
--------------------------------------------------------------------------------

-- | Verify all B+Tree structural invariants on a host 'BTreeMap':
-- 1. Occupancy bounds (@1 <= root <= 15@, @7 <= nonRoot <= 15@)
-- 2. Strict key ordering inside every internal and leaf node
-- 3. Separator lower/upper bounds across all children
-- 4. Uniform leaf depth == 'height'
-- 5. Forward & backward doubly-linked leaf chain matches in-order leaf sequence & total 'size'.
checkInvariants ::
  forall k v.
  (BTreeMapCtx IO Identity k v) =>
  BTreeMap Identity k v ->
  IO Bool
checkInvariants (BTreeMap (Identity rawHdr)) = do
  let tf = projectPtrs (Identity rawHdr)
  Identity rootP <- peek (getPtrOf (btmsRoot tf))
  Identity firstLP <- peek (getPtrOf (btmsFirstLeaf tf))
  Identity lastLP <- peek (getPtrOf (btmsLastLeaf tf))
  Identity expectedLen <- peek (getPtrOf (btmsLen tf))
  Identity h <- peek (getPtrOf (btmsHeight tf))
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

    checkSubtree ::
      Bool ->
      Int64 ->
      Ptr Word8 ->
      Maybe (Elem Identity k) ->
      Maybe (Elem Identity k) ->
      Int64 ->
      IO (Bool, [Ptr Word8], Int64)
    checkSubtree isRoot depth rawNode loBound hiBound totalH
      | rawNode == runIdentity (nullPtr @Identity @Word8) = pure (False, [], 0)
      | depth == totalH = do
          let pLeaf = asLeaf @k @v (Identity rawNode)
              lf = projectPtrs pLeaf
          Identity n <- peek (getPtrOf (btlNumKeys lf))
          Identity isL <- peek (getPtrOf (btlIsLeaf lf))
          (kb, _) <- leafBufs pLeaf
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

    checkLeafChain :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> [Ptr Word8] -> IO Bool
    checkLeafChain _ _ _ [] = pure False
    checkLeafChain firstP lastP nullW leaves = do
      fwdOk <- goFwd nullW firstP leaves
      pure (fwdOk && last leaves == lastP)
      where
        goFwd prevExpected cur [] = pure (cur == nullW && prevExpected == lastP)
        goFwd prevExpected cur (expected : rest)
          | cur /= expected = pure False
          | otherwise = do
              let lf = projectPtrs (asLeaf @k @v (Identity cur))
              Identity prv <- peek (getPtrOf (btlPrevLeaf lf))
              Identity nxt <- peek (getPtrOf (btlNextLeaf lf))
              if prv /= prevExpected
                then pure False
                else goFwd cur nxt rest

--------------------------------------------------------------------------------
-- Two-Register Zero-Stack Leaf-Walking Streams & Drains
--------------------------------------------------------------------------------

-- | Ascending in-order 'Stream' walking the doubly-linked leaf chain in 2 scalar registers
-- (@curLeaf@, @slotIdx@) with ZERO stack usage!
toStream ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Stream m (Elem e k, Elem e v)
toStream (BTreeMap hdr) = Stream $ Codensity $ \kOuter -> do
  firstRaw <- peek (getPtrOf (btmsFirstLeaf (projectPtrs hdr)))
  leafRef <- newRef firstRaw
  idxRef <- newRef (0 :: e Int64)
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asLeaf @k @v curRaw
              lf = projectPtrs pLeaf
          n <- peek (getPtrOf (btlNumKeys lf))
          idx <- readRef idxRef
          ifThenElseM_
            (idx `lt` n)
            ( do
                (kb, vb) <- leafBufs pLeaf
                writeRef idxRef (idx + 1)
                kv <- (,) <$> readBuf kb idx <*> readBuf vb idx
                kStep (Yield kv)
            )
            ( do
                nxtRaw <- peek (getPtrOf (btlNextLeaf lf))
                writeRef leafRef nxtRaw
                writeRef idxRef 0
                kStep Skip
            )
      )

-- | Descending 'Stream' walking @lastLeaf -> prevLeaf@ in 2 scalar registers.
reverseStream ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Stream m (Elem e k, Elem e v)
reverseStream (BTreeMap hdr) = Stream $ Codensity $ \kOuter -> do
  lastRaw <- peek (getPtrOf (btmsLastLeaf (projectPtrs hdr)))
  leafRef <- newRef lastRaw
  idxRef <- newRef (-2 :: e Int64) -- -2 = needs initialization for current leaf
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asLeaf @k @v curRaw
              lf = projectPtrs pLeaf
          idx0 <- readRef idxRef
          ifThenElseM_
            (idx0 `eq` (-2))
            ( do
                n <- peek (getPtrOf (btlNumKeys lf))
                writeRef idxRef (n - 1)
                kStep Skip
            )
            ( ifThenElseM_
                (idx0 `ge` 0)
                ( do
                    (kb, vb) <- leafBufs pLeaf
                    writeRef idxRef (idx0 - 1)
                    kv <- (,) <$> readBuf kb idx0 <*> readBuf vb idx0
                    kStep (Yield kv)
                )
                ( do
                    prvRaw <- peek (getPtrOf (btlPrevLeaf lf))
                    writeRef leafRef prvRaw
                    writeRef idxRef (-2)
                    kStep Skip
                )
            )
      )

keysS :: (BTreeMapCtx m e k v) => BTreeMap e k v -> Stream m (Elem e k)
keysS bm = fst <$> toStream bm

valuesS :: (BTreeMapCtx m e k v) => BTreeMap e k v -> Stream m (Elem e v)
valuesS bm = snd <$> toStream bm

-- | Leaf-by-leaf unit-stride in-place SIMD map over sorted values across the doubly-linked leaf chain,
-- passing the running 0-based global sorted index @(globalOffset + localIdx)@ to @step@.
foreachValuesSPMD ::
  forall w k v m e.
  (BTreeMapCtx m e k v, LayoutUnitSPMD w m e v) =>
  Proxy w ->
  BTreeMap e k v ->
  (SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (Elem (SPMD w e) v)) ->
  m ()
foreachValuesSPMD pw (BTreeMap hdr) step = do
  firstRaw <- peek (getPtrOf (btmsFirstLeaf (projectPtrs hdr)))
  runDrain drain_ $
    unfoldWhileM (firstRaw, 0 :: e Int64) (\(curL, _) -> pure (ptrToWord64Expr curL `ne` 0)) $
      \(curL, globalOff) -> do
        let pLeaf = asLeaf @k @v curL
            lf = projectPtrs pLeaf
        leafLen <- peek (getPtrOf (btlNumKeys lf))
        nxtL <- peek (getPtrOf (btlNextLeaf lf))
        vb <- inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btlVals lf))
        foreachBufSPMD pw False vb leafLen (\localIdx val -> step (uniform globalOff + localIdx) val)
        pure ((nxtL, globalOff + leafLen), ())

-- | Leaf-by-leaf masked-safe unit-stride SIMD fold over sorted values across the doubly-linked leaf chain,
-- passing the running 0-based global sorted index @(globalOffset + localIdx)@ to @step@.
foldValuesSPMD ::
  forall w k v s m e.
  (BTreeMapCtx m e k v, LayoutUnitSPMD w m e v, SPMDTyped w e s) =>
  Proxy w ->
  BTreeMap e k v ->
  SPMD w e s ->
  (SPMD w e s -> SPMD w e Int64 -> Elem (SPMD w e) v -> SPMDM w m e (SPMD w e s)) ->
  m (SPMD w e s)
foldValuesSPMD pw (BTreeMap hdr) s0 step = do
  firstRaw <- peek (getPtrOf (btmsFirstLeaf (projectPtrs hdr)))
  accRef <- withSPMD pw (newRef s0)
  runDrain drain_ $
    unfoldWhileM (firstRaw, 0 :: e Int64) (\(curL, _) -> pure (ptrToWord64Expr curL `ne` 0)) $
      \(curL, globalOff) -> do
        let pLeaf = asLeaf @k @v curL
            lf = projectPtrs pLeaf
        leafLen <- peek (getPtrOf (btlNumKeys lf))
        nxtL <- peek (getPtrOf (btlNextLeaf lf))
        vb <- inlineAsBuf (Proxy @BTreeMaxKeys) (getPtrOf (btlVals lf))
        traverseBufSPMD_ pw False vb leafLen $ \localIdx val -> do
          curAcc <- readRef accRef
          nxtAcc <- step curAcc (uniform globalOff + localIdx) val
          writeRef accRef nxtAcc
        pure ((nxtL, globalOff + leafLen), ())
  withSPMD pw (readRef accRef)

-- | O(log N + K) range 'Stream' over @[lo, hi]@ (inclusive): descends once to the @lo@ leaf,
-- then walks @nextLeaf@ links sequentially until @key > hi@!
rangeStream ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  BTreeMap e k v ->
  Elem e k ->
  Elem e k ->
  Stream m (Elem e k, Elem e v)
rangeStream bm@(BTreeMap hdr) lo hi = Stream $ Codensity $ \kOuter -> do
  h <- peek (getPtrOf (btmsHeight (projectPtrs hdr)))
  leafRef <- newRef (nullPtr @e @Word8)
  idxRef <- newRef (0 :: e Int64)
  whenM_ ((h `gt` 0) `land` lnot (ltKey @e @k hi lo)) $ do
    pStart <- descendToLeaf bm lo
    nStart <- peek (getPtrOf (btlNumKeys (projectPtrs pStart)))
    (kbStart, _) <- leafBufs pStart
    posStart <- lowerBoundBuf (ltKey @e @k) kbStart nStart lo
    writeRef leafRef (toRawNode pStart)
    writeRef idxRef posStart
  kOuter $ ContT $ \kStep -> do
    curRaw <- readRef leafRef
    ifThenElseM_
      (ptrToWord64Expr curRaw `eq` 0)
      (kStep Stop)
      ( do
          let pLeaf = asLeaf @k @v curRaw
              lf = projectPtrs pLeaf
          n <- peek (getPtrOf (btlNumKeys lf))
          idx <- readRef idxRef
          ifThenElseM_
            (idx `lt` n)
            ( do
                (kb, vb) <- leafBufs pLeaf
                candK <- readBuf kb idx
                ifThenElseM_
                  (ltKey @e @k hi candK)
                  (writeRef leafRef nullPtr >> kStep Stop)
                  ( do
                      candV <- readBuf vb idx
                      writeRef idxRef (idx + 1)
                      kStep (Yield (candK, candV))
                  )
            )
            ( do
                nxtRaw <- peek (getPtrOf (btlNextLeaf lf))
                writeRef leafRef nxtRaw
                writeRef idxRef 0
                kStep Skip
            )
      )

unionWithS ::
  forall k v m e.
  (BTreeMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  (Elem e v -> Elem e v -> Elem e v) ->
  BTreeMap e k v ->
  BTreeMap e k v ->
  Stream m (Elem e k, Elem e v)
unionWithS combine a b =
  unionSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (\(ka, va) (_, vb) -> (ka, combine va vb))
    (toStream a)
    (toStream b)

intersectWithS ::
  forall k v m e.
  (BTreeMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  (Elem e v -> Elem e v -> Elem e v) ->
  BTreeMap e k v ->
  BTreeMap e k v ->
  Stream m (Elem e k, Elem e v)
intersectWithS combine a b =
  intersectSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (\(ka, va) (_, vb) -> (ka, combine va vb))
    (toStream a)
    (toStream b)

differenceS ::
  forall k v m e.
  (BTreeMapCtx m e k v, RefCell m e (Elem e k, Elem e v)) =>
  BTreeMap e k v ->
  BTreeMap e k v ->
  Stream m (Elem e k, Elem e v)
differenceS a b =
  differenceSortedByS
    (\(ka, _) (kb, _) -> ltKey @e @k ka kb)
    (\(ka, _) (kb, _) -> eqKey @e @k ka kb)
    (toStream a)
    (toStream b)

drainToBTreeMap ::
  forall k v m e.
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  Drain m (Elem e k, Elem e v) (BTreeMap e k v)
drainToBTreeMap alloc = Drain $ \s -> do
  bm <- new alloc
  runDrain (drainAppendBTreeMap alloc bm) s
  pure bm

fromStream ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  Stream m (Elem e k, Elem e v) ->
  m (BTreeMap e k v)
fromStream alloc = runDrain (drainToBTreeMap alloc)

drainAppendBTreeMap ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  BTreeMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendBTreeMap alloc bm = drainForEach (\(k, v) -> insert alloc bm k v)

drainAppendBTreeMapWith ::
  (BTreeMapCtx m e k v) =>
  Alloc e ->
  (Elem e v -> Elem e v -> Elem e v) ->
  BTreeMap e k v ->
  Drain m (Elem e k, Elem e v) ()
drainAppendBTreeMapWith alloc combine bm =
  drainForEach (\(k, v) -> insertWith alloc bm combine k v)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCHost (BTreeMap (JanusCT _) k v) = BTreeMap Identity k v

deriving via
  ViaPtrHandle t (CStruct (BTreeMapSpine k v)) (BTreeMap Identity k v)
  instance
    (Typeable k, Typeable v) => JanusCArg t (BTreeMap (JanusCT t) k v)

deriving via
  ViaPtrHandle t (CStruct (BTreeMapSpine k v)) (BTreeMap Identity k v)
  instance
    (Typeable k, Typeable v) => JanusCValRet t (BTreeMap (JanusCT t) k v)
