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

module Janus.Data.DisjointSet
  ( -- * Flat Signed-Parent Union-Find Spine & Handle
    DisjointSetSpine (..),
    DisjointSet (..),
    DisjointSetCtx,

    -- * Lifecycle & Capacity Growth
    new,
    free,
    reset,
    grow,

    -- * Partition Queries & Union-By-Size With Path Halving
    numElements,
    capacity,
    numSets,
    find,
    sameSet,
    componentSize,
    union,

    -- * Partition Streams & Mutating Drains
    rootsS,
    componentMembersS,
    drainAppendUnions,

    -- * First-Class SPMD / SIMD Partition Queries & Reductions
    isRootSPMD,
    findSPMD,
    connectedSPMD,
    spmdCountComponents,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Policy
  ( Alloc,
    CmdAlloc,
    allocStruct,
    allocTypedArray,
    freeStruct,
    freeTypedArray,
    reallocTypedArray,
  )
import Janus.Backend.C
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.TH
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..), ExpSized (..))
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (Drain, drainFind, drainForEach, drainLast, drain_, runDrain)
import Janus.Control.Stream (Stream, filterMapS, iterateS, rangeS, unfoldWhileM, withStreamSetup)
import Janus.Data.HKD
import Janus.Data.Layout (ScalarBuf (..), foldBufSPMD)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD
  ( SPMD,
    SPMDGang,
    SPMDM,
    gangReduceAdd,
    liftSPMD,
    uniform,
    uniformAlignedPtr,
  )
import Janus.Typed (JanusTyped)

-- | Spine struct for flat signed-parent 'DisjointSet' (Union-Find):
-- * @dssData[x] < 0@: @x@ is a canonical component root of size @-dssData[x]@.
-- * @dssData[x] >= 0@: @dssData[x]@ is @x@'s parent index.
data DisjointSetSpine f = DisjointSetSpine
  { dssData :: !(f (Ptr Int64)),
    dssLen :: !(f Int64),
    dssCap :: !(f Int64),
    dssNumSets :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (DisjointSetSpine Identity)

deriving stock instance Show (DisjointSetSpine Identity)

instance JanusStruct DisjointSetSpine

newtype DisjointSet (e :: K.Type -> K.Type) = DisjointSet
  { unDisjointSet :: e (Ptr (CStruct DisjointSetSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (DisjointSet Identity)

deriving stock instance Show (DisjointSet Identity)

instance JanusHKD DisjointSet

type DisjointSetCtx m e =
  ( CmdAlloc m e,
    JanusTyped e (Ptr Int64),
    CmdStorable m e (Ptr Int64),
    ExpSized e (CStruct DisjointSetSpine),
    JanusTyped e (CStruct DisjointSetSpine)
  )

-- | Create a 'DisjointSet' over @n@ singleton elements @[0 .. n - 1]@ initialized to @-1@.
new ::
  forall m e.
  (DisjointSetCtx m e) =>
  Alloc e ->
  e Int64 ->
  m (DisjointSet e)
new alloc initN = do
  n <- letM (ifThenElse (initN `le` 0) 0 initN)
  hdr <- allocStruct @(CStruct DisjointSetSpine) alloc
  arr <- allocTypedArray @Int64 alloc n
  runDrain (drainForEach (\i -> pokeElemOff arr (-1) i)) (rangeS 0 n)
  let fields = projectPtrs hdr
  poke (getPtrOf (dssData fields)) arr
  poke (getPtrOf (dssLen fields)) n
  poke (getPtrOf (dssCap fields)) n
  poke (getPtrOf (dssNumSets fields)) n
  pure (DisjointSet hdr)

-- | Release the backing parent/size array (when non-empty capacity) and spine struct via @alloc@.
free ::
  (DisjointSetCtx m e) =>
  Alloc e ->
  DisjointSet e ->
  m ()
free alloc (DisjointSet hdr) = do
  let fields = projectPtrs hdr
  curCap <- peek (getPtrOf (dssCap fields))
  whenM_ (curCap `gt` 0) $ do
    arr <- peek (getPtrOf (dssData fields))
    freeTypedArray @Int64 alloc arr curCap
  freeStruct alloc hdr

-- | Reset all @[0 .. numElements - 1]@ back to disjoint singletons (@-1@) without reallocating.
reset ::
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  m ()
reset (DisjointSet hdr) = do
  let fields = projectPtrs hdr
  curLen <- peek (getPtrOf (dssLen fields))
  arr <- peek (getPtrOf (dssData fields))
  runDrain (drainForEach (\i -> pokeElemOff arr (-1) i)) (rangeS 0 curLen)
  poke (getPtrOf (dssNumSets fields)) curLen

-- | Append @additional@ new singleton elements @[oldLen .. oldLen + additional - 1]@,
-- growing backing storage geometrically via @alloc@ when needed.
grow ::
  (DisjointSetCtx m e) =>
  Alloc e ->
  DisjointSet e ->
  e Int64 ->
  m ()
grow alloc (DisjointSet hdr) additional =
  whenM_ (additional `gt` 0) $ do
    let fields = projectPtrs hdr
    curLen <- peek (getPtrOf (dssLen fields))
    curCap <- peek (getPtrOf (dssCap fields))
    newLen <- letM (curLen + additional)
    whenM_ (newLen `gt` curCap) $ do
      let c0 = ifThenElse (curCap `lt` 8) 8 (curCap * 2)
      (_, newCap) <- runDrain (drainFind c0 (`ge` newLen)) (iterateS c0 (* 2))
      oldArr <- peek (getPtrOf (dssData fields))
      newArr <- reallocTypedArray @Int64 alloc oldArr curCap newCap
      poke (getPtrOf (dssData fields)) newArr
      poke (getPtrOf (dssCap fields)) newCap
    arr <- peek (getPtrOf (dssData fields))
    runDrain (drainForEach (\i -> pokeElemOff arr (-1) i)) (rangeS curLen newLen)
    curSets <- peek (getPtrOf (dssNumSets fields))
    poke (getPtrOf (dssLen fields)) newLen
    poke (getPtrOf (dssNumSets fields)) (curSets + additional)

numElements :: (DisjointSetCtx m e) => DisjointSet e -> m (e Int64)
numElements (DisjointSet hdr) = peek (getPtrOf (dssLen (projectPtrs hdr)))

capacity :: (DisjointSetCtx m e) => DisjointSet e -> m (e Int64)
capacity (DisjointSet hdr) = peek (getPtrOf (dssCap (projectPtrs hdr)))

numSets :: (DisjointSetCtx m e) => DisjointSet e -> m (e Int64)
numSets (DisjointSet hdr) = peek (getPtrOf (dssNumSets (projectPtrs hdr)))

-- | Find the canonical root of @x@ using iterative path-halving:
-- while @a[cur] >= 0@, let @p = a[cur]@; if @a[p] >= 0@ set @a[cur] = a[p]@ and step to @a[p]@,
-- else step to @p@.
find ::
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  e Int64 ->
  m (e Int64)
find (DisjointSet hdr) x = do
  let fields = projectPtrs hdr
  curLen <- peek (getPtrOf (dssLen fields))
  rootRef <- newRef x
  whenM_ ((x `ge` 0) `land` (x `lt` curLen)) $ do
    arr <- peek (getPtrOf (dssData fields))
    runDrain drain_ $
      unfoldWhileM
        x
        (\cur -> (`ge` 0) <$> peekElemOff arr cur)
        ( \cur -> do
            p <- peekElemOff arr cur
            gp <- peekElemOff arr p
            whenM_ (gp `ge` 0) $
              pokeElemOff arr gp cur
            let nxt = ifThenElse (gp `ge` 0) gp p
            writeRef rootRef nxt
            pure (nxt, ())
        )
  readRef rootRef

-- | Return @true@ iff @x@ and @y@ are valid indices in the same equivalence class.
sameSet ::
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  e Int64 ->
  e Int64 ->
  m (e Bool)
sameSet ds x y = do
  curLen <- numElements ds
  okRef <- newRef false
  whenM_ ((x `ge` 0) `land` (x `lt` curLen) `land` (y `ge` 0) `land` (y `lt` curLen)) $ do
    rx <- find ds x
    ry <- find ds y
    writeRef okRef (rx `eq` ry)
  readRef okRef

-- | Return the cardinality of @x@'s component (or @0@ if @x@ is out of bounds).
componentSize ::
  forall m e.
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  e Int64 ->
  m (e Int64)
componentSize ds@(DisjointSet hdr) x = do
  let fields = projectPtrs hdr
  curLen <- peek (getPtrOf (dssLen fields))
  szRef <- newRef (0 :: e Int64)
  whenM_ ((x `ge` 0) `land` (x `lt` curLen)) $ do
    r <- find ds x
    arr <- peek (getPtrOf (dssData fields))
    negSz <- peekElemOff arr r
    writeRef szRef (0 - negSz)
  readRef szRef

-- | Union-by-size of @x@ and @y@'s components: attaches the smaller root under the larger root,
-- updates the negative size at the new root, decrements @dssNumSets@, and returns
-- @(didMerge, canonicalRoot)@.
union ::
  forall m e.
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  e Int64 ->
  e Int64 ->
  m (e Bool, e Int64)
union ds@(DisjointSet hdr) x y = do
  let fields = projectPtrs hdr
  curLen <- peek (getPtrOf (dssLen fields))
  mergedRef <- newRef false
  rootRef <- newRef (-1 :: e Int64)
  whenM_ ((x `ge` 0) `land` (x `lt` curLen) `land` (y `ge` 0) `land` (y `lt` curLen)) $ do
    rx <- find ds x
    ry <- find ds y
    ifThenElseM_
      (rx `eq` ry)
      (writeRef rootRef rx)
      ( do
          arr <- peek (getPtrOf (dssData fields))
          sxNeg <- peekElemOff arr rx
          syNeg <- peekElemOff arr ry
          let big = ifThenElse (sxNeg `le` syNeg) rx ry
              small = ifThenElse (sxNeg `le` syNeg) ry rx
          pokeElemOff arr (sxNeg + syNeg) big
          pokeElemOff arr big small
          curSets <- peek (getPtrOf (dssNumSets fields))
          poke (getPtrOf (dssNumSets fields)) (curSets - 1)
          writeRef mergedRef true
          writeRef rootRef big
      )
  (,) <$> readRef mergedRef <*> readRef rootRef

-- | Ascending 'Stream' of all canonical component root indices @[0 .. numElements - 1]@.
rootsS ::
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  Stream m (e Int64)
rootsS (DisjointSet hdr) =
  let fields = projectPtrs hdr
   in withStreamSetup
        ((,) <$> peek (getPtrOf (dssLen fields)) <*> peek (getPtrOf (dssData fields)))
        ( \(curLen, arr) ->
            filterMapS
              ( \i -> do
                  v <- peekElemOff arr i
                  pure (v `lt` 0, i)
              )
              (rangeS 0 curLen)
        )

-- | Ascending 'Stream' of all element indices belonging to the same component as @x@.
componentMembersS ::
  forall m e.
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  e Int64 ->
  Stream m (e Int64)
componentMembersS ds x =
  withStreamSetup
    ( do
        curLen <- numElements ds
        let valid = (x `ge` 0) `land` (x `lt` curLen)
        rRef <- newRef (-1 :: e Int64)
        whenM_ valid $
          find ds x >>= writeRef rRef
        targetRoot <- readRef rRef
        let effLen = ifThenElse valid curLen 0
        pure (effLen, targetRoot)
    )
    ( \(effLen, targetRoot) ->
        filterMapS
          ( \i -> do
              ri <- find ds i
              pure (ri `eq` targetRoot, i)
          )
          (rangeS 0 effLen)
    )

-- | Apply all @(u, v)@ union pairs from a 'Stream' into an existing 'DisjointSet'.
drainAppendUnions ::
  (DisjointSetCtx m e) =>
  DisjointSet e ->
  Drain m (e Int64, e Int64) ()
drainAppendUnions ds = drainForEach (\(u, v) -> union ds u v >> pure ())

--------------------------------------------------------------------------------
-- First-Class SPMD / SIMD Partition Queries & Reductions
--------------------------------------------------------------------------------

-- | Bounds-masked SIMD gather checking whether each lane's index @xs@ is a canonical
-- component root (@dssData[xs] < 0@). Out-of-bounds lanes return @false@.
isRootSPMD ::
  forall w m e.
  (DisjointSetCtx m e, SPMDGang w m e) =>
  DisjointSet e ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Bool)
isRootSPMD (DisjointSet hdr) xs = do
  let fields = projectPtrs hdr
  curLen <- liftSPMD (peek (getPtrOf (dssLen fields)))
  outRef <- newRef (false :: SPMD w e Bool)
  let valid = (xs `ge` 0) `land` (xs `lt` uniform curLen)
  whenM_ valid $ do
    arr <- liftSPMD (peek (getPtrOf (dssData fields)))
    v <- peekElemOff (uniformAlignedPtr arr) xs
    writeRef outRef (v `lt` 0)
  readRef outRef

-- | Read-only lane-parallel root chase across @w@ SIMD lanes using two-step pointer
-- jumping inside 'unfoldWhileM' (zero writes, so duplicate elements across lanes are
-- completely race-free). Out-of-bounds lanes return @-1@.
findSPMD ::
  forall w m e.
  (DisjointSetCtx m e, SPMDGang w m e) =>
  DisjointSet e ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Int64)
findSPMD (DisjointSet hdr) xs = do
  let fields = projectPtrs hdr
  curLen <- liftSPMD (peek (getPtrOf (dssLen fields)))
  rootRef <- newRef (-1 :: SPMD w e Int64)
  let valid = (xs `ge` 0) `land` (xs `lt` uniform curLen)
  whenM_ valid $ do
    arr <- liftSPMD (peek (getPtrOf (dssData fields)))
    let spArr = uniformAlignedPtr arr
    (_, finalRoot) <-
      runDrain (drainLast xs) $
        unfoldWhileM
          xs
          (\cur -> (`ge` 0) <$> peekElemOff spArr cur)
          ( \cur -> do
              p <- peekElemOff spArr cur
              gp <- peekElemOff spArr p
              let nxt = ifThenElse (gp `ge` 0) gp p
              pure (nxt, nxt)
          )
    writeRef rootRef finalRoot
  readRef rootRef

-- | Lane-parallel connectivity query returning @true@ iff both @us@ and @vs@ are valid
-- indices in the same equivalence class.
connectedSPMD ::
  forall w m e.
  (DisjointSetCtx m e, SPMDGang w m e) =>
  DisjointSet e ->
  SPMD w e Int64 ->
  SPMD w e Int64 ->
  SPMDM w m e (SPMD w e Bool)
connectedSPMD ds us vs = do
  ru <- findSPMD ds us
  rv <- findSPMD ds vs
  pure ((ru `ge` 0) `land` (ru `eq` rv))

-- | Count canonical component roots (@dssData[i] < 0@) across @[0, numElements)@ using
-- 64B-aligned SIMD strip-mining and horizontal gang reduction.
spmdCountComponents ::
  forall w m e.
  (DisjointSetCtx m e, SPMDGang w m e) =>
  Proxy w ->
  DisjointSet e ->
  m (e Int64)
spmdCountComponents pw (DisjointSet hdr) = do
  let fields = projectPtrs hdr
  curLen <- peek (getPtrOf (dssLen fields))
  arr <- peek (getPtrOf (dssData fields))
  acc <-
    foldBufSPMD
      pw
      True
      (ScalarBuf arr)
      curLen
      (0 :: SPMD w e Int64)
      (\s _ v -> pure (s + ifThenElse (v `lt` 0) 1 0))
  pure (gangReduceAdd acc)

--------------------------------------------------------------------------------
-- Host <-> C FFI Marshalling Instances
--------------------------------------------------------------------------------

type instance JanusCEval (DisjointSet JanusC -> r) = DisjointSet Identity -> JanusCEval r

type instance JanusCRetType (DisjointSet JanusC -> r) = JanusCRetType r

instance (JanusCParam r) => JanusCParam (DisjointSet JanusC -> r) where
  jcparam = mkPtrHandleJCParam DisjointSet

instance (JanusCEvaluate r) => JanusCEvaluate (DisjointSet JanusC -> r) where
  jceval = mkPtrHandleJCEval unDisjointSet DisjointSet

type instance JanusCEval (JanusCM (DisjointSet JanusC)) = IO (DisjointSet Identity)

type instance JanusCRetType (JanusCM (DisjointSet JanusC)) = DisjointSet Identity

instance JanusCParam (JanusCM (DisjointSet JanusC)) where
  jcparam = mkPtrHandleJCRetParam unDisjointSet

instance JanusCEvaluate (JanusCM (DisjointSet JanusC)) where
  jceval = mkPtrHandleJCRetEval DisjointSet

type instance LinuxX86_64CEval (DisjointSet LinuxX86_64C -> r) = DisjointSet Identity -> LinuxX86_64CEval r

type instance LinuxX86_64BaseRet (DisjointSet LinuxX86_64C -> r) = LinuxX86_64BaseRet r

instance (LinuxX86_64Param r) => LinuxX86_64Param (DisjointSet LinuxX86_64C -> r) where
  x86param = mkPtrHandleX86Param DisjointSet

instance (X86DeferCont r, LinuxX86_64Evaluate r) => LinuxX86_64Evaluate (DisjointSet LinuxX86_64C -> r) where
  x86eval = mkPtrHandleX86EvalParam unDisjointSet DisjointSet

instance (X86DeferCont r) => X86DeferCall (DisjointSet LinuxX86_64C -> r) where
  x86DeferArg = mkPtrHandleX86DeferArg unDisjointSet DisjointSet

instance (X86DeferCont r) => X86DeferCont (DisjointSet LinuxX86_64C -> r) where
  x86DeferCont = mkPtrHandleX86DeferCont unDisjointSet DisjointSet

type instance LinuxX86_64CEval (LinuxX86_64CM (DisjointSet LinuxX86_64C)) = IO (DisjointSet Identity)

type instance LinuxX86_64BaseRet (LinuxX86_64CM (DisjointSet LinuxX86_64C)) = DisjointSet Identity

instance LinuxX86_64Param (LinuxX86_64CM (DisjointSet LinuxX86_64C)) where
  x86param = mkPtrHandleX86RetParam unDisjointSet

instance LinuxX86_64Evaluate (LinuxX86_64CM (DisjointSet LinuxX86_64C)) where
  x86eval = mkPtrHandleX86RetEval DisjointSet

instance X86DeferCall (LinuxX86_64CM (DisjointSet LinuxX86_64C)) where
  x86DeferArg = mkPtrHandleX86RetDeferArg DisjointSet

instance X86DeferCont (LinuxX86_64CM (DisjointSet LinuxX86_64C)) where
  x86DeferCont = mkPtrHandleX86RetDeferCont DisjointSet

instance (LinuxX86_64Param r, JanusCTH r) => JanusCTH (DisjointSet LinuxX86_64C -> r) where
  type JanusCTHEval (DisjointSet LinuxX86_64C -> r) = DisjointSet Identity -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (mkPtrHandleTHParamSpec (Proxy @(DisjointSet Identity)) : ps, retSpec)

instance JanusCTH (LinuxX86_64CM (DisjointSet LinuxX86_64C)) where
  type JanusCTHEval (LinuxX86_64CM (DisjointSet LinuxX86_64C)) = IO (DisjointSet Identity)
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    ([], mkPtrHandleTHRetSpec THRetIO (Proxy @(DisjointSet Identity)) (Proxy @(CStruct DisjointSetSpine)))
