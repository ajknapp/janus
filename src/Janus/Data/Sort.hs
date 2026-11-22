{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Janus.Data.Sort
  ( -- * Branchless Binary Search Over Sorted Layout Buffers
    lowerBoundBuf,
    upperBoundBuf,
    binarySearchBuf,

    -- * Zero-Allocation In-Place Sorting Over Layout Buffers
    sortBufBy,
    sortBuf,
    sortKeyValBufsBy,
    stableSortKeyValBufsBy,
    sortKeyValBufs,
  )
where

import Data.Int (Int64)
import Janus.Alloc.Policy (Alloc, CmdAlloc, allocTypedArray, freeTypedArray)
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Command.While (whileM)
import Janus.Control.Drain (drainForEach, drain_, runDrain)
import Janus.Control.Stream (rangeS, reverseRangeS, unfoldWhileM)
import Janus.Data.Layout (Buf, Elem, LayoutElemCtx, StorageLayout (..))
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))

-- | Branchless lower-bound search over @[0, len)@, returning the first index
-- @i@ such that @lnot (isLt buf[i] target)@ (or @len@ if all elements @< target@).
lowerBoundBuf ::
  forall k m e.
  (CmdAlloc m e, StorageLayout k, LayoutElemCtx m e k) =>
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  e Int64 ->
  Elem e k ->
  m (e Int64)
lowerBoundBuf isLt buf len target = do
  baseRef <- newRef 0
  remRef <- newRef len
  whileM (len `gt` 1) $ \c -> do
    remLen <- readRef remRef
    base <- readRef baseRef
    half <- letM (remLen `rshift` 1)
    midVal <- readBuf buf (base + half)
    let nextBase = ifThenElse (isLt midVal target) (base + half) base
        nextRem = remLen - half
    writeRef baseRef nextBase
    writeRef remRef nextRem
    writeRef c (nextRem `gt` 1)
  finalBase <- readRef baseRef
  resRef <- newRef finalBase
  whenM_ (len `gt` 0) $ do
    v <- readBuf buf finalBase
    writeRef resRef (ifThenElse (isLt v target) (finalBase + 1) finalBase)
  readRef resRef

-- | Branchless upper-bound search over @[0, len)@, returning the first index
-- @i@ such that @isLt target buf[i]@ (or @len@ if none).
upperBoundBuf ::
  forall k m e.
  (CmdAlloc m e, StorageLayout k, LayoutElemCtx m e k) =>
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  e Int64 ->
  Elem e k ->
  m (e Int64)
upperBoundBuf isLt buf len target = do
  baseRef <- newRef 0
  remRef <- newRef len
  whileM (len `gt` 1) $ \c -> do
    remLen <- readRef remRef
    base <- readRef baseRef
    half <- letM (remLen `rshift` 1)
    midVal <- readBuf buf (base + half)
    let leTarget = lnot (isLt target midVal)
        nextBase = ifThenElse leTarget (base + half) base
        nextRem = remLen - half
    writeRef baseRef nextBase
    writeRef remRef nextRem
    writeRef c (nextRem `gt` 1)
  finalBase <- readRef baseRef
  resRef <- newRef finalBase
  whenM_ (len `gt` 0) $ do
    v <- readBuf buf finalBase
    writeRef resRef (ifThenElse (lnot (isLt target v)) (finalBase + 1) finalBase)
  readRef resRef

-- | Exact binary search returning @(found, indexOrInsertionPoint)@.
binarySearchBuf ::
  forall k m e.
  (CmdAlloc m e, StorageLayout k, LayoutElemCtx m e k) =>
  (Elem e k -> Elem e k -> e Bool) ->
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  e Int64 ->
  Elem e k ->
  m (e Bool, e Int64)
binarySearchBuf isLt isEq buf len target = do
  idx <- lowerBoundBuf isLt buf len target
  foundRef <- newRef false
  whenM_ (idx `lt` len) $ do
    v <- readBuf buf idx
    writeRef foundRef (isEq v target)
  found <- readRef foundRef
  pure (found, idx)

--------------------------------------------------------------------------------
-- In-Place Hybrid Insertion + Heapsort Over Layout Buffers (Zero Allocations,
-- O(N log N) Worst-Case, Cache-Friendly Sequential Small-N Fast Path)
--------------------------------------------------------------------------------

swapAt ::
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  Buf a e ->
  e Int64 ->
  e Int64 ->
  m ()
swapAt buf i j =
  whenM_ (i `ne` j) $ do
    vi <- readBuf buf i
    vj <- readBuf buf j
    writeBuf buf i vj
    writeBuf buf j vi

insertionSortBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  (Elem e a -> Elem e a -> e Bool) ->
  Buf a e ->
  e Int64 ->
  m ()
insertionSortBuf isLt buf len =
  rangeM 1 len $ \i -> do
    key <- readBuf buf i
    jRef <- newRef i
    whileM (i `gt` 0) $ \c -> do
      j <- readRef jRef
      ifThenElseM_
        (j `le` 0)
        (writeRef c false)
        ( do
            prev <- readBuf buf (j - 1)
            ifThenElseM_
              (isLt key prev)
              ( do
                  writeBuf buf j prev
                  writeRef jRef (j - 1)
              )
              (writeRef c false)
        )
    jFinal <- readRef jRef
    writeBuf buf jFinal key

siftDownBuf ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  (Elem e a -> Elem e a -> e Bool) ->
  Buf a e ->
  e Int64 ->
  e Int64 ->
  m ()
siftDownBuf isLt buf start end = do
  rootRef <- newRef start
  whileM true $ \c -> do
    root <- readRef rootRef
    child <- letM (root * 2 + 1)
    ifThenElseM_
      (child `ge` end)
      (writeRef c false)
      ( do
          bestRef <- newRef root
          vRoot <- readBuf buf root
          vLeft <- readBuf buf child
          whenM_ (isLt vRoot vLeft) $
            writeRef bestRef child
          whenM_ ((child + 1) `lt` end) $ do
            curBest <- readRef bestRef
            vBest <- readBuf buf curBest
            vRight <- readBuf buf (child + 1)
            whenM_ (isLt vBest vRight) $
              writeRef bestRef (child + 1)
          best <- readRef bestRef
          ifThenElseM_
            (best `eq` root)
            (writeRef c false)
            ( do
                swapAt buf root best
                writeRef rootRef best
            )
      )

-- | Sort @[0, len)@ in-place using insertion sort for @len <= 24@ and
-- guaranteed O(N log N) zero-allocation heapsort for larger @len@.
sortBufBy ::
  forall a m e.
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a) =>
  (Elem e a -> Elem e a -> e Bool) ->
  Buf a e ->
  e Int64 ->
  m ()
sortBufBy isLt buf len =
  whenM_ (len `gt` 1) $
    ifThenElseM_
      (len `le` 24)
      (insertionSortBuf isLt buf len)
      ( do
          startRef <- newRef ((len `rshift` 1) - 1)
          whileM true $ \c -> do
            s <- readRef startRef
            siftDownBuf isLt buf s len
            ifThenElseM_
              (s `le` 0)
              (writeRef c false)
              (writeRef startRef (s - 1))
          endRef <- newRef (len - 1)
          whileM true $ \c -> do
            end <- readRef endRef
            ifThenElseM_
              (end `le` 0)
              (writeRef c false)
              ( do
                  swapAt buf 0 end
                  siftDownBuf isLt buf 0 end
                  writeRef endRef (end - 1)
              )
      )

sortBuf ::
  (CmdAlloc m e, StorageLayout a, LayoutElemCtx m e a, Elem e a ~ e s, ExpOrd e s) =>
  Buf a e ->
  e Int64 ->
  m ()
sortBuf = sortBufBy lt

--------------------------------------------------------------------------------
-- Co-Sorting Parallel Key + Value Layout Buffers (Used by FlatMap Bulk Build)
--------------------------------------------------------------------------------

swapKVAt ::
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v
  ) =>
  Buf k e ->
  Buf v e ->
  e Int64 ->
  e Int64 ->
  m ()
swapKVAt kBuf vBuf i j =
  whenM_ (i `ne` j) $ do
    swapAt kBuf i j
    swapAt vBuf i j

insertionSortKV ::
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v
  ) =>
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  Buf v e ->
  e Int64 ->
  m ()
insertionSortKV isLt kBuf vBuf len =
  rangeM 1 len $ \i -> do
    key <- readBuf kBuf i
    val <- readBuf vBuf i
    jRef <- newRef i
    whileM (i `gt` 0) $ \c -> do
      j <- readRef jRef
      ifThenElseM_
        (j `le` 0)
        (writeRef c false)
        ( do
            prevK <- readBuf kBuf (j - 1)
            ifThenElseM_
              (isLt key prevK)
              ( do
                  prevV <- readBuf vBuf (j - 1)
                  writeBuf kBuf j prevK
                  writeBuf vBuf j prevV
                  writeRef jRef (j - 1)
              )
              (writeRef c false)
        )
    jFinal <- readRef jRef
    writeBuf kBuf jFinal key
    writeBuf vBuf jFinal val

siftDownKV ::
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v
  ) =>
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  Buf v e ->
  e Int64 ->
  e Int64 ->
  m ()
siftDownKV isLt kBuf vBuf start end = do
  rootRef <- newRef start
  whileM true $ \c -> do
    root <- readRef rootRef
    child <- letM (root * 2 + 1)
    ifThenElseM_
      (child `ge` end)
      (writeRef c false)
      ( do
          bestRef <- newRef root
          kRoot <- readBuf kBuf root
          kLeft <- readBuf kBuf child
          whenM_ (isLt kRoot kLeft) $
            writeRef bestRef child
          whenM_ ((child + 1) `lt` end) $ do
            curBest <- readRef bestRef
            kBest <- readBuf kBuf curBest
            kRight <- readBuf kBuf (child + 1)
            whenM_ (isLt kBest kRight) $
              writeRef bestRef (child + 1)
          best <- readRef bestRef
          ifThenElseM_
            (best `eq` root)
            (writeRef c false)
            ( do
                swapKVAt kBuf vBuf root best
                writeRef rootRef best
            )
      )

sortKeyValBufsBy ::
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v
  ) =>
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  Buf v e ->
  e Int64 ->
  m ()
sortKeyValBufsBy isLt kBuf vBuf len =
  whenM_ (len `gt` 1) $
    ifThenElseM_
      (len `le` 24)
      (insertionSortKV isLt kBuf vBuf len)
      ( do
          startRef <- newRef ((len `rshift` 1) - 1)
          whileM true $ \c -> do
            s <- readRef startRef
            siftDownKV isLt kBuf vBuf s len
            ifThenElseM_
              (s `le` 0)
              (writeRef c false)
              (writeRef startRef (s - 1))
          endRef <- newRef (len - 1)
          whileM true $ \c -> do
            end <- readRef endRef
            ifThenElseM_
              (end `le` 0)
              (writeRef c false)
              ( do
                  swapKVAt kBuf vBuf 0 end
                  siftDownKV isLt kBuf vBuf 0 end
                  writeRef endRef (end - 1)
              )
      )

sortKeyValBufs ::
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v,
    Elem e k ~ e ks,
    ExpOrd e ks
  ) =>
  Buf k e ->
  Buf v e ->
  e Int64 ->
  m ()
sortKeyValBufs = sortKeyValBufsBy lt

-- | Stable $O(N \log N)$ key-value co-sort preserving arrival order among equal keys
-- (zero allocation for @len <= 24@; uses @alloc@ for a temporary arrival-index scratch array when @len > 24@).
stableSortKeyValBufsBy ::
  forall k v m e.
  ( CmdAlloc m e,
    StorageLayout k,
    LayoutElemCtx m e k,
    StorageLayout v,
    LayoutElemCtx m e v
  ) =>
  Alloc e ->
  (Elem e k -> Elem e k -> e Bool) ->
  (Elem e k -> Elem e k -> e Bool) ->
  Buf k e ->
  Buf v e ->
  e Int64 ->
  m ()
stableSortKeyValBufsBy alloc isLt isEq kBuf vBuf len =
  whenM_ (len `gt` 1) $
    ifThenElseM_
      (len `le` 24)
      (insertionSortKV isLt kBuf vBuf len)
      ( do
          arr <- allocTypedArray @Int64 alloc len
          let swapKVA i j =
                whenM_ (i `ne` j) $ do
                  swapKVAt kBuf vBuf i j
                  ai <- peekElemOff arr i
                  aj <- peekElemOff arr j
                  pokeElemOff arr aj i
                  pokeElemOff arr ai j
              lessKVA i j = do
                ki <- readBuf kBuf i
                kj <- readBuf kBuf j
                ai <- peekElemOff arr i
                aj <- peekElemOff arr j
                pure (isLt ki kj `lor` (isEq ki kj `land` (ai `lt` aj)))
              siftKVA start end =
                runDrain drain_ $
                  unfoldWhileM start (\root -> pure ((root * 2 + 1) `lt` end)) $ \root -> do
                    child <- letM (root * 2 + 1)
                    bestRef <- newRef root
                    ltLeft <- lessKVA root child
                    whenM_ ltLeft $
                      writeRef bestRef child
                    whenM_ ((child + 1) `lt` end) $ do
                      curBest <- readRef bestRef
                      ltRight <- lessKVA curBest (child + 1)
                      whenM_ ltRight $
                        writeRef bestRef (child + 1)
                    best <- readRef bestRef
                    whenM_ (best `ne` root) (swapKVA root best)
                    pure (ifThenElse (best `eq` root) end best, ())
          runDrain (drainForEach (\i -> pokeElemOff arr i i)) (rangeS 0 len)
          runDrain (drainForEach (\s -> siftKVA s len)) (reverseRangeS 0 (len `rshift` 1))
          runDrain (drainForEach (\end -> swapKVA 0 end >> siftKVA 0 end)) (reverseRangeS 1 len)
          freeTypedArray @Int64 alloc arr len
      )
