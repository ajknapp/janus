-- | Umbrella module re-exporting Higher-Kinded Data C-ABI support, SIMD/SPMD storage
-- layouts (@Scalar@, @AoS@, @SoA@, @AoSoA w@, @SOAOA w@), branchless sorting, and
-- all 16 mutable single-threaded mechanically sympathetic data structures.
--
-- For per-container operations ('new', 'free', 'insert', 'lookup', 'stream', etc.),
-- import the specific container modules qualified (e.g. @import qualified Janus.Data.DynVec as DV@).
module Janus.Data
  ( -- * HKD C-ABI & Storage Layouts (`Scalar`, `AoS`, `SoA`, `AoSoA w`, `SOAOA w`)
    module Janus.Data.HKD,
    module Janus.Data.Layout,
    module Janus.Data.Sort,

    -- * Sequential & Positional Data Structures
    DynVec (..),
    DynVecCtx,
    drainToVect,
    SmallVec (..),
    SmallVecCtx,
    StableVec (..),
    StableVec64,
    StableVecCtx,
    RingBuffer (..),
    RingBufferCtx,
    Deque (..),
    DequeCtx,

    -- * Associative & Ordered Data Structures
    HashKey (..),
    HashMap (..),
    HashMapCtx,
    HashSet (..),
    HashSetCtx,
    OrdKey (..),
    FlatMap (..),
    FlatMapCtx,
    FlatSet (..),
    FlatSetCtx,
    BTreeMap (..),
    BTreeMapCtx,
    BTreeSet (..),
    BTreeSetCtx,

    -- * Stable-Handle, Sparse, Pool, & Intrusive Data Structures
    SlotKey (..),
    packSlotKey,
    unpackSlotKey,
    SlotMap (..),
    SlotMapCtx,
    SparseSet (..),
    SparseSetCtx,
    SparseVec (..),
    SparseVecCtx,
    PoolHandle (..),
    packPoolHandle,
    unpackPoolHandle,
    Pool (..),
    PoolCtx,
    RawListHook (..),
    HookSpec (..),
    mkHookSpec,
    IntrusiveList (..),
    IntrusiveListCtx,
  )
where

import Janus.Data.BTreeMap (BTreeMap (..), BTreeMapCtx)
import Janus.Data.BTreeSet (BTreeSet (..), BTreeSetCtx)
import Janus.Data.Deque (Deque (..), DequeCtx)
import Janus.Data.DynVec (DynVec (..), DynVecCtx, drainToVect)
import Janus.Data.FlatMap (FlatMap (..), FlatMapCtx, OrdKey (..))
import Janus.Data.FlatSet (FlatSet (..), FlatSetCtx)
import Janus.Data.HKD
import Janus.Data.HashMap (HashKey (..), HashMap (..), HashMapCtx)
import Janus.Data.HashSet (HashSet (..), HashSetCtx)
import Janus.Data.IntrusiveList
  ( HookSpec (..),
    IntrusiveList (..),
    IntrusiveListCtx,
    RawListHook (..),
    mkHookSpec,
  )
import Janus.Data.Layout
import Janus.Data.Pool
  ( Pool (..),
    PoolCtx,
    PoolHandle (..),
    packPoolHandle,
    unpackPoolHandle,
  )
import Janus.Data.RingBuffer (RingBuffer (..), RingBufferCtx)
import Janus.Data.SlotMap
  ( SlotKey (..),
    SlotMap (..),
    SlotMapCtx,
    packSlotKey,
    unpackSlotKey,
  )
import Janus.Data.SmallVec (SmallVec (..), SmallVecCtx)
import Janus.Data.Sort
import Janus.Data.SparseSet (SparseSet (..), SparseSetCtx)
import Janus.Data.SparseVec (SparseVec (..), SparseVecCtx)
import Janus.Data.StableVec (StableVec (..), StableVec64, StableVecCtx)
