-- | Umbrella module re-exporting Higher-Kinded Data C-ABI support, SIMD/SPMD storage
-- layouts (@Scalar@, @AoS@, @SoA@, @AoSoA w@, @SOAOA w@), branchless sorting, and
-- all 35 mutable single-threaded mechanically sympathetic data structures.
--
-- For per-container operations ('new', 'free', 'insert', 'lookup', 'stream', etc.),
-- import the specific container modules qualified (e.g. @import qualified Janus.Data.DynVec as DV@).
module Janus.Data
  ( -- * Reusable Constraint Bundles (`ExpJanus`, `CmdJanus`, `CmdJanusIO`, ...)
    module Janus.Constraints,

    -- * HKD C-ABI & Storage Layouts (`Scalar`, `AoS`, `SoA`, `AoSoA w`, `SOAOA w`)
    module Janus.Data.HKD,
    module Janus.Data.Layout,
    module Janus.Data.Sort,

    -- * Sequential, Stack, & Priority Queue Data Structures
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
    Stack (..),
    StackCtx,
    SmallStack (..),
    SmallStackCtx,
    PriorityQueue (..),
    PriorityQueueCtx,
    MinHeap (..),
    MinHeapCtx,
    IndexedPQ (..),
    IndexedPQCtx,

    -- * Bitwise, SWAR, & Probabilistic Filter Data Structures
    BitSet (..),
    BitSetCtx,
    BitTree (..),
    BitTreeCtx,
    BloomFilter (..),
    BloomFilterCtx,
    XorFilter (..),
    XorFilterCtx,

    -- * Associative, Insertion-Ordered, Eviction-Cache, & Ordered Data Structures
    HashKey (..),
    HashFilterCtx,
    HashLayoutCtx,
    HashMap (..),
    HashMapCtx,
    HashSet (..),
    HashSetCtx,
    IndexMap (..),
    IndexMapCtx,
    IndexSet (..),
    IndexSetCtx,
    ClockCache (..),
    ClockCacheCtx,
    LruCache (..),
    LruCacheCtx,
    OrdKey (..),
    OrdLayoutCtx,
    InlineOrdLayoutCtx,
    FlatMap (..),
    FlatMapCtx,
    FlatSet (..),
    FlatSetCtx,
    BTreeMap (..),
    BTreeMapCtx,
    BTreeSet (..),
    BTreeSetCtx,

    -- * Partition, Graph, & Spatial Search Data Structures
    DisjointSet (..),
    DisjointSetCtx,
    CsrGraph (..),
    CsrGraphCtx,
    EytzingerVec (..),
    EytzingerVecCtx,
    FlatIntervalMap (..),
    FlatIntervalMapCtx,

    -- * Stable-Handle, Sparse, Pool, Interning, & Intrusive Data Structures
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
    Symbol (..),
    symbolEq,
    Interner (..),
    InternerCtx,
    RawListHook (..),
    HookSpec (..),
    mkHookSpec,
    IntrusiveList (..),
    IntrusiveListCtx,
    RawHeapHook (..),
    emptyHeapHook,
    HeapSpec (..),
    mkHeapSpec,
    IntrusiveHeap (..),
    IntrusiveHeapCtx,
    emptyTimerHook,
    TimerSpec (..),
    mkTimerSpec,
    TimerWheel (..),
    TimerWheelCtx,
  )
where

import Janus.Constraints
import Janus.Data.BTreeMap (BTreeMap (..), BTreeMapCtx)
import Janus.Data.BTreeSet (BTreeSet (..), BTreeSetCtx)
import Janus.Data.BitSet (BitSet (..), BitSetCtx)
import Janus.Data.BitTree (BitTree (..), BitTreeCtx)
import Janus.Data.BloomFilter (BloomFilter (..), BloomFilterCtx)
import Janus.Data.ClockCache (ClockCache (..), ClockCacheCtx)
import Janus.Data.CsrGraph (CsrGraph (..), CsrGraphCtx)
import Janus.Data.Deque (Deque (..), DequeCtx)
import Janus.Data.DisjointSet (DisjointSet (..), DisjointSetCtx)
import Janus.Data.DynVec (DynVec (..), DynVecCtx, drainToVect)
import Janus.Data.EytzingerVec (EytzingerVec (..), EytzingerVecCtx)
import Janus.Data.FlatIntervalMap (FlatIntervalMap (..), FlatIntervalMapCtx)
import Janus.Data.FlatMap (FlatMap (..), FlatMapCtx, InlineOrdLayoutCtx, OrdKey (..), OrdLayoutCtx)
import Janus.Data.FlatSet (FlatSet (..), FlatSetCtx)
import Janus.Data.HKD
import Janus.Data.HashMap (HashFilterCtx, HashKey (..), HashLayoutCtx, HashMap (..), HashMapCtx)
import Janus.Data.HashSet (HashSet (..), HashSetCtx)
import Janus.Data.IndexMap (IndexMap (..), IndexMapCtx)
import Janus.Data.IndexSet (IndexSet (..), IndexSetCtx)
import Janus.Data.IndexedPQ (IndexedPQ (..), IndexedPQCtx)
import Janus.Data.Interner (Interner (..), InternerCtx, Symbol (..), symbolEq)
import Janus.Data.IntrusiveHeap
  ( HeapSpec (..),
    IntrusiveHeap (..),
    IntrusiveHeapCtx,
    RawHeapHook (..),
    emptyHeapHook,
    mkHeapSpec,
  )
import Janus.Data.IntrusiveList
  ( HookSpec (..),
    IntrusiveList (..),
    IntrusiveListCtx,
    RawListHook (..),
    mkHookSpec,
  )
import Janus.Data.Layout
import Janus.Data.LruCache (LruCache (..), LruCacheCtx)
import Janus.Data.Pool
  ( Pool (..),
    PoolCtx,
    PoolHandle (..),
    packPoolHandle,
    unpackPoolHandle,
  )
import Janus.Data.PriorityQueue (MinHeap (..), MinHeapCtx, PriorityQueue (..), PriorityQueueCtx)
import Janus.Data.RingBuffer (RingBuffer (..), RingBufferCtx)
import Janus.Data.SlotMap
  ( SlotKey (..),
    SlotMap (..),
    SlotMapCtx,
    packSlotKey,
    unpackSlotKey,
  )
import Janus.Data.SmallStack (SmallStack (..), SmallStackCtx)
import Janus.Data.SmallVec (SmallVec (..), SmallVecCtx)
import Janus.Data.Sort
import Janus.Data.SparseSet (SparseSet (..), SparseSetCtx)
import Janus.Data.SparseVec (SparseVec (..), SparseVecCtx)
import Janus.Data.StableVec (StableVec (..), StableVec64, StableVecCtx)
import Janus.Data.Stack (Stack (..), StackCtx)
import Janus.Data.TimerWheel
  ( TimerSpec (..),
    TimerWheel (..),
    TimerWheelCtx,
    emptyTimerHook,
    mkTimerSpec,
  )
import Janus.Data.XorFilter (XorFilter (..), XorFilterCtx)
