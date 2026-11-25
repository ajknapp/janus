{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'CSEReturn', 'CSETargetReturn', and 'CSEParam' instances for all
-- @Janus.Alloc.*@ allocator handles and @Janus.Data.*@ container handles,
-- derived via 'Generic' default signatures.
module Janus.CSE.Data () where

import GHC.Generics (Rep)
import Janus.Alloc.Arena (Arena, ArenaMark)
import Janus.Alloc.Fixed (FixedBuf)
import Janus.Alloc.Monotonic (Monotonic)
import Janus.Alloc.Pool (MemPool)
import Janus.Alloc.Recycling (Recycler)
import Janus.Alloc.Stats (AllocSnapshot, AllocTracker)
import Janus.CSE.Optimize
  ( CSEParam (..),
    CSEReturn,
    CSETargetReturn (..),
    GCSEHKD,
  )
import Janus.CSE.Types (CSEExp)
import Janus.Data.BTreeMap (BTreeMap)
import Janus.Data.BTreeSet (BTreeSet)
import Janus.Data.BitSet (BitSet)
import Janus.Data.BitTree (BitTree)
import Janus.Data.BloomFilter (BloomFilter)
import Janus.Data.ClockCache (ClockCache)
import Janus.Data.CsrGraph (CsrGraph)
import Janus.Data.Deque (Deque)
import Janus.Data.DisjointSet (DisjointSet)
import Janus.Data.DynVec (DynVec)
import Janus.Data.EytzingerVec (EytzingerVec)
import Janus.Data.FlatIntervalMap (FlatIntervalMap)
import Janus.Data.FlatMap (FlatMap)
import Janus.Data.FlatSet (FlatSet)
import Janus.Data.HashMap (HashMap)
import Janus.Data.HashSet (HashSet)
import Janus.Data.IndexMap (IndexMap)
import Janus.Data.IndexSet (IndexSet)
import Janus.Data.IndexedPQ (IndexedPQ)
import Janus.Data.Interner (Interner, Symbol)
import Janus.Data.IntrusiveHeap (IntrusiveHeap)
import Janus.Data.IntrusiveList (IntrusiveList)
import Janus.Data.LruCache (LruCache)
import Janus.Data.Pool (Pool, PoolHandle)
import Janus.Data.PriorityQueue (MinHeap, PriorityQueue)
import Janus.Data.RingBuffer (RingBuffer)
import Janus.Data.SlotMap (SlotKey, SlotMap)
import Janus.Data.SmallStack (SmallStack)
import Janus.Data.SmallVec (SmallVec)
import Janus.Data.SparseSet (SparseSet)
import Janus.Data.SparseVec (SparseVec)
import Janus.Data.StableVec (StableVec)
import Janus.Data.Stack (Stack)
import Janus.Data.TimerWheel (TimerWheel)
import Janus.Data.XorFilter (XorFilter)

--------------------------------------------------------------------------------
-- Janus.Alloc.* Handles & Records
--------------------------------------------------------------------------------

instance CSEReturn e (Arena (CSEExp e))

instance CSETargetReturn e (Arena (CSEExp e)) where
  type CSETargetVal e (Arena (CSEExp e)) = Arena e

instance (GCSEHKD e (Rep (Arena (CSEExp e))) (Rep (Arena e))) => CSEParam (Arena (CSEExp e)) where
  type CSEParamExp (Arena (CSEExp e)) = e
  type CSEParamTarget (Arena (CSEExp e)) = Arena e

instance CSEReturn e (ArenaMark (CSEExp e))

instance CSETargetReturn e (ArenaMark (CSEExp e)) where
  type CSETargetVal e (ArenaMark (CSEExp e)) = ArenaMark e

instance (GCSEHKD e (Rep (ArenaMark (CSEExp e))) (Rep (ArenaMark e))) => CSEParam (ArenaMark (CSEExp e)) where
  type CSEParamExp (ArenaMark (CSEExp e)) = e
  type CSEParamTarget (ArenaMark (CSEExp e)) = ArenaMark e

instance CSEReturn e (FixedBuf (CSEExp e))

instance CSETargetReturn e (FixedBuf (CSEExp e)) where
  type CSETargetVal e (FixedBuf (CSEExp e)) = FixedBuf e

instance (GCSEHKD e (Rep (FixedBuf (CSEExp e))) (Rep (FixedBuf e))) => CSEParam (FixedBuf (CSEExp e)) where
  type CSEParamExp (FixedBuf (CSEExp e)) = e
  type CSEParamTarget (FixedBuf (CSEExp e)) = FixedBuf e

instance CSEReturn e (Monotonic (CSEExp e))

instance CSETargetReturn e (Monotonic (CSEExp e)) where
  type CSETargetVal e (Monotonic (CSEExp e)) = Monotonic e

instance (GCSEHKD e (Rep (Monotonic (CSEExp e))) (Rep (Monotonic e))) => CSEParam (Monotonic (CSEExp e)) where
  type CSEParamExp (Monotonic (CSEExp e)) = e
  type CSEParamTarget (Monotonic (CSEExp e)) = Monotonic e

instance CSEReturn e (MemPool (CSEExp e))

instance CSETargetReturn e (MemPool (CSEExp e)) where
  type CSETargetVal e (MemPool (CSEExp e)) = MemPool e

instance (GCSEHKD e (Rep (MemPool (CSEExp e))) (Rep (MemPool e))) => CSEParam (MemPool (CSEExp e)) where
  type CSEParamExp (MemPool (CSEExp e)) = e
  type CSEParamTarget (MemPool (CSEExp e)) = MemPool e

instance CSEReturn e (Recycler (CSEExp e))

instance CSETargetReturn e (Recycler (CSEExp e)) where
  type CSETargetVal e (Recycler (CSEExp e)) = Recycler e

instance (GCSEHKD e (Rep (Recycler (CSEExp e))) (Rep (Recycler e))) => CSEParam (Recycler (CSEExp e)) where
  type CSEParamExp (Recycler (CSEExp e)) = e
  type CSEParamTarget (Recycler (CSEExp e)) = Recycler e

instance CSEReturn e (AllocTracker (CSEExp e))

instance CSETargetReturn e (AllocTracker (CSEExp e)) where
  type CSETargetVal e (AllocTracker (CSEExp e)) = AllocTracker e

instance (GCSEHKD e (Rep (AllocTracker (CSEExp e))) (Rep (AllocTracker e))) => CSEParam (AllocTracker (CSEExp e)) where
  type CSEParamExp (AllocTracker (CSEExp e)) = e
  type CSEParamTarget (AllocTracker (CSEExp e)) = AllocTracker e

instance CSEReturn e (AllocSnapshot (CSEExp e))

instance CSETargetReturn e (AllocSnapshot (CSEExp e)) where
  type CSETargetVal e (AllocSnapshot (CSEExp e)) = AllocSnapshot e

instance (GCSEHKD e (Rep (AllocSnapshot (CSEExp e))) (Rep (AllocSnapshot e))) => CSEParam (AllocSnapshot (CSEExp e)) where
  type CSEParamExp (AllocSnapshot (CSEExp e)) = e
  type CSEParamTarget (AllocSnapshot (CSEExp e)) = AllocSnapshot e

--------------------------------------------------------------------------------
-- Janus.Data.* Container Handles & Keys
--------------------------------------------------------------------------------

instance CSEReturn e (BTreeMap (CSEExp e) k v)

instance CSETargetReturn e (BTreeMap (CSEExp e) k v) where
  type CSETargetVal e (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance (GCSEHKD e (Rep (BTreeMap (CSEExp e) k v)) (Rep (BTreeMap e k v))) => CSEParam (BTreeMap (CSEExp e) k v) where
  type CSEParamExp (BTreeMap (CSEExp e) k v) = e
  type CSEParamTarget (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance CSEReturn e (BTreeSet (CSEExp e) k)

instance CSETargetReturn e (BTreeSet (CSEExp e) k) where
  type CSETargetVal e (BTreeSet (CSEExp e) k) = BTreeSet e k

instance (GCSEHKD e (Rep (BTreeSet (CSEExp e) k)) (Rep (BTreeSet e k))) => CSEParam (BTreeSet (CSEExp e) k) where
  type CSEParamExp (BTreeSet (CSEExp e) k) = e
  type CSEParamTarget (BTreeSet (CSEExp e) k) = BTreeSet e k

instance CSEReturn e (BitSet (CSEExp e))

instance CSETargetReturn e (BitSet (CSEExp e)) where
  type CSETargetVal e (BitSet (CSEExp e)) = BitSet e

instance (GCSEHKD e (Rep (BitSet (CSEExp e))) (Rep (BitSet e))) => CSEParam (BitSet (CSEExp e)) where
  type CSEParamExp (BitSet (CSEExp e)) = e
  type CSEParamTarget (BitSet (CSEExp e)) = BitSet e

instance CSEReturn e (BitTree (CSEExp e))

instance CSETargetReturn e (BitTree (CSEExp e)) where
  type CSETargetVal e (BitTree (CSEExp e)) = BitTree e

instance (GCSEHKD e (Rep (BitTree (CSEExp e))) (Rep (BitTree e))) => CSEParam (BitTree (CSEExp e)) where
  type CSEParamExp (BitTree (CSEExp e)) = e
  type CSEParamTarget (BitTree (CSEExp e)) = BitTree e

instance CSEReturn e (BloomFilter (CSEExp e) k)

instance CSETargetReturn e (BloomFilter (CSEExp e) k) where
  type CSETargetVal e (BloomFilter (CSEExp e) k) = BloomFilter e k

instance (GCSEHKD e (Rep (BloomFilter (CSEExp e) k)) (Rep (BloomFilter e k))) => CSEParam (BloomFilter (CSEExp e) k) where
  type CSEParamExp (BloomFilter (CSEExp e) k) = e
  type CSEParamTarget (BloomFilter (CSEExp e) k) = BloomFilter e k

instance CSEReturn e (ClockCache (CSEExp e) k v)

instance CSETargetReturn e (ClockCache (CSEExp e) k v) where
  type CSETargetVal e (ClockCache (CSEExp e) k v) = ClockCache e k v

instance (GCSEHKD e (Rep (ClockCache (CSEExp e) k v)) (Rep (ClockCache e k v))) => CSEParam (ClockCache (CSEExp e) k v) where
  type CSEParamExp (ClockCache (CSEExp e) k v) = e
  type CSEParamTarget (ClockCache (CSEExp e) k v) = ClockCache e k v

instance CSEReturn e (CsrGraph (CSEExp e) w)

instance CSETargetReturn e (CsrGraph (CSEExp e) w) where
  type CSETargetVal e (CsrGraph (CSEExp e) w) = CsrGraph e w

instance (GCSEHKD e (Rep (CsrGraph (CSEExp e) w)) (Rep (CsrGraph e w))) => CSEParam (CsrGraph (CSEExp e) w) where
  type CSEParamExp (CsrGraph (CSEExp e) w) = e
  type CSEParamTarget (CsrGraph (CSEExp e) w) = CsrGraph e w

instance CSEReturn e (Deque (CSEExp e) a)

instance CSETargetReturn e (Deque (CSEExp e) a) where
  type CSETargetVal e (Deque (CSEExp e) a) = Deque e a

instance (GCSEHKD e (Rep (Deque (CSEExp e) a)) (Rep (Deque e a))) => CSEParam (Deque (CSEExp e) a) where
  type CSEParamExp (Deque (CSEExp e) a) = e
  type CSEParamTarget (Deque (CSEExp e) a) = Deque e a

instance CSEReturn e (DisjointSet (CSEExp e))

instance CSETargetReturn e (DisjointSet (CSEExp e)) where
  type CSETargetVal e (DisjointSet (CSEExp e)) = DisjointSet e

instance (GCSEHKD e (Rep (DisjointSet (CSEExp e))) (Rep (DisjointSet e))) => CSEParam (DisjointSet (CSEExp e)) where
  type CSEParamExp (DisjointSet (CSEExp e)) = e
  type CSEParamTarget (DisjointSet (CSEExp e)) = DisjointSet e

instance CSEReturn e (DynVec (CSEExp e) a)

instance CSETargetReturn e (DynVec (CSEExp e) a) where
  type CSETargetVal e (DynVec (CSEExp e) a) = DynVec e a

instance (GCSEHKD e (Rep (DynVec (CSEExp e) a)) (Rep (DynVec e a))) => CSEParam (DynVec (CSEExp e) a) where
  type CSEParamExp (DynVec (CSEExp e) a) = e
  type CSEParamTarget (DynVec (CSEExp e) a) = DynVec e a

instance CSEReturn e (EytzingerVec (CSEExp e) k)

instance CSETargetReturn e (EytzingerVec (CSEExp e) k) where
  type CSETargetVal e (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance (GCSEHKD e (Rep (EytzingerVec (CSEExp e) k)) (Rep (EytzingerVec e k))) => CSEParam (EytzingerVec (CSEExp e) k) where
  type CSEParamExp (EytzingerVec (CSEExp e) k) = e
  type CSEParamTarget (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance CSEReturn e (FlatIntervalMap (CSEExp e) k v)

instance CSETargetReturn e (FlatIntervalMap (CSEExp e) k v) where
  type CSETargetVal e (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance (GCSEHKD e (Rep (FlatIntervalMap (CSEExp e) k v)) (Rep (FlatIntervalMap e k v))) => CSEParam (FlatIntervalMap (CSEExp e) k v) where
  type CSEParamExp (FlatIntervalMap (CSEExp e) k v) = e
  type CSEParamTarget (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance CSEReturn e (FlatMap (CSEExp e) k v)

instance CSETargetReturn e (FlatMap (CSEExp e) k v) where
  type CSETargetVal e (FlatMap (CSEExp e) k v) = FlatMap e k v

instance (GCSEHKD e (Rep (FlatMap (CSEExp e) k v)) (Rep (FlatMap e k v))) => CSEParam (FlatMap (CSEExp e) k v) where
  type CSEParamExp (FlatMap (CSEExp e) k v) = e
  type CSEParamTarget (FlatMap (CSEExp e) k v) = FlatMap e k v

instance CSEReturn e (FlatSet (CSEExp e) k)

instance CSETargetReturn e (FlatSet (CSEExp e) k) where
  type CSETargetVal e (FlatSet (CSEExp e) k) = FlatSet e k

instance (GCSEHKD e (Rep (FlatSet (CSEExp e) k)) (Rep (FlatSet e k))) => CSEParam (FlatSet (CSEExp e) k) where
  type CSEParamExp (FlatSet (CSEExp e) k) = e
  type CSEParamTarget (FlatSet (CSEExp e) k) = FlatSet e k

instance CSEReturn e (HashMap (CSEExp e) k v)

instance CSETargetReturn e (HashMap (CSEExp e) k v) where
  type CSETargetVal e (HashMap (CSEExp e) k v) = HashMap e k v

instance (GCSEHKD e (Rep (HashMap (CSEExp e) k v)) (Rep (HashMap e k v))) => CSEParam (HashMap (CSEExp e) k v) where
  type CSEParamExp (HashMap (CSEExp e) k v) = e
  type CSEParamTarget (HashMap (CSEExp e) k v) = HashMap e k v

instance CSEReturn e (HashSet (CSEExp e) k)

instance CSETargetReturn e (HashSet (CSEExp e) k) where
  type CSETargetVal e (HashSet (CSEExp e) k) = HashSet e k

instance (GCSEHKD e (Rep (HashSet (CSEExp e) k)) (Rep (HashSet e k))) => CSEParam (HashSet (CSEExp e) k) where
  type CSEParamExp (HashSet (CSEExp e) k) = e
  type CSEParamTarget (HashSet (CSEExp e) k) = HashSet e k

instance CSEReturn e (IndexMap (CSEExp e) k v)

instance CSETargetReturn e (IndexMap (CSEExp e) k v) where
  type CSETargetVal e (IndexMap (CSEExp e) k v) = IndexMap e k v

instance (GCSEHKD e (Rep (IndexMap (CSEExp e) k v)) (Rep (IndexMap e k v))) => CSEParam (IndexMap (CSEExp e) k v) where
  type CSEParamExp (IndexMap (CSEExp e) k v) = e
  type CSEParamTarget (IndexMap (CSEExp e) k v) = IndexMap e k v

instance CSEReturn e (IndexSet (CSEExp e) k)

instance CSETargetReturn e (IndexSet (CSEExp e) k) where
  type CSETargetVal e (IndexSet (CSEExp e) k) = IndexSet e k

instance (GCSEHKD e (Rep (IndexSet (CSEExp e) k)) (Rep (IndexSet e k))) => CSEParam (IndexSet (CSEExp e) k) where
  type CSEParamExp (IndexSet (CSEExp e) k) = e
  type CSEParamTarget (IndexSet (CSEExp e) k) = IndexSet e k

instance CSEReturn e (IndexedPQ (CSEExp e) p v)

instance CSETargetReturn e (IndexedPQ (CSEExp e) p v) where
  type CSETargetVal e (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance (GCSEHKD e (Rep (IndexedPQ (CSEExp e) p v)) (Rep (IndexedPQ e p v))) => CSEParam (IndexedPQ (CSEExp e) p v) where
  type CSEParamExp (IndexedPQ (CSEExp e) p v) = e
  type CSEParamTarget (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance CSEReturn e (Interner (CSEExp e))

instance CSETargetReturn e (Interner (CSEExp e)) where
  type CSETargetVal e (Interner (CSEExp e)) = Interner e

instance (GCSEHKD e (Rep (Interner (CSEExp e))) (Rep (Interner e))) => CSEParam (Interner (CSEExp e)) where
  type CSEParamExp (Interner (CSEExp e)) = e
  type CSEParamTarget (Interner (CSEExp e)) = Interner e

instance CSEReturn e (Symbol (CSEExp e))

instance CSETargetReturn e (Symbol (CSEExp e)) where
  type CSETargetVal e (Symbol (CSEExp e)) = Symbol e

instance (GCSEHKD e (Rep (Symbol (CSEExp e))) (Rep (Symbol e))) => CSEParam (Symbol (CSEExp e)) where
  type CSEParamExp (Symbol (CSEExp e)) = e
  type CSEParamTarget (Symbol (CSEExp e)) = Symbol e

instance CSEReturn e (IntrusiveHeap s k (CSEExp e))

instance CSETargetReturn e (IntrusiveHeap s k (CSEExp e)) where
  type CSETargetVal e (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e

instance (GCSEHKD e (Rep (IntrusiveHeap s k (CSEExp e))) (Rep (IntrusiveHeap s k e))) => CSEParam (IntrusiveHeap s k (CSEExp e)) where
  type CSEParamExp (IntrusiveHeap s k (CSEExp e)) = e
  type CSEParamTarget (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e

instance CSEReturn e (IntrusiveList s (CSEExp e))

instance CSETargetReturn e (IntrusiveList s (CSEExp e)) where
  type CSETargetVal e (IntrusiveList s (CSEExp e)) = IntrusiveList s e

instance (GCSEHKD e (Rep (IntrusiveList s (CSEExp e))) (Rep (IntrusiveList s e))) => CSEParam (IntrusiveList s (CSEExp e)) where
  type CSEParamExp (IntrusiveList s (CSEExp e)) = e
  type CSEParamTarget (IntrusiveList s (CSEExp e)) = IntrusiveList s e

instance CSEReturn e (LruCache (CSEExp e) k v)

instance CSETargetReturn e (LruCache (CSEExp e) k v) where
  type CSETargetVal e (LruCache (CSEExp e) k v) = LruCache e k v

instance (GCSEHKD e (Rep (LruCache (CSEExp e) k v)) (Rep (LruCache e k v))) => CSEParam (LruCache (CSEExp e) k v) where
  type CSEParamExp (LruCache (CSEExp e) k v) = e
  type CSEParamTarget (LruCache (CSEExp e) k v) = LruCache e k v

instance CSEReturn e (Pool (CSEExp e) a)

instance CSETargetReturn e (Pool (CSEExp e) a) where
  type CSETargetVal e (Pool (CSEExp e) a) = Pool e a

instance (GCSEHKD e (Rep (Pool (CSEExp e) a)) (Rep (Pool e a))) => CSEParam (Pool (CSEExp e) a) where
  type CSEParamExp (Pool (CSEExp e) a) = e
  type CSEParamTarget (Pool (CSEExp e) a) = Pool e a

instance CSEReturn e (PoolHandle (CSEExp e))

instance CSETargetReturn e (PoolHandle (CSEExp e)) where
  type CSETargetVal e (PoolHandle (CSEExp e)) = PoolHandle e

instance (GCSEHKD e (Rep (PoolHandle (CSEExp e))) (Rep (PoolHandle e))) => CSEParam (PoolHandle (CSEExp e)) where
  type CSEParamExp (PoolHandle (CSEExp e)) = e
  type CSEParamTarget (PoolHandle (CSEExp e)) = PoolHandle e

instance CSEReturn e (PriorityQueue (CSEExp e) k v)

instance CSETargetReturn e (PriorityQueue (CSEExp e) k v) where
  type CSETargetVal e (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance (GCSEHKD e (Rep (PriorityQueue (CSEExp e) k v)) (Rep (PriorityQueue e k v))) => CSEParam (PriorityQueue (CSEExp e) k v) where
  type CSEParamExp (PriorityQueue (CSEExp e) k v) = e
  type CSEParamTarget (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance CSEReturn e (MinHeap (CSEExp e) k)

instance CSETargetReturn e (MinHeap (CSEExp e) k) where
  type CSETargetVal e (MinHeap (CSEExp e) k) = MinHeap e k

instance (GCSEHKD e (Rep (MinHeap (CSEExp e) k)) (Rep (MinHeap e k))) => CSEParam (MinHeap (CSEExp e) k) where
  type CSEParamExp (MinHeap (CSEExp e) k) = e
  type CSEParamTarget (MinHeap (CSEExp e) k) = MinHeap e k

instance CSEReturn e (RingBuffer (CSEExp e) a)

instance CSETargetReturn e (RingBuffer (CSEExp e) a) where
  type CSETargetVal e (RingBuffer (CSEExp e) a) = RingBuffer e a

instance (GCSEHKD e (Rep (RingBuffer (CSEExp e) a)) (Rep (RingBuffer e a))) => CSEParam (RingBuffer (CSEExp e) a) where
  type CSEParamExp (RingBuffer (CSEExp e) a) = e
  type CSEParamTarget (RingBuffer (CSEExp e) a) = RingBuffer e a

instance CSEReturn e (SlotMap (CSEExp e) a)

instance CSETargetReturn e (SlotMap (CSEExp e) a) where
  type CSETargetVal e (SlotMap (CSEExp e) a) = SlotMap e a

instance (GCSEHKD e (Rep (SlotMap (CSEExp e) a)) (Rep (SlotMap e a))) => CSEParam (SlotMap (CSEExp e) a) where
  type CSEParamExp (SlotMap (CSEExp e) a) = e
  type CSEParamTarget (SlotMap (CSEExp e) a) = SlotMap e a

instance CSEReturn e (SlotKey (CSEExp e))

instance CSETargetReturn e (SlotKey (CSEExp e)) where
  type CSETargetVal e (SlotKey (CSEExp e)) = SlotKey e

instance (GCSEHKD e (Rep (SlotKey (CSEExp e))) (Rep (SlotKey e))) => CSEParam (SlotKey (CSEExp e)) where
  type CSEParamExp (SlotKey (CSEExp e)) = e
  type CSEParamTarget (SlotKey (CSEExp e)) = SlotKey e

instance CSEReturn e (SmallStack n (CSEExp e) a)

instance CSETargetReturn e (SmallStack n (CSEExp e) a) where
  type CSETargetVal e (SmallStack n (CSEExp e) a) = SmallStack n e a

instance (GCSEHKD e (Rep (SmallStack n (CSEExp e) a)) (Rep (SmallStack n e a))) => CSEParam (SmallStack n (CSEExp e) a) where
  type CSEParamExp (SmallStack n (CSEExp e) a) = e
  type CSEParamTarget (SmallStack n (CSEExp e) a) = SmallStack n e a

instance CSEReturn e (SmallVec n (CSEExp e) a)

instance CSETargetReturn e (SmallVec n (CSEExp e) a) where
  type CSETargetVal e (SmallVec n (CSEExp e) a) = SmallVec n e a

instance (GCSEHKD e (Rep (SmallVec n (CSEExp e) a)) (Rep (SmallVec n e a))) => CSEParam (SmallVec n (CSEExp e) a) where
  type CSEParamExp (SmallVec n (CSEExp e) a) = e
  type CSEParamTarget (SmallVec n (CSEExp e) a) = SmallVec n e a

instance CSEReturn e (SparseSet (CSEExp e))

instance CSETargetReturn e (SparseSet (CSEExp e)) where
  type CSETargetVal e (SparseSet (CSEExp e)) = SparseSet e

instance (GCSEHKD e (Rep (SparseSet (CSEExp e))) (Rep (SparseSet e))) => CSEParam (SparseSet (CSEExp e)) where
  type CSEParamExp (SparseSet (CSEExp e)) = e
  type CSEParamTarget (SparseSet (CSEExp e)) = SparseSet e

instance CSEReturn e (SparseVec (CSEExp e) a)

instance CSETargetReturn e (SparseVec (CSEExp e) a) where
  type CSETargetVal e (SparseVec (CSEExp e) a) = SparseVec e a

instance (GCSEHKD e (Rep (SparseVec (CSEExp e) a)) (Rep (SparseVec e a))) => CSEParam (SparseVec (CSEExp e) a) where
  type CSEParamExp (SparseVec (CSEExp e) a) = e
  type CSEParamTarget (SparseVec (CSEExp e) a) = SparseVec e a

instance CSEReturn e (StableVec k (CSEExp e) a)

instance CSETargetReturn e (StableVec k (CSEExp e) a) where
  type CSETargetVal e (StableVec k (CSEExp e) a) = StableVec k e a

instance (GCSEHKD e (Rep (StableVec k (CSEExp e) a)) (Rep (StableVec k e a))) => CSEParam (StableVec k (CSEExp e) a) where
  type CSEParamExp (StableVec k (CSEExp e) a) = e
  type CSEParamTarget (StableVec k (CSEExp e) a) = StableVec k e a

instance CSEReturn e (Stack (CSEExp e) a)

instance CSETargetReturn e (Stack (CSEExp e) a) where
  type CSETargetVal e (Stack (CSEExp e) a) = Stack e a

instance (GCSEHKD e (Rep (Stack (CSEExp e) a)) (Rep (Stack e a))) => CSEParam (Stack (CSEExp e) a) where
  type CSEParamExp (Stack (CSEExp e) a) = e
  type CSEParamTarget (Stack (CSEExp e) a) = Stack e a

instance CSEReturn e (TimerWheel s (CSEExp e))

instance CSETargetReturn e (TimerWheel s (CSEExp e)) where
  type CSETargetVal e (TimerWheel s (CSEExp e)) = TimerWheel s e

instance (GCSEHKD e (Rep (TimerWheel s (CSEExp e))) (Rep (TimerWheel s e))) => CSEParam (TimerWheel s (CSEExp e)) where
  type CSEParamExp (TimerWheel s (CSEExp e)) = e
  type CSEParamTarget (TimerWheel s (CSEExp e)) = TimerWheel s e

instance CSEReturn e (XorFilter (CSEExp e) k)

instance CSETargetReturn e (XorFilter (CSEExp e) k) where
  type CSETargetVal e (XorFilter (CSEExp e) k) = XorFilter e k

instance (GCSEHKD e (Rep (XorFilter (CSEExp e) k)) (Rep (XorFilter e k))) => CSEParam (XorFilter (CSEExp e) k) where
  type CSEParamExp (XorFilter (CSEExp e) k) = e
  type CSEParamTarget (XorFilter (CSEExp e) k) = XorFilter e k
