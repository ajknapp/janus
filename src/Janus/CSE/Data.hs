{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'CSEReturn', 'CSETargetReturn', 'CSEParam', 'JanusCFixParam',
-- 'JanusCFixMonRet', 'CSEFixParam', and 'CSEFixMonRet' instances for all
-- @Janus.Alloc.*@ allocator handles, @Janus.Data.Layout@ buffer handles, and
-- @Janus.Data.*@ container handles, derived via 'Generic' default signatures.
module Janus.CSE.Data () where

import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic, Rep)
import Janus.Alloc.Arena (Arena, ArenaMark)
import Janus.Alloc.Fixed (FixedBuf)
import Janus.Alloc.Monotonic (Monotonic)
import Janus.Alloc.Pool (MemPool)
import Janus.Alloc.Recycling (Recycler)
import Janus.Alloc.Stats (AllocSnapshot, AllocTracker)
import Janus.Backend.C.Raw (JanusCT)
import Janus.CSE.Instances (CSEFixMonRet (..), CSEFixParam (..), GCSEFixLeaves (gFixTypes))
import Janus.CSE.Optimize
  ( CSEParam (..),
    CSEReturn,
    CSETargetReturn (..),
    GCSEHKD,
    GCSEMap,
    GCSETraverse,
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
import Janus.Data.HKD (PtrOf, PtrStruct)
import Janus.Data.HashMap (HashMap)
import Janus.Data.HashSet (HashSet)
import Janus.Data.IndexMap (IndexMap)
import Janus.Data.IndexSet (IndexSet)
import Janus.Data.IndexedPQ (IndexedPQ)
import Janus.Data.Interner (Interner, Symbol)
import Janus.Data.IntrusiveHeap (IntrusiveHeap)
import Janus.Data.IntrusiveList (IntrusiveList)
import Janus.Data.Layout (AoSBuf, AoSoABuf, SOAOABuf, ScalarBuf)
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
import Janus.Fix (GJanusCFixLeaves (gFixLeafTypes), JanusCFixMonRet (..), JanusCFixParam (..))
import qualified Type.Reflection as TR

--------------------------------------------------------------------------------
-- Janus.Data.Layout Buffer Handles (`ScalarBuf`, `AoSBuf`, `PtrStruct`, `AoSoABuf`, `SOAOABuf`)
--------------------------------------------------------------------------------

instance CSEReturn e (ScalarBuf a (CSEExp e))

instance CSETargetReturn e (ScalarBuf a (CSEExp e)) where
  type CSETargetVal e (ScalarBuf a (CSEExp e)) = ScalarBuf a e

instance (GCSEHKD e (Rep (ScalarBuf a (CSEExp e))) (Rep (ScalarBuf a e))) => CSEParam (ScalarBuf a (CSEExp e)) where
  type CSEParamExp (ScalarBuf a (CSEExp e)) = e
  type CSEParamTarget (ScalarBuf a (CSEExp e)) = ScalarBuf a e

instance (GJanusCFixLeaves t (Rep (ScalarBuf a (JanusCT t)))) => JanusCFixParam t (ScalarBuf a (JanusCT t))

instance (GJanusCFixLeaves t (Rep (ScalarBuf a (JanusCT t)))) => JanusCFixMonRet t (ScalarBuf a (JanusCT t))

instance (GCSEFixLeaves e (Rep (ScalarBuf a (CSEExp e))) (Rep (ScalarBuf a e))) => CSEFixParam e (ScalarBuf a (CSEExp e)) where
  type CSEFixParamTarget e (ScalarBuf a (CSEExp e)) = ScalarBuf a e

instance (GCSEFixLeaves e (Rep (ScalarBuf a (CSEExp e))) (Rep (ScalarBuf a e))) => CSEFixMonRet e (ScalarBuf a (CSEExp e)) where
  type CSEFixRetTarget e (ScalarBuf a (CSEExp e)) = ScalarBuf a e

instance CSEReturn e (AoSBuf s (CSEExp e))

instance CSETargetReturn e (AoSBuf s (CSEExp e)) where
  type CSETargetVal e (AoSBuf s (CSEExp e)) = AoSBuf s e

instance (GCSEHKD e (Rep (AoSBuf s (CSEExp e))) (Rep (AoSBuf s e))) => CSEParam (AoSBuf s (CSEExp e)) where
  type CSEParamExp (AoSBuf s (CSEExp e)) = e
  type CSEParamTarget (AoSBuf s (CSEExp e)) = AoSBuf s e

instance (GJanusCFixLeaves t (Rep (AoSBuf s (JanusCT t)))) => JanusCFixParam t (AoSBuf s (JanusCT t))

instance (GJanusCFixLeaves t (Rep (AoSBuf s (JanusCT t)))) => JanusCFixMonRet t (AoSBuf s (JanusCT t))

instance (GCSEFixLeaves e (Rep (AoSBuf s (CSEExp e))) (Rep (AoSBuf s e))) => CSEFixParam e (AoSBuf s (CSEExp e)) where
  type CSEFixParamTarget e (AoSBuf s (CSEExp e)) = AoSBuf s e

instance (GCSEFixLeaves e (Rep (AoSBuf s (CSEExp e))) (Rep (AoSBuf s e))) => CSEFixMonRet e (AoSBuf s (CSEExp e)) where
  type CSEFixRetTarget e (AoSBuf s (CSEExp e)) = AoSBuf s e

instance
  (Generic (sub (PtrOf (CSEExp e))), GCSETraverse e (Rep (sub (PtrOf (CSEExp e))))) =>
  CSEReturn e (PtrStruct sub (CSEExp e))

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSETraverse e (Rep (sub (PtrOf (CSEExp e)))),
    GCSEMap e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  CSETargetReturn e (PtrStruct sub (CSEExp e))
  where
  type CSETargetVal e (PtrStruct sub (CSEExp e)) = PtrStruct sub e

instance
  ( Generic (sub (PtrOf (CSEExp e))),
    Generic (sub (PtrOf e)),
    GCSEHKD e (Rep (sub (PtrOf (CSEExp e)))) (Rep (sub (PtrOf e)))
  ) =>
  CSEParam (PtrStruct sub (CSEExp e))
  where
  type CSEParamExp (PtrStruct sub (CSEExp e)) = e
  type CSEParamTarget (PtrStruct sub (CSEExp e)) = PtrStruct sub e

instance CSEReturn e (AoSoABuf w s (CSEExp e))

instance CSETargetReturn e (AoSoABuf w s (CSEExp e)) where
  type CSETargetVal e (AoSoABuf w s (CSEExp e)) = AoSoABuf w s e

instance (GCSEHKD e (Rep (AoSoABuf w s (CSEExp e))) (Rep (AoSoABuf w s e))) => CSEParam (AoSoABuf w s (CSEExp e)) where
  type CSEParamExp (AoSoABuf w s (CSEExp e)) = e
  type CSEParamTarget (AoSoABuf w s (CSEExp e)) = AoSoABuf w s e

instance (GJanusCFixLeaves t (Rep (AoSoABuf w s (JanusCT t)))) => JanusCFixParam t (AoSoABuf w s (JanusCT t))

instance (GJanusCFixLeaves t (Rep (AoSoABuf w s (JanusCT t)))) => JanusCFixMonRet t (AoSoABuf w s (JanusCT t))

instance (GCSEFixLeaves e (Rep (AoSoABuf w s (CSEExp e))) (Rep (AoSoABuf w s e))) => CSEFixParam e (AoSoABuf w s (CSEExp e)) where
  type CSEFixParamTarget e (AoSoABuf w s (CSEExp e)) = AoSoABuf w s e

instance (GCSEFixLeaves e (Rep (AoSoABuf w s (CSEExp e))) (Rep (AoSoABuf w s e))) => CSEFixMonRet e (AoSoABuf w s (CSEExp e)) where
  type CSEFixRetTarget e (AoSoABuf w s (CSEExp e)) = AoSoABuf w s e

instance CSEReturn e (SOAOABuf w outer (CSEExp e))

instance CSETargetReturn e (SOAOABuf w outer (CSEExp e)) where
  type CSETargetVal e (SOAOABuf w outer (CSEExp e)) = SOAOABuf w outer e

instance (GCSEHKD e (Rep (SOAOABuf w outer (CSEExp e))) (Rep (SOAOABuf w outer e))) => CSEParam (SOAOABuf w outer (CSEExp e)) where
  type CSEParamExp (SOAOABuf w outer (CSEExp e)) = e
  type CSEParamTarget (SOAOABuf w outer (CSEExp e)) = SOAOABuf w outer e

instance
  (Typeable w, Typeable outer, GJanusCFixLeaves t (Rep (SOAOABuf w outer (JanusCT t)))) =>
  JanusCFixParam t (SOAOABuf w outer (JanusCT t))
  where
  jcfixParamTypes _ _ =
    [ typeRep (Proxy @w),
      typeRep (Proxy @outer),
      typeRep (Proxy @(Ptr (Ptr Word8)))
    ]

instance
  (Typeable w, Typeable outer, GJanusCFixLeaves t (Rep (SOAOABuf w outer (JanusCT t)))) =>
  JanusCFixMonRet t (SOAOABuf w outer (JanusCT t))
  where
  jcfixRetTypes _ _ =
    [ typeRep (Proxy @w),
      typeRep (Proxy @outer),
      typeRep (Proxy @(Ptr (Ptr Word8)))
    ]

instance
  (Typeable w, Typeable outer, GCSEFixLeaves e (Rep (SOAOABuf w outer (CSEExp e))) (Rep (SOAOABuf w outer e))) =>
  CSEFixParam e (SOAOABuf w outer (CSEExp e))
  where
  type CSEFixParamTarget e (SOAOABuf w outer (CSEExp e)) = SOAOABuf w outer e
  cseFixParamTypes _ _ =
    [ TR.SomeTypeRep (TR.typeRep @w),
      TR.SomeTypeRep (TR.typeRep @outer),
      TR.SomeTypeRep (TR.typeRep @(Ptr (Ptr Word8)))
    ]

instance
  (Typeable w, Typeable outer, GCSEFixLeaves e (Rep (SOAOABuf w outer (CSEExp e))) (Rep (SOAOABuf w outer e))) =>
  CSEFixMonRet e (SOAOABuf w outer (CSEExp e))
  where
  type CSEFixRetTarget e (SOAOABuf w outer (CSEExp e)) = SOAOABuf w outer e
  cseFixRetTypes _ _ =
    [ TR.SomeTypeRep (TR.typeRep @w),
      TR.SomeTypeRep (TR.typeRep @outer),
      TR.SomeTypeRep (TR.typeRep @(Ptr (Ptr Word8)))
    ]

--------------------------------------------------------------------------------
-- Janus.Alloc.* Handles & Records
--------------------------------------------------------------------------------

instance CSEReturn e (Arena (CSEExp e))

instance CSETargetReturn e (Arena (CSEExp e)) where
  type CSETargetVal e (Arena (CSEExp e)) = Arena e

instance (GCSEHKD e (Rep (Arena (CSEExp e))) (Rep (Arena e))) => CSEParam (Arena (CSEExp e)) where
  type CSEParamExp (Arena (CSEExp e)) = e
  type CSEParamTarget (Arena (CSEExp e)) = Arena e

instance (GJanusCFixLeaves t (Rep (Arena (JanusCT t)))) => JanusCFixParam t (Arena (JanusCT t))

instance (GJanusCFixLeaves t (Rep (Arena (JanusCT t)))) => JanusCFixMonRet t (Arena (JanusCT t))

instance (GCSEFixLeaves e (Rep (Arena (CSEExp e))) (Rep (Arena e))) => CSEFixParam e (Arena (CSEExp e)) where
  type CSEFixParamTarget e (Arena (CSEExp e)) = Arena e

instance (GCSEFixLeaves e (Rep (Arena (CSEExp e))) (Rep (Arena e))) => CSEFixMonRet e (Arena (CSEExp e)) where
  type CSEFixRetTarget e (Arena (CSEExp e)) = Arena e

instance CSEReturn e (ArenaMark (CSEExp e))

instance CSETargetReturn e (ArenaMark (CSEExp e)) where
  type CSETargetVal e (ArenaMark (CSEExp e)) = ArenaMark e

instance (GCSEHKD e (Rep (ArenaMark (CSEExp e))) (Rep (ArenaMark e))) => CSEParam (ArenaMark (CSEExp e)) where
  type CSEParamExp (ArenaMark (CSEExp e)) = e
  type CSEParamTarget (ArenaMark (CSEExp e)) = ArenaMark e

instance (GJanusCFixLeaves t (Rep (ArenaMark (JanusCT t)))) => JanusCFixParam t (ArenaMark (JanusCT t))

instance (GJanusCFixLeaves t (Rep (ArenaMark (JanusCT t)))) => JanusCFixMonRet t (ArenaMark (JanusCT t))

instance (GCSEFixLeaves e (Rep (ArenaMark (CSEExp e))) (Rep (ArenaMark e))) => CSEFixParam e (ArenaMark (CSEExp e)) where
  type CSEFixParamTarget e (ArenaMark (CSEExp e)) = ArenaMark e

instance (GCSEFixLeaves e (Rep (ArenaMark (CSEExp e))) (Rep (ArenaMark e))) => CSEFixMonRet e (ArenaMark (CSEExp e)) where
  type CSEFixRetTarget e (ArenaMark (CSEExp e)) = ArenaMark e

instance CSEReturn e (FixedBuf (CSEExp e))

instance CSETargetReturn e (FixedBuf (CSEExp e)) where
  type CSETargetVal e (FixedBuf (CSEExp e)) = FixedBuf e

instance (GCSEHKD e (Rep (FixedBuf (CSEExp e))) (Rep (FixedBuf e))) => CSEParam (FixedBuf (CSEExp e)) where
  type CSEParamExp (FixedBuf (CSEExp e)) = e
  type CSEParamTarget (FixedBuf (CSEExp e)) = FixedBuf e

instance (GJanusCFixLeaves t (Rep (FixedBuf (JanusCT t)))) => JanusCFixParam t (FixedBuf (JanusCT t))

instance (GJanusCFixLeaves t (Rep (FixedBuf (JanusCT t)))) => JanusCFixMonRet t (FixedBuf (JanusCT t))

instance (GCSEFixLeaves e (Rep (FixedBuf (CSEExp e))) (Rep (FixedBuf e))) => CSEFixParam e (FixedBuf (CSEExp e)) where
  type CSEFixParamTarget e (FixedBuf (CSEExp e)) = FixedBuf e

instance (GCSEFixLeaves e (Rep (FixedBuf (CSEExp e))) (Rep (FixedBuf e))) => CSEFixMonRet e (FixedBuf (CSEExp e)) where
  type CSEFixRetTarget e (FixedBuf (CSEExp e)) = FixedBuf e

instance CSEReturn e (Monotonic (CSEExp e))

instance CSETargetReturn e (Monotonic (CSEExp e)) where
  type CSETargetVal e (Monotonic (CSEExp e)) = Monotonic e

instance (GCSEHKD e (Rep (Monotonic (CSEExp e))) (Rep (Monotonic e))) => CSEParam (Monotonic (CSEExp e)) where
  type CSEParamExp (Monotonic (CSEExp e)) = e
  type CSEParamTarget (Monotonic (CSEExp e)) = Monotonic e

instance (GJanusCFixLeaves t (Rep (Monotonic (JanusCT t)))) => JanusCFixParam t (Monotonic (JanusCT t))

instance (GJanusCFixLeaves t (Rep (Monotonic (JanusCT t)))) => JanusCFixMonRet t (Monotonic (JanusCT t))

instance (GCSEFixLeaves e (Rep (Monotonic (CSEExp e))) (Rep (Monotonic e))) => CSEFixParam e (Monotonic (CSEExp e)) where
  type CSEFixParamTarget e (Monotonic (CSEExp e)) = Monotonic e

instance (GCSEFixLeaves e (Rep (Monotonic (CSEExp e))) (Rep (Monotonic e))) => CSEFixMonRet e (Monotonic (CSEExp e)) where
  type CSEFixRetTarget e (Monotonic (CSEExp e)) = Monotonic e

instance CSEReturn e (MemPool (CSEExp e))

instance CSETargetReturn e (MemPool (CSEExp e)) where
  type CSETargetVal e (MemPool (CSEExp e)) = MemPool e

instance (GCSEHKD e (Rep (MemPool (CSEExp e))) (Rep (MemPool e))) => CSEParam (MemPool (CSEExp e)) where
  type CSEParamExp (MemPool (CSEExp e)) = e
  type CSEParamTarget (MemPool (CSEExp e)) = MemPool e

instance (GJanusCFixLeaves t (Rep (MemPool (JanusCT t)))) => JanusCFixParam t (MemPool (JanusCT t))

instance (GJanusCFixLeaves t (Rep (MemPool (JanusCT t)))) => JanusCFixMonRet t (MemPool (JanusCT t))

instance (GCSEFixLeaves e (Rep (MemPool (CSEExp e))) (Rep (MemPool e))) => CSEFixParam e (MemPool (CSEExp e)) where
  type CSEFixParamTarget e (MemPool (CSEExp e)) = MemPool e

instance (GCSEFixLeaves e (Rep (MemPool (CSEExp e))) (Rep (MemPool e))) => CSEFixMonRet e (MemPool (CSEExp e)) where
  type CSEFixRetTarget e (MemPool (CSEExp e)) = MemPool e

instance CSEReturn e (Recycler (CSEExp e))

instance CSETargetReturn e (Recycler (CSEExp e)) where
  type CSETargetVal e (Recycler (CSEExp e)) = Recycler e

instance (GCSEHKD e (Rep (Recycler (CSEExp e))) (Rep (Recycler e))) => CSEParam (Recycler (CSEExp e)) where
  type CSEParamExp (Recycler (CSEExp e)) = e
  type CSEParamTarget (Recycler (CSEExp e)) = Recycler e

instance (GJanusCFixLeaves t (Rep (Recycler (JanusCT t)))) => JanusCFixParam t (Recycler (JanusCT t))

instance (GJanusCFixLeaves t (Rep (Recycler (JanusCT t)))) => JanusCFixMonRet t (Recycler (JanusCT t))

instance (GCSEFixLeaves e (Rep (Recycler (CSEExp e))) (Rep (Recycler e))) => CSEFixParam e (Recycler (CSEExp e)) where
  type CSEFixParamTarget e (Recycler (CSEExp e)) = Recycler e

instance (GCSEFixLeaves e (Rep (Recycler (CSEExp e))) (Rep (Recycler e))) => CSEFixMonRet e (Recycler (CSEExp e)) where
  type CSEFixRetTarget e (Recycler (CSEExp e)) = Recycler e

instance CSEReturn e (AllocTracker (CSEExp e))

instance CSETargetReturn e (AllocTracker (CSEExp e)) where
  type CSETargetVal e (AllocTracker (CSEExp e)) = AllocTracker e

instance (GCSEHKD e (Rep (AllocTracker (CSEExp e))) (Rep (AllocTracker e))) => CSEParam (AllocTracker (CSEExp e)) where
  type CSEParamExp (AllocTracker (CSEExp e)) = e
  type CSEParamTarget (AllocTracker (CSEExp e)) = AllocTracker e

instance (GJanusCFixLeaves t (Rep (AllocTracker (JanusCT t)))) => JanusCFixParam t (AllocTracker (JanusCT t))

instance (GJanusCFixLeaves t (Rep (AllocTracker (JanusCT t)))) => JanusCFixMonRet t (AllocTracker (JanusCT t))

instance (GCSEFixLeaves e (Rep (AllocTracker (CSEExp e))) (Rep (AllocTracker e))) => CSEFixParam e (AllocTracker (CSEExp e)) where
  type CSEFixParamTarget e (AllocTracker (CSEExp e)) = AllocTracker e

instance (GCSEFixLeaves e (Rep (AllocTracker (CSEExp e))) (Rep (AllocTracker e))) => CSEFixMonRet e (AllocTracker (CSEExp e)) where
  type CSEFixRetTarget e (AllocTracker (CSEExp e)) = AllocTracker e

instance CSEReturn e (AllocSnapshot (CSEExp e))

instance CSETargetReturn e (AllocSnapshot (CSEExp e)) where
  type CSETargetVal e (AllocSnapshot (CSEExp e)) = AllocSnapshot e

instance (GCSEHKD e (Rep (AllocSnapshot (CSEExp e))) (Rep (AllocSnapshot e))) => CSEParam (AllocSnapshot (CSEExp e)) where
  type CSEParamExp (AllocSnapshot (CSEExp e)) = e
  type CSEParamTarget (AllocSnapshot (CSEExp e)) = AllocSnapshot e

instance (GJanusCFixLeaves t (Rep (AllocSnapshot (JanusCT t)))) => JanusCFixParam t (AllocSnapshot (JanusCT t))

instance (GJanusCFixLeaves t (Rep (AllocSnapshot (JanusCT t)))) => JanusCFixMonRet t (AllocSnapshot (JanusCT t))

instance (GCSEFixLeaves e (Rep (AllocSnapshot (CSEExp e))) (Rep (AllocSnapshot e))) => CSEFixParam e (AllocSnapshot (CSEExp e)) where
  type CSEFixParamTarget e (AllocSnapshot (CSEExp e)) = AllocSnapshot e

instance (GCSEFixLeaves e (Rep (AllocSnapshot (CSEExp e))) (Rep (AllocSnapshot e))) => CSEFixMonRet e (AllocSnapshot (CSEExp e)) where
  type CSEFixRetTarget e (AllocSnapshot (CSEExp e)) = AllocSnapshot e

--------------------------------------------------------------------------------
-- Janus.Data.* Container Handles & Keys
--------------------------------------------------------------------------------

instance CSEReturn e (BTreeMap (CSEExp e) k v)

instance CSETargetReturn e (BTreeMap (CSEExp e) k v) where
  type CSETargetVal e (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance (GCSEHKD e (Rep (BTreeMap (CSEExp e) k v)) (Rep (BTreeMap e k v))) => CSEParam (BTreeMap (CSEExp e) k v) where
  type CSEParamExp (BTreeMap (CSEExp e) k v) = e
  type CSEParamTarget (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance (GJanusCFixLeaves t (Rep (BTreeMap (JanusCT t) k v))) => JanusCFixParam t (BTreeMap (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (BTreeMap (JanusCT t) k v))) => JanusCFixMonRet t (BTreeMap (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (BTreeMap (CSEExp e) k v)) (Rep (BTreeMap e k v))) => CSEFixParam e (BTreeMap (CSEExp e) k v) where
  type CSEFixParamTarget e (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance (GCSEFixLeaves e (Rep (BTreeMap (CSEExp e) k v)) (Rep (BTreeMap e k v))) => CSEFixMonRet e (BTreeMap (CSEExp e) k v) where
  type CSEFixRetTarget e (BTreeMap (CSEExp e) k v) = BTreeMap e k v

instance CSEReturn e (BTreeSet (CSEExp e) k)

instance CSETargetReturn e (BTreeSet (CSEExp e) k) where
  type CSETargetVal e (BTreeSet (CSEExp e) k) = BTreeSet e k

instance (GCSEHKD e (Rep (BTreeSet (CSEExp e) k)) (Rep (BTreeSet e k))) => CSEParam (BTreeSet (CSEExp e) k) where
  type CSEParamExp (BTreeSet (CSEExp e) k) = e
  type CSEParamTarget (BTreeSet (CSEExp e) k) = BTreeSet e k

instance (GJanusCFixLeaves t (Rep (BTreeSet (JanusCT t) k))) => JanusCFixParam t (BTreeSet (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (BTreeSet (JanusCT t) k))) => JanusCFixMonRet t (BTreeSet (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (BTreeSet (CSEExp e) k)) (Rep (BTreeSet e k))) => CSEFixParam e (BTreeSet (CSEExp e) k) where
  type CSEFixParamTarget e (BTreeSet (CSEExp e) k) = BTreeSet e k

instance (GCSEFixLeaves e (Rep (BTreeSet (CSEExp e) k)) (Rep (BTreeSet e k))) => CSEFixMonRet e (BTreeSet (CSEExp e) k) where
  type CSEFixRetTarget e (BTreeSet (CSEExp e) k) = BTreeSet e k

instance CSEReturn e (BitSet (CSEExp e))

instance CSETargetReturn e (BitSet (CSEExp e)) where
  type CSETargetVal e (BitSet (CSEExp e)) = BitSet e

instance (GCSEHKD e (Rep (BitSet (CSEExp e))) (Rep (BitSet e))) => CSEParam (BitSet (CSEExp e)) where
  type CSEParamExp (BitSet (CSEExp e)) = e
  type CSEParamTarget (BitSet (CSEExp e)) = BitSet e

instance (GJanusCFixLeaves t (Rep (BitSet (JanusCT t)))) => JanusCFixParam t (BitSet (JanusCT t))

instance (GJanusCFixLeaves t (Rep (BitSet (JanusCT t)))) => JanusCFixMonRet t (BitSet (JanusCT t))

instance (GCSEFixLeaves e (Rep (BitSet (CSEExp e))) (Rep (BitSet e))) => CSEFixParam e (BitSet (CSEExp e)) where
  type CSEFixParamTarget e (BitSet (CSEExp e)) = BitSet e

instance (GCSEFixLeaves e (Rep (BitSet (CSEExp e))) (Rep (BitSet e))) => CSEFixMonRet e (BitSet (CSEExp e)) where
  type CSEFixRetTarget e (BitSet (CSEExp e)) = BitSet e

instance CSEReturn e (BitTree (CSEExp e))

instance CSETargetReturn e (BitTree (CSEExp e)) where
  type CSETargetVal e (BitTree (CSEExp e)) = BitTree e

instance (GCSEHKD e (Rep (BitTree (CSEExp e))) (Rep (BitTree e))) => CSEParam (BitTree (CSEExp e)) where
  type CSEParamExp (BitTree (CSEExp e)) = e
  type CSEParamTarget (BitTree (CSEExp e)) = BitTree e

instance (GJanusCFixLeaves t (Rep (BitTree (JanusCT t)))) => JanusCFixParam t (BitTree (JanusCT t))

instance (GJanusCFixLeaves t (Rep (BitTree (JanusCT t)))) => JanusCFixMonRet t (BitTree (JanusCT t))

instance (GCSEFixLeaves e (Rep (BitTree (CSEExp e))) (Rep (BitTree e))) => CSEFixParam e (BitTree (CSEExp e)) where
  type CSEFixParamTarget e (BitTree (CSEExp e)) = BitTree e

instance (GCSEFixLeaves e (Rep (BitTree (CSEExp e))) (Rep (BitTree e))) => CSEFixMonRet e (BitTree (CSEExp e)) where
  type CSEFixRetTarget e (BitTree (CSEExp e)) = BitTree e

instance CSEReturn e (BloomFilter (CSEExp e) k)

instance CSETargetReturn e (BloomFilter (CSEExp e) k) where
  type CSETargetVal e (BloomFilter (CSEExp e) k) = BloomFilter e k

instance (GCSEHKD e (Rep (BloomFilter (CSEExp e) k)) (Rep (BloomFilter e k))) => CSEParam (BloomFilter (CSEExp e) k) where
  type CSEParamExp (BloomFilter (CSEExp e) k) = e
  type CSEParamTarget (BloomFilter (CSEExp e) k) = BloomFilter e k

instance (GJanusCFixLeaves t (Rep (BloomFilter (JanusCT t) k))) => JanusCFixParam t (BloomFilter (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (BloomFilter (JanusCT t) k))) => JanusCFixMonRet t (BloomFilter (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (BloomFilter (CSEExp e) k)) (Rep (BloomFilter e k))) => CSEFixParam e (BloomFilter (CSEExp e) k) where
  type CSEFixParamTarget e (BloomFilter (CSEExp e) k) = BloomFilter e k

instance (GCSEFixLeaves e (Rep (BloomFilter (CSEExp e) k)) (Rep (BloomFilter e k))) => CSEFixMonRet e (BloomFilter (CSEExp e) k) where
  type CSEFixRetTarget e (BloomFilter (CSEExp e) k) = BloomFilter e k

instance CSEReturn e (ClockCache (CSEExp e) k v)

instance CSETargetReturn e (ClockCache (CSEExp e) k v) where
  type CSETargetVal e (ClockCache (CSEExp e) k v) = ClockCache e k v

instance (GCSEHKD e (Rep (ClockCache (CSEExp e) k v)) (Rep (ClockCache e k v))) => CSEParam (ClockCache (CSEExp e) k v) where
  type CSEParamExp (ClockCache (CSEExp e) k v) = e
  type CSEParamTarget (ClockCache (CSEExp e) k v) = ClockCache e k v

instance (GJanusCFixLeaves t (Rep (ClockCache (JanusCT t) k v))) => JanusCFixParam t (ClockCache (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (ClockCache (JanusCT t) k v))) => JanusCFixMonRet t (ClockCache (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (ClockCache (CSEExp e) k v)) (Rep (ClockCache e k v))) => CSEFixParam e (ClockCache (CSEExp e) k v) where
  type CSEFixParamTarget e (ClockCache (CSEExp e) k v) = ClockCache e k v

instance (GCSEFixLeaves e (Rep (ClockCache (CSEExp e) k v)) (Rep (ClockCache e k v))) => CSEFixMonRet e (ClockCache (CSEExp e) k v) where
  type CSEFixRetTarget e (ClockCache (CSEExp e) k v) = ClockCache e k v

instance CSEReturn e (CsrGraph (CSEExp e) w)

instance CSETargetReturn e (CsrGraph (CSEExp e) w) where
  type CSETargetVal e (CsrGraph (CSEExp e) w) = CsrGraph e w

instance (GCSEHKD e (Rep (CsrGraph (CSEExp e) w)) (Rep (CsrGraph e w))) => CSEParam (CsrGraph (CSEExp e) w) where
  type CSEParamExp (CsrGraph (CSEExp e) w) = e
  type CSEParamTarget (CsrGraph (CSEExp e) w) = CsrGraph e w

instance (GJanusCFixLeaves t (Rep (CsrGraph (JanusCT t) w))) => JanusCFixParam t (CsrGraph (JanusCT t) w)

instance (GJanusCFixLeaves t (Rep (CsrGraph (JanusCT t) w))) => JanusCFixMonRet t (CsrGraph (JanusCT t) w)

instance (GCSEFixLeaves e (Rep (CsrGraph (CSEExp e) w)) (Rep (CsrGraph e w))) => CSEFixParam e (CsrGraph (CSEExp e) w) where
  type CSEFixParamTarget e (CsrGraph (CSEExp e) w) = CsrGraph e w

instance (GCSEFixLeaves e (Rep (CsrGraph (CSEExp e) w)) (Rep (CsrGraph e w))) => CSEFixMonRet e (CsrGraph (CSEExp e) w) where
  type CSEFixRetTarget e (CsrGraph (CSEExp e) w) = CsrGraph e w

instance CSEReturn e (Deque (CSEExp e) a)

instance CSETargetReturn e (Deque (CSEExp e) a) where
  type CSETargetVal e (Deque (CSEExp e) a) = Deque e a

instance (GCSEHKD e (Rep (Deque (CSEExp e) a)) (Rep (Deque e a))) => CSEParam (Deque (CSEExp e) a) where
  type CSEParamExp (Deque (CSEExp e) a) = e
  type CSEParamTarget (Deque (CSEExp e) a) = Deque e a

instance (GJanusCFixLeaves t (Rep (Deque (JanusCT t) a))) => JanusCFixParam t (Deque (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (Deque (JanusCT t) a))) => JanusCFixMonRet t (Deque (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (Deque (CSEExp e) a)) (Rep (Deque e a))) => CSEFixParam e (Deque (CSEExp e) a) where
  type CSEFixParamTarget e (Deque (CSEExp e) a) = Deque e a

instance (GCSEFixLeaves e (Rep (Deque (CSEExp e) a)) (Rep (Deque e a))) => CSEFixMonRet e (Deque (CSEExp e) a) where
  type CSEFixRetTarget e (Deque (CSEExp e) a) = Deque e a

instance CSEReturn e (DisjointSet (CSEExp e))

instance CSETargetReturn e (DisjointSet (CSEExp e)) where
  type CSETargetVal e (DisjointSet (CSEExp e)) = DisjointSet e

instance (GCSEHKD e (Rep (DisjointSet (CSEExp e))) (Rep (DisjointSet e))) => CSEParam (DisjointSet (CSEExp e)) where
  type CSEParamExp (DisjointSet (CSEExp e)) = e
  type CSEParamTarget (DisjointSet (CSEExp e)) = DisjointSet e

instance (GJanusCFixLeaves t (Rep (DisjointSet (JanusCT t)))) => JanusCFixParam t (DisjointSet (JanusCT t))

instance (GJanusCFixLeaves t (Rep (DisjointSet (JanusCT t)))) => JanusCFixMonRet t (DisjointSet (JanusCT t))

instance (GCSEFixLeaves e (Rep (DisjointSet (CSEExp e))) (Rep (DisjointSet e))) => CSEFixParam e (DisjointSet (CSEExp e)) where
  type CSEFixParamTarget e (DisjointSet (CSEExp e)) = DisjointSet e

instance (GCSEFixLeaves e (Rep (DisjointSet (CSEExp e))) (Rep (DisjointSet e))) => CSEFixMonRet e (DisjointSet (CSEExp e)) where
  type CSEFixRetTarget e (DisjointSet (CSEExp e)) = DisjointSet e

instance CSEReturn e (DynVec (CSEExp e) a)

instance CSETargetReturn e (DynVec (CSEExp e) a) where
  type CSETargetVal e (DynVec (CSEExp e) a) = DynVec e a

instance (GCSEHKD e (Rep (DynVec (CSEExp e) a)) (Rep (DynVec e a))) => CSEParam (DynVec (CSEExp e) a) where
  type CSEParamExp (DynVec (CSEExp e) a) = e
  type CSEParamTarget (DynVec (CSEExp e) a) = DynVec e a

instance (GJanusCFixLeaves t (Rep (DynVec (JanusCT t) a))) => JanusCFixParam t (DynVec (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (DynVec (JanusCT t) a))) => JanusCFixMonRet t (DynVec (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (DynVec (CSEExp e) a)) (Rep (DynVec e a))) => CSEFixParam e (DynVec (CSEExp e) a) where
  type CSEFixParamTarget e (DynVec (CSEExp e) a) = DynVec e a

instance (GCSEFixLeaves e (Rep (DynVec (CSEExp e) a)) (Rep (DynVec e a))) => CSEFixMonRet e (DynVec (CSEExp e) a) where
  type CSEFixRetTarget e (DynVec (CSEExp e) a) = DynVec e a

instance CSEReturn e (EytzingerVec (CSEExp e) k)

instance CSETargetReturn e (EytzingerVec (CSEExp e) k) where
  type CSETargetVal e (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance (GCSEHKD e (Rep (EytzingerVec (CSEExp e) k)) (Rep (EytzingerVec e k))) => CSEParam (EytzingerVec (CSEExp e) k) where
  type CSEParamExp (EytzingerVec (CSEExp e) k) = e
  type CSEParamTarget (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance (GJanusCFixLeaves t (Rep (EytzingerVec (JanusCT t) k))) => JanusCFixParam t (EytzingerVec (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (EytzingerVec (JanusCT t) k))) => JanusCFixMonRet t (EytzingerVec (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (EytzingerVec (CSEExp e) k)) (Rep (EytzingerVec e k))) => CSEFixParam e (EytzingerVec (CSEExp e) k) where
  type CSEFixParamTarget e (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance (GCSEFixLeaves e (Rep (EytzingerVec (CSEExp e) k)) (Rep (EytzingerVec e k))) => CSEFixMonRet e (EytzingerVec (CSEExp e) k) where
  type CSEFixRetTarget e (EytzingerVec (CSEExp e) k) = EytzingerVec e k

instance CSEReturn e (FlatIntervalMap (CSEExp e) k v)

instance CSETargetReturn e (FlatIntervalMap (CSEExp e) k v) where
  type CSETargetVal e (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance (GCSEHKD e (Rep (FlatIntervalMap (CSEExp e) k v)) (Rep (FlatIntervalMap e k v))) => CSEParam (FlatIntervalMap (CSEExp e) k v) where
  type CSEParamExp (FlatIntervalMap (CSEExp e) k v) = e
  type CSEParamTarget (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance (GJanusCFixLeaves t (Rep (FlatIntervalMap (JanusCT t) k v))) => JanusCFixParam t (FlatIntervalMap (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (FlatIntervalMap (JanusCT t) k v))) => JanusCFixMonRet t (FlatIntervalMap (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (FlatIntervalMap (CSEExp e) k v)) (Rep (FlatIntervalMap e k v))) => CSEFixParam e (FlatIntervalMap (CSEExp e) k v) where
  type CSEFixParamTarget e (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance (GCSEFixLeaves e (Rep (FlatIntervalMap (CSEExp e) k v)) (Rep (FlatIntervalMap e k v))) => CSEFixMonRet e (FlatIntervalMap (CSEExp e) k v) where
  type CSEFixRetTarget e (FlatIntervalMap (CSEExp e) k v) = FlatIntervalMap e k v

instance CSEReturn e (FlatMap (CSEExp e) k v)

instance CSETargetReturn e (FlatMap (CSEExp e) k v) where
  type CSETargetVal e (FlatMap (CSEExp e) k v) = FlatMap e k v

instance (GCSEHKD e (Rep (FlatMap (CSEExp e) k v)) (Rep (FlatMap e k v))) => CSEParam (FlatMap (CSEExp e) k v) where
  type CSEParamExp (FlatMap (CSEExp e) k v) = e
  type CSEParamTarget (FlatMap (CSEExp e) k v) = FlatMap e k v

instance (GJanusCFixLeaves t (Rep (FlatMap (JanusCT t) k v))) => JanusCFixParam t (FlatMap (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (FlatMap (JanusCT t) k v))) => JanusCFixMonRet t (FlatMap (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (FlatMap (CSEExp e) k v)) (Rep (FlatMap e k v))) => CSEFixParam e (FlatMap (CSEExp e) k v) where
  type CSEFixParamTarget e (FlatMap (CSEExp e) k v) = FlatMap e k v

instance (GCSEFixLeaves e (Rep (FlatMap (CSEExp e) k v)) (Rep (FlatMap e k v))) => CSEFixMonRet e (FlatMap (CSEExp e) k v) where
  type CSEFixRetTarget e (FlatMap (CSEExp e) k v) = FlatMap e k v

instance CSEReturn e (FlatSet (CSEExp e) k)

instance CSETargetReturn e (FlatSet (CSEExp e) k) where
  type CSETargetVal e (FlatSet (CSEExp e) k) = FlatSet e k

instance (GCSEHKD e (Rep (FlatSet (CSEExp e) k)) (Rep (FlatSet e k))) => CSEParam (FlatSet (CSEExp e) k) where
  type CSEParamExp (FlatSet (CSEExp e) k) = e
  type CSEParamTarget (FlatSet (CSEExp e) k) = FlatSet e k

instance (GJanusCFixLeaves t (Rep (FlatSet (JanusCT t) k))) => JanusCFixParam t (FlatSet (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (FlatSet (JanusCT t) k))) => JanusCFixMonRet t (FlatSet (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (FlatSet (CSEExp e) k)) (Rep (FlatSet e k))) => CSEFixParam e (FlatSet (CSEExp e) k) where
  type CSEFixParamTarget e (FlatSet (CSEExp e) k) = FlatSet e k

instance (GCSEFixLeaves e (Rep (FlatSet (CSEExp e) k)) (Rep (FlatSet e k))) => CSEFixMonRet e (FlatSet (CSEExp e) k) where
  type CSEFixRetTarget e (FlatSet (CSEExp e) k) = FlatSet e k

instance CSEReturn e (HashMap (CSEExp e) k v)

instance CSETargetReturn e (HashMap (CSEExp e) k v) where
  type CSETargetVal e (HashMap (CSEExp e) k v) = HashMap e k v

instance (GCSEHKD e (Rep (HashMap (CSEExp e) k v)) (Rep (HashMap e k v))) => CSEParam (HashMap (CSEExp e) k v) where
  type CSEParamExp (HashMap (CSEExp e) k v) = e
  type CSEParamTarget (HashMap (CSEExp e) k v) = HashMap e k v

instance (GJanusCFixLeaves t (Rep (HashMap (JanusCT t) k v))) => JanusCFixParam t (HashMap (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (HashMap (JanusCT t) k v))) => JanusCFixMonRet t (HashMap (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (HashMap (CSEExp e) k v)) (Rep (HashMap e k v))) => CSEFixParam e (HashMap (CSEExp e) k v) where
  type CSEFixParamTarget e (HashMap (CSEExp e) k v) = HashMap e k v

instance (GCSEFixLeaves e (Rep (HashMap (CSEExp e) k v)) (Rep (HashMap e k v))) => CSEFixMonRet e (HashMap (CSEExp e) k v) where
  type CSEFixRetTarget e (HashMap (CSEExp e) k v) = HashMap e k v

instance CSEReturn e (HashSet (CSEExp e) k)

instance CSETargetReturn e (HashSet (CSEExp e) k) where
  type CSETargetVal e (HashSet (CSEExp e) k) = HashSet e k

instance (GCSEHKD e (Rep (HashSet (CSEExp e) k)) (Rep (HashSet e k))) => CSEParam (HashSet (CSEExp e) k) where
  type CSEParamExp (HashSet (CSEExp e) k) = e
  type CSEParamTarget (HashSet (CSEExp e) k) = HashSet e k

instance (GJanusCFixLeaves t (Rep (HashSet (JanusCT t) k))) => JanusCFixParam t (HashSet (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (HashSet (JanusCT t) k))) => JanusCFixMonRet t (HashSet (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (HashSet (CSEExp e) k)) (Rep (HashSet e k))) => CSEFixParam e (HashSet (CSEExp e) k) where
  type CSEFixParamTarget e (HashSet (CSEExp e) k) = HashSet e k

instance (GCSEFixLeaves e (Rep (HashSet (CSEExp e) k)) (Rep (HashSet e k))) => CSEFixMonRet e (HashSet (CSEExp e) k) where
  type CSEFixRetTarget e (HashSet (CSEExp e) k) = HashSet e k

instance CSEReturn e (IndexMap (CSEExp e) k v)

instance CSETargetReturn e (IndexMap (CSEExp e) k v) where
  type CSETargetVal e (IndexMap (CSEExp e) k v) = IndexMap e k v

instance (GCSEHKD e (Rep (IndexMap (CSEExp e) k v)) (Rep (IndexMap e k v))) => CSEParam (IndexMap (CSEExp e) k v) where
  type CSEParamExp (IndexMap (CSEExp e) k v) = e
  type CSEParamTarget (IndexMap (CSEExp e) k v) = IndexMap e k v

instance (GJanusCFixLeaves t (Rep (IndexMap (JanusCT t) k v))) => JanusCFixParam t (IndexMap (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (IndexMap (JanusCT t) k v))) => JanusCFixMonRet t (IndexMap (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (IndexMap (CSEExp e) k v)) (Rep (IndexMap e k v))) => CSEFixParam e (IndexMap (CSEExp e) k v) where
  type CSEFixParamTarget e (IndexMap (CSEExp e) k v) = IndexMap e k v

instance (GCSEFixLeaves e (Rep (IndexMap (CSEExp e) k v)) (Rep (IndexMap e k v))) => CSEFixMonRet e (IndexMap (CSEExp e) k v) where
  type CSEFixRetTarget e (IndexMap (CSEExp e) k v) = IndexMap e k v

instance CSEReturn e (IndexSet (CSEExp e) k)

instance CSETargetReturn e (IndexSet (CSEExp e) k) where
  type CSETargetVal e (IndexSet (CSEExp e) k) = IndexSet e k

instance (GCSEHKD e (Rep (IndexSet (CSEExp e) k)) (Rep (IndexSet e k))) => CSEParam (IndexSet (CSEExp e) k) where
  type CSEParamExp (IndexSet (CSEExp e) k) = e
  type CSEParamTarget (IndexSet (CSEExp e) k) = IndexSet e k

instance (GJanusCFixLeaves t (Rep (IndexSet (JanusCT t) k))) => JanusCFixParam t (IndexSet (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (IndexSet (JanusCT t) k))) => JanusCFixMonRet t (IndexSet (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (IndexSet (CSEExp e) k)) (Rep (IndexSet e k))) => CSEFixParam e (IndexSet (CSEExp e) k) where
  type CSEFixParamTarget e (IndexSet (CSEExp e) k) = IndexSet e k

instance (GCSEFixLeaves e (Rep (IndexSet (CSEExp e) k)) (Rep (IndexSet e k))) => CSEFixMonRet e (IndexSet (CSEExp e) k) where
  type CSEFixRetTarget e (IndexSet (CSEExp e) k) = IndexSet e k

instance CSEReturn e (IndexedPQ (CSEExp e) p v)

instance CSETargetReturn e (IndexedPQ (CSEExp e) p v) where
  type CSETargetVal e (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance (GCSEHKD e (Rep (IndexedPQ (CSEExp e) p v)) (Rep (IndexedPQ e p v))) => CSEParam (IndexedPQ (CSEExp e) p v) where
  type CSEParamExp (IndexedPQ (CSEExp e) p v) = e
  type CSEParamTarget (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance (GJanusCFixLeaves t (Rep (IndexedPQ (JanusCT t) p v))) => JanusCFixParam t (IndexedPQ (JanusCT t) p v)

instance (GJanusCFixLeaves t (Rep (IndexedPQ (JanusCT t) p v))) => JanusCFixMonRet t (IndexedPQ (JanusCT t) p v)

instance (GCSEFixLeaves e (Rep (IndexedPQ (CSEExp e) p v)) (Rep (IndexedPQ e p v))) => CSEFixParam e (IndexedPQ (CSEExp e) p v) where
  type CSEFixParamTarget e (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance (GCSEFixLeaves e (Rep (IndexedPQ (CSEExp e) p v)) (Rep (IndexedPQ e p v))) => CSEFixMonRet e (IndexedPQ (CSEExp e) p v) where
  type CSEFixRetTarget e (IndexedPQ (CSEExp e) p v) = IndexedPQ e p v

instance CSEReturn e (Interner (CSEExp e))

instance CSETargetReturn e (Interner (CSEExp e)) where
  type CSETargetVal e (Interner (CSEExp e)) = Interner e

instance (GCSEHKD e (Rep (Interner (CSEExp e))) (Rep (Interner e))) => CSEParam (Interner (CSEExp e)) where
  type CSEParamExp (Interner (CSEExp e)) = e
  type CSEParamTarget (Interner (CSEExp e)) = Interner e

instance (GJanusCFixLeaves t (Rep (Interner (JanusCT t)))) => JanusCFixParam t (Interner (JanusCT t))

instance (GJanusCFixLeaves t (Rep (Interner (JanusCT t)))) => JanusCFixMonRet t (Interner (JanusCT t))

instance (GCSEFixLeaves e (Rep (Interner (CSEExp e))) (Rep (Interner e))) => CSEFixParam e (Interner (CSEExp e)) where
  type CSEFixParamTarget e (Interner (CSEExp e)) = Interner e

instance (GCSEFixLeaves e (Rep (Interner (CSEExp e))) (Rep (Interner e))) => CSEFixMonRet e (Interner (CSEExp e)) where
  type CSEFixRetTarget e (Interner (CSEExp e)) = Interner e

instance CSEReturn e (Symbol (CSEExp e))

instance CSETargetReturn e (Symbol (CSEExp e)) where
  type CSETargetVal e (Symbol (CSEExp e)) = Symbol e

instance (GCSEHKD e (Rep (Symbol (CSEExp e))) (Rep (Symbol e))) => CSEParam (Symbol (CSEExp e)) where
  type CSEParamExp (Symbol (CSEExp e)) = e
  type CSEParamTarget (Symbol (CSEExp e)) = Symbol e

instance (GJanusCFixLeaves t (Rep (Symbol (JanusCT t)))) => JanusCFixParam t (Symbol (JanusCT t))

instance (GJanusCFixLeaves t (Rep (Symbol (JanusCT t)))) => JanusCFixMonRet t (Symbol (JanusCT t))

instance (GCSEFixLeaves e (Rep (Symbol (CSEExp e))) (Rep (Symbol e))) => CSEFixParam e (Symbol (CSEExp e)) where
  type CSEFixParamTarget e (Symbol (CSEExp e)) = Symbol e

instance (GCSEFixLeaves e (Rep (Symbol (CSEExp e))) (Rep (Symbol e))) => CSEFixMonRet e (Symbol (CSEExp e)) where
  type CSEFixRetTarget e (Symbol (CSEExp e)) = Symbol e

instance CSEReturn e (IntrusiveHeap s k (CSEExp e))

instance CSETargetReturn e (IntrusiveHeap s k (CSEExp e)) where
  type CSETargetVal e (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e

instance (GCSEHKD e (Rep (IntrusiveHeap s k (CSEExp e))) (Rep (IntrusiveHeap s k e))) => CSEParam (IntrusiveHeap s k (CSEExp e)) where
  type CSEParamExp (IntrusiveHeap s k (CSEExp e)) = e
  type CSEParamTarget (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e

instance
  (Typeable s, Typeable k, GJanusCFixLeaves t (Rep (IntrusiveHeap s k (JanusCT t)))) =>
  JanusCFixParam t (IntrusiveHeap s k (JanusCT t))
  where
  jcfixParamTypes pt _ =
    typeRep (Proxy @s)
      : typeRep (Proxy @k)
      : gFixTypesC pt (Proxy @(Rep (IntrusiveHeap s k (JanusCT t))))

instance
  (Typeable s, Typeable k, GJanusCFixLeaves t (Rep (IntrusiveHeap s k (JanusCT t)))) =>
  JanusCFixMonRet t (IntrusiveHeap s k (JanusCT t))
  where
  jcfixRetTypes pt _ =
    typeRep (Proxy @s)
      : typeRep (Proxy @k)
      : gFixTypesC pt (Proxy @(Rep (IntrusiveHeap s k (JanusCT t))))

instance
  (Typeable s, Typeable k, GCSEFixLeaves e (Rep (IntrusiveHeap s k (CSEExp e))) (Rep (IntrusiveHeap s k e))) =>
  CSEFixParam e (IntrusiveHeap s k (CSEExp e))
  where
  type CSEFixParamTarget e (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e
  cseFixParamTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : TR.SomeTypeRep (TR.typeRep @k)
      : gFixTypesCSE @e @(Rep (IntrusiveHeap s k (CSEExp e))) @(Rep (IntrusiveHeap s k e)) pe

instance
  (Typeable s, Typeable k, GCSEFixLeaves e (Rep (IntrusiveHeap s k (CSEExp e))) (Rep (IntrusiveHeap s k e))) =>
  CSEFixMonRet e (IntrusiveHeap s k (CSEExp e))
  where
  type CSEFixRetTarget e (IntrusiveHeap s k (CSEExp e)) = IntrusiveHeap s k e
  cseFixRetTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : TR.SomeTypeRep (TR.typeRep @k)
      : gFixTypesCSE @e @(Rep (IntrusiveHeap s k (CSEExp e))) @(Rep (IntrusiveHeap s k e)) pe

instance CSEReturn e (IntrusiveList s (CSEExp e))

instance CSETargetReturn e (IntrusiveList s (CSEExp e)) where
  type CSETargetVal e (IntrusiveList s (CSEExp e)) = IntrusiveList s e

instance (GCSEHKD e (Rep (IntrusiveList s (CSEExp e))) (Rep (IntrusiveList s e))) => CSEParam (IntrusiveList s (CSEExp e)) where
  type CSEParamExp (IntrusiveList s (CSEExp e)) = e
  type CSEParamTarget (IntrusiveList s (CSEExp e)) = IntrusiveList s e

instance
  (Typeable s, GJanusCFixLeaves t (Rep (IntrusiveList s (JanusCT t)))) =>
  JanusCFixParam t (IntrusiveList s (JanusCT t))
  where
  jcfixParamTypes pt _ =
    typeRep (Proxy @s)
      : gFixTypesC pt (Proxy @(Rep (IntrusiveList s (JanusCT t))))

instance
  (Typeable s, GJanusCFixLeaves t (Rep (IntrusiveList s (JanusCT t)))) =>
  JanusCFixMonRet t (IntrusiveList s (JanusCT t))
  where
  jcfixRetTypes pt _ =
    typeRep (Proxy @s)
      : gFixTypesC pt (Proxy @(Rep (IntrusiveList s (JanusCT t))))

instance
  (Typeable s, GCSEFixLeaves e (Rep (IntrusiveList s (CSEExp e))) (Rep (IntrusiveList s e))) =>
  CSEFixParam e (IntrusiveList s (CSEExp e))
  where
  type CSEFixParamTarget e (IntrusiveList s (CSEExp e)) = IntrusiveList s e
  cseFixParamTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : gFixTypesCSE @e @(Rep (IntrusiveList s (CSEExp e))) @(Rep (IntrusiveList s e)) pe

instance
  (Typeable s, GCSEFixLeaves e (Rep (IntrusiveList s (CSEExp e))) (Rep (IntrusiveList s e))) =>
  CSEFixMonRet e (IntrusiveList s (CSEExp e))
  where
  type CSEFixRetTarget e (IntrusiveList s (CSEExp e)) = IntrusiveList s e
  cseFixRetTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : gFixTypesCSE @e @(Rep (IntrusiveList s (CSEExp e))) @(Rep (IntrusiveList s e)) pe

instance CSEReturn e (LruCache (CSEExp e) k v)

instance CSETargetReturn e (LruCache (CSEExp e) k v) where
  type CSETargetVal e (LruCache (CSEExp e) k v) = LruCache e k v

instance (GCSEHKD e (Rep (LruCache (CSEExp e) k v)) (Rep (LruCache e k v))) => CSEParam (LruCache (CSEExp e) k v) where
  type CSEParamExp (LruCache (CSEExp e) k v) = e
  type CSEParamTarget (LruCache (CSEExp e) k v) = LruCache e k v

instance (GJanusCFixLeaves t (Rep (LruCache (JanusCT t) k v))) => JanusCFixParam t (LruCache (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (LruCache (JanusCT t) k v))) => JanusCFixMonRet t (LruCache (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (LruCache (CSEExp e) k v)) (Rep (LruCache e k v))) => CSEFixParam e (LruCache (CSEExp e) k v) where
  type CSEFixParamTarget e (LruCache (CSEExp e) k v) = LruCache e k v

instance (GCSEFixLeaves e (Rep (LruCache (CSEExp e) k v)) (Rep (LruCache e k v))) => CSEFixMonRet e (LruCache (CSEExp e) k v) where
  type CSEFixRetTarget e (LruCache (CSEExp e) k v) = LruCache e k v

instance CSEReturn e (Pool (CSEExp e) a)

instance CSETargetReturn e (Pool (CSEExp e) a) where
  type CSETargetVal e (Pool (CSEExp e) a) = Pool e a

instance (GCSEHKD e (Rep (Pool (CSEExp e) a)) (Rep (Pool e a))) => CSEParam (Pool (CSEExp e) a) where
  type CSEParamExp (Pool (CSEExp e) a) = e
  type CSEParamTarget (Pool (CSEExp e) a) = Pool e a

instance (GJanusCFixLeaves t (Rep (Pool (JanusCT t) a))) => JanusCFixParam t (Pool (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (Pool (JanusCT t) a))) => JanusCFixMonRet t (Pool (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (Pool (CSEExp e) a)) (Rep (Pool e a))) => CSEFixParam e (Pool (CSEExp e) a) where
  type CSEFixParamTarget e (Pool (CSEExp e) a) = Pool e a

instance (GCSEFixLeaves e (Rep (Pool (CSEExp e) a)) (Rep (Pool e a))) => CSEFixMonRet e (Pool (CSEExp e) a) where
  type CSEFixRetTarget e (Pool (CSEExp e) a) = Pool e a

instance CSEReturn e (PoolHandle (CSEExp e))

instance CSETargetReturn e (PoolHandle (CSEExp e)) where
  type CSETargetVal e (PoolHandle (CSEExp e)) = PoolHandle e

instance (GCSEHKD e (Rep (PoolHandle (CSEExp e))) (Rep (PoolHandle e))) => CSEParam (PoolHandle (CSEExp e)) where
  type CSEParamExp (PoolHandle (CSEExp e)) = e
  type CSEParamTarget (PoolHandle (CSEExp e)) = PoolHandle e

instance (GJanusCFixLeaves t (Rep (PoolHandle (JanusCT t)))) => JanusCFixParam t (PoolHandle (JanusCT t))

instance (GJanusCFixLeaves t (Rep (PoolHandle (JanusCT t)))) => JanusCFixMonRet t (PoolHandle (JanusCT t))

instance (GCSEFixLeaves e (Rep (PoolHandle (CSEExp e))) (Rep (PoolHandle e))) => CSEFixParam e (PoolHandle (CSEExp e)) where
  type CSEFixParamTarget e (PoolHandle (CSEExp e)) = PoolHandle e

instance (GCSEFixLeaves e (Rep (PoolHandle (CSEExp e))) (Rep (PoolHandle e))) => CSEFixMonRet e (PoolHandle (CSEExp e)) where
  type CSEFixRetTarget e (PoolHandle (CSEExp e)) = PoolHandle e

instance CSEReturn e (PriorityQueue (CSEExp e) k v)

instance CSETargetReturn e (PriorityQueue (CSEExp e) k v) where
  type CSETargetVal e (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance (GCSEHKD e (Rep (PriorityQueue (CSEExp e) k v)) (Rep (PriorityQueue e k v))) => CSEParam (PriorityQueue (CSEExp e) k v) where
  type CSEParamExp (PriorityQueue (CSEExp e) k v) = e
  type CSEParamTarget (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance (GJanusCFixLeaves t (Rep (PriorityQueue (JanusCT t) k v))) => JanusCFixParam t (PriorityQueue (JanusCT t) k v)

instance (GJanusCFixLeaves t (Rep (PriorityQueue (JanusCT t) k v))) => JanusCFixMonRet t (PriorityQueue (JanusCT t) k v)

instance (GCSEFixLeaves e (Rep (PriorityQueue (CSEExp e) k v)) (Rep (PriorityQueue e k v))) => CSEFixParam e (PriorityQueue (CSEExp e) k v) where
  type CSEFixParamTarget e (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance (GCSEFixLeaves e (Rep (PriorityQueue (CSEExp e) k v)) (Rep (PriorityQueue e k v))) => CSEFixMonRet e (PriorityQueue (CSEExp e) k v) where
  type CSEFixRetTarget e (PriorityQueue (CSEExp e) k v) = PriorityQueue e k v

instance CSEReturn e (MinHeap (CSEExp e) k)

instance CSETargetReturn e (MinHeap (CSEExp e) k) where
  type CSETargetVal e (MinHeap (CSEExp e) k) = MinHeap e k

instance (GCSEHKD e (Rep (MinHeap (CSEExp e) k)) (Rep (MinHeap e k))) => CSEParam (MinHeap (CSEExp e) k) where
  type CSEParamExp (MinHeap (CSEExp e) k) = e
  type CSEParamTarget (MinHeap (CSEExp e) k) = MinHeap e k

instance (GJanusCFixLeaves t (Rep (MinHeap (JanusCT t) k))) => JanusCFixParam t (MinHeap (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (MinHeap (JanusCT t) k))) => JanusCFixMonRet t (MinHeap (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (MinHeap (CSEExp e) k)) (Rep (MinHeap e k))) => CSEFixParam e (MinHeap (CSEExp e) k) where
  type CSEFixParamTarget e (MinHeap (CSEExp e) k) = MinHeap e k

instance (GCSEFixLeaves e (Rep (MinHeap (CSEExp e) k)) (Rep (MinHeap e k))) => CSEFixMonRet e (MinHeap (CSEExp e) k) where
  type CSEFixRetTarget e (MinHeap (CSEExp e) k) = MinHeap e k

instance CSEReturn e (RingBuffer (CSEExp e) a)

instance CSETargetReturn e (RingBuffer (CSEExp e) a) where
  type CSETargetVal e (RingBuffer (CSEExp e) a) = RingBuffer e a

instance (GCSEHKD e (Rep (RingBuffer (CSEExp e) a)) (Rep (RingBuffer e a))) => CSEParam (RingBuffer (CSEExp e) a) where
  type CSEParamExp (RingBuffer (CSEExp e) a) = e
  type CSEParamTarget (RingBuffer (CSEExp e) a) = RingBuffer e a

instance (GJanusCFixLeaves t (Rep (RingBuffer (JanusCT t) a))) => JanusCFixParam t (RingBuffer (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (RingBuffer (JanusCT t) a))) => JanusCFixMonRet t (RingBuffer (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (RingBuffer (CSEExp e) a)) (Rep (RingBuffer e a))) => CSEFixParam e (RingBuffer (CSEExp e) a) where
  type CSEFixParamTarget e (RingBuffer (CSEExp e) a) = RingBuffer e a

instance (GCSEFixLeaves e (Rep (RingBuffer (CSEExp e) a)) (Rep (RingBuffer e a))) => CSEFixMonRet e (RingBuffer (CSEExp e) a) where
  type CSEFixRetTarget e (RingBuffer (CSEExp e) a) = RingBuffer e a

instance CSEReturn e (SlotMap (CSEExp e) a)

instance CSETargetReturn e (SlotMap (CSEExp e) a) where
  type CSETargetVal e (SlotMap (CSEExp e) a) = SlotMap e a

instance (GCSEHKD e (Rep (SlotMap (CSEExp e) a)) (Rep (SlotMap e a))) => CSEParam (SlotMap (CSEExp e) a) where
  type CSEParamExp (SlotMap (CSEExp e) a) = e
  type CSEParamTarget (SlotMap (CSEExp e) a) = SlotMap e a

instance (GJanusCFixLeaves t (Rep (SlotMap (JanusCT t) a))) => JanusCFixParam t (SlotMap (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (SlotMap (JanusCT t) a))) => JanusCFixMonRet t (SlotMap (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (SlotMap (CSEExp e) a)) (Rep (SlotMap e a))) => CSEFixParam e (SlotMap (CSEExp e) a) where
  type CSEFixParamTarget e (SlotMap (CSEExp e) a) = SlotMap e a

instance (GCSEFixLeaves e (Rep (SlotMap (CSEExp e) a)) (Rep (SlotMap e a))) => CSEFixMonRet e (SlotMap (CSEExp e) a) where
  type CSEFixRetTarget e (SlotMap (CSEExp e) a) = SlotMap e a

instance CSEReturn e (SlotKey (CSEExp e))

instance CSETargetReturn e (SlotKey (CSEExp e)) where
  type CSETargetVal e (SlotKey (CSEExp e)) = SlotKey e

instance (GCSEHKD e (Rep (SlotKey (CSEExp e))) (Rep (SlotKey e))) => CSEParam (SlotKey (CSEExp e)) where
  type CSEParamExp (SlotKey (CSEExp e)) = e
  type CSEParamTarget (SlotKey (CSEExp e)) = SlotKey e

instance (GJanusCFixLeaves t (Rep (SlotKey (JanusCT t)))) => JanusCFixParam t (SlotKey (JanusCT t))

instance (GJanusCFixLeaves t (Rep (SlotKey (JanusCT t)))) => JanusCFixMonRet t (SlotKey (JanusCT t))

instance (GCSEFixLeaves e (Rep (SlotKey (CSEExp e))) (Rep (SlotKey e))) => CSEFixParam e (SlotKey (CSEExp e)) where
  type CSEFixParamTarget e (SlotKey (CSEExp e)) = SlotKey e

instance (GCSEFixLeaves e (Rep (SlotKey (CSEExp e))) (Rep (SlotKey e))) => CSEFixMonRet e (SlotKey (CSEExp e)) where
  type CSEFixRetTarget e (SlotKey (CSEExp e)) = SlotKey e

instance CSEReturn e (SmallStack n (CSEExp e) a)

instance CSETargetReturn e (SmallStack n (CSEExp e) a) where
  type CSETargetVal e (SmallStack n (CSEExp e) a) = SmallStack n e a

instance (GCSEHKD e (Rep (SmallStack n (CSEExp e) a)) (Rep (SmallStack n e a))) => CSEParam (SmallStack n (CSEExp e) a) where
  type CSEParamExp (SmallStack n (CSEExp e) a) = e
  type CSEParamTarget (SmallStack n (CSEExp e) a) = SmallStack n e a

instance (GJanusCFixLeaves t (Rep (SmallStack n (JanusCT t) a))) => JanusCFixParam t (SmallStack n (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (SmallStack n (JanusCT t) a))) => JanusCFixMonRet t (SmallStack n (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (SmallStack n (CSEExp e) a)) (Rep (SmallStack n e a))) => CSEFixParam e (SmallStack n (CSEExp e) a) where
  type CSEFixParamTarget e (SmallStack n (CSEExp e) a) = SmallStack n e a

instance (GCSEFixLeaves e (Rep (SmallStack n (CSEExp e) a)) (Rep (SmallStack n e a))) => CSEFixMonRet e (SmallStack n (CSEExp e) a) where
  type CSEFixRetTarget e (SmallStack n (CSEExp e) a) = SmallStack n e a

instance CSEReturn e (SmallVec n (CSEExp e) a)

instance CSETargetReturn e (SmallVec n (CSEExp e) a) where
  type CSETargetVal e (SmallVec n (CSEExp e) a) = SmallVec n e a

instance (GCSEHKD e (Rep (SmallVec n (CSEExp e) a)) (Rep (SmallVec n e a))) => CSEParam (SmallVec n (CSEExp e) a) where
  type CSEParamExp (SmallVec n (CSEExp e) a) = e
  type CSEParamTarget (SmallVec n (CSEExp e) a) = SmallVec n e a

instance (GJanusCFixLeaves t (Rep (SmallVec n (JanusCT t) a))) => JanusCFixParam t (SmallVec n (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (SmallVec n (JanusCT t) a))) => JanusCFixMonRet t (SmallVec n (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (SmallVec n (CSEExp e) a)) (Rep (SmallVec n e a))) => CSEFixParam e (SmallVec n (CSEExp e) a) where
  type CSEFixParamTarget e (SmallVec n (CSEExp e) a) = SmallVec n e a

instance (GCSEFixLeaves e (Rep (SmallVec n (CSEExp e) a)) (Rep (SmallVec n e a))) => CSEFixMonRet e (SmallVec n (CSEExp e) a) where
  type CSEFixRetTarget e (SmallVec n (CSEExp e) a) = SmallVec n e a

instance CSEReturn e (SparseSet (CSEExp e))

instance CSETargetReturn e (SparseSet (CSEExp e)) where
  type CSETargetVal e (SparseSet (CSEExp e)) = SparseSet e

instance (GCSEHKD e (Rep (SparseSet (CSEExp e))) (Rep (SparseSet e))) => CSEParam (SparseSet (CSEExp e)) where
  type CSEParamExp (SparseSet (CSEExp e)) = e
  type CSEParamTarget (SparseSet (CSEExp e)) = SparseSet e

instance (GJanusCFixLeaves t (Rep (SparseSet (JanusCT t)))) => JanusCFixParam t (SparseSet (JanusCT t))

instance (GJanusCFixLeaves t (Rep (SparseSet (JanusCT t)))) => JanusCFixMonRet t (SparseSet (JanusCT t))

instance (GCSEFixLeaves e (Rep (SparseSet (CSEExp e))) (Rep (SparseSet e))) => CSEFixParam e (SparseSet (CSEExp e)) where
  type CSEFixParamTarget e (SparseSet (CSEExp e)) = SparseSet e

instance (GCSEFixLeaves e (Rep (SparseSet (CSEExp e))) (Rep (SparseSet e))) => CSEFixMonRet e (SparseSet (CSEExp e)) where
  type CSEFixRetTarget e (SparseSet (CSEExp e)) = SparseSet e

instance CSEReturn e (SparseVec (CSEExp e) a)

instance CSETargetReturn e (SparseVec (CSEExp e) a) where
  type CSETargetVal e (SparseVec (CSEExp e) a) = SparseVec e a

instance (GCSEHKD e (Rep (SparseVec (CSEExp e) a)) (Rep (SparseVec e a))) => CSEParam (SparseVec (CSEExp e) a) where
  type CSEParamExp (SparseVec (CSEExp e) a) = e
  type CSEParamTarget (SparseVec (CSEExp e) a) = SparseVec e a

instance (GJanusCFixLeaves t (Rep (SparseVec (JanusCT t) a))) => JanusCFixParam t (SparseVec (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (SparseVec (JanusCT t) a))) => JanusCFixMonRet t (SparseVec (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (SparseVec (CSEExp e) a)) (Rep (SparseVec e a))) => CSEFixParam e (SparseVec (CSEExp e) a) where
  type CSEFixParamTarget e (SparseVec (CSEExp e) a) = SparseVec e a

instance (GCSEFixLeaves e (Rep (SparseVec (CSEExp e) a)) (Rep (SparseVec e a))) => CSEFixMonRet e (SparseVec (CSEExp e) a) where
  type CSEFixRetTarget e (SparseVec (CSEExp e) a) = SparseVec e a

instance CSEReturn e (StableVec k (CSEExp e) a)

instance CSETargetReturn e (StableVec k (CSEExp e) a) where
  type CSETargetVal e (StableVec k (CSEExp e) a) = StableVec k e a

instance (GCSEHKD e (Rep (StableVec k (CSEExp e) a)) (Rep (StableVec k e a))) => CSEParam (StableVec k (CSEExp e) a) where
  type CSEParamExp (StableVec k (CSEExp e) a) = e
  type CSEParamTarget (StableVec k (CSEExp e) a) = StableVec k e a

instance (GJanusCFixLeaves t (Rep (StableVec k (JanusCT t) a))) => JanusCFixParam t (StableVec k (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (StableVec k (JanusCT t) a))) => JanusCFixMonRet t (StableVec k (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (StableVec k (CSEExp e) a)) (Rep (StableVec k e a))) => CSEFixParam e (StableVec k (CSEExp e) a) where
  type CSEFixParamTarget e (StableVec k (CSEExp e) a) = StableVec k e a

instance (GCSEFixLeaves e (Rep (StableVec k (CSEExp e) a)) (Rep (StableVec k e a))) => CSEFixMonRet e (StableVec k (CSEExp e) a) where
  type CSEFixRetTarget e (StableVec k (CSEExp e) a) = StableVec k e a

instance CSEReturn e (Stack (CSEExp e) a)

instance CSETargetReturn e (Stack (CSEExp e) a) where
  type CSETargetVal e (Stack (CSEExp e) a) = Stack e a

instance (GCSEHKD e (Rep (Stack (CSEExp e) a)) (Rep (Stack e a))) => CSEParam (Stack (CSEExp e) a) where
  type CSEParamExp (Stack (CSEExp e) a) = e
  type CSEParamTarget (Stack (CSEExp e) a) = Stack e a

instance (GJanusCFixLeaves t (Rep (Stack (JanusCT t) a))) => JanusCFixParam t (Stack (JanusCT t) a)

instance (GJanusCFixLeaves t (Rep (Stack (JanusCT t) a))) => JanusCFixMonRet t (Stack (JanusCT t) a)

instance (GCSEFixLeaves e (Rep (Stack (CSEExp e) a)) (Rep (Stack e a))) => CSEFixParam e (Stack (CSEExp e) a) where
  type CSEFixParamTarget e (Stack (CSEExp e) a) = Stack e a

instance (GCSEFixLeaves e (Rep (Stack (CSEExp e) a)) (Rep (Stack e a))) => CSEFixMonRet e (Stack (CSEExp e) a) where
  type CSEFixRetTarget e (Stack (CSEExp e) a) = Stack e a

instance CSEReturn e (TimerWheel s (CSEExp e))

instance CSETargetReturn e (TimerWheel s (CSEExp e)) where
  type CSETargetVal e (TimerWheel s (CSEExp e)) = TimerWheel s e

instance (GCSEHKD e (Rep (TimerWheel s (CSEExp e))) (Rep (TimerWheel s e))) => CSEParam (TimerWheel s (CSEExp e)) where
  type CSEParamExp (TimerWheel s (CSEExp e)) = e
  type CSEParamTarget (TimerWheel s (CSEExp e)) = TimerWheel s e

instance
  (Typeable s, GJanusCFixLeaves t (Rep (TimerWheel s (JanusCT t)))) =>
  JanusCFixParam t (TimerWheel s (JanusCT t))
  where
  jcfixParamTypes pt _ =
    typeRep (Proxy @s)
      : gFixTypesC pt (Proxy @(Rep (TimerWheel s (JanusCT t))))

instance
  (Typeable s, GJanusCFixLeaves t (Rep (TimerWheel s (JanusCT t)))) =>
  JanusCFixMonRet t (TimerWheel s (JanusCT t))
  where
  jcfixRetTypes pt _ =
    typeRep (Proxy @s)
      : gFixTypesC pt (Proxy @(Rep (TimerWheel s (JanusCT t))))

instance
  (Typeable s, GCSEFixLeaves e (Rep (TimerWheel s (CSEExp e))) (Rep (TimerWheel s e))) =>
  CSEFixParam e (TimerWheel s (CSEExp e))
  where
  type CSEFixParamTarget e (TimerWheel s (CSEExp e)) = TimerWheel s e
  cseFixParamTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : gFixTypesCSE @e @(Rep (TimerWheel s (CSEExp e))) @(Rep (TimerWheel s e)) pe

instance
  (Typeable s, GCSEFixLeaves e (Rep (TimerWheel s (CSEExp e))) (Rep (TimerWheel s e))) =>
  CSEFixMonRet e (TimerWheel s (CSEExp e))
  where
  type CSEFixRetTarget e (TimerWheel s (CSEExp e)) = TimerWheel s e
  cseFixRetTypes pe _ =
    TR.SomeTypeRep (TR.typeRep @s)
      : gFixTypesCSE @e @(Rep (TimerWheel s (CSEExp e))) @(Rep (TimerWheel s e)) pe

instance CSEReturn e (XorFilter (CSEExp e) k)

instance CSETargetReturn e (XorFilter (CSEExp e) k) where
  type CSETargetVal e (XorFilter (CSEExp e) k) = XorFilter e k

instance (GCSEHKD e (Rep (XorFilter (CSEExp e) k)) (Rep (XorFilter e k))) => CSEParam (XorFilter (CSEExp e) k) where
  type CSEParamExp (XorFilter (CSEExp e) k) = e
  type CSEParamTarget (XorFilter (CSEExp e) k) = XorFilter e k

instance (GJanusCFixLeaves t (Rep (XorFilter (JanusCT t) k))) => JanusCFixParam t (XorFilter (JanusCT t) k)

instance (GJanusCFixLeaves t (Rep (XorFilter (JanusCT t) k))) => JanusCFixMonRet t (XorFilter (JanusCT t) k)

instance (GCSEFixLeaves e (Rep (XorFilter (CSEExp e) k)) (Rep (XorFilter e k))) => CSEFixParam e (XorFilter (CSEExp e) k) where
  type CSEFixParamTarget e (XorFilter (CSEExp e) k) = XorFilter e k

instance (GCSEFixLeaves e (Rep (XorFilter (CSEExp e) k)) (Rep (XorFilter e k))) => CSEFixMonRet e (XorFilter (CSEExp e) k) where
  type CSEFixRetTarget e (XorFilter (CSEExp e) k) = XorFilter e k

--------------------------------------------------------------------------------
-- Internal Helpers for Phantom-Type-Tagged Leaf Type Lists
--------------------------------------------------------------------------------

gFixTypesC :: (GJanusCFixLeaves t rep) => Proxy t -> Proxy rep -> [TypeRep]
gFixTypesC = gFixLeafTypes

gFixTypesCSE :: forall e repCSE repTgt. (GCSEFixLeaves e repCSE repTgt) => Proxy e -> [TR.SomeTypeRep]
gFixTypesCSE = gFixTypes @e @repCSE @repTgt
