{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core tensor view types, zero-copy bridges to 'Janus.Data.Tensor', and
-- open-world, AD-agnostic extensible metadata ('PrimAttrMap') for custom
-- primitives and IR annotations.
module Janus.Tensor.Types
  ( -- * Runtime Tensor Views & Zero-Copy Bridges to `Janus.Data.Tensor`
    TensorView (..),
    mkTensorView,
    mkTensorViewWithDims,
    tensorViewSize,
    tensorViewBytes,
    fromJanusTensor,
    toJanusTensor,
    ShapeToIx,

    -- * Open-World Extensible Attribute Map (`PrimAttrMap`)
    PrimAttrOf,
    PrimAttrKey (..),
    primAttrKey,
    PrimAttrMap,
    emptyPrimAttrs,
    insertPrimAttr,
    lookupPrimAttr,
  )
where

import Data.Constraint.Compose (ComposeC)
import Data.Constraint.Extras (Has (argDict))
import qualified Data.Dependent.HashMap as DHM
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Show (GShow (..), defaultGshowsPrec)
import Data.Hashable (Hashable (..))
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Some (Some (..))
import Data.Type.Equality ((:~:) (..))
import Data.Type.Ord (OrderingI (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (sizeOf))
import GHC.TypeLits (KnownSymbol, Symbol, cmpSymbol, sameSymbol, symbolVal)
import GHC.TypeNats (Nat)
import qualified Janus.Data.Tensor as JDT
import Janus.Tensor.Index (DimEnv, IxKind (..), IxSet (..), SIx (..), emptyDimEnv, sixEvalSize)

--------------------------------------------------------------------------------
-- 1. Zero-Copy TensorView & Bridge to `Janus.Data.Tensor`
--------------------------------------------------------------------------------

-- | Typed pointer view over a flat contiguous row-major buffer indexed by @i@.
data TensorView (e :: K.Type -> K.Type) (i :: IxKind) (a :: K.Type) = TensorView
  { tvShape :: !(SIx i),
    tvDims :: !DimEnv,
    tvPtr :: !(e (Ptr a))
  }

mkTensorView :: SIx i -> e (Ptr a) -> TensorView e i a
mkTensorView sh = TensorView sh emptyDimEnv

mkTensorViewWithDims :: SIx i -> DimEnv -> e (Ptr a) -> TensorView e i a
mkTensorViewWithDims = TensorView

tensorViewSize :: TensorView e i a -> Int
tensorViewSize tv = sixEvalSize (tvDims tv) (tvShape tv)

tensorViewBytes :: forall e i a. (Storable a) => TensorView e i a -> Int
tensorViewBytes tv = tensorViewSize tv * sizeOf (undefined :: a)

-- | Convert a static list of 'Nat' dimensions (from 'Janus.Data.Tensor.Tensor')
-- into a product 'IxKind'.
type family ShapeToIx (ds :: [Nat]) :: IxKind where
  ShapeToIx '[] = 'IxUnit
  ShapeToIx '[n] = 'IxFin n
  ShapeToIx (n ': ns) = 'IxProd ('IxFin n) (ShapeToIx ns)

-- | Zero-copy bridge from an existing 'Janus.Data.Tensor.Tensor' into a 'TensorView'.
fromJanusTensor ::
  forall ds e a.
  (IxSet (ShapeToIx ds)) =>
  JDT.Tensor ds e a ->
  TensorView e (ShapeToIx ds) a
fromJanusTensor (JDT.Tensor _ ptr) =
  TensorView
    { tvShape = six @(ShapeToIx ds),
      tvDims = mempty,
      tvPtr = ptr
    }

-- | Zero-copy bridge from a 'TensorView' back into 'Janus.Data.Tensor.Tensor'.
toJanusTensor ::
  forall ds e a.
  (JDT.ReifyTensorBounds ds) =>
  TensorView e (ShapeToIx ds) a ->
  JDT.Tensor ds e a
toJanusTensor tv =
  JDT.Tensor (JDT.reifyTensorBounds (Proxy @ds)) (tvPtr tv)

--------------------------------------------------------------------------------
-- 2. Open-World Extensible Attribute Map (`PrimAttrMap`)
--------------------------------------------------------------------------------

-- | Open type family allowing downstream libraries (such as an external AD
-- library or profiler) to attach typed metadata to 'CustomPrim's and
-- 'Annotate' IR nodes without 'Janus.Tensor' knowing anything about AD.
type family PrimAttrOf (tag :: Symbol) :: K.Type

data PrimAttrKey (v :: K.Type) where
  PrimAttrKey ::
    (KnownSymbol tag, v ~ PrimAttrOf tag) =>
    !(Proxy tag) ->
    PrimAttrKey v

primAttrKey ::
  forall tag v.
  (KnownSymbol tag, v ~ PrimAttrOf tag) =>
  PrimAttrKey v
primAttrKey = PrimAttrKey (Proxy @tag)

instance Eq (PrimAttrKey v) where
  PrimAttrKey p1 == PrimAttrKey p2 = symbolVal p1 == symbolVal p2

instance Ord (PrimAttrKey v) where
  compare (PrimAttrKey p1) (PrimAttrKey p2) = compare (symbolVal p1) (symbolVal p2)

instance Show (PrimAttrKey v) where
  showsPrec d (PrimAttrKey p) =
    showParen (d > 10) $ showString "PrimAttrKey @" . shows (symbolVal p)

instance GShow PrimAttrKey where
  gshowsPrec = defaultGshowsPrec

instance GEq PrimAttrKey where
  geq (PrimAttrKey p1) (PrimAttrKey p2) =
    case sameSymbol p1 p2 of
      Just Refl -> Just Refl
      Nothing -> Nothing

instance GCompare PrimAttrKey where
  gcompare (PrimAttrKey p1) (PrimAttrKey p2) =
    case cmpSymbol p1 p2 of
      LTI -> GLT
      EQI -> GEQ
      GTI -> GGT

instance Hashable (PrimAttrKey v) where
  hashWithSalt s (PrimAttrKey p) = hashWithSalt s (symbolVal p)

instance Hashable (Some PrimAttrKey) where
  hashWithSalt s (Some k) = hashWithSalt s k

instance Has (ComposeC Eq Identity) PrimAttrKey where
  argDict (PrimAttrKey _) = error "PrimAttrKey: Eq not required"

type PrimAttrMap = DHM.DHashMap PrimAttrKey Identity

emptyPrimAttrs :: PrimAttrMap
emptyPrimAttrs = DHM.empty

insertPrimAttr :: PrimAttrKey v -> v -> PrimAttrMap -> PrimAttrMap
insertPrimAttr k v = DHM.insert k (Identity v)

lookupPrimAttr :: PrimAttrKey v -> PrimAttrMap -> Maybe v
lookupPrimAttr k m = runIdentity <$> DHM.lookup k m
