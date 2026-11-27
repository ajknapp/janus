{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dex-inspired algebraic finite index sets ('IxKind', 'SIx', 'IxSet') and
-- Kmett-style stratified index optics ('IxIso', 'IxPrism', 'IxAffine', 'IxMorph').
--
-- Static index sets ('IxUnit', 'IxFin', 'IxProd', 'IxSum', 'IxTagged') specialize
-- completely at compile time (enabling zero-allocation static scratchpads), while
-- runtime-sized branded dimensions ('IxDyn') use the exact same algebra via a
-- runtime 'DimEnv'.
module Janus.Tensor.Index
  ( -- * Index Set Kinds & Singletons
    IxKind (..),
    DimSpec (..),
    DimEnv,
    emptyDimEnv,
    bindDim,
    lookupDim,
    SIx (..),
    SomeSIx (..),
    IxSet (..),
    sixStaticSize,
    sixEvalSize,
    sixFlatDims,
    sixDigest,
    eqSIx,

    -- * Stratified Index Optics (`IxIso`, `IxPrism`, `IxAffine`, `IxMorph`)
    IxIso (..),
    invIso,
    isoSrc,
    isoDst,
    isoDigest,
    ixId,
    ixSwap,
    ixAssocL,
    ixAssocR,
    ixRev,
    ixShift,
    ixReshape,
    ixTag,
    ixUntag,
    ixCompIso,

    IxPrism (..),
    prismSrc,
    prismDst,
    prismDigest,
    ixPrismIso,
    ixSlice,
    ixStride,
    ixInL,
    ixInR,
    ixDiag,
    ixCompPrism,

    IxAffine (..),
    affineSrc,
    affineDst,
    affineDigest,
    ixBroadcast,
    ixWindow1D,

    IxMorph (..),
    morphSrc,
    morphDst,
    morphDigest,
    morphIsBijective,
    morphIsInjective,

    -- * Pure Ordinal-Level Evaluation (Host & Embedded Codegen)
    evalIsoOrd,
    evalIsoOrdE,
    evalPrismForwardOrd,
    evalPrismForwardOrdE,
    evalPrismBackwardOrd,
    evalPrismBackwardOrdE,
    evalAffineForwardOrd,
    evalAffineForwardOrdE,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, sameSymbol, symbolVal)
import GHC.TypeNats (KnownNat, Nat, natVal, sameNat)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 1. Index Set Kinds, Dimensions, & Singletons
--------------------------------------------------------------------------------

-- | Type-level descriptor for finite algebraic index sets.
data IxKind
  = IxUnit
  | IxFin Nat
  | IxDyn Symbol
  | IxProd IxKind IxKind
  | IxSum IxKind IxKind
  | IxTagged Symbol IxKind
  deriving stock (Typeable)

-- | Dimension specification for flat shape reporting and contraction planning.
data DimSpec
  = DimStatic !Int
  | DimDyn !Text.Text
  deriving stock (Eq, Ord, Show)

-- | Runtime bindings for dynamically sized 'IxDyn' dimensions.
type DimEnv = Map.Map Text.Text Int

emptyDimEnv :: DimEnv
emptyDimEnv = Map.empty

bindDim :: Text.Text -> Int -> DimEnv -> DimEnv
bindDim = Map.insert

lookupDim :: Text.Text -> DimEnv -> Int
lookupDim name env =
  case Map.lookup name env of
    Just n -> n
    Nothing -> error ("Janus.Tensor.Index.lookupDim: unbound dynamic dimension " <> Text.unpack name)

-- | Singleton witness carrying the structure and cardinality of an 'IxKind'.
data SIx (i :: IxKind) where
  SIxUnit :: SIx 'IxUnit
  SIxFin :: (KnownNat n) => !(Proxy n) -> SIx ('IxFin n)
  SIxDyn :: (KnownSymbol s) => !(Proxy s) -> SIx ('IxDyn s)
  SIxProd :: !(SIx i) -> !(SIx j) -> SIx ('IxProd i j)
  SIxSum :: !(SIx i) -> !(SIx j) -> SIx ('IxSum i j)
  SIxTagged :: (KnownSymbol s) => !(Proxy s) -> !(SIx i) -> SIx ('IxTagged s i)

data SomeSIx where
  SomeSIx :: !(SIx i) -> SomeSIx

instance Show (SIx i) where
  showsPrec d = \case
    SIxUnit -> showString "1"
    SIxFin p -> showString "Fin " . shows (natVal p)
    SIxDyn p -> showString "Dyn " . shows (symbolVal p)
    SIxProd a b -> showParen (d > 6) $ showsPrec 7 a . showString " × " . showsPrec 7 b
    SIxSum a b -> showParen (d > 5) $ showsPrec 6 a . showString " + " . showsPrec 6 b
    SIxTagged p a -> showParen (d > 10) $ showString (symbolVal p) . showString ":" . showsPrec 11 a

instance Show SomeSIx where
  showsPrec d (SomeSIx s) = showsPrec d s

-- | Typeclass reifying finite index sets from type-level 'IxKind's.
class (Typeable i) => IxSet (i :: IxKind) where
  six :: SIx i

instance IxSet 'IxUnit where
  six = SIxUnit

instance (KnownNat n) => IxSet ('IxFin n) where
  six = SIxFin (Proxy @n)

instance (KnownSymbol s) => IxSet ('IxDyn s) where
  six = SIxDyn (Proxy @s)

instance (IxSet i, IxSet j) => IxSet ('IxProd i j) where
  six = SIxProd six six

instance (IxSet i, IxSet j) => IxSet ('IxSum i j) where
  six = SIxSum six six

instance (KnownSymbol s, IxSet i) => IxSet ('IxTagged s i) where
  six = SIxTagged (Proxy @s) six

-- | Compile-time cardinality when all dimensions in @i@ are static.
sixStaticSize :: SIx i -> Maybe Int
sixStaticSize = \case
  SIxUnit -> Just 1
  SIxFin p -> Just (fromIntegral (natVal p))
  SIxDyn _ -> Nothing
  SIxProd a b -> (*) <$> sixStaticSize a <*> sixStaticSize b
  SIxSum a b -> (+) <$> sixStaticSize a <*> sixStaticSize b
  SIxTagged _ a -> sixStaticSize a

-- | Exact cardinality given a runtime 'DimEnv' (for static shapes, ignores 'DimEnv').
sixEvalSize :: DimEnv -> SIx i -> Int
sixEvalSize env = \case
  SIxUnit -> 1
  SIxFin p -> fromIntegral (natVal p)
  SIxDyn p -> lookupDim (Text.pack (symbolVal p)) env
  SIxProd a b -> sixEvalSize env a * sixEvalSize env b
  SIxSum a b -> sixEvalSize env a + sixEvalSize env b
  SIxTagged _ a -> sixEvalSize env a

-- | Flatten product dimensions into a canonical list of 'DimSpec's.
sixFlatDims :: SIx i -> [DimSpec]
sixFlatDims = \case
  SIxUnit -> []
  SIxFin p -> [DimStatic (fromIntegral (natVal p))]
  SIxDyn p -> [DimDyn (Text.pack (symbolVal p))]
  SIxProd a b -> sixFlatDims a <> sixFlatDims b
  SIxSum a b ->
    case (sixStaticSize a, sixStaticSize b) of
      (Just m, Just n) -> [DimStatic (m + n)]
      _ -> [DimDyn (Text.pack (show (SIxSum a b)))]
  SIxTagged _ a -> sixFlatDims a

-- | Decidable type-level equality on index-set singletons.
eqSIx :: SIx i -> SIx j -> Maybe (i :~: j)
eqSIx SIxUnit SIxUnit = Just Refl
eqSIx (SIxFin p1) (SIxFin p2) =
  case sameNat p1 p2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
eqSIx (SIxDyn p1) (SIxDyn p2) =
  case sameSymbol p1 p2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
eqSIx (SIxProd a1 b1) (SIxProd a2 b2) = do
  Refl <- eqSIx a1 a2
  Refl <- eqSIx b1 b2
  Just Refl
eqSIx (SIxSum a1 b1) (SIxSum a2 b2) = do
  Refl <- eqSIx a1 a2
  Refl <- eqSIx b1 b2
  Just Refl
eqSIx (SIxTagged p1 a1) (SIxTagged p2 a2) = do
  Refl <- sameSymbol p1 p2
  Refl <- eqSIx a1 a2
  Just Refl
eqSIx _ _ = Nothing

frameBS :: BS.ByteString -> BSB.Builder
frameBS bs = BSB.int64LE (fromIntegral (BS.length bs)) <> BSB.byteString bs

sixBuilder :: SIx i -> BSB.Builder
sixBuilder = \case
  SIxUnit -> frameBS "U"
  SIxFin p -> frameBS "F" <> BSB.int64LE (fromIntegral (natVal p))
  SIxDyn p -> frameBS "D" <> frameBS (Text.encodeUtf8 (Text.pack (symbolVal p)))
  SIxProd a b -> frameBS "P" <> sixBuilder a <> sixBuilder b
  SIxSum a b -> frameBS "S" <> sixBuilder a <> sixBuilder b
  SIxTagged p a -> frameBS "T" <> frameBS (Text.encodeUtf8 (Text.pack (symbolVal p))) <> sixBuilder a

sixDigest :: SIx i -> BS.ByteString
sixDigest = SHA256.hashlazy . BSB.toLazyByteString . sixBuilder

--------------------------------------------------------------------------------
-- 2. Bijective Index Isomorphisms (`IxIso`)
--------------------------------------------------------------------------------

-- | Bijective, invertible index transformation @i <-> j@.
-- Reverse-mode transposition and scatter through an 'IxIso' always compile to
-- pure coalesced views with zero copies and zero atomics.
data IxIso (i :: IxKind) (j :: IxKind) where
  IsoRefl :: !(SIx i) -> IxIso i i
  IsoSwap :: !(SIx i) -> !(SIx j) -> IxIso ('IxProd i j) ('IxProd j i)
  IsoAssocL :: !(SIx i) -> !(SIx j) -> !(SIx k) -> IxIso ('IxProd i ('IxProd j k)) ('IxProd ('IxProd i j) k)
  IsoAssocR :: !(SIx i) -> !(SIx j) -> !(SIx k) -> IxIso ('IxProd ('IxProd i j) k) ('IxProd i ('IxProd j k))
  IsoUnitL :: !(SIx i) -> IxIso ('IxProd 'IxUnit i) i
  IsoUnitLInv :: !(SIx i) -> IxIso i ('IxProd 'IxUnit i)
  IsoUnitR :: !(SIx i) -> IxIso ('IxProd i 'IxUnit) i
  IsoUnitRInv :: !(SIx i) -> IxIso i ('IxProd i 'IxUnit)
  IsoRev :: !(SIx i) -> IxIso i i
  IsoShift :: !(SIx i) -> !Int -> IxIso i i
  IsoReshape :: !(SIx i) -> !(SIx j) -> IxIso i j
  IsoTag :: (KnownSymbol s) => !(Proxy s) -> !(SIx i) -> IxIso i ('IxTagged s i)
  IsoUntag :: (KnownSymbol s) => !(Proxy s) -> !(SIx i) -> IxIso ('IxTagged s i) i
  IsoProd :: !(IxIso i1 j1) -> !(IxIso i2 j2) -> IxIso ('IxProd i1 i2) ('IxProd j1 j2)
  IsoSum :: !(IxIso i1 j1) -> !(IxIso i2 j2) -> IxIso ('IxSum i1 i2) ('IxSum j1 j2)
  IsoComp :: !(IxIso j k) -> !(IxIso i j) -> IxIso i k

isoSrc :: IxIso i j -> SIx i
isoSrc = \case
  IsoRefl s -> s
  IsoSwap a b -> SIxProd a b
  IsoAssocL a b c -> SIxProd a (SIxProd b c)
  IsoAssocR a b c -> SIxProd (SIxProd a b) c
  IsoUnitL a -> SIxProd SIxUnit a
  IsoUnitLInv a -> a
  IsoUnitR a -> SIxProd a SIxUnit
  IsoUnitRInv a -> a
  IsoRev a -> a
  IsoShift a _ -> a
  IsoReshape a _ -> a
  IsoTag _ a -> a
  IsoUntag p a -> SIxTagged p a
  IsoProd f g -> SIxProd (isoSrc f) (isoSrc g)
  IsoSum f g -> SIxSum (isoSrc f) (isoSrc g)
  IsoComp _ f -> isoSrc f

isoDst :: IxIso i j -> SIx j
isoDst = \case
  IsoRefl s -> s
  IsoSwap a b -> SIxProd b a
  IsoAssocL a b c -> SIxProd (SIxProd a b) c
  IsoAssocR a b c -> SIxProd a (SIxProd b c)
  IsoUnitL a -> a
  IsoUnitLInv a -> SIxProd SIxUnit a
  IsoUnitR a -> a
  IsoUnitRInv a -> SIxProd a SIxUnit
  IsoRev a -> a
  IsoShift a _ -> a
  IsoReshape _ b -> b
  IsoTag p a -> SIxTagged p a
  IsoUntag _ a -> a
  IsoProd f g -> SIxProd (isoDst f) (isoDst g)
  IsoSum f g -> SIxSum (isoDst f) (isoDst g)
  IsoComp g _ -> isoDst g

-- | Exact structural inverse of any 'IxIso' (@invIso . invIso === id@).
invIso :: IxIso i j -> IxIso j i
invIso = \case
  IsoRefl s -> IsoRefl s
  IsoSwap a b -> IsoSwap b a
  IsoAssocL a b c -> IsoAssocR a b c
  IsoAssocR a b c -> IsoAssocL a b c
  IsoUnitL a -> IsoUnitLInv a
  IsoUnitLInv a -> IsoUnitL a
  IsoUnitR a -> IsoUnitRInv a
  IsoUnitRInv a -> IsoUnitR a
  IsoRev a -> IsoRev a
  IsoShift a k -> IsoShift a (negate k)
  IsoReshape a b -> IsoReshape b a
  IsoTag p a -> IsoUntag p a
  IsoUntag p a -> IsoTag p a
  IsoProd f g -> IsoProd (invIso f) (invIso g)
  IsoSum f g -> IsoSum (invIso f) (invIso g)
  IsoComp g f -> IsoComp (invIso f) (invIso g)

ixId :: (IxSet i) => IxIso i i
ixId = IsoRefl six

ixSwap :: (IxSet i, IxSet j) => IxIso ('IxProd i j) ('IxProd j i)
ixSwap = IsoSwap six six

ixAssocL :: (IxSet i, IxSet j, IxSet k) => IxIso ('IxProd i ('IxProd j k)) ('IxProd ('IxProd i j) k)
ixAssocL = IsoAssocL six six six

ixAssocR :: (IxSet i, IxSet j, IxSet k) => IxIso ('IxProd ('IxProd i j) k) ('IxProd i ('IxProd j k))
ixAssocR = IsoAssocR six six six

ixRev :: (IxSet i) => IxIso i i
ixRev = IsoRev six

ixShift :: (IxSet i) => Int -> IxIso i i
ixShift = IsoShift six

-- | Flat ordinal-preserving reshape between index sets of equal cardinality.
-- Validates equality immediately when both index sets are statically known.
ixReshape :: (IxSet i, IxSet j) => IxIso i j
ixReshape =
  let sa = six
      sb = six
   in case (sixStaticSize sa, sixStaticSize sb) of
        (Just m, Just n)
          | m /= n ->
              error $
                "ixReshape: cardinality mismatch ("
                  <> show sa
                  <> " has size "
                  <> show m
                  <> ", "
                  <> show sb
                  <> " has size "
                  <> show n
                  <> ")"
        _ -> IsoReshape sa sb

ixTag :: forall s i. (KnownSymbol s, IxSet i) => IxIso i ('IxTagged s i)
ixTag = IsoTag (Proxy @s) six

ixUntag :: forall s i. (KnownSymbol s, IxSet i) => IxIso ('IxTagged s i) i
ixUntag = IsoUntag (Proxy @s) six

-- | Smart composition simplifying identity and self-inverse chains.
ixCompIso :: IxIso j k -> IxIso i j -> IxIso i k
ixCompIso (IsoRefl _) f = f
ixCompIso g (IsoRefl _) = g
ixCompIso g f = IsoComp g f

isoBuilder :: IxIso i j -> BSB.Builder
isoBuilder = \case
  IsoRefl s -> frameBS "IR" <> sixBuilder s
  IsoSwap a b -> frameBS "ISw" <> sixBuilder a <> sixBuilder b
  IsoAssocL a b c -> frameBS "IAL" <> sixBuilder a <> sixBuilder b <> sixBuilder c
  IsoAssocR a b c -> frameBS "IAR" <> sixBuilder a <> sixBuilder b <> sixBuilder c
  IsoUnitL a -> frameBS "IUL" <> sixBuilder a
  IsoUnitLInv a -> frameBS "IULI" <> sixBuilder a
  IsoUnitR a -> frameBS "IUR" <> sixBuilder a
  IsoUnitRInv a -> frameBS "IURI" <> sixBuilder a
  IsoRev a -> frameBS "IRv" <> sixBuilder a
  IsoShift a k -> frameBS "ISh" <> sixBuilder a <> BSB.int64LE (fromIntegral k)
  IsoReshape a b -> frameBS "IRe" <> sixBuilder a <> sixBuilder b
  IsoTag p a -> frameBS "ITg" <> frameBS (Text.encodeUtf8 (Text.pack (symbolVal p))) <> sixBuilder a
  IsoUntag p a -> frameBS "IUt" <> frameBS (Text.encodeUtf8 (Text.pack (symbolVal p))) <> sixBuilder a
  IsoProd f g -> frameBS "IPr" <> isoBuilder f <> isoBuilder g
  IsoSum f g -> frameBS "ISm" <> isoBuilder f <> isoBuilder g
  IsoComp g f -> frameBS "ICp" <> isoBuilder g <> isoBuilder f

isoDigest :: IxIso i j -> BS.ByteString
isoDigest = SHA256.hashlazy . BSB.toLazyByteString . isoBuilder

--------------------------------------------------------------------------------
-- 3. Partial Injective Index Prisms (`IxPrism`)
--------------------------------------------------------------------------------

-- | Partial injective index morphism @i ↪ j@ equipped with an exact partial
-- inverse @j ⇀ Maybe i@. Enables zero-atomic masked scatter and reverse AD.
data IxPrism (i :: IxKind) (j :: IxKind) where
  PrismIso :: !(IxIso i j) -> IxPrism i j
  PrismSlice :: !(SIx i) -> !(SIx j) -> !Int -> IxPrism i j
  PrismStride :: !(SIx i) -> !(SIx j) -> !Int -> !Int -> IxPrism i j
  PrismInL :: !(SIx i) -> !(SIx j) -> IxPrism i ('IxSum i j)
  PrismInR :: !(SIx i) -> !(SIx j) -> IxPrism j ('IxSum i j)
  PrismDiag :: !(SIx i) -> IxPrism i ('IxProd i i)
  PrismProd :: !(IxPrism i1 j1) -> !(IxPrism i2 j2) -> IxPrism ('IxProd i1 i2) ('IxProd j1 j2)
  PrismComp :: !(IxPrism j k) -> !(IxPrism i j) -> IxPrism i k

prismSrc :: IxPrism i j -> SIx i
prismSrc = \case
  PrismIso iso -> isoSrc iso
  PrismSlice a _ _ -> a
  PrismStride a _ _ _ -> a
  PrismInL a _ -> a
  PrismInR _ b -> b
  PrismDiag a -> a
  PrismProd f g -> SIxProd (prismSrc f) (prismSrc g)
  PrismComp _ f -> prismSrc f

prismDst :: IxPrism i j -> SIx j
prismDst = \case
  PrismIso iso -> isoDst iso
  PrismSlice _ b _ -> b
  PrismStride _ b _ _ -> b
  PrismInL a b -> SIxSum a b
  PrismInR a b -> SIxSum a b
  PrismDiag a -> SIxProd a a
  PrismProd f g -> SIxProd (prismDst f) (prismDst g)
  PrismComp g _ -> prismDst g

ixPrismIso :: IxIso i j -> IxPrism i j
ixPrismIso = PrismIso

ixSlice :: (IxSet i, IxSet j) => Int -> IxPrism i j
ixSlice off =
  let sa = six
      sb = six
   in case (sixStaticSize sa, sixStaticSize sb) of
        (Just m, Just n)
          | off < 0 || off + m > n ->
              error $ "ixSlice: out of bounds slice [" <> show off <> ".." <> show (off + m) <> ") in size " <> show n
        _ -> PrismSlice sa sb off

ixStride :: (IxSet i, IxSet j) => Int -> Int -> IxPrism i j
ixStride off stride
  | stride <= 0 = error "ixStride: stride must be positive"
  | off < 0 = error "ixStride: offset must be non-negative"
  | otherwise =
      let sa = six
          sb = six
       in case (sixStaticSize sa, sixStaticSize sb) of
            (Just m, Just n)
              | m > 0 && off + (m - 1) * stride >= n ->
                  error $ "ixStride: strided range exceeds target size " <> show n
            _ -> PrismStride sa sb off stride

ixInL :: (IxSet i, IxSet j) => IxPrism i ('IxSum i j)
ixInL = PrismInL six six

ixInR :: (IxSet i, IxSet j) => IxPrism j ('IxSum i j)
ixInR = PrismInR six six

ixDiag :: (IxSet i) => IxPrism i ('IxProd i i)
ixDiag = PrismDiag six

ixCompPrism :: IxPrism j k -> IxPrism i j -> IxPrism i k
ixCompPrism (PrismIso (IsoRefl _)) f = f
ixCompPrism g (PrismIso (IsoRefl _)) = g
ixCompPrism (PrismIso g) (PrismIso f) = PrismIso (ixCompIso g f)
ixCompPrism g f = PrismComp g f

prismBuilder :: IxPrism i j -> BSB.Builder
prismBuilder = \case
  PrismIso iso -> frameBS "PI" <> isoBuilder iso
  PrismSlice a b o -> frameBS "PSl" <> sixBuilder a <> sixBuilder b <> BSB.int64LE (fromIntegral o)
  PrismStride a b o s -> frameBS "PSt" <> sixBuilder a <> sixBuilder b <> BSB.int64LE (fromIntegral o) <> BSB.int64LE (fromIntegral s)
  PrismInL a b -> frameBS "PL" <> sixBuilder a <> sixBuilder b
  PrismInR a b -> frameBS "PR" <> sixBuilder a <> sixBuilder b
  PrismDiag a -> frameBS "PD" <> sixBuilder a
  PrismProd f g -> frameBS "PPr" <> prismBuilder f <> prismBuilder g
  PrismComp g f -> frameBS "PCp" <> prismBuilder g <> prismBuilder f

prismDigest :: IxPrism i j -> BS.ByteString
prismDigest = SHA256.hashlazy . BSB.toLazyByteString . prismBuilder

--------------------------------------------------------------------------------
-- 4. Affine / Stencil Index Morphisms (`IxAffine`) & Unified `IxMorph`
--------------------------------------------------------------------------------

-- | Constant-stride affine / stencil index maps (e.g., broadcasts and 1D/2D
-- sliding convolution windows).
data IxAffine (i :: IxKind) (j :: IxKind) where
  -- | Constant broadcast @i -> 1@.
  AffineBroadcast :: !(SIx i) -> IxAffine i 'IxUnit
  -- | 1D sliding window @(out, ker) -> stride * out + dilation * ker - pad@.
  -- Out-of-bounds indices evaluate to invalid (zero-padded).
  AffineWindow1D ::
    !(SIx outIx) ->
    !(SIx kerIx) ->
    !(SIx inIx) ->
    !Int -> -- stride
    !Int -> -- dilation
    !Int -> -- pad
    IxAffine ('IxProd outIx kerIx) inIx
  AffineProd :: !(IxAffine i1 j1) -> !(IxAffine i2 j2) -> IxAffine ('IxProd i1 i2) ('IxProd j1 j2)

affineSrc :: IxAffine i j -> SIx i
affineSrc = \case
  AffineBroadcast s -> s
  AffineWindow1D o k _ _ _ _ -> SIxProd o k
  AffineProd f g -> SIxProd (affineSrc f) (affineSrc g)

affineDst :: IxAffine i j -> SIx j
affineDst = \case
  AffineBroadcast _ -> SIxUnit
  AffineWindow1D _ _ inp _ _ _ -> inp
  AffineProd f g -> SIxProd (affineDst f) (affineDst g)

ixBroadcast :: (IxSet i) => IxAffine i 'IxUnit
ixBroadcast = AffineBroadcast six

ixWindow1D :: (IxSet o, IxSet k, IxSet inp) => Int -> Int -> Int -> IxAffine ('IxProd o k) inp
ixWindow1D = AffineWindow1D six six six

affineBuilder :: IxAffine i j -> BSB.Builder
affineBuilder = \case
  AffineBroadcast s -> frameBS "AB" <> sixBuilder s
  AffineWindow1D o k inp st dil pad ->
    frameBS "AW1"
      <> sixBuilder o
      <> sixBuilder k
      <> sixBuilder inp
      <> BSB.int64LE (fromIntegral st)
      <> BSB.int64LE (fromIntegral dil)
      <> BSB.int64LE (fromIntegral pad)
  AffineProd f g -> frameBS "APr" <> affineBuilder f <> affineBuilder g

affineDigest :: IxAffine i j -> BS.ByteString
affineDigest = SHA256.hashlazy . BSB.toLazyByteString . affineBuilder

data IxMorph (i :: IxKind) (j :: IxKind) where
  MorphIso :: !(IxIso i j) -> IxMorph i j
  MorphPrism :: !(IxPrism i j) -> IxMorph i j
  MorphAffine :: !(IxAffine i j) -> IxMorph i j

morphSrc :: IxMorph i j -> SIx i
morphSrc = \case
  MorphIso f -> isoSrc f
  MorphPrism f -> prismSrc f
  MorphAffine f -> affineSrc f

morphDst :: IxMorph i j -> SIx j
morphDst = \case
  MorphIso f -> isoDst f
  MorphPrism f -> prismDst f
  MorphAffine f -> affineDst f

morphDigest :: IxMorph i j -> BS.ByteString
morphDigest = \case
  MorphIso f -> SHA256.hash ("MI:" <> isoDigest f)
  MorphPrism f -> SHA256.hash ("MP:" <> prismDigest f)
  MorphAffine f -> SHA256.hash ("MA:" <> affineDigest f)

morphIsBijective :: IxMorph i j -> Bool
morphIsBijective = \case
  MorphIso _ -> True
  MorphPrism (PrismIso _) -> True
  _ -> False

morphIsInjective :: IxMorph i j -> Bool
morphIsInjective = \case
  MorphIso _ -> True
  MorphPrism _ -> True
  MorphAffine _ -> False

--------------------------------------------------------------------------------
-- 5. Ordinal-Level Evaluation (Host `Int` and Embedded Janus `e Int64`)
--------------------------------------------------------------------------------

evalIsoOrd :: DimEnv -> IxIso i j -> Int -> Int
evalIsoOrd env = \case
  IsoRefl _ -> id
  IsoSwap sa sb ->
    let na = sixEvalSize env sa
        nb = sixEvalSize env sb
     in \idx ->
          let (ia, ib) = idx `quotRem` nb
           in ib * na + ia
  IsoAssocL _ _ _ -> id
  IsoAssocR _ _ _ -> id
  IsoUnitL _ -> id
  IsoUnitLInv _ -> id
  IsoUnitR _ -> id
  IsoUnitRInv _ -> id
  IsoRev sa ->
    let n = sixEvalSize env sa
     in \idx -> n - 1 - idx
  IsoShift sa k ->
    let n = max 1 (sixEvalSize env sa)
        shiftAmt = ((k `rem` n) + n) `rem` n
     in \idx -> (idx + shiftAmt) `rem` n
  IsoReshape _ _ -> id
  IsoTag _ _ -> id
  IsoUntag _ _ -> id
  IsoProd f g ->
    let nb1 = sixEvalSize env (isoSrc g)
        nb2 = sixEvalSize env (isoDst g)
        ef = evalIsoOrd env f
        eg = evalIsoOrd env g
     in \idx ->
          let (ia, ib) = idx `quotRem` nb1
           in ef ia * nb2 + eg ib
  IsoSum f g ->
    let na1 = sixEvalSize env (isoSrc f)
        na2 = sixEvalSize env (isoDst f)
        ef = evalIsoOrd env f
        eg = evalIsoOrd env g
     in \idx ->
          if idx < na1 then ef idx else na2 + eg (idx - na1)
  IsoComp g f -> evalIsoOrd env g . evalIsoOrd env f

evalIsoOrdE ::
  ( Num (e Int64),
    ExpIntegral e Int64,
    ExpOrd e Int64,
    ExpCond e,
    JanusTyped e Int64
  ) =>
  DimEnv ->
  IxIso i j ->
  e Int64 ->
  e Int64
evalIsoOrdE env = \case
  IsoRefl _ -> id
  IsoSwap sa sb ->
    let na = fromIntegral (sixEvalSize env sa)
        nb = fromIntegral (sixEvalSize env sb)
     in \idx ->
          let ia = idx `quotient` nb
              ib = idx `remainder` nb
           in ib * na + ia
  IsoAssocL _ _ _ -> id
  IsoAssocR _ _ _ -> id
  IsoUnitL _ -> id
  IsoUnitLInv _ -> id
  IsoUnitR _ -> id
  IsoUnitRInv _ -> id
  IsoRev sa ->
    let n = fromIntegral (sixEvalSize env sa)
     in \idx -> (n - 1) - idx
  IsoShift sa k ->
    let n = max 1 (sixEvalSize env sa)
        shiftAmt = fromIntegral (((k `rem` n) + n) `rem` n)
        nE = fromIntegral n
     in \idx -> (idx + shiftAmt) `remainder` nE
  IsoReshape _ _ -> id
  IsoTag _ _ -> id
  IsoUntag _ _ -> id
  IsoProd f g ->
    let nb1 = fromIntegral (sixEvalSize env (isoSrc g))
        nb2 = fromIntegral (sixEvalSize env (isoDst g))
        ef = evalIsoOrdE env f
        eg = evalIsoOrdE env g
     in \idx ->
          let ia = idx `quotient` nb1
              ib = idx `remainder` nb1
           in ef ia * nb2 + eg ib
  IsoSum f g ->
    let na1 = fromIntegral (sixEvalSize env (isoSrc f))
        na2 = fromIntegral (sixEvalSize env (isoDst f))
        ef = evalIsoOrdE env f
        eg = evalIsoOrdE env g
     in \idx ->
          ifThenElse (idx `lt` na1) (ef idx) (na2 + eg (idx - na1))
  IsoComp g f -> evalIsoOrdE env g . evalIsoOrdE env f

evalPrismForwardOrd :: DimEnv -> IxPrism i j -> Int -> Int
evalPrismForwardOrd env = \case
  PrismIso iso -> evalIsoOrd env iso
  PrismSlice _ _ off -> (+ off)
  PrismStride _ _ off st -> \idx -> off + idx * st
  PrismInL _ _ -> id
  PrismInR sa _ -> (+ sixEvalSize env sa)
  PrismDiag sa ->
    let n = sixEvalSize env sa
     in \idx -> idx * n + idx
  PrismProd f g ->
    let nb1 = sixEvalSize env (prismSrc g)
        nb2 = sixEvalSize env (prismDst g)
        ef = evalPrismForwardOrd env f
        eg = evalPrismForwardOrd env g
     in \idx ->
          let (ia, ib) = idx `quotRem` nb1
           in ef ia * nb2 + eg ib
  PrismComp g f -> evalPrismForwardOrd env g . evalPrismForwardOrd env f

evalPrismForwardOrdE ::
  ( Num (e Int64),
    ExpIntegral e Int64,
    ExpOrd e Int64,
    ExpCond e,
    JanusTyped e Int64
  ) =>
  DimEnv ->
  IxPrism i j ->
  e Int64 ->
  e Int64
evalPrismForwardOrdE env = \case
  PrismIso iso -> evalIsoOrdE env iso
  PrismSlice _ _ off -> (+ fromIntegral off)
  PrismStride _ _ off st -> \idx -> fromIntegral off + idx * fromIntegral st
  PrismInL _ _ -> id
  PrismInR sa _ -> (+ fromIntegral (sixEvalSize env sa))
  PrismDiag sa ->
    let n = fromIntegral (sixEvalSize env sa)
     in \idx -> idx * n + idx
  PrismProd f g ->
    let nb1 = fromIntegral (sixEvalSize env (prismSrc g))
        nb2 = fromIntegral (sixEvalSize env (prismDst g))
        ef = evalPrismForwardOrdE env f
        eg = evalPrismForwardOrdE env g
     in \idx ->
          let ia = idx `quotient` nb1
              ib = idx `remainder` nb1
           in ef ia * nb2 + eg ib
  PrismComp g f -> evalPrismForwardOrdE env g . evalPrismForwardOrdE env f

-- | Exact partial inverse of an 'IxPrism' (@Nothing@ if target index is not in the image).
evalPrismBackwardOrd :: DimEnv -> IxPrism i j -> Int -> Maybe Int
evalPrismBackwardOrd env = \case
  PrismIso iso -> Just . evalIsoOrd env (invIso iso)
  PrismSlice sa _ off ->
    let m = sixEvalSize env sa
     in \j ->
          let i = j - off
           in if i >= 0 && i < m then Just i else Nothing
  PrismStride sa _ off st ->
    let m = sixEvalSize env sa
     in \j ->
          let d = j - off
              (q, r) = d `quotRem` st
           in if d >= 0 && r == 0 && q < m then Just q else Nothing
  PrismInL sa _ ->
    let m = sixEvalSize env sa
     in \j -> if j >= 0 && j < m then Just j else Nothing
  PrismInR sa sb ->
    let m = sixEvalSize env sa
        n = sixEvalSize env sb
     in \j ->
          let i = j - m
           in if i >= 0 && i < n then Just i else Nothing
  PrismDiag sa ->
    let n = sixEvalSize env sa
     in \j ->
          let (r, c) = j `quotRem` n
           in if r == c && r >= 0 && r < n then Just r else Nothing
  PrismProd f g ->
    let nb1 = sixEvalSize env (prismSrc g)
        nb2 = sixEvalSize env (prismDst g)
        ef = evalPrismBackwardOrd env f
        eg = evalPrismBackwardOrd env g
     in \j -> do
          let (ja, jb) = j `quotRem` nb2
          ia <- ef ja
          ib <- eg jb
          Just (ia * nb1 + ib)
  PrismComp g f -> \k -> evalPrismBackwardOrd env g k >>= evalPrismBackwardOrd env f

-- | Embedded branchless partial inverse @(isValid, srcOrd)@ for zero-atomic
-- masked GPU/CPU scatter and reverse-mode views.
evalPrismBackwardOrdE ::
  ( Num (e Int64),
    ExpIntegral e Int64,
    ExpEq e Int64,
    ExpOrd e Int64,
    ExpBool e,
    ExpCond e,
    JanusTyped e Int64
  ) =>
  DimEnv ->
  IxPrism i j ->
  e Int64 ->
  (e Bool, e Int64)
evalPrismBackwardOrdE env = \case
  PrismIso iso -> \j -> (true, evalIsoOrdE env (invIso iso) j)
  PrismSlice sa _ off ->
    let m = fromIntegral (sixEvalSize env sa)
        offE = fromIntegral off
     in \j ->
          let i = j - offE
              ok = (i `ge` 0) `land` (i `lt` m)
           in (ok, ifThenElse ok i 0)
  PrismStride sa _ off st ->
    let m = fromIntegral (sixEvalSize env sa)
        offE = fromIntegral off
        stE = fromIntegral st
     in \j ->
          let d = j - offE
              q = d `quotient` stE
              r = d `remainder` stE
              ok = (d `ge` 0) `land` (r `eq` 0) `land` (q `lt` m)
           in (ok, ifThenElse ok q 0)
  PrismInL sa _ ->
    let m = fromIntegral (sixEvalSize env sa)
     in \j ->
          let ok = (j `ge` 0) `land` (j `lt` m)
           in (ok, ifThenElse ok j 0)
  PrismInR sa sb ->
    let m = fromIntegral (sixEvalSize env sa)
        n = fromIntegral (sixEvalSize env sb)
     in \j ->
          let i = j - m
              ok = (i `ge` 0) `land` (i `lt` n)
           in (ok, ifThenElse ok i 0)
  PrismDiag sa ->
    let n = fromIntegral (sixEvalSize env sa)
     in \j ->
          let r = j `quotient` n
              c = j `remainder` n
              ok = (r `eq` c) `land` (r `ge` 0) `land` (r `lt` n)
           in (ok, ifThenElse ok r 0)
  PrismProd f g ->
    let nb1 = fromIntegral (sixEvalSize env (prismSrc g))
        nb2 = fromIntegral (sixEvalSize env (prismDst g))
        ef = evalPrismBackwardOrdE env f
        eg = evalPrismBackwardOrdE env g
     in \j ->
          let ja = j `quotient` nb2
              jb = j `remainder` nb2
              (okA, ia) = ef ja
              (okB, ib) = eg jb
              ok = okA `land` okB
           in (ok, ifThenElse ok (ia * nb1 + ib) 0)
  PrismComp g f -> \k ->
    let (okG, j) = evalPrismBackwardOrdE env g k
        (okF, i) = evalPrismBackwardOrdE env f j
        ok = okG `land` okF
     in (ok, ifThenElse ok i 0)

-- | Forward affine map evaluation returning @(inBounds, dstOrd)@.
evalAffineForwardOrd :: DimEnv -> IxAffine i j -> Int -> (Bool, Int)
evalAffineForwardOrd env = \case
  AffineBroadcast _ -> const (True, 0)
  AffineWindow1D _ sk sinp st dil pad ->
    let nk = sixEvalSize env sk
        nin = sixEvalSize env sinp
     in \idx ->
          let (io, ik) = idx `quotRem` nk
              pos = io * st + ik * dil - pad
           in if pos >= 0 && pos < nin then (True, pos) else (False, 0)
  AffineProd f g ->
    let nb1 = sixEvalSize env (affineSrc g)
        nb2 = sixEvalSize env (affineDst g)
        ef = evalAffineForwardOrd env f
        eg = evalAffineForwardOrd env g
     in \idx ->
          let (ia, ib) = idx `quotRem` nb1
              (okA, ja) = ef ia
              (okB, jb) = eg ib
           in if okA && okB then (True, ja * nb2 + jb) else (False, 0)

evalAffineForwardOrdE ::
  ( Num (e Int64),
    ExpIntegral e Int64,
    ExpOrd e Int64,
    ExpBool e,
    ExpCond e,
    JanusTyped e Int64
  ) =>
  DimEnv ->
  IxAffine i j ->
  e Int64 ->
  (e Bool, e Int64)
evalAffineForwardOrdE env = \case
  AffineBroadcast _ -> const (true, 0)
  AffineWindow1D _ sk sinp st dil pad ->
    let nk = fromIntegral (sixEvalSize env sk)
        nin = fromIntegral (sixEvalSize env sinp)
        stE = fromIntegral st
        dilE = fromIntegral dil
        padE = fromIntegral pad
     in \idx ->
          let io = idx `quotient` nk
              ik = idx `remainder` nk
              pos = io * stE + ik * dilE - padE
              ok = (pos `ge` 0) `land` (pos `lt` nin)
           in (ok, ifThenElse ok pos 0)
  AffineProd f g ->
    let nb1 = fromIntegral (sixEvalSize env (affineSrc g))
        nb2 = fromIntegral (sixEvalSize env (affineDst g))
        ef = evalAffineForwardOrdE env f
        eg = evalAffineForwardOrdE env g
     in \idx ->
          let ia = idx `quotient` nb1
              ib = idx `remainder` nb1
              (okA, ja) = ef ia
              (okB, jb) = eg ib
              ok = okA `land` okB
           in (ok, ifThenElse ok (ja * nb2 + jb) 0)
