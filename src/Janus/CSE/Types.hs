{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Core AST, SSA variable keys, Merkle digests, scoped block representation,
-- and target capability witnesses for backend-agnostic Common Subexpression
-- Elimination ('CSEExp' and 'CSEM').
module Janus.CSE.Types
  ( -- * Content-Addressed Merkle Digests
    Digest (..),
    encodeStr,
    encodeInt,
    encodeInteger,
    encodeRational,
    encodeTypeRep,
    encodeSomeTypeRep,
    mkNodeDigest,

    -- * Disjoint Deterministic Variable & Reference Keys
    VarKey (..),
    encodeVarKey,
    VarId (..),
    RefKey (..),
    CSERef (..),

    -- * Target Backend Binding Capabilities
    CSETyped,
    SIMDVecWitness (..),
    SIMDMaskWitness (..),
    CSESIMDTarget (..),
    BindCap (..),
    CSETargetExp (..),
    BindRep (..),
    resolveBindRep,
    cseLetWithCap,
    cseLetMWithCap,

    -- * Associated SIMD Wrappers for `CSEExp`
    SIMDVector (..),
    SIMDMask (..),

    -- * Heterogeneous Argument Lists
    HArgs (..),
    hargsDigests,
    hargsFreeVars,
    hargsHasTrapOrFix,
    mapHArgsM,

    -- * Pure Expression AST (`CSEExp`)
    ShortCircuitOp (..),
    ExpMeta (..),
    expMeta,
    PureFunDef (..),
    MonadicFunDef (..),
    CSEExp (..),
    mkExpVar,
    mkExpLit,
    mkExpOp,
    mkExpZeroCostOp,
    mkExpListOp,
    mkExpShortCircuit,
    mkExpCond,
    mkExpUserLet,
    mkExpCSELet,
    mkExpFixCall,

    -- * Monadic Command & Structured Block IR (`CSEM`)
    Stm (..),
    ScopeItem (..),
    Block,
    ProgTree (..),
    CSEM (..),
    allocSSAId,
    emitStm,
    emitEffectVal,
    emitEffectUnit,
    prependItems,
    wrapScopeWith,
  )
where

import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Reader (MonadReader)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex)
import Data.Constraint (Dict (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable, typeRepFingerprint)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C (CFile, CInt, CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import GHC.Fingerprint (Fingerprint (..))
import GHC.TypeNats (Nat)
import Janus.Backend.C (JanusCT (..), JanusCTyped (..))
import Janus.Backend.C.CUDA.CPU ()
import Janus.Backend.C.CUDA.CPUSIMD ()
import Janus.Backend.C.CUDA.Kernel ()
import Janus.Backend.C.CUDA.SIMD.Base ()
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..), CUDAThreadInfo, JanusCUDA (..))
import Janus.Backend.C.LinuxX86_64.SIMD.Base ()
import Janus.Backend.C.LinuxX86_64.Types (LinuxX86_64C)
import Janus.Bench.Instances (SIMDMask (..), SIMDVector (..))
import Janus.Bench.Types (Bench (..))
import Janus.Command.Case (CmdCase)
import Janus.Command.Cond (CmdCond)
import Janus.Command.Format (CmdString)
import Janus.Command.Memory (CmdRefPtr)
import Janus.Command.Range (CmdRange)
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile)
import Janus.Control.Reader.Base (ExpNewtype (..), ReaderExp (..))
import Janus.Expression.Cast (janusCReallyUnsafeCast)
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.SIMD (ExpSIMD (..), HostSIMDMask (..), HostSIMDValue (..), SIMDElem, SIMDShape)
import Janus.SPMD.Gang (CmdOptimalGang)
import Janus.SPMD.Types (SIMDPair, VoidPtr (..))
import Janus.Typed (JanusTyped, KnownJanusType, TypeableTyped)
import qualified Type.Reflection as TR

--------------------------------------------------------------------------------
-- Content-Addressed Merkle Digests
--------------------------------------------------------------------------------

-- | 256-bit SHA-256 Merkle digest uniquely identifying the structure, types,
-- and static immediates of a pure 'CSEExp' expression without 'StableName's.
newtype Digest = Digest {getDigest :: BS.ByteString}
  deriving stock (Eq, Ord)

instance Show Digest where
  show (Digest bs) = "Digest:" <> take 16 (BS8.unpack (B16.encode bs))

encodeStr :: String -> BS.ByteString
encodeStr = Text.encodeUtf8 . Text.pack

encodeInt :: Int -> BS.ByteString
encodeInt n = BS.toStrict (BSB.toLazyByteString (BSB.int64LE (fromIntegral n)))

encodeInteger :: Integer -> BS.ByteString
encodeInteger n = BS8.pack (show n)

encodeRational :: Rational -> BS.ByteString
encodeRational q = BS8.pack (show (numerator q) <> "/" <> show (denominator q))

encodeSomeTypeRep :: TR.SomeTypeRep -> BS.ByteString
encodeSomeTypeRep str =
  let Fingerprint w1 w2 = typeRepFingerprint str
   in BS.toStrict (BSB.toLazyByteString (BSB.word64LE w1 <> BSB.word64LE w2))

encodeTypeRep :: TR.TypeRep a -> BS.ByteString
encodeTypeRep tr = encodeSomeTypeRep (TR.SomeTypeRep tr)

frameChunk :: BS.ByteString -> BSB.Builder
frameChunk bs = BSB.int64LE (fromIntegral (BS.length bs)) <> BSB.byteString bs

-- | Construct a collision-resistant, unambiguous length-framed SHA-256 Merkle
-- digest from an operator tag, immediate byte payloads, and child digests.
mkNodeDigest :: BS.ByteString -> [BS.ByteString] -> [Digest] -> Digest
mkNodeDigest tag imms children =
  let builder =
        frameChunk tag
          <> BSB.int64LE (fromIntegral (length imms))
          <> foldMap frameChunk imms
          <> BSB.int64LE (fromIntegral (length children))
          <> foldMap (frameChunk . getDigest) children
   in Digest (SHA256.hashlazy (BSB.toLazyByteString builder))

--------------------------------------------------------------------------------
-- Disjoint Deterministic Variable & Reference Keys
--------------------------------------------------------------------------------

-- | Deterministic, collision-free variable identifier covering all binding
-- forms across pure and monadic Janus code without 'IO' or 'StableName'.
data VarKey
  = -- | Formal parameter of a top-level function reified by @cse@ / @interpretCSE@.
    VarParam !Int
  | -- | Immutable SSA result bound by a monadic command in 'CSEM'.
    VarSSA !Int
  | -- | Pure binder introduced by user-written 'let_' (keyed by RHS Merkle digest).
    VarUserLet !Digest
  | -- | Formal parameter of a local recursive function introduced via @ExpFix@.
    VarLocalFun !Digest !Int
  | -- | Fresh variable synthesized by the CSE pass for a shared subexpression.
    VarCSE !Int
  | -- | Immutable CUDA thread/block/grid coordinate field (0..11).
    VarCUDAField !Int
  deriving stock (Eq, Ord, Show)

encodeVarKey :: VarKey -> BS.ByteString
encodeVarKey = \case
  VarParam i -> "P:" <> encodeInt i
  VarSSA i -> "S:" <> encodeInt i
  VarUserLet (Digest d) -> "U:" <> d
  VarLocalFun (Digest fd) i -> "F:" <> fd <> ":" <> encodeInt i
  VarCSE i -> "C:" <> encodeInt i
  VarCUDAField i -> "G:" <> encodeInt i

data VarId (e :: K.Type -> K.Type) (a :: K.Type) = VarId
  { vidKey :: !VarKey,
    vidTypeRep :: !(TR.TypeRep a),
    vidBind :: !(Maybe (BindCap e a))
  }

data RefKey
  = RefParam !Int
  | RefLocal !Int
  deriving stock (Eq, Ord, Show)

-- | Mutable reference handle for @('CSEM' m e, 'CSEExp' e)@. Carries @m@, @e@,
-- and @a@ to satisfy the injective @Ref m e a = r | r -> m e a@ type family.
data CSERef (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) (a :: K.Type) = CSERef
  { cseRefKey :: !RefKey,
    cseRefTypeRep :: !(TR.TypeRep a),
    cseRefBind :: !(BindCap e a)
  }

--------------------------------------------------------------------------------
-- Target Backend Binding Capabilities & `JanusTyped (CSEExp e)`
--------------------------------------------------------------------------------

-- | Constraint bundle combining target @'JanusTyped' e a@ with @'Typeable' a@
-- for GADT-safe variable environment lookup without @unsafeCoerce@.
type CSETyped = TypeableTyped

data SIMDVecWitness (e :: K.Type -> K.Type) (n :: Nat) (a :: K.Type) where
  SIMDVecWitness ::
    (Coercible (SIMDVector e n a) baseV, JanusTyped e baseV) =>
    !(Proxy baseV) ->
    SIMDVecWitness e n a

data SIMDMaskWitness (e :: K.Type -> K.Type) (n :: Nat) (a :: K.Type) where
  SIMDMaskWitness ::
    (Coercible (SIMDMask e n a) baseM, JanusTyped e baseM) =>
    !(Proxy baseM) ->
    SIMDMaskWitness e n a

-- | Target SIMD expression capability class supplying representational witnesses
-- so method-polymorphic SIMD ops ('Janus.Expression.SIMD.vbitcast',
-- 'Janus.Expression.SIMD.vcvt', 'Janus.Expression.SIMD.vnarrowSat',
-- 'Janus.Expression.SIMD.vdp4aVecS') can be bound by CSE across all backends.
class
  ( ExpNewtype e,
    ExpSIMD e,
    Typeable e,
    KnownJanusType e Bool,
    KnownJanusType e Int8,
    KnownJanusType e Word8,
    KnownJanusType e Int16,
    KnownJanusType e Word16,
    KnownJanusType e Int32,
    KnownJanusType e Word32,
    KnownJanusType e Int64,
    KnownJanusType e Word64,
    KnownJanusType e Float,
    KnownJanusType e Double,
    KnownJanusType e (Complex Float),
    KnownJanusType e (Complex Double)
  ) =>
  CSESIMDTarget (e :: K.Type -> K.Type)
  where
  cseSIMDVecWitness :: (SIMDShape n a) => SIMDVecWitness e n a
  cseSIMDMaskWitness :: (SIMDShape n a) => SIMDMaskWitness e n a

instance CSESIMDTarget Identity where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness Identity n a
  cseSIMDVecWitness = SIMDVecWitness (Proxy @(SIMDVector Identity n a))
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness Identity n a
  cseSIMDMaskWitness = SIMDMaskWitness (Proxy @(SIMDMask Identity n a))

instance CSESIMDTarget LinuxX86_64C where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness LinuxX86_64C n a
  cseSIMDVecWitness = SIMDVecWitness (Proxy @(SIMDVector LinuxX86_64C n a))
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness LinuxX86_64C n a
  cseSIMDMaskWitness = SIMDMaskWitness (Proxy @(SIMDMask LinuxX86_64C n a))

instance CSESIMDTarget JanusCUDA where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness JanusCUDA n a
  cseSIMDVecWitness = SIMDVecWitness (Proxy @(SIMDVector JanusCUDA n a))
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness JanusCUDA n a
  cseSIMDMaskWitness = SIMDMaskWitness (Proxy @(SIMDMask JanusCUDA n a))

instance CSESIMDTarget CUDAIO where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness CUDAIO n a
  cseSIMDVecWitness = SIMDVecWitness (Proxy @(SIMDVector CUDAIO n a))
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness CUDAIO n a
  cseSIMDMaskWitness = SIMDMaskWitness (Proxy @(SIMDMask CUDAIO n a))

instance CSESIMDTarget CUDADejaFu where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness CUDADejaFu n a
  cseSIMDVecWitness = SIMDVecWitness (Proxy @(SIMDVector CUDADejaFu n a))
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness CUDADejaFu n a
  cseSIMDMaskWitness = SIMDMaskWitness (Proxy @(SIMDMask CUDADejaFu n a))

instance (Typeable r, CSESIMDTarget e) => CSESIMDTarget (ReaderExp r e) where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness (ReaderExp r e) n a
  cseSIMDVecWitness =
    case cseSIMDVecWitness @e @n @a of
      SIMDVecWitness p -> SIMDVecWitness p
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness (ReaderExp r e) n a
  cseSIMDMaskWitness =
    case cseSIMDMaskWitness @e @n @a of
      SIMDMaskWitness p -> SIMDMaskWitness p

instance (Typeable c, CSESIMDTarget e) => CSESIMDTarget (Bench c e) where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness (Bench c e) n a
  cseSIMDVecWitness =
    case cseSIMDVecWitness @e @n @a of
      SIMDVecWitness p -> SIMDVecWitness p
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness (Bench c e) n a
  cseSIMDMaskWitness =
    case cseSIMDMaskWitness @e @n @a of
      SIMDMaskWitness p -> SIMDMaskWitness p

-- | Witness enabling CSE to bind an expression of type @a@ via 'let_' or 'letM'
-- in target backend @e@ without imposing extra 'JanusTyped' constraints on
-- expression classes with fixed or composite result types.
data BindCap (e :: K.Type -> K.Type) (a :: K.Type) where
  BindDirect :: (JanusTyped e a) => BindCap e a
  BindBool :: BindCap e Bool
  BindInt32 :: BindCap e Int32
  BindWord32 :: BindCap e Word32
  BindInt64 :: BindCap e Int64
  BindWord64 :: BindCap e Word64
  BindCInt :: BindCap e CInt
  BindCFileP :: BindCap e (Ptr CFile)
  BindCString :: BindCap e CString
  BindVoidPtr :: BindCap e (Ptr ())
  BindPtrOf :: !(BindCap e x) -> BindCap e (Ptr x)
  BindPairOf :: !(BindCap e x) -> !(BindCap e y) -> BindCap e (SIMDPair x y)
  BindSIMDVec :: (CSESIMDTarget e, SIMDShape n a) => BindCap e (SIMDVector (CSEExp e) n a)
  BindSIMDMask :: (CSESIMDTarget e, SIMDShape n a) => BindCap e (SIMDMask (CSEExp e) n a)

-- | Target expression capability class supplying 'JanusTyped' witnesses for
-- built-in scalar, pointer, and pair types plus zero-cost @Ptr () <-> VoidPtr@
-- wrapping so even @e (Ptr ())@ subexpressions can be bound via 'let_'/'letM'.
class (ExpLet e, ExpNewtype e) => CSETargetExp (e :: K.Type -> K.Type) where
  cseTargetBool :: Dict (JanusTyped e Bool)
  cseTargetInt32 :: Dict (JanusTyped e Int32)
  cseTargetWord32 :: Dict (JanusTyped e Word32)
  cseTargetInt64 :: Dict (JanusTyped e Int64)
  cseTargetWord64 :: Dict (JanusTyped e Word64)
  cseTargetCInt :: Dict (JanusTyped e CInt)
  cseTargetCFileP :: Dict (JanusTyped e (Ptr CFile))
  cseTargetCStr :: Dict (JanusTyped e CString)
  cseTargetVoidPtr :: Dict (JanusTyped e VoidPtr)
  cseTargetPtr :: (JanusTyped e a) => Dict (JanusTyped e (Ptr a))
  cseTargetPair :: (JanusTyped e a, JanusTyped e b) => Dict (JanusTyped e (SIMDPair a b))
  cseWrapVoidPtr :: e (Ptr ()) -> e VoidPtr
  cseUnwrapVoidPtr :: e VoidPtr -> e (Ptr ())

instance CSETargetExp Identity where
  cseTargetBool = Dict
  cseTargetInt32 = Dict
  cseTargetWord32 = Dict
  cseTargetInt64 = Dict
  cseTargetWord64 = Dict
  cseTargetCInt = Dict
  cseTargetCFileP = Dict
  cseTargetCStr = Dict
  cseTargetVoidPtr = Dict
  cseTargetPtr = Dict
  cseTargetPair = Dict
  cseWrapVoidPtr = coerce
  cseUnwrapVoidPtr = coerce

instance CSETargetExp (JanusCT t) where
  cseTargetBool = Dict
  cseTargetInt32 = Dict
  cseTargetWord32 = Dict
  cseTargetInt64 = Dict
  cseTargetWord64 = Dict
  cseTargetCInt = Dict
  cseTargetCFileP = Dict
  cseTargetCStr = Dict
  cseTargetVoidPtr = Dict
  cseTargetPtr = Dict
  cseTargetPair = Dict
  cseWrapVoidPtr = janusCReallyUnsafeCast
  cseUnwrapVoidPtr = janusCReallyUnsafeCast

instance CSETargetExp JanusCUDA where
  cseTargetBool = Dict
  cseTargetInt32 = Dict
  cseTargetWord32 = Dict
  cseTargetInt64 = Dict
  cseTargetWord64 = Dict
  cseTargetCInt = Dict
  cseTargetCFileP = Dict
  cseTargetCStr = Dict
  cseTargetVoidPtr = Dict
  cseTargetPtr = Dict
  cseTargetPair = Dict
  cseWrapVoidPtr (JanusCUDA p) = JanusCUDA (janusCReallyUnsafeCast p)
  cseUnwrapVoidPtr (JanusCUDA p) = JanusCUDA (janusCReallyUnsafeCast p)

instance CSETargetExp CUDAIO where
  cseTargetBool = Dict
  cseTargetInt32 = Dict
  cseTargetWord32 = Dict
  cseTargetInt64 = Dict
  cseTargetWord64 = Dict
  cseTargetCInt = Dict
  cseTargetCFileP = Dict
  cseTargetCStr = Dict
  cseTargetVoidPtr = Dict
  cseTargetPtr = Dict
  cseTargetPair = Dict
  cseWrapVoidPtr = coerce
  cseUnwrapVoidPtr = coerce

instance CSETargetExp CUDADejaFu where
  cseTargetBool = Dict
  cseTargetInt32 = Dict
  cseTargetWord32 = Dict
  cseTargetInt64 = Dict
  cseTargetWord64 = Dict
  cseTargetCInt = Dict
  cseTargetCFileP = Dict
  cseTargetCStr = Dict
  cseTargetVoidPtr = Dict
  cseTargetPtr = Dict
  cseTargetPair = Dict
  cseWrapVoidPtr = coerce
  cseUnwrapVoidPtr = coerce

instance (CSETargetExp e) => CSETargetExp (ReaderExp r e) where
  cseTargetBool = cseTargetBool @e
  cseTargetInt32 = cseTargetInt32 @e
  cseTargetWord32 = cseTargetWord32 @e
  cseTargetInt64 = cseTargetInt64 @e
  cseTargetWord64 = cseTargetWord64 @e
  cseTargetCInt = cseTargetCInt @e
  cseTargetCFileP = cseTargetCFileP @e
  cseTargetCStr = cseTargetCStr @e
  cseTargetVoidPtr = cseTargetVoidPtr @e
  cseTargetPtr :: forall a. (JanusTyped (ReaderExp r e) a) => Dict (JanusTyped (ReaderExp r e) (Ptr a))
  cseTargetPtr = cseTargetPtr @e @a
  cseTargetPair :: forall a b. (JanusTyped (ReaderExp r e) a, JanusTyped (ReaderExp r e) b) => Dict (JanusTyped (ReaderExp r e) (SIMDPair a b))
  cseTargetPair = cseTargetPair @e @a @b
  cseWrapVoidPtr (ReaderExp p) = ReaderExp (cseWrapVoidPtr p)
  cseUnwrapVoidPtr (ReaderExp p) = ReaderExp (cseUnwrapVoidPtr p)

instance (CSETargetExp e) => CSETargetExp (Bench c e) where
  cseTargetBool = cseTargetBool @e
  cseTargetInt32 = cseTargetInt32 @e
  cseTargetWord32 = cseTargetWord32 @e
  cseTargetInt64 = cseTargetInt64 @e
  cseTargetWord64 = cseTargetWord64 @e
  cseTargetCInt = cseTargetCInt @e
  cseTargetCFileP = cseTargetCFileP @e
  cseTargetCStr = cseTargetCStr @e
  cseTargetVoidPtr = cseTargetVoidPtr @e
  cseTargetPtr :: forall a. (JanusTyped (Bench c e) a) => Dict (JanusTyped (Bench c e) (Ptr a))
  cseTargetPtr = cseTargetPtr @e @a
  cseTargetPair :: forall a b. (JanusTyped (Bench c e) a, JanusTyped (Bench c e) b) => Dict (JanusTyped (Bench c e) (SIMDPair a b))
  cseTargetPair = cseTargetPair @e @a @b
  cseWrapVoidPtr (Bench p) = Bench (cseWrapVoidPtr p)
  cseUnwrapVoidPtr (Bench p) = Bench (cseUnwrapVoidPtr p)

data BindRep (e :: K.Type -> K.Type) (a :: K.Type) where
  BindRep ::
    (Coercible a rep, JanusTyped e rep) =>
    !(e a -> e rep) ->
    !(e rep -> e a) ->
    BindRep e a

-- | Resolve any 'BindCap' to a uniform 'BindRep' carrying a @JanusTyped e rep@
-- witness and zero-cost representational conversions.
resolveBindRep :: forall e a. (CSETargetExp e) => BindCap e a -> BindRep e a
resolveBindRep = \case
  BindDirect -> BindRep id id
  BindBool -> case cseTargetBool @e of Dict -> BindRep id id
  BindInt32 -> case cseTargetInt32 @e of Dict -> BindRep id id
  BindWord32 -> case cseTargetWord32 @e of Dict -> BindRep id id
  BindInt64 -> case cseTargetInt64 @e of Dict -> BindRep id id
  BindWord64 -> case cseTargetWord64 @e of Dict -> BindRep id id
  BindCInt -> case cseTargetCInt @e of Dict -> BindRep id id
  BindCFileP -> case cseTargetCFileP @e of Dict -> BindRep id id
  BindCString -> case cseTargetCStr @e of Dict -> BindRep id id
  BindVoidPtr ->
    case cseTargetVoidPtr @e of
      Dict -> BindRep cseWrapVoidPtr cseUnwrapVoidPtr
  BindPtrOf inner ->
    case resolveBindRep @e inner of
      BindRep (_ :: e x -> e repX) _ ->
        case cseTargetPtr @e @repX of
          Dict -> BindRep (coerceExpNewtype @e @(Ptr x) @(Ptr repX)) (coerceExpNewtype @e @(Ptr repX) @(Ptr x))
  BindPairOf ca cb ->
    case (resolveBindRep @e ca, resolveBindRep @e cb) of
      (BindRep (_ :: e x -> e repX) _, BindRep (_ :: e y -> e repY) _) ->
        case cseTargetPair @e @repX @repY of
          Dict ->
            BindRep
              (coerceExpNewtype @e @(SIMDPair x y) @(SIMDPair repX repY))
              (coerceExpNewtype @e @(SIMDPair repX repY) @(SIMDPair x y))
  BindSIMDVec ->
    goSIMDVec
    where
      goSIMDVec :: forall n elemTy. (CSESIMDTarget e, SIMDShape n elemTy) => BindRep e (SIMDVector (CSEExp e) n elemTy)
      goSIMDVec =
        case cseSIMDVecWitness @e @n @elemTy of
          SIMDVecWitness (_ :: Proxy baseV) ->
            BindRep
              (coerceExpNewtype @e @(SIMDVector (CSEExp e) n elemTy) @baseV)
              (coerceExpNewtype @e @baseV @(SIMDVector (CSEExp e) n elemTy))
  BindSIMDMask ->
    goSIMDMask
    where
      goSIMDMask :: forall n elemTy. (CSESIMDTarget e, SIMDShape n elemTy) => BindRep e (SIMDMask (CSEExp e) n elemTy)
      goSIMDMask =
        case cseSIMDMaskWitness @e @n @elemTy of
          SIMDMaskWitness (_ :: Proxy baseM) ->
            BindRep
              (coerceExpNewtype @e @(SIMDMask (CSEExp e) n elemTy) @baseM)
              (coerceExpNewtype @e @baseM @(SIMDMask (CSEExp e) n elemTy))

-- | Emit a pure 'let_' binding in target @e@ using 'BindCap' witnesses on both
-- the bound expression and the body result.
cseLetWithCap ::
  forall e a b.
  (CSETargetExp e) =>
  BindCap e a ->
  BindCap e b ->
  e a ->
  (e a -> e b) ->
  e b
cseLetWithCap capA capB rhs k =
  case (resolveBindRep @e capA, resolveBindRep @e capB) of
    (BindRep toRepA fromRepA, BindRep toRepB fromRepB) ->
      fromRepB (let_ (toRepA rhs) (toRepB . k . fromRepA))

-- | Emit a monadic 'letM' binding in target @(m, e)@ using a 'BindCap' witness.
cseLetMWithCap ::
  forall m e a.
  (CSETargetExp e, CmdRef m e) =>
  BindCap e a ->
  e a ->
  m (e a)
cseLetMWithCap capA rhs =
  case resolveBindRep @e capA of
    BindRep toRepA fromRepA ->
      fromRepA <$> letM (toRepA rhs)

--------------------------------------------------------------------------------
-- Heterogeneous Argument Lists
--------------------------------------------------------------------------------

data HArgs (f :: K.Type -> K.Type) (ts :: [K.Type]) where
  HNil :: HArgs f '[]
  HCons :: !(f t) -> !(HArgs f ts) -> HArgs f (t ': ts)

mapHArgsM ::
  (Applicative m) =>
  (forall x. f x -> m (g x)) ->
  HArgs f ts ->
  m (HArgs g ts)
mapHArgsM _ HNil = pure HNil
mapHArgsM fn (HCons x xs) = HCons <$> fn x <*> mapHArgsM fn xs

--------------------------------------------------------------------------------
-- Pure Expression AST (`CSEExp`)
--------------------------------------------------------------------------------

data ShortCircuitOp
  = SCLand
  | SCLor
  deriving stock (Eq, Ord, Show)

data ExpMeta (e :: K.Type -> K.Type) (a :: K.Type) = ExpMeta
  { emDigest :: !Digest,
    emFreeVars :: !(Set.Set VarKey),
    emHasTrapOrFix :: !Bool,
    emIsTrivial :: !Bool,
    emBind :: !(Maybe (TR.TypeRep a, BindCap e a))
  }

-- | Pure local recursive function descriptor created by 'Janus.Fix.fun'.
data PureFunDef (e :: K.Type -> K.Type) (params :: [K.Type]) (ret :: K.Type) = PureFunDef
  { pfdKey :: !Digest,
    pfdName :: !String,
    pfdParams :: !(HArgs (VarId e) params),
    pfdRetType :: !(TR.TypeRep ret),
    pfdRetBind :: !(BindCap e ret),
    pfdBody :: CSEExp e ret,
    pfdEmitTarget :: ((HArgs e params -> e ret) -> (HArgs e params -> e ret)) -> HArgs e params -> e ret
  }

-- | Monadic local recursive function descriptor created by 'Janus.Fix.fun'.
data MonadicFunDef (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) (params :: [K.Type]) (ret :: K.Type) = MonadicFunDef
  { mfdKey :: !Digest,
    mfdName :: !String,
    mfdParams :: !(HArgs (VarId e) params),
    mfdRetType :: !(TR.TypeRep ret),
    mfdRetBind :: !(BindCap e ret),
    mfdBody :: CSEM m e (CSEExp e ret),
    mfdEmitTarget :: ((HArgs e params -> m (e ret)) -> (HArgs e params -> m (e ret))) -> HArgs e params -> m (e ret)
  }

-- | Pure Janus expression AST supporting deterministic structural CSE.
data CSEExp (e :: K.Type -> K.Type) (a :: K.Type) where
  -- | Reference to a bound variable (formal parameter, monadic SSA result,
  -- pure 'let_'/'letM' binder, recursive function parameter, or CUDA thread field).
  ExpVar ::
    -- | Cached Merkle digest, free-variable set, trap/fix flag, triviality flag, and binding capability.
    !(ExpMeta e a) ->
    -- | Typed variable identifier looked up in the interpreter environment.
    !(VarId e a) ->
    CSEExp e a
  -- | Constant or 0-ary pure leaf expression (numeric/boolean literal,
  -- 'Janus.Command.Memory.nullPtr', 'Janus.Command.Memory.sizeOf',
  -- 'Janus.Command.IO.stdout', etc.).
  ExpLit ::
    -- | Cached expression metadata (marked trivial so CSE never allocates a temporary for a literal).
    !(ExpMeta e a) ->
    -- | Pre-built target expression value in @e@.
    !(e a) ->
    CSEExp e a
  -- | Strict fixed-arity pure operator over a heterogeneous operand list.
  ExpOp ::
    -- | Cached expression metadata.
    !(ExpMeta e a) ->
    -- | Canonical operator tag included in the Merkle digest (e.g. @\"Num.+\"@, @\"SIMD.vshuffle\"@).
    !BS.ByteString ->
    -- | Serialized type tags and immediate compile-time parameters (lane indices,
    -- struct field offsets, etc.) included in the Merkle digest.
    ![BS.ByteString] ->
    -- | @canTrap@: 'True' if evaluating this operation can trap on invalid inputs
    -- (e.g. 'Janus.Expression.Integral.quotient' and 'Janus.Expression.Integral.remainder'),
    -- preventing speculative hoisting.
    !Bool ->
    -- | @zeroCost@: 'True' for representational casts/wrappers (e.g.
    -- 'Janus.Expression.Cast.ptrCast', 'Janus.Expression.Cast.toVoidPtr') that emit no
    -- runtime instructions and remain marked trivial when their inputs are trivial.
    !Bool ->
    -- | Heterogeneous list of operand subexpressions.
    !(HArgs (CSEExp e) ts) ->
    -- | Target evaluator lowering evaluated operand values into @e a@.
    !(HArgs e ts -> e a) ->
    CSEExp e a
  -- | Strict homogeneous list-arity pure operator (e.g.
  -- 'Janus.Expression.SIMD.vfromLanes' and 'Janus.Expression.SIMD.mfromBools').
  ExpListOp ::
    -- | Cached expression metadata.
    !(ExpMeta e a) ->
    -- | Canonical operator tag included in the Merkle digest.
    !BS.ByteString ->
    -- | Serialized type tags and immediate compile-time parameters included in the Merkle digest.
    ![BS.ByteString] ->
    -- | Homogeneous list of operand subexpressions.
    ![CSEExp e b] ->
    -- | Target evaluator lowering the evaluated operand list into @e a@.
    !([e b] -> e a) ->
    CSEExp e a
  -- | Short-circuit boolean operator ('Janus.Expression.Bool.land' or
  -- 'Janus.Expression.Bool.lor') whose right-hand operand is guarded by the
  -- left-hand operand.
  ExpShortCircuit ::
    -- | Cached expression metadata.
    !(ExpMeta e Bool) ->
    -- | Logical connective ('SCLand' or 'SCLor').
    !ShortCircuitOp ->
    -- | Left-hand boolean expression, evaluated unconditionally.
    !(CSEExp e Bool) ->
    -- | Right-hand boolean expression, guarded by the left-hand expression.
    !(CSEExp e Bool) ->
    -- | Target evaluator lowering @(lhs, rhs)@ into @e 'Bool'@.
    !(e Bool -> e Bool -> e Bool) ->
    CSEExp e Bool
  -- | Pure conditional expression ('Janus.Expression.Cond.ifThenElse') with
  -- isolated branch scopes.
  ExpCondNode ::
    -- | Cached expression metadata.
    !(ExpMeta e a) ->
    -- | Boolean condition expression.
    !(CSEExp e Bool) ->
    -- | True-branch expression.
    !(CSEExp e a) ->
    -- | False-branch expression.
    !(CSEExp e a) ->
    -- | Target evaluator lowering @(cond, thenVal, elseVal)@ into @e a@.
    !(e Bool -> e a -> e a -> e a) ->
    CSEExp e a
  -- | Scoped pure local binding ('Janus.Expression.Let.let_'), either written
  -- by the user or synthesized by CSE inside a pure branch scope.
  ExpLetNode ::
    -- | Cached metadata for the overall @let@ expression.
    !(ExpMeta e b) ->
    -- | Identifier bound to the evaluated right-hand side within the body.
    !(VarId e a) ->
    -- | Target binding capability witness for the bound value type @a@.
    !(BindCap e a) ->
    -- | Target binding capability witness for the body result type @b@.
    !(BindCap e b) ->
    -- | Bound right-hand side expression.
    !(CSEExp e a) ->
    -- | Body expression in which the bound 'VarId' is in scope.
    !(CSEExp e b) ->
    CSEExp e b
  -- | Application of a pure local recursive function defined via 'Janus.Fix.fun'.
  ExpFixCall ::
    -- | Cached expression metadata (marked @emHasTrapOrFix = True@ so recursive calls
    -- are never speculatively hoisted out of base-case guards).
    !(ExpMeta e a) ->
    -- | Descriptor of the pure recursive function being invoked.
    !(PureFunDef e params a) ->
    -- | Actual argument expressions passed to this call.
    !(HArgs (CSEExp e) params) ->
    CSEExp e a

type instance JanusTyped (CSEExp e) = CSETyped e

instance (KnownJanusType e a, Typeable a) => KnownJanusType (CSEExp e) a

expMeta :: CSEExp e a -> ExpMeta e a
expMeta = \case
  ExpVar m _ -> m
  ExpLit m _ -> m
  ExpOp m _ _ _ _ _ _ -> m
  ExpListOp m _ _ _ _ -> m
  ExpShortCircuit m _ _ _ _ -> m
  ExpCondNode m _ _ _ _ -> m
  ExpLetNode m _ _ _ _ _ -> m
  ExpFixCall m _ _ -> m

hargsDigests :: HArgs (CSEExp e) ts -> [Digest]
hargsDigests HNil = []
hargsDigests (HCons x xs) = emDigest (expMeta x) : hargsDigests xs

hargsFreeVars :: HArgs (CSEExp e) ts -> Set.Set VarKey
hargsFreeVars HNil = Set.empty
hargsFreeVars (HCons x xs) = Set.union (emFreeVars (expMeta x)) (hargsFreeVars xs)

hargsHasTrapOrFix :: HArgs (CSEExp e) ts -> Bool
hargsHasTrapOrFix HNil = False
hargsHasTrapOrFix (HCons x xs) = emHasTrapOrFix (expMeta x) || hargsHasTrapOrFix xs

bindImms :: Maybe (TR.TypeRep a, BindCap e a) -> [BS.ByteString]
bindImms Nothing = []
bindImms (Just (tr, _)) = [encodeTypeRep tr]

mkExpVar :: VarId e a -> CSEExp e a
mkExpVar vid@(VarId k tr mbCap) =
  let d = mkNodeDigest "Var" [encodeVarKey k, encodeTypeRep tr] []
      mbBind = fmap (\c -> (tr, c)) mbCap
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = Set.singleton k,
            emHasTrapOrFix = False,
            emIsTrivial = True,
            emBind = mbBind
          }
   in ExpVar meta vid

mkExpLit ::
  BS.ByteString ->
  [BS.ByteString] ->
  Maybe (TR.TypeRep a, BindCap e a) ->
  e a ->
  CSEExp e a
mkExpLit tag imms mbBind val =
  let d = mkNodeDigest ("Lit:" <> tag) (bindImms mbBind <> imms) []
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = Set.empty,
            emHasTrapOrFix = False,
            emIsTrivial = True,
            emBind = mbBind
          }
   in ExpLit meta val

mkExpOp ::
  BS.ByteString ->
  [BS.ByteString] ->
  Bool ->
  Maybe (TR.TypeRep a, BindCap e a) ->
  HArgs (CSEExp e) ts ->
  (HArgs e ts -> e a) ->
  CSEExp e a
mkExpOp tag imms canTrap mbBind args eval =
  let fullImms = bindImms mbBind <> imms
      d = mkNodeDigest ("Op:" <> tag) fullImms (hargsDigests args)
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = hargsFreeVars args,
            emHasTrapOrFix = canTrap || hargsHasTrapOrFix args,
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpOp meta tag imms canTrap False args eval

mkExpZeroCostOp ::
  BS.ByteString ->
  [BS.ByteString] ->
  Maybe (TR.TypeRep a, BindCap e a) ->
  HArgs (CSEExp e) ts ->
  (HArgs e ts -> e a) ->
  CSEExp e a
mkExpZeroCostOp tag imms mbBind args eval =
  let fullImms = bindImms mbBind <> imms
      d = mkNodeDigest ("ZOp:" <> tag) fullImms (hargsDigests args)
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = hargsFreeVars args,
            emHasTrapOrFix = hargsHasTrapOrFix args,
            emIsTrivial = True,
            emBind = mbBind
          }
   in ExpOp meta tag imms False True args eval

mkExpListOp ::
  BS.ByteString ->
  [BS.ByteString] ->
  Maybe (TR.TypeRep a, BindCap e a) ->
  [CSEExp e b] ->
  ([e b] -> e a) ->
  CSEExp e a
mkExpListOp tag imms mbBind xs eval =
  let fullImms = bindImms mbBind <> imms
      d = mkNodeDigest ("LOp:" <> tag) fullImms (map (emDigest . expMeta) xs)
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = foldMap (emFreeVars . expMeta) xs,
            emHasTrapOrFix = any (emHasTrapOrFix . expMeta) xs,
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpListOp meta tag imms xs eval

mkExpShortCircuit ::
  ShortCircuitOp ->
  CSEExp e Bool ->
  CSEExp e Bool ->
  (e Bool -> e Bool -> e Bool) ->
  CSEExp e Bool
mkExpShortCircuit op lhs rhs eval =
  let tag = case op of
        SCLand -> "SC:land"
        SCLor -> "SC:lor"
      mbBind = Just (TR.typeRep @Bool, BindBool)
      d = mkNodeDigest tag (bindImms mbBind) [emDigest (expMeta lhs), emDigest (expMeta rhs)]
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = Set.union (emFreeVars (expMeta lhs)) (emFreeVars (expMeta rhs)),
            emHasTrapOrFix = emHasTrapOrFix (expMeta lhs) || emHasTrapOrFix (expMeta rhs),
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpShortCircuit meta op lhs rhs eval

mkExpCond ::
  TR.TypeRep a ->
  BindCap e a ->
  CSEExp e Bool ->
  CSEExp e a ->
  CSEExp e a ->
  (e Bool -> e a -> e a -> e a) ->
  CSEExp e a
mkExpCond tr cap c t f eval =
  let mbBind = Just (tr, cap)
      d =
        mkNodeDigest
          "Cond"
          [encodeTypeRep tr]
          [emDigest (expMeta c), emDigest (expMeta t), emDigest (expMeta f)]
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars =
              Set.unions
                [ emFreeVars (expMeta c),
                  emFreeVars (expMeta t),
                  emFreeVars (expMeta f)
                ],
            emHasTrapOrFix =
              emHasTrapOrFix (expMeta c)
                || emHasTrapOrFix (expMeta t)
                || emHasTrapOrFix (expMeta f),
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpCondNode meta c t f eval

mkExpUserLet ::
  forall e a b.
  (CSETyped e a, CSETyped e b) =>
  CSEExp e a ->
  (CSEExp e a -> CSEExp e b) ->
  CSEExp e b
mkExpUserLet rhs k =
  let trA = TR.typeRep @a
      trB = TR.typeRep @b
      capA = BindDirect @e @a
      capB = BindDirect @e @b
      vKey = VarUserLet (emDigest (expMeta rhs))
      vid = VarId vKey trA (Just capA)
      body = k (mkExpVar vid)
   in mkExpCSELet vid capA trB capB rhs body

mkExpCSELet ::
  VarId e a ->
  BindCap e a ->
  TR.TypeRep b ->
  BindCap e b ->
  CSEExp e a ->
  CSEExp e b ->
  CSEExp e b
mkExpCSELet vid@(VarId vKey trA _) capA trB capB rhs body =
  let mbBind = Just (trB, capB)
      d =
        mkNodeDigest
          "Let"
          [encodeVarKey vKey, encodeTypeRep trA, encodeTypeRep trB]
          [emDigest (expMeta rhs), emDigest (expMeta body)]
      fvs =
        Set.union
          (emFreeVars (expMeta rhs))
          (Set.delete vKey (emFreeVars (expMeta body)))
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = fvs,
            emHasTrapOrFix = emHasTrapOrFix (expMeta rhs) || emHasTrapOrFix (expMeta body),
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpLetNode meta vid capA capB rhs body

mkExpFixCall ::
  PureFunDef e params a ->
  HArgs (CSEExp e) params ->
  CSEExp e a
mkExpFixCall pfd args =
  let tr = pfdRetType pfd
      cap = pfdRetBind pfd
      mbBind = Just (tr, cap)
      d = mkNodeDigest "FixCall" [getDigest (pfdKey pfd), encodeTypeRep tr] (hargsDigests args)
      meta =
        ExpMeta
          { emDigest = d,
            emFreeVars = hargsFreeVars args,
            emHasTrapOrFix = True,
            emIsTrivial = False,
            emBind = mbBind
          }
   in ExpFixCall meta pfd args

--------------------------------------------------------------------------------
-- Monadic Command & Structured Block IR (`CSEM`)
--------------------------------------------------------------------------------

type Block m e = Seq.Seq (ScopeItem m e)

-- | Single monadic statement inside a structured 'Block'.
data Stm (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  -- | Pure expression bound to an immutable SSA variable via
  -- 'Janus.Command.Ref.letM' (either user-written or synthesized by CSE to
  -- share a pure subexpression across monadic statements).
  StmLetM ::
    -- | SSA/CSE variable identifier bound by this statement.
    !(VarId e a) ->
    -- | Target binding capability witness used to emit 'Janus.Command.Ref.letM' in @m@.
    !(BindCap e a) ->
    -- | Pure right-hand side expression evaluated once and bound to the variable.
    !(CSEExp e a) ->
    Stm m e
  -- | Effectful primitive command producing an SSA result value (e.g.
  -- 'Janus.Command.Memory.peek', 'Janus.Command.Memory.malloc',
  -- 'Janus.Expression.SIMD.vload', 'Janus.Command.TicToc.tic'). Never reordered
  -- or deduplicated by CSE.
  StmEffectVal ::
    -- | Fresh 'VarSSA' identifier receiving the command's result.
    !(VarId e res) ->
    -- | Pure argument expressions passed to the command.
    !(HArgs (CSEExp e) ts) ->
    -- | Target effectful action executed with the lowered arguments.
    !(HArgs e ts -> m (e res)) ->
    Stm m e
  -- | Effectful primitive command returning @()@ (e.g.
  -- 'Janus.Command.Memory.poke', 'Janus.Command.Memory.free',
  -- 'Janus.Expression.SIMD.vstore', 'Janus.Command.Format.hformat',
  -- 'Janus.Backend.C.CUDA.Types.syncBlock'). Never reordered or deduplicated by CSE.
  StmEffectUnit ::
    -- | Pure argument expressions passed to the command.
    !(HArgs (CSEExp e) ts) ->
    -- | Target effectful action executed with the lowered arguments.
    !(HArgs e ts -> m ()) ->
    Stm m e
  -- | Allocate a mutable reference cell via 'Janus.Command.Ref.newRef' or
  -- uninitialized 'Janus.Command.Ref.newRef''.
  StmNewRef ::
    (CmdRef m e, JanusTyped e a) =>
    -- | Fresh local reference handle bound for subsequent reads, writes, and 'ItemWithRefPtr' scopes.
    !(CSERef m e a) ->
    -- | Optional initial value ('Just' for 'newRef', 'Nothing' for 'newRef'').
    !(Maybe (CSEExp e a)) ->
    Stm m e
  -- | Read the current value of a mutable reference ('Janus.Command.Ref.readRef')
  -- into a fresh immutable SSA snapshot variable.
  StmReadRef ::
    (CmdRef m e, JanusTyped e a) =>
    -- | Fresh 'VarSSA' identifier capturing the value read at this program point.
    !(VarId e a) ->
    -- | Mutable reference cell being read.
    !(CSERef m e a) ->
    Stm m e
  -- | Mutate a reference cell ('Janus.Command.Ref.writeRef') with a pure expression.
  StmWriteRef ::
    (CmdRef m e, JanusTyped e a) =>
    -- | Target mutable reference cell being updated.
    !(CSERef m e a) ->
    -- | Pure expression written into the reference cell.
    !(CSEExp e a) ->
    Stm m e
  -- | Structured two-way conditional command ('Janus.Command.Cond.ifThenElseM_').
  StmIf ::
    (CmdCond m e) =>
    -- | Branch condition expression evaluated in the enclosing block.
    !(CSEExp e Bool) ->
    -- | Statements executed when the condition holds.
    !(Block m e) ->
    -- | Statements executed when the condition does not hold.
    !(Block m e) ->
    Stm m e
  -- | Structured @while@ loop ('Janus.Command.While.whileM') parameterized by
  -- its mutable loop-continuation reference.
  StmWhile ::
    (CmdWhile m e) =>
    -- | Initial loop entry condition evaluated before the first iteration.
    !(CSEExp e Bool) ->
    -- | Loop-scoped mutable boolean reference updated by the body to continue or exit.
    !(CSERef m e Bool) ->
    -- | Loop body block executed on each iteration.
    !(Block m e) ->
    Stm m e
  -- | Half-open @[lo, hi)@ counted loop ('Janus.Command.Range.rangeM').
  StmRange ::
    (CmdRange m e) =>
    -- | Inclusive lower bound expression evaluated in the enclosing block.
    !(CSEExp e Int64) ->
    -- | Exclusive upper bound expression evaluated in the enclosing block.
    !(CSEExp e Int64) ->
    -- | Loop-induction variable bound inside the loop body.
    !(VarId e Int64) ->
    -- | Loop body block executed for each index in @[lo, hi)@.
    !(Block m e) ->
    Stm m e
  -- | Multi-way integer @switch@ statement ('Janus.Command.Case.switchCase_').
  StmSwitch ::
    (CmdCase m e) =>
    -- | Scrutinee integer tag expression evaluated in the enclosing block.
    !(CSEExp e Int64) ->
    -- | Explicit @(caseTag, branchBlock)@ arms.
    ![(Int, Block m e)] ->
    -- | Default fallback block executed when no explicit case tag matches.
    !(Block m e) ->
    Stm m e
  -- | Snapshot the ambient CUDA thread/grid coordinates
  -- ('Control.Monad.Reader.ask') into SSA variables.
  StmCUDAAsk ::
    (MonadReader (CUDAThreadInfo e) m) =>
    -- | Record of variable identifiers receiving each @blockIdx@/@blockDim@/@threadIdx@/@gridDim@ coordinate.
    !(CUDAThreadInfo (VarId e)) ->
    Stm m e
  -- | Effectful call to a monadic local recursive function defined via 'Janus.Fix.fun'.
  StmFixCall ::
    -- | Fresh 'VarSSA' identifier receiving the call's return value.
    !(VarId e ret) ->
    -- | Descriptor of the monadic recursive function being invoked.
    !(MonadicFunDef m e params ret) ->
    -- | Pure argument expressions passed to this call.
    !(HArgs (CSEExp e) params) ->
    Stm m e

-- | Element of a 'Block': either an ordinary statement or a lexically scoped
-- resource continuation whose inner block executes unconditionally.
data ScopeItem (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  -- | Ordinary non-continuation statement.
  ItemStm ::
    -- | Wrapped monadic statement.
    !(Stm m e) ->
    ScopeItem m e
  -- | Scoped address-of-reference continuation ('Janus.Command.Memory.withRefPtr')
  -- exposing a stack/local pointer for the duration of the inner block.
  ItemWithRefPtr ::
    (CmdRefPtr m e a) =>
    -- | Backing mutable reference cell whose address is taken (and written back on exit in 'IO').
    !(CSERef m e a) ->
    -- | Scoped pointer variable valid only within the inner block.
    !(VarId e (Ptr a)) ->
    -- | Inner block executed unconditionally with the pointer variable in scope.
    !(Block m e) ->
    ScopeItem m e
  -- | Scoped C string literal continuation ('Janus.Command.Format.withString')
  -- exposing a 'CString' pointer for the duration of the inner block.
  ItemWithString ::
    (CmdString m e) =>
    -- | Host string literal materialized as a null-terminated 'CString'.
    !String ->
    -- | Scoped 'CString' variable valid only within the inner block.
    !(VarId e CString) ->
    -- | Inner block executed unconditionally with the 'CString' variable in scope.
    !(Block m e) ->
    ScopeItem m e

-- | Top-level program tree of a 'CSEM' action, representing either a linear
-- statement block or a compile-time SIMD gang-width dispatch split.
data ProgTree (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) (r :: K.Type) where
  -- | Single sequential block of statements followed by a pure/structured return value.
  ProgLinear ::
    -- | Ordered sequence of statements and scoped continuations executed by this branch.
    !(Block m e) ->
    -- | Final Haskell return value (typically a 'CSEExp', @()@, tuple, or
    -- 'Janus.Data.HKD.HKD' record) whose embedded expressions may reference
    -- bindings from the block.
    r ->
    ProgTree m e r
  -- | Multi-versioned SIMD gang dispatch ('Janus.SPMD.Gang.withOptimalGang')
  -- capturing specialized program trees for all supported gang widths so CSE
  -- optimizes each width independently before target selection.
  ProgOptimalGang ::
    (SIMDElem a, CmdOptimalGang m e) =>
    -- | Type representation of the primary 'SIMDElem' scalar type @a@ queried by
    -- 'withOptimalGang' to choose the target's optimal hardware gang width.
    !(TR.TypeRep a) ->
    -- | Specialized program tree for gang width @n = 2@.
    !(ProgTree m e r) ->
    -- | Specialized program tree for gang width @n = 4@.
    !(ProgTree m e r) ->
    -- | Specialized program tree for gang width @n = 8@.
    !(ProgTree m e r) ->
    -- | Specialized program tree for gang width @n = 16@.
    !(ProgTree m e r) ->
    -- | Specialized program tree for gang width @n = 32@.
    !(ProgTree m e r) ->
    ProgTree m e r

deriving stock instance Functor (ProgTree m e)

prependItems :: Block m e -> ProgTree m e r -> ProgTree m e r
prependItems items tree
  | Seq.null items = tree
  | otherwise = case tree of
      ProgLinear items2 r -> ProgLinear (items Seq.>< items2) r
      ProgOptimalGang tr b2 b4 b8 b16 b32 ->
        ProgOptimalGang
          tr
          (prependItems items b2)
          (prependItems items b4)
          (prependItems items b8)
          (prependItems items b16)
          (prependItems items b32)

wrapScopeWith ::
  (Block m e -> ScopeItem m e) ->
  ProgTree m e r ->
  ProgTree m e r
wrapScopeWith mkItem = \case
  ProgLinear blk r -> ProgLinear (Seq.singleton (mkItem blk)) r
  ProgOptimalGang tr b2 b4 b8 b16 b32 ->
    ProgOptimalGang
      tr
      (wrapScopeWith mkItem b2)
      (wrapScopeWith mkItem b4)
      (wrapScopeWith mkItem b8)
      (wrapScopeWith mkItem b16)
      (wrapScopeWith mkItem b32)

-- | Monadic Janus command builder paired with 'CSEExp'.
newtype CSEM (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) (r :: K.Type) = CSEM
  { unCSEM :: Int -> (Int, ProgTree m e r)
  }
  deriving stock (Functor)

bindProgTree :: Int -> ProgTree m e a -> (a -> CSEM m e b) -> (Int, ProgTree m e b)
bindProgTree s1 (ProgLinear items1 a) k =
  let (s2, t2) = unCSEM (k a) s1
   in (s2, prependItems items1 t2)
bindProgTree s1 (ProgOptimalGang tr b2 b4 b8 b16 b32) k =
  let (s2, b2') = bindProgTree s1 b2 k
      (s3, b4') = bindProgTree s2 b4 k
      (s4, b8') = bindProgTree s3 b8 k
      (s5, b16') = bindProgTree s4 b16 k
      (s6, b32') = bindProgTree s5 b32 k
   in (s6, ProgOptimalGang tr b2' b4' b8' b16' b32')

instance Applicative (CSEM m e) where
  pure x = CSEM $ \s -> (s, ProgLinear Seq.empty x)
  mf <*> mx = CSEM $ \s0 ->
    let (s1, tf) = unCSEM mf s0
     in bindProgTree s1 tf (\f -> fmap f mx)

instance Monad (CSEM m e) where
  ma >>= k = CSEM $ \s0 ->
    let (s1, ta) = unCSEM ma s0
     in bindProgTree s1 ta k

extractLazyResult :: ProgTree m e a -> a
extractLazyResult (ProgLinear _ a) = a
extractLazyResult (ProgOptimalGang _ b2 _ _ _ _) = extractLazyResult b2

instance MonadFix (CSEM m e) where
  mfix f = CSEM $ \s0 ->
    let res =
          let (s1, tree) = unCSEM (f (extractLazyResult tree)) s0
           in (s1, tree)
     in res

allocSSAId :: TR.TypeRep a -> Maybe (BindCap e a) -> CSEM m e (VarId e a)
allocSSAId tr mbCap = CSEM $ \n ->
  (n + 1, ProgLinear Seq.empty (VarId (VarSSA n) tr mbCap))

emitStm :: Stm m e -> CSEM m e ()
emitStm stm = CSEM $ \n ->
  (n, ProgLinear (Seq.singleton (ItemStm stm)) ())

emitEffectVal ::
  TR.TypeRep res ->
  Maybe (BindCap e res) ->
  HArgs (CSEExp e) ts ->
  (HArgs e ts -> m (e res)) ->
  CSEM m e (CSEExp e res)
emitEffectVal tr mbCap args eval = do
  vid <- allocSSAId tr mbCap
  emitStm (StmEffectVal vid args eval)
  pure (mkExpVar vid)

emitEffectUnit ::
  HArgs (CSEExp e) ts ->
  (HArgs e ts -> m ()) ->
  CSEM m e ()
emitEffectUnit args eval = emitStm (StmEffectUnit args eval)


--------------------------------------------------------------------------------
-- Representational Coercion & Associated SIMD Types for `CSEExp`
--------------------------------------------------------------------------------

instance (ExpNewtype e) => ExpNewtype (CSEExp e) where
  coerceExpNewtype x =
    mkExpZeroCostOp
      "coerceExpNewtype"
      []
      Nothing
      (HCons x HNil)
      (\(HCons v HNil) -> coerceExpNewtype v)

instance (ExpSIMD e) => ExpSIMD (CSEExp e) where
  newtype SIMDVector (CSEExp e) n a = CSESIMDVector
    { getCSESIMDVector :: SIMDVector e n a
    }
  newtype SIMDMask (CSEExp e) n a = CSESIMDMask
    { getCSESIMDMask :: SIMDMask e n a
    }

deriving newtype instance (Eq (SIMDVector e n a)) => Eq (SIMDVector (CSEExp e) n a)

deriving newtype instance (Ord (SIMDVector e n a)) => Ord (SIMDVector (CSEExp e) n a)

deriving newtype instance (Show (SIMDVector e n a)) => Show (SIMDVector (CSEExp e) n a)

deriving newtype instance (Storable (SIMDVector e n a)) => Storable (SIMDVector (CSEExp e) n a)

deriving newtype instance
  (Typeable e, Typeable n, Typeable a, JanusCTyped (SIMDVector e n a)) =>
  JanusCTyped (SIMDVector (CSEExp e) n a)

deriving newtype instance (Eq (SIMDMask e n a)) => Eq (SIMDMask (CSEExp e) n a)

deriving newtype instance (Ord (SIMDMask e n a)) => Ord (SIMDMask (CSEExp e) n a)

deriving newtype instance (Show (SIMDMask e n a)) => Show (SIMDMask (CSEExp e) n a)

deriving newtype instance (Storable (SIMDMask e n a)) => Storable (SIMDMask (CSEExp e) n a)

deriving newtype instance
  (Typeable e, Typeable n, Typeable a, JanusCTyped (SIMDMask e n a)) =>
  JanusCTyped (SIMDMask (CSEExp e) n a)

instance
  (SIMDShape n a, HostSIMDValue (SIMDVector e n a) n a) =>
  HostSIMDValue (SIMDVector (CSEExp e) n a) n a
  where
  simdFromList = CSESIMDVector . simdFromList
  simdToList = simdToList . getCSESIMDVector

instance
  (SIMDShape n a, HostSIMDMask (SIMDMask e n a) n a) =>
  HostSIMDMask (SIMDMask (CSEExp e) n a) n a
  where
  simdMaskFromBools = CSESIMDMask . simdMaskFromBools
  simdMaskToBools = simdMaskToBools . getCSESIMDMask
