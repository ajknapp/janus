{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Reusable constraint bundles for writing concise, backend-polymorphic Janus
-- expressions and imperative programs.
--
-- * Use 'ExpJanus' for pure expression functions polymorphic in @e@.
-- * Use 'CmdJanus' for imperative, memory, allocator, streaming, and
--   @Janus.Data@ container programs polymorphic in @(m, e)@.
-- * Use 'CmdJanusIO' when formatted @FILE*@ output, string literals, or
--   high-resolution timers are also needed.
-- * Use 'ExpScalar', 'ExpNum', 'ExpInt', 'ExpFloat', 'ExpComplexElem', and
--   'CmdElem' when writing functions additionally polymorphic in a scalar
--   element type @a@.
module Janus.Constraints
  ( -- * Primary Consumer Constraint Bundles
    ExpJanus,
    CmdJanus,
    CmdJanusIO,

    -- * Polymorphic Element Capability Synonyms
    ExpScalar,
    ExpNum,
    ExpInt,
    ExpFloat,
    ExpComplexElem,
    CmdElem,

    -- * Numeric Cast Sub-Bundles
    ExpIntCastsFrom,
    ExpIntCastsInto,
    ExpFloatCasts,

    -- * Assumed Primitive Type Sub-Bundles
    ExpPrimitives,
    ExpBools,
    ExpHaskellInts,
    ExpCInts,
    ExpFloats,
    ExpComplexPrims,
    ExpPrimPtrs,
    ExpCPtrs,

    -- * Open Type-Family Witness Bridge
    KnownJanusType,
    withJanusType,
  )
where

import Data.Complex (Complex)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C
  ( CBool,
    CChar,
    CDouble,
    CFile,
    CFloat,
    CInt,
    CLLong,
    CLong,
    CSChar,
    CShort,
    CSize,
    CString,
    CUChar,
    CUInt,
    CULLong,
    CULong,
    CUShort,
  )
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Janus.Backend.C.Raw (JanusCMT, JanusCT, JanusCTyped)
import Janus.Command.Case (CmdCase)
import Janus.Command.Cond (CmdCond)
import Janus.Command.Format (CmdFormat, CmdPutString, CmdString)
import Janus.Command.IO (CmdIO)
import Janus.Command.Memory
  ( CmdDynamicAligned,
    CmdMem,
    CmdMemCopy,
    CmdRefPtr,
    CmdStorable,
    ExpPtr,
    ExpSized,
  )
import Janus.Command.Range (CmdRange)
import Janus.Command.Ref (CmdRef, RefCell)
import Janus.Command.While (CmdWhile)
import Janus.Data.HKD (ExpPtrWord, ExpStructPtr)
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cast
  ( ExpBoolCast,
    ExpFloatingCast,
    ExpIntegralCast,
    ExpPtrCast,
  )
import Janus.Expression.Cond (ExpCond)
import Janus.Expression.Eq (ExpEq)
import Janus.Expression.Integral (ExpIntegral)
import Janus.Expression.Let (ExpLet)
import Janus.Expression.Math (ExpComplex, FMA, Hypot)
import Janus.Expression.MurmurHash (ExpMurmurHash)
import Janus.Expression.Ord (ExpOrd)
import Janus.Typed (JanusTyped, KnownJanusType)

--------------------------------------------------------------------------------
-- Witness Helper Bridging `KnownJanusType` -> `JanusTyped`
--------------------------------------------------------------------------------

-- | Bring @'JanusTyped' e a@ into scope from @'KnownJanusType' e a@ (which is
-- discharged automatically via 'ExpJanus' for any @('Storable' a, 'JanusCTyped' a)@).
{-# INLINE withJanusType #-}
withJanusType :: forall a e r. (KnownJanusType e a) => ((JanusTyped e a) => r) -> r
withJanusType r = r

--------------------------------------------------------------------------------
-- Numeric Cast Sub-Bundles (<= 28 Constraints Per Tuple)
--------------------------------------------------------------------------------

-- | Outgoing casts from an integer type @a@ to all 20 Janus integer types and
-- all 4 Janus floating-point types.
type ExpIntCastsFrom e a =
  ( ExpIntegralCast e a Int8,
    ExpIntegralCast e a Int16,
    ExpIntegralCast e a Int32,
    ExpIntegralCast e a Int64,
    ExpIntegralCast e a Word8,
    ExpIntegralCast e a Word16,
    ExpIntegralCast e a Word32,
    ExpIntegralCast e a Word64,
    ExpIntegralCast e a CChar,
    ExpIntegralCast e a CSChar,
    ExpIntegralCast e a CUChar,
    ExpIntegralCast e a CShort,
    ExpIntegralCast e a CUShort,
    ExpIntegralCast e a CInt,
    ExpIntegralCast e a CUInt,
    ExpIntegralCast e a CLong,
    ExpIntegralCast e a CULong,
    ExpIntegralCast e a CLLong,
    ExpIntegralCast e a CULLong,
    ExpIntegralCast e a CSize,
    ExpFloatingCast e a Float,
    ExpFloatingCast e a Double,
    ExpFloatingCast e a CFloat,
    ExpFloatingCast e a CDouble
  )

-- | Incoming integral casts from all 20 Janus integer types into integer type @a@.
type ExpIntCastsInto e a =
  ( ExpIntegralCast e Int8 a,
    ExpIntegralCast e Int16 a,
    ExpIntegralCast e Int32 a,
    ExpIntegralCast e Int64 a,
    ExpIntegralCast e Word8 a,
    ExpIntegralCast e Word16 a,
    ExpIntegralCast e Word32 a,
    ExpIntegralCast e Word64 a,
    ExpIntegralCast e CChar a,
    ExpIntegralCast e CSChar a,
    ExpIntegralCast e CUChar a,
    ExpIntegralCast e CShort a,
    ExpIntegralCast e CUShort a,
    ExpIntegralCast e CInt a,
    ExpIntegralCast e CUInt a,
    ExpIntegralCast e CLong a,
    ExpIntegralCast e CULong a,
    ExpIntegralCast e CLLong a,
    ExpIntegralCast e CULLong a,
    ExpIntegralCast e CSize a
  )

-- | Incoming and outgoing floating-point casts for a floating-point type @a@.
type ExpFloatCasts e a =
  ( ExpFloatingCast e Int8 a,
    ExpFloatingCast e Int16 a,
    ExpFloatingCast e Int32 a,
    ExpFloatingCast e Int64 a,
    ExpFloatingCast e Word8 a,
    ExpFloatingCast e Word16 a,
    ExpFloatingCast e Word32 a,
    ExpFloatingCast e Word64 a,
    ExpFloatingCast e CChar a,
    ExpFloatingCast e CSChar a,
    ExpFloatingCast e CUChar a,
    ExpFloatingCast e CShort a,
    ExpFloatingCast e CUShort a,
    ExpFloatingCast e CInt a,
    ExpFloatingCast e CUInt a,
    ExpFloatingCast e CLong a,
    ExpFloatingCast e CULong a,
    ExpFloatingCast e CLLong a,
    ExpFloatingCast e CULLong a,
    ExpFloatingCast e CSize a,
    ExpFloatingCast e Float a,
    ExpFloatingCast e Double a,
    ExpFloatingCast e CFloat a,
    ExpFloatingCast e CDouble a,
    ExpFloatingCast e a Float,
    ExpFloatingCast e a Double,
    ExpFloatingCast e a CFloat,
    ExpFloatingCast e a CDouble
  )

--------------------------------------------------------------------------------
-- Polymorphic Element Capability Synonyms (Zero `-Wsimplifiable-class-constraints`)
--------------------------------------------------------------------------------

-- | Comparable, Janus-typeable scalar element @a@ (implies 'JanusTyped', 'ExpEq', and 'ExpOrd').
type ExpScalar e a =
  ( KnownJanusType e a,
    ExpOrd e a
  )

-- | Numeric scalar element @a@ supporting arithmetic, ordering, boolean casts, and MurmurHash.
type ExpNum e a =
  ( ExpScalar e a,
    Num (e a),
    ExpBoolCast e a,
    ExpMurmurHash e (e a)
  )

-- | Signed or unsigned integer scalar element @a@ with bitwise, division, bounds, and numeric casts.
type ExpInt e a =
  ( ExpNum e a,
    Bounded (e a),
    ExpBits e a,
    ExpIntegral e a,
    ExpIntCastsFrom e a,
    ExpIntCastsInto e a
  )

-- | Floating-point scalar element @a@ with transcendental math, 'FMA', 'Hypot', and numeric casts.
type ExpFloat e a =
  ( ExpNum e a,
    Fractional (e a),
    Floating (e a),
    FMA (e a),
    Hypot (e a),
    ExpFloatCasts e a
  )

-- | Real base type @a@ whose @'Complex' a@ type is supported in Janus expressions.
type ExpComplexElem e a =
  ( ExpFloat e a,
    KnownJanusType e (Complex a),
    Num (e (Complex a)),
    Fractional (e (Complex a)),
    Floating (e (Complex a)),
    ExpComplex e a
  )

-- | Storable, pointer-addressable, and 'RefCell'-bindable element @a@ (along with @'Ptr' a@).
type CmdElem m e a =
  ( KnownJanusType e a,
    CmdStorable m e a,
    CmdRefPtr m e a,
    ExpSized e a,
    ExpPtr e a,
    RefCell m e (e a),
    KnownJanusType e (Ptr a),
    CmdStorable m e (Ptr a),
    CmdRefPtr m e (Ptr a),
    ExpSized e (Ptr a),
    ExpPtr e (Ptr a),
    RefCell m e (e (Ptr a))
  )

--------------------------------------------------------------------------------
-- Assumed Primitive Type Sub-Bundles
--------------------------------------------------------------------------------

-- | Boolean and opaque C handle primitive capabilities.
class
  ( KnownJanusType e Bool,
    ExpBool e,
    ExpEq e Bool,
    ExpMurmurHash e (e Bool),
    KnownJanusType e CBool,
    ExpEq e CBool,
    ExpBoolCast e CBool,
    ExpMurmurHash e (e CBool),
    Num (e CBool),
    Bounded (e CBool),
    KnownJanusType e CFile
  ) =>
  ExpBools (e :: K.Type -> K.Type)

instance
  ( KnownJanusType e Bool,
    ExpBool e,
    ExpEq e Bool,
    ExpMurmurHash e (e Bool),
    KnownJanusType e CBool,
    ExpEq e CBool,
    ExpBoolCast e CBool,
    ExpMurmurHash e (e CBool),
    Num (e CBool),
    Bounded (e CBool),
    KnownJanusType e CFile
  ) =>
  ExpBools e

-- | All 8 standard fixed-width Haskell integer types (@Int8..Int64@, @Word8..Word64@).
class
  ( ExpInt e Int8,
    ExpInt e Int16,
    ExpInt e Int32,
    ExpInt e Int64,
    ExpInt e Word8,
    ExpInt e Word16,
    ExpInt e Word32,
    ExpInt e Word64
  ) =>
  ExpHaskellInts (e :: K.Type -> K.Type)

instance
  ( ExpInt e Int8,
    ExpInt e Int16,
    ExpInt e Int32,
    ExpInt e Int64,
    ExpInt e Word8,
    ExpInt e Word16,
    ExpInt e Word32,
    ExpInt e Word64
  ) =>
  ExpHaskellInts e

-- | All 12 standard @Foreign.C@ integer types.
class
  ( ExpInt e CChar,
    ExpInt e CSChar,
    ExpInt e CUChar,
    ExpInt e CShort,
    ExpInt e CUShort,
    ExpInt e CInt,
    ExpInt e CUInt,
    ExpInt e CLong,
    ExpInt e CULong,
    ExpInt e CLLong,
    ExpInt e CULLong,
    ExpInt e CSize
  ) =>
  ExpCInts (e :: K.Type -> K.Type)

instance
  ( ExpInt e CChar,
    ExpInt e CSChar,
    ExpInt e CUChar,
    ExpInt e CShort,
    ExpInt e CUShort,
    ExpInt e CInt,
    ExpInt e CUInt,
    ExpInt e CLong,
    ExpInt e CULong,
    ExpInt e CLLong,
    ExpInt e CULLong,
    ExpInt e CSize
  ) =>
  ExpCInts e

-- | All 4 floating-point scalar types ('Float', 'Double', 'CFloat', 'CDouble').
class
  ( ExpFloat e Float,
    ExpFloat e Double,
    ExpFloat e CFloat,
    ExpFloat e CDouble
  ) =>
  ExpFloats (e :: K.Type -> K.Type)

instance
  ( ExpFloat e Float,
    ExpFloat e Double,
    ExpFloat e CFloat,
    ExpFloat e CDouble
  ) =>
  ExpFloats e

-- | Standard single- and double-precision complex numbers.
class
  ( ExpComplexElem e Float,
    ExpComplexElem e Double
  ) =>
  ExpComplexPrims (e :: K.Type -> K.Type)

instance
  ( ExpComplexElem e Float,
    ExpComplexElem e Double
  ) =>
  ExpComplexPrims e

-- | Concrete pointer typing witnesses for Haskell & Complex primitives (plus @Ptr (Ptr Word8)@),
-- ensuring direct @'JanusTyped' e ('Ptr' P)@ Givens are immediately in scope.
class
  ( KnownJanusType e (Ptr Bool),
    KnownJanusType e (Ptr Int8),
    KnownJanusType e (Ptr Int16),
    KnownJanusType e (Ptr Int32),
    KnownJanusType e (Ptr Int64),
    KnownJanusType e (Ptr Word8),
    KnownJanusType e (Ptr Word16),
    KnownJanusType e (Ptr Word32),
    KnownJanusType e (Ptr Word64),
    KnownJanusType e (Ptr Float),
    KnownJanusType e (Ptr Double),
    KnownJanusType e (Ptr (Complex Float)),
    KnownJanusType e (Ptr (Complex Double)),
    KnownJanusType e (Ptr (Ptr Word8))
  ) =>
  ExpPrimPtrs (e :: K.Type -> K.Type)

instance
  ( KnownJanusType e (Ptr Bool),
    KnownJanusType e (Ptr Int8),
    KnownJanusType e (Ptr Int16),
    KnownJanusType e (Ptr Int32),
    KnownJanusType e (Ptr Int64),
    KnownJanusType e (Ptr Word8),
    KnownJanusType e (Ptr Word16),
    KnownJanusType e (Ptr Word32),
    KnownJanusType e (Ptr Word64),
    KnownJanusType e (Ptr Float),
    KnownJanusType e (Ptr Double),
    KnownJanusType e (Ptr (Complex Float)),
    KnownJanusType e (Ptr (Complex Double)),
    KnownJanusType e (Ptr (Ptr Word8))
  ) =>
  ExpPrimPtrs e

-- | Concrete pointer typing witnesses for @Foreign.C@ primitives (including 'CString' and @'Ptr' 'CFile'@).
class
  ( KnownJanusType e (Ptr CBool),
    KnownJanusType e CString,
    KnownJanusType e (Ptr CString),
    KnownJanusType e (Ptr CSChar),
    KnownJanusType e (Ptr CUChar),
    KnownJanusType e (Ptr CShort),
    KnownJanusType e (Ptr CUShort),
    KnownJanusType e (Ptr CInt),
    KnownJanusType e (Ptr CUInt),
    KnownJanusType e (Ptr CLong),
    KnownJanusType e (Ptr CULong),
    KnownJanusType e (Ptr CLLong),
    KnownJanusType e (Ptr CULLong),
    KnownJanusType e (Ptr CSize),
    KnownJanusType e (Ptr CFloat),
    KnownJanusType e (Ptr CDouble),
    KnownJanusType e (Ptr CFile)
  ) =>
  ExpCPtrs (e :: K.Type -> K.Type)

instance
  ( KnownJanusType e (Ptr CBool),
    KnownJanusType e CString,
    KnownJanusType e (Ptr CString),
    KnownJanusType e (Ptr CSChar),
    KnownJanusType e (Ptr CUChar),
    KnownJanusType e (Ptr CShort),
    KnownJanusType e (Ptr CUShort),
    KnownJanusType e (Ptr CInt),
    KnownJanusType e (Ptr CUInt),
    KnownJanusType e (Ptr CLong),
    KnownJanusType e (Ptr CULong),
    KnownJanusType e (Ptr CLLong),
    KnownJanusType e (Ptr CULLong),
    KnownJanusType e (Ptr CSize),
    KnownJanusType e (Ptr CFloat),
    KnownJanusType e (Ptr CDouble),
    KnownJanusType e (Ptr CFile)
  ) =>
  ExpCPtrs e

-- | Combined assumed primitive types and operations across booleans, Haskell & C
-- integers, floats, complex numbers, and pointers.
class
  ( ExpBools e,
    ExpHaskellInts e,
    ExpCInts e,
    ExpFloats e,
    ExpComplexPrims e,
    ExpPrimPtrs e,
    ExpCPtrs e
  ) =>
  ExpPrimitives (e :: K.Type -> K.Type)

instance
  ( ExpBools e,
    ExpHaskellInts e,
    ExpCInts e,
    ExpFloats e,
    ExpComplexPrims e,
    ExpPrimPtrs e,
    ExpCPtrs e
  ) =>
  ExpPrimitives e

--------------------------------------------------------------------------------
-- Top-Level Consumer Constraint Bundles (Concrete Per-Backend Instances)
--------------------------------------------------------------------------------

-- | Pure Janus expression bundle providing control expressions ('ExpBool',
-- 'ExpCond', 'ExpLet'), pointer/struct addressing ('ExpPtrCast', 'ExpPtrWord',
-- 'ExpStructPtr'), all assumed primitive types ('ExpPrimitives'), and quantified
-- typing/sizing/pointer rules for all @('Storable', 'JanusCTyped')@ types.
class
  ( ExpBool e,
    ExpCond e,
    ExpLet e,
    ExpPtrCast e,
    ExpPtrWord e,
    ExpStructPtr e,
    ExpPrimitives e,
    forall x. (Storable x, JanusCTyped x) => KnownJanusType e x,
    forall x. (Storable x, JanusCTyped x) => ExpSized e x,
    forall x. (Storable x, JanusCTyped x) => ExpPtr e x,
    forall x. ExpEq e (Ptr x),
    forall x. ExpMurmurHash e (e (Ptr x))
  ) =>
  ExpJanus (e :: K.Type -> K.Type)

instance ExpJanus Identity

instance ExpJanus (JanusCT t)

-- | Full imperative Janus bundle combining 'ExpJanus' with mutable references,
-- structured control flow, aligned heap/policy memory operations, and quantified
-- 'CmdStorable' / 'CmdRefPtr' / 'RefCell' support across all @('Storable', 'JanusCTyped')@
-- types (primitives, pointers, 'Janus.Data.HKD.FixedArray', and 'Janus.Data.HKD.CStruct').
--
-- Satisfied by @(IO, Identity)@, @(JanusCMT t, JanusCT t)@, CUDA backends,
-- @(CSEM m e, CSEExp e)@, @(ReaderT r m, ReaderExp r e)@, and @(BenchT c m e, Bench c e)@.
class
  ( Monad m,
    ExpJanus e,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdRange m e,
    CmdCase m e,
    CmdMem m e,
    CmdMemCopy m e,
    CmdDynamicAligned m e,
    forall x. (Storable x, JanusCTyped x) => CmdStorable m e x,
    forall x. (Storable x, JanusCTyped x) => CmdRefPtr m e x,
    forall x. (Storable x, JanusCTyped x) => RefCell m e (e x)
  ) =>
  CmdJanus (m :: K.Type -> K.Type) (e :: K.Type -> K.Type)
    | m -> e, e -> m

instance CmdJanus IO Identity

instance CmdJanus (JanusCMT t) (JanusCT t)

-- | Extension of 'CmdJanus' adding @FILE*@ handles, scoped C strings, and
-- formatted numeric output across all standard integer and floating-point types.
class
  ( CmdJanus m e,
    CmdIO m e,
    CmdString m e,
    CmdPutString m e,
    CmdFormat m e Int8,
    CmdFormat m e Int16,
    CmdFormat m e Int32,
    CmdFormat m e Int64,
    CmdFormat m e Word8,
    CmdFormat m e Word16,
    CmdFormat m e Word32,
    CmdFormat m e Word64,
    CmdFormat m e Float,
    CmdFormat m e Double
  ) =>
  CmdJanusIO (m :: K.Type -> K.Type) (e :: K.Type -> K.Type)
    | m -> e, e -> m

instance CmdJanusIO IO Identity

instance CmdJanusIO (JanusCMT t) (JanusCT t)
