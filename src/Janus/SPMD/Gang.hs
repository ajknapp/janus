{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -O0 #-}

module Janus.SPMD.Gang where

import Control.Monad (when)
import Data.Complex (Complex)
import Data.Constraint (Dict (..))
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C.Raw (JanusCTyped)
import Janus.Backend.C.Target
  ( CUDAFeatures,
    X86Feature (..),
    X86Features,
    getActiveHostX86Features,
    hasX86Feature,
  )
import qualified Janus.Command.Memory as M
import Janus.Command.Cond (CmdCond, whenM_)
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile)
import Janus.Data.Tensor (Vector, TensorIndex (..))
import Janus.Control.Reader.Base (ExpNewtype)
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cast (ExpPtrCast)
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Inject (ExpInject)
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet)
import Janus.Expression.Math (FMA, Hypot)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD
  ( CmdSIMDMem,
    ExpSIMDArith,
    ExpSIMDBits,
    ExpSIMDCast,
    ExpSIMDCmp,
    ExpSIMDFloat,
    ExpSIMDMask,
    ExpSIMDReduce,
    ExpSIMDSplit,
    SIMDElem (..),
    SIMDElemProof (..),
    SIMDMask,
    SIMDShape,
    SIMDVector,
  )
import Janus.SPMD.Cast
  ( SPMDBitcastFloat32,
    SPMDBitcastFloat64,
    SPMDCvtByLayout,
    SPMDInjectByLayout,
  )
import Janus.SPMD.Command ()
import Janus.SPMD.Expression
  ( SPMDBitsByLayout,
    SPMDCmpByLayout,
    SPMDFloatByLayout,
    SPMDIntegralByLayout,
    SPMDNumByLayout,
  )
import Janus.SPMD.Lane
  ( SPMDLanesByLayout,
    SPMDReduceByLayout,
    spmdAlignedUnitStride,
    spmdUnitStride,
    uniform,
  )
import Janus.SPMD.Raw
  ( SPMDRawBits,
    SPMDRawIntegral,
    SPMDRawReduce,
  )
import Janus.SPMD.Typed (SPMDTyped)
import Janus.SPMD.Types
import Janus.Typed (IdConstraint, JanusTyped, TypeableTyped)

--------------------------------------------------------------------------------
-- 15. Optimal Non-Splitting Gang Size Selection & Strip-Mining Loops
--------------------------------------------------------------------------------

data GangSize (n :: Nat) where
  Gang2 :: GangSize 2
  Gang4 :: GangSize 4
  Gang8 :: GangSize 8
  Gang16 :: GangSize 16
  Gang32 :: GangSize 32

-- | Static non-splitting gang width given target hardware vector register byte
-- size (@16@ for SSE2 / CUDA @float4@, @32@ for AVX / AVX2) and main element type @a@.
type family MaxSingleRegGang (regBytes :: Nat) (a :: K.Type) :: Nat where
  MaxSingleRegGang 32 Int8 = 32
  MaxSingleRegGang 32 Word8 = 32
  MaxSingleRegGang 32 Int16 = 16
  MaxSingleRegGang 32 Word16 = 16
  MaxSingleRegGang 32 Int32 = 8
  MaxSingleRegGang 32 Word32 = 8
  MaxSingleRegGang 32 Float = 8
  MaxSingleRegGang 32 Int64 = 4
  MaxSingleRegGang 32 Word64 = 4
  MaxSingleRegGang 32 Double = 4
  MaxSingleRegGang 16 Int8 = 16
  MaxSingleRegGang 16 Word8 = 16
  MaxSingleRegGang 16 Int16 = 8
  MaxSingleRegGang 16 Word16 = 8
  MaxSingleRegGang 16 Int32 = 4
  MaxSingleRegGang 16 Word32 = 4
  MaxSingleRegGang 16 Float = 4
  MaxSingleRegGang 16 Int64 = 2
  MaxSingleRegGang 16 Word64 = 2
  MaxSingleRegGang 16 Double = 2

clampGangWidth :: Int -> Int
clampGangWidth k
  | k >= 32 = 32
  | k >= 16 = 16
  | k >= 8 = 8
  | k >= 4 = 4
  | otherwise = 2

-- | Calculate the widest x86-64 gang size for a main type @a@ that fits in a
-- single hardware SIMD register without struct splitting (accounting for SSE2
-- 16B, AVX1 32B-float / 16B-integer, and AVX2 32B-all).
optimalX86GangSize :: forall a. (SIMDElem a) => X86Features -> Proxy a -> Int
optimalX86GangSize fs pa =
  let has256 =
        if simdElemIsFloat pa
          then hasX86Feature fs X86_AVX
          else hasX86Feature fs X86_AVX2
      regBytes = if has256 then 32 else 16
   in clampGangWidth (regBytes `div` simdElemBytes pa)

-- | Calculate the widest CUDA per-thread SIMD gang size for a main type @a@
-- that fits in a single @float4@ / @double2@ / @int4@ / @uint4@ 16-byte vector
-- without struct splitting.
optimalCUDAGangSize :: forall a. (SIMDElem a) => CUDAFeatures -> Proxy a -> Int
optimalCUDAGangSize _ pa =
  clampGangWidth (16 `div` simdElemBytes pa)

--------------------------------------------------------------------------------
-- Minimal SPMD Core, Element-Focused `SPMDOptimal`, & Trimmed `SPMDGang`
--------------------------------------------------------------------------------

-- | Core SPMD control, mask, memory, and @Int64@ index operations at gang
-- width @n@ without pulling non-@Int64@ scalar types into scope.
--
-- Higher-level @SPMD@ / @SPMDM@ instances ('ExpBool', 'ExpCond', 'ExpLet',
-- 'CmdRef', 'CmdCond', 'CmdWhile', 'CmdRange', 'Janus.Command.Case.CmdCase',
-- 'Bounded', and 'Janus.Expression.Cast.ExpBoolCast') resolve on demand via
-- their top-level instances from these primitives.
class
  ( KnownNat n,
    Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdRange m e,
    ExpBool e,
    ExpCond e,
    ExpLet e,
    ExpPair e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    JanusTyped e Int64,
    JanusTyped e Bool,
    ExpSIMDMask e n (SPMDMaskElem n),
    SPMDTyped n e Bool,
    SPMDTyped n e (),
    SPMDTyped n e (Ptr ()),
    ExpPtrCast (SPMD n e),
    M.CmdMem (SPMDM n m e) (SPMD n e),
    M.CmdStorable (SPMDM n m e) (SPMD n e) Int64,
    M.ExpPtr e Int64,
    PtrTreeOps n e (Ptr Int64),
    SPMDRawIntegral (SPMDRawLayout n Int64) n e Int64,
    SPMDRawBits (SPMDRawLayout n Int64) n e Int64,
    SPMDRawReduce (SPMDRawLayout n Int64) n e Int64
  ) =>
  SPMDGangCore (n :: Nat) m e

instance
  ( KnownNat n,
    Monad m,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdRange m e,
    ExpBool e,
    ExpCond e,
    ExpLet e,
    ExpPair e,
    ExpOrd e Int64,
    ExpIntegral e Int64,
    JanusTyped e Int64,
    JanusTyped e Bool,
    ExpSIMDMask e n (SPMDMaskElem n),
    SPMDTyped n e Bool,
    SPMDTyped n e (),
    SPMDTyped n e (Ptr ()),
    ExpPtrCast (SPMD n e),
    M.CmdMem (SPMDM n m e) (SPMD n e),
    M.CmdStorable (SPMDM n m e) (SPMD n e) Int64,
    M.ExpPtr e Int64,
    PtrTreeOps n e (Ptr Int64),
    SPMDRawIntegral (SPMDRawLayout n Int64) n e Int64,
    SPMDRawBits (SPMDRawLayout n Int64) n e Int64,
    SPMDRawReduce (SPMDRawLayout n Int64) n e Int64
  ) =>
  SPMDGangCore n m e

type SPMDElemCtx n m e a =
  ( M.CmdStorable (SPMDM n m e) (SPMD n e) a,
    M.ExpPtr e a,
    PtrTreeOps n e (Ptr a),
    SPMDCmpByLayout (SPMDLayout n a) n e a,
    SPMDInjectByLayout (SPMDLayout n a) n e a,
    SPMDLanesByLayout (SPMDLayout n a) n e a,
    SPMDReduceByLayout (SPMDLayout n a) n e a
  )

type SPMDIntCasts n e a =
  ( SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int64) n e a Int64,
    SPMDCvtByLayout (SPMDLayout n Int64) (SPMDLayout n a) n e Int64 a,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int32) n e a Int32,
    SPMDCvtByLayout (SPMDLayout n Int32) (SPMDLayout n a) n e Int32 a,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Float) n e a Float,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Double) n e a Double
  )

type SPMDIntElemCtx n m e a =
  ( SPMDElemCtx n m e a,
    SPMDBitsByLayout (SPMDLayout n a) n e a,
    SPMDIntegralByLayout (SPMDLayout n a) n e a,
    SPMDIntCasts n e a
  )

type SPMDFloatElemCtx n m e a =
  ( SPMDElemCtx n m e a,
    SPMDFloatByLayout (SPMDLayout n a) n e a,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int64) n e a Int64,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int32) n e a Int32,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Float) n e a Float,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Double) n e a Double,
    SPMDTyped n e (Complex a),
    SPMDExpr n e a ~ e (SPMDRep n e a)
  )

-- | Rich bundled constraint providing all SPMD expression and command
-- instances at gang width @n@ across all standard Janus scalar types.
class
  ( SPMDGangCore n m e,
    SPMDIntElemCtx n m e Int8,
    SPMDIntElemCtx n m e Word8,
    SPMDIntElemCtx n m e Int16,
    SPMDIntElemCtx n m e Word16,
    SPMDIntElemCtx n m e Int32,
    SPMDIntElemCtx n m e Word32,
    SPMDIntCasts n e Int64,
    SPMDIntElemCtx n m e Word64,
    SPMDFloatElemCtx n m e Float,
    SPMDFloatElemCtx n m e Double,
    SPMDCvtByLayout (SPMDLayout n Int32) (SPMDRawLayout n Word64) n e Int32 Word64,
    SPMDCvtByLayout (SPMDLayout n Int64) (SPMDRawLayout n Word64) n e Int64 Word64,
    SPMDCvtByLayout (SPMDLayout n Word32) (SPMDRawLayout n Word64) n e Word32 Word64,
    SPMDCvtByLayout (SPMDRawLayout n Word64) (SPMDRawLayout n Word64) n e Word64 Word64,
    SPMDBitcastFloat32 n e,
    SPMDBitcastFloat64 n e
  ) =>
  SPMDGang (n :: Nat) m e

instance
  ( SPMDGangCore n m e,
    SPMDIntElemCtx n m e Int8,
    SPMDIntElemCtx n m e Word8,
    SPMDIntElemCtx n m e Int16,
    SPMDIntElemCtx n m e Word16,
    SPMDIntElemCtx n m e Int32,
    SPMDIntElemCtx n m e Word32,
    SPMDIntCasts n e Int64,
    SPMDIntElemCtx n m e Word64,
    SPMDFloatElemCtx n m e Float,
    SPMDFloatElemCtx n m e Double,
    SPMDCvtByLayout (SPMDLayout n Int32) (SPMDRawLayout n Word64) n e Int32 Word64,
    SPMDCvtByLayout (SPMDLayout n Int64) (SPMDRawLayout n Word64) n e Int64 Word64,
    SPMDCvtByLayout (SPMDLayout n Word32) (SPMDRawLayout n Word64) n e Word32 Word64,
    SPMDCvtByLayout (SPMDRawLayout n Word64) (SPMDRawLayout n Word64) n e Word64 Word64,
    SPMDBitcastFloat32 n e,
    SPMDBitcastFloat64 n e
  ) =>
  SPMDGang n m e

type SPMDOptimalIntExtra n e a =
  ( SPMDBitsByLayout (SPMDLayout n a) n e a,
    SPMDIntegralByLayout (SPMDLayout n a) n e a,
    SPMDIntCasts n e a
  )

type SPMDOptimalFloatExtra n e a =
  ( SPMDFloatByLayout (SPMDLayout n a) n e a,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int64) n e a Int64,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Int32) n e a Int32,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Float) n e a Float,
    SPMDCvtByLayout (SPMDLayout n a) (SPMDLayout n Double) n e a Double,
    SPMDTyped n e (Complex a),
    SPMDExpr n e a ~ e (SPMDRep n e a)
  )

-- | Element-specific arithmetic, bitwise, or floating-point constraints
-- brought into scope by 'SPMDOptimal' when @a@ is instantiated to a concrete
-- 'SIMDElem' type.
type family SPMDOptimalExtra (n :: Nat) (e :: K.Type -> K.Type) (a :: K.Type) :: K.Constraint where
  SPMDOptimalExtra n e Int8 = SPMDOptimalIntExtra n e Int8
  SPMDOptimalExtra n e Word8 = SPMDOptimalIntExtra n e Word8
  SPMDOptimalExtra n e Int16 = SPMDOptimalIntExtra n e Int16
  SPMDOptimalExtra n e Word16 = SPMDOptimalIntExtra n e Word16
  SPMDOptimalExtra n e Int32 =
    ( SPMDOptimalIntExtra n e Int32,
      SPMDCvtByLayout (SPMDLayout n Int32) (SPMDRawLayout n Word64) n e Int32 Word64
    )
  SPMDOptimalExtra n e Word32 =
    ( SPMDOptimalIntExtra n e Word32,
      SPMDCvtByLayout (SPMDLayout n Word32) (SPMDRawLayout n Word64) n e Word32 Word64
    )
  SPMDOptimalExtra n e Int64 =
    ( SPMDIntCasts n e Int64,
      SPMDCvtByLayout (SPMDLayout n Int64) (SPMDRawLayout n Word64) n e Int64 Word64
    )
  SPMDOptimalExtra n e Word64 =
    ( SPMDOptimalIntExtra n e Word64,
      SPMDCvtByLayout (SPMDRawLayout n Word64) (SPMDRawLayout n Word64) n e Word64 Word64
    )
  SPMDOptimalExtra n e Float =
    ( SPMDOptimalFloatExtra n e Float,
      SPMDCvtByLayout (SPMDLayout n Word32) (SPMDRawLayout n Word64) n e Word32 Word64,
      SPMDBitcastFloat32 n e
    )
  SPMDOptimalExtra n e Double =
    ( SPMDOptimalFloatExtra n e Double,
      SPMDBitcastFloat64 n e
    )

-- | Focused SPMD constraint provided inside 'withOptimalGang' continuations,
-- bringing only 'SPMDGangCore' (@Bool@, @()@, @Ptr ()@, @Int64@) and main
-- element type @a@ into scope instead of all 10 scalar element types.
--
-- Callers that also need secondary element types or @Janus.Data.*@ SPMD
-- combinators can invoke 'withSPMDGang' on demand.
class
  ( SPMDGangCore n m e,
    M.CmdStorable (SPMDM n m e) (SPMD n e) a,
    M.ExpPtr (SPMD n e) a,
    PtrTreeOps n e (Ptr a),
    SPMDNumByLayout (SPMDLayout n a) n e a,
    SPMDCmpByLayout (SPMDLayout n a) n e a,
    SPMDInjectByLayout (SPMDLayout n a) n e a,
    SPMDLanesByLayout (SPMDLayout n a) n e a,
    SPMDReduceByLayout (SPMDLayout n a) n e a,
    SPMDOptimalExtra n e a
  ) =>
  SPMDOptimal (n :: Nat) m e a
  where
  -- | Bring the full multi-type 'SPMDGang' bundle into scope on demand.
  withSPMDGang :: ((SPMDGang n m e) => r) -> r

instance
  ( SPMDGang n m e,
    M.CmdStorable (SPMDM n m e) (SPMD n e) a,
    M.ExpPtr (SPMD n e) a,
    PtrTreeOps n e (Ptr a),
    SPMDNumByLayout (SPMDLayout n a) n e a,
    SPMDCmpByLayout (SPMDLayout n a) n e a,
    SPMDInjectByLayout (SPMDLayout n a) n e a,
    SPMDLanesByLayout (SPMDLayout n a) n e a,
    SPMDReduceByLayout (SPMDLayout n a) n e a,
    SPMDOptimalExtra n e a
  ) =>
  SPMDOptimal n m e a
  where
  withSPMDGang r = r

--------------------------------------------------------------------------------
-- Opaque Base Backend Capability Witness (`OptimalGangCaps`)
--------------------------------------------------------------------------------

-- | Quantified C-type witness for associated 'SIMDVector' and 'SIMDMask'
-- types on C/CUDA codegen backends and wrappers over them.
class
  ( forall n x. (SIMDShape n x) => JanusCTyped (SIMDVector topE n x),
    forall n x. (SIMDShape n x) => JanusCTyped (SIMDMask topE n x)
  ) =>
  TopSIMDCTyped (topE :: K.Type -> K.Type)

instance
  ( forall n x. (SIMDShape n x) => JanusCTyped (SIMDVector topE n x),
    forall n x. (SIMDShape n x) => JanusCTyped (SIMDMask topE n x)
  ) =>
  TopSIMDCTyped topE

-- | Inductive witness characterizing the open 'JanusTyped' type family for
-- an expression functor @topE@ (and any nested 'TypeableTyped' layers @curE@).
data JanusTypedChain (topE :: K.Type -> K.Type) (curE :: K.Type -> K.Type) where
  -- | Host-evaluated base backend whose 'JanusTyped' is 'IdConstraint'.
  JTChainId ::
    (JanusTyped curE ~ IdConstraint) =>
    JanusTypedChain topE curE
  -- | C/CUDA codegen base backend whose 'JanusTyped' is 'JanusCTyped'.
  JTChainC ::
    (JanusTyped curE ~ JanusCTyped, TopSIMDCTyped topE) =>
    JanusTypedChain topE curE
  -- | Wrapper expression layer adding 'Typeable' via 'TypeableTyped'.
  JTChainStep ::
    (Typeable nextE, JanusTyped curE ~ TypeableTyped nextE) =>
    -- | Underlying 'JanusTypedChain' for the wrapped @nextE@ functor.
    !(JanusTypedChain topE nextE) ->
    JanusTypedChain topE curE

proveTyped ::
  forall topE curE a.
  (Typeable a, (TopSIMDCTyped topE) => JanusCTyped a) =>
  JanusTypedChain topE curE ->
  Dict (JanusTyped curE a)
proveTyped = \case
  JTChainId -> Dict
  JTChainC -> Dict
  JTChainStep rest ->
    case proveTyped @topE @_ @a rest of
      Dict -> Dict

liftWrapperTypedChainAux ::
  forall wrapE baseE curE.
  ((TopSIMDCTyped baseE) => TopSIMDCTyped wrapE) =>
  JanusTypedChain baseE curE ->
  JanusTypedChain wrapE curE
liftWrapperTypedChainAux = \case
  JTChainId -> JTChainId
  JTChainC -> JTChainC
  JTChainStep rest -> JTChainStep (liftWrapperTypedChainAux @wrapE @baseE rest)

-- | Lift a 'JanusTypedChain' across a newtype expression wrapper @wrapE@ that
-- shares @'JanusTyped' wrapE ~ 'JanusTyped' baseE@ (e.g. 'Janus.Control.Reader.Base.ReaderExp', 'Janus.Bench.Types.Bench').
liftWrapperTypedChain ::
  forall wrapE baseE.
  ( JanusTyped wrapE ~ JanusTyped baseE,
    (TopSIMDCTyped baseE) => TopSIMDCTyped wrapE
  ) =>
  JanusTypedChain baseE baseE ->
  JanusTypedChain wrapE wrapE
liftWrapperTypedChain = \case
  JTChainId -> JTChainId
  JTChainC -> JTChainC
  JTChainStep rest -> JTChainStep (liftWrapperTypedChainAux @wrapE @baseE rest)

-- | Lift a 'JanusTypedChain' across an expression wrapper @wrapE@ whose
-- @'JanusTyped' wrapE@ is @'TypeableTyped' baseE@ (e.g. @CSEExp@).
liftStepTypedChain ::
  forall wrapE baseE.
  ( Typeable baseE,
    JanusTyped wrapE ~ TypeableTyped baseE,
    (TopSIMDCTyped baseE) => TopSIMDCTyped wrapE
  ) =>
  JanusTypedChain baseE baseE ->
  JanusTypedChain wrapE wrapE
liftStepTypedChain ch = JTChainStep (liftWrapperTypedChainAux @wrapE @baseE ch)

class
  ( Num (e (SIMDVector e n a)),
    ExpSIMDArith e n a,
    ExpSIMDCmp e n a,
    ExpSIMDBits e n a,
    ExpSIMDReduce e n a,
    CmdSIMDMem m e n a
  ) =>
  GangIntShape m e (n :: Nat) a

instance
  ( Num (e (SIMDVector e n a)),
    ExpSIMDArith e n a,
    ExpSIMDCmp e n a,
    ExpSIMDBits e n a,
    ExpSIMDReduce e n a,
    CmdSIMDMem m e n a
  ) =>
  GangIntShape m e n a

class
  ( Floating (e (SIMDVector e n a)),
    FMA (e (SIMDVector e n a)),
    Hypot (e (SIMDVector e n a)),
    ExpSIMDFloat e n a,
    ExpSIMDCmp e n a,
    ExpSIMDReduce e n a,
    CmdSIMDMem m e n a
  ) =>
  GangFloatShape m e (n :: Nat) a

instance
  ( Floating (e (SIMDVector e n a)),
    FMA (e (SIMDVector e n a)),
    Hypot (e (SIMDVector e n a)),
    ExpSIMDFloat e n a,
    ExpSIMDCmp e n a,
    ExpSIMDReduce e n a,
    CmdSIMDMem m e n a
  ) =>
  GangFloatShape m e n a

class
  ( ExpOrd e a,
    ExpBits e a,
    ExpIntegral e a,
    ExpInject e a,
    M.ExpPtr e a,
    M.CmdStorable m e a
  ) =>
  GangIntScalar m e a

instance
  ( ExpOrd e a,
    ExpBits e a,
    ExpIntegral e a,
    ExpInject e a,
    M.ExpPtr e a,
    M.CmdStorable m e a
  ) =>
  GangIntScalar m e a

class
  ( GangIntScalar m e a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    GangIntShape m e 16 a,
    GangIntShape m e 32 a,
    ExpSIMDSplit e 8 4 a,
    ExpSIMDSplit e 16 8 a,
    ExpSIMDSplit e 32 16 a
  ) =>
  GangInt8Caps m e a

instance
  ( GangIntScalar m e a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    GangIntShape m e 16 a,
    GangIntShape m e 32 a,
    ExpSIMDSplit e 8 4 a,
    ExpSIMDSplit e 16 8 a,
    ExpSIMDSplit e 32 16 a
  ) =>
  GangInt8Caps m e a

class
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    GangIntShape m e 16 a,
    ExpSIMDSplit e 4 2 a,
    ExpSIMDSplit e 8 4 a,
    ExpSIMDSplit e 16 8 a
  ) =>
  GangInt16Caps m e a

instance
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    GangIntShape m e 16 a,
    ExpSIMDSplit e 4 2 a,
    ExpSIMDSplit e 8 4 a,
    ExpSIMDSplit e 16 8 a
  ) =>
  GangInt16Caps m e a

class
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    ExpSIMDSplit e 4 2 a,
    ExpSIMDSplit e 8 4 a
  ) =>
  GangInt32Caps m e a

instance
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    GangIntShape m e 8 a,
    ExpSIMDSplit e 4 2 a,
    ExpSIMDSplit e 8 4 a
  ) =>
  GangInt32Caps m e a

class
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    ExpSIMDSplit e 4 2 a
  ) =>
  GangInt64Caps m e a

instance
  ( GangIntScalar m e a,
    GangIntShape m e 2 a,
    GangIntShape m e 4 a,
    ExpSIMDSplit e 4 2 a
  ) =>
  GangInt64Caps m e a

class
  ( ExpOrd e Float,
    Floating (e Float),
    FMA (e Float),
    Hypot (e Float),
    ExpInject e Float,
    M.ExpPtr e Float,
    M.CmdStorable m e Float,
    M.ExpPtr e (Complex Float),
    GangFloatShape m e 2 Float,
    GangFloatShape m e 4 Float,
    GangFloatShape m e 8 Float,
    ExpSIMDSplit e 4 2 Float,
    ExpSIMDSplit e 8 4 Float
  ) =>
  GangFloat32Caps m e

instance
  ( ExpOrd e Float,
    Floating (e Float),
    FMA (e Float),
    Hypot (e Float),
    ExpInject e Float,
    M.ExpPtr e Float,
    M.CmdStorable m e Float,
    M.ExpPtr e (Complex Float),
    GangFloatShape m e 2 Float,
    GangFloatShape m e 4 Float,
    GangFloatShape m e 8 Float,
    ExpSIMDSplit e 4 2 Float,
    ExpSIMDSplit e 8 4 Float
  ) =>
  GangFloat32Caps m e

class
  ( ExpOrd e Double,
    Floating (e Double),
    FMA (e Double),
    Hypot (e Double),
    ExpInject e Double,
    M.ExpPtr e Double,
    M.CmdStorable m e Double,
    M.ExpPtr e (Complex Double),
    GangFloatShape m e 2 Double,
    GangFloatShape m e 4 Double,
    ExpSIMDSplit e 4 2 Double
  ) =>
  GangFloat64Caps m e

instance
  ( ExpOrd e Double,
    Floating (e Double),
    FMA (e Double),
    Hypot (e Double),
    ExpInject e Double,
    M.ExpPtr e Double,
    M.CmdStorable m e Double,
    M.ExpPtr e (Complex Double),
    GangFloatShape m e 2 Double,
    GangFloatShape m e 4 Double,
    ExpSIMDSplit e 4 2 Double
  ) =>
  GangFloat64Caps m e

-- | Opaque capability bundle encapsulating the base scalar, memory, and
-- hardware SIMD typeclasses of @(m, e)@.
--
-- Storing these capabilities inside a GADT method of 'CmdOptimalGang' (rather
-- than as superclasses or 5-width 'SPMDGang' instance contexts) keeps
-- 'CmdOptimalGang' wrapper instances @O(1)@ at call sites.
data OptimalGangCaps (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) where
  -- | Construct an 'OptimalGangCaps' bundle from the base @(m, e)@ instances
  -- and a 'JanusTypedChain' witness for @e@.
  OptimalGangCaps ::
    ( Typeable m,
      Typeable e,
      Monad m,
      CmdRef m e,
      CmdCond m e,
      CmdWhile m e,
      CmdRange m e,
      M.CmdMem m e,
      ExpBool e,
      ExpCond e,
      ExpLet e,
      ExpPair e,
      ExpPtrCast e,
      ExpNewtype e,
      ExpSIMDCast e,
      M.ExpPtr e Bool,
      GangInt8Caps m e Int8,
      GangInt8Caps m e Word8,
      GangInt16Caps m e Int16,
      GangInt16Caps m e Word16,
      GangInt32Caps m e Int32,
      GangInt32Caps m e Word32,
      GangInt64Caps m e Int64,
      GangInt64Caps m e Word64,
      GangFloat32Caps m e,
      GangFloat64Caps m e
    ) =>
    -- | Witness characterizing @'JanusTyped' e@ for scalar, pointer, pair, and SIMD types.
    !(JanusTypedChain e e) ->
    OptimalGangCaps m e

withPtrTrees ::
  forall e p r.
  (Typeable e, Typeable p, ExpPair e, (TopSIMDCTyped e) => JanusCTyped p) =>
  JanusTypedChain e e ->
  ( ( JanusTyped e p,
      PtrTreeOps 2 e p,
      PtrTreeOps 4 e p,
      PtrTreeOps 8 e p,
      PtrTreeOps 16 e p,
      PtrTreeOps 32 e p
    ) =>
    r
  ) ->
  r
withPtrTrees ch k =
  case ( proveTyped @e @e @p ch,
         proveTyped @e @e @(PtrTree 2 p) ch,
         proveTyped @e @e @(PtrTree 4 p) ch,
         proveTyped @e @e @(PtrTree 8 p) ch,
         proveTyped @e @e @(PtrTree 16 p) ch,
         proveTyped @e @e @(PtrTree 32 p) ch
       ) of
    (Dict, Dict, Dict, Dict, Dict, Dict) -> k

withAllRepsTyped ::
  forall e t r r2 r4 r8 r16 r32.
  ( r2 ~ SPMDRep 2 e t,
    r4 ~ SPMDRep 4 e t,
    r8 ~ SPMDRep 8 e t,
    r16 ~ SPMDRep 16 e t,
    r32 ~ SPMDRep 32 e t,
    Typeable r2,
    Typeable r4,
    Typeable r8,
    Typeable r16,
    Typeable r32,
    (TopSIMDCTyped e) => JanusCTyped r2,
    (TopSIMDCTyped e) => JanusCTyped r4,
    (TopSIMDCTyped e) => JanusCTyped r8,
    (TopSIMDCTyped e) => JanusCTyped r16,
    (TopSIMDCTyped e) => JanusCTyped r32
  ) =>
  JanusTypedChain e e ->
  ( ( JanusTyped e r2,
      JanusTyped e r4,
      JanusTyped e r8,
      JanusTyped e r16,
      JanusTyped e r32
    ) =>
    r
  ) ->
  r
withAllRepsTyped ch k =
  case ( proveTyped @e @e @r2 ch,
         proveTyped @e @e @r4 ch,
         proveTyped @e @e @r8 ch,
         proveTyped @e @e @r16 ch,
         proveTyped @e @e @r32 ch
       ) of
    (Dict, Dict, Dict, Dict, Dict) -> k

withAllSPMDGangs ::
  forall m e r.
  OptimalGangCaps m e ->
  ((SPMDGang 2 m e, SPMDGang 4 m e, SPMDGang 8 m e, SPMDGang 16 m e, SPMDGang 32 m e) => r) ->
  r
withAllSPMDGangs (OptimalGangCaps ch) k =
  case ( proveTyped @e @e @Bool ch,
         proveTyped @e @e @VoidPtr ch,
         proveTyped @e @e @SPMDUnit ch,
         proveTyped @e @e @(Complex Float) ch,
         proveTyped @e @e @(Complex Double) ch
       ) of
    (Dict, Dict, Dict, Dict, Dict) ->
      withPtrTrees @e @VoidPtr ch $
        withPtrTrees @e @(Ptr Bool) ch $
          withPtrTrees @e @(Ptr Int8) ch $
            withPtrTrees @e @(Ptr Word8) ch $
              withPtrTrees @e @(Ptr Int16) ch $
                withPtrTrees @e @(Ptr Word16) ch $
                  withPtrTrees @e @(Ptr Int32) ch $
                    withPtrTrees @e @(Ptr Word32) ch $
                      withPtrTrees @e @(Ptr Int64) ch $
                        withPtrTrees @e @(Ptr Word64) ch $
                          withPtrTrees @e @(Ptr Float) ch $
                            withPtrTrees @e @(Ptr Double) ch $
                              withPtrTrees @e @(Ptr (Complex Float)) ch $
                                withPtrTrees @e @(Ptr (Complex Double)) ch $
                                  withAllRepsTyped @e @Bool ch $
                                    withAllRepsTyped @e @Int8 ch $
                                      withAllRepsTyped @e @Word8 ch $
                                        withAllRepsTyped @e @Int16 ch $
                                          withAllRepsTyped @e @Word16 ch $
                                            withAllRepsTyped @e @Int32 ch $
                                              withAllRepsTyped @e @Word32 ch $
                                                withAllRepsTyped @e @Int64 ch $
                                                  withAllRepsTyped @e @Word64 ch $
                                                    withAllRepsTyped @e @Float ch $
                                                      withAllRepsTyped @e @Double ch $
                                                        withAllRepsTyped @e @(Complex Float) ch $
                                                          withAllRepsTyped @e @(Complex Double) ch k

dispatchGangInt ::
  forall m e r.
  (SPMDGang 2 m e, SPMDGang 4 m e, SPMDGang 8 m e, SPMDGang 16 m e, SPMDGang 32 m e) =>
  Int ->
  (forall n. (SPMDGang n m e) => Proxy n -> m r) ->
  m r
dispatchGangInt w k = case clampGangWidth w of
  2 -> k (Proxy @2)
  4 -> k (Proxy @4)
  8 -> k (Proxy @8)
  16 -> k (Proxy @16)
  _ -> k (Proxy @32)

dispatchOptimalElem ::
  forall m e a r.
  (SPMDGang 2 m e, SPMDGang 4 m e, SPMDGang 8 m e, SPMDGang 16 m e, SPMDGang 32 m e) =>
  Int ->
  SIMDElemProof a ->
  (forall n. (SPMDOptimal n m e a) => Proxy n -> m r) ->
  m r
dispatchOptimalElem w prf k =
  let cw = clampGangWidth w
      go ::
        forall x.
        ( SPMDOptimal 2 m e x,
          SPMDOptimal 4 m e x,
          SPMDOptimal 8 m e x,
          SPMDOptimal 16 m e x,
          SPMDOptimal 32 m e x
        ) =>
        ((forall n. (SPMDOptimal n m e x) => Proxy n -> m r) -> m r)
      go f = case cw of
        2 -> f (Proxy @2)
        4 -> f (Proxy @4)
        8 -> f (Proxy @8)
        16 -> f (Proxy @16)
        _ -> f (Proxy @32)
   in case prf of
        SIMDElemInt8 -> go @Int8 k
        SIMDElemWord8 -> go @Word8 k
        SIMDElemInt16 -> go @Int16 k
        SIMDElemWord16 -> go @Word16 k
        SIMDElemInt32 -> go @Int32 k
        SIMDElemWord32 -> go @Word32 k
        SIMDElemInt64 -> go @Int64 k
        SIMDElemWord64 -> go @Word64 k
        SIMDElemFloat -> go @Float k
        SIMDElemDouble -> go @Double k

-- | Dispatch an 'SPMDOptimal' continuation at a clamped runtime/target gang
-- width @w@ using an 'OptimalGangCaps' capability bundle.
dispatchOptimalGang ::
  forall m e a r.
  (SIMDElem a) =>
  OptimalGangCaps m e ->
  Int ->
  Proxy a ->
  (forall n. (SPMDOptimal n m e a) => Proxy n -> m r) ->
  m r
dispatchOptimalGang caps w _ k =
  withAllSPMDGangs caps $
    dispatchOptimalElem @m @e @a w (simdElemProof @a) k

-- | Transition from an unlifted @(m, e)@ program to an SPMD subprogram at the
-- optimal non-splitting gang width for a main element type @a@.
class (Monad m) => CmdOptimalGang m e | m -> e, e -> m where
  -- | Base backend capability bundle for @(m, e)@.
  optimalGangCaps :: OptimalGangCaps m e

  -- | Run an SPMD continuation at the target's optimal single-register gang
  -- width for element type @a@, bringing only 'SPMDOptimal' dictionaries for
  -- @a@ (plus @Int64@ and @Bool@) into scope.
  withOptimalGang ::
    (SIMDElem a) =>
    Proxy a ->
    (forall n. (SPMDOptimal n m e a) => Proxy n -> m r) ->
    m r

instance CmdOptimalGang IO Identity where
  optimalGangCaps = OptimalGangCaps JTChainId
  withOptimalGang pa k = do
    fs <- getActiveHostX86Features
    dispatchOptimalGang optimalGangCaps (optimalX86GangSize fs pa) pa k

-- Strip-Mining Combinators Over Unlifted Ranges & Tensors
foreachImpl ::
  forall n m e.
  (SPMDGangCore n m e) =>
  Bool ->
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachImpl knownAligned p l u body = do
  let w = fromIntegral (natVal p) :: Int64
      wE = fromIntegral w :: e Int64
  total <- letM (ifThenElse (u `gt` l) (u - l) 0)
  numFull <- letM (total `quotient` wE)
  alignedEnd <- letM (l + numFull * wE)
  rangeM 0 numFull $ \g -> do
    base <- letM (l + g * wE)
    let idx = if knownAligned then spmdAlignedUnitStride base else spmdUnitStride base
    withSPMD p (body idx)
  whenM_ (alignedEnd `lt` u) $ do
    let tailIdx = if knownAligned then spmdAlignedUnitStride alignedEnd else spmdUnitStride alignedEnd
    withSPMDMasked (tailIdx `lt` uniform u) (body tailIdx)

-- | Strip-mine @[0, u)@ into an aligned unmasked main SIMD loop plus a masked tail.
foreach0M ::
  forall n m e.
  (SPMDGangCore n m e) =>
  Proxy n ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreach0M p u = foreachImpl True p 0 u

-- | Strip-mine @[l, u)@ where @l@ is known to be a multiple of @n@.
foreachAlignedM ::
  forall n m e.
  (SPMDGangCore n m e) =>
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachAlignedM = foreachImpl True

-- | Strip-mine a general @[l, u)@ range into unmasked SIMD gangs plus a masked tail.
foreachM ::
  forall n m e.
  (SPMDGangCore n m e) =>
  Proxy n ->
  e Int64 ->
  e Int64 ->
  (SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachM = foreachImpl False

-- | Strip-mine a 1D 'A.Vector' @[0, d)@ with static tail elimination when @d % n == 0@.
foreachTensorM ::
  forall d n m e a.
  (KnownNat d, SPMDGangCore n m e) =>
  Proxy n ->
  Vector d e a ->
  (TensorIndex '[d] (SPMD n e) -> SPMDM n m e ()) ->
  m ()
foreachTensorM p _ body = do
  let total = fromIntegral (natVal (Proxy @d)) :: Int64
      w = fromIntegral (natVal p) :: Int64
      wE = fromIntegral w :: e Int64
      fullGangs = total `quot` w
      remElems = total `rem` w
  when (fullGangs > 0) $
    rangeM 0 (fromIntegral fullGangs) $ \g -> do
      base <- letM (g * wE)
      withSPMD p (body (spmdAlignedUnitStride base :. Z))
  when (remElems > 0) $ do
    let tailStart = fullGangs * w
        tailIdx = SPMD (pure (I64UnitStride True (fromIntegral tailStart)))
    withSPMDMasked (tailIdx `lt` fromIntegral total) (body (tailIdx :. Z))

foldForeach0M ::
  forall n m e s.
  (SPMDGangCore n m e, SPMDTyped n e s) =>
  Proxy n ->
  e Int64 ->
  SPMD n e s ->
  (SPMD n e s -> SPMD n e Int64 -> SPMDM n m e (SPMD n e s)) ->
  m (SPMD n e s)
foldForeach0M p u s0 step = do
  accRef <- withSPMD p (newRef s0)
  foreach0M p u $ \i -> do
    cur <- readRef accRef
    nxt <- step cur i
    writeRef accRef nxt
  withSPMD p (readRef accRef)

foldForeachTensorM ::
  forall d n m e a s.
  (KnownNat d, SPMDGangCore n m e, SPMDTyped n e s) =>
  Proxy n ->
  Vector d e a ->
  SPMD n e s ->
  (SPMD n e s -> TensorIndex '[d] (SPMD n e) -> SPMDM n m e (SPMD n e s)) ->
  m (SPMD n e s)
foldForeachTensorM p vec s0 step = do
  accRef <- withSPMD p (newRef s0)
  foreachTensorM p vec $ \idx -> do
    cur <- readRef accRef
    nxt <- step cur idx
    writeRef accRef nxt
  withSPMD p (readRef accRef)

foreachOptimal0M ::
  (CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  e Int64 ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachOptimal0M pm u body =
  withOptimalGang pm $ \pn -> foreach0M pn u body

foreachOptimalM ::
  (CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  e Int64 ->
  e Int64 ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e Int64 -> SPMDM n m e ()) ->
  m ()
foreachOptimalM pm l u body =
  withOptimalGang pm $ \pn -> foreachM pn l u body

foreachOptimalTensorM ::
  (KnownNat d, CmdOptimalGang m e, SIMDElem mainTy) =>
  Proxy mainTy ->
  Vector d e a ->
  (forall n. (SPMDOptimal n m e mainTy) => TensorIndex '[d] (SPMD n e) -> SPMDM n m e ()) ->
  m ()
foreachOptimalTensorM pm vec body =
  withOptimalGang pm $ \pn -> foreachTensorM pn vec body

foldForeachOptimal0M ::
  (CmdOptimalGang m e, SIMDElem mainTy, forall n. (SPMDOptimal n m e mainTy) => SPMDTyped n e s) =>
  Proxy mainTy ->
  e Int64 ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s) ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s -> SPMD n e Int64 -> SPMDM n m e (SPMD n e s)) ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s -> e s) ->
  m (e s)
foldForeachOptimal0M pm u s0 step reduceFn =
  withOptimalGang pm $ \pn ->
    reduceFn <$> foldForeach0M pn u s0 step

foldForeachOptimalTensorM ::
  (KnownNat d, CmdOptimalGang m e, SIMDElem mainTy, forall n. (SPMDOptimal n m e mainTy) => SPMDTyped n e s) =>
  Proxy mainTy ->
  Vector d e a ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s) ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s -> TensorIndex '[d] (SPMD n e) -> SPMDM n m e (SPMD n e s)) ->
  (forall n. (SPMDOptimal n m e mainTy) => SPMD n e s -> e s) ->
  m (e s)
foldForeachOptimalTensorM pm vec s0 step reduceFn =
  withOptimalGang pm $ \pn ->
    reduceFn <$> foldForeachTensorM pn vec s0 step
