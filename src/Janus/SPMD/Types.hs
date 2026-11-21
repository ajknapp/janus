{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.SPMD.Types where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), Reader, ReaderT (..), asks, runReader)
import Control.Monad.Trans.Class (lift)
import Data.Bits (Bits (..))
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Kind as K
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (eqT)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types
  ( CBool,
    CChar,
    CDouble,
    CFloat,
    CInt,
    CLLong,
    CLong,
    CSChar,
    CShort,
    CSize,
    CUChar,
    CUInt,
    CULLong,
    CULong,
    CUShort,
  )
import Foreign.LibFFI (argPtr, callFFI, retVoid)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal)
import Janus.Backend.C
import Janus.Backend.C.CUDA (CUDADejaFu (..), CUDAIO (..), CmdSync (..), JanusCUDA (..))
import Janus.Backend.C.LinuxX86_64
import Janus.Command.Ref (CmdRef (..))
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (janusCReallyUnsafeCast, janusCUnsafeCast)
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Inject (defaultJanusCType)
import Janus.Expression.SIMD
import Janus.FFI.Arg (FFIArg)
import Janus.FFI.Ret (FFIRet)
import Janus.Typed (JanusTyped)
import Language.C.Quote

--------------------------------------------------------------------------------
-- 1. Canonical Mask Element & Execution Mask Representation
--------------------------------------------------------------------------------

type family SPMDMaskElem (n :: Nat) :: K.Type where
  SPMDMaskElem 2 = Int64
  SPMDMaskElem 4 = Int32
  SPMDMaskElem 8 = Int32
  SPMDMaskElem 16 = Int16
  SPMDMaskElem 32 = Word8

-- | Execution mask tracking whether all @n@ lanes are statically active
-- ('SPMDMaskAllTrue') or governed by a runtime 'SIMDMask' ('SPMDMask').
data SPMDMask (n :: Nat) (e :: K.Type -> K.Type)
  = SPMDMaskAllTrue
  | SPMDMask (e (SIMDMask e n (SPMDMaskElem n)))

toSIMDMask ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMDMask n e ->
  e (SIMDMask e n (SPMDMaskElem n))
toSIMDMask SPMDMaskAllTrue = mtrue
toSIMDMask (SPMDMask m) = m

fromSIMDMask :: e (SIMDMask e n (SPMDMaskElem n)) -> SPMDMask n e
fromSIMDMask = SPMDMask

spmdMaskAndRaw ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMDMask n e ->
  e (SIMDMask e n (SPMDMaskElem n)) ->
  e (SIMDMask e n (SPMDMaskElem n))
spmdMaskAndRaw SPMDMaskAllTrue c = c
spmdMaskAndRaw (SPMDMask m) c = m `mand` c

spmdMaskAndNotRaw ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  e (SIMDMask e n (SPMDMaskElem n)) ->
  SPMDMask n e ->
  e (SIMDMask e n (SPMDMaskElem n))
spmdMaskAndNotRaw c SPMDMaskAllTrue = mnot c
spmdMaskAndNotRaw c (SPMDMask m) = c `mandnot` m

spmdMaskAnd ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMDMask n e ->
  e (SIMDMask e n (SPMDMaskElem n)) ->
  SPMDMask n e
spmdMaskAnd m c = SPMDMask (spmdMaskAndRaw m c)

spmdMaskAndNot ::
  (ExpSIMDMask e n (SPMDMaskElem n)) =>
  e (SIMDMask e n (SPMDMaskElem n)) ->
  SPMDMask n e ->
  SPMDMask n e
spmdMaskAndNot c m = SPMDMask (spmdMaskAndNotRaw c m)

--------------------------------------------------------------------------------
-- 2. Multi-Register / Struct Storage Types (SIMDPair, VoidPtr, SPMDUnit)
--------------------------------------------------------------------------------

data SIMDPair a b = SIMDPair
  { simdPairFst :: !a,
    simdPairSnd :: !b
  }
  deriving stock (Eq, Ord, Show, Generic)

simdPairLayout :: forall a b. (Storable.Storable a, Storable.Storable b) => (Int, Int, Int)
simdPairLayout =
  let szA = Storable.sizeOf (undefined :: a)
      alA = max 1 (Storable.alignment (undefined :: a))
      szB = Storable.sizeOf (undefined :: b)
      alB = max 1 (Storable.alignment (undefined :: b))
      al = max alA alB
      offB = (szA + alB - 1) .&. complement (alB - 1)
      tot = (offB + szB + al - 1) .&. complement (al - 1)
   in (tot, al, offB)

instance (Storable.Storable a, Storable.Storable b) => Storable.Storable (SIMDPair a b) where
  sizeOf _ = let (tot, _, _) = simdPairLayout @a @b in tot
  alignment _ = let (_, al, _) = simdPairLayout @a @b in al
  peek p = do
    let (_, _, offB) = simdPairLayout @a @b
    a <- Storable.peek (castPtr p :: Ptr a)
    b <- Storable.peek (castPtr (p `plusPtr` offB) :: Ptr b)
    pure (SIMDPair a b)
  poke p (SIMDPair a b) = do
    let (_, _, offB) = simdPairLayout @a @b
    Storable.poke (castPtr p :: Ptr a) a
    Storable.poke (castPtr (p `plusPtr` offB) :: Ptr b) b

instance (JanusCTyped a, JanusCTyped b) => JanusCTyped (SIMDPair a b) where
  jctype = defaultJanusCType

instance (JanusCTyped a, JanusCTyped b, Storable.Storable a, Storable.Storable b) => LinuxX86_64ABIParam (SIMDPair a b) where
  x86EmitParam k = do
    let aid = mkArgId k
        JCType vspec vdec = voidPtrType
        (tot, _, _) = simdPairLayout @a @b
    loadExp <- emitMemcpyLoad @(SIMDPair a b) tot (Var aid noLoc)
    pure (Param (Just aid) vspec vdec noLoc, loadExp)
  x86WithArg v k =
    let (tot, al, _) = simdPairLayout @a @b
     in allocaBytesAligned tot (max 32 al) $ \p -> Storable.poke p v >> k (argPtr p)

instance (JanusCTyped a, JanusCTyped b, Storable.Storable a, Storable.Storable b) => LinuxX86_64ABIRet (SIMDPair a b) where
  x86FinishReturn =
    let (tot, _, _) = simdPairLayout @a @b
     in emitMemcpyRet tot
  x86CallReturn _ fp args =
    let (tot, al, _) = simdPairLayout @a @b
     in allocaBytesAligned tot (max 32 al) $ \pOut -> do
          callFFI fp retVoid (reverse args <> [argPtr pOut])
          Storable.peek pOut

newtype VoidPtr = VoidPtr {getVoidPtr :: Ptr ()}
  deriving newtype (Eq, Ord, Show, Storable.Storable, FFIArg, FFIRet)

instance JanusCTyped VoidPtr where
  jctype _ = pure voidPtrType

newtype SPMDUnit = SPMDUnit {getSPMDUnit :: Word8}
  deriving newtype (Eq, Ord, Show, Storable.Storable, FFIArg, FFIRet)

instance JanusCTyped SPMDUnit where
  jctype _ = jctype (Proxy @Word8)

class ExpPair (e :: K.Type -> K.Type) where
  mkPair :: (JanusTyped e a, JanusTyped e b) => e a -> e b -> e (SIMDPair a b)
  fstPair :: (JanusTyped e a, JanusTyped e b) => e (SIMDPair a b) -> e a
  sndPair :: (JanusTyped e a, JanusTyped e b) => e (SIMDPair a b) -> e b
  mkVoidPtr :: e (Ptr ()) -> e VoidPtr
  unVoidPtr :: e VoidPtr -> e (Ptr ())
  mkSPMDUnit :: e SPMDUnit
  coerceExp :: (Coercible a b, JanusTyped e b) => e a -> e b

instance ExpPair Identity where
  mkPair = liftA2 SIMDPair
  fstPair = fmap simdPairFst
  sndPair = fmap simdPairSnd
  mkVoidPtr = coerce
  unVoidPtr = coerce
  mkSPMDUnit = Identity (SPMDUnit 0)
  coerceExp = coerce

instance ExpPair JanusC where
  mkPair :: forall a b. (JanusCTyped a, JanusCTyped b) => JanusC a -> JanusC b -> JanusC (SIMDPair a b)
  mkPair (JanusC ma) (JanusC mb) = JanusC $ do
    RVal a <- ma
    RVal b <- mb
    ty@(JCType spec dec) <- getJanusCType (Proxy @(SIMDPair a b))
    let lit =
          CompoundLit
            (Type spec dec noLoc)
            [ (Just (Designation [MemberDesignator (Id "simdPairFst" noLoc) noLoc] noLoc), ExpInitializer a noLoc),
              (Just (Designation [MemberDesignator (Id "simdPairSnd" noLoc) noLoc] noLoc), ExpInitializer b noLoc)
            ]
            noLoc
    RVal <$> emitLocalVal ty lit
  fstPair (JanusC mp) = JanusC $ do
    RVal p <- mp
    pure (RVal (Member p (Id "simdPairFst" noLoc) noLoc))
  sndPair (JanusC mp) = JanusC $ do
    RVal p <- mp
    pure (RVal (Member p (Id "simdPairSnd" noLoc) noLoc))
  mkVoidPtr = janusCReallyUnsafeCast
  unVoidPtr = janusCReallyUnsafeCast
  mkSPMDUnit = JanusC (RVal <$> jlitc (0 :: Word8))
  coerceExp = janusCUnsafeCast

instance ExpPair LinuxX86_64C where
  mkPair (LinuxX86_64C a) (LinuxX86_64C b) = LinuxX86_64C (mkPair a b)
  fstPair (LinuxX86_64C p) = LinuxX86_64C (fstPair p)
  sndPair (LinuxX86_64C p) = LinuxX86_64C (sndPair p)
  mkVoidPtr (LinuxX86_64C p) = LinuxX86_64C (mkVoidPtr p)
  unVoidPtr (LinuxX86_64C p) = LinuxX86_64C (unVoidPtr p)
  mkSPMDUnit = LinuxX86_64C mkSPMDUnit
  coerceExp (LinuxX86_64C x) = LinuxX86_64C (coerceExp x)

instance ExpPair JanusCUDA where
  mkPair (JanusCUDA a) (JanusCUDA b) = JanusCUDA (mkPair a b)
  fstPair (JanusCUDA p) = JanusCUDA (fstPair p)
  sndPair (JanusCUDA p) = JanusCUDA (sndPair p)
  mkVoidPtr (JanusCUDA p) = JanusCUDA (mkVoidPtr p)
  unVoidPtr (JanusCUDA p) = JanusCUDA (unVoidPtr p)
  mkSPMDUnit = JanusCUDA mkSPMDUnit
  coerceExp (JanusCUDA x) = JanusCUDA (coerceExp x)

instance ExpPair CUDAIO where
  mkPair = liftA2 SIMDPair
  fstPair = fmap simdPairFst
  sndPair = fmap simdPairSnd
  mkVoidPtr = coerce
  unVoidPtr = coerce
  mkSPMDUnit = CUDAIO (SPMDUnit 0)
  coerceExp = coerce

instance ExpPair CUDADejaFu where
  mkPair = liftA2 SIMDPair
  fstPair = fmap simdPairFst
  sndPair = fmap simdPairSnd
  mkVoidPtr = coerce
  unVoidPtr = coerce
  mkSPMDUnit = CUDADejaFu (SPMDUnit 0)
  coerceExp = coerce

--------------------------------------------------------------------------------
-- 3. Layout Classification & Underlying Representations
--------------------------------------------------------------------------------

data Layout
  = LDirect
  | LSplit Nat
  | LPad4
  | LInt64 Layout
  | LNewtype K.Type
  | LComplex K.Type
  | LBool
  | LPtr K.Type
  | LVoidPtr
  | LUnit

type family SPMDRawLayout (n :: Nat) (a :: K.Type) :: Layout where
  SPMDRawLayout 2 Int8 = 'LPad4
  SPMDRawLayout 2 Word8 = 'LPad4
  SPMDRawLayout n Int8 = 'LDirect
  SPMDRawLayout n Word8 = 'LDirect
  SPMDRawLayout 32 Int16 = 'LSplit 16
  SPMDRawLayout 32 Word16 = 'LSplit 16
  SPMDRawLayout n Int16 = 'LDirect
  SPMDRawLayout n Word16 = 'LDirect
  SPMDRawLayout 16 Int32 = 'LSplit 8
  SPMDRawLayout 16 Word32 = 'LSplit 8
  SPMDRawLayout 16 Float = 'LSplit 8
  SPMDRawLayout 32 Int32 = 'LSplit 16
  SPMDRawLayout 32 Word32 = 'LSplit 16
  SPMDRawLayout 32 Float = 'LSplit 16
  SPMDRawLayout n Int32 = 'LDirect
  SPMDRawLayout n Word32 = 'LDirect
  SPMDRawLayout n Float = 'LDirect
  SPMDRawLayout 8 Int64 = 'LSplit 4
  SPMDRawLayout 8 Word64 = 'LSplit 4
  SPMDRawLayout 8 Double = 'LSplit 4
  SPMDRawLayout 16 Int64 = 'LSplit 8
  SPMDRawLayout 16 Word64 = 'LSplit 8
  SPMDRawLayout 16 Double = 'LSplit 8
  SPMDRawLayout 32 Int64 = 'LSplit 16
  SPMDRawLayout 32 Word64 = 'LSplit 16
  SPMDRawLayout 32 Double = 'LSplit 16
  SPMDRawLayout n Int64 = 'LDirect
  SPMDRawLayout n Word64 = 'LDirect
  SPMDRawLayout n Double = 'LDirect

type family SPMDLayout (n :: Nat) (a :: K.Type) :: Layout where
  SPMDLayout _ () = 'LUnit
  SPMDLayout _ Bool = 'LBool
  SPMDLayout _ (Ptr ()) = 'LVoidPtr
  SPMDLayout _ (Ptr b) = 'LPtr b
  SPMDLayout _ (Complex b) = 'LComplex b
  SPMDLayout _ CBool = 'LNewtype Word8
  SPMDLayout _ CChar = 'LNewtype Int8
  SPMDLayout _ CSChar = 'LNewtype Int8
  SPMDLayout _ CUChar = 'LNewtype Word8
  SPMDLayout _ CShort = 'LNewtype Int16
  SPMDLayout _ CUShort = 'LNewtype Word16
  SPMDLayout _ CInt = 'LNewtype Int32
  SPMDLayout _ CUInt = 'LNewtype Word32
  SPMDLayout _ CLong = 'LNewtype Int64
  SPMDLayout _ CLLong = 'LNewtype Int64
  SPMDLayout _ CULong = 'LNewtype Word64
  SPMDLayout _ CULLong = 'LNewtype Word64
  SPMDLayout _ CSize = 'LNewtype Word64
  SPMDLayout _ CFloat = 'LNewtype Float
  SPMDLayout _ CDouble = 'LNewtype Double
  SPMDLayout n Int64 = 'LInt64 (SPMDRawLayout n Int64)
  SPMDLayout n a = SPMDRawLayout n a

type family PtrTree (n :: Nat) (p :: K.Type) :: K.Type where
  PtrTree 2 p = SIMDPair p p
  PtrTree 4 p = SIMDPair (PtrTree 2 p) (PtrTree 2 p)
  PtrTree 8 p = SIMDPair (PtrTree 4 p) (PtrTree 4 p)
  PtrTree 16 p = SIMDPair (PtrTree 8 p) (PtrTree 8 p)
  PtrTree 32 p = SIMDPair (PtrTree 16 p) (PtrTree 16 p)

type family SPMDRepByLayout (l :: Layout) (n :: Nat) (e :: K.Type -> K.Type) (a :: K.Type) :: K.Type where
  SPMDRepByLayout 'LDirect n e a = SIMDVector e n a
  SPMDRepByLayout ('LSplit h) _ e a =
    SIMDPair
      (SPMDRepByLayout (SPMDRawLayout h a) h e a)
      (SPMDRepByLayout (SPMDRawLayout h a) h e a)
  SPMDRepByLayout 'LPad4 2 e a = SIMDVector e 4 a
  SPMDRepByLayout ('LInt64 rl) n e _ = SPMDRepByLayout rl n e Int64
  SPMDRepByLayout ('LNewtype b) n e _ = SPMDRep n e b
  SPMDRepByLayout ('LComplex b) n e _ = SIMDPair (SPMDRep n e b) (SPMDRep n e b)
  SPMDRepByLayout 'LBool n e _ = SIMDMask e n (SPMDMaskElem n)
  SPMDRepByLayout ('LPtr b) n _ _ = PtrTree n (Ptr b)
  SPMDRepByLayout 'LVoidPtr n _ _ = PtrTree n VoidPtr
  SPMDRepByLayout 'LUnit _ _ _ = SPMDUnit

type SPMDRep n e a = SPMDRepByLayout (SPMDLayout n a) n e a

-- | One-pass affine & alignment provenance for @Int64@ indices.
data SPMDInt64Expr (n :: Nat) (e :: K.Type -> K.Type)
  = I64Const !Int64
  | I64Uniform !Bool !(e Int64)
  | I64UnitStride !Bool !(e Int64)
  | I64Varying !(e (SPMDRep n e Int64))

-- | One-pass uniformity & SIMD-alignment provenance for typed pointers.
data SPMDPtrExpr (n :: Nat) (e :: K.Type -> K.Type) a
  = PtrUniform !Bool !(e (Ptr a))
  | PtrUnitStride !Bool !(e (Ptr a))
  | PtrVarying !(e (PtrTree n (Ptr a)))

-- | One-pass uniformity & SIMD-alignment provenance for @Ptr ()@.
data SPMDVoidPtrExpr (n :: Nat) (e :: K.Type -> K.Type)
  = VoidPtrUniform !Bool !(e (Ptr ()))
  | VoidPtrVarying !(e (PtrTree n VoidPtr))

type family SPMDExprByLayout (l :: Layout) (n :: Nat) (e :: K.Type -> K.Type) (a :: K.Type) :: K.Type where
  SPMDExprByLayout 'LDirect n e a = e (SPMDRepByLayout 'LDirect n e a)
  SPMDExprByLayout ('LSplit h) n e a = e (SPMDRepByLayout ('LSplit h) n e a)
  SPMDExprByLayout 'LPad4 n e a = e (SPMDRepByLayout 'LPad4 n e a)
  SPMDExprByLayout ('LComplex b) n e a = e (SPMDRepByLayout ('LComplex b) n e a)
  SPMDExprByLayout 'LBool n e a = e (SPMDRepByLayout 'LBool n e a)
  SPMDExprByLayout 'LUnit n e a = e (SPMDRepByLayout 'LUnit n e a)
  SPMDExprByLayout ('LInt64 _) n e _ = SPMDInt64Expr n e
  SPMDExprByLayout ('LPtr b) n e _ = SPMDPtrExpr n e b
  SPMDExprByLayout 'LVoidPtr n e _ = SPMDVoidPtrExpr n e
  SPMDExprByLayout ('LNewtype b) n e _ = SPMDExpr n e b

type SPMDExpr n e a = SPMDExprByLayout (SPMDLayout n a) n e a

--------------------------------------------------------------------------------
-- 4. Core SPMD, SPMDM, & SPMDRef Newtypes
--------------------------------------------------------------------------------

newtype SPMD (n :: Nat) (e :: K.Type -> K.Type) a = SPMD
  { getSPMD :: Reader (SPMDMask n e) (SPMDExpr n e a)
  }

newtype SPMDM (n :: Nat) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = SPMDM
  { getSPMDM :: ReaderT (SPMDMask n e) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadReader (SPMDMask n e),
      MonadIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask
    )

instance (Monad m, CmdSync m) => CmdSync (SPMDM n m e) where
  syncWarp = SPMDM (lift syncWarp)
  syncBlock = SPMDM (lift syncBlock)

newtype SPMDRef (n :: Nat) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = SPMDRef
  { getSPMDRef :: Ref m e (SPMDRep n e a)
  }

liftSPMD :: (Monad m) => m a -> SPMDM n m e a
liftSPMD = SPMDM . lift

evalSPMD :: (Monad m) => SPMD n e a -> SPMDM n m e (SPMDExpr n e a)
evalSPMD (SPMD r) = asks (runReader r)

runSPMD :: SPMD n e a -> SPMDExpr n e a
runSPMD (SPMD r) = runReader r SPMDMaskAllTrue

runSPMDWithMask :: e (SIMDMask e n (SPMDMaskElem n)) -> SPMD n e a -> SPMDExpr n e a
runSPMDWithMask m (SPMD r) = runReader r (SPMDMask m)

runSPMDM :: SPMDM n m e a -> m a
runSPMDM (SPMDM m) = runReaderT m SPMDMaskAllTrue

runSPMDMWithMask :: e (SIMDMask e n (SPMDMaskElem n)) -> SPMDM n m e a -> m a
runSPMDMWithMask msk (SPMDM m) = runReaderT m (SPMDMask msk)

withSPMD :: Proxy n -> SPMDM n m e a -> m a
withSPMD _ = runSPMDM

withSPMDMasked :: SPMD n e Bool -> SPMDM n m e a -> m a
withSPMDMasked (SPMD mb) (SPMDM act) = runReaderT act (SPMDMask (runReader mb SPMDMaskAllTrue))

askSIMDMask ::
  (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMDM n m e (e (SIMDMask e n (SPMDMaskElem n)))
askSIMDMask = asks toSIMDMask

askMaskBool ::
  (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMDM n m e (SPMD n e Bool)
askMaskBool = SPMD . pure <$> askSIMDMask

withMask ::
  (Monad m, ExpSIMDMask e n (SPMDMaskElem n)) =>
  SPMD n e Bool ->
  SPMDM n m e a ->
  SPMDM n m e a
withMask b act = do
  c <- evalSPMD b
  local (`spmdMaskAnd` c) act

unmasked :: (Monad m) => SPMDM n m e a -> SPMDM n m e a
unmasked = local (const SPMDMaskAllTrue)

--------------------------------------------------------------------------------
-- 5. Mask Conversion, Mask Splitting, & Per-Lane Bit Helpers
--------------------------------------------------------------------------------

extractLaneAt :: (ExpSIMDVec e n a) => Int -> e (SIMDVector e n a) -> e a
extractLaneAt i v = case someNatVal (fromIntegral i) of
  SomeNat p -> vextractLane p v

maskToCanonical ::
  forall n e a me.
  ( me ~ SPMDMaskElem n,
    ExpSIMDCmp e n a,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n me,
    ExpSIMDVec e n me,
    ExpSIMDCast e,
    CPUSIMDElem a,
    CPUSIMDElem me,
    Num (e a),
    Num (e me)
  ) =>
  e (SIMDMask e n a) ->
  e (SIMDMask e n me)
maskToCanonical m =
  case eqT @a @me of
    Just Refl -> m
    Nothing ->
      let onesA = vselect m (vsplat 1) vzero
          asMe
            | simdBytes (Proxy @n) (Proxy @a) == simdBytes (Proxy @n) (Proxy @me) =
                vbitcast @e @n @a @n @me onesA
            | otherwise = vcvt @e @n @a @me onesA
       in asMe `vne` vzero

maskFromCanonical ::
  forall n e a me.
  ( me ~ SPMDMaskElem n,
    ExpSIMDCmp e n a,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n me,
    ExpSIMDVec e n me,
    ExpSIMDCast e,
    CPUSIMDElem a,
    CPUSIMDElem me,
    Num (e a),
    Num (e me)
  ) =>
  e (SIMDMask e n me) ->
  e (SIMDMask e n a)
maskFromCanonical m =
  case eqT @a @me of
    Just Refl -> m
    Nothing ->
      let onesMe = vselect m (vsplat 1) vzero
          asA
            | simdBytes (Proxy @n) (Proxy @a) == simdBytes (Proxy @n) (Proxy @me) =
                vbitcast @e @n @me @n @a onesMe
            | otherwise = vcvt @e @n @me @a onesMe
       in asA `vne` vzero

pad4MaskFrom2 ::
  forall e a.
  ( SIMDShape 4 a,
    CPUSIMDElem a,
    ExpSIMDVec e 4 a,
    ExpSIMDCmp e 4 a,
    ExpSIMDVec e 4 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDMask e 2 Int64,
    ExpSIMDCast e,
    Num (e Int64),
    Num (e a)
  ) =>
  e (SIMDMask e 2 Int64) ->
  e (SIMDMask e 4 a)
pad4MaskFrom2 m2 =
  let ones2 = vselect m2 (vsplat 1) (vzero @e @2 @Int64)
      l0 = extractLaneAt 0 ones2
      l1 = extractLaneAt 1 ones2
      ones4 = vfromLanes [l0, l1, 0, 0] :: e (SIMDVector e 4 Int64)
   in vcvt @e @4 @Int64 @a ones4 `vne` vzero

pad4MaskTo2 ::
  forall e a.
  ( SIMDShape 4 a,
    CPUSIMDElem a,
    ExpSIMDVec e 4 a,
    ExpSIMDMask e 4 a,
    ExpSIMDVec e 4 Int64,
    ExpSIMDCmp e 2 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDSplit e 4 2 Int64,
    ExpSIMDCast e,
    Num (e Int64),
    Num (e a)
  ) =>
  e (SIMDMask e 4 a) ->
  e (SIMDMask e 2 Int64)
pad4MaskTo2 m4 =
  let ones4 = vcvt @e @4 @a @Int64 (vselect m4 (vsplat 1) vzero)
      (lo2, _) = vsplit @e @4 @2 ones4
   in lo2 `vne` vzero

class
  ( ExpSIMDMask e n (SPMDMaskElem n),
    ExpSIMDMask e h (SPMDMaskElem h)
  ) =>
  SPMDMaskSplit (n :: Nat) (h :: Nat) e
  where
  splitSPMDMask ::
    e (SIMDMask e n (SPMDMaskElem n)) ->
    (e (SIMDMask e h (SPMDMaskElem h)), e (SIMDMask e h (SPMDMaskElem h)))
  concatSPMDMask ::
    e (SIMDMask e h (SPMDMaskElem h)) ->
    e (SIMDMask e h (SPMDMaskElem h)) ->
    e (SIMDMask e n (SPMDMaskElem n))

splitExecMask ::
  forall n h e.
  (SPMDMaskSplit n h e) =>
  SPMDMask n e ->
  (SPMDMask h e, SPMDMask h e)
splitExecMask SPMDMaskAllTrue = (SPMDMaskAllTrue, SPMDMaskAllTrue)
splitExecMask (SPMDMask m) =
  let (lo, hi) = splitSPMDMask @n @h m
   in (SPMDMask lo, SPMDMask hi)

instance
  ( ExpSIMDMask e 4 Int32,
    ExpSIMDMask e 2 Int64,
    ExpSIMDCmp e 4 Int32,
    ExpSIMDVec e 4 Int32,
    ExpSIMDCmp e 2 Int32,
    ExpSIMDVec e 2 Int32,
    ExpSIMDCmp e 2 Int64,
    ExpSIMDVec e 2 Int64,
    ExpSIMDSplit e 4 2 Int32,
    ExpSIMDCast e,
    Num (e Int32),
    Num (e Int64)
  ) =>
  SPMDMaskSplit 4 2 e
  where
  splitSPMDMask m4 =
    let (lo2, hi2) = vsplit @e @4 @2 (vselect m4 (vsplat 1) vzero)
     in (vcvt @e @2 @Int32 @Int64 lo2 `vne` vzero, vcvt @e @2 @Int32 @Int64 hi2 `vne` vzero)
  concatSPMDMask mLo mHi =
    let lo2 = vcvt @e @2 @Int64 @Int32 (vselect mLo (vsplat 1) vzero)
        hi2 = vcvt @e @2 @Int64 @Int32 (vselect mHi (vsplat 1) vzero)
     in vconcat @e @4 @2 lo2 hi2 `vne` vzero

instance
  ( ExpSIMDMask e 8 Int32,
    ExpSIMDMask e 4 Int32,
    ExpSIMDCmp e 8 Int32,
    ExpSIMDVec e 8 Int32,
    ExpSIMDCmp e 4 Int32,
    ExpSIMDVec e 4 Int32,
    ExpSIMDSplit e 8 4 Int32,
    Num (e Int32)
  ) =>
  SPMDMaskSplit 8 4 e
  where
  splitSPMDMask m8 =
    let (lo4, hi4) = vsplit @e @8 @4 (vselect m8 (vsplat 1) vzero)
     in (lo4 `vne` vzero, hi4 `vne` vzero)
  concatSPMDMask mLo mHi =
    vconcat @e @8 @4 (vselect mLo (vsplat 1) vzero) (vselect mHi (vsplat 1) vzero) `vne` vzero

instance
  ( ExpSIMDMask e 16 Int16,
    ExpSIMDMask e 8 Int32,
    ExpSIMDCmp e 16 Int16,
    ExpSIMDVec e 16 Int16,
    ExpSIMDCmp e 8 Int16,
    ExpSIMDVec e 8 Int16,
    ExpSIMDCmp e 8 Int32,
    ExpSIMDVec e 8 Int32,
    ExpSIMDSplit e 16 8 Int16,
    ExpSIMDCast e,
    Num (e Int16),
    Num (e Int32)
  ) =>
  SPMDMaskSplit 16 8 e
  where
  splitSPMDMask m16 =
    let (lo8, hi8) = vsplit @e @16 @8 (vselect m16 (vsplat 1) vzero)
     in (vcvt @e @8 @Int16 @Int32 lo8 `vne` vzero, vcvt @e @8 @Int16 @Int32 hi8 `vne` vzero)
  concatSPMDMask mLo mHi =
    let lo8 = vcvt @e @8 @Int32 @Int16 (vselect mLo (vsplat 1) vzero)
        hi8 = vcvt @e @8 @Int32 @Int16 (vselect mHi (vsplat 1) vzero)
     in vconcat @e @16 @8 lo8 hi8 `vne` vzero

instance
  ( ExpSIMDMask e 32 Word8,
    ExpSIMDMask e 16 Int16,
    ExpSIMDCmp e 32 Word8,
    ExpSIMDVec e 32 Word8,
    ExpSIMDCmp e 16 Word8,
    ExpSIMDVec e 16 Word8,
    ExpSIMDCmp e 16 Int16,
    ExpSIMDVec e 16 Int16,
    ExpSIMDSplit e 32 16 Word8,
    ExpSIMDCast e,
    Num (e Word8),
    Num (e Int16)
  ) =>
  SPMDMaskSplit 32 16 e
  where
  splitSPMDMask m32 =
    let (lo16, hi16) = vsplit @e @32 @16 (vselect m32 (vsplat 1) vzero)
     in (vcvt @e @16 @Word8 @Int16 lo16 `vne` vzero, vcvt @e @16 @Word8 @Int16 hi16 `vne` vzero)
  concatSPMDMask mLo mHi =
    let lo16 = vcvt @e @16 @Int16 @Word8 (vselect mLo (vsplat 1) vzero)
        hi16 = vcvt @e @16 @Int16 @Word8 (vselect mHi (vsplat 1) vzero)
     in vconcat @e @32 @16 lo16 hi16 `vne` vzero

laneActivePreds ::
  forall n e.
  ( KnownNat n,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32)
  ) =>
  e (SIMDMask e n (SPMDMaskElem n)) ->
  [e Bool]
laneActivePreds m =
  let lanes = fromIntegral (natVal (Proxy @n)) :: Int
      bits = mmovemask m
   in [((bits `iand` fromIntegral (bit i :: Word32)) `ne` 0) | i <- [0 .. lanes - 1]]

laneActivePredsM ::
  forall n m e.
  ( KnownNat n,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32
  ) =>
  SPMDMask n e ->
  m [e Bool]
laneActivePredsM SPMDMaskAllTrue =
  pure $ replicate (fromIntegral (natVal (Proxy @n))) true
laneActivePredsM (SPMDMask m) = do
  let lanes = fromIntegral (natVal (Proxy @n)) :: Int
  bits <- letM (mmovemask m)
  pure [((bits `iand` fromIntegral (bit i :: Word32)) `ne` 0) | i <- [0 .. lanes - 1]]

--------------------------------------------------------------------------------
-- 6. Pointer Tree Helpers (PtrTree)
--------------------------------------------------------------------------------

class
  (KnownNat n, ExpPair e, JanusTyped e p, JanusTyped e (PtrTree n p)) =>
  PtrTreeOps (n :: Nat) e p
  where
  ptrTreeReplicate :: e p -> e (PtrTree n p)
  ptrTreeFromList :: [e p] -> e (PtrTree n p)
  ptrTreeToList :: e (PtrTree n p) -> [e p]

instance (ExpPair e, JanusTyped e p, JanusTyped e (SIMDPair p p)) => PtrTreeOps 2 e p where
  ptrTreeReplicate x = mkPair x x
  ptrTreeFromList [a, b] = mkPair a b
  ptrTreeFromList xs = error $ "ptrTreeFromList @2: expected 2 elements, got " <> show (length xs)
  ptrTreeToList t = [fstPair t, sndPair t]

instance
  (ExpPair e, JanusTyped e p, PtrTreeOps 2 e p, JanusTyped e (SIMDPair (PtrTree 2 p) (PtrTree 2 p))) =>
  PtrTreeOps 4 e p
  where
  ptrTreeReplicate x = let h = ptrTreeReplicate @2 x in mkPair h h
  ptrTreeFromList xs
    | length xs == 4 = mkPair (ptrTreeFromList @2 (take 2 xs)) (ptrTreeFromList @2 (drop 2 xs))
    | otherwise = error $ "ptrTreeFromList @4: expected 4 elements, got " <> show (length xs)
  ptrTreeToList t = ptrTreeToList @2 (fstPair t) <> ptrTreeToList @2 (sndPair t)

instance
  (ExpPair e, JanusTyped e p, PtrTreeOps 4 e p, JanusTyped e (SIMDPair (PtrTree 4 p) (PtrTree 4 p))) =>
  PtrTreeOps 8 e p
  where
  ptrTreeReplicate x = let h = ptrTreeReplicate @4 x in mkPair h h
  ptrTreeFromList xs
    | length xs == 8 = mkPair (ptrTreeFromList @4 (take 4 xs)) (ptrTreeFromList @4 (drop 4 xs))
    | otherwise = error $ "ptrTreeFromList @8: expected 8 elements, got " <> show (length xs)
  ptrTreeToList t = ptrTreeToList @4 (fstPair t) <> ptrTreeToList @4 (sndPair t)

instance
  (ExpPair e, JanusTyped e p, PtrTreeOps 8 e p, JanusTyped e (SIMDPair (PtrTree 8 p) (PtrTree 8 p))) =>
  PtrTreeOps 16 e p
  where
  ptrTreeReplicate x = let h = ptrTreeReplicate @8 x in mkPair h h
  ptrTreeFromList xs
    | length xs == 16 = mkPair (ptrTreeFromList @8 (take 8 xs)) (ptrTreeFromList @8 (drop 8 xs))
    | otherwise = error $ "ptrTreeFromList @16: expected 16 elements, got " <> show (length xs)
  ptrTreeToList t = ptrTreeToList @8 (fstPair t) <> ptrTreeToList @8 (sndPair t)

instance
  (ExpPair e, JanusTyped e p, PtrTreeOps 16 e p, JanusTyped e (SIMDPair (PtrTree 16 p) (PtrTree 16 p))) =>
  PtrTreeOps 32 e p
  where
  ptrTreeReplicate x = let h = ptrTreeReplicate @16 x in mkPair h h
  ptrTreeFromList xs
    | length xs == 32 = mkPair (ptrTreeFromList @16 (take 16 xs)) (ptrTreeFromList @16 (drop 16 xs))
    | otherwise = error $ "ptrTreeFromList @32: expected 32 elements, got " <> show (length xs)
  ptrTreeToList t = ptrTreeToList @16 (fstPair t) <> ptrTreeToList @16 (sndPair t)
