{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.CPUSIMD where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce (coerce)
import Data.Int (Int32, Int8)
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Word (Word32, Word8)
import Foreign.Storable (Storable)
import GHC.TypeLits (natVal)
import Janus.Backend.C.CUDA.CPU ()
import Janus.Backend.C.CUDA.Runtime
import Janus.Backend.C.CUDA.Types
import Janus.Expression.Math (FMA, Hypot)
import Janus.Expression.SIMD
import Janus.Expression.SIMD.CPU

--------------------------------------------------------------------------------
-- CPU-Emulated SIMD Instances for CUDAIO & CUDADejaFu
--------------------------------------------------------------------------------

instance ExpSIMD CUDAIO where
  newtype SIMDVector CUDAIO n a = CUDAIOSIMDVector {getCUDAIOSIMDVector :: CPUSIMD n a}
    deriving stock (Eq, Ord, Show)
  newtype SIMDMask CUDAIO n a = CUDAIOSIMDMask {getCUDAIOSIMDMask :: CPUMask n a}
    deriving stock (Eq, Ord, Show)

deriving newtype instance (SIMDShape n a) => Storable (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a) => Num (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Fractional a) => Fractional (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a) => Floating (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, FMA a) => FMA (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a, Hypot a) => Hypot (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Join (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Meet (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Lower (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Upper (SIMDMask CUDAIO n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector CUDAIO n a) n a where
  simdFromList = CUDAIOSIMDVector . cpuFromLanes
  simdToList = unCPUSIMD . getCUDAIOSIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask CUDAIO n a) n a where
  simdMaskFromBools = CUDAIOSIMDMask . CPUMask
  simdMaskToBools = unCPUMask . getCUDAIOSIMDMask

instance (SIMDShape n a) => Join (CUDAIO (SIMDMask CUDAIO n a)) where (\/) = coerce (cpuMaskOr @n @a)
instance (SIMDShape n a) => Meet (CUDAIO (SIMDMask CUDAIO n a)) where (/\) = coerce (cpuMaskAnd @n @a)
instance (SIMDShape n a) => Lower (CUDAIO (SIMDMask CUDAIO n a)) where lowerBound = CUDAIO (CUDAIOSIMDMask cpuMaskFalse)
instance (SIMDShape n a) => Upper (CUDAIO (SIMDMask CUDAIO n a)) where upperBound = CUDAIO (CUDAIOSIMDMask cpuMaskTrue)

instance ExpSIMD CUDADejaFu where
  newtype SIMDVector CUDADejaFu n a = CUDADejaFuSIMDVector {getCUDADejaFuSIMDVector :: CPUSIMD n a}
    deriving stock (Eq, Ord, Show)
  newtype SIMDMask CUDADejaFu n a = CUDADejaFuSIMDMask {getCUDADejaFuSIMDMask :: CPUMask n a}
    deriving stock (Eq, Ord, Show)

deriving newtype instance (SIMDShape n a) => Storable (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a) => Num (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Fractional a) => Fractional (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a) => Floating (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, FMA a) => FMA (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a, Hypot a) => Hypot (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Join (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Meet (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Lower (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Upper (SIMDMask CUDADejaFu n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector CUDADejaFu n a) n a where
  simdFromList = CUDADejaFuSIMDVector . cpuFromLanes
  simdToList = unCPUSIMD . getCUDADejaFuSIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask CUDADejaFu n a) n a where
  simdMaskFromBools = CUDADejaFuSIMDMask . CPUMask
  simdMaskToBools = unCPUMask . getCUDADejaFuSIMDMask

instance (SIMDShape n a) => Join (CUDADejaFu (SIMDMask CUDADejaFu n a)) where (\/) = coerce (cpuMaskOr @n @a)
instance (SIMDShape n a) => Meet (CUDADejaFu (SIMDMask CUDADejaFu n a)) where (/\) = coerce (cpuMaskAnd @n @a)
instance (SIMDShape n a) => Lower (CUDADejaFu (SIMDMask CUDADejaFu n a)) where lowerBound = CUDADejaFu (CUDADejaFuSIMDMask cpuMaskFalse)
instance (SIMDShape n a) => Upper (CUDADejaFu (SIMDMask CUDADejaFu n a)) where upperBound = CUDADejaFu (CUDADejaFuSIMDMask cpuMaskTrue)

$( let mkCPUSIMDInstances expTy monadTy =
         [d|
           instance (SIMDShape n a, Num a) => ExpSIMDVec $expTy n a where
             vzero = coerce (cpuZero @n @a)
             vsplat = coerce (cpuSplat @n @a)
             viota = coerce (cpuIota @n @a)
             vfromLanes xs = coerce (cpuFromLanes @n @a (coerce xs))
             vextractLane p v =
               let i = fromIntegral (natVal p) :: Int
                in coerce (cpuExtractLane @n @a i) v
             vinsertLane p x v =
               let i = fromIntegral (natVal p) :: Int
                in coerce (cpuInsertLane @n @a i) x v

           instance (SIMDShape n a) => ExpSIMDMask $expTy n a where
             mfalse = coerce (cpuMaskFalse @n @a)
             mtrue = coerce (cpuMaskTrue @n @a)
             mand = coerce (cpuMaskAnd @n @a)
             mor = coerce (cpuMaskOr @n @a)
             mxor = coerce (cpuMaskXor @n @a)
             mandnot = coerce (cpuMaskAndNot @n @a)
             mnot = coerce (cpuMaskNot @n @a)
             vselect = coerce (cpuSelect @n @a)
             mmovemask = coerce (cpuMovemask @n @a)
             mpopcount = coerce (cpuMaskPopcount @n @a)
             mfirstTrue = coerce (cpuMaskFirstTrue @n @a)
             mallOf = coerce (cpuAllOf @n @a)
             manyOf = coerce (cpuAnyOf @n @a)
             mnoneOf = coerce (cpuNoneOf @n @a)

           instance (SIMDShape n a, Ord a) => ExpSIMDCmp $expTy n a where
             veq = coerce (cpuEq @n @a)
             vne = coerce (cpuNe @n @a)
             vlt = coerce (cpuLt @n @a)
             vle = coerce (cpuLe @n @a)
             vgt = coerce (cpuGt @n @a)
             vge = coerce (cpuGe @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith $expTy n a where
             vadd = coerce (cpuAdd @n @a)
             vsub = coerce (cpuSub @n @a)
             vmul = coerce (cpuMul @n @a)
             vneg = coerce (cpuNeg @n @a)
             vabs = coerce (cpuAbs @n @a)
             vmin = coerce (cpuMin @n @a)
             vmax = coerce (cpuMax @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat $expTy n a where
             vadds = coerce (cpuAdds @n @a)
             vsubs = coerce (cpuSubs @n @a)
             vabss = coerce (cpuAbss @n @a)
             vavg = coerce (cpuAvg @n @a)
             vabsdiff = coerce (cpuAbsDiff @n @a)

           instance (SIMDShape n a, CPUSIMDElem a, RealFloat a, FMA a) => ExpSIMDFloat $expTy n a where
             vdiv = coerce (cpuDiv @n @a)
             vsqrt = coerce (cpuSqrt @n @a)
             vrsqrtApprox = coerce (cpuRsqrtApprox @n @a)
             vrcpApprox = coerce (cpuRcpApprox @n @a)
             vfma = coerce (cpuFma @n @a)
             vfms = coerce (cpuFms @n @a)
             vfnma = coerce (cpuFnma @n @a)
             vfnms = coerce (cpuFnms @n @a)
             vmulAdd = coerce (cpuMulAdd @n @a)
             vfloor = coerce (cpuFloor @n @a)
             vceil = coerce (cpuCeil @n @a)
             vtrunc = coerce (cpuTrunc @n @a)
             vroundNearest = coerce (cpuRoundNearest @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits $expTy n a where
             vand = coerce (cpuBitAnd @n @a)
             vor = coerce (cpuBitOr @n @a)
             vxor = coerce (cpuBitXor @n @a)
             vandnot = coerce (cpuBitAndNot @n @a)
             vnot = coerce (cpuBitNot @n @a)
             vshl = coerce (cpuShl @n @a)
             vshr = coerce (cpuShr @n @a)
             vshlImm p v = vshl v (fromIntegral (natVal p))
             vshrImm p v = vshr v (fromIntegral (natVal p))
             vshlVar = coerce (cpuShlVar @n @a)
             vshrVar = coerce (cpuShrVar @n @a)
             vpopcnt = coerce (cpuPopcnt @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle $expTy n a where
             vshuffleLanes idxs = coerce (\v -> cpuPermuteLanes @n @a v (Left idxs))
             vpermuteVar = coerce (\v idx -> cpuPermuteLanes @n @a v (Right idx))
             vshuffleBytes ::
               forall b.
               (SIMDShape b Word8) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy b Word8) ->
               $expTy (SIMDVector $expTy n a)
             vshuffleBytes = coerce (cpuShuffleBytes @n @a @b)
             vzipLo = coerce (cpuZipLo @n @a)
             vzipHi = coerce (cpuZipHi @n @a)

           instance (SIMDHalfPair n2 n a) => ExpSIMDSplit $expTy n2 n a where
             vsplit = coerce (cpuSplit @n2 @n @a)
             vconcat = coerce (cpuConcat @n @n2 @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDReduce $expTy n a where
             vreduceAdd = coerce (cpuReduceAdd @n @a)
             vreduceMin = coerce (cpuReduceMin @n @a)
             vreduceMax = coerce (cpuReduceMax @n @a)
             vreduceAnd = coerce (cpuReduceAnd @n @a)
             vreduceOr = coerce (cpuReduceOr @n @a)
             vreduceXor = coerce (cpuReduceXor @n @a)

           instance ExpSIMDDot $expTy where
             vdp4aS = coerce cpuDp4aS
             vdp4aU = coerce cpuDp4aU
             vdp4aVecS ::
               forall m k.
               (SIMDDp4aShape m k) =>
               $expTy (SIMDVector $expTy m Int8) ->
               $expTy (SIMDVector $expTy m Int8) ->
               $expTy (SIMDVector $expTy k Int32) ->
               $expTy (SIMDVector $expTy k Int32)
             vdp4aVecS = coerce (cpuDp4aVecS @m @k)
             vdp4aVecU ::
               forall m k.
               (SIMDDp4aShape m k) =>
               $expTy (SIMDVector $expTy m Word8) ->
               $expTy (SIMDVector $expTy m Word8) ->
               $expTy (SIMDVector $expTy k Word32) ->
               $expTy (SIMDVector $expTy k Word32)
             vdp4aVecU = coerce (cpuDp4aVecU @m @k)
             vdp2aLoS = coerce (cpuDp2aS False)
             vdp2aHiS = coerce (cpuDp2aS True)
             vdp2aLoU = coerce (cpuDp2aU False)
             vdp2aHiU = coerce (cpuDp2aU True)
             vsad8 ::
               forall n.
               (SIMDShape n Word8) =>
               $expTy (SIMDVector $expTy n Word8) ->
               $expTy (SIMDVector $expTy n Word8) ->
               $expTy Word32
             vsad8 = coerce (cpuSadU8 @n)

           instance ExpSIMDCast $expTy where
             vbitcast ::
               forall n a m b.
               (SIMDShape n a, SIMDShape m b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy m b)
             vbitcast = coerce (cpuBitcast @n @a @m @b)
             vcvt ::
               forall n a b.
               (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy n b)
             vcvt = coerce (cpuCvt @n @a @b)
             vnarrowSat ::
               forall n m a b.
               (SIMDPackShape n m a b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy m b)
             vnarrowSat = coerce (cpuPackSat @n @m @a @b)

           instance (SIMDShape n a) => CmdSIMDMem $monadTy $expTy n a where
             vload p = liftIO (coerce (cpuLoad @IO @n @a) p)
             vloadAligned p = liftIO (coerce (cpuLoadAligned @IO @n @a) p)
             vstore p v = liftIO (coerce (cpuStore @IO @n @a) p v)
             vstoreAligned p v = liftIO (coerce (cpuStoreAligned @IO @n @a) p v)
             vloadMasked m fb p = liftIO (coerce (cpuLoadMasked @IO @n @a) m fb p)
             vstoreMasked m p v = liftIO (coerce (cpuStoreMasked @IO @n @a) m p v)
             vgather p idx = liftIO (coerce (cpuGather @IO @n @a) p idx)
             vgatherMasked m fb p idx = liftIO (coerce (cpuGatherMasked @IO @n @a) m fb p idx)
           |]
    in fmap concat $
         sequence
           [ mkCPUSIMDInstances [t|CUDAIO|] [t|CUDAIOM|],
             mkCPUSIMDInstances [t|CUDADejaFu|] [t|CUDADejaFuM|]
           ]
 )
