{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.CUDA.Types where

import Control.Exception (bracket)
import Control.Lens (makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT (..), lift)
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef)
import Data.Int (Int32)
import Data.Loc (noLoc)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import Janus.Backend.C.Raw
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.Target
import Janus.Command.Ref (JanusCRef)
import Janus.Expression.Bool (ExpBool)
import Janus.Typed
import Language.C.Quote

cudaArgMaxSize :: Int
cudaArgMaxSize = 4096

cudaWarpSize :: Int
cudaWarpSize = 32

withCuDeviceArray :: Int -> (CUdeviceptr -> IO a) -> IO a
withCuDeviceArray nbytes k = alloca $ \dp ->
  bracket (cuMemAlloc dp (fromIntegral nbytes) >> peek dp) cuMemFree k

--------------------------------------------------------------------------------

data CUDAThreadInfo e = CUDAThreadInfo
  { _cudaThreadInfoBlockIdxX :: e Int32,
    _cudaThreadInfoBlockIdxY :: e Int32,
    _cudaThreadInfoBlockIdxZ :: e Int32,
    _cudaThreadInfoBlockDimX :: e Int32,
    _cudaThreadInfoBlockDimY :: e Int32,
    _cudaThreadInfoBlockDimZ :: e Int32,
    _cudaThreadInfoThreadIdxX :: e Int32,
    _cudaThreadInfoThreadIdxY :: e Int32,
    _cudaThreadInfoThreadIdxZ :: e Int32,
    _cudaThreadInfoGridDimX :: e Int32,
    _cudaThreadInfoGridDimY :: e Int32,
    _cudaThreadInfoGridDimZ :: e Int32
  }
  deriving (Generic)

deriving instance (Show (e Int32)) => Show (CUDAThreadInfo e)

$(makeLenses ''CUDAThreadInfo)

mapCUDAThreadInfo :: (e Int32 -> f Int32) -> CUDAThreadInfo e -> CUDAThreadInfo f
mapCUDAThreadInfo f CUDAThreadInfo {..} =
  CUDAThreadInfo
    { _cudaThreadInfoBlockIdxX = f _cudaThreadInfoBlockIdxX,
      _cudaThreadInfoBlockIdxY = f _cudaThreadInfoBlockIdxY,
      _cudaThreadInfoBlockIdxZ = f _cudaThreadInfoBlockIdxZ,
      _cudaThreadInfoBlockDimX = f _cudaThreadInfoBlockDimX,
      _cudaThreadInfoBlockDimY = f _cudaThreadInfoBlockDimY,
      _cudaThreadInfoBlockDimZ = f _cudaThreadInfoBlockDimZ,
      _cudaThreadInfoThreadIdxX = f _cudaThreadInfoThreadIdxX,
      _cudaThreadInfoThreadIdxY = f _cudaThreadInfoThreadIdxY,
      _cudaThreadInfoThreadIdxZ = f _cudaThreadInfoThreadIdxZ,
      _cudaThreadInfoGridDimX = f _cudaThreadInfoGridDimX,
      _cudaThreadInfoGridDimY = f _cudaThreadInfoGridDimY,
      _cudaThreadInfoGridDimZ = f _cudaThreadInfoGridDimZ
    }

newtype JanusCUDA a = JanusCUDA {getJanusCUDA :: JanusC a}

newtype JanusCUDAM a = JanusCUDAM {getJanusCUDAM :: ReaderT (CUDAThreadInfo JanusCUDA) JanusCM a}
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadReader (CUDAThreadInfo JanusCUDA), MonadTargetFeatures)

runJanusCUDAM :: JanusCUDAM a -> JanusCM a
runJanusCUDAM (JanusCUDAM m) = runReaderT m magicThreadInfo
  where
    magicCUDAVar var mem = JanusCUDA $ JanusC $ pure $ RVal (Member (Var (Id var noLoc) noLoc) (Id mem noLoc) noLoc)
    magicThreadInfo =
      CUDAThreadInfo
        { _cudaThreadInfoBlockDimX = magicCUDAVar "blockDim" "x",
          _cudaThreadInfoBlockDimY = magicCUDAVar "blockDim" "y",
          _cudaThreadInfoBlockDimZ = magicCUDAVar "blockDim" "z",
          _cudaThreadInfoBlockIdxX = magicCUDAVar "blockIdx" "x",
          _cudaThreadInfoBlockIdxY = magicCUDAVar "blockIdx" "y",
          _cudaThreadInfoBlockIdxZ = magicCUDAVar "blockIdx" "z",
          _cudaThreadInfoThreadIdxX = magicCUDAVar "threadIdx" "x",
          _cudaThreadInfoThreadIdxY = magicCUDAVar "threadIdx" "y",
          _cudaThreadInfoThreadIdxZ = magicCUDAVar "threadIdx" "z",
          _cudaThreadInfoGridDimX = magicCUDAVar "gridDim" "x",
          _cudaThreadInfoGridDimY = magicCUDAVar "gridDim" "y",
          _cudaThreadInfoGridDimZ = magicCUDAVar "gridDim" "z"
        }

newtype JanusCUDARef a = JanusCUDARef {getJanusCUDARef :: JanusCRef a}

mkCUDAFuncInfo :: CUDAFeatures -> String -> JCFuncInfo
mkCUDAFuncInfo fs name = setTargetFeature cudaFeatureKey fs (mkJCFuncInfo name JCUDA False)

askCUDAFeatures :: (MonadTargetFeatures m) => m CUDAFeatures
askCUDAFeatures = askTargetFeature cudaFeatureKey

class CmdSync m where
  syncWarp :: m ()
  syncBlock :: m ()

instance CmdSync JanusCUDAM where
  syncWarp = JanusCUDAM $ lift $ janusCFFICall Nothing "__syncwarp"
  syncBlock = JanusCUDAM $ lift $ janusCFFICall Nothing "__syncthreads"

--------------------------------------------------------------------------------
-- Concrete IO and DejaFu CPU CUDA Expression & Ref Types

newtype CUDAIO a = CUDAIO {getCUDAIO :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (Applicative, Monad, ExpBool) via Identity

newtype CUDAIORef a = CUDAIORef {getCUDAIORef :: IORef a}

newtype CUDADejaFu a = CUDADejaFu {getCUDADejaFu :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (Applicative, Monad, ExpBool) via Identity

newtype CUDADejaFuRef a = CUDADejaFuRef {getCUDADejaFuRef :: IORef a}

type instance JanusTyped JanusCUDA = JanusCTyped

type instance JanusTyped CUDAIO = IdConstraint

type instance JanusTyped CUDADejaFu = IdConstraint

instance (JanusCTyped a) => KnownJanusType JanusCUDA a

instance KnownJanusType CUDAIO a

instance KnownJanusType CUDADejaFu a
