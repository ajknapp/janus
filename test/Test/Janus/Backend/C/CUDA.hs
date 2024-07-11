{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Test.Janus.Backend.C.CUDA where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Int
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Janus.Backend.C.CUDA
import Janus.Backend.C.CUDA.Foreign
import qualified Janus.Command.Array as A
import Janus.Command.Cond
import Janus.Command.Ref
import Janus.Expression.Cast
import Janus.Expression.Ord
import Janus.Typed
import Test.Tasty.HUnit

saxpy ::
  ( Monad m,
    JanusTyped e Int32,
    CmdCond m e,
    CmdRef m e,
    A.CmdStorable m e a,
    ExpOrd e Int32,
    ExpIntegralCast e Int32 Int64,
    Num (e Int32),
    Num (e a)
  ) =>
  e Int32 -> e a -> e (Ptr a) -> e (Ptr a) -> CUDAT e m ()
saxpy n a x y = do
  blockDim <- asks _cudaThreadInfoBlockDimX
  blockIdx <- asks _cudaThreadInfoBlockIdxX
  threadIdx <- asks _cudaThreadInfoThreadIdxX
  lift $ do
    i <- letM $ blockDim * blockIdx + threadIdx
    whenM_ (i `lt` n) $ do
      xi <- A.peekElemOff x (toIntegral i)
      yi <- A.peekElemOff y (toIntegral i)
      A.pokeElemOff y (a * xi + yi) (toIntegral i)

unit_saxpy :: IO ()
unit_saxpy = bracket malloc free $ \pctx ->
  withCudaDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \ctx -> do
    let n = 1024
        nbytes = n * Foreign.Storable.sizeOf (undefined :: Float)
        fill p i
          | i >= n = pure ()
          | otherwise = pokeElemOff p i (fromIntegral i :: Float) >> fill p (i + 1)
    bracket (mallocBytes nbytes) free $ \px -> bracket (mallocBytes nbytes) free $ \py -> do
      fill px 0 >> fill py 0
      withCudaDeviceArray nbytes $ \d_x ->
        withCudaDeviceArray nbytes $ \d_y -> do
          cuMemcpyHtoD d_x (castPtr px) (fromIntegral nbytes)
          cuMemcpyHtoD d_y (castPtr py) (fromIntegral nbytes)
          withJanusCUDAFn dev ctx (\m a x y -> runCUDAT_ $ saxpy @_ @_ @Float m a x y) $ \fn k -> do
            (minGrid, _) <- cuOccupancyMaxPotentialBlockSize fn nullFunPtr 0 0
            let kernelLaunch =
                  CUlaunchConfig
                    { _cuLaunchConfigBlockDimX = fromIntegral $ n `div` fromIntegral minGrid + 1,
                      _cuLaunchConfigBlockDimY = 1,
                      _cuLaunchConfigBlockDimZ = 1,
                      _cuLaunchConfigGridDimX = fromIntegral minGrid,
                      _cuLaunchConfigGridDimY = 1,
                      _cuLaunchConfigGridDimZ = 1,
                      _cuLaunchConfigSharedMemBytes = 0,
                      _cuLaunchConfigStream = CUstream Foreign.Ptr.nullPtr
                    }
            k kernelLaunch (fromIntegral n) 1.0 d_x d_y
            cuMemcpyDtoH (castPtr px) d_x (fromIntegral nbytes)
            cuMemcpyDtoH (castPtr py) d_y (fromIntegral nbytes)
            forM_ [0 .. n - 1 :: Int] $ \i -> do
              peekElemOff py i >>= (@?= (fromIntegral $ 2*i))
