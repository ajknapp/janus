{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Test.Janus.Backend.C.CUDA where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Int
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Janus.Backend.C.CUDA
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.Build
import qualified Janus.Command.Memory as M
import Janus.Command.Cond
import Janus.Command.Ref
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Integral
import Janus.Expression.Ord
import Janus.Typed
import System.Environment
import Test.DejaFu.Schedule (roundRobinSchedNP)
import Test.DejaFu.Settings (defaultMemType)
import Test.Tasty.HUnit

saxpy ::
  ( Monad m,
    MonadReader (CUDAThreadInfo e) m,
    JanusTyped e Int32,
    CmdCond m e,
    CmdRef m e,
    M.CmdStorable m e a,
    ExpOrd e Int32,
    ExpIntegralCast e Int32 Int64,
    Num (e Int32),
    Num (e a)
  ) =>
  e Int32 -> e a -> e (Ptr a) -> e (Ptr a) -> m ()
saxpy n a x y = do
  blockDim <- asks _cudaThreadInfoBlockDimX
  blockIdx <- asks _cudaThreadInfoBlockIdxX
  threadIdx <- asks _cudaThreadInfoThreadIdxX
  i <- letM $ blockDim * blockIdx + threadIdx
  whenM_ (i `lt` n) $ do
    xi <- M.peekElemOff x (toIntegral i)
    yi <- M.peekElemOff y (toIntegral i)
    M.pokeElemOff y (a * xi + yi) (toIntegral i)

unit_saxpy :: IO ()
unit_saxpy = runInBoundThread $ try (cuInit 0) >>= \case
  Left (CUException CUDA_ERROR_STUB_LIBRARY) -> pure ()
  Left (CUException CUDA_ERROR_NO_DEVICE) -> pure ()
  Left e -> throwIO e
  Right _ -> alloca $ \pctx ->
    withCuDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \ctx -> do
      let n = 1024
          nbytes = n * Foreign.Storable.sizeOf (undefined :: Float)
          fill p i
            | i >= n = pure ()
            | otherwise = pokeElemOff p i (fromIntegral i :: Float) >> fill p (i + 1)
      allocaBytes nbytes $ \px -> allocaBytes nbytes $ \py -> do
        fill px 0 >> fill py 0
        withCuDeviceArray nbytes $ \d_x ->
          withCuDeviceArray nbytes $ \d_y -> do
            cuMemcpyHtoD d_x (castPtr px) (fromIntegral nbytes)
            cuMemcpyHtoD d_y (castPtr py) (fromIntegral nbytes)
            tmpdir <- getEnv "TMPDIR"
            let buildConfig = defaultCBuildConfig { _cBuildConfigCacheDir = tmpdir }
            withJanusCUDAFn dev ctx buildConfig (\m a x y -> saxpy @JanusCUDAMCSE @JanusCUDACSE @Float m a x y) $ \fn k -> do
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
              cuMemcpyDtoH (castPtr py) d_y (fromIntegral nbytes)
              forM_ [0 .. n - 1 :: Int] $ \i -> do
                peekElemOff py i >>= (@?= (fromIntegral $ 2*i))

cpuLaunchConfig1D :: Int -> Int -> CUlaunchConfig
cpuLaunchConfig1D gridX blockX =
  CUlaunchConfig
    { _cuLaunchConfigBlockDimX = fromIntegral blockX,
      _cuLaunchConfigBlockDimY = 1,
      _cuLaunchConfigBlockDimZ = 1,
      _cuLaunchConfigGridDimX = fromIntegral gridX,
      _cuLaunchConfigGridDimY = 1,
      _cuLaunchConfigGridDimZ = 1,
      _cuLaunchConfigSharedMemBytes = 0,
      _cuLaunchConfigStream = CUstream Foreign.Ptr.nullPtr
    }

unit_saxpy_cuda_io :: IO ()
unit_saxpy_cuda_io = do
  let n = 100
      nbytes = n * Foreign.Storable.sizeOf (undefined :: Float)
  allocaBytes nbytes $ \px -> allocaBytes nbytes $ \py -> do
    forM_ [0 .. n - 1] $ \i -> do
      pokeElemOff px i (fromIntegral i :: Float)
      pokeElemOff py i (fromIntegral i :: Float)
    let conf = cpuLaunchConfig1D 3 37
    launchCUDAIOM conf $
      saxpy @CUDAIOM @CUDAIO @Float (fromIntegral n) 2.0 (inject px) (inject py)
    forM_ [0 .. n - 1] $ \i ->
      peekElemOff py i >>= (@?= fromIntegral (3 * i))

unit_saxpy_cuda_dejafu :: IO ()
unit_saxpy_cuda_dejafu = do
  let n = 70
      nbytes = n * Foreign.Storable.sizeOf (undefined :: Float)
  allocaBytes nbytes $ \px -> allocaBytes nbytes $ \py -> do
    forM_ [0 .. n - 1] $ \i -> do
      pokeElemOff px i (fromIntegral i :: Float)
      pokeElemOff py i (fromIntegral i :: Float)
    let conf = cpuLaunchConfig1D 2 35
    (res, _trace) <-
      launchCUDADejaFuM conf $
        saxpy @CUDADejaFuM @CUDADejaFu @Float (fromIntegral n) 2.0 (inject px) (inject py)
    res @?= Right ()
    forM_ [0 .. n - 1] $ \i ->
      peekElemOff py i >>= (@?= fromIntegral (3 * i))

blockNeighborSum ::
  ( Monad m,
    MonadReader (CUDAThreadInfo e) m,
    CmdSync m,
    CmdRef m e,
    M.CmdStorable m e Int32,
    JanusTyped e Int32,
    ExpIntegral e Int32,
    ExpIntegralCast e Int32 Int64,
    Num (e Int32)
  ) =>
  e (Ptr Int32) -> m ()
blockNeighborSum buf = do
  blockDim <- asks _cudaThreadInfoBlockDimX
  blockIdx <- asks _cudaThreadInfoBlockIdxX
  threadIdx <- asks _cudaThreadInfoThreadIdxX
  base <- letM $ blockDim * blockIdx
  gid <- letM $ base + threadIdx
  nextInBlock <- letM $ base + ((threadIdx + 1) `remainder` blockDim)
  M.pokeElemOff buf (gid + 1) (toIntegral gid)
  syncWarp
  syncBlock
  mine <- M.peekElemOff buf (toIntegral gid)
  neighbor <- M.peekElemOff buf (toIntegral nextInBlock)
  syncBlock
  M.pokeElemOff buf (mine + neighbor) (toIntegral gid)

expectedNeighborSum :: Int -> Int -> Int -> Int32
expectedNeighborSum blockX b t =
  let base = b * blockX
      gid = base + t
      nextGid = base + ((t + 1) `rem` blockX)
   in fromIntegral ((gid + 1) + (nextGid + 1))

unit_sync_barriers_io :: IO ()
unit_sync_barriers_io = do
  let gridX = 2
      blockX = 35
      total = gridX * blockX
      nbytes = total * Foreign.Storable.sizeOf (undefined :: Int32)
  allocaBytes nbytes $ \pbuf -> do
    forM_ [0 .. total - 1] $ \i -> pokeElemOff pbuf i (0 :: Int32)
    launchCUDAIOM (cpuLaunchConfig1D gridX blockX) $
      blockNeighborSum @CUDAIOM @CUDAIO (inject pbuf)
    forM_ [0 .. gridX - 1] $ \b ->
      forM_ [0 .. blockX - 1] $ \t ->
        peekElemOff pbuf (b * blockX + t) >>= (@?= expectedNeighborSum blockX b t)

unit_sync_barriers_dejafu :: IO ()
unit_sync_barriers_dejafu = do
  let gridX = 2
      blockX = 35
      total = gridX * blockX
      nbytes = total * Foreign.Storable.sizeOf (undefined :: Int32)
      conf = cpuLaunchConfig1D gridX blockX
  allocaBytes nbytes $ \pbuf -> do
    forM_ [0 .. total - 1] $ \i -> pokeElemOff pbuf i (0 :: Int32)
    (res, _trace) <-
      launchCUDADejaFuM conf $
        blockNeighborSum @CUDADejaFuM @CUDADejaFu (inject pbuf)
    res @?= Right ()
    forM_ [0 .. gridX - 1] $ \b ->
      forM_ [0 .. blockX - 1] $ \t ->
        peekElemOff pbuf (b * blockX + t) >>= (@?= expectedNeighborSum blockX b t)

    forM_ [0 .. total - 1] $ \i -> pokeElemOff pbuf i (0 :: Int32)
    (resNP, (), _traceNP) <-
      launchCUDADejaFuMWith roundRobinSchedNP defaultMemType () conf $
        blockNeighborSum @CUDADejaFuM @CUDADejaFu (inject pbuf)
    resNP @?= Right ()
    forM_ [0 .. gridX - 1] $ \b ->
      forM_ [0 .. blockX - 1] $ \t ->
        peekElemOff pbuf (b * blockX + t) >>= (@?= expectedNeighborSum blockX b t)
