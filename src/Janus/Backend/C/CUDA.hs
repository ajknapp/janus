{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Janus.Backend.C.CUDA where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Janus.Backend.C.CUDA.Foreign
import System.IO.Unsafe

cuInitialized :: IORef Bool
cuInitialized = unsafePerformIO $ newIORef False
{-# NOINLINE cuInitialized #-}

withCudaDevice :: Int -> (CUdevice -> IO a) -> IO a
withCudaDevice i k = alloca $ \pdev -> do
  ini <- readIORef cuInitialized
  unless ini $ cuInit 0 >> writeIORef cuInitialized True
  dev <- cuDeviceGet pdev (fromIntegral i) >> peek pdev
  k dev

withCompiledPTXFiles :: Foldable t => CUdevice -> t String -> (CUmodule -> IO a) -> IO a
withCompiledPTXFiles dev files k = alloca $ \ip -> do
  major <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR dev >> peek ip
  minor <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR dev >> peek ip
  let arch = "sm_" <> show major <> show minor
  alloca $ \h -> withCString "-O3" $ \o3 -> withCString ("-arch=" <> arch) $ \arch' -> withArray [o3,arch'] $ \nvargs -> do
    withNvJitLink h 2 nvargs $ \nvjh -> do
      traverse_ (\s -> withCString s $ \s' -> nvJitLinkAddFile nvjh NVJITLINK_INPUT_PTX s') files
      nvJitLinkComplete nvjh
      sz <- nvJitLinkGetLinkedCubinSize nvjh
      alloca $ \pmod -> allocaBytes (fromIntegral sz) $ \cubin -> do
        nvJitLinkGetLinkedCubin nvjh cubin
        bracket (cuModuleLoadData pmod cubin >> peek pmod) cuModuleUnload k

-- NOTE device pointers have to be allocated with C malloc/free to avoid cuda error 700
withCudaDeviceArray :: Int -> (CUdeviceptr -> IO a) -> IO a
withCudaDeviceArray nbytes k = bracket malloc free $ \dp -> bracket (cuMemAlloc dp (fromIntegral nbytes) >> peek dp) cuMemFree k

testCuda :: [String] -> IO ()
testCuda ptxFiles = alloca $ \pctx ->
  withCudaDevice 0 $ \dev -> withCuCtx pctx 0 dev $ \_ctx -> do
    let n = 1024
        nbytes = n*sizeOf (undefined :: Float)
        fill p i | i >= n = pure ()
                 | otherwise = pokeElemOff p i (fromIntegral i :: Float) >> fill p (i+1)
    withCompiledPTXFiles dev ptxFiles $ \m ->
      allocaBytes cudaArgMaxSize $ \argbuf -> allocaBytes cudaArgMaxSize $ \args -> allocaBytes nbytes $ \px -> allocaBytes nbytes $ \py -> do
        fill px 0 >> fill py 0
        withCudaDeviceArray nbytes $ \d_x ->
          withCudaDeviceArray nbytes $ \d_y -> do
            cuMemcpyHtoD d_x (castPtr px) (fromIntegral nbytes)
            cuMemcpyHtoD d_y (castPtr py) (fromIntegral nbytes)
            alloca $ \pfn -> withCString "janus_main" $ \s -> do
              fn <- cuModuleGetFunction pfn m s >> peek pfn
              packArgsAndRun fn argbuf 0 args 0 (pure ()) (fromIntegral n :: CInt) (1.0 :: Float) d_x d_y :: IO ()
              cuMemcpyDtoH (castPtr px) d_x (fromIntegral nbytes)
              cuMemcpyDtoH (castPtr py) d_y (fromIntegral nbytes)
              forM_ [0..16::Int] $ \i -> do
                peekElemOff py i >>= print

cudaArgMaxSize :: Int
cudaArgMaxSize = 4096

class CUDAPack r where
  packArgsAndRun :: CUfunction -> Ptr () -> Int -> Ptr (Ptr ()) -> Int -> IO () -> r

instance CUDAPack (IO ()) where
  packArgsAndRun fn _ _ argPtrs _ pack = pack >> cuLaunchKernel fn 1 1 1 1024 1 1 0 (CUstream nullPtr) argPtrs nullPtr

instance (Storable a, CUDAPack r) => CUDAPack (a -> r) where
  packArgsAndRun fn argBuf nbuf argPtrs nptrs pack a = packArgsAndRun fn argBuf (nbuf + sizeOf (undefined :: a)) argPtrs (nptrs + 1) $
    if nbuf >= cudaArgMaxSize
    then error "packArgsAndRun: too much space used"
    else do
      poke (argBuf `plusPtr` nbuf) a
      pokeElemOff argPtrs nptrs $ argBuf `plusPtr` nbuf
      pack
