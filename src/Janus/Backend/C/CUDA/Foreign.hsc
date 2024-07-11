{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Janus.Backend.C.CUDA.Foreign where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.IORef
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

#include <cuda.h>
#include <nvJitLink.h>

newtype CUresult = CUresult CInt
  deriving (Eq, Ord, Show)

data CUException = CUException CUresult
  deriving (Eq, Ord, Show)

instance Exception CUException

pattern CUDA_SUCCESS :: CUresult
pattern CUDA_SUCCESS = CUresult #{const CUDA_SUCCESS}

pattern CUDA_ERROR_INVALID_VALUE :: CUresult
pattern CUDA_ERROR_INVALID_VALUE = CUresult #{const CUDA_ERROR_INVALID_VALUE}

pattern CUDA_ERROR_OUT_OF_MEMORY :: CUresult
pattern CUDA_ERROR_OUT_OF_MEMORY = CUresult #{const CUDA_ERROR_OUT_OF_MEMORY}

pattern CUDA_ERROR_NOT_INITIALIZED :: CUresult
pattern CUDA_ERROR_NOT_INITIALIZED = CUresult #{const CUDA_ERROR_NOT_INITIALIZED}

pattern CUDA_ERROR_DEINITIALIZED :: CUresult
pattern CUDA_ERROR_DEINITIALIZED = CUresult #{const CUDA_ERROR_DEINITIALIZED}

pattern CUDA_ERROR_PROFILER_DISABLED :: CUresult
pattern CUDA_ERROR_PROFILER_DISABLED = CUresult #{const CUDA_ERROR_PROFILER_DISABLED}

pattern CUDA_ERROR_PROFILER_NOT_INITIALIZED :: CUresult
pattern CUDA_ERROR_PROFILER_NOT_INITIALIZED = CUresult #{const CUDA_ERROR_PROFILER_NOT_INITIALIZED}

pattern CUDA_ERROR_PROFILER_ALREADY_STARTED :: CUresult
pattern CUDA_ERROR_PROFILER_ALREADY_STARTED = CUresult #{const CUDA_ERROR_PROFILER_ALREADY_STARTED}

pattern CUDA_ERROR_PROFILER_ALREADY_STOPPED :: CUresult
pattern CUDA_ERROR_PROFILER_ALREADY_STOPPED = CUresult #{const CUDA_ERROR_PROFILER_ALREADY_STOPPED}

pattern CUDA_ERROR_STUB_LIBRARY :: CUresult
pattern CUDA_ERROR_STUB_LIBRARY = CUresult #{const CUDA_ERROR_STUB_LIBRARY}

pattern CUDA_ERROR_DEVICE_UNAVAILABLE :: CUresult
pattern CUDA_ERROR_DEVICE_UNAVAILABLE = CUresult #{const CUDA_ERROR_DEVICE_UNAVAILABLE}

pattern CUDA_ERROR_NO_DEVICE :: CUresult
pattern CUDA_ERROR_NO_DEVICE = CUresult #{const CUDA_ERROR_NO_DEVICE}

pattern CUDA_ERROR_INVALID_DEVICE :: CUresult
pattern CUDA_ERROR_INVALID_DEVICE = CUresult #{const CUDA_ERROR_INVALID_DEVICE}

pattern CUDA_ERROR_DEVICE_NOT_LICENSED :: CUresult
pattern CUDA_ERROR_DEVICE_NOT_LICENSED = CUresult #{const CUDA_ERROR_DEVICE_NOT_LICENSED}

pattern CUDA_ERROR_INVALID_IMAGE :: CUresult
pattern CUDA_ERROR_INVALID_IMAGE = CUresult #{const CUDA_ERROR_INVALID_IMAGE}

pattern CUDA_ERROR_INVALID_CONTEXT :: CUresult
pattern CUDA_ERROR_INVALID_CONTEXT = CUresult #{const CUDA_ERROR_INVALID_CONTEXT}

pattern CUDA_ERROR_CONTEXT_ALREADY_CURRENT :: CUresult
pattern CUDA_ERROR_CONTEXT_ALREADY_CURRENT = CUresult #{const CUDA_ERROR_CONTEXT_ALREADY_CURRENT}

pattern CUDA_ERROR_MAP_FAILED :: CUresult
pattern CUDA_ERROR_MAP_FAILED = CUresult #{const CUDA_ERROR_MAP_FAILED}

pattern CUDA_ERROR_UNMAP_FAILED :: CUresult
pattern CUDA_ERROR_UNMAP_FAILED = CUresult #{const CUDA_ERROR_UNMAP_FAILED}

pattern CUDA_ERROR_ARRAY_IS_MAPPED :: CUresult
pattern CUDA_ERROR_ARRAY_IS_MAPPED = CUresult #{const CUDA_ERROR_ARRAY_IS_MAPPED}

pattern CUDA_ERROR_ALREADY_MAPPED :: CUresult
pattern CUDA_ERROR_ALREADY_MAPPED = CUresult #{const CUDA_ERROR_ALREADY_MAPPED}

pattern CUDA_ERROR_NO_BINARY_FOR_GPU :: CUresult
pattern CUDA_ERROR_NO_BINARY_FOR_GPU = CUresult #{const CUDA_ERROR_NO_BINARY_FOR_GPU}

pattern CUDA_ERROR_ALREADY_ACQUIRED :: CUresult
pattern CUDA_ERROR_ALREADY_ACQUIRED = CUresult #{const CUDA_ERROR_ALREADY_ACQUIRED}

pattern CUDA_ERROR_NOT_MAPPED :: CUresult
pattern CUDA_ERROR_NOT_MAPPED = CUresult #{const CUDA_ERROR_NOT_MAPPED}

pattern CUDA_ERROR_NOT_MAPPED_AS_ARRAY :: CUresult
pattern CUDA_ERROR_NOT_MAPPED_AS_ARRAY = CUresult #{const CUDA_ERROR_NOT_MAPPED_AS_ARRAY}

pattern CUDA_ERROR_NOT_MAPPED_AS_POINTER :: CUresult
pattern CUDA_ERROR_NOT_MAPPED_AS_POINTER = CUresult #{const CUDA_ERROR_NOT_MAPPED_AS_POINTER}

pattern CUDA_ERROR_ECC_UNCORRECTABLE :: CUresult
pattern CUDA_ERROR_ECC_UNCORRECTABLE = CUresult #{const CUDA_ERROR_ECC_UNCORRECTABLE}

pattern CUDA_ERROR_UNSUPPORTED_LIMIT :: CUresult
pattern CUDA_ERROR_UNSUPPORTED_LIMIT = CUresult #{const CUDA_ERROR_UNSUPPORTED_LIMIT}

pattern CUDA_ERROR_CONTEXT_ALREADY_IN_USE :: CUresult
pattern CUDA_ERROR_CONTEXT_ALREADY_IN_USE = CUresult #{const CUDA_ERROR_CONTEXT_ALREADY_IN_USE}

pattern CUDA_ERROR_PEER_ACCESS_UNSUPPORTED :: CUresult
pattern CUDA_ERROR_PEER_ACCESS_UNSUPPORTED = CUresult #{const CUDA_ERROR_PEER_ACCESS_UNSUPPORTED}

pattern CUDA_ERROR_INVALID_PTX :: CUresult
pattern CUDA_ERROR_INVALID_PTX = CUresult #{const CUDA_ERROR_INVALID_PTX}

pattern CUDA_ERROR_INVALID_GRAPHICS_CONTEXT :: CUresult
pattern CUDA_ERROR_INVALID_GRAPHICS_CONTEXT = CUresult #{const CUDA_ERROR_INVALID_GRAPHICS_CONTEXT}

pattern CUDA_ERROR_NVLINK_UNCORRECTABLE :: CUresult
pattern CUDA_ERROR_NVLINK_UNCORRECTABLE = CUresult #{const CUDA_ERROR_NVLINK_UNCORRECTABLE}

pattern CUDA_ERROR_JIT_COMPILER_NOT_FOUND :: CUresult
pattern CUDA_ERROR_JIT_COMPILER_NOT_FOUND = CUresult #{const CUDA_ERROR_JIT_COMPILER_NOT_FOUND}

pattern CUDA_ERROR_UNSUPPORTED_PTX_VERSION :: CUresult
pattern CUDA_ERROR_UNSUPPORTED_PTX_VERSION = CUresult #{const CUDA_ERROR_UNSUPPORTED_PTX_VERSION}

pattern CUDA_ERROR_JIT_COMPILATION_DISABLED :: CUresult
pattern CUDA_ERROR_JIT_COMPILATION_DISABLED = CUresult #{const CUDA_ERROR_JIT_COMPILATION_DISABLED}

pattern CUDA_ERROR_UNSUPPORTED_DEVSIDE_SYNC :: CUresult
pattern CUDA_ERROR_UNSUPPORTED_DEVSIDE_SYNC = CUresult #{const CUDA_ERROR_UNSUPPORTED_DEVSIDE_SYNC}

pattern CUDA_ERROR_INVALID_SOURCE :: CUresult
pattern CUDA_ERROR_INVALID_SOURCE = CUresult #{const CUDA_ERROR_INVALID_SOURCE}

pattern CUDA_ERROR_FILE_NOT_FOUND :: CUresult
pattern CUDA_ERROR_FILE_NOT_FOUND = CUresult #{const CUDA_ERROR_FILE_NOT_FOUND}

pattern CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND :: CUresult
pattern CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND = CUresult #{const CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND}

pattern CUDA_ERROR_SHARED_OBJECT_INIT_FAILED :: CUresult
pattern CUDA_ERROR_SHARED_OBJECT_INIT_FAILED = CUresult #{const CUDA_ERROR_SHARED_OBJECT_INIT_FAILED}

pattern CUDA_ERROR_OPERATING_SYSTEM :: CUresult
pattern CUDA_ERROR_OPERATING_SYSTEM = CUresult #{const CUDA_ERROR_OPERATING_SYSTEM}

pattern CUDA_ERROR_INVALID_HANDLE :: CUresult
pattern CUDA_ERROR_INVALID_HANDLE = CUresult #{const CUDA_ERROR_INVALID_HANDLE}

pattern CUDA_ERROR_ILLEGAL_STATE :: CUresult
pattern CUDA_ERROR_ILLEGAL_STATE = CUresult #{const CUDA_ERROR_ILLEGAL_STATE}

pattern CUDA_ERROR_NOT_FOUND :: CUresult
pattern CUDA_ERROR_NOT_FOUND = CUresult #{const CUDA_ERROR_NOT_FOUND}

pattern CUDA_ERROR_NOT_READY :: CUresult
pattern CUDA_ERROR_NOT_READY = CUresult #{const CUDA_ERROR_NOT_READY}

pattern CUDA_ERROR_ILLEGAL_ADDRESS :: CUresult
pattern CUDA_ERROR_ILLEGAL_ADDRESS = CUresult #{const CUDA_ERROR_ILLEGAL_ADDRESS}

pattern CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES :: CUresult
pattern CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = CUresult #{const CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES}

pattern CUDA_ERROR_LAUNCH_TIMEOUT :: CUresult
pattern CUDA_ERROR_LAUNCH_TIMEOUT = CUresult #{const CUDA_ERROR_LAUNCH_TIMEOUT}

pattern CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING :: CUresult
pattern CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = CUresult #{const CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING}

pattern CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED :: CUresult
pattern CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED = CUresult #{const CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED}

pattern CUDA_ERROR_PEER_ACCESS_NOT_ENABLED :: CUresult
pattern CUDA_ERROR_PEER_ACCESS_NOT_ENABLED = CUresult #{const CUDA_ERROR_PEER_ACCESS_NOT_ENABLED}

pattern CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE :: CUresult
pattern CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE = CUresult #{const CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE}

pattern CUDA_ERROR_CONTEXT_IS_DESTROYED :: CUresult
pattern CUDA_ERROR_CONTEXT_IS_DESTROYED = CUresult #{const CUDA_ERROR_CONTEXT_IS_DESTROYED}

pattern CUDA_ERROR_ASSERT :: CUresult
pattern CUDA_ERROR_ASSERT = CUresult #{const CUDA_ERROR_ASSERT}

pattern CUDA_ERROR_TOO_MANY_PEERS :: CUresult
pattern CUDA_ERROR_TOO_MANY_PEERS = CUresult #{const CUDA_ERROR_TOO_MANY_PEERS}

pattern CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED :: CUresult
pattern CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED = CUresult #{const CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED}

pattern CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED :: CUresult
pattern CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED = CUresult #{const CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED}

pattern CUDA_ERROR_HARDWARE_STACK_ERROR :: CUresult
pattern CUDA_ERROR_HARDWARE_STACK_ERROR = CUresult #{const CUDA_ERROR_HARDWARE_STACK_ERROR}

pattern CUDA_ERROR_ILLEGAL_INSTRUCTION :: CUresult
pattern CUDA_ERROR_ILLEGAL_INSTRUCTION = CUresult #{const CUDA_ERROR_ILLEGAL_INSTRUCTION}

pattern CUDA_ERROR_MISALIGNED_ADDRESS :: CUresult
pattern CUDA_ERROR_MISALIGNED_ADDRESS = CUresult #{const CUDA_ERROR_MISALIGNED_ADDRESS}

pattern CUDA_ERROR_INVALID_ADDRESS_SPACE :: CUresult
pattern CUDA_ERROR_INVALID_ADDRESS_SPACE = CUresult #{const CUDA_ERROR_INVALID_ADDRESS_SPACE}

pattern CUDA_ERROR_INVALID_PC :: CUresult
pattern CUDA_ERROR_INVALID_PC = CUresult #{const CUDA_ERROR_INVALID_PC}

pattern CUDA_ERROR_LAUNCH_FAILED :: CUresult
pattern CUDA_ERROR_LAUNCH_FAILED = CUresult #{const CUDA_ERROR_LAUNCH_FAILED}

pattern CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE :: CUresult
pattern CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE = CUresult #{const CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE}

pattern CUDA_ERROR_NOT_PERMITTED :: CUresult
pattern CUDA_ERROR_NOT_PERMITTED = CUresult #{const CUDA_ERROR_NOT_PERMITTED}

pattern CUDA_ERROR_NOT_SUPPORTED :: CUresult
pattern CUDA_ERROR_NOT_SUPPORTED = CUresult #{const CUDA_ERROR_NOT_SUPPORTED}

pattern CUDA_ERROR_SYSTEM_NOT_READY :: CUresult
pattern CUDA_ERROR_SYSTEM_NOT_READY = CUresult #{const CUDA_ERROR_SYSTEM_NOT_READY}

pattern CUDA_ERROR_SYSTEM_DRIVER_MISMATCH :: CUresult
pattern CUDA_ERROR_SYSTEM_DRIVER_MISMATCH = CUresult #{const CUDA_ERROR_SYSTEM_DRIVER_MISMATCH}

pattern CUDA_ERROR_COMPAT_NOT_SUPPORTED_ON_DEVICE :: CUresult
pattern CUDA_ERROR_COMPAT_NOT_SUPPORTED_ON_DEVICE = CUresult #{const CUDA_ERROR_COMPAT_NOT_SUPPORTED_ON_DEVICE}

pattern CUDA_ERROR_MPS_CONNECTION_FAILED :: CUresult
pattern CUDA_ERROR_MPS_CONNECTION_FAILED = CUresult #{const CUDA_ERROR_MPS_CONNECTION_FAILED}

pattern CUDA_ERROR_MPS_RPC_FAILURE :: CUresult
pattern CUDA_ERROR_MPS_RPC_FAILURE = CUresult #{const CUDA_ERROR_MPS_RPC_FAILURE}

pattern CUDA_ERROR_MPS_SERVER_NOT_READY :: CUresult
pattern CUDA_ERROR_MPS_SERVER_NOT_READY = CUresult #{const CUDA_ERROR_MPS_SERVER_NOT_READY}

pattern CUDA_ERROR_MPS_MAX_CLIENTS_REACHED :: CUresult
pattern CUDA_ERROR_MPS_MAX_CLIENTS_REACHED = CUresult #{const CUDA_ERROR_MPS_MAX_CLIENTS_REACHED}

pattern CUDA_ERROR_MPS_MAX_CONNECTIONS_REACHED :: CUresult
pattern CUDA_ERROR_MPS_MAX_CONNECTIONS_REACHED = CUresult #{const CUDA_ERROR_MPS_MAX_CONNECTIONS_REACHED}

pattern CUDA_ERROR_MPS_CLIENT_TERMINATED :: CUresult
pattern CUDA_ERROR_MPS_CLIENT_TERMINATED = CUresult #{const CUDA_ERROR_MPS_CLIENT_TERMINATED}

pattern CUDA_ERROR_CDP_NOT_SUPPORTED :: CUresult
pattern CUDA_ERROR_CDP_NOT_SUPPORTED = CUresult #{const CUDA_ERROR_CDP_NOT_SUPPORTED}

pattern CUDA_ERROR_CDP_VERSION_MISMATCH :: CUresult
pattern CUDA_ERROR_CDP_VERSION_MISMATCH = CUresult #{const CUDA_ERROR_CDP_VERSION_MISMATCH}

pattern CUDA_ERROR_STREAM_CAPTURE_UNSUPPORTED :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_UNSUPPORTED = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_UNSUPPORTED}

pattern CUDA_ERROR_STREAM_CAPTURE_INVALIDATED :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_INVALIDATED = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_INVALIDATED}

pattern CUDA_ERROR_STREAM_CAPTURE_MERGE :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_MERGE = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_MERGE}

pattern CUDA_ERROR_STREAM_CAPTURE_UNMATCHED :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_UNMATCHED = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_UNMATCHED}

pattern CUDA_ERROR_STREAM_CAPTURE_UNJOINED :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_UNJOINED = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_UNJOINED}

pattern CUDA_ERROR_STREAM_CAPTURE_ISOLATION :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_ISOLATION = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_ISOLATION}

pattern CUDA_ERROR_STREAM_CAPTURE_IMPLICIT :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_IMPLICIT = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_IMPLICIT}

pattern CUDA_ERROR_CAPTURED_EVENT :: CUresult
pattern CUDA_ERROR_CAPTURED_EVENT = CUresult #{const CUDA_ERROR_CAPTURED_EVENT}

pattern CUDA_ERROR_STREAM_CAPTURE_WRONG_THREAD :: CUresult
pattern CUDA_ERROR_STREAM_CAPTURE_WRONG_THREAD = CUresult #{const CUDA_ERROR_STREAM_CAPTURE_WRONG_THREAD}

pattern CUDA_ERROR_TIMEOUT :: CUresult
pattern CUDA_ERROR_TIMEOUT = CUresult #{const CUDA_ERROR_TIMEOUT}

pattern CUDA_ERROR_GRAPH_EXEC_UPDATE_FAILURE :: CUresult
pattern CUDA_ERROR_GRAPH_EXEC_UPDATE_FAILURE = CUresult #{const CUDA_ERROR_GRAPH_EXEC_UPDATE_FAILURE}

pattern CUDA_ERROR_EXTERNAL_DEVICE :: CUresult
pattern CUDA_ERROR_EXTERNAL_DEVICE = CUresult #{const CUDA_ERROR_EXTERNAL_DEVICE}

pattern CUDA_ERROR_INVALID_CLUSTER_SIZE :: CUresult
pattern CUDA_ERROR_INVALID_CLUSTER_SIZE = CUresult #{const CUDA_ERROR_INVALID_CLUSTER_SIZE}

pattern CUDA_ERROR_UNKNOWN :: CUresult
pattern CUDA_ERROR_UNKNOWN = CUresult #{const CUDA_ERROR_UNKNOWN}

{-# COMPLETE CUDA_SUCCESS,
  CUDA_ERROR_INVALID_VALUE,
  CUDA_ERROR_OUT_OF_MEMORY,
  CUDA_ERROR_NOT_INITIALIZED,
  CUDA_ERROR_DEINITIALIZED,
  CUDA_ERROR_PROFILER_DISABLED,
  CUDA_ERROR_PROFILER_NOT_INITIALIZED,
  CUDA_ERROR_PROFILER_ALREADY_STARTED,
  CUDA_ERROR_PROFILER_ALREADY_STOPPED,
  CUDA_ERROR_STUB_LIBRARY,
  CUDA_ERROR_DEVICE_UNAVAILABLE,
  CUDA_ERROR_NO_DEVICE,
  CUDA_ERROR_INVALID_DEVICE,
  CUDA_ERROR_DEVICE_NOT_LICENSED,
  CUDA_ERROR_INVALID_IMAGE,
  CUDA_ERROR_INVALID_CONTEXT,
  CUDA_ERROR_CONTEXT_ALREADY_CURRENT,
  CUDA_ERROR_MAP_FAILED,
  CUDA_ERROR_UNMAP_FAILED,
  CUDA_ERROR_ARRAY_IS_MAPPED,
  CUDA_ERROR_ALREADY_MAPPED,
  CUDA_ERROR_NO_BINARY_FOR_GPU,
  CUDA_ERROR_ALREADY_ACQUIRED,
  CUDA_ERROR_NOT_MAPPED,
  CUDA_ERROR_NOT_MAPPED_AS_ARRAY,
  CUDA_ERROR_NOT_MAPPED_AS_POINTER,
  CUDA_ERROR_ECC_UNCORRECTABLE,
  CUDA_ERROR_UNSUPPORTED_LIMIT,
  CUDA_ERROR_CONTEXT_ALREADY_IN_USE,
  CUDA_ERROR_PEER_ACCESS_UNSUPPORTED,
  CUDA_ERROR_INVALID_PTX,
  CUDA_ERROR_INVALID_GRAPHICS_CONTEXT,
  CUDA_ERROR_NVLINK_UNCORRECTABLE,
  CUDA_ERROR_JIT_COMPILER_NOT_FOUND,
  CUDA_ERROR_UNSUPPORTED_PTX_VERSION,
  CUDA_ERROR_JIT_COMPILATION_DISABLED,
  CUDA_ERROR_UNSUPPORTED_DEVSIDE_SYNC,
  CUDA_ERROR_INVALID_SOURCE,
  CUDA_ERROR_FILE_NOT_FOUND,
  CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND,
  CUDA_ERROR_SHARED_OBJECT_INIT_FAILED,
  CUDA_ERROR_OPERATING_SYSTEM,
  CUDA_ERROR_INVALID_HANDLE,
  CUDA_ERROR_ILLEGAL_STATE,
  CUDA_ERROR_NOT_FOUND,
  CUDA_ERROR_NOT_READY,
  CUDA_ERROR_ILLEGAL_ADDRESS,
  CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES,
  CUDA_ERROR_LAUNCH_TIMEOUT,
  CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING,
  CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED,
  CUDA_ERROR_PEER_ACCESS_NOT_ENABLED,
  CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE,
  CUDA_ERROR_CONTEXT_IS_DESTROYED,
  CUDA_ERROR_ASSERT,
  CUDA_ERROR_TOO_MANY_PEERS,
  CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED,
  CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED,
  CUDA_ERROR_HARDWARE_STACK_ERROR,
  CUDA_ERROR_ILLEGAL_INSTRUCTION,
  CUDA_ERROR_MISALIGNED_ADDRESS,
  CUDA_ERROR_INVALID_ADDRESS_SPACE,
  CUDA_ERROR_INVALID_PC,
  CUDA_ERROR_LAUNCH_FAILED,
  CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE,
  CUDA_ERROR_NOT_PERMITTED,
  CUDA_ERROR_NOT_SUPPORTED,
  CUDA_ERROR_SYSTEM_NOT_READY,
  CUDA_ERROR_SYSTEM_DRIVER_MISMATCH,
  CUDA_ERROR_COMPAT_NOT_SUPPORTED_ON_DEVICE,
  CUDA_ERROR_MPS_CONNECTION_FAILED,
  CUDA_ERROR_MPS_RPC_FAILURE,
  CUDA_ERROR_MPS_SERVER_NOT_READY,
  CUDA_ERROR_MPS_MAX_CLIENTS_REACHED,
  CUDA_ERROR_MPS_MAX_CONNECTIONS_REACHED,
  CUDA_ERROR_MPS_CLIENT_TERMINATED,
  CUDA_ERROR_CDP_NOT_SUPPORTED,
  CUDA_ERROR_CDP_VERSION_MISMATCH,
  CUDA_ERROR_STREAM_CAPTURE_UNSUPPORTED,
  CUDA_ERROR_STREAM_CAPTURE_INVALIDATED,
  CUDA_ERROR_STREAM_CAPTURE_MERGE,
  CUDA_ERROR_STREAM_CAPTURE_UNMATCHED,
  CUDA_ERROR_STREAM_CAPTURE_UNJOINED,
  CUDA_ERROR_STREAM_CAPTURE_ISOLATION,
  CUDA_ERROR_STREAM_CAPTURE_IMPLICIT,
  CUDA_ERROR_CAPTURED_EVENT,
  CUDA_ERROR_STREAM_CAPTURE_WRONG_THREAD,
  CUDA_ERROR_TIMEOUT,
  CUDA_ERROR_GRAPH_EXEC_UPDATE_FAILURE,
  CUDA_ERROR_EXTERNAL_DEVICE,
  CUDA_ERROR_INVALID_CLUSTER_SIZE,
  CUDA_ERROR_UNKNOWN #-}

cudaCheck :: IO CUresult -> IO ()
cudaCheck m = m >>= \case
  CUDA_SUCCESS -> pure ()
  e -> throwIO (CUException e)

foreign import ccall unsafe "cuInit" c_cuInit :: CUInt -> IO CUresult

cuInit :: CUInt -> IO ()
cuInit = cudaCheck . c_cuInit

newtype CUdevice = CUdevice CInt
  deriving (Eq, Ord, Show, Storable)

foreign import ccall unsafe "cuDeviceGet" c_cuDeviceGet :: Ptr CUdevice -> CInt -> IO CUresult

cuInitialized :: IORef Bool
cuInitialized = unsafePerformIO $ newIORef False
{-# NOINLINE cuInitialized #-}

withCudaDevice :: Int -> (CUdevice -> IO a) -> IO a
withCudaDevice i k = bracket malloc free $ \pdev -> do
  ini <- readIORef cuInitialized
  unless ini $ cuInit 0 >> writeIORef cuInitialized True
  dev <- cuDeviceGet pdev (fromIntegral i) >> peek pdev
  k dev

cuDeviceGet :: Ptr CUdevice -> CInt -> IO ()
cuDeviceGet dev i = cudaCheck (c_cuDeviceGet dev i)

newtype CUdevice_attribute = CUdevice_attribute CInt
  deriving (Eq, Ord, Show, Storable)

pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR :: CUdevice_attribute
pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR = CUdevice_attribute #{const CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR}

pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR :: CUdevice_attribute
pattern CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR = CUdevice_attribute #{const CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR}

foreign import ccall unsafe "cuDeviceGetAttribute" c_cuDeviceGetAttribute :: Ptr CInt -> CUdevice_attribute -> CUdevice -> IO CUresult

cuDeviceGetAttribute :: Ptr CInt -> CUdevice_attribute -> CUdevice -> IO ()
cuDeviceGetAttribute i a d = cudaCheck (c_cuDeviceGetAttribute i a d)

data CUctx_st

newtype CUctx = CUCtx (Ptr CUctx_st)
  deriving (Eq, Ord, Show, Storable)

foreign import ccall unsafe "cuCtxCreate" c_cuCtxCreate :: Ptr CUctx -> CUInt -> CUdevice -> IO CUresult

cuCtxCreate :: Ptr CUctx -> CUInt -> CUdevice -> IO ()
cuCtxCreate ctx flags dev = cudaCheck (c_cuCtxCreate ctx flags dev)

foreign import ccall unsafe "cuCtxSetCurrent" c_cuCtxSetCurrent :: CUctx -> IO CUresult

cuCtxSetCurrent :: CUctx -> IO ()
cuCtxSetCurrent ctx = cudaCheck (c_cuCtxSetCurrent ctx)

foreign import ccall unsafe "cuCtxDestroy" c_cuCtxDestroy :: CUctx -> IO CUresult

cuCtxDestroy :: CUctx -> IO ()
cuCtxDestroy ctx = cudaCheck (c_cuCtxDestroy ctx)

withCuCtx :: Ptr CUctx -> CUInt -> CUdevice -> (CUctx -> IO a) -> IO a
withCuCtx pctx i dev = bracket (cuCtxCreate pctx i dev >> peek pctx) cuCtxDestroy -- (\_ -> peek pctx >>= \ctx -> cuCtxSetCurrent ctx >> cuCtxDestroy ctx)

newtype CUdeviceptr = CUdeviceptr (Ptr ())
  deriving (Eq, Ord, Show, Storable)

foreign import ccall unsafe "cuMemAlloc" c_cuMemAlloc :: Ptr CUdeviceptr -> CSize -> IO CUresult

cuMemAlloc :: Ptr CUdeviceptr -> CSize -> IO ()
cuMemAlloc p n = cudaCheck (c_cuMemAlloc p n)

foreign import ccall unsafe "cuMemFree" c_cuMemFree :: CUdeviceptr -> IO CUresult

cuMemFree :: CUdeviceptr -> IO ()
cuMemFree p = cudaCheck (c_cuMemFree p)

foreign import ccall unsafe "cuMemcpyHtoD" c_cuMemcpyHtoD :: CUdeviceptr -> Ptr () -> CSize -> IO CUresult

cuMemcpyHtoD :: CUdeviceptr -> Ptr () -> CSize -> IO ()
cuMemcpyHtoD dev ptr n = cudaCheck (c_cuMemcpyHtoD dev ptr n)

foreign import ccall unsafe "cuMemcpyDtoH" c_cuMemcpyDtoH :: Ptr () -> CUdeviceptr -> CSize -> IO CUresult

cuMemcpyDtoH :: Ptr () -> CUdeviceptr -> CSize -> IO ()
cuMemcpyDtoH ptr dev n = cudaCheck (c_cuMemcpyDtoH ptr dev n)

newtype NVJitLinkResult = NVJitLinkResult CInt
  deriving (Eq, Ord, Show)

pattern NVJITLINK_SUCCESS :: NVJitLinkResult
pattern NVJITLINK_SUCCESS = NVJitLinkResult #{const NVJITLINK_SUCCESS}

pattern NVJITLINK_ERROR_UNRECOGNIZED_OPTION :: NVJitLinkResult
pattern NVJITLINK_ERROR_UNRECOGNIZED_OPTION = NVJitLinkResult #{const NVJITLINK_ERROR_UNRECOGNIZED_OPTION}

pattern NVJITLINK_ERROR_MISSING_ARCH :: NVJitLinkResult
pattern NVJITLINK_ERROR_MISSING_ARCH = NVJitLinkResult #{const NVJITLINK_ERROR_MISSING_ARCH}

pattern NVJITLINK_ERROR_INVALID_INPUT :: NVJitLinkResult
pattern NVJITLINK_ERROR_INVALID_INPUT = NVJitLinkResult #{const NVJITLINK_ERROR_INVALID_INPUT}

pattern NVJITLINK_ERROR_PTX_COMPILE :: NVJitLinkResult
pattern NVJITLINK_ERROR_PTX_COMPILE = NVJitLinkResult #{const NVJITLINK_ERROR_PTX_COMPILE}

pattern NVJITLINK_ERROR_NVVM_COMPILE :: NVJitLinkResult
pattern NVJITLINK_ERROR_NVVM_COMPILE = NVJitLinkResult #{const NVJITLINK_ERROR_NVVM_COMPILE}

pattern NVJITLINK_ERROR_INTERNAL :: NVJitLinkResult
pattern NVJITLINK_ERROR_INTERNAL = NVJitLinkResult #{const NVJITLINK_ERROR_INTERNAL}

pattern NVJITLINK_ERROR_THREADPOOL :: NVJitLinkResult
pattern NVJITLINK_ERROR_THREADPOOL = NVJitLinkResult #{const NVJITLINK_ERROR_THREADPOOL}

data NVJitLinkException = NVJitLinkException NVJitLinkResult
  deriving (Eq, Ord, Show)

instance Exception NVJitLinkException

{-# COMPLETE NVJITLINK_SUCCESS,
  NVJITLINK_ERROR_UNRECOGNIZED_OPTION,
  NVJITLINK_ERROR_MISSING_ARCH,
  NVJITLINK_ERROR_INVALID_INPUT,
  NVJITLINK_ERROR_PTX_COMPILE,
  NVJITLINK_ERROR_NVVM_COMPILE,
  NVJITLINK_ERROR_INTERNAL,
  NVJITLINK_ERROR_THREADPOOL #-}

newtype NVJitLinkInputType = NVJitLinkInputType CInt
  deriving (Eq, Ord, Show)

pattern NVJITLINK_INPUT_NONE :: NVJitLinkInputType
pattern NVJITLINK_INPUT_NONE = NVJitLinkInputType #{const NVJITLINK_INPUT_NONE}

pattern NVJITLINK_INPUT_CUBIN :: NVJitLinkInputType
pattern NVJITLINK_INPUT_CUBIN = NVJitLinkInputType #{const NVJITLINK_INPUT_CUBIN}

pattern NVJITLINK_INPUT_PTX :: NVJitLinkInputType
pattern NVJITLINK_INPUT_PTX = NVJitLinkInputType #{const NVJITLINK_INPUT_PTX}

pattern NVJITLINK_INPUT_LTOIR :: NVJitLinkInputType
pattern NVJITLINK_INPUT_LTOIR = NVJitLinkInputType #{const NVJITLINK_INPUT_LTOIR}

pattern NVJITLINK_INPUT_FATBIN :: NVJitLinkInputType
pattern NVJITLINK_INPUT_FATBIN = NVJitLinkInputType #{const NVJITLINK_INPUT_FATBIN}

pattern NVJITLINK_INPUT_OBJECT :: NVJitLinkInputType
pattern NVJITLINK_INPUT_OBJECT = NVJitLinkInputType #{const NVJITLINK_INPUT_OBJECT}

pattern NVJITLINK_INPUT_LIBRARY :: NVJitLinkInputType
pattern NVJITLINK_INPUT_LIBRARY = NVJitLinkInputType #{const NVJITLINK_INPUT_LIBRARY}

{-# COMPLETE NVJITLINK_INPUT_NONE,
  NVJITLINK_INPUT_CUBIN,
  NVJITLINK_INPUT_PTX,
  NVJITLINK_INPUT_LTOIR,
  NVJITLINK_INPUT_FATBIN,
  NVJITLINK_INPUT_OBJECT,
  NVJITLINK_INPUT_LIBRARY #-}

nvJitLinkCheck :: IO NVJitLinkResult -> IO ()
nvJitLinkCheck m = m >>= \case
  NVJITLINK_SUCCESS -> pure ()
  err -> throwIO (NVJitLinkException err)

data NVJitLink

newtype NVJitLinkHandle = NVJitLinkHandle (Ptr NVJitLink)
  deriving (Eq, Show, Ord, Storable)

foreign import ccall unsafe "nvJitLinkCreate" c_nvJitLinkCreate :: Ptr NVJitLinkHandle -> Word32 -> Ptr (Ptr CChar) -> IO NVJitLinkResult

nvJitLinkCreate :: Ptr NVJitLinkHandle -> Word32 -> Ptr (Ptr CChar) -> IO ()
nvJitLinkCreate h f o = nvJitLinkCheck (c_nvJitLinkCreate h f o)

foreign import ccall unsafe "nvJitLinkDestroy" c_nvJitLinkDestroy :: Ptr NVJitLinkHandle -> IO NVJitLinkResult

nvJitLinkDestroy :: Ptr NVJitLinkHandle -> IO ()
nvJitLinkDestroy h = nvJitLinkCheck (c_nvJitLinkDestroy h)

withNvJitLink :: Ptr NVJitLinkHandle -> Word32 -> Ptr (Ptr CChar) -> (NVJitLinkHandle -> IO a) -> IO a
withNvJitLink h f o = bracket (nvJitLinkCreate h f o >> peek h) (\_ -> nvJitLinkDestroy h)

foreign import ccall unsafe "nvJitLinkAddFile" c_nvJitLinkAddFile :: NVJitLinkHandle -> NVJitLinkInputType -> Ptr CChar -> IO NVJitLinkResult

nvJitLinkAddFile :: NVJitLinkHandle -> NVJitLinkInputType -> Ptr CChar -> IO ()
nvJitLinkAddFile h i s = nvJitLinkCheck (c_nvJitLinkAddFile h i s)

foreign import ccall unsafe "nvJitLinkComplete" c_nvJitLinkComplete :: NVJitLinkHandle -> IO NVJitLinkResult

nvJitLinkComplete :: NVJitLinkHandle -> IO ()
nvJitLinkComplete h = nvJitLinkCheck (c_nvJitLinkComplete h)

foreign import ccall unsafe "nvJitLinkGetLinkedCubinSize" c_nvJitLinkGetLinkedCubinSize :: NVJitLinkHandle -> Ptr CSize -> IO NVJitLinkResult

nvJitLinkGetLinkedCubinSize :: NVJitLinkHandle -> IO CSize
nvJitLinkGetLinkedCubinSize h = bracket malloc free $ \p -> do
  nvJitLinkCheck (c_nvJitLinkGetLinkedCubinSize h p)
  peek p

foreign import ccall unsafe "nvJitLinkGetLinkedCubin" c_nvJitLinkGetLinkedCubin :: NVJitLinkHandle -> Ptr () -> IO NVJitLinkResult

nvJitLinkGetLinkedCubin :: NVJitLinkHandle -> Ptr () -> IO ()
nvJitLinkGetLinkedCubin h p = nvJitLinkCheck (c_nvJitLinkGetLinkedCubin h p)

data CUmod_st

newtype CUmodule = CUmodule (Ptr CUmod_st)
  deriving (Eq, Ord, Show, Storable)

foreign import ccall unsafe "cuModuleLoadData" c_cuModuleLoadData :: Ptr CUmodule -> Ptr () -> IO CUresult

cuModuleLoadData :: Ptr CUmodule -> Ptr () -> IO ()
cuModuleLoadData m p = cudaCheck (c_cuModuleLoadData m p)

foreign import ccall unsafe "cuModuleUnload" c_cuModuleUnload :: CUmodule -> IO CUresult

cuModuleUnload :: CUmodule -> IO ()
cuModuleUnload m = cudaCheck (c_cuModuleUnload m)

data CUfunc_st

newtype CUfunction = CUfunction (Ptr CUfunc_st)
  deriving (Eq, Ord, Show, Storable)

foreign import ccall unsafe "cuModuleGetFunction" c_cuModuleGetFunction :: Ptr CUfunction -> CUmodule -> Ptr CChar -> IO CUresult

cuModuleGetFunction :: Ptr CUfunction -> CUmodule -> Ptr CChar -> IO ()
cuModuleGetFunction p m s = cudaCheck (c_cuModuleGetFunction p m s)

data CUstream_st

newtype CUstream = CUstream (Ptr CUstream_st)
  deriving (Eq, Ord, Show, Storable)

foreign import ccall "cuLaunchKernel" c_cuLaunchKernel
  :: CUfunction
  -> CUInt -> CUInt -> CUInt
  -> CUInt -> CUInt -> CUInt
  -> CUInt -> CUstream
  -> Ptr (Ptr ())
  -> Ptr (Ptr ())
  -> IO CUresult

cuLaunchKernel
  :: CUfunction
  -> CUInt -> CUInt -> CUInt
  -> CUInt -> CUInt -> CUInt
  -> CUInt -> CUstream
  -> Ptr (Ptr ())
  -> Ptr (Ptr ())
  -> IO ()
cuLaunchKernel fn gridx gridy gridz blockx blocky blockz shmemBytes stream params extra = cudaCheck $
  c_cuLaunchKernel fn gridx gridy gridz blockx blocky blockz shmemBytes stream params extra

foreign import ccall "cuCtxSynchronize" c_cuCtxSynchronize :: IO CUresult

cuCtxSynchronize :: IO ()
cuCtxSynchronize = cudaCheck c_cuCtxSynchronize

-- TODO add flag field/flag count
data CUlaunchConfig
  = CUlaunchConfig
  { _cuLaunchConfigGridDimX :: CUInt
  , _cuLaunchConfigGridDimY :: CUInt
  , _cuLaunchConfigGridDimZ :: CUInt
  , _cuLaunchConfigBlockDimX :: CUInt
  , _cuLaunchConfigBlockDimY :: CUInt
  , _cuLaunchConfigBlockDimZ :: CUInt
  , _cuLaunchConfigSharedMemBytes :: CUInt
  , _cuLaunchConfigStream :: CUstream
  } deriving (Eq, Ord, Show)

$(makeLenses ''CUlaunchConfig)

instance Storable CUlaunchConfig where
  sizeOf _ = #{size CUlaunchConfig}
  alignment _ = #{alignment CUlaunchConfig}
  peek p = do
    _cuLaunchConfigGridDimX <- #{peek CUlaunchConfig, gridDimX} p
    _cuLaunchConfigGridDimY <- #{peek CUlaunchConfig, gridDimY} p
    _cuLaunchConfigGridDimZ <- #{peek CUlaunchConfig, gridDimZ} p
    _cuLaunchConfigBlockDimX <- #{peek CUlaunchConfig, blockDimX} p
    _cuLaunchConfigBlockDimY <- #{peek CUlaunchConfig, blockDimY} p
    _cuLaunchConfigBlockDimZ <- #{peek CUlaunchConfig, blockDimZ} p
    _cuLaunchConfigSharedMemBytes <- #{peek CUlaunchConfig, sharedMemBytes} p
    _cuLaunchConfigStream <- #{peek CUlaunchConfig, hStream} p
    pure CUlaunchConfig {..}
  poke p CUlaunchConfig {..} = do
    #{poke CUlaunchConfig, gridDimX} p _cuLaunchConfigGridDimX
    #{poke CUlaunchConfig, gridDimY} p _cuLaunchConfigGridDimY
    #{poke CUlaunchConfig, gridDimZ} p _cuLaunchConfigGridDimZ
    #{poke CUlaunchConfig, blockDimX} p _cuLaunchConfigBlockDimX
    #{poke CUlaunchConfig, blockDimY} p _cuLaunchConfigBlockDimY
    #{poke CUlaunchConfig, blockDimZ} p _cuLaunchConfigBlockDimZ
    #{poke CUlaunchConfig, sharedMemBytes} p _cuLaunchConfigSharedMemBytes
    #{poke CUlaunchConfig, hStream} p _cuLaunchConfigStream

foreign import ccall unsafe "cuOccupancyMaxPotentialBlockSize" c_cuOccupancyMaxPotentialBlockSize
  :: Ptr CInt -> Ptr CInt -> CUfunction -> FunPtr (CInt -> CSize) -> CSize -> CInt -> IO CUresult

cuOccupancyMaxPotentialBlockSize :: CUfunction -> FunPtr (CInt -> CSize) -> CSize -> CInt -> IO (CInt, CInt)
cuOccupancyMaxPotentialBlockSize fn blockSizeToDynamicSMemSize dynamicSMemSize blockSizeLimit =
  bracket malloc free $ \pmingrid -> bracket malloc free $ \pblockSize -> do
    cudaCheck $ c_cuOccupancyMaxPotentialBlockSize pmingrid pblockSize fn blockSizeToDynamicSMemSize dynamicSMemSize blockSizeLimit
    (,) <$> peek pmingrid <*> peek pblockSize
