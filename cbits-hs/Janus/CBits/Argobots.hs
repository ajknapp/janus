{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level FFI bindings to Janus's Argobots execution-stream pool
-- (@cbits/janus_abt.c@).
module Janus.CBits.Argobots
  ( JanusAbtWorkerFn,
    janusAbtInit,
    janusAbtFinalize,
    janusAbtSetNumXstreams,
    janusAbtNumXstreams,
    withArgobotsXstreams,
    c_janus_abt_parallel_for,
    c_janus_abt_parallel_for_addr,
    c_janus_abt_query_libdir,
  )
where

import Control.Exception (bracket)
import Data.Int (Int32, Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr)

type JanusAbtWorkerFn = Int32 -> Int64 -> Int64 -> Ptr () -> IO ()

foreign import ccall safe "janus_abt_init"
  c_janus_abt_init :: CInt -> IO CInt

foreign import ccall safe "janus_abt_finalize"
  c_janus_abt_finalize :: IO ()

foreign import ccall safe "janus_abt_set_num_xstreams"
  c_janus_abt_set_num_xstreams :: CInt -> IO CInt

foreign import ccall unsafe "janus_abt_num_xstreams"
  c_janus_abt_num_xstreams :: IO CInt

foreign import ccall safe "janus_abt_parallel_for"
  c_janus_abt_parallel_for ::
    Int64 ->
    Int64 ->
    Int64 ->
    Ptr () ->
    FunPtr JanusAbtWorkerFn ->
    IO ()

foreign import ccall "&janus_abt_parallel_for"
  c_janus_abt_parallel_for_addr ::
    FunPtr (Int64 -> Int64 -> Int64 -> Ptr () -> FunPtr JanusAbtWorkerFn -> IO ())

foreign import ccall unsafe "janus_abt_query_libdir"
  c_janus_abt_query_libdir :: CString -> CInt -> IO CInt

janusAbtInit :: Int -> IO Int
janusAbtInit n = fromIntegral <$> c_janus_abt_init (fromIntegral n)

janusAbtFinalize :: IO ()
janusAbtFinalize = c_janus_abt_finalize

janusAbtSetNumXstreams :: Int -> IO Int
janusAbtSetNumXstreams n = fromIntegral <$> c_janus_abt_set_num_xstreams (fromIntegral n)

janusAbtNumXstreams :: IO Int
janusAbtNumXstreams = fromIntegral <$> c_janus_abt_num_xstreams

-- | Temporarily configure the Argobots execution-stream count for the duration
-- of an action, restoring the previous count afterwards.
withArgobotsXstreams :: Int -> IO a -> IO a
withArgobotsXstreams n act = do
  prev <- janusAbtNumXstreams
  bracket
    (janusAbtSetNumXstreams n)
    (\_ -> janusAbtSetNumXstreams prev >> pure ())
    (\_ -> act)
