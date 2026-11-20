{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Janus.Backend.C.Build where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (unpack)
import Data.Foldable
import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Some
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Traversable
import Foreign.C
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.Target
import qualified Rock
import System.Directory
import System.Exit
import System.FilePath
import System.IO (hClose, openBinaryTempFile, openTempFile)
import System.Posix hiding (createDirectory)
import System.Process

data JanusCC = Clang | GCC
  deriving (Eq, Ord, Show, Generic)

data CBuildConfig = CBuildConfig
  { _cBuildConfigCacheDir :: FilePath
  }

$(makeLenses ''CBuildConfig)

defaultCBuildConfig :: CBuildConfig
defaultCBuildConfig = CBuildConfig "_cache"

ccName :: JanusCC -> String
ccName Clang = "clang"
ccName GCC = "gcc"

data Query a where
  CacheDir :: FilePath -> Query (Maybe FilePath)
  CObjectFile :: FilePath -> FilePath -> [String] -> Query (Maybe FilePath)
  CSharedLibrary :: FilePath -> FilePath -> [FilePath] -> [String] -> [String] -> Query (Maybe FilePath)
  CUPTXFile :: FilePath -> FilePath -> Int -> Int -> Query (Maybe FilePath)
  CUBinFile :: FilePath -> FilePath -> [FilePath] -> Int -> Int -> Query (Maybe FilePath)

deriving instance Show (Query a)

deriving instance Eq (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt query =
    case query of
      CacheDir fp -> hashWithSalt salt fp
      CObjectFile dir fp cflags ->
        hashWithSalt salt (hashWithSalt (hashWithSalt salt (dir </> fp)) cflags + 1)
      CSharedLibrary dir sofile fps cflags ldflags ->
        hashWithSalt salt (hashWithSalt (hashWithSalt (hashWithSalt salt (dir : sofile : fps)) cflags) ldflags + 2)
      CUPTXFile dir ptxfile major minor ->
        let h1 = hashWithSalt salt [dir, ptxfile]
            h2 = hashWithSalt salt (hashWithSalt h1 major)
        in hashWithSalt salt (hashWithSalt h2 minor)
      CUBinFile dir cubinfile ptxfiles major minor ->
        let h1 = hashWithSalt salt (dir:cubinfile:ptxfiles)
            h2 = hashWithSalt salt (hashWithSalt h1 major)
        in hashWithSalt salt (hashWithSalt h2 minor)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

getCUDAComputeVersion :: CUdevice -> IO (Int, Int)
getCUDAComputeVersion dev = bracket malloc free $ \ip -> do
  major <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR dev >> peek ip
  minor <- cuDeviceGetAttribute ip CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR dev >> peek ip
  pure (fromIntegral major, fromIntegral minor)

janusRules :: Rock.Rules Query
janusRules key = do
  case key of
    CacheDir dir -> do
      liftIO $ createDirectoryIfMissing True dir
      pure (Just dir)

    CObjectFile dir cfile cflags -> do
      Rock.fetch (CacheDir dir) >>= \case
        Just _ -> do
          let (root, _) = splitExtension cfile
              flagsSuffix
                | null cflags = ""
                | otherwise =
                    "_" <> unpack (B16.encode (SHA256.hash (Text.encodeUtf8 (Text.pack (unwords cflags)))))
              ofile = root <> flagsSuffix <> ".o"
              ofilePath = dir </> ofile
          liftIO $ do
            ofileExists <- doesPathExist ofilePath
            if ofileExists
              then pure (Just ofilePath)
              else
                bracketOnError
                  (openTempFile dir "janus_obj.o")
                  (\(tmpPath, hndl) -> hClose hndl >> removePathForcibly tmpPath)
                  $ \(tmpPath, hndl) -> do
                    hClose hndl
                    p <- spawnProcess (ccName GCC) (["-O3", "-fPIC", "-flto"] <> cflags <> ["-o", tmpPath, "-c", dir </> cfile])
                    waitForProcess p >>= \case
                      ExitSuccess -> renameFile tmpPath ofilePath >> pure (Just ofilePath)
                      _exitFailure -> removePathForcibly tmpPath >> pure Nothing
        Nothing -> pure Nothing

    CSharedLibrary dir sofile cfiles cflags ldflags -> do
      ofiles <- fetchConcurrently (fmap (\cf -> CObjectFile dir cf cflags) cfiles)
      let success = all isJust ofiles
          sofilePath = dir </> sofile
      if not success
        then pure Nothing
        else liftIO $ do
          let ofiles' = catMaybes ofiles
          sofileExists <- liftIO $ doesPathExist sofilePath
          if sofileExists then pure (Just sofilePath) else
            bracketOnError
              (openTempFile dir "janus_lib.so")
              (\(tmpPath, hndl) -> hClose hndl >> removePathForcibly tmpPath)
              $ \(tmpPath, hndl) -> do
                hClose hndl
                p <- spawnProcess (ccName GCC) $ ["-O3", "-fPIC", "-shared", "-flto"] <> cflags <> ["-o", tmpPath] <> ofiles' <> ldflags
                waitForProcess p >>= \case
                  ExitSuccess -> renameFile tmpPath sofilePath >> pure (Just sofilePath)
                  _exitFailure -> removePathForcibly tmpPath >> pure Nothing

    CUPTXFile dir cufile major minor -> do
      Rock.fetch (CacheDir dir) >>= \case
        Just _ -> do
          let (root, _) = splitExtension cufile
              ptxfile = root <> "_sm_" <> show major <> show minor <> ".ptx"
              ptxfilePath = dir </> ptxfile
          liftIO $ do
            ptxFileExists <- doesPathExist ptxfilePath
            if ptxFileExists
              then pure (Just ptxfilePath)
              else
                bracketOnError
                  (openTempFile dir "janus_ptx.ptx")
                  (\(tmpPath, hndl) -> hClose hndl >> removePathForcibly tmpPath)
                  $ \(tmpPath, hndl) -> do
                    hClose hndl
                    let arch = "-arch=sm_" <> show major <> show minor
                    p <- spawnProcess "nvcc" ["-O3", "-rdc=true", "--ptx", arch, "-o", tmpPath, dir </> cufile]
                    waitForProcess p >>= \case
                      ExitSuccess -> renameFile tmpPath ptxfilePath >> pure (Just ptxfilePath)
                      _exitFailure -> removePathForcibly tmpPath >> pure Nothing
        Nothing -> pure Nothing

    CUBinFile dir cubinfile cufiles major minor -> do
      ptxfiles <- fetchConcurrently (fmap (\cf -> CUPTXFile dir cf major minor) cufiles)
      let success = all isJust ptxfiles
          cubinfilePath = dir </> cubinfile
      if not success then pure Nothing else liftIO $ do
        cubinExists <- doesPathExist cubinfilePath
        if cubinExists then pure (Just cubinfilePath) else do
          let arch = "sm_" <> show major <> show minor
          bracket malloc free $ \h -> withCString "-O3" $ \o3 -> withCString ("-arch=" <> arch) $ \arch' -> withArray [o3,arch'] $ \nvargs -> do
            withNvJitLink h 2 nvargs $ \nvjh -> do
              traverse_ (\s -> withCString s $ \s' -> nvJitLinkAddFile nvjh NVJITLINK_INPUT_PTX s') (catMaybes ptxfiles)
              nvJitLinkComplete nvjh
              sz <- nvJitLinkGetLinkedCubinSize nvjh
              bracket (mallocBytes (fromIntegral sz)) free $ \cubin -> do
                nvJitLinkGetLinkedCubin nvjh cubin
                bs <- BS.unsafePackCStringLen (castPtr cubin, fromIntegral sz)
                bracketOnError
                  (openBinaryTempFile dir "janus_cubin.cubin")
                  (\(tmpPath, hndl) -> hClose hndl >> removePathForcibly tmpPath)
                  $ \(tmpPath, hndl) -> do
                    BS.hPut hndl bs
                    hClose hndl
                    renameFile tmpPath cubinfilePath
                pure (Just cubinfilePath)

fetchConcurrently :: (Traversable g, Hashable (Some f), GEq f) => g (f a) -> Rock.Task f (g a)
fetchConcurrently queries = do
  workQueue <- liftIO newTQueueIO
  jobs <- for queries $ \q -> liftIO $ do
    r <- newIORef $ error "fetchConcurrently: query not evaluated"
    pure (q, r)
  liftIO . atomically $ for_ jobs (writeTQueue workQueue)
  liftBaseWith $ \runInIO -> do
    let workerThread =
          atomically (tryReadTQueue workQueue) >>= \case
            Just (fa, r) -> do
              a <- runInIO $ Rock.fetch fa
              writeIORef r a
              workerThread
            Nothing -> pure ()
    n <- getNumCapabilities
    replicateConcurrently_ (max n 8) workerThread
    for jobs (readIORef . snd)

acquireJanusCDLWithFlags :: FilePath -> [String] -> [String] -> [FilePath] -> IO DL
acquireJanusCDLWithFlags dir cflags ldflags cfiles = do
  memoVar <- newIORef mempty
  let hashPayload = foldMap Text.pack cfiles <> Text.pack ("|" <> unwords cflags <> "|" <> unwords ldflags)
      filesHash = B16.encode (SHA256.hash $ Text.encodeUtf8 hashPayload)
      sofile_ = unpack filesHash <> ".so"
  result <- Rock.runTask (Rock.memoise memoVar janusRules) . Rock.fetch $ CSharedLibrary dir sofile_ cfiles cflags ldflags
  case result of
    Just sofile -> dlopen sofile [RTLD_LAZY, RTLD_LOCAL]
    Nothing -> error "withJanusDL: compilation failed!"

acquireJanusCDL :: FilePath -> [FilePath] -> IO DL
acquireJanusCDL dir = acquireJanusCDLWithFlags dir [] []

releaseJanusCDL :: DL -> IO ()
releaseJanusCDL = dlclose

acquireJanusCFunPtrWithFlags :: FilePath -> [String] -> [String] -> [FilePath] -> String -> IO (ForeignPtr ())
acquireJanusCFunPtrWithFlags dir cflags ldflags cfiles sym =
  mask $ \restore -> do
    dl <- restore (acquireJanusCDLWithFlags dir cflags ldflags cfiles)
    ( do
        fp <- dlsym dl sym
        if fp == nullFunPtr
          then error ("acquireJanusCFunPtrWithFlags: null function pointer for " <> sym)
          else FC.newForeignPtr (castFunPtrToPtr fp) (releaseJanusCDL dl)
      )
      `onException` releaseJanusCDL dl

acquireJanusCFunPtr :: FilePath -> [FilePath] -> String -> IO (ForeignPtr ())
acquireJanusCFunPtr dir = acquireJanusCFunPtrWithFlags dir [] []

acquireJanusCUmoduleWithFeatures :: CUDAFeatures -> CUdevice -> CUctx -> FilePath -> [FilePath] -> IO (Ptr CUmodule)
acquireJanusCUmoduleWithFeatures reqFeatures dev _ctx dir cfiles = do
  observed <- observeCUDAFeatures dev
  ensureCUDAAdmitted observed reqFeatures
  let major = cudaSMMajor reqFeatures
      minor = cudaSMMinor reqFeatures
  memoVar <- newIORef mempty
  let hashPayload = foldMap Text.pack cfiles <> Text.pack ("|sm_" <> show major <> show minor)
      filesHash = B16.encode (SHA256.hash $ Text.encodeUtf8 hashPayload)
      cubinfile_ = unpack filesHash <> ".cubin"
  result <- Rock.runTask (Rock.memoise memoVar janusRules) . Rock.fetch $ CUBinFile dir cubinfile_ cfiles major minor
  case result of
    Just cubinfile -> do
      cubin <- BS.readFile cubinfile
      pmod <- malloc
      handle (\(e :: SomeException) -> free pmod >> throwIO e) $ BS.unsafeUseAsCString cubin $ \p ->
        cuModuleLoadData pmod (castPtr p) >> pure pmod
    Nothing -> error "withJanusCUmodule: compilation failed!"

-- for use with tasty
acquireJanusCUmodule :: CUdevice -> CUctx -> FilePath -> [FilePath] -> IO (Ptr CUmodule)
acquireJanusCUmodule dev ctx dir cfiles = do
  observed <- observeCUDAFeatures dev
  acquireJanusCUmoduleWithFeatures observed dev ctx dir cfiles

-- for use with tasty
releaseJanusCUmodule :: Ptr CUmodule -> IO ()
releaseJanusCUmodule p = peek p >>= cuModuleUnload >> free p

withJanusCUmoduleWithFeatures :: CUDAFeatures -> CUdevice -> CUctx -> FilePath -> [FilePath] -> (Ptr CUmodule -> IO a) -> IO a
withJanusCUmoduleWithFeatures fs dev ctx path cufiles =
  bracket (acquireJanusCUmoduleWithFeatures fs dev ctx path cufiles) releaseJanusCUmodule

withJanusCUmodule :: CUdevice -> CUctx -> FilePath -> [FilePath] -> (Ptr CUmodule -> IO a) -> IO a
withJanusCUmodule dev ctx path cufiles = bracket (acquireJanusCUmodule dev ctx path cufiles) releaseJanusCUmodule
