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
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.Hash.SHA256 as SHA256
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
import GHC.Generics
import qualified Rock
import System.Directory
import System.Exit
import System.FilePath
import System.Posix hiding (createDirectory)
import System.Process

data JanusCC = Clang | GCC | NVCC
  deriving (Eq, Ord, Show, Generic)

ccName :: JanusCC -> String
ccName Clang = "clang"
ccName GCC = "gcc"
ccName NVCC = "nvcc"

data Query a where
  CacheDir :: FilePath -> Query (Maybe FilePath)
  CObjectFile :: FilePath -> FilePath -> Query (Maybe FilePath)
  CSharedLibrary :: FilePath -> FilePath -> [FilePath] -> Query (Maybe FilePath)

deriving instance Show (Query a)

deriving instance Eq (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt query =
    case query of
      CacheDir fp -> hashWithSalt salt fp
      CObjectFile dir fp -> hashWithSalt salt (hashWithSalt salt (dir </> fp) + 1)
      CSharedLibrary dir sofile fps -> hashWithSalt salt (hashWithSalt salt (dir : sofile : fps) + 2)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

janusRules :: Rock.Rules Query
janusRules key = do
  case key of
    CacheDir dir -> do
      e <- liftIO $ doesDirectoryExist dir
      if e
        then pure (Just dir)
        else do
          liftIO $ createDirectory dir
          pure (Just dir)
    CObjectFile dir cfile -> do
      let (root, _) = splitExtension cfile
          ofile = root <> ".o"
          ofilePath = dir </> ofile
      liftIO $ do
        ofileExists <- doesPathExist ofilePath
        if ofileExists
          then pure (Just ofilePath)
          else do
            p <- spawnProcess (ccName GCC) ["-O3", "-fPIC", "-flto", "-o", ofilePath, "-c", dir </> cfile]
            waitForProcess p >>= \case
              ExitSuccess -> pure (Just ofilePath)
              _exitFailure -> pure Nothing
    CSharedLibrary dir sofile cfiles -> do
      ofiles <- fetchConcurrently (fmap (CObjectFile dir) cfiles)
      let success = all isJust ofiles
          sofilePath = dir </> sofile
      if not success
        then pure Nothing
        else liftIO $ do
          let ofiles' = catMaybes ofiles
          sofileExists <- liftIO $ doesPathExist sofilePath
          if sofileExists then pure (Just sofilePath) else do
            p <- spawnProcess (ccName GCC) $ ["-O3", "-fPIC", "-shared", "-flto", "-o", sofilePath] <> ofiles'
            waitForProcess p >>= \case
              ExitSuccess -> pure (Just sofilePath)
              _exitFailure -> pure Nothing

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

acquireJanusCDL :: FilePath -> [FilePath] -> IO DL
acquireJanusCDL dir cfiles = do
  memoVar <- newIORef mempty
  let filesHash = B16.encode (SHA256.hash $ Text.encodeUtf8 (foldMap Text.pack cfiles))
      sofile_ = unpack filesHash <> ".so"
  result <- Rock.runTask (Rock.memoise memoVar janusRules) . Rock.fetch $ CSharedLibrary dir sofile_ cfiles
  case result of
    Just sofile -> dlopen sofile [RTLD_LAZY, RTLD_LOCAL]
    Nothing -> error "withJanusDL: compilation failed!"

releaseJanusCDL :: DL -> IO ()
releaseJanusCDL = dlclose
