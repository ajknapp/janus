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
import Data.Foldable
import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable
import Data.IORef
import Data.Maybe
import Data.Some
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
  ObjectFile :: FilePath -> FilePath -> Query (Maybe FilePath)
  SharedLibrary :: FilePath -> FilePath -> [FilePath] -> Query (Maybe FilePath)

deriving instance Show (Query a)

deriving instance Eq (Query a)

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt query =
    case query of
      CacheDir fp -> hashWithSalt salt fp
      ObjectFile dir fp -> hashWithSalt salt (hashWithSalt salt (dir </> fp) + 1)
      SharedLibrary dir sofile fps -> hashWithSalt salt (hashWithSalt salt (dir : sofile : fps) + 2)

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
    ObjectFile dir cfile -> do
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
              _ -> pure Nothing
    SharedLibrary dir sofile cfiles -> do
      ofiles <- fetchConcurrently (fmap (ObjectFile dir) cfiles)
      let success = all isJust ofiles
          sofilePath = dir </> sofile
      if not success
        then pure Nothing
        else liftIO $ do
          let ofiles' = catMaybes ofiles
          p <- spawnProcess (ccName GCC) $ ["-O3", "-fPIC", "-shared", "-flto", "-o", sofilePath] <> ofiles'
          waitForProcess p >>= \case
            ExitSuccess -> pure (Just sofilePath)
            _ -> pure Nothing

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

withJanusCDL :: FilePath -> [FilePath] -> (DL -> IO a) -> IO a
withJanusCDL dir cfiles k = do
  memoVar <- newIORef mempty
  result <- Rock.runTask (Rock.memoise memoVar janusRules) . Rock.fetch $ SharedLibrary dir "libjanus.so" cfiles
  case result of
    Just sofile -> withDL sofile [RTLD_LAZY, RTLD_LOCAL] k
    Nothing -> error "withJanusDL: compilation failed!"
