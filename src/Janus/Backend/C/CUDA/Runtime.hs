{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.Runtime where

import qualified Control.Concurrent.Classy as CC
import qualified Control.Concurrent.Classy.Async as CCA
import Control.Lens ((^.))
import Control.Monad (forM_, when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), MonadTrans (..), ReaderT (..), asks)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import qualified Data.Vector as V
import Data.Word (Word64)
import Foreign.C (CChar, CFile, CInt, CString, newCString)
import qualified Foreign.Marshal.Alloc
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek, poke))
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.CUDA.Types
import Janus.Backend.C.Target
import qualified Janus.Command.Memory as M
import Janus.Command.Format (hformat, hputString)
import Janus.Command.IO (fclose, fopen)
import Janus.Command.TicToc (Tic (..), Toc (..), tic, toc)
import qualified System.IO as SIO
import Test.DejaFu.Conc (ConcIO, Condition, MemType, Trace, runConcurrent)
import Test.DejaFu.Schedule (Scheduler, roundRobinSched)
import Test.DejaFu.Settings (defaultMemType)

data Barrier m = Barrier
  { _barrierMVar :: CC.MVar m (Int, [CC.MVar m ()]),
    _barrierResetCount :: Int
  }

newBarrier :: (CC.MonadConc m) => Int -> m (Barrier m)
newBarrier n = flip Barrier n <$> CC.newMVar (n, [])

syncBarrier :: (CC.MonadConc m) => Barrier m -> m ()
syncBarrier (Barrier b reset) = do
  m <- CC.modifyMVarMasked b \case
    (0, _) -> pure ((0, []), Nothing)
    (1, ms) -> forM_ ms (flip CC.putMVar ()) >> pure ((reset, []), Nothing)
    (n, ms) -> do
      m <- CC.newEmptyMVar
      pure ((n - 1, m : ms), Just m)
  case m of
    Just m' -> CC.takeMVar m'
    Nothing -> pure ()

data Thread m = Thread
  { _threadWarpBarrier :: Barrier m,
    _threadBlockBarrier :: Barrier m,
    _threadActive :: Bool,
    _threadInfo :: CUDAThreadInfo Identity,
    _threadFeatures :: CUDAFeatures
  }

newtype CUDAM (m :: K.Type -> K.Type) a = CUDAM {runCUDAM :: ReaderT (Thread m) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadReader (Thread m),
      MonadIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask
    )

instance MonadTrans CUDAM where
  lift = CUDAM . lift

instance (CC.MonadConc m) => CmdSync (CUDAM m) where
  syncWarp = ask >>= lift . syncBarrier . _threadWarpBarrier
  syncBlock = ask >>= lift . syncBarrier . _threadBlockBarrier

foo :: (CC.MonadConc m, MonadIO m) => CUDAM m ()
foo = do
  info <- _threadInfo <$> ask
  liftIO $
    print
      ( runIdentity $ _cudaThreadInfoThreadIdxX info,
        runIdentity $ _cudaThreadInfoThreadIdxY info,
        runIdentity $ _cudaThreadInfoThreadIdxZ info,
        runIdentity $ _cudaThreadInfoBlockIdxX info,
        runIdentity $ _cudaThreadInfoBlockIdxY info,
        runIdentity $ _cudaThreadInfoBlockIdxZ info
      )
  syncWarp
  liftIO $
    print
      ( runIdentity $ _cudaThreadInfoThreadIdxX info,
        runIdentity $ _cudaThreadInfoThreadIdxY info,
        runIdentity $ _cudaThreadInfoThreadIdxZ info,
        runIdentity $ _cudaThreadInfoBlockIdxX info,
        runIdentity $ _cudaThreadInfoBlockIdxY info,
        runIdentity $ _cudaThreadInfoBlockIdxZ info
      )
  syncWarp

launchCUDAMWithFeatures :: (CC.MonadConc m, MonadIO m) => CUDAFeatures -> CUlaunchConfig -> CUDAM m () -> m ()
launchCUDAMWithFeatures fs conf (CUDAM m) = do
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  let gridDimX = fromIntegral (conf ^. cuLaunchConfigGridDimX) :: Int
      gridDimY = fromIntegral (conf ^. cuLaunchConfigGridDimY) :: Int
      gridDimZ = fromIntegral (conf ^. cuLaunchConfigGridDimZ) :: Int
      blockDimX = fromIntegral (conf ^. cuLaunchConfigBlockDimX) :: Int
      blockDimY = fromIntegral (conf ^. cuLaunchConfigBlockDimY) :: Int
      blockDimZ = fromIntegral (conf ^. cuLaunchConfigBlockDimZ) :: Int
      blockCount = gridDimX * gridDimY * gridDimZ
      blockSize = blockDimX * blockDimY * blockDimZ
      warpsPerBlock = (blockSize + cudaWarpSize - 1) `quot` cudaWarpSize
  blockBarriers <- V.generateM blockCount $ \_ -> newBarrier blockSize
  warpBarriers <- V.generateM blockCount $ \_ ->
    V.generateM warpsPerBlock $ \w ->
      newBarrier (min cudaWarpSize (blockSize - w * cudaWarpSize))
  CCA.forConcurrently_ [(b, t) | b <- [0 .. blockCount - 1], t <- [0 .. blockSize - 1]] $ \(b, t) -> do
    let info =
          CUDAThreadInfo
            { _cudaThreadInfoBlockIdxX = Identity $ fromIntegral (b `rem` gridDimX),
              _cudaThreadInfoBlockIdxY = Identity $ fromIntegral ((b `quot` gridDimX) `rem` gridDimY),
              _cudaThreadInfoBlockIdxZ = Identity $ fromIntegral ((b `quot` (gridDimX * gridDimY)) `rem` gridDimZ),
              _cudaThreadInfoBlockDimX = Identity $ fromIntegral blockDimX,
              _cudaThreadInfoBlockDimY = Identity $ fromIntegral blockDimY,
              _cudaThreadInfoBlockDimZ = Identity $ fromIntegral blockDimZ,
              _cudaThreadInfoThreadIdxX = Identity $ fromIntegral (t `rem` blockDimX),
              _cudaThreadInfoThreadIdxY = Identity $ fromIntegral ((t `quot` blockDimX) `rem` blockDimY),
              _cudaThreadInfoThreadIdxZ = Identity $ fromIntegral ((t `quot` (blockDimX * blockDimY)) `rem` blockDimZ),
              _cudaThreadInfoGridDimX = Identity $ fromIntegral gridDimX,
              _cudaThreadInfoGridDimY = Identity $ fromIntegral gridDimY,
              _cudaThreadInfoGridDimZ = Identity $ fromIntegral gridDimZ
            }
        thread =
          Thread
            { _threadWarpBarrier = (warpBarriers V.! b) V.! (t `quot` cudaWarpSize),
              _threadBlockBarrier = blockBarriers V.! b,
              _threadActive = True,
              _threadInfo = info,
              _threadFeatures = fs
            }
    runReaderT m thread

launchCUDAM :: (CC.MonadConc m, MonadIO m) => CUlaunchConfig -> CUDAM m () -> m ()
launchCUDAM = launchCUDAMWithFeatures cudaSM80

--------------------------------------------------------------------------------
-- Shared CUDAM Command Helpers

cudaAskThreadInfo :: forall m e. (Monad m, Coercible (Identity Int32) (e Int32)) => CUDAM m (CUDAThreadInfo e)
cudaAskThreadInfo = asks (mapCUDAThreadInfo (coerce :: Identity Int32 -> e Int32) . _threadInfo)

cudaLocalThreadInfo ::
  forall m e a.
  (Monad m, Coercible (Identity Int32) (e Int32)) =>
  (CUDAThreadInfo e -> CUDAThreadInfo e) ->
  CUDAM m a ->
  CUDAM m a
cudaLocalThreadInfo f =
  local
    ( \th ->
        th
          { _threadInfo =
              mapCUDAThreadInfo
                (coerce :: e Int32 -> Identity Int32)
                (f (mapCUDAThreadInfo (coerce :: Identity Int32 -> e Int32) (_threadInfo th)))
          }
    )

cudaMalloc :: (MonadIO m) => Identity Int64 -> CUDAM m (Identity (Ptr ()))
cudaMalloc = liftIO . M.malloc @IO @Identity

cudaCalloc :: (MonadIO m) => Identity Int64 -> Identity Int64 -> CUDAM m (Identity (Ptr ()))
cudaCalloc n s = liftIO $ M.calloc @IO @Identity n s

cudaRealloc :: (MonadIO m) => Identity (Ptr ()) -> Identity Int64 -> CUDAM m (Identity (Ptr ()))
cudaRealloc p n = liftIO $ M.realloc @IO @Identity p n

cudaFree :: (MonadIO m) => Identity (Ptr ()) -> CUDAM m ()
cudaFree = liftIO . M.free @IO @Identity

cudaPeek :: (MonadIO m, Storable a) => Identity (Ptr a) -> CUDAM m (Identity a)
cudaPeek = liftIO . M.peek @IO @Identity

cudaPoke :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity a -> CUDAM m ()
cudaPoke p a = liftIO $ M.poke @IO @Identity p a

cudaPeekElemOff :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity Int64 -> CUDAM m (Identity a)
cudaPeekElemOff p i = liftIO $ M.peekElemOff @IO @Identity p i

cudaPokeElemOff :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity a -> Identity Int64 -> CUDAM m ()
cudaPokeElemOff p a i = liftIO $ M.pokeElemOff @IO @Identity p a i

cudaWithRefPtr ::
  (Catch.MonadMask m, MonadIO m, Storable a) =>
  IORef a ->
  (Identity (Ptr a) -> CUDAM m b) ->
  CUDAM m b
cudaWithRefPtr r f = do
  r' <- liftIO $ readIORef r
  Catch.bracket (liftIO malloc) (liftIO . Foreign.Marshal.Alloc.free) $ \p -> do
    liftIO $ poke p r'
    b <- f (Identity p)
    liftIO (peek p >>= writeIORef r)
    pure b

cudaSwitchCase_ :: (Monad m) => Identity Int64 -> [(Int, CUDAM m ())] -> CUDAM m () -> CUDAM m ()
cudaSwitchCase_ _ [] def = def
cudaSwitchCase_ i' ((i, c) : cs) def =
  if fromIntegral (runIdentity i') == i
    then c
    else cudaSwitchCase_ i' cs def

cudaIfThenElseM_ :: (Monad m) => Identity Bool -> CUDAM m () -> CUDAM m () -> CUDAM m ()
cudaIfThenElseM_ (Identity p) a b = if p then a else b

cudaFopen :: (MonadIO m) => Identity (Ptr CChar) -> Identity (Ptr CChar) -> CUDAM m (Identity (Ptr CFile))
cudaFopen h mode = liftIO $ fopen @IO @Identity h mode

cudaFclose :: (MonadIO m) => Identity (Ptr CFile) -> CUDAM m (Identity CInt)
cudaFclose = liftIO . fclose @IO @Identity

cudaWithString :: (Catch.MonadMask m, MonadIO m) => String -> (Identity CString -> CUDAM m a) -> CUDAM m a
cudaWithString s f =
  Catch.bracket (liftIO $ newCString s) (liftIO . Foreign.Marshal.Alloc.free) (f . Identity)

cudaHputString :: (MonadIO m) => Identity (Ptr CFile) -> Identity CString -> CUDAM m ()
cudaHputString h s = liftIO $ hputString @IO @Identity h s

cudaHformat :: (MonadIO m, Show a) => Identity (Ptr CFile) -> Identity a -> String -> CUDAM m ()
cudaHformat h a term = liftIO $ hformat @IO @Identity h a term

cudaNewRef :: (MonadIO m) => Identity a -> CUDAM m (IORef a)
cudaNewRef (Identity a) = liftIO $ newIORef a

cudaNewRef' :: (MonadIO m) => CUDAM m (IORef a)
cudaNewRef' = liftIO $ newIORef undefined

cudaReadRef :: (MonadIO m) => IORef a -> CUDAM m (Identity a)
cudaReadRef r = Identity <$> liftIO (readIORef r)

cudaWriteRef :: (MonadIO m) => IORef a -> Identity a -> CUDAM m ()
cudaWriteRef r (Identity a) = liftIO $ writeIORef r a

cudaWhileM :: (MonadIO m) => Identity Bool -> (IORef Bool -> CUDAM m ()) -> CUDAM m ()
cudaWhileM (Identity c) body = do
  r <- liftIO $ newIORef c
  let go = do
        r' <- liftIO $ readIORef r
        when r' $ body r >> go
  go

cudaTic :: (MonadIO m) => CUDAM m Word64
cudaTic = runIdentity . getTic <$> liftIO (tic @IO @Identity)

cudaToc :: (MonadIO m) => CUDAM m Word64
cudaToc = runIdentity . getToc <$> liftIO (toc @IO @Identity)

--------------------------------------------------------------------------------
-- Concrete IO and DejaFu CPU CUDA Monads & Launchers

newtype CUDAIOM a = CUDAIOM {getCUDAIOM :: CUDAM IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask,
      CmdSync
    )

newtype CUDADejaFuM a = CUDADejaFuM {getCUDADejaFuM :: CUDAM ConcIO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask,
      CmdSync
    )

launchCUDAIOMWithFeatures :: CUDAFeatures -> CUlaunchConfig -> CUDAIOM () -> IO ()
launchCUDAIOMWithFeatures fs conf (CUDAIOM m) = launchCUDAMWithFeatures fs conf m

launchCUDAIOM :: CUlaunchConfig -> CUDAIOM () -> IO ()
launchCUDAIOM conf m = do
  fs <- observeDefaultCUDAFeatures
  launchCUDAIOMWithFeatures fs conf m

launchCUDADejaFuMWithFeatures ::
  CUDAFeatures ->
  Scheduler s ->
  MemType ->
  s ->
  CUlaunchConfig ->
  CUDADejaFuM () ->
  IO (Either Condition (), s, Trace)
launchCUDADejaFuMWithFeatures fs sched mem s conf (CUDADejaFuM m) =
  runConcurrent sched mem s (launchCUDAMWithFeatures fs conf m)

launchCUDADejaFuMWith ::
  Scheduler s ->
  MemType ->
  s ->
  CUlaunchConfig ->
  CUDADejaFuM () ->
  IO (Either Condition (), s, Trace)
launchCUDADejaFuMWith = launchCUDADejaFuMWithFeatures cudaSM80

launchCUDADejaFuM :: CUlaunchConfig -> CUDADejaFuM () -> IO (Either Condition (), Trace)
launchCUDADejaFuM conf m = do
  (res, (), trace) <- launchCUDADejaFuMWith roundRobinSched defaultMemType () conf m
  pure (res, trace)

askCUDAIOFeatures :: CUDAIOM CUDAFeatures
askCUDAIOFeatures = CUDAIOM (asks _threadFeatures)

askCUDADejaFuFeatures :: CUDADejaFuM CUDAFeatures
askCUDADejaFuFeatures = CUDADejaFuM (asks _threadFeatures)
