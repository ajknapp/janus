{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.CUDA where

import qualified Control.Concurrent.Classy as CC
import qualified Control.Concurrent.Classy.Async as CCA
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import qualified Control.Monad.Catch as Catch
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State (StateT (..))
import Data.Bits (Bits)
import Data.Coerce
import Data.Complex
import Data.IORef
import Data.Int
import qualified Data.Kind as K
import Data.Loc
import Data.Proxy
import qualified Data.Vector as V
import Data.Word (Word64)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.CUDA.Foreign
import qualified Janus.Command.Array as A
import Janus.Command.Case
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.TicToc
import Janus.Command.While
import Janus.Expression.Bits
import Janus.Expression.Bool
import Janus.Expression.Cast
import Janus.Expression.Cond
import Janus.Expression.Eq
import Janus.Expression.Extract
import Janus.Expression.Inject
import Janus.Expression.Integral
import Janus.Expression.Let
import Janus.Expression.Math
import Janus.Expression.MurmurHash
import Janus.Expression.Ord
import Janus.Typed
import Language.C.Quote
import qualified System.IO as SIO
import Test.DejaFu.Conc (ConcIO, Condition, MemType, Trace, runConcurrent)
import Test.DejaFu.Schedule (Scheduler, roundRobinSched)
import Test.DejaFu.Settings (defaultMemType)

cudaArgMaxSize :: Int
cudaArgMaxSize = 4096

-- TODO figure out why the first four bytes of the device pointer aren't written sometimes
-- this happened but I haven't been able to reproduce it for months...
-- hopefully this was because it was fixed upstream in CUDA
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
  } deriving (Generic)

deriving instance Show (e Int32) => Show (CUDAThreadInfo e)

$(makeLenses ''CUDAThreadInfo)

newtype JanusCUDA a = JanusCUDA {getJanusCUDA :: JanusC a}

newtype JanusCUDAM a = JanusCUDAM {getJanusCUDAM :: ReaderT (CUDAThreadInfo JanusCUDA) JanusCM a}
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadReader (CUDAThreadInfo JanusCUDA))

runJanusCUDAM :: JanusCUDAM a -> JanusCM a
runJanusCUDAM (JanusCUDAM m) = runReaderT m magicThreadInfo
  where
    magicCUDAVar var mem = JanusCUDA $ JanusC $ pure $ RVal (Member (Var (Id var noLoc) noLoc) (Id mem noLoc) noLoc)
    magicThreadInfo = CUDAThreadInfo
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

type instance JanusTyped JanusCUDA = JanusCTyped

deriving via JanusC a instance (JanusLitC a, Num a) => Num (JanusCUDA a)

deriving via JanusC a instance (JanusLitC a, Fractional a) => Fractional (JanusCUDA a)

deriving via JanusC Float instance Floating (JanusCUDA Float)

deriving via JanusC Double instance Floating (JanusCUDA Double)

deriving via JanusC (Complex Float) instance Floating (JanusCUDA (Complex Float))

deriving via JanusC (Complex Double) instance Floating (JanusCUDA (Complex Double))

deriving via JanusC instance ExpBool JanusCUDA

instance (ExpBits JanusC a) => ExpBits JanusCUDA a where
  iand = coerce (iand @JanusC @a)
  ior = coerce (ior @JanusC @a)
  ieor = coerce (ieor @JanusC @a)
  inot = coerce (inot @JanusC @a)
  lshift = coerce (lshift @JanusC @a)
  rshift = coerce (rshift @JanusC @a)

instance (ExpBoolCast JanusC a) => ExpBoolCast JanusCUDA a where
  toBool = coerce (toBool @JanusC @a)
  fromBool = coerce (fromBool @JanusC @a)

instance (ExpFloatingCast JanusC a b) => ExpFloatingCast JanusCUDA a b where
  toFloating = coerce (toFloating @JanusC @a @b)

instance (ExpIntegralCast JanusC a b) => ExpIntegralCast JanusCUDA a b where
  toIntegral = coerce (toIntegral @JanusC @a @b)

deriving via JanusC instance ExpCond JanusCUDA

instance (ExpEq JanusC a) => ExpEq JanusCUDA a where
  eq = coerce (eq @JanusC @a)
  ne = coerce (ne @JanusC @a)

instance (ExpExtract JanusC a) => ExpExtract JanusCUDA a where
  extract = coerce (extract @JanusC @a)

instance (ExpInject JanusC a) => ExpInject JanusCUDA a where
  inject = coerce (inject @JanusC @a)

instance (Num a, JanusLitC a, ExpIntegral JanusC a) => ExpIntegral JanusCUDA a where
  remainder = coerce (remainder @JanusC @a)
  quotient = coerce (quotient @JanusC @a)

deriving via JanusC instance ExpLet JanusCUDA

deriving via JanusC a instance (FMA (JanusC a), JanusLitC a, Num a) => FMA (JanusCUDA a)

deriving via JanusC a instance (Hypot (JanusC a), JanusLitC a, Floating (JanusCUDA a)) => Hypot (JanusCUDA a)

instance (ExpMurmurHash JanusC a) => ExpMurmurHash JanusCUDA a where
  murmurHashWithSalt = coerce (murmurHashWithSalt @JanusC @a)

instance (ExpOrd JanusC a) => ExpOrd JanusCUDA a where
  lt = coerce (lt @JanusC @a)
  le = coerce (le @JanusC @a)
  gt = coerce (gt @JanusC @a)
  ge = coerce (ge @JanusC @a)

instance (JanusCTyped a, A.ExpSized JanusC a) => A.ExpSized JanusCUDA a where
  sizeOf = coerce (A.sizeOf @JanusC @a)
  alignOf = coerce (A.alignOf @JanusC @a)

instance (A.ExpPtr JanusC a) => A.ExpPtr JanusCUDA a where
  nullPtr = coerce (A.nullPtr @JanusC @a)
  ptrAdd = coerce (A.ptrAdd @JanusC @a)
  ptrIndex = coerce (A.ptrIndex @JanusC @a)

instance A.CmdMem JanusCUDAM JanusCUDA where
  malloc n = JanusCUDAM $ lift $ coerce (A.malloc @JanusCM @JanusC) n
  calloc n m = JanusCUDAM $ lift $ coerce (A.calloc @JanusCM @JanusC) n m
  realloc p n = JanusCUDAM $ lift $ coerce (A.realloc @JanusCM @JanusC) p n
  free p = JanusCUDAM $ lift $ coerce (A.free @JanusCM @JanusC) p

instance (A.ExpSized JanusCUDA a, JanusCTyped a) => A.CmdStorable JanusCUDAM JanusCUDA a where
  peek p = JanusCUDAM $ lift $ coerce (A.peek @JanusCM @JanusC @a) p
  poke p a = JanusCUDAM $ lift $ coerce (A.poke @JanusCM @JanusC @a) p a
  peekElemOff p n = JanusCUDAM $ lift $ coerce (A.peekElemOff @JanusCM @JanusC @a) p n
  pokeElemOff p a n = JanusCUDAM $ lift $ coerce (A.pokeElemOff @JanusCM @JanusC @a) p a n

instance CmdCase JanusCUDAM JanusCUDA where
  switchCase_ tag cases def = JanusCUDAM $ lift $ switchCase_ (getJanusCUDA tag) (fmap (\(i,m) -> (i,runJanusCUDAM m)) cases) (runJanusCUDAM def)

instance CmdCond JanusCUDAM JanusCUDA where
  ifThenElseM_ b t f = JanusCUDAM $ lift $ ifThenElseM_ (getJanusCUDA b) (runJanusCUDAM t) (runJanusCUDAM f)

instance CmdIO JanusCUDAM JanusCUDA where
  stdout = coerce (stdout @JanusCM @JanusC)
  stderr = coerce (stderr @JanusCM @JanusC)
  fopen h m = JanusCUDAM $ lift $ JanusCUDA <$> fopen @JanusCM @JanusC (getJanusCUDA h) (getJanusCUDA m)
  fclose h = JanusCUDAM $ lift $ JanusCUDA <$> fclose (getJanusCUDA h)

instance CmdString JanusCUDAM JanusCUDA where
  withString s f = JanusCUDAM $ lift $ withString s (runJanusCUDAM . f . JanusCUDA)

instance CmdPutString JanusCUDAM JanusCUDA where
  hputString h s = JanusCUDAM $ lift $ hputString (getJanusCUDA h) (getJanusCUDA s)

instance (CmdFormat JanusCM JanusC a) => CmdFormat JanusCUDAM JanusCUDA a where
  hformat h a s = JanusCUDAM $ lift $ hformat (getJanusCUDA h) (getJanusCUDA a) s

newtype JanusCUDARef a = JanusCUDARef {getJanusCUDARef :: JanusCRef a}

instance CmdRef JanusCUDAM JanusCUDA where
  type Ref JanusCUDAM JanusCUDA a = JanusCUDARef a
  letM (JanusCUDA a) = JanusCUDAM $ lift $ JanusCUDA <$> letM a
  newRef (JanusCUDA a) = JanusCUDAM $ lift $ JanusCUDARef <$> newRef a
  newRef' = JanusCUDAM $ lift $ JanusCUDARef <$> newRef'
  readRef (JanusCUDARef r) = JanusCUDAM $ lift $ JanusCUDA <$> readRef r
  writeRef (JanusCUDARef r) (JanusCUDA a) = JanusCUDAM $ lift $ writeRef r a

instance CmdRange JanusCUDAM JanusCUDA where
  rangeM l u f = JanusCUDAM $ lift $ rangeM (getJanusCUDA l) (getJanusCUDA u) (runJanusCUDAM . f . JanusCUDA)

instance CmdWhile JanusCUDAM JanusCUDA where
  whileM i f = JanusCUDAM $ lift $ whileM (getJanusCUDA i) (runJanusCUDAM . f . JanusCUDARef)

--------------------------------------------------------------------------------

instance JanusCBackendTypes JanusCUDA JanusCUDAM where
  toJanusC = getJanusCUDA
  fromJanusC = JanusCUDA
  toJanusCM = runJanusCUDAM
  fromJanusCM = JanusCUDAM . lift

class JanusCUDAParam r where
  jcudaparam :: String -> Int -> JanusCUDAM [Param] -> JanusCBackend -> r -> [JCFunc]

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDA a) where
  jcudaparam = jcparamImpl (Proxy @JanusCUDA)

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDAM (JanusCUDA a)) where
  jcudaparam = jcparamImpl (Proxy @JanusCUDA)

instance JanusCUDAParam (JanusCUDAM ()) where
  jcudaparam = jcparamImpl (Proxy @JanusCUDA)

instance (JanusCTyped a, JanusCUDAParam r, JanusCParamImpl JanusCUDA JanusCUDAM r) => JanusCUDAParam (JanusCUDA a -> r) where
  jcudaparam = jcparamImpl (Proxy @JanusCUDA)

type family JanusCUDAEval r where
  JanusCUDAEval (JanusCUDAM ()) = IO ()
  JanusCUDAEval (JanusCUDA a -> r) = JanusCUDAEval' a -> JanusCUDAEval r

showJanusCUDA :: forall a. (JanusCUDAParam a) => String -> a -> String
showJanusCUDA name a =
  let jcs = jcudaparam name 0 (pure []) JCUDA a
      strChar 0 = '\n'
      strChar 81 = '\n'
      strChar _ = '-'
      str = fmap strChar [0 .. 81 :: Int]
   in foldMap ((<> str) . renderJCFunc) jcs

printJanusCUDA :: (JanusCUDAParam r) => r -> IO ()
printJanusCUDA = putStrLn . showJanusCUDA "janus_main"

-- this is required for GHC to infer enough injectivity for jcudaeval to compile
type family JanusCUDAEval' a where
  JanusCUDAEval' (Ptr a) = CUdeviceptr
  JanusCUDAEval' a = a

class JanusCUDAEvaluate r where
  jcudaeval :: CUfunction -> Ptr () -> Int -> Ptr (Ptr ()) -> Int -> IO () -> CUlaunchConfig -> r -> JanusCUDAEval r

instance JanusCUDAEvaluate (JanusCUDAM ()) where
  jcudaeval fn _ _ argPtrs _ pack args _ =
    pack
      >> cuLaunchKernel
        fn
        (args ^. cuLaunchConfigGridDimX)
        (args ^. cuLaunchConfigGridDimY)
        (args ^. cuLaunchConfigGridDimZ)
        (args ^. cuLaunchConfigBlockDimX)
        (args ^. cuLaunchConfigBlockDimY)
        (args ^. cuLaunchConfigBlockDimZ)
        (args ^. cuLaunchConfigSharedMemBytes)
        (args ^. cuLaunchConfigStream)
        argPtrs
        nullPtr

instance (Storable (JanusCUDAEval' a), JanusCUDAEvaluate r) => JanusCUDAEvaluate (JanusCUDA a -> r) where
  jcudaeval fn argBuf nbuf argPtrs nptrs pack cuargs k a =
    jcudaeval
      fn
      argBuf
      (nbuf + sizeOf proxy)
      argPtrs
      (nptrs + 1)
      pack'
      cuargs
      (k $ JanusCUDA $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)
    where
      proxy = undefined :: JanusCUDAEval' a
      pack' = do
        if nbuf >= cudaArgMaxSize
          then error "jcudaeval: too much space used"
          else do
            let pa = castPtr (argBuf `plusPtr` nbuf)
            poke @(JanusCUDAEval' a) pa a
            pokeElemOff argPtrs nptrs (castPtr pa)
            pack

withJanusCUDAFn ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDAFn dev ctx config f k = do
  let dir = config ^. cBuildConfigCacheDir
  cufiles <- writeJanusCFiles JCUDA dir (jcudaparam "janus_main" 0 (pure []) JCUDA f)
  withJanusCUmodule dev ctx dir cufiles $ \pmod ->
    bracket (mallocBytes cudaArgMaxSize) free $ \argbuf ->
      bracket (mallocBytes (cudaArgMaxSize * sizeOf (undefined :: Ptr ()))) free $ \argPtrs ->
        alloca $ \pfn -> withCString "janus_main" $ \s -> do
          m <- peek pmod
          fn <- cuModuleGetFunction pfn m s >> peek pfn
          k fn (\kernelArgs -> jcudaeval fn argbuf 0 argPtrs 0 (pure ()) kernelArgs f)

withJanusCUDA ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  ((CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDA dev ctx config f k = withJanusCUDAFn dev ctx config f (const k)

-- -- class CmdCudaSynchronized m e | m -> e where
-- --   synchronized :: (CUDAThreadInfo e -> m ()) -> CUDAT e m ()

-- -- newtype CUDAT e m a = CUDAT {getCUDAT :: ReaderT (CUDAThreadInfo e) m a}
-- --   deriving newtype (Functor, Applicative, Monad, MonadFix, MonadReader (CUDAThreadInfo e), MonadTrans)

-- -- instance CmdCudaSynchronized JanusCUDAM JanusCUDA where
-- --   synchronized f = ask >>= lift . f >> lift syncthreads
-- --     where syncthreads :: JanusCUDAM ()
-- --           syncthreads = JanusCUDAM $ janusCFFICall Nothing "__syncthreads"

-- -- instance CmdCudaSynchronized JanusCM JanusC where
-- --   synchronized f = do
-- --     info <- ask
-- --     lift $ rangeM (0 :: JanusC Int64) (toIntegral $ info ^. cudaThreadInfoBlockDimX) $ \idxX ->
-- --       rangeM 0 (toIntegral $ info ^. cudaThreadInfoBlockDimY) $ \idxY ->
-- --         rangeM 0 (toIntegral $ info ^. cudaThreadInfoBlockDimZ) $ \idxZ ->
-- --           f $
-- --             info
-- --               & cudaThreadInfoThreadIdxX .~ toIntegral idxX
-- --               & cudaThreadInfoThreadIdxY .~ toIntegral idxY
-- --               & cudaThreadInfoThreadIdxZ .~ toIntegral idxZ

-- -- runCUDAT_ :: CUDAT JanusCUDA JanusCUDAM () -> JanusCUDAM ()
-- -- runCUDAT_ (CUDAT m) =
-- --   runReaderT
-- --     m
-- --     CUDAThreadInfo
-- --       { _cudaThreadInfoBlockDimX = magicCUDAVar "blockDim" "x",
-- --         _cudaThreadInfoBlockDimY = magicCUDAVar "blockDim" "y",
-- --         _cudaThreadInfoBlockDimZ = magicCUDAVar "blockDim" "z",
-- --         _cudaThreadInfoBlockIdxX = magicCUDAVar "blockIdx" "x",
-- --         _cudaThreadInfoBlockIdxY = magicCUDAVar "blockIdx" "y",
-- --         _cudaThreadInfoBlockIdxZ = magicCUDAVar "blockIdx" "z",
-- --         _cudaThreadInfoThreadIdxX = magicCUDAVar "threadIdx" "x",
-- --         _cudaThreadInfoThreadIdxY = magicCUDAVar "threadIdx" "y",
-- --         _cudaThreadInfoThreadIdxZ = magicCUDAVar "threadIdx" "z",
-- --         _cudaThreadInfoGridDimX = magicCUDAVar "gridDim" "x",
-- --         _cudaThreadInfoGridDimY = magicCUDAVar "gridDim" "y",
-- --         _cudaThreadInfoGridDimZ = magicCUDAVar "gridDim" "z"
-- --       }
-- --   where
-- --     magicCUDAVar var mem = JanusCUDA $ JanusC $ pure $ RVal (Member (Var (Id var noLoc) noLoc) (Id mem noLoc) noLoc)

-- -- runEmulatedCUDAT_ ::
-- --   ( JanusTyped e Int64,
-- --     ExpOrd e Int64,
-- --     CmdRange m e,
-- --     Num (e Int64),
-- --     Num (e Int32),
-- --     ExpIntegralCast e Int64 Int32
-- --   ) =>
-- --   CUlaunchConfig ->
-- --   CUDAT e m () ->
-- --   m ()
-- -- runEmulatedCUDAT_ conf (CUDAT m) =
-- --   rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimX) $ \blockIdxX ->
-- --     rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimY) $ \blockIdxY ->
-- --       rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimZ) $ \blockIdxZ ->
-- --         runReaderT m $
-- --           CUDAThreadInfo
-- --             { _cudaThreadInfoBlockIdxX = toIntegral blockIdxX,
-- --               _cudaThreadInfoBlockIdxY = toIntegral blockIdxY,
-- --               _cudaThreadInfoBlockIdxZ = toIntegral blockIdxZ,
-- --               _cudaThreadInfoBlockDimX = fromIntegral (conf ^. cuLaunchConfigBlockDimX),
-- --               _cudaThreadInfoBlockDimY = fromIntegral (conf ^. cuLaunchConfigBlockDimY),
-- --               _cudaThreadInfoBlockDimZ = fromIntegral (conf ^. cuLaunchConfigBlockDimZ),
-- --               _cudaThreadInfoThreadIdxX = undefined,
-- --               _cudaThreadInfoThreadIdxY = undefined,
-- --               _cudaThreadInfoThreadIdxZ = undefined,
-- --               _cudaThreadInfoGridDimX = fromIntegral (conf ^. cuLaunchConfigGridDimX),
-- --               _cudaThreadInfoGridDimY = fromIntegral (conf ^. cuLaunchConfigGridDimY),
-- --               _cudaThreadInfoGridDimZ = fromIntegral (conf ^. cuLaunchConfigGridDimZ)
-- --             }

cudaWarpSize :: Int
cudaWarpSize = 32

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
    _threadInfo :: CUDAThreadInfo Identity
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

class CmdSync m where
  syncWarp :: m ()
  syncBlock :: m ()

instance (CC.MonadConc m) => CmdSync (CUDAM m) where
  syncWarp = ask >>= lift . syncBarrier . _threadWarpBarrier
  syncBlock = ask >>= lift . syncBarrier . _threadBlockBarrier

instance CmdSync JanusCUDAM where
  syncWarp = JanusCUDAM $ lift $ janusCFFICall Nothing "__syncwarp"
  syncBlock = JanusCUDAM $ lift $ janusCFFICall Nothing "__syncthreads"

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

launchCUDAM :: (CC.MonadConc m, MonadIO m) => CUlaunchConfig -> CUDAM m () -> m ()
launchCUDAM conf (CUDAM m) = do
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
              _threadInfo = info
            }
    runReaderT m thread

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
cudaMalloc = liftIO . A.malloc @IO @Identity

cudaCalloc :: (MonadIO m) => Identity Int64 -> Identity Int64 -> CUDAM m (Identity (Ptr ()))
cudaCalloc n s = liftIO $ A.calloc @IO @Identity n s

cudaRealloc :: (MonadIO m) => Identity (Ptr ()) -> Identity Int64 -> CUDAM m (Identity (Ptr ()))
cudaRealloc p n = liftIO $ A.realloc @IO @Identity p n

cudaFree :: (MonadIO m) => Identity (Ptr ()) -> CUDAM m ()
cudaFree = liftIO . A.free @IO @Identity

cudaPeek :: (MonadIO m, Storable a) => Identity (Ptr a) -> CUDAM m (Identity a)
cudaPeek = liftIO . A.peek @IO @Identity

cudaPoke :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity a -> CUDAM m ()
cudaPoke p a = liftIO $ A.poke @IO @Identity p a

cudaPeekElemOff :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity Int64 -> CUDAM m (Identity a)
cudaPeekElemOff p i = liftIO $ A.peekElemOff @IO @Identity p i

cudaPokeElemOff :: (MonadIO m, Storable a) => Identity (Ptr a) -> Identity a -> Identity Int64 -> CUDAM m ()
cudaPokeElemOff p a i = liftIO $ A.pokeElemOff @IO @Identity p a i

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
-- Concrete IO and DejaFu CPU CUDA Backends

newtype CUDAIO a = CUDAIO {getCUDAIO :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (Applicative, Monad, ExpBool) via Identity

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

newtype CUDAIORef a = CUDAIORef {getCUDAIORef :: IORef a}

newtype CUDADejaFu a = CUDADejaFu {getCUDADejaFu :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving (Applicative, Monad, ExpBool) via Identity

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

newtype CUDADejaFuRef a = CUDADejaFuRef {getCUDADejaFuRef :: IORef a}

$( let mkCUDACPUBackend expTy monadTy refTy baseM =
         [d|
           type instance JanusTyped $expTy = IdConstraint

           deriving via Identity instance ExpCond $expTy

           deriving via Identity instance ExpLet $expTy

           deriving via Identity instance ExpPtrCast $expTy

           deriving via Identity a instance (Num a) => Num ($expTy a)

           deriving via Identity a instance (Fractional a) => Fractional ($expTy a)

           deriving via Identity a instance (Floating a) => Floating ($expTy a)

           deriving via Identity a instance (FMA a) => FMA ($expTy a)

           deriving via Identity a instance (Hypot a) => Hypot ($expTy a)

           instance (Bits a, Integral a) => ExpBits $expTy a where
             iand :: $expTy a -> $expTy a -> $expTy a
             iand = coerce (iand @Identity @a)
             ior :: $expTy a -> $expTy a -> $expTy a
             ior = coerce (ior @Identity @a)
             ieor :: $expTy a -> $expTy a -> $expTy a
             ieor = coerce (ieor @Identity @a)
             inot :: $expTy a -> $expTy a
             inot = coerce (inot @Identity @a)
             lshift :: $expTy a -> $expTy a -> $expTy a
             lshift = coerce (lshift @Identity @a)
             rshift :: $expTy a -> $expTy a -> $expTy a
             rshift = coerce (rshift @Identity @a)

           instance (Num a, Eq a) => ExpBoolCast $expTy a where
             fromBool :: $expTy Bool -> $expTy a
             fromBool = coerce (fromBool @Identity @a)
             toBool :: $expTy a -> $expTy Bool
             toBool = coerce (toBool @Identity @a)

           instance (Real a, Fractional b) => ExpFloatingCast $expTy a b where
             toFloating :: $expTy a -> $expTy b
             toFloating = coerce (toFloating @Identity @a @b)

           instance (Integral a, Num b) => ExpIntegralCast $expTy a b where
             toIntegral :: $expTy a -> $expTy b
             toIntegral = coerce (toIntegral @Identity @a @b)

           instance (Eq a) => ExpEq $expTy a where
             eq :: $expTy a -> $expTy a -> $expTy Bool
             eq = coerce (eq @Identity @a)
             ne :: $expTy a -> $expTy a -> $expTy Bool
             ne = coerce (ne @Identity @a)

           instance ExpExtract $expTy a where
             extract :: $expTy a -> a
             extract = coerce (extract @Identity @a)

           instance ExpInject $expTy a where
             inject :: a -> $expTy a
             inject = coerce (inject @Identity @a)

           instance (Integral a) => ExpIntegral $expTy a where
             quotient :: $expTy a -> $expTy a -> $expTy a
             quotient = coerce (quotient @Identity @a)
             remainder :: $expTy a -> $expTy a -> $expTy a
             remainder = coerce (remainder @Identity @a)

           instance ExpComplex $expTy a where
             rePart :: $expTy (Complex a) -> $expTy a
             rePart = coerce (rePart @Identity @a)
             imPart :: $expTy (Complex a) -> $expTy a
             imPart = coerce (imPart @Identity @a)

           instance (ExpMurmurHash Identity (Identity a)) => ExpMurmurHash $expTy ($expTy a) where
             murmurHashWithSalt :: $expTy Word64 -> $expTy a -> $expTy Word64
             murmurHashWithSalt = coerce (murmurHashWithSalt @Identity @(Identity a))

           instance (Ord a) => ExpOrd $expTy a where
             lt :: $expTy a -> $expTy a -> $expTy Bool
             lt = coerce (lt @Identity @a)
             le :: $expTy a -> $expTy a -> $expTy Bool
             le = coerce (le @Identity @a)
             gt :: $expTy a -> $expTy a -> $expTy Bool
             gt = coerce (gt @Identity @a)
             ge :: $expTy a -> $expTy a -> $expTy Bool
             ge = coerce (ge @Identity @a)

           instance (Storable a) => A.ExpSized $expTy a where
             sizeOf :: Proxy a -> $expTy Int64
             sizeOf = coerce (A.sizeOf @Identity @a)
             alignOf :: Proxy a -> $expTy Int64
             alignOf = coerce (A.alignOf @Identity @a)

           instance (Storable a) => A.ExpPtr $expTy a where
             nullPtr :: $expTy (Ptr a)
             nullPtr = coerce (A.nullPtr @Identity @a)
             ptrAdd :: $expTy (Ptr a) -> $expTy Int64 -> $expTy (Ptr a)
             ptrAdd = coerce (A.ptrAdd @Identity @a)
             ptrIndex :: $expTy (Ptr a) -> $expTy Int64 -> $expTy (Ptr a)
             ptrIndex = coerce (A.ptrIndex @Identity @a)

           instance MonadReader (CUDAThreadInfo $expTy) $monadTy where
             ask :: $monadTy (CUDAThreadInfo $expTy)
             ask = coerce (cudaAskThreadInfo @($baseM) @($expTy))
             local :: forall a. (CUDAThreadInfo $expTy -> CUDAThreadInfo $expTy) -> $monadTy a -> $monadTy a
             local = coerce (cudaLocalThreadInfo @($baseM) @($expTy) @a)

           instance A.CmdMem $monadTy $expTy where
             malloc :: $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             malloc = coerce (cudaMalloc @($baseM))
             calloc :: $expTy Int64 -> $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             calloc = coerce (cudaCalloc @($baseM))
             realloc :: $expTy (Ptr ()) -> $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             realloc = coerce (cudaRealloc @($baseM))
             free :: $expTy (Ptr ()) -> $monadTy ()
             free = coerce (cudaFree @($baseM))

           instance (Storable a) => A.CmdStorable $monadTy $expTy a where
             peek :: $expTy (Ptr a) -> $monadTy ($expTy a)
             peek = coerce (cudaPeek @($baseM) @a)
             poke :: $expTy (Ptr a) -> $expTy a -> $monadTy ()
             poke = coerce (cudaPoke @($baseM) @a)
             peekElemOff :: $expTy (Ptr a) -> $expTy Int64 -> $monadTy ($expTy a)
             peekElemOff = coerce (cudaPeekElemOff @($baseM) @a)
             pokeElemOff :: $expTy (Ptr a) -> $expTy a -> $expTy Int64 -> $monadTy ()
             pokeElemOff = coerce (cudaPokeElemOff @($baseM) @a)

           instance (Storable a) => A.CmdRefPtr $monadTy $expTy a where
             withRefPtr :: forall b. $refTy a -> ($expTy (Ptr a) -> $monadTy b) -> $monadTy b
             withRefPtr = coerce (cudaWithRefPtr @($baseM) @a @b)

           instance CmdCase $monadTy $expTy where
             switchCase_ :: $expTy Int64 -> [(Int, $monadTy ())] -> $monadTy () -> $monadTy ()
             switchCase_ = coerce (cudaSwitchCase_ @($baseM))

           instance CmdCond $monadTy $expTy where
             ifThenElseM_ :: $expTy Bool -> $monadTy () -> $monadTy () -> $monadTy ()
             ifThenElseM_ = coerce (cudaIfThenElseM_ @($baseM))

           instance CmdIO $monadTy $expTy where
             stdout :: $expTy (Ptr CFile)
             stdout = coerce (stdout @IO @Identity)
             stderr :: $expTy (Ptr CFile)
             stderr = coerce (stderr @IO @Identity)
             fopen :: $expTy (Ptr CChar) -> $expTy (Ptr CChar) -> $monadTy ($expTy (Ptr CFile))
             fopen = coerce (cudaFopen @($baseM))
             fclose :: $expTy (Ptr CFile) -> $monadTy ($expTy CInt)
             fclose = coerce (cudaFclose @($baseM))

           instance CmdString $monadTy $expTy where
             withString :: forall a. String -> ($expTy CString -> $monadTy a) -> $monadTy a
             withString = coerce (cudaWithString @($baseM) @a)

           instance CmdPutString $monadTy $expTy where
             hputString :: $expTy (Ptr CFile) -> $expTy CString -> $monadTy ()
             hputString = coerce (cudaHputString @($baseM))

           instance (Show a) => CmdFormat $monadTy $expTy a where
             hformat :: $expTy (Ptr CFile) -> $expTy a -> String -> $monadTy ()
             hformat = coerce (cudaHformat @($baseM) @a)

           instance CmdRef $monadTy $expTy where
             type Ref $monadTy $expTy a = $refTy a
             letM = pure
             newRef :: forall a. (JanusTyped $expTy a) => $expTy a -> $monadTy ($refTy a)
             newRef = coerce (cudaNewRef @($baseM) @a)
             newRef' :: forall a. (JanusTyped $expTy a) => $monadTy ($refTy a)
             newRef' = coerce (cudaNewRef' @($baseM) @a)
             readRef :: forall a. (JanusTyped $expTy a) => $refTy a -> $monadTy ($expTy a)
             readRef = coerce (cudaReadRef @($baseM) @a)
             writeRef :: forall a. (JanusTyped $expTy a) => $refTy a -> $expTy a -> $monadTy ()
             writeRef = coerce (cudaWriteRef @($baseM) @a)

           instance CmdRange $monadTy $expTy

           instance CmdWhile $monadTy $expTy where
             whileM :: $expTy Bool -> ($refTy Bool -> $monadTy ()) -> $monadTy ()
             whileM = coerce (cudaWhileM @($baseM))

           instance CmdTicToc $monadTy $expTy where
             tic :: $monadTy (Tic $expTy)
             tic = coerce (cudaTic @($baseM))
             toc :: $monadTy (Toc $expTy)
             toc = coerce (cudaToc @($baseM))
           |]
    in fmap concat $
         sequence
           [ mkCUDACPUBackend [t|CUDAIO|] [t|CUDAIOM|] [t|CUDAIORef|] [t|IO|],
             mkCUDACPUBackend [t|CUDADejaFu|] [t|CUDADejaFuM|] [t|CUDADejaFuRef|] [t|ConcIO|]
           ]
 )

launchCUDAIOM :: CUlaunchConfig -> CUDAIOM () -> IO ()
launchCUDAIOM conf (CUDAIOM m) = launchCUDAM conf m

launchCUDADejaFuMWith ::
  Scheduler s ->
  MemType ->
  s ->
  CUlaunchConfig ->
  CUDADejaFuM () ->
  IO (Either Condition (), s, Trace)
launchCUDADejaFuMWith sched mem s conf (CUDADejaFuM m) =
  runConcurrent sched mem s (launchCUDAM conf m)

launchCUDADejaFuM :: CUlaunchConfig -> CUDADejaFuM () -> IO (Either Condition (), Trace)
launchCUDADejaFuM conf m = do
  (res, (), trace) <- launchCUDADejaFuMWith roundRobinSched defaultMemType () conf m
  pure (res, trace)
