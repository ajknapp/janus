{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.CUDA where

import Control.Exception
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import Data.Complex
import Data.Int
import Data.Loc
import Data.Proxy
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
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

cudaArgMaxSize :: Int
cudaArgMaxSize = 4096

-- NOTE device pointers have to be allocated with C malloc/free to avoid cuda error 700
withCudaDeviceArray :: Int -> (CUdeviceptr -> IO a) -> IO a
withCudaDeviceArray nbytes k = bracket malloc free $ \dp -> bracket (cuMemAlloc dp (fromIntegral nbytes) >> peek dp) cuMemFree k

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
  }

$(makeLenses ''CUDAThreadInfo)

newtype JanusCUDA a = JanusCUDA {getJanusCUDA :: JanusC a}

newtype JanusCUDAM a = JanusCUDAM {getJanusCUDAM :: JanusCM a}
  deriving (Functor, Applicative, Monad, MonadFix) via JanusCM

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

instance (A.ExpSized JanusC a) => A.ExpSized JanusCUDA a where
  sizeOf = coerce (A.sizeOf @JanusC @a)
  alignOf = coerce (A.alignOf @JanusC @a)

instance (A.ExpPtr JanusC a) => A.ExpPtr JanusCUDA a where
  nullPtr = coerce (A.nullPtr @JanusC @a)
  ptrAdd = coerce (A.ptrAdd @JanusC @a)

instance A.CmdMem JanusCUDAM JanusCUDA where
  malloc = coerce (A.malloc @JanusCM @JanusC)
  calloc = coerce (A.calloc @JanusCM @JanusC)
  realloc = coerce (A.realloc @JanusCM @JanusC)
  free = coerce (A.free @JanusCM @JanusC)

instance (A.ExpSized JanusCUDA a, JanusCTyped a) => A.CmdStorable JanusCUDAM JanusCUDA a where
  peek = coerce (A.peek @JanusCM @JanusC @a)
  poke = coerce (A.poke @JanusCM @JanusC @a)
  peekElemOff = coerce (A.peekElemOff @JanusCM @JanusC @a)
  pokeElemOff = coerce (A.pokeElemOff @JanusCM @JanusC @a)

instance CmdCase JanusCUDAM JanusCUDA where
  switchCase_ = coerce (switchCase_ @JanusCM @JanusC)

instance CmdCond JanusCUDAM JanusCUDA where
  ifThenElseM_ = coerce (ifThenElseM_ @JanusCM @JanusC)

instance CmdIO JanusCUDAM JanusCUDA where
  stdout = coerce (stdout @JanusCM @JanusC)
  stderr = coerce (stderr @JanusCM @JanusC)
  fopen = coerce (fopen @JanusCM @JanusC)
  fclose = coerce (fclose @JanusCM @JanusC)

instance CmdString JanusCUDAM JanusCUDA where
  withString s f = JanusCUDAM (withString @JanusCM @JanusC s $ getJanusCUDAM . f . JanusCUDA)

instance CmdPutString JanusCUDAM JanusCUDA where
  hputString = coerce (hputString @JanusCM @JanusC)

instance (CmdFormat JanusCM JanusC a) => CmdFormat JanusCUDAM JanusCUDA a where
  hformat = coerce (hformat @JanusCM @JanusC @a)

newtype JanusCUDARef a = JanusCUDARef {getJanusCUDARef :: JanusCRef a}

instance CmdRef JanusCUDAM JanusCUDA where
  type Ref JanusCUDAM JanusCUDA a = JanusCUDARef a
  letM (JanusCUDA a) = JanusCUDAM $ JanusCUDA <$> letM a
  newRef (JanusCUDA a) = JanusCUDAM (JanusCUDARef <$> newRef a)
  newRef' = JanusCUDAM (JanusCUDARef <$> newRef')
  readRef (JanusCUDARef r) = JanusCUDAM (JanusCUDA <$> readRef r)
  writeRef (JanusCUDARef r) (JanusCUDA a) = JanusCUDAM (writeRef r a)

instance CmdRange JanusCUDAM JanusCUDA where
  rangeM = coerce (rangeM @JanusCM @JanusC)

instance CmdWhile JanusCUDAM JanusCUDA where
  whileM = coerce (whileM @JanusCM @JanusC)

--------------------------------------------------------------------------------

class JanusCUDAParam r where
  jcudaparam :: String -> Int -> JanusCUDAM [Param] -> r -> [JCFunc]

instance JanusCUDAParam (JanusCUDAM ()) where
  jcudaparam name n params (JanusCUDAM a) = jcparam name n (getJanusCUDAM params) JCUDA a

instance (JanusCTyped a, JanusCUDAParam r) => JanusCUDAParam (JanusCUDA a -> r) where
  jcudaparam name n args f = jcudaparam name (n + 1) args' (f $ JanusCUDA $ JanusC $ pure $ RVal $ Var (mkArgId n) noLoc)
    where
      args' = do
        JCType spec dec <- JanusCUDAM $ getJanusCType (Proxy @a)
        args'' <- args
        pure $ Param (Just (mkArgId n)) spec dec noLoc : args''

type family JanusCUDAEval r where
  JanusCUDAEval (JanusCUDAM ()) = IO ()
  JanusCUDAEval (JanusCUDA a -> r) = JanusCUDAEval' a -> JanusCUDAEval r

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
      (nbuf + sizeOf (undefined :: JanusCUDAEval' a))
      argPtrs
      (nptrs + 1)
      pack'
      cuargs
      (k $ JanusCUDA $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)
    where
      pack' = do
        if nbuf >= cudaArgMaxSize
          then error "jcudaeval: too much space used"
          else do
            let pa = castPtr (argBuf `plusPtr` nbuf)
            poke @(JanusCUDAEval' a) pa a
            pokeElemOff argPtrs nptrs (castPtr pa)
            pack

showJanusCUDA :: forall a. (JanusCUDAParam a) => String -> a -> String
showJanusCUDA name a =
  let jcs = jcudaparam name 0 (pure []) a
      strChar 0 = '\n'
      strChar 81 = '\n'
      strChar _ = '-'
      str = fmap strChar [0 .. 81 :: Int]
   in foldMap ((<> str) . renderJCFunc) jcs

printJanusCUDA :: (JanusCUDAParam r) => r -> IO ()
printJanusCUDA = putStrLn . showJanusCUDA "janus_main"

withJanusCUDAFn ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUdevice ->
  CUctx ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDAFn dev ctx f k = do
  let dir = "_cache"
  cufiles <- writeJanusCFiles JCUDA dir (jcudaparam "janus_main" 0 (pure []) f)
  withJanusCUmodule dev ctx dir cufiles $ \pmod ->
    bracket (mallocBytes cudaArgMaxSize) free $ \argbuf ->
      bracket (mallocBytes (cudaArgMaxSize * sizeOf (undefined :: Ptr ()))) free $ \argPtrs ->
        bracket malloc free $ \pfn -> withCString "janus_main" $ \s -> do
          m <- peek pmod
          fn <- cuModuleGetFunction pfn m s >> peek pfn
          k fn (\kernelArgs -> jcudaeval fn argbuf 0 argPtrs 0 (pure ()) kernelArgs f)

withJanusCUDA ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUdevice ->
  CUctx ->
  r ->
  ((CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDA dev ctx f k = withJanusCUDAFn dev ctx f (const k)

newtype CUDAT e m a = CUDAT {getCUDAT :: ReaderT (CUDAThreadInfo e) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadReader (CUDAThreadInfo e), MonadTrans)

runCUDAT_ :: CUDAT JanusCUDA JanusCUDAM () -> JanusCUDAM ()
runCUDAT_ (CUDAT m) =
  let magicVar var mem = JanusCUDA $ JanusC $ pure $ RVal (Member (Var (Id var noLoc) noLoc) (Id mem noLoc) noLoc)
   in runReaderT
        m
        CUDAThreadInfo
          { _cudaThreadInfoBlockDimX = magicVar "blockDim" "x",
            _cudaThreadInfoBlockDimY = magicVar "blockDim" "y",
            _cudaThreadInfoBlockDimZ = magicVar "blockDim" "z",
            _cudaThreadInfoBlockIdxX = magicVar "blockIdx" "x",
            _cudaThreadInfoBlockIdxY = magicVar "blockIdx" "y",
            _cudaThreadInfoBlockIdxZ = magicVar "blockIdx" "z",
            _cudaThreadInfoThreadIdxX = magicVar "threadIdx" "x",
            _cudaThreadInfoThreadIdxY = magicVar "threadIdx" "y",
            _cudaThreadInfoThreadIdxZ = magicVar "threadIdx" "z",
            _cudaThreadInfoGridDimX = magicVar "gridDim" "x",
            _cudaThreadInfoGridDimY = magicVar "gridDim" "y",
            _cudaThreadInfoGridDimZ = magicVar "gridDim" "z"
          }

runEmulatedCUDAT_ ::
  ( JanusTyped e Int64,
    ExpOrd e Int64,
    CmdRange m e,
    Num (e Int64),
    Num (e Int32),
    ExpIntegralCast e Int64 Int32
  ) =>
  CUlaunchConfig ->
  CUDAT e m () ->
  m ()
runEmulatedCUDAT_ conf (CUDAT m) =
  rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimX) $ \blockIdxX ->
    rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimY) $ \blockIdxY ->
      rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigGridDimZ) $ \blockIdxZ ->
        rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigBlockDimX) $ \threadIdxX ->
          rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigBlockDimY) $ \threadIdxY ->
            rangeM 0 (fromIntegral $ conf ^. cuLaunchConfigBlockDimZ) $ \threadIdxZ ->
              runReaderT m $
                CUDAThreadInfo
                  { _cudaThreadInfoBlockIdxX = toIntegral blockIdxX,
                    _cudaThreadInfoBlockIdxY = toIntegral blockIdxY,
                    _cudaThreadInfoBlockIdxZ = toIntegral blockIdxZ,
                    _cudaThreadInfoBlockDimX = fromIntegral (conf ^. cuLaunchConfigBlockDimX),
                    _cudaThreadInfoBlockDimY = fromIntegral (conf ^. cuLaunchConfigBlockDimY),
                    _cudaThreadInfoBlockDimZ = fromIntegral (conf ^. cuLaunchConfigBlockDimZ),
                    _cudaThreadInfoThreadIdxX = toIntegral threadIdxX,
                    _cudaThreadInfoThreadIdxY = toIntegral threadIdxY,
                    _cudaThreadInfoThreadIdxZ = toIntegral threadIdxZ,
                    _cudaThreadInfoGridDimX = fromIntegral (conf ^. cuLaunchConfigGridDimX),
                    _cudaThreadInfoGridDimY = fromIntegral (conf ^. cuLaunchConfigGridDimY),
                    _cudaThreadInfoGridDimZ = fromIntegral (conf ^. cuLaunchConfigGridDimZ)
                  }

newtype CUDABlock m e = CUDABlock {getCUDABlock :: [CUDAT e m ()]}
  deriving (Semigroup, Monoid)

cudaBlock :: CUDAT e m () -> CUDABlock m e
cudaBlock m = CUDABlock (pure m)

runEmulatedCUDABlock_ ::
  ( JanusTyped e Int64,
    ExpOrd e Int64,
    CmdRange m e,
    Num (e Int64),
    Num (e Int32),
    ExpIntegralCast e Int64 Int32,
    Monad m
  ) =>
  CUlaunchConfig ->
  CUDABlock m e ->
  m ()
runEmulatedCUDABlock_ _ (CUDABlock []) = pure ()
runEmulatedCUDABlock_ conf (CUDABlock (m : ms)) = runEmulatedCUDAT_ conf m >> runEmulatedCUDABlock_ conf (CUDABlock ms)
