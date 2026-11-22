{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Control.Monad.State (StateT (..), modify)
import Data.Bits (Bits (..))
import Data.Coerce
import Data.Complex
import Data.IORef
import Data.Int
import qualified Data.Kind as K
import Data.Loc
import Data.Proxy
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word (Word32, Word64, Word8)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.Target
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
import Janus.Expression.SIMD
import Janus.Expression.SIMD.CPU
import Janus.Expression.SIMD.Types (rawMaskFromBools, rawMaskToBools, rawSIMDFromList, rawSIMDToList)
import Janus.Typed
import Language.C.Quote as C
import qualified System.IO as SIO
import Test.DejaFu.Conc (ConcIO, Condition, MemType, Trace, runConcurrent)
import Test.DejaFu.Schedule (Scheduler, roundRobinSched)
import Test.DejaFu.Settings (defaultMemType)

cudaArgMaxSize :: Int
cudaArgMaxSize = 4096

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

deriving via JanusC instance ExpPtrCast JanusCUDA

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

instance (ExpMurmurHash JanusC (JanusC a)) => ExpMurmurHash JanusCUDA (JanusCUDA a) where
  murmurHashWithSalt = coerce (murmurHashWithSalt @JanusC @(JanusC a))

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
  jcudaparamWithFeatures :: CUDAFeatures -> String -> Int -> JanusCUDAM [Param] -> r -> [JCFunc]

jcudaparam :: (JanusCUDAParam r) => String -> Int -> JanusCUDAM [Param] -> JanusCBackend -> r -> [JCFunc]
jcudaparam name n params _ = jcudaparamWithFeatures cudaSM80 name n params

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDA a) where
  jcudaparamWithFeatures fs name n params =
    jcparamImplWithInfo (Proxy @JanusCUDA) ((mkJCFuncInfo name JCUDA False) {_jcfiCUDAFeatures = fs}) n params

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDAM (JanusCUDA a)) where
  jcudaparamWithFeatures fs name n params =
    jcparamImplWithInfo (Proxy @JanusCUDA) ((mkJCFuncInfo name JCUDA False) {_jcfiCUDAFeatures = fs}) n params

instance JanusCUDAParam (JanusCUDAM ()) where
  jcudaparamWithFeatures fs name n params =
    jcparamImplWithInfo (Proxy @JanusCUDA) ((mkJCFuncInfo name JCUDA False) {_jcfiCUDAFeatures = fs}) n params

instance (JanusCTyped a, JanusCUDAParam r, JanusCParamImpl JanusCUDA JanusCUDAM r) => JanusCUDAParam (JanusCUDA a -> r) where
  jcudaparamWithFeatures fs name n params =
    jcparamImplWithInfo (Proxy @JanusCUDA) ((mkJCFuncInfo name JCUDA False) {_jcfiCUDAFeatures = fs}) n params

type family JanusCUDAEval r where
  JanusCUDAEval (JanusCUDAM ()) = IO ()
  JanusCUDAEval (JanusCUDA a -> r) = JanusCUDAEval' a -> JanusCUDAEval r

showJanusCUDAWith :: forall a. (JanusCUDAParam a) => CUDAFeatures -> String -> a -> String
showJanusCUDAWith fs name a =
  let jcs = jcudaparamWithFeatures fs name 0 (pure []) a
      banner = "/* janus-cuda-target: sm_" <> show (cudaSMMajor fs) <> show (cudaSMMinor fs) <> " */\n"
      strChar 0 = '\n'
      strChar 81 = '\n'
      strChar _ = '-'
      str = fmap strChar [0 .. 81 :: Int]
   in banner <> foldMap ((<> str) . renderJCFunc) jcs

showJanusCUDA :: forall a. (JanusCUDAParam a) => String -> a -> String
showJanusCUDA = showJanusCUDAWith cudaSM80

printJanusCUDAWith :: (JanusCUDAParam r) => CUDAFeatures -> r -> IO ()
printJanusCUDAWith fs = putStrLn . showJanusCUDAWith fs "janus_main"

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
      (alignedOff + sizeOf proxy)
      argPtrs
      (nptrs + 1)
      pack'
      cuargs
      (k $ JanusCUDA $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)
    where
      proxy = undefined :: JanusCUDAEval' a
      al = max 1 (alignment proxy)
      alignedOff = (nbuf + al - 1) .&. complement (al - 1)
      pack' = do
        if alignedOff + sizeOf proxy > cudaArgMaxSize
          then error "jcudaeval: too much space used"
          else do
            let pa = castPtr (argBuf `plusPtr` alignedOff)
            poke @(JanusCUDAEval' a) pa a
            pokeElemOff argPtrs nptrs (castPtr pa)
            pack

withJanusCUDAFnWith ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUDAFeatures ->
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDAFnWith reqFeatures dev ctx config f k = do
  observed <- observeCUDAFeatures dev
  ensureCUDAAdmitted observed reqFeatures
  let dir = config ^. cBuildConfigCacheDir
  cufiles <- writeJanusCFiles JCUDA dir (jcudaparamWithFeatures reqFeatures "janus_main" 0 (pure []) f)
  withJanusCUmoduleWithFeatures reqFeatures dev ctx dir cufiles $ \pmod ->
    bracket (mallocBytes cudaArgMaxSize) free $ \argbuf ->
      bracket (mallocBytes (cudaArgMaxSize * sizeOf (undefined :: Ptr ()))) free $ \argPtrs ->
        alloca $ \pfn -> withCString "janus_main" $ \s -> do
          m <- peek pmod
          fn <- cuModuleGetFunction pfn m s >> peek pfn
          k fn (\kernelArgs -> jcudaeval fn argbuf 0 argPtrs 0 (pure ()) kernelArgs f)

withJanusCUDAWith ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUDAFeatures ->
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  ((CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDAWith fs dev ctx config f k = withJanusCUDAFnWith fs dev ctx config f (const k)

withJanusCUDAFn ::
  (JanusCUDAParam r, JanusCUDAEvaluate r) =>
  CUdevice ->
  CUctx ->
  CBuildConfig ->
  r ->
  (CUfunction -> (CUlaunchConfig -> JanusCUDAEval r) -> IO a) ->
  IO a
withJanusCUDAFn dev ctx config f k = do
  fs <- observeCUDAFeatures dev
  withJanusCUDAFnWith fs dev ctx config f k

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

--------------------------------------------------------------------------------
-- CPU-Emulated SIMD Instances for CUDAIO & CUDADejaFu
--------------------------------------------------------------------------------

instance ExpSIMD CUDAIO where
  newtype SIMDVector CUDAIO n a = CUDAIOSIMDVector {getCUDAIOSIMDVector :: CPUSIMD n a}
    deriving stock (Eq, Ord, Show)
  newtype SIMDMask CUDAIO n a = CUDAIOSIMDMask {getCUDAIOSIMDMask :: CPUMask n a}
    deriving stock (Eq, Ord, Show)

deriving newtype instance (SIMDShape n a) => Storable (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a) => Num (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Fractional a) => Fractional (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a) => Floating (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, FMA a) => FMA (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a, Hypot a) => Hypot (SIMDVector CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Join (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Meet (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Lower (SIMDMask CUDAIO n a)
deriving newtype instance (SIMDShape n a) => Upper (SIMDMask CUDAIO n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector CUDAIO n a) n a where
  simdFromList = CUDAIOSIMDVector . cpuFromLanes
  simdToList = unCPUSIMD . getCUDAIOSIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask CUDAIO n a) n a where
  simdMaskFromBools = CUDAIOSIMDMask . CPUMask
  simdMaskToBools = unCPUMask . getCUDAIOSIMDMask

instance (SIMDShape n a) => Join (CUDAIO (SIMDMask CUDAIO n a)) where (\/) = coerce (cpuMaskOr @n @a)
instance (SIMDShape n a) => Meet (CUDAIO (SIMDMask CUDAIO n a)) where (/\) = coerce (cpuMaskAnd @n @a)
instance (SIMDShape n a) => Lower (CUDAIO (SIMDMask CUDAIO n a)) where lowerBound = CUDAIO (CUDAIOSIMDMask cpuMaskFalse)
instance (SIMDShape n a) => Upper (CUDAIO (SIMDMask CUDAIO n a)) where upperBound = CUDAIO (CUDAIOSIMDMask cpuMaskTrue)

instance ExpSIMD CUDADejaFu where
  newtype SIMDVector CUDADejaFu n a = CUDADejaFuSIMDVector {getCUDADejaFuSIMDVector :: CPUSIMD n a}
    deriving stock (Eq, Ord, Show)
  newtype SIMDMask CUDADejaFu n a = CUDADejaFuSIMDMask {getCUDADejaFuSIMDMask :: CPUMask n a}
    deriving stock (Eq, Ord, Show)

deriving newtype instance (SIMDShape n a) => Storable (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a) => Num (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Fractional a) => Fractional (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a) => Floating (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, FMA a) => FMA (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a, CPUSIMDElem a, Floating a, Hypot a) => Hypot (SIMDVector CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Join (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Meet (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Lower (SIMDMask CUDADejaFu n a)
deriving newtype instance (SIMDShape n a) => Upper (SIMDMask CUDADejaFu n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector CUDADejaFu n a) n a where
  simdFromList = CUDADejaFuSIMDVector . cpuFromLanes
  simdToList = unCPUSIMD . getCUDADejaFuSIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask CUDADejaFu n a) n a where
  simdMaskFromBools = CUDADejaFuSIMDMask . CPUMask
  simdMaskToBools = unCPUMask . getCUDADejaFuSIMDMask

instance (SIMDShape n a) => Join (CUDADejaFu (SIMDMask CUDADejaFu n a)) where (\/) = coerce (cpuMaskOr @n @a)
instance (SIMDShape n a) => Meet (CUDADejaFu (SIMDMask CUDADejaFu n a)) where (/\) = coerce (cpuMaskAnd @n @a)
instance (SIMDShape n a) => Lower (CUDADejaFu (SIMDMask CUDADejaFu n a)) where lowerBound = CUDADejaFu (CUDADejaFuSIMDMask cpuMaskFalse)
instance (SIMDShape n a) => Upper (CUDADejaFu (SIMDMask CUDADejaFu n a)) where upperBound = CUDADejaFu (CUDADejaFuSIMDMask cpuMaskTrue)

$( let mkCPUSIMDInstances expTy monadTy =
         [d|
           instance (SIMDShape n a, Num a) => ExpSIMDVec $expTy n a where
             vzero = coerce (cpuZero @n @a)
             vsplat = coerce (cpuSplat @n @a)
             viota = coerce (cpuIota @n @a)
             vfromLanes xs = coerce (cpuFromLanes @n @a (coerce xs))
             vextractLane p v =
               let i = fromIntegral (natVal p) :: Int
                in coerce (cpuExtractLane @n @a i) v
             vinsertLane p x v =
               let i = fromIntegral (natVal p) :: Int
                in coerce (cpuInsertLane @n @a i) x v

           instance (SIMDShape n a) => ExpSIMDMask $expTy n a where
             mfalse = coerce (cpuMaskFalse @n @a)
             mtrue = coerce (cpuMaskTrue @n @a)
             mand = coerce (cpuMaskAnd @n @a)
             mor = coerce (cpuMaskOr @n @a)
             mxor = coerce (cpuMaskXor @n @a)
             mandnot = coerce (cpuMaskAndNot @n @a)
             mnot = coerce (cpuMaskNot @n @a)
             vselect = coerce (cpuSelect @n @a)
             mmovemask = coerce (cpuMovemask @n @a)
             mpopcount = coerce (cpuMaskPopcount @n @a)
             mfirstTrue = coerce (cpuMaskFirstTrue @n @a)
             mallOf = coerce (cpuAllOf @n @a)
             manyOf = coerce (cpuAnyOf @n @a)
             mnoneOf = coerce (cpuNoneOf @n @a)

           instance (SIMDShape n a, Ord a) => ExpSIMDCmp $expTy n a where
             veq = coerce (cpuEq @n @a)
             vne = coerce (cpuNe @n @a)
             vlt = coerce (cpuLt @n @a)
             vle = coerce (cpuLe @n @a)
             vgt = coerce (cpuGt @n @a)
             vge = coerce (cpuGe @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith $expTy n a where
             vadd = coerce (cpuAdd @n @a)
             vsub = coerce (cpuSub @n @a)
             vmul = coerce (cpuMul @n @a)
             vneg = coerce (cpuNeg @n @a)
             vabs = coerce (cpuAbs @n @a)
             vmin = coerce (cpuMin @n @a)
             vmax = coerce (cpuMax @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat $expTy n a where
             vadds = coerce (cpuAdds @n @a)
             vsubs = coerce (cpuSubs @n @a)
             vabss = coerce (cpuAbss @n @a)
             vavg = coerce (cpuAvg @n @a)
             vabsdiff = coerce (cpuAbsDiff @n @a)

           instance (SIMDShape n a, CPUSIMDElem a, RealFloat a, FMA a) => ExpSIMDFloat $expTy n a where
             vdiv = coerce (cpuDiv @n @a)
             vsqrt = coerce (cpuSqrt @n @a)
             vrsqrtApprox = coerce (cpuRsqrtApprox @n @a)
             vrcpApprox = coerce (cpuRcpApprox @n @a)
             vfma = coerce (cpuFma @n @a)
             vfms = coerce (cpuFms @n @a)
             vfnma = coerce (cpuFnma @n @a)
             vfnms = coerce (cpuFnms @n @a)
             vmulAdd = coerce (cpuMulAdd @n @a)
             vfloor = coerce (cpuFloor @n @a)
             vceil = coerce (cpuCeil @n @a)
             vtrunc = coerce (cpuTrunc @n @a)
             vroundNearest = coerce (cpuRoundNearest @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits $expTy n a where
             vand = coerce (cpuBitAnd @n @a)
             vor = coerce (cpuBitOr @n @a)
             vxor = coerce (cpuBitXor @n @a)
             vandnot = coerce (cpuBitAndNot @n @a)
             vnot = coerce (cpuBitNot @n @a)
             vshl = coerce (cpuShl @n @a)
             vshr = coerce (cpuShr @n @a)
             vshlImm p v = vshl v (fromIntegral (natVal p))
             vshrImm p v = vshr v (fromIntegral (natVal p))
             vshlVar = coerce (cpuShlVar @n @a)
             vshrVar = coerce (cpuShrVar @n @a)
             vpopcnt = coerce (cpuPopcnt @n @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle $expTy n a where
             vshuffleLanes idxs = coerce (\v -> cpuPermuteLanes @n @a v (Left idxs))
             vpermuteVar = coerce (\v idx -> cpuPermuteLanes @n @a v (Right idx))
             vshuffleBytes ::
               forall b.
               (SIMDShape b Word8) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy b Word8) ->
               $expTy (SIMDVector $expTy n a)
             vshuffleBytes = coerce (cpuShuffleBytes @n @a @b)
             vzipLo = coerce (cpuZipLo @n @a)
             vzipHi = coerce (cpuZipHi @n @a)

           instance (SIMDHalfPair n2 n a) => ExpSIMDSplit $expTy n2 n a where
             vsplit = coerce (cpuSplit @n2 @n @a)
             vconcat = coerce (cpuConcat @n @n2 @a)

           instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDReduce $expTy n a where
             vreduceAdd = coerce (cpuReduceAdd @n @a)
             vreduceMin = coerce (cpuReduceMin @n @a)
             vreduceMax = coerce (cpuReduceMax @n @a)
             vreduceAnd = coerce (cpuReduceAnd @n @a)
             vreduceOr = coerce (cpuReduceOr @n @a)
             vreduceXor = coerce (cpuReduceXor @n @a)

           instance ExpSIMDDot $expTy where
             vdp4aS = coerce cpuDp4aS
             vdp4aU = coerce cpuDp4aU
             vdp4aVecS ::
               forall m k.
               (SIMDDp4aShape m k) =>
               $expTy (SIMDVector $expTy m Int8) ->
               $expTy (SIMDVector $expTy m Int8) ->
               $expTy (SIMDVector $expTy k Int32) ->
               $expTy (SIMDVector $expTy k Int32)
             vdp4aVecS = coerce (cpuDp4aVecS @m @k)
             vdp4aVecU ::
               forall m k.
               (SIMDDp4aShape m k) =>
               $expTy (SIMDVector $expTy m Word8) ->
               $expTy (SIMDVector $expTy m Word8) ->
               $expTy (SIMDVector $expTy k Word32) ->
               $expTy (SIMDVector $expTy k Word32)
             vdp4aVecU = coerce (cpuDp4aVecU @m @k)
             vdp2aLoS = coerce (cpuDp2aS False)
             vdp2aHiS = coerce (cpuDp2aS True)
             vdp2aLoU = coerce (cpuDp2aU False)
             vdp2aHiU = coerce (cpuDp2aU True)
             vsad8 ::
               forall n.
               (SIMDShape n Word8) =>
               $expTy (SIMDVector $expTy n Word8) ->
               $expTy (SIMDVector $expTy n Word8) ->
               $expTy Word32
             vsad8 = coerce (cpuSadU8 @n)

           instance ExpSIMDCast $expTy where
             vbitcast ::
               forall n a m b.
               (SIMDShape n a, SIMDShape m b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy m b)
             vbitcast = coerce (cpuBitcast @n @a @m @b)
             vcvt ::
               forall n a b.
               (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy n b)
             vcvt = coerce (cpuCvt @n @a @b)
             vnarrowSat ::
               forall n m a b.
               (SIMDPackShape n m a b) =>
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy n a) ->
               $expTy (SIMDVector $expTy m b)
             vnarrowSat = coerce (cpuPackSat @n @m @a @b)

           instance (SIMDShape n a) => CmdSIMDMem $monadTy $expTy n a where
             vload p = liftIO (coerce (cpuLoad @IO @n @a) p)
             vloadAligned p = liftIO (coerce (cpuLoadAligned @IO @n @a) p)
             vstore p v = liftIO (coerce (cpuStore @IO @n @a) p v)
             vstoreAligned p v = liftIO (coerce (cpuStoreAligned @IO @n @a) p v)
             vloadMasked m fb p = liftIO (coerce (cpuLoadMasked @IO @n @a) m fb p)
             vstoreMasked m p v = liftIO (coerce (cpuStoreMasked @IO @n @a) m p v)
             vgather p idx = liftIO (coerce (cpuGather @IO @n @a) p idx)
             vgatherMasked m fb p idx = liftIO (coerce (cpuGatherMasked @IO @n @a) m fb p idx)
           |]
    in fmap concat $
         sequence
           [ mkCPUSIMDInstances [t|CUDAIO|] [t|CUDAIOM|],
             mkCPUSIMDInstances [t|CUDADejaFu|] [t|CUDADejaFuM|]
           ]
 )

--------------------------------------------------------------------------------
-- JanusCUDA Device SIMD Codegen (__v*2, __v*4, __dp4a, __dp2a, uint4/float4)
--------------------------------------------------------------------------------

instance ExpSIMD JanusCUDA where
  newtype SIMDVector JanusCUDA n a = JanusCUDASIMDVector {getJanusCUDASIMDVector :: RawSIMD n a}
    deriving stock (Eq, Ord)
  newtype SIMDMask JanusCUDA n a = JanusCUDASIMDMask {getJanusCUDASIMDMask :: RawMask n a}
    deriving stock (Eq, Ord)

deriving newtype instance (SIMDShape n a) => Show (SIMDVector JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Show (SIMDMask JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDVector JanusCUDA n a)
deriving newtype instance (SIMDShape n a) => Storable (SIMDMask JanusCUDA n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector JanusCUDA n a) n a where
  simdFromList = JanusCUDASIMDVector . rawSIMDFromList
  simdToList = rawSIMDToList . getJanusCUDASIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask JanusCUDA n a) n a where
  simdMaskFromBools = JanusCUDASIMDMask . rawMaskFromBools
  simdMaskToBools = rawMaskToBools . getJanusCUDASIMDMask

cuNamedType :: String -> JCType
cuNamedType nm = JCType (DeclSpec [] [] (Tnamed (Id nm noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

cu16BVecName :: forall a. (SIMDElem a) => Proxy a -> String
cu16BVecName p = case simdElemTag p of
  JanusElemFloat -> "float4"
  JanusElemDouble -> "double2"
  JanusElemInt32 -> "int4"
  JanusElemInt64 -> "longlong2"
  JanusElemWord64 -> "ulonglong2"
  _ -> "uint4"

cu8BVecName :: forall a. (SIMDElem a) => Proxy a -> String
cu8BVecName p = case simdElemTag p of
  JanusElemFloat -> "float2"
  JanusElemInt32 -> "int2"
  _ -> "uint2"

cudaSIMDType :: forall n a. (SIMDShape n a) => JanusCM JCType
cudaSIMDType = do
  addHeader "stdint.h"
  addHeader "string.h"
  case simdBytes (Proxy @n) (Proxy @a) of
    4 -> pure (cuNamedType "uint32_t")
    8 -> pure (cuNamedType (cu8BVecName (Proxy @a)))
    16 -> pure (cuNamedType (cu16BVecName (Proxy @a)))
    32 -> do
      let JCType hspec _ = cuNamedType (cu16BVecName (Proxy @a))
          fgs =
            [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
              FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
            ]
      pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "cudaSIMDType: unsupported byte size"

cudaMaskType :: forall n a. (SIMDShape n a) => JanusCM JCType
cudaMaskType = do
  addHeader "stdint.h"
  addHeader "string.h"
  case simdBytes (Proxy @n) (Proxy @a) of
    4 -> pure (cuNamedType "uint32_t")
    8 -> pure (cuNamedType "uint2")
    16 -> pure (cuNamedType "uint4")
    32 -> do
      let JCType hspec _ = cuNamedType "uint4"
          fgs =
            [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
              FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
            ]
      pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "cudaMaskType: unsupported byte size"

instance (SIMDShape n a) => JanusCTyped (SIMDVector JanusCUDA n a) where
  jctype _ = cudaSIMDType @n @a

instance (SIMDShape n a) => JanusCTyped (SIMDMask JanusCUDA n a) where
  jctype _ = cudaMaskType @n @a

cuVar :: String -> Exp
cuVar s = Var (Id s noLoc) noLoc

cuCall :: String -> [Exp] -> Exp
cuCall fn args = FnCall (cuVar fn) args noLoc

cuCast :: String -> Exp -> Exp
cuCast ty e =
  let JCType spec dec = cuNamedType ty
   in Cast (Type spec dec noLoc) e noLoc

cuPtrCast :: String -> Exp -> Exp
cuPtrCast ty e =
  let JCType spec dec = cuNamedType ty
   in Cast (Type spec (Ptr [] dec noLoc) noLoc) e noLoc

cuConstPtrCast :: String -> Exp -> Exp
cuConstPtrCast ty e =
  Cast (Type (DeclSpec [] [Tconst noLoc] (Tnamed (Id ty noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc) noLoc) e noLoc

cuInt :: Int -> Exp
cuInt = signedIntConst

cuMem :: Exp -> String -> Exp
cuMem e f = Member e (Id f noLoc) noLoc

cuStmt :: Stm -> JanusCM ()
cuStmt s = modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm s]

cuUninit :: JCType -> JanusCM Exp
cuUninit (JCType spec dec) = do
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl])
  pure (Var vid noLoc)

-- Apply a 32-bit CUDA SIMD intrinsic (e.g. __vadd4, __vadd2, __vcmpgts4) across
-- 4B (uint32_t), 8B (uint2), 16B (uint4), or 32B ({ uint4 lo; uint4 hi; })
emitCUDAPackedWordBin ::
  forall resTy.
  (JanusCTyped resTy) =>
  Int ->
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDAPackedWordBin bytes opWord ea0 eb0 = do
  rty <- getJanusCType (Proxy @resTy)
  ea <- emitLocalVal rty ea0
  eb <- emitLocalVal rty eb0
  case bytes of
    4 -> emitLocalVal rty (opWord ea eb)
    8 ->
      emitLocalVal rty $
        cuCall
          "make_uint2"
          [ opWord (cuMem ea "x") (cuMem eb "x"),
            opWord (cuMem ea "y") (cuMem eb "y")
          ]
    16 ->
      emitLocalVal rty $
        cuCall
          "make_uint4"
          [ opWord (cuMem ea "x") (cuMem eb "x"),
            opWord (cuMem ea "y") (cuMem eb "y"),
            opWord (cuMem ea "z") (cuMem eb "z"),
            opWord (cuMem ea "w") (cuMem eb "w")
          ]
    32 -> do
      out <- cuUninit rty
      let mk4 l r =
            cuCall
              "make_uint4"
              [ opWord (cuMem l "x") (cuMem r "x"),
                opWord (cuMem l "y") (cuMem r "y"),
                opWord (cuMem l "z") (cuMem r "z"),
                opWord (cuMem l "w") (cuMem r "w")
              ]
      cuStmt $ Exp (Just $ Assign (cuMem out "lo") JustAssign (mk4 (cuMem ea "lo") (cuMem eb "lo")) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (cuMem out "hi") JustAssign (mk4 (cuMem ea "hi") (cuMem eb "hi")) noLoc) noLoc
      pure out
    _ -> error "emitCUDAPackedWordBin: invalid byte count"

emitCUDAPackedWordUn ::
  forall resTy.
  (JanusCTyped resTy) =>
  Int ->
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitCUDAPackedWordUn bytes opWord ea0 = do
  rty <- getJanusCType (Proxy @resTy)
  ea <- emitLocalVal rty ea0
  case bytes of
    4 -> emitLocalVal rty (opWord ea)
    8 -> emitLocalVal rty $ cuCall "make_uint2" [opWord (cuMem ea "x"), opWord (cuMem ea "y")]
    16 ->
      emitLocalVal rty $
        cuCall "make_uint4" [opWord (cuMem ea "x"), opWord (cuMem ea "y"), opWord (cuMem ea "z"), opWord (cuMem ea "w")]
    32 -> do
      out <- cuUninit rty
      let mk4 v = cuCall "make_uint4" [opWord (cuMem v "x"), opWord (cuMem v "y"), opWord (cuMem v "z"), opWord (cuMem v "w")]
      cuStmt $ Exp (Just $ Assign (cuMem out "lo") JustAssign (mk4 (cuMem ea "lo")) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (cuMem out "hi") JustAssign (mk4 (cuMem ea "hi")) noLoc) noLoc
      pure out
    _ -> error "emitCUDAPackedWordUn: invalid byte count"

emitCUDALaneBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDALaneBin fn ea0 eb0 = do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  ea <- emitLocalVal ty ea0
  eb <- emitLocalVal ty eb0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- cuUninit arrTy
  ab <- cuUninit arrTy
  ar <- cuUninit arrTy
  res <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc) (Index ab idx noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
  pure res

emitCUDALaneUn ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitCUDALaneUn fn ea0 = do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  ea <- emitLocalVal ty ea0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- cuUninit arrTy
  ar <- cuUninit arrTy
  res <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
  pure res

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, JanusLitC a) => Num (JanusCUDA (SIMDVector JanusCUDA n a)) where
  (+) = vadd
  (-) = vsub
  (*) = vmul
  negate = vneg
  abs = vabs
  signum (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    let z = cuInt 0
        one = if simdElemIsFloat (Proxy @a) then C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc else cuInt 1
        negOne = UnOp Negate one noLoc
        body x
          | not (simdElemSigned (Proxy @a)) = Cond (BinOp Gt x z noLoc) (cuInt 1) z noLoc
          | simdElemIsFloat (Proxy @a) = Cond (BinOp Gt x z noLoc) one (Cond (BinOp Lt x z noLoc) negOne x noLoc) noLoc
          | otherwise = cCastElem (BinOp Sub (BinOp Gt x z noLoc) (BinOp Lt x z noLoc) noLoc)
        cCastElem = cuCast (simdElemCTypeName (Proxy @a))
    RVal <$> emitCUDALaneUn @n @a body ev
  fromInteger k = vsplat (fromInteger k)

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => Fractional (JanusCUDA (SIMDVector JanusCUDA n a)) where
  (/) = vdiv
  recip x = fromRational 1 / x
  fromRational q = vsplat (fromRational q)

instance (SIMDShape n Float) => Floating (JanusCUDA (SIMDVector JanusCUDA n Float)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "expf" [x]) e
  log (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "logf" [x]) e
  (JanusCUDA (JanusC ma)) ** (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Float (\x y -> cuCall "powf" [x, y]) a b
  sin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "sinf" [x]) e
  cos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "cosf" [x]) e
  tan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "tanf" [x]) e
  asin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "asinf" [x]) e
  acos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "acosf" [x]) e
  atan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "atanf" [x]) e
  sinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "sinhf" [x]) e
  cosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "coshf" [x]) e
  tanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "tanhf" [x]) e
  asinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "asinhf" [x]) e
  acosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "acoshf" [x]) e
  atanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Float (\x -> cuCall "atanhf" [x]) e

instance (SIMDShape n Double) => Floating (JanusCUDA (SIMDVector JanusCUDA n Double)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "exp" [x]) e
  log (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "log" [x]) e
  (JanusCUDA (JanusC ma)) ** (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Double (\x y -> cuCall "pow" [x, y]) a b
  sin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "sin" [x]) e
  cos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "cos" [x]) e
  tan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "tan" [x]) e
  asin (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "asin" [x]) e
  acos (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "acos" [x]) e
  atan (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "atan" [x]) e
  sinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "sinh" [x]) e
  cosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "cosh" [x]) e
  tanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "tanh" [x]) e
  asinh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "asinh" [x]) e
  acosh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "acosh" [x]) e
  atanh (JanusCUDA (JanusC m)) = JanusCUDA $ JanusC $ m >>= \(RVal e) -> RVal <$> emitCUDALaneUn @n @Double (\x -> cuCall "atanh" [x]) e

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => FMA (JanusCUDA (SIMDVector JanusCUDA n a)) where
  fma = vfma

instance {-# OVERLAPPING #-} (SIMDShape n Float) => Hypot (JanusCUDA (SIMDVector JanusCUDA n Float)) where
  hypot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Float (\x y -> cuCall "hypotf" [x, y]) a b

instance {-# OVERLAPPING #-} (SIMDShape n Double) => Hypot (JanusCUDA (SIMDVector JanusCUDA n Double)) where
  hypot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @Double (\x y -> cuCall "hypot" [x, y]) a b

instance (SIMDShape n a) => Join (JanusCUDA (SIMDMask JanusCUDA n a)) where (\/) = mor
instance (SIMDShape n a) => Meet (JanusCUDA (SIMDMask JanusCUDA n a)) where (/\) = mand
instance (SIMDShape n a) => Lower (JanusCUDA (SIMDMask JanusCUDA n a)) where lowerBound = mfalse
instance (SIMDShape n a) => Upper (JanusCUDA (SIMDMask JanusCUDA n a)) where upperBound = mtrue

-- 1. ExpSIMDVec JanusCUDA
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDVec JanusCUDA n a where
  vzero = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  vsplat (JanusCUDA (JanusC ms)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal s0 <- ms
    s <- emitLocalVal (cuNamedType (simdElemCTypeName (Proxy @a))) s0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign s noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  viota = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst i)) noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  vfromLanes es = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    when (length es /= lanes) $ error "vfromLanes: lane count mismatch"
    arr <- cuUninit (JCType espec arrDec)
    forM_ (zip [0 :: Int ..] es) $ \(i, JanusCUDA (JanusC me)) -> do
      RVal ev <- me
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign ev noLoc) noLoc
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (RVal out)

  vextractLane p (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    let srcPtr = BinOp Add (cuConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst (idx * sz)) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst sz]) noLoc
    pure (RVal out)

  vinsertLane p (JanusCUDA (JanusC mx)) (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ex <- mx
    RVal ev0 <- mv
    res <- emitLocalVal ty ev0
    sx <- emitLocalVal (cuNamedType (simdElemCTypeName (Proxy @a))) ex
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        dstPtr = BinOp Add (cuPtrCast "uint8_t" (UnOp AddrOf res noLoc)) (signedIntConst (idx * sz)) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [dstPtr, UnOp AddrOf sx noLoc, signedIntConst sz]) noLoc
    pure (RVal res)

-- 2. ExpSIMDMask JanusCUDA
instance (SIMDShape n a) => ExpSIMDMask JanusCUDA n a where
  mfalse = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    out <- cuUninit mty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  mtrue = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    out <- cuUninit mty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0xFF, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out)

  mand (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp And x y noLoc) a b

  mor (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp Or x y noLoc) a b

  mxor (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp Xor x y noLoc) a b

  mandnot (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> BinOp And (UnOp Not x noLoc) y noLoc) a b

  mnot (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal <$> emitCUDAPackedWordUn @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x -> UnOp Not x noLoc) a

  vselect (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mt)) (JanusCUDA (JanusC mf)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      RVal em0 <- mm
      RVal et0 <- mt
      RVal ef0 <- mf
      em <- emitLocalVal mty em0
      et <- emitLocalVal vty et0
      ef <- emitLocalVal vty ef0
      let bytes = simdBytes (Proxy @n) (Proxy @a)
          words32 = bytes `div` 4
          JCType u32spec _ = cuNamedType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
          arrTy = JCType u32spec arrDec
      mw <- cuUninit arrTy
      tw <- cuUninit arrTy
      fw <- cuUninit arrTy
      rw <- cuUninit arrTy
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [mw, UnOp AddrOf em noLoc, signedIntConst bytes]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [tw, UnOp AddrOf et noLoc, signedIntConst bytes]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [fw, UnOp AddrOf ef noLoc, signedIntConst bytes]) noLoc
      forM_ [0 .. words32 - 1] $ \w -> do
        let wi = signedIntConst w
            m = Index mw wi noLoc
            t = Index tw wi noLoc
            f = Index fw wi noLoc
            lop3 = BinOp Or (BinOp And m t noLoc) (BinOp And (UnOp Not m noLoc) f noLoc) noLoc
        cuStmt $ Exp (Just $ Assign (Index rw wi noLoc) JustAssign lop3 noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst bytes]) noLoc
      pure (RVal out)

  mmovemask (JanusCUDA (JanusC mm)) = JanusCUDA $ JanusC $ do
    mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
    RVal em0 <- mm
    em <- emitLocalVal mty em0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        bytePtr = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
    acc <- emitLocalVal (cuNamedType "uint32_t") (unsignedIntConst (0 :: Word32))
    forM_ [0 .. lanes - 1] $ \i -> do
      let msb = Index bytePtr (signedIntConst (i * sz + (sz - 1))) noLoc
          bitE = Cond (BinOp And msb (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (bit i :: Word32)) (unsignedIntConst (0 :: Word32)) noLoc
      cuStmt $ Exp (Just $ Assign acc OrAssign bitE noLoc) noLoc
    pure (RVal acc)

  mpopcount m = JanusCUDA $ JanusC $ do
    RVal bits <- getJanusC (getJanusCUDA (mmovemask m))
    pure . RVal $ cuCast "int32_t" (cuCall "__popc" [bits])

  mfirstTrue m = JanusCUDA $ JanusC $ do
    RVal bits <- getJanusC (getJanusCUDA (mmovemask m))
    bvar <- emitLocalVal (cuNamedType "uint32_t") bits
    -- CUDA __ffs(0) == 0, and __ffs(x) is 1-based for x != 0; subtracting 1 yields -1 for 0 and 0-based index otherwise!
    pure . RVal $ BinOp Sub (cuCast "int32_t" (cuCall "__ffs" [bvar])) (signedIntConst (1 :: Int32)) noLoc

  mallOf m =
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        full = if lanes == 32 then maxBound else (1 `shiftL` lanes) - 1
     in mmovemask m `eq` inject full
  manyOf m = mmovemask m `ne` inject 0
  mnoneOf m = mmovemask m `eq` inject 0

-- 3. ExpSIMDCmp JanusCUDA (using native __vcmp*2 / __vcmp*4 for 8-bit & 16-bit!)
emitCUDACmpFallback ::
  forall n a.
  (SIMDShape n a) =>
  BinOp ->
  Exp ->
  Exp ->
  JanusCM Exp
emitCUDACmpFallback cop ea0 eb0 = do
  addHeader "string.h"
  vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
  ea <- emitLocalVal vty ea0
  eb <- emitLocalVal vty eb0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      sz = simdElemBytes (Proxy @a)
      bytes = lanes * sz
      uMaskTy = "uint" <> show (sz * 8) <> "_t"
      JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
      JCType mspec _ = cuNamedType uMaskTy
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  aa <- cuUninit (JCType espec arrDec)
  ab <- cuUninit (JCType espec arrDec)
  am <- cuUninit (JCType mspec arrDec)
  out <- cuUninit mty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  let allOnes = UnOp Not (cuCast uMaskTy (unsignedIntConst (0 :: Word64))) noLoc
      zeroM = cuCast uMaskTy (unsignedIntConst (0 :: Word64))
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
        c = BinOp cop (Index aa idx noLoc) (Index ab idx noLoc) noLoc
    cuStmt $ Exp (Just $ Assign (Index am idx noLoc) JustAssign (Cond c allOnes zeroM noLoc) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, am, signedIntConst bytes]) noLoc
  pure out

cudaVideoCmpName :: JanusElemTag -> BinOp -> Maybe String
cudaVideoCmpName tag cop = case (tag, cop) of
  (JanusElemInt8, Eq) -> Just "__vcmpeq4"
  (JanusElemWord8, Eq) -> Just "__vcmpeq4"
  (JanusElemInt16, Eq) -> Just "__vcmpeq2"
  (JanusElemWord16, Eq) -> Just "__vcmpeq2"
  (JanusElemInt8, Ne) -> Just "__vcmpne4"
  (JanusElemWord8, Ne) -> Just "__vcmpne4"
  (JanusElemInt16, Ne) -> Just "__vcmpne2"
  (JanusElemWord16, Ne) -> Just "__vcmpne2"
  (JanusElemInt8, Lt) -> Just "__vcmplts4"
  (JanusElemWord8, Lt) -> Just "__vcmpltu4"
  (JanusElemInt16, Lt) -> Just "__vcmplts2"
  (JanusElemWord16, Lt) -> Just "__vcmpltu2"
  (JanusElemInt8, Le) -> Just "__vcmples4"
  (JanusElemWord8, Le) -> Just "__vcmpleu4"
  (JanusElemInt16, Le) -> Just "__vcmples2"
  (JanusElemWord16, Le) -> Just "__vcmpleu2"
  (JanusElemInt8, Gt) -> Just "__vcmpgts4"
  (JanusElemWord8, Gt) -> Just "__vcmpgtu4"
  (JanusElemInt16, Gt) -> Just "__vcmpgts2"
  (JanusElemWord16, Gt) -> Just "__vcmpgtu2"
  (JanusElemInt8, Ge) -> Just "__vcmpges4"
  (JanusElemWord8, Ge) -> Just "__vcmpgeu4"
  (JanusElemInt16, Ge) -> Just "__vcmpges2"
  (JanusElemWord16, Ge) -> Just "__vcmpgeu2"
  _ -> Nothing

emitCUDACmp ::
  forall n a.
  (SIMDShape n a) =>
  BinOp ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDMask JanusCUDA n a)
emitCUDACmp cop (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
  RVal a <- ma
  RVal b <- mb
  case cudaVideoCmpName (simdElemTag (Proxy @a)) cop of
    Just fn -> RVal <$> emitCUDAPackedWordBin @(SIMDMask JanusCUDA n a) (simdBytes (Proxy @n) (Proxy @a)) (\x y -> cuCall fn [x, y]) a b
    Nothing -> RVal <$> emitCUDACmpFallback @n @a cop a b

instance (SIMDShape n a) => ExpSIMDCmp JanusCUDA n a where
  veq = emitCUDACmp Eq
  vne = emitCUDACmp Ne
  vlt = emitCUDACmp Lt
  vle = emitCUDACmp Le
  vgt = emitCUDACmp Gt
  vge = emitCUDACmp Ge

-- 4. ExpSIMDArith JanusCUDA (__vadd2/4, __vsub2/4, __vneg2/4, __vabs2/4, __vmin*2/4, __vmax*2/4)
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith JanusCUDA n a where
  vadd (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vadd2" [x, y]) a b
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneBin @n @a (\x y -> BinOp Add x y noLoc) a b
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vsub (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsub2" [x, y]) a b
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneBin @n @a (\x y -> BinOp Sub x y noLoc) a b
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vmul (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> if simdElemIsFloat (Proxy @a)
      then emitCUDALaneBin @n @a (\x y -> BinOp Mul x y noLoc) a b
      else do
        let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
        emitCUDALaneBin @n @a (\x y -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Mul (cuCast uTy x) (cuCast uTy y) noLoc)) a b

  vneg (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg4" [x]) a
      JanusElemWord8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg4" [x]) a
      JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg2" [x]) a
      JanusElemWord16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vneg2" [x]) a
      _
        | simdElemIsFloat (Proxy @a) -> emitCUDALaneUn @n @a (\x -> UnOp Negate x noLoc) a
        | otherwise -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneUn @n @a (\x -> cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (unsignedIntConst (0 :: Word64)) (cuCast uTy x) noLoc)) a

  vabs v@(JanusCUDA (JanusC ma))
    | not (simdElemSigned (Proxy @a)) = v
    | otherwise = JanusCUDA $ JanusC $ do
        RVal a <- ma
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabs4" [x]) a
          JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabs2" [x]) a
          JanusElemFloat -> emitCUDALaneUn @n @a (\x -> cuCall "fabsf" [x]) a
          JanusElemDouble -> emitCUDALaneUn @n @a (\x -> cuCall "fabs" [x]) a
          _ -> do
            let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
            emitCUDALaneUn @n @a (\x -> Cond (BinOp Lt x (cuInt 0) noLoc) (cuCast (simdElemCTypeName (Proxy @a)) (BinOp Sub (unsignedIntConst (0 :: Word64)) (cuCast uTy x) noLoc)) x noLoc) a

  vmin (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmins4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vminu4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmins2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vminu2" [x, y]) a b
      _ -> emitCUDALaneBin @n @a (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc) a b

  vmax (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        tag = simdElemTag (Proxy @a)
    RVal <$> case tag of
      JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxs4" [x, y]) a b
      JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxu4" [x, y]) a b
      JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxs2" [x, y]) a b
      JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vmaxu2" [x, y]) a b
      _ -> emitCUDALaneBin @n @a (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc) a b

-- 5. ExpSIMDSat JanusCUDA (__vaddss*, __vaddus*, __vsubss*, __vsubus*, __vabsss*, __vavgu*, __vabsdiff*)
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat JanusCUDA n a where
  vadds a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vadd a b
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddss4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddus4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddss2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vaddus2" [x, y]) ea eb
          _ | not (simdElemSigned (Proxy @a)) ->
            emitCUDALaneBin @n @a (\x y -> Cond (BinOp Lt (BinOp Add x y noLoc) x noLoc) (UnOp Not (cuCast (simdElemCTypeName (Proxy @a)) (unsignedIntConst (0 :: Word64))) noLoc) (BinOp Add x y noLoc) noLoc) ea eb
          JanusElemInt32 -> do
            let s x y = BinOp Add (cuCast "int64_t" x) (cuCast "int64_t" y) noLoc
                mx = signedIntConst (fromIntegral (maxBound :: Int32) :: Int64)
                mn = signedIntConst (fromIntegral (minBound :: Int32) :: Int64)
            emitCUDALaneBin @n @a (\x y -> cuCast "int32_t" (Cond (BinOp Gt (s x y) mx noLoc) mx (Cond (BinOp Lt (s x y) mn noLoc) mn (s x y) noLoc) noLoc)) ea eb
          _ -> do
            -- 64-bit signed saturating add without __int128
            let body x y =
                  let sumU = BinOp Add (cuCast "uint64_t" x) (cuCast "uint64_t" y) noLoc
                      resS = cuCast "int64_t" sumU
                      ovf = BinOp Ge (BinOp And (UnOp Not (BinOp Xor x y noLoc) noLoc) (BinOp Xor x resS noLoc) noLoc) (cuInt 0) noLoc
                      satVal = Cond (BinOp Lt x (cuInt 0) noLoc) (cuVar "INT64_MIN") (cuVar "INT64_MAX") noLoc
                   in Cond ovf resS satVal noLoc
            emitCUDALaneBin @n @a body ea eb

  vsubs a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vsub a b
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubss4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubus4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubss2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vsubus2" [x, y]) ea eb
          _ | not (simdElemSigned (Proxy @a)) ->
            emitCUDALaneBin @n @a (\x y -> Cond (BinOp Ge x y noLoc) (BinOp Sub x y noLoc) (unsignedIntConst (0 :: Word64)) noLoc) ea eb
          JanusElemInt32 -> do
            let d x y = BinOp Sub (cuCast "int64_t" x) (cuCast "int64_t" y) noLoc
                mx = signedIntConst (fromIntegral (maxBound :: Int32) :: Int64)
                mn = signedIntConst (fromIntegral (minBound :: Int32) :: Int64)
            emitCUDALaneBin @n @a (\x y -> cuCast "int32_t" (Cond (BinOp Gt (d x y) mx noLoc) mx (Cond (BinOp Lt (d x y) mn noLoc) mn (d x y) noLoc) noLoc)) ea eb
          _ -> do
            let body x y =
                  let diffU = BinOp Sub (cuCast "uint64_t" x) (cuCast "uint64_t" y) noLoc
                      resS = cuCast "int64_t" diffU
                      noOvf = BinOp Ge (BinOp And (BinOp Xor x y noLoc) (BinOp Xor x resS noLoc) noLoc) (cuInt 0) noLoc
                      satVal = Cond (BinOp Lt x (cuInt 0) noLoc) (cuVar "INT64_MIN") (cuVar "INT64_MAX") noLoc
                   in Cond noOvf resS satVal noLoc
            emitCUDALaneBin @n @a body ea eb

  vabss v@(JanusCUDA (JanusC ma))
    | not (simdElemSigned (Proxy @a)) || simdElemIsFloat (Proxy @a) = vabs v
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabsss4" [x]) ea
          JanusElemInt16 -> emitCUDAPackedWordUn @(SIMDVector JanusCUDA n a) bytes (\x -> cuCall "__vabsss2" [x]) ea
          _ -> do
            let bits = simdElemBytes (Proxy @a) * 8
                sminE = if bits == 64 then cuVar "INT64_MIN" else cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64))
                smaxE = if bits == 64 then cuVar "INT64_MAX" else cuCast (simdElemCTypeName (Proxy @a)) (signedIntConst (((1 `shiftL` (bits - 1)) - 1) :: Int64))
                absBody x = Cond (BinOp Eq x sminE noLoc) smaxE (Cond (BinOp Lt x (cuInt 0) noLoc) (UnOp Negate x noLoc) x noLoc) noLoc
            emitCUDALaneUn @n @a absBody ea

  vavg (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let halfLit = if simdElemTag (Proxy @a) == JanusElemFloat then C.Const (FloatConst "0.5f" 0.5 noLoc) noLoc else C.Const (DoubleConst "0.5" 0.5 noLoc) noLoc
        RVal <$> emitCUDALaneBin @n @a (\x y -> BinOp Mul halfLit (BinOp Add x y noLoc) noLoc) ea eb
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
            b8 = unsignedIntConst (0x80808080 :: Word32)
            b16 = unsignedIntConst (0x80008000 :: Word32)
        RVal <$> case tag of
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vavgu4" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vavgu2" [x, y]) ea eb
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> BinOp Xor (cuCall "__vavgu4" [BinOp Xor x b8 noLoc, BinOp Xor y b8 noLoc]) b8 noLoc) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> BinOp Xor (cuCall "__vavgu2" [BinOp Xor x b16 noLoc, BinOp Xor y b16 noLoc]) b16 noLoc) ea eb
          _ -> do
            -- Branchless Dietz rounding average: (x | y) - ((x ^ y) >> 1)
            emitCUDALaneBin @n @a (\x y -> BinOp Sub (BinOp Or x y noLoc) (BinOp Rsh (BinOp Xor x y noLoc) (cuInt 1) noLoc) noLoc) ea eb

  vabsdiff a@(JanusCUDA (JanusC ma)) b@(JanusCUDA (JanusC mb))
    | simdElemIsFloat (Proxy @a) = vabs (vsub a b)
    | otherwise = JanusCUDA $ JanusC $ do
        RVal ea <- ma
        RVal eb <- mb
        let bytes = simdBytes (Proxy @n) (Proxy @a)
            tag = simdElemTag (Proxy @a)
        RVal <$> case tag of
          JanusElemInt8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffs4" [x, y]) ea eb
          JanusElemWord8 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffu4" [x, y]) ea eb
          JanusElemInt16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffs2" [x, y]) ea eb
          JanusElemWord16 -> emitCUDAPackedWordBin @(SIMDVector JanusCUDA n a) bytes (\x y -> cuCall "__vabsdiffu2" [x, y]) ea eb
          _ -> do
            let a' = JanusCUDA (JanusC (pure (RVal ea))) :: JanusCUDA (SIMDVector JanusCUDA n a)
                b' = JanusCUDA (JanusC (pure (RVal eb))) :: JanusCUDA (SIMDVector JanusCUDA n a)
            RVal eRes <- getJanusC $ getJanusCUDA $ vsub (vmax a' b') (vmin a' b')
            pure eRes

-- 6. ExpSIMDFloat JanusCUDA
emitCUDAFloatTern ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a)
emitCUDAFloatTern fn (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
  JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    RVal ec0 <- mc
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    ec <- emitLocalVal ty ec0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    aa <- cuUninit arrTy
    ab <- cuUninit arrTy
    ac <- cuUninit arrTy
    ar <- cuUninit arrTy
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ac, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let idx = signedIntConst i
      cuStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (fn (Index aa idx noLoc) (Index ab idx noLoc) (Index ac idx noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

instance (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a) => ExpSIMDFloat JanusCUDA n a where
  vdiv (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    RVal <$> emitCUDALaneBin @n @a (\x y -> BinOp Div x y noLoc) a b

  vsqrt (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn = if simdElemTag (Proxy @a) == JanusElemFloat then "sqrtf" else "sqrt"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall fn [x]) a

  vrsqrtApprox (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn = if simdElemTag (Proxy @a) == JanusElemFloat then "rsqrtf" else "rsqrt"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall fn [x]) a

  vrcpApprox (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let fn x = if simdElemTag (Proxy @a) == JanusElemFloat then cuCall "__frcp_rn" [x] else cuCall "__drcp_rn" [x]
    RVal <$> emitCUDALaneUn @n @a fn a

  vfma =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [x, y, z])
  vfms =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [x, y, UnOp Negate z noLoc])
  vfnma =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [UnOp Negate x noLoc, y, z])
  vfnms =
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "fmaf" else "fma"
     in emitCUDAFloatTern (\x y z -> cuCall f [UnOp Negate x noLoc, y, UnOp Negate z noLoc])
  vmulAdd = vfma

  vfloor (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "floorf" else "floor"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vceil (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "ceilf" else "ceil"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vtrunc (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "truncf" else "trunc"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a
  vroundNearest (JanusCUDA (JanusC ma)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    let f = if simdElemTag (Proxy @a) == JanusElemFloat then "rintf" else "rint"
    RVal <$> emitCUDALaneUn @n @a (\x -> cuCall f [x]) a

-- 7. ExpSIMDBits JanusCUDA
emitCUDABitWordBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA (SIMDVector JanusCUDA n a)
emitCUDABitWordBin opW (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  RVal ea0 <- ma
  RVal eb0 <- mb
  ea <- emitLocalVal ty ea0
  eb <- emitLocalVal ty eb0
  let bytes = simdBytes (Proxy @n) (Proxy @a)
      words32 = bytes `div` 4
      JCType u32spec _ = cuNamedType "uint32_t"
      arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType u32spec arrDec
  aw <- cuUninit arrTy
  bw <- cuUninit arrTy
  rw <- cuUninit arrTy
  out <- cuUninit ty
  cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. words32 - 1] $ \w -> do
    let wi = signedIntConst w
    cuStmt $ Exp (Just $ Assign (Index rw wi noLoc) JustAssign (opW (Index aw wi noLoc) (Index bw wi noLoc)) noLoc) noLoc
  cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst bytes]) noLoc
  pure (RVal out)

instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits JanusCUDA n a where
  vand = emitCUDABitWordBin (\x y -> BinOp And x y noLoc)
  vor = emitCUDABitWordBin (\x y -> BinOp Or x y noLoc)
  vxor = emitCUDABitWordBin (\x y -> BinOp Xor x y noLoc)
  vandnot = emitCUDABitWordBin (\x y -> BinOp And (UnOp Not x noLoc) y noLoc)
  vnot v = vxor v (JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memset" [UnOp AddrOf out noLoc, cuInt 0xFF, signedIntConst (simdBytes (Proxy @n) (Proxy @a))]) noLoc
    pure (RVal out))

  vshl (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = cuNamedType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- cuUninit (JCType uspec arrDec)
        out <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= cuUninit
        evLoc <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= (`emitLocalVal` ev)
        cuStmt $ Exp (Just $ cuCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              sh = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (Index arrU idx noLoc) c noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign sh noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal out)
      else do
        let body x = cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cuCast uTy x) c noLoc) noLoc
        RVal <$> emitCUDALaneUn @n @a body ev

  vshr (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = cuNamedType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- cuUninit (JCType uspec arrDec)
        out <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= cuUninit
        evLoc <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a)) >>= (`emitLocalVal` ev)
        cuStmt $ Exp (Just $ cuCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              sh = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (Index arrU idx noLoc) c noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign sh noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal out)
      else do
        let body x
              | simdElemSigned (Proxy @a) =
                  let sx = cuCast sTy x
                      fill = Cond (BinOp Lt sx (cuInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cuInt 0) noLoc
                   in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx c noLoc) noLoc
              | otherwise =
                  cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cuCast uTy x) c noLoc) noLoc
        RVal <$> emitCUDALaneUn @n @a body ev

  vshlImm p v = vshl v (fromIntegral (natVal p))
  vshrImm p v = vshr v (fromIntegral (natVal p))

  vshlVar (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        body x y =
          let uc = cuCast uTy y
           in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cuCast uTy x) uc noLoc) noLoc
    RVal <$> emitCUDALaneBin @n @a body a b

  vshrVar (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    RVal a <- ma
    RVal b <- mb
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
        body x y
          | simdElemSigned (Proxy @a) =
              let uc = cuCast uTy y
                  sx = cuCast sTy x
                  fill = Cond (BinOp Lt sx (cuInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cuInt 0) noLoc
               in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx uc noLoc) noLoc
          | otherwise =
              let uc = cuCast uTy y
               in cuCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cuCast uTy x) uc noLoc) noLoc
    RVal <$> emitCUDALaneBin @n @a body a b

  vpopcnt (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    RVal ev <- mv
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        popFn = if bits == 64 then "__popcll" else "__popc"
        argCast = if bits == 64 then "unsigned long long" else "uint32_t"
        body x = cuCast (simdElemCTypeName (Proxy @a)) (cuCall popFn [cuCast argCast (cuCast uTy x)])
    RVal <$> emitCUDALaneUn @n @a body ev

-- 8. ExpSIMDShuffle & ExpSIMDSplit JanusCUDA
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle JanusCUDA n a where
  vshuffleLanes idxs (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    when (length idxs /= lanes) $ error "vshuffleLanes: index list length mismatch"
    sa <- cuUninit arrTy
    da <- cuUninit arrTy
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ (zip [0 :: Int ..] idxs) $ \(outI, inI) ->
      cuStmt $ Exp (Just $ Assign (Index da (signedIntConst outI) noLoc) JustAssign (Index sa (signedIntConst (inI .&. (lanes - 1))) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
    pure (RVal out)

  vpermuteVar (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC midx)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
    RVal ev0 <- mv
    RVal eidx0 <- midx
    ev <- emitLocalVal ty ev0
    eidx <- emitLocalVal ity eidx0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType ispec _ = cuNamedType "int32_t"
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    sa <- cuUninit (JCType espec arrDec)
    ia <- cuUninit (JCType ispec arrDec)
    da <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let k = BinOp And (cuCast "uint32_t" (Index ia (signedIntConst i) noLoc)) (unsignedIntConst (lanes - 1)) noLoc
      cuStmt $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index sa k noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
    pure (RVal out)

  vshuffleBytes ::
    forall b.
    (SIMDShape b Word8) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA b Word8) ->
    JanusCUDA (SIMDVector JanusCUDA n a)
  vshuffleBytes (JanusCUDA (JanusC mv)) (JanusCUDA (JanusC mc)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    cty <- getJanusCType (Proxy @(SIMDVector JanusCUDA b Word8))
    RVal ev0 <- mv
    RVal ec0 <- mc
    ev <- emitLocalVal ty ev0
    ec <- emitLocalVal cty ec0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 4
      then do
        -- Use CUDA __byte_perm for 4-byte vectors + high-bit zero mask!
        -- Selector nibbles: (c0 & 3) | ((c1 & 3) << 4) | ((c2 & 3) << 8) | ((c3 & 3) << 12)
        let nib (k :: Int) = BinOp Lsh (BinOp And (BinOp Rsh ec (cuInt (k * 8)) noLoc) (unsignedIntConst (3 :: Word32)) noLoc) (cuInt (k * 4)) noLoc
            sel = foldl1 (\x y -> BinOp Or x y noLoc) [nib 0, nib 1, nib 2, nib 3]
            permed = cuCall "__byte_perm" [ev, unsignedIntConst (0 :: Word32), sel]
            zeroMask = cuCall "__vcmpltu4" [ec, unsignedIntConst (0x80808080 :: Word32)]
        RVal <$> emitLocalVal ty (BinOp And permed zeroMask noLoc)
      else do
        let JCType bspec _ = cuNamedType "uint8_t"
            arrDec = Array [] (ArraySize False (signedIntConst bytes) noLoc) (DeclRoot noLoc) noLoc
        sb <- cuUninit (JCType bspec arrDec)
        cb <- cuUninit (JCType bspec arrDec)
        db <- cuUninit (JCType bspec arrDec)
        out <- cuUninit ty
        cuStmt $ Exp (Just $ cuCall "memcpy" [sb, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [cb, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. bytes - 1] $ \i -> do
          let ci = Index cb (signedIntConst i) noLoc
              s = Cond (BinOp And ci (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (0 :: Word32)) (Index sb (BinOp And ci (unsignedIntConst (bytes - 1)) noLoc) noLoc) noLoc
          cuStmt $ Exp (Just $ Assign (Index db (signedIntConst i) noLoc) JustAssign s noLoc) noLoc
        cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, db, signedIntConst bytes]) noLoc
        pure (RVal out)

  vzipLo (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        half = lanes `div` 2
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    aa <- cuUninit (JCType espec arrDec)
    ab <- cuUninit (JCType espec arrDec)
    ar <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst i) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst i) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

  vzipHi (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        half = lanes `div` 2
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    aa <- cuUninit (JCType espec arrDec)
    ab <- cuUninit (JCType espec arrDec)
    ar <- cuUninit (JCType espec arrDec)
    out <- cuUninit ty
    cuStmt $ Exp (Just $ cuCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst (half + i)) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst (half + i)) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, ar, signedIntConst bytes]) noLoc
    pure (RVal out)

instance (SIMDHalfPair n2 n a) => ExpSIMDSplit JanusCUDA n2 n a where
  vsplit (JanusCUDA (JanusC mv)) =
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        extractPart off = JanusCUDA $ JanusC $ do
          addHeader "string.h"
          ty2 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n2 a))
          ty1 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
          RVal ev0 <- mv
          ev <- emitLocalVal ty2 ev0
          out <- cuUninit ty1
          cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, BinOp Add (cuConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst off) noLoc, signedIntConst halfB]) noLoc
          pure (RVal out)
     in (extractPart 0, extractPart halfB)

  vconcat (JanusCUDA (JanusC mlo)) (JanusCUDA (JanusC mhi)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty1 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    ty2 <- getJanusCType (Proxy @(SIMDVector JanusCUDA n2 a))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal ty1 elo0
    ehi <- emitLocalVal ty1 ehi0
    out <- cuUninit ty2
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        dstPtr = cuPtrCast "uint8_t" (UnOp AddrOf out noLoc)
    cuStmt $ Exp (Just $ cuCall "memcpy" [dstPtr, UnOp AddrOf elo noLoc, signedIntConst halfB]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [BinOp Add dstPtr (signedIntConst halfB) noLoc, UnOp AddrOf ehi noLoc, signedIntConst halfB]) noLoc
    pure (RVal out)

-- 9. ExpSIMDReduce JanusCUDA
emitCUDAReduce ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  JanusCUDA (SIMDVector JanusCUDA n a) ->
  JanusCUDA a
emitCUDAReduce combine (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
  RVal ev0 <- mv
  ev <- emitLocalVal ty ev0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemName = simdElemCTypeName (Proxy @a)
      JCType espec _ = cuNamedType elemName
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  arr <- cuUninit (JCType espec arrDec)
  cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
  acc <- emitLocalVal (cuNamedType elemName) (Index arr (cuInt 0) noLoc)
  forM_ [1 .. lanes - 1] $ \i ->
    cuStmt $ Exp (Just $ Assign acc JustAssign (combine acc (Index arr (signedIntConst i) noLoc)) noLoc) noLoc
  pure (RVal acc)

instance (SIMDShape n a, CPUSIMDElem a, JanusCTyped a) => ExpSIMDReduce JanusCUDA n a where
  vreduceAdd =
    let uTy = "uint" <> show (simdElemBytes (Proxy @a) * 8) <> "_t"
        comb x y
          | simdElemIsFloat (Proxy @a) = BinOp Add x y noLoc
          | otherwise = cuCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cuCast uTy x) (cuCast uTy y) noLoc)
     in emitCUDAReduce comb
  vreduceMin = emitCUDAReduce (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc)
  vreduceMax = emitCUDAReduce (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc)
  vreduceAnd (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc AndAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceOr (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc OrAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceXor (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = cuNamedType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType uspec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (cuNamedType uTy) (Index arr (cuInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> cuStmt $ Exp (Just $ Assign acc XorAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- cuUninit (cuNamedType (simdElemCTypeName (Proxy @a)))
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)

-- 10. ExpSIMDDot JanusCUDA (__dp4a, __dp2a_lo, __dp2a_hi on sm_61+, __vsadu4)
emitCUDADp4aSWord :: CUDAFeatures -> Exp -> Exp -> Exp -> JanusCM Exp
emitCUDADp4aSWord fs ea eb ec
  | cudaHasDP4A fs = do
      addHeader "sm_61_intrinsics.h"
      emitLocalVal (cuNamedType "int32_t") (cuCall "__dp4a" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
  | otherwise = do
      a <- emitLocalVal (cuNamedType "uint32_t") ea
      b <- emitLocalVal (cuNamedType "uint32_t") eb
      acc <- emitLocalVal (cuNamedType "uint32_t") (cuCast "uint32_t" ec)
      let byteS u k = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh u (signedIntConst (k * 8)) noLoc))
          term k = cuCast "uint32_t" (BinOp Mul (byteS a k) (byteS b k) noLoc)
          s4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
      emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" s4)

emitCUDADp4aUWord :: CUDAFeatures -> Exp -> Exp -> Exp -> JanusCM Exp
emitCUDADp4aUWord fs ea eb ec
  | cudaHasDP4A fs = do
      addHeader "sm_61_intrinsics.h"
      emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp4a" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
  | otherwise = do
      a <- emitLocalVal (cuNamedType "uint32_t") ea
      b <- emitLocalVal (cuNamedType "uint32_t") eb
      acc <- emitLocalVal (cuNamedType "uint32_t") ec
      let byteU u k = BinOp And (BinOp Rsh u (signedIntConst (k * 8)) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          term k = BinOp Mul (byteU a k) (byteU b k) noLoc
          s4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
      emitLocalVal (cuNamedType "uint32_t") s4

instance ExpSIMDDot JanusCUDA where
  vdp4aS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitCUDADp4aSWord fs a b c

  vdp4aU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitCUDADp4aUWord fs a b c

  vdp4aVecS ::
    forall m k.
    (SIMDDp4aShape m k) =>
    JanusCUDA (SIMDVector JanusCUDA m Int8) ->
    JanusCUDA (SIMDVector JanusCUDA m Int8) ->
    JanusCUDA (SIMDVector JanusCUDA k Int32) ->
    JanusCUDA (SIMDVector JanusCUDA k Int32)
  vdp4aVecS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      fs <- askCUDAFeatures
      tyA <- getJanusCType (Proxy @(SIMDVector JanusCUDA m Int8))
      tyK <- getJanusCType (Proxy @(SIMDVector JanusCUDA k Int32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Int32)
          JCType u32spec _ = cuNamedType "uint32_t"
          JCType i32spec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- cuUninit (JCType u32spec arrDec)
      bw <- cuUninit (JCType u32spec arrDec)
      cw <- cuUninit (JCType i32spec arrDec)
      rw <- cuUninit (JCType i32spec arrDec)
      out <- cuUninit tyK
      cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitCUDADp4aSWord fs (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        cuStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal out)

  vdp4aVecU ::
    forall m k.
    (SIMDDp4aShape m k) =>
    JanusCUDA (SIMDVector JanusCUDA m Word8) ->
    JanusCUDA (SIMDVector JanusCUDA m Word8) ->
    JanusCUDA (SIMDVector JanusCUDA k Word32) ->
    JanusCUDA (SIMDVector JanusCUDA k Word32)
  vdp4aVecU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      fs <- askCUDAFeatures
      tyA <- getJanusCType (Proxy @(SIMDVector JanusCUDA m Word8))
      tyK <- getJanusCType (Proxy @(SIMDVector JanusCUDA k Word32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Word32)
          JCType u32spec _ = cuNamedType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- cuUninit (JCType u32spec arrDec)
      bw <- cuUninit (JCType u32spec arrDec)
      cw <- cuUninit (JCType u32spec arrDec)
      rw <- cuUninit (JCType u32spec arrDec)
      out <- cuUninit tyK
      cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitCUDADp4aUWord fs (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        cuStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal out)

  vdp2aLoS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCall "__dp2a_lo" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = cuCast "int32_t" (cuCast "int16_t" a)
              a1 = cuCast "int32_t" (cuCast "int16_t" (BinOp Rsh a (cuInt 16) noLoc))
              b0 = cuCast "int32_t" (cuCast "int8_t" b)
              b1 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 8) noLoc))
              prod = cuCast "uint32_t" (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc)
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" (BinOp Add (cuCast "uint32_t" ec) prod noLoc))

  vdp2aHiS (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCall "__dp2a_hi" [cuCast "int" ea, cuCast "int" eb, cuCast "int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = cuCast "int32_t" (cuCast "int16_t" a)
              a1 = cuCast "int32_t" (cuCast "int16_t" (BinOp Rsh a (cuInt 16) noLoc))
              b2 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 16) noLoc))
              b3 = cuCast "int32_t" (cuCast "int8_t" (BinOp Rsh b (cuInt 24) noLoc))
              prod = cuCast "uint32_t" (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc)
          RVal <$> emitLocalVal (cuNamedType "int32_t") (cuCast "int32_t" (BinOp Add (cuCast "uint32_t" ec) prod noLoc))

  vdp2aLoU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp2a_lo" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
              a1 = BinOp Rsh a (cuInt 16) noLoc
              b0 = BinOp And b (unsignedIntConst (0xFF :: Word32)) noLoc
              b1 = BinOp And (BinOp Rsh b (cuInt 8) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc) noLoc)

  vdp2aHiU (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) (JanusCUDA (JanusC mc)) =
    JanusCUDA $ JanusC $ do
      fs <- askCUDAFeatures
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      if cudaHasDP2A fs
        then do
          addHeader "sm_61_intrinsics.h"
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__dp2a_hi" [cuCast "unsigned int" ea, cuCast "unsigned int" eb, cuCast "unsigned int" ec])
        else do
          a <- emitLocalVal (cuNamedType "uint32_t") ea
          b <- emitLocalVal (cuNamedType "uint32_t") eb
          let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
              a1 = BinOp Rsh a (cuInt 16) noLoc
              b2 = BinOp And (BinOp Rsh b (cuInt 16) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
              b3 = BinOp Rsh b (cuInt 24) noLoc
          RVal <$> emitLocalVal (cuNamedType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc) noLoc)

  vsad8 ::
    forall n.
    (SIMDShape n Word8) =>
    JanusCUDA (SIMDVector JanusCUDA n Word8) ->
    JanusCUDA (SIMDVector JanusCUDA n Word8) ->
    JanusCUDA Word32
  vsad8 (JanusCUDA (JanusC ma)) (JanusCUDA (JanusC mb)) =
    JanusCUDA $ JanusC $ do
      addHeader "string.h"
      ty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Word8))
      RVal ea0 <- ma
      RVal eb0 <- mb
      ea <- emitLocalVal ty ea0
      eb <- emitLocalVal ty eb0
      let bytes = simdBytes (Proxy @n) (Proxy @Word8)
          words32 = bytes `div` 4
      if words32 == 1
        then RVal <$> emitLocalVal (cuNamedType "uint32_t") (cuCall "__vsadu4" [ea, eb])
        else do
          let JCType u32spec _ = cuNamedType "uint32_t"
              arrDec = Array [] (ArraySize False (signedIntConst words32) noLoc) (DeclRoot noLoc) noLoc
          aw <- cuUninit (JCType u32spec arrDec)
          bw <- cuUninit (JCType u32spec arrDec)
          cuStmt $ Exp (Just $ cuCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
          cuStmt $ Exp (Just $ cuCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
          acc <- emitLocalVal (cuNamedType "uint32_t") (unsignedIntConst (0 :: Word32))
          forM_ [0 .. words32 - 1] $ \w -> do
            let wi = signedIntConst w
            cuStmt $ Exp (Just $ Assign acc AddAssign (cuCall "__vsadu4" [Index aw wi noLoc, Index bw wi noLoc]) noLoc) noLoc
          pure (RVal acc)

-- 11. ExpSIMDCast JanusCUDA
instance ExpSIMDCast JanusCUDA where
  vbitcast ::
    forall n a m b.
    (SIMDShape n a, SIMDShape m b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA m b)
  vbitcast (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    let b1 = simdBytes (Proxy @n) (Proxy @a)
        b2 = simdBytes (Proxy @m) (Proxy @b)
    when (b1 /= b2) $ error "vbitcast: mismatched byte widths"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA m b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf ev noLoc, signedIntConst b1]) noLoc
    pure (RVal out)

  vcvt ::
    forall n a b.
    (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA n b)
  vcvt (JanusCUDA (JanusC mv)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sb = simdBytes (Proxy @n) (Proxy @a)
        db = simdBytes (Proxy @n) (Proxy @b)
        JCType sspec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = cuNamedType (simdElemCTypeName (Proxy @b))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    sa <- cuUninit (JCType sspec arrDec)
    da <- cuUninit (JCType dspec arrDec)
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst sb]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let idx = signedIntConst i
      cuStmt $ Exp (Just $ Assign (Index da idx noLoc) JustAssign (cuCast (simdElemCTypeName (Proxy @b)) (Index sa idx noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst db]) noLoc
    pure (RVal out)

  vnarrowSat ::
    forall n m a b.
    (SIMDPackShape n m a b) =>
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA n a) ->
    JanusCUDA (SIMDVector JanusCUDA m b)
  vnarrowSat (JanusCUDA (JanusC mlo)) (JanusCUDA (JanusC mhi)) = JanusCUDA $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector JanusCUDA m b))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal srcTy elo0
    ehi <- emitLocalVal srcTy ehi0
    let half = simdLanes (Proxy @n) (Proxy @a)
        total = 2 * half
        sb = simdBytes (Proxy @n) (Proxy @a)
        db = simdBytes (Proxy @m) (Proxy @b)
        dstBits = simdElemBytes (Proxy @b) * 8
        dstSigned = simdElemSigned (Proxy @b)
        dmin = if dstSigned then -(1 `shiftL` (dstBits - 1) :: Int64) else 0
        dmax = (if dstSigned then (1 `shiftL` (dstBits - 1)) - 1 else (1 `shiftL` dstBits) - 1) :: Int64
        JCType sspec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = cuNamedType (simdElemCTypeName (Proxy @b))
        sDec = Array [] (ArraySize False (signedIntConst half) noLoc) (DeclRoot noLoc) noLoc
        dDec = Array [] (ArraySize False (signedIntConst total) noLoc) (DeclRoot noLoc) noLoc
    alo <- cuUninit (JCType sspec sDec)
    ahi <- cuUninit (JCType sspec sDec)
    adst <- cuUninit (JCType dspec dDec)
    out <- cuUninit dstTy
    cuStmt $ Exp (Just $ cuCall "memcpy" [alo, UnOp AddrOf elo noLoc, signedIntConst sb]) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [ahi, UnOp AddrOf ehi noLoc, signedIntConst sb]) noLoc
    let clampExp v =
          let v64 = cuCast "int64_t" v
              loE = signedIntConst dmin
              hiE = signedIntConst dmax
           in cuCast (simdElemCTypeName (Proxy @b)) (Cond (BinOp Lt v64 loE noLoc) loE (Cond (BinOp Gt v64 hiE noLoc) hiE v64 noLoc) noLoc)
    forM_ [0 .. half - 1] $ \i -> do
      cuStmt $ Exp (Just $ Assign (Index adst (signedIntConst i) noLoc) JustAssign (clampExp (Index alo (signedIntConst i) noLoc)) noLoc) noLoc
      cuStmt $ Exp (Just $ Assign (Index adst (signedIntConst (half + i)) noLoc) JustAssign (clampExp (Index ahi (signedIntConst i) noLoc)) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, adst, signedIntConst db]) noLoc
    pure (RVal out)

-- 12. CmdSIMDMem JanusCUDAM JanusCUDA
instance (SIMDShape n a, Typeable a, JanusCTyped a) => CmdSIMDMem JanusCUDAM JanusCUDA n a where
  vload (JanusCUDA (JanusC mp)) = JanusCUDAM $ lift $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    pty <- getJanusCType (Proxy @(Ptr a))
    RVal ep0 <- mp
    ep <- emitLocalVal pty ep0
    out <- cuUninit vty
    -- Safe element-by-element load for unaligned global/shared pointers
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
    cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
    pure (JanusCUDA (JanusC (pure (RVal out))))

  vloadAligned (JanusCUDA (JanusC mp)) = JanusCUDAM $ lift $ do
    vty@(JCType spec dec) <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ep0 <- mp
    -- Coalesced wide load (e.g. ld.global.v4.f32 / ld.global.v4.u32)
    let castP = Cast (Type (setTypeQualifiers [Tconst noLoc] spec) (Ptr [] dec noLoc) noLoc) ep0 noLoc
    out <- emitLocalVal vty (UnOp Deref castP noLoc)
    pure (JanusCUDA (JanusC (pure (RVal out))))

  vstore (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) = JanusCUDAM $ lift $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    pty <- getJanusCType (Proxy @(Ptr a))
    RVal ep0 <- mp
    RVal ev0 <- mv
    ep <- emitLocalVal pty ep0
    ev <- emitLocalVal vty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- cuUninit (JCType espec arrDec)
    cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ [0 .. lanes - 1] $ \i ->
      cuStmt $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc

  vstoreAligned (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) = JanusCUDAM $ lift $ do
    vty@(JCType spec dec) <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
    RVal ep0 <- mp
    RVal ev0 <- mv
    ev <- emitLocalVal vty ev0
    let castP = Cast (Type spec (Ptr [] dec noLoc) noLoc) ep0 noLoc
    cuStmt $ Exp (Just $ Assign (UnOp Deref castP noLoc) JustAssign ev noLoc) noLoc

  vloadMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mfb)) (JanusCUDA (JanusC mp)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal efb0 <- mfb
      RVal ep0 <- mp
      em <- emitLocalVal mty em0
      efb <- emitLocalVal vty efb0
      ep <- emitLocalVal pty ep0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignLoad = BlockStm $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignLoad] noLoc) Nothing noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))

  vstoreMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC mv)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal ep0 <- mp
      RVal ev0 <- mv
      em <- emitLocalVal mty em0
      ep <- emitLocalVal pty ep0
      ev <- emitLocalVal vty ev0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- cuUninit (JCType espec arrDec)
      cuStmt $ Exp (Just $ cuCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignStore = BlockStm $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignStore] noLoc) Nothing noLoc

  vgather (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC midx)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal ep0 <- mp
      RVal eidx0 <- midx
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          bytes = simdBytes (Proxy @n) (Proxy @a)
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      ia <- cuUninit (JCType ispec arrDec)
      da <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      forM_ [0 .. lanes - 1] $ \i ->
        cuStmt $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index ep (Index ia (signedIntConst i) noLoc) noLoc) noLoc) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))

  vgatherMasked (JanusCUDA (JanusC mm)) (JanusCUDA (JanusC mfb)) (JanusCUDA (JanusC mp)) (JanusCUDA (JanusC midx)) =
    JanusCUDAM $ lift $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask JanusCUDA n a))
      vty <- getJanusCType (Proxy @(SIMDVector JanusCUDA n a))
      ity <- getJanusCType (Proxy @(SIMDVector JanusCUDA n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal em0 <- mm
      RVal efb0 <- mfb
      RVal ep0 <- mp
      RVal eidx0 <- midx
      em <- emitLocalVal mty em0
      efb <- emitLocalVal vty efb0
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          bytes = lanes * sz
          JCType espec _ = cuNamedType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = cuNamedType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      ia <- cuUninit (JCType ispec arrDec)
      da <- cuUninit (JCType espec arrDec)
      out <- cuUninit vty
      cuStmt $ Exp (Just $ cuCall "memcpy" [ia, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [da, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cuConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignGather = BlockStm $ Exp (Just $ Assign (Index da (signedIntConst i) noLoc) JustAssign (Index ep (Index ia (signedIntConst i) noLoc) noLoc) noLoc) noLoc
        cuStmt $ If active (Block [assignGather] noLoc) Nothing noLoc
      cuStmt $ Exp (Just $ cuCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst bytes]) noLoc
      pure (JanusCUDA (JanusC (pure (RVal out))))
