{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.Kernel where

import Control.Exception (bracket)
import Control.Lens ((&), (.~), (^.), ix)
import Control.Monad.Reader (ReaderT (..), lift)
import Control.Monad.State (StateT (..), modify)
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Functor.Identity (Identity (..))
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Foreign.C (withCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (alignment, peek, poke, pokeElemOff, sizeOf))
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.CUDA.Foreign
import Janus.Backend.C.CUDA.Types
import Janus.Backend.C.Target
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
import Language.C.Quote as C

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
  switchCase_ tag cases def = JanusCUDAM $ lift $ switchCase_ (getJanusCUDA tag) (fmap (\(i, m) -> (i, runJanusCUDAM m)) cases) (runJanusCUDAM def)

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

class JanusCUDAParam r where
  jcudaparamWithFeatures :: CUDAFeatures -> String -> Int -> JanusCUDAM [Param] -> r -> [JCFunc]

jcudaparam :: (JanusCUDAParam r) => String -> Int -> JanusCUDAM [Param] -> JanusCBackend -> r -> [JCFunc]
jcudaparam name n params _ = jcudaparamWithFeatures cudaSM80 name n params

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDA a) where
  jcudaparamWithFeatures fs name _ params (JanusCUDA a) =
    runJCCodegen (mkCUDAFuncInfo fs name) $ do
      ps <- runJanusCUDAM params
      finishFunction a
      modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps) False noLoc

instance (JanusCTyped a) => JanusCUDAParam (JanusCUDAM (JanusCUDA a)) where
  jcudaparamWithFeatures fs name _ params ma =
    runJCCodegen (mkCUDAFuncInfo fs name) $ do
      ps <- runJanusCUDAM params
      JanusCUDA a <- runJanusCUDAM ma
      finishFunction a
      modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps) False noLoc

instance JanusCUDAParam (JanusCUDAM ()) where
  jcudaparamWithFeatures fs name _ params ma =
    runJCCodegen (mkCUDAFuncInfo fs name) $ do
      ps <- runJanusCUDAM params
      runJanusCUDAM ma
      finishFunction_
      modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps) False noLoc

instance (JanusCTyped a, JanusCUDAParam r) => JanusCUDAParam (JanusCUDA a -> r) where
  jcudaparamWithFeatures fs name n params f =
    let aid = mkArgId n
        params' = do
          ps <- params
          JCType spec dec <- JanusCUDAM (lift (getJanusCType (Proxy @a)))
          pure (Param (Just aid) spec dec noLoc : ps)
        argExp = JanusCUDA (JanusC (pure (RVal (Var aid noLoc))))
     in jcudaparamWithFeatures fs name (n + 1) params' (f argExp)

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
