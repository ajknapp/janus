{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Argobots-backed fork-join CPU parallelism ('CmdParallel') with explicit
-- stack-allocated capture arguments ('LocalArgs'), zero-overhead serial fast
-- path below @minGrain@, nested-safe execution streams in C ('JanusCMT'),
-- concurrent 'IO' evaluation, and DejaFu-verifiable 'ParallelDejaFuM' testing.
module Janus.Command.Parallel
  ( -- * Worker Capability Bundle & Core Class
    CmdParWorker,
    CmdParallel (..),

    -- * Argobots Runtime Controls (Re-exported from @Janus.CBits.Argobots@)
    janusAbtInit,
    janusAbtFinalize,
    janusAbtSetNumXstreams,
    janusAbtNumXstreams,
    withArgobotsXstreams,

    -- * DejaFu-Verifiable Parallel CPU Emulator
    ParallelDejaFu (..),
    ParallelDejaFuRef (..),
    ParallelDejaFuM (..),
    runParallelDejaFuMWith,
    runParallelDejaFuM,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Classy as CC
import qualified Control.Concurrent.Classy.Async as CCA
import Control.Lens ((&), (+~), (^.))
import Control.Monad (when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), ask)
import Control.Monad.State (gets)
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import Data.Loc (noLoc)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import Foreign.C (newCString)
import qualified Foreign.Marshal.Alloc as FMA
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek, poke))
import Janus.Backend.C.CUDA.Atomic
  ( CmdAtomic (..),
    CmdAtomicAdd (..),
    CmdAtomicBits (..),
    CmdAtomicMinMax (..),
  )
import Janus.Backend.C.Quote
  ( captureBlock_,
    freshCVar,
    jcFuncs,
    jcStms,
    jcType,
  )
import Janus.Backend.C.Raw
  ( JCFunc (..),
    JanusCM,
    JanusCMT (..),
    JanusCT (..),
    JCType (..),
    RVal (..),
    getFunction,
    getJanusCType,
    jcfVarCounter,
    liftJanusCM,
    mangleJanusLocalName,
    modifyFunction,
    voidPtrType,
  )
import Janus.CBits.Argobots
  ( janusAbtFinalize,
    janusAbtInit,
    janusAbtNumXstreams,
    janusAbtSetNumXstreams,
    withArgobotsXstreams,
  )
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import qualified Janus.Command.Memory as M
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (Cell (..), CmdRef (..), RefCell (..))
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Constraints (CmdJanus, CmdJanusIO, CmdLocalFun (..), ExpJanus)
import Janus.Data.HKD (ExpPtrWord (..), ExpStructPtr (..))
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast
  ( ExpBoolCast (..),
    ExpFloatingCast (..),
    ExpIntegralCast (..),
    ExpPtrCast (..),
  )
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Extract (ExpExtract (..))
import Janus.Expression.Inject (ExpInject (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.MurmurHash (ExpMurmurHash (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SpecialMath (ExpSpecialMath (..))
import Janus.Fix (ExpFix (..), LocalArgs (..), localArgsTypeReps)
import Janus.SPMD.Types (ExpPair (..))
import Janus.Typed (IdConstraint, JanusTyped, KnownJanusType)
import Language.C.Quote
  ( Decl (..),
    DeclSpec (..),
    Definition (..),
    Exp (..),
    Field (..),
    FieldGroup (..),
    Id (..),
    InitGroup (..),
    Initializer (..),
    TypeSpec (..),
    Typedef (..),
    UnOp (..),
  )
import Test.DejaFu.Conc (ConcIO, Condition, MemType, Trace, runConcurrent)
import Test.DejaFu.Schedule (Scheduler, roundRobinSched)
import Test.DejaFu.Settings (defaultMemType)

--------------------------------------------------------------------------------
-- Worker Capability Bundle & Core `CmdParallel` Class
--------------------------------------------------------------------------------

-- | Capabilities available inside a 'parallelForRangeM' worker body: full
-- 'CmdJanus', nested 'CmdParallel', and thread-safe 'CmdAtomic' operations.
type CmdParWorker m e =
  ( CmdJanus m e,
    CmdParallel m e,
    M.CmdStackArray m e,
    ExpPair e,
    ExpSpecialMath e Float,
    ExpSpecialMath e Double,
    CmdAtomic m e,
    CmdAtomicAdd m e Int32,
    CmdAtomicAdd m e Word32,
    CmdAtomicAdd m e Int64,
    CmdAtomicAdd m e Word64,
    CmdAtomicAdd m e Float,
    CmdAtomicAdd m e Double,
    CmdAtomicMinMax m e Int32,
    CmdAtomicMinMax m e Word32,
    CmdAtomicMinMax m e Int64,
    CmdAtomicMinMax m e Word64,
    CmdAtomicMinMax m e Float,
    CmdAtomicMinMax m e Double,
    CmdAtomicBits m e Int32,
    CmdAtomicBits m e Word32,
    CmdAtomicBits m e Int64,
    CmdAtomicBits m e Word64
  )

class (CmdJanus m e) => CmdParallel (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) | m -> e, e -> m where
  -- | Fork-join parallel loop partitioning @[lo, hi)@ into contiguous chunks
  -- of size @>= minGrain@ across Argobots execution streams (or concurrent
  -- threads in 'IO' / 'ParallelDejaFuM').
  --
  -- Each worker receives @(workerId, chunkLo, chunkHi, capturedArgs)@ where
  -- @workerId@ is strictly unique in @[0, numChunks)@ across all active chunks.
  parallelForRangeM ::
    String ->
    e Int64 ->
    e Int64 ->
    e Int64 ->
    LocalArgs e ts ->
    ( forall m0 e0.
      (CmdParWorker m0 e0) =>
      e0 Int32 ->
      e0 Int64 ->
      e0 Int64 ->
      LocalArgs e0 ts ->
      m0 ()
    ) ->
    m ()

--------------------------------------------------------------------------------
-- Chunk Partitioning Helper
--------------------------------------------------------------------------------

partitionChunks :: Int -> Int64 -> Int64 -> Int64 -> [(Int32, Int64, Int64)]
partitionChunks nWorkers lo hi minGrain
  | lo >= hi = []
  | otherwise =
      let total = hi - lo
          grain = max 1 minGrain
          nEs = max 1 nWorkers
          maxChunks = (total + grain - 1) `quot` grain
          nChunks = fromIntegral (min (fromIntegral nEs) maxChunks) :: Int
       in if nChunks <= 1
            then [(0, lo, hi)]
            else
              let baseSz = total `quot` fromIntegral nChunks
                  remSz = total `rem` fromIntegral nChunks
                  go c cur
                    | c >= nChunks = []
                    | otherwise =
                        let sz = baseSz + if fromIntegral c < remSz then 1 else 0
                         in (fromIntegral c, cur, cur + sz) : go (c + 1) (cur + sz)
               in go 0 lo

--------------------------------------------------------------------------------
-- IO / Identity Instance
--------------------------------------------------------------------------------

instance CmdParallel IO Identity where
  parallelForRangeM _ (Identity lo) (Identity hi) (Identity minGrain) args body = do
    nEs <- janusAbtNumXstreams
    let chunks = partitionChunks nEs lo hi minGrain
    case chunks of
      [] -> pure ()
      [(wid, cLo, cHi)] -> body (Identity wid) (Identity cLo) (Identity cHi) args
      _ ->
        Async.forConcurrently_ chunks $ \(wid, cLo, cHi) ->
          body (Identity wid) (Identity cLo) (Identity cHi) args

--------------------------------------------------------------------------------
-- JanusCMT t / JanusCT t (Argobots C Codegen) Instance
--------------------------------------------------------------------------------

extractLocalArgsExps :: LocalArgs (JanusCT t) ts -> JanusCM [Exp]
extractLocalArgsExps LANil = pure []
extractLocalArgsExps (LACons (JanusCT m) rest) = do
  RVal e <- m
  es <- extractLocalArgsExps rest
  pure (e : es)

buildCtxStructFields :: Int -> LocalArgs (JanusCT t) ts -> JanusCM [FieldGroup]
buildCtxStructFields _ LANil = pure []
buildCtxStructFields idx (LACons (_ :: JanusCT t a) rest) = do
  JCType spec dec <- getJanusCType (Proxy @a)
  let fid = Id ("f_" <> show idx) noLoc
      fg = FieldGroup spec [Field (Just fid) (Just dec) Nothing noLoc] noLoc
  fgs <- buildCtxStructFields (idx + 1) rest
  pure (fg : fgs)

unpackCtxArgs :: forall t ts. Exp -> Int -> LocalArgs (JanusCT t) ts -> LocalArgs (JanusCT t) ts
unpackCtxArgs _ _ LANil = LANil
unpackCtxArgs pctxExp idx (LACons (_ :: JanusCT t a) rest) =
  let fld = PtrMember pctxExp (Id ("f_" <> show idx) noLoc) noLoc
   in LACons (JanusCT (pure (RVal fld))) (unpackCtxArgs pctxExp (idx + 1) rest)

instance CmdParallel (JanusCMT t) (JanusCT t) where
  parallelForRangeM label (JanusCT mlo) (JanusCT mhi) (JanusCT mgrain) actualArgs body = do
    RVal loExp <- liftJanusCM mlo
    RVal hiExp <- liftJanusCM mhi
    RVal grainExp <- liftJanusCM mgrain
    let inTys = localArgsTypeReps actualArgs
    callerFn <- getFunction
    let callSiteId = callerFn ^. jcfVarCounter
    modifyFunction (\f -> f & jcfVarCounter +~ 1)
    let mangledWorker = mangleJanusLocalName ("par_" <> label <> "_" <> show callSiteId) inTys []
        ctxStructName = "janus_par_ctx_" <> mangledWorker
        isEmptyArgs = case actualArgs of
          LANil -> True
          LACons {} -> False
    ctxTypedefDecls <-
      if isEmptyArgs
        then pure []
        else do
          fields <- liftJanusCM (buildCtxStructFields 0 actualArgs)
          let structSpec = DeclSpec [] [] (Tstruct Nothing (Just fields) [] noLoc) noLoc
              td = TypedefGroup structSpec [] [Typedef (Id ctxStructName noLoc) (DeclRoot noLoc) [] noLoc] noLoc
          pure [DecDef td noLoc]
    alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup mangledWorker)
    workerItems <-
      if alreadyDefined
        then pure []
        else captureBlock_ $ do
          let widE = JanusCT (pure (RVal (Var (Id "wid" noLoc) noLoc)))
              loE = JanusCT (pure (RVal (Var (Id "lo" noLoc) noLoc)))
              hiE = JanusCT (pure (RVal (Var (Id "hi" noLoc) noLoc)))
          formalArgs <-
            if isEmptyArgs
              then do
                [jcStms| (void)raw_ctx; |]
                pure actualArgs
              else do
                [jcStms| typename $id:ctxStructName *pctx = (typename $id:ctxStructName *)raw_ctx; |]
                pure (unpackCtxArgs (Var (Id "pctx" noLoc) noLoc) 0 actualArgs)
          body widE loE hiE formalArgs
    [jcFuncs|
      #include <stdint.h>
      #include <stddef.h>
      $edecls:ctxTypedefDecls
      extern void janus_abt_parallel_for(int64_t, int64_t, int64_t, void *, void (*)(int32_t, int64_t, int64_t, void *)) __attribute__((weak));
      void (*janus_abt_pfor_hook)(int64_t, int64_t, int64_t, void *, void (*)(int32_t, int64_t, int64_t, void *)) __attribute__((weak)) = NULL;

      void $id:mangledWorker(int32_t wid, int64_t lo, int64_t hi, void *raw_ctx) {
        $items:workerItems
      }
    |]
    ctxPtrExp <-
      if isEmptyArgs
        then pure (Var (Id "NULL" noLoc) noLoc)
        else do
          argExps <- liftJanusCM (extractLocalArgsExps actualArgs)
          let argInits = [ExpInitializer e noLoc | e <- argExps]
          (ctxId, ctxVar) <- freshCVar "par_ctx"
          [jcStms| typename $id:ctxStructName $id:ctxId = { $inits:argInits }; |]
          pure (Cast (jcType voidPtrType) (UnOp AddrOf ctxVar noLoc) noLoc)
    [jcStms|
      if (janus_abt_pfor_hook != NULL) {
        janus_abt_pfor_hook($exp:loExp, $exp:hiExp, $exp:grainExp, $exp:ctxPtrExp, $id:mangledWorker);
      } else {
        janus_abt_parallel_for($exp:loExp, $exp:hiExp, $exp:grainExp, $exp:ctxPtrExp, $id:mangledWorker);
      }
    |]

--------------------------------------------------------------------------------
-- DejaFu-Verifiable Parallel CPU Emulator (`ParallelDejaFuM`)
--------------------------------------------------------------------------------

data ParallelDejaFuEnv = ParallelDejaFuEnv
  { _pdfNumWorkers :: !Int,
    _pdfAtomicLock :: !(CC.MVar ConcIO ())
  }

newtype ParallelDejaFu a = ParallelDejaFu {getParallelDejaFu :: a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving (Applicative, Monad, ExpBool) via Identity

newtype ParallelDejaFuRef a = ParallelDejaFuRef {getParallelDejaFuRef :: IORef a}

newtype ParallelDejaFuM a = ParallelDejaFuM {getParallelDejaFuM :: ReaderT ParallelDejaFuEnv ConcIO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ParallelDejaFuEnv,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask
    )

type instance JanusTyped ParallelDejaFu = IdConstraint

instance KnownJanusType ParallelDejaFu a

deriving via Identity instance ExpCond ParallelDejaFu
deriving via Identity instance ExpLet ParallelDejaFu
deriving via Identity instance ExpPtrCast ParallelDejaFu
deriving via Identity a instance (Num a) => Num (ParallelDejaFu a)
deriving via Identity a instance (Bounded a) => Bounded (ParallelDejaFu a)
deriving via Identity a instance (Fractional a) => Fractional (ParallelDejaFu a)
deriving via Identity a instance (Floating a) => Floating (ParallelDejaFu a)
deriving via Identity a instance (FMA a) => FMA (ParallelDejaFu a)
deriving via Identity a instance (Hypot a) => Hypot (ParallelDejaFu a)

instance ExpPtrWord ParallelDejaFu where
  ptrToWord64Expr :: forall a. ParallelDejaFu (Ptr a) -> ParallelDejaFu Word64
  ptrToWord64Expr = coerce (ptrToWord64Expr @Identity @a)
  word64ToPtrExpr :: forall a. (JanusTyped ParallelDejaFu a) => ParallelDejaFu Word64 -> ParallelDejaFu (Ptr a)
  word64ToPtrExpr = coerce (word64ToPtrExpr @Identity @a)

instance ExpStructPtr ParallelDejaFu where
  structFieldPtr :: forall structTy field. (JanusTyped ParallelDejaFu field) => ParallelDejaFu (Ptr structTy) -> Int -> String -> ParallelDejaFu (Ptr field)
  structFieldPtr = coerce (structFieldPtr @Identity @structTy @field)

instance (Bits a, Integral a) => ExpBits ParallelDejaFu a where
  iand = coerce (iand @Identity @a)
  ior = coerce (ior @Identity @a)
  ieor = coerce (ieor @Identity @a)
  inot = coerce (inot @Identity @a)
  lshift = coerce (lshift @Identity @a)
  rshift = coerce (rshift @Identity @a)

instance (Num a, Eq a) => ExpBoolCast ParallelDejaFu a where
  fromBool = coerce (fromBool @Identity @a)
  toBool = coerce (toBool @Identity @a)

instance (Real a, Fractional b) => ExpFloatingCast ParallelDejaFu a b where
  toFloating = coerce (toFloating @Identity @a @b)

instance (Integral a, Num b) => ExpIntegralCast ParallelDejaFu a b where
  toIntegral = coerce (toIntegral @Identity @a @b)

instance (Eq a) => ExpEq ParallelDejaFu a where
  eq = coerce (eq @Identity @a)
  ne = coerce (ne @Identity @a)

instance ExpExtract ParallelDejaFu a where
  extract = coerce (extract @Identity @a)

instance ExpInject ParallelDejaFu a where
  inject = coerce (inject @Identity @a)

instance (Integral a) => ExpIntegral ParallelDejaFu a where
  quotient = coerce (quotient @Identity @a)
  remainder = coerce (remainder @Identity @a)

instance ExpComplex ParallelDejaFu a where
  rePart = coerce (rePart @Identity @a)
  imPart = coerce (imPart @Identity @a)

instance (ExpMurmurHash Identity (Identity a)) => ExpMurmurHash ParallelDejaFu (ParallelDejaFu a) where
  murmurHashWithSalt = coerce (murmurHashWithSalt @Identity @(Identity a))

instance (Ord a) => ExpOrd ParallelDejaFu a where
  lt = coerce (lt @Identity @a)
  le = coerce (le @Identity @a)
  gt = coerce (gt @Identity @a)
  ge = coerce (ge @Identity @a)

instance (Storable a) => M.ExpSized ParallelDejaFu a where
  sizeOf = coerce (M.sizeOf @Identity @a)
  alignOf = coerce (M.alignOf @Identity @a)

instance (Storable a) => M.ExpPtr ParallelDejaFu a where
  nullPtr = coerce (M.nullPtr @Identity @a)
  ptrAdd = coerce (M.ptrAdd @Identity @a)
  ptrIndex = coerce (M.ptrIndex @Identity @a)

instance M.CmdMem ParallelDejaFuM ParallelDejaFu where
  malloc (ParallelDejaFu n) = ParallelDejaFu . runIdentity <$> liftIO (M.malloc @IO @Identity (Identity n))
  calloc (ParallelDejaFu n) (ParallelDejaFu s) = ParallelDejaFu . runIdentity <$> liftIO (M.calloc @IO @Identity (Identity n) (Identity s))
  realloc (ParallelDejaFu p) (ParallelDejaFu n) = ParallelDejaFu . runIdentity <$> liftIO (M.realloc @IO @Identity (Identity p) (Identity n))
  free (ParallelDejaFu p) = liftIO (M.free @IO @Identity (Identity p))

instance M.CmdDynamicAligned ParallelDejaFuM ParallelDejaFu where
  alignedMalloc (ParallelDejaFu sz) (ParallelDejaFu al) =
    ParallelDejaFu . runIdentity <$> liftIO (M.alignedMalloc @IO @Identity (Identity sz) (Identity al))

instance M.CmdMemCopy ParallelDejaFuM ParallelDejaFu where
  cmdMemcpy (ParallelDejaFu d) (ParallelDejaFu s) (ParallelDejaFu n) =
    liftIO (M.cmdMemcpy @IO @Identity (Identity d) (Identity s) (Identity n))
  cmdMemmove (ParallelDejaFu d) (ParallelDejaFu s) (ParallelDejaFu n) =
    liftIO (M.cmdMemmove @IO @Identity (Identity d) (Identity s) (Identity n))
  cmdMemset (ParallelDejaFu d) (ParallelDejaFu v) (ParallelDejaFu n) =
    liftIO (M.cmdMemset @IO @Identity (Identity d) (Identity v) (Identity n))

instance (Storable a) => M.CmdStorable ParallelDejaFuM ParallelDejaFu a where
  peek (ParallelDejaFu p) = ParallelDejaFu . runIdentity <$> liftIO (M.peek @IO @Identity (Identity p))
  poke (ParallelDejaFu p) (ParallelDejaFu v) = liftIO (M.poke @IO @Identity (Identity p) (Identity v))
  peekElemOff (ParallelDejaFu p) (ParallelDejaFu i) =
    ParallelDejaFu . runIdentity <$> liftIO (M.peekElemOff @IO @Identity (Identity p) (Identity i))
  pokeElemOff (ParallelDejaFu p) (ParallelDejaFu v) (ParallelDejaFu i) =
    liftIO (M.pokeElemOff @IO @Identity (Identity p) (Identity v) (Identity i))

instance (Storable a) => M.CmdRefPtr ParallelDejaFuM ParallelDejaFu a where
  withRefPtr (ParallelDejaFuRef r) f = do
    v <- liftIO (readIORef r)
    Catch.bracket (liftIO FMA.malloc) (liftIO . FMA.free) $ \p -> do
      liftIO (poke p v)
      res <- f (ParallelDejaFu p)
      liftIO (peek p >>= writeIORef r)
      pure res

instance CmdCase ParallelDejaFuM ParallelDejaFu where
  switchCase_ _ [] def = def
  switchCase_ i' ((i, c) : cs) def =
    if fromIntegral (getParallelDejaFu i') == i then c else switchCase_ i' cs def

instance CmdCond ParallelDejaFuM ParallelDejaFu where
  ifThenElseM_ (ParallelDejaFu b) t f = if b then t else f

instance CmdIO ParallelDejaFuM ParallelDejaFu where
  stdout = coerce (stdout @IO @Identity)
  stderr = coerce (stderr @IO @Identity)
  fopen (ParallelDejaFu h) (ParallelDejaFu m) = ParallelDejaFu . runIdentity <$> liftIO (fopen @IO @Identity (Identity h) (Identity m))
  fclose (ParallelDejaFu h) = ParallelDejaFu . runIdentity <$> liftIO (fclose @IO @Identity (Identity h))

instance CmdString ParallelDejaFuM ParallelDejaFu where
  withString s f = Catch.bracket (liftIO (newCString s)) (liftIO . FMA.free) (f . ParallelDejaFu)

instance CmdPutString ParallelDejaFuM ParallelDejaFu where
  hputString (ParallelDejaFu h) (ParallelDejaFu s) = liftIO (hputString @IO @Identity (Identity h) (Identity s))

instance (Show a) => CmdFormat ParallelDejaFuM ParallelDejaFu a where
  hformat (ParallelDejaFu h) (ParallelDejaFu a) t = liftIO (hformat @IO @Identity (Identity h) (Identity a) t)

instance CmdRef ParallelDejaFuM ParallelDejaFu where
  type Ref ParallelDejaFuM ParallelDejaFu a = ParallelDejaFuRef a
  letM = pure
  newRef (ParallelDejaFu a) = ParallelDejaFuRef <$> liftIO (newIORef a)
  newRef' = ParallelDejaFuRef <$> liftIO (newIORef undefined)
  readRef (ParallelDejaFuRef r) = ParallelDejaFu <$> liftIO (readIORef r)
  writeRef (ParallelDejaFuRef r) (ParallelDejaFu a) = liftIO (writeIORef r a)

instance RefCell ParallelDejaFuM ParallelDejaFu (ParallelDejaFu a) where
  newCell x = do
    r <- newRef x
    pure (Cell (readRef r) (writeRef r))
  newCell' = do
    r <- newRef'
    pure (Cell (readRef r) (writeRef r))

instance CmdRange ParallelDejaFuM ParallelDejaFu

instance CmdWhile ParallelDejaFuM ParallelDejaFu where
  whileM (ParallelDejaFu c) body = do
    r <- liftIO (newIORef c)
    let go = do
          b <- liftIO (readIORef r)
          when b (body (ParallelDejaFuRef r) >> go)
    go

instance CmdTicToc ParallelDejaFuM ParallelDejaFu where
  tic = Tic . ParallelDejaFu . runIdentity . getTic <$> liftIO (tic @IO @Identity)
  toc = Toc . ParallelDejaFu . runIdentity . getToc <$> liftIO (toc @IO @Identity)

instance ExpFix ParallelDejaFu r where
  fun _ _ = fix

instance CmdLocalFun ParallelDejaFuM ParallelDejaFu where
  emitLocalFunArgsM _ = fix
  emitLocalFunArgsM_ _ = fix

instance ExpJanus ParallelDejaFu
instance CmdJanus ParallelDejaFuM ParallelDejaFu
instance CmdJanusIO ParallelDejaFuM ParallelDejaFu

pdfAtomicCAS :: (Eq a, Storable a) => ParallelDejaFu (Ptr a) -> ParallelDejaFu a -> ParallelDejaFu a -> ParallelDejaFuM (ParallelDejaFu a)
pdfAtomicCAS (ParallelDejaFu p) (ParallelDejaFu cmp) (ParallelDejaFu val) = do
  env <- ask
  old <- ParallelDejaFuM $ ReaderT $ \_ ->
    CC.withMVar (_pdfAtomicLock env) $ \_ -> liftIO $ do
      cur <- peek p
      when (cur == cmp) (poke p val)
      pure cur
  pure (ParallelDejaFu old)

pdfAtomicRMW :: (Storable a) => (a -> a -> a) -> ParallelDejaFu (Ptr a) -> ParallelDejaFu a -> ParallelDejaFuM (ParallelDejaFu a)
pdfAtomicRMW f (ParallelDejaFu p) (ParallelDejaFu val) = do
  env <- ask
  old <- ParallelDejaFuM $ ReaderT $ \_ ->
    CC.withMVar (_pdfAtomicLock env) $ \_ -> liftIO $ do
      cur <- peek p
      poke p (f cur val)
      pure cur
  pure (ParallelDejaFu old)

instance CmdAtomic ParallelDejaFuM ParallelDejaFu where
  atomicCAS32Ptr = pdfAtomicCAS
  atomicCAS64Ptr = pdfAtomicCAS

instance (Storable a, Num a) => CmdAtomicAdd ParallelDejaFuM ParallelDejaFu a where
  atomicAddPtr = pdfAtomicRMW (+)

instance (Storable a, Ord a) => CmdAtomicMinMax ParallelDejaFuM ParallelDejaFu a where
  atomicMinPtr = pdfAtomicRMW min
  atomicMaxPtr = pdfAtomicRMW max

instance (Storable a, Bits a) => CmdAtomicBits ParallelDejaFuM ParallelDejaFu a where
  atomicAndPtr = pdfAtomicRMW (.&.)
  atomicOrPtr = pdfAtomicRMW (.|.)
  atomicXorPtr = pdfAtomicRMW xor

instance ExpPair ParallelDejaFu where
  mkPair (ParallelDejaFu a) (ParallelDejaFu b) = ParallelDejaFu (runIdentity (mkPair (Identity a) (Identity b)))
  fstPair (ParallelDejaFu p) = ParallelDejaFu (runIdentity (fstPair (Identity p)))
  sndPair (ParallelDejaFu p) = ParallelDejaFu (runIdentity (sndPair (Identity p)))
  mkVoidPtr (ParallelDejaFu p) = ParallelDejaFu (runIdentity (mkVoidPtr (Identity p)))
  unVoidPtr (ParallelDejaFu p) = ParallelDejaFu (runIdentity (unVoidPtr (Identity p)))
  mkSPMDUnit = ParallelDejaFu (runIdentity mkSPMDUnit)
  coerceExp (ParallelDejaFu a) = ParallelDejaFu (coerce a)

instance ExpSpecialMath ParallelDejaFu Float where
  lgammaE = coerce (lgammaE :: Identity Float -> Identity Float)
  digammaE = coerce (digammaE :: Identity Float -> Identity Float)
  trigammaE = coerce (trigammaE :: Identity Float -> Identity Float)
  erfE = coerce (erfE :: Identity Float -> Identity Float)
  erfcE = coerce (erfcE :: Identity Float -> Identity Float)
  erfinvE = coerce (erfinvE :: Identity Float -> Identity Float)
  log1pE = coerce (log1pE :: Identity Float -> Identity Float)
  expm1E = coerce (expm1E :: Identity Float -> Identity Float)
  rsqrtE = coerce (rsqrtE :: Identity Float -> Identity Float)

instance ExpSpecialMath ParallelDejaFu Double where
  lgammaE = coerce (lgammaE :: Identity Double -> Identity Double)
  digammaE = coerce (digammaE :: Identity Double -> Identity Double)
  trigammaE = coerce (trigammaE :: Identity Double -> Identity Double)
  erfE = coerce (erfE :: Identity Double -> Identity Double)
  erfcE = coerce (erfcE :: Identity Double -> Identity Double)
  erfinvE = coerce (erfinvE :: Identity Double -> Identity Double)
  log1pE = coerce (log1pE :: Identity Double -> Identity Double)
  expm1E = coerce (expm1E :: Identity Double -> Identity Double)
  rsqrtE = coerce (rsqrtE :: Identity Double -> Identity Double)

instance M.CmdStackArray ParallelDejaFuM ParallelDejaFu where
  withStackBytes nBytes k = do
    p <- M.malloc (fromIntegral (max 8 nBytes))
    r <- k (fromVoidPtr p)
    M.free p
    pure r

instance CmdParallel ParallelDejaFuM ParallelDejaFu where
  parallelForRangeM _ (ParallelDejaFu lo) (ParallelDejaFu hi) (ParallelDejaFu minGrain) args body = do
    env <- ask
    let chunks = partitionChunks (_pdfNumWorkers env) lo hi minGrain
    case chunks of
      [] -> pure ()
      [(wid, cLo, cHi)] -> body (ParallelDejaFu wid) (ParallelDejaFu cLo) (ParallelDejaFu cHi) args
      _ ->
        ParallelDejaFuM $ ReaderT $ \_ ->
          CCA.forConcurrently_ chunks $ \(wid, cLo, cHi) ->
            runReaderT
              (getParallelDejaFuM (body (ParallelDejaFu wid) (ParallelDejaFu cLo) (ParallelDejaFu cHi) args))
              env

runParallelDejaFuMWith ::
  Scheduler s ->
  MemType ->
  s ->
  Int ->
  ParallelDejaFuM a ->
  IO (Either Condition a, s, Trace)
runParallelDejaFuMWith sched mem s nWorkers (ParallelDejaFuM act) =
  runConcurrent sched mem s $ do
    lk <- CC.newMVar ()
    runReaderT act (ParallelDejaFuEnv (max 1 nWorkers) lk)

runParallelDejaFuM :: Int -> ParallelDejaFuM a -> IO (Either Condition a, Trace)
runParallelDejaFuM nWorkers act = do
  (res, (), tr) <- runParallelDejaFuMWith roundRobinSched defaultMemType () nWorkers act
  pure (res, tr)
