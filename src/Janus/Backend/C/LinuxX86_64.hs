{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.LinuxX86_64 where

import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits (Bits (..))
import Data.Coerce
import Data.Complex
import Data.Int
import Data.Loc
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Typeable (Typeable)
import Data.Word
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.LibFFI (Arg, argPtr, callFFI, retVoid)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr)
import qualified Foreign.Storable as Storable
import GHC.Float (expm1, log1p)
import GHC.TypeLits (natVal)
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.Target
import Janus.Command.Array
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
import Janus.Expression.SIMD.Types (rawMaskFromBools, rawMaskToBools, rawSIMDFromList, rawSIMDToList)
import Janus.FFI.Arg (FFIArg (..))
import Janus.FFI.Ret (FFIRet (..))
import Janus.Typed
import Language.C.Quote as C
import System.IO.Unsafe (unsafePerformIO)

newtype LinuxX86_64C a = LinuxX86_64C {getLinuxX86_64C :: JanusC a}

newtype LinuxX86_64CM a = LinuxX86_64CM {getLinuxX86_64CM :: JanusCM a}
  deriving (Functor, Applicative, Monad, MonadFix) via JanusCM

type instance JanusTyped LinuxX86_64C = JanusCTyped

instance JanusCBackendTypes LinuxX86_64C LinuxX86_64CM where
  toJanusC = getLinuxX86_64C
  fromJanusC = LinuxX86_64C
  toJanusCM = getLinuxX86_64CM
  fromJanusCM = LinuxX86_64CM

deriving via JanusC a instance (JanusLitC a, Num a) => Num (LinuxX86_64C a)

deriving via JanusC a instance (JanusLitC a, Fractional a) => Fractional (LinuxX86_64C a)

deriving via JanusC Float instance Floating (LinuxX86_64C Float)

deriving via JanusC Double instance Floating (LinuxX86_64C Double)

deriving via JanusC (Complex Float) instance Floating (LinuxX86_64C (Complex Float))

deriving via JanusC (Complex Double) instance Floating (LinuxX86_64C (Complex Double))

deriving via JanusC instance ExpBool LinuxX86_64C

instance (ExpBits JanusC a) => ExpBits LinuxX86_64C a where
  iand = coerce (iand @JanusC @a)
  ior = coerce (ior @JanusC @a)
  ieor = coerce (ieor @JanusC @a)
  inot = coerce (inot @JanusC @a)
  lshift = coerce (lshift @JanusC @a)
  rshift = coerce (rshift @JanusC @a)

instance (ExpBoolCast JanusC a) => ExpBoolCast LinuxX86_64C a where
  toBool = coerce (toBool @JanusC @a)
  fromBool = coerce (fromBool @JanusC @a)

instance (ExpIntegralCast JanusC a b) => ExpIntegralCast LinuxX86_64C a b where
  toIntegral = coerce (toIntegral @JanusC @a @b)

instance (ExpFloatingCast JanusC a b) => ExpFloatingCast LinuxX86_64C a b where
  toFloating = coerce (toFloating @JanusC @a @b)

deriving via JanusC instance ExpPtrCast LinuxX86_64C

deriving via JanusC instance ExpCond LinuxX86_64C

instance (ExpEq JanusC a) => ExpEq LinuxX86_64C a where
  eq = coerce (eq @JanusC @a)
  ne = coerce (ne @JanusC @a)

instance (ExpExtract JanusC a) => ExpExtract LinuxX86_64C a where
  extract = coerce (extract @JanusC @a)

instance (ExpInject JanusC a) => ExpInject LinuxX86_64C a where
  inject = coerce (inject @JanusC @a)

instance (Num a, JanusLitC a, ExpIntegral JanusC a) => ExpIntegral LinuxX86_64C a where
  remainder = coerce (remainder @JanusC @a)
  quotient = coerce (quotient @JanusC @a)

deriving via JanusC instance ExpLet LinuxX86_64C

deriving via JanusC a instance (FMA (JanusC a), JanusLitC a, Num a) => FMA (LinuxX86_64C a)

deriving via JanusC a instance (Hypot (JanusC a), JanusLitC a, Floating (LinuxX86_64C a)) => Hypot (LinuxX86_64C a)

instance (ExpMurmurHash JanusC a) => ExpMurmurHash LinuxX86_64C a where
  murmurHashWithSalt = coerce (murmurHashWithSalt @JanusC @a)

instance (ExpOrd JanusC a) => ExpOrd LinuxX86_64C a where
  lt = coerce (lt @JanusC @a)
  le = coerce (le @JanusC @a)
  gt = coerce (gt @JanusC @a)
  ge = coerce (ge @JanusC @a)

instance (JanusCTyped a, ExpSized JanusC a) => ExpSized LinuxX86_64C a where
  sizeOf = coerce (sizeOf @JanusC @a)
  alignOf = coerce (alignOf @JanusC @a)

instance (ExpPtr JanusC a) => ExpPtr LinuxX86_64C a where
  nullPtr = coerce (nullPtr @JanusC @a)
  ptrAdd = coerce (ptrAdd @JanusC @a)
  ptrIndex = coerce (ptrIndex @JanusC @a)

instance CmdMem LinuxX86_64CM LinuxX86_64C where
  malloc = coerce (malloc @JanusCM @JanusC)
  calloc = coerce (calloc @JanusCM @JanusC)
  realloc = coerce (realloc @JanusCM @JanusC)
  free = coerce (free @JanusCM @JanusC)

instance (ExpSized LinuxX86_64C a, JanusCTyped a) => CmdStorable LinuxX86_64CM LinuxX86_64C a where
  peek = coerce (peek @JanusCM @JanusC @a)
  poke = coerce (poke @JanusCM @JanusC @a)
  peekElemOff = coerce (peekElemOff @JanusCM @JanusC @a)
  pokeElemOff = coerce (pokeElemOff @JanusCM @JanusC @a)

instance (JanusCTyped a) => CmdRefPtr LinuxX86_64CM LinuxX86_64C a where
  withRefPtr (LinuxX86_64CRef r) f =
    LinuxX86_64CM $ withRefPtr @JanusCM @JanusC r (getLinuxX86_64CM . f . LinuxX86_64C)

instance CmdCase LinuxX86_64CM LinuxX86_64C where
  switchCase_ = coerce (switchCase_ @JanusCM @JanusC)

instance CmdCond LinuxX86_64CM LinuxX86_64C where
  ifThenElseM_ = coerce (ifThenElseM_ @JanusCM @JanusC)

instance CmdIO LinuxX86_64CM LinuxX86_64C where
  stdout = coerce (stdout @JanusCM @JanusC)
  stderr = coerce (stderr @JanusCM @JanusC)
  fopen = coerce (fopen @JanusCM @JanusC)
  fclose = coerce (fclose @JanusCM @JanusC)

instance CmdString LinuxX86_64CM LinuxX86_64C where
  withString s f = LinuxX86_64CM (withString @JanusCM @JanusC s $ getLinuxX86_64CM . f . LinuxX86_64C)

instance CmdPutString LinuxX86_64CM LinuxX86_64C where
  hputString = coerce (hputString @JanusCM @JanusC)

instance (CmdFormat JanusCM JanusC a) => CmdFormat LinuxX86_64CM LinuxX86_64C a where
  hformat = coerce (hformat @JanusCM @JanusC @a)

newtype LinuxX86_64CRef a = LinuxX86_64CRef {getLinuxX86_64CRef :: JanusCRef a}

instance CmdRef LinuxX86_64CM LinuxX86_64C where
  type Ref LinuxX86_64CM LinuxX86_64C a = LinuxX86_64CRef a
  letM (LinuxX86_64C a) = LinuxX86_64CM $ LinuxX86_64C <$> letM a
  newRef (LinuxX86_64C a) = LinuxX86_64CM (LinuxX86_64CRef <$> newRef a)
  newRef' = LinuxX86_64CM (LinuxX86_64CRef <$> newRef')
  readRef (LinuxX86_64CRef r) = LinuxX86_64CM (LinuxX86_64C <$> readRef r)
  writeRef (LinuxX86_64CRef r) (LinuxX86_64C a) = LinuxX86_64CM (writeRef r a)

instance CmdRange LinuxX86_64CM LinuxX86_64C where
  rangeM = coerce (rangeM @JanusCM @JanusC)

instance CmdWhile LinuxX86_64CM LinuxX86_64C where
  whileM = coerce (whileM @JanusCM @JanusC)

--------------------------------------------------------------------------------

ticTocAsmBlock ::
  (LinuxX86_64C a -> b) -> String -> LinuxX86_64CM b
ticTocAsmBlock newty str = LinuxX86_64CM $ do
  fname <- askFuncName
  JCType spec dec <- getJanusCType (Proxy @Word64)
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> do
      let c = f ^. jcfVarCounter
          vid = Id ("x_" <> show c) noLoc
          ini = Init vid dec Nothing Nothing [] noLoc
          asm = Asm True [] (StringLit [show str] "" noLoc) [AsmOut Nothing "\"=a\"" vid] [] ["\"rdx\""] noLoc

          block = appendBlock (f ^. jcfBlock) [BlockDecl (InitGroup spec [] [ini] noLoc), BlockStm asm]
      put $
        jcs
          & at fname . _Just . jcfVarCounter +~ 1
          & at fname . _Just . jcfBlock .~ block
      pure $ newty $ LinuxX86_64C $ JanusC $ pure $ RVal (Var vid noLoc)
    Nothing -> error "tic: the impossible happened"

instance CmdTicToc LinuxX86_64CM LinuxX86_64C where
  tic = ticTocAsmBlock Tic "rdtsc\n\tlfence\n\tshl $32, %%rdx\n\tor %%rdx, %0"
  toc = ticTocAsmBlock Toc "rdtscp\n\tshl $32, %%rdx\n\tor %%rdx, %0"

--------------------------------------------------------------------------------
-- LinuxX86_64C Compilation, Inspection, & ABI-Safe Evaluation
--------------------------------------------------------------------------------

type family LinuxX86_64CEval a where
  LinuxX86_64CEval (LinuxX86_64C a -> r) = a -> LinuxX86_64CEval r
  LinuxX86_64CEval (LinuxX86_64C a) = a
  LinuxX86_64CEval (LinuxX86_64CM (LinuxX86_64C a)) = IO a
  LinuxX86_64CEval (LinuxX86_64CM ()) = IO ()

class (JanusCTyped a) => LinuxX86_64ABIParam a where
  x86EmitParam :: Int -> JanusCM (Param, Exp)
  x86WithArg :: a -> (Arg -> IO b) -> IO b

class LinuxX86_64ABIRet a where
  x86FinishReturn :: JanusC a -> JanusCM [Param]
  x86CallReturn :: Proxy a -> FunPtr () -> [Arg] -> IO a

instance {-# OVERLAPPABLE #-} (JanusCTyped a, FFIArg a) => LinuxX86_64ABIParam a where
  x86EmitParam n = do
    JCType spec dec <- getJanusCType (Proxy @a)
    let aid = mkArgId n
    pure (Param (Just aid) spec dec noLoc, Var aid noLoc)
  x86WithArg a k = k (arg a)

instance {-# OVERLAPPABLE #-} (JanusCTyped a, FFIRet a) => LinuxX86_64ABIRet a where
  x86FinishReturn a = finishFunction a >> pure []
  x86CallReturn p fp args = callFFI fp (ret p) (reverse args)

emitMemcpyLoad :: forall a. (JanusCTyped a) => Int -> Exp -> JanusCM Exp
emitMemcpyLoad bytes ptrExp = do
  addHeader "string.h"
  JCType spec dec <- getJanusCType (Proxy @a)
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
      cp = FnCall (Var (Id "memcpy" noLoc) noLoc) [UnOp AddrOf (Var vid noLoc) noLoc, ptrExp, signedIntConst bytes] noLoc
      stm = BlockStm $ Exp (Just cp) noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl, stm])
  pure (Var vid noLoc)

emitMemcpyRet :: forall a. (JanusCTyped a) => Int -> JanusC a -> JanusCM [Param]
emitMemcpyRet bytes (JanusC mval) = do
  addHeader "string.h"
  RVal e <- mval
  ty <- getJanusCType (Proxy @a)
  v <- emitLocalVal ty e
  let outId = Id "janus_ret_out" noLoc
      JCType vspec vdec = voidPtrType
      outParam = Param (Just outId) vspec vdec noLoc
      cp = FnCall (Var (Id "memcpy" noLoc) noLoc) [Var outId noLoc, UnOp AddrOf v noLoc, signedIntConst bytes] noLoc
  modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm $ Exp (Just cp) noLoc]
  finishFunction_
  pure [outParam]

class LinuxX86_64Param r where
  x86param :: X86Features -> String -> Int -> JanusCM [Param] -> r -> [JCFunc]

class LinuxX86_64Evaluate r where
  x86eval :: ForeignPtr () -> (([Arg] -> IO b) -> [Arg] -> IO b) -> [Arg] -> r -> LinuxX86_64CEval r

{-# NOINLINE evalPureLinuxX86_64FFI #-}
evalPureLinuxX86_64FFI :: ForeignPtr () -> (FunPtr () -> IO a) -> a
evalPureLinuxX86_64FFI dlFPtr k = unsafePerformIO $
  withForeignPtr dlFPtr $ \p ->
    k (castPtrToFunPtr p)

runX86Codegen :: X86Features -> String -> JanusCM () -> [JCFunc]
runX86Codegen fs name action =
  let info = (mkJCFuncInfo name JC False) {_jcfiX86Features = fs}
      initSt = Map.singleton name (defaultJCFunc name (Params [] False noLoc))
   in Map.elems $ execState (runReaderT (getJanusCM action) info) initSt

instance (LinuxX86_64ABIRet a) => LinuxX86_64Param (LinuxX86_64C a) where
  x86param fs name _ mparams (LinuxX86_64C a) = runX86Codegen fs name $ do
    ps <- mparams
    extra <- x86FinishReturn a
    modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps <> extra) False noLoc

instance (LinuxX86_64ABIRet a) => LinuxX86_64Evaluate (LinuxX86_64C a) where
  x86eval dlFPtr _ args _ = evalPureLinuxX86_64FFI dlFPtr $ \fp -> x86CallReturn (Proxy @a) fp args

instance (LinuxX86_64ABIRet a) => LinuxX86_64Param (LinuxX86_64CM (LinuxX86_64C a)) where
  x86param fs name _ mparams (LinuxX86_64CM ma) = runX86Codegen fs name $ do
    ps <- mparams
    LinuxX86_64C a <- ma
    extra <- x86FinishReturn a
    modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps <> extra) False noLoc

instance (LinuxX86_64ABIRet a) => LinuxX86_64Evaluate (LinuxX86_64CM (LinuxX86_64C a)) where
  x86eval dlFPtr _ args _ =
    withForeignPtr dlFPtr $ \p -> x86CallReturn (Proxy @a) (castPtrToFunPtr p) args

instance LinuxX86_64Param (LinuxX86_64CM ()) where
  x86param fs name _ mparams (LinuxX86_64CM ma) = runX86Codegen fs name $ do
    ps <- mparams
    modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps) False noLoc
    ma >> finishFunction_

instance LinuxX86_64Evaluate (LinuxX86_64CM ()) where
  x86eval dlFPtr _ args _ =
    withForeignPtr dlFPtr $ \p -> callFFI (castPtrToFunPtr p) retVoid (reverse args)

instance (LinuxX86_64ABIParam a, LinuxX86_64Param r) => LinuxX86_64Param (LinuxX86_64C a -> r) where
  x86param fs name n mparams f =
    let stateVarId = Id ("arg_val_" <> show n) noLoc
        mparams' = do
          ps <- mparams
          (p, loadExp) <- x86EmitParam @a n
          ty <- getJanusCType (Proxy @a)
          let JCType spec dec = ty
              ini = Init stateVarId dec Nothing (Just (ExpInitializer loadExp noLoc)) [] noLoc
          modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockDecl $ InitGroup spec [] [ini] noLoc]
          pure (p : ps)
     in x86param fs name (n + 1) mparams' (f (LinuxX86_64C $ JanusC $ pure $ RVal $ Var stateVarId noLoc))

instance (LinuxX86_64ABIParam a, LinuxX86_64Evaluate r, X86DeferCall r) => LinuxX86_64Evaluate (LinuxX86_64C a -> r) where
  x86eval dlFPtr _ args f val =
    let dummy = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
     in x86DeferArg val dlFPtr args (f dummy)

class X86DeferCall r where
  x86DeferArg :: (LinuxX86_64ABIParam a) => a -> ForeignPtr () -> [Arg] -> r -> LinuxX86_64CEval r

instance (LinuxX86_64ABIRet b) => X86DeferCall (LinuxX86_64C b) where
  x86DeferArg x dlFPtr args _ =
    evalPureLinuxX86_64FFI dlFPtr $ \fp -> x86WithArg x $ \a -> x86CallReturn (Proxy @b) fp (a : args)

instance (LinuxX86_64ABIRet b) => X86DeferCall (LinuxX86_64CM (LinuxX86_64C b)) where
  x86DeferArg x dlFPtr args _ =
    withForeignPtr dlFPtr $ \p -> x86WithArg x $ \a -> x86CallReturn (Proxy @b) (castPtrToFunPtr p) (a : args)

instance X86DeferCall (LinuxX86_64CM ()) where
  x86DeferArg x dlFPtr args _ =
    withForeignPtr dlFPtr $ \p -> x86WithArg x $ \a -> callFFI (castPtrToFunPtr p) retVoid (reverse (a : args))

instance (LinuxX86_64ABIParam b, X86DeferCall r, X86DeferCont r, LinuxX86_64Evaluate r) => X86DeferCall (LinuxX86_64C b -> r) where
  x86DeferArg x dlFPtr args f y =
    let dummy = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
     in x86DeferCont (\k -> x86WithArg x $ \ax -> x86WithArg y $ \ay -> k (ay : ax : args)) dlFPtr (f dummy)

class X86DeferCont r where
  x86DeferCont :: (([Arg] -> IO (LinuxX86_64BaseRet r)) -> IO (LinuxX86_64BaseRet r)) -> ForeignPtr () -> r -> LinuxX86_64CEval r

type family LinuxX86_64BaseRet r where
  LinuxX86_64BaseRet (LinuxX86_64C a) = a
  LinuxX86_64BaseRet (LinuxX86_64CM (LinuxX86_64C a)) = a
  LinuxX86_64BaseRet (LinuxX86_64CM ()) = ()
  LinuxX86_64BaseRet (LinuxX86_64C _ -> r) = LinuxX86_64BaseRet r

instance (LinuxX86_64ABIRet a) => X86DeferCont (LinuxX86_64C a) where
  x86DeferCont withArgs dlFPtr _ =
    evalPureLinuxX86_64FFI dlFPtr $ \fp -> withArgs (x86CallReturn (Proxy @a) fp)

instance (LinuxX86_64ABIRet a) => X86DeferCont (LinuxX86_64CM (LinuxX86_64C a)) where
  x86DeferCont withArgs dlFPtr _ =
    withForeignPtr dlFPtr $ \p -> withArgs (x86CallReturn (Proxy @a) (castPtrToFunPtr p))

instance X86DeferCont (LinuxX86_64CM ()) where
  x86DeferCont withArgs dlFPtr _ =
    withForeignPtr dlFPtr $ \p -> withArgs (callFFI (castPtrToFunPtr p) retVoid . reverse)

instance (LinuxX86_64ABIParam a, X86DeferCont r) => X86DeferCont (LinuxX86_64C a -> r) where
  x86DeferCont withArgs dlFPtr f val =
    let dummy = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
     in x86DeferCont (\k -> withArgs $ \as -> x86WithArg val $ \av -> k (av : as)) dlFPtr (f dummy)

showLinuxX86_64CWith :: (LinuxX86_64Param r) => X86Features -> String -> r -> String
showLinuxX86_64CWith fs name a =
  let jcs = x86param fs name 0 (pure []) a
      targetBanner = "/* janus-x86-target: " <> unwords (x86CompilerFlags fs) <> " */\n"
      sep = "\n" <> replicate 80 '-' <> "\n"
   in targetBanner <> foldMap ((<> sep) . renderJCFunc) jcs

showLinuxX86_64C :: (LinuxX86_64Param r) => String -> r -> IO String
showLinuxX86_64C name a = do
  fs <- observeHostX86Features
  pure (showLinuxX86_64CWith fs name a)

printLinuxX86_64CWith :: (LinuxX86_64Param r) => X86Features -> r -> IO ()
printLinuxX86_64CWith fs = putStrLn . showLinuxX86_64CWith fs "janus_main"

printLinuxX86_64C :: (LinuxX86_64Param r) => r -> IO ()
printLinuxX86_64C a = showLinuxX86_64C "janus_main" a >>= putStrLn

compileLinuxX86_64CWith ::
  forall r.
  (LinuxX86_64Param r, X86DeferCont r) =>
  X86Features ->
  CBuildConfig ->
  r ->
  IO (LinuxX86_64CEval r)
compileLinuxX86_64CWith reqFeatures config a = fst <$> acquireLinuxX86_64CWith reqFeatures config a

compileLinuxX86_64C ::
  (LinuxX86_64Param r, X86DeferCont r) =>
  CBuildConfig ->
  r ->
  IO (LinuxX86_64CEval r)
compileLinuxX86_64C config a = do
  fs <- observeHostX86Features
  compileLinuxX86_64CWith fs config a

acquireLinuxX86_64CWith ::
  forall r.
  (LinuxX86_64Param r, X86DeferCont r) =>
  X86Features ->
  CBuildConfig ->
  r ->
  IO (LinuxX86_64CEval r, ForeignPtr ())
acquireLinuxX86_64CWith reqFeatures config a = do
  observed <- observeHostX86Features
  ensureX86Admitted observed reqFeatures
  let dir = config ^. cBuildConfigCacheDir
      funcs = x86param reqFeatures "janus_main" 0 (pure []) a
      needsSleef = any (Set.member "sleef.h" . _jcfHeaders) funcs
  (sleefCFlags, sleefLDFlags) <- if needsSleef then observeSleefFlags else pure ([], [])
  let cflags = x86CompilerFlags reqFeatures <> sleefCFlags
  files <- writeJanusCFiles JC dir funcs
  dlFPtr <- acquireJanusCFunPtrWithFlags dir cflags sleefLDFlags files "janus_main"
  pure (x86DeferCont ($ []) dlFPtr a, dlFPtr)

acquireLinuxX86_64C ::
  (LinuxX86_64Param r, X86DeferCont r) =>
  CBuildConfig ->
  r ->
  IO (LinuxX86_64CEval r, ForeignPtr ())
acquireLinuxX86_64C config a = do
  fs <- observeHostX86Features
  acquireLinuxX86_64CWith fs config a

releaseLinuxX86_64C :: (a, ForeignPtr ()) -> IO ()
releaseLinuxX86_64C _ = pure ()

withLinuxX86_64CWith ::
  (LinuxX86_64Param r, X86DeferCont r) =>
  X86Features ->
  CBuildConfig ->
  r ->
  (LinuxX86_64CEval r -> IO a) ->
  IO a
withLinuxX86_64CWith fs config a k =
  compileLinuxX86_64CWith fs config a >>= k

withLinuxX86_64C ::
  (LinuxX86_64Param r, X86DeferCont r) =>
  CBuildConfig ->
  r ->
  (LinuxX86_64CEval r -> IO a) ->
  IO a
withLinuxX86_64C config a k = do
  fs <- observeHostX86Features
  withLinuxX86_64CWith fs config a k

--------------------------------------------------------------------------------
-- ExpSIMD LinuxX86_64C: Associated Types, ABI, & Register Representation
--------------------------------------------------------------------------------

instance ExpSIMD LinuxX86_64C where
  newtype SIMDVector LinuxX86_64C n a = LinuxX86_64SIMDVector {getLinuxX86_64SIMDVector :: RawSIMD n a}
    deriving stock (Eq, Ord)
  newtype SIMDMask LinuxX86_64C n a = LinuxX86_64SIMDMask {getLinuxX86_64SIMDMask :: RawMask n a}
    deriving stock (Eq, Ord)

deriving newtype instance (SIMDShape n a) => Show (SIMDVector LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Show (SIMDMask LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Storable.Storable (SIMDVector LinuxX86_64C n a)
deriving newtype instance (SIMDShape n a) => Storable.Storable (SIMDMask LinuxX86_64C n a)

instance (SIMDShape n a) => HostSIMDValue (SIMDVector LinuxX86_64C n a) n a where
  simdFromList = LinuxX86_64SIMDVector . rawSIMDFromList
  simdToList = rawSIMDToList . getLinuxX86_64SIMDVector

instance (SIMDShape n a) => HostSIMDMask (SIMDMask LinuxX86_64C n a) n a where
  simdMaskFromBools = LinuxX86_64SIMDMask . rawMaskFromBools
  simdMaskToBools = rawMaskToBools . getLinuxX86_64SIMDMask

instance (SIMDShape n a) => LinuxX86_64ABIParam (SIMDVector LinuxX86_64C n a) where
  x86EmitParam k = do
    let aid = mkArgId k
        JCType vspec vdec = voidPtrType
    loadExp <- emitMemcpyLoad @(SIMDVector LinuxX86_64C n a) (simdBytes (Proxy @n) (Proxy @a)) (Var aid noLoc)
    pure (Param (Just aid) vspec vdec noLoc, loadExp)
  x86WithArg v k =
    let b = simdBytes (Proxy @n) (Proxy @a)
     in allocaBytes b $ \p -> Storable.poke p v >> k (argPtr p)

instance (SIMDShape n a) => LinuxX86_64ABIRet (SIMDVector LinuxX86_64C n a) where
  x86FinishReturn = emitMemcpyRet (simdBytes (Proxy @n) (Proxy @a))
  x86CallReturn _ fp args =
    let b = simdBytes (Proxy @n) (Proxy @a)
     in allocaBytes b $ \pOut -> do
          callFFI fp retVoid (reverse args <> [argPtr pOut])
          Storable.peek pOut

instance (SIMDShape n a) => LinuxX86_64ABIParam (SIMDMask LinuxX86_64C n a) where
  x86EmitParam k = do
    let aid = mkArgId k
        JCType vspec vdec = voidPtrType
    loadExp <- emitMemcpyLoad @(SIMDMask LinuxX86_64C n a) (simdBytes (Proxy @n) (Proxy @a)) (Var aid noLoc)
    pure (Param (Just aid) vspec vdec noLoc, loadExp)
  x86WithArg m k =
    let b = simdBytes (Proxy @n) (Proxy @a)
     in allocaBytes b $ \p -> Storable.poke p m >> k (argPtr p)

instance (SIMDShape n a) => LinuxX86_64ABIRet (SIMDMask LinuxX86_64C n a) where
  x86FinishReturn = emitMemcpyRet (simdBytes (Proxy @n) (Proxy @a))
  x86CallReturn _ fp args =
    let b = simdBytes (Proxy @n) (Proxy @a)
     in allocaBytes b $ \pOut -> do
          callFFI fp retVoid (reverse args <> [argPtr pOut])
          Storable.peek pOut

namedCType :: String -> JCType
namedCType nm = JCType (DeclSpec [] [] (Tnamed (Id nm noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

x86128RegName :: forall a. (SIMDElem a) => Proxy a -> String
x86128RegName p = case simdElemTag p of
  8 -> "__m128"
  9 -> "__m128d"
  _ -> "__m128i"

x86Has256Reg :: forall a. (SIMDElem a) => X86Features -> Proxy a -> Bool
x86Has256Reg fs p =
  if simdElemIsFloat p
    then hasX86Feature fs X86_AVX
    else hasX86Feature fs X86_AVX2

x86256RegName :: forall a. (SIMDElem a) => Proxy a -> String
x86256RegName p = case simdElemTag p of
  8 -> "__m256"
  9 -> "__m256d"
  _ -> "__m256i"

x86SIMDType :: forall n a. (SIMDShape n a) => JanusCM JCType
x86SIMDType = do
  addHeader "immintrin.h"
  addHeader "stdint.h"
  addHeader "string.h"
  fs <- askX86Features
  let b = simdBytes (Proxy @n) (Proxy @a)
  case b of
    4 -> pure (namedCType "uint32_t")
    8 -> pure (namedCType "uint64_t")
    16 -> pure (namedCType (x86128RegName (Proxy @a)))
    32
      | x86Has256Reg fs (Proxy @a) -> pure (namedCType (x86256RegName (Proxy @a)))
      | otherwise -> do
          let JCType hspec _ = namedCType (x86128RegName (Proxy @a))
              fgs =
                [ FieldGroup hspec [Field (Just (Id "lo" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc,
                  FieldGroup hspec [Field (Just (Id "hi" noLoc)) (Just (DeclRoot noLoc)) Nothing noLoc] noLoc
                ]
          pure $ JCType (DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc) (DeclRoot noLoc)
    _ -> error "x86SIMDType: unsupported byte width"

instance (SIMDShape n a) => JanusCTyped (SIMDVector LinuxX86_64C n a) where
  jctype _ = x86SIMDType @n @a

instance (SIMDShape n a) => JanusCTyped (SIMDMask LinuxX86_64C n a) where
  jctype _ = getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))

--------------------------------------------------------------------------------
-- AST & <immintrin.h> Lowering Helpers
--------------------------------------------------------------------------------

cVar :: String -> Exp
cVar s = Var (Id s noLoc) noLoc

cCall :: String -> [Exp] -> Exp
cCall fn args = FnCall (cVar fn) args noLoc

cType :: JCType -> Type
cType (JCType spec dec) = Type spec dec noLoc

cNamedType :: String -> Type
cNamedType = cType . namedCType

cCast :: String -> Exp -> Exp
cCast ty e = Cast (cNamedType ty) e noLoc

cPtrCast :: String -> Exp -> Exp
cPtrCast ty e =
  let JCType spec dec = namedCType ty
   in Cast (Type spec (Ptr [] dec noLoc) noLoc) e noLoc

cConstPtrCast :: String -> Exp -> Exp
cConstPtrCast ty e =
  Cast (Type (DeclSpec [] [Tconst noLoc] (Tnamed (Id ty noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc) noLoc) e noLoc

cInt :: Int -> Exp
cInt = signedIntConst

cMember :: Exp -> String -> Exp
cMember e m = Member e (Id m noLoc) noLoc

emitStmt :: Stm -> JanusCM ()
emitStmt stm = modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm stm]

emitUninitLocal :: JCType -> JanusCM Exp
emitUninitLocal (JCType spec dec) = do
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing Nothing [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl])
  pure (Var vid noLoc)

emitPairStruct :: JCType -> Exp -> Exp -> JanusCM Exp
emitPairStruct pairTy lo hi =
  emitLocalVal pairTy $
    CompoundLit
      (cType pairTy)
      [ (Just (Designation [MemberDesignator (Id "lo" noLoc) noLoc] noLoc), ExpInitializer lo noLoc),
        (Just (Designation [MemberDesignator (Id "hi" noLoc) noLoc] noLoc), ExpInitializer hi noLoc)
      ]
      noLoc

-- Convert a 4B/8B/16B vector expression into its 128-bit XMM register (__m128 / __m128d / __m128i)
toXMM128 :: forall a. (SIMDElem a) => Int -> Exp -> JanusCM Exp
toXMM128 16 e = pure e
toXMM128 4 e = do
  let xi = cCall "_mm_cvtsi32_si128" [cCast "int32_t" e]
  case simdElemTag (Proxy @a) of
    8 -> emitLocalVal (namedCType "__m128") (cCall "_mm_castsi128_ps" [xi])
    9 -> emitLocalVal (namedCType "__m128d") (cCall "_mm_castsi128_pd" [xi])
    _ -> emitLocalVal (namedCType "__m128i") xi
toXMM128 8 e = do
  let xi = cCall "_mm_cvtsi64_si128" [cCast "int64_t" e]
  case simdElemTag (Proxy @a) of
    8 -> emitLocalVal (namedCType "__m128") (cCall "_mm_castsi128_ps" [xi])
    9 -> emitLocalVal (namedCType "__m128d") (cCall "_mm_castsi128_pd" [xi])
    _ -> emitLocalVal (namedCType "__m128i") xi
toXMM128 _ _ = error "toXMM128: invalid byte size"

fromXMM128 :: forall a. (SIMDElem a) => Int -> Exp -> JanusCM Exp
fromXMM128 16 e = emitLocalVal (namedCType (x86128RegName (Proxy @a))) e
fromXMM128 4 e = do
  let asI = case simdElemTag (Proxy @a) of
        8 -> cCall "_mm_castps_si128" [e]
        9 -> cCall "_mm_castpd_si128" [e]
        _ -> e
  emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si32" [asI]))
fromXMM128 8 e = do
  let asI = case simdElemTag (Proxy @a) of
        8 -> cCall "_mm_castps_si128" [e]
        9 -> cCall "_mm_castpd_si128" [e]
        _ -> e
  emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [asI]))
fromXMM128 _ _ = error "fromXMM128: invalid byte size"

asM128i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
asM128i p e = case simdElemTag p of
  8 -> cCall "_mm_castps_si128" [e]
  9 -> cCall "_mm_castpd_si128" [e]
  _ -> e

fromM128i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
fromM128i p e = case simdElemTag p of
  8 -> cCall "_mm_castsi128_ps" [e]
  9 -> cCall "_mm_castsi128_pd" [e]
  _ -> e

asM256i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
asM256i p e = case simdElemTag p of
  8 -> cCall "_mm256_castps_si256" [e]
  9 -> cCall "_mm256_castpd_si256" [e]
  _ -> e

fromM256i :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
fromM256i p e = case simdElemTag p of
  8 -> cCall "_mm256_castsi256_ps" [e]
  9 -> cCall "_mm256_castsi256_pd" [e]
  _ -> e

-- Scalar lane fallback inside generated C using stack array round-trip
emitLaneLoopBin ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  Exp ->
  Exp ->
  JanusCM Exp
emitLaneLoopBin bodyFn va vb = do
  addHeader "string.h"
  addHeader "stdint.h"
  addHeader "math.h"
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemTyStr = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemTyStr
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- emitUninitLocal arrTy
  ab <- emitUninitLocal arrTy
  ar <- emitUninitLocal arrTy
  resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
  va' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` va)
  vb' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` vb)
  emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf va' noLoc, signedIntConst bytes]) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf vb' noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
        lhs = Index ar idx noLoc
        rhs = bodyFn (Index aa idx noLoc) (Index ab idx noLoc)
    emitStmt $ Exp (Just $ Assign lhs JustAssign rhs noLoc) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst bytes]) noLoc
  pure resVar

emitLaneLoopUn ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp) ->
  Exp ->
  JanusCM Exp
emitLaneLoopUn bodyFn va = do
  addHeader "string.h"
  addHeader "stdint.h"
  addHeader "math.h"
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemTyStr = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemTyStr
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arrTy = JCType espec arrDec
  aa <- emitUninitLocal arrTy
  ar <- emitUninitLocal arrTy
  resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
  va' <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` va)
  emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf va' noLoc, signedIntConst bytes]) noLoc
  forM_ [0 .. lanes - 1] $ \i -> do
    let idx = signedIntConst i
    emitStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (bodyFn (Index aa idx noLoc)) noLoc) noLoc
  emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst bytes]) noLoc
  pure resVar

extractLo256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
extractLo256 p e = case simdElemTag p of
  8 -> cCall "_mm256_castps256_ps128" [e]
  9 -> cCall "_mm256_castpd256_pd128" [e]
  _ -> cCall "_mm256_castsi256_si128" [e]

extractHi256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp
extractHi256 p e = case simdElemTag p of
  8 -> cCall "_mm256_extractf128_ps" [e, cInt 1]
  9 -> cCall "_mm256_extractf128_pd" [e, cInt 1]
  _ -> cCall "_mm256_extracti128_si256" [e, cInt 1]

combine256 :: forall a. (SIMDElem a) => Proxy a -> Exp -> Exp -> Exp
combine256 p lo hi = case simdElemTag p of
  8 -> cCall "_mm256_insertf128_ps" [cCall "_mm256_castps128_ps256" [lo], hi, cInt 1]
  9 -> cCall "_mm256_insertf128_pd" [cCall "_mm256_castpd128_pd256" [lo], hi, cInt 1]
  _ -> cCall "_mm256_inserti128_si256" [cCall "_mm256_castsi128_si256" [lo], hi, cInt 1]

liftX86Bin ::
  forall n a.
  (SIMDShape n a) =>
  (X86Features -> Exp -> Exp -> JanusCM Exp) ->
  Maybe (X86Features -> Exp -> Exp -> JanusCM Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
liftX86Bin f128 mf256 (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) =
  LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32 -> case mf256 of
        Just f256 | x86Has256Reg fs pa -> do
          r256 <- f256 fs ea eb
          emitLocalVal ty r256
        Nothing | x86Has256Reg fs pa -> do
          rlo <- f128 fs (extractLo256 pa ea) (extractLo256 pa eb) >>= fromXMM128 @a 16
          rhi <- f128 fs (extractHi256 pa ea) (extractHi256 pa eb) >>= fromXMM128 @a 16
          emitLocalVal ty (combine256 pa rlo rhi)
        _ -> do
          rlo <- f128 fs (cMember ea "lo") (cMember eb "lo") >>= fromXMM128 @a 16
          rhi <- f128 fs (cMember ea "hi") (cMember eb "hi") >>= fromXMM128 @a 16
          emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xb <- toXMM128 @a b eb
        xr <- f128 fs xa xb
        fromXMM128 @a b xr
    pure (RVal r)

liftX86Un ::
  forall n a.
  (SIMDShape n a) =>
  (X86Features -> Exp -> JanusCM Exp) ->
  Maybe (X86Features -> Exp -> JanusCM Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
liftX86Un f128 mf256 (LinuxX86_64C (JanusC ma)) =
  LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    ea <- emitLocalVal ty ea0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32 -> case mf256 of
        Just f256 | x86Has256Reg fs pa -> do
          r256 <- f256 fs ea
          emitLocalVal ty r256
        Nothing | x86Has256Reg fs pa -> do
          rlo <- f128 fs (extractLo256 pa ea) >>= fromXMM128 @a 16
          rhi <- f128 fs (extractHi256 pa ea) >>= fromXMM128 @a 16
          emitLocalVal ty (combine256 pa rlo rhi)
        _ -> do
          rlo <- f128 fs (cMember ea "lo") >>= fromXMM128 @a 16
          rhi <- f128 fs (cMember ea "hi") >>= fromXMM128 @a 16
          emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xr <- f128 fs xa
        fromXMM128 @a b xr
    pure (RVal r)

--------------------------------------------------------------------------------
-- Instruction Selection Implementations (SSE2 .. AVX2 + FMA)
--------------------------------------------------------------------------------

x86Set1_128 :: forall a. (SIMDElem a) => Exp -> Exp
x86Set1_128 s = case simdElemTag (Proxy @a) of
  0 -> cCall "_mm_set1_epi8" [cCast "char" s]
  1 -> cCall "_mm_set1_epi8" [cCast "char" s]
  2 -> cCall "_mm_set1_epi16" [cCast "short" s]
  3 -> cCall "_mm_set1_epi16" [cCast "short" s]
  4 -> cCall "_mm_set1_epi32" [cCast "int" s]
  5 -> cCall "_mm_set1_epi32" [cCast "int" s]
  6 -> cCall "_mm_set1_epi64x" [cCast "long long" s]
  7 -> cCall "_mm_set1_epi64x" [cCast "long long" s]
  8 -> cCall "_mm_set1_ps" [s]
  _ -> cCall "_mm_set1_pd" [s]

x86Set1_256 :: forall a. (SIMDElem a) => Exp -> Exp
x86Set1_256 s = case simdElemTag (Proxy @a) of
  0 -> cCall "_mm256_set1_epi8" [cCast "char" s]
  1 -> cCall "_mm256_set1_epi8" [cCast "char" s]
  2 -> cCall "_mm256_set1_epi16" [cCast "short" s]
  3 -> cCall "_mm256_set1_epi16" [cCast "short" s]
  4 -> cCall "_mm256_set1_epi32" [cCast "int" s]
  5 -> cCall "_mm256_set1_epi32" [cCast "int" s]
  6 -> cCall "_mm256_set1_epi64x" [cCast "long long" s]
  7 -> cCall "_mm256_set1_epi64x" [cCast "long long" s]
  8 -> cCall "_mm256_set1_ps" [s]
  _ -> cCall "_mm256_set1_pd" [s]

x86Zero128 :: forall a. (SIMDElem a) => Exp
x86Zero128 = case simdElemTag (Proxy @a) of
  8 -> cCall "_mm_setzero_ps" []
  9 -> cCall "_mm_setzero_pd" []
  _ -> cCall "_mm_setzero_si128" []

x86Zero256 :: forall a. (SIMDElem a) => Exp
x86Zero256 = case simdElemTag (Proxy @a) of
  8 -> cCall "_mm256_setzero_ps" []
  9 -> cCall "_mm256_setzero_pd" []
  _ -> cCall "_mm256_setzero_si256" []

x86Add128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Add128 _ a b = pure $ case simdElemTag (Proxy @a) of
  0 -> cCall "_mm_add_epi8" [a, b]
  1 -> cCall "_mm_add_epi8" [a, b]
  2 -> cCall "_mm_add_epi16" [a, b]
  3 -> cCall "_mm_add_epi16" [a, b]
  4 -> cCall "_mm_add_epi32" [a, b]
  5 -> cCall "_mm_add_epi32" [a, b]
  6 -> cCall "_mm_add_epi64" [a, b]
  7 -> cCall "_mm_add_epi64" [a, b]
  8 -> cCall "_mm_add_ps" [a, b]
  _ -> cCall "_mm_add_pd" [a, b]

x86Add256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Add256 _ a b = pure $ case simdElemTag (Proxy @a) of
  0 -> cCall "_mm256_add_epi8" [a, b]
  1 -> cCall "_mm256_add_epi8" [a, b]
  2 -> cCall "_mm256_add_epi16" [a, b]
  3 -> cCall "_mm256_add_epi16" [a, b]
  4 -> cCall "_mm256_add_epi32" [a, b]
  5 -> cCall "_mm256_add_epi32" [a, b]
  6 -> cCall "_mm256_add_epi64" [a, b]
  7 -> cCall "_mm256_add_epi64" [a, b]
  8 -> cCall "_mm256_add_ps" [a, b]
  _ -> cCall "_mm256_add_pd" [a, b]

x86Sub128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Sub128 _ a b = pure $ case simdElemTag (Proxy @a) of
  0 -> cCall "_mm_sub_epi8" [a, b]
  1 -> cCall "_mm_sub_epi8" [a, b]
  2 -> cCall "_mm_sub_epi16" [a, b]
  3 -> cCall "_mm_sub_epi16" [a, b]
  4 -> cCall "_mm_sub_epi32" [a, b]
  5 -> cCall "_mm_sub_epi32" [a, b]
  6 -> cCall "_mm_sub_epi64" [a, b]
  7 -> cCall "_mm_sub_epi64" [a, b]
  8 -> cCall "_mm_sub_ps" [a, b]
  _ -> cCall "_mm_sub_pd" [a, b]

x86Sub256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Sub256 _ a b = pure $ case simdElemTag (Proxy @a) of
  0 -> cCall "_mm256_sub_epi8" [a, b]
  1 -> cCall "_mm256_sub_epi8" [a, b]
  2 -> cCall "_mm256_sub_epi16" [a, b]
  3 -> cCall "_mm256_sub_epi16" [a, b]
  4 -> cCall "_mm256_sub_epi32" [a, b]
  5 -> cCall "_mm256_sub_epi32" [a, b]
  6 -> cCall "_mm256_sub_epi64" [a, b]
  7 -> cCall "_mm256_sub_epi64" [a, b]
  8 -> cCall "_mm256_sub_ps" [a, b]
  _ -> cCall "_mm256_sub_pd" [a, b]

x86Mul128 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Mul128 fs a b = case simdElemTag (Proxy @a) of
  8 -> pure $ cCall "_mm_mul_ps" [a, b]
  9 -> pure $ cCall "_mm_mul_pd" [a, b]
  2 -> pure $ cCall "_mm_mullo_epi16" [a, b]
  3 -> pure $ cCall "_mm_mullo_epi16" [a, b]
  t | t == 4 || t == 5 ->
      if hasX86Feature fs X86_SSE4_1
        then pure $ cCall "_mm_mullo_epi32" [a, b]
        else do
          m02 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_mul_epu32" [a, b])
          m13 <-
            emitLocalVal
              (namedCType "__m128i")
              (cCall "_mm_mul_epu32" [cCall "_mm_srli_si128" [a, cInt 4], cCall "_mm_srli_si128" [b, cInt 4]])
          pure $
            cCall
              "_mm_unpacklo_epi32"
              [ cCall "_mm_shuffle_epi32" [m02, cInt 8],
                cCall "_mm_shuffle_epi32" [m13, cInt 8]
              ]
  t | t == 0 || t == 1 -> do
      ev <- emitLocalVal (namedCType "__m128i") (cCall "_mm_mullo_epi16" [a, b])
      od <-
        emitLocalVal
          (namedCType "__m128i")
          (cCall "_mm_mullo_epi16" [cCall "_mm_srli_epi16" [a, cInt 8], cCall "_mm_srli_epi16" [b, cInt 8]])
      pure $
        cCall
          "_mm_or_si128"
          [ cCall "_mm_and_si128" [ev, cCall "_mm_set1_epi16" [cInt 0x00FF]],
            cCall "_mm_slli_epi16" [od, cInt 8]
          ]
  _ -> do
    -- 64-bit integer multiply: extract 2x uint64_t and repack
    a0 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [a]))
    b0 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [b]))
    a1 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [a, cInt 8]]))
    b1 <- emitLocalVal (namedCType "uint64_t") (cCast "uint64_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [b, cInt 8]]))
    pure $
      cCall
        "_mm_set_epi64x"
        [ cCast "long long" (BinOp Mul a1 b1 noLoc),
          cCast "long long" (BinOp Mul a0 b0 noLoc)
        ]

x86Mul256 :: forall a. (SIMDElem a) => X86Features -> Exp -> Exp -> JanusCM Exp
x86Mul256 _ a b = case simdElemTag (Proxy @a) of
  8 -> pure $ cCall "_mm256_mul_ps" [a, b]
  9 -> pure $ cCall "_mm256_mul_pd" [a, b]
  2 -> pure $ cCall "_mm256_mullo_epi16" [a, b]
  3 -> pure $ cCall "_mm256_mullo_epi16" [a, b]
  4 -> pure $ cCall "_mm256_mullo_epi32" [a, b]
  5 -> pure $ cCall "_mm256_mullo_epi32" [a, b]
  t | t == 0 || t == 1 -> do
      ev <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_mullo_epi16" [a, b])
      od <-
        emitLocalVal
          (namedCType "__m256i")
          (cCall "_mm256_mullo_epi16" [cCall "_mm256_srli_epi16" [a, cInt 8], cCall "_mm256_srli_epi16" [b, cInt 8]])
      pure $
        cCall
          "_mm256_or_si256"
          [ cCall "_mm256_and_si256" [ev, cCall "_mm256_set1_epi16" [cInt 0x00FF]],
            cCall "_mm256_slli_epi16" [od, cInt 8]
          ]
  _ -> do
    alo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [a])
    ahi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [a, cInt 1])
    blo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [b])
    bhi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [b, cInt 1])
    rlo <- x86Mul128 @a x86AVX2 alo blo
    rhi <- x86Mul128 @a x86AVX2 ahi bhi
    pure $ cCall "_mm256_set_m128i" [rhi, rlo]

--------------------------------------------------------------------------------
-- Instance Implementations for LinuxX86_64C / LinuxX86_64CM
--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, JanusLitC a) => Num (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  (+) = vadd
  (-) = vsub
  (*) = vmul
  negate = vneg
  abs = vabs
  signum v = LinuxX86_64C $ JanusC $ do
    RVal ev <- getJanusC (getLinuxX86_64C v)
    let tag = simdElemTag (Proxy @a)
        z = cInt 0
        one = if simdElemIsFloat (Proxy @a) then C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc else cInt 1
        negOne = UnOp Negate one noLoc
    r <- emitLaneLoopUn @n @a (\x -> if simdElemSigned (Proxy @a) then (if tag == 8 || tag == 9 then Cond (BinOp Gt x z noLoc) one (Cond (BinOp Lt x z noLoc) negOne x noLoc) noLoc else BinOp Sub (BinOp Gt x z noLoc) (BinOp Lt x z noLoc) noLoc) else Cond (BinOp Gt x z noLoc) (cInt 1) z noLoc) ev
    pure (RVal r)
  fromInteger k = vsplat (fromInteger k)

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => Fractional (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  (/) = vdiv
  recip x = fromRational 1 / x
  fromRational q = vsplat (fromRational q)

sleefSuffix128 :: X86Features -> String
sleefSuffix128 fs
  | hasX86Feature fs X86_AVX2 && hasX86Feature fs X86_FMA = "avx2128"
  | hasX86Feature fs X86_SSE4_1 = "sse4"
  | otherwise = "sse2"

sleefSuffix256 :: X86Features -> String
sleefSuffix256 fs
  | hasX86Feature fs X86_AVX2 && hasX86Feature fs X86_FMA = "avx2"
  | otherwise = "avx"

sleefUnOp ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
sleefUnOp opName ulp =
  let isF32 = simdElemTag (Proxy @a) == 8
      w128 = if isF32 then "f4_" else "d2_"
      w256 = if isF32 then "f8_" else "d4_"
      fn128 fs x = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w128 <> ulp <> sleefSuffix128 fs) [x])
      fn256 fs x = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w256 <> ulp <> sleefSuffix256 fs) [x])
   in liftX86Un @n @a fn128 (Just fn256)

sleefBinOp ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
sleefBinOp opName ulp =
  let isF32 = simdElemTag (Proxy @a) == 8
      w128 = if isF32 then "f4_" else "d2_"
      w256 = if isF32 then "f8_" else "d4_"
      fn128 fs x y = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w128 <> ulp <> sleefSuffix128 fs) [x, y])
      fn256 fs x y = addHeader "sleef.h" >> pure (cCall ("Sleef_" <> opName <> w256 <> ulp <> sleefSuffix256 fs) [x, y])
   in liftX86Bin @n @a fn128 (Just fn256)

instance (SIMDShape n Float) => Floating (LinuxX86_64C (SIMDVector LinuxX86_64C n Float)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp = sleefUnOp "exp" "u10"
  log = sleefUnOp "log" "u10"
  (**) = sleefBinOp "pow" "u10"
  logBase x y = log y / log x
  sin = sleefUnOp "sin" "u10"
  cos = sleefUnOp "cos" "u10"
  tan = sleefUnOp "tan" "u10"
  asin = sleefUnOp "asin" "u10"
  acos = sleefUnOp "acos" "u10"
  atan = sleefUnOp "atan" "u10"
  sinh = sleefUnOp "sinh" "u10"
  cosh = sleefUnOp "cosh" "u10"
  tanh = sleefUnOp "tanh" "u10"
  asinh = sleefUnOp "asinh" "u10"
  acosh = sleefUnOp "acosh" "u10"
  atanh = sleefUnOp "atanh" "u10"
  log1p = sleefUnOp "log1p" "u10"
  expm1 = sleefUnOp "expm1" "u10"

instance (SIMDShape n Double) => Floating (LinuxX86_64C (SIMDVector LinuxX86_64C n Double)) where
  pi = vsplat pi
  sqrt = vsqrt
  exp = sleefUnOp "exp" "u10"
  log = sleefUnOp "log" "u10"
  (**) = sleefBinOp "pow" "u10"
  logBase x y = log y / log x
  sin = sleefUnOp "sin" "u10"
  cos = sleefUnOp "cos" "u10"
  tan = sleefUnOp "tan" "u10"
  asin = sleefUnOp "asin" "u10"
  acos = sleefUnOp "acos" "u10"
  atan = sleefUnOp "atan" "u10"
  sinh = sleefUnOp "sinh" "u10"
  cosh = sleefUnOp "cosh" "u10"
  tanh = sleefUnOp "tanh" "u10"
  asinh = sleefUnOp "asinh" "u10"
  acosh = sleefUnOp "acosh" "u10"
  atanh = sleefUnOp "atanh" "u10"
  log1p = sleefUnOp "log1p" "u10"
  expm1 = sleefUnOp "expm1" "u10"

instance {-# OVERLAPPING #-} (SIMDShape n Float) => Hypot (LinuxX86_64C (SIMDVector LinuxX86_64C n Float)) where
  hypot = sleefBinOp "hypot" "u05"

instance {-# OVERLAPPING #-} (SIMDShape n Double) => Hypot (LinuxX86_64C (SIMDVector LinuxX86_64C n Double)) where
  hypot = sleefBinOp "hypot" "u05"

instance {-# OVERLAPPING #-} (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a, JanusLitC a) => FMA (LinuxX86_64C (SIMDVector LinuxX86_64C n a)) where
  fma = vfma

instance (SIMDShape n a) => Join (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  (\/) = mor

instance (SIMDShape n a) => Meet (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  (/\) = mand

instance (SIMDShape n a) => Lower (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  lowerBound = mfalse

instance (SIMDShape n a) => Upper (LinuxX86_64C (SIMDMask LinuxX86_64C n a)) where
  upperBound = mtrue

-- 1. ExpSIMDVec
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDVec LinuxX86_64C n a where
  vzero = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    let b = simdBytes (Proxy @n) (Proxy @a)
    r <- case b of
      4 -> emitLocalVal ty (unsignedIntConst (0 :: Word32))
      8 -> emitLocalVal ty (unsignedIntConst (0 :: Word64))
      16 -> emitLocalVal ty (x86Zero128 @a)
      32
        | x86Has256Reg fs (Proxy @a) -> emitLocalVal ty (x86Zero256 @a)
        | otherwise -> do
            z <- emitLocalVal (namedCType (x86128RegName (Proxy @a))) (x86Zero128 @a)
            emitPairStruct ty z z
      _ -> error "vzero: invalid size"
    pure (RVal r)

  vsplat (LinuxX86_64C (JanusC ms)) = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal s0 <- ms
    let sTy = namedCType (simdElemCTypeName (Proxy @a))
    s <- emitLocalVal sTy s0
    let b = simdBytes (Proxy @n) (Proxy @a)
    r <- case b of
      32
        | x86Has256Reg fs (Proxy @a) -> emitLocalVal ty (x86Set1_256 @a s)
        | otherwise -> do
            h <- emitLocalVal (namedCType (x86128RegName (Proxy @a))) (x86Set1_128 @a s)
            emitPairStruct ty h h
      _ -> fromXMM128 @a b (x86Set1_128 @a s)
    pure (RVal r)

  viota = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arrVar <- emitUninitLocal (JCType espec arrDec)
    forM_ [0 .. lanes - 1] $ \k ->
      emitStmt $ Exp (Just $ Assign (Index arrVar (signedIntConst k) noLoc) JustAssign (cCast (simdElemCTypeName (Proxy @a)) (signedIntConst k)) noLoc) noLoc
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, arrVar, signedIntConst bytes]) noLoc
    pure (RVal res)

  vfromLanes es = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    when (length es /= lanes) $ error "vfromLanes: lane count mismatch"
    arrVar <- emitUninitLocal (JCType espec arrDec)
    forM_ (zip [0 :: Int ..] es) $ \(i, LinuxX86_64C (JanusC me)) -> do
      RVal ev <- me
      emitStmt $ Exp (Just $ Assign (Index arrVar (signedIntConst i) noLoc) JustAssign ev noLoc) noLoc
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, arrVar, signedIntConst bytes]) noLoc
    pure (RVal res)

  vextractLane p (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- mv
    ev <- emitLocalVal vty ev0
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        sTy = namedCType (simdElemCTypeName (Proxy @a))
    out <- emitUninitLocal sTy
    let srcPtr = BinOp Add (cConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst (idx * sz)) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst sz]) noLoc
    pure (RVal out)

  vinsertLane p (LinuxX86_64C (JanusC mx)) (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ex <- mx
    RVal ev0 <- mv
    res <- emitLocalVal vty ev0
    let sTy = namedCType (simdElemCTypeName (Proxy @a))
    sx <- emitLocalVal sTy ex
    let idx = fromIntegral (natVal p) :: Int
        sz = simdElemBytes (Proxy @a)
        dstPtr = BinOp Add (cPtrCast "uint8_t" (UnOp AddrOf res noLoc)) (signedIntConst (idx * sz)) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [dstPtr, UnOp AddrOf sx noLoc, signedIntConst sz]) noLoc
    pure (RVal res)

-- 2. ExpSIMDMask
maskBinX86 ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  (Proxy a -> Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDMask LinuxX86_64C n a)
maskBinX86 op128i op256 (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) =
  LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    r <- case b of
      32
        | x86Has256Reg fs pa -> emitLocalVal ty (op256 pa ea eb)
        | otherwise -> do
            rlo <- fromXMM128 @a 16 (fromM128i pa (op128i (asM128i pa (cMember ea "lo")) (asM128i pa (cMember eb "lo"))))
            rhi <- fromXMM128 @a 16 (fromM128i pa (op128i (asM128i pa (cMember ea "hi")) (asM128i pa (cMember eb "hi"))))
            emitPairStruct ty rlo rhi
      _ -> do
        xa <- toXMM128 @a b ea
        xb <- toXMM128 @a b eb
        fromXMM128 @a b (fromM128i pa (op128i (asM128i pa xa) (asM128i pa xb)))
    pure (RVal r)

instance (SIMDShape n a) => ExpSIMDMask LinuxX86_64C n a where
  mfalse = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    let b = simdBytes (Proxy @n) (Proxy @a)
    r <- case b of
      4 -> emitLocalVal ty (unsignedIntConst (0 :: Word32))
      8 -> emitLocalVal ty (unsignedIntConst (0 :: Word64))
      16 -> emitLocalVal ty (x86Zero128 @a)
      32
        | x86Has256Reg fs (Proxy @a) -> emitLocalVal ty (x86Zero256 @a)
        | otherwise -> do
            z <- emitLocalVal (namedCType (x86128RegName (Proxy @a))) (x86Zero128 @a)
            emitPairStruct ty z z
      _ -> error "mfalse: invalid size"
    pure (RVal r)

  mtrue = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    let b = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
        ones128 = fromM128i pa (cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)])
        ones256 = fromM256i pa (cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)])
    r <- case b of
      4 -> emitLocalVal ty (unsignedIntConst (0xFFFFFFFF :: Word32))
      8 -> emitLocalVal ty (unsignedIntConst (0xFFFFFFFFFFFFFFFF :: Word64))
      16 -> emitLocalVal ty ones128
      32
        | x86Has256Reg fs pa -> emitLocalVal ty ones256
        | otherwise -> do
            o <- emitLocalVal (namedCType (x86128RegName pa)) ones128
            emitPairStruct ty o o
      _ -> error "mtrue: invalid size"
    pure (RVal r)

  mand = maskBinX86 (\x y -> cCall "_mm_and_si128" [x, y]) (\p x y -> case simdElemTag p of 8 -> cCall "_mm256_and_ps" [x, y]; 9 -> cCall "_mm256_and_pd" [x, y]; _ -> cCall "_mm256_and_si256" [x, y])
  mor = maskBinX86 (\x y -> cCall "_mm_or_si128" [x, y]) (\p x y -> case simdElemTag p of 8 -> cCall "_mm256_or_ps" [x, y]; 9 -> cCall "_mm256_or_pd" [x, y]; _ -> cCall "_mm256_or_si256" [x, y])
  mxor = maskBinX86 (\x y -> cCall "_mm_xor_si128" [x, y]) (\p x y -> case simdElemTag p of 8 -> cCall "_mm256_xor_ps" [x, y]; 9 -> cCall "_mm256_xor_pd" [x, y]; _ -> cCall "_mm256_xor_si256" [x, y])
  mandnot = maskBinX86 (\x y -> cCall "_mm_andnot_si128" [x, y]) (\p x y -> case simdElemTag p of 8 -> cCall "_mm256_andnot_ps" [x, y]; 9 -> cCall "_mm256_andnot_pd" [x, y]; _ -> cCall "_mm256_andnot_si256" [x, y])
  mnot m = mxor m mtrue

  vselect (LinuxX86_64C (JanusC mm)) (LinuxX86_64C (JanusC mt)) (LinuxX86_64C (JanusC mf)) =
    LinuxX86_64C $ JanusC $ do
      ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      fs <- askX86Features
      RVal em0 <- mm
      RVal et0 <- mt
      RVal ef0 <- mf
      em <- emitLocalVal mty em0
      et <- emitLocalVal ty et0
      ef <- emitLocalVal ty ef0
      let b = simdBytes (Proxy @n) (Proxy @a)
          pa = Proxy @a
          sel128 xm xt xf
            | hasX86Feature fs X86_SSE4_1 = case simdElemTag pa of
                8 -> cCall "_mm_blendv_ps" [xf, xt, xm]
                9 -> cCall "_mm_blendv_pd" [xf, xt, xm]
                _ -> cCall "_mm_blendv_epi8" [xf, xt, xm]
            | otherwise =
                fromM128i pa $
                  cCall
                    "_mm_or_si128"
                    [ cCall "_mm_and_si128" [asM128i pa xm, asM128i pa xt],
                      cCall "_mm_andnot_si128" [asM128i pa xm, asM128i pa xf]
                    ]
      r <- case b of
        32
          | x86Has256Reg fs pa -> do
              let e256 = case simdElemTag pa of
                    8 -> cCall "_mm256_blendv_ps" [ef, et, em]
                    9 -> cCall "_mm256_blendv_pd" [ef, et, em]
                    _ -> cCall "_mm256_blendv_epi8" [ef, et, em]
              emitLocalVal ty e256
          | otherwise -> do
              rlo <- fromXMM128 @a 16 (sel128 (cMember em "lo") (cMember et "lo") (cMember ef "lo"))
              rhi <- fromXMM128 @a 16 (sel128 (cMember em "hi") (cMember et "hi") (cMember ef "hi"))
              emitPairStruct ty rlo rhi
        _ -> do
          xm <- toXMM128 @a b em
          xt <- toXMM128 @a b et
          xf <- toXMM128 @a b ef
          fromXMM128 @a b (sel128 xm xt xf)
      pure (RVal r)

  mmovemask (LinuxX86_64C (JanusC mm)) = LinuxX86_64C $ JanusC $ do
    mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
    fs <- askX86Features
    RVal em0 <- mm
    em <- emitLocalVal mty em0
    let sz = simdElemBytes (Proxy @a)
        lanes = simdLanes (Proxy @n) (Proxy @a)
        b = lanes * sz
        lmask = if lanes == 32 then 0xFFFFFFFF :: Word32 else (1 `shiftL` lanes) - 1
    case (sz, b) of
      (1, _) | b <= 16 -> do
        xm <- toXMM128 @a b em
        pure . RVal $ BinOp And (cCast "uint32_t" (cCall "_mm_movemask_epi8" [asM128i (Proxy @a) xm])) (unsignedIntConst lmask) noLoc
      (1, 32) | hasX86Feature fs X86_AVX2 ->
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_epi8" [em])
      (4, _) | b <= 16 -> do
        xm <- toXMM128 @a b em
        let ps = if simdElemTag (Proxy @a) == 8 then xm else cCall "_mm_castsi128_ps" [xm]
        pure . RVal $ BinOp And (cCast "uint32_t" (cCall "_mm_movemask_ps" [ps])) (unsignedIntConst lmask) noLoc
      (4, 32) | x86Has256Reg fs (Proxy @a) -> do
        let ps = if simdElemTag (Proxy @a) == 8 then em else cCall "_mm256_castsi256_ps" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_ps" [ps])
      (8, 16) -> do
        let pd = if simdElemTag (Proxy @a) == 9 then em else cCall "_mm_castsi128_pd" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm_movemask_pd" [pd])
      (8, 32) | x86Has256Reg fs (Proxy @a) -> do
        let pd = if simdElemTag (Proxy @a) == 9 then em else cCall "_mm256_castsi256_pd" [em]
        pure . RVal $ cCast "uint32_t" (cCall "_mm256_movemask_pd" [pd])
      _ -> do
        acc <- emitLocalVal (namedCType "uint32_t") (unsignedIntConst (0 :: Word32))
        let bytePtr = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
        forM_ [0 .. lanes - 1] $ \i -> do
          let msb = Index bytePtr (signedIntConst (i * sz + (sz - 1))) noLoc
              bitVal = Cond (BinOp And msb (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (bit i :: Word32)) (unsignedIntConst (0 :: Word32)) noLoc
          emitStmt $ Exp (Just $ Assign acc OrAssign bitVal noLoc) noLoc
        pure (RVal acc)

  mpopcount m = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    RVal bits <- getJanusC (getLinuxX86_64C (mmovemask m))
    bvar <- emitLocalVal (namedCType "uint32_t") bits
    let e =
          if hasX86Feature fs X86_POPCNT
            then cCast "int32_t" (cCall "_mm_popcnt_u32" [bvar])
            else cCast "int32_t" (cCall "__builtin_popcount" [bvar])
    pure (RVal e)

  mfirstTrue m = LinuxX86_64C $ JanusC $ do
    RVal bits <- getJanusC (getLinuxX86_64C (mmovemask m))
    bvar <- emitLocalVal (namedCType "uint32_t") bits
    let e =
          Cond
            (BinOp Eq bvar (unsignedIntConst (0 :: Word32)) noLoc)
            (signedIntConst (-1 :: Int32))
            (cCast "int32_t" (cCall "__builtin_ctz" [bvar]))
            noLoc
    pure (RVal e)

  mallOf m =
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        full = if lanes == 32 then maxBound else (1 `shiftL` lanes) - 1
     in mmovemask m `eq` inject full
  manyOf m = mmovemask m `ne` inject 0
  mnoneOf m = mmovemask m `eq` inject 0

-- 3. ExpSIMDCmp
x86Cmp128 :: forall a. (SIMDElem a) => BinOp -> X86Features -> Exp -> Exp -> JanusCM Exp
x86Cmp128 cop fs a b = case simdElemTag (Proxy @a) of
  8 -> pure $ case cop of
    Eq -> cCall "_mm_cmpeq_ps" [a, b]
    Ne -> cCall "_mm_cmpneq_ps" [a, b]
    Lt -> cCall "_mm_cmplt_ps" [a, b]
    Le -> cCall "_mm_cmple_ps" [a, b]
    Gt -> cCall "_mm_cmpgt_ps" [a, b]
    _ -> cCall "_mm_cmpge_ps" [a, b]
  9 -> pure $ case cop of
    Eq -> cCall "_mm_cmpeq_pd" [a, b]
    Ne -> cCall "_mm_cmpneq_pd" [a, b]
    Lt -> cCall "_mm_cmplt_pd" [a, b]
    Le -> cCall "_mm_cmple_pd" [a, b]
    Gt -> cCall "_mm_cmpgt_pd" [a, b]
    _ -> cCall "_mm_cmpge_pd" [a, b]
  tag -> do
    let ones = cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)]
        notI x = cCall "_mm_xor_si128" [x, ones]
        eqI x y = case tag of
          0 -> cCall "_mm_cmpeq_epi8" [x, y]
          1 -> cCall "_mm_cmpeq_epi8" [x, y]
          2 -> cCall "_mm_cmpeq_epi16" [x, y]
          3 -> cCall "_mm_cmpeq_epi16" [x, y]
          4 -> cCall "_mm_cmpeq_epi32" [x, y]
          5 -> cCall "_mm_cmpeq_epi32" [x, y]
          _
            | hasX86Feature fs X86_SSE4_1 -> cCall "_mm_cmpeq_epi64" [x, y]
            | otherwise ->
                let c32 = cCall "_mm_cmpeq_epi32" [x, y]
                 in cCall "_mm_and_si128" [c32, cCall "_mm_shuffle_epi32" [c32, cInt 0xB1]]
        bias = case tag of
          1 -> Just (cCall "_mm_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))])
          3 -> Just (cCall "_mm_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))])
          5 -> Just (cCall "_mm_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))])
          7 -> Just (cCall "_mm_set1_epi64x" [cCast "long long" (unsignedIntConst (0x8000000000000000 :: Word64))])
          _ -> Nothing
        (ax, bx) = case bias of
          Just bs -> (cCall "_mm_xor_si128" [a, bs], cCall "_mm_xor_si128" [b, bs])
          Nothing -> (a, b)
        gtSigned x y = case tag of
          0 -> pure $ cCall "_mm_cmpgt_epi8" [x, y]
          1 -> pure $ cCall "_mm_cmpgt_epi8" [x, y]
          2 -> pure $ cCall "_mm_cmpgt_epi16" [x, y]
          3 -> pure $ cCall "_mm_cmpgt_epi16" [x, y]
          4 -> pure $ cCall "_mm_cmpgt_epi32" [x, y]
          5 -> pure $ cCall "_mm_cmpgt_epi32" [x, y]
          _
            | hasX86Feature fs X86_SSE4_2 -> pure $ cCall "_mm_cmpgt_epi64" [x, y]
            | otherwise -> do
                -- SSE2 64-bit signed > via 32-bit limbs
                let b64 = cCall "_mm_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))]
                xu <- emitLocalVal (namedCType "__m128i") (cCall "_mm_xor_si128" [x, b64])
                yu <- emitLocalVal (namedCType "__m128i") (cCall "_mm_xor_si128" [y, b64])
                gt32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpgt_epi32" [x, y])
                gtu32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpgt_epi32" [xu, yu])
                eq32 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_cmpeq_epi32" [x, y])
                let hiGt = cCall "_mm_shuffle_epi32" [gt32, cInt 0xF5]
                    hiEq = cCall "_mm_shuffle_epi32" [eq32, cInt 0xF5]
                    loGt = cCall "_mm_shuffle_epi32" [gtu32, cInt 0xA0]
                pure $ cCall "_mm_or_si128" [hiGt, cCall "_mm_and_si128" [hiEq, loGt]]
    case cop of
      Eq -> pure (eqI a b)
      Ne -> pure (notI (eqI a b))
      Gt -> gtSigned ax bx
      Lt -> gtSigned bx ax
      Ge -> notI <$> gtSigned bx ax
      _ -> notI <$> gtSigned ax bx

x86Cmp256 :: forall a. (SIMDElem a) => BinOp -> X86Features -> Exp -> Exp -> JanusCM Exp
x86Cmp256 cop _ a b = case simdElemTag (Proxy @a) of
  8 ->
    let imm = case cop of
          Eq -> "_CMP_EQ_OQ"
          Ne -> "_CMP_NEQ_UQ"
          Lt -> "_CMP_LT_OQ"
          Le -> "_CMP_LE_OQ"
          Gt -> "_CMP_GT_OQ"
          _ -> "_CMP_GE_OQ"
     in pure $ cCall "_mm256_cmp_ps" [a, b, cVar imm]
  9 ->
    let imm = case cop of
          Eq -> "_CMP_EQ_OQ"
          Ne -> "_CMP_NEQ_UQ"
          Lt -> "_CMP_LT_OQ"
          Le -> "_CMP_LE_OQ"
          Gt -> "_CMP_GT_OQ"
          _ -> "_CMP_GE_OQ"
     in pure $ cCall "_mm256_cmp_pd" [a, b, cVar imm]
  tag -> do
    let ones = cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]
        notI x = cCall "_mm256_xor_si256" [x, ones]
        eqI x y = case tag of
          0 -> cCall "_mm256_cmpeq_epi8" [x, y]
          1 -> cCall "_mm256_cmpeq_epi8" [x, y]
          2 -> cCall "_mm256_cmpeq_epi16" [x, y]
          3 -> cCall "_mm256_cmpeq_epi16" [x, y]
          4 -> cCall "_mm256_cmpeq_epi32" [x, y]
          5 -> cCall "_mm256_cmpeq_epi32" [x, y]
          _ -> cCall "_mm256_cmpeq_epi64" [x, y]
        bias = case tag of
          1 -> Just (cCall "_mm256_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))])
          3 -> Just (cCall "_mm256_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))])
          5 -> Just (cCall "_mm256_set1_epi32" [cCast "int" (unsignedIntConst (0x80000000 :: Word32))])
          7 -> Just (cCall "_mm256_set1_epi64x" [cCast "long long" (unsignedIntConst (0x8000000000000000 :: Word64))])
          _ -> Nothing
        (ax, bx) = case bias of
          Just bs -> (cCall "_mm256_xor_si256" [a, bs], cCall "_mm256_xor_si256" [b, bs])
          Nothing -> (a, b)
        gtSigned x y = case tag of
          0 -> cCall "_mm256_cmpgt_epi8" [x, y]
          1 -> cCall "_mm256_cmpgt_epi8" [x, y]
          2 -> cCall "_mm256_cmpgt_epi16" [x, y]
          3 -> cCall "_mm256_cmpgt_epi16" [x, y]
          4 -> cCall "_mm256_cmpgt_epi32" [x, y]
          5 -> cCall "_mm256_cmpgt_epi32" [x, y]
          _ -> cCall "_mm256_cmpgt_epi64" [x, y]
    pure $ case cop of
      Eq -> eqI a b
      Ne -> notI (eqI a b)
      Gt -> gtSigned ax bx
      Lt -> gtSigned bx ax
      Ge -> notI (gtSigned bx ax)
      _ -> notI (gtSigned ax bx)

instance (SIMDShape n a) => ExpSIMDCmp LinuxX86_64C n a where
  veq a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Eq) (Just (x86Cmp256 @a Eq)) a b)
  vne a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Ne) (Just (x86Cmp256 @a Ne)) a b)
  vlt a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Lt) (Just (x86Cmp256 @a Lt)) a b)
  vle a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Le) (Just (x86Cmp256 @a Le)) a b)
  vgt a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Gt) (Just (x86Cmp256 @a Gt)) a b)
  vge a b = coerce (liftX86Bin @n @a (x86Cmp128 @a Ge) (Just (x86Cmp256 @a Ge)) a b)

-- 4. ExpSIMDArith
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDArith LinuxX86_64C n a where
  vadd = liftX86Bin @n @a (x86Add128 @a) (Just (x86Add256 @a))
  vsub = liftX86Bin @n @a (x86Sub128 @a) (Just (x86Sub256 @a))
  vmul = liftX86Bin @n @a (x86Mul128 @a) (Just (x86Mul256 @a))
  vneg v
    | simdElemIsFloat (Proxy @a) =
        let signMask128 _ x =
              pure $
                if simdElemTag (Proxy @a) == 8
                  then cCall "_mm_xor_ps" [x, cCall "_mm_set1_ps" [C.Const (FloatConst "-0.0f" (-0.0) noLoc) noLoc]]
                  else cCall "_mm_xor_pd" [x, cCall "_mm_set1_pd" [C.Const (DoubleConst "-0.0" (-0.0) noLoc) noLoc]]
            signMask256 _ x =
              pure $
                if simdElemTag (Proxy @a) == 8
                  then cCall "_mm256_xor_ps" [x, cCall "_mm256_set1_ps" [C.Const (FloatConst "-0.0f" (-0.0) noLoc) noLoc]]
                  else cCall "_mm256_xor_pd" [x, cCall "_mm256_set1_pd" [C.Const (DoubleConst "-0.0" (-0.0) noLoc) noLoc]]
         in liftX86Un @n @a signMask128 (Just signMask256) v
    | otherwise = vsub vzero v

  vabs v
    | not (simdElemSigned (Proxy @a)) = v
    | simdElemTag (Proxy @a) == 8 =
        liftX86Un @n @a
          (\_ x -> pure $ cCall "_mm_and_ps" [x, cCall "_mm_castsi128_ps" [cCall "_mm_set1_epi32" [cInt 0x7FFFFFFF]]])
          (Just (\_ x -> pure $ cCall "_mm256_and_ps" [x, cCall "_mm256_castsi256_ps" [cCall "_mm256_set1_epi32" [cInt 0x7FFFFFFF]]]))
          v
    | simdElemTag (Proxy @a) == 9 =
        liftX86Un @n @a
          (\_ x -> pure $ cCall "_mm_and_pd" [x, cCall "_mm_castsi128_pd" [cCall "_mm_set1_epi64x" [signedIntConst (0x7FFFFFFFFFFFFFFF :: Int64)]]])
          (Just (\_ x -> pure $ cCall "_mm256_and_pd" [x, cCall "_mm256_castsi256_pd" [cCall "_mm256_set1_epi64x" [signedIntConst (0x7FFFFFFFFFFFFFFF :: Int64)]]]))
          v
    | otherwise =
        let abs128 fs x
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == 0 = pure $ cCall "_mm_abs_epi8" [x]
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == 2 = pure $ cCall "_mm_abs_epi16" [x]
              | hasX86Feature fs X86_SSSE3 && simdElemTag (Proxy @a) == 4 = pure $ cCall "_mm_abs_epi32" [x]
              | otherwise = do
                  negX <- x86Sub128 @a fs (x86Zero128 @a) x
                  m <- x86Cmp128 @a Lt fs x (x86Zero128 @a)
                  pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, negX], cCall "_mm_andnot_si128" [m, x]]
            abs256 _ x = case simdElemTag (Proxy @a) of
              0 -> pure $ cCall "_mm256_abs_epi8" [x]
              2 -> pure $ cCall "_mm256_abs_epi16" [x]
              4 -> pure $ cCall "_mm256_abs_epi32" [x]
              _ -> do
                lo <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_castsi256_si128" [x])
                hi <- emitLocalVal (namedCType "__m128i") (cCall "_mm256_extracti128_si256" [x, cInt 1])
                rlo <- abs128 x86AVX2 lo
                rhi <- abs128 x86AVX2 hi
                pure $ cCall "_mm256_set_m128i" [rhi, rlo]
         in liftX86Un @n @a abs128 (Just abs256) v

  vmin a b =
    let min128 fs x y = case simdElemTag (Proxy @a) of
          8 -> pure $ cCall "_mm_min_ps" [x, y]
          9 -> pure $ cCall "_mm_min_pd" [x, y]
          1 -> pure $ cCall "_mm_min_epu8" [x, y]
          2 -> pure $ cCall "_mm_min_epi16" [x, y]
          0 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epi8" [x, y]
          3 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epu16" [x, y]
          4 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epi32" [x, y]
          5 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_min_epu32" [x, y]
          _ -> do
            m <- x86Cmp128 @a Lt fs x y
            pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, x], cCall "_mm_andnot_si128" [m, y]]
        min256 _ x y = case simdElemTag (Proxy @a) of
          8 -> pure $ cCall "_mm256_min_ps" [x, y]
          9 -> pure $ cCall "_mm256_min_pd" [x, y]
          0 -> pure $ cCall "_mm256_min_epi8" [x, y]
          1 -> pure $ cCall "_mm256_min_epu8" [x, y]
          2 -> pure $ cCall "_mm256_min_epi16" [x, y]
          3 -> pure $ cCall "_mm256_min_epu16" [x, y]
          4 -> pure $ cCall "_mm256_min_epi32" [x, y]
          5 -> pure $ cCall "_mm256_min_epu32" [x, y]
          _ -> do
            lo <- min128 x86AVX2 (cCall "_mm256_castsi256_si128" [x]) (cCall "_mm256_castsi256_si128" [y])
            hi <- min128 x86AVX2 (cCall "_mm256_extracti128_si256" [x, cInt 1]) (cCall "_mm256_extracti128_si256" [y, cInt 1])
            pure $ cCall "_mm256_set_m128i" [hi, lo]
     in liftX86Bin @n @a min128 (Just min256) a b

  vmax a b =
    let max128 fs x y = case simdElemTag (Proxy @a) of
          8 -> pure $ cCall "_mm_max_ps" [x, y]
          9 -> pure $ cCall "_mm_max_pd" [x, y]
          1 -> pure $ cCall "_mm_max_epu8" [x, y]
          2 -> pure $ cCall "_mm_max_epi16" [x, y]
          0 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epi8" [x, y]
          3 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epu16" [x, y]
          4 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epi32" [x, y]
          5 | hasX86Feature fs X86_SSE4_1 -> pure $ cCall "_mm_max_epu32" [x, y]
          _ -> do
            m <- x86Cmp128 @a Gt fs x y
            pure $ cCall "_mm_or_si128" [cCall "_mm_and_si128" [m, x], cCall "_mm_andnot_si128" [m, y]]
        max256 _ x y = case simdElemTag (Proxy @a) of
          8 -> pure $ cCall "_mm256_max_ps" [x, y]
          9 -> pure $ cCall "_mm256_max_pd" [x, y]
          0 -> pure $ cCall "_mm256_max_epi8" [x, y]
          1 -> pure $ cCall "_mm256_max_epu8" [x, y]
          2 -> pure $ cCall "_mm256_max_epi16" [x, y]
          3 -> pure $ cCall "_mm256_max_epu16" [x, y]
          4 -> pure $ cCall "_mm256_max_epi32" [x, y]
          5 -> pure $ cCall "_mm256_max_epu32" [x, y]
          _ -> do
            lo <- max128 x86AVX2 (cCall "_mm256_castsi256_si128" [x]) (cCall "_mm256_castsi256_si128" [y])
            hi <- max128 x86AVX2 (cCall "_mm256_extracti128_si256" [x, cInt 1]) (cCall "_mm256_extracti128_si256" [y, cInt 1])
            pure $ cCall "_mm256_set_m128i" [hi, lo]
     in liftX86Bin @n @a max128 (Just max256) a b

-- 5. ExpSIMDSat
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDSat LinuxX86_64C n a where
  vadds a b
    | simdElemIsFloat (Proxy @a) = vadd a b
    | simdElemTag (Proxy @a) `elem` [0, 1, 2, 3] =
        let adds128 _ x y = pure $ case simdElemTag (Proxy @a) of
              0 -> cCall "_mm_adds_epi8" [x, y]
              1 -> cCall "_mm_adds_epu8" [x, y]
              2 -> cCall "_mm_adds_epi16" [x, y]
              _ -> cCall "_mm_adds_epu16" [x, y]
            adds256 _ x y = pure $ case simdElemTag (Proxy @a) of
              0 -> cCall "_mm256_adds_epi8" [x, y]
              1 -> cCall "_mm256_adds_epu8" [x, y]
              2 -> cCall "_mm256_adds_epi16" [x, y]
              _ -> cCall "_mm256_adds_epu16" [x, y]
         in liftX86Bin @n @a adds128 (Just adds256) a b
    | not (simdElemSigned (Proxy @a)) =
        -- Unsigned saturating add: sum = x + y; sum < x ? ~0 : sum
        LinuxX86_64C $ JanusC $ do
          RVal ea <- getJanusC (getLinuxX86_64C a)
          RVal eb <- getJanusC (getLinuxX86_64C b)
          RVal <$> emitLaneLoopBin @n @a (\x y -> Cond (BinOp Lt (BinOp Add x y noLoc) x noLoc) (UnOp Not (cInt 0) noLoc) (BinOp Add x y noLoc) noLoc) ea eb
    | otherwise =
        LinuxX86_64C $ JanusC $ do
          RVal ea <- getJanusC (getLinuxX86_64C a)
          RVal eb <- getJanusC (getLinuxX86_64C b)
          let bits = simdElemBytes (Proxy @a) * 8
              mx = if bits == 64 then cVar "INT64_MAX" else signedIntConst ((1 `shiftL` (bits - 1)) - 1 :: Int64)
              mn = if bits == 64 then cVar "INT64_MIN" else signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64)
              body x y =
                let s = BinOp Add (cCast "__int128" x) (cCast "__int128" y) noLoc
                 in cCast (simdElemCTypeName (Proxy @a)) (Cond (BinOp Gt s (cCast "__int128" mx) noLoc) (cCast "__int128" mx) (Cond (BinOp Lt s (cCast "__int128" mn) noLoc) (cCast "__int128" mn) s noLoc) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vsubs a b
    | simdElemIsFloat (Proxy @a) = vsub a b
    | simdElemTag (Proxy @a) `elem` [0, 1, 2, 3] =
        let subs128 _ x y = pure $ case simdElemTag (Proxy @a) of
              0 -> cCall "_mm_subs_epi8" [x, y]
              1 -> cCall "_mm_subs_epu8" [x, y]
              2 -> cCall "_mm_subs_epi16" [x, y]
              _ -> cCall "_mm_subs_epu16" [x, y]
            subs256 _ x y = pure $ case simdElemTag (Proxy @a) of
              0 -> cCall "_mm256_subs_epi8" [x, y]
              1 -> cCall "_mm256_subs_epu8" [x, y]
              2 -> cCall "_mm256_subs_epi16" [x, y]
              _ -> cCall "_mm256_subs_epu16" [x, y]
         in liftX86Bin @n @a subs128 (Just subs256) a b
    | not (simdElemSigned (Proxy @a)) = vsub (vmax a b) b
    | otherwise =
        LinuxX86_64C $ JanusC $ do
          RVal ea <- getJanusC (getLinuxX86_64C a)
          RVal eb <- getJanusC (getLinuxX86_64C b)
          let bits = simdElemBytes (Proxy @a) * 8
              mx = if bits == 64 then cVar "INT64_MAX" else signedIntConst ((1 `shiftL` (bits - 1)) - 1 :: Int64)
              mn = if bits == 64 then cVar "INT64_MIN" else signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64)
              body x y =
                let d = BinOp Sub (cCast "__int128" x) (cCast "__int128" y) noLoc
                 in cCast (simdElemCTypeName (Proxy @a)) (Cond (BinOp Gt d (cCast "__int128" mx) noLoc) (cCast "__int128" mx) (Cond (BinOp Lt d (cCast "__int128" mn) noLoc) (cCast "__int128" mn) d noLoc) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vabss v
    | not (simdElemSigned (Proxy @a)) || simdElemIsFloat (Proxy @a) = vabs v
    | otherwise = LinuxX86_64C $ JanusC $ do
        RVal ev <- getJanusC (getLinuxX86_64C v)
        let bits = simdElemBytes (Proxy @a) * 8
            sminE = if bits == 64 then cVar "INT64_MIN" else cCast (simdElemCTypeName (Proxy @a)) (signedIntConst (-(1 `shiftL` (bits - 1)) :: Int64))
            smaxE = if bits == 64 then cVar "INT64_MAX" else cCast (simdElemCTypeName (Proxy @a)) (signedIntConst (((1 `shiftL` (bits - 1)) - 1) :: Int64))
            absBody x = Cond (BinOp Eq x sminE noLoc) smaxE (Cond (BinOp Lt x (cInt 0) noLoc) (UnOp Negate x noLoc) x noLoc) noLoc
        RVal <$> emitLaneLoopUn @n @a absBody ev

  vavg a b
    | simdElemIsFloat (Proxy @a) =
        let isF32 = simdElemTag (Proxy @a) == 8
            halfLit = if isF32 then C.Const (FloatConst "0.5f" 0.5 noLoc) noLoc else C.Const (DoubleConst "0.5" 0.5 noLoc) noLoc
            avg128 _ x y =
              pure $
                if isF32
                  then cCall "_mm_mul_ps" [cCall "_mm_set1_ps" [halfLit], cCall "_mm_add_ps" [x, y]]
                  else cCall "_mm_mul_pd" [cCall "_mm_set1_pd" [halfLit], cCall "_mm_add_pd" [x, y]]
            avg256 _ x y =
              pure $
                if isF32
                  then cCall "_mm256_mul_ps" [cCall "_mm256_set1_ps" [halfLit], cCall "_mm256_add_ps" [x, y]]
                  else cCall "_mm256_mul_pd" [cCall "_mm256_set1_pd" [halfLit], cCall "_mm256_add_pd" [x, y]]
         in liftX86Bin @n @a avg128 (Just avg256) a b
    | simdElemTag (Proxy @a) == 1 =
        liftX86Bin @n @a (\_ x y -> pure $ cCall "_mm_avg_epu8" [x, y]) (Just (\_ x y -> pure $ cCall "_mm256_avg_epu8" [x, y])) a b
    | simdElemTag (Proxy @a) == 3 =
        liftX86Bin @n @a (\_ x y -> pure $ cCall "_mm_avg_epu16" [x, y]) (Just (\_ x y -> pure $ cCall "_mm256_avg_epu16" [x, y])) a b
    | simdElemTag (Proxy @a) == 0 =
        let avgI8_128 _ x y = do
              let bs = cCall "_mm_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))]
              pure $ cCall "_mm_xor_si128" [cCall "_mm_avg_epu8" [cCall "_mm_xor_si128" [x, bs], cCall "_mm_xor_si128" [y, bs]], bs]
            avgI8_256 _ x y = do
              let bs = cCall "_mm256_set1_epi8" [cCast "char" (unsignedIntConst (0x80 :: Word32))]
              pure $ cCall "_mm256_xor_si256" [cCall "_mm256_avg_epu8" [cCall "_mm256_xor_si256" [x, bs], cCall "_mm256_xor_si256" [y, bs]], bs]
         in liftX86Bin @n @a avgI8_128 (Just avgI8_256) a b
    | simdElemTag (Proxy @a) == 2 =
        let avgI16_128 _ x y = do
              let bs = cCall "_mm_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))]
              pure $ cCall "_mm_xor_si128" [cCall "_mm_avg_epu16" [cCall "_mm_xor_si128" [x, bs], cCall "_mm_xor_si128" [y, bs]], bs]
            avgI16_256 _ x y = do
              let bs = cCall "_mm256_set1_epi16" [cCast "short" (unsignedIntConst (0x8000 :: Word32))]
              pure $ cCall "_mm256_xor_si256" [cCall "_mm256_avg_epu16" [cCall "_mm256_xor_si256" [x, bs], cCall "_mm256_xor_si256" [y, bs]], bs]
         in liftX86Bin @n @a avgI16_128 (Just avgI16_256) a b
    | otherwise =
        LinuxX86_64C $ JanusC $ do
          RVal ea <- getJanusC (getLinuxX86_64C a)
          RVal eb <- getJanusC (getLinuxX86_64C b)
          let wideTy = if simdElemSigned (Proxy @a) then "__int128" else "unsigned __int128"
              body x y = cCast (simdElemCTypeName (Proxy @a)) (BinOp Rsh (BinOp Add (BinOp Add (cCast wideTy x) (cCast wideTy y) noLoc) (cInt 1) noLoc) (cInt 1) noLoc)
          RVal <$> emitLaneLoopBin @n @a body ea eb

  vabsdiff a b
    | simdElemIsFloat (Proxy @a) = vabs (vsub a b)
    | simdElemTag (Proxy @a) == 1 = vor (vsubs a b) (vsubs b a)
    | simdElemTag (Proxy @a) == 3 = vor (vsubs a b) (vsubs b a)
    | otherwise = vsub (vmax a b) (vmin a b)

-- 6. ExpSIMDFloat
x86FmaOp ::
  forall n a.
  (SIMDShape n a, CPUSIMDElem a) =>
  String ->
  String ->
  (Exp -> Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
x86FmaOp fn128 fn256 scalarFallback (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
  LinuxX86_64C $ JanusC $ do
    addHeader "math.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    fs <- askX86Features
    RVal ea0 <- ma
    RVal eb0 <- mb
    RVal ec0 <- mc
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    ec <- emitLocalVal ty ec0
    let b = simdBytes (Proxy @n) (Proxy @a)
    if hasX86Feature fs X86_FMA
      then case b of
        32 | x86Has256Reg fs (Proxy @a) -> RVal <$> emitLocalVal ty (cCall fn256 [ea, eb, ec])
        32 -> do
          rlo <- fromXMM128 @a 16 (cCall fn128 [cMember ea "lo", cMember eb "lo", cMember ec "lo"])
          rhi <- fromXMM128 @a 16 (cCall fn128 [cMember ea "hi", cMember eb "hi", cMember ec "hi"])
          RVal <$> emitPairStruct ty rlo rhi
        _ -> do
          xa <- toXMM128 @a b ea
          xb <- toXMM128 @a b eb
          xc <- toXMM128 @a b ec
          RVal <$> fromXMM128 @a b (cCall fn128 [xa, xb, xc])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            elemTyStr = simdElemCTypeName (Proxy @a)
            JCType espec _ = namedCType elemTyStr
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
            arrTy = JCType espec arrDec
        aa <- emitUninitLocal arrTy
        ab <- emitUninitLocal arrTy
        ac <- emitUninitLocal arrTy
        ar <- emitUninitLocal arrTy
        resVar <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst b]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst b]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ac, UnOp AddrOf ec noLoc, signedIntConst b]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
          emitStmt $ Exp (Just $ Assign (Index ar idx noLoc) JustAssign (scalarFallback (Index aa idx noLoc) (Index ab idx noLoc) (Index ac idx noLoc)) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, ar, signedIntConst b]) noLoc
        pure (RVal resVar)

instance (SIMDShape n a, CPUSIMDElem a, Floating a, FMA a) => ExpSIMDFloat LinuxX86_64C n a where
  vdiv a b =
    let isF32 = simdElemTag (Proxy @a) == 8
        div128 _ x y
          | simdBytes (Proxy @n) (Proxy @a) < 16 = do
              -- Set unused upper lanes of divisor to 1.0f to remain safe even under FP exceptions
              let one = C.Const (FloatConst "1.0f" 1.0 noLoc) noLoc
                  paddedY = cCall "_mm_move_ss" [cCall "_mm_set1_ps" [one], y]
                  paddedY2 = cCall "_mm_shuffle_ps" [y, cCall "_mm_set1_ps" [one], cInt 0xE4]
              pure $ cCall "_mm_div_ps" [x, if simdBytes (Proxy @n) (Proxy @a) == 8 then paddedY2 else paddedY]
          | otherwise = pure $ cCall (if isF32 then "_mm_div_ps" else "_mm_div_pd") [x, y]
        div256 _ x y = pure $ cCall (if isF32 then "_mm256_div_ps" else "_mm256_div_pd") [x, y]
     in liftX86Bin @n @a div128 (Just div256) a b

  vsqrt =
    let isF32 = simdElemTag (Proxy @a) == 8
     in liftX86Un @n @a
          (\_ x -> pure $ cCall (if isF32 then "_mm_sqrt_ps" else "_mm_sqrt_pd") [x])
          (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_sqrt_ps" else "_mm256_sqrt_pd") [x]))

  vrsqrtApprox v
    | simdElemTag (Proxy @a) == 8 =
        liftX86Un @n @a (\_ x -> pure $ cCall "_mm_rsqrt_ps" [x]) (Just (\_ x -> pure $ cCall "_mm256_rsqrt_ps" [x])) v
    | otherwise =
        let oneD = C.Const (DoubleConst "1.0" 1.0 noLoc) noLoc
         in liftX86Un @n @a (\_ x -> pure $ cCall "_mm_div_pd" [cCall "_mm_set1_pd" [oneD], cCall "_mm_sqrt_pd" [x]]) (Just (\_ x -> pure $ cCall "_mm256_div_pd" [cCall "_mm256_set1_pd" [oneD], cCall "_mm256_sqrt_pd" [x]])) v

  vrcpApprox v
    | simdElemTag (Proxy @a) == 8 =
        liftX86Un @n @a (\_ x -> pure $ cCall "_mm_rcp_ps" [x]) (Just (\_ x -> pure $ cCall "_mm256_rcp_ps" [x])) v
    | otherwise =
        let oneD = C.Const (DoubleConst "1.0" 1.0 noLoc) noLoc
         in liftX86Un @n @a (\_ x -> pure $ cCall "_mm_div_pd" [cCall "_mm_set1_pd" [oneD], x]) (Just (\_ x -> pure $ cCall "_mm256_div_pd" [cCall "_mm256_set1_pd" [oneD], x])) v

  vfma =
    let isF32 = simdElemTag (Proxy @a) == 8
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fmadd_ps" else "_mm_fmadd_pd") (if isF32 then "_mm256_fmadd_ps" else "_mm256_fmadd_pd") (\x y z -> cCall cFma [x, y, z])

  vfms =
    let isF32 = simdElemTag (Proxy @a) == 8
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fmsub_ps" else "_mm_fmsub_pd") (if isF32 then "_mm256_fmsub_ps" else "_mm256_fmsub_pd") (\x y z -> cCall cFma [x, y, UnOp Negate z noLoc])

  vfnma =
    let isF32 = simdElemTag (Proxy @a) == 8
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fnmadd_ps" else "_mm_fnmadd_pd") (if isF32 then "_mm256_fnmadd_ps" else "_mm256_fnmadd_pd") (\x y z -> cCall cFma [UnOp Negate x noLoc, y, z])

  vfnms =
    let isF32 = simdElemTag (Proxy @a) == 8
        cFma = if isF32 then "fmaf" else "fma"
     in x86FmaOp @n @a (if isF32 then "_mm_fnmsub_ps" else "_mm_fnmsub_pd") (if isF32 then "_mm256_fnmsub_ps" else "_mm256_fnmsub_pd") (\x y z -> cCall cFma [UnOp Negate x noLoc, y, UnOp Negate z noLoc])

  vmulAdd a b c =
    LinuxX86_64C $ JanusC $ do
      fs <- askX86Features
      getJanusC $ getLinuxX86_64C $ if hasX86Feature fs X86_FMA then vfma a b c else vadd (vmul a b) c

  vfloor (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == 8
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
          getLinuxX86_64C $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_floor_ps" else "_mm_floor_pd") [x])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_floor_ps" else "_mm256_floor_pd") [x]))
              (LinuxX86_64C (JanusC mv))
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "floorf" else "floor") [x]) e

  vceil (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == 8
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
          getLinuxX86_64C $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_ceil_ps" else "_mm_ceil_pd") [x])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_ceil_ps" else "_mm256_ceil_pd") [x]))
              (LinuxX86_64C (JanusC mv))
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "ceilf" else "ceil") [x]) e

  vtrunc (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == 8
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
          getLinuxX86_64C $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_round_ps" else "_mm_round_pd") [x, cInt 11])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_round_ps" else "_mm256_round_pd") [x, cInt 11]))
              (LinuxX86_64C (JanusC mv))
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "truncf" else "trunc") [x]) e

  vroundNearest (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let isF32 = simdElemTag (Proxy @a) == 8
    if hasX86Feature fs X86_SSE4_1
      then
        getJanusC $
          getLinuxX86_64C $
            liftX86Un @n @a
              (\_ x -> pure $ cCall (if isF32 then "_mm_round_ps" else "_mm_round_pd") [x, cInt 8])
              (Just (\_ x -> pure $ cCall (if isF32 then "_mm256_round_ps" else "_mm256_round_pd") [x, cInt 8]))
              (LinuxX86_64C (JanusC mv))
      else mv >>= \(RVal e) -> RVal <$> emitLaneLoopUn @n @a (\x -> cCall (if isF32 then "nearbyintf" else "nearbyint") [x]) e

-- 7. ExpSIMDBits
bitBinX86 ::
  forall n a.
  (SIMDShape n a) =>
  String ->
  String ->
  String ->
  String ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a)
bitBinX86 fn128 fn256ps fn256pd fn256si =
  let pa = Proxy @a
   in liftX86Bin @n @a
        (\_ x y -> pure $ fromM128i pa (cCall fn128 [asM128i pa x, asM128i pa y]))
        ( Just
            ( \_ x y -> pure $ case simdElemTag pa of
                8 -> cCall fn256ps [x, y]
                9 -> cCall fn256pd [x, y]
                _ -> cCall fn256si [x, y]
            )
        )

instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDBits LinuxX86_64C n a where
  vand = bitBinX86 "_mm_and_si128" "_mm256_and_ps" "_mm256_and_pd" "_mm256_and_si256"
  vor = bitBinX86 "_mm_or_si128" "_mm256_or_ps" "_mm256_or_pd" "_mm256_or_si256"
  vxor = bitBinX86 "_mm_xor_si128" "_mm256_xor_ps" "_mm256_xor_pd" "_mm256_xor_si256"
  vandnot = bitBinX86 "_mm_andnot_si128" "_mm256_andnot_ps" "_mm256_andnot_pd" "_mm256_andnot_si256"
  vnot v =
    let pa = Proxy @a
        ones128 _ x = pure $ fromM128i pa (cCall "_mm_xor_si128" [asM128i pa x, cCall "_mm_set1_epi32" [signedIntConst (-1 :: Int32)]])
        ones256 _ x = pure $ case simdElemTag pa of
          8 -> cCall "_mm256_xor_ps" [x, cCall "_mm256_castsi256_ps" [cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]]
          9 -> cCall "_mm256_xor_pd" [x, cCall "_mm256_castsi256_pd" [cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]]
          _ -> cCall "_mm256_xor_si256" [x, cCall "_mm256_set1_epi32" [signedIntConst (-1 :: Int32)]]
     in liftX86Un @n @a ones128 (Just ones256) v

  vshl (LinuxX86_64C (JanusC mv)) (LinuxX86_64C (JanusC mc)) = LinuxX86_64C $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        body x =
          let ux = if simdElemIsFloat (Proxy @a) then error "vshl float" else cCast uTy x
           in cCast (simdElemCTypeName (Proxy @a)) $
                Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh ux c noLoc) noLoc
    if simdElemIsFloat (Proxy @a)
      then do
        -- Bitwise shift on raw integer lanes of equal width via union/memcpy loop
        addHeader "string.h"
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = namedCType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- emitUninitLocal (JCType uspec arrDec)
        resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
        evLoc <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` ev)
        emitStmt $ Exp (Just $ cCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              shifted = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (Index arrU idx noLoc) c noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign shifted noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal resVar)
      else RVal <$> emitLaneLoopUn @n @a body ev

  vshr (LinuxX86_64C (JanusC mv)) (LinuxX86_64C (JanusC mc)) = LinuxX86_64C $ JanusC $ do
    RVal ev <- mv
    RVal ec0 <- mc
    c <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" ec0)
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        sTy = "int" <> show bits <> "_t"
    if simdElemIsFloat (Proxy @a)
      then do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            bytes = simdBytes (Proxy @n) (Proxy @a)
            JCType uspec _ = namedCType uTy
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrU <- emitUninitLocal (JCType uspec arrDec)
        resVar <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= emitUninitLocal
        evLoc <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a)) >>= (`emitLocalVal` ev)
        emitStmt $ Exp (Just $ cCall "memcpy" [arrU, UnOp AddrOf evLoc noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let idx = signedIntConst i
              shifted = Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (Index arrU idx noLoc) c noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index arrU idx noLoc) JustAssign shifted noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf resVar noLoc, arrU, signedIntConst bytes]) noLoc
        pure (RVal resVar)
      else do
        let body x
              | simdElemSigned (Proxy @a) =
                  let sx = cCast sTy x
                      fill = Cond (BinOp Lt sx (cInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cInt 0) noLoc
                   in cCast (simdElemCTypeName (Proxy @a)) $
                        Cond (BinOp Ge c (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx c noLoc) noLoc
              | otherwise =
                  let ux = cCast uTy x
                   in cCast (simdElemCTypeName (Proxy @a)) $
                        Cond (BinOp Ge c (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh ux c noLoc) noLoc
        RVal <$> emitLaneLoopUn @n @a body ev

  vshlImm p v = vshl v (fromIntegral (natVal p))
  vshrImm p v = vshr v (fromIntegral (natVal p))

  vshlVar a b = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_AVX2 && tag `elem` [4, 5, 6, 7]
      then do
        let sll128 = if tag `elem` [4, 5] then "_mm_sllv_epi32" else "_mm_sllv_epi64"
            sll256 = if tag `elem` [4, 5] then "_mm256_sllv_epi32" else "_mm256_sllv_epi64"
        getJanusC $ getLinuxX86_64C $ liftX86Bin @n @a (\_ x y -> pure $ cCall sll128 [x, y]) (Just (\_ x y -> pure $ cCall sll256 [x, y])) a b
      else do
        RVal ea <- getJanusC (getLinuxX86_64C a)
        RVal eb <- getJanusC (getLinuxX86_64C b)
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            body x y =
              let uc = cCast uTy y
               in cCast (simdElemCTypeName (Proxy @a)) $
                    Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Lsh (cCast uTy x) uc noLoc) noLoc
        RVal <$> emitLaneLoopBin @n @a body ea eb

  vshrVar a b = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_AVX2 && tag `elem` [4, 5, 7]
      then do
        let srl128 = case tag of
              4 -> "_mm_srav_epi32"
              5 -> "_mm_srlv_epi32"
              _ -> "_mm_srlv_epi64"
            srl256 = case tag of
              4 -> "_mm256_srav_epi32"
              5 -> "_mm256_srlv_epi32"
              _ -> "_mm256_srlv_epi64"
        getJanusC $ getLinuxX86_64C $ liftX86Bin @n @a (\_ x y -> pure $ cCall srl128 [x, y]) (Just (\_ x y -> pure $ cCall srl256 [x, y])) a b
      else do
        RVal ea <- getJanusC (getLinuxX86_64C a)
        RVal eb <- getJanusC (getLinuxX86_64C b)
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            sTy = "int" <> show bits <> "_t"
            body x y
              | simdElemSigned (Proxy @a) =
                  let uc = cCast uTy y
                      sx = cCast sTy x
                      fill = Cond (BinOp Lt sx (cInt 0) noLoc) (signedIntConst (-1 :: Int32)) (cInt 0) noLoc
                   in cCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) fill (BinOp Rsh sx uc noLoc) noLoc
              | otherwise =
                  let uc = cCast uTy y
                   in cCast (simdElemCTypeName (Proxy @a)) $ Cond (BinOp Ge uc (unsignedIntConst bits) noLoc) (unsignedIntConst (0 :: Word64)) (BinOp Rsh (cCast uTy x) uc noLoc) noLoc
        RVal <$> emitLaneLoopBin @n @a body ea eb

  vpopcnt v = LinuxX86_64C $ JanusC $ do
    fs <- askX86Features
    let tag = simdElemTag (Proxy @a)
    if hasX86Feature fs X86_SSSE3 && tag `elem` [0, 1]
      then do
        let pop8_128 _ x = do
              lut <- emitLocalVal (namedCType "__m128i") (cCall "_mm_setr_epi8" (map signedIntConst ([0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4] :: [Int])))
              msk <- emitLocalVal (namedCType "__m128i") (cCall "_mm_set1_epi8" [cInt 0x0F])
              let lo = cCall "_mm_shuffle_epi8" [lut, cCall "_mm_and_si128" [x, msk]]
                  hi = cCall "_mm_shuffle_epi8" [lut, cCall "_mm_and_si128" [cCall "_mm_srli_epi16" [x, cInt 4], msk]]
              pure $ cCall "_mm_add_epi8" [lo, hi]
            pop8_256 _ x = do
              lut <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_setr_epi8" (map signedIntConst ([0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4] :: [Int])))
              msk <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_set1_epi8" [cInt 0x0F])
              let lo = cCall "_mm256_shuffle_epi8" [lut, cCall "_mm256_and_si256" [x, msk]]
                  hi = cCall "_mm256_shuffle_epi8" [lut, cCall "_mm256_and_si256" [cCall "_mm256_srli_epi16" [x, cInt 4], msk]]
              pure $ cCall "_mm256_add_epi8" [lo, hi]
        getJanusC $ getLinuxX86_64C $ liftX86Un @n @a pop8_128 (Just pop8_256) v
      else do
        RVal ev <- getJanusC (getLinuxX86_64C v)
        let bits = simdElemBytes (Proxy @a) * 8
            uTy = "uint" <> show bits <> "_t"
            body x = cCast (simdElemCTypeName (Proxy @a)) (cCall "__builtin_popcountll" [cCast "uint64_t" (cCast uTy x)])
        RVal <$> emitLaneLoopUn @n @a body ev

-- 8. ExpSIMDShuffle & ExpSIMDSplit
instance (SIMDShape n a, CPUSIMDElem a) => ExpSIMDShuffle LinuxX86_64C n a where
  vshuffleLanes idxs (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- mv
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        bytes = simdBytes (Proxy @n) (Proxy @a)
        JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        arrTy = JCType espec arrDec
    when (length idxs /= lanes) $ error "vshuffleLanes: index list length mismatch"
    srcArr <- emitUninitLocal arrTy
    dstArr <- emitUninitLocal arrTy
    res <- emitUninitLocal ty
    emitStmt $ Exp (Just $ cCall "memcpy" [srcArr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
    forM_ (zip [0 :: Int ..] idxs) $ \(outI, inI) -> do
      let masked = inI .&. (lanes - 1)
      emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst outI) noLoc) JustAssign (Index srcArr (signedIntConst masked) noLoc) noLoc) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst bytes]) noLoc
    pure (RVal res)

  vpermuteVar (LinuxX86_64C (JanusC mv)) (LinuxX86_64C (JanusC midx)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    fs <- askX86Features
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
    RVal ev0 <- mv
    RVal eidx0 <- midx
    ev <- emitLocalVal ty ev0
    eidx <- emitLocalVal ity eidx0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        bytes = lanes * sz
    if hasX86Feature fs X86_AVX2 && sz == 4 && lanes == 8
      then do
        let perm =
              if simdElemTag (Proxy @a) == 8
                then cCall "_mm256_permutevar8x32_ps" [ev, eidx]
                else cCall "_mm256_permutevar8x32_epi32" [ev, eidx]
        RVal <$> emitLocalVal ty perm
      else do
        let JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            JCType ispec _ = namedCType "int32_t"
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        srcArr <- emitUninitLocal (JCType espec arrDec)
        idxArr <- emitUninitLocal (JCType ispec arrDec)
        dstArr <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [srcArr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
        forM_ [0 .. lanes - 1] $ \i -> do
          let k = BinOp And (cCast "uint32_t" (Index idxArr (signedIntConst i) noLoc)) (unsignedIntConst (lanes - 1)) noLoc
          emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index srcArr k noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst bytes]) noLoc
        pure (RVal res)

  vshuffleBytes ::
    forall b.
    (SIMDShape b Word8) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C b Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n a)
  vshuffleBytes (LinuxX86_64C (JanusC mv)) (LinuxX86_64C (JanusC mc)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    fs <- askX86Features
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    cty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C b Word8))
    RVal ev0 <- mv
    RVal ec0 <- mc
    ev <- emitLocalVal ty ev0
    ec <- emitLocalVal cty ec0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
        pa = Proxy @a
    if bytes == 16 && hasX86Feature fs X86_SSSE3
      then RVal <$> emitLocalVal ty (fromM128i pa (cCall "_mm_shuffle_epi8" [asM128i pa ev, ec]))
      else do
        let JCType bspec _ = namedCType "uint8_t"
            arrDec = Array [] (ArraySize False (signedIntConst bytes) noLoc) (DeclRoot noLoc) noLoc
        sb <- emitUninitLocal (JCType bspec arrDec)
        cb <- emitUninitLocal (JCType bspec arrDec)
        db <- emitUninitLocal (JCType bspec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [sb, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [cb, UnOp AddrOf ec noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. bytes - 1] $ \i -> do
          let ci = Index cb (signedIntConst i) noLoc
              sel = Cond (BinOp And ci (unsignedIntConst (0x80 :: Word32)) noLoc) (unsignedIntConst (0 :: Word32)) (Index sb (BinOp And ci (unsignedIntConst (bytes - 1)) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index db (signedIntConst i) noLoc) JustAssign sel noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, db, signedIntConst bytes]) noLoc
        pure (RVal res)

  vzipLo (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 16
      then do
        let fn = case simdElemTag (Proxy @a) of
              0 -> "_mm_unpacklo_epi8"
              1 -> "_mm_unpacklo_epi8"
              2 -> "_mm_unpacklo_epi16"
              3 -> "_mm_unpacklo_epi16"
              4 -> "_mm_unpacklo_epi32"
              5 -> "_mm_unpacklo_epi32"
              6 -> "_mm_unpacklo_epi64"
              7 -> "_mm_unpacklo_epi64"
              8 -> "_mm_unpacklo_ps"
              _ -> "_mm_unpacklo_pd"
        RVal <$> emitLocalVal ty (cCall fn [ea, eb])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            half = lanes `div` 2
            JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        aa <- emitUninitLocal (JCType espec arrDec)
        ab <- emitUninitLocal (JCType espec arrDec)
        ar <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. half - 1] $ \i -> do
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst i) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
        pure (RVal res)

  vzipHi (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) = LinuxX86_64C $ JanusC $ do
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ea0 <- ma
    RVal eb0 <- mb
    ea <- emitLocalVal ty ea0
    eb <- emitLocalVal ty eb0
    let bytes = simdBytes (Proxy @n) (Proxy @a)
    if bytes == 16
      then do
        let fn = case simdElemTag (Proxy @a) of
              0 -> "_mm_unpackhi_epi8"
              1 -> "_mm_unpackhi_epi8"
              2 -> "_mm_unpackhi_epi16"
              3 -> "_mm_unpackhi_epi16"
              4 -> "_mm_unpackhi_epi32"
              5 -> "_mm_unpackhi_epi32"
              6 -> "_mm_unpackhi_epi64"
              7 -> "_mm_unpackhi_epi64"
              8 -> "_mm_unpackhi_ps"
              _ -> "_mm_unpackhi_pd"
        RVal <$> emitLocalVal ty (cCall fn [ea, eb])
      else do
        let lanes = simdLanes (Proxy @n) (Proxy @a)
            half = lanes `div` 2
            JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
            arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
        aa <- emitUninitLocal (JCType espec arrDec)
        ab <- emitUninitLocal (JCType espec arrDec)
        ar <- emitUninitLocal (JCType espec arrDec)
        res <- emitUninitLocal ty
        emitStmt $ Exp (Just $ cCall "memcpy" [aa, UnOp AddrOf ea noLoc, signedIntConst bytes]) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [ab, UnOp AddrOf eb noLoc, signedIntConst bytes]) noLoc
        forM_ [0 .. half - 1] $ \i -> do
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i)) noLoc) JustAssign (Index aa (signedIntConst (half + i)) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ Assign (Index ar (signedIntConst (2 * i + 1)) noLoc) JustAssign (Index ab (signedIntConst (half + i)) noLoc) noLoc) noLoc
        emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, ar, signedIntConst bytes]) noLoc
        pure (RVal res)

instance (SIMDHalfPair n2 n a) => ExpSIMDSplit LinuxX86_64C n2 n a where
  vsplit (LinuxX86_64C (JanusC mv)) =
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        extractPart offset = LinuxX86_64C $ JanusC $ do
          addHeader "string.h"
          ty2 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n2 a))
          ty1 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
          RVal ev0 <- mv
          ev <- emitLocalVal ty2 ev0
          out <- emitUninitLocal ty1
          let srcPtr = BinOp Add (cConstPtrCast "uint8_t" (UnOp AddrOf ev noLoc)) (signedIntConst offset) noLoc
          emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, srcPtr, signedIntConst halfB]) noLoc
          pure (RVal out)
     in (extractPart 0, extractPart halfB)

  vconcat (LinuxX86_64C (JanusC mlo)) (LinuxX86_64C (JanusC mhi)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty1 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    ty2 <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n2 a))
    RVal elo0 <- mlo
    RVal ehi0 <- mhi
    elo <- emitLocalVal ty1 elo0
    ehi <- emitLocalVal ty1 ehi0
    out <- emitUninitLocal ty2
    let halfB = simdBytes (Proxy @n) (Proxy @a)
        dstPtr = cPtrCast "uint8_t" (UnOp AddrOf out noLoc)
    emitStmt $ Exp (Just $ cCall "memcpy" [dstPtr, UnOp AddrOf elo noLoc, signedIntConst halfB]) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [BinOp Add dstPtr (signedIntConst halfB) noLoc, UnOp AddrOf ehi noLoc, signedIntConst halfB]) noLoc
    pure (RVal out)

-- 9. ExpSIMDReduce
emitX86Reduce ::
  forall n a.
  (SIMDShape n a) =>
  (Exp -> Exp -> Exp) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
  LinuxX86_64C a
emitX86Reduce combine (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
  addHeader "string.h"
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  RVal ev0 <- mv
  ev <- emitLocalVal ty ev0
  let lanes = simdLanes (Proxy @n) (Proxy @a)
      bytes = simdBytes (Proxy @n) (Proxy @a)
      elemName = simdElemCTypeName (Proxy @a)
      JCType espec _ = namedCType elemName
      arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
  arr <- emitUninitLocal (JCType espec arrDec)
  emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
  acc <- emitLocalVal (namedCType elemName) (Index arr (cInt 0) noLoc)
  forM_ [1 .. lanes - 1] $ \i ->
    emitStmt $ Exp (Just $ Assign acc JustAssign (combine acc (Index arr (signedIntConst i) noLoc)) noLoc) noLoc
  pure (RVal acc)

instance (SIMDShape n a, CPUSIMDElem a, JanusCTyped a) => ExpSIMDReduce LinuxX86_64C n a where
  vreduceAdd =
    let bits = simdElemBytes (Proxy @a) * 8
        uTy = "uint" <> show bits <> "_t"
        comb x y
          | simdElemIsFloat (Proxy @a) = BinOp Add x y noLoc
          | otherwise = cCast (simdElemCTypeName (Proxy @a)) (BinOp Add (cCast uTy x) (cCast uTy y) noLoc)
     in emitX86Reduce comb
  vreduceMin = emitX86Reduce (\x y -> Cond (BinOp Lt x y noLoc) x y noLoc)
  vreduceMax = emitX86Reduce (\x y -> Cond (BinOp Gt x y noLoc) x y noLoc)
  vreduceAnd v = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC (getLinuxX86_64C v)
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc AndAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceOr v = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC (getLinuxX86_64C v)
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc OrAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)
  vreduceXor v = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    RVal ev0 <- getJanusC (getLinuxX86_64C v)
    ev <- emitLocalVal ty ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sz = simdElemBytes (Proxy @a)
        uTy = "uint" <> show (sz * 8) <> "_t"
        JCType uspec _ = namedCType uTy
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    arr <- emitUninitLocal (JCType uspec arrDec)
    emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst (lanes * sz)]) noLoc
    acc <- emitLocalVal (namedCType uTy) (Index arr (cInt 0) noLoc)
    forM_ [1 .. lanes - 1] $ \i -> emitStmt $ Exp (Just $ Assign acc XorAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
    out <- emitUninitLocal (namedCType (simdElemCTypeName (Proxy @a)))
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf acc noLoc, signedIntConst sz]) noLoc
    pure (RVal out)

-- 10. ExpSIMDDot
emitX86Dp4aSWord :: Exp -> Exp -> Exp -> JanusCM Exp
emitX86Dp4aSWord ea eb eacc = do
  a <- emitLocalVal (namedCType "uint32_t") ea
  b <- emitLocalVal (namedCType "uint32_t") eb
  acc <- emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" eacc)
  let byteS u k = cCast "int32_t" (cCast "int8_t" (BinOp Rsh u (signedIntConst (k * 8)) noLoc))
      term k = cCast "uint32_t" (BinOp Mul (byteS a k) (byteS b k) noLoc)
      sum4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
  emitLocalVal (namedCType "int32_t") (cCast "int32_t" sum4)

emitX86Dp4aUWord :: Exp -> Exp -> Exp -> JanusCM Exp
emitX86Dp4aUWord ea eb eacc = do
  a <- emitLocalVal (namedCType "uint32_t") ea
  b <- emitLocalVal (namedCType "uint32_t") eb
  acc <- emitLocalVal (namedCType "uint32_t") eacc
  let byteU u k = BinOp And (BinOp Rsh u (signedIntConst (k * 8)) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
      term k = BinOp Mul (byteU a k) (byteU b k) noLoc
      sum4 = foldl (\s k -> BinOp Add s (term k) noLoc) acc [0 .. 3 :: Int]
  emitLocalVal (namedCType "uint32_t") sum4

instance ExpSIMDDot LinuxX86_64C where
  vdp4aS (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitX86Dp4aSWord a b c

  vdp4aU (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal a <- ma
      RVal b <- mb
      RVal c <- mc
      RVal <$> emitX86Dp4aUWord a b c

  vdp4aVecS ::
    forall m k.
    (SIMDDp4aShape m k) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C m Int8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m Int8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Int32) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Int32)
  vdp4aVecS (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      addHeader "string.h"
      tyA <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m Int8))
      tyK <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C k Int32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Int32)
          JCType u32spec _ = namedCType "uint32_t"
          JCType i32spec _ = namedCType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- emitUninitLocal (JCType u32spec arrDec)
      bw <- emitUninitLocal (JCType u32spec arrDec)
      cw <- emitUninitLocal (JCType i32spec arrDec)
      rw <- emitUninitLocal (JCType i32spec arrDec)
      res <- emitUninitLocal tyK
      emitStmt $ Exp (Just $ cCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitX86Dp4aSWord (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        emitStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal res)

  vdp4aVecU ::
    forall m k.
    (SIMDDp4aShape m k) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C m Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Word32) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C k Word32)
  vdp4aVecU (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      addHeader "string.h"
      tyA <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m Word8))
      tyK <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C k Word32))
      RVal ea0 <- ma
      RVal eb0 <- mb
      RVal ec0 <- mc
      ea <- emitLocalVal tyA ea0
      eb <- emitLocalVal tyA eb0
      ec <- emitLocalVal tyK ec0
      let groups = simdLanes (Proxy @k) (Proxy @Word32)
          JCType u32spec _ = namedCType "uint32_t"
          arrDec = Array [] (ArraySize False (signedIntConst groups) noLoc) (DeclRoot noLoc) noLoc
      aw <- emitUninitLocal (JCType u32spec arrDec)
      bw <- emitUninitLocal (JCType u32spec arrDec)
      cw <- emitUninitLocal (JCType u32spec arrDec)
      rw <- emitUninitLocal (JCType u32spec arrDec)
      res <- emitUninitLocal tyK
      emitStmt $ Exp (Just $ cCall "memcpy" [aw, UnOp AddrOf ea noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [bw, UnOp AddrOf eb noLoc, signedIntConst (groups * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [cw, UnOp AddrOf ec noLoc, signedIntConst (groups * 4)]) noLoc
      forM_ [0 .. groups - 1] $ \g -> do
        let gi = signedIntConst g
        rg <- emitX86Dp4aUWord (Index aw gi noLoc) (Index bw gi noLoc) (Index cw gi noLoc)
        emitStmt $ Exp (Just $ Assign (Index rw gi noLoc) JustAssign rg noLoc) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, rw, signedIntConst (groups * 4)]) noLoc
      pure (RVal res)

  vdp2aLoS (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = cCast "int32_t" (cCast "int16_t" a)
          a1 = cCast "int32_t" (cCast "int16_t" (BinOp Rsh a (cInt 16) noLoc))
          b0 = cCast "int32_t" (cCast "int8_t" b)
          b1 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 8) noLoc))
          prod = cCast "uint32_t" (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc)
      RVal <$> emitLocalVal (namedCType "int32_t") (cCast "int32_t" (BinOp Add (cCast "uint32_t" ec) prod noLoc))

  vdp2aHiS (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = cCast "int32_t" (cCast "int16_t" a)
          a1 = cCast "int32_t" (cCast "int16_t" (BinOp Rsh a (cInt 16) noLoc))
          b2 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 16) noLoc))
          b3 = cCast "int32_t" (cCast "int8_t" (BinOp Rsh b (cInt 24) noLoc))
          prod = cCast "uint32_t" (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc)
      RVal <$> emitLocalVal (namedCType "int32_t") (cCast "int32_t" (BinOp Add (cCast "uint32_t" ec) prod noLoc))

  vdp2aLoU (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
          a1 = BinOp Rsh a (cInt 16) noLoc
          b0 = BinOp And b (unsignedIntConst (0xFF :: Word32)) noLoc
          b1 = BinOp And (BinOp Rsh b (cInt 8) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
      RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b0 noLoc) (BinOp Mul a1 b1 noLoc) noLoc) noLoc)

  vdp2aHiU (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) (LinuxX86_64C (JanusC mc)) =
    LinuxX86_64C $ JanusC $ do
      RVal ea <- ma
      RVal eb <- mb
      RVal ec <- mc
      a <- emitLocalVal (namedCType "uint32_t") ea
      b <- emitLocalVal (namedCType "uint32_t") eb
      let a0 = BinOp And a (unsignedIntConst (0xFFFF :: Word32)) noLoc
          a1 = BinOp Rsh a (cInt 16) noLoc
          b2 = BinOp And (BinOp Rsh b (cInt 16) noLoc) (unsignedIntConst (0xFF :: Word32)) noLoc
          b3 = BinOp Rsh b (cInt 24) noLoc
      RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add ec (BinOp Add (BinOp Mul a0 b2 noLoc) (BinOp Mul a1 b3 noLoc) noLoc) noLoc)

  vsad8 ::
    forall n.
    (SIMDShape n Word8) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n Word8) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n Word8) ->
    LinuxX86_64C Word32
  vsad8 (LinuxX86_64C (JanusC ma)) (LinuxX86_64C (JanusC mb)) =
    LinuxX86_64C $ JanusC $ do
      fs <- askX86Features
      ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Word8))
      RVal ea0 <- ma
      RVal eb0 <- mb
      ea <- emitLocalVal ty ea0
      eb <- emitLocalVal ty eb0
      let lanes = simdLanes (Proxy @n) (Proxy @Word8)
      case lanes of
        32 | hasX86Feature fs X86_AVX2 -> do
          s <- emitLocalVal (namedCType "__m256i") (cCall "_mm256_sad_epu8" [ea, eb])
          s128 <- emitLocalVal (namedCType "__m128i") (cCall "_mm_add_epi64" [cCall "_mm256_castsi256_si128" [s], cCall "_mm256_extracti128_si256" [s, cInt 1]])
          let lo = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s128])
              hi = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [s128, cInt 8]])
          RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add lo hi noLoc)
        16 -> do
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [ea, eb])
          let lo = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s])
              hi = cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [cCall "_mm_srli_si128" [s, cInt 8]])
          RVal <$> emitLocalVal (namedCType "uint32_t") (BinOp Add lo hi noLoc)
        8 -> do
          xa <- toXMM128 @Word8 8 ea
          xb <- toXMM128 @Word8 8 eb
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [xa, xb])
          RVal <$> emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s]))
        _ -> do
          xa <- toXMM128 @Word8 4 ea
          xb <- toXMM128 @Word8 4 eb
          s <- emitLocalVal (namedCType "__m128i") (cCall "_mm_sad_epu8" [xa, xb])
          RVal <$> emitLocalVal (namedCType "uint32_t") (cCast "uint32_t" (cCall "_mm_cvtsi128_si64" [s]))

-- 11. ExpSIMDCast
instance ExpSIMDCast LinuxX86_64C where
  vbitcast ::
    forall n a m b.
    (SIMDShape n a, SIMDShape m b) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m b)
  vbitcast (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    let b1 = simdBytes (Proxy @n) (Proxy @a)
        b2 = simdBytes (Proxy @m) (Proxy @b)
    when (b1 /= b2) $ error "vbitcast: mismatched byte widths"
    srcTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    out <- emitUninitLocal dstTy
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, UnOp AddrOf ev noLoc, signedIntConst b1]) noLoc
    pure (RVal out)

  vcvt ::
    forall n a b.
    (SIMDShape n a, SIMDShape n b, CPUSIMDElem a, CPUSIMDElem b) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n b)
  vcvt (LinuxX86_64C (JanusC mv)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n b))
    RVal ev0 <- mv
    ev <- emitLocalVal srcTy ev0
    let lanes = simdLanes (Proxy @n) (Proxy @a)
        sb = simdBytes (Proxy @n) (Proxy @a)
        db = simdBytes (Proxy @n) (Proxy @b)
        JCType sspec _ = namedCType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = namedCType (simdElemCTypeName (Proxy @b))
        arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
    sa <- emitUninitLocal (JCType sspec arrDec)
    da <- emitUninitLocal (JCType dspec arrDec)
    out <- emitUninitLocal dstTy
    emitStmt $ Exp (Just $ cCall "memcpy" [sa, UnOp AddrOf ev noLoc, signedIntConst sb]) noLoc
    forM_ [0 .. lanes - 1] $ \i -> do
      let idx = signedIntConst i
      emitStmt $ Exp (Just $ Assign (Index da idx noLoc) JustAssign (cCast (simdElemCTypeName (Proxy @b)) (Index sa idx noLoc)) noLoc) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, da, signedIntConst db]) noLoc
    pure (RVal out)

  vnarrowSat ::
    forall n m a b.
    (SIMDPackShape n m a b) =>
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C n a) ->
    LinuxX86_64C (SIMDVector LinuxX86_64C m b)
  vnarrowSat (LinuxX86_64C (JanusC mlo)) (LinuxX86_64C (JanusC mhi)) = LinuxX86_64C $ JanusC $ do
    addHeader "string.h"
    srcTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
    dstTy <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C m b))
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
        JCType sspec _ = namedCType (simdElemCTypeName (Proxy @a))
        JCType dspec _ = namedCType (simdElemCTypeName (Proxy @b))
        sDec = Array [] (ArraySize False (signedIntConst half) noLoc) (DeclRoot noLoc) noLoc
        dDec = Array [] (ArraySize False (signedIntConst total) noLoc) (DeclRoot noLoc) noLoc
    alo <- emitUninitLocal (JCType sspec sDec)
    ahi <- emitUninitLocal (JCType sspec sDec)
    adst <- emitUninitLocal (JCType dspec dDec)
    out <- emitUninitLocal dstTy
    emitStmt $ Exp (Just $ cCall "memcpy" [alo, UnOp AddrOf elo noLoc, signedIntConst sb]) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [ahi, UnOp AddrOf ehi noLoc, signedIntConst sb]) noLoc
    let clampExp v =
          let v64 = cCast "int64_t" v
              loE = signedIntConst dmin
              hiE = signedIntConst dmax
           in cCast (simdElemCTypeName (Proxy @b)) (Cond (BinOp Lt v64 loE noLoc) loE (Cond (BinOp Gt v64 hiE noLoc) hiE v64 noLoc) noLoc)
    forM_ [0 .. half - 1] $ \i -> do
      emitStmt $ Exp (Just $ Assign (Index adst (signedIntConst i) noLoc) JustAssign (clampExp (Index alo (signedIntConst i) noLoc)) noLoc) noLoc
      emitStmt $ Exp (Just $ Assign (Index adst (signedIntConst (half + i)) noLoc) JustAssign (clampExp (Index ahi (signedIntConst i) noLoc)) noLoc) noLoc
    emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, adst, signedIntConst db]) noLoc
    pure (RVal out)

-- 12. CmdSIMDMem
emitX86Load :: forall n a. (SIMDShape n a) => Bool -> Exp -> JanusCM Exp
emitX86Load aligned ptrExp = do
  fs <- askX86Features
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  p <- emitLocalVal (JCType (DeclSpec [] [Tconst noLoc] (Tvoid noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)) ptrExp
  let b = simdBytes (Proxy @n) (Proxy @a)
      tag = simdElemTag (Proxy @a)
  case b of
    16 ->
      let call = case tag of
            8 -> cCall (if aligned then "_mm_load_ps" else "_mm_loadu_ps") [cConstPtrCast "float" p]
            9 -> cCall (if aligned then "_mm_load_pd" else "_mm_loadu_pd") [cConstPtrCast "double" p]
            _ -> cCall (if aligned then "_mm_load_si128" else "_mm_loadu_si128") [cConstPtrCast "__m128i" p]
       in emitLocalVal ty call
    32 | x86Has256Reg fs (Proxy @a) ->
      let call = case tag of
            8 -> cCall (if aligned then "_mm256_load_ps" else "_mm256_loadu_ps") [cConstPtrCast "float" p]
            9 -> cCall (if aligned then "_mm256_load_pd" else "_mm256_loadu_pd") [cConstPtrCast "double" p]
            _ -> cCall (if aligned then "_mm256_load_si256" else "_mm256_loadu_si256") [cConstPtrCast "__m256i" p]
       in emitLocalVal ty call
    _ -> emitMemcpyLoad @(SIMDVector LinuxX86_64C n a) b p

emitX86Store :: forall n a. (SIMDShape n a) => Bool -> Exp -> Exp -> JanusCM ()
emitX86Store aligned ptrExp valExp = do
  fs <- askX86Features
  ty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
  p <- emitLocalVal voidPtrType ptrExp
  v <- emitLocalVal ty valExp
  let b = simdBytes (Proxy @n) (Proxy @a)
      tag = simdElemTag (Proxy @a)
  case b of
    16 ->
      let call = case tag of
            8 -> cCall (if aligned then "_mm_store_ps" else "_mm_storeu_ps") [cPtrCast "float" p, v]
            9 -> cCall (if aligned then "_mm_store_pd" else "_mm_storeu_pd") [cPtrCast "double" p, v]
            _ -> cCall (if aligned then "_mm_store_si128" else "_mm_storeu_si128") [cPtrCast "__m128i" p, v]
       in emitStmt (Exp (Just call) noLoc)
    32 | x86Has256Reg fs (Proxy @a) ->
      let call = case tag of
            8 -> cCall (if aligned then "_mm256_store_ps" else "_mm256_storeu_ps") [cPtrCast "float" p, v]
            9 -> cCall (if aligned then "_mm256_store_pd" else "_mm256_storeu_pd") [cPtrCast "double" p, v]
            _ -> cCall (if aligned then "_mm256_store_si256" else "_mm256_storeu_si256") [cPtrCast "__m256i" p, v]
       in emitStmt (Exp (Just call) noLoc)
    _ -> do
      addHeader "string.h"
      emitStmt (Exp (Just $ cCall "memcpy" [p, UnOp AddrOf v noLoc, signedIntConst b]) noLoc)

instance (SIMDShape n a, Typeable a, JanusCTyped a) => CmdSIMDMem LinuxX86_64CM LinuxX86_64C n a where
  vload (LinuxX86_64C (JanusC mp)) = LinuxX86_64CM $ do
    RVal p <- mp
    v <- emitX86Load @n @a False p
    pure (LinuxX86_64C (JanusC (pure (RVal v))))

  vloadAligned (LinuxX86_64C (JanusC mp)) = LinuxX86_64CM $ do
    RVal p <- mp
    v <- emitX86Load @n @a True p
    pure (LinuxX86_64C (JanusC (pure (RVal v))))

  vstore (LinuxX86_64C (JanusC mp)) (LinuxX86_64C (JanusC mv)) = LinuxX86_64CM $ do
    RVal p <- mp
    RVal v <- mv
    emitX86Store @n @a False p v

  vstoreAligned (LinuxX86_64C (JanusC mp)) (LinuxX86_64C (JanusC mv)) = LinuxX86_64CM $ do
    RVal p <- mp
    RVal v <- mv
    emitX86Store @n @a True p v

  vloadMasked (LinuxX86_64C (JanusC mm)) (LinuxX86_64C (JanusC mfb)) (LinuxX86_64C (JanusC mp)) =
    LinuxX86_64CM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- emitUninitLocal (JCType espec arrDec)
      out <- emitUninitLocal vty
      emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignLoad = BlockStm $ Exp (Just $ Assign (Index arr (signedIntConst i) noLoc) JustAssign (Index ep (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignLoad] noLoc) Nothing noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, arr, signedIntConst bytes]) noLoc
      pure (LinuxX86_64C (JanusC (pure (RVal out))))

  vstoreMasked (LinuxX86_64C (JanusC mm)) (LinuxX86_64C (JanusC mp)) (LinuxX86_64C (JanusC mv)) =
    LinuxX86_64CM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      arr <- emitUninitLocal (JCType espec arrDec)
      emitStmt $ Exp (Just $ cCall "memcpy" [arr, UnOp AddrOf ev noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignStore = BlockStm $ Exp (Just $ Assign (Index ep (signedIntConst i) noLoc) JustAssign (Index arr (signedIntConst i) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignStore] noLoc) Nothing noLoc

  vgather (LinuxX86_64C (JanusC mp)) (LinuxX86_64C (JanusC midx)) =
    LinuxX86_64CM $ do
      addHeader "string.h"
      fs <- askX86Features
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
      pty <- getJanusCType (Proxy @(Ptr a))
      RVal ep0 <- mp
      RVal eidx0 <- midx
      ep <- emitLocalVal pty ep0
      eidx <- emitLocalVal ity eidx0
      let lanes = simdLanes (Proxy @n) (Proxy @a)
          sz = simdElemBytes (Proxy @a)
          tag = simdElemTag (Proxy @a)
      out <- case (hasX86Feature fs X86_AVX2, tag, lanes) of
        (True, 8, 8) -> emitLocalVal vty (cCall "_mm256_i32gather_ps" [cConstPtrCast "float" ep, eidx, cInt 4])
        (True, 8, 4) -> emitLocalVal vty (cCall "_mm_i32gather_ps" [cConstPtrCast "float" ep, eidx, cInt 4])
        (True, t, 8) | t `elem` [4, 5] -> emitLocalVal vty (cCall "_mm256_i32gather_epi32" [cConstPtrCast "int" ep, eidx, cInt 4])
        (True, t, 4) | t `elem` [4, 5] -> emitLocalVal vty (cCall "_mm_i32gather_epi32" [cConstPtrCast "int" ep, eidx, cInt 4])
        _ -> do
          let JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
              JCType ispec _ = namedCType "int32_t"
              arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
          idxArr <- emitUninitLocal (JCType ispec arrDec)
          dstArr <- emitUninitLocal (JCType espec arrDec)
          res <- emitUninitLocal vty
          emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
          forM_ [0 .. lanes - 1] $ \i ->
            emitStmt $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index ep (Index idxArr (signedIntConst i) noLoc) noLoc) noLoc) noLoc
          emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf res noLoc, dstArr, signedIntConst (lanes * sz)]) noLoc
          pure res
      pure (LinuxX86_64C (JanusC (pure (RVal out))))

  vgatherMasked (LinuxX86_64C (JanusC mm)) (LinuxX86_64C (JanusC mfb)) (LinuxX86_64C (JanusC mp)) (LinuxX86_64C (JanusC midx)) =
    LinuxX86_64CM $ do
      addHeader "string.h"
      mty <- getJanusCType (Proxy @(SIMDMask LinuxX86_64C n a))
      vty <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n a))
      ity <- getJanusCType (Proxy @(SIMDVector LinuxX86_64C n Int32))
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
          JCType espec _ = namedCType (simdElemCTypeName (Proxy @a))
          JCType ispec _ = namedCType "int32_t"
          arrDec = Array [] (ArraySize False (signedIntConst lanes) noLoc) (DeclRoot noLoc) noLoc
      idxArr <- emitUninitLocal (JCType ispec arrDec)
      dstArr <- emitUninitLocal (JCType espec arrDec)
      out <- emitUninitLocal vty
      emitStmt $ Exp (Just $ cCall "memcpy" [idxArr, UnOp AddrOf eidx noLoc, signedIntConst (lanes * 4)]) noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [dstArr, UnOp AddrOf efb noLoc, signedIntConst bytes]) noLoc
      let maskBytes = cConstPtrCast "uint8_t" (UnOp AddrOf em noLoc)
      forM_ [0 .. lanes - 1] $ \i -> do
        let active = BinOp And (Index maskBytes (signedIntConst (i * sz + (sz - 1))) noLoc) (unsignedIntConst (0x80 :: Word32)) noLoc
            assignGather = BlockStm $ Exp (Just $ Assign (Index dstArr (signedIntConst i) noLoc) JustAssign (Index ep (Index idxArr (signedIntConst i) noLoc) noLoc) noLoc) noLoc
        emitStmt $ If active (Block [assignGather] noLoc) Nothing noLoc
      emitStmt $ Exp (Just $ cCall "memcpy" [UnOp AddrOf out noLoc, dstArr, signedIntConst bytes]) noLoc
      pure (LinuxX86_64C (JanusC (pure (RVal out))))
