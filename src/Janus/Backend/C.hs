{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Janus.Backend.C where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (unpack)
import Data.Coerce
import Data.Complex
import Data.Int
import Data.Loc
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import Data.Semilattice.Join
import Data.Semilattice.Lower
import Data.Semilattice.Meet
import Data.Semilattice.Upper
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Traversable
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr
import GHC.Float
import Janus.Backend.C.Build
import Janus.FFI.Arg
import Janus.FFI.Ret
import Language.C.Quote as C
import qualified Rock
import System.FilePath
import System.IO
import System.Posix
import Text.PrettyPrint.Mainland (pretty)
import Text.PrettyPrint.Mainland.Class

data JanusCBackend = JC | JCUDA
  deriving (Eq, Ord, Show)

newtype LVal a = LVal {getLVal :: Exp}

newtype RVal a = RVal {getRVal :: Exp}

data JCType = JCType DeclSpec Decl
  deriving (Eq, Ord, Show)

data JCTypedef = JCTypedef DeclSpec String
  deriving (Eq, Ord, Show)

data JCFunc = JCFunc
  { _jcfName :: String,
    _jcfHeaders :: Set.Set String,
    _jcfTypedefs :: Set.Set JCTypedef,
    _jcfTypeCache :: Map.Map TypeRep JCType,
    _jcfProtos :: Set.Set InitGroup,
    _jcfType :: Maybe JCType,
    _jcfParams :: Params,
    _jcfBlock :: Endo [BlockItem],
    _jcfExp :: Maybe Exp,
    _jcfVarCounter :: Int
  }

$(makeLenses ''JCFunc)

type JanusCMState = Map.Map String JCFunc

data JCFuncInfo
  = JCFuncInfo
  { _jcfiName :: String
  , _jcfiBackend :: JanusCBackend
  , _jcfiIsLocal :: Bool
  } deriving (Eq, Ord, Show)

$(makeLenses ''JCFuncInfo)

newtype JanusC a = JanusC {getJanusC :: JanusCM (RVal a)}

newtype JanusCM a = JanusCM {getJanusCM :: ReaderT JCFuncInfo (State JanusCMState) a}
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadState JanusCMState, MonadReader JCFuncInfo)

askFuncName :: JanusCM String
askFuncName = asks _jcfiName

jcTypeQuals :: JCFuncInfo -> [TypeQual]
jcTypeQuals (JCFuncInfo _ JC _) = []
jcTypeQuals (JCFuncInfo _ JCUDA isLocal) = if isLocal then [TCUDAdevice noLoc] else [TCUDAglobal noLoc]

askTypeQuals :: JanusCM [TypeQual]
askTypeQuals = asks jcTypeQuals

defaultJanusState :: JanusCMState
defaultJanusState = Map.fromList [("janus_main", defaultJCFunc "janus_main" (Params [] False noLoc))]

appendBlock :: Endo [BlockItem] -> [BlockItem] -> Endo [BlockItem]
appendBlock block items = Endo (<> items) <> block

setTypeQualifiers :: [TypeQual] -> DeclSpec -> DeclSpec
setTypeQualifiers tqs (DeclSpec sto _ ts loc) = DeclSpec sto tqs ts loc
setTypeQualifiers _ _ = error "setTypeQualifiers: the impossible happened"

renderJCFunc :: JCFunc -> String
renderJCFunc (JCFunc fname headerSet typedefSet _ funcSet (Just (JCType spec dec)) params block e _) =
  let headers = foldMap (\h -> "#include <" <> h <> ">\n") headerSet
      tdef (JCTypedef spec' name) =
        DecDef
          ( TypedefGroup
              spec'
              []
              [Typedef (Id name noLoc) (DeclRoot noLoc) [] noLoc]
              noLoc
          )
          noLoc
      protos = foldMap ((<> "\n") . pretty 120 . ppr . BlockDecl) funcSet
      typedefs = foldMap ((<> "\n") . pretty 120 . ppr . tdef) typedefSet
      block' = appendBlock block [BlockStm $ Return e noLoc]
      body = pretty 120 $ ppr $ Func spec (Id fname noLoc) dec params (appEndo block' []) noLoc
      externCBegin = "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"
      externCEnd = "\n\n#ifdef __cplusplus\n}\n#endif\n"
   in headers <> ['\n' | not (null headers)] <> externCBegin <> typedefs <> ['\n' | not (null typedefs)] <> protos <> ['\n' | not (null protos)] <> body <> externCEnd
renderJCFunc _ = error "renderJCFunc: function has no type specified!"

modifyFunction :: (JCFunc -> JCFunc) -> JanusCM ()
modifyFunction f = do
  fname <- askFuncName
  modify $ \s -> s & ix fname %~ f

class (Typeable a) => JanusCTyped a where
  jctype :: Proxy a -> JanusCM JCType

finishFunction :: forall a. (JanusCTyped a) => JanusC a -> JanusCM ()
finishFunction a = do
  JCType spec dec <- getJanusCType (Proxy @a)
  RVal e <- getJanusC a
  fname <- askFuncName
  tqs <- askTypeQuals
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> put $ flip (Map.insert fname) jcs $ f & jcfExp ?~ e & jcfType ?~ JCType (setTypeQualifiers tqs spec) dec
    Nothing -> error "finishFunction: the impossible happened"

finishFunction_ :: JanusCM ()
finishFunction_ = do
  fname <- askFuncName
  tqs <- askTypeQuals
  jcs <- get
  case Map.lookup fname jcs of
    Just f ->
      let voidty = JCType (DeclSpec [] tqs (Tvoid noLoc) noLoc) (DeclRoot noLoc)
       in put $ flip (Map.insert fname) jcs $ f & jcfType ?~ voidty
    Nothing -> error "finishFunction_: the impossible happened"

defaultJCFunc :: String -> Params -> JCFunc
defaultJCFunc name params =
  JCFunc
    { _jcfName = name,
      _jcfHeaders = Set.empty,
      _jcfTypedefs = Set.empty,
      _jcfTypeCache = Map.empty,
      _jcfProtos = Set.empty,
      _jcfType = Nothing,
      _jcfParams = params,
      _jcfBlock = mempty,
      _jcfExp = Nothing,
      _jcfVarCounter = 0
    }

mkArgId :: Int -> Id
mkArgId m = Id ("arg_" <> show m) noLoc

type family JanusCEval a where
  JanusCEval (JanusC a -> r) = a -> JanusCEval r
  -- TODO figure out why the unsafePerformIO required for this allows the DL to be closed before use
  JanusCEval (JanusC a) = IO a
  JanusCEval (JanusCM (JanusC a)) = IO a
  JanusCEval (JanusCM ()) = IO ()

type family JanusCRetType a where
  JanusCRetType (JanusC a) = a
  JanusCRetType (JanusCM (JanusC a)) = a
  JanusCRetType (JanusC a -> r) = JanusCRetType r

class JanusCParam r where
  jcparam :: String -> Int -> JanusCM [Param] -> JanusCBackend -> r -> [JCFunc]

class JanusCEvaluate r where
  jceval :: FunPtr r -> [Arg] -> r -> JanusCEval r

-- TODO figure out why the unsafePerformIO required for this allows the DL to be closed before use
instance (FFIRet a, JanusCTyped a) => JanusCParam (JanusC a) where
  jcparam name _ params backend a = Map.elems $ flip execState defaultJanusState $ flip runReaderT (JCFuncInfo name backend False) $ getJanusCM $ do
    fname <- askFuncName
    params' <- params
    modify $ \s -> s & ix fname . jcfParams .~ Params (reverse params') False noLoc
    finishFunction a
    getJanusCType (Proxy @a)

instance (FFIRet a, JanusCTyped a) => JanusCEvaluate (JanusC a) where
  jceval fp args _ = callFFI fp (ret (Proxy @a)) (reverse args)

instance (FFIRet a, JanusCTyped a) => JanusCParam (JanusCM (JanusC a)) where
  jcparam name _ params backend a = Map.elems $ flip execState defaultJanusState $ flip runReaderT (JCFuncInfo name backend False) $ getJanusCM $ do
    fname <- askFuncName
    params' <- params
    modify $ \s -> s & ix fname . jcfParams .~ Params (reverse params') False noLoc
    a >>= finishFunction
    getJanusCType (Proxy @a)

instance (FFIRet a, JanusCTyped a) => JanusCEvaluate (JanusCM (JanusC a)) where
  jceval fp args _ = callFFI fp (ret (Proxy @a)) (reverse args)

instance JanusCParam (JanusCM ()) where
  jcparam name _ params backend a = Map.elems $ flip execState defaultJanusState $ flip runReaderT (JCFuncInfo name backend False) $ getJanusCM $ do
    fname <- askFuncName
    params' <- params
    modify $ \s -> s & ix fname . jcfParams .~ Params (reverse params') False noLoc
    a >> finishFunction_
    -- getJanusCType (Proxy @a)

instance JanusCEvaluate (JanusCM ()) where
  jceval fp args _ = callFFI fp (ret (Proxy @())) (reverse args)

instance (JanusCTyped a, JanusCParam r) => JanusCParam (JanusC a -> r) where
  jcparam name n args tqs f = jcparam name (n + 1) args' tqs (f $ JanusC $ pure $ RVal $ Var (mkArgId n) noLoc)
    where
      args' = do
        JCType spec dec <- getJanusCType (Proxy @a)
        args'' <- args
        pure $ Param (Just (mkArgId n)) spec dec noLoc:args''

instance (JanusCTyped a, FFIArg a, JanusCEvaluate r) => JanusCEvaluate (JanusC a -> r) where
  jceval fp args f a = jceval (castFunPtr fp) (arg a:args) (f $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)

-- this mostly exists for use with hedgehog withResource
acquireJanusC :: forall r. (JanusCParam r, JanusCEvaluate r) => JanusCBackend -> r -> IO (JanusCEval r, DL)
acquireJanusC backend a = do
  let dir = "_cache"
  files <- writeJanusCFiles backend dir (jcparam "janus_main" 0 (pure []) backend a)
  dl <- acquireJanusCDL dir files
  fp <- dlsym dl "janus_main"
  if fp == nullFunPtr
    then error "withJanusC: null function pointer"
    else pure (jceval fp [] a :: JanusCEval r, dl)

-- this mostly exists for use with hedgehog withResource
releaseJanusC :: (a, DL) -> IO ()
releaseJanusC = releaseJanusCDL . snd

withJanusC :: forall a r. (JanusCParam r, JanusCEvaluate r, FFIRet (JanusCRetType r)) => r -> (JanusCEval r -> IO a) -> IO a
withJanusC a k = bracket (acquireJanusC JC a) releaseJanusC (k . fst)

showJanusC :: forall a. (JanusCParam a) => String -> JanusCBackend -> a -> String
showJanusC name backend a =
  let jcs = jcparam name 0 (pure []) backend a
      strChar 0 = '\n'
      strChar 81 = '\n'
      strChar _ = '-'
      str = fmap strChar [0 .. 81 :: Int]
   in foldMap ((<> str) . renderJCFunc) jcs

printJanusC :: (JanusCParam r) => r -> IO ()
printJanusC = putStrLn . showJanusC "janus_main" JC

backendExt :: JanusCBackend -> String
backendExt JC = ".c"
backendExt JCUDA = ".cu"

writeJanusCFiles :: JanusCBackend -> FilePath -> [JCFunc] -> IO [String]
writeJanusCFiles backend dir funcs = do
  _ <- Rock.runTask janusRules (Rock.fetch $ CacheDir dir)
  for funcs $ \f -> do
    let body = renderJCFunc f
        sha = SHA256.hash (Text.encodeUtf8 $ Text.pack body)
        filename = unpack (B16.encode sha) <> backendExt backend
        path = dir </> filename
    exist <- fileExist path
    unless exist $ do
      withFile path WriteMode $ \hndl -> do
        hPutStrLn hndl body
    pure filename

getFunction :: JanusCM JCFunc
getFunction = do
  fname <- askFuncName
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> pure f
    Nothing -> do
      let f = defaultJCFunc fname (Params [] False noLoc)
      put $ jcs & at fname ?~ f
      pure f

-- NOTE the memoization here is mandatory for fixpoints to work
getJanusCType :: (Typeable a, JanusCTyped a) => Proxy a -> JanusCM JCType
getJanusCType p = do
  fname <- askFuncName
  f <- getFunction
  let tyrep = typeRep p
  case Map.lookup tyrep (f ^. jcfTypeCache) of
    Just t -> pure t
    Nothing -> do
      t@(JCType spec _) <- jctype p
      let isntStruct (DeclSpec _ _ (Tstruct {}) _) = False
          isntStruct _ = True
      if isntStruct spec
        then do
          -- have to get this again because jctype can insert headers
          f' <- getFunction
          let f'' = f' & jcfTypeCache %~ Map.insert tyrep t
          modify $ \s -> s & at fname ?~ f''
          pure t
        else do
          -- TODO generate name by hashing typerep
          let c = f ^. jcfVarCounter
              tyname = "type_" <> show c
              tyId = Id tyname noLoc
              spec' = DeclSpec [] [] (Tnamed tyId [] noLoc) noLoc
              dec' = DeclRoot noLoc
              t' = JCType spec' dec'
              f' =
                f
                  & jcfTypeCache %~ Map.insert tyrep t'
                  & jcfVarCounter +~ 1
                  & jcfTypedefs %~ Set.insert (JCTypedef spec tyname)
          modify $ \s -> s & at fname ?~ f'
          pure t'

addHeader :: String -> JanusCM ()
addHeader header = do
  fname <- askFuncName
  modify $ \s -> s & ix fname . jcfHeaders %~ Set.insert header

instance JanusCTyped Bool where
  jctype _ = do
    addHeader "stdbool.h"
    pure $ JCType (DeclSpec [] [] (Tnamed (Id "bool" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CChar where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tchar Nothing noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CSChar where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tchar (Just (Tsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CShort where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tshort Nothing noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CInt where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tint Nothing noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CLong where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tlong Nothing noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CLLong where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tlong_long Nothing noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CUChar where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tchar (Just (Tunsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CUShort where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tshort (Just (Tunsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CUInt where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tint (Just (Tunsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CULong where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tlong (Just (Tunsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CULLong where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tlong_long (Just (Tunsigned noLoc)) noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CSize where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

jcIntType :: String -> JanusCM JCType
jcIntType name = do
  addHeader "stdint.h"
  pure $ JCType (DeclSpec [] [] (Tnamed (Id name noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped Int8 where
  jctype _ = jcIntType "int8_t"

instance JanusCTyped Int16 where
  jctype _ = jcIntType "int16_t"

instance JanusCTyped Int32 where
  jctype _ = jcIntType "int32_t"

instance JanusCTyped Int64 where
  jctype _ = jcIntType "int64_t"

instance JanusCTyped Word8 where
  jctype _ = jcIntType "uint8_t"

instance JanusCTyped Word16 where
  jctype _ = jcIntType "uint16_t"

instance JanusCTyped Word32 where
  jctype _ = jcIntType "uint32_t"

instance JanusCTyped Word64 where
  jctype _ = jcIntType "uint64_t"

instance JanusCTyped Float where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tfloat noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped Double where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tdouble noLoc) noLoc) (DeclRoot noLoc)

instance (JanusCTyped a) => JanusCTyped (Ptr a) where
  jctype _ =
    jctype (Proxy @a) >>= \case
      JCType spec dec -> pure $ JCType spec (Ptr [] dec noLoc)

voidPtrType :: JCType
voidPtrType = JCType (DeclSpec [] [] (Tvoid noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)

instance JanusCTyped CFile where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tnamed (Id "FILE" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped (Complex Float) where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tfloat_Complex noLoc) noLoc) (DeclRoot noLoc)

class JanusLitC a where
  jlitc :: a -> JanusCM Exp

signedIntConst :: (Show a, Integral a) => a -> Exp
signedIntConst a = C.Const (IntConst (show a) Signed (toInteger a) noLoc) noLoc

unsignedIntConst :: (Show a, Integral a) => a -> Exp
unsignedIntConst a = C.Const (IntConst (show a) Unsigned (toInteger a) noLoc) noLoc

-- TODO make this figure out if C platform has signed char or not
instance JanusLitC CChar where
  jlitc = pure . signedIntConst

instance JanusLitC CSChar where
  jlitc = pure . signedIntConst

instance JanusLitC CShort where
  jlitc = pure . signedIntConst

instance JanusLitC CInt where
  jlitc = pure . signedIntConst

instance JanusLitC CLong where
  jlitc = pure . signedIntConst

instance JanusLitC CLLong where
  jlitc = pure . signedIntConst

instance JanusLitC CSize where
  jlitc = pure . signedIntConst

instance JanusLitC Int8 where
  jlitc = pure . signedIntConst

instance JanusLitC Int16 where
  jlitc = pure . signedIntConst

instance JanusLitC Int32 where
  jlitc = pure . signedIntConst

instance JanusLitC Int64 where
  jlitc = pure . signedIntConst

instance JanusLitC CUChar where
  jlitc = pure . unsignedIntConst

instance JanusLitC CUShort where
  jlitc = pure . unsignedIntConst

instance JanusLitC CUInt where
  jlitc = pure . unsignedIntConst

instance JanusLitC CULong where
  jlitc = pure . unsignedIntConst

instance JanusLitC CULLong where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word8 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word16 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word32 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word64 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Float where
  jlitc a = pure $ C.Const (FloatConst (show a) a noLoc) noLoc

instance JanusLitC Double where
  jlitc a = pure $ C.Const (DoubleConst (show a) a noLoc) noLoc

instance JanusLitC (Complex Float) where
  jlitc a = do
    addHeader "complex.h"
    r <- jlitc (realPart a)
    i <- jlitc (imagPart a)
    pure $ FnCall (Var (Id "CMPLX" noLoc) noLoc) [r, i] noLoc

instance JanusLitC (Complex Double) where
  jlitc a = do
    addHeader "complex.h"
    r <- jlitc (realPart a)
    i <- jlitc (imagPart a)
    pure $ FnCall (Var (Id "CMPLX" noLoc) noLoc) [r, i] noLoc

janusCUnOp :: UnOp -> JanusC a -> JanusC a
janusCUnOp unop (JanusC a) = JanusC $ do
  RVal a' <- a
  pure . RVal $ UnOp unop a' noLoc

janusCBinOp ::
  BinOp -> JanusC a -> JanusC a -> JanusC a
janusCBinOp binop (JanusC a) (JanusC b) = JanusC $ do
  RVal a' <- a
  RVal b' <- b
  pure $ RVal $ BinOp binop a' b' noLoc

janusCBoolBinOp ::
  BinOp -> JanusC a -> JanusC a -> JanusC Bool
janusCBoolBinOp = coerce janusCBinOp

instance (Num a, JanusLitC a) => Num (JanusC a) where
  fromInteger n = JanusC $ do
    n' <- jlitc @a (fromInteger n)
    pure $ RVal n'
  a + b = janusCBinOp Add a b
  a - b = janusCBinOp Sub a b
  a * b = janusCBinOp Mul a b
  negate = janusCUnOp Negate
  abs x = x * signum x
  signum (JanusC a) = JanusC $ do
    RVal a' <- a
    zero <- jlitc @a 0
    pure . RVal $
      BinOp
        Sub
        (BinOp Gt a' zero noLoc)
        (BinOp Lt a' zero noLoc)
        noLoc

instance (JanusLitC a, Fractional a) => Fractional (JanusC a) where
  fromRational a = JanusC $ do
    a' <- jlitc $ fromRational @a a
    pure $ RVal a'
  a / b = janusCBinOp Div a b

class JanusCFFICall r where
  jcffi :: Maybe String -> String -> JanusCM [Exp] -> r

instance JanusCFFICall (JanusC a) where
  jcffi mheader name args = JanusC $ do
    args' <- args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (JanusCTyped a) => JanusCFFICall (JanusCM (JanusC a)) where
  jcffi mheader name args = do
    args' <- args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    JCType spec dec <- getJanusCType (Proxy @a)
    fn <- getFunction
    fname <- askFuncName
    let c = fn ^. jcfVarCounter
        var = Id ("x_" <> show c) noLoc
        val = FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
        ini = Init var dec Nothing (Just $ ExpInitializer val noLoc) [] noLoc
        fn' =
          fn
            & jcfBlock %~ flip appendBlock [BlockDecl $ InitGroup spec [] [ini] noLoc]
            & jcfVarCounter +~ 1
    modify $ \st -> st & ix fname .~ fn'
    pure $ JanusC $ pure $ RVal $ Var var noLoc

instance {-# OVERLAPS #-} JanusCFFICall (JanusCM (JanusC (Ptr ()))) where
  jcffi mheader name args = do
    args' <- args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    let JCType spec dec = voidPtrType
    fn <- getFunction
    fname <- askFuncName
    let c = fn ^. jcfVarCounter
        var = Id ("x_" <> show c) noLoc
        val = FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
        ini = Init var dec Nothing (Just $ ExpInitializer val noLoc) [] noLoc
        fn' =
          fn
            & jcfBlock %~ flip appendBlock [BlockDecl $ InitGroup spec [] [ini] noLoc]
            & jcfVarCounter +~ 1
    modify $ \st -> st & ix fname .~ fn'
    pure $ JanusC $ pure $ RVal $ Var var noLoc

instance JanusCFFICall (JanusCM ()) where
  jcffi mheader name args = do
    args' <- args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    let fncall = FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
    modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm $ Exp (Just fncall) noLoc]

instance (JanusCFFICall r) => JanusCFFICall (JanusC a -> r) where
  jcffi mheader name args a = jcffi mheader name args''
    where
      args'' = do
        RVal arg' <- getJanusC a
        args' <- args
        pure (arg' : args')

janusCFFICall :: (JanusCFFICall r) => Maybe String -> String -> r
janusCFFICall header name = jcffi header name (pure [])

unMathFun :: String -> JanusC a -> JanusC a
unMathFun = janusCFFICall (Just "math.h")

binMathFun :: String -> JanusC a -> JanusC a -> JanusC a
binMathFun = janusCFFICall (Just "math.h")

instance Floating (JanusC Float) where
  pi = JanusC $ do
    pi' <- jlitc @Float pi
    pure $ RVal pi'
  exp = unMathFun "expf"
  log = unMathFun "logf"
  sqrt = unMathFun "sqrtf"
  x ** y = binMathFun "powf" x y
  sin = unMathFun "sinf"
  cos = unMathFun "cosf"
  tan = unMathFun "tanf"
  asin = unMathFun "asinf"
  acos = unMathFun "acosf"
  atan = unMathFun "atanf"
  sinh = unMathFun "sinhf"
  cosh = unMathFun "coshf"
  tanh = unMathFun "tanhf"
  asinh = unMathFun "asinhf"
  acosh = unMathFun "acoshf"
  atanh = unMathFun "atanhf"
  log1p = unMathFun "log1pf"
  expm1 = unMathFun "expm1f"

instance Floating (JanusC Double) where
  pi = JanusC $ do
    pi' <- jlitc @Double pi
    pure $ RVal pi'
  exp = unMathFun "exp"
  log = unMathFun "log"
  sqrt = unMathFun "sqrt"
  x ** y = binMathFun "pow" x y
  sin = unMathFun "sin"
  cos = unMathFun "cos"
  tan = unMathFun "tan"
  asin = unMathFun "asin"
  acos = unMathFun "acos"
  atan = unMathFun "atan"
  sinh = unMathFun "sinh"
  cosh = unMathFun "cosh"
  tanh = unMathFun "tanh"
  asinh = unMathFun "asinh"
  acosh = unMathFun "acosh"
  atanh = unMathFun "atanh"
  log1p = unMathFun "log1p"
  expm1 = unMathFun "expm1"

instance Floating (JanusC (Complex Float)) where
  pi = JanusC $ do
    pi' <- jlitc @(Complex Float) pi
    pure $ RVal pi'
  exp = unMathFun "cexpf"
  log = unMathFun "clogf"
  sqrt = unMathFun "csqrtf"
  x ** y = binMathFun "cpowf" x y
  sin = unMathFun "csinf"
  cos = unMathFun "ccosf"
  tan = unMathFun "ctanf"
  asin = unMathFun "casinf"
  acos = unMathFun "cacosf"
  atan = unMathFun "catanf"
  sinh = unMathFun "csinhf"
  cosh = unMathFun "ccoshf"
  tanh = unMathFun "ctanhf"
  asinh = unMathFun "casinhf"
  acosh = unMathFun "cacoshf"
  atanh = unMathFun "catanhf"

instance Floating (JanusC (Complex Double)) where
  pi = JanusC $ do
    pi' <- jlitc @(Complex Double) pi
    pure $ RVal pi'
  exp = unMathFun "cexp"
  log = unMathFun "clog"
  sqrt = unMathFun "csqrt"
  x ** y = binMathFun "cpow" x y
  sin = unMathFun "csin"
  cos = unMathFun "ccos"
  tan = unMathFun "ctan"
  asin = unMathFun "casin"
  acos = unMathFun "cacos"
  atan = unMathFun "catan"
  sinh = unMathFun "csinh"
  cosh = unMathFun "ccosh"
  tanh = unMathFun "ctanh"
  asinh = unMathFun "casinh"
  acosh = unMathFun "cacosh"
  atanh = unMathFun "catanh"

instance Join (JanusC Bool) where
  p \/ q = janusCBinOp Lor p q

instance Lower (JanusC Bool) where
  lowerBound = JanusC $ do
    addHeader "stdbool.h"
    pure . RVal $ Var (Id "false" noLoc) noLoc

instance Meet (JanusC Bool) where
  p /\ q = janusCBinOp Land p q

instance Upper (JanusC Bool) where
  upperBound = JanusC $ do
    addHeader "stdbool.h"
    pure . RVal $ Var (Id "true" noLoc) noLoc
