{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.Raw where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (unpack)
import Data.Char (isAlphaNum)
import Data.Coerce
import Data.Complex
import qualified Data.Dependent.HashMap as DHM
import Data.Int
import qualified Data.Kind as K
import Data.List (intercalate)
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
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.LibFFI
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr
import qualified Foreign.Storable as Storable
import GHC.Float
import Janus.Backend.C.Build
import Janus.Backend.C.Target (TargetFeatureKey, TargetFeatureMap, portableCFeatureKey)
import Janus.FFI.Arg
import Janus.FFI.Ret
import Language.C.Quote as C
import qualified Language.Haskell.TH as TH
import qualified Rock
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix
import Text.PrettyPrint.Mainland (pretty)
import Text.PrettyPrint.Mainland.Class

data JanusCBackend = JC | JCUDA
  deriving (Eq, Ord, Show)

newtype LVal a = LVal {getLVal :: Exp}

instance ToExp (LVal a) where
  toExp (LVal e) _ = e

newtype RVal a = RVal {getRVal :: Exp}

instance ToExp (RVal a) where
  toExp (RVal e) _ = e

data JCType = JCType DeclSpec Decl
  deriving (Eq, Ord, Show)

data JCTypedef
  = JCTypedef DeclSpec String
  | JCTypedefFull DeclSpec Decl String
  deriving (Eq, Ord, Show)

data JCFunc = JCFunc
  { _jcfName :: String,
    _jcfHeaders :: Set.Set String,
    _jcfTypedefs :: [JCTypedef],
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

data JCFuncInfo = JCFuncInfo
  { _jcfiName :: String,
    _jcfiBackend :: JanusCBackend,
    _jcfiIsLocal :: Bool,
    _jcfiTargetFeatures :: TargetFeatureMap
  }
  deriving (Eq, Ord, Show)

$(makeLenses ''JCFuncInfo)

mkJCFuncInfo :: String -> JanusCBackend -> Bool -> JCFuncInfo
mkJCFuncInfo name backend isLocal =
  JCFuncInfo
    { _jcfiName = name,
      _jcfiBackend = backend,
      _jcfiIsLocal = isLocal,
      _jcfiTargetFeatures = DHM.empty
    }

setTargetFeature :: TargetFeatureKey f -> f -> JCFuncInfo -> JCFuncInfo
setTargetFeature k f info =
  info & jcfiTargetFeatures %~ DHM.insert k (Identity f)

--------------------------------------------------------------------------------
-- Target-Indexed C Expression & Monad Types
--------------------------------------------------------------------------------

-- | Target tag for portable ISO C99/C11 host targets without architecture-specific ISA extensions.
data PortableC

newtype JanusCMT (t :: K.Type) a = JanusCMT
  { getJanusCMT :: ReaderT JCFuncInfo (State JanusCMState) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadState JanusCMState, MonadReader JCFuncInfo)

type JanusCM = JanusCMT PortableC

getJanusCM :: JanusCM a -> ReaderT JCFuncInfo (State JanusCMState) a
getJanusCM = getJanusCMT

liftJanusCM :: JanusCM a -> JanusCMT t a
liftJanusCM = coerce

unJanusCMT :: JanusCMT t a -> JanusCM a
unJanusCMT = coerce

newtype JanusCT (t :: K.Type) a = JanusCT
  { getJanusCT :: JanusCM (RVal a)
  }

pattern JanusC :: JanusCM (RVal a) -> JanusCT t a
pattern JanusC m = JanusCT m

{-# COMPLETE JanusC #-}

getJanusC :: JanusCT t a -> JanusCM (RVal a)
getJanusC = getJanusCT

type JanusC = JanusCT PortableC

class (Monad m) => MonadJanusC (m :: K.Type -> K.Type) where
  liftJC :: JanusCM a -> m a

instance MonadJanusC (JanusCMT t) where
  liftJC = liftJanusCM

instance (MonadJanusC m) => MonadJanusC (ReaderT r m) where
  liftJC = lift . liftJC

askFuncName :: JanusCMT t String
askFuncName = asks _jcfiName

class (Monad m) => MonadTargetFeatures (m :: K.Type -> K.Type) where
  askTargetFeature :: (Lower f) => TargetFeatureKey f -> m f

instance MonadTargetFeatures (JanusCMT t) where
  askTargetFeature k =
    asks (maybe lowerBound runIdentity . DHM.lookup k . _jcfiTargetFeatures)

instance (MonadTargetFeatures m) => MonadTargetFeatures (ReaderT r m) where
  askTargetFeature = lift . askTargetFeature

jcTypeQuals :: JCFuncInfo -> [TypeQual]
jcTypeQuals JCFuncInfo {_jcfiBackend = JC} = []
jcTypeQuals JCFuncInfo {_jcfiBackend = JCUDA, _jcfiIsLocal = isLocal} =
  if isLocal then [TCUDAdevice noLoc] else [TCUDAglobal noLoc]

askTypeQuals :: JanusCMT t [TypeQual]
askTypeQuals = asks jcTypeQuals

defaultJanusState :: JanusCMState
defaultJanusState = Map.fromList [("janus_main", defaultJCFunc "janus_main" (Params [] False noLoc))]

appendBlock :: Endo [BlockItem] -> [BlockItem] -> Endo [BlockItem]
appendBlock block items = Endo (<> items) <> block

setTypeQualifiers :: [TypeQual] -> DeclSpec -> DeclSpec
setTypeQualifiers tqs (DeclSpec sto _ ts loc) = DeclSpec sto tqs ts loc
setTypeQualifiers _ _ = error "setTypeQualifiers: the impossible happened"

addStaticStorage :: DeclSpec -> DeclSpec
addStaticStorage (DeclSpec sto tqs ts loc) =
  let isStatic = \case
        Tstatic _ -> True
        _ -> False
      sto' = if any isStatic sto then sto else Tstatic noLoc : sto
   in DeclSpec sto' tqs ts loc
addStaticStorage ds = ds

jcTypedefName :: JCTypedef -> String
jcTypedefName (JCTypedef _ nm) = nm
jcTypedefName (JCTypedefFull _ _ nm) = nm

dedupTypedefs :: [JCTypedef] -> [JCTypedef]
dedupTypedefs = go Set.empty
  where
    go _ [] = []
    go seen (td : rest)
      | Set.member (jcTypedefName td) seen = go seen rest
      | otherwise = td : go (Set.insert (jcTypedefName td) seen) rest

renderJCFunc :: JCFunc -> String
renderJCFunc (JCFunc fname headerSet typedefList _ funcSet (Just (JCType spec dec)) params block e _) =
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
      tdef (JCTypedefFull spec' dec' name) =
        DecDef
          ( TypedefGroup
              spec'
              []
              [Typedef (Id name noLoc) dec' [] noLoc]
              noLoc
          )
          noLoc
      protos = foldMap ((<> "\n") . pretty 120 . ppr . BlockDecl) funcSet
      typedefs = foldMap ((<> "\n") . pretty 120 . ppr . tdef) (dedupTypedefs typedefList)
      block' = case e of
        Just _ -> appendBlock block [BlockStm $ Return e noLoc]
        Nothing -> block
      body = pretty 120 $ ppr $ Func spec (Id fname noLoc) dec params (appEndo block' []) noLoc
      externCBegin = "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"
      externCEnd = "\n\n#ifdef __cplusplus\n}\n#endif\n"
   in headers <> ['\n' | not (null headers)] <> externCBegin <> typedefs <> ['\n' | not (null typedefs)] <> protos <> ['\n' | not (null protos)] <> body <> externCEnd
renderJCFunc _ = error "renderJCFunc: function has no type specified!"

renderJCProgram :: String -> [JCFunc] -> String
renderJCProgram entryName funcs =
  let definedNames = Set.fromList (map _jcfName funcs)
      orderedFuncs =
        filter ((/= entryName) . _jcfName) funcs
          <> filter ((== entryName) . _jcfName) funcs
      headerSet = foldMap _jcfHeaders orderedFuncs
      headers = foldMap (\h -> "#include <" <> h <> ">\n") headerSet
      tdef (JCTypedef spec' name) =
        DecDef
          ( TypedefGroup
              spec'
              []
              [Typedef (Id name noLoc) (DeclRoot noLoc) [] noLoc]
              noLoc
          )
          noLoc
      tdef (JCTypedefFull spec' dec' name) =
        DecDef
          ( TypedefGroup
              spec'
              []
              [Typedef (Id name noLoc) dec' [] noLoc]
              noLoc
          )
          noLoc
      typedefList = dedupTypedefs (foldMap _jcfTypedefs orderedFuncs)
      typedefs = foldMap ((<> "\n") . pretty 120 . ppr . tdef) typedefList
      isExternalProto (InitGroup _ _ inits _) =
        all
          ( \case
              Init (Id nm _) _ _ _ _ _ -> not (Set.member nm definedNames)
              _ -> True
          )
          inits
      isExternalProto _ = True
      extProtoSet = Set.filter isExternalProto (foldMap _jcfProtos orderedFuncs)
      mkFuncProto (JCFunc fname _ _ _ _ (Just (JCType spec dec)) params _ _ _) =
        let spec' = if fname == entryName then spec else addStaticStorage spec
         in InitGroup spec' [] [Init (Id fname noLoc) (Proto dec params noLoc) Nothing Nothing [] noLoc] noLoc
      mkFuncProto _ = error "renderJCProgram: function has no type specified!"
      extProtos = foldMap ((<> "\n") . pretty 120 . ppr . BlockDecl) extProtoSet
      localProtos = foldMap ((<> "\n") . pretty 120 . ppr . BlockDecl . mkFuncProto) orderedFuncs
      allProtos = extProtos <> localProtos
      renderBody (JCFunc fname _ _ _ _ (Just (JCType spec dec)) params block e _) =
        let spec' = if fname == entryName then spec else addStaticStorage spec
            block' = case e of
              Just _ -> appendBlock block [BlockStm $ Return e noLoc]
              Nothing -> block
         in pretty 120 $ ppr $ Func spec' (Id fname noLoc) dec params (appEndo block' []) noLoc
      renderBody _ = error "renderJCProgram: function has no type specified!"
      bodies = intercalate "\n\n" (map renderBody orderedFuncs)
      externCBegin = "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"
      externCEnd = "\n\n#ifdef __cplusplus\n}\n#endif\n"
   in headers
        <> ['\n' | not (null headers)]
        <> externCBegin
        <> typedefs
        <> ['\n' | not (null typedefs)]
        <> allProtos
        <> ['\n' | not (null allProtos)]
        <> bodies
        <> externCEnd

modifyFunction :: (JCFunc -> JCFunc) -> JanusCMT t ()
modifyFunction f = do
  fname <- askFuncName
  modify $ \s -> s & ix fname %~ f

class (Typeable a) => JanusCTyped a where
  jctype :: Proxy a -> JanusCM JCType

finishFunction :: forall s t a. (JanusCTyped a) => JanusCT s a -> JanusCMT t ()
finishFunction a = do
  JCType spec dec <- getJanusCType (Proxy @a)
  RVal e <- liftJanusCM (getJanusCT a)
  fname <- askFuncName
  tqs <- askTypeQuals
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> put $ flip (Map.insert fname) jcs $ f & jcfExp ?~ e & jcfType ?~ JCType (setTypeQualifiers tqs spec) dec
    Nothing -> error "finishFunction: the impossible happened"

finishFunction_ :: JanusCMT t ()
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
      _jcfTypedefs = [],
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

runJCCodegen :: JCFuncInfo -> JanusCM () -> [JCFunc]
runJCCodegen info action =
  let name = _jcfiName info
      initSt = Map.singleton name (defaultJCFunc name (Params [] False noLoc))
   in Map.elems $ execState (runReaderT (getJanusCMT action) info) initSt

--------------------------------------------------------------------------------
-- Target Descriptor, Value ABI, & Unified Function Compilation / Evaluation
--------------------------------------------------------------------------------

class (Lower (CTargetFeatures t)) => JanusCTarget (t :: K.Type) where
  type CTargetFeatures t :: K.Type
  cTargetFeatureKey :: Proxy t -> TargetFeatureKey (CTargetFeatures t)
  observeCTargetFeatures :: Proxy t -> IO (CTargetFeatures t)
  ensureCTargetAdmitted :: Proxy t -> CTargetFeatures t -> IO ()
  cTargetBanner :: Proxy t -> CTargetFeatures t -> String
  cTargetBuildFlags :: Proxy t -> CTargetFeatures t -> [JCFunc] -> IO ([String], [String])

askCTargetFeatures :: forall t m. (JanusCTarget t, MonadTargetFeatures m) => m (CTargetFeatures t)
askCTargetFeatures = askTargetFeature (cTargetFeatureKey (Proxy @t))

instance JanusCTarget PortableC where
  type CTargetFeatures PortableC = ()
  cTargetFeatureKey _ = portableCFeatureKey
  observeCTargetFeatures _ = pure ()
  ensureCTargetAdmitted _ () = pure ()
  cTargetBanner _ () = ""
  cTargetBuildFlags _ () _ = pure ([], [])

{-# INLINE withIndirectStorableArg #-}
withIndirectStorableArg :: (Storable.Storable a) => a -> (Ptr () -> IO b) -> IO b
withIndirectStorableArg v k =
  let b = Storable.sizeOf v
      al = max 32 (Storable.alignment v)
   in allocaBytesAligned b al $ \p -> Storable.poke p v >> k (castPtr p)

{-# INLINE withIndirectStorableRet #-}
withIndirectStorableRet :: forall a. (Storable.Storable a) => (Ptr () -> IO ()) -> IO a
withIndirectStorableRet callOut =
  let b = Storable.sizeOf (undefined :: a)
      al = max 32 (Storable.alignment (undefined :: a))
   in allocaBytesAligned b al $ \pOut -> callOut (castPtr pOut) >> Storable.peek pOut

emitIndirectABIParam :: forall a. (JanusCTyped a) => Int -> Int -> (JanusCM Param, Exp)
emitIndirectABIParam bytes n =
  let aid = mkArgId n
      valId = Id ("arg_val_" <> show n) noLoc
      JCType vspec vdec = voidPtrType
      mkP = do
        addHeader "string.h"
        JCType spec dec <- getJanusCType (Proxy @a)
        let decl = BlockDecl $ InitGroup spec [] [Init valId dec Nothing Nothing [] noLoc] noLoc
            cp = FnCall (Var (Id "memcpy" noLoc) noLoc) [UnOp AddrOf (Var valId noLoc) noLoc, Var aid noLoc, signedIntConst bytes] noLoc
            stm = BlockStm $ Exp (Just cp) noLoc
        modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [decl, stm]
        pure (Param (Just aid) vspec vdec noLoc)
   in (mkP, Var valId noLoc)

emitIndirectABIRet :: forall t a. (JanusCTyped a) => Int -> JanusCT t a -> JanusCM [Param]
emitIndirectABIRet bytes (JanusCT mval) = do
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

indirectABIParamTHSpec :: (Typeable a) => Proxy a -> THParamSpec
indirectABIParamTHSpec p =
  THParamSpec
    { thParamUserType = typeRepToTHType (typeRep p),
      thParamRawType = voidPtrTHType,
      thParamMode = THParamIO $ \k argExp -> do
        pName <- TH.newName "p_arg"
        inner <- k (TH.VarE pName)
        pure $
          TH.AppE
            (TH.AppE (TH.VarE 'withIndirectStorableArg) argExp)
            (TH.LamE [TH.VarP pName] inner)
    }

indirectABIRetTHSpec :: (Typeable a) => THRetEffect -> Proxy a -> THRetSpec
indirectABIRetTHSpec eff p =
  THRetSpec
    { thRetEffect = eff,
      thRetUserBaseType = typeRepToTHType (typeRep p),
      thRetMode = THRetOutPtr $ \callWithOut -> do
        pOutName <- TH.newName "p_out"
        callExp <- callWithOut (TH.VarE pOutName)
        pure $ TH.AppE (TH.VarE 'withIndirectStorableRet) (TH.LamE [TH.VarP pOutName] callExp)
    }

scalarParamSpec :: (Typeable a, FFIArg a) => Proxy a -> THParamSpec
scalarParamSpec = ffiTHParamSpec

scalarRetSpec :: (Typeable a, FFIRet a) => THRetEffect -> Proxy a -> THRetSpec
scalarRetSpec = ffiTHRetSpec

class (JanusCTyped a) => JanusCABIParam (t :: K.Type) (a :: K.Type) where
  jcEmitABIParam :: Proxy t -> Int -> (JanusCM Param, Exp)
  jcWithABIArg :: Proxy t -> a -> (Arg -> IO b) -> IO b
  jcABIParamIsIndirect :: Proxy t -> Proxy a -> Bool
  jcABIParamIsIndirect _ _ = False
  jcABIParamTHSpec :: Proxy t -> Proxy a -> THParamSpec

class (JanusCTyped a) => JanusCABIRet (t :: K.Type) (a :: K.Type) where
  jcFinishABIRet :: Proxy t -> JanusCT t a -> JanusCM [Param]
  jcCallABIRet :: Proxy t -> Proxy a -> FunPtr () -> [Arg] -> IO a
  jcABIRetIsIndirect :: Proxy t -> Proxy a -> Bool
  jcABIRetIsIndirect _ _ = False
  jcABIRetTHSpec :: Proxy t -> THRetEffect -> Proxy a -> THRetSpec

instance {-# OVERLAPPABLE #-} (JanusCTyped a, FFIArg a) => JanusCABIParam t a where
  jcEmitABIParam _ n =
    let aid = mkArgId n
        mkP = do
          JCType spec dec <- getJanusCType (Proxy @a)
          pure (Param (Just aid) spec dec noLoc)
     in (mkP, Var aid noLoc)
  jcWithABIArg _ a k = k (arg a)
  jcABIParamTHSpec _ = ffiTHParamSpec

instance {-# OVERLAPPABLE #-} (JanusCTyped a, FFIRet a) => JanusCABIRet t a where
  jcFinishABIRet _ a = finishFunction a >> pure []
  jcCallABIRet _ p fp args = callFFI fp (ret p) (reverse args)
  jcABIRetTHSpec _ = ffiTHRetSpec

type family JanusCHost (p :: K.Type) :: K.Type

type instance JanusCHost () = ()
type instance JanusCHost (JanusCT t a) = a

type family JanusCTermEval (mres :: K.Type) :: K.Type
type family JanusCTermRet (mres :: K.Type) :: K.Type

type instance JanusCTermEval (JanusCT t a) = a
type instance JanusCTermRet (JanusCT t a) = a

type instance JanusCTermEval (JanusCMT t res) = IO (JanusCHost res)
type instance JanusCTermRet (JanusCMT t res) = JanusCHost res

type family JanusCEval (r :: K.Type) :: K.Type where
  JanusCEval (p -> r) = JanusCHost p -> JanusCEval r
  JanusCEval mres = JanusCTermEval mres

type family JanusCRetType (r :: K.Type) :: K.Type where
  JanusCRetType (_ -> r) = JanusCRetType r
  JanusCRetType mres = JanusCTermRet mres

--------------------------------------------------------------------------------
-- Canonical Type Signature Hashing & C Symbol Mangling
--------------------------------------------------------------------------------

renderCanonicalTypeRep :: TypeRep -> String
renderCanonicalTypeRep tr =
  let (tc, args) = splitTyConApp tr
      headStr = tyConPackage tc <> ":" <> tyConModule tc <> "." <> tyConName tc
   in case args of
        [] -> headStr
        _ -> headStr <> "(" <> intercalate "," (map renderCanonicalTypeRep args) <> ")"

sanitizeCIdent :: String -> String
sanitizeCIdent s =
  case map (\c -> if isAlphaNum c || c == '_' then c else '_') s of
    "" -> "janus_fn"
    c0 : cs
      | c0 >= '0' && c0 <= '9' -> '_' : c0 : cs
      | otherwise -> c0 : cs

hashJanusSignature :: [TypeRep] -> [TypeRep] -> String
hashJanusSignature inTys outTys =
  let payload =
        "in:["
          <> intercalate "," (map renderCanonicalTypeRep inTys)
          <> "]|out:["
          <> intercalate "," (map renderCanonicalTypeRep outTys)
          <> "]"
      digest = SHA256.hash (Text.encodeUtf8 (Text.pack payload))
   in take 16 (unpack (B16.encode digest))

mangleJanusEntryName :: String -> [TypeRep] -> [TypeRep] -> String
mangleJanusEntryName baseName inTys outTys =
  sanitizeCIdent baseName <> "_" <> hashJanusSignature inTys outTys

mangleJanusLocalName :: String -> [TypeRep] -> [TypeRep] -> String
mangleJanusLocalName baseName inTys outTys =
  "janus_local_" <> sanitizeCIdent baseName <> "_" <> hashJanusSignature inTys outTys

class JanusCArg (t :: K.Type) (p :: K.Type) | p -> t where
  jcEmitArg :: Int -> (Int, JanusCM [Param], p)
  jcDummyArg :: p
  jcWithHostArg :: Proxy p -> JanusCHost p -> [Arg] -> ([Arg] -> IO b) -> IO b
  jcTHParamSpecs :: Proxy p -> [THParamSpec]
  jcArgSignatureTypes :: Proxy p -> [TypeRep]

class JanusCValRet (t :: K.Type) (res :: K.Type) where
  jcFinishValRet :: Proxy t -> res -> JanusCM [Param]
  jcCallValRet :: Proxy t -> Proxy res -> FunPtr () -> [Arg] -> IO (JanusCHost res)
  jcTHValRetSpec :: Proxy t -> THRetEffect -> Proxy res -> THRetSpec
  jcRetSignatureTypes :: Proxy t -> Proxy res -> [TypeRep]

instance (JanusCABIParam t a) => JanusCArg t (JanusCT t a) where
  jcEmitArg n =
    let (mkP, e) = jcEmitABIParam @t @a (Proxy @t) n
     in (n + 1, pure <$> mkP, JanusCT (pure (RVal e)))
  jcDummyArg =
    JanusCT (pure (RVal (Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc)))
  jcWithHostArg _ val prevArgs k =
    jcWithABIArg (Proxy @t) val (\a -> k (a : prevArgs))
  jcTHParamSpecs _ = [jcABIParamTHSpec (Proxy @t) (Proxy @a)]
  jcArgSignatureTypes _ = [typeRep (Proxy @a)]

instance JanusCValRet t () where
  jcFinishValRet _ () = finishFunction_ >> pure []
  jcCallValRet _ _ fp args = callFFI fp retVoid (reverse args)
  jcTHValRetSpec _ eff _ = THRetSpec eff unitTHType (THRetDirect unitTHType)
  jcRetSignatureTypes _ _ = []

instance (JanusCABIRet t a) => JanusCValRet t (JanusCT t a) where
  jcFinishValRet pt a = jcFinishABIRet pt a
  jcCallValRet pt _ fp args = jcCallABIRet pt (Proxy @a) fp args
  jcTHValRetSpec pt eff _ = jcABIRetTHSpec pt eff (Proxy @a)
  jcRetSignatureTypes _ _ = [typeRep (Proxy @a)]

class (JanusCTarget t) => JanusCParam (t :: K.Type) (r :: K.Type) | r -> t where
  jcparamWith :: CTargetFeatures t -> String -> Int -> JanusCM [Param] -> r -> [JCFunc]
  jcTHSpec :: Proxy r -> ([THParamSpec], THRetSpec)
  jcSignatureTypes :: Proxy r -> ([TypeRep], [TypeRep])

jcMangledSymbol :: forall t r. (JanusCParam t r) => Proxy r -> String -> String
jcMangledSymbol pr baseName =
  let (inTys, outTys) = jcSignatureTypes @t @r pr
   in mangleJanusEntryName baseName inTys outTys

class (JanusCTarget t) => JanusCEvaluate (t :: K.Type) (r :: K.Type) | r -> t where
  jcevalCont :: (([Arg] -> IO (JanusCRetType r)) -> IO (JanusCRetType r)) -> ForeignPtr () -> r -> JanusCEval r

jcparam :: forall t r. (JanusCParam t r) => String -> Int -> JanusCM [Param] -> r -> [JCFunc]
jcparam = jcparamWith (lowerBound @(CTargetFeatures t))

{-# NOINLINE evalPureJanusCFFI #-}
evalPureJanusCFFI :: ForeignPtr () -> (FunPtr () -> IO a) -> a
evalPureJanusCFFI dlFPtr k = unsafePerformIO $
  withForeignPtr dlFPtr $ \p ->
    k (castPtrToFunPtr p)

mkTargetFuncInfo :: forall t. (JanusCTarget t) => Proxy t -> CTargetFeatures t -> String -> JCFuncInfo
mkTargetFuncInfo pt fs name =
  setTargetFeature (cTargetFeatureKey pt) fs (mkJCFuncInfo name JC False)

instance (JanusCTarget t, JanusCABIRet t a) => JanusCParam t (JanusCT t a) where
  jcparamWith fs name _ mparams a =
    runJCCodegen (mkTargetFuncInfo (Proxy @t) fs name) $ do
      ps <- mparams
      extra <- jcFinishABIRet (Proxy @t) a
      modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps <> extra) False noLoc
  jcTHSpec _ = ([], jcABIRetTHSpec (Proxy @t) THRetPure (Proxy @a))
  jcSignatureTypes _ = ([], [typeRep (Proxy @a)])

instance (JanusCTarget t, JanusCABIRet t a) => JanusCEvaluate t (JanusCT t a) where
  jcevalCont withArgs dlFPtr _ =
    evalPureJanusCFFI dlFPtr $ \fp ->
      withArgs (jcCallABIRet (Proxy @t) (Proxy @a) fp)

instance (JanusCTarget t, JanusCValRet t res) => JanusCParam t (JanusCMT t res) where
  jcparamWith fs name _ mparams ma =
    runJCCodegen (mkTargetFuncInfo (Proxy @t) fs name) $ do
      ps <- mparams
      res <- unJanusCMT ma
      extra <- jcFinishValRet (Proxy @t) res
      modify $ \s -> s & ix name . jcfParams .~ Params (reverse ps <> extra) False noLoc
  jcTHSpec _ = ([], jcTHValRetSpec (Proxy @t) THRetIO (Proxy @res))
  jcSignatureTypes _ = ([], jcRetSignatureTypes (Proxy @t) (Proxy @res))

instance (JanusCTarget t, JanusCValRet t res) => JanusCEvaluate t (JanusCMT t res) where
  jcevalCont withArgs dlFPtr _ =
    withForeignPtr dlFPtr $ \p ->
      withArgs (jcCallValRet (Proxy @t) (Proxy @res) (castPtrToFunPtr p))

instance (JanusCArg t p, JanusCParam t r) => JanusCParam t (p -> r) where
  jcparamWith fs name n mparams f =
    let (n', mkPs, pExpr) = jcEmitArg @t @p n
        mparams' = do
          ps <- mparams
          emitted <- mkPs
          pure (reverse emitted <> ps)
     in jcparamWith fs name n' mparams' (f pExpr)
  jcTHSpec _ =
    let (ps, retSpec) = jcTHSpec @t @r (Proxy @r)
     in (jcTHParamSpecs (Proxy @p) <> ps, retSpec)
  jcSignatureTypes _ =
    let (inRest, outTys) = jcSignatureTypes @t @r (Proxy @r)
     in (jcArgSignatureTypes @t @p (Proxy @p) <> inRest, outTys)

instance (JanusCArg t p, JanusCEvaluate t r) => JanusCEvaluate t (p -> r) where
  jcevalCont ::
    (([Arg] -> IO (JanusCRetType r)) -> IO (JanusCRetType r)) ->
    ForeignPtr () ->
    (p -> r) ->
    JanusCHost p ->
    JanusCEval r
  jcevalCont withArgs dlFPtr f valHs =
    jcevalCont @t @r
      (\k -> withArgs (\as -> jcWithHostArg @t @p (Proxy @p) valHs as k))
      dlFPtr
      (f (jcDummyArg @t @p))

--------------------------------------------------------------------------------
-- Raw (Un-CSE'd) Compilation, Inspection & Evaluation Entrypoints
--------------------------------------------------------------------------------

resolveCTargetFeatures :: forall t. (JanusCTarget t) => Proxy t -> CBuildConfig -> IO (CTargetFeatures t)
resolveCTargetFeatures pt config =
  case config ^. cBuildConfigTargetFeature (cTargetFeatureKey pt) of
    Just fs -> pure fs
    Nothing -> observeCTargetFeatures pt

prepareRawJanusCUnit ::
  forall t r.
  (JanusCParam t r) =>
  CTargetFeatures t ->
  String ->
  r ->
  IO (String, [String], [String])
prepareRawJanusCUnit reqFeatures name prog = do
  let pt = Proxy @t
      mangledName = jcMangledSymbol @t @r (Proxy @r) name
      funcs = jcparamWith reqFeatures mangledName 0 (pure []) prog
      cSource = cTargetBanner pt reqFeatures <> renderJCProgram mangledName funcs
  (cflags, ldflags) <- cTargetBuildFlags pt reqFeatures funcs
  pure (cSource, cflags, ldflags)

showRawJanusCWith :: forall t r. (JanusCParam t r) => CTargetFeatures t -> String -> r -> String
showRawJanusCWith fs name a =
  let mangledName = jcMangledSymbol @t @r (Proxy @r) name
      jcs = jcparamWith fs mangledName 0 (pure []) a
      banner = cTargetBanner (Proxy @t) fs
      sep = "\n" <> replicate 80 '-' <> "\n"
   in banner <> foldMap ((<> sep) . renderJCFunc) jcs

showRawJanusC :: forall t r. (JanusCParam t r) => String -> r -> IO String
showRawJanusC name a = do
  fs <- observeCTargetFeatures (Proxy @t)
  pure (showRawJanusCWith fs name a)

printRawJanusCWith :: (JanusCParam t r) => CTargetFeatures t -> r -> IO ()
printRawJanusCWith fs = putStrLn . showRawJanusCWith fs "janus_main"

printRawJanusC :: (JanusCParam t r) => r -> IO ()
printRawJanusC a = showRawJanusC "janus_main" a >>= putStrLn

acquireRawJanusCNamedWith ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r, ForeignPtr ())
acquireRawJanusCNamedWith name reqFeatures config a = do
  let pt = Proxy @t
  ensureCTargetAdmitted pt reqFeatures
  let dir = config ^. cBuildConfigCacheDir
      mangledName = jcMangledSymbol @t @r (Proxy @r) name
      funcs = jcparamWith reqFeatures mangledName 0 (pure []) a
  (extraCFlags, extraLDFlags) <- cTargetBuildFlags pt reqFeatures funcs
  files <- writeJanusCFiles JC dir funcs
  dlFPtr <- acquireJanusCFunPtrWithConfig config extraCFlags extraLDFlags files mangledName
  pure (jcevalCont ($ []) dlFPtr a, dlFPtr)

acquireRawJanusCWith ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r, ForeignPtr ())
acquireRawJanusCWith = acquireRawJanusCNamedWith "janus_main"

acquireRawJanusCNamed ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r, ForeignPtr ())
acquireRawJanusCNamed name config a = do
  fs <- resolveCTargetFeatures (Proxy @t) config
  acquireRawJanusCNamedWith name fs config a

acquireRawJanusC ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  CBuildConfig ->
  r ->
  IO (JanusCEval r, ForeignPtr ())
acquireRawJanusC = acquireRawJanusCNamed "janus_main"

-- this mostly exists for use with hedgehog withResource; the DL is closed by
-- the ForeignPtr finalizer once all closures/thunks referencing it become unreachable
releaseJanusC :: (a, ForeignPtr ()) -> IO ()
releaseJanusC _ = pure ()

compileRawJanusCNamedWith ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r)
compileRawJanusCNamedWith name reqFeatures config a =
  fst <$> acquireRawJanusCNamedWith name reqFeatures config a

compileRawJanusCWith ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r)
compileRawJanusCWith = compileRawJanusCNamedWith "janus_main"

compileRawJanusCNamed ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  IO (JanusCEval r)
compileRawJanusCNamed name config a =
  fst <$> acquireRawJanusCNamed name config a

compileRawJanusC ::
  forall t r.
  (JanusCParam t r, JanusCEvaluate t r) =>
  CBuildConfig ->
  r ->
  IO (JanusCEval r)
compileRawJanusC = compileRawJanusCNamed "janus_main"

withRawJanusCNamedWith ::
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  (JanusCEval r -> IO a) ->
  IO a
withRawJanusCNamedWith name fs config a k =
  compileRawJanusCNamedWith name fs config a >>= k

withRawJanusCWith ::
  (JanusCParam t r, JanusCEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  (JanusCEval r -> IO a) ->
  IO a
withRawJanusCWith = withRawJanusCNamedWith "janus_main"

withRawJanusCNamed ::
  (JanusCParam t r, JanusCEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  (JanusCEval r -> IO a) ->
  IO a
withRawJanusCNamed name config a k =
  compileRawJanusCNamed name config a >>= k

withRawJanusC ::
  (JanusCParam t r, JanusCEvaluate t r) =>
  CBuildConfig ->
  r ->
  (JanusCEval r -> IO a) ->
  IO a
withRawJanusC = withRawJanusCNamed "janus_main"

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
      bracketOnError
        (openTempFile dir ("janus_src" <> backendExt backend))
        (\(tmpPath, hndl) -> hClose hndl >> removeLink tmpPath)
        $ \(tmpPath, hndl) -> do
          hPutStrLn hndl body
          hClose hndl
          rename tmpPath path
    pure filename

getFunction :: JanusCMT t JCFunc
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
getJanusCType :: (Typeable a, JanusCTyped a) => Proxy a -> JanusCMT t JCType
getJanusCType p = do
  fname <- askFuncName
  f <- getFunction
  let tyrep = typeRep p
  case Map.lookup tyrep (f ^. jcfTypeCache) of
    Just t -> pure t
    Nothing -> do
      t@(JCType spec dec) <- liftJanusCM (jctype p)
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
          -- have to get this again because jctype can insert headers
          fUpdated <- getFunction
          let specRendered = pretty 120 (ppr spec)
              specSha = SHA256.hash (Text.encodeUtf8 (Text.pack specRendered))
              tyname = "type_" <> take 16 (unpack (B16.encode specSha))
              tyId = Id tyname noLoc
              spec' = DeclSpec [] [] (Tnamed tyId [] noLoc) noLoc
              t' = JCType spec' dec
              td = JCTypedef spec tyname
              f' =
                fUpdated
                  & jcfTypeCache %~ Map.insert tyrep t'
                  & jcfTypedefs %~ (\tds -> if td `elem` tds then tds else tds <> [td])
          modify $ \s -> s & at fname ?~ f'
          pure t'

addHeader :: String -> JanusCMT t ()
addHeader header = do
  fname <- askFuncName
  modify $ \s -> s & ix fname . jcfHeaders %~ Set.insert header

-- | Bind an expression to a fresh SSA-style local variable in the current C
-- function block and return its variable reference expression.
emitLocalVal :: JCType -> Exp -> JanusCMT t Exp
emitLocalVal (JCType spec dec) rhs = do
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      vid = Id ("x_" <> show c) noLoc
      ini = Init vid dec Nothing (Just $ ExpInitializer rhs noLoc) [] noLoc
      decl = BlockDecl $ InitGroup spec [] [ini] noLoc
      fn' =
        fn
          & jcfBlock %~ flip appendBlock [decl]
          & jcfVarCounter +~ 1
  modify $ \st -> st & ix fname .~ fn'
  pure (Var vid noLoc)

--------------------------------------------------------------------------------
-- Scalar C Type & Literal Instances
--------------------------------------------------------------------------------

instance JanusCTyped Bool where
  jctype _ = do
    addHeader "stdbool.h"
    pure $ JCType (DeclSpec [] [] (Tnamed (Id "bool" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CBool where
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
  jctype _ = do
    addHeader "stddef.h"
    pure $ JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

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

instance JanusCTyped CFloat where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tfloat noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped Double where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tdouble noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped CDouble where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tdouble noLoc) noLoc) (DeclRoot noLoc)

instance (JanusCTyped a) => JanusCTyped (Ptr a) where
  jctype _ =
    getJanusCType (Proxy @a) >>= \case
      JCType spec dec -> pure $ JCType spec (Ptr [] dec noLoc)

voidPtrType :: JCType
voidPtrType = JCType (DeclSpec [] [] (Tvoid noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)

instance JanusCTyped CFile where
  jctype _ = do
    addHeader "stdio.h"
    pure $ JCType (DeclSpec [] [] (Tnamed (Id "FILE" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped (Complex Float) where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tfloat_Complex noLoc) noLoc) (DeclRoot noLoc)

instance JanusCTyped (Complex Double) where
  jctype _ = pure $ JCType (DeclSpec [] [] (Tdouble_Complex noLoc) noLoc) (DeclRoot noLoc)

class JanusLitC a where
  jlitc :: a -> JanusCM Exp

signedIntConst :: (Show a, Integral a) => a -> Exp
signedIntConst a
  | toInteger a <= toInteger (minBound :: Int64) =
      BinOp
        Sub
        (C.Const (IntConst "-9223372036854775807LL" Signed (-9223372036854775807) noLoc) noLoc)
        (C.Const (IntConst "1LL" Signed 1 noLoc) noLoc)
        noLoc
  | otherwise = C.Const (IntConst (show a) Signed (toInteger a) noLoc) noLoc

unsignedIntConst :: (Show a, Integral a) => a -> Exp
unsignedIntConst a
  | toInteger a > toInteger (maxBound :: Int64) =
      C.Const (IntConst (show a <> "ULL") Unsigned (toInteger a) noLoc) noLoc
  | otherwise = C.Const (IntConst (show a) Unsigned (toInteger a) noLoc) noLoc

signedIntConst64 :: (Show a, Integral a) => a -> Exp
signedIntConst64 a
  | toInteger a <= toInteger (minBound :: Int64) =
      BinOp
        Sub
        (C.Const (IntConst "-9223372036854775807LL" Signed (-9223372036854775807) noLoc) noLoc)
        (C.Const (IntConst "1LL" Signed 1 noLoc) noLoc)
        noLoc
  | otherwise = C.Const (IntConst (show a <> "LL") Signed (toInteger a) noLoc) noLoc

unsignedIntConst32 :: (Show a, Integral a) => a -> Exp
unsignedIntConst32 a =
  C.Const (IntConst (show a <> "U") Unsigned (toInteger a) noLoc) noLoc

unsignedIntConst64 :: (Show a, Integral a) => a -> Exp
unsignedIntConst64 a =
  C.Const (IntConst (show a <> "ULL") Unsigned (toInteger a) noLoc) noLoc

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
  jlitc = pure . signedIntConst64

instance JanusLitC CLLong where
  jlitc = pure . signedIntConst64

instance JanusLitC CSize where
  jlitc = pure . unsignedIntConst64

instance JanusLitC Int8 where
  jlitc = pure . signedIntConst

instance JanusLitC Int16 where
  jlitc = pure . signedIntConst

instance JanusLitC Int32 where
  jlitc = pure . signedIntConst

instance JanusLitC Int64 where
  jlitc = pure . signedIntConst64

instance JanusLitC CUChar where
  jlitc = pure . unsignedIntConst

instance JanusLitC CUShort where
  jlitc = pure . unsignedIntConst

instance JanusLitC CUInt where
  jlitc = pure . unsignedIntConst32

instance JanusLitC CULong where
  jlitc = pure . unsignedIntConst64

instance JanusLitC CULLong where
  jlitc = pure . unsignedIntConst64

instance JanusLitC Word8 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word16 where
  jlitc = pure . unsignedIntConst

instance JanusLitC Word32 where
  jlitc = pure . unsignedIntConst32

instance JanusLitC Word64 where
  jlitc = pure . unsignedIntConst64

instance JanusLitC CBool where
  jlitc = pure . unsignedIntConst

instance JanusLitC Float where
  jlitc a = pure $ C.Const (FloatConst (show a) a noLoc) noLoc

instance JanusLitC CFloat where
  jlitc (CFloat a) = jlitc a

instance JanusLitC Double where
  jlitc a = pure $ C.Const (DoubleConst (show a) a noLoc) noLoc

instance JanusLitC CDouble where
  jlitc (CDouble a) = jlitc a

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

--------------------------------------------------------------------------------
-- Portable C Operators, Math, & Semilattice Instances on JanusCT t
--------------------------------------------------------------------------------

janusCUnOp :: UnOp -> JanusCT t a -> JanusCT t a
janusCUnOp unop (JanusCT a) = JanusCT $ do
  RVal a' <- a
  pure . RVal $ UnOp unop a' noLoc

janusCBinOp ::
  BinOp -> JanusCT t a -> JanusCT t a -> JanusCT t a
janusCBinOp binop (JanusCT a) (JanusCT b) = JanusCT $ do
  RVal a' <- a
  RVal b' <- b
  pure $ RVal $ BinOp binop a' b' noLoc

janusCBoolBinOp ::
  forall t a. BinOp -> JanusCT t a -> JanusCT t a -> JanusCT t Bool
janusCBoolBinOp = coerce (janusCBinOp @t @a)

instance (Num a, JanusLitC a) => Num (JanusCT t a) where
  fromInteger n = JanusCT $ do
    n' <- jlitc @a (fromInteger n)
    pure $ RVal n'
  a + b = janusCBinOp Add a b
  a - b = janusCBinOp Sub a b
  a * b = janusCBinOp Mul a b
  negate = janusCUnOp Negate
  abs x = x * signum x
  signum (JanusCT a) = JanusCT $ do
    RVal a' <- a
    zero <- jlitc @a 0
    pure . RVal $
      BinOp
        Sub
        (BinOp Gt a' zero noLoc)
        (BinOp Lt a' zero noLoc)
        noLoc

instance (Bounded a, JanusLitC a) => Bounded (JanusCT t a) where
  minBound = JanusCT (RVal <$> jlitc (minBound :: a))
  maxBound = JanusCT (RVal <$> jlitc (maxBound :: a))

instance (JanusLitC a, Fractional a) => Fractional (JanusCT t a) where
  fromRational a = JanusCT $ do
    a' <- jlitc $ fromRational @a a
    pure $ RVal a'
  a / b = janusCBinOp Div a b

class JanusCFFICall r where
  jcffi :: Maybe String -> String -> JanusCM [Exp] -> r

instance JanusCFFICall (JanusCT t a) where
  jcffi mheader name args = JanusCT $ do
    args' <- args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc

instance (JanusCTyped a) => JanusCFFICall (JanusCMT t (JanusCT t a)) where
  jcffi mheader name args = do
    args' <- liftJanusCM args
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
    pure $ JanusCT $ pure $ RVal $ Var var noLoc

instance {-# OVERLAPS #-} JanusCFFICall (JanusCMT t (JanusCT t (Ptr ()))) where
  jcffi mheader name args = do
    args' <- liftJanusCM args
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
    pure $ JanusCT $ pure $ RVal $ Var var noLoc

instance JanusCFFICall (JanusCMT t ()) where
  jcffi mheader name args = do
    args' <- liftJanusCM args
    case mheader of
      Just h -> modifyFunction $ \fn -> fn & jcfHeaders %~ Set.insert h
      Nothing -> pure ()
    let fncall = FnCall (Var (Id name noLoc) noLoc) (reverse args') noLoc
    modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm $ Exp (Just fncall) noLoc]

instance (JanusCFFICall r) => JanusCFFICall (JanusCT t a -> r) where
  jcffi mheader name args a = jcffi mheader name args''
    where
      args'' = do
        RVal arg' <- getJanusCT a
        args' <- args
        pure (arg' : args')

janusCFFICall :: (JanusCFFICall r) => Maybe String -> String -> r
janusCFFICall header name = jcffi header name (pure [])

unMathFun :: String -> JanusCT t a -> JanusCT t a
unMathFun = janusCFFICall (Just "math.h")

binMathFun :: String -> JanusCT t a -> JanusCT t a -> JanusCT t a
binMathFun = janusCFFICall (Just "math.h")

instance Floating (JanusCT t Float) where
  pi = JanusCT $ do
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

instance Floating (JanusCT t Double) where
  pi = JanusCT $ do
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

deriving via (JanusCT t Float) instance Floating (JanusCT t CFloat)

deriving via (JanusCT t Double) instance Floating (JanusCT t CDouble)

instance Floating (JanusCT t (Complex Float)) where
  pi = JanusCT $ do
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

instance Floating (JanusCT t (Complex Double)) where
  pi = JanusCT $ do
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

instance Join (JanusCT t Bool) where
  p \/ q = janusCBinOp Lor p q

instance Lower (JanusCT t Bool) where
  lowerBound = JanusCT $ do
    addHeader "stdbool.h"
    pure . RVal $ Var (Id "false" noLoc) noLoc

instance Meet (JanusCT t Bool) where
  p /\ q = janusCBinOp Land p q

instance Upper (JanusCT t Bool) where
  upperBound = JanusCT $ do
    addHeader "stdbool.h"
    pure . RVal $ Var (Id "true" noLoc) noLoc
