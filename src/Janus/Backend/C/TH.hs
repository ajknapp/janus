{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.TH
  ( -- * Foreign Call Safety (re-exported from "Language.Haskell.TH")
    Safety (Unsafe, Safe, Interruptible),

    -- * Unified Template Haskell Compilation & Linking Class
    JanusCTH (..),
    THBackend (..),
    THParamMode (..),
    THParamSpec (..),
    THRetEffect (..),
    THRetMode (..),
    THRetSpec (..),

    -- * Top-Level Declaration Splices
    declareJanusC,
    declareJanusCAs,
    declareJanusCWithConfig,
    declareJanusCAsWithConfig,

    -- * Expression Splices
    spliceJanusC,
    spliceJanusCWithConfig,

    -- * Low-Level Declaration Builder & Validation
    buildForeignAndWrapperDecls,
    checkCSymbolName,
    checkHsVarName,
    validateCSymbolName,
    validateHsVarName,

    -- * Internal Runtime Marshalling Helpers (used by generated wrappers)
    THNat1,
    THNat2,
    THNat4,
    THNat8,
    THNat16,
    THNat32,
    THNat64,
    boolToWord8,
    word8ToBool,
    spmdFromListProxy,
    spmdToListProxy,

    -- * Re-exported C Newtype Constructors (for FFI unwrapping convenience)
    CBool (..),
    CChar (..),
    CSChar (..),
    CUChar (..),
    CShort (..),
    CUShort (..),
    CInt (..),
    CUInt (..),
    CLong (..),
    CULong (..),
    CLLong (..),
    CULLong (..),
    CSize (..),
    CFloat (..),
    CDouble (..),
    CWchar (..),
  )
where

import Control.Lens ((^.))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (TypeRep, Typeable, eqT, splitTyConApp, tyConModule, tyConName, tyConPackage, typeRep)
import Data.Word (Word8)
import Foreign.C.Types
  ( CBool (..),
    CChar (..),
    CDouble (..),
    CFloat (..),
    CInt (..),
    CLLong (..),
    CLong (..),
    CSChar (..),
    CShort (..),
    CSize (..),
    CUChar (..),
    CUInt (..),
    CULLong (..),
    CULong (..),
    CUShort (..),
    CWchar (..),
  )
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, natVal)
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Backend.C.LinuxX86_64
import Janus.Backend.C.Target
import Janus.FFI.Arg (FFIArg)
import Janus.FFI.Ret (FFIRet)
import Janus.SPMD.Host (HostSPMDValue (..))
import Janus.SPMD.Typed (SPMDTyped)
import Janus.SPMD.Types (SPMD (..), SPMDM (..), SPMDRep, SPMDUnit (..), VoidPtr (..))
import Language.C.Quote (Param)
import Language.Haskell.TH
  ( Body (NormalB),
    Callconv (CCall),
    Clause (Clause),
    Dec (ForeignD, FunD, PragmaD, SigD, ValD),
    Exp (AppE, ConE, LamE, SigE, VarE),
    Foreign (ImportF),
    Inline (Inline),
    Pat (VarP),
    Phases (AllPhases),
    Pragma (InlineP),
    Q,
    RuleMatch (FunLike),
    Safety (Interruptible, Safe, Unsafe),
    TyLit (NumTyLit),
    Type (AppT, ArrowT, ConT, ListT, LitT, PromotedT, TupleT),
    mkName,
    newName,
    runIO,
    varE,
  )
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
  ( ForeignSrcLang (RawObject),
    addForeignFilePath,
    addTempFile,
    addTopDecls,
    mkNameG_d,
    mkNameG_tc,
  )
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Internal Type Synonyms & Inline Helpers for Extension-Free Splice Sites
--------------------------------------------------------------------------------

type THNat1 = 1

type THNat2 = 2

type THNat4 = 4

type THNat8 = 8

type THNat16 = 16

type THNat32 = 32

type THNat64 = 64

{-# INLINE boolToWord8 #-}
boolToWord8 :: Bool -> Word8
boolToWord8 b = if b then 1 else 0

{-# INLINE word8ToBool #-}
word8ToBool :: Word8 -> Bool
word8ToBool b = b /= 0

{-# INLINE spmdFromListProxy #-}
spmdFromListProxy ::
  forall n e a.
  (HostSPMDValue n e a) =>
  Proxy n ->
  Proxy e ->
  Proxy a ->
  [a] ->
  SPMDRep n e a
spmdFromListProxy _ _ _ = spmdRawFromList @n @e @a

{-# INLINE spmdToListProxy #-}
spmdToListProxy ::
  forall n e a.
  (HostSPMDValue n e a) =>
  Proxy n ->
  Proxy e ->
  Proxy a ->
  SPMDRep n e a ->
  [a]
spmdToListProxy _ _ _ = spmdRawToList @n @e @a

--------------------------------------------------------------------------------
-- TypeRep -> TH.Type Conversion
--------------------------------------------------------------------------------

natToTHType :: Integer -> TH.Type
natToTHType 1 = ConT ''THNat1
natToTHType 2 = ConT ''THNat2
natToTHType 4 = ConT ''THNat4
natToTHType 8 = ConT ''THNat8
natToTHType 16 = ConT ''THNat16
natToTHType 32 = ConT ''THNat32
natToTHType 64 = ConT ''THNat64
natToTHType n = LitT (NumTyLit n)

typeRepToTHType :: TypeRep -> TH.Type
typeRepToTHType tr =
  let (tc, args) = splitTyConApp tr
      pkg = tyConPackage tc
      modl = tyConModule tc
      nm = tyConName tc
      argTys = map typeRepToTHType args
      applyAll hd = foldl AppT hd argTys
   in case nm of
        "Unit" | null argTys -> TupleT 0
        "()" | null argTys -> TupleT 0
        "List" -> applyAll ListT
        "[]" -> applyAll ListT
        "->" -> applyAll ArrowT
        "FUN" | [_, a, b] <- argTys -> AppT (AppT ArrowT a) b
        _
          | not (null nm) && all isDigit nm && null argTys ->
              natToTHType (read nm)
          | '\'' : rest <- nm,
            not (null rest) ->
              applyAll (PromotedT (mkNameG_d pkg modl rest))
          | otherwise ->
              applyAll (ConT (mkNameG_tc pkg modl nm))

voidPtrTHType :: TH.Type
voidPtrTHType = AppT (ConT ''Ptr) (TupleT 0)

unitTHType :: TH.Type
unitTHType = TupleT 0

--------------------------------------------------------------------------------
-- TH ABI Parameter & Return Specifications
--------------------------------------------------------------------------------

data THBackend
  = THBackendJanusC
  | THBackendLinuxX86_64
  deriving (Eq, Ord, Show)

data THParamMode
  = THParamDirect
  | THParamPure (TH.Exp -> TH.Exp)
  | THParamIO ((TH.Exp -> Q TH.Exp) -> TH.Exp -> Q TH.Exp)

data THParamSpec = THParamSpec
  { thParamUserType :: TH.Type,
    thParamRawType :: TH.Type,
    thParamMode :: THParamMode
  }

data THRetEffect
  = THRetPure
  | THRetIO
  deriving (Eq, Ord, Show)

data THRetMode
  = THRetDirect TH.Type
  | THRetPureMap TH.Type (TH.Exp -> TH.Exp)
  | THRetOutPtr ((TH.Exp -> Q TH.Exp) -> Q TH.Exp)

data THRetSpec = THRetSpec
  { thRetEffect :: THRetEffect,
    thRetUserBaseType :: TH.Type,
    thRetMode :: THRetMode
  }

scalarParamSpec :: forall a. (Typeable a) => Proxy a -> THParamSpec
scalarParamSpec p =
  let userTy = typeRepToTHType (typeRep p)
   in case eqT @a @Bool of
        Just Refl ->
          THParamSpec
            { thParamUserType = userTy,
              thParamRawType = ConT ''Word8,
              thParamMode = THParamPure (AppE (VarE 'boolToWord8))
            }
        Nothing -> case eqT @a @VoidPtr of
          Just Refl ->
            THParamSpec
              { thParamUserType = userTy,
                thParamRawType = voidPtrTHType,
                thParamMode = THParamPure (AppE (VarE 'getVoidPtr))
              }
          Nothing -> case eqT @a @SPMDUnit of
            Just Refl ->
              THParamSpec
                { thParamUserType = userTy,
                  thParamRawType = ConT ''Word8,
                  thParamMode = THParamPure (AppE (VarE 'getSPMDUnit))
                }
            Nothing ->
              THParamSpec
                { thParamUserType = userTy,
                  thParamRawType = userTy,
                  thParamMode = THParamDirect
                }

scalarRetSpec :: forall a. (Typeable a) => THRetEffect -> Proxy a -> THRetSpec
scalarRetSpec eff p =
  let userTy = typeRepToTHType (typeRep p)
   in case eqT @a @Bool of
        Just Refl ->
          THRetSpec
            { thRetEffect = eff,
              thRetUserBaseType = userTy,
              thRetMode = THRetPureMap (ConT ''Word8) (AppE (VarE 'word8ToBool))
            }
        Nothing -> case eqT @a @VoidPtr of
          Just Refl ->
            THRetSpec
              { thRetEffect = eff,
                thRetUserBaseType = userTy,
                thRetMode = THRetPureMap voidPtrTHType (AppE (ConE 'VoidPtr))
              }
          Nothing -> case eqT @a @SPMDUnit of
            Just Refl ->
              THRetSpec
                { thRetEffect = eff,
                  thRetUserBaseType = userTy,
                  thRetMode = THRetPureMap (ConT ''Word8) (AppE (ConE 'SPMDUnit))
                }
            Nothing ->
              THRetSpec
                { thRetEffect = eff,
                  thRetUserBaseType = userTy,
                  thRetMode = THRetDirect userTy
                }

x86THParamSpec :: forall a. (LinuxX86_64ABIParam a) => Proxy a -> THParamSpec
x86THParamSpec p
  | x86ParamIsIndirect p =
      THParamSpec
        { thParamUserType = typeRepToTHType (typeRep p),
          thParamRawType = voidPtrTHType,
          thParamMode = THParamIO $ \k argExp -> do
            pName <- newName "p_arg"
            inner <- k (VarE pName)
            pure $
              AppE
                (AppE (VarE 'withX86StorableArg) argExp)
                (LamE [VarP pName] inner)
        }
  | otherwise = scalarParamSpec p

x86THRetSpec :: forall a. (LinuxX86_64ABIRet a, Typeable a) => THRetEffect -> Proxy a -> THRetSpec
x86THRetSpec eff p
  | x86RetIsIndirect p =
      THRetSpec
        { thRetEffect = eff,
          thRetUserBaseType = typeRepToTHType (typeRep p),
          thRetMode = THRetOutPtr $ \callWithOut -> do
            pOutName <- newName "p_out"
            callExp <- callWithOut (VarE pOutName)
            pure $ AppE (VarE 'withX86StorableRet) (LamE [VarP pOutName] callExp)
        }
  | otherwise = scalarRetSpec eff p

spmdProxyExps :: forall n a. (KnownNat n, Typeable a) => (TH.Exp, TH.Exp, TH.Exp)
spmdProxyExps =
  let mkProxy ty = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) ty)
      pN = mkProxy (natToTHType (toInteger (natVal (Proxy @n))))
      pE = mkProxy (ConT ''LinuxX86_64C)
      pA = mkProxy (typeRepToTHType (typeRep (Proxy @a)))
   in (pN, pE, pA)

spmdTHParamSpec ::
  forall n a.
  ( KnownNat n,
    Typeable a,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  THParamSpec
spmdTHParamSpec =
  let (pN, pE, pA) = spmdProxyExps @n @a
      fromListCall = AppE (AppE (AppE (VarE 'spmdFromListProxy) pN) pE) pA
      repSpec = x86THParamSpec (Proxy @(SPMDRep n LinuxX86_64C a))
      userTy = AppT ListT (typeRepToTHType (typeRep (Proxy @a)))
      mode = case thParamMode repSpec of
        THParamDirect -> THParamPure (AppE fromListCall)
        THParamPure conv -> THParamPure (conv . AppE fromListCall)
        THParamIO withIO -> THParamIO (\k xsExp -> withIO k (AppE fromListCall xsExp))
   in THParamSpec
        { thParamUserType = userTy,
          thParamRawType = thParamRawType repSpec,
          thParamMode = mode
        }

spmdTHRetSpec ::
  forall n a.
  ( SPMDTyped n LinuxX86_64C a,
    Typeable a,
    LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  THRetEffect ->
  THRetSpec
spmdTHRetSpec eff =
  let (pN, pE, pA) = spmdProxyExps @n @a
      toListFn = AppE (AppE (AppE (VarE 'spmdToListProxy) pN) pE) pA
      repSpec = x86THRetSpec eff (Proxy @(SPMDRep n LinuxX86_64C a))
      userTy = AppT ListT (typeRepToTHType (typeRep (Proxy @a)))
      mode = case thRetMode repSpec of
        THRetDirect rawTy -> THRetPureMap rawTy (AppE toListFn)
        THRetPureMap rawTy conv -> THRetPureMap rawTy (AppE toListFn . conv)
        THRetOutPtr runWithOut ->
          THRetOutPtr $ \callWithOut -> do
            ioRepExp <- runWithOut callWithOut
            pure $ AppE (AppE (VarE 'fmap) toListFn) ioRepExp
   in THRetSpec
        { thRetEffect = eff,
          thRetUserBaseType = userTy,
          thRetMode = mode
        }

--------------------------------------------------------------------------------
-- Unified JanusCTH Class & Instances
--------------------------------------------------------------------------------

class JanusCTH r where
  type JanusCTHEval r :: K.Type
  janusCTHBackend :: Proxy r -> THBackend
  janusCTHCodegen :: X86Features -> String -> Int -> JanusCM [Param] -> r -> [JCFunc]
  janusCTHSpec :: Proxy r -> ([THParamSpec], THRetSpec)

-- JanusC / JanusCM Instances

instance (FFIRet a, JanusCTyped a) => JanusCTH (JanusC a) where
  type JanusCTHEval (JanusC a) = a
  janusCTHBackend _ = THBackendJanusC
  janusCTHCodegen _ name n params a = jcparam name n params JC a
  janusCTHSpec _ = ([], scalarRetSpec THRetPure (Proxy @a))

instance (FFIRet a, JanusCTyped a) => JanusCTH (JanusCM (JanusC a)) where
  type JanusCTHEval (JanusCM (JanusC a)) = IO a
  janusCTHBackend _ = THBackendJanusC
  janusCTHCodegen _ name n params a = jcparam name n params JC a
  janusCTHSpec _ = ([], scalarRetSpec THRetIO (Proxy @a))

instance JanusCTH (JanusCM ()) where
  type JanusCTHEval (JanusCM ()) = IO ()
  janusCTHBackend _ = THBackendJanusC
  janusCTHCodegen _ name n params a = jcparam name n params JC a
  janusCTHSpec _ = ([], THRetSpec THRetIO unitTHType (THRetDirect unitTHType))

instance
  ( FFIArg a,
    JanusCTyped a,
    JanusCParam r,
    JanusCParamImpl JanusC JanusCM r,
    JanusCTH r
  ) =>
  JanusCTH (JanusC a -> r)
  where
  type JanusCTHEval (JanusC a -> r) = a -> JanusCTHEval r
  janusCTHBackend _ = THBackendJanusC
  janusCTHCodegen _ name n params f = jcparam name n params JC f
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (scalarParamSpec (Proxy @a) : ps, retSpec)

-- LinuxX86_64C / LinuxX86_64CM Instances

instance (LinuxX86_64ABIRet a, JanusCTyped a) => JanusCTH (LinuxX86_64C a) where
  type JanusCTHEval (LinuxX86_64C a) = a
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], x86THRetSpec THRetPure (Proxy @a))

instance (LinuxX86_64ABIRet a, JanusCTyped a) => JanusCTH (LinuxX86_64CM (LinuxX86_64C a)) where
  type JanusCTHEval (LinuxX86_64CM (LinuxX86_64C a)) = IO a
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], x86THRetSpec THRetIO (Proxy @a))

instance JanusCTH (LinuxX86_64CM ()) where
  type JanusCTHEval (LinuxX86_64CM ()) = IO ()
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], THRetSpec THRetIO unitTHType (THRetDirect unitTHType))

instance
  ( LinuxX86_64ABIParam a,
    LinuxX86_64Param r,
    JanusCTH r
  ) =>
  JanusCTH (LinuxX86_64C a -> r)
  where
  type JanusCTHEval (LinuxX86_64C a -> r) = a -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (x86THParamSpec (Proxy @a) : ps, retSpec)

-- SPMD n LinuxX86_64C Instances

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a,
    Typeable a
  ) =>
  JanusCTH (SPMD n LinuxX86_64C a)
  where
  type JanusCTHEval (SPMD n LinuxX86_64C a) = [a]
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], spmdTHRetSpec @n @a THRetPure)

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a,
    Typeable a
  ) =>
  JanusCTH (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a))
  where
  type JanusCTHEval (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a)) = IO [a]
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], spmdTHRetSpec @n @a THRetIO)

instance JanusCTH (SPMDM n LinuxX86_64CM LinuxX86_64C ()) where
  type JanusCTHEval (SPMDM n LinuxX86_64CM LinuxX86_64C ()) = IO ()
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ = ([], THRetSpec THRetIO unitTHType (THRetDirect unitTHType))

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a,
    Typeable a,
    LinuxX86_64Param r,
    JanusCTH r
  ) =>
  JanusCTH (SPMD n LinuxX86_64C a -> r)
  where
  type JanusCTHEval (SPMD n LinuxX86_64C a -> r) = [a] -> JanusCTHEval r
  janusCTHBackend _ = THBackendLinuxX86_64
  janusCTHCodegen = x86param
  janusCTHSpec _ =
    let (ps, retSpec) = janusCTHSpec (Proxy @r)
     in (spmdTHParamSpec @n @a : ps, retSpec)

--------------------------------------------------------------------------------
-- Identifier Validation, Object Linking, & Declaration Generation
--------------------------------------------------------------------------------

cReservedWords :: Set.Set String
cReservedWords =
  Set.fromList
    [ "auto",
      "break",
      "case",
      "char",
      "const",
      "continue",
      "default",
      "do",
      "double",
      "else",
      "enum",
      "extern",
      "float",
      "for",
      "goto",
      "if",
      "inline",
      "int",
      "long",
      "register",
      "restrict",
      "return",
      "short",
      "signed",
      "sizeof",
      "static",
      "struct",
      "switch",
      "typedef",
      "union",
      "unsigned",
      "void",
      "volatile",
      "while",
      "_Alignas",
      "_Alignof",
      "_Atomic",
      "_Bool",
      "_Complex",
      "_Generic",
      "_Imaginary",
      "_Noreturn",
      "_Static_assert",
      "_Thread_local"
    ]

hsReservedWords :: Set.Set String
hsReservedWords =
  Set.fromList
    [ "_",
      "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "foreign",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "of",
      "then",
      "type",
      "where"
    ]

checkCSymbolName :: String -> Either String ()
checkCSymbolName "" = Left "Janus.Backend.C.TH: C function name must not be empty"
checkCSymbolName s@(c0 : cs)
  | not (isAsciiUpper c0 || isAsciiLower c0 || c0 == '_') =
      Left ("Janus.Backend.C.TH: invalid C function name " <> show s <> " (must start with ASCII letter or '_')")
  | not (all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '_') cs) =
      Left ("Janus.Backend.C.TH: invalid C function name " <> show s <> " (must contain only ASCII alphanumerics or '_')")
  | Set.member s cReservedWords =
      Left ("Janus.Backend.C.TH: C function name " <> show s <> " is a reserved C keyword")
  | otherwise = Right ()

validateCSymbolName :: String -> Q ()
validateCSymbolName = either fail pure . checkCSymbolName

checkHsVarName :: String -> Either String ()
checkHsVarName "" = Left "Janus.Backend.C.TH: Haskell binding name must not be empty"
checkHsVarName s@(c0 : cs)
  | not (isAsciiLower c0 || c0 == '_') =
      Left
        ( "Janus.Backend.C.TH: invalid Haskell variable name "
            <> show s
            <> " (must start with lowercase letter or '_'; use declareJanusCAs for uppercase C symbols)"
        )
  | not (all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '\'') cs) =
      Left ("Janus.Backend.C.TH: invalid Haskell variable name " <> show s)
  | Set.member s hsReservedWords =
      Left ("Janus.Backend.C.TH: Haskell binding name " <> show s <> " is a reserved keyword")
  | otherwise = Right ()

validateHsVarName :: String -> Q ()
validateHsVarName = either fail pure . checkHsVarName

prepareJanusCTHUnit :: forall r. (JanusCTH r) => CBuildConfig -> String -> r -> IO (String, [String])
prepareJanusCTHUnit config cSymName prog =
  case janusCTHBackend (Proxy @r) of
    THBackendJanusC ->
      let funcs = janusCTHCodegen x86Baseline cSymName 0 (pure []) prog
       in pure (renderJCProgram cSymName funcs, [])
    THBackendLinuxX86_64 -> do
      fs <- case config ^. cBuildConfigX86Features of
        Just req -> pure req
        Nothing -> observeHostX86Features
      let funcs = janusCTHCodegen fs cSymName 0 (pure []) prog
          targetBanner = "/* janus-x86-target: " <> unwords (x86CompilerFlags fs) <> " */\n"
          cSource = targetBanner <> renderJCProgram cSymName funcs
          needsSleef = any (Set.member "sleef.h" . _jcfHeaders) funcs
      (sleefCFlags, _) <- if needsSleef then observeSleefFlags else pure ([], [])
      pure (cSource, x86CompilerFlags fs <> sleefCFlags)

linkJanusCObject :: CBuildConfig -> [String] -> String -> Q ()
linkJanusCObject config extraCFlags cSource = do
  objPath <-
    if config ^. cBuildConfigUseTempDir
      then do
        cPath <- addTempFile "c"
        oPath <- addTempFile "o"
        let cc = config ^. cBuildConfigCC
            cflags = (config ^. cBuildConfigCFlags) <> extraCFlags
        runIO $ do
          writeFile cPath cSource
          compileJanusCSourceToObject cc cflags cPath oPath
        pure oPath
      else runIO $ buildCachedJanusObject config extraCFlags cSource
  addForeignFilePath RawObject objPath

mkFunArrows :: [TH.Type] -> TH.Type -> TH.Type
mkFunArrows argTys resTy = foldr (\t acc -> AppT (AppT ArrowT t) acc) resTy argTys

buildForeignAndWrapperDecls ::
  Safety ->
  String ->
  TH.Name ->
  ([THParamSpec], THRetSpec) ->
  Q [Dec]
buildForeignAndWrapperDecls safety cSymName hsName (paramSpecs, retSpec) = do
  let userResType = case thRetEffect retSpec of
        THRetPure -> thRetUserBaseType retSpec
        THRetIO -> AppT (ConT ''IO) (thRetUserBaseType retSpec)
      userFullType = mkFunArrows (map thParamUserType paramSpecs) userResType
      isDirectParam p = case thParamMode p of
        THParamDirect -> True
        _ -> False
      isIOParam p = case thParamMode p of
        THParamIO _ -> True
        _ -> False
      isDirectRet = case thRetMode retSpec of
        THRetDirect _ -> True
        _ -> False
      isOutPtrRet = case thRetMode retSpec of
        THRetOutPtr _ -> True
        _ -> False
  if all isDirectParam paramSpecs && isDirectRet
    then pure [ForeignD (ImportF CCall safety cSymName hsName userFullType)]
    else do
      rawName <- newName ("janus_raw_" <> cSymName)
      argNames <- mapM (\i -> newName ("arg_" <> show i)) [0 .. length paramSpecs - 1]
      let needsIOCall = thRetEffect retSpec == THRetIO || any isIOParam paramSpecs || isOutPtrRet
          rawFullType = case thRetMode retSpec of
            THRetOutPtr _ ->
              mkFunArrows (map thParamRawType paramSpecs <> [voidPtrTHType]) (AppT (ConT ''IO) unitTHType)
            THRetDirect rawBaseTy ->
              let rTy = if needsIOCall then AppT (ConT ''IO) rawBaseTy else rawBaseTy
               in mkFunArrows (map thParamRawType paramSpecs) rTy
            THRetPureMap rawBaseTy _ ->
              let rTy = if needsIOCall then AppT (ConT ''IO) rawBaseTy else rawBaseTy
               in mkFunArrows (map thParamRawType paramSpecs) rTy
      bodyExp <-
        if not needsIOCall
          then do
            let rawArgs =
                  zipWith
                    ( \nm ps -> case thParamMode ps of
                        THParamDirect -> VarE nm
                        THParamPure conv -> conv (VarE nm)
                        THParamIO _ -> error "buildForeignAndWrapperDecls: unexpected THParamIO in pure path"
                    )
                    argNames
                    paramSpecs
                callExp = foldl AppE (VarE rawName) rawArgs
            pure $ case thRetMode retSpec of
              THRetDirect _ -> callExp
              THRetPureMap _ conv -> conv callExp
              THRetOutPtr _ -> error "buildForeignAndWrapperDecls: unexpected THRetOutPtr in pure path"
          else do
            let withPreparedArgs [] k = k []
                withPreparedArgs ((nm, ps) : rest) k =
                  case thParamMode ps of
                    THParamDirect -> withPreparedArgs rest (\es -> k (VarE nm : es))
                    THParamPure conv -> withPreparedArgs rest (\es -> k (conv (VarE nm) : es))
                    THParamIO withIO ->
                      withIO (\rawE -> withPreparedArgs rest (\es -> k (rawE : es))) (VarE nm)
            ioExp <- withPreparedArgs (zip argNames paramSpecs) $ \rawArgs -> do
              let baseCall = foldl AppE (VarE rawName) rawArgs
              case thRetMode retSpec of
                THRetDirect _ -> pure baseCall
                THRetPureMap _ conv -> do
                  rName <- newName "raw_res"
                  pure $ AppE (AppE (VarE 'fmap) (LamE [VarP rName] (conv (VarE rName)))) baseCall
                THRetOutPtr runWithOut ->
                  runWithOut (\outPtrExp -> pure (AppE baseCall outPtrExp))
            pure $ case thRetEffect retSpec of
              THRetIO -> ioExp
              THRetPure -> AppE (VarE 'unsafePerformIO) ioExp
      let bindDec =
            if null argNames
              then ValD (VarP hsName) (NormalB bodyExp) []
              else FunD hsName [Clause (map VarP argNames) (NormalB bodyExp) []]
      pure
        [ ForeignD (ImportF CCall safety cSymName rawName rawFullType),
          SigD hsName userFullType,
          bindDec,
          PragmaD (InlineP hsName Inline FunLike AllPhases)
        ]

compileAndBuildDecls ::
  forall r.
  (JanusCTH r) =>
  CBuildConfig ->
  Safety ->
  String ->
  TH.Name ->
  r ->
  Q [Dec]
compileAndBuildDecls config safety cSymName hsName prog = do
  validateCSymbolName cSymName
  (cSource, extraCFlags) <- runIO (prepareJanusCTHUnit config cSymName prog)
  linkJanusCObject config extraCFlags cSource
  buildForeignAndWrapperDecls safety cSymName hsName (janusCTHSpec (Proxy @r))

--------------------------------------------------------------------------------
-- Public Template Haskell Splices
--------------------------------------------------------------------------------

-- | Compile a Janus program into a C object file linked into the current
-- module and declare a top-level Haskell binding named @name@ backed by a
-- @foreign import ccall@ (using 'Unsafe', 'Safe', or 'Interruptible').
declareJanusC :: (JanusCTH r) => Safety -> String -> r -> Q [Dec]
declareJanusC = declareJanusCWithConfig defaultTHBuildConfig

-- | Like 'declareJanusC', but allows specifying separate C symbol and Haskell
-- binding names (@declareJanusCAs safety cSymbolName hsVarName prog@).
declareJanusCAs :: (JanusCTH r) => Safety -> String -> String -> r -> Q [Dec]
declareJanusCAs = declareJanusCAsWithConfig defaultTHBuildConfig

-- | Like 'declareJanusC', using an explicit 'CBuildConfig' (for custom compiler
-- flags, 'X86Features', or 'Rock' object caching).
declareJanusCWithConfig :: (JanusCTH r) => CBuildConfig -> Safety -> String -> r -> Q [Dec]
declareJanusCWithConfig config safety name =
  declareJanusCAsWithConfig config safety name name

-- | Like 'declareJanusCAs', using an explicit 'CBuildConfig'.
declareJanusCAsWithConfig ::
  (JanusCTH r) =>
  CBuildConfig ->
  Safety ->
  String ->
  String ->
  r ->
  Q [Dec]
declareJanusCAsWithConfig config safety cSymName hsVarName prog = do
  validateHsVarName hsVarName
  compileAndBuildDecls config safety cSymName (mkName hsVarName) prog

-- | Compile a Janus program into a C object file linked into the current
-- module, emit top-level @foreign import ccall@ declarations via 'addTopDecls',
-- and return an expression splice of type @JanusCTHEval r@ referring to the
-- compiled function.
spliceJanusC :: (JanusCTH r) => Safety -> String -> r -> Q Exp
spliceJanusC = spliceJanusCWithConfig defaultTHBuildConfig

-- | Like 'spliceJanusC', using an explicit 'CBuildConfig'.
spliceJanusCWithConfig ::
  forall r.
  (JanusCTH r) =>
  CBuildConfig ->
  Safety ->
  String ->
  r ->
  Q Exp
spliceJanusCWithConfig config safety cSymName prog = do
  hsName <- newName ("janus_th_" <> cSymName)
  decs <- compileAndBuildDecls config safety cSymName hsName prog
  addTopDecls decs
  varE hsName
