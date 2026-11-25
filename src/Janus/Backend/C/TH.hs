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

    -- * Low-Level Declaration Builder, Spec Helpers & Validation
    scalarParamSpec,
    scalarRetSpec,
    typeRepToTHType,
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
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.CSE (CSETarget)
import Janus.FFI.Arg
  ( THNat1,
    THNat16,
    THNat2,
    THNat32,
    THNat4,
    THNat64,
    THNat8,
    THParamMode (..),
    THParamSpec (..),
    boolToWord8,
    typeRepToTHType,
    unitTHType,
    voidPtrTHType,
  )
import Janus.FFI.Ret
  ( THRetEffect (..),
    THRetMode (..),
    THRetSpec (..),
    word8ToBool,
  )
import Language.Haskell.TH
  ( Body (NormalB),
    Callconv (CCall),
    Clause (Clause),
    Dec (ForeignD, FunD, PragmaD, SigD, ValD),
    Exp (AppE, LamE, VarE),
    Foreign (ImportF),
    Inline (Inline),
    Pat (VarP),
    Phases (AllPhases),
    Pragma (InlineP),
    Q,
    RuleMatch (FunLike),
    Safety (Interruptible, Safe, Unsafe),
    Type (AppT, ArrowT, ConT),
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
  )
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Unified Target-Polymorphic JanusCTH Class & Blanket Instance
--------------------------------------------------------------------------------

class JanusCTH r where
  type JanusCTHEval r :: K.Type
  janusCTHPrepare :: CBuildConfig -> String -> r -> IO (String, [String])
  janusCTHSpec :: Proxy r -> ([THParamSpec], THRetSpec)

instance (JanusCCSEParam t r) => JanusCTH r where
  type JanusCTHEval r = JanusCCSEEval r
  janusCTHPrepare config cSymName prog = do
    fs <- resolveCTargetFeatures (Proxy @t) config
    (cSrc, cflags, _ldflags) <- prepareJanusCUnit fs cSymName prog
    pure (cSrc, cflags)
  janusCTHSpec _ = jcTHSpec @t @(CSETarget r) (Proxy @(CSETarget r))

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
  (cSource, extraCFlags) <- runIO (janusCTHPrepare config cSymName prog)
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
-- flags, target features, or 'Rock' object caching).
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
