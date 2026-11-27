{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Monadic C/GCC/CUDA quasiquoters for writing C-generating Janus instances.
--
-- Adapted directly from "Language.C.Quote.Base" with combined C99, C11, GCC,
-- and CUDA parser extensions, a pre-populated table of standard C/SIMD/CUDA
-- typedef names, and leading @#include \<...\>@ header extraction:
--
-- * 'jcStms' parses C statements/declarations and emits them into the
--   currently active C function's block (type @'MonadJanusC' m => m ()@).
-- * 'jcFuncs' parses a C translation unit containing one or more function
--   definitions (along with typedefs, external declarations, and prototypes)
--   and emits all functions into 'JanusCMState', automatically propagating
--   headers, typedefs, mutual sibling prototypes, caller prototypes, and CUDA
--   @__device__@ qualifiers.
module Janus.Backend.C.Quote
  ( -- * Monadic C Quasiquoters
    jcStms,
    jcFuncs,

    -- * Monadic Codegen Helpers
    freshCId,
    freshCVar,
    jcType,
    getCType,
    captureBlock,
    captureBlock_,

    -- * Low-Level Emitters & Parser Configuration
    emitCBlockItems,
    emitCDefinitions,
    janusCExtensions,
    janusTypenames,
    extractIncludes,

    -- * Re-exported Antiquotation Classes
    ToIdent (..),
    ToConst (..),
    ToExp (..),
  )
where

import Control.Lens ((%~), (&), (+~), (.~), (^.))
import Control.Monad (unless, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets, modify)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.Loc (noLoc)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Janus.Backend.C.Raw
  ( JCFunc (..),
    JCType (..),
    JCTypedef (..),
    JanusCBackend (..),
    JanusCMT,
    JanusCTyped,
    MonadJanusC (..),
    addHeader,
    appendBlock,
    askFuncName,
    dedupTypedefs,
    defaultJCFunc,
    getFunction,
    getJanusCType,
    jcfBlock,
    jcfHeaders,
    jcfProtos,
    jcfTypeCache,
    jcfTypedefs,
    jcfVarCounter,
    jcfiBackend,
    modifyFunction,
    unJanusCMT,
  )
import qualified Language.C.Parser as P
import Language.C.Quote.Base (ToConst (..), ToExp (..), ToIdent (..), quasiquote)
import qualified Language.C.Syntax as C
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH

--------------------------------------------------------------------------------
-- Parser Configuration (C99 + C11 + GCC + CUDA + Standard/SIMD/CUDA Typenames)
--------------------------------------------------------------------------------

-- | C dialect extensions enabled in 'jcStms' and 'jcFuncs'.
janusCExtensions :: [C.Extensions]
janusCExtensions = [C.C99, C.C11, C.Gcc, C.CUDA]

-- | Pre-registered C, x86-64 SIMD, and CUDA typedef names recognized by the
-- C parser without requiring explicit @typename@ annotations.
janusTypenames :: [String]
janusTypenames =
  standardCTypenames <> x86SIMDTypenames <> cudaTypenames
  where
    standardCTypenames =
      [ "bool",
        "size_t",
        "ssize_t",
        "ptrdiff_t",
        "intptr_t",
        "uintptr_t",
        "int8_t",
        "int16_t",
        "int32_t",
        "int64_t",
        "uint8_t",
        "uint16_t",
        "uint32_t",
        "uint64_t",
        "FILE"
      ]
    x86SIMDTypenames =
      [ "__m64",
        "__m128",
        "__m128i",
        "__m128d",
        "__m256",
        "__m256i",
        "__m256d",
        "__m512",
        "__m512i",
        "__m512d"
      ]
    cudaTypenames =
      [ base <> show n
        | base <-
            [ "char",
              "uchar",
              "short",
              "ushort",
              "int",
              "uint",
              "long",
              "ulong",
              "longlong",
              "ulonglong",
              "float",
              "double"
            ],
          n <- [1 .. 4 :: Int]
      ]
        <> ["dim3"]

baseBlockItemsQQ :: QuasiQuoter
baseBlockItemsQQ = quasiquote janusCExtensions janusTypenames P.parseBlockItems

baseUnitQQ :: QuasiQuoter
baseUnitQQ = quasiquote janusCExtensions janusTypenames P.parseUnit

--------------------------------------------------------------------------------
-- Leading `#include <...>` Extraction (Preserving Line Numbers)
--------------------------------------------------------------------------------

-- | Extract leading @#include \<header.h\>@ directives from a quasiquote body,
-- replacing each extracted line with an empty line so parser source locations
-- stay aligned with the original Haskell file.
extractIncludes :: String -> Either String ([String], String)
extractIncludes src = go [] [] (lines src)
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace

    isSingleLineBlockComment s =
      "/*" `isPrefixOf` s && "*/" `isSuffixOf` s && length s >= 4

    isTrailingOk s =
      let ts = trim s
       in null ts || "//" `isPrefixOf` ts || isSingleLineBlockComment ts

    parseIncludeLine t
      | "#" `isPrefixOf` t =
          let afterHash = dropWhile isSpace (drop 1 t)
           in case stripPrefixWord "include" afterHash of
                Just afterInc0 ->
                  let afterInc = dropWhile isSpace afterInc0
                   in case afterInc of
                        '<' : xs ->
                          case break (== '>') xs of
                            (hdr, '>' : trailing)
                              | not (null hdr) && isTrailingOk trailing -> Just (Right hdr)
                            _ -> Just (Left ("Janus.Backend.C.Quote: malformed #include directive: " <> t))
                        '"' : _ ->
                          Just (Left ("Janus.Backend.C.Quote: quoted #include is not supported (use #include <...>): " <> t))
                        _ -> Just (Left ("Janus.Backend.C.Quote: malformed #include directive: " <> t))
                Nothing -> Nothing
      | otherwise = Nothing

    stripPrefixWord kw s
      | kw `isPrefixOf` s =
          case drop (length kw) s of
            rest@(c : _) | isSpace c || c == '<' || c == '"' -> Just rest
            "" -> Just ""
            _ -> Nothing
      | otherwise = Nothing

    go incs acc [] = Right (reverse incs, unlines (reverse acc))
    go incs acc (ln : rest) =
      let t = trim ln
       in if null t || "//" `isPrefixOf` t || isSingleLineBlockComment t
            then go incs (ln : acc) rest
            else case parseIncludeLine t of
              Just (Right hdr) -> go (hdr : incs) ("" : acc) rest
              Just (Left err) -> Left err
              Nothing -> Right (reverse incs, unlines (reverse acc <> (ln : rest)))

isBlankOrComments :: String -> Bool
isBlankOrComments = go
  where
    go [] = True
    go (c : cs) | isSpace c = go cs
    go ('/' : '/' : cs) = go (dropWhile (/= '\n') cs)
    go ('/' : '*' : cs) =
      case dropBlockComment cs of
        Just rest -> go rest
        Nothing -> False
    go _ = False

    dropBlockComment [] = Nothing
    dropBlockComment ('*' : '/' : cs) = Just cs
    dropBlockComment (_ : cs) = dropBlockComment cs

--------------------------------------------------------------------------------
-- Monadic C Quasiquoters
--------------------------------------------------------------------------------

-- | Quasiquoter that parses C99/C11/GCC/CUDA statements and declarations (with
-- optional leading @#include \<...\>@ lines) and generates an action of type
-- @'MonadJanusC' m => m ()@ appending them to the current function's block.
jcStms :: QuasiQuoter
jcStms =
  QuasiQuoter
    { quoteExp = \str ->
        case extractIncludes str of
          Left err -> fail err
          Right (hdrs, body)
            | isBlankOrComments body ->
                [|emitCBlockItems $(TH.lift hdrs) []|]
            | otherwise -> do
                itemsExp <- quoteExp baseBlockItemsQQ body
                [|emitCBlockItems $(TH.lift hdrs) $(pure itemsExp)|],
      quotePat = \_ -> fail "jcStms: pattern quasiquotation is not supported",
      quoteType = \_ -> fail "jcStms: type quasiquotation is not supported",
      quoteDec = \_ -> fail "jcStms: declaration quasiquotation is not supported"
    }

-- | Quasiquoter that parses a C99/C11/GCC/CUDA translation unit containing one
-- or more function definitions (plus optional leading @#include \<...\>@ lines,
-- @typedef@s, and external declarations/prototypes) and generates an action of
-- type @'MonadJanusC' m => m ()@ emitting all functions into 'JanusCMState'.
jcFuncs :: QuasiQuoter
jcFuncs =
  QuasiQuoter
    { quoteExp = \str ->
        case extractIncludes str of
          Left err -> fail err
          Right (hdrs, body)
            | isBlankOrComments body ->
                [|emitCDefinitions $(TH.lift hdrs) []|]
            | otherwise -> do
                defsExp <- quoteExp baseUnitQQ body
                [|emitCDefinitions $(TH.lift hdrs) $(pure defsExp)|],
      quotePat = \_ -> fail "jcFuncs: pattern quasiquotation is not supported",
      quoteType = \_ -> fail "jcFuncs: type quasiquotation is not supported",
      quoteDec = \_ -> fail "jcFuncs: declaration quasiquotation is not supported"
    }

--------------------------------------------------------------------------------
-- Monadic Codegen Helpers
--------------------------------------------------------------------------------

-- | Allocate a fresh C identifier @\<prefix\>_\<n\>@ unique within the current
-- function using '_jcfVarCounter'.
freshCId :: (MonadJanusC m) => String -> m C.Id
freshCId prefix = liftJC $ do
  fn <- getFunction
  let c = fn ^. jcfVarCounter
  modifyFunction (\f -> f & jcfVarCounter +~ 1)
  pure (C.Id (prefix <> "_" <> show c) noLoc)

-- | Like 'freshCId', returning both the 'C.Id' (for @$id:@) and its variable
-- reference 'C.Exp' (for returning as an 'RVal' or splicing with @$exp:@).
freshCVar :: (MonadJanusC m) => String -> m (C.Id, C.Exp)
freshCVar prefix = do
  vid <- freshCId prefix
  pure (vid, C.Var vid noLoc)

-- | Convert a Janus 'JCType' to a @language-c-quote@ 'C.Type' for use with @$ty:@.
jcType :: JCType -> C.Type
jcType (JCType spec dec) = C.Type spec dec noLoc

-- | Resolve a Haskell type's C representation as a 'C.Type' (registering any
-- required headers and struct typedefs in the current function) for use with
-- @$ty:@.
getCType :: forall a m. (JanusCTyped a, MonadJanusC m) => Proxy a -> m C.Type
getCType p = jcType <$> liftJC (getJanusCType p)

-- | Execute a 'JanusCMT' sub-action with an isolated empty statement block,
-- returning its result and the emitted '[C.BlockItem]' while restoring the
-- caller's block and preserving headers, typedefs, prototypes, variable
-- counter updates, and newly emitted helper functions.
captureBlock :: (MonadJanusC m) => JanusCMT t a -> m (a, [C.BlockItem])
captureBlock action = liftJC $ do
  fn0 <- getFunction
  let savedBlock = fn0 ^. jcfBlock
  modifyFunction (\f -> f & jcfBlock .~ mempty)
  res <- unJanusCMT action
  fn1 <- getFunction
  let captured = appEndo (fn1 ^. jcfBlock) []
  modifyFunction (\f -> f & jcfBlock .~ savedBlock)
  pure (res, captured)

-- | Like 'captureBlock' for unit-returning actions.
captureBlock_ :: (MonadJanusC m) => JanusCMT t () -> m [C.BlockItem]
captureBlock_ = fmap snd . captureBlock

--------------------------------------------------------------------------------
-- Runtime Emitters
--------------------------------------------------------------------------------

-- | Register @headers@ and append @items@ to the current function's block.
emitCBlockItems :: (MonadJanusC m) => [String] -> [C.BlockItem] -> m ()
emitCBlockItems hdrs items = liftJC $ do
  mapM_ addHeader hdrs
  unless (null items) $
    modifyFunction (\fn -> fn & jcfBlock %~ flip appendBlock items)

-- | Register @headers@, typedefs, top-level declarations, and all function
-- definitions in @defs@ into 'JanusCMState' and the caller's 'JCFunc'.
emitCDefinitions :: (MonadJanusC m) => [String] -> [C.Definition] -> m ()
emitCDefinitions hdrs defs = liftJC $ do
  (escHdrs, quoteTypedefs, quoteDecls, funcs) <-
    either error pure (partitionDefinitions defs)
  let allHdrs = hdrs <> escHdrs
  mapM_ addHeader allHdrs
  callerName <- askFuncName
  fi <- ask
  let backend = fi ^. jcfiBackend
      preparedFuncs =
        [ (fnName, adjustFuncSpec backend spec, dec, params, body)
          | (fnName, spec, dec, params, body) <- funcs
        ]
  mapM_
    ( \(fnName, _, _, _, _) ->
        when (fnName == callerName) $
          error ("emitCDefinitions: cannot redefine currently active function " <> show fnName)
    )
    preparedFuncs
  let mkProto (fnName, spec', dec, params, _) =
        C.InitGroup
          spec'
          []
          [C.Init (C.Id fnName noLoc) (C.Proto dec params noLoc) Nothing Nothing [] noLoc]
          noLoc
      funcProtoList = [mkProto pf | pf <- preparedFuncs]
      allFuncProtoSet = Set.fromList funcProtoList
      quoteDeclSet = Set.fromList quoteDecls
  modifyFunction $ \fn ->
    fn
      & jcfTypedefs %~ (\tds -> dedupTypedefs (tds <> quoteTypedefs))
      & jcfProtos %~ Set.union (Set.union quoteDeclSet allFuncProtoSet)
  callerFn <- getFunction
  let inheritedHeaders = callerFn ^. jcfHeaders
      inheritedTypedefs = callerFn ^. jcfTypedefs
      inheritedTypeCache = callerFn ^. jcfTypeCache
      inheritedProtos = callerFn ^. jcfProtos
  mapM_
    ( \pf@(fnName, spec', dec, params, body) -> do
        alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup fnName)
        unless alreadyDefined $ do
          let selfProto = mkProto pf
              fnProtos = Set.delete selfProto inheritedProtos
              newFn =
                (defaultJCFunc fnName params)
                  { _jcfHeaders = inheritedHeaders,
                    _jcfTypedefs = inheritedTypedefs,
                    _jcfTypeCache = inheritedTypeCache,
                    _jcfProtos = fnProtos,
                    _jcfType = Just (JCType spec' dec),
                    _jcfBlock = appendBlock mempty body,
                    _jcfExp = Nothing
                  }
          modify (Map.insert fnName newFn)
    )
    preparedFuncs

adjustFuncSpec :: JanusCBackend -> C.DeclSpec -> C.DeclSpec
adjustFuncSpec backend spec0 =
  let spec1 = stripStatic spec0
   in case (backend, spec1) of
        (JCUDA, C.DeclSpec sto tqs ts loc)
          | not (any isCUDAExecQual tqs) ->
              C.DeclSpec sto (C.TCUDAdevice noLoc : tqs) ts loc
        _ -> spec1
  where
    stripStatic = \case
      C.DeclSpec sto tqs ts loc ->
        C.DeclSpec (filter (not . isStaticStorage) sto) tqs ts loc
      ds -> ds
    isStaticStorage = \case
      C.Tstatic _ -> True
      _ -> False
    isCUDAExecQual = \case
      C.TCUDAdevice _ -> True
      C.TCUDAglobal _ -> True
      C.TCUDAhost _ -> True
      _ -> False

partitionDefinitions ::
  [C.Definition] ->
  Either String ([String], [JCTypedef], [C.InitGroup], [(String, C.DeclSpec, C.Decl, C.Params, [C.BlockItem])])
partitionDefinitions = go [] [] [] []
  where
    go hs tds decls fns [] = Right (reverse hs, reverse tds, reverse decls, reverse fns)
    go hs tds decls fns (d : ds) =
      case d of
        C.FuncDef (C.Func spec (C.Id fnName _) dec params body _) _ ->
          go hs tds decls ((fnName, spec, dec, params, body) : fns) ds
        C.FuncDef (C.Func _ otherId _ _ _ _) _ ->
          Left ("emitCDefinitions: unexpected function identifier: " <> show otherId)
        C.FuncDef (C.OldFunc {}) _ ->
          Left "emitCDefinitions: K&R old-style function definitions are not supported; use ISO C prototypes"
        C.DecDef (C.TypedefGroup spec _ typedefs _) _ -> do
          newTds <- mapM (convertTypedef spec) typedefs
          go hs (reverse newTds <> tds) decls fns ds
        C.DecDef ig@(C.InitGroup {}) _ ->
          go hs tds (ig : decls) fns ds
        C.DecDef other _ ->
          Left ("emitCDefinitions: unsupported declaration form: " <> show other)
        C.EscDef s _ ->
          case extractIncludes s of
            Right (ehs, rest) | isBlankOrComments rest ->
              go (reverse ehs <> hs) tds decls fns ds
            _ ->
              Left ("emitCDefinitions: unsupported top-level escape directive: " <> show s)
        other ->
          Left ("emitCDefinitions: unsupported top-level definition: " <> show other)

    convertTypedef spec = \case
      C.Typedef (C.Id nm _) (C.DeclRoot _) _ _ -> Right (JCTypedef spec nm)
      C.Typedef (C.Id nm _) dec _ _ -> Right (JCTypedefFull spec dec nm)
      other -> Left ("emitCDefinitions: unsupported typedef declarator: " <> show other)
