{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Higher-kinded choice spaces, Cartesian enumeration via "Data.HKD"
-- ('FFunctor', 'FFoldable', 'FTraversable', 'FZip'), CSV column/description
-- extraction, and zero-overhead library promotion helpers.
module Janus.Bench.Choice
  ( -- * Constructing Implementation Pieces & Choice Spaces
    impl,
    implWithId,
    candidates,
    emptyChoiceSpace,
    emptyChoiceSelection,
    simpleChoiceSpace,

    -- * HKD Enumeration & CSV Metadata
    fromChoiceImpl,
    enumerateChoiceImpls,
    enumerateChoices,
    choiceHeaderColumns,
    choiceDescriptionFields,

    -- * Zero-Overhead Library Execution & Selection Persistence
    SelectionSpec,
    extractSelectionSpec,
    resolveSelection,
    resolveSelectionOrFail,
    saveSelection,
    loadSelectionSpec,
    loadSelection,
    loadSelectionTH,
    runWithDescriptions,
    runWithChoices,
    runProduction,
  )
where

import Control.Lens (view)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.HKD (FFoldable (..), FFunctor (..), FTraversable (..), FZip (..))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Janus.Bench.Types
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Constructing Implementation Pieces & Choice Spaces
--------------------------------------------------------------------------------

-- | Construct an 'Impl' using @description@ as both its programmatic ID and
-- human-readable CSV description.
impl :: String -> a -> Impl a
impl desc val =
  Impl
    { implId = desc,
      implDescription = desc,
      implValue = val
    }

-- | Construct an 'Impl' with an explicit short programmatic identifier
-- ('implId', used for persistence) and human-readable description
-- ('implDescription', emitted in CSV cells).
implWithId :: String -> String -> a -> Impl a
implWithId ident desc val =
  Impl
    { implId = ident,
      implDescription = desc,
      implValue = val
    }

-- | Wrap a non-empty list of candidate implementations for a choice field.
candidates :: [Impl a] -> Candidates a
candidates = Candidates

-- | Empty 'ChoiceSpace' for zero-choice ('NoChoice') benchmarks.
emptyChoiceSpace :: ChoiceSpace NoChoice
emptyChoiceSpace = NoChoice

-- | Empty 'ChoiceSelection' for zero-choice ('NoChoice') benchmarks.
emptyChoiceSelection :: ChoiceSelection NoChoice
emptyChoiceSelection =
  ChoiceSelection
    { selChoices = NoChoice,
      selDescriptions = NoChoice,
      selIds = NoChoice
    }

-- | Single-axis 'ChoiceSpace' over 'SimpleChoice' for flat candidate lists.
simpleChoiceSpace :: [Impl a] -> ChoiceSpace (SimpleChoice a)
simpleChoiceSpace xs = SimpleChoice (Candidates xs)

--------------------------------------------------------------------------------
-- HKD Enumeration & CSV Metadata
--------------------------------------------------------------------------------

-- | Split a concrete @c 'Impl'@ record into its @c 'Identity'@ implementation
-- pieces, @c ('Const' 'String')@ human-readable descriptions, and
-- @c ('Const' 'String')@ persistent identifiers via 'ffmap'.
fromChoiceImpl :: (FFunctor c) => c Impl -> ChoiceSelection c
fromChoiceImpl recImpl =
  ChoiceSelection
    { selChoices = ffmap (Identity . implValue) recImpl,
      selDescriptions = ffmap (Const . implDescription) recImpl,
      selIds = ffmap (Const . implId) recImpl
    }

-- | Enumerate the deterministic Cartesian product of all candidate
-- implementations across @c@ using 'ftraverse' in @[]@.
enumerateChoiceImpls :: (FTraversable c) => ChoiceSpace c -> [c Impl]
enumerateChoiceImpls = ftraverse getCandidates

-- | Enumerate all 'ChoiceSelection' combinations in a 'ChoiceSpace'.
enumerateChoices :: (FTraversable c) => ChoiceSpace c -> [ChoiceSelection c]
enumerateChoices = map fromChoiceImpl . enumerateChoiceImpls

-- | Ordered CSV header column names extracted from @'choiceColumns' \@c@ via
-- 'ffoldMap'.
choiceHeaderColumns :: forall c. (ChoiceColumns c) => [String]
choiceHeaderColumns = ffoldMap (\(Const col) -> [col]) (choiceColumns @c)

-- | Ordered human-readable description cells extracted from @c ('Const' 'String')@
-- via 'ffoldMap' in the exact same field order as 'choiceHeaderColumns'.
choiceDescriptionFields :: (FFoldable c) => c (Const String) -> [String]
choiceDescriptionFields = ffoldMap (\(Const desc) -> [desc])

--------------------------------------------------------------------------------
-- Zero-Overhead Library Execution & Selection Persistence
--------------------------------------------------------------------------------

-- | Serializable mapping from CSV column name to winning 'implId'.
type SelectionSpec = Map.Map String String

-- | Extract a serializable 'SelectionSpec' from a 'ChoiceSelection' using
-- 'fzipWith' and 'ffoldMap'.
extractSelectionSpec :: forall c. (ChoiceColumns c) => ChoiceSelection c -> SelectionSpec
extractSelectionSpec sel =
  Map.fromList $
    ffoldMap
      (\(Const pair) -> [pair])
      (fzipWith (\(Const col) (Const ident) -> Const (col, ident)) (choiceColumns @c) (selIds sel))

-- | Reconstruct a typed 'ChoiceSelection' from a 'ChoiceSpace' and a saved
-- 'SelectionSpec' using 'fzipWith' and 'ftraverse'.
resolveSelection ::
  forall c.
  (ChoiceColumns c) =>
  ChoiceSpace c ->
  SelectionSpec ->
  Either String (ChoiceSelection c)
resolveSelection space spec =
  fromChoiceImpl <$> ftraverse resolveField (fzipWith Pair (choiceColumns @c) space)
  where
    resolveField :: Product (Const String) Candidates a -> Either String (Impl a)
    resolveField (Pair (Const col) (Candidates cands)) = do
      chosenId <- case Map.lookup col spec of
        Just ident -> Right ident
        Nothing -> Left $ "resolveSelection: missing choice column " <> show col <> " in SelectionSpec"
      case filter (\i -> implId i == chosenId) cands of
        (winner : _) -> Right winner
        [] ->
          Left $
            "resolveSelection: unknown implId "
              <> show chosenId
              <> " for column "
              <> show col
              <> " (available: "
              <> intercalate ", " (map (show . implId) cands)
              <> ")"

-- | Total wrapper around 'resolveSelection' suitable for top-level library
-- bindings paired with 'loadSelectionTH'.
resolveSelectionOrFail ::
  (ChoiceColumns c) =>
  ChoiceSpace c ->
  SelectionSpec ->
  ChoiceSelection c
resolveSelectionOrFail space spec =
  case resolveSelection space spec of
    Right sel -> sel
    Left err -> error ("Janus.Bench.resolveSelectionOrFail: " <> err)

formatSelectionSpec :: SelectionSpec -> String
formatSelectionSpec spec =
  "[\n"
    <> intercalate
      ",\n"
      ["  " <> show pair | pair <- Map.toAscList spec]
    <> "\n]\n"

parseSelectionSpec :: FilePath -> String -> Either String SelectionSpec
parseSelectionSpec path raw =
  case readMaybe @[(String, String)] raw of
    Just pairs -> Right (Map.fromList pairs)
    Nothing -> Left $ "Failed to parse SelectionSpec from " <> path

-- | Persist a winning 'ChoiceSelection' to @path@ so it can be reloaded at
-- runtime ('loadSelection') or embedded at compile time ('loadSelectionTH') in
-- a production library.
saveSelection :: (ChoiceColumns c) => FilePath -> ChoiceSelection c -> IO ()
saveSelection path sel = do
  let dir = takeDirectory path
  if null dir || dir == "."
    then pure ()
    else createDirectoryIfMissing True dir
  writeFile path (formatSelectionSpec (extractSelectionSpec sel))

-- | Read a 'SelectionSpec' from disk.
loadSelectionSpec :: FilePath -> IO SelectionSpec
loadSelectionSpec path = do
  raw <- readFile path
  case parseSelectionSpec path raw of
    Right spec -> pure spec
    Left err -> ioError (userError err)

-- | Load and resolve a saved 'ChoiceSelection' against a 'ChoiceSpace'.
loadSelection :: (ChoiceColumns c) => ChoiceSpace c -> FilePath -> IO (ChoiceSelection c)
loadSelection space path = do
  spec <- loadSelectionSpec path
  case resolveSelection space spec of
    Right sel -> pure sel
    Left err -> ioError (userError err)

-- | Embed a saved 'SelectionSpec' file directly into a Haskell module at
-- compile time via Template Haskell (registering @path@ with
-- 'THS.qAddDependentFile' so GHC recompiles automatically when the selection
-- file changes).
--
-- Typical library usage:
--
-- @
-- winningChoice :: ChoiceSelection MyChoice
-- winningChoice = resolveSelectionOrFail myChoiceSpace $(loadSelectionTH "bench/winning.sel")
-- @
loadSelectionTH :: FilePath -> TH.Q TH.Exp
loadSelectionTH path = do
  THS.qAddDependentFile path
  raw <- TH.runIO (readFile path)
  pairs <- case readMaybe @[(String, String)] raw of
    Just ps -> pure ps
    Nothing -> fail $ "loadSelectionTH: invalid SelectionSpec in " <> path
  let pairExps =
        [ TH.TupE
            [ Just (TH.LitE (TH.StringL k)),
              Just (TH.LitE (TH.StringL v))
            ]
        | (k, v) <- pairs
        ]
  pure (TH.AppE (TH.VarE 'Map.fromList) (TH.ListE pairExps))

-- | Execute a 'BenchT' computation in production/library mode with explicit
-- @c 'Identity'@ choices and @c ('Const' 'String')@ descriptions.
--
-- In this mode '_benchTimers' is 'Nothing', so all 'namedTic', 'namedToc', and
-- 'withNamedTimer' calls disappear completely at staging/compile time with
-- zero runtime or instruction overhead.
runWithDescriptions :: (Monad m) => c Identity -> c (Const String) -> BenchT c m e a -> m a
runWithDescriptions cs ds =
  runBenchT
    BenchEnv
      { _benchChoices = cs,
        _benchDescriptions = ds,
        _benchTimers = Nothing
      }

-- | Execute a 'BenchT' computation in production/library mode using any @r@
-- carrying @c 'Identity'@ (such as @c 'Identity'@ directly or a
-- @'ChoiceSelection' c@).
runWithChoices :: (FFunctor c, HasChoices c r, Monad m) => r -> BenchT c m e a -> m a
runWithChoices r =
  let cs = view choicesL r
   in runWithDescriptions cs (ffmap (const (Const "")) cs)

-- | Convenience wrapper around 'runWithChoices' that also unwraps a returned
-- @'Bench' c e a@ expression into @e a@ for direct use in library code.
runProduction :: (FFunctor c, HasChoices c r, Monad m) => r -> BenchT c m e (Bench c e a) -> m (e a)
runProduction r act = getBench <$> runWithChoices r act
