{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation choice spaces, Cartesian enumeration, Template Haskell
-- derivation, and zero-overhead library promotion helpers.
module Janus.Bench.Choice
  ( -- * Constructing Implementation Pieces & Choice Spaces
    impl,
    implWithId,
    candidates,
    DSum ((:=>)),
    choiceSpace,
    choiceSelection,
    emptyChoiceSpace,
    emptyChoiceSelection,
    simpleChoiceSpace,

    -- * Choice Enumeration & CSV Metadata
    enumerateChoices,
    choiceHeaderColumns,
    choiceDescriptionFields,
    camelToSnake,
    deriveChoiceKey,

    -- * Zero-Overhead Library Execution & Selection Persistence
    SelectionSpec,
    extractSelectionSpec,
    resolveSelection,
    resolveSelectionOrFail,
    saveSelection,
    loadSelectionSpec,
    loadSelection,
    loadSelectionTH,
    runWithChoices,
    runProduction,
  )
where

import Data.Char (isLower, isUpper, toLower)
import qualified Data.Dependent.HashMap as DHM
import Data.Dependent.Sum (DSum ((:=>)))
import Data.GADT.Compare (GEq)
import Data.GADT.Compare.TH (deriveGEq)
import Data.Hashable (Hashable (..))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Some (Some (..))
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

-- | Wrap a non-empty list of candidate implementations for a choice key.
candidates :: [Impl a] -> Candidates a
candidates = Candidates

-- | Build a 'ChoiceSpace' from @key :=> candidates [...]@ entries.
choiceSpace :: (GEq c, Hashable (Some c)) => [DSum c Candidates] -> ChoiceSpace c
choiceSpace = DHM.fromList

-- | Build a concrete 'ChoiceSelection' from @key :=> impl ...@ entries.
choiceSelection :: (GEq c, Hashable (Some c)) => [DSum c Impl] -> ChoiceSelection c
choiceSelection = DHM.fromList

-- | Empty 'ChoiceSpace' for zero-choice ('NoChoice') benchmarks.
emptyChoiceSpace :: ChoiceSpace NoChoice
emptyChoiceSpace = DHM.empty

-- | Empty 'ChoiceSelection' for zero-choice ('NoChoice') benchmarks.
emptyChoiceSelection :: ChoiceSelection NoChoice
emptyChoiceSelection = DHM.empty

-- | Single-axis 'ChoiceSpace' over 'SimpleChoice' for flat candidate lists.
simpleChoiceSpace :: [Impl a] -> ChoiceSpace (SimpleChoice a)
simpleChoiceSpace xs = DHM.singleton SimpleImpl (Candidates xs)

--------------------------------------------------------------------------------
-- Choice Enumeration & CSV Metadata
--------------------------------------------------------------------------------

-- | Enumerate the deterministic Cartesian product of all candidate
-- implementations across @'choiceColumns' \@c@.
enumerateChoices :: forall c. (ChoiceKey c) => ChoiceSpace c -> [ChoiceSelection c]
enumerateChoices space = go (choiceColumns @c)
  where
    go :: [Some c] -> [ChoiceSelection c]
    go [] = [DHM.empty]
    go (Some k : rest) =
      case DHM.lookup k space of
        Nothing ->
          error $
            "Janus.Bench.enumerateChoices: ChoiceSpace is missing candidates for column "
              <> show (choiceColumnName k)
        Just (Candidates cs) ->
          [ DHM.insert k cand tailSel
          | cand <- cs,
            tailSel <- go rest
          ]

-- | Ordered CSV column names corresponding to each constructor of @c@.
choiceHeaderColumns :: forall c. (ChoiceKey c) => [String]
choiceHeaderColumns = [choiceColumnName k | Some k <- choiceColumns @c]

-- | Ordered human-readable 'implDescription' cells for a concrete 'ChoiceSelection'.
choiceDescriptionFields :: forall c. (ChoiceKey c) => ChoiceSelection c -> [String]
choiceDescriptionFields sel =
  [ maybe "" implDescription (DHM.lookup k sel)
  | Some k <- choiceColumns @c
  ]

-- | Convert a Haskell constructor name (e.g., @\"SIMDWidth\"@ or @\"DataLayout\"@)
-- into @snake_case@ (@\"simd_width\"@, @\"data_layout\"@) suitable for CSV headers.
camelToSnake :: String -> String
camelToSnake = go Nothing
  where
    go _ [] = []
    go mPrev (c : cs)
      | isUpper c =
          let nextIsLower = case cs of
                (n : _) -> isLower n
                [] -> False
              needsSep = case mPrev of
                Nothing -> False
                Just p -> isLower p || (isUpper p && nextIsLower)
           in (if needsSep then ['_', toLower c] else [toLower c]) <> go (Just c) cs
      | otherwise = c : go (Just c) cs

extractZeroArgConNames :: TH.Con -> [TH.Name]
extractZeroArgConNames = \case
  TH.NormalC nm [] -> [nm]
  TH.GadtC nms [] _ -> nms
  TH.ForallC _ _ inner -> extractZeroArgConNames inner
  other ->
    error $
      "Janus.Bench.deriveChoiceKey: expected zero-argument GADT constructors, got "
        <> show other

-- | Derive 'GEq', @'Hashable' ('Some' c)@, and 'ChoiceKey' instances for a
-- choice GADT @c :: Type -> Type@ whose constructors represent implementation
-- axes.
deriveChoiceKey :: TH.Name -> TH.Q [TH.Dec]
deriveChoiceKey tyName = do
  geqDecs <- deriveGEq tyName
  info <- TH.reify tyName
  cons <- case info of
    TH.TyConI (TH.DataD _ _ _ _ cs _) -> pure (concatMap extractZeroArgConNames cs)
    TH.TyConI (TH.NewtypeD _ _ _ _ c _) -> pure (extractZeroArgConNames c)
    _ -> fail $ "Janus.Bench.deriveChoiceKey: " <> show tyName <> " is not a data/newtype GADT"
  let tyCon = TH.ConT tyName
      colExprs = [TH.AppE (TH.ConE 'Some) (TH.ConE nm) | nm <- cons]
      xVar = TH.mkName "k"
      matches =
        [ TH.Match
            (TH.ConP nm [] [])
            (TH.NormalB (TH.LitE (TH.StringL (camelToSnake (TH.nameBase nm)))))
            []
        | nm <- cons
        ]
      colNameBody =
        if null matches
          then TH.CaseE (TH.VarE xVar) []
          else TH.CaseE (TH.VarE xVar) matches
      choiceKeyInst =
        TH.InstanceD
          Nothing
          []
          (TH.AppT (TH.ConT ''ChoiceKey) tyCon)
          [ TH.ValD (TH.VarP 'choiceColumns) (TH.NormalB (TH.ListE colExprs)) [],
            TH.FunD 'choiceColumnName [TH.Clause [TH.VarP xVar] (TH.NormalB colNameBody) []]
          ]
      sVar = TH.mkName "s"
      someArg = TH.mkName "cKey"
      hashBody =
        TH.AppE
          (TH.AppE (TH.VarE 'hashWithSalt) (TH.VarE sVar))
          (TH.AppE (TH.VarE 'choiceColumnName) (TH.VarE someArg))
      hashInst =
        TH.InstanceD
          Nothing
          []
          (TH.AppT (TH.ConT ''Hashable) (TH.AppT (TH.ConT ''Some) tyCon))
          [ TH.FunD
              'hashWithSalt
              [ TH.Clause
                  [TH.VarP sVar, TH.ConP 'Some [] [TH.VarP someArg]]
                  (TH.NormalB hashBody)
                  []
              ]
          ]
  pure (geqDecs <> [choiceKeyInst, hashInst])

--------------------------------------------------------------------------------
-- Zero-Overhead Library Execution & Selection Persistence
--------------------------------------------------------------------------------

-- | Serializable mapping from 'choiceColumnName' to winning 'implId'.
type SelectionSpec = Map.Map String String

-- | Extract a serializable 'SelectionSpec' from a concrete 'ChoiceSelection'.
extractSelectionSpec :: forall c. (ChoiceKey c) => ChoiceSelection c -> SelectionSpec
extractSelectionSpec sel =
  Map.fromList
    [ (choiceColumnName k, implId i)
    | Some k <- choiceColumns @c,
      Just i <- [DHM.lookup k sel]
    ]

-- | Reconstruct a typed 'ChoiceSelection' from a 'ChoiceSpace' and a saved
-- 'SelectionSpec'.
resolveSelection ::
  forall c.
  (ChoiceKey c) =>
  ChoiceSpace c ->
  SelectionSpec ->
  Either String (ChoiceSelection c)
resolveSelection space spec = go (choiceColumns @c) DHM.empty
  where
    go [] acc = Right acc
    go (Some k : rest) acc = do
      let col = choiceColumnName k
      chosenId <- case Map.lookup col spec of
        Just ident -> Right ident
        Nothing -> Left $ "resolveSelection: missing choice column " <> show col <> " in SelectionSpec"
      Candidates cands <- case DHM.lookup k space of
        Just cs -> Right cs
        Nothing -> Left $ "resolveSelection: missing choice column " <> show col <> " in ChoiceSpace"
      case filter (\c -> implId c == chosenId) cands of
        (winner : _) -> go rest (DHM.insert k winner acc)
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
  forall c.
  (ChoiceKey c) =>
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
saveSelection :: forall c. (ChoiceKey c) => FilePath -> ChoiceSelection c -> IO ()
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
loadSelection :: forall c. (ChoiceKey c) => ChoiceSpace c -> FilePath -> IO (ChoiceSelection c)
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

-- | Execute a 'BenchT' computation in production/library mode using a fixed
-- 'ChoiceSelection'.
--
-- In this mode '_benchTimers' is 'Nothing', so all 'namedTic', 'namedToc', and
-- 'withNamedTimer' calls disappear completely at staging/compile time with
-- zero runtime or instruction overhead.
runWithChoices :: (Monad m) => ChoiceSelection c -> BenchT c m e a -> m a
runWithChoices sel =
  runBenchT
    BenchEnv
      { _benchChoices = sel,
        _benchTimers = Nothing
      }

-- | Convenience wrapper around 'runWithChoices' that also unwraps a returned
-- @'Bench' c e a@ expression into @e a@ for direct use in library code.
runProduction :: (Monad m) => ChoiceSelection c -> BenchT c m e (Bench c e a) -> m (e a)
runProduction sel act = getBench <$> runWithChoices sel act
