{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types for the Janus benchmarking monad transformer ('BenchT'),
-- expression wrapper ('Bench'), implementation choice environment, and
-- zero-contamination named @tic@/@toc@ timer collector.
--
-- 'BenchT' and 'Bench' wrap @('ReaderT' ('BenchEnv' c e) m, 'ReaderExp' ('BenchEnv' c e) e)@
-- and derive their Janus EDSL instances from "Janus.Control.Reader", while
-- 'choose' is a plain function built directly on 'ask'.
module Janus.Bench.Types
  ( -- * Implementation Pieces & Dependent Choice Maps
    Impl (..),
    Candidates (..),
    ChoiceSelection,
    ChoiceSpace,
    ChoiceKey (..),
    NoChoice,
    SimpleChoice (..),

    -- * Pure-Janus Named Timer Collector
    CmdNamedTimer,
    TimerCollector (..),
    sanitizeTimerFileStem,
    normalizeUserTimerName,
    hashTimerName,
    newTimerCollector,
    freeTimerCollector,
    withTimerCollector,
    resetTimerIterAccumulators,
    resolveTimerSlot,

    -- * Benchmarking Environment, Expression, Reference, & Monad Transformer
    BenchEnv (..),
    Bench (..),
    getBench,
    liftBench,
    BenchRef (..),
    getBenchRef,
    BenchT (..),
    BenchM,
    runBenchT,
    liftBenchM,
    benchReaderSpec,

    -- * Reader-Based Choice Functions (`choose` via `ask`)
    HasChoices (..),
    askChoiceImpl,
    choose,

    -- * Named Timer Operations
    namedTic,
    namedToc,
    withNamedTimer,
  )
where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Class (lift)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Dependent.HashMap (DHashMap)
import qualified Data.Dependent.HashMap as DHM
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import qualified Data.Kind as K
import Data.Some (Some (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.C (CChar, CString)
import Foreign.Ptr (Ptr)
import Janus.Backend.C.CUDA.Types (CmdSync)
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Format (CmdString (..))
import Janus.Command.Memory (CmdMem (..), CmdMemCopy (..), CmdStorable (..))
import Janus.Command.Ref (CmdRef (..), Ref)
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Control.Reader.Base (ExpNewtype, ReaderExp (..), ReaderRef (..))
import Janus.Control.Reader.TH (JanusReaderSpec (..))
import Janus.Data.HKD (ExpPtrWord, ExpStructPtr, ptrToWord64Expr)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond)
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Let (ExpLet)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.SPMD.Types (ExpPair)
import Janus.Typed (JanusTyped)
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------
-- Implementation Pieces & Dependent Choice Maps
--------------------------------------------------------------------------------

-- | An instantiation of a single implementation piece @a@, paired with a
-- stable programmatic identifier ('implId', used for library persistence) and
-- a human-readable description ('implDescription', emitted in CSV reports).
data Impl a = Impl
  { implId :: !String,
    implDescription :: !String,
    implValue :: a
  }
  deriving stock (Functor)

instance Show (Impl a) where
  showsPrec d i =
    showParen (d > 10) $
      showString "Impl "
        . shows (implId i)
        . showChar ' '
        . shows (implDescription i)

-- | Ordered candidate list for a single choice key @c a@.
newtype Candidates a = Candidates {getCandidates :: [Impl a]}
  deriving stock (Functor)
  deriving newtype (Semigroup, Monoid)

-- | Concrete instantiation of every choice key in @c@.
type ChoiceSelection c = DHashMap c Impl

-- | Search space mapping each choice key in @c@ to its candidate implementations.
type ChoiceSpace c = DHashMap c Candidates

-- | Class characterizing a choice GADT @c :: Type -> Type@ whose constructors
-- correspond to configurable pieces of an algorithm (data layout, unroll
-- factor, sub-algorithm, SIMD strategy, etc.).
class (GEq c, Hashable (Some c)) => ChoiceKey (c :: K.Type -> K.Type) where
  -- | Ordered list of choice axes, defining the CSV column order.
  choiceColumns :: [Some c]

  -- | CSV header column name for a given choice constructor.
  choiceColumnName :: c a -> String

-- | Empty choice key for simple single-implementation benchmarks.
data NoChoice a

instance GEq NoChoice where
  geq x _ = case x of {}

instance GCompare NoChoice where
  gcompare x _ = case x of {}

instance Hashable (Some NoChoice) where
  hashWithSalt _ (Some x) = case x of {}

instance ChoiceKey NoChoice where
  choiceColumns = []
  choiceColumnName x = case x of {}

-- | Single-axis choice key for flat multi-candidate benchmarks without a custom GADT.
data SimpleChoice implTy a where
  SimpleImpl :: SimpleChoice implTy implTy

instance GEq (SimpleChoice implTy) where
  geq SimpleImpl SimpleImpl = Just Refl

instance GCompare (SimpleChoice implTy) where
  gcompare SimpleImpl SimpleImpl = GEQ

instance Hashable (Some (SimpleChoice implTy)) where
  hashWithSalt s (Some SimpleImpl) = hashWithSalt s (0 :: Int)

instance ChoiceKey (SimpleChoice implTy) where
  choiceColumns = [Some SimpleImpl]
  choiceColumnName SimpleImpl = "implementation"

--------------------------------------------------------------------------------
-- Pure-Janus Named Timer Collector
--------------------------------------------------------------------------------

-- | Sanitize a timer name into a clean, portable CSV file stem.
sanitizeTimerFileStem :: String -> String
sanitizeTimerFileStem raw =
  let mapped =
        map
          (\ch -> if isAlphaNum ch || ch == '_' || ch == '-' then ch else '_')
          raw
   in if null mapped then "unnamed" else mapped

-- | Normalize a user-supplied named timer identifier so slot 0 (@\"overall\"@)
-- remains reserved for whole-implementation timing.
normalizeUserTimerName :: String -> String
normalizeUserTimerName s =
  let stem = sanitizeTimerFileStem s
   in if stem == "overall" then "overall_named" else stem

-- | Deterministic non-zero 64-bit hash of a timer name (@0@ is reserved as the
-- empty-bucket sentinel in the open-addressed timer hash table).
hashTimerName :: String -> Word64
hashTimerName name =
  let digest = SHA256.hash (Text.encodeUtf8 (Text.pack name))
      w64 =
        foldl
          (\acc i -> (acc `shiftL` 8) .|. fromIntegral (BS.index digest i))
          (0 :: Word64)
          [0 .. 7]
   in if w64 == 0 then 1 else w64

-- | Constraint bundle required by pure-Janus named @tic@/@toc@ operations.
type CmdNamedTimer m e =
  ( CmdTicToc m e,
    CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdMem m e,
    CmdMemCopy m e,
    CmdString m e,
    CmdStorable m e Int64,
    CmdStorable m e Word64,
    CmdStorable m e Word8,
    CmdStorable m e CString,
    ExpBool e,
    ExpPtrCast e,
    ExpPtrWord e,
    ExpBits e Word64,
    ExpBits e Int64,
    ExpOrd e Int64,
    ExpEq e Int64,
    ExpEq e Word64,
    ExpEq e Word8,
    ExpIntegralCast e Word64 Int64,
    Num (e Int64),
    Num (e Word64),
    Num (e Word8),
    JanusTyped e Bool,
    JanusTyped e Int64,
    JanusTyped e Word64,
    JanusTyped e Word8,
    JanusTyped e CChar,
    JanusTyped e CString
  )

-- | Pure-Janus heap-backed timer table and per-slot accumulators.
--
-- Slot @0@ is reserved for @\"overall\"@; slots @1 .. numSlots - 1@ are
-- allocated on demand via an open-addressed power-of-2 hash table keyed by
-- compile-time 64-bit hashes of timer names.
data TimerCollector e = TimerCollector
  { tcMaxSlots :: !Int,
    tcTableCap :: !Int,
    tcRawCount :: !(e (Ptr ())),
    tcRawHashes :: !(e (Ptr ())),
    tcRawSlots :: !(e (Ptr ())),
    tcRawNames :: !(e (Ptr ())),
    tcRawStarts :: !(e (Ptr ())),
    tcRawDeltas :: !(e (Ptr ())),
    tcRawCounts :: !(e (Ptr ())),
    tcCountPtr :: !(e (Ptr Int64)),
    tcTableHashes :: !(e (Ptr Word64)),
    tcTableSlots :: !(e (Ptr Int64)),
    tcSlotNames :: !(e (Ptr CString)),
    tcStarts :: !(e (Ptr Word64)),
    tcIterDeltas :: !(e (Ptr Word64)),
    tcIterCounts :: !(e (Ptr Int64))
  }

nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go p
      | p >= n = p
      | otherwise = go (p * 2)

-- | Duplicate a host 'String' into a newly @calloc@-allocated NUL-terminated
-- @e CString@ inside Janus so pointers remain valid across scopes in both @IO@
-- and compiled C.
allocDupCString :: forall m e. (CmdNamedTimer m e) => String -> m (e CString)
allocDupCString s =
  withString s $ \src -> do
    let src8 :: e (Ptr Word8)
        src8 = ptrCast src
    lenRef <- newRef (0 :: e Int64)
    whileM true $ \cont -> do
      i <- readRef lenRef
      b <- peekElemOff src8 i
      ifThenElseM_
        (b `eq` 0)
        (writeRef cont false)
        (writeRef lenRef (i + 1))
    len <- readRef lenRef
    raw <- calloc (len + 1) 1
    whenM_ (len `gt` 0) $
      cmdMemcpy raw (toVoidPtr src) len
    pure (fromVoidPtr raw)

-- | Allocate and initialize a 'TimerCollector' capable of tracking @\"overall\"@
-- plus up to @maxNamed@ distinct named @tic@/@toc@ pairs.
newTimerCollector :: forall m e. (CmdNamedTimer m e) => Int -> m (TimerCollector e)
newTimerCollector maxNamed = do
  let maxSlots = max 2 (1 + max 1 maxNamed)
      tableCap = nextPow2 (max 16 (maxSlots * 2))
      maxSlotsE = fromIntegral maxSlots :: e Int64
      tableCapE = fromIntegral tableCap :: e Int64
  rawCount <- calloc 1 8
  rawHashes <- calloc tableCapE 8
  rawSlots <- calloc tableCapE 8
  rawNames <- calloc maxSlotsE 8
  rawStarts <- calloc maxSlotsE 8
  rawDeltas <- calloc maxSlotsE 8
  rawCounts <- calloc maxSlotsE 8
  let countPtr = fromVoidPtr rawCount :: e (Ptr Int64)
      hashesPtr = fromVoidPtr rawHashes :: e (Ptr Word64)
      slotsPtr = fromVoidPtr rawSlots :: e (Ptr Int64)
      namesPtr = fromVoidPtr rawNames :: e (Ptr CString)
      startsPtr = fromVoidPtr rawStarts :: e (Ptr Word64)
      deltasPtr = fromVoidPtr rawDeltas :: e (Ptr Word64)
      countsPtr = fromVoidPtr rawCounts :: e (Ptr Int64)
  poke countPtr 1
  overallCStr <- allocDupCString "overall"
  pokeElemOff namesPtr overallCStr 0
  pure
    TimerCollector
      { tcMaxSlots = maxSlots,
        tcTableCap = tableCap,
        tcRawCount = rawCount,
        tcRawHashes = rawHashes,
        tcRawSlots = rawSlots,
        tcRawNames = rawNames,
        tcRawStarts = rawStarts,
        tcRawDeltas = rawDeltas,
        tcRawCounts = rawCounts,
        tcCountPtr = countPtr,
        tcTableHashes = hashesPtr,
        tcTableSlots = slotsPtr,
        tcSlotNames = namesPtr,
        tcStarts = startsPtr,
        tcIterDeltas = deltasPtr,
        tcIterCounts = countsPtr
      }

-- | Release all heap memory owned by a 'TimerCollector' (including duplicated
-- slot name strings).
freeTimerCollector :: forall m e. (CmdNamedTimer m e) => TimerCollector e -> m ()
freeTimerCollector tc = do
  numUsed <- peek (tcCountPtr tc)
  idxRef <- newRef (0 :: e Int64)
  whileM true $ \cont -> do
    i <- readRef idxRef
    ifThenElseM_
      (i `ge` numUsed)
      (writeRef cont false)
      ( do
          nm <- peekElemOff (tcSlotNames tc) i
          whenM_ (ptrToWord64Expr nm `ne` 0) $
            free (toVoidPtr nm)
          writeRef idxRef (i + 1)
      )
  free (tcRawCount tc)
  free (tcRawHashes tc)
  free (tcRawSlots tc)
  free (tcRawNames tc)
  free (tcRawStarts tc)
  free (tcRawDeltas tc)
  free (tcRawCounts tc)

-- | Bracketed allocation and cleanup of a 'TimerCollector'.
withTimerCollector :: (CmdNamedTimer m e) => Int -> (TimerCollector e -> m a) -> m a
withTimerCollector maxNamed k = do
  tc <- newTimerCollector maxNamed
  res <- k tc
  freeTimerCollector tc
  pure res

-- | Zero per-batch @tcIterDeltas@ and @tcIterCounts@ across all allocated timer slots.
resetTimerIterAccumulators :: forall m e. (CmdNamedTimer m e) => TimerCollector e -> m ()
resetTimerIterAccumulators tc = do
  numUsed <- peek (tcCountPtr tc)
  idxRef <- newRef (0 :: e Int64)
  whileM true $ \cont -> do
    i <- readRef idxRef
    ifThenElseM_
      (i `ge` numUsed)
      (writeRef cont false)
      ( do
          pokeElemOff (tcIterDeltas tc) 0 i
          pokeElemOff (tcIterCounts tc) 0 i
          writeRef idxRef (i + 1)
      )

-- | Resolve (or lazily register on first encounter) the dense slot index
-- @1 .. tcMaxSlots - 1@ for a named timer. Returns @-1@ only if @tcMaxSlots@
-- is exhausted.
resolveTimerSlot :: forall m e. (CmdNamedTimer m e) => TimerCollector e -> String -> m (e Int64)
resolveTimerSlot tc rawName = do
  let cleanName = normalizeUserTimerName rawName
      hVal = fromIntegral (hashTimerName cleanName) :: e Word64
      mask = fromIntegral (tcTableCap tc - 1) :: e Int64
      capE = fromIntegral (tcTableCap tc) :: e Int64
      maxSlotsE = fromIntegral (tcMaxSlots tc) :: e Int64
      initIdx = (toIntegral hVal :: e Int64) `iand` mask
  idxRef <- newRef initIdx
  stepsRef <- newRef (0 :: e Int64)
  slotRef <- newRef (-1 :: e Int64)
  whileM true $ \cont -> do
    steps <- readRef stepsRef
    ifThenElseM_
      (steps `ge` capE)
      (writeRef cont false)
      ( do
          idx <- readRef idxRef
          curH <- peekElemOff (tcTableHashes tc) idx
          ifThenElseM_
            (curH `eq` hVal)
            ( do
                s <- peekElemOff (tcTableSlots tc) idx
                writeRef slotRef s
                writeRef cont false
            )
            ( ifThenElseM_
                (curH `eq` 0)
                ( do
                    nextSlot <- peek (tcCountPtr tc)
                    whenM_ (nextSlot `lt` maxSlotsE) $ do
                      poke (tcCountPtr tc) (nextSlot + 1)
                      pokeElemOff (tcTableHashes tc) hVal idx
                      pokeElemOff (tcTableSlots tc) nextSlot idx
                      dupName <- allocDupCString cleanName
                      pokeElemOff (tcSlotNames tc) dupName nextSlot
                      pokeElemOff (tcStarts tc) 0 nextSlot
                      pokeElemOff (tcIterDeltas tc) 0 nextSlot
                      pokeElemOff (tcIterCounts tc) 0 nextSlot
                      writeRef slotRef nextSlot
                    writeRef cont false
                )
                ( do
                    writeRef idxRef ((idx + 1) `iand` mask)
                    writeRef stepsRef (steps + 1)
                )
            )
      )
  readRef slotRef

--------------------------------------------------------------------------------
-- Benchmarking Environment, Expression, Reference, & Monad Transformer
--------------------------------------------------------------------------------

-- | Reader environment carried by 'BenchT'.
--
-- When '_benchTimers' is 'Nothing' (production mode via 'runWithChoices'),
-- 'namedTic' and 'namedToc' compile to @pure ()@ with zero runtime overhead.
data BenchEnv (c :: K.Type -> K.Type) (e :: K.Type -> K.Type) = BenchEnv
  { _benchChoices :: !(ChoiceSelection c),
    _benchTimers :: !(Maybe (TimerCollector e))
  }

-- | Zero-cost Janus expression wrapper backed by @'ReaderExp' ('BenchEnv' c e) e@.
newtype Bench (c :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = Bench
  { unBench :: ReaderExp (BenchEnv c e) e a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype
    ( Applicative,
      Monad,
      ExpNewtype,
      ExpBool,
      ExpPtrCast,
      ExpCond,
      ExpLet,
      ExpPtrWord,
      ExpStructPtr,
      ExpPair
    )

type instance JanusTyped (Bench c e) = JanusTyped e

-- | Unwrap a @'Bench' c e a@ expression into the underlying @e a@.
getBench :: Bench c e a -> e a
getBench = getReaderExp . unBench

-- | Lift an underlying Janus expression @e a@ into @'Bench' c e a@.
liftBench :: e a -> Bench c e a
liftBench = Bench . ReaderExp

-- | Mutable reference wrapper for 'BenchT', backed by 'ReaderRef'.
newtype BenchRef (c :: K.Type -> K.Type) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = BenchRef
  { unBenchRef :: ReaderRef (BenchEnv c e) m e a
  }

-- | Unwrap a 'BenchRef' to the underlying @'Ref' m e a@.
getBenchRef :: BenchRef c m e a -> Ref m e a
getBenchRef = getReaderRef . unBenchRef

-- | Polymorphic Janus benchmarking monad transformer parameterized by choice
-- GADT @c@, base monad @m@, and base expression type @e@, deriving its effects
-- and Janus typeclasses from @'ReaderT' ('BenchEnv' c e) m@.
newtype BenchT (c :: K.Type -> K.Type) (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) a = BenchT
  { unBenchT :: ReaderT (BenchEnv c e) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadIO,
      MonadReader (BenchEnv c e),
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask,
      CmdSync
    )

-- | Synonym for 'BenchT'.
type BenchM = BenchT

-- | Execute a 'BenchT' computation under an explicit 'BenchEnv'.
runBenchT :: BenchEnv c e -> BenchT c m e a -> m a
runBenchT env (BenchT m) = runReaderT m env

-- | Lift an underlying Janus command @m a@ into @BenchT c m e a@.
liftBenchM :: (Monad m) => m a -> BenchT c m e a
liftBenchM = BenchT . lift

-- | 'JanusReaderSpec' used by 'Janus.Control.Reader.TH.deriveJanusReader' and
-- 'Janus.Control.Reader.TH.deriveJanusReaderGang' to derive all multi-parameter
-- Janus instances for @('BenchT' c m e, 'Bench' c e)@ from @('ReaderT', 'ReaderExp')@.
benchReaderSpec :: JanusReaderSpec
benchReaderSpec =
  JanusReaderSpec
    { jrsInstantiate = \mQ eQ -> do
        cVar <- TH.newName "c"
        let cQ = TH.varT cVar
        envTy <- [t|BenchEnv $cQ $eQ|]
        mTy <- [t|BenchT $cQ $mQ $eQ|]
        eTy <- [t|Bench $cQ $eQ|]
        refTy <- [t|BenchRef $cQ $mQ $eQ|]
        cPred <- [t|Typeable $cQ|]
        pure (envTy, mTy, eTy, refTy, [cPred]),
      jrsExpCon = 'Bench,
      jrsUnExp = 'unBench,
      jrsRefCon = 'BenchRef,
      jrsUnRef = 'unBenchRef,
      jrsMonadCon = 'BenchT,
      jrsUnMonad = 'unBenchT,
      jrsSIMDPrefix = "Bench"
    }

--------------------------------------------------------------------------------
-- Reader-Based Choice Functions (`choose` via `ask`)
--------------------------------------------------------------------------------

-- | Environments that carry a 'ChoiceSelection' for @c@, enabling 'choose' and
-- 'askChoiceImpl' to operate over any @'MonadReader' r m@ (including 'BenchT',
-- @'ReaderT' ('BenchEnv' c e) m@, and @'ReaderT' ('ChoiceSelection' c) m@).
class HasChoices (c :: K.Type -> K.Type) r | r -> c where
  getChoiceSelection :: r -> ChoiceSelection c

instance HasChoices c (ChoiceSelection c) where
  getChoiceSelection = id

instance HasChoices c (BenchEnv c e) where
  getChoiceSelection = _benchChoices

-- | Retrieve the full 'Impl' metadata (identifier, description, and value) for
-- a choice constructor @c a@ via 'ask'.
askChoiceImpl ::
  (GEq c, Hashable (Some c), HasChoices c r, MonadReader r m) =>
  c a ->
  m (Impl a)
askChoiceImpl key = do
  env <- ask
  case DHM.lookup key (getChoiceSelection env) of
    Just i -> pure i
    Nothing ->
      error "Janus.Bench.choose: ChoiceSelection is missing an entry for the requested choice key"

-- | Select the active implementation piece for constructor @c a@ via 'ask'.
choose ::
  (GEq c, Hashable (Some c), HasChoices c r, MonadReader r m) =>
  c a ->
  m a
choose key = implValue <$> askChoiceImpl key

--------------------------------------------------------------------------------
-- Named Timer Operations (Lookup Strictly Outside Measurement Window)
--------------------------------------------------------------------------------

-- | Start named timer @name@.
--
-- Slot resolution executes *before* hardware @tic@ is sampled so hash-table
-- lookup never contaminates the measured tick delta. Compiles to @pure ()@
-- when executed via 'runWithChoices'.
namedTic :: (CmdNamedTimer m e) => String -> BenchT c m e ()
namedTic name = do
  env <- ask
  case _benchTimers env of
    Nothing -> pure ()
    Just tc -> liftBenchM $ do
      slot <- resolveTimerSlot tc name
      whenM_ (slot `gt` 0) $ do
        Tic t0 <- tic
        pokeElemOff (tcStarts tc) t0 slot

-- | Stop named timer @name@ and accumulate its elapsed ticks into the current
-- iteration batch.
--
-- Hardware @toc@ is sampled *first*—before slot resolution and accumulator
-- updates—so bookkeeping overhead is excluded from the measured interval.
-- Compiles to @pure ()@ when executed via 'runWithChoices'.
namedToc :: (CmdNamedTimer m e) => String -> BenchT c m e ()
namedToc name = do
  env <- ask
  case _benchTimers env of
    Nothing -> pure ()
    Just tc -> liftBenchM $ do
      Toc t1 <- toc
      slot <- resolveTimerSlot tc name
      whenM_ (slot `gt` 0) $ do
        t0 <- peekElemOff (tcStarts tc) slot
        prevDelta <- peekElemOff (tcIterDeltas tc) slot
        prevCount <- peekElemOff (tcIterCounts tc) slot
        pokeElemOff (tcIterDeltas tc) (prevDelta + (t1 - t0)) slot
        pokeElemOff (tcIterCounts tc) (prevCount + 1) slot

-- | Measure an action under named timer pair @name@ (@namedTic name *> act <* namedToc name@).
withNamedTimer :: (CmdNamedTimer m e) => String -> BenchT c m e a -> BenchT c m e a
withNamedTimer name act = do
  namedTic name
  res <- act
  namedToc name
  pure res
