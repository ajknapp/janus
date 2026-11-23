{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Backend-polymorphic RFC 4180 CSV formatting and streaming 'Drain' sinks.
--
-- Works identically in @(IO, Identity)@, @(JanusCMT t, JanusCT t)@, and
-- @(BenchT c m e, Bench c e)@ without per-row FFI callbacks when compiled to C.
module Janus.Control.CSV
  ( -- * RFC 4180 Escaping
    csvNeedsQuoting,
    csvEscapeString,
    PreEscapedCSV (..),
    hputCSVCString,

    -- * Field & Row Serialization Classes
    ToCSVField (..),
    CSVRow (..),
    csvField,
    ToCSVRow (..),

    -- * Header & Streaming Drain Sinks
    emitCSVHeader,
    drainCSVHandle,
    drainCSVFileCString,
    drainCSVFile,
  )
where

import Control.Monad (void)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intercalate)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C (CChar, CFile, CString)
import Foreign.Ptr (Ptr)
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import Janus.Command.Memory (CmdMem (..), CmdStorable (..))
import Janus.Command.Ref (CmdRef (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Control.Drain (Drain (..), drainForEach, runDrain)
import Janus.Data.HKD (ExpPtrWord (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- RFC 4180 Escaping
--------------------------------------------------------------------------------

-- | Return 'True' when an RFC 4180 CSV field requires enclosing double quotes.
csvNeedsQuoting :: String -> Bool
csvNeedsQuoting = any (\ch -> ch == ',' || ch == '"' || ch == '\n' || ch == '\r')

-- | Escape a host-side 'String' according to RFC 4180 (quoting and doubling @\"@
-- whenever the string contains a comma, double quote, CR, or LF).
csvEscapeString :: String -> String
csvEscapeString s
  | csvNeedsQuoting s = "\"" <> concatMap (\ch -> if ch == '"' then "\"\"" else [ch]) s <> "\""
  | otherwise = s

-- | Wrapper for strings that have already been escaped via 'csvEscapeString'.
newtype PreEscapedCSV = PreEscapedCSV {getPreEscapedCSV :: String}
  deriving (Eq, Ord, Show)

-- | Pure-Janus RFC 4180 writer for dynamic NUL-terminated @e CString@ values.
--
-- Scans the string once for @','@, @'\"'@, @'\\n'@, or @'\\r'@. When none are
-- present, emits the string with a single @hputString@ call; otherwise wraps
-- in quotes and doubles embedded @'\"'@ characters purely in Janus.
hputCSVCString ::
  forall m e.
  ( CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdMem m e,
    CmdStorable m e Word8,
    CmdPutString m e,
    ExpBool e,
    ExpPtrCast e,
    ExpPtrWord e,
    ExpEq e Word8,
    ExpEq e Word64,
    Num (e Int64),
    Num (e Word8),
    Num (e Word64),
    JanusTyped e Bool,
    JanusTyped e Int64,
    JanusTyped e Word8,
    JanusTyped e CChar
  ) =>
  e (Ptr CFile) ->
  e CString ->
  m ()
hputCSVCString h s =
  whenM_ (ptrToWord64Expr s `ne` 0) $ do
    let p8 :: e (Ptr Word8)
        p8 = ptrCast s
    idxRef <- newRef (0 :: e Int64)
    needsQuoteRef <- newRef false
    whileM true $ \cont -> do
      i <- readRef idxRef
      b <- peekElemOff p8 i
      ifThenElseM_
        (b `eq` 0)
        (writeRef cont false)
        ( do
            let special =
                  (b `eq` 44)
                    `lor` (b `eq` 34)
                    `lor` (b `eq` 10)
                    `lor` (b `eq` 13)
            whenM_ special $ writeRef needsQuoteRef true
            writeRef idxRef (i + 1)
        )
    needsQuote <- readRef needsQuoteRef
    ifThenElseM_
      (lnot needsQuote)
      (hputString h s)
      ( do
          withString "\"" (hputString h)
          rawTmp <- calloc 2 1
          let tmp8 :: e (Ptr Word8)
              tmp8 = fromVoidPtr rawTmp
              tmpCStr :: e CString
              tmpCStr = ptrCast tmp8
          pokeElemOff tmp8 0 1
          jRef <- newRef (0 :: e Int64)
          whileM true $ \cont -> do
            j <- readRef jRef
            b <- peekElemOff p8 j
            ifThenElseM_
              (b `eq` 0)
              (writeRef cont false)
              ( do
                  ifThenElseM_
                    (b `eq` 34)
                    (withString "\"\"" (hputString h))
                    ( do
                        poke tmp8 b
                        hputString h tmpCStr
                    )
                  writeRef jRef (j + 1)
              )
          free rawTmp
          withString "\"" (hputString h)
      )

--------------------------------------------------------------------------------
-- Field & Row Serialization Typeclasses
--------------------------------------------------------------------------------

-- | Values that can be formatted as a single CSV cell onto a Janus file handle.
class ToCSVField m e a | m -> e, e -> m where
  hputCSVField :: e (Ptr CFile) -> a -> m ()

instance (CmdPutString m e) => ToCSVField m e PreEscapedCSV where
  hputCSVField h (PreEscapedCSV s) = withString s (hputString h)

instance (CmdPutString m e) => ToCSVField m e String where
  hputCSVField h s = withString (csvEscapeString s) (hputString h)

instance (CmdFormat m e Int8) => ToCSVField m e (e Int8) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Word8) => ToCSVField m e (e Word8) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Int16) => ToCSVField m e (e Int16) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Word16) => ToCSVField m e (e Word16) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Int32) => ToCSVField m e (e Int32) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Word32) => ToCSVField m e (e Word32) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Int64) => ToCSVField m e (e Int64) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Word64) => ToCSVField m e (e Word64) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Float) => ToCSVField m e (e Float) where
  hputCSVField h x = hformat h x ""

instance (CmdFormat m e Double) => ToCSVField m e (e Double) where
  hputCSVField h x = hformat h x ""

instance (CmdCond m e, CmdPutString m e) => ToCSVField m e (e Bool) where
  hputCSVField h b =
    ifThenElseM_
      b
      (withString "true" (hputString h))
      (withString "false" (hputString h))

instance
  ( CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdMem m e,
    CmdStorable m e Word8,
    CmdPutString m e,
    ExpBool e,
    ExpPtrCast e,
    ExpPtrWord e,
    ExpEq e Word8,
    ExpEq e Word64,
    Num (e Int64),
    Num (e Word8),
    Num (e Word64),
    JanusTyped e Bool,
    JanusTyped e Int64,
    JanusTyped e Word8,
    JanusTyped e CChar
  ) =>
  ToCSVField m e (e CString)
  where
  hputCSVField = hputCSVCString

-- | Composable CSV row builder whose 'Semigroup' instance joins columns with @\",\"@.
newtype CSVRow m e = CSVRow {emitCSVRow :: e (Ptr CFile) -> m ()}

instance (Monad m, CmdPutString m e) => Semigroup (CSVRow m e) where
  CSVRow a <> CSVRow b =
    CSVRow $ \h -> do
      a h
      withString "," (hputString h)
      b h

-- | Lift a single 'ToCSVField' value into a one-column 'CSVRow'.
csvField :: (ToCSVField m e a) => a -> CSVRow m e
csvField x = CSVRow $ \h -> hputCSVField h x

-- | Values that can be emitted as a complete comma-separated CSV record (without trailing newline).
class ToCSVRow m e row | m -> e, e -> m where
  toCSVRow :: row -> CSVRow m e

instance (CmdIO m e, m' ~ m, e' ~ e) => ToCSVRow m e (CSVRow m' e') where
  toCSVRow = id

instance (Monad m, CmdPutString m e, ToCSVField m e a) => ToCSVRow m e [a] where
  toCSVRow [] = CSVRow $ \_ -> pure ()
  toCSVRow (x : xs) = foldl (<>) (csvField x) (map csvField xs)

instance
  (Monad m, CmdPutString m e, ToCSVField m e a, ToCSVField m e b) =>
  ToCSVRow m e (a, b)
  where
  toCSVRow (a, b) = csvField a <> csvField b

instance
  (Monad m, CmdPutString m e, ToCSVField m e a, ToCSVField m e b, ToCSVField m e c) =>
  ToCSVRow m e (a, b, c)
  where
  toCSVRow (a, b, c) = csvField a <> csvField b <> csvField c

instance
  ( Monad m,
    CmdPutString m e,
    ToCSVField m e a,
    ToCSVField m e b,
    ToCSVField m e c,
    ToCSVField m e d
  ) =>
  ToCSVRow m e (a, b, c, d)
  where
  toCSVRow (a, b, c, d) =
    csvField a <> csvField b <> csvField c <> csvField d

instance
  ( Monad m,
    CmdPutString m e,
    ToCSVField m e a,
    ToCSVField m e b,
    ToCSVField m e c,
    ToCSVField m e d,
    ToCSVField m e f
  ) =>
  ToCSVRow m e (a, b, c, d, f)
  where
  toCSVRow (a, b, c, d, f) =
    csvField a <> csvField b <> csvField c <> csvField d <> csvField f

instance
  ( Monad m,
    CmdPutString m e,
    ToCSVField m e a,
    ToCSVField m e b,
    ToCSVField m e c,
    ToCSVField m e d,
    ToCSVField m e f,
    ToCSVField m e g
  ) =>
  ToCSVRow m e (a, b, c, d, f, g)
  where
  toCSVRow (a, b, c, d, f, g) =
    csvField a <> csvField b <> csvField c <> csvField d <> csvField f <> csvField g

--------------------------------------------------------------------------------
-- Header & Streaming Drain Sinks
--------------------------------------------------------------------------------

-- | Write a single header line (escaping column names as needed) followed by @'\\n'@.
emitCSVHeader :: (CmdPutString m e) => e (Ptr CFile) -> [String] -> m ()
emitCSVHeader h cols =
  let line = intercalate "," (map csvEscapeString cols) <> "\n"
   in withString line (hputString h)

-- | Stream rows to an already-open 'CFile' handle, optionally preceded by a header row.
drainCSVHandle ::
  ( CmdRef m e,
    CmdWhile m e,
    CmdPutString m e,
    ExpBool e,
    JanusTyped e Bool,
    ToCSVRow m e row
  ) =>
  Maybe [String] ->
  e (Ptr CFile) ->
  Drain m row ()
drainCSVHandle mHeader h = Drain $ \s -> do
  case mHeader of
    Nothing -> pure ()
    Just cols -> emitCSVHeader h cols
  runDrain
    ( drainForEach $ \row -> do
        emitCSVRow (toCSVRow row) h
        withString "\n" (hputString h)
    )
    s

-- | Open a file at a dynamic @e CString@ path in write mode (@\"w\"@), write an
-- optional CSV header and all streamed rows, and close the file handle.
drainCSVFileCString ::
  ( CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdPutString m e,
    ExpBool e,
    ExpPtrWord e,
    ExpEq e Word64,
    Num (e Word64),
    JanusTyped e Bool,
    ToCSVRow m e row
  ) =>
  e CString ->
  Maybe [String] ->
  Drain m row ()
drainCSVFileCString cpath mHeader = Drain $ \s ->
  withString "w" $ \cmode -> do
    h <- fopen cpath cmode
    whenM_ (ptrToWord64Expr h `ne` 0) $ do
      runDrain (drainCSVHandle mHeader h) s
      void (fclose h)

-- | Open @path@ in write mode (@\"w\"@), write an optional CSV header and all
-- streamed rows, and close the file handle.
drainCSVFile ::
  ( CmdRef m e,
    CmdCond m e,
    CmdWhile m e,
    CmdPutString m e,
    ExpBool e,
    ExpPtrWord e,
    ExpEq e Word64,
    Num (e Word64),
    JanusTyped e Bool,
    ToCSVRow m e row
  ) =>
  FilePath ->
  Maybe [String] ->
  Drain m row ()
drainCSVFile path mHeader = Drain $ \s ->
  withString path $ \cpath ->
    runDrain (drainCSVFileCString cpath mHeader) s
