-- | Reusable Janus expression ('ReaderExp'), reference ('ReaderRef'), and
-- command instances for @('Control.Monad.Reader.ReaderT' r m, 'ReaderExp' r e)@,
-- together with Template Haskell derivers ('deriveJanusReader',
-- 'deriveJanusReaderGang') for custom @Reader@-backed monads.
module Janus.Control.Reader
  ( -- * Reader Expression, Reference, & Coercion Wrappers
    ReaderExp (..),
    liftReaderExp,
    ReaderRef (..),
    ExpNewtype (..),

    -- * Associated SIMD Wrappers for ReaderExp
    SIMDVector (..),
    SIMDMask (..),

    -- * Reusable TH Derivers for Custom Reader Monads
    JanusReaderSpec (..),
    deriveJanusReader,
    deriveJanusReaderGang,
  )
where

import Janus.Control.Reader.Base
import Janus.Control.Reader.Gang ()
import Janus.Control.Reader.TH
