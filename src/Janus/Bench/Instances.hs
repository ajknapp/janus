{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Multi-parameter Janus expression, SIMD, fixpoint, memory, I/O, and control
-- instances for @('BenchT' c m e, 'Bench' c e)@, derived from
-- @('Control.Monad.Reader.ReaderT', 'Janus.Control.Reader.ReaderExp')@.
module Janus.Bench.Instances
  ( SIMDVector (..),
    SIMDMask (..),
  )
where

import Janus.Bench.Types
import Janus.Control.Reader.Base (SIMDMask (..), SIMDVector (..))
import Janus.Control.Reader.TH (deriveJanusReader)

deriveJanusReader benchReaderSpec
