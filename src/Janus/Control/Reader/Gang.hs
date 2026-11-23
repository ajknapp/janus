{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 -Wno-orphans #-}

-- | Optimal non-splitting SIMD gang dispatch ('CmdOptimalGang') for
-- @('ReaderT' r m, 'ReaderExp' r e)@, isolated in a @-O0@ module for fast compilation.
module Janus.Control.Reader.Gang () where

import Control.Monad.Reader (ReaderT (..))
import GHC.TypeNats (natVal)
import Janus.Control.Reader.Base (ReaderExp)
import Janus.SPMD.Gang (CmdOptimalGang (..), SPMDGang, dispatchGangInt)

instance
  ( CmdOptimalGang m e,
    SPMDGang 2 (ReaderT r m) (ReaderExp r e),
    SPMDGang 4 (ReaderT r m) (ReaderExp r e),
    SPMDGang 8 (ReaderT r m) (ReaderExp r e),
    SPMDGang 16 (ReaderT r m) (ReaderExp r e),
    SPMDGang 32 (ReaderT r m) (ReaderExp r e)
  ) =>
  CmdOptimalGang (ReaderT r m) (ReaderExp r e)
  where
  withOptimalGang pa k =
    ReaderT $ \env ->
      withOptimalGang pa $ \pn ->
        runReaderT
          (dispatchGangInt @(ReaderT r m) @(ReaderExp r e) (fromIntegral (natVal pn)) k)
          env
