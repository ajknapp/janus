{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Typeable (Typeable)
import GHC.TypeNats (natVal)
import Janus.Control.Reader.Base (ReaderExp)
import Janus.SPMD.Gang
  ( CmdOptimalGang (..),
    OptimalGangCaps (..),
    dispatchOptimalGang,
    liftWrapperTypedChain,
  )

instance
  (Typeable r, CmdOptimalGang m e) =>
  CmdOptimalGang (ReaderT r m) (ReaderExp r e)
  where
  optimalGangCaps =
    case optimalGangCaps @m @e of
      OptimalGangCaps ch ->
        OptimalGangCaps (liftWrapperTypedChain @(ReaderExp r e) @e ch)
  withOptimalGang pa k =
    ReaderT $ \env ->
      withOptimalGang @m @e pa $ \pn ->
        runReaderT
          ( dispatchOptimalGang
              (optimalGangCaps @(ReaderT r m) @(ReaderExp r e))
              (fromIntegral (natVal pn))
              pa
              k
          )
          env
