{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 -Wno-orphans #-}

-- | Optimal non-splitting SIMD gang dispatch ('Janus.SPMD.Gang.CmdOptimalGang')
-- for @('BenchT' c m e, 'Bench' c e)@, derived from "Janus.Control.Reader" in a
-- @-O0@ module for fast compilation.
module Janus.Bench.Gang () where

import Janus.Bench.Instances ()
import Janus.Bench.Types (benchReaderSpec)
import Janus.Control.Reader.TH (deriveJanusReaderGang)

deriveJanusReaderGang benchReaderSpec
