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
-- @('CSEM' m e, 'CSEExp' e)@, isolated in a @-O0@ module for fast compilation.
module Janus.CSE.Gang () where

import Data.Proxy (Proxy (..))
import Janus.CSE.Instances ()
import Janus.CSE.Types (CSEExp, CSEM (..), CSESIMDTarget, ProgTree (..))
import Janus.SPMD.Gang
  ( CmdOptimalGang (..),
    OptimalGangCaps (..),
    dispatchOptimalGang,
    liftStepTypedChain,
  )
import qualified Type.Reflection as TR

instance
  (CmdOptimalGang m e, CSESIMDTarget e) =>
  CmdOptimalGang (CSEM m e) (CSEExp e)
  where
  optimalGangCaps =
    case optimalGangCaps @m @e of
      OptimalGangCaps ch ->
        OptimalGangCaps (liftStepTypedChain @(CSEExp e) @e ch)
  withOptimalGang (pa :: Proxy a) k = CSEM $ \n0 ->
    let caps = optimalGangCaps @(CSEM m e) @(CSEExp e)
        (n1, t2) = unCSEM (dispatchOptimalGang caps 2 pa k) n0
        (n2, t4) = unCSEM (dispatchOptimalGang caps 4 pa k) n1
        (n3, t8) = unCSEM (dispatchOptimalGang caps 8 pa k) n2
        (n4, t16) = unCSEM (dispatchOptimalGang caps 16 pa k) n3
        (n5, t32) = unCSEM (dispatchOptimalGang caps 32 pa k) n4
     in (n5, ProgOptimalGang (TR.typeRep @a) t2 t4 t8 t16 t32)
