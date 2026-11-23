{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O0 -Wno-orphans #-}

module Janus.Backend.C.LinuxX86_64.Gang where

import Janus.Backend.C.LinuxX86_64.SIMD.Arith ()
import Janus.Backend.C.LinuxX86_64.SIMD.Base ()
import Janus.Backend.C.LinuxX86_64.SIMD.Bits ()
import Janus.Backend.C.LinuxX86_64.SIMD.Cast ()
import Janus.Backend.C.LinuxX86_64.SIMD.Float ()
import Janus.Backend.C.LinuxX86_64.SIMD.MaskCmp ()
import Janus.Backend.C.LinuxX86_64.SIMD.Mem ()
import Janus.Backend.C.LinuxX86_64.SIMD.ReduceDot ()
import Janus.Backend.C.LinuxX86_64.SIMD.Shuffle ()
import Janus.Backend.C.LinuxX86_64.Types
import Janus.SPMD.Gang (CmdOptimalGang (..), dispatchGangInt, optimalX86GangSize)

--------------------------------------------------------------------------------
-- Optimal Non-Splitting Gang Dispatch for LinuxX86_64
--------------------------------------------------------------------------------

instance CmdOptimalGang LinuxX86_64CM LinuxX86_64C where
  withOptimalGang pa k = do
    fs <- askX86Features
    dispatchGangInt (optimalX86GangSize fs pa) k
