-- | Backend-agnostic algorithmic Common Subexpression Elimination for Janus.
--
-- Instantiate polymorphic Janus programs at @('CSEM' m e, 'CSEExp' e)@,
-- run 'cse' to eliminate common pure subexpressions across both pure code and
-- monadic commands/control flow without 'System.Mem.StableName.StableName's,
-- and lower the optimized AST into any target backend @(m, e)@ via
-- 'interpretCSE' (or in a single step via 'withCSE').
module Janus.CSE
  ( -- * CSE Monad, Expression, & Reference Types
    CSEExp,
    CSEM,
    CSERef,
    CSETyped,

    -- * Associated SIMD Wrappers for `CSEExp`
    SIMDVector (..),
    SIMDMask (..),

    -- * Target Backend Capability Classes
    CSETargetExp (..),
    CSESIMDTarget (..),
    SIMDVecWitness (..),
    SIMDMaskWitness (..),

    -- * Function Parameter, Return, & HKD Classes
    CSEHKD (..),
    CSEReturn (..),
    CSETargetReturn (..),
    CSEParam (..),
    CSEParamMonad,
    CSEFix (..),

    -- * Algorithmic Common Subexpression Elimination Pass
    CSEOptimize (..),
    cseExp,
    cseM,

    -- * Polymorphic Interpreter into Janus Typeclasses
    CSEInterpret (..),
    CSETarget,
    interpretCSEExp,
    interpretCSEM,
    withCSE,
  )
where

import Janus.CSE.Gang ()
import Janus.CSE.Instances
import Janus.CSE.Interpret
import Janus.CSE.Optimize
import Janus.CSE.Types
