{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.CSEDefs where

import Data.Int (Int64)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Backend.C (JanusC, JanusCM)
import Janus.CSE (CSEExp, CSEM, withCSE)
import qualified Janus.Command.Memory as M
import Janus.Data.HKD (HKD (..), JanusHKD)

data Pair64 f = Pair64
  { pFirst :: !(f Int64),
    pSecond :: !(f Int64)
  }
  deriving (Generic)

instance JanusHKD Pair64

-- | Effectful helper `bar`: increments counter at `logPtr[0]`, records `1000 + step`
-- at `logPtr[step]`, and returns `x + 10 * step`.
barEffect ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64)
barEffect x logPtr = do
  c0 <- M.peekElemOff logPtr 0
  let c1 = c0 + 1
  M.pokeElemOff logPtr c1 0
  M.pokeElemOff logPtr (1000 + c1) c1
  pure (x + 10 * c1)

-- | Effectful helper `quux`: increments counter at `logPtr[0]`, records `2000 + step`
-- at `logPtr[step]`, and returns `x * 3 + step`.
quuxEffect ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64)
quuxEffect x logPtr = do
  c0 <- M.peekElemOff logPtr 0
  let c1 = c0 + 1
  M.pokeElemOff logPtr c1 0
  M.pokeElemOff logPtr (2000 + c1) c1
  pure (x * 3 + c1)

-- | User's canonical scenario:
-- @do { let x = foo y; a <- bar x; b <- quux x; pure (a, b) }@
-- using an ordinary Haskell @let@ (no @let_@ or @letM@).
canonicalTupleProg ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64, e Int64)
canonicalTupleProg y logPtr = do
  let x = (y * 17 + 23) * (y - 5)
  a <- barEffect x logPtr
  b <- quuxEffect x logPtr
  pure (a, b)

canonicalOutProg ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  e (Ptr Int64) ->
  m (e Int64)
canonicalOutProg y outPtr logPtr = do
  (a, b) <- canonicalTupleProg y logPtr
  M.pokeElemOff outPtr a 0
  M.pokeElemOff outPtr b 1
  pure (a * 31 + b)

hkdInputProg :: (Num (e Int64)) => HKD Pair64 e -> e Int64 -> e Int64
hkdInputProg (HKD p) x =
  let shared = pFirst p * 89 + pSecond p * x
   in shared * shared + shared

-- | Deep chained Haskell @let@ pyramid (@let x_{k+1} = x_k + x_k@).
-- Without DAG CSE, tree unfolding has @2^n@ nodes.
deepChain :: (Num (e Int64)) => Int -> e Int64 -> e Int64
deepChain n x0 = iterate (\x -> x + x) x0 !! n

thCanonicalCSE :: JanusC Int64 -> JanusC (Ptr Int64) -> JanusC (Ptr Int64) -> JanusCM (JanusC Int64)
thCanonicalCSE =
  withCSE (canonicalOutProg @(CSEM JanusCM JanusC) @(CSEExp JanusC))

thDeepChain40CSE :: JanusC Int64 -> JanusC Int64
thDeepChain40CSE =
  withCSE (deepChain @(CSEExp JanusC) 40)
