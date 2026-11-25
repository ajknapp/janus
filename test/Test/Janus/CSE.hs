{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.CSE where

import Control.Lens ((&), (.~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Char (ord)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import Data.List (isInfixOf, isPrefixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.C (CChar)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Alloc.Policy (dynamicAlloc)
import Janus.Backend.C
  ( JanusC,
    JanusCCSE,
    JanusCM,
    JanusCMCSE,
    acquireJanusC,
    releaseJanusC,
    showJanusC,
    showRawJanusC,
    withJanusC,
  )
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.CUDA
  ( CUDADejaFu,
    CUDADejaFuM,
    CUDAIO,
    CUDAIOM,
    CUDAThreadInfo (..),
    JanusCUDA,
    JanusCUDACSE,
    JanusCUDAM,
    JanusCUDAMCSE,
    launchCUDADejaFuM,
    launchCUDAIOM,
    showJanusCUDA,
    showRawJanusCUDA,
  )
import Janus.Backend.C.CUDA.Foreign (CUlaunchConfig (..), CUstream (..))
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.Backend.C.TH (Safety (Safe, Unsafe), declareJanusC, spliceJanusC)
import Janus.CSE
  ( CSEExp,
    CSEM,
    CSETyped,
    cse,
    withCSE,
  )
import Janus.Command.Cond (CmdCond, whenM_)
import Janus.Command.Format (CmdString (withString))
import qualified Janus.Command.Memory as M
import Janus.Command.Range (CmdRange, rangeM)
import Janus.Command.Ref (CmdRef, letM, modifyRef, newRef, readRef, writeRef)
import Janus.Control.Reader (ReaderExp (..), liftReaderExp)
import qualified Janus.Data.DynVec as DV
import Janus.Expression.Bool (ExpBool (land))
import Janus.Expression.Cast (ExpIntegralCast (toIntegral), ExpPtrCast (ptrCast))
import Janus.Expression.Cond (ExpCond (ifThenElse))
import Janus.Expression.Eq (ExpEq (ne))
import Janus.Expression.Inject (ExpInject (inject))
import Janus.Expression.Integral (ExpIntegral (quotient, remainder))
import Janus.Expression.Ord (ExpOrd (gt, le, lt))
import Janus.Expression.SIMD
  ( ExpSIMD,
    ExpSIMDFloat (vfma),
    HostSIMDValue (simdFromList, simdToList),
    SIMDVector,
  )
import Janus.Fix (ExpFix (fun))
import Janus.SPMD
  ( CmdOptimalGang,
    SPMD,
    foldForeachOptimal0M,
    gangReduceAdd,
  )
import Janus.Typed (JanusTyped)
import System.Environment (lookupEnv)
import Test.Janus.CSEDefs
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, (@?=))
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------
-- Template Haskell Static Compilation & Expression Splices with `withCSE`
--------------------------------------------------------------------------------

$(declareJanusC Unsafe "th_cse_canonical" thCanonicalCSE)

$(declareJanusC Safe "th_cse_chain40" thDeepChain40CSE)

splicedDeepChain40 :: Int64 -> Int64
splicedDeepChain40 =
  $(spliceJanusC Unsafe "th_spliced_cse_chain40" thDeepChain40CSE)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

getCSETestConfig :: IO CBuildConfig
getCSETestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

-- | Count non-overlapping occurrences of `needle` in `haystack`.
countOccurrences :: String -> String -> Int
countOccurrences [] _ = 0
countOccurrences needle haystack = go haystack
  where
    nLen = length needle
    go [] = 0
    go s@(_ : rest)
      | needle `isPrefixOf` s = 1 + go (drop nLen s)
      | otherwise = go rest

canonicalSpec :: Int64 -> (Int64, Int64)
canonicalSpec y =
  let x = (y * 17 + 23) * (y - 5)
   in (x + 10, x * 3 + 2)

--------------------------------------------------------------------------------
-- 1. User's Canonical Monadic Tuple Sharing Scenario
--------------------------------------------------------------------------------

unit_canonical_monadic_tuple_cse :: IO ()
unit_canonical_monadic_tuple_cse = do
  cfg <- getCSETestConfig

  -- A. Direct `CSEM IO Identity` returning a Haskell tuple `(Identity Int64, Identity Int64)`,
  -- verifying both exact tuple output and strict monadic effect ordering (`bar` before `quux`).
  withArray [0, 0, 0 :: Int64] $ \logPtr -> do
    let ioFn =
          withCSE
            (canonicalTupleProg @(CSEM IO Identity) @(CSEExp Identity))
    (Identity a, Identity b) <- ioFn (Identity 12) (Identity logPtr)
    let (expA, expB) = canonicalSpec 12
    (a, b) @?= (expA, expB)
    logged <- peekArray 3 logPtr
    logged @?= [2, 1001, 2002]

  -- B. Inspect generated C code without CSE (`showRawJanusC`) vs automatic CSE (`showJanusC`)
  -- and verify `showJanusC` is idempotent with explicit `cse`.
  -- In `canonicalOutProg`, `(a, b)` are each used twice (poke + return), so without CSE
  -- `x = (y * 17 + 23) * (y - 5)` is emitted 4 times; with CSE, `x`, `a`, and `b` are each
  -- computed once!
  rawC <- showRawJanusC "canonical_raw" (canonicalOutProg @JanusCM @JanusC)
  optC <- showJanusC "canonical_cse" (canonicalOutProg @JanusCMCSE @JanusCCSE)
  optIdempC <- showJanusC "canonical_cse" (cse (canonicalOutProg @JanusCMCSE @JanusCCSE))
  optIdempC @?= optC

  countOccurrences "17LL" rawC @?= 4
  countOccurrences "23LL" rawC @?= 4
  countOccurrences "17LL" optC @?= 1
  countOccurrences "23LL" optC @?= 1

  -- C. Execute dynamically compiled C (`withJanusC`) and statically linked TH (`th_cse_canonical`).
  withJanusC cfg (canonicalOutProg @JanusCMCSE @JanusCCSE) $ \cFn -> do
    forM_ [-19, -5, 0, 5, 12, 100 :: Int64] $ \y -> do
      let (expA, expB) = canonicalSpec y
      withArray [0, 0 :: Int64] $ \outPtr ->
        withArray [0, 0, 0 :: Int64] $ \logPtr -> do
          cRet <- cFn y outPtr logPtr
          cRet @?= expA * 31 + expB
          [ca, cb] <- peekArray 2 outPtr
          (ca, cb) @?= (expA, expB)
          logged <- peekArray 3 logPtr
          logged @?= [2, 1001, 2002]

      withArray [0, 0 :: Int64] $ \thOutPtr ->
        withArray [0, 0, 0 :: Int64] $ \thLogPtr -> do
          thRet <- th_cse_canonical y thOutPtr thLogPtr
          thRet @?= expA * 31 + expB
          [ta, tb] <- peekArray 2 thOutPtr
          (ta, tb) @?= (expA, expB)
          thLogged <- peekArray 3 thLogPtr
          thLogged @?= [2, 1001, 2002]

canonicalHedgehogProp :: IO (Int64 -> Ptr Int64 -> Ptr Int64 -> IO Int64) -> Property
canonicalHedgehogProp getCFn = property $ do
  cFn <- liftIO getCFn
  y <- forAll (Gen.int64 (Range.linear (-10000) 10000))
  (cRet, ca, cb, cLog, ia, ib, iLog) <- liftIO $ do
    (cRet', ca', cb', cLog') <-
      withArray [0, 0 :: Int64] $ \outP ->
        withArray [0, 0, 0 :: Int64] $ \logP -> do
          r <- cFn y outP logP
          [a, b] <- peekArray 2 outP
          lg <- peekArray 3 logP
          pure (r, a, b, lg)
    (ia', ib', iLog') <- withArray [0, 0, 0 :: Int64] $ \p -> do
      (Identity a, Identity b) <-
        withCSE (canonicalTupleProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity y) (Identity p)
      lg <- peekArray 3 p
      pure (a, b, lg)
    pure (cRet', ca', cb', cLog', ia', ib', iLog')
  (ca, cb) === canonicalSpec y
  cRet === ca * 31 + cb
  (ca, cb) === (ia, ib)
  cLog === [2, 1001, 2002]
  iLog === [2, 1001, 2002]

test_cse_canonical_property :: TestTree
test_cse_canonical_property =
  let acquire = do
        cfg <- getCSETestConfig
        acquireJanusC cfg (canonicalOutProg @JanusCMCSE @JanusCCSE)
   in withResource acquire releaseJanusC $ \res ->
        testGroup
          "CSE canonical property"
          [testProperty "compiled C == IO == spec across random inputs" (canonicalHedgehogProp (fst <$> res))]

--------------------------------------------------------------------------------
-- 2. Deep Chained Haskell `let` Pyramid (O(N) vs 2^40 Blowup)
--------------------------------------------------------------------------------

unit_deep_let_pyramid_40_levels :: IO ()
unit_deep_let_pyramid_40_levels = do
  cfg <- getCSETestConfig
  let ioChain40 = withCSE (deepChain @(CSEExp Identity) 40)
  runIdentity (ioChain40 (Identity 3)) @?= 3 * (2 ^ (40 :: Int))

  cCode <- showJanusC "chain40" thDeepChain40CSE
  -- Exactly 39 intermediate `int64_t x_k = x_{k-1} + x_{k-1};` bindings + 1 return
  countOccurrences "int64_t x_" cCode @?= 39
  assertBool "expected concise linear C code" (length (lines cCode) <= 100)

  cRes <- withJanusC cfg thDeepChain40CSE (\f -> pure (f 5))
  cRes @?= 5 * (2 ^ (40 :: Int))
  th_cse_chain40 7 @?= 7 * (2 ^ (40 :: Int))
  splicedDeepChain40 (-3) @?= (-3) * (2 ^ (40 :: Int))

--------------------------------------------------------------------------------
-- 3. Minimal DAG Temporaries (Effective Use Counts) & Copy Propagation
--------------------------------------------------------------------------------

nestedTwoUses ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64)
nestedTwoUses y logPtr = do
  let fy = y * 31 + 19
      gfy = fy * 43 + 29
  a <- barEffect gfy logPtr
  b <- quuxEffect gfy logPtr
  pure (a + b)

nestedThreeUses ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64)
nestedThreeUses y logPtr = do
  let fy = y * 31 + 19
      gfy = fy * 43 + 29
  a <- barEffect gfy logPtr
  b <- quuxEffect gfy logPtr
  pure (a + b + fy)

mixedExplicitLetM ::
  (Monad m, CmdRef m e, M.CmdStorable m e Int64, Num (e Int64), JanusTyped e Int64) =>
  e Int64 ->
  e (Ptr Int64) ->
  m (e Int64)
mixedExplicitLetM y logPtr = do
  x <- letM (y * 53 + 61)
  a <- barEffect x logPtr
  b <- quuxEffect (y * 53 + 61) logPtr
  pure (a + b)

unit_minimal_dag_temporaries_and_copy_prop :: IO ()
unit_minimal_dag_temporaries_and_copy_prop = do
  cfg <- getCSETestConfig

  -- A. `g (f y)` used twice, `f y` NOT used elsewhere:
  -- Effective use count of `f y` is 1 (inside `g (f y)`), so `f y` is inlined into
  -- `g (f y)`'s single `letM` binding rather than emitting a redundant temporary!
  -- (`barEffect` and `quuxEffect` each have 1 `peek` + 1 CSE'd `c0 + 1` = 4 locals,
  -- plus 1 local for `g (f y)` = 5 total `int64_t x_` declarations.)
  twoC <-
    showJanusC
      "nested_two"
      (nestedTwoUses @JanusCMCSE @JanusCCSE)
  countOccurrences "31LL" twoC @?= 1
  countOccurrences "43LL" twoC @?= 1
  countOccurrences "int64_t x_" twoC @?= 5

  -- B. `g (f y)` used twice AND `f y` used directly once:
  -- Effective use count of `f y` is 2 (1 inside `g (f y)` + 1 in return), so BOTH
  -- `f y` and `g x_0` are bound in topological order (6 total `int64_t x_` declarations)!
  threeC <-
    showJanusC
      "nested_three"
      (nestedThreeUses @JanusCMCSE @JanusCCSE)
  countOccurrences "31LL" threeC @?= 1
  countOccurrences "43LL" threeC @?= 1
  countOccurrences "int64_t x_" threeC @?= 6

  -- C. Explicit `letM` + duplicate inline expression:
  -- Computed once, and post-pass copy propagation removes any trivial `x_1 = x_0;` copy.
  mixedC <-
    showJanusC
      "mixed_letm"
      (mixedExplicitLetM @JanusCMCSE @JanusCCSE)
  countOccurrences "53LL" mixedC @?= 1
  assertBool "copy propagation should eliminate = x_0; alias" (not ("= x_0;" `isInfixOf` mixedC))

  withJanusC cfg (mixedExplicitLetM @JanusCMCSE @JanusCCSE) $ \fn ->
    withArray [0, 0, 0 :: Int64] $ \p -> do
      res <- fn 10 p
      let x = 10 * 53 + 61
      res @?= (x + 10) + (x * 3 + 2)

--------------------------------------------------------------------------------
-- 4. Mutable `Ref` and Pointer `poke` Effect-Ordering Soundness
--------------------------------------------------------------------------------

refSnapshotProg ::
  (Monad m, CmdRef m e, Num (e Int64), JanusTyped e Int64) =>
  e Int64 ->
  m (e Int64)
refSnapshotProg initVal = do
  r <- newRef initVal
  y1 <- readRef r
  writeRef r (y1 + 100)
  y2 <- readRef r
  pure ((y1 * 37 + 11) + (y1 * 37 + 11) + (y2 * 37 + 11))

ptrAliasingProg ::
  (Monad m, M.CmdStorable m e Int64, Num (e Int64)) =>
  e (Ptr Int64) ->
  m (e Int64)
ptrAliasingProg p = do
  v1 <- M.peek p
  M.poke p (v1 + 50)
  v2 <- M.peek p
  pure ((v1 * 41 + 7) + (v1 * 41 + 7) + (v2 * 41 + 7))

unit_mutable_ref_and_pointer_ordering_soundness :: IO ()
unit_mutable_ref_and_pointer_ordering_soundness = do
  cfg <- getCSETestConfig

  -- A. Mutable `Ref`: `y1 * 37 + 11` is shared across `writeRef` (since `y1` is immutable SSA),
  -- while `y2 * 37 + 11` uses the distinct post-write snapshot `y2`.
  refC <- showJanusC "ref_snap" (refSnapshotProg @JanusCMCSE @JanusCCSE)
  countOccurrences "37LL" refC @?= 2

  let refExpected z = 2 * (z * 37 + 11) + ((z + 100) * 37 + 11)
  Identity ioRefRes <-
    withCSE (refSnapshotProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity 9)
  ioRefRes @?= refExpected 9
  cRefRes <-
    withJanusC cfg (refSnapshotProg @JanusCMCSE @JanusCCSE) (\f -> f 9)
  cRefRes @?= refExpected 9

  -- B. Pointer `peek`/`poke`: `peek p` is effectful (never CSE'd across `poke p`),
  -- while pure math on `v1` IS CSE'd.
  ptrC <- showJanusC "ptr_alias" (ptrAliasingProg @JanusCMCSE @JanusCCSE)
  countOccurrences "41LL" ptrC @?= 2

  let ptrExpected v = 2 * (v * 41 + 7) + ((v + 50) * 41 + 7)
  alloca $ \p -> do
    poke p (15 :: Int64)
    Identity ioPtrRes <-
      withCSE (ptrAliasingProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity p)
    ioPtrRes @?= ptrExpected 15
    afterIO <- peek p
    afterIO @?= 65

    poke p (15 :: Int64)
    cPtrRes <-
      withJanusC cfg (ptrAliasingProg @JanusCMCSE @JanusCCSE) (\f -> f p)
    cPtrRes @?= ptrExpected 15
    afterC <- peek p
    afterC @?= 65

--------------------------------------------------------------------------------
-- 5. Conditional Hoisting & Trap Safety (`quotient` / `remainder` Non-Speculation)
--------------------------------------------------------------------------------

siblingHoistProg ::
  (Monad m, CmdRef m e, CmdCond m e, ExpOrd e Int64, Num (e Int64), JanusTyped e Int64) =>
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e Int64)
siblingHoistProg y flag1 flag2 = do
  acc <- newRef 0
  let shared = (y * 59 + 67) * (y - 3)
  whenM_ (flag1 `gt` 0) $ modifyRef acc (+ shared)
  whenM_ (flag2 `gt` 0) $ modifyRef acc (+ (shared * 2))
  readRef acc

pureGuardedDiv ::
  (ExpCond e, ExpEq e Int64, ExpIntegral e Int64, Num (e Int64), JanusTyped e Int64) =>
  e Int64 ->
  e Int64 ->
  e Int64
pureGuardedDiv x y =
  ifThenElse (y `ne` 0) ((x `quotient` y) + (x `quotient` y)) 777

monadicGuardedDiv ::
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    ExpEq e Int64,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  e Int64 ->
  m (e Int64)
monadicGuardedDiv x y = do
  out <- newRef 999
  whenM_ (y `ne` 0) $
    writeRef out ((x `quotient` y) + (x `remainder` y) + (x `quotient` y))
  readRef out

shortCircuitGuardedDiv ::
  (ExpBool e, ExpEq e Int64, ExpOrd e Int64, ExpIntegral e Int64, Num (e Int64)) =>
  e Int64 ->
  e Int64 ->
  e Bool
shortCircuitGuardedDiv x y =
  (y `ne` 0) `land` (((x `quotient` y) + (x `quotient` y)) `gt` 10)

zeroTripLoopDiv ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpIntegral e Int64,
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  e Int64 ->
  m (e Int64)
zeroTripLoopDiv x n = do
  acc <- newRef 0
  rangeM 0 n $ \_ ->
    modifyRef acc (+ ((x `quotient` n) + (x `quotient` n)))
  readRef acc

unit_conditional_hoisting_and_trap_safety :: IO ()
unit_conditional_hoisting_and_trap_safety = do
  cfg <- getCSETestConfig

  -- 1. Non-trapping pure expression shared across sibling `whenM_` blocks IS hoisted
  hoistC <- showJanusC "sib_hoist" (siblingHoistProg @JanusCMCSE @JanusCCSE)
  countOccurrences "59LL" hoistC @?= 1
  withJanusC cfg (siblingHoistProg @JanusCMCSE @JanusCCSE) $ \f -> do
    f 10 0 0 >>= (@?= 0)
    f 10 1 1 >>= (@?= 3 * ((10 * 59 + 67) * 7))

  -- 2. Pure `ifThenElse` guarding `quotient`: shared INSIDE true branch, never speculated on y == 0
  pureDivC <- showJanusC "pure_div" (pureGuardedDiv @JanusCCSE)
  countOccurrences "/" pureDivC @?= 1
  runIdentity (withCSE (pureGuardedDiv @(CSEExp Identity)) 100 0) @?= 777
  runIdentity (withCSE (pureGuardedDiv @(CSEExp Identity)) 43 5) @?= 16
  withJanusC cfg (pureGuardedDiv @JanusCCSE) $ \f -> do
    f 100 0 @?= 777
    f 43 5 @?= 16

  -- 3. Monadic `whenM_` guarding `quotient` & `remainder`: shared inside `whenM_`, safe on y == 0
  monDivC <- showJanusC "mon_div" (monadicGuardedDiv @JanusCMCSE @JanusCCSE)
  countOccurrences "/" monDivC @?= 1
  Identity ioMon0 <- withCSE (monadicGuardedDiv @(CSEM IO Identity) @(CSEExp Identity)) 100 0
  ioMon0 @?= 999
  withJanusC cfg (monadicGuardedDiv @JanusCMCSE @JanusCCSE) $ \f -> do
    f 100 0 >>= (@?= 999)
    f 50 7 >>= (@?= 15)

  -- 4. Short-circuit `land` guarding `quotient`: never lifted into `jcfBlock` before `&&`!
  runIdentity (withCSE (shortCircuitGuardedDiv @(CSEExp Identity)) 100 0) @?= False
  runIdentity (withCSE (shortCircuitGuardedDiv @(CSEExp Identity)) 21 3) @?= True
  withJanusC cfg (shortCircuitGuardedDiv @JanusCCSE) $ \f -> do
    f 100 0 @?= False
    f 21 3 @?= True
    f 12 3 @?= False

  -- 5. Zero-trip `rangeM 0 n` guarding `x quotient n`: stays inside loop body, safe when n == 0!
  loopDivC <- showJanusC "loop_div" (zeroTripLoopDiv @JanusCMCSE @JanusCCSE)
  countOccurrences "/" loopDivC @?= 1
  withJanusC cfg (zeroTripLoopDiv @JanusCMCSE @JanusCCSE) $ \f -> do
    f 100 0 >>= (@?= 0)
    f 20 4 >>= (@?= 40)

--------------------------------------------------------------------------------
-- 6. Scoped Commands (`withRefPtr` & `withString`) with Live-Out SSA Values
--------------------------------------------------------------------------------

scopedRefPtrProg ::
  ( Monad m,
    CmdRef m e,
    M.CmdRefPtr m e Int64,
    M.CmdStorable m e Int64,
    M.ExpPtr e Int64,
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  m (e Int64)
scopedRefPtrProg x0 = do
  r <- newRef x0
  outRef <- newRef 0
  M.withRefPtr r $ \p -> do
    v <- M.peek (M.ptrAdd p 0)
    let liveExpr = v * 71 + 13
    M.poke (M.ptrAdd p 0) (liveExpr + 5)
    writeRef outRef liveExpr
  afterMut <- readRef r
  saved <- readRef outRef
  -- Also compute a shared expression over `x0` spanning inside and outside `withRefPtr`
  pure (afterMut + saved + (x0 * 79 + 3) + (x0 * 79 + 3))

scopedStringProg ::
  forall m e.
  ( Monad m,
    CmdString m e,
    M.CmdStorable m e Word8,
    ExpPtrCast e,
    ExpIntegralCast e Word8 Int64,
    JanusTyped e CChar,
    JanusTyped e Word8,
    Num (e Int64)
  ) =>
  e Int64 ->
  m (e Int64)
scopedStringProg idx = do
  withString "JanusCSE" $ \cs -> do
    let bytePtr = ptrCast cs :: e (Ptr Word8)
        off = idx + 1
    b1 <- M.peekElemOff bytePtr off
    b2 <- M.peekElemOff bytePtr off
    pure (toIntegral b1 + toIntegral b2)

unit_scoped_withRefPtr_and_withString :: IO ()
unit_scoped_withRefPtr_and_withString = do
  cfg <- getCSETestConfig

  scopedC <- showJanusC "scoped_refptr" (scopedRefPtrProg @JanusCMCSE @JanusCCSE)
  countOccurrences "71LL" scopedC @?= 1
  countOccurrences "79LL" scopedC @?= 1

  let expected x =
        let live = x * 71 + 13
         in (live + 5) + live + 2 * (x * 79 + 3)
  Identity ioRes <- withCSE (scopedRefPtrProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity 6)
  ioRes @?= expected 6
  cRes <- withJanusC cfg (scopedRefPtrProg @JanusCMCSE @JanusCCSE) (\f -> f 6)
  cRes @?= expected 6

  Identity ioStr <- withCSE (scopedStringProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity 4)
  ioStr @?= fromIntegral (2 * ord 'C')
  cStr <- withJanusC cfg (scopedStringProg @JanusCMCSE @JanusCCSE) (\f -> f 4)
  cStr @?= fromIntegral (2 * ord 'C')

--------------------------------------------------------------------------------
-- 7. Recursive `ExpFix` (`fun`) — Pure and Monadic
--------------------------------------------------------------------------------

-- | Without CSE inside `fun`, `self (n - 1) + self (n - 1) + 1` makes 2^n calls.
-- With CSE, `self (n - 1)` is shared inside the `ifThenElse` false branch (1 call per level)
-- AND is not speculated above `ifThenElse` (terminates cleanly at n <= 0).
dupTreeRec ::
  forall e.
  (Num (e Int64), ExpCond e, ExpOrd e Int64, ExpFix e (e Int64 -> e Int64), JanusTyped e Int64) =>
  e Int64 ->
  e Int64
dupTreeRec =
  fun (Proxy @e) "dup_tree" $ \self n ->
    ifThenElse (n `le` 0) 1 (self (n - 1) + self (n - 1) + 1)

monadicRecProg ::
  forall m e.
  ( Monad m,
    CmdRef m e,
    CmdCond m e,
    ExpOrd e Int64,
    ExpFix e (e Int64 -> m (e Int64)),
    Num (e Int64),
    JanusTyped e Int64
  ) =>
  e Int64 ->
  m (e Int64)
monadicRecProg =
  fun (Proxy @e) "mon_rec" $ \self n -> do
    out <- newRef 0
    let term = n * 73 + 19
    whenM_ (n `gt` 0) $ do
      sub <- self (n - 1)
      writeRef out (sub + term + term)
    readRef out

unit_recursive_expfix_pure_and_monadic :: IO ()
unit_recursive_expfix_pure_and_monadic = do
  cfg <- getCSETestConfig

  rawRecC <- showRawJanusC "dup_tree_raw" (dupTreeRec @JanusC)
  recC <- showJanusC "dup_tree_main" (dupTreeRec @JanusCCSE)
  -- `jfix` mangles `"dup_tree"` as `"janus_local_dup_tree_<hash>"`; CSE eliminates
  -- one of the two recursive calls inside `dup_tree`, reducing total occurrences by 1!
  (countOccurrences "janus_local_dup_tree_" rawRecC - countOccurrences "janus_local_dup_tree_" recC) @?= 1

  runIdentity (withCSE (dupTreeRec @(CSEExp Identity)) 0) @?= 1
  runIdentity (withCSE (dupTreeRec @(CSEExp Identity)) 35) @?= (2 ^ (36 :: Int)) - 1

  withJanusC cfg (dupTreeRec @JanusCCSE) $ \f -> do
    f 0 @?= 1
    f 35 @?= (2 ^ (36 :: Int)) - 1

  monRecC <- showJanusC "mon_rec_main" (monadicRecProg @JanusCMCSE @JanusCCSE)
  countOccurrences "73LL" monRecC @?= 1

  let monExpected k = sum [2 * (i * 73 + 19) | i <- [1 .. k]]
  Identity ioMon <- withCSE (monadicRecProg @(CSEM IO Identity) @(CSEExp Identity)) (Identity 10)
  ioMon @?= monExpected 10
  cMon <- withJanusC cfg (monadicRecProg @JanusCMCSE @JanusCCSE) (\f -> f 10)
  cMon @?= monExpected 10

--------------------------------------------------------------------------------
-- 8. Multi-Backend Coverage: SIMD (`LinuxX86_64C`), SPMD (`CmdOptimalGang`), & `ReaderT`
--------------------------------------------------------------------------------

simdDupKernel ::
  (ExpSIMD e, ExpSIMDFloat e 4 Float, Num (e (SIMDVector e 4 Float))) =>
  e (SIMDVector e 4 Float) ->
  e (SIMDVector e 4 Float) ->
  e (SIMDVector e 4 Float)
simdDupKernel va vb =
  let s = va + vb
   in vfma s s (s * s)

spmdFoldCSE ::
  (Monad m, CmdOptimalGang m e, Num (e Int64)) =>
  e Int64 ->
  m (e Int64)
spmdFoldCSE count =
  foldForeachOptimal0M
    (Proxy @Int64)
    count
    0
    (\acc i -> let x = i * 3 + 1 in pure (acc + x * x))
    gangReduceAdd

readerCSEProg ::
  (Monad m, MonadReader r m, CmdRef m (ReaderExp r e), Num (e Int64), JanusTyped e Int64) =>
  (r -> e Int64) ->
  ReaderExp r e Int64 ->
  m (ReaderExp r e Int64)
readerCSEProg getScale x = do
  scale <- asks (liftReaderExp . getScale)
  r <- newRef ((x * 83 + 17) * scale)
  v <- readRef r
  pure (v + (x * 83 + 17) * scale)

unit_simd_spmd_and_reader_cse :: IO ()
unit_simd_spmd_and_reader_cse = do
  cfg <- getCSETestConfig

  -- A. SIMD (`LinuxX86_64C` & `Identity`): `vadd va vb` computed once instead of 4 times
  simdRawC <- showRawJanusC "simd_raw" (simdDupKernel @LinuxX86_64C)
  simdOptC <- showJanusC "simd_cse" (simdDupKernel @LinuxX86_64CCSE)
  countOccurrences "_mm_add_ps" simdRawC @?= 4
  countOccurrences "_mm_add_ps" simdOptC @?= 1

  let aLanes = [1.0, 2.0, 3.0, 4.0 :: Float]
      bLanes = [0.5, 1.5, 2.5, 3.5 :: Float]
      expectedLanes = [2 * (a + b) * (a + b) | (a, b) <- zip aLanes bLanes]
      Identity idVec =
        withCSE
          (simdDupKernel @(CSEExp Identity))
          (Identity (simdFromList aLanes))
          (Identity (simdFromList bLanes))
  simdToList idVec @?= expectedLanes

  cVec <-
    withJanusC cfg (simdDupKernel @LinuxX86_64CCSE) $ \f ->
      pure (f (simdFromList aLanes) (simdFromList bLanes))
  simdToList cVec @?= expectedLanes

  -- B. SPMD (`CmdOptimalGang`) with automatic CSE across `IO` and `LinuxX86_64C`
  let spmdSpec n = sum [(i * 3 + 1) * (i * 3 + 1) | i <- [0 .. n - 1]]
  Identity ioSpmd <-
    withCSE (spmdFoldCSE @(CSEM IO Identity) @(CSEExp Identity)) (Identity 37)
  ioSpmd @?= spmdSpec 37
  cSpmd <-
    withJanusC cfg (spmdFoldCSE @LinuxX86_64CMCSE @LinuxX86_64CCSE) $ \f ->
      f 37
  cSpmd @?= spmdSpec 37

  -- C. `ReaderT` / `ReaderExp` both over `CSEM` AND as a target of `interpretCSE`
  let runReaderOverCSE ::
        JanusCCSE Int64 ->
        JanusCCSE Int64 ->
        JanusCMCSE (JanusCCSE Int64)
      runReaderOverCSE scale arg =
        getReaderExp <$> runReaderT (readerCSEProg id (ReaderExp arg)) scale
  readerC <- showJanusC "reader_cse" runReaderOverCSE
  countOccurrences "83LL" readerC @?= 1
  withJanusC cfg runReaderOverCSE $ \f ->
    f 4 10 >>= (@?= 2 * ((10 * 83 + 17) * 4))

  let runTargetReaderC :: JanusCCSE Int64 -> JanusCMCSE (JanusCCSE Int64)
      runTargetReaderC arg =
        getReaderExp
          <$> runReaderT
            ( withCSE
                ( refSnapshotProg
                    @(CSEM (ReaderT () JanusCMCSE) (ReaderExp () JanusCCSE))
                    @(CSEExp (ReaderExp () JanusCCSE))
                )
                (ReaderExp arg)
            )
            ()
  withJanusC cfg runTargetReaderC $ \f ->
    f 9 >>= (@?= 2 * (9 * 37 + 11) + (109 * 37 + 11))

  -- D. Zero-boilerplate `HKD` record parameter CSE
  hkdC <- showJanusC "hkd_cse" (hkdInputProg @JanusCCSE)
  countOccurrences "89LL" hkdC @?= 1
  hkdVal <-
    withJanusC cfg (hkdInputProg @JanusCCSE) $ \f ->
      pure (f (Pair64 (Identity 3) (Identity 7)) 5)
  let hkdShared = 3 * 89 + 7 * 5
  hkdVal @?= hkdShared * hkdShared + hkdShared

  -- E. Stacked `CSEExp (CSEExp JanusC)`, direct `SPMD` parameter/return CSE, & container handle CSE
  let stackedChain :: JanusCCSE Int64 -> JanusCCSE Int64
      stackedChain = withCSE (deepChain @(CSEExp JanusCCSE) 20)
  stackedC <- showJanusC "stacked_chain20" stackedChain
  countOccurrences "int64_t x_" stackedC @?= 19
  withJanusC cfg stackedChain (\f -> f 3 @?= 3 * (2 ^ (20 :: Int)))

  let spmdDirectDup :: SPMD 4 LinuxX86_64CCSE Float -> SPMD 4 LinuxX86_64CCSE Float -> SPMD 4 LinuxX86_64CCSE Float
      spmdDirectDup xs ys =
        let s = xs + ys
         in s * s + s
  spmdDirectC <- showJanusC "spmd_direct_cse" spmdDirectDup
  countOccurrences "_mm_add_ps" spmdDirectC @?= 2
  withJanusC cfg spmdDirectDup $ \f ->
    f aLanes bLanes @?= [(a + b) * (a + b) + (a + b) | (a, b) <- zip aLanes bLanes]

  let dynVecDup :: DV.DynVec JanusCCSE Int64 -> JanusCCSE Int64 -> JanusCMCSE (JanusCCSE Int64)
      dynVecDup dv x = do
        let shared = x * 97 + 13
        DV.push dynamicAlloc dv shared
        DV.push dynamicAlloc dv (shared + 1)
        n <- DV.len dv
        pure (n + shared)
  dvC <- showJanusC "dynvec_cse" dynVecDup
  countOccurrences "97LL" dvC @?= 1
  dvHost <- DV.new @Int64 dynamicAlloc
  withJanusC cfg dynVecDup $ \f -> do
    r <- f dvHost 5
    r @?= 2 + (5 * 97 + 13)
  DV.free dynamicAlloc dvHost

--------------------------------------------------------------------------------
-- 9. CUDA Backend (`showJanusCUDA`, `CUDAIOM`, `CUDADejaFuM`)
--------------------------------------------------------------------------------

-- | `saxpy` written with an ordinary Haskell `let` (forgetting `letM`!) and 3x
-- repeated `toIntegral i`.
saxpyImplicit ::
  ( Monad m,
    MonadReader (CUDAThreadInfo e) m,
    CSETyped e Int32,
    CmdCond m e,
    CmdRef m e,
    M.CmdStorable m e a,
    ExpOrd e Int32,
    ExpIntegralCast e Int32 Int64,
    Num (e Int32),
    Num (e a)
  ) =>
  e Int32 ->
  e a ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
saxpyImplicit n a x y = do
  blockDim <- asks _cudaThreadInfoBlockDimX
  blockIdx <- asks _cudaThreadInfoBlockIdxX
  threadIdx <- asks _cudaThreadInfoThreadIdxX
  let i = blockDim * blockIdx + threadIdx
  whenM_ (i `lt` n) $ do
    xi <- M.peekElemOff x (toIntegral i)
    yi <- M.peekElemOff y (toIntegral i)
    M.pokeElemOff y (a * xi + yi) (toIntegral i)

cpuLaunch1D :: Int -> Int -> CUlaunchConfig
cpuLaunch1D gridX blockX =
  CUlaunchConfig
    { _cuLaunchConfigBlockDimX = fromIntegral blockX,
      _cuLaunchConfigBlockDimY = 1,
      _cuLaunchConfigBlockDimZ = 1,
      _cuLaunchConfigGridDimX = fromIntegral gridX,
      _cuLaunchConfigGridDimY = 1,
      _cuLaunchConfigGridDimZ = 1,
      _cuLaunchConfigSharedMemBytes = 0,
      _cuLaunchConfigStream = CUstream nullPtr
    }

unit_cuda_saxpy_cse_codegen_and_emulation :: IO ()
unit_cuda_saxpy_cse_codegen_and_emulation = do
  -- 1. Inspect generated CUDA C source (works even on machines without a GPU!)
  let rawCuda =
        showRawJanusCUDA
          "saxpy_raw"
          (saxpyImplicit @JanusCUDAM @JanusCUDA @Float)
      optCuda =
        showJanusCUDA
          "saxpy_cse"
          (saxpyImplicit @JanusCUDAMCSE @JanusCUDACSE @Float)
      optCudaIdemp =
        showJanusCUDA
          "saxpy_cse"
          (cse (saxpyImplicit @JanusCUDAMCSE @JanusCUDACSE @Float))
  optCudaIdemp @?= optCuda

  -- Without CSE, `blockDim.x * blockIdx.x + threadIdx.x` is emitted 4 times
  -- and `(int64_t)` cast is emitted 3 times.
  countOccurrences "blockDim.x * blockIdx.x + threadIdx.x" rawCuda @?= 4
  countOccurrences "(int64_t)" rawCuda @?= 3

  -- With CSE, `i` is bound once in the entry block AND `toIntegral i` is bound
  -- once inside the `whenM_` block!
  countOccurrences "blockDim.x * blockIdx.x + threadIdx.x" optCuda @?= 1
  countOccurrences "(int64_t)" optCuda @?= 1

  -- 2. Execute CSE-optimized CUDA kernel on `CUDAIOM`
  let nIO = 95
      nbytesIO = nIO * sizeOf (undefined :: Float)
  allocaBytes nbytesIO $ \px -> allocaBytes nbytesIO $ \py -> do
    forM_ [0 .. nIO - 1] $ \i -> do
      pokeElemOff px i (fromIntegral i :: Float)
      pokeElemOff py i (fromIntegral (10 * i) :: Float)
    let optIO =
          withCSE
            (saxpyImplicit @(CSEM CUDAIOM CUDAIO) @(CSEExp CUDAIO) @Float)
    launchCUDAIOM (cpuLaunch1D 3 32) $
      optIO (fromIntegral nIO) 2.5 (inject px) (inject py)
    forM_ [0 .. nIO - 1] $ \i ->
      peekElemOff py i >>= (@?= (2.5 * fromIntegral i + fromIntegral (10 * i)))

  -- 3. Execute CSE-optimized CUDA kernel on `CUDADejaFuM`
  let nDF = 64
      nbytesDF = nDF * sizeOf (undefined :: Float)
  allocaBytes nbytesDF $ \px -> allocaBytes nbytesDF $ \py -> do
    forM_ [0 .. nDF - 1] $ \i -> do
      pokeElemOff px i (fromIntegral i :: Float)
      pokeElemOff py i (fromIntegral i :: Float)
    let optDF =
          withCSE
            (saxpyImplicit @(CSEM CUDADejaFuM CUDADejaFu) @(CSEExp CUDADejaFu) @Float)
    (res, _trace) <-
      launchCUDADejaFuM (cpuLaunch1D 2 32) $
        optDF (fromIntegral nDF) 3.0 (inject px) (inject py)
    res @?= Right ()
    forM_ [0 .. nDF - 1] $ \i ->
      peekElemOff py i >>= (@?= fromIntegral (4 * i))
