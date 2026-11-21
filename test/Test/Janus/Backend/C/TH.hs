{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.TH where

import Control.Concurrent.Async (concurrently)
import Control.Lens ((%~), (&), (.~), (?~))
import Control.Monad (when)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C (JanusC, JanusCBackend (JC), compileJanusCNamed, renderJCProgram, jcparam)
import Janus.Backend.C.Build
  ( buildCachedJanusObject,
    cBuildConfigCFlags,
    cBuildConfigCacheDir,
    cBuildConfigX86Features,
    defaultCBuildConfig,
    defaultTHBuildConfig,
  )
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM, compileLinuxX86_64CNamed)
import Janus.Backend.C.TH
import Janus.Backend.C.Target
  ( admitsX86Features,
    observeHostX86Features,
    x86AVX2,
    x86Baseline,
  )
import Janus.Expression.SIMD
  ( HostSIMDMask (simdMaskFromBools),
    HostSIMDValue (simdFromList, simdToList),
    SIMDVector,
  )
import Language.Haskell.TH
  ( Callconv (CCall),
    Dec (ForeignD, FunD, PragmaD, SigD),
    Foreign (ImportF),
    Pragma (InlineP),
    mkName,
    runQ,
  )
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Janus.Backend.C.THDefs
import qualified Test.Janus.Backend.C.THOther as THOther
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------
-- Statically Compiled & Linked Janus Functions (Top-Level & Expression Splices)
--------------------------------------------------------------------------------

-- Entry point explicitly named "fac", matching internal ExpFix helper name "fac"
$(declareJanusC Unsafe "fac" (facPoly @JanusC @Int64))

-- Safe ccall with multiple specializations of polymorphic ExpFix helper in one TU
$(declareJanusC Safe "th_double_fac_sum" doubleFacSum)

-- 0-argument pure constant
$(declareJanusC Unsafe "th_const_42" constFortyTwo)

-- Both Unsafe and Safe ccall imports of the same Janus definition under distinct C names
$(declareJanusC Unsafe "th_affine_unsafe" affineI64)
$(declareJanusC Safe "th_affine_safe" affineI64)

-- Distinct C symbol name vs Haskell binding name + Bool <-> CBool marshalling
$(declareJanusCAs Unsafe "JANUS_UPPER_BOOL" "thUpperBool" boolLogic)

-- Effectful pointer array mutation (Unsafe) and reduction (Safe)
$(declareJanusC Unsafe "th_saxpy" saxpyJanusC)
$(declareJanusC Safe "th_dot_safe" dotJanusC)

-- Custom compiler flags passed through CBuildConfig
$( declareJanusCWithConfig
     (defaultTHBuildConfig & cBuildConfigCFlags %~ (<> ["-fno-math-errno"]))
     Unsafe
     "th_custom_cflags_sub"
     (\(a :: JanusC Double) b -> a - b)
 )

-- Explicit multi-ISA static compilation (SSE2 baseline + AVX2) in the same module
$( declareJanusCWithConfig
     (defaultTHBuildConfig & cBuildConfigX86Features ?~ x86Baseline)
     Unsafe
     "th_vfma8_sse2"
     x86Vfma8
 )

$( declareJanusCWithConfig
     (defaultTHBuildConfig & cBuildConfigX86Features ?~ x86AVX2)
     Unsafe
     "th_vfma8_avx2"
     x86Vfma8
 )

-- SIMDMask + SIMDVector marshalling
$(declareJanusC Unsafe "th_mask_blend4" x86MaskBlend4)

-- SLEEF vector math (sin) via Safe ccall
$(declareJanusC Safe "th_sleef_sin4" x86SleefSin4)

-- Expression splices via addTopDecls
splicedInlineMul :: Int64 -> Int64 -> Int64
splicedInlineMul =
  $(spliceJanusC Unsafe "th_spliced_mul" (\(x :: JanusC Int64) y -> x * y))

splicedX86Saxpy :: Int64 -> Float -> Ptr Float -> Ptr Float -> IO ()
splicedX86Saxpy =
  $(spliceJanusC Unsafe "th_spliced_x86_saxpy" x86Saxpy)

splicedSpmdPoly8 :: Float -> [Float] -> [Float] -> [Float]
splicedSpmdPoly8 =
  $(spliceJanusC Unsafe "th_spliced_spmd_poly8" spmdPoly8)

splicedSpmdEffectAdd4 :: [Double] -> IO [Double]
splicedSpmdEffectAdd4 =
  $(spliceJanusC Safe "th_spliced_spmd_add4" spmdEffectAdd4)

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

unit_th_ast_safety_and_fastpath :: IO ()
unit_th_ast_safety_and_fastpath = do
  -- 1. Direct scalar/pointer signature takes zero-wrapper fast path for both Unsafe and Safe
  unsafeDecs <-
    runQ $
      buildForeignAndWrapperDecls
        Unsafe
        "fast_affine"
        (mkName "fast_affine")
        (janusCTHSpec (Proxy @(JanusC Int64 -> JanusC Int64 -> JanusC Int64 -> JanusC Int64)))
  case unsafeDecs of
    [ForeignD (ImportF CCall Unsafe "fast_affine" _ _)] -> pure ()
    other -> assertFailure ("Expected single Unsafe ForeignD, got: " <> show other)

  safeDecs <-
    runQ $
      buildForeignAndWrapperDecls
        Safe
        "safe_saxpy"
        (mkName "safe_saxpy")
        (janusCTHSpec (Proxy @(LinuxX86_64C Int64 -> LinuxX86_64C Float -> LinuxX86_64C (Ptr Float) -> LinuxX86_64C (Ptr Float) -> LinuxX86_64CM ())))
  case safeDecs of
    [ForeignD (ImportF CCall Safe "safe_saxpy" _ _)] -> pure ()
    other -> assertFailure ("Expected single Safe ForeignD for x86 scalar/ptr kernel, got: " <> show other)

  -- 2. Bool and SIMD/SPMD signatures emit raw ForeignD + SigD + FunD + INLINE pragma
  boolDecs <-
    runQ $
      buildForeignAndWrapperDecls
        Unsafe
        "bool_fn"
        (mkName "bool_fn")
        (janusCTHSpec (Proxy @(JanusC Int64 -> JanusC Bool -> JanusC Bool)))
  case boolDecs of
    [ForeignD (ImportF CCall Unsafe "bool_fn" _ _), SigD {}, FunD {}, PragmaD (InlineP {})] -> pure ()
    other -> assertFailure ("Expected wrapped Bool declarations, got: " <> show other)

  simdDecs <-
    runQ $
      buildForeignAndWrapperDecls
        Safe
        "simd_fn"
        (mkName "simd_fn")
        (janusCTHSpec (Proxy @( Sx86Vec8F )))
  case simdDecs of
    [ForeignD (ImportF CCall Safe "simd_fn" _ _), SigD {}, FunD {}, PragmaD (InlineP {})] -> pure ()
    other -> assertFailure ("Expected wrapped SIMD declarations, got: " <> show other)

  -- 3. Identifier validation rejects malformed C / Haskell names cleanly
  assertBool "valid C name accepted" (isRight (checkCSymbolName "JANUS_UPPER_BOOL"))
  assertBool "valid Hs name accepted" (isRight (checkHsVarName "thUpperBool"))
  assertBool "empty C name rejected" (isLeft (checkCSymbolName ""))
  assertBool "digit-leading C name rejected" (isLeft (checkCSymbolName "123abc"))
  assertBool "hyphenated C name rejected" (isLeft (checkCSymbolName "bad-name"))
  assertBool "C keyword rejected" (isLeft (checkCSymbolName "return"))
  assertBool "uppercase Haskell var rejected" (isLeft (checkHsVarName "UpperVar"))
  assertBool "Haskell keyword rejected" (isLeft (checkHsVarName "foreign"))

type Sx86Vec8F =
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float)

unit_th_janus_c_execution :: IO ()
unit_th_janus_c_execution = do
  th_const_42 @?= 42
  fac 10 @?= 3628800
  th_double_fac_sum (5 :: Int32) (6 :: Int64) @?= (120 + 720)
  th_affine_unsafe 3 7 5 @?= 26
  th_affine_safe 3 7 5 @?= 26
  thUpperBool 5 True @?= True
  thUpperBool 5 False @?= False
  thUpperBool (-1) True @?= False
  th_custom_cflags_sub 10.5 3.25 @?= 7.25
  splicedInlineMul 6 7 @?= 42

  -- Cross-module static object linking without helper symbol collisions
  THOther.th_other_fac 8 @?= 40320
  THOther.splicedOtherCube 4 @?= 64

  -- Pointer mutation (Unsafe) & concurrent Safe ccall execution
  let xs = [1.0, 2.0, 3.0, 4.0 :: Float]
      ys = [10.0, 20.0, 30.0, 40.0 :: Float]
  outYs <- withArray xs $ \px ->
    withArray ys $ \py -> do
      th_saxpy 4 2.5 px py
      peekArray 4 py
  outYs @?= [12.5, 25.0, 37.5, 50.0]

  let da = [1.0 .. 64.0 :: Double]
      db = [2.0, 4.0 .. 128.0 :: Double]
      expectedDot = sum (zipWith (*) da db)
  (d1, d2) <- concurrently
    (withArray da $ \pa -> withArray db $ \pb -> th_dot_safe 64 pa pb)
    (withArray da $ \pa -> withArray db $ \pb -> th_dot_safe 64 pa pb)
  d1 @?= expectedDot
  d2 @?= expectedDot

unit_th_linux_x86_64_simd_sleef_and_spmd :: IO ()
unit_th_linux_x86_64_simd_sleef_and_spmd = do
  -- Direct scalar/pointer LinuxX86_64C splice
  let xs = [2.0, 4.0, 6.0, 8.0 :: Float]
      ys = [1.0, 1.0, 1.0, 1.0 :: Float]
  outYs <- withArray xs $ \px ->
    withArray ys $ \py -> do
      splicedX86Saxpy 4 3.0 px py
      peekArray 4 py
  outYs @?= [7.0, 13.0, 19.0, 25.0]

  -- Multi-ISA SIMD 8xFloat FMA (SSE2 always runs; AVX2 runs when host admits AVX2)
  let aL = [1, 2, 3, 4, 5, 6, 7, 8 :: Float]
      bL = [10, 20, 30, 40, 50, 60, 70, 80 :: Float]
      cL = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5 :: Float]
      expectedFma = zipWith3 (\a b c -> a * b + c) aL bL cL
      va = simdFromList aL
      vb = simdFromList bL
      vc = simdFromList cL
  simdToList (th_vfma8_sse2 va vb vc) @?= expectedFma
  hostFs <- observeHostX86Features
  when (admitsX86Features hostFs x86AVX2) $
    simdToList (th_vfma8_avx2 va vb vc) @?= expectedFma

  -- SIMDMask + SIMDVector blend
  let m4 = simdMaskFromBools [True, False, True, False]
      t4 = simdFromList [10, 20, 30, 40 :: Int32]
      f4 = simdFromList [100, 200, 300, 400 :: Int32]
  simdToList (th_mask_blend4 m4 t4 f4) @?= [10, 200, 30, 400]

  -- SLEEF vector sin via statically linked .o
  let angles = [0.0, 0.5, -0.5, 1.0 :: Float]
      gotSin = simdToList (th_sleef_sin4 (simdFromList angles))
      maxErr = maximum (zipWith (\g x -> abs (g - sin x)) gotSin angles)
  assertBool ("SLEEF sin max error <= 1e-5, got " <> show maxErr) (maxErr <= 1e-5)

  -- SPMD pure & IO splices
  splicedSpmdPoly8 2.0 aL bL @?= zipWith (\x y -> 2.0 * x + y) aL bL
  effOut <- splicedSpmdEffectAdd4 [1.5, 2.5, 3.5, 4.5]
  effOut @?= [4.0, 6.0, 8.0, 10.0]

unit_th_cached_object_and_named_dynamic_build :: IO ()
unit_th_cached_object_and_named_dynamic_build = do
  mtmp <- lookupEnv "TMPDIR"
  let baseDir = maybe "_cache" id mtmp
      cacheDir = baseDir </> "janus_th_cache_test"
      cfg1 = defaultCBuildConfig & cBuildConfigCacheDir .~ cacheDir
      cfg2 = cfg1 & cBuildConfigCFlags %~ (<> ["-fno-math-errno"])
      cSrc = renderJCProgram "cached_fn" (jcparam "cached_fn" 0 (pure []) JC affineI64)

  obj1 <- buildCachedJanusObject cfg1 [] cSrc
  obj1Again <- buildCachedJanusObject cfg1 [] cSrc
  obj2 <- buildCachedJanusObject cfg2 [] cSrc

  exists1 <- doesFileExist obj1
  exists2 <- doesFileExist obj2
  assertBool "cached object 1 exists" exists1
  assertBool "cached object 2 exists" exists2
  obj1 @?= obj1Again
  assertBool "distinct cflags produce distinct cached .o path" (obj1 /= obj2)

  -- Named dynamic compilation entrypoints
  dynJanus <- compileJanusCNamed "custom_dyn_affine" JC cfg1 affineI64
  dynJanus 4 5 6 @?= 26

  dynX86 <- compileLinuxX86_64CNamed "custom_dyn_vfma8" cfg1 x86Vfma8
  let v1 = simdFromList [1 .. 8 :: Float]
  simdToList (dynX86 v1 v1 v1) @?= [x * x + x | x <- [1 .. 8 :: Float]]

--------------------------------------------------------------------------------
-- Differential Hedgehog Properties
--------------------------------------------------------------------------------

thAffineMatchesReference :: Property
thAffineMatchesReference = property $ do
  let g = Gen.int64 (Range.linear (-1000000) 1000000)
  a <- forAll g
  x <- forAll g
  b <- forAll g
  let expected = a * x + b
  th_affine_unsafe a x b === expected
  th_affine_safe a x b === expected

thFacMatchesInterpreted :: Property
thFacMatchesInterpreted = property $ do
  n <- forAll $ Gen.int64 (Range.linear 0 20)
  let expected = runIdentity (facPoly (Identity n))
  fac n === expected

thSpmdPoly8MatchesLanes :: Property
thSpmdPoly8MatchesLanes = property $ do
  let gf = Gen.float (Range.linearFrac (-100.0) 100.0)
  s <- forAll gf
  xs <- forAll $ Gen.list (Range.singleton 8) gf
  ys <- forAll $ Gen.list (Range.singleton 8) gf
  let got = splicedSpmdPoly8 s xs ys
      expected = zipWith (\x y -> s * x + y) xs ys
      expectedMax = maximum (map abs expected)
      close a b = abs (a - b) <= 1e-3 * max 1.0 expectedMax
  assert (and (zipWith close got expected))

test_th_properties :: TestTree
test_th_properties =
  testGroup
    "TH static C compilation properties"
    [ testProperty "unsafe & safe affine match reference" thAffineMatchesReference,
      testProperty "recursive fac named 'fac' matches Identity" thFacMatchesInterpreted,
      testProperty "SPMD 8xFloat kernel matches lane spec" thSpmdPoly8MatchesLanes
    ]
