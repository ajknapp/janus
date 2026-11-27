{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.Quote where

import Control.Lens ((&), (.~))
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Functor.Identity (Identity (..))
import Data.List (isInfixOf)
import Foreign.Marshal.Utils (with)
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Janus.Backend.C
  ( PortableC,
    prepareRawJanusCUnit,
    showRawJanusCWith,
    withRawJanusC,
  )
import Janus.Backend.C.Build
  ( buildCachedJanusObject,
    cBuildConfigCFlags,
    defaultCBuildConfig,
    defaultTHBuildConfig,
  )
import Janus.Backend.C.CUDA (showRawJanusCUDA)
import Janus.Backend.C.Quote (extractIncludes)
import Janus.Backend.C.TH (Safety (Unsafe), declareJanusC)
import Janus.Data.HKD (CStruct (..))
import System.Directory (doesFileExist)
import Test.Janus.Backend.C.QuoteDefs
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

-- Compile refactored Cond/While/Range/Case/Memory statically via Template Haskell
$(declareJanusC Unsafe "th_refactored_ctrl_mem" refactoredControlAndMemCSE)

test_quote :: TestTree
test_quote =
  testGroup
    "Backend.C.Quote"
    [ testCase "extractIncludes handles headers, comments, and errors" $ do
        extractIncludes "#include <stdint.h>\n// comment\n#include <string.h>\nint x = 0;\n"
          @?= Right (["stdint.h", "string.h"], "\n// comment\n\nint x = 0;\n")
        extractIncludes "  /* lead */\n#include <stddef.h> // ok\n"
          @?= Right (["stddef.h"], "  /* lead */\n\n")
        assertBool "rejects quoted #include" (isLeft (extractIncludes "#include \"local.h\"\nint x;\n"))
        assertBool "rejects malformed #include" (isLeft (extractIncludes "#include <unclosed\nint x;\n")),
      testProperty "jcStms hygiene & execution across repeated calls" $
        property $ do
          a <- forAll (Gen.int64 (Range.linear (-1000) 1000))
          b <- forAll (Gen.int64 (Range.linear (-1000) 1000))
          res <- liftIO $ withRawJanusC defaultCBuildConfig addScaledTwiceRaw (\f -> f a b)
          -- ((a * 2 + b) * 2 + b) = 4*a + 3*b
          res === 4 * a + 3 * b,
      testProperty "jcStms captureBlock_ & empty/include-only quotes" $
        property $ do
          x <- forAll (Gen.int64 (Range.linear (-10000) 10000))
          res <- liftIO $ withRawJanusC defaultCBuildConfig absWithCaptureRaw (\f -> f x)
          res === abs x,
      testProperty "jcFuncs multi-function mutual recursion, typedefs & static (separate-TU dynamic .so)" $
        property $ do
          n <- forAll (Gen.int64 (Range.linear (-25) 25))
          res <- liftIO $ withRawJanusC defaultCBuildConfig multiFuncParityRaw (\f -> f n)
          let mag = abs n
              evenFlag = if even mag then 1 else 0
          res === mag * mag * 10 + evenFlag,
      testCase "jcFuncs single-TU renderJCProgram compiles cleanly with -Wall -Werror and no bare return;" $ do
        (cSrc, cflags, _) <- prepareRawJanusCUnit @PortableC () "quote_multi_unit" multiFuncParityRaw
        assertBool "static helper preserved in single-TU" ("static int64_t janus_q_square" `isInfixOf` cSrc)
        assertBool "function pointer typedef rendered" ("janus_q_unary_fn" `isInfixOf` cSrc)
        assertBool "no spurious bare return; in non-void functions" (not ("\n    return;\n" `isInfixOf` cSrc))
        let strictCfg = defaultTHBuildConfig & cBuildConfigCFlags .~ ["-O2", "-fPIC", "-Wall", "-Werror"]
        objPath <- buildCachedJanusObject strictCfg cflags cSrc
        exists <- doesFileExist objPath
        assertBool "compiled single-TU object exists" exists,
      testProperty "jcFuncs inherits caller CStruct typedefs across separate TUs" $
        property $ do
          a <- forAll (Gen.int64 (Range.linear (-5000) 5000))
          b <- forAll (Gen.int64 (Range.linear (-5000) 5000))
          let pair = CStruct (QuotePair (Identity a) (Identity b))
          res <- liftIO $ withRawJanusC defaultCBuildConfig structHelperRaw (\f -> with pair f)
          res === a + b * 3,
      testCase "jcStms and jcFuncs CUDA codegen emits __shared__ and automatic __device__" $ do
        let cuSrc = showRawJanusCUDA "quote_cu_kernel" cudaQuoteKernel
        assertBool "contains __shared__ array" ("__shared__" `isInfixOf` cuSrc)
        assertBool "contains __device__ on step1" ("__device__" `isInfixOf` cuSrc && "janus_cu_step1" `isInfixOf` cuSrc)
        assertBool "contains __device__ on step2" ("__device__" `isInfixOf` cuSrc && "janus_cu_step2" `isInfixOf` cuSrc)
        assertBool "no spurious bare return; in CUDA device helpers" (not ("\n    return;\n" `isInfixOf` cuSrc)),
      testCase "showRawJanusC renders clean multi-function output without bare return;" $ do
        let rawSrc = showRawJanusCWith @PortableC () "quote_raw_show" multiFuncParityRaw
        assertBool "contains helper definition" ("janus_q_is_even" `isInfixOf` rawSrc)
        assertBool "no bare return; emitted" (not ("\n    return;\n" `isInfixOf` rawSrc)),
      testProperty "TH static compilation of refactored Cond/While/Range/Case/Memory" $
        property $ do
          n <- forAll (Gen.int64 (Range.linear 0 12))
          res <- liftIO $ th_refactored_ctrl_mem n
          let expectedRange =
                sum
                  [ case i of
                      0 -> 10
                      1 -> 20
                      _ | i < 4 -> i * 3
                      _ -> i
                    | i <- [0 .. n - 1]
                  ]
              expectedWhile = max 0 n
          res === expectedRange + expectedWhile
    ]
