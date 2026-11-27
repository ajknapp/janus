{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.QuoteDefs where

import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Backend.C
  ( JanusC,
    JanusCCSE,
    JanusCM,
    JanusCMCSE,
    MonadJanusC (..),
    RVal (..),
    pattern JanusC,
  )
import Janus.Backend.C.CUDA (JanusCUDA (..), JanusCUDAM, withSharedArray)
import Janus.Backend.C.Quote
  ( captureBlock_,
    freshCVar,
    getCType,
    jcFuncs,
    jcStms,
  )
import Janus.Command.Case (switchCase)
import Janus.Command.Cond (ifThenElseM)
import Janus.Command.Memory (CmdMem (..))
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (CmdRef (..), modifyRef)
import Janus.Command.While (whileM)
import Janus.Data.HKD (CStruct, JanusStruct)
import Janus.Expression.Ord (ExpOrd (..))

-- | HKD C struct used to verify that caller struct typedefs are inherited by
-- separate-TU helper functions emitted via 'jcFuncs'.
data QuotePair f = QuotePair
  { qpFirst :: !(f Int64),
    qpSecond :: !(f Int64)
  }
  deriving stock (Generic)

instance JanusStruct QuotePair

-- | Statement quasiquote invoked twice in the same function to verify fresh
-- variable hygiene, #include extraction, and RVal/C.Type antiquotation.
addScaledTwiceRaw :: JanusC Int64 -> JanusC Int64 -> JanusCM (JanusC Int64)
addScaledTwiceRaw a b = do
  r1 <- emitScaledAdd a b
  r2 <- emitScaledAdd r1 b
  pure r2
  where
    emitScaledAdd :: JanusC Int64 -> JanusC Int64 -> JanusCM (JanusC Int64)
    emitScaledAdd (JanusC mx) (JanusC my) = do
      vx <- mx
      vy <- my
      i64Ty <- getCType (Proxy @Int64)
      (resId, resExp) <- freshCVar "qres"
      [jcStms|
        #include <stdint.h>
        $ty:i64Ty $id:resId;
        {
          int64_t tmp = ($exp:vx * 2LL) + $exp:vy;
          $id:resId = tmp;
        }
      |]
      pure (JanusC (pure (RVal resExp)))

-- | Include-only and empty 'jcStms' quotes plus 'captureBlock_' splicing.
absWithCaptureRaw :: JanusC Int64 -> JanusCM (JanusC Int64)
absWithCaptureRaw (JanusC mx) = do
  vx <- mx
  r <- newRef (0 :: JanusC Int64)
  [jcStms|
    #include <stdint.h>
  |]
  [jcStms|   |]
  posItems <- captureBlock_ $ [jcStms| $exp:r = $exp:vx; |]
  negItems <- captureBlock_ $ [jcStms| $exp:r = -($exp:vx); |]
  [jcStms|
    if ($exp:vx >= 0) {
      $items:posItems
    } else {
      $items:negItems
    }
  |]
  readRef r

-- | Multi-function 'jcFuncs' emitting:
--   * a struct typedef
--   * a function-pointer typedef (non-DeclRoot -> JCTypedefFull)
--   * a static helper function (stripped for multi-TU, static in single-TU)
--   * two mutually recursive non-void functions (calling each other via sibling protos)
--   * an orchestrator calling the other helpers and applying the function pointer typedef
multiFuncParityRaw :: JanusC Int64 -> JanusCM (JanusC Int64)
multiFuncParityRaw (JanusC mn) = do
  vn <- mn
  emitParityHelpers
  -- Call a second time to verify idempotent re-emission of the same helper names
  emitParityHelpers
  (resId, resExp) <- freshCVar "par_res"
  [jcStms|
    #include <stdint.h>
    int64_t $id:resId = janus_q_combine($exp:vn);
  |]
  pure (JanusC (pure (RVal resExp)))
  where
    emitParityHelpers :: JanusCM ()
    emitParityHelpers =
      [jcFuncs|
        #include <stdint.h>

        typedef struct {
          int64_t even_flag;
          int64_t squared;
        } janus_q_pair_t;

        typedef int64_t (*janus_q_unary_fn)(int64_t);

        static int64_t janus_q_square(int64_t x) {
          return x * x;
        }

        int64_t janus_q_is_even(int64_t n) {
          if (n == 0) {
            return 1;
          }
          return janus_q_is_odd(n - 1);
        }

        int64_t janus_q_is_odd(int64_t n) {
          if (n == 0) {
            return 0;
          }
          return janus_q_is_even(n - 1);
        }

        int64_t janus_q_apply(janus_q_unary_fn f, int64_t x) {
          return f(x);
        }

        int64_t janus_q_combine(int64_t n) {
          int64_t mag = n < 0 ? -n : n;
          janus_q_pair_t p;
          p.even_flag = janus_q_is_even(mag);
          p.squared = janus_q_apply(janus_q_square, mag);
          return p.squared * 10 + p.even_flag;
        }
      |]

-- | Verify that a caller-registered 'CStruct' typedef (via 'getCType') is
-- inherited by a helper function emitted with 'jcFuncs' even when compiled as
-- separate translation units.
structHelperRaw :: JanusC (Ptr (CStruct QuotePair)) -> JanusCM (JanusC Int64)
structHelperRaw (JanusC mp) = do
  vp <- mp
  pairTy <- getCType (Proxy @(CStruct QuotePair))
  [jcFuncs|
    #include <stdint.h>
    int64_t janus_q_sum_pair(const $ty:pairTy *p) {
      return p->qpFirst + p->qpSecond * 3LL;
    }
  |]
  (resId, resExp) <- freshCVar "sum"
  [jcStms|
    #include <stdint.h>
    int64_t $id:resId = janus_q_sum_pair($exp:vp);
  |]
  pure (JanusC (pure (RVal resExp)))

-- | CUDA kernel using both 'jcStms' (via 'withSharedArray') and 'jcFuncs'
-- (automatic @__device__@ annotation on multi-function helpers).
cudaQuoteKernel :: JanusCUDA (Ptr Int64) -> JanusCUDAM ()
cudaQuoteKernel (JanusCUDA (JanusC mp)) = do
  vp <- liftJC mp
  [jcFuncs|
    #include <stdint.h>
    int64_t janus_cu_step1(int64_t x) {
      return x + 7LL;
    }
    int64_t janus_cu_step2(int64_t x) {
      return janus_cu_step1(x) * 2LL;
    }
  |]
  withSharedArray (Proxy @16) $ \(JanusCUDA (JanusC msm) :: JanusCUDA (Ptr Int64)) -> do
    vsm <- liftJC msm
    [jcStms|
      $exp:vsm[0] = janus_cu_step2($exp:vp[0]);
      $exp:vp[0] = $exp:vsm[0];
    |]

-- | CSE program exercising the refactored Cond/While/Range/Case/Memory instances
-- for static TH compilation via 'declareJanusC'.
refactoredControlAndMemCSE :: JanusCCSE Int64 -> JanusCMCSE (JanusCCSE Int64)
refactoredControlAndMemCSE n = do
  acc <- newRef (0 :: JanusCCSE Int64)
  rangeM 0 n $ \i -> do
    v <- switchCase i [(0, pure 10), (1, pure 20)] (ifThenElseM (i `lt` 4) (pure (i * 3)) (pure i))
    modifyRef acc (+ v)
  cnt <- newRef (0 :: JanusCCSE Int64)
  whileM (0 `lt` n) $ \keep -> do
    c <- readRef cnt
    modifyRef acc (+ 1)
    let c' = c + 1
    writeRef cnt c'
    writeRef keep (c' `lt` n)
  -- Exercise refactored malloc/realloc/free
  p0 <- malloc 16
  p1 <- realloc p0 64
  free p1
  readRef acc
