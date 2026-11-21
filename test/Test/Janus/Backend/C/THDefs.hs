{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.THDefs where

import Control.Lens ((&), (.~))
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import Janus.Backend.C (JanusC, JanusCM)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64C, LinuxX86_64CM)
import qualified Janus.Command.Array as A
import Janus.Command.Range (rangeM)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Expression.Bool (ExpBool (land))
import Janus.Expression.Cast (ExpIntegralCast (toIntegral))
import Janus.Expression.Cond (ExpCond (ifThenElse))
import Janus.Expression.Ord (ExpOrd (gt, le))
import Janus.Expression.SIMD
  ( SIMDMask,
    SIMDVector,
    vfma,
    vselect,
  )
import Janus.Fix (ExpFix (fun))
import Janus.SPMD (SPMD, SPMDM, spmdSplat)
import Janus.Typed (JanusTyped)
import System.Environment (lookupEnv)

getTHTestCacheConfig :: IO CBuildConfig
getTHTestCacheConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

constFortyTwo :: JanusC Int64
constFortyTwo = 42

affineI64 :: JanusC Int64 -> JanusC Int64 -> JanusC Int64 -> JanusC Int64
affineI64 a x b = a * x + b

boolLogic :: JanusC Int64 -> JanusC Bool -> JanusC Bool
boolLogic x flag = (x `gt` 0) `land` flag

saxpyJanusC ::
  JanusC Int64 ->
  JanusC Float ->
  JanusC (Ptr Float) ->
  JanusC (Ptr Float) ->
  JanusCM ()
saxpyJanusC n alpha xs ys =
  rangeM 0 n $ \i -> do
    xi <- A.peekElemOff xs i
    yi <- A.peekElemOff ys i
    A.pokeElemOff ys (alpha * xi + yi) i

dotJanusC ::
  JanusC Int64 ->
  JanusC (Ptr Double) ->
  JanusC (Ptr Double) ->
  JanusCM (JanusC Double)
dotJanusC n xs ys = do
  acc <- newRef 0
  rangeM 0 n $ \i -> do
    xi <- A.peekElemOff xs i
    yi <- A.peekElemOff ys i
    cur <- readRef acc
    writeRef acc (cur + xi * yi)
  readRef acc

facPoly ::
  forall e a.
  ( Num (e a),
    ExpCond e,
    ExpOrd e a,
    ExpFix e (e a -> e a),
    JanusTyped e a
  ) =>
  e a ->
  e a
facPoly = fun (Proxy @e) "fac" $ \self n -> ifThenElse (n `le` 0) 1 (n * self (n - 1))

doubleFacSum :: JanusC Int32 -> JanusC Int64 -> JanusC Int64
doubleFacSum a b =
  toIntegral (facPoly @JanusC @Int32 a) + facPoly @JanusC @Int64 b

x86Saxpy ::
  LinuxX86_64C Int64 ->
  LinuxX86_64C Float ->
  LinuxX86_64C (Ptr Float) ->
  LinuxX86_64C (Ptr Float) ->
  LinuxX86_64CM ()
x86Saxpy n alpha xs ys =
  rangeM 0 n $ \i -> do
    xi <- A.peekElemOff xs i
    yi <- A.peekElemOff ys i
    A.pokeElemOff ys (alpha * xi + yi) i

x86Vfma8 ::
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 8 Float)
x86Vfma8 = vfma

x86MaskBlend4 ::
  LinuxX86_64C (SIMDMask LinuxX86_64C 4 Int32) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 4 Int32) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 4 Int32) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 4 Int32)
x86MaskBlend4 = vselect

x86SleefSin4 ::
  LinuxX86_64C (SIMDVector LinuxX86_64C 4 Float) ->
  LinuxX86_64C (SIMDVector LinuxX86_64C 4 Float)
x86SleefSin4 = sin

spmdPoly8 ::
  LinuxX86_64C Float ->
  SPMD 8 LinuxX86_64C Float ->
  SPMD 8 LinuxX86_64C Float ->
  SPMD 8 LinuxX86_64C Float
spmdPoly8 scale xs ys = spmdSplat scale * xs + ys

spmdEffectAdd4 ::
  SPMD 4 LinuxX86_64C Double ->
  SPMDM 4 LinuxX86_64CM LinuxX86_64C (SPMD 4 LinuxX86_64C Double)
spmdEffectAdd4 xs = do
  doubled <- letM (xs * 2)
  pure (doubled + 1)
