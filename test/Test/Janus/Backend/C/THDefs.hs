{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.THDefs where

import Control.Lens ((&), (.~))
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import Janus.Backend.C (JanusCCSE, JanusCMCSE)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64CCSE, LinuxX86_64CMCSE)
import qualified Janus.Command.Memory as M
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

constFortyTwo :: JanusCCSE Int64
constFortyTwo = 42

affineI64 :: JanusCCSE Int64 -> JanusCCSE Int64 -> JanusCCSE Int64 -> JanusCCSE Int64
affineI64 a x b = a * x + b

boolLogic :: JanusCCSE Int64 -> JanusCCSE Bool -> JanusCCSE Bool
boolLogic x flag = (x `gt` 0) `land` flag

saxpyJanusC ::
  JanusCCSE Int64 ->
  JanusCCSE Float ->
  JanusCCSE (Ptr Float) ->
  JanusCCSE (Ptr Float) ->
  JanusCMCSE ()
saxpyJanusC n alpha xs ys =
  rangeM 0 n $ \i -> do
    xi <- M.peekElemOff xs i
    yi <- M.peekElemOff ys i
    M.pokeElemOff ys (alpha * xi + yi) i

dotJanusC ::
  JanusCCSE Int64 ->
  JanusCCSE (Ptr Double) ->
  JanusCCSE (Ptr Double) ->
  JanusCMCSE (JanusCCSE Double)
dotJanusC n xs ys = do
  acc <- newRef 0
  rangeM 0 n $ \i -> do
    xi <- M.peekElemOff xs i
    yi <- M.peekElemOff ys i
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

doubleFacSum :: JanusCCSE Int32 -> JanusCCSE Int64 -> JanusCCSE Int64
doubleFacSum a b =
  toIntegral (facPoly @JanusCCSE @Int32 a) + facPoly @JanusCCSE @Int64 b

x86Saxpy ::
  LinuxX86_64CCSE Int64 ->
  LinuxX86_64CCSE Float ->
  LinuxX86_64CCSE (Ptr Float) ->
  LinuxX86_64CCSE (Ptr Float) ->
  LinuxX86_64CMCSE ()
x86Saxpy n alpha xs ys =
  rangeM 0 n $ \i -> do
    xi <- M.peekElemOff xs i
    yi <- M.peekElemOff ys i
    M.pokeElemOff ys (alpha * xi + yi) i

x86Vfma8 ::
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 8 Float) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 8 Float) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 8 Float) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 8 Float)
x86Vfma8 = vfma

x86MaskBlend4 ::
  LinuxX86_64CCSE (SIMDMask LinuxX86_64CCSE 4 Int32) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 4 Int32) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 4 Int32) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 4 Int32)
x86MaskBlend4 = vselect

x86SleefSin4 ::
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 4 Float) ->
  LinuxX86_64CCSE (SIMDVector LinuxX86_64CCSE 4 Float)
x86SleefSin4 = sin

spmdPoly8 ::
  LinuxX86_64CCSE Float ->
  SPMD 8 LinuxX86_64CCSE Float ->
  SPMD 8 LinuxX86_64CCSE Float ->
  SPMD 8 LinuxX86_64CCSE Float
spmdPoly8 scale xs ys = spmdSplat scale * xs + ys

spmdEffectAdd4 ::
  SPMD 4 LinuxX86_64CCSE Double ->
  SPMDM 4 LinuxX86_64CMCSE LinuxX86_64CCSE (SPMD 4 LinuxX86_64CCSE Double)
spmdEffectAdd4 xs = do
  doubled <- letM (xs * 2)
  pure (doubled + 1)
