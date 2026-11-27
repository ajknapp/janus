{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Warp-level synchronous shuffle intrinsics ('CmdWarpShuffle':
-- @__shfl_down_sync@, @__shfl_up_sync@, @__shfl_xor_sync@, @__shfl_sync@),
-- with native CUDA codegen ('JanusCUDAM') and race-free CPU emulation on
-- 'CUDAIOM' and 'CUDADejaFuM'.
module Janus.Backend.C.CUDA.Warp
  ( CmdWarpShuffle (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, lift)
import Data.Bits (xor, (.&.))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (peekElemOff, pokeElemOff))
import Janus.Backend.C.CUDA.Runtime (CUDAM (..), CUDADejaFuM (..), CUDAIOM (..), Thread (..))
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..), CmdSync (..), JanusCUDA (..), JanusCUDAM (..))
import Janus.Backend.C.Raw
  ( JanusCT (..),
    JanusCTyped,
    RVal (..),
    emitLocalVal,
    getJanusCType,
  )
import Language.C.Quote (Exp (..), Id (..))
import qualified Control.Concurrent.Classy as CC
import Test.DejaFu.Conc (ConcIO)

class (Monad m) => CmdWarpShuffle (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) (a :: K.Type) where
  shflDownSync :: e Word32 -> e a -> e Int32 -> m (e a)
  shflUpSync :: e Word32 -> e a -> e Int32 -> m (e a)
  shflXorSync :: e Word32 -> e a -> e Int32 -> m (e a)
  shflSync :: e Word32 -> e a -> e Int32 -> m (e a)

emitCUDAShfl ::
  forall a.
  (JanusCTyped a) =>
  String ->
  JanusCUDA Word32 ->
  JanusCUDA a ->
  JanusCUDA Int32 ->
  JanusCUDAM (JanusCUDA a)
emitCUDAShfl fnName (JanusCUDA (JanusCT mm)) (JanusCUDA (JanusCT mv)) (JanusCUDA (JanusCT md)) =
  JanusCUDAM $ lift $ do
    RVal em <- mm
    RVal ev <- mv
    RVal ed <- md
    ty <- getJanusCType (Proxy @a)
    res <- emitLocalVal ty (FnCall (Var (Id fnName noLoc) noLoc) [em, ev, ed] noLoc)
    pure (JanusCUDA (JanusCT (pure (RVal res))))

instance CmdWarpShuffle JanusCUDAM JanusCUDA Int32 where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

instance CmdWarpShuffle JanusCUDAM JanusCUDA Word32 where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

instance CmdWarpShuffle JanusCUDAM JanusCUDA Int64 where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

instance CmdWarpShuffle JanusCUDAM JanusCUDA Word64 where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

instance CmdWarpShuffle JanusCUDAM JanusCUDA Float where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

instance CmdWarpShuffle JanusCUDAM JanusCUDA Double where
  shflDownSync = emitCUDAShfl "__shfl_down_sync"
  shflUpSync = emitCUDAShfl "__shfl_up_sync"
  shflXorSync = emitCUDAShfl "__shfl_xor_sync"
  shflSync = emitCUDAShfl "__shfl_sync"

-- | Faithful double-barrier warp exchange emulation for 'CUDAIOM' and 'CUDADejaFuM'.
cudaEmulShfl ::
  forall baseM e a.
  ( CC.MonadConc baseM,
    MonadIO baseM,
    Storable a,
    Coercible (Identity a) (e a),
    Coercible (Identity Int32) (e Int32)
  ) =>
  (Int -> Int -> Int -> Int) ->
  e a ->
  e Int32 ->
  CUDAM baseM (e a)
cudaEmulShfl srcLaneFn valE deltaE = do
  th <- CUDAM ask
  let lane = _threadLaneId th
      wSize = _threadWarpSize th
      buf = castPtr (_threadWarpBuf th) :: Ptr a
      Identity (val :: a) = coerce valE
      Identity (delta32 :: Int32) = coerce deltaE
      delta = fromIntegral delta32 :: Int
  liftIO (pokeElemOff buf lane val)
  syncWarp
  let src = srcLaneFn lane wSize delta
  res <-
    if src >= 0 && src < wSize
      then liftIO (peekElemOff buf src)
      else pure val
  syncWarp
  pure (coerce @(Identity a) @(e a) (Identity res))

shflDownLane, shflUpLane, shflXorLane, shflIdxLane :: Int -> Int -> Int -> Int
shflDownLane lane wSize delta =
  let s = lane + delta
   in if s < wSize then s else lane
shflUpLane lane _ delta =
  let s = lane - delta
   in if s >= 0 then s else lane
shflXorLane lane wSize mask =
  let s = lane `xor` mask
   in if s < wSize then s else lane
shflIdxLane _ wSize src =
  let s = src .&. 31
   in if s < wSize then s else 0

instance (Storable a) => CmdWarpShuffle CUDAIOM CUDAIO a where
  shflDownSync _ v d = CUDAIOM (cudaEmulShfl @IO @CUDAIO shflDownLane v d)
  shflUpSync _ v d = CUDAIOM (cudaEmulShfl @IO @CUDAIO shflUpLane v d)
  shflXorSync _ v d = CUDAIOM (cudaEmulShfl @IO @CUDAIO shflXorLane v d)
  shflSync _ v d = CUDAIOM (cudaEmulShfl @IO @CUDAIO shflIdxLane v d)

instance (Storable a) => CmdWarpShuffle CUDADejaFuM CUDADejaFu a where
  shflDownSync _ v d = CUDADejaFuM (cudaEmulShfl @ConcIO @CUDADejaFu shflDownLane v d)
  shflUpSync _ v d = CUDADejaFuM (cudaEmulShfl @ConcIO @CUDADejaFu shflUpLane v d)
  shflXorSync _ v d = CUDADejaFuM (cudaEmulShfl @ConcIO @CUDADejaFu shflXorLane v d)
  shflSync _ v d = CUDADejaFuM (cudaEmulShfl @ConcIO @CUDADejaFu shflIdxLane v d)
