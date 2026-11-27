{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Block-scoped CUDA @__shared__@ array allocation ('CmdSharedMem'), with
-- native CUDA codegen ('JanusCUDAM') and faithful per-block CPU emulation
-- ('CUDAIOM' and DejaFu-verifiable 'CUDADejaFuM').
module Janus.Backend.C.CUDA.SharedMem
  ( CmdSharedMem (..),
  )
where

import qualified Control.Concurrent.Classy as CC
import Control.Lens ((%~), (&), (+~), (^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, lift)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.IORef (atomicModifyIORef')
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (sizeOf))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C.CUDA.Runtime (CUDAM (..), CUDADejaFuM (..), CUDAIOM (..), Thread (..))
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..), JanusCUDA (..), JanusCUDAM (..))
import Janus.Backend.C.Raw
  ( JanusCT (..),
    JanusCTyped,
    JCType (..),
    RVal (..),
    appendBlock,
    getFunction,
    getJanusCType,
    jcfBlock,
    jcfVarCounter,
    modifyFunction,
    signedIntConst,
  )
import Janus.Command.Memory (allocaAligned)
import Language.C.Quote
  ( ArraySize (..),
    BlockItem (..),
    Decl (..),
    DeclSpec (..),
    Exp (..),
    Id (..),
    Init (..),
    InitGroup (..),
    TypeQual (..),
  )
import Test.DejaFu.Conc (ConcIO)

class (Monad m) => CmdSharedMem (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) | m -> e, e -> m where
  -- | Allocate a statically-sized block-shared array of @n@ elements of type @a@
  -- (@__shared__ a s_k[n];@ in CUDA; per-block shared buffer in CPU/DejaFu emulation).
  withSharedArray ::
    forall (n :: Nat) a r.
    (KnownNat n, Typeable a, Storable a, JanusCTyped a) =>
    Proxy n ->
    (e (Ptr a) -> m r) ->
    m r

instance CmdSharedMem JanusCUDAM JanusCUDA where
  withSharedArray ::
    forall (n :: Nat) a r.
    (KnownNat n, Typeable a, Storable a, JanusCTyped a) =>
    Proxy n ->
    (JanusCUDA (Ptr a) -> JanusCUDAM r) ->
    JanusCUDAM r
  withSharedArray pn k = do
    let len = max 1 (fromIntegral (natVal pn) :: Int)
    arrExp <- JanusCUDAM $ lift $ do
      JCType spec dec <- getJanusCType (Proxy @a)
      fn <- getFunction
      let c = fn ^. jcfVarCounter
          vid = Id ("smem_" <> show c) noLoc
          sharedSpec = case spec of
            DeclSpec sto tqs ts loc -> DeclSpec sto (TCUDAshared noLoc : tqs) ts loc
            other -> other
          arrDec = Array [] (ArraySize False (signedIntConst len) noLoc) dec noLoc
          decl = BlockDecl $ InitGroup sharedSpec [] [Init vid arrDec Nothing Nothing [] noLoc] noLoc
      modifyFunction $ \f ->
        f
          & jcfBlock %~ flip appendBlock [decl]
          & jcfVarCounter +~ 1
      pure (JanusCUDA (JanusCT (pure (RVal (Var vid noLoc)))))
    k arrExp

cudaWithSharedArray ::
  forall baseM e (n :: Nat) a r.
  ( CC.MonadConc baseM,
    MonadIO baseM,
    Coercible (Identity (Ptr a)) (e (Ptr a)),
    KnownNat n,
    Storable a
  ) =>
  Proxy n ->
  (e (Ptr a) -> CUDAM baseM r) ->
  CUDAM baseM r
cudaWithSharedArray pn k = do
  th <- CUDAM ask
  slotId <- liftIO $ atomicModifyIORef' (_threadSharedCounter th) (\i -> (i + 1, i))
  let len = max 1 (fromIntegral (natVal pn) :: Int)
      nbytes = len * max 1 (sizeOf (undefined :: a))
  rawPtr <- CUDAM $ lift $ CC.modifyMVar (_threadSharedMem th) $ \mp ->
    case Map.lookup slotId mp of
      Just p -> pure (mp, p)
      Nothing -> do
        p <- liftIO (callocBytes nbytes)
        pure (Map.insert slotId p mp, p)
  k (coerce (Identity (castPtr rawPtr :: Ptr a)))

instance CmdSharedMem CUDAIOM CUDAIO where
  withSharedArray pn k = CUDAIOM (cudaWithSharedArray @IO @CUDAIO pn (getCUDAIOM . k))

instance CmdSharedMem CUDADejaFuM CUDADejaFu where
  withSharedArray pn k = CUDADejaFuM (cudaWithSharedArray @ConcIO @CUDADejaFu pn (getCUDADejaFuM . k))

instance CmdSharedMem IO Identity where
  withSharedArray pn k =
    let len = max 1 (fromIntegral (natVal pn) :: Int)
     in allocaAligned len (k . Identity)
