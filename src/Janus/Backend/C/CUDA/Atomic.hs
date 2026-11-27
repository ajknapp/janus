{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Atomic read-modify-write primitives ('CmdAtomic', 'CmdAtomicAdd',
-- 'CmdAtomicMinMax', 'CmdAtomicBits') across CUDA ('JanusCUDAM'), CUDA CPU
-- emulation ('CUDAIOM', DejaFu-verifiable 'CUDADejaFuM'), multi-threaded CPU C
-- ('JanusCMT' via C11/GCC @__atomic_*@), and host 'IO'.
module Janus.Backend.C.CUDA.Atomic
  ( CmdAtomic (..),
    CmdAtomicAdd (..),
    CmdAtomicMinMax (..),
    CmdAtomicBits (..),
  )
where

import qualified Control.Concurrent.Classy as CC
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Lens ((%~), (&), (+~), (^.))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, lift)
import Data.Bits (Bits (..))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32, Int64)
import qualified Data.Kind as K
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek, poke))
import Janus.Backend.C.CUDA.Runtime (CUDAM (..), CUDADejaFuM (..), CUDAIOM (..), Thread (..))
import Janus.Backend.C.CUDA.Types
  ( CUDADejaFu (..),
    CUDAIO (..),
    JanusCUDA (..),
    JanusCUDAM (..),
    askCUDAFeatures,
  )
import Janus.Backend.C.Raw
  ( JanusCM,
    JanusCMT (..),
    JanusCT (..),
    JanusCTyped,
    JCType (..),
    RVal (..),
    addHeader,
    appendBlock,
    emitLocalVal,
    getFunction,
    getJanusCType,
    jcfBlock,
    jcfVarCounter,
    liftJanusCM,
    modifyFunction,
    signedIntConst,
  )
import Janus.Backend.C.Target (cudaSM)
import Language.C.Quote
  ( AssignOp (..),
    BinOp (..),
    BlockItem (..),
    Decl (..),
    DeclSpec (..),
    Exp (..),
    Id (..),
    Init (..),
    InitGroup (..),
    Stm (..),
    Type (..),
    TypeSpec (..),
    UnOp (..),
  )
import System.IO.Unsafe (unsafePerformIO)
import Test.DejaFu.Conc (ConcIO)

--------------------------------------------------------------------------------
-- Global Host IO Atomic Lock
--------------------------------------------------------------------------------

{-# NOINLINE hostAtomicLock #-}
hostAtomicLock :: MVar ()
hostAtomicLock = unsafePerformIO (newMVar ())

--------------------------------------------------------------------------------
-- Typeclasses
--------------------------------------------------------------------------------

-- | Word-sized compare-and-swap primitives, sufficient to implement lock-free
-- atomic updates for ANY open-world 32-bit or 64-bit commutative monoid.
class (Monad m) => CmdAtomic (m :: K.Type -> K.Type) (e :: K.Type -> K.Type) | m -> e, e -> m where
  atomicCAS32Ptr :: e (Ptr Word32) -> e Word32 -> e Word32 -> m (e Word32)
  atomicCAS64Ptr :: e (Ptr Word64) -> e Word64 -> e Word64 -> m (e Word64)

class (CmdAtomic m e) => CmdAtomicAdd m e a where
  atomicAddPtr :: e (Ptr a) -> e a -> m (e a)

class (CmdAtomic m e) => CmdAtomicMinMax m e a where
  atomicMinPtr :: e (Ptr a) -> e a -> m (e a)
  atomicMaxPtr :: e (Ptr a) -> e a -> m (e a)

class (CmdAtomic m e) => CmdAtomicBits m e a where
  atomicAndPtr :: e (Ptr a) -> e a -> m (e a)
  atomicOrPtr :: e (Ptr a) -> e a -> m (e a)
  atomicXorPtr :: e (Ptr a) -> e a -> m (e a)

--------------------------------------------------------------------------------
-- Shared C Codegen Helpers
--------------------------------------------------------------------------------

cNamedTy :: String -> Type
cNamedTy s = Type (DeclSpec [] [] (Tnamed (Id s noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc

cPtrTy :: String -> Type
cPtrTy s = Type (DeclSpec [] [] (Tnamed (Id s noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc) noLoc

cCall :: String -> [Exp] -> Exp
cCall f args = FnCall (Var (Id f noLoc) noLoc) args noLoc

emitUninitLocal :: JCType -> JanusCM Exp
emitUninitLocal (JCType spec dec) = do
  fn <- getFunction
  let c = fn ^. jcfVarCounter
      vid = Id ("at_" <> show c) noLoc
      decl = BlockDecl $ InitGroup spec [] [Init vid dec Nothing Nothing [] noLoc] noLoc
  modifyFunction $ \f ->
    f
      & jcfBlock %~ flip appendBlock [decl]
      & jcfVarCounter +~ 1
  pure (Var vid noLoc)

appendCStm :: Stm -> JanusCM ()
appendCStm stm = modifyFunction $ \f -> f & jcfBlock %~ flip appendBlock [BlockStm stm]

-- | Emit a GCC/Clang C11 @__atomic_compare_exchange_n@ returning the observed
-- old value (matching CUDA @atomicCAS@ semantics!).
emitHostCAS ::
  forall t a.
  (JanusCTyped a) =>
  JanusCT t (Ptr a) ->
  JanusCT t a ->
  JanusCT t a ->
  JanusCMT t (JanusCT t a)
emitHostCAS (JanusCT mp) (JanusCT mcmp) (JanusCT mval) = liftJanusCM $ do
  RVal ep <- mp
  RVal ecmp <- mcmp
  RVal eval <- mval
  ty <- getJanusCType (Proxy @a)
  oldVar <- emitLocalVal ty ecmp
  let call =
        cCall
          "__atomic_compare_exchange_n"
          [ ep,
            UnOp AddrOf oldVar noLoc,
            eval,
            signedIntConst (0 :: Int),
            Var (Id "__ATOMIC_ACQ_REL" noLoc) noLoc,
            Var (Id "__ATOMIC_ACQUIRE" noLoc) noLoc
          ]
  appendCStm (Exp (Just call) noLoc)
  pure (JanusCT (pure (RVal oldVar)))

emitHostFetchOp ::
  forall t a.
  (JanusCTyped a) =>
  String ->
  JanusCT t (Ptr a) ->
  JanusCT t a ->
  JanusCMT t (JanusCT t a)
emitHostFetchOp fnName (JanusCT mp) (JanusCT mv) = liftJanusCM $ do
  RVal ep <- mp
  RVal ev <- mv
  ty <- getJanusCType (Proxy @a)
  res <- emitLocalVal ty (cCall fnName [ep, ev, Var (Id "__ATOMIC_ACQ_REL" noLoc) noLoc])
  pure (JanusCT (pure (RVal res)))

-- | Generic host CAS loop for floating-point add/min/max and integer min/max.
emitHostCASLoop ::
  forall t a.
  (JanusCTyped a) =>
  String ->
  Int ->
  (Exp -> Exp -> Exp) ->
  JanusCT t (Ptr a) ->
  JanusCT t a ->
  JanusCMT t (JanusCT t a)
emitHostCASLoop uName nbytes combine (JanusCT mp) (JanusCT mv) = liftJanusCM $ do
  addHeader "stdint.h"
  addHeader "string.h"
  RVal ep <- mp
  RVal ev <- mv
  tyA <- getJanusCType (Proxy @a)
  let tyU = JCType (DeclSpec [] [] (Tnamed (Id uName noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
  ptrU <- emitLocalVal (JCType (DeclSpec [] [] (Tnamed (Id uName noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)) (Cast (cPtrTy uName) ep noLoc)
  valA <- emitLocalVal tyA ev
  oldU <- emitLocalVal tyU (cCall "__atomic_load_n" [ptrU, Var (Id "__ATOMIC_RELAXED" noLoc) noLoc])
  oldA <- emitUninitLocal tyA
  newA <- emitUninitLocal tyA
  newU <- emitUninitLocal tyU
  let szE = signedIntConst nbytes
      body =
        [ BlockStm $ Exp (Just $ cCall "memcpy" [UnOp AddrOf oldA noLoc, UnOp AddrOf oldU noLoc, szE]) noLoc,
          BlockStm $ Exp (Just $ Assign newA JustAssign (combine oldA valA) noLoc) noLoc,
          BlockStm $ Exp (Just $ cCall "memcpy" [UnOp AddrOf newU noLoc, UnOp AddrOf newA noLoc, szE]) noLoc
        ]
      casCond =
        UnOp
          Lnot
          ( cCall
              "__atomic_compare_exchange_n"
              [ ptrU,
                UnOp AddrOf oldU noLoc,
                newU,
                signedIntConst (1 :: Int),
                Var (Id "__ATOMIC_ACQ_REL" noLoc) noLoc,
                Var (Id "__ATOMIC_RELAXED" noLoc) noLoc
              ]
          )
          noLoc
  appendCStm (DoWhile (Block body noLoc) casCond noLoc)
  pure (JanusCT (pure (RVal oldA)))

--------------------------------------------------------------------------------
-- JanusCMT t (Multi-Threaded CPU C) Instances
--------------------------------------------------------------------------------

instance CmdAtomic (JanusCMT t) (JanusCT t) where
  atomicCAS32Ptr = emitHostCAS
  atomicCAS64Ptr = emitHostCAS

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Int32 where
  atomicAddPtr = emitHostFetchOp "__atomic_fetch_add"

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Word32 where
  atomicAddPtr = emitHostFetchOp "__atomic_fetch_add"

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Int64 where
  atomicAddPtr = emitHostFetchOp "__atomic_fetch_add"

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Word64 where
  atomicAddPtr = emitHostFetchOp "__atomic_fetch_add"

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Float where
  atomicAddPtr = emitHostCASLoop "uint32_t" 4 (\a b -> BinOp Add a b noLoc)

instance CmdAtomicAdd (JanusCMT t) (JanusCT t) Double where
  atomicAddPtr = emitHostCASLoop "uint64_t" 8 (\a b -> BinOp Add a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Int32 where
  atomicMinPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Word32 where
  atomicMinPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Int64 where
  atomicMinPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Word64 where
  atomicMinPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Float where
  atomicMinPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint32_t" 4 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax (JanusCMT t) (JanusCT t) Double where
  atomicMinPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitHostCASLoop "uint64_t" 8 (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicBits (JanusCMT t) (JanusCT t) Word32 where
  atomicAndPtr = emitHostFetchOp "__atomic_fetch_and"
  atomicOrPtr = emitHostFetchOp "__atomic_fetch_or"
  atomicXorPtr = emitHostFetchOp "__atomic_fetch_xor"

instance CmdAtomicBits (JanusCMT t) (JanusCT t) Word64 where
  atomicAndPtr = emitHostFetchOp "__atomic_fetch_and"
  atomicOrPtr = emitHostFetchOp "__atomic_fetch_or"
  atomicXorPtr = emitHostFetchOp "__atomic_fetch_xor"

instance CmdAtomicBits (JanusCMT t) (JanusCT t) Int32 where
  atomicAndPtr = emitHostFetchOp "__atomic_fetch_and"
  atomicOrPtr = emitHostFetchOp "__atomic_fetch_or"
  atomicXorPtr = emitHostFetchOp "__atomic_fetch_xor"

instance CmdAtomicBits (JanusCMT t) (JanusCT t) Int64 where
  atomicAndPtr = emitHostFetchOp "__atomic_fetch_and"
  atomicOrPtr = emitHostFetchOp "__atomic_fetch_or"
  atomicXorPtr = emitHostFetchOp "__atomic_fetch_xor"

--------------------------------------------------------------------------------
-- JanusCUDAM (CUDA Device Atomics) Instances
--------------------------------------------------------------------------------

emitCUDAAtomic2 ::
  forall a.
  (JanusCTyped a) =>
  String ->
  String ->
  JanusCUDA (Ptr a) ->
  JanusCUDA a ->
  JanusCUDAM (JanusCUDA a)
emitCUDAAtomic2 fnName cCastTy (JanusCUDA (JanusCT mp)) (JanusCUDA (JanusCT mv)) =
  JanusCUDAM $ lift $ do
    RVal ep <- mp
    RVal ev <- mv
    ty <- getJanusCType (Proxy @a)
    let call = cCall fnName [Cast (cPtrTy cCastTy) ep noLoc, Cast (cNamedTy cCastTy) ev noLoc]
    res <- emitLocalVal ty (Cast (let JCType s d = ty in Type s d noLoc) call noLoc)
    pure (JanusCUDA (JanusCT (pure (RVal res))))

-- | Generic CUDA CAS loop using @atomicCAS@ on 32-bit @unsigned int@ or 64-bit
-- @unsigned long long int@ (supports SM50 Double add, float/double min/max, etc.).
emitCUDACASLoop ::
  forall a.
  (JanusCTyped a) =>
  String ->
  String ->
  String ->
  (Exp -> Exp -> Exp) ->
  JanusCUDA (Ptr a) ->
  JanusCUDA a ->
  JanusCUDAM (JanusCUDA a)
emitCUDACASLoop uTy toA toU combine (JanusCUDA (JanusCT mp)) (JanusCUDA (JanusCT mv)) =
  JanusCUDAM $ lift $ do
    RVal ep <- mp
    RVal ev <- mv
    tyA <- getJanusCType (Proxy @a)
    let tyU = JCType (DeclSpec [] [] (Tnamed (Id uTy noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
        tyUPtr = JCType (DeclSpec [] [] (Tnamed (Id uTy noLoc) [] noLoc) noLoc) (Ptr [] (DeclRoot noLoc) noLoc)
    addrU <- emitLocalVal tyUPtr (Cast (cPtrTy uTy) ep noLoc)
    valA <- emitLocalVal tyA ev
    oldU <- emitLocalVal tyU (UnOp Deref addrU noLoc)
    assumedU <- emitUninitLocal tyU
    let newExpr = combine (cCall toA [assumedU]) valA
        newU = cCall toU [newExpr]
        body =
          [ BlockStm $ Exp (Just $ Assign assumedU JustAssign oldU noLoc) noLoc,
            BlockStm $ Exp (Just $ Assign oldU JustAssign (cCall "atomicCAS" [addrU, assumedU, newU]) noLoc) noLoc
          ]
    appendCStm (DoWhile (Block body noLoc) (BinOp Ne assumedU oldU noLoc) noLoc)
    res <- emitLocalVal tyA (cCall toA [oldU])
    pure (JanusCUDA (JanusCT (pure (RVal res))))

instance CmdAtomic JanusCUDAM JanusCUDA where
  atomicCAS32Ptr (JanusCUDA (JanusCT mp)) (JanusCUDA (JanusCT mcmp)) (JanusCUDA (JanusCT mval)) =
    JanusCUDAM $ lift $ do
      RVal ep <- mp
      RVal ecmp <- mcmp
      RVal eval <- mval
      ty <- getJanusCType (Proxy @Word32)
      let call =
            cCall
              "atomicCAS"
              [ Cast (cPtrTy "unsigned int") ep noLoc,
                Cast (cNamedTy "unsigned int") ecmp noLoc,
                Cast (cNamedTy "unsigned int") eval noLoc
              ]
      res <- emitLocalVal ty (Cast (cNamedTy "uint32_t") call noLoc)
      pure (JanusCUDA (JanusCT (pure (RVal res))))
  atomicCAS64Ptr (JanusCUDA (JanusCT mp)) (JanusCUDA (JanusCT mcmp)) (JanusCUDA (JanusCT mval)) =
    JanusCUDAM $ lift $ do
      RVal ep <- mp
      RVal ecmp <- mcmp
      RVal eval <- mval
      ty <- getJanusCType (Proxy @Word64)
      let call =
            cCall
              "atomicCAS"
              [ Cast (cPtrTy "unsigned long long int") ep noLoc,
                Cast (cNamedTy "unsigned long long int") ecmp noLoc,
                Cast (cNamedTy "unsigned long long int") eval noLoc
              ]
      res <- emitLocalVal ty (Cast (cNamedTy "uint64_t") call noLoc)
      pure (JanusCUDA (JanusCT (pure (RVal res))))

instance CmdAtomicAdd JanusCUDAM JanusCUDA Int32 where
  atomicAddPtr = emitCUDAAtomic2 "atomicAdd" "int"

instance CmdAtomicAdd JanusCUDAM JanusCUDA Word32 where
  atomicAddPtr = emitCUDAAtomic2 "atomicAdd" "unsigned int"

instance CmdAtomicAdd JanusCUDAM JanusCUDA Int64 where
  atomicAddPtr = emitCUDAAtomic2 "atomicAdd" "unsigned long long int"

instance CmdAtomicAdd JanusCUDAM JanusCUDA Word64 where
  atomicAddPtr = emitCUDAAtomic2 "atomicAdd" "unsigned long long int"

instance CmdAtomicAdd JanusCUDAM JanusCUDA Float where
  atomicAddPtr = emitCUDAAtomic2 "atomicAdd" "float"

instance CmdAtomicAdd JanusCUDAM JanusCUDA Double where
  atomicAddPtr p v = do
    fs <- askCUDAFeatures
    if fs >= cudaSM 6 0
      then emitCUDAAtomic2 "atomicAdd" "double" p v
      else emitCUDACASLoop "unsigned long long int" "__longlong_as_double" "__double_as_longlong" (\a b -> BinOp Add a b noLoc) p v

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Int32 where
  atomicMinPtr = emitCUDAAtomic2 "atomicMin" "int"
  atomicMaxPtr = emitCUDAAtomic2 "atomicMax" "int"

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Word32 where
  atomicMinPtr = emitCUDAAtomic2 "atomicMin" "unsigned int"
  atomicMaxPtr = emitCUDAAtomic2 "atomicMax" "unsigned int"

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Int64 where
  atomicMinPtr = emitCUDAAtomic2 "atomicMin" "long long int"
  atomicMaxPtr = emitCUDAAtomic2 "atomicMax" "long long int"

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Word64 where
  atomicMinPtr = emitCUDAAtomic2 "atomicMin" "unsigned long long int"
  atomicMaxPtr = emitCUDAAtomic2 "atomicMax" "unsigned long long int"

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Float where
  atomicMinPtr = emitCUDACASLoop "unsigned int" "__uint_as_float" "__float_as_uint" (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitCUDACASLoop "unsigned int" "__uint_as_float" "__float_as_uint" (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicMinMax JanusCUDAM JanusCUDA Double where
  atomicMinPtr = emitCUDACASLoop "unsigned long long int" "__longlong_as_double" "__double_as_longlong" (\a b -> Cond (BinOp Lt a b noLoc) a b noLoc)
  atomicMaxPtr = emitCUDACASLoop "unsigned long long int" "__longlong_as_double" "__double_as_longlong" (\a b -> Cond (BinOp Gt a b noLoc) a b noLoc)

instance CmdAtomicBits JanusCUDAM JanusCUDA Word32 where
  atomicAndPtr = emitCUDAAtomic2 "atomicAnd" "unsigned int"
  atomicOrPtr = emitCUDAAtomic2 "atomicOr" "unsigned int"
  atomicXorPtr = emitCUDAAtomic2 "atomicXor" "unsigned int"

instance CmdAtomicBits JanusCUDAM JanusCUDA Word64 where
  atomicAndPtr = emitCUDAAtomic2 "atomicAnd" "unsigned long long int"
  atomicOrPtr = emitCUDAAtomic2 "atomicOr" "unsigned long long int"
  atomicXorPtr = emitCUDAAtomic2 "atomicXor" "unsigned long long int"

instance CmdAtomicBits JanusCUDAM JanusCUDA Int32 where
  atomicAndPtr = emitCUDAAtomic2 "atomicAnd" "int"
  atomicOrPtr = emitCUDAAtomic2 "atomicOr" "int"
  atomicXorPtr = emitCUDAAtomic2 "atomicXor" "int"

instance CmdAtomicBits JanusCUDAM JanusCUDA Int64 where
  atomicAndPtr = emitCUDAAtomic2 "atomicAnd" "unsigned long long int"
  atomicOrPtr = emitCUDAAtomic2 "atomicOr" "unsigned long long int"
  atomicXorPtr = emitCUDAAtomic2 "atomicXor" "unsigned long long int"

--------------------------------------------------------------------------------
-- CPU Emulation (CUDAIOM, CUDADejaFuM, & Host IO)
--------------------------------------------------------------------------------

cudaEmulCAS ::
  forall baseM e a.
  ( CC.MonadConc baseM,
    MonadIO baseM,
    Eq a,
    Storable a,
    Coercible (Identity (Ptr a)) (e (Ptr a)),
    Coercible (Identity a) (e a)
  ) =>
  e (Ptr a) ->
  e a ->
  e a ->
  CUDAM baseM (e a)
cudaEmulCAS pE cmpE valE = do
  th <- CUDAM ask
  let Identity (p :: Ptr a) = coerce pE
      Identity (cmp :: a) = coerce cmpE
      Identity (val :: a) = coerce valE
  old <- CUDAM $ lift $ CC.withMVar (_threadAtomicLock th) $ \_ -> liftIO $ do
    cur <- peek p
    if cur == cmp
      then poke p val >> pure cur
      else pure cur
  pure (coerce @(Identity a) @(e a) (Identity old))

cudaEmulRMW ::
  forall baseM e a.
  ( CC.MonadConc baseM,
    MonadIO baseM,
    Storable a,
    Coercible (Identity (Ptr a)) (e (Ptr a)),
    Coercible (Identity a) (e a)
  ) =>
  (a -> a -> a) ->
  e (Ptr a) ->
  e a ->
  CUDAM baseM (e a)
cudaEmulRMW f pE valE = do
  th <- CUDAM ask
  let Identity (p :: Ptr a) = coerce pE
      Identity (val :: a) = coerce valE
  old <- CUDAM $ lift $ CC.withMVar (_threadAtomicLock th) $ \_ -> liftIO $ do
    cur <- peek p
    poke p (f cur val)
    pure cur
  pure (coerce @(Identity a) @(e a) (Identity old))

instance CmdAtomic CUDAIOM CUDAIO where
  atomicCAS32Ptr p c v = CUDAIOM (cudaEmulCAS @IO @CUDAIO p c v)
  atomicCAS64Ptr p c v = CUDAIOM (cudaEmulCAS @IO @CUDAIO p c v)

instance (Storable a, Num a) => CmdAtomicAdd CUDAIOM CUDAIO a where
  atomicAddPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO (+) p v)

instance (Storable a, Ord a) => CmdAtomicMinMax CUDAIOM CUDAIO a where
  atomicMinPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO min p v)
  atomicMaxPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO max p v)

instance (Storable a, Bits a) => CmdAtomicBits CUDAIOM CUDAIO a where
  atomicAndPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO (.&.) p v)
  atomicOrPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO (.|.) p v)
  atomicXorPtr p v = CUDAIOM (cudaEmulRMW @IO @CUDAIO xor p v)

instance CmdAtomic CUDADejaFuM CUDADejaFu where
  atomicCAS32Ptr p c v = CUDADejaFuM (cudaEmulCAS @ConcIO @CUDADejaFu p c v)
  atomicCAS64Ptr p c v = CUDADejaFuM (cudaEmulCAS @ConcIO @CUDADejaFu p c v)

instance (Storable a, Num a) => CmdAtomicAdd CUDADejaFuM CUDADejaFu a where
  atomicAddPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu (+) p v)

instance (Storable a, Ord a) => CmdAtomicMinMax CUDADejaFuM CUDADejaFu a where
  atomicMinPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu min p v)
  atomicMaxPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu max p v)

instance (Storable a, Bits a) => CmdAtomicBits CUDADejaFuM CUDADejaFu a where
  atomicAndPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu (.&.) p v)
  atomicOrPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu (.|.) p v)
  atomicXorPtr p v = CUDADejaFuM (cudaEmulRMW @ConcIO @CUDADejaFu xor p v)

hostAtomicCAS :: (Eq a, Storable a) => Identity (Ptr a) -> Identity a -> Identity a -> IO (Identity a)
hostAtomicCAS (Identity p) (Identity cmp) (Identity val) =
  withMVar hostAtomicLock $ \_ -> do
    cur <- peek p
    if cur == cmp then poke p val else pure ()
    pure (Identity cur)

hostAtomicRMW :: (Storable a) => (a -> a -> a) -> Identity (Ptr a) -> Identity a -> IO (Identity a)
hostAtomicRMW f (Identity p) (Identity val) =
  withMVar hostAtomicLock $ \_ -> do
    cur <- peek p
    poke p (f cur val)
    pure (Identity cur)

instance CmdAtomic IO Identity where
  atomicCAS32Ptr = hostAtomicCAS
  atomicCAS64Ptr = hostAtomicCAS

instance (Storable a, Num a) => CmdAtomicAdd IO Identity a where
  atomicAddPtr = hostAtomicRMW (+)

instance (Storable a, Ord a) => CmdAtomicMinMax IO Identity a where
  atomicMinPtr = hostAtomicRMW min
  atomicMaxPtr = hostAtomicRMW max

instance (Storable a, Bits a) => CmdAtomicBits IO Identity a where
  atomicAndPtr = hostAtomicRMW (.&.)
  atomicOrPtr = hostAtomicRMW (.|.)
  atomicXorPtr = hostAtomicRMW xor
