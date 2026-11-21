{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Janus.Command.Array where

import Control.Category
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Reader (asks)
import Data.Foldable
import Data.Int
import Data.Kind as K
import Data.Loc
import Data.Proxy
import Data.Typeable
import Foreign.C.Types (CSize (..))
import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr as Ptr
import qualified Foreign.Storable as Storable
import GHC.TypeLits
import Janus.Backend.C
import Janus.Control.Drain
import Janus.Control.Pipe
import Janus.Control.Step
import Janus.Control.Stream
import Janus.Command.Ref
import Janus.Command.Range
import Janus.Command.While
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.Typed
import Language.C.Quote
import Prelude hiding (id, (.))
import Text.PrettyPrint.Mainland (pretty)
import Text.PrettyPrint.Mainland.Class

class JanusTyped e a => ExpSized e a where
  sizeOf :: Proxy a -> e Int64
  alignOf :: Proxy a -> e Int64

instance (Storable.Storable a) => ExpSized Identity a where
  sizeOf _ = fromIntegral $ Storable.sizeOf (undefined :: a)
  alignOf _ = fromIntegral $ Storable.alignment (undefined :: a)

instance JanusCTyped a => ExpSized JanusC a where
  sizeOf p = JanusC $ do
    JCType spec dec <- getJanusCType p
    pure $ RVal $ SizeofType (Type spec dec noLoc) noLoc
  -- see https://wambold.com/Martin/writings/alignof.html for details
  -- this may need to be fixed if we add function pointers
  alignOf p = JanusC $ do
    addHeader "stddef.h"
    JCType spec dec <- getJanusCType p
    let str = "struct { char x; " <> pretty 120 (ppr $ Type spec dec noLoc) <> " field; }"
        ty = Var (Id str noLoc) noLoc
        aof = FnCall (Var (Id "offsetof" noLoc) noLoc) [ty, Var (Id "field" noLoc) noLoc] noLoc
    pure $ RVal aof

class ExpPtr e a where
  nullPtr :: e (Ptr a)
  ptrAdd :: e (Ptr a) -> e Int64 -> e (Ptr a)
  ptrIndex :: e (Ptr a) -> e Int64 -> e (Ptr a)

instance (Storable.Storable a) => ExpPtr Identity a where
  nullPtr = Identity Ptr.nullPtr
  ptrAdd (Identity p) (Identity i) = Identity $ plusPtr p (fromIntegral i)
  ptrIndex (Identity p) (Identity i) = Identity $ plusPtr p (fromIntegral i*Storable.sizeOf (undefined :: a))

instance JanusCTyped a => ExpPtr JanusC a where
  nullPtr = JanusC $ do
    addHeader "stddef.h"
    pure $ RVal $ Var (Id "NULL" noLoc) noLoc
  ptrAdd (JanusC p) (JanusC i) = JanusC $ do
    RVal p' <- p
    RVal i' <- i
    let JCType spec dec = voidPtrType
    JCType spec' dec' <- getJanusCType (Proxy @(Ptr a))
    pure $ RVal $ Cast (Type spec' dec' noLoc) (BinOp Add (Cast (Type spec dec noLoc) p' noLoc) i' noLoc) noLoc
  ptrIndex (JanusC p) (JanusC i) = JanusC $ do
    RVal p' <- p
    RVal i' <- i
    pure $ RVal $ BinOp Add p' i' noLoc

janusSIMDAlignment :: Int
janusSIMDAlignment = 64

foreign import ccall unsafe "janus_aligned_malloc"
  c_janus_aligned_malloc :: CSize -> IO (Ptr ())

foreign import ccall unsafe "janus_aligned_calloc"
  c_janus_aligned_calloc :: CSize -> CSize -> IO (Ptr ())

foreign import ccall unsafe "janus_aligned_realloc"
  c_janus_aligned_realloc :: Ptr () -> CSize -> IO (Ptr ())

foreign import ccall unsafe "janus_aligned_free"
  c_janus_aligned_free :: Ptr () -> IO ()

allocaAligned :: forall a b. (Storable.Storable a) => Int -> (Ptr a -> IO b) -> IO b
allocaAligned n k =
  let elemBytes = Storable.sizeOf (undefined :: a)
   in bracket
        (c_janus_aligned_calloc (fromIntegral (max 0 n)) (fromIntegral elemBytes))
        c_janus_aligned_free
        (k . castPtr)

withAlignedArray :: (Storable.Storable a) => [a] -> (Ptr a -> IO b) -> IO b
withAlignedArray xs k =
  allocaAligned (length xs) $ \p -> do
    pokeArray p xs
    k p

class CmdMem m e | m -> e, e -> m where
  malloc :: e Int64 -> m (e (Ptr ()))
  calloc :: e Int64 -> e Int64 -> m (e (Ptr ()))
  realloc :: e (Ptr ()) -> e Int64 -> m (e (Ptr ()))
  free :: e (Ptr ()) -> m ()

instance CmdMem IO Identity where
  malloc (Identity i) = Identity <$> c_janus_aligned_malloc (fromIntegral i)
  calloc (Identity i) (Identity s) = Identity <$> c_janus_aligned_calloc (fromIntegral i) (fromIntegral s)
  realloc (Identity p) (Identity i) = Identity <$> c_janus_aligned_realloc p (fromIntegral i)
  free (Identity p) = c_janus_aligned_free p

cAlignedRoundUp :: Exp -> Exp
cAlignedRoundUp szExp =
  let alignE = unsignedIntConst (fromIntegral janusSIMDAlignment :: Word)
      maskE = unsignedIntConst (fromIntegral (janusSIMDAlignment - 1) :: Word)
      zeroE = unsignedIntConst (0 :: Word)
      rounded = BinOp And (BinOp Add szExp maskE noLoc) (UnOp Not maskE noLoc) noLoc
   in Cond (BinOp Eq szExp zeroE noLoc) alignE rounded noLoc

cSizeCast :: Exp -> Exp
cSizeCast e = Cast (Type (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc) e noLoc

instance CmdMem JanusCM JanusC where
  malloc (JanusC mi) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "malloc" (JanusC mi)
      JC -> do
        addHeader "stdlib.h"
        addHeader "stddef.h"
        RVal i' <- mi
        sz <- emitLocalVal (JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)) (cSizeCast i')
        let alignE = unsignedIntConst (fromIntegral janusSIMDAlignment :: Word)
            callE = FnCall (Var (Id "aligned_alloc" noLoc) noLoc) [alignE, cAlignedRoundUp sz] noLoc
        p <- emitLocalVal voidPtrType callE
        pure (JanusC (pure (RVal p)))
  calloc (JanusC mn) (JanusC ms) = do
    backend <- asks _jcfiBackend
    addHeader "stdlib.h"
    addHeader "stddef.h"
    addHeader "string.h"
    RVal n' <- mn
    RVal s' <- ms
    let sizeTy = JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
    total <- emitLocalVal sizeTy (BinOp Mul (cSizeCast n') (cSizeCast s') noLoc)
    p <- case backend of
      JCUDA -> emitLocalVal voidPtrType (FnCall (Var (Id "malloc" noLoc) noLoc) [total] noLoc)
      JC -> do
        let alignE = unsignedIntConst (fromIntegral janusSIMDAlignment :: Word)
        emitLocalVal voidPtrType (FnCall (Var (Id "aligned_alloc" noLoc) noLoc) [alignE, cAlignedRoundUp total] noLoc)
    let zeroStm =
          BlockStm $
            If
              (BinOp Ne p (Var (Id "NULL" noLoc) noLoc) noLoc)
              (Block [BlockStm $ Exp (Just $ FnCall (Var (Id "memset" noLoc) noLoc) [p, signedIntConst (0 :: Int), total] noLoc) noLoc] noLoc)
              Nothing
              noLoc
    modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [zeroStm]
    pure (JanusC (pure (RVal p)))
  realloc (JanusC mp) (JanusC mi) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "realloc" (JanusC mp) (JanusC mi)
      JC -> do
        addHeader "stdlib.h"
        addHeader "stddef.h"
        addHeader "string.h"
        addHeader "malloc.h"
        RVal p' <- mp
        RVal i' <- mi
        let sizeTy = JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
            alignE = unsignedIntConst (fromIntegral janusSIMDAlignment :: Word)
            nullE = Var (Id "NULL" noLoc) noLoc
        oldP <- emitLocalVal voidPtrType p'
        newSz <- emitLocalVal sizeTy (cSizeCast i')
        outP <- emitLocalVal voidPtrType nullE
        let allocCall = FnCall (Var (Id "aligned_alloc" noLoc) noLoc) [alignE, cAlignedRoundUp newSz] noLoc
            usableE = FnCall (Var (Id "malloc_usable_size" noLoc) noLoc) [oldP] noLoc
            oldSzId = Id "janus_old_sz" noLoc
            oldSzDecl =
              BlockDecl $
                InitGroup
                  (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc)
                  []
                  [Init oldSzId (DeclRoot noLoc) Nothing (Just (ExpInitializer usableE noLoc)) [] noLoc]
                  noLoc
            oldSzVar = Var oldSzId noLoc
            copyLen = Cond (BinOp Lt oldSzVar newSz noLoc) oldSzVar newSz noLoc
            moveBlock =
              Block
                [ oldSzDecl,
                  BlockStm $ Exp (Just $ Assign outP JustAssign allocCall noLoc) noLoc,
                  BlockStm $
                    If
                      (BinOp Ne outP nullE noLoc)
                      ( Block
                          [ BlockStm $ Exp (Just $ FnCall (Var (Id "memcpy" noLoc) noLoc) [outP, oldP, copyLen] noLoc) noLoc,
                            BlockStm $ Exp (Just $ FnCall (Var (Id "free" noLoc) noLoc) [oldP] noLoc) noLoc
                          ]
                          noLoc
                      )
                      Nothing
                      noLoc
                ]
                noLoc
            reallocStm =
              BlockStm $
                If
                  (BinOp Eq oldP nullE noLoc)
                  (Block [BlockStm $ Exp (Just $ Assign outP JustAssign allocCall noLoc) noLoc] noLoc)
                  ( Just $
                      Block
                        [ BlockStm $
                            If
                              (BinOp Eq newSz (unsignedIntConst (0 :: Word)) noLoc)
                              (Block [BlockStm $ Exp (Just $ FnCall (Var (Id "free" noLoc) noLoc) [oldP] noLoc) noLoc] noLoc)
                              (Just moveBlock)
                              noLoc
                        ]
                        noLoc
                  )
                  noLoc
        modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [reallocStm]
        pure (JanusC (pure (RVal outP)))
  free p = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "free" p
      JC -> janusCFFICall (Just "stdlib.h") "free" p

class (ExpSized e a) => CmdStorable m e a | m -> e, e -> m where
  peek :: e (Ptr a) -> m (e a)
  poke :: e (Ptr a) -> e a -> m ()
  peekElemOff :: e (Ptr a) -> e Int64 -> m (e a)
  pokeElemOff :: e (Ptr a) -> e a -> e Int64 -> m ()

instance (Storable.Storable a) => CmdStorable IO Identity a where
  peek (Identity p) = Identity <$> Storable.peek p
  peekElemOff (Identity p) (Identity i) = Identity <$> Storable.peekElemOff p (fromIntegral i)
  poke (Identity p) (Identity a) = Storable.poke p a
  pokeElemOff (Identity p) (Identity a) (Identity i) = Storable.pokeElemOff p (fromIntegral i) a

instance JanusCTyped a => CmdStorable JanusCM JanusC a where
  peek (JanusC p) = do
    RVal p' <- p
    letM $ JanusC $ pure $ RVal $ UnOp Deref p' noLoc
  poke (JanusC p) (JanusC e) = do
    RVal p' <- p
    RVal e' <- e
    modifyFunction $ \f ->
      let pokestm = [BlockStm $ Exp (Just $ Assign (UnOp Deref p' noLoc) JustAssign e' noLoc) noLoc]
      in f & jcfBlock %~ flip appendBlock pokestm
  peekElemOff (JanusC p) (JanusC i) = do
    RVal p' <- p
    RVal i' <- i
    letM $ JanusC $ pure $ RVal $ Index p' i' noLoc
  pokeElemOff (JanusC p) (JanusC e) (JanusC i) = do
    RVal p' <- p
    RVal e' <- e
    RVal i' <- i
    modifyFunction $ \f ->
      let pokestm = [BlockStm $ Exp (Just $ Assign (Index p' i' noLoc) JustAssign e' noLoc) noLoc]
      in f & jcfBlock %~ flip appendBlock pokestm

-- using the ref while the pointer in scope is undefined behavior
class (CmdRef m e, CmdStorable m e a) => CmdRefPtr m e a | m -> e, e -> m where
  withRefPtr :: Ref m e a -> (e (Ptr a) -> m b) -> m b

instance Storable.Storable a => CmdRefPtr IO Identity a where
  withRefPtr r f = do
    Identity r' <- readRef r
    Alloc.alloca $ \p -> do
      Storable.poke p r'
      b <- f (Identity p)
      Storable.peek p >>= writeRef r . Identity
      pure b

instance JanusCTyped a => CmdRefPtr JanusCM JanusC a where
  withRefPtr (JanusCRef (LVal r)) f = f $ JanusC $ pure $ RVal $ UnOp AddrOf r noLoc

data Tensor (ds :: [Nat]) e a where
  Tensor :: TensorBound ds e -> e (Ptr a) -> Tensor ds e a

instance (JanusCTyped a, Typeable ds) => JanusCTyped (Tensor ds JanusC a) where
  jctype _ = jctype (Proxy @(Ptr a))

data TensorBound (ds :: [Nat]) (e :: K.Type -> K.Type) where
  TensorBoundNil :: TensorBound '[] e
  TensorBoundCons :: (KnownNat n) => Proxy n -> TensorBound ns e -> TensorBound (n ': ns) e

deriving instance Show (TensorBound ds e)

data TensorIndex (ds :: [Nat]) e where
  Z :: TensorIndex '[] e
  (:.) :: e Int64 -> TensorIndex ds e -> TensorIndex (d ': ds) e

infixr 5 :.

deriving instance (Show (e Int64)) => Show (TensorIndex ds e)

class ReifyTensorBound (ds :: [Nat]) where
  tensorBound :: Proxy ds -> TensorBound ds e

instance ReifyTensorBound '[] where
  tensorBound _ = TensorBoundNil

instance (KnownNat d, ReifyTensorBound ds) => ReifyTensorBound (d ': ds) where
  tensorBound _ = TensorBoundCons (Proxy @d) (tensorBound (Proxy @ds))

tensorBoundSize :: TensorBound ds e -> Int
tensorBoundSize = go 1
  where
    go :: Int -> TensorBound ds e -> Int
    go s TensorBoundNil = s
    go s (TensorBoundCons p ts) = go (fromIntegral (natVal p) * s) ts

tensorSize :: Tensor ds e a -> Int
tensorSize (Tensor tb _) = tensorBoundSize tb

withTensor ::
  forall ds e a b.
  (Storable.Storable a, Num (e Int64), ExpInject e (Ptr a)) =>
  TensorBound ds e ->
  (Tensor ds e a -> IO b) ->
  IO b
withTensor ti f =
  allocaAligned (tensorBoundSize ti) $ \(p :: Ptr a) ->
    f (Tensor ti (inject p))

tbToList :: (Num (e Int64)) => TensorBound ds e -> [e Int64]
tbToList TensorBoundNil = []
tbToList (TensorBoundCons p bs) = fromIntegral (natVal p) : tbToList bs

tiToList :: TensorIndex ds e -> [e Int64]
tiToList Z = []
tiToList (i :. is) = i : tiToList is

rowIndex :: (Num (e Int64)) => [e Int64] -> [e Int64] -> e Int64
rowIndex ti (_:tb) = foldl' (\s (i, b) -> i + b * s) 0 $ zip ti (1 : tb)
rowIndex _ _ = error "rowIndex: the impossible happened"

readTensor :: (Num (e Int64), CmdStorable m e a) => Tensor ds e a -> TensorIndex ds e -> m (e a)
readTensor (Tensor tb p) ti = peekElemOff p (rowIndex ti' tb')
  where
    tb' = tbToList tb
    ti' = tiToList ti

writeTensor :: (Num (e Int64), CmdStorable m e a) => Tensor ds e a -> TensorIndex ds e -> e a -> m ()
writeTensor (Tensor tb p) ti a = pokeElemOff p a (rowIndex ti' tb')
  where
    tb' = tbToList tb
    ti' = tiToList ti

newTensor ::
  forall m e a ds.
  ( Monad m,
    CmdMem m e,
    ExpSized e a,
    ExpPtrCast e,
    JanusTyped e a,
    Num (e Int64)
  ) =>
  TensorBound ds e ->
  m (Tensor ds e a)
newTensor tb = do
  let n = tensorBoundSize tb
  p <- calloc (fromIntegral n) (sizeOf (Proxy @a))
  pure $ Tensor tb (fromVoidPtr p)

freeTensor :: (CmdMem m e, ExpPtrCast e, JanusTyped e a) => Tensor ds e a -> m ()
freeTensor (Tensor _ p) = free (toVoidPtr p)

type Vector n e a = Tensor '[n] e a

type Matrix n m e a = Tensor '[n, m] e a

type Tensor3 n m p e a = Tensor '[n, m, p] e a

type Tensor4 n m p q e a = Tensor '[n, m, p, q] e a

tensorIndices ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (TensorIndex ds e)
tensorIndices (Tensor bds _) = Stream $ pure $ ContT $ \k -> do
  iterateTensorBounds bds (k . Yield)
  k Stop

iterateTensorBounds ::
  ( Monad m,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  TensorBound sh e ->
  (TensorIndex sh e -> m ()) ->
  m ()
iterateTensorBounds TensorBoundNil k = k Z
iterateTensorBounds (TensorBoundCons p ts) k = rangeM 0 (fromIntegral $ natVal p) $ \i ->
  iterateTensorBounds ts $ \idx -> k (i :. idx)

tensorValuesS ::
  ( Monad m,
    CmdRef m e,
    CmdStorable m e a,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (e a)
tensorValuesS t = streamM (tensorIndices t) (readTensor t)

tensorIndicesValuesS ::
  ( Monad m,
    CmdRef m e,
    CmdStorable m e a,
    CmdRange m e,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  Tensor ds e a ->
  Stream m (TensorIndex ds e, e a)
tensorIndicesValuesS t = streamM (tensorIndices t) $ \idx -> do
  ti <- readTensor t idx
  pure (idx, ti)

innerProduct ::
  ( CmdRange m e,
    CmdRef m e,
    CmdWhile m e,
    CmdStorable m e a,
    JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    ExpOrd e Int64,
    Num (e Int64),
    Num (e a)
  ) =>
  Vector n e a ->
  Vector n e a ->
  m (e a)
innerProduct x y = runDrain drainSum $ runPipe (pipeM $ \idx -> do
  xi <- readTensor x idx
  yi <- readTensor y idx
  letM (xi * yi)) (tensorIndices x)

vectorNorm2 ::
  ( JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    Floating (e a),
    CmdRange m e,
    CmdWhile m e,
    CmdRef m e,
    CmdStorable m e a,
    ExpOrd e Int64,
    Num (e Int64)
  ) =>
  Vector n e a ->
  m (e a)
vectorNorm2 x = runDrain drainSum $ runPipe (pipeM $ \idx -> do
  xi <- readTensor x idx
  letM (xi * xi)) (tensorIndices x)

vectorNorm ::
  ( JanusTyped e Bool,
    JanusTyped e a,
    JanusTyped e Int64,
    Floating (e a),
    CmdRange m e,
    CmdWhile m e,
    CmdRef m e,
    CmdStorable m e a,
    ExpOrd e Int64,
    Num (e Int64),
    JanusTyped e Bool
  ) =>
  Vector n e a ->
  m (e a)
vectorNorm x = vectorNorm2 x >>= letM . sqrt
