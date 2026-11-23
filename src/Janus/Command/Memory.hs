{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Core Janus memory, pointer, alignment, copy/fill, and Storable primitives.
module Janus.Command.Memory
  ( ExpSized (..),
    ExpPtr (..),
    janusSIMDAlignment,
    allocaAligned,
    withAlignedArray,
    CmdMem (..),
    CmdDynamicAligned (..),
    CmdMemCopy (..),
    CmdStorable (..),
    CmdRefPtr (..),
  )
where

import Control.Exception (bracket)
import Control.Lens ((%~), (&))
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.C.Types (CSize (..))
import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.Marshal.Array (pokeArray)
import qualified Foreign.Marshal.Utils as MarshalUtils
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import qualified Foreign.Ptr as Ptr
import qualified Foreign.Storable as Storable
import Janus.Backend.C
import Janus.Command.Ref (CmdRef (..), JanusCRefT (..))
import Janus.Typed (JanusTyped)
import Language.C.Quote
import qualified Language.C.Quote as C
import Text.PrettyPrint.Mainland (pretty)
import Text.PrettyPrint.Mainland.Class (ppr)

class (JanusTyped e a) => ExpSized e a where
  sizeOf :: Proxy a -> e Int64
  alignOf :: Proxy a -> e Int64

instance (Storable.Storable a) => ExpSized Identity a where
  sizeOf _ = fromIntegral $ Storable.sizeOf (undefined :: a)
  alignOf _ = fromIntegral $ Storable.alignment (undefined :: a)

instance (JanusCTyped a) => ExpSized (JanusCT t) a where
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
  ptrIndex (Identity p) (Identity i) = Identity $ plusPtr p (fromIntegral i * Storable.sizeOf (undefined :: a))

instance (JanusCTyped a) => ExpPtr (JanusCT t) a where
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

foreign import ccall unsafe "janus_aligned_alloc"
  c_janus_aligned_alloc :: CSize -> CSize -> IO (Ptr ())

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
  malloc (Identity i) = Identity <$> c_janus_aligned_malloc (fromIntegral (max 0 i))
  calloc (Identity i) (Identity s) = Identity <$> c_janus_aligned_calloc (fromIntegral (max 0 i)) (fromIntegral (max 0 s))
  realloc (Identity p) (Identity i) = Identity <$> c_janus_aligned_realloc p (fromIntegral (max 0 i))
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

instance CmdMem (JanusCMT t) (JanusCT t) where
  malloc (JanusC mi) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "malloc" (JanusC mi)
      JC -> do
        addHeader "stdlib.h"
        addHeader "stddef.h"
        RVal i' <- liftJanusCM mi
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
    RVal n' <- liftJanusCM mn
    RVal s' <- liftJanusCM ms
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
        RVal p' <- liftJanusCM mp
        RVal i' <- liftJanusCM mi
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

-- | Janus memory primitive supporting arbitrary power-of-2 alignments @>= 64@
-- paired with Janus 'free'.
class (CmdMem m e) => CmdDynamicAligned m e | m -> e, e -> m where
  alignedMalloc :: e Int64 -> e Int64 -> m (e (Ptr ()))

instance CmdDynamicAligned IO Identity where
  alignedMalloc (Identity sz) (Identity al) =
    let effAl = max (fromIntegral janusSIMDAlignment) al
        rawSz = max 0 sz
     in Identity <$> c_janus_aligned_alloc (fromIntegral effAl) (fromIntegral rawSz)

instance CmdDynamicAligned (JanusCMT t) (JanusCT t) where
  alignedMalloc (JanusC msz) (JanusC mal) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> malloc (JanusC msz)
      JC -> do
        addHeader "stdlib.h"
        addHeader "stddef.h"
        RVal szE <- liftJanusCM msz
        RVal alE <- liftJanusCM mal
        let sizeTy = JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
            minAl = unsignedIntConst (fromIntegral janusSIMDAlignment :: Word)
        reqAl <- emitLocalVal sizeTy (cSizeCast alE)
        effAl <- emitLocalVal sizeTy (Cond (BinOp C.Lt reqAl minAl noLoc) minAl reqAl noLoc)
        rawSz <- emitLocalVal sizeTy (cSizeCast szE)
        let mask = BinOp C.Sub effAl (unsignedIntConst (1 :: Word)) noLoc
            roundedNonZero = BinOp C.And (BinOp C.Add rawSz mask noLoc) (UnOp C.Not mask noLoc) noLoc
            rounded = Cond (BinOp C.Eq rawSz (unsignedIntConst (0 :: Word)) noLoc) effAl roundedNonZero noLoc
            callE = FnCall (Var (Id "aligned_alloc" noLoc) noLoc) [effAl, rounded] noLoc
        p <- emitLocalVal voidPtrType callE
        pure (JanusC (pure (RVal p)))

class (CmdMem m e) => CmdMemCopy m e | m -> e, e -> m where
  cmdMemcpy :: e (Ptr ()) -> e (Ptr ()) -> e Int64 -> m ()
  cmdMemmove :: e (Ptr ()) -> e (Ptr ()) -> e Int64 -> m ()
  cmdMemset :: e (Ptr ()) -> e Word8 -> e Int64 -> m ()

instance CmdMemCopy IO Identity where
  cmdMemcpy (Identity dst) (Identity src) (Identity n) =
    when (n > 0) $ MarshalUtils.copyBytes dst src (fromIntegral n)
  cmdMemmove (Identity dst) (Identity src) (Identity n) =
    when (n > 0) $ MarshalUtils.moveBytes dst src (fromIntegral n)
  cmdMemset (Identity dst) (Identity val) (Identity n) =
    when (n > 0) $ MarshalUtils.fillBytes dst val (fromIntegral n)

emitGuardedMemCall :: String -> [Exp] -> Exp -> JanusCMT t ()
emitGuardedMemCall fnName callArgs nExp = do
  addHeader "string.h"
  addHeader "stddef.h"
  addHeader "stdint.h"
  let sizeTy = JCType (DeclSpec [] [] (Tnamed (Id "size_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)
  nVar <- emitLocalVal (JCType (DeclSpec [] [] (Tnamed (Id "int64_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc)) nExp
  szVar <- emitLocalVal sizeTy (cSizeCast nVar)
  let stm =
        BlockStm $
          If
            (BinOp Gt nVar (signedIntConst (0 :: Int)) noLoc)
            (Block [BlockStm $ Exp (Just $ FnCall (Var (Id fnName noLoc) noLoc) (callArgs <> [szVar]) noLoc) noLoc] noLoc)
            Nothing
            noLoc
  modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [stm]

instance CmdMemCopy (JanusCMT t) (JanusCT t) where
  cmdMemcpy (JanusC mdst) (JanusC msrc) (JanusC mn) = do
    RVal dst <- liftJanusCM mdst
    RVal src <- liftJanusCM msrc
    RVal n <- liftJanusCM mn
    emitGuardedMemCall "memcpy" [dst, src] n
  cmdMemmove (JanusC mdst) (JanusC msrc) (JanusC mn) = do
    RVal dst <- liftJanusCM mdst
    RVal src <- liftJanusCM msrc
    RVal n <- liftJanusCM mn
    emitGuardedMemCall "memmove" [dst, src] n
  cmdMemset (JanusC mdst) (JanusC mval) (JanusC mn) = do
    RVal dst <- liftJanusCM mdst
    RVal val <- liftJanusCM mval
    RVal n <- liftJanusCM mn
    let intTy = Type (DeclSpec [] [] (Tint Nothing noLoc) noLoc) (DeclRoot noLoc) noLoc
    emitGuardedMemCall "memset" [dst, Cast intTy val noLoc] n

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

cDerefPtr :: Exp -> Exp
cDerefPtr (UnOp AddrOf lval _) = lval
cDerefPtr p = UnOp Deref p noLoc

instance (JanusCTyped a) => CmdStorable (JanusCMT t) (JanusCT t) a where
  peek (JanusC p) = do
    RVal p' <- liftJanusCM p
    letM $ JanusC $ pure $ RVal $ cDerefPtr p'
  poke (JanusC p) (JanusC e) = do
    RVal p' <- liftJanusCM p
    RVal e' <- liftJanusCM e
    modifyFunction $ \f ->
      let pokestm = [BlockStm $ Exp (Just $ Assign (cDerefPtr p') JustAssign e' noLoc) noLoc]
       in f & jcfBlock %~ flip appendBlock pokestm
  peekElemOff (JanusC p) (JanusC i) = do
    RVal p' <- liftJanusCM p
    RVal i' <- liftJanusCM i
    letM $ JanusC $ pure $ RVal $ Index p' i' noLoc
  pokeElemOff (JanusC p) (JanusC e) (JanusC i) = do
    RVal p' <- liftJanusCM p
    RVal e' <- liftJanusCM e
    RVal i' <- liftJanusCM i
    modifyFunction $ \f ->
      let pokestm = [BlockStm $ Exp (Just $ Assign (Index p' i' noLoc) JustAssign e' noLoc) noLoc]
       in f & jcfBlock %~ flip appendBlock pokestm

-- using the ref while the pointer in scope is undefined behavior
class (CmdRef m e, CmdStorable m e a) => CmdRefPtr m e a | m -> e, e -> m where
  withRefPtr :: Ref m e a -> (e (Ptr a) -> m b) -> m b

instance (Storable.Storable a) => CmdRefPtr IO Identity a where
  withRefPtr r f = do
    Identity r' <- readRef r
    Alloc.alloca $ \p -> do
      Storable.poke p r'
      b <- f (Identity p)
      Storable.peek p >>= writeRef r . Identity
      pure b

instance (JanusCTyped a) => CmdRefPtr (JanusCMT t) (JanusCT t) a where
  withRefPtr (JanusCRefT (LVal r)) f = f $ JanusC $ pure $ RVal $ UnOp AddrOf r noLoc
