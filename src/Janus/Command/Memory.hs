{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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
    CmdStackArray (..),
  )
where

import Control.Exception (bracket)
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
import Janus.Backend.C.Quote (freshCId, freshCVar, getCType, jcStms)
import Janus.Backend.C.Raw
import Janus.CBits ()
import Janus.Command.Ref (CmdRef (..), JanusCRefT (..))
import Janus.Typed (JanusTyped)
import Language.C.Quote
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

instance CmdMem (JanusCMT t) (JanusCT t) where
  malloc (JanusC mi) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "malloc" (JanusC mi)
      JC -> do
        RVal i' <- liftJanusCM mi
        (pId, pExp) <- freshCVar "x"
        let align = fromIntegral janusSIMDAlignment :: Word
            mask = fromIntegral (janusSIMDAlignment - 1) :: Word
        [jcStms|
          #include <stdlib.h>
          #include <stddef.h>
          void *$id:pId;
          {
            size_t janus_sz = (size_t)$exp:i';
            size_t janus_alloc_sz = janus_sz == 0U ? $uint:align : ((janus_sz + $uint:mask) & ~$uint:mask);
            $id:pId = aligned_alloc($uint:align, janus_alloc_sz);
          }
        |]
        pure (JanusC (pure (RVal pExp)))
  calloc (JanusC mn) (JanusC ms) = do
    backend <- asks _jcfiBackend
    RVal n' <- liftJanusCM mn
    RVal s' <- liftJanusCM ms
    (pId, pExp) <- freshCVar "x"
    let align = fromIntegral janusSIMDAlignment :: Word
        mask = fromIntegral (janusSIMDAlignment - 1) :: Word
    case backend of
      JCUDA ->
        [jcStms|
          #include <stdlib.h>
          #include <stddef.h>
          #include <string.h>
          void *$id:pId;
          {
            size_t janus_total = (size_t)$exp:n' * (size_t)$exp:s';
            $id:pId = malloc(janus_total);
            if ($id:pId != NULL) {
              memset($id:pId, 0, janus_total);
            }
          }
        |]
      JC ->
        [jcStms|
          #include <stdlib.h>
          #include <stddef.h>
          #include <string.h>
          void *$id:pId;
          {
            size_t janus_total = (size_t)$exp:n' * (size_t)$exp:s';
            size_t janus_alloc_sz = janus_total == 0U ? $uint:align : ((janus_total + $uint:mask) & ~$uint:mask);
            $id:pId = aligned_alloc($uint:align, janus_alloc_sz);
            if ($id:pId != NULL) {
              memset($id:pId, 0, janus_total);
            }
          }
        |]
    pure (JanusC (pure (RVal pExp)))
  realloc (JanusC mp) (JanusC mi) = do
    backend <- asks _jcfiBackend
    case backend of
      JCUDA -> janusCFFICall Nothing "realloc" (JanusC mp) (JanusC mi)
      JC -> do
        RVal p' <- liftJanusCM mp
        RVal i' <- liftJanusCM mi
        (outId, outExp) <- freshCVar "x"
        let align = fromIntegral janusSIMDAlignment :: Word
            mask = fromIntegral (janusSIMDAlignment - 1) :: Word
        [jcStms|
          #include <stdlib.h>
          #include <stddef.h>
          #include <string.h>
          #include <malloc.h>
          void *$id:outId = NULL;
          {
            void *janus_old_p = $exp:p';
            size_t janus_new_sz = (size_t)$exp:i';
            size_t janus_alloc_sz = janus_new_sz == 0U ? $uint:align : ((janus_new_sz + $uint:mask) & ~$uint:mask);
            if (janus_old_p == NULL) {
              $id:outId = aligned_alloc($uint:align, janus_alloc_sz);
            } else if (janus_new_sz == 0U) {
              free(janus_old_p);
            } else {
              size_t janus_old_sz = malloc_usable_size(janus_old_p);
              $id:outId = aligned_alloc($uint:align, janus_alloc_sz);
              if ($id:outId != NULL) {
                memcpy($id:outId, janus_old_p, janus_old_sz < janus_new_sz ? janus_old_sz : janus_new_sz);
                free(janus_old_p);
              }
            }
          }
        |]
        pure (JanusC (pure (RVal outExp)))
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
        RVal szE <- liftJanusCM msz
        RVal alE <- liftJanusCM mal
        (pId, pExp) <- freshCVar "x"
        let minAl = fromIntegral janusSIMDAlignment :: Word
        [jcStms|
          #include <stdlib.h>
          #include <stddef.h>
          void *$id:pId;
          {
            size_t janus_req_al = (size_t)$exp:alE;
            size_t janus_eff_al = janus_req_al < $uint:minAl ? $uint:minAl : janus_req_al;
            size_t janus_raw_sz = (size_t)$exp:szE;
            size_t janus_mask = janus_eff_al - 1U;
            size_t janus_rounded = janus_raw_sz == 0U ? janus_eff_al : ((janus_raw_sz + janus_mask) & ~janus_mask);
            $id:pId = aligned_alloc(janus_eff_al, janus_rounded);
          }
        |]
        pure (JanusC (pure (RVal pExp)))

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
emitGuardedMemCall fnName callArgs nExp =
  [jcStms|
    #include <string.h>
    #include <stddef.h>
    #include <stdint.h>
    {
      int64_t janus_n = $exp:nExp;
      size_t janus_sz = (size_t)janus_n;
      if (janus_n > 0) {
        $id:fnName($args:callArgs, janus_sz);
      }
    }
  |]

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
    let lhs = cDerefPtr p'
    [jcStms| $exp:lhs = $exp:e'; |]
  peekElemOff (JanusC p) (JanusC i) = do
    RVal p' <- liftJanusCM p
    RVal i' <- liftJanusCM i
    letM $ JanusC $ pure $ RVal $ Index p' i' noLoc
  pokeElemOff (JanusC p) (JanusC e) (JanusC i) = do
    RVal p' <- liftJanusCM p
    RVal e' <- liftJanusCM e
    RVal i' <- liftJanusCM i
    [jcStms| $exp:p'[$exp:i'] = $exp:e'; |]

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

-- | Zero-heap-allocation stack-scoped aligned byte buffer (C stack array
-- @uint64_t stack_buf_k[nWords];@ in 'JanusCMT', pinned @allocaBytesAligned@
-- in 'IO'). Enables statically-sized @Janus.Tensor@ programs to execute with
-- zero @malloc@/@calloc@/@free@ calls when intermediates fit within budget.
class (Monad m) => CmdStackArray m e | m -> e, e -> m where
  withStackBytes :: Int -> (e (Ptr Word8) -> m r) -> m r

instance CmdStackArray IO Identity where
  withStackBytes nBytes k =
    Alloc.allocaBytesAligned (max 8 nBytes) 64 (\p -> k (Identity (castPtr p)))

instance CmdStackArray (JanusCMT t) (JanusCT t) where
  withStackBytes nBytes k = do
    let nWords = max 1 ((max 8 nBytes + 7) `div` 8)
    w8PtrTy <- getCType (Proxy @(Ptr Word8))
    vid <- freshCId "stack_buf"
    [jcStms|
      #include <stdint.h>
      uint64_t $id:vid[$int:nWords];
    |]
    k (JanusC (pure (RVal (Cast w8PtrTy (Var vid noLoc) noLoc))))
