{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Command.Format where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Loc
import Data.Proxy
import Data.Void
import Data.Word
import Foreign.C
import Foreign.Ptr
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Janus.Command.IO
import Janus.Expression.Cast
import Janus.Expression.Extract
import Language.C.Quote

class FormatString a where
  formatString :: Proxy a -> String
  default formatString :: (Generic a, GFormatString (Rep a)) => Proxy a -> String
  formatString _ = gformatString (Proxy @(Rep a Void))

instance FormatString Word8 where
  formatString _ = "%hhu"

instance FormatString Word16 where
  formatString _ = "%hu"

instance FormatString Word32 where
  formatString _ = "%u"

instance FormatString Word64 where
  formatString _ = "%llu"

instance FormatString Int8 where
  formatString _ = "%hhd"

instance FormatString Int16 where
  formatString _ = "%hd"

instance FormatString Int32 where
  formatString _ = "%d"

instance FormatString Int64 where
  formatString _ = "%lld"

-- this is a double specifier, but the format instance converts its input from float to double
instance FormatString Float where
  formatString _ = "%f"

instance FormatString Double where
  formatString _ = "%f"

class GFormatString f where
  gformatString :: Proxy (f a) -> String

instance {-# OVERLAPPABLE #-} (GFormatString f) => GFormatString (M1 c m f) where
  gformatString (_ :: Proxy (M1 c m f a)) = gformatString (Proxy @(f a))

instance (KnownSymbol c, GFormatString f) => GFormatString (C1 ('MetaCons c p n) f) where
  gformatString (_ :: Proxy (C1 (MetaCons c p n) f a)) = symbolVal (Proxy @c) <> " {" <> gformatString (Proxy @(f a)) <> "}"

instance (FormatString c) => GFormatString (K1 i c) where
  gformatString _ = formatString (Proxy @c)

instance {-# OVERLAPS #-} (FormatString c) => GFormatString (K1 i (JanusC c)) where
  gformatString _ = formatString (Proxy @c)

instance (KnownSymbol c, GFormatString f) => GFormatString (S1 ('MetaSel ('Just c) u s l) f) where
  gformatString (_ :: Proxy (S1 ('MetaSel ('Just c) u s l) f a)) = symbolVal (Proxy @c) <> " = " <> gformatString (Proxy @(f a))

instance (GFormatString f, GFormatString g) => GFormatString (f :*: g) where
  gformatString (_ :: Proxy ((f :*: g) a)) = gformatString (Proxy @(f a)) <> ", " <> gformatString (Proxy @(g a))

class (CmdIO m e) => CmdFormat m e a | m -> e, e -> m where
  hformat :: e (Ptr CFile) -> e a -> String -> m ()

instance (Show a) => CmdFormat IO Identity a where
  hformat (Identity h) (Identity a) term = withCString (show a <> term) $ \ptr -> void $ c_fputs ptr h

primFormatJanusC :: forall a. (FormatString a) => JanusC (Ptr CFile) -> JanusC a -> String -> JanusCM ()
primFormatJanusC h a term = withString (formatString (Proxy @a) <> term) $ \s -> do
  janusCFFICall (Just "stdio.h") "fprintf" h s a

instance CmdFormat JanusCM JanusC Int32 where
  hformat = primFormatJanusC

instance CmdFormat JanusCM JanusC Word32 where
  hformat = primFormatJanusC

instance CmdFormat JanusCM JanusC Int64 where
  hformat = primFormatJanusC

instance CmdFormat JanusCM JanusC Word64 where
  hformat = primFormatJanusC

instance CmdFormat JanusCM JanusC Float where
  hformat h a = primFormatJanusC h (toFloating a :: JanusC Double)

instance CmdFormat JanusCM JanusC Double where
  hformat = primFormatJanusC

class GJanusCFormatFields f where
  gjcFormatFields :: f a -> String -> JanusCM [Exp]

instance (Generic f, GJanusCFormatFields (Rep f)) => GJanusCFormatFields (K1 i f) where
  gjcFormatFields (K1 f) = gjcFormatFields (GHC.Generics.from f)

instance {-# OVERLAPS #-} GJanusCFormatFields (K1 i (JanusC a)) where
  gjcFormatFields (K1 f) _ = do
    RVal f' <- getJanusC f
    pure [f']

instance (GJanusCFormatFields f, GJanusCFormatFields g) => GJanusCFormatFields (f :*: g) where
  gjcFormatFields (f :*: g) s = (<>) <$> gjcFormatFields f s <*> gjcFormatFields g s

instance (GJanusCFormatFields f, KnownSymbol name) => GJanusCFormatFields (S1 ('MetaSel ('Just name) p s l) f) where
  gjcFormatFields (M1 f) _ = gjcFormatFields f (symbolVal (Proxy @name))

instance (GJanusCFormatFields f) => GJanusCFormatFields (C1 i f) where
  gjcFormatFields (M1 f) = gjcFormatFields f

instance (GJanusCFormatFields f) => GJanusCFormatFields (D1 i f) where
  gjcFormatFields (M1 f) = gjcFormatFields f

defaultJanusCHFormat ::
  forall a.
  (Generic a, FormatString a, GJanusCFormatFields (Rep a), ExpExtract JanusC a) =>
  JanusC (Ptr CFile) ->
  JanusC a ->
  String ->
  JanusCM ()
defaultJanusCHFormat (JanusC h) e term = withString (formatString (Proxy @a) <> term) $ \s -> do
  fields <- gjcFormatFields (GHC.Generics.from $ extract e) ""
  RVal h' <- h
  RVal s' <- getJanusC s
  modifyFunction $ \fn ->
    let block = fn ^. jcfBlock
        x = FnCall (Var (Id "fprintf" noLoc) noLoc) (h' : s' : fields) noLoc
     in fn & jcfBlock .~ appendBlock block [BlockStm $ Exp (Just x) noLoc]

format :: (CmdFormat m e a) => e a -> m ()
format s = hformat stdout s "\n"

class (CmdIO m e, CmdString m e) => CmdPutString m e | e -> m, m -> e where
  hputString :: e (Ptr CFile) -> e CString -> m ()

foreign import ccall "fputs" c_fputs :: CString -> Ptr CFile -> IO CInt

instance CmdPutString IO Identity where
  hputString (Identity h) (Identity s) = void $ c_fputs s h

instance CmdPutString JanusCM JanusC where
  hputString f s = janusCFFICall (Just "stdio.h") "fputs" s f

class CmdString m e | m -> e, e -> m where
  withString :: String -> (e CString -> m a) -> m a

instance CmdString IO Identity where
  withString s f = withCString s (f . Identity)

instance CmdString JanusCM JanusC where
  withString s f = f $ JanusC $ do
    JCType spec dec <- getJanusCType (Proxy @(Ptr CChar))
    case spec of
      DeclSpec attr tqual tspec _ -> do
        fn <- getFunction
        let block = fn ^. jcfBlock
            c = fn ^. jcfVarCounter
            x = Id ("x_" <> show c) noLoc
            strlit = Language.C.Quote.Const (StringConst (pure (show s)) "" noLoc) noLoc
            ini = Init x dec Nothing (Just $ ExpInitializer strlit noLoc) [] noLoc
            inigroup = InitGroup (DeclSpec attr (Tconst noLoc : tqual) tspec noLoc) [] (pure ini) noLoc
            fn' = fn
              & jcfBlock .~ appendBlock block [BlockDecl inigroup]
              & jcfVarCounter +~ 1
        fname <- askFuncName
        modify $ \st -> st & ix fname .~ fn'
        pure $ RVal (Var x noLoc)
      _antiSpec -> error "JanusC.withString: the impossible happened"

putString :: (CmdPutString m e) => e CString -> m ()
putString = hputString stdout

putStringLit :: (CmdString m e, CmdPutString m e) => String -> m ()
putStringLit s = withString s putString
