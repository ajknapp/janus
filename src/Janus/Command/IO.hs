{-# LANGUAGE FunctionalDependencies #-}

module Janus.Command.IO where

import Data.Functor.Identity
import Data.Loc
import Foreign.C
import Foreign.Ptr
import Janus.Backend.C
import Language.C.Quote

class CmdIO m e | m -> e, e -> m where
  stdout :: e (Ptr CFile)
  stderr :: e (Ptr CFile)
  fopen :: e (Ptr CChar) -> e (Ptr CChar) -> m (e (Ptr CFile))
  fclose :: e (Ptr CFile) -> m (e CInt)

foreign import ccall "janus_stdout" c_stdout :: Ptr CFile

foreign import ccall "janus_stderr" c_stderr :: Ptr CFile

foreign import ccall "fopen" c_fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall "fclose" c_fclose :: Ptr CFile -> IO CInt

instance CmdIO IO Identity where
  stdout = Identity c_stdout
  stderr = Identity c_stderr
  fopen (Identity cstr) (Identity mode) = Identity <$> c_fopen cstr mode
  fclose (Identity cptr) = Identity <$> c_fclose cptr

instance CmdIO JanusCM JanusC where
  stdout = JanusC $ do
    addHeader "stdio.h"
    pure $ RVal $ Var (Id "stdout" noLoc) noLoc
  stderr = JanusC $ do
    addHeader "stdio.h"
    pure $ RVal $ Var (Id "stderr" noLoc) noLoc
  fopen = janusCFFICall (Just "stdio.h") "fopen"
  fclose = janusCFFICall (Just "stdio.h") "fclose"
