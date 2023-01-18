{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Janus.FFI.Ret where

import Data.Coerce
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr

class FFIRet a where
  ret :: Proxy a -> RetType a

instance FFIRet () where
  ret _ = retVoid

instance FFIRet CInt where
  ret _ = retCInt

instance FFIRet CUInt where
  ret _ = retCUInt

instance FFIRet CLong where
  ret _ = retCLong

instance FFIRet CULong where
  ret _ = retCULong

instance FFIRet Int8 where
  ret _ = retInt8

instance FFIRet Int16 where
  ret _ = retInt16

instance FFIRet Int32 where
  ret _ = retInt32

instance FFIRet Int64 where
  ret _ = retInt64

instance FFIRet Word8 where
  ret _ = retWord8

instance FFIRet Word16 where
  ret _ = retWord16

instance FFIRet Word32 where
  ret _ = retWord32

instance FFIRet Word64 where
  ret _ = retWord64

instance FFIRet CFloat where
  ret _ = retCFloat

instance FFIRet Float where
  ret _ = coerce retCFloat

instance FFIRet CDouble where
  ret _ = retCDouble

instance FFIRet Double where
  ret _ = coerce retCDouble

instance FFIRet CSize where
  ret _ = retCSize

instance FFIRet CChar where
  ret _ = retCChar

instance FFIRet CUChar where
  ret _ = retCUChar

instance FFIRet CWchar where
  ret _ = retCWchar

instance FFIRet a => FFIRet (Ptr a) where
  ret _ = retPtr (ret (Proxy @a))
