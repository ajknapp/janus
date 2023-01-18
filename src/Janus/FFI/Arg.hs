{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.FFI.Arg where

import Data.Coerce
import Data.Int
import Data.Word
import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr

class FFIArg a where
  arg :: a -> Arg

instance FFIArg CInt where
  arg = argCInt

instance FFIArg CUInt where
  arg = argCUInt

instance FFIArg CLong where
  arg = argCLong

instance FFIArg CULong where
  arg = argCULong

instance FFIArg Int8 where
  arg = argInt8

instance FFIArg Int16 where
  arg = argInt16

instance FFIArg Int32 where
  arg = argInt32

instance FFIArg Int64 where
  arg = argInt64

instance FFIArg Word8 where
  arg = argWord8

instance FFIArg Word16 where
  arg = argWord16

instance FFIArg Word32 where
  arg = argWord32

instance FFIArg Word64 where
  arg = argWord64

instance FFIArg CFloat where
  arg = argCFloat

instance FFIArg CDouble where
  arg = argCDouble

instance FFIArg Float where
  arg = coerce argCFloat

instance FFIArg Double where
  arg = coerce argCDouble

instance FFIArg CSize where
  arg = argCSize

instance FFIArg CChar where
  arg = argCChar

instance FFIArg CUChar where
  arg = argCUChar

instance FFIArg CWchar where
  arg = argCWchar

instance FFIArg (Ptr a) where
  arg = argPtr

class FFIArgPack a where
  argPack :: a -> [Arg]

instance {-# OVERLAPS #-} FFIArg a => FFIArgPack a where
  argPack a = [arg a]

instance FFIArgPack () where
  argPack _ = []

instance (FFIArg a, FFIArg b) => FFIArgPack (a,b) where
  argPack (a,b) = [arg a, arg b]

instance (FFIArg a, FFIArg b, FFIArg c) => FFIArgPack (a,b,c) where
  argPack (a,b,c) = [arg a, arg b, arg c]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d) => FFIArgPack (a,b,c,d) where
  argPack (a,b,c,d) = [arg a, arg b, arg c, arg d]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e) => FFIArgPack (a,b,c,d,e) where
  argPack (a,b,c,d,e) = [arg a, arg b, arg c, arg d, arg e]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f) => FFIArgPack (a,b,c,d,e,f) where
  argPack (a,b,c,d,e,f) = [arg a, arg b, arg c, arg d, arg e, arg f]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g) => FFIArgPack (a,b,c,d,e,f,g) where
  argPack (a,b,c,d,e,f,g) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g, FFIArg h) => FFIArgPack (a,b,c,d,e,f,g,h) where
  argPack (a,b,c,d,e,f,g,h) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g, arg h]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g, FFIArg h, FFIArg i) => FFIArgPack (a,b,c,d,e,f,g,h,i) where
  argPack (a,b,c,d,e,f,g,h,i) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g, arg h, arg i]
