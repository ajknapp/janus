{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Janus.FFI.Ret where

import Data.Coerce
import Data.Int
import Data.Proxy
import Data.Typeable (Typeable, typeRep)
import Data.Word
import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr
import Janus.FFI.Arg (typeRepToTHType)
import Language.Haskell.TH (Exp (AppE, VarE), Q, Type (ConT))
import qualified Language.Haskell.TH as TH

{-# INLINE word8ToBool #-}
word8ToBool :: Word8 -> Bool
word8ToBool b = b /= 0

data THRetEffect
  = THRetPure
  | THRetIO
  deriving (Eq, Ord, Show)

data THRetMode
  = THRetDirect TH.Type
  | THRetPureMap TH.Type (TH.Exp -> TH.Exp)
  | THRetOutPtr ((TH.Exp -> Q TH.Exp) -> Q TH.Exp)

data THRetSpec = THRetSpec
  { thRetEffect :: THRetEffect,
    thRetUserBaseType :: TH.Type,
    thRetMode :: THRetMode
  }

defaultFFITHRetSpec :: (Typeable a) => THRetEffect -> Proxy a -> THRetSpec
defaultFFITHRetSpec eff p =
  let userTy = typeRepToTHType (typeRep p)
   in THRetSpec
        { thRetEffect = eff,
          thRetUserBaseType = userTy,
          thRetMode = THRetDirect userTy
        }

class FFIRet a where
  ret :: Proxy a -> RetType a
  ffiTHRetSpec :: (Typeable a) => THRetEffect -> Proxy a -> THRetSpec
  ffiTHRetSpec = defaultFFITHRetSpec

instance FFIRet () where
  ret _ = retVoid

instance FFIRet Bool where
  ret _ = withRetType (pure . (/= 0)) retCChar
  ffiTHRetSpec eff _ =
    THRetSpec
      { thRetEffect = eff,
        thRetUserBaseType = ConT ''Bool,
        thRetMode = THRetPureMap (ConT ''Word8) (AppE (VarE 'word8ToBool))
      }

instance FFIRet CBool where
  ret _ = coerce retWord8

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

instance (FFIRet a) => FFIRet (Ptr a) where
  ret _ = retPtr (ret (Proxy @a))
