{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.FFI.Arg where

import Data.Char (isDigit)
import Data.Coerce
import Data.Int
import Data.Proxy (Proxy)
import Data.Typeable (TypeRep, Typeable, splitTyConApp, tyConModule, tyConName, tyConPackage, typeRep)
import Data.Word
import Foreign.C
import Foreign.LibFFI
import Foreign.Ptr
import Language.Haskell.TH (Exp (AppE, VarE), Q, TyLit (NumTyLit), Type (AppT, ArrowT, ConT, ListT, LitT, PromotedT, TupleT))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (mkNameG_d, mkNameG_tc)

type THNat1 = 1

type THNat2 = 2

type THNat4 = 4

type THNat8 = 8

type THNat16 = 16

type THNat32 = 32

type THNat64 = 64

{-# INLINE boolToWord8 #-}
boolToWord8 :: Bool -> Word8
boolToWord8 b = if b then 1 else 0

natToTHType :: Integer -> TH.Type
natToTHType 1 = ConT ''THNat1
natToTHType 2 = ConT ''THNat2
natToTHType 4 = ConT ''THNat4
natToTHType 8 = ConT ''THNat8
natToTHType 16 = ConT ''THNat16
natToTHType 32 = ConT ''THNat32
natToTHType 64 = ConT ''THNat64
natToTHType n = LitT (NumTyLit n)

typeRepToTHType :: TypeRep -> TH.Type
typeRepToTHType tr =
  let (tc, args) = splitTyConApp tr
      pkg = tyConPackage tc
      modl = tyConModule tc
      nm = tyConName tc
      argTys = map typeRepToTHType args
      applyAll hd = foldl AppT hd argTys
   in case nm of
        "Unit" | null argTys -> TupleT 0
        "()" | null argTys -> TupleT 0
        "List" -> applyAll ListT
        "[]" -> applyAll ListT
        "->" -> applyAll ArrowT
        "FUN" | [_, a, b] <- argTys -> AppT (AppT ArrowT a) b
        _
          | not (null nm) && all isDigit nm && null argTys ->
              natToTHType (read nm)
          | '\'' : rest <- nm,
            not (null rest) ->
              applyAll (PromotedT (mkNameG_d pkg modl rest))
          | otherwise ->
              applyAll (ConT (mkNameG_tc pkg modl nm))

voidPtrTHType :: TH.Type
voidPtrTHType = AppT (ConT ''Ptr) (TupleT 0)

unitTHType :: TH.Type
unitTHType = TupleT 0

data THParamMode
  = THParamDirect
  | THParamPure (TH.Exp -> TH.Exp)
  | THParamIO ((TH.Exp -> Q TH.Exp) -> TH.Exp -> Q TH.Exp)

data THParamSpec = THParamSpec
  { thParamUserType :: TH.Type,
    thParamRawType :: TH.Type,
    thParamMode :: THParamMode
  }

defaultFFITHParamSpec :: (Typeable a) => Proxy a -> THParamSpec
defaultFFITHParamSpec p =
  let userTy = typeRepToTHType (typeRep p)
   in THParamSpec
        { thParamUserType = userTy,
          thParamRawType = userTy,
          thParamMode = THParamDirect
        }

class FFIArg a where
  arg :: a -> Arg
  ffiTHParamSpec :: (Typeable a) => Proxy a -> THParamSpec
  ffiTHParamSpec = defaultFFITHParamSpec

instance FFIArg Bool where
  arg = argCChar . fromIntegral . fromEnum
  ffiTHParamSpec _ =
    THParamSpec
      { thParamUserType = ConT ''Bool,
        thParamRawType = ConT ''Word8,
        thParamMode = THParamPure (AppE (VarE 'boolToWord8))
      }

instance FFIArg CBool where
  arg = coerce argWord8

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

instance {-# OVERLAPS #-} (FFIArg a) => FFIArgPack a where
  argPack a = [arg a]

instance FFIArgPack () where
  argPack _ = []

instance (FFIArg a, FFIArg b) => FFIArgPack (a, b) where
  argPack (a, b) = [arg a, arg b]

instance (FFIArg a, FFIArg b, FFIArg c) => FFIArgPack (a, b, c) where
  argPack (a, b, c) = [arg a, arg b, arg c]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d) => FFIArgPack (a, b, c, d) where
  argPack (a, b, c, d) = [arg a, arg b, arg c, arg d]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e) => FFIArgPack (a, b, c, d, e) where
  argPack (a, b, c, d, e) = [arg a, arg b, arg c, arg d, arg e]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f) => FFIArgPack (a, b, c, d, e, f) where
  argPack (a, b, c, d, e, f) = [arg a, arg b, arg c, arg d, arg e, arg f]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g) => FFIArgPack (a, b, c, d, e, f, g) where
  argPack (a, b, c, d, e, f, g) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g, FFIArg h) => FFIArgPack (a, b, c, d, e, f, g, h) where
  argPack (a, b, c, d, e, f, g, h) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g, arg h]

instance (FFIArg a, FFIArg b, FFIArg c, FFIArg d, FFIArg e, FFIArg f, FFIArg g, FFIArg h, FFIArg i) => FFIArgPack (a, b, c, d, e, f, g, h, i) where
  argPack (a, b, c, d, e, f, g, h, i) = [arg a, arg b, arg c, arg d, arg e, arg f, arg g, arg h, arg i]
