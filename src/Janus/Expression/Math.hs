{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Janus.Expression.Math where

import Foreign.C
import Data.Functor.Identity
import Data.Coerce
import Data.Complex
import Janus.Backend.C
import Language.C.Quote
import Data.Loc

class Num a => FMA a where
  fma :: a -> a -> a -> a
  default fma :: Num a => a -> a -> a -> a
  fma a b c = a+b*c

instance FMA Float where
  fma a b c = coerce $ c_fmaf (coerce a) (coerce b) (coerce c)

instance FMA CFloat where
  fma = c_fmaf

instance FMA Double where
  fma a b c = coerce $ c_fma (coerce a) (coerce b) (coerce c)

instance FMA CDouble where
  fma = c_fma

instance FMA a => FMA (Identity a) where
  fma a b c = Identity $ fma (coerce a) (coerce b) (coerce c)

instance FMA (JanusC Float) where
  fma (JanusC a) (JanusC b) (JanusC c) = JanusC $ do
    RVal a' <- a
    RVal b' <- b
    RVal c' <- c
    pure $ RVal $ FnCall (Var (Id "fmaf" noLoc) noLoc) [a', b', c'] noLoc

instance FMA (JanusC Double) where
  fma (JanusC a) (JanusC b) (JanusC c) = JanusC $ do
    RVal a' <- a
    RVal b' <- b
    RVal c' <- c
    pure $ RVal $ FnCall (Var (Id "fma" noLoc) noLoc) [a', b', c'] noLoc

foreign import ccall "fma" c_fma :: CDouble -> CDouble -> CDouble -> CDouble
foreign import ccall "fmaf" c_fmaf :: CFloat -> CFloat -> CFloat -> CFloat

class Floating a => Hypot a where
  hypot :: a -> a -> a
  default hypot :: Floating a => a -> a -> a
  hypot a b = sqrt $ a*a + b*b

instance Hypot (JanusC Float) where
  hypot (JanusC a) (JanusC b) = JanusC $ do
    RVal a' <- a
    RVal b' <- b
    pure $ RVal $ FnCall (Var (Id "hypotf" noLoc) noLoc) [a', b'] noLoc

instance Hypot (JanusC Double) where
  hypot (JanusC a) (JanusC b) = JanusC $ do
    RVal a' <- a
    RVal b' <- b
    pure $ RVal $ FnCall (Var (Id "hypot" noLoc) noLoc) [a', b'] noLoc

instance Hypot Float where
  hypot a b = coerce $ c_hypotf (coerce a) (coerce b)

instance Hypot CFloat where
  hypot = c_hypotf

instance Hypot Double where
  hypot a b = coerce $ c_hypot (coerce a) (coerce b)

instance Hypot CDouble where
  hypot = c_hypot

instance Hypot a => Hypot (Identity a) where
  hypot (Identity a) (Identity b) = Identity (hypot a b)

foreign import ccall "hypot" c_hypot :: CDouble -> CDouble -> CDouble
foreign import ccall "hypotf" c_hypotf :: CFloat -> CFloat -> CFloat

class ExpComplex e a where
  rePart :: e (Complex a) -> e a
  imPart :: e (Complex a) -> e a

instance ExpComplex Identity a where
  rePart = Identity . realPart . runIdentity
  imPart = Identity . imagPart . runIdentity

instance ExpComplex JanusC Float where
  rePart (JanusC z) = JanusC $ do
    RVal z' <- z
    pure $ RVal $ FnCall (Var (Id "crealf" noLoc) noLoc) [z'] noLoc
  imPart (JanusC z) = JanusC $ do
    RVal z' <- z
    pure $ RVal $ FnCall (Var (Id "cimagf" noLoc) noLoc) [z'] noLoc

instance ExpComplex JanusC Double where
  rePart (JanusC z) = JanusC $ do
    RVal z' <- z
    pure $ RVal $ FnCall (Var (Id "creal" noLoc) noLoc) [z'] noLoc
  imPart (JanusC z) = JanusC $ do
    RVal z' <- z
    pure $ RVal $ FnCall (Var (Id "cimag" noLoc) noLoc) [z'] noLoc

extractComplex :: ExpComplex e a => e (Complex a) -> Complex (e a)
extractComplex z = rePart z :+ imPart z
