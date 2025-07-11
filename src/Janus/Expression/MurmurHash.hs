{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -lcuda -lnvJitLink #-}

module Janus.Expression.MurmurHash where

import Data.Functor.Identity
import qualified Data.Set as Set
import Data.Word
import GHC.Float
import GHC.Generics
import Janus.Backend.C
import Janus.Expression.Bits
import Janus.Expression.Cast
import Janus.Expression.Let
import Janus.Typed
import Language.Haskell.TH

type ExpHash' e = (JanusTyped e Word64, ExpBits e Word64, ExpLet e, Num (e Word64))

-- constants from https://mostlymangling.blogspot.com/2019/12/
hash64 :: ExpHash' e => e Word64 -> e Word64
hash64 = flip let_ id . shiftEor 27 . (* 0x1c69b3f74ac4ae35) . shiftEor 33 . (* 0x3c79ac492ba7b653) . shiftEor 27
  where shiftEor i h = let_ h $ \h' -> h' `ieor` (h' `rshift` i)

hash64WithSalt :: ExpHash' e => e Word64 -> e Word64 -> e Word64
hash64WithSalt salt x = hash64 (hash64 salt `ieor` hash64 x)

class ExpMurmurHash e a where
  murmurHashWithSalt :: e Word64 -> a -> e Word64

$( let inst t =
         [d|
           instance ExpMurmurHash JanusC (JanusC $t) where
             murmurHashWithSalt seed x = hash64WithSalt seed (toIntegral x)

           instance ExpMurmurHash Identity (Identity $t) where
             murmurHashWithSalt seed x = hash64WithSalt seed (toIntegral x)
           |]
       tys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes])
    in concat <$> traverse (inst . pure . ConT) tys
 )

instance ExpMurmurHash Identity (Identity Float) where
  murmurHashWithSalt salt x = hash64WithSalt salt (fromIntegral $ fmap castFloatToWord32 x)

instance ExpMurmurHash Identity (Identity Double) where
  murmurHashWithSalt salt x = hash64WithSalt salt (fmap castDoubleToWord64 x)

instance ExpMurmurHash JanusC (JanusC Float) where
  murmurHashWithSalt salt x = hash64WithSalt salt (toIntegral x)

instance ExpMurmurHash JanusC (JanusC Double) where
  murmurHashWithSalt salt x = hash64WithSalt salt (toIntegral x)

class GExpHash e f where
  ghashWithSalt :: e Word64 -> f a -> e Word64

instance ExpMurmurHash e a => GExpHash e (K1 i a) where
  ghashWithSalt salt (K1 x) = murmurHashWithSalt salt x

instance (ExpHash' e, GExpHash e f, GExpHash e g) => GExpHash e (f :*: g) where
  ghashWithSalt salt (f :*: g) = hash64WithSalt salt (ghashWithSalt salt f `ieor` ghashWithSalt salt g)

instance GExpHash e f => GExpHash e (M1 i c f) where
  ghashWithSalt salt (M1 x) = ghashWithSalt salt x

defaultHashWithSalt :: (GExpHash e (Rep a), Generic a) => e Word64 -> a -> e Word64
defaultHashWithSalt salt x = ghashWithSalt salt (from x)

