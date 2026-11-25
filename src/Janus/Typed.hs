{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Janus.Typed where

import Data.Functor.Identity
import Data.Int
import Data.Kind as K
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word
import Foreign.C
import Janus.Backend.C.Raw
import Language.Haskell.TH

type family JanusTyped (e :: K.Type -> K.Type) :: K.Type -> Constraint

class IdConstraint a where
instance IdConstraint a where

-- | Constraint bundle combining @'JanusTyped' e a@ with @'Typeable' a@.
class (JanusTyped e a, Typeable a) => TypeableTyped (e :: K.Type -> K.Type) (a :: K.Type)

instance (JanusTyped e a, Typeable a) => TypeableTyped e a

type instance JanusTyped Identity = IdConstraint

type instance JanusTyped (JanusCT t) = JanusCTyped

-- | Class wrapper carrying @'JanusTyped' e a@ as a superclass without a blanket
-- instance, allowing @QuantifiedConstraints@ over @'KnownJanusType' e@ to compose
-- across concrete backends and expression transformers without instance overlap.
class (JanusTyped e a) => KnownJanusType (e :: K.Type -> K.Type) (a :: K.Type)

instance KnownJanusType Identity a

instance (JanusCTyped a) => KnownJanusType (JanusCT t) a

janusBoolTypes :: Set.Set Name
janusBoolTypes = Set.fromList [''CBool, ''Bool]

-- TODO check if char is signed
janusSignedIntTypes :: Set.Set Name
janusSignedIntTypes = Set.fromList
  [''CChar, ''CSChar, ''CShort, ''CInt, ''CLong, ''CLLong, ''Int8, ''Int16, ''Int32, ''Int64]

janusUnsignedIntTypes :: Set.Set Name
janusUnsignedIntTypes = Set.fromList
  [''CSize, ''CUChar, ''CUShort, ''CUInt, ''CULong, ''CULLong, ''Word8, ''Word16, ''Word32, ''Word64]

janusFloatTypes :: Set.Set Name
janusFloatTypes = Set.fromList [''Float, ''Double, ''CFloat, ''CDouble]
