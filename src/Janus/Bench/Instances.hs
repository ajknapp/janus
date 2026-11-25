{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Multi-parameter Janus expression, SIMD, fixpoint, memory, I/O, and control
-- instances for @('BenchT' c m e, 'Bench' c e)@, derived from
-- @('Control.Monad.Reader.ReaderT', 'Janus.Control.Reader.ReaderExp')@.
module Janus.Bench.Instances
  ( SIMDVector (..),
    SIMDMask (..),
  )
where

import Data.Constraint (Dict (..))
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import Janus.Bench.Types
import Janus.CSE.Instances ()
import Janus.CSE.Types
  ( CSEExp,
    CSESIMDTarget (..),
    CSETargetExp (..),
    SIMDMaskWitness (..),
    SIMDVecWitness (..),
  )
import Janus.Control.Reader.Base (ReaderExp (..), SIMDMask (..), SIMDVector (..))
import Janus.Control.Reader.TH (deriveJanusReader)
import Janus.Expression.SIMD (SIMDShape)
import Janus.Fix (ExpFix (..))
import Janus.SPMD.Types (SIMDPair)
import Janus.Typed (JanusTyped)

deriveJanusReader benchReaderSpec

instance (Typeable c, CSESIMDTarget e) => CSESIMDTarget (Bench c e) where
  cseSIMDVecWitness :: forall n a. (SIMDShape n a) => SIMDVecWitness (Bench c e) n a
  cseSIMDVecWitness =
    case cseSIMDVecWitness @e @n @a of
      SIMDVecWitness p -> SIMDVecWitness p
  cseSIMDMaskWitness :: forall n a. (SIMDShape n a) => SIMDMaskWitness (Bench c e) n a
  cseSIMDMaskWitness =
    case cseSIMDMaskWitness @e @n @a of
      SIMDMaskWitness p -> SIMDMaskWitness p
  cseOptimalGangWidth = cseOptimalGangWidth @e

instance (CSETargetExp e) => CSETargetExp (Bench c e) where
  cseTargetBool = cseTargetBool @e
  cseTargetInt32 = cseTargetInt32 @e
  cseTargetWord32 = cseTargetWord32 @e
  cseTargetInt64 = cseTargetInt64 @e
  cseTargetWord64 = cseTargetWord64 @e
  cseTargetCInt = cseTargetCInt @e
  cseTargetCFileP = cseTargetCFileP @e
  cseTargetCStr = cseTargetCStr @e
  cseTargetVoidPtr = cseTargetVoidPtr @e
  cseTargetPtr :: forall a. (JanusTyped (Bench c e) a) => Dict (JanusTyped (Bench c e) (Ptr a))
  cseTargetPtr = cseTargetPtr @e @a
  cseTargetPair :: forall a b. (JanusTyped (Bench c e) a, JanusTyped (Bench c e) b) => Dict (JanusTyped (Bench c e) (SIMDPair a b))
  cseTargetPair = cseTargetPair @e @a @b
  cseWrapVoidPtr (Bench p) = Bench (cseWrapVoidPtr p)
  cseUnwrapVoidPtr (Bench p) = Bench (cseUnwrapVoidPtr p)

class BenchCSEFix (c :: (K.Type -> K.Type) -> K.Type) (e :: K.Type -> K.Type) (fn :: K.Type) | fn -> c e where
  type BenchCSEFixInner fn :: K.Type
  toBenchCSEFix :: fn -> BenchCSEFixInner fn
  fromBenchCSEFix :: BenchCSEFixInner fn -> fn

instance BenchCSEFix c e (Bench c (CSEExp e) a) where
  type BenchCSEFixInner (Bench c (CSEExp e) a) = CSEExp e a
  toBenchCSEFix (Bench (ReaderExp x)) = x
  fromBenchCSEFix x = Bench (ReaderExp x)

instance (BenchCSEFix c e fn) => BenchCSEFix c e (Bench c (CSEExp e) a -> fn) where
  type BenchCSEFixInner (Bench c (CSEExp e) a -> fn) = CSEExp e a -> BenchCSEFixInner fn
  toBenchCSEFix f = toBenchCSEFix . f . Bench . ReaderExp
  fromBenchCSEFix g (Bench (ReaderExp x)) = fromBenchCSEFix (g x)

instance
  (BenchCSEFix c e fn, ExpFix (CSEExp e) (BenchCSEFixInner fn)) =>
  ExpFix (Bench c (CSEExp e)) fn
  where
  fun _ name step =
    fromBenchCSEFix $
      fun (Proxy @(CSEExp e)) name (toBenchCSEFix . step . fromBenchCSEFix)
