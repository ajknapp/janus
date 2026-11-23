{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.SPMD.Host where

import Data.Complex (Complex (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C
import Janus.Backend.C.CUDA.Types (CUDADejaFu (..), CUDAIO (..))
import Janus.Expression.Extract (ExpExtract (..))
import Janus.Expression.SIMD
import Janus.FFI.Arg (THParamMode (..), THParamSpec (..), natToTHType, typeRepToTHType)
import Janus.FFI.Ret (THRetEffect (..), THRetMode (..), THRetSpec (..))
import Janus.SPMD.Typed
import Janus.SPMD.Types
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------
-- 14. Host Conversion Helpers & Target-Polymorphic C Compilation Instances
--------------------------------------------------------------------------------

class (KnownNat n) => HostSPMDValue (n :: Nat) e a where
  spmdHostFromList :: [a] -> SPMD n e a
  spmdHostToList :: SPMD n e a -> [a]
  spmdRawFromList :: [a] -> SPMDRep n e a
  spmdRawToList :: SPMDRep n e a -> [a]

class HostRawSIMD (rl :: Layout) (n :: Nat) e a where
  hostRawFrom :: [a] -> SPMDRepByLayout rl n e a
  hostRawTo :: SPMDRepByLayout rl n e a -> [a]

instance
  (SIMDShape n a, HostSIMDValue (SIMDVector e n a) n a) =>
  HostRawSIMD 'LDirect n e a
  where
  hostRawFrom = simdFromList
  hostRawTo = simdToList

instance
  ( rlH ~ SPMDRawLayout h a,
    KnownNat h,
    HostRawSIMD rlH h e a
  ) =>
  HostRawSIMD ('LSplit h) n e a
  where
  hostRawFrom xs =
    let h = fromIntegral (natVal (Proxy @h)) :: Int
     in SIMDPair (hostRawFrom @rlH @h @e @a (take h xs)) (hostRawFrom @rlH @h @e @a (drop h xs))
  hostRawTo (SIMDPair lo hi) =
    hostRawTo @rlH @h @e @a lo <> hostRawTo @rlH @h @e @a hi

instance
  (SIMDShape 4 a, Num a, HostSIMDValue (SIMDVector e 4 a) 4 a) =>
  HostRawSIMD 'LPad4 2 e a
  where
  hostRawFrom [x0, x1] = simdFromList [x0, x1, 0, 0]
  hostRawFrom xs = error $ "spmdHostFromList @2: expected 2 elements, got " <> show (length xs)
  hostRawTo v = take 2 (simdToList v)

class HostSPMDByLayout (l :: Layout) (n :: Nat) e a where
  hostRawFromL :: [a] -> SPMDRepByLayout l n e a
  hostRawToL :: SPMDRepByLayout l n e a -> [a]

instance (HostRawSIMD 'LDirect n e a) => HostSPMDByLayout 'LDirect n e a where
  hostRawFromL = hostRawFrom @'LDirect @n @e @a
  hostRawToL = hostRawTo @'LDirect @n @e @a

instance (HostRawSIMD ('LSplit h) n e a) => HostSPMDByLayout ('LSplit h) n e a where
  hostRawFromL = hostRawFrom @('LSplit h) @n @e @a
  hostRawToL = hostRawTo @('LSplit h) @n @e @a

instance (HostRawSIMD 'LPad4 2 e a) => HostSPMDByLayout 'LPad4 2 e a where
  hostRawFromL = hostRawFrom @'LPad4 @2 @e @a
  hostRawToL = hostRawTo @'LPad4 @2 @e @a

instance (rl ~ SPMDRawLayout n Int64, HostRawSIMD rl n e Int64) => HostSPMDByLayout ('LInt64 rl) n e Int64 where
  hostRawFromL = hostRawFrom @rl @n @e @Int64
  hostRawToL = hostRawTo @rl @n @e @Int64

instance
  (SIMDShape n (SPMDMaskElem n), HostSIMDMask (SIMDMask e n (SPMDMaskElem n)) n (SPMDMaskElem n)) =>
  HostSPMDByLayout 'LBool n e Bool
  where
  hostRawFromL = simdMaskFromBools
  hostRawToL = simdMaskToBools

instance
  (HostSPMDValue n e b) =>
  HostSPMDByLayout ('LComplex b) n e (Complex b)
  where
  hostRawFromL zs =
    SIMDPair
      (spmdRawFromList @n @e @b [r | r :+ _ <- zs])
      (spmdRawFromList @n @e @b [im | _ :+ im <- zs])
  hostRawToL (SIMDPair rs ims) =
    zipWith (:+) (spmdRawToList @n @e @b rs) (spmdRawToList @n @e @b ims)

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    HostSPMDByLayout l n Identity a,
    SPMDTyped n Identity a
  ) =>
  HostSPMDValue n Identity a
  where
  spmdRawFromList = hostRawFromL @l @n @Identity @a
  spmdRawToList = hostRawToL @l @n @Identity @a
  spmdHostFromList xs = SPMD (pure (spmdDematerialize @n @Identity @a (Identity (spmdRawFromList @n @Identity @a xs))))
  spmdHostToList x = spmdRawToList @n @Identity @a (runIdentity (spmdMaterialize @n @Identity @a (runSPMD x)))

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    HostSPMDByLayout l n CUDAIO a,
    SPMDTyped n CUDAIO a
  ) =>
  HostSPMDValue n CUDAIO a
  where
  spmdRawFromList = hostRawFromL @l @n @CUDAIO @a
  spmdRawToList = hostRawToL @l @n @CUDAIO @a
  spmdHostFromList xs = SPMD (pure (spmdDematerialize @n @CUDAIO @a (CUDAIO (spmdRawFromList @n @CUDAIO @a xs))))
  spmdHostToList x = spmdRawToList @n @CUDAIO @a (getCUDAIO (spmdMaterialize @n @CUDAIO @a (runSPMD x)))

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    HostSPMDByLayout l n CUDADejaFu a,
    SPMDTyped n CUDADejaFu a
  ) =>
  HostSPMDValue n CUDADejaFu a
  where
  spmdRawFromList = hostRawFromL @l @n @CUDADejaFu @a
  spmdRawToList = hostRawToL @l @n @CUDADejaFu @a
  spmdHostFromList xs = SPMD (pure (spmdDematerialize @n @CUDADejaFu @a (CUDADejaFu (spmdRawFromList @n @CUDADejaFu @a xs))))
  spmdHostToList x = spmdRawToList @n @CUDADejaFu @a (getCUDADejaFu (spmdMaterialize @n @CUDADejaFu @a (runSPMD x)))

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    HostSPMDByLayout l n (JanusCT t) a
  ) =>
  HostSPMDValue n (JanusCT t) a
  where
  spmdRawFromList = hostRawFromL @l @n @(JanusCT t) @a
  spmdRawToList = hostRawToL @l @n @(JanusCT t) @a
  spmdHostFromList _ = error "spmdHostFromList (JanusCT): pass [a] directly to compiled JanusCT function or use spmdFromLanes"
  spmdHostToList _ = error "spmdHostToList (JanusCT): evaluate via withJanusC"

instance (HostSPMDValue n Identity a) => ExpExtract (SPMD n Identity) a where
  extract x = case spmdHostToList x of
    (x0 : _) -> x0
    [] -> error "extract (SPMD): empty lanes"

{-# INLINE spmdFromListProxy #-}
spmdFromListProxy ::
  forall n e a.
  (HostSPMDValue n e a) =>
  Proxy n ->
  Proxy e ->
  Proxy a ->
  [a] ->
  SPMDRep n e a
spmdFromListProxy _ _ _ = spmdRawFromList @n @e @a

{-# INLINE spmdToListProxy #-}
spmdToListProxy ::
  forall n e a.
  (HostSPMDValue n e a) =>
  Proxy n ->
  Proxy e ->
  Proxy a ->
  SPMDRep n e a ->
  [a]
spmdToListProxy _ _ _ = spmdRawToList @n @e @a

spmdProxyExps :: forall t n a. (Typeable t, KnownNat n, Typeable a) => (TH.Exp, TH.Exp, TH.Exp)
spmdProxyExps =
  let mkProxy ty = TH.SigE (TH.ConE 'Proxy) (TH.AppT (TH.ConT ''Proxy) ty)
      pN = mkProxy (natToTHType (toInteger (natVal (Proxy @n))))
      pE = mkProxy (typeRepToTHType (typeRep (Proxy @(JanusCT t))))
      pA = mkProxy (typeRepToTHType (typeRep (Proxy @a)))
   in (pN, pE, pA)

spmdTHParamSpec ::
  forall t n a.
  ( Typeable t,
    KnownNat n,
    Typeable a,
    JanusCABIParam t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  THParamSpec
spmdTHParamSpec =
  let (pN, pE, pA) = spmdProxyExps @t @n @a
      fromListCall = TH.AppE (TH.AppE (TH.AppE (TH.VarE 'spmdFromListProxy) pN) pE) pA
      repSpec = jcABIParamTHSpec (Proxy @t) (Proxy @(SPMDRep n (JanusCT t) a))
      userTy = TH.AppT TH.ListT (typeRepToTHType (typeRep (Proxy @a)))
      mode = case thParamMode repSpec of
        THParamDirect -> THParamPure (TH.AppE fromListCall)
        THParamPure conv -> THParamPure (conv . TH.AppE fromListCall)
        THParamIO withIO -> THParamIO (\k xsExp -> withIO k (TH.AppE fromListCall xsExp))
   in THParamSpec
        { thParamUserType = userTy,
          thParamRawType = thParamRawType repSpec,
          thParamMode = mode
        }

spmdTHRetSpec ::
  forall t n a.
  ( Typeable t,
    KnownNat n,
    Typeable a,
    JanusCABIRet t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  THRetEffect ->
  THRetSpec
spmdTHRetSpec eff =
  let (pN, pE, pA) = spmdProxyExps @t @n @a
      toListFn = TH.AppE (TH.AppE (TH.AppE (TH.VarE 'spmdToListProxy) pN) pE) pA
      repSpec = jcABIRetTHSpec (Proxy @t) eff (Proxy @(SPMDRep n (JanusCT t) a))
      userTy = TH.AppT TH.ListT (typeRepToTHType (typeRep (Proxy @a)))
      mode = case thRetMode repSpec of
        THRetDirect rawTy -> THRetPureMap rawTy (TH.AppE toListFn)
        THRetPureMap rawTy conv -> THRetPureMap rawTy (TH.AppE toListFn . conv)
        THRetOutPtr runWithOut ->
          THRetOutPtr $ \callWithOut -> do
            ioRepExp <- runWithOut callWithOut
            pure $ TH.AppE (TH.AppE (TH.VarE 'fmap) toListFn) ioRepExp
   in THRetSpec
        { thRetEffect = eff,
          thRetUserBaseType = userTy,
          thRetMode = mode
        }

-- Target-Polymorphic Compilation & Evaluation Support on JanusCT t / JanusCMT t

type instance JanusCHost (SPMD n (JanusCT t) a) = [a]

type instance JanusCTermEval (SPMD n (JanusCT t) a) = [a]
type instance JanusCTermRet (SPMD n (JanusCT t) a) = [a]

type instance JanusCTermEval (SPMDM n (JanusCMT t) (JanusCT t) res) = IO (JanusCHost res)
type instance JanusCTermRet (SPMDM n (JanusCMT t) (JanusCT t) res) = JanusCHost res

instance
  ( Typeable t,
    KnownNat n,
    Typeable a,
    SPMDTyped n (JanusCT t) a,
    JanusCABIParam t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  JanusCArg t (SPMD n (JanusCT t) a)
  where
  jcEmitArg k =
    let (mkP, e) = jcEmitABIParam @t @(SPMDRep n (JanusCT t) a) (Proxy @t) k
        rawArg = JanusCT (pure (RVal e))
     in (k + 1, pure <$> mkP, SPMD (pure (spmdDematerialize @n @(JanusCT t) @a rawArg)))
  jcDummyArg =
    SPMD
      ( pure
          ( spmdDematerialize @n @(JanusCT t) @a
              (jcDummyArg @t @(JanusCT t (SPMDRep n (JanusCT t) a)))
          )
      )
  jcWithHostArg _ xs prevArgs cont =
    jcWithABIArg (Proxy @t) (spmdRawFromList @n @(JanusCT t) @a xs) (\av -> cont (av : prevArgs))
  jcTHParamSpecs _ = [spmdTHParamSpec @t @n @a]

instance
  ( Typeable t,
    KnownNat n,
    Typeable a,
    SPMDTyped n (JanusCT t) a,
    JanusCABIRet t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  JanusCValRet t (SPMD n (JanusCT t) a)
  where
  jcFinishValRet pt x =
    jcFinishABIRet pt (spmdMaterialize @n @(JanusCT t) @a (runSPMD x))
  jcCallValRet pt _ fp args =
    spmdRawToList @n @(JanusCT t) @a
      <$> jcCallABIRet pt (Proxy @(SPMDRep n (JanusCT t) a)) fp args
  jcTHValRetSpec _ eff _ = spmdTHRetSpec @t @n @a eff

instance
  ( JanusCTarget t,
    Typeable t,
    KnownNat n,
    Typeable a,
    SPMDTyped n (JanusCT t) a,
    JanusCABIRet t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  JanusCParam t (SPMD n (JanusCT t) a)
  where
  jcparamWith fs name k mparams x =
    jcparamWith fs name k mparams (spmdMaterialize @n @(JanusCT t) @a (runSPMD x))
  jcTHSpec _ = ([], spmdTHRetSpec @t @n @a THRetPure)

instance
  ( JanusCTarget t,
    JanusCABIRet t (SPMDRep n (JanusCT t) a),
    HostSPMDValue n (JanusCT t) a
  ) =>
  JanusCEvaluate t (SPMD n (JanusCT t) a)
  where
  jcevalCont withArgs dlFPtr _ =
    evalPureJanusCFFI dlFPtr $ \fp ->
      withArgs
        ( \args ->
            spmdRawToList @n @(JanusCT t) @a
              <$> jcCallABIRet (Proxy @t) (Proxy @(SPMDRep n (JanusCT t) a)) fp args
        )

instance
  (JanusCTarget t, JanusCValRet t res) =>
  JanusCParam t (SPMDM n (JanusCMT t) (JanusCT t) res)
  where
  jcparamWith fs name k mparams act =
    jcparamWith fs name k mparams (runSPMDM act)
  jcTHSpec _ = ([], jcTHValRetSpec (Proxy @t) THRetIO (Proxy @res))

instance
  (JanusCTarget t, JanusCValRet t res) =>
  JanusCEvaluate t (SPMDM n (JanusCMT t) (JanusCT t) res)
  where
  jcevalCont withArgs dlFPtr act =
    jcevalCont @t @(JanusCMT t res) withArgs dlFPtr (runSPMDM act)
