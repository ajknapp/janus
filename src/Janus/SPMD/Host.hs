{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.SPMD.Host where

import Data.Complex (Complex (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.LibFFI (callFFI, retVoid)
import Foreign.Ptr (castPtrToFunPtr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Janus.Backend.C (JanusC (..), RVal (..))
import Janus.Backend.C.CUDA (CUDADejaFu (..), CUDAIO (..))
import Janus.Backend.C.LinuxX86_64
import Janus.Expression.Extract (ExpExtract (..))
import Janus.Expression.SIMD
import Janus.SPMD.Typed
import Janus.SPMD.Types
import Language.C.Quote (Exp (Var), Id (Id))

--------------------------------------------------------------------------------
-- 14. Host Conversion Helpers & Direct LinuxX86_64C Compilation Instances
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
    HostSPMDByLayout l n LinuxX86_64C a
  ) =>
  HostSPMDValue n LinuxX86_64C a
  where
  spmdRawFromList = hostRawFromL @l @n @LinuxX86_64C @a
  spmdRawToList = hostRawToL @l @n @LinuxX86_64C @a
  spmdHostFromList _ = error "spmdHostFromList (LinuxX86_64C): pass [a] directly to compiled LinuxX86_64C function or use spmdFromLanes"
  spmdHostToList _ = error "spmdHostToList (LinuxX86_64C): evaluate via withLinuxX86_64C"

instance (HostSPMDValue n Identity a) => ExpExtract (SPMD n Identity) a where
  extract x = case spmdHostToList x of
    (x0 : _) -> x0
    [] -> error "extract (SPMD): empty lanes"

-- Direct LinuxX86_64C Compilation & Evaluation Support
type instance LinuxX86_64CEval (SPMD n LinuxX86_64C a -> r) = [a] -> LinuxX86_64CEval r
type instance LinuxX86_64CEval (SPMD n LinuxX86_64C a) = [a]
type instance LinuxX86_64CEval (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a)) = IO [a]
type instance LinuxX86_64CEval (SPMDM n LinuxX86_64CM LinuxX86_64C ()) = IO ()

type instance LinuxX86_64BaseRet (SPMD n LinuxX86_64C a) = [a]
type instance LinuxX86_64BaseRet (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a)) = [a]
type instance LinuxX86_64BaseRet (SPMDM n LinuxX86_64CM LinuxX86_64C ()) = ()
type instance LinuxX86_64BaseRet (SPMD n LinuxX86_64C _ -> r) = LinuxX86_64BaseRet r

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  LinuxX86_64Param (SPMD n LinuxX86_64C a)
  where
  x86param fs name k mparams x =
    x86param fs name k mparams (spmdMaterialize @n @LinuxX86_64C @a (runSPMD x))

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  LinuxX86_64Evaluate (SPMD n LinuxX86_64C a)
  where
  x86eval dlFPtr _ args _ =
    evalPureLinuxX86_64FFI dlFPtr $ \fp ->
      spmdRawToList @n @LinuxX86_64C @a <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C a)) fp args

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  LinuxX86_64Param (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a))
  where
  x86param fs name k mparams act =
    let lowered :: LinuxX86_64CM (LinuxX86_64C (SPMDRep n LinuxX86_64C a))
        lowered = do
          res <- runSPMDM act
          pure (spmdMaterialize @n @LinuxX86_64C @a (runSPMD res))
     in x86param fs name k mparams lowered

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  LinuxX86_64Evaluate (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a))
  where
  x86eval dlFPtr _ args _ =
    withForeignPtr dlFPtr $ \p ->
      spmdRawToList @n @LinuxX86_64C @a <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C a)) (castPtrToFunPtr p) args

instance LinuxX86_64Param (SPMDM n LinuxX86_64CM LinuxX86_64C ()) where
  x86param fs name k mparams act = x86param fs name k mparams (runSPMDM act)

instance LinuxX86_64Evaluate (SPMDM n LinuxX86_64CM LinuxX86_64C ()) where
  x86eval dlFPtr _ args _ =
    withForeignPtr dlFPtr $ \p -> callFFI (castPtrToFunPtr p) retVoid (reverse args)

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C a),
    LinuxX86_64Param r
  ) =>
  LinuxX86_64Param (SPMD n LinuxX86_64C a -> r)
  where
  x86param fs name k mparams f =
    let wrapped :: LinuxX86_64C (SPMDRep n LinuxX86_64C a) -> r
        wrapped rawArg = f (SPMD (pure (spmdDematerialize @n @LinuxX86_64C @a rawArg)))
     in x86param fs name k mparams wrapped

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a,
    LinuxX86_64Evaluate r,
    X86DeferCall r
  ) =>
  LinuxX86_64Evaluate (SPMD n LinuxX86_64C a -> r)
  where
  x86eval dlFPtr _ args f xs =
    let dummyRaw = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
        dummy = SPMD (pure (spmdDematerialize @n @LinuxX86_64C @a dummyRaw))
     in x86DeferArg (spmdRawFromList @n @LinuxX86_64C @a xs) dlFPtr args (f dummy)

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C b),
    HostSPMDValue n LinuxX86_64C b
  ) =>
  X86DeferCall (SPMD n LinuxX86_64C b)
  where
  x86DeferArg x dlFPtr args _ =
    evalPureLinuxX86_64FFI dlFPtr $ \fp ->
      x86WithArg x $ \a ->
        spmdRawToList @n @LinuxX86_64C @b <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C b)) fp (a : args)

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C b),
    HostSPMDValue n LinuxX86_64C b
  ) =>
  X86DeferCall (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C b))
  where
  x86DeferArg x dlFPtr args _ =
    withForeignPtr dlFPtr $ \p ->
      x86WithArg x $ \a ->
        spmdRawToList @n @LinuxX86_64C @b <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C b)) (castPtrToFunPtr p) (a : args)

instance X86DeferCall (SPMDM n LinuxX86_64CM LinuxX86_64C ()) where
  x86DeferArg x dlFPtr args _ =
    withForeignPtr dlFPtr $ \p ->
      x86WithArg x $ \a ->
        callFFI (castPtrToFunPtr p) retVoid (reverse (a : args))

instance
  ( SPMDTyped n LinuxX86_64C b,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C b),
    HostSPMDValue n LinuxX86_64C b,
    X86DeferCall r,
    X86DeferCont r,
    LinuxX86_64Evaluate r
  ) =>
  X86DeferCall (SPMD n LinuxX86_64C b -> r)
  where
  x86DeferArg x dlFPtr args f ys =
    let dummyRaw = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
        dummy = SPMD (pure (spmdDematerialize @n @LinuxX86_64C @b dummyRaw))
        yRaw = spmdRawFromList @n @LinuxX86_64C @b ys
     in x86DeferCont (\k -> x86WithArg x $ \ax -> x86WithArg yRaw $ \ay -> k (ay : ax : args)) dlFPtr (f dummy)

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  X86DeferCont (SPMD n LinuxX86_64C a)
  where
  x86DeferCont withArgs dlFPtr _ =
    evalPureLinuxX86_64FFI dlFPtr $ \fp ->
      withArgs (\args -> spmdRawToList @n @LinuxX86_64C @a <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C a)) fp args)

instance
  ( LinuxX86_64ABIRet (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a
  ) =>
  X86DeferCont (SPMDM n LinuxX86_64CM LinuxX86_64C (SPMD n LinuxX86_64C a))
  where
  x86DeferCont withArgs dlFPtr _ =
    withForeignPtr dlFPtr $ \p ->
      withArgs (\args -> spmdRawToList @n @LinuxX86_64C @a <$> x86CallReturn (Proxy @(SPMDRep n LinuxX86_64C a)) (castPtrToFunPtr p) args)

instance X86DeferCont (SPMDM n LinuxX86_64CM LinuxX86_64C ()) where
  x86DeferCont withArgs dlFPtr _ =
    withForeignPtr dlFPtr $ \p ->
      withArgs (callFFI (castPtrToFunPtr p) retVoid . reverse)

instance
  ( SPMDTyped n LinuxX86_64C a,
    LinuxX86_64ABIParam (SPMDRep n LinuxX86_64C a),
    HostSPMDValue n LinuxX86_64C a,
    X86DeferCont r
  ) =>
  X86DeferCont (SPMD n LinuxX86_64C a -> r)
  where
  x86DeferCont withArgs dlFPtr f xs =
    let dummyRaw = LinuxX86_64C $ JanusC $ pure $ RVal $ Var (Id "this_is_a_bug_if_you_see_this" noLoc) noLoc
        dummy = SPMD (pure (spmdDematerialize @n @LinuxX86_64C @a dummyRaw))
        xRaw = spmdRawFromList @n @LinuxX86_64C @a xs
     in x86DeferCont (\k -> withArgs $ \as -> x86WithArg xRaw $ \av -> k (av : as)) dlFPtr (f dummy)
