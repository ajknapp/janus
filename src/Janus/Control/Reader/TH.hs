{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reusable Template Haskell derivers ('deriveJanusReader' and
-- 'deriveJanusReaderGang') for newtype monad/expression pairs backed by
-- @('ReaderT' r m, 'ReaderExp' r e)@.
module Janus.Control.Reader.TH
  ( JanusReaderSpec (..),
    deriveJanusReader,
    deriveJanusReaderGang,
  )
where

import Control.Monad.Reader (ReaderT (..))
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity (Identity)
import Data.Semilattice.Join (Join (..))
import Data.Semilattice.Lower (Lower (..))
import Data.Semilattice.Meet (Meet (..))
import Data.Semilattice.Upper (Upper (..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import GHC.Float (expm1, log1p)
import GHC.TypeNats (natVal)
import Janus.Backend.C (JanusCMT, JanusCT, JanusCTyped (..))
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..))
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import Janus.Command.Memory
  ( CmdDynamicAligned (..),
    CmdMem (..),
    CmdMemCopy (..),
    CmdRefPtr (..),
    CmdStorable (..),
    ExpPtr (..),
    ExpSized (..),
  )
import Janus.Command.Range (CmdRange (..))
import Janus.Command.Ref (Cell (..), CmdRef (..), RefCell (..))
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Control.Reader.Base
import Janus.Control.Reader.Gang ()
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Cast
  ( ExpBoolCast (..),
    ExpFloatingCast (..),
    ExpIntegralCast (..),
  )
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Extract (ExpExtract (..))
import Janus.Expression.Inject (ExpInject (..))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.MurmurHash (ExpMurmurHash (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Expression.SIMD
import Janus.Fix (ExpFix (..), JanusCFix (..), jfix)
import Janus.SPMD.Gang
  ( CmdOptimalGang (..),
    OptimalGangCaps (..),
    dispatchOptimalGang,
    liftWrapperTypedChain,
  )
import Janus.Typed (JanusTyped)
import qualified Language.Haskell.TH as TH

-- | Specification describing how a custom @Reader@-backed monad/expression/ref
-- triple wraps @('ReaderT' env m, 'ReaderExp' env e, 'ReaderRef' env m e)@.
--
-- Single-parameter classes whose last argument is @E@ or @M@ ('Functor',
-- 'Applicative', 'Monad', 'MonadFix', 'MonadIO', 'Control.Monad.Reader.MonadReader',
-- 'Janus.Expression.Bool.ExpBool', 'Janus.Expression.Cond.ExpCond',
-- 'Janus.Expression.Let.ExpLet', 'Janus.Expression.Cast.ExpPtrCast',
-- 'Janus.Data.HKD.ExpPtrWord', 'Janus.Data.HKD.ExpStructPtr',
-- 'Janus.SPMD.Types.ExpPair', 'ExpNewtype', 'Janus.Backend.C.CUDA.Types.CmdSync')
-- can be derived directly via @deriving newtype@ on the @newtype@ declarations;
-- 'deriveJanusReader' and 'deriveJanusReaderGang' derive all remaining
-- multi-parameter Janus expression, SIMD, fixpoint, memory, I/O, and control
-- typeclasses by delegating to @('ReaderT' env m, 'ReaderExp' env e)@.
data JanusReaderSpec = JanusReaderSpec
  { -- | Given @m@ and @e@ types, allocate any extra quantified type variables
    -- (e.g. @c@) and return @(envTy, mTy, eTy, refTy, extraTypeablePreds)@.
    jrsInstantiate ::
      TH.Q TH.Type ->
      TH.Q TH.Type ->
      TH.Q (TH.Type, TH.Type, TH.Type, TH.Type, [TH.Pred]),
    jrsExpCon :: TH.Name,
    jrsUnExp :: TH.Name,
    jrsRefCon :: TH.Name,
    jrsUnRef :: TH.Name,
    jrsMonadCon :: TH.Name,
    jrsUnMonad :: TH.Name,
    jrsSIMDPrefix :: String
  }

-- | Derive all multi-parameter Janus scalar, algebraic, SIMD, fixpoint, and
-- command instances for a @Reader@-backed monad/expression pair via
-- @('ReaderT' env m, 'ReaderExp' env e)@.
deriveJanusReader :: JanusReaderSpec -> TH.Q [TH.Dec]
deriveJanusReader spec = do
  let mkE = TH.conE (jrsExpCon spec)
      unE = TH.varE (jrsUnExp spec)
      unRef = TH.varE (jrsUnRef spec)
      mkM = TH.conE (jrsMonadCon spec)
      unM = TH.varE (jrsUnMonad spec)

  -- 1. Polymorphic (m, e) scalar + command + SIMD instances
  mVar <- TH.newName "m"
  eVar <- TH.newName "e"
  aVar <- TH.newName "a"
  bVar <- TH.newName "b"
  nVar <- TH.newName "n"
  n2Var <- TH.newName "n2"
  let mQ = TH.varT mVar
      eQ = TH.varT eVar
      aQ = TH.varT aVar
      bQ = TH.varT bVar
      nQ = TH.varT nVar
      n2Q = TH.varT n2Var
  (envTy, mTy, eTy, refTy, extraTypeables) <- jrsInstantiate spec mQ eQ
  let envQ = pure envTy
      mTyQ = pure mTy
      eTyQ = pure eTy
      refTyQ = pure refTy
      rExpQ = [t|ReaderExp $envQ $eQ|]
      rMonQ = [t|ReaderT $envQ $mQ|]

  -- Associated ExpSIMD data family instance
  let vecConName = TH.mkName (jrsSIMDPrefix spec <> "SIMDVector")
      unVecName = TH.mkName ("get" <> jrsSIMDPrefix spec <> "SIMDVector")
      maskConName = TH.mkName (jrsSIMDPrefix spec <> "SIMDMask")
      unMaskName = TH.mkName ("get" <> jrsSIMDPrefix spec <> "SIMDMask")
      noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
  rExpTy <- rExpQ
  let vecInstD =
        TH.NewtypeInstD
          []
          Nothing
          (TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDVector) eTy) (TH.VarT nVar)) (TH.VarT aVar))
          Nothing
          ( TH.RecC
              vecConName
              [(unVecName, noBang, TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDVector) rExpTy) (TH.VarT nVar)) (TH.VarT aVar))]
          )
          []
      maskInstD =
        TH.NewtypeInstD
          []
          Nothing
          (TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDMask) eTy) (TH.VarT nVar)) (TH.VarT aVar))
          Nothing
          ( TH.RecC
              maskConName
              [(unMaskName, noBang, TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDMask) rExpTy) (TH.VarT nVar)) (TH.VarT aVar))]
          )
          []
      expSimdDec =
        TH.InstanceD
          Nothing
          [TH.AppT (TH.ConT ''ExpSIMD) (TH.VarT eVar)]
          (TH.AppT (TH.ConT ''ExpSIMD) eTy)
          [vecInstD, maskInstD]

  -- JanusCTyped instances on SIMDVector / SIMDMask (with extraTypeables)
  let mkSIMDTypeableCxt famName =
        extraTypeables
          <> [ TH.AppT (TH.ConT ''Typeable) envTy,
               TH.AppT (TH.ConT ''Typeable) (TH.VarT eVar),
               TH.AppT (TH.ConT ''Typeable) (TH.VarT nVar),
               TH.AppT (TH.ConT ''Typeable) (TH.VarT aVar),
               TH.AppT
                 (TH.ConT ''JanusCTyped)
                 (TH.AppT (TH.AppT (TH.AppT (TH.ConT famName) (TH.VarT eVar)) (TH.VarT nVar)) (TH.VarT aVar))
             ]
      jcTypedVecDec =
        TH.StandaloneDerivD
          (Just TH.NewtypeStrategy)
          (mkSIMDTypeableCxt ''SIMDVector)
          (TH.AppT (TH.ConT ''JanusCTyped) (TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDVector) eTy) (TH.VarT nVar)) (TH.VarT aVar)))
      jcTypedMaskDec =
        TH.StandaloneDerivD
          (Just TH.NewtypeStrategy)
          (mkSIMDTypeableCxt ''SIMDMask)
          (TH.AppT (TH.ConT ''JanusCTyped) (TH.AppT (TH.AppT (TH.AppT (TH.ConT ''SIMDMask) eTy) (TH.VarT nVar)) (TH.VarT aVar)))

  let vecConE = TH.conE vecConName
      unVecE = TH.varE unVecName
      maskConE = TH.conE maskConName
      unMaskE = TH.varE unMaskName
      mkRef = TH.conE (jrsRefCon spec)
      toRVec v = [|coerceExpNewtype ($unE $v) :: $rExpQ (SIMDVector $rExpQ $nQ $aQ)|]
      fromRVec v = [|$mkE (coerceExpNewtype ($v :: $rExpQ (SIMDVector $rExpQ $nQ $aQ)))|]
      toRMask m = [|coerceExpNewtype ($unE $m) :: $rExpQ (SIMDMask $rExpQ $nQ $aQ)|]
      fromRMask m = [|$mkE (coerceExpNewtype ($m :: $rExpQ (SIMDMask $rExpQ $nQ $aQ)))|]
      toRVecOf nT aT v = [|coerceExpNewtype ($unE $v) :: $rExpQ (SIMDVector $rExpQ $nT $aT)|]
      fromRVecOf nT aT v = [|$mkE (coerceExpNewtype ($v :: $rExpQ (SIMDVector $rExpQ $nT $aT)))|]

  scalarAndCmdDecs <-
    [d|
      instance {-# OVERLAPPABLE #-} (Num ($eQ $aQ)) => Num ($eTyQ $aQ) where
        (+) = coerce ((+) @($rExpQ $aQ))
        (-) = coerce ((-) @($rExpQ $aQ))
        (*) = coerce ((*) @($rExpQ $aQ))
        negate = coerce (negate @($rExpQ $aQ))
        abs = coerce (abs @($rExpQ $aQ))
        signum = coerce (signum @($rExpQ $aQ))
        fromInteger = coerce (fromInteger @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Bounded ($eQ $aQ)) => Bounded ($eTyQ $aQ) where
        minBound = coerce (minBound @($rExpQ $aQ))
        maxBound = coerce (maxBound @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Fractional ($eQ $aQ)) => Fractional ($eTyQ $aQ) where
        (/) = coerce ((/) @($rExpQ $aQ))
        recip = coerce (recip @($rExpQ $aQ))
        fromRational = coerce (fromRational @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Floating ($eQ $aQ)) => Floating ($eTyQ $aQ) where
        pi = coerce (pi @($rExpQ $aQ))
        exp = coerce (exp @($rExpQ $aQ))
        log = coerce (log @($rExpQ $aQ))
        sqrt = coerce (sqrt @($rExpQ $aQ))
        (**) = coerce ((**) @($rExpQ $aQ))
        logBase = coerce (logBase @($rExpQ $aQ))
        sin = coerce (sin @($rExpQ $aQ))
        cos = coerce (cos @($rExpQ $aQ))
        tan = coerce (tan @($rExpQ $aQ))
        asin = coerce (asin @($rExpQ $aQ))
        acos = coerce (acos @($rExpQ $aQ))
        atan = coerce (atan @($rExpQ $aQ))
        sinh = coerce (sinh @($rExpQ $aQ))
        cosh = coerce (cosh @($rExpQ $aQ))
        tanh = coerce (tanh @($rExpQ $aQ))
        asinh = coerce (asinh @($rExpQ $aQ))
        acosh = coerce (acosh @($rExpQ $aQ))
        atanh = coerce (atanh @($rExpQ $aQ))
        log1p = coerce (log1p @($rExpQ $aQ))
        expm1 = coerce (expm1 @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (FMA ($eQ $aQ)) => FMA ($eTyQ $aQ) where
        fma = coerce (fma @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Hypot ($eQ $aQ)) => Hypot ($eTyQ $aQ) where
        hypot = coerce (hypot @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Join ($eQ $aQ)) => Join ($eTyQ $aQ) where
        (\/) = coerce ((\/) @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Meet ($eQ $aQ)) => Meet ($eTyQ $aQ) where
        (/\) = coerce ((/\) @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Lower ($eQ $aQ)) => Lower ($eTyQ $aQ) where
        lowerBound = coerce (lowerBound @($rExpQ $aQ))

      instance {-# OVERLAPPABLE #-} (Upper ($eQ $aQ)) => Upper ($eTyQ $aQ) where
        upperBound = coerce (upperBound @($rExpQ $aQ))

      instance (ExpEq $eQ $aQ) => ExpEq $eTyQ $aQ where
        eq = coerce (eq @($rExpQ) @($aQ))
        ne = coerce (ne @($rExpQ) @($aQ))

      instance (ExpOrd $eQ $aQ) => ExpOrd $eTyQ $aQ where
        lt = coerce (lt @($rExpQ) @($aQ))
        le = coerce (le @($rExpQ) @($aQ))
        gt = coerce (gt @($rExpQ) @($aQ))
        ge = coerce (ge @($rExpQ) @($aQ))

      instance (ExpBits $eQ $aQ) => ExpBits $eTyQ $aQ where
        iand = coerce (iand @($rExpQ) @($aQ))
        ior = coerce (ior @($rExpQ) @($aQ))
        ieor = coerce (ieor @($rExpQ) @($aQ))
        inot = coerce (inot @($rExpQ) @($aQ))
        lshift = coerce (lshift @($rExpQ) @($aQ))
        rshift = coerce (rshift @($rExpQ) @($aQ))

      instance (ExpBoolCast $eQ $aQ) => ExpBoolCast $eTyQ $aQ where
        fromBool = coerce (fromBool @($rExpQ) @($aQ))
        toBool = coerce (toBool @($rExpQ) @($aQ))

      instance (ExpFloatingCast $eQ $aQ $bQ) => ExpFloatingCast $eTyQ $aQ $bQ where
        toFloating = coerce (toFloating @($rExpQ) @($aQ) @($bQ))

      instance (ExpIntegralCast $eQ $aQ $bQ) => ExpIntegralCast $eTyQ $aQ $bQ where
        toIntegral = coerce (toIntegral @($rExpQ) @($aQ) @($bQ))

      instance (ExpExtract $eQ $aQ) => ExpExtract $eTyQ $aQ where
        extract = coerce (extract @($rExpQ) @($aQ))

      instance (ExpInject $eQ $aQ) => ExpInject $eTyQ $aQ where
        inject = coerce (inject @($rExpQ) @($aQ))

      instance (ExpIntegral $eQ $aQ) => ExpIntegral $eTyQ $aQ where
        quotient = coerce (quotient @($rExpQ) @($aQ))
        remainder = coerce (remainder @($rExpQ) @($aQ))

      instance (ExpComplex $eQ $aQ) => ExpComplex $eTyQ $aQ where
        rePart = coerce (rePart @($rExpQ) @($aQ))
        imPart = coerce (imPart @($rExpQ) @($aQ))

      instance (ExpMurmurHash $eQ ($eQ $aQ)) => ExpMurmurHash $eTyQ ($eTyQ $aQ) where
        murmurHashWithSalt = coerce (murmurHashWithSalt @($rExpQ) @($rExpQ $aQ))

      instance (ExpSized $eQ $aQ) => ExpSized $eTyQ $aQ where
        sizeOf = coerce (sizeOf @($rExpQ) @($aQ))
        alignOf = coerce (alignOf @($rExpQ) @($aQ))

      instance (ExpPtr $eQ $aQ) => ExpPtr $eTyQ $aQ where
        nullPtr = coerce (nullPtr @($rExpQ) @($aQ))
        ptrAdd = coerce (ptrAdd @($rExpQ) @($aQ))
        ptrIndex = coerce (ptrIndex @($rExpQ) @($aQ))

      deriving newtype instance
        (Eq (SIMDVector $eQ $nQ $aQ)) =>
        Eq (SIMDVector $eTyQ $nQ $aQ)

      deriving newtype instance
        (Ord (SIMDVector $eQ $nQ $aQ)) =>
        Ord (SIMDVector $eTyQ $nQ $aQ)

      deriving newtype instance
        (Show (SIMDVector $eQ $nQ $aQ)) =>
        Show (SIMDVector $eTyQ $nQ $aQ)

      deriving newtype instance
        (Storable (SIMDVector $eQ $nQ $aQ)) =>
        Storable (SIMDVector $eTyQ $nQ $aQ)

      deriving newtype instance
        (Eq (SIMDMask $eQ $nQ $aQ)) =>
        Eq (SIMDMask $eTyQ $nQ $aQ)

      deriving newtype instance
        (Ord (SIMDMask $eQ $nQ $aQ)) =>
        Ord (SIMDMask $eTyQ $nQ $aQ)

      deriving newtype instance
        (Show (SIMDMask $eQ $nQ $aQ)) =>
        Show (SIMDMask $eTyQ $nQ $aQ)

      deriving newtype instance
        (Storable (SIMDMask $eQ $nQ $aQ)) =>
        Storable (SIMDMask $eTyQ $nQ $aQ)

      instance
        (SIMDShape $nQ $aQ, HostSIMDValue (SIMDVector $eQ $nQ $aQ) $nQ $aQ) =>
        HostSIMDValue (SIMDVector $eTyQ $nQ $aQ) $nQ $aQ
        where
        simdFromList = $vecConE . simdFromList
        simdToList = simdToList . $unVecE

      instance
        (SIMDShape $nQ $aQ, HostSIMDMask (SIMDMask $eQ $nQ $aQ) $nQ $aQ) =>
        HostSIMDMask (SIMDMask $eTyQ $nQ $aQ) $nQ $aQ
        where
        simdMaskFromBools = $maskConE . simdMaskFromBools
        simdMaskToBools = simdMaskToBools . $unMaskE

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Num ($eQ (SIMDVector $eQ $nQ $aQ))) =>
        Num ($eTyQ (SIMDVector $eTyQ $nQ $aQ))
        where
        x + y = $(fromRVec [|$(toRVec [|x|]) + $(toRVec [|y|])|])
        x - y = $(fromRVec [|$(toRVec [|x|]) - $(toRVec [|y|])|])
        x * y = $(fromRVec [|$(toRVec [|x|]) * $(toRVec [|y|])|])
        negate x = $(fromRVec [|negate $(toRVec [|x|])|])
        abs x = $(fromRVec [|abs $(toRVec [|x|])|])
        signum x = $(fromRVec [|signum $(toRVec [|x|])|])
        fromInteger k = $(fromRVec [|fromInteger k|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Fractional ($eQ (SIMDVector $eQ $nQ $aQ))) =>
        Fractional ($eTyQ (SIMDVector $eTyQ $nQ $aQ))
        where
        x / y = $(fromRVec [|$(toRVec [|x|]) / $(toRVec [|y|])|])
        recip x = $(fromRVec [|recip $(toRVec [|x|])|])
        fromRational q = $(fromRVec [|fromRational q|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Floating ($eQ (SIMDVector $eQ $nQ $aQ))) =>
        Floating ($eTyQ (SIMDVector $eTyQ $nQ $aQ))
        where
        pi = $(fromRVec [|pi|])
        exp x = $(fromRVec [|exp $(toRVec [|x|])|])
        log x = $(fromRVec [|log $(toRVec [|x|])|])
        sqrt x = $(fromRVec [|sqrt $(toRVec [|x|])|])
        x ** y = $(fromRVec [|$(toRVec [|x|]) ** $(toRVec [|y|])|])
        logBase x y = $(fromRVec [|logBase $(toRVec [|x|]) $(toRVec [|y|])|])
        sin x = $(fromRVec [|sin $(toRVec [|x|])|])
        cos x = $(fromRVec [|cos $(toRVec [|x|])|])
        tan x = $(fromRVec [|tan $(toRVec [|x|])|])
        asin x = $(fromRVec [|asin $(toRVec [|x|])|])
        acos x = $(fromRVec [|acos $(toRVec [|x|])|])
        atan x = $(fromRVec [|atan $(toRVec [|x|])|])
        sinh x = $(fromRVec [|sinh $(toRVec [|x|])|])
        cosh x = $(fromRVec [|cosh $(toRVec [|x|])|])
        tanh x = $(fromRVec [|tanh $(toRVec [|x|])|])
        asinh x = $(fromRVec [|asinh $(toRVec [|x|])|])
        acosh x = $(fromRVec [|acosh $(toRVec [|x|])|])
        atanh x = $(fromRVec [|atanh $(toRVec [|x|])|])
        log1p x = $(fromRVec [|log1p $(toRVec [|x|])|])
        expm1 x = $(fromRVec [|expm1 $(toRVec [|x|])|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, FMA ($eQ (SIMDVector $eQ $nQ $aQ))) =>
        FMA ($eTyQ (SIMDVector $eTyQ $nQ $aQ))
        where
        fma x y z = $(fromRVec [|fma $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Hypot ($eQ (SIMDVector $eQ $nQ $aQ))) =>
        Hypot ($eTyQ (SIMDVector $eTyQ $nQ $aQ))
        where
        hypot x y = $(fromRVec [|hypot $(toRVec [|x|]) $(toRVec [|y|])|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Join ($eQ (SIMDMask $eQ $nQ $aQ))) =>
        Join ($eTyQ (SIMDMask $eTyQ $nQ $aQ))
        where
        x \/ y = $(fromRMask [|$(toRMask [|x|]) \/ $(toRMask [|y|])|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Meet ($eQ (SIMDMask $eQ $nQ $aQ))) =>
        Meet ($eTyQ (SIMDMask $eTyQ $nQ $aQ))
        where
        x /\ y = $(fromRMask [|$(toRMask [|x|]) /\ $(toRMask [|y|])|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Lower ($eQ (SIMDMask $eQ $nQ $aQ))) =>
        Lower ($eTyQ (SIMDMask $eTyQ $nQ $aQ))
        where
        lowerBound = $(fromRMask [|lowerBound|])

      instance
        {-# OVERLAPPING #-}
        (ExpNewtype $eQ, ExpSIMD $eQ, Upper ($eQ (SIMDMask $eQ $nQ $aQ))) =>
        Upper ($eTyQ (SIMDMask $eTyQ $nQ $aQ))
        where
        upperBound = $(fromRMask [|upperBound|])

      instance (ExpNewtype $eQ, ExpSIMDVec $eQ $nQ $aQ) => ExpSIMDVec $eTyQ $nQ $aQ where
        vzero = $(fromRVec [|vzero @($rExpQ) @($nQ) @($aQ)|])
        vsplat x = $(fromRVec [|vsplat ($unE x)|])
        viota = $(fromRVec [|viota @($rExpQ) @($nQ) @($aQ)|])
        vfromLanes xs = $(fromRVec [|vfromLanes (map $unE xs)|])
        vextractLane p v = $mkE (vextractLane p $(toRVec [|v|]))
        vinsertLane p x v = $(fromRVec [|vinsertLane p ($unE x) $(toRVec [|v|])|])

      instance (ExpNewtype $eQ, ExpSIMDMask $eQ $nQ $aQ) => ExpSIMDMask $eTyQ $nQ $aQ where
        mfalse = $(fromRMask [|mfalse @($rExpQ) @($nQ) @($aQ)|])
        mtrue = $(fromRMask [|mtrue @($rExpQ) @($nQ) @($aQ)|])
        mand x y = $(fromRMask [|mand $(toRMask [|x|]) $(toRMask [|y|])|])
        mor x y = $(fromRMask [|mor $(toRMask [|x|]) $(toRMask [|y|])|])
        mxor x y = $(fromRMask [|mxor $(toRMask [|x|]) $(toRMask [|y|])|])
        mandnot x y = $(fromRMask [|mandnot $(toRMask [|x|]) $(toRMask [|y|])|])
        mnot x = $(fromRMask [|mnot $(toRMask [|x|])|])
        vselect m x y = $(fromRVec [|vselect $(toRMask [|m|]) $(toRVec [|x|]) $(toRVec [|y|])|])
        mmovemask m = $mkE (mmovemask $(toRMask [|m|]))
        mpopcount m = $mkE (mpopcount $(toRMask [|m|]))
        mfirstTrue m = $mkE (mfirstTrue $(toRMask [|m|]))
        mallOf m = $mkE (mallOf $(toRMask [|m|]))
        manyOf m = $mkE (manyOf $(toRMask [|m|]))
        mnoneOf m = $mkE (mnoneOf $(toRMask [|m|]))

      instance (ExpNewtype $eQ, ExpSIMDCmp $eQ $nQ $aQ) => ExpSIMDCmp $eTyQ $nQ $aQ where
        veq x y = $(fromRMask [|veq $(toRVec [|x|]) $(toRVec [|y|])|])
        vne x y = $(fromRMask [|vne $(toRVec [|x|]) $(toRVec [|y|])|])
        vlt x y = $(fromRMask [|vlt $(toRVec [|x|]) $(toRVec [|y|])|])
        vle x y = $(fromRMask [|vle $(toRVec [|x|]) $(toRVec [|y|])|])
        vgt x y = $(fromRMask [|vgt $(toRVec [|x|]) $(toRVec [|y|])|])
        vge x y = $(fromRMask [|vge $(toRVec [|x|]) $(toRVec [|y|])|])

      instance (ExpNewtype $eQ, ExpSIMDArith $eQ $nQ $aQ) => ExpSIMDArith $eTyQ $nQ $aQ where
        vadd x y = $(fromRVec [|vadd $(toRVec [|x|]) $(toRVec [|y|])|])
        vsub x y = $(fromRVec [|vsub $(toRVec [|x|]) $(toRVec [|y|])|])
        vmul x y = $(fromRVec [|vmul $(toRVec [|x|]) $(toRVec [|y|])|])
        vneg x = $(fromRVec [|vneg $(toRVec [|x|])|])
        vabs x = $(fromRVec [|vabs $(toRVec [|x|])|])
        vmin x y = $(fromRVec [|vmin $(toRVec [|x|]) $(toRVec [|y|])|])
        vmax x y = $(fromRVec [|vmax $(toRVec [|x|]) $(toRVec [|y|])|])

      instance (ExpNewtype $eQ, ExpSIMDSat $eQ $nQ $aQ) => ExpSIMDSat $eTyQ $nQ $aQ where
        vadds x y = $(fromRVec [|vadds $(toRVec [|x|]) $(toRVec [|y|])|])
        vsubs x y = $(fromRVec [|vsubs $(toRVec [|x|]) $(toRVec [|y|])|])
        vabss x = $(fromRVec [|vabss $(toRVec [|x|])|])
        vavg x y = $(fromRVec [|vavg $(toRVec [|x|]) $(toRVec [|y|])|])
        vabsdiff x y = $(fromRVec [|vabsdiff $(toRVec [|x|]) $(toRVec [|y|])|])

      instance (ExpNewtype $eQ, ExpSIMDFloat $eQ $nQ $aQ) => ExpSIMDFloat $eTyQ $nQ $aQ where
        vdiv x y = $(fromRVec [|vdiv $(toRVec [|x|]) $(toRVec [|y|])|])
        vsqrt x = $(fromRVec [|vsqrt $(toRVec [|x|])|])
        vrsqrtApprox x = $(fromRVec [|vrsqrtApprox $(toRVec [|x|])|])
        vrcpApprox x = $(fromRVec [|vrcpApprox $(toRVec [|x|])|])
        vfma x y z = $(fromRVec [|vfma $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])
        vfms x y z = $(fromRVec [|vfms $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])
        vfnma x y z = $(fromRVec [|vfnma $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])
        vfnms x y z = $(fromRVec [|vfnms $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])
        vmulAdd x y z = $(fromRVec [|vmulAdd $(toRVec [|x|]) $(toRVec [|y|]) $(toRVec [|z|])|])
        vfloor x = $(fromRVec [|vfloor $(toRVec [|x|])|])
        vceil x = $(fromRVec [|vceil $(toRVec [|x|])|])
        vtrunc x = $(fromRVec [|vtrunc $(toRVec [|x|])|])
        vroundNearest x = $(fromRVec [|vroundNearest $(toRVec [|x|])|])

      instance (ExpNewtype $eQ, ExpSIMDBits $eQ $nQ $aQ) => ExpSIMDBits $eTyQ $nQ $aQ where
        vand x y = $(fromRVec [|vand $(toRVec [|x|]) $(toRVec [|y|])|])
        vor x y = $(fromRVec [|vor $(toRVec [|x|]) $(toRVec [|y|])|])
        vxor x y = $(fromRVec [|vxor $(toRVec [|x|]) $(toRVec [|y|])|])
        vandnot x y = $(fromRVec [|vandnot $(toRVec [|x|]) $(toRVec [|y|])|])
        vnot x = $(fromRVec [|vnot $(toRVec [|x|])|])
        vshl x s = $(fromRVec [|vshl $(toRVec [|x|]) ($unE s)|])
        vshr x s = $(fromRVec [|vshr $(toRVec [|x|]) ($unE s)|])
        vshlImm p x = $(fromRVec [|vshlImm p $(toRVec [|x|])|])
        vshrImm p x = $(fromRVec [|vshrImm p $(toRVec [|x|])|])
        vshlVar x y = $(fromRVec [|vshlVar $(toRVec [|x|]) $(toRVec [|y|])|])
        vshrVar x y = $(fromRVec [|vshrVar $(toRVec [|x|]) $(toRVec [|y|])|])
        vpopcnt x = $(fromRVec [|vpopcnt $(toRVec [|x|])|])

      instance (ExpNewtype $eQ, ExpSIMDShuffle $eQ $nQ $aQ) => ExpSIMDShuffle $eTyQ $nQ $aQ where
        vshuffleLanes idxs x = $(fromRVec [|vshuffleLanes idxs $(toRVec [|x|])|])
        vpermuteVar x idxs = $(fromRVec [|vpermuteVar $(toRVec [|x|]) (coerceExpNewtype ($unE idxs))|])
        vshuffleBytes x ctrl = $(fromRVec [|vshuffleBytes $(toRVec [|x|]) (coerceExpNewtype ($unE ctrl))|])
        vzipLo x y = $(fromRVec [|vzipLo $(toRVec [|x|]) $(toRVec [|y|])|])
        vzipHi x y = $(fromRVec [|vzipHi $(toRVec [|x|]) $(toRVec [|y|])|])

      instance (ExpNewtype $eQ, ExpSIMDSplit $eQ $n2Q $nQ $aQ) => ExpSIMDSplit $eTyQ $n2Q $nQ $aQ where
        vsplit v =
          let (lo, hi) = vsplit $(toRVecOf n2Q aQ [|v|])
           in ($(fromRVec [|lo|]), $(fromRVec [|hi|]))
        vconcat lo hi = $(fromRVecOf n2Q aQ [|vconcat $(toRVec [|lo|]) $(toRVec [|hi|])|])

      instance (ExpNewtype $eQ, ExpSIMDReduce $eQ $nQ $aQ) => ExpSIMDReduce $eTyQ $nQ $aQ where
        vreduceAdd x = $mkE (vreduceAdd $(toRVec [|x|]))
        vreduceMin x = $mkE (vreduceMin $(toRVec [|x|]))
        vreduceMax x = $mkE (vreduceMax $(toRVec [|x|]))
        vreduceAnd x = $mkE (vreduceAnd $(toRVec [|x|]))
        vreduceOr x = $mkE (vreduceOr $(toRVec [|x|]))
        vreduceXor x = $mkE (vreduceXor $(toRVec [|x|]))

      instance (ExpNewtype $eQ, ExpSIMDDot $eQ) => ExpSIMDDot $eTyQ where
        vdp4aS x y acc = $mkE (vdp4aS (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vdp4aU x y acc = $mkE (vdp4aU (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vdp4aVecS x y acc = $mkE (coerceExpNewtype (vdp4aVecS (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) (coerceExpNewtype ($unE acc))))
        vdp4aVecU x y acc = $mkE (coerceExpNewtype (vdp4aVecU (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) (coerceExpNewtype ($unE acc))))
        vdp2aLoS x y acc = $mkE (vdp2aLoS (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vdp2aHiS x y acc = $mkE (vdp2aHiS (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vdp2aLoU x y acc = $mkE (vdp2aLoU (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vdp2aHiU x y acc = $mkE (vdp2aHiU (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)) ($unE acc))
        vsad8 x y = $mkE (vsad8 (coerceExpNewtype ($unE x)) (coerceExpNewtype ($unE y)))

      instance (ExpNewtype $eQ, ExpSIMDCast $eQ) => ExpSIMDCast $eTyQ where
        vbitcast x = $mkE (coerceExpNewtype (vbitcast (coerceExpNewtype ($unE x))))
        vcvt x = $mkE (coerceExpNewtype (vcvt (coerceExpNewtype ($unE x))))
        vnarrowSat lo hi = $mkE (coerceExpNewtype (vnarrowSat (coerceExpNewtype ($unE lo)) (coerceExpNewtype ($unE hi))))

      instance (ExpNewtype $eQ, CmdSIMDMem $mQ $eQ $nQ $aQ) => CmdSIMDMem $mTyQ $eTyQ $nQ $aQ where
        vload p = $mkM ((\v -> $(fromRVec [|v|])) <$> vload ($unE p))
        vloadAligned p = $mkM ((\v -> $(fromRVec [|v|])) <$> vloadAligned ($unE p))
        vstore p v = $mkM (vstore ($unE p) $(toRVec [|v|]))
        vstoreAligned p v = $mkM (vstoreAligned ($unE p) $(toRVec [|v|]))
        vloadMasked m fb p =
          $mkM ((\v -> $(fromRVec [|v|])) <$> vloadMasked $(toRMask [|m|]) $(toRVec [|fb|]) ($unE p))
        vstoreMasked m p v =
          $mkM (vstoreMasked $(toRMask [|m|]) ($unE p) $(toRVec [|v|]))
        vgather p idxs =
          $mkM ((\v -> $(fromRVec [|v|])) <$> vgather ($unE p) (coerceExpNewtype ($unE idxs)))
        vgatherMasked m fb p idxs =
          $mkM ((\v -> $(fromRVec [|v|])) <$> vgatherMasked $(toRMask [|m|]) $(toRVec [|fb|]) ($unE p) (coerceExpNewtype ($unE idxs)))

      instance (CmdRef $mQ $eQ) => CmdRef $mTyQ $eTyQ where
        type Ref $mTyQ $eTyQ x = $refTyQ x
        letM x = $mkM ($mkE <$> letM ($unE x))
        newRef x = $mkM ($mkRef <$> newRef ($unE x))
        newRef' = $mkM ($mkRef <$> newRef')
        readRef ref = $mkM ($mkE <$> readRef ($unRef ref))
        writeRef ref x = $mkM (writeRef ($unRef ref) ($unE x))

      instance (CmdRef $mQ $eQ, JanusTyped $eQ $aQ) => RefCell $mTyQ $eTyQ ($eTyQ $aQ) where
        newCell x = do
          ref <- newRef x
          pure (Cell (readRef ref) (writeRef ref))
        newCell' = do
          ref <- newRef'
          pure (Cell (readRef ref) (writeRef ref))

      instance (CmdCond $mQ $eQ) => CmdCond $mTyQ $eTyQ where
        ifThenElseM_ = coerce (ifThenElseM_ @($rMonQ) @($rExpQ))

      instance (CmdWhile $mQ $eQ) => CmdWhile $mTyQ $eTyQ where
        whileM = coerce (whileM @($rMonQ) @($rExpQ))

      instance (CmdRange $mQ $eQ) => CmdRange $mTyQ $eTyQ where
        rangeM = coerce (rangeM @($rMonQ) @($rExpQ))

      instance (CmdCase $mQ $eQ) => CmdCase $mTyQ $eTyQ where
        switchCase_ tag cases def =
          $mkM (switchCase_ ($unE tag) [(k, $unM act) | (k, act) <- cases] ($unM def))

      instance (Monad $mQ, CmdMem $mQ $eQ) => CmdMem $mTyQ $eTyQ where
        malloc sz = $mkM ($mkE <$> malloc ($unE sz))
        calloc n sz = $mkM ($mkE <$> calloc ($unE n) ($unE sz))
        realloc p sz = $mkM ($mkE <$> realloc ($unE p) ($unE sz))
        free p = $mkM (free ($unE p))

      instance (Monad $mQ, CmdDynamicAligned $mQ $eQ) => CmdDynamicAligned $mTyQ $eTyQ where
        alignedMalloc sz al = $mkM ($mkE <$> alignedMalloc ($unE sz) ($unE al))

      instance (Monad $mQ, CmdMemCopy $mQ $eQ) => CmdMemCopy $mTyQ $eTyQ where
        cmdMemcpy = coerce (cmdMemcpy @($rMonQ) @($rExpQ))
        cmdMemmove = coerce (cmdMemmove @($rMonQ) @($rExpQ))
        cmdMemset = coerce (cmdMemset @($rMonQ) @($rExpQ))

      instance (Monad $mQ, CmdStorable $mQ $eQ $aQ) => CmdStorable $mTyQ $eTyQ $aQ where
        peek p = $mkM ($mkE <$> peek ($unE p))
        poke p x = $mkM (poke ($unE p) ($unE x))
        peekElemOff p i = $mkM ($mkE <$> peekElemOff ($unE p) ($unE i))
        pokeElemOff p x i = $mkM (pokeElemOff ($unE p) ($unE x) ($unE i))

      instance (Monad $mQ, CmdRefPtr $mQ $eQ $aQ) => CmdRefPtr $mTyQ $eTyQ $aQ where
        withRefPtr ref f = $mkM (withRefPtr ($unRef ref) ($unM . f . $mkE))

      instance (Monad $mQ, CmdIO $mQ $eQ) => CmdIO $mTyQ $eTyQ where
        stdout = $mkE stdout
        stderr = $mkE stderr
        fopen path mode = $mkM ($mkE <$> fopen ($unE path) ($unE mode))
        fclose h = $mkM ($mkE <$> fclose ($unE h))

      instance (CmdString $mQ $eQ) => CmdString $mTyQ $eTyQ where
        withString s f = $mkM (withString s ($unM . f . $mkE))

      instance (Monad $mQ, CmdPutString $mQ $eQ) => CmdPutString $mTyQ $eTyQ where
        hputString = coerce (hputString @($rMonQ) @($rExpQ))

      instance (Monad $mQ, CmdFormat $mQ $eQ $aQ) => CmdFormat $mTyQ $eTyQ $aQ where
        hformat = coerce (hformat @($rMonQ) @($rExpQ) @($aQ))

      instance (Monad $mQ, CmdTicToc $mQ $eQ) => CmdTicToc $mTyQ $eTyQ where
        tic = $mkM ((\(Tic t) -> Tic ($mkE t)) <$> tic)
        toc = $mkM ((\(Toc t) -> Toc ($mkE t)) <$> toc)
      |]

  -- 2. ExpFix / JanusCFix instances for Identity and JanusCT t
  fnVar <- TH.newName "fn"
  tVar <- TH.newName "t"
  let fnQ = TH.varT fnVar
      tQ = TH.varT tVar

  (_, _, eIdTy, _, _) <- jrsInstantiate spec [t|IO|] [t|Identity|]
  (envCTTy, _, eCTTy, _, ctExtraTypeables) <- jrsInstantiate spec [t|JanusCMT $tQ|] [t|JanusCT $tQ|]
  let eIdTyQ = pure eIdTy
      eCTTyQ = pure eCTTy
      envCTTyQ = pure envCTTy
      ctCxtBasePreds =
        ctExtraTypeables
          <> [ TH.AppT (TH.ConT ''Typeable) (TH.VarT tVar),
               TH.AppT (TH.ConT ''JanusCTyped) (TH.VarT aVar),
               TH.AppT (TH.ConT ''Typeable) envCTTy
             ]
      ctCxtArrPreds =
        ctCxtBasePreds <> [TH.AppT (TH.ConT ''JanusCFix) (TH.VarT fnVar)]

  fixQuotes <-
    [d|
      instance ExpFix $eIdTyQ $fnQ where
        fun _ _ = fix
        {-# INLINE fun #-}

      instance JanusCFix ($eCTTyQ $aQ) where
        jcfun name args params n body =
          $mkE (jcfun name args params n ($unE body :: ReaderExp $envCTTyQ (JanusCT $tQ) $aQ))
        jcfake name args body =
          $mkE (jcfake name args ($unE body :: ReaderExp $envCTTyQ (JanusCT $tQ) $aQ))

      instance JanusCFix ($eCTTyQ $aQ -> $fnQ) where
        jcfun name args params n body x =
          jcfun name args params n (body . $mkE) ($unE x :: ReaderExp $envCTTyQ (JanusCT $tQ) $aQ)
        jcfake name args f x =
          jcfake name args (f . $mkE) ($unE x :: ReaderExp $envCTTyQ (JanusCT $tQ) $aQ)

      instance (JanusCFix $fnQ) => ExpFix $eCTTyQ $fnQ where
        fun _ = jfix
      |]
  let fixDecs = case fixQuotes of
        [dId, TH.InstanceD ov1 _ hd1 b1, TH.InstanceD ov2 _ hd2 b2, dCT] ->
          [ dId,
            TH.InstanceD ov1 ctCxtBasePreds hd1 b1,
            TH.InstanceD ov2 ctCxtArrPreds hd2 b2,
            dCT
          ]
        other -> other

  pure ([expSimdDec, jcTypedVecDec, jcTypedMaskDec] <> scalarAndCmdDecs <> fixDecs)

-- | Derive 'CmdOptimalGang' for a @Reader@-backed monad/expression pair via
-- @('ReaderT' env m, 'ReaderExp' env e)@. Place in a small @{-# OPTIONS_GHC -O0 #-}@
-- module to keep compilation fast.
deriveJanusReaderGang :: JanusReaderSpec -> TH.Q [TH.Dec]
deriveJanusReaderGang spec = do
  let mkM = TH.conE (jrsMonadCon spec)
      unM = TH.varE (jrsUnMonad spec)
  mVar <- TH.newName "m"
  eVar <- TH.newName "e"
  let mQ = TH.varT mVar
      eQ = TH.varT eVar
  (_, mTy, eTy, _, extraTypeables) <- jrsInstantiate spec mQ eQ
  let mTyQ = pure mTy
      eTyQ = pure eTy
  basePred <- [t|CmdOptimalGang $mQ $eQ|]
  decs <-
    [d|
      instance CmdOptimalGang $mTyQ $eTyQ where
        optimalGangCaps =
          case optimalGangCaps @($mQ) @($eQ) of
            OptimalGangCaps ch ->
              OptimalGangCaps (liftWrapperTypedChain @($eTyQ) @($eQ) ch)
        withOptimalGang pa k =
          $mkM $
            ReaderT $ \env ->
              withOptimalGang @($mQ) @($eQ) pa $ \pn ->
                runReaderT
                  ($unM (dispatchOptimalGang (optimalGangCaps @($mTyQ) @($eTyQ)) (fromIntegral (natVal pn)) pa k))
                  env
      |]
  pure $ case decs of
    [TH.InstanceD ov _ hd body] ->
      [TH.InstanceD ov (extraTypeables <> [basePred]) hd body]
    other -> other
