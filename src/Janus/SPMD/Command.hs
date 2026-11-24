{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.SPMD.Command where

import Control.Monad (forM, forM_)
import Control.Monad.Reader (MonadReader (..), runReader, runReaderT)
import Data.Coerce (Coercible)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CFile, CInt)
import Foreign.Ptr (Ptr)
import GHC.TypeNats (KnownNat, Nat, natVal)
import qualified Janus.Command.Memory as M
import Janus.Command.Case (CmdCase (..))
import Janus.Command.Cond (CmdCond (..), whenM_)
import Janus.Command.Format (CmdFormat (..), CmdPutString (..), CmdString (..))
import Janus.Command.IO (CmdIO (..))
import Janus.Command.Range (CmdRange)
import Janus.Command.Ref (Cell (..), CmdRef (..), RefCell (..))
import Janus.Command.TicToc (CmdTicToc (..), Tic (..), Toc (..))
import Janus.Command.While (CmdWhile (..))
import Janus.Expression.Bits (ExpBits)
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd)
import Janus.Expression.SIMD
  ( CPUSIMDElem,
    CmdSIMDMem (..),
    ExpSIMDCast,
    ExpSIMDCmp (..),
    ExpSIMDMask (..),
    ExpSIMDVec (..),
    SIMDMask,
  )
import Janus.SPMD.Cast ()
import Janus.SPMD.Expression ()
import Janus.SPMD.Lane (SPMDLanesByLayout, spmdFromLanes, spmdSplat)
import Janus.SPMD.Raw
import Janus.SPMD.Typed
import Janus.SPMD.Types
import Janus.Typed (JanusTyped)

--------------------------------------------------------------------------------
-- 12. Mask-Aware Command Instances on (SPMDM n m e, SPMD n e)
--------------------------------------------------------------------------------

-- CmdRef: direct SIMD register read in readRef; masked vselect blend in writeRef
instance
  ( Monad m,
    CmdRef m e
  ) =>
  CmdRef (SPMDM n m e) (SPMD n e)
  where
  type Ref (SPMDM n m e) (SPMD n e) a = SPMDRef n m e a

  letM :: forall a. (JanusTyped (SPMD n e) a) => SPMD n e a -> SPMDM n m e (SPMD n e a)
  letM x = do
    ex <- evalSPMD x
    raw <- liftSPMD (letM (spmdMaterialize @n @e @a ex))
    pure (SPMD (pure (spmdDematerialize @n @e @a raw)))

  newRef :: forall a. (JanusTyped (SPMD n e) a) => SPMD n e a -> SPMDM n m e (SPMDRef n m e a)
  newRef x = do
    ex <- evalSPMD x
    SPMDRef <$> liftSPMD (newRef (spmdMaterialize @n @e @a ex))

  newRef' :: forall a. (JanusTyped (SPMD n e) a) => SPMDM n m e (SPMDRef n m e a)
  newRef' =
    SPMDRef <$> liftSPMD (newRef (spmdMaterialize @n @e @a (spmdDefaultExpr @n @e @a)))

  readRef :: forall a. (JanusTyped (SPMD n e) a) => SPMDRef n m e a -> SPMDM n m e (SPMD n e a)
  readRef (SPMDRef r) =
    SPMD . pure . spmdDematerialize @n @e @a <$> liftSPMD (readRef r)

  writeRef :: forall a. (JanusTyped (SPMD n e) a) => SPMDRef n m e a -> SPMD n e a -> SPMDM n m e ()
  writeRef (SPMDRef r) x = do
    msk <- ask
    ex <- evalSPMD x
    let rawNew = spmdMaterialize @n @e @a ex
    case msk of
      SPMDMaskAllTrue -> liftSPMD (writeRef r rawNew)
      SPMDMask m -> do
        rawOld <- liftSPMD (readRef r)
        liftSPMD (writeRef r (spmdSelectRaw @n @e @a m rawNew rawOld))

instance
  ( Monad m,
    CmdRef m e,
    SPMDTyped n e a
  ) =>
  RefCell (SPMDM n m e) (SPMD n e) (SPMD n e a)
  where
  newCell x = do
    r <- newRef x
    pure (Cell (readRef r) (writeRef r))
  newCell' = do
    r <- newRef'
    pure (Cell (readRef r) (writeRef r))

-- CmdCond: refines mask for then/else and skips empty coherent branches via manyOf
instance
  ( Monad m,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    JanusTyped e (SIMDMask e n (SPMDMaskElem n))
  ) =>
  CmdCond (SPMDM n m e) (SPMD n e)
  where
  ifThenElseM_ b t f = do
    cur <- ask
    cond <- evalSPMD b
    let rawThen = spmdMaskAndRaw cur cond
        rawElse = spmdMaskAndNotRaw cond cur
    thenM <- liftSPMD (letM rawThen)
    elseM <- liftSPMD (letM rawElse)
    liftSPMD $ whenM_ (manyOf thenM) $ runReaderT (getSPMDM t) (SPMDMask thenM)
    liftSPMD $ whenM_ (manyOf elseM) $ runReaderT (getSPMDM f) (SPMDMask elseM)

-- CmdWhile: loops while manyOf (outerMask & condRef) holds
instance
  ( Monad m,
    CmdWhile m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    JanusTyped e Bool,
    JanusTyped e (SIMDMask e n (SPMDMaskElem n))
  ) =>
  CmdWhile (SPMDM n m e) (SPMD n e)
  where
  whileM initCond body = do
    outer <- ask
    c0 <- evalSPMD initCond
    let rawInit = spmdMaskAndRaw outer c0
    initM <- liftSPMD (letM rawInit)
    r <- liftSPMD (newRef initM)
    let condRef = SPMDRef r
    liftSPMD $ whileM (manyOf initM) $ \scalarCont -> do
      curCond <- readRef r
      let rawLoop = spmdMaskAndRaw outer curCond
      loopM <- letM rawLoop
      runReaderT (getSPMDM (body condRef)) (SPMDMask loopM)
      nextCond <- readRef r
      let rawNext = spmdMaskAndRaw outer nextCond
      nextM <- letM rawNext
      writeRef scalarCont (manyOf nextM)

-- CmdRange: uses default rangeM so bounds [l, u) may vary independently per lane
instance
  ( Monad m,
    CmdWhile (SPMDM n m e) (SPMD n e),
    CmdRef (SPMDM n m e) (SPMD n e),
    JanusTyped (SPMD n e) Int64,
    JanusTyped (SPMD n e) Bool,
    ExpOrd (SPMD n e) Int64,
    Num (SPMD n e Int64)
  ) =>
  CmdRange (SPMDM n m e) (SPMD n e)

-- CmdCase: partitions active mask across matching tags in order, running def on remainder
instance
  ( Monad m,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpEq (SPMD n e) Int64,
    Num (SPMD n e Int64),
    JanusTyped e (SIMDMask e n (SPMDMaskElem n))
  ) =>
  CmdCase (SPMDM n m e) (SPMD n e)
  where
  switchCase_ tag cases defBranch = do
    cur <- ask
    let step remMask [] =
          case remMask of
            SPMDMaskAllTrue -> runReaderT (getSPMDM defBranch) SPMDMaskAllTrue
            SPMDMask m -> whenM_ (manyOf m) (runReaderT (getSPMDM defBranch) (SPMDMask m))
        step remMask ((k, act) : rest) = do
          let matchRaw = runReader (getSPMD (tag `eq` fromIntegral k)) remMask
              rawHit = spmdMaskAndRaw remMask matchRaw
              rawMiss = spmdMaskAndNotRaw matchRaw remMask
          hitM <- letM rawHit
          missM <- letM rawMiss
          whenM_ (manyOf hitM) (runReaderT (getSPMDM act) (SPMDMask hitM))
          step (SPMDMask missM) rest
    liftSPMD (step cur cases)

-- CmdMem: masked per-lane heap allocations (enables per-lane newTensor / freeTensor)
instance
  ( KnownNat n,
    Monad m,
    M.CmdMem m e,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    JanusTyped e Word8,
    JanusTyped e VoidPtr,
    PtrTreeOps n e VoidPtr,
    ExpPair e,
    ExpPtrCast e,
    M.ExpPtr e Word8,
    rl ~ SPMDRawLayout n Int64,
    SPMDRawVec rl n e Int64,
    Num (e Int64)
  ) =>
  M.CmdMem (SPMDM n m e) (SPMD n e)
  where
  malloc sz = do
    msk <- ask
    szE <- evalSPMD sz
    preds <- liftSPMD (laneActivePredsM msk)
    let szs = int64ToLanes szE
        nullVP = mkVoidPtr (toVoidPtr (M.nullPtr @e @Word8))
    vps <- liftSPMD $ forM (zip preds szs) $ \(act, s) -> do
      r <- newRef nullVP
      whenM_ act $ do
        p <- M.malloc s
        writeRef r (mkVoidPtr p)
      readRef r
    pure (SPMD (pure (VoidPtrVarying (ptrTreeFromList @n vps))))

  calloc cnt sz = do
    msk <- ask
    cntE <- evalSPMD cnt
    szE <- evalSPMD sz
    preds <- liftSPMD (laneActivePredsM msk)
    let cnts = int64ToLanes cntE
        szs = int64ToLanes szE
        nullVP = mkVoidPtr (toVoidPtr (M.nullPtr @e @Word8))
    vps <- liftSPMD $ forM (zip3 preds cnts szs) $ \(act, c, s) -> do
      r <- newRef nullVP
      whenM_ act $ do
        p <- M.calloc c s
        writeRef r (mkVoidPtr p)
      readRef r
    pure (SPMD (pure (VoidPtrVarying (ptrTreeFromList @n vps))))

  realloc vp sz = do
    msk <- ask
    vpE <- evalSPMD vp
    szE <- evalSPMD sz
    preds <- liftSPMD (laneActivePredsM msk)
    let curVPs = ptrTreeToList @n (materializeVoidPtr vpE)
        szs = int64ToLanes szE
    newVPs <- liftSPMD $ forM (zip3 preds curVPs szs) $ \(act, oldVP, s) -> do
      r <- newRef oldVP
      whenM_ act $ do
        p <- M.realloc (unVoidPtr oldVP) s
        writeRef r (mkVoidPtr p)
      readRef r
    pure (SPMD (pure (VoidPtrVarying (ptrTreeFromList @n newVPs))))

  free vp = do
    msk <- ask
    vpE <- evalSPMD vp
    preds <- liftSPMD (laneActivePredsM msk)
    let curVPs = ptrTreeToList @n (materializeVoidPtr vpE)
    liftSPMD $ forM_ (zip preds curVPs) $ \(act, p) ->
      whenM_ act (M.free (unVoidPtr p))

-- CmdStorable: aligned/unaligned/masked contiguous SIMD loads & stores + masked per-lane fallback
class
  (Monad m, SPMDTyped n e a) =>
  SPMDStorableByLayout (l :: Layout) (n :: Nat) m e a
  where
  spmdHasContigSIMD :: Bool
  spmdHasContigSIMD = True
  spmdContigLoadL :: Bool -> SPMDMask n e -> e (Ptr a) -> m (SPMDExprByLayout l n e a)
  spmdContigStoreL :: Bool -> SPMDMask n e -> e (Ptr a) -> SPMDExprByLayout l n e a -> m ()
  spmdScalarZeroL :: e a
  spmdPackScalarsL :: [e a] -> SPMDExprByLayout l n e a
  spmdUnpackScalarsL :: SPMDExprByLayout l n e a -> [e a]
  spmdBroadcastScalarL :: e a -> SPMDExprByLayout l n e a

contigLoadRaw ::
  forall rl n m e a.
  (SPMDRawVec rl n e a) =>
  Proxy rl ->
  (Bool -> SPMDMask n e -> e (Ptr a) -> m (e (SPMDRepByLayout rl n e a))) ->
  Bool ->
  SPMDMask n e ->
  e (Ptr a) ->
  m (e (SPMDRepByLayout rl n e a))
contigLoadRaw _ f = f

class (Monad m, SPMDRawVec rl n e a) => SPMDRawMem (rl :: Layout) (n :: Nat) m e a where
  rawContigLoad :: Bool -> SPMDMask n e -> e (Ptr a) -> m (e (SPMDRepByLayout rl n e a))
  rawContigStore :: Bool -> SPMDMask n e -> e (Ptr a) -> e (SPMDRepByLayout rl n e a) -> m ()

instance
  ( me ~ SPMDMaskElem n,
    Monad m,
    SPMDRawVec 'LDirect n e a,
    CmdSIMDMem m e n a,
    ExpSIMDCmp e n a,
    ExpSIMDVec e n a,
    ExpSIMDCmp e n me,
    ExpSIMDVec e n me,
    ExpSIMDCast e,
    CPUSIMDElem a,
    CPUSIMDElem me,
    Num (e a),
    Num (e me)
  ) =>
  SPMDRawMem 'LDirect n m e a
  where
  rawContigLoad aligned msk p = case msk of
    SPMDMaskAllTrue -> if aligned then vloadAligned p else vload p
    SPMDMask m -> vloadMasked (maskFromCanonical @n @e @a m) vzero p
  rawContigStore aligned msk p v = case msk of
    SPMDMaskAllTrue -> if aligned then vstoreAligned p v else vstore p v
    SPMDMask m -> vstoreMasked (maskFromCanonical @n @e @a m) p v

instance
  ( rlH ~ SPMDRawLayout h a,
    Monad m,
    KnownNat h,
    SPMDRawVec ('LSplit h) n e a,
    SPMDRawMem rlH h m e a,
    SPMDMaskSplit n h e,
    ExpPair e,
    M.ExpPtr e a,
    Num (e Int64)
  ) =>
  SPMDRawMem ('LSplit h) n m e a
  where
  rawContigLoad aligned msk p = do
    let (mLo, mHi) = splitExecMask @n @h msk
        hOff = fromIntegral (natVal (Proxy @h)) :: Int64
    lo <- rawContigLoad @rlH @h @m @e @a aligned mLo p
    hi <- rawContigLoad @rlH @h @m @e @a aligned mHi (M.ptrIndex p (fromIntegral hOff))
    pure (mkPair lo hi)
  rawContigStore aligned msk p v = do
    let (mLo, mHi) = splitExecMask @n @h msk
        hOff = fromIntegral (natVal (Proxy @h)) :: Int64
    rawContigStore @rlH @h @m @e @a aligned mLo p (fstPair v)
    rawContigStore @rlH @h @m @e @a aligned mHi (M.ptrIndex p (fromIntegral hOff)) (sndPair v)

instance
  ( Monad m,
    SPMDRawVec 'LPad4 2 e a,
    M.CmdStorable m e a,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e 2 Int64,
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    JanusTyped e a,
    Num (e Int64),
    Num (e a)
  ) =>
  SPMDRawMem 'LPad4 2 m e a
  where
  rawContigLoad _ msk p = do
    preds <- laneActivePredsM msk
    xs <- forM (zip [0 :: Int64, 1] preds) $ \(i, act) -> do
      r <- newRef 0
      whenM_ act (M.peekElemOff p (fromIntegral i) >>= writeRef r)
      readRef r
    pure (rawFromLanes @'LPad4 @2 @e @a xs)
  rawContigStore _ msk p v = do
    preds <- laneActivePredsM msk
    let xs = rawToLanes @'LPad4 @2 @e @a v
    forM_ (zip3 [0 :: Int64, 1] preds xs) $ \(i, act, x) ->
      whenM_ act (M.pokeElemOff p x (fromIntegral i))

instance
  ( Monad m,
    SPMDTyped n e a,
    SPMDLayout n a ~ 'LDirect,
    SPMDRawMem 'LDirect n m e a,
    Num (e a)
  ) =>
  SPMDStorableByLayout 'LDirect n m e a
  where
  spmdContigLoadL = contigLoadRaw (Proxy @'LDirect) (rawContigLoad @'LDirect @n @m @e @a)
  spmdContigStoreL = rawContigStore @'LDirect @n @m @e @a
  spmdScalarZeroL = 0
  spmdPackScalarsL = rawFromLanes @'LDirect @n @e @a
  spmdUnpackScalarsL = rawToLanes @'LDirect @n @e @a
  spmdBroadcastScalarL = rawSplat @'LDirect @n @e @a

instance
  ( Monad m,
    SPMDTyped n e a,
    SPMDLayout n a ~ ('LSplit h),
    SPMDRawMem ('LSplit h) n m e a,
    Num (e a)
  ) =>
  SPMDStorableByLayout ('LSplit h) n m e a
  where
  spmdContigLoadL = rawContigLoad @('LSplit h) @n @m @e @a
  spmdContigStoreL = rawContigStore @('LSplit h) @n @m @e @a
  spmdScalarZeroL = 0
  spmdPackScalarsL = rawFromLanes @('LSplit h) @n @e @a
  spmdUnpackScalarsL = rawToLanes @('LSplit h) @n @e @a
  spmdBroadcastScalarL = rawSplat @('LSplit h) @n @e @a

instance
  ( Monad m,
    SPMDTyped 2 e a,
    SPMDLayout 2 a ~ 'LPad4,
    SPMDRawMem 'LPad4 2 m e a,
    Num (e a)
  ) =>
  SPMDStorableByLayout 'LPad4 2 m e a
  where
  spmdContigLoadL = rawContigLoad @'LPad4 @2 @m @e @a
  spmdContigStoreL = rawContigStore @'LPad4 @2 @m @e @a
  spmdScalarZeroL = 0
  spmdPackScalarsL = rawFromLanes @'LPad4 @2 @e @a
  spmdUnpackScalarsL = rawToLanes @'LPad4 @2 @e @a
  spmdBroadcastScalarL = rawSplat @'LPad4 @2 @e @a

instance
  ( KnownNat n,
    Monad m,
    rl ~ SPMDRawLayout n Int64,
    SPMDTyped n e Int64,
    SPMDRawMem rl n m e Int64,
    SPMDRawNum rl n e Int64,
    Num (e Int64)
  ) =>
  SPMDStorableByLayout ('LInt64 rl) n m e Int64
  where
  spmdContigLoadL al msk p = I64Varying <$> rawContigLoad @rl @n @m @e @Int64 al msk p
  spmdContigStoreL al msk p expr = rawContigStore @rl @n @m @e @Int64 al msk p (materializeInt64 expr)
  spmdScalarZeroL = 0
  spmdPackScalarsL xs = I64Varying (rawFromLanes @rl @n @e @Int64 xs)
  spmdUnpackScalarsL = int64ToLanes
  spmdBroadcastScalarL u = I64Uniform False u

instance
  ( Monad m,
    lb ~ SPMDLayout n base,
    SPMDTyped n e a,
    SPMDStorableByLayout lb n m e base,
    Coercible a base,
    ExpPair e,
    ExpPtrCast e,
    JanusTyped e a,
    JanusTyped e base,
    Num (e a)
  ) =>
  SPMDStorableByLayout ('LNewtype base) n m e a
  where
  spmdHasContigSIMD = spmdHasContigSIMD @lb @n @m @e @base
  spmdContigLoadL al msk p =
    spmdContigLoadL @lb @n @m @e @base al msk (fromVoidPtr (toVoidPtr p))
  spmdContigStoreL al msk p v =
    spmdContigStoreL @lb @n @m @e @base al msk (fromVoidPtr (toVoidPtr p)) v
  spmdScalarZeroL = 0
  spmdPackScalarsL xs = spmdPackScalarsL @lb @n @m @e @base (map coerceExp xs)
  spmdUnpackScalarsL v = map coerceExp (spmdUnpackScalarsL @lb @n @m @e @base v)
  spmdBroadcastScalarL x = spmdBroadcastScalarL @lb @n @m @e @base (coerceExp x)

instance
  ( KnownNat n,
    Monad m,
    SPMDTyped n e Bool,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpSIMDCmp e n (SPMDMaskElem n),
    ExpSIMDVec e n (SPMDMaskElem n),
    ExpBool e,
    ExpCond e,
    ExpBits e Word32,
    JanusTyped e (SPMDMaskElem n),
    Num (e Word32),
    Num (e (SPMDMaskElem n))
  ) =>
  SPMDStorableByLayout 'LBool n m e Bool
  where
  spmdHasContigSIMD = False
  spmdContigLoadL _ msk p = error "spmdContigLoadL: unreachable for Bool"
    where
      _ = (msk, p)
  spmdContigStoreL _ msk p v = error "spmdContigStoreL: unreachable for Bool"
    where
      _ = (msk, p, v)
  spmdScalarZeroL = false
  spmdPackScalarsL bs =
    vfromLanes [ifThenElse b 1 0 | b <- bs] `vne` (vzero @e @n @(SPMDMaskElem n))
  spmdUnpackScalarsL m = laneActivePreds @n m
  spmdBroadcastScalarL b = vsplat (ifThenElse b 1 0) `vne` (vzero @e @n @(SPMDMaskElem n))

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    M.ExpSized (SPMD n e) a,
    SPMDStorableByLayout l n m e a,
    M.CmdStorable m e a,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    JanusTyped e a,
    PtrTreeOps n e (Ptr a),
    M.ExpPtr e a,
    rlI ~ SPMDRawLayout n Int64,
    SPMDRawVec rlI n e Int64,
    Num (e Int64)
  ) =>
  M.CmdStorable (SPMDM n m e) (SPMD n e) a
  where
  peek ptr = do
    msk <- ask
    pe <- evalSPMD ptr
    case pe of
      PtrUnitStride al base | spmdHasContigSIMD @l @n @m @e @a ->
        SPMD . pure <$> liftSPMD (spmdContigLoadL @l @n @m @e @a al msk base)
      PtrUniform _ base -> liftSPMD $ do
        r <- newRef (spmdScalarZeroL @l @n @m @e @a)
        case msk of
          SPMDMaskAllTrue -> M.peek base >>= writeRef r
          SPMDMask m -> whenM_ (manyOf m) (M.peek base >>= writeRef r)
        val <- readRef r
        pure (SPMD (pure (spmdBroadcastScalarL @l @n @m @e @a val)))
      _ -> liftSPMD $ do
        preds <- laneActivePredsM msk
        let ps = ptrToLanes pe
        xs <- forM (zip preds ps) $ \(act, p) -> do
          r <- newRef (spmdScalarZeroL @l @n @m @e @a)
          whenM_ act (M.peek p >>= writeRef r)
          readRef r
        pure (SPMD (pure (spmdPackScalarsL @l @n @m @e @a xs)))

  poke ptr val = do
    msk <- ask
    pe <- evalSPMD ptr
    ve <- evalSPMD val
    case pe of
      PtrUnitStride al base | spmdHasContigSIMD @l @n @m @e @a ->
        liftSPMD (spmdContigStoreL @l @n @m @e @a al msk base ve)
      _ -> liftSPMD $ do
        preds <- laneActivePredsM msk
        let ps = ptrToLanes pe
            xs = spmdUnpackScalarsL @l @n @m @e @a ve
        forM_ (zip3 preds ps xs) $ \(act, p, x) ->
          whenM_ act (M.poke p x)

  peekElemOff ptr idx = do
    msk <- ask
    pe <- evalSPMD ptr
    ie <- evalSPMD idx
    let w = fromIntegral (natVal (Proxy @n)) :: Int64
    case (pe, ie) of
      (PtrUniform al1 base, I64UnitStride al2 off) | spmdHasContigSIMD @l @n @m @e @a ->
        SPMD . pure <$> liftSPMD (spmdContigLoadL @l @n @m @e @a (al1 && al2) msk (M.ptrIndex base off))
      (PtrUnitStride al base, I64Const c) | spmdHasContigSIMD @l @n @m @e @a ->
        SPMD . pure <$> liftSPMD (spmdContigLoadL @l @n @m @e @a (al && c `mod` w == 0) msk (M.ptrIndex base (fromIntegral c)))
      (PtrUnitStride al1 base, I64Uniform al2 off) | spmdHasContigSIMD @l @n @m @e @a ->
        SPMD . pure <$> liftSPMD (spmdContigLoadL @l @n @m @e @a (al1 && al2) msk (M.ptrIndex base off))
      (PtrUniform _ base, I64Const c) -> liftSPMD $ do
        r <- newRef (spmdScalarZeroL @l @n @m @e @a)
        case msk of
          SPMDMaskAllTrue -> M.peekElemOff base (fromIntegral c) >>= writeRef r
          SPMDMask m -> whenM_ (manyOf m) (M.peekElemOff base (fromIntegral c) >>= writeRef r)
        val <- readRef r
        pure (SPMD (pure (spmdBroadcastScalarL @l @n @m @e @a val)))
      (PtrUniform _ base, I64Uniform _ off) -> liftSPMD $ do
        r <- newRef (spmdScalarZeroL @l @n @m @e @a)
        case msk of
          SPMDMaskAllTrue -> M.peekElemOff base off >>= writeRef r
          SPMDMask m -> whenM_ (manyOf m) (M.peekElemOff base off >>= writeRef r)
        val <- readRef r
        pure (SPMD (pure (spmdBroadcastScalarL @l @n @m @e @a val)))
      _ -> liftSPMD $ do
        preds <- laneActivePredsM msk
        let ps = ptrToLanes pe
            is = int64ToLanes ie
        xs <- forM (zip3 preds ps is) $ \(act, p, off) -> do
          r <- newRef (spmdScalarZeroL @l @n @m @e @a)
          whenM_ act (M.peekElemOff p off >>= writeRef r)
          readRef r
        pure (SPMD (pure (spmdPackScalarsL @l @n @m @e @a xs)))

  pokeElemOff ptr val idx = do
    msk <- ask
    pe <- evalSPMD ptr
    ve <- evalSPMD val
    ie <- evalSPMD idx
    let w = fromIntegral (natVal (Proxy @n)) :: Int64
    case (pe, ie) of
      (PtrUniform al1 base, I64UnitStride al2 off) | spmdHasContigSIMD @l @n @m @e @a ->
        liftSPMD (spmdContigStoreL @l @n @m @e @a (al1 && al2) msk (M.ptrIndex base off) ve)
      (PtrUnitStride al base, I64Const c) | spmdHasContigSIMD @l @n @m @e @a ->
        liftSPMD (spmdContigStoreL @l @n @m @e @a (al && c `mod` w == 0) msk (M.ptrIndex base (fromIntegral c)) ve)
      (PtrUnitStride al1 base, I64Uniform al2 off) | spmdHasContigSIMD @l @n @m @e @a ->
        liftSPMD (spmdContigStoreL @l @n @m @e @a (al1 && al2) msk (M.ptrIndex base off) ve)
      _ -> liftSPMD $ do
        preds <- laneActivePredsM msk
        let ps = ptrToLanes pe
            is = int64ToLanes ie
            xs = spmdUnpackScalarsL @l @n @m @e @a ve
        forM_ (zip3 preds (zip ps is) xs) $ \(act, (p, off), x) ->
          whenM_ act (M.pokeElemOff p x off)

-- CmdRefPtr
instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    SPMDLayout n (Ptr a) ~ 'LPtr a,
    M.CmdStorable (SPMDM n m e) (SPMD n e) a,
    SPMDStorableByLayout l n m e a,
    M.CmdRefPtr m e a,
    CmdRef m e,
    JanusTyped e a,
    PtrTreeOps n e (Ptr a)
  ) =>
  M.CmdRefPtr (SPMDM n m e) (SPMD n e) a
  where
  withRefPtr ref@(SPMDRef r) f = do
    msk <- ask
    rawInit <- liftSPMD (readRef r)
    let initScalars = spmdUnpackScalarsL @l @n @m @e @a (spmdDematerialize @n @e @a rawInit)
    scalarRefs <- liftSPMD (mapM newRef initScalars)
    let withPtrs [] accPtrs = do
          let spmdPtr = SPMD (pure (PtrVarying (ptrTreeFromList @n @e @(Ptr a) (reverse accPtrs))))
          runReaderT (getSPMDM (f spmdPtr)) msk
        withPtrs (sr : srs) accPtrs =
          M.withRefPtr sr (\p -> withPtrs srs (p : accPtrs))
    res <- liftSPMD (withPtrs scalarRefs [])
    finalScalars <- liftSPMD (mapM readRef scalarRefs)
    writeRef ref (SPMD (pure (spmdPackScalarsL @l @n @m @e @a finalScalars)))
    pure res

-- CmdIO, CmdString, CmdPutString, CmdFormat, CmdTicToc
instance
  ( KnownNat n,
    Monad m,
    CmdIO m e,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    JanusTyped e CInt,
    PtrTreeOps n e (Ptr CChar),
    PtrTreeOps n e (Ptr CFile),
    M.ExpPtr e CChar,
    M.ExpPtr e CFile,
    lCInt ~ SPMDLayout n CInt,
    SPMDLanesByLayout lCInt n e CInt,
    Num (e Int64),
    Num (e CInt)
  ) =>
  CmdIO (SPMDM n m e) (SPMD n e)
  where
  stdout = SPMD (pure (PtrUniform False stdout))
  stderr = SPMD (pure (PtrUniform False stderr))
  fopen path mode = do
    msk <- ask
    pe <- evalSPMD path
    me <- evalSPMD mode
    preds <- liftSPMD (laneActivePredsM msk)
    let ps = ptrToLanes pe
        ms = ptrToLanes me
    files <- liftSPMD $ forM (zip3 preds ps ms) $ \(act, p, md) -> do
      r <- newRef (M.nullPtr @e @CFile)
      whenM_ act (fopen p md >>= writeRef r)
      readRef r
    pure (SPMD (pure (PtrVarying (ptrTreeFromList @n @e @(Ptr CFile) files))))
  fclose h = do
    msk <- ask
    he <- evalSPMD h
    preds <- liftSPMD (laneActivePredsM msk)
    let hs = ptrToLanes he
    codes <- liftSPMD $ forM (zip preds hs) $ \(act, fp) -> do
      r <- newRef (0 :: e CInt)
      whenM_ act (fclose fp >>= writeRef r)
      readRef r
    pure (spmdFromLanes @n @e @CInt codes)

instance
  (Monad m, CmdString m e) =>
  CmdString (SPMDM n m e) (SPMD n e)
  where
  withString :: forall a. String -> (SPMD n e CString -> SPMDM n m e a) -> SPMDM n m e a
  withString s f = do
    msk <- ask
    liftSPMD $ withString s $ \cs ->
      runReaderT (getSPMDM (f (SPMD (pure (PtrUniform False cs))))) msk

instance
  ( KnownNat n,
    Monad m,
    CmdPutString m e,
    CmdIO (SPMDM n m e) (SPMD n e),
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    PtrTreeOps n e (Ptr CFile),
    PtrTreeOps n e (Ptr CChar),
    M.ExpPtr e CFile,
    M.ExpPtr e CChar,
    Num (e Int64)
  ) =>
  CmdPutString (SPMDM n m e) (SPMD n e)
  where
  hputString fh cs = do
    msk <- ask
    fhe <- evalSPMD fh
    cse <- evalSPMD cs
    preds <- liftSPMD (laneActivePredsM msk)
    let hs = ptrToLanes fhe
        ss = ptrToLanes cse
    liftSPMD $ forM_ (zip3 preds hs ss) $ \(act, h, s) ->
      whenM_ act (hputString h s)

instance
  ( KnownNat n,
    l ~ SPMDLayout n a,
    Monad m,
    CmdFormat m e a,
    CmdIO (SPMDM n m e) (SPMD n e),
    SPMDStorableByLayout l n m e a,
    CmdCond m e,
    CmdRef m e,
    ExpSIMDMask e n (SPMDMaskElem n),
    ExpBool e,
    ExpBits e Word32,
    Num (e Word32),
    JanusTyped e Word32,
    PtrTreeOps n e (Ptr CFile),
    M.ExpPtr e CFile,
    Num (e Int64)
  ) =>
  CmdFormat (SPMDM n m e) (SPMD n e) a
  where
  hformat fh val term = do
    msk <- ask
    fhe <- evalSPMD fh
    ve <- evalSPMD val
    preds <- liftSPMD (laneActivePredsM msk)
    let hs = ptrToLanes fhe
        xs = spmdUnpackScalarsL @l @n @m @e @a ve
    liftSPMD $ forM_ (zip3 preds hs xs) $ \(act, h, x) ->
      whenM_ act (hformat h x term)

instance
  ( Monad m,
    CmdTicToc m e,
    l ~ SPMDLayout n Word64,
    SPMDLanesByLayout l n e Word64
  ) =>
  CmdTicToc (SPMDM n m e) (SPMD n e)
  where
  tic = do
    Tic t <- liftSPMD tic
    pure (Tic (spmdSplat @n @e @Word64 t))
  toc = do
    Toc t <- liftSPMD toc
    pure (Toc (spmdSplat @n @e @Word64 t))
