{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Constraints where

import Control.Lens ((&), (.~))
import Control.Monad.Reader (ReaderT (..))
import Data.Complex (Complex)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C (CBool, CChar, CDouble, CFloat, CInt, CSize, withCString)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, nullPtr)
import qualified Foreign.Storable as Storable
import GHC.Generics (Generic)
import Janus
import Janus.Alloc hiding (CmdJanus, CmdJanusIO, ExpJanus)
import Janus.Backend.C (JanusCCSE, JanusCMCSE, PortableC, withJanusC)
import Janus.Backend.C.Build (CBuildConfig, cBuildConfigCacheDir, defaultCBuildConfig)
import Janus.Backend.C.CUDA
  ( CUDAIO (..),
    CUDAIOM,
    JanusCUDACSE,
    JanusCUDAMCSE,
    launchCUDAIOM,
    showJanusCUDA,
  )
import Janus.Backend.C.CUDA.Foreign (CUlaunchConfig (..), CUstream (..))
import Janus.Backend.C.LinuxX86_64 (LinuxX86_64, LinuxX86_64CCSE, LinuxX86_64CMCSE)
import Janus.CSE (CSEExp, cse, withCSE)
import Janus.Command.Cond (whenM_)
import Janus.Command.Format (hformat, hputString, withString)
import Janus.Command.IO (fclose, fopen)
import qualified Janus.Command.Memory as M
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Control.Drain (drainSum, runDrain)
import Janus.Control.Reader (ReaderExp (..))
import Janus.Control.Stream (Stream, rangeS, streamM)
import Janus.Data
  ( AoS,
    CStruct,
    Elem,
    HashLayoutCtx,
    HookSpec,
    JanusHKD,
    JanusStruct,
    OrdLayoutCtx,
    RawListHook,
    SoA,
    getPtrOf,
    mkHookSpec,
    projectPtrs,
    symbolEq,
  )
import qualified Janus.Data.BTreeMap as BTM
import qualified Janus.Data.BitSet as BS
import qualified Janus.Data.BloomFilter as BF
import qualified Janus.Data.DisjointSet as DS
import qualified Janus.Data.DynVec as DV
import qualified Janus.Data.FlatSet as FS
import qualified Janus.Data.HashMap as HM
import qualified Janus.Data.HashSet as HS
import qualified Janus.Data.Interner as IN
import qualified Janus.Data.IntrusiveList as IL
import qualified Janus.Data.LruCache as LRU
import qualified Janus.Data.SmallVec as SV
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpBoolCast (..), ExpFloatingCast (..), ExpIntegralCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Inject (ExpInject (inject))
import Janus.Expression.Integral (ExpIntegral (..))
import Janus.Expression.Let (ExpLet (..))
import Janus.Expression.Math (ExpComplex (..), FMA (..), Hypot (..))
import Janus.Expression.MurmurHash (murmurHash)
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Fix (ExpFix (..))
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, (@?=))

getConstraintsTestConfig :: IO CBuildConfig
getConstraintsTestConfig = do
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "_cache" id mtmp
  pure (defaultCBuildConfig & cBuildConfigCacheDir .~ dir)

assertApprox :: String -> Double -> Double -> IO ()
assertApprox msg expected actual =
  assertBool
    (msg <> ": expected ~" <> show expected <> ", got " <> show actual)
    (abs (expected - actual) <= 1e-6)

--------------------------------------------------------------------------------
-- Custom HKD Structs Used in Multi-Container & Polymorphic Key Tests
--------------------------------------------------------------------------------

data SampleRec f = SampleRec
  { srTag :: !(f Int32),
    srWeight :: !(f Double)
  }
  deriving stock (Generic)

deriving stock instance Eq (SampleRec Identity)

deriving stock instance Show (SampleRec Identity)

instance JanusStruct SampleRec

instance JanusHKD SampleRec

data ListItem f = ListItem
  { liHook :: !(f (CStruct RawListHook)),
    liValue :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (ListItem Identity)

deriving stock instance Show (ListItem Identity)

instance JanusStruct ListItem

liHookSpec :: HookSpec ListItem
liHookSpec = mkHookSpec liHook

--------------------------------------------------------------------------------
-- 1. Pure `(ExpJanus e) =>` Kernel Across All Backends & Transformers
--------------------------------------------------------------------------------

-- | Pure expression kernel exercising Haskell integers, Foreign.C integers,
-- floats (`Float`, `Double`, `CFloat`, `CDouble`), `CBool`, `Complex Double`,
-- `fma`, `hypot`, trig, bitwise ops, `murmurHash`, `let_`, `ifThenElse`, and
-- cross-width `toIntegral`/`toFloating` casts under a SINGLE `(ExpJanus e) =>`!
purePrimKernel :: forall e. (ExpJanus e) => e Int64 -> e Double
purePrimKernel x =
  let_ (x + 5) $ \n ->
    let i8 = toIntegral n :: e Int8
        i16 = toIntegral i8 :: e Int16
        i32 = toIntegral i16 :: e Int32
        w8 = toIntegral (n `iand` 0xFF) :: e Word8
        w16 = toIntegral w8 :: e Word16
        w32 = (toIntegral w16 :: e Word32) `lshift` 2
        w64 = (toIntegral w32 :: e Word64) `ieor` murmurHash i32
        ci = toIntegral i32 :: e CInt
        csz = toIntegral (w64 `iand` 1023) :: e CSize
        cb = fromBool (ci `gt` 0) :: e CBool
        cbOk = cb `ne` (0 :: e CBool)
        fl = toFloating i32 :: e Float
        cfl = toFloating fl :: e CFloat
        cdbl = fma (toFloating cfl :: e CDouble) 1.5 0.25
        hyp = hypot (3.0 :: e CDouble) (4.0 :: e CDouble)
        trig = sin (0.0 :: e Double) + cos (0.0 :: e Double)
        z = (3.5 :: e (Complex Double)) * 2.0
        zSq = z + 1.25
        q = (toIntegral csz :: e Int64) `quotient` 3 + (n `remainder` 4)
        base = toFloating cdbl + toFloating hyp + rePart zSq * 0.01 + imPart zSq * 0.01 + trig + toFloating q
     in ifThenElse (cbOk `land` toBool i32) base (-1.0)

cudaConstraintStoreKernel :: forall m e. (CmdJanus m e) => e Int64 -> e (Ptr Double) -> m ()
cudaConstraintStoreKernel x outPtr = do
  v <- letM (purePrimKernel x)
  M.pokeElemOff outPtr v 0

unit_exp_janus_all_backends :: IO ()
unit_exp_janus_all_backends = do
  cfg <- getConstraintsTestConfig
  let arg :: Int64
      arg = 17
      expected = runIdentity (purePrimKernel @Identity (Identity arg))

  cPortable <- withJanusC @PortableC cfg (purePrimKernel @JanusCCSE) (\f -> pure (f arg))
  cX86 <- withJanusC @LinuxX86_64 cfg (purePrimKernel @LinuxX86_64CCSE) (\f -> pure (f arg))
  cCSE <-
    withJanusC
      @PortableC
      cfg
      (withCSE (purePrimKernel @(CSEExp JanusCCSE)))
      (\f -> pure (f arg))
  let readerCProg :: JanusCCSE Int64 -> JanusCMCSE (JanusCCSE Double)
      readerCProg x =
        pure (getReaderExp (purePrimKernel @(ReaderExp Int64 JanusCCSE) (ReaderExp x)))
  cReader <- withJanusC @PortableC cfg readerCProg (\f -> f arg)

  let cudaVal = getCUDAIO (purePrimKernel @CUDAIO (CUDAIO arg))
      cudaSrc = showJanusCUDA "cuda_constraints_kern" (cudaConstraintStoreKernel @JanusCUDAMCSE @JanusCUDACSE)
  assertBool "CUDA source should contain kernel name" ("cuda_constraints_kern" `isInfixOf` cudaSrc)

  withArray [0.0 :: Double] $ \pOut -> do
    let launchCfg =
          CUlaunchConfig
            { _cuLaunchConfigBlockDimX = 1,
              _cuLaunchConfigBlockDimY = 1,
              _cuLaunchConfigBlockDimZ = 1,
              _cuLaunchConfigGridDimX = 1,
              _cuLaunchConfigGridDimY = 1,
              _cuLaunchConfigGridDimZ = 1,
              _cuLaunchConfigSharedMemBytes = 0,
              _cuLaunchConfigStream = CUstream nullPtr
            }
    launchCUDAIOM launchCfg $
      cudaConstraintStoreKernel @CUDAIOM @CUDAIO (fromIntegral arg) (inject pOut)
    [cudaMemOut] <- peekArray 1 pOut
    assertApprox "CUDAIOM kernel output" expected cudaMemOut

  assertApprox "PortableC vs Identity" expected cPortable
  assertApprox "LinuxX86_64C vs Identity" expected cX86
  assertApprox "CSEExp JanusC vs Identity" expected cCSE
  assertApprox "ReaderExp JanusC vs Identity" expected cReader
  assertApprox "CUDAIO vs Identity" expected cudaVal

--------------------------------------------------------------------------------
-- 2. Multi-Container + Multi-Layout `(CmdJanus m e) =>` Kernel (Single Constraint!)
--------------------------------------------------------------------------------

-- | Single imperative kernel constrained ONLY by `(CmdJanus m e) =>` that builds
-- a composite allocation policy and exercises 11 `Janus.Data` structures, custom
-- HKD `SoA` and intrusive `JanusStruct`s, `Stream`, and `Drain` with zero leaks!
multiContainerKernel :: forall m e. (CmdJanus m e) => e Int64 -> m (e Double)
multiContainerKernel seed = do
  tracker <- newAllocTracker
  rec <- newRecycler dynamicAlloc 16
  let mkAlloc tr rc =
        localAlloc "mc_alloc" (statsAlloc (AllocTracker tr) (recyclingAlloc (Recycler rc) dynamicAlloc))
      alloc = mkAlloc (unAllocTracker tracker) (unRecycler rec)
  ar <- newArena alloc 4096

  -- Phase A: DynVec (Scalar Double), DynVec (SoA SampleRec), & SmallVec 8 Int32
  vPhase <-
    localFunArgsM
      "mc_phase_vecs"
      ( \_ (LACons pTr (LACons pRc (LACons sd LANil))) -> do
          let al = localAlloc "mc_alloc" (statsAlloc (AllocTracker pTr) (recyclingAlloc (Recycler pRc) dynamicAlloc))
          dvScalar <- DV.new @Double al
          dvSoA <- DV.new @(SoA SampleRec) al
          _ <-
            runDrain
              (DV.drainAppendDynVec al dvScalar)
              (streamM (rangeS 0 6) (\i -> pure (toFloating (sd + i) * 1.25)))
          _ <-
            runDrain
              (DV.drainAppendDynVec al dvSoA)
              ( streamM (rangeS (0 * sd) 6) $ \i ->
                  pure
                    SampleRec
                      { srTag = toIntegral i,
                        srWeight = toFloating (i + 1) * 2.0
                      }
              )
          sDvScalar <- runDrain drainSum (DV.stream dvScalar)
          sDvSoA <- runDrain drainSum (srWeight <$> DV.stream dvSoA)
          sv <- SV.new @8 @Int32 al
          _ <-
            runDrain
              (SV.drainAppendSmallVec al sv)
              (streamM (rangeS (0 * sd + 1) 11) (\i -> pure (toIntegral i)))
          (_, svPopped) <- SV.pop 0 sv
          svLen <- SV.len sv
          SV.free al sv
          DV.free al dvSoA
          DV.free al dvScalar
          pure (sDvScalar + sDvSoA + toFloating svPopped + toFloating svLen)
      )
      (LACons (unAllocTracker tracker) (LACons (unRecycler rec) (LACons seed LANil)))

  -- Phase B: HashMap Int64 Double & BTreeMap Word64 Float
  mPhase <-
    localFunArgsM
      "mc_phase_maps"
      ( \_ (LACons pTr (LACons pRc LANil)) -> do
          let al = localAlloc "mc_alloc" (statsAlloc (AllocTracker pTr) (recyclingAlloc (Recycler pRc) dynamicAlloc))
          hm <- HM.new @Int64 @Double al
          HM.insert al hm 10 3.5
          HM.insert al hm 20 6.5
          HM.insertWith al hm (+) 10 1.5
          (hmFound, hmVal) <- HM.lookup 0.0 hm 10

          btm <- BTM.new @Word64 @Float al
          BTM.insert al btm 100 4.25
          BTM.insert al btm 50 2.75
          BTM.insert al btm 150 8.0
          (btmFound, btmVal) <- BTM.lookup 0.0 btm 50

          BTM.free al btm
          HM.free al hm
          pure (ifThenElse (hmFound `land` btmFound) (hmVal + toFloating btmVal) (-999999.0))
      )
      (LACons (unAllocTracker tracker) (LACons (unRecycler rec) LANil))

  -- Phase C: LruCache, BitSet, BloomFilter, & DisjointSet
  sPhase <-
    localFunArgsM
      "mc_phase_sets"
      ( \_ (LACons pTr (LACons pRc LANil)) -> do
          let al = localAlloc "mc_alloc" (statsAlloc (AllocTracker pTr) (recyclingAlloc (Recycler pRc) dynamicAlloc))
          lru <- LRU.new @Int32 @Int64 al 4
          LRU.insert lru 1 100
          LRU.insert lru 2 200
          (lruFound, lruVal) <- LRU.lookup 0 lru 1

          bs <- BS.new al
          _ <- BS.setBit al bs 3
          _ <- BS.setBit al bs 67
          bsCnt <- BS.popcount bs

          bf <- BF.newWithBlocksSeeded @Double al 2 0x12345678
          BF.insert bf 42.5
          bfHit <- BF.mightContain bf 42.5

          ds <- DS.new al 6
          _ <- DS.union ds 0 1
          _ <- DS.union ds 1 2
          dsConn <- DS.sameSet ds 0 2
          dsSets <- DS.numSets ds

          DS.free al ds
          BF.free al bf
          BS.free al bs
          LRU.free al lru
          pure
            ( ifThenElse
                (lruFound `land` bfHit `land` dsConn)
                (toFloating lruVal + toFloating bsCnt + toFloating dsSets)
                (-999999.0)
            )
      )
      (LACons (unAllocTracker tracker) (LACons (unRecycler rec) LANil))

  -- Phase D: Interner (with Arena scratchAlloc) & IntrusiveList over custom `ListItem`
  iPhase <-
    localFunArgsM
      "mc_phase_intrusive"
      ( \_ (LACons pTr (LACons pRc (LACons pAr LANil))) -> do
          let al = localAlloc "mc_alloc" (statsAlloc (AllocTracker pTr) (recyclingAlloc (Recycler pRc) dynamicAlloc))
              scAl = localAlloc "mc_scratch" (arenaAlloc (Arena pAr) al)
          interner <- IN.new al
          strBuf <- allocTypedArray @Word8 scAl 4
          M.pokeElemOff strBuf 65 0
          M.pokeElemOff strBuf 66 1
          M.pokeElemOff strBuf 67 2
          M.pokeElemOff strBuf 68 3
          sym1 <- IN.internBytes al interner strBuf 4
          sym2 <- IN.internBytes al interner strBuf 4
          let symSame = symbolEq sym1 sym2
          freeTypedArray @Word8 scAl strBuf 4

          il <- IL.new al liHookSpec
          n1 <- allocStruct @(CStruct ListItem) al
          M.poke (getPtrOf (liValue (projectPtrs n1))) 11
          IL.initNodeHook il n1
          IL.pushBackNode il n1
          n2 <- allocStruct @(CStruct ListItem) al
          M.poke (getPtrOf (liValue (projectPtrs n2))) 29
          IL.initNodeHook il n2
          IL.pushBackNode il n2
          ilSum <-
            runDrain drainSum $
              streamM (IL.nodePtrStream il) $ \p ->
                M.peek (getPtrOf (liValue (projectPtrs p)))

          IL.freeAllNodesAndSpine al il
          IN.free al interner
          pure (ifThenElse symSame (toFloating ilSum) (-999999.0))
      )
      (LACons (unAllocTracker tracker) (LACons (unRecycler rec) (LACons (unArena ar) LANil)))

  destroyArena alloc ar
  destroyRecycler dynamicAlloc rec
  snap <- readAllocSnapshot tracker
  freeAllocTracker tracker

  let allChecksOk =
        (vPhase `gt` (-100000.0))
          `land` (mPhase `gt` (-100000.0))
          `land` (sPhase `gt` (-100000.0))
          `land` (iPhase `gt` (-100000.0))
          `land` (snapLiveBlocks snap `eq` 0)
          `land` (snapLiveBytes snap `eq` 0)
  pure (ifThenElse allChecksOk (vPhase + mPhase + sPhase + iPhase) (-999999.0))

unit_cmd_janus_multi_container_all_backends :: IO ()
unit_cmd_janus_multi_container_all_backends = do
  cfg <- getConstraintsTestConfig
  let seed :: Int64
      seed = 10
  Identity ioRes <- multiContainerKernel @IO @Identity (Identity seed)
  assertBool "multiContainerKernel checks & zero-leak passed" (ioRes > 0)

  cX86Res <-
    withJanusC
      @LinuxX86_64
      cfg
      (multiContainerKernel @LinuxX86_64CMCSE @LinuxX86_64CCSE)
      (\f -> f seed)
  cCSERes <-
    withJanusC
      @PortableC
      cfg
      (cse (multiContainerKernel @JanusCMCSE @JanusCCSE))
      (\f -> f seed)
  ReaderExp (Identity readerRes) <-
    runReaderT
      (multiContainerKernel @(ReaderT Int64 IO) @(ReaderExp Int64 Identity) (ReaderExp (Identity seed)))
      0

  assertApprox "LinuxX86_64C multi-container vs IO" ioRes cX86Res
  assertApprox "CSEM JanusC multi-container vs IO" ioRes cCSERes
  assertApprox "ReaderT IO multi-container vs IO" ioRes readerRes

--------------------------------------------------------------------------------
-- 3. `CmdJanusIO` Formatted File Output Parity (`Int8`, `Word16`, `Int64`, `Double`)
--------------------------------------------------------------------------------

formatSummaryToFile :: forall m e. (CmdJanusIO m e) => e (Ptr CChar) -> e Int64 -> m ()
formatSummaryToFile path n =
  withString "w" $ \mode -> do
    h <- fopen path mode
    withString "hdr:" $ \tag -> hputString h tag
    let i8 = toIntegral n :: e Int8
        w16 = toIntegral (n * 100) :: e Word16
        dbl = toFloating n * 1.5 :: e Double
    hformat h i8 ","
    hformat h w16 ","
    hformat h n ","
    hformat h dbl "\n"
    _ <- fclose h
    pure ()

unit_cmd_janus_io_format_parity :: IO ()
unit_cmd_janus_io_format_parity = do
  cfg <- getConstraintsTestConfig
  mtmp <- lookupEnv "TMPDIR"
  let dir = maybe "/tmp" id mtmp
      ioPath = dir </> "janus_constraints_io.txt"
      cPath = dir </> "janus_constraints_c.txt"
  withCString ioPath $ \pIO ->
    formatSummaryToFile @IO @Identity (Identity pIO) (Identity 12)
  withJanusC @PortableC cfg (formatSummaryToFile @JanusCMCSE @JanusCCSE) $ \fn ->
    withCString cPath $ \pC ->
      fn pC 12
  ioTxt <- readFile ioPath
  cTxt <- readFile cPath
  removeFile ioPath
  removeFile cPath
  -- Both start with "hdr:12,1200,12,18."
  take 18 ioTxt @?= "hdr:12,1200,12,18."
  take 18 cTxt @?= "hdr:12,1200,12,18."

--------------------------------------------------------------------------------
-- 4. Polymorphic Element & Key Helpers (`ExpInt`, `ExpFloat`, `CmdElem`, `*LayoutCtx`)
--------------------------------------------------------------------------------

polyIntMix :: forall a e. (ExpJanus e, ExpInt e a, ExpFix e (e a -> e a -> e Int64)) => e a -> e a -> e Int64
polyIntMix =
  fun (Proxy @e) "poly_int_mix" $ \_ a b ->
    let x = (a + b) `ieor` (a `lshift` 1)
        y = x `quotient` 2
     in ifThenElse (toBool y) (toIntegral y) 0

polyFloatMix :: forall a e. (ExpJanus e, ExpFloat e a, ExpFix e (e a -> e a -> e Double)) => e a -> e a -> e Double
polyFloatMix =
  fun (Proxy @e) "poly_float_mix" $ \_ a b ->
    toFloating (fma a b (hypot a b))

polySwapElems ::
  forall a m e.
  (CmdJanus m e, CmdElem m e a, ExpFix e (e (Ptr a) -> e Int64 -> e Int64 -> m ())) =>
  e (Ptr a) ->
  e Int64 ->
  e Int64 ->
  m ()
polySwapElems =
  fun (Proxy @e) "poly_swap_elems" $ \_ arr i j ->
    whenM_ (i `ne` j) $ do
      vi <- M.peekElemOff arr i
      vj <- M.peekElemOff arr j
      tmpRef <- newRef vi
      M.pokeElemOff arr vj i
      readRef tmpRef >>= \t -> M.pokeElemOff arr t j
      writeRef tmpRef vj

polyCountUniqueHashed :: forall k m e. (HashLayoutCtx m e k) => Stream m (Elem e k) -> m (e Int64)
polyCountUniqueHashed s = do
  hs :: HS.HashSet e k <- HS.fromStream dynamicAlloc s
  n <- HS.size hs
  HS.free dynamicAlloc hs
  pure n

polyCountUniqueOrdered :: forall k m e. (OrdLayoutCtx m e k) => Stream m (Elem e k) -> m (e Int64)
polyCountUniqueOrdered s = do
  fs :: FS.FlatSet e k <- FS.fromStream dynamicAlloc s
  n <- FS.size fs
  FS.free dynamicAlloc fs
  pure n

polyElemKernel ::
  forall m e.
  ( CmdJanus m e,
    ExpFix e (e Int16 -> e Int16 -> e Int64),
    ExpFix e (e Word32 -> e Word32 -> e Int64),
    ExpFix e (e CInt -> e CInt -> e Int64),
    ExpFix e (e Float -> e Float -> e Double),
    ExpFix e (e Double -> e Double -> e Double),
    ExpFix e (e CDouble -> e CDouble -> e Double),
    ExpFix e (e (Ptr Double) -> e Int64 -> e Int64 -> m ()),
    ExpFix e (e Int64 -> m (e Int64))
  ) =>
  e (Ptr Double) ->
  m (e Double)
polyElemKernel dblArr = do
  let m1 = polyIntMix @Int16 12 7
      m2 = polyIntMix @Word32 100 55
      m3 = polyIntMix @CInt 9 4
      f1 = polyFloatMix @Float 3.0 4.0
      f2 = polyFloatMix @Double 3.0 4.0
      f3 = polyFloatMix @CDouble 3.0 4.0
  polySwapElems @Double dblArr 0 1

  let countScalarDup :: e Int64 -> m (e Int64)
      countScalarDup =
        fun (Proxy @e) "count_scalar_dup" $ \_ n -> do
          let scalarDupS = streamM (rangeS 0 n) (\i -> pure (i `remainder` 3))
          uhScalar <- polyCountUniqueHashed @Int64 scalarDupS
          uoScalar <- polyCountUniqueOrdered @Int64 scalarDupS
          pure (uhScalar * 10 + uoScalar * 100)

      countStructDup :: e Int64 -> m (e Int64)
      countStructDup =
        fun (Proxy @e) "count_struct_dup" $ \_ n -> do
          let structDupS =
                streamM (rangeS 0 n) $ \i ->
                  pure
                    SampleRec
                      { srTag = toIntegral (i `remainder` 4),
                        srWeight = 1.5
                      }
          uhStruct <- polyCountUniqueHashed @(AoS SampleRec) structDupS
          uoStruct <- polyCountUniqueOrdered @(AoS SampleRec) structDupS
          pure (uhStruct * 1000 + uoStruct * 10000)

  sDup <- countScalarDup 8
  stDup <- countStructDup 8
  let intTotal = m1 + m2 + m3 + sDup + stDup
  pure (toFloating intTotal + f1 + f2 + f3)

unit_polymorphic_element_and_key_helpers :: IO ()
unit_polymorphic_element_and_key_helpers = do
  cfg <- getConstraintsTestConfig
  let initArr = [10.5, 99.25 :: Double]
  (ioRes, ioArr) <-
    withArray initArr $ \p -> do
      Identity r <- polyElemKernel @IO @Identity (Identity p)
      arrOut <- peekArray 2 p
      pure (r, arrOut)
  (cRes, cArr) <-
    withJanusC @LinuxX86_64 cfg (polyElemKernel @LinuxX86_64CMCSE @LinuxX86_64CCSE) $ \fn ->
      withArray initArr $ \p -> do
        r <- fn p
        arrOut <- peekArray 2 p
        pure (r, arrOut)
  let _ = Storable.sizeOf (undefined :: CStruct SampleRec)
  ioArr @?= [99.25, 10.5]
  cArr @?= [99.25, 10.5]
  assertApprox "polyElemKernel LinuxX86_64C vs IO" ioRes cRes
