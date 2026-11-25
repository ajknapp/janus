{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Alloc.Fixed
  ( FixedBufSpine (..),
    FixedBuf (..),
    initFixedBufRaw,
    withFixedBuf,
    withFixedBufAligned,
    resetFixedBuf,
    fixedOwnsPtr,
    fixedAllocImpl,
    fixedReallocImpl,
    fixedFreeImpl,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import Janus.Alloc.Dynamic (CmdAllocBase, alignUpWord64, isNullVoidPtr, sameVoidPtr)
import Janus.Backend.C.Raw
import Janus.Command.Memory (CmdMemCopy (..), CmdStorable (..), ExpPtr (..), allocaAligned, janusSIMDAlignment)
import Janus.Command.Cond (ifThenElseM_, whenM_)
import Janus.Command.Ref (letM, newRef, readRef, writeRef)
import Janus.Data.HKD
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpIntegralCast (..), ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))

data FixedBufSpine f = FixedBufSpine
  { fbsBase :: !(f (Ptr Word8)),
    fbsCap :: !(f Int64),
    fbsOffset :: !(f Int64),
    fbsLastPtr :: !(f (Ptr Word8)),
    fbsLastOffset :: !(f Int64),
    fbsMinAlign :: !(f Int64)
  }
  deriving stock (Generic)

deriving stock instance Eq (FixedBufSpine Identity)

deriving stock instance Show (FixedBufSpine Identity)

instance JanusStruct FixedBufSpine

newtype FixedBuf e = FixedBuf
  { unFixedBuf :: e (Ptr (CStruct FixedBufSpine))
  }
  deriving stock (Generic)

deriving stock instance Eq (FixedBuf Identity)

deriving stock instance Show (FixedBuf Identity)

instance JanusHKD FixedBuf

initFixedBufRaw ::
  (CmdAllocBase m e) =>
  e (Ptr (CStruct FixedBufSpine)) ->
  e (Ptr Word8) ->
  e Int64 ->
  e Int64 ->
  m (FixedBuf e)
initFixedBufRaw hdr base cap minAl = do
  let f = projectPtrs hdr
      effMin = ifThenElse (minAl `lt` 1) 1 minAl
  poke (getPtrOf (fbsBase f)) base
  poke (getPtrOf (fbsCap f)) cap
  poke (getPtrOf (fbsOffset f)) 0
  poke (getPtrOf (fbsLastPtr f)) nullPtr
  poke (getPtrOf (fbsLastOffset f)) 0
  poke (getPtrOf (fbsMinAlign f)) effMin
  pure (FixedBuf hdr)

-- | Scoped fixed-size buffer on the host stack with default 64B alignment.
withFixedBuf :: Int -> (FixedBuf Identity -> IO a) -> IO a
withFixedBuf cap = withFixedBufAligned cap janusSIMDAlignment

-- | Scoped fixed-size buffer on the host stack with configurable minimum alignment.
withFixedBufAligned :: Int -> Int -> (FixedBuf Identity -> IO a) -> IO a
withFixedBufAligned cap minAl k =
  allocaAligned 1 $ \(hdr :: Ptr (CStruct FixedBufSpine)) ->
    allocaAligned (max 64 cap) $ \(buf :: Ptr Word8) -> do
      fb <- initFixedBufRaw (Identity hdr) (Identity buf) (fromIntegral cap) (fromIntegral minAl)
      k fb

resetFixedBuf :: (CmdAllocBase m e) => FixedBuf e -> m ()
resetFixedBuf (FixedBuf hdr) = do
  let f = projectPtrs hdr
  poke (getPtrOf (fbsOffset f)) 0
  poke (getPtrOf (fbsLastPtr f)) nullPtr
  poke (getPtrOf (fbsLastOffset f)) 0

fixedOwnsPtr :: forall m e. (CmdAllocBase m e) => FixedBuf e -> e (Ptr ()) -> m (e Bool)
fixedOwnsPtr (FixedBuf hdr) vp = do
  let f = projectPtrs hdr
  base <- peek (getPtrOf (fbsBase f))
  cap <- peek (getPtrOf (fbsCap f))
  let baseW = ptrToWord64Expr base
      endW = baseW + toIntegral cap
      pW = ptrToWord64Expr (fromVoidPtr vp :: e (Ptr Word8))
  pure ((pW `ge` baseW) `land` (pW `lt` endW))

fixedAllocImpl ::
  forall m e.
  (CmdAllocBase m e) =>
  FixedBuf e ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
fixedAllocImpl (FixedBuf hdr) reqSz reqAlign = do
  let f = projectPtrs hdr
  base <- peek (getPtrOf (fbsBase f))
  cap <- peek (getPtrOf (fbsCap f))
  curOff <- peek (getPtrOf (fbsOffset f))
  minAl <- peek (getPtrOf (fbsMinAlign f))
  effAlI <- letM (ifThenElse (reqAlign `gt` minAl) reqAlign minAl)
  let effAlW = toIntegral (ifThenElse (effAlI `lt` 1) 1 effAlI) :: e Word64
      szPos = ifThenElse (reqSz `le` 0) (ifThenElse (effAlI `lt` 1) 1 effAlI) reqSz
      baseW = ptrToWord64Expr base
      curW = baseW + toIntegral curOff
      alignedW = alignUpWord64 curW effAlW
      relStart = toIntegral (alignedW - baseW) :: e Int64
  newOff <- letM (relStart + szPos)
  outRef <- newRef (nullPtr @e @Word8)
  whenM_ (newOff `le` cap) $ do
    let outP = word64ToPtrExpr alignedW :: e (Ptr Word8)
    poke (getPtrOf (fbsLastOffset f)) curOff
    poke (getPtrOf (fbsLastPtr f)) outP
    poke (getPtrOf (fbsOffset f)) newOff
    writeRef outRef outP
  toVoidPtr <$> readRef outRef

fixedReallocImpl ::
  forall m e.
  (CmdAllocBase m e) =>
  FixedBuf e ->
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
fixedReallocImpl fb@(FixedBuf hdr) oldPtr oldSz newSz reqAlign = do
  outRef <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (isNullVoidPtr oldPtr)
    (fixedAllocImpl fb newSz reqAlign >>= writeRef outRef . fromVoidPtr)
    ( ifThenElseM_
        (newSz `le` 0)
        (fixedFreeImpl fb oldPtr oldSz >> writeRef outRef (nullPtr @e @Word8))
        ( do
            let f = projectPtrs hdr
            lastP <- peek (getPtrOf (fbsLastPtr f))
            base <- peek (getPtrOf (fbsBase f))
            cap <- peek (getPtrOf (fbsCap f))
            minAl <- peek (getPtrOf (fbsMinAlign f))
            let effAl = ifThenElse (reqAlign `gt` minAl) reqAlign minAl
                oldW = ptrToWord64Expr (fromVoidPtr oldPtr :: e (Ptr Word8))
                isAlignedEnough = (oldW `iand` toIntegral (effAl - 1)) `eq` 0
                relStart = toIntegral (oldW - ptrToWord64Expr base) :: e Int64
                inPlaceEnd = relStart + newSz
            ifThenElseM_
              (sameVoidPtr oldPtr (toVoidPtr lastP) `land` isAlignedEnough `land` (inPlaceEnd `le` cap))
              ( do
                  poke (getPtrOf (fbsOffset f)) inPlaceEnd
                  writeRef outRef (fromVoidPtr oldPtr)
              )
              ( do
                  np <- fixedAllocImpl fb newSz reqAlign
                  whenM_ (lnot (isNullVoidPtr np)) $ do
                    let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                    cmdMemcpy np oldPtr copyLen
                  writeRef outRef (fromVoidPtr np)
              )
        )
    )
  toVoidPtr <$> readRef outRef

fixedFreeImpl ::
  (CmdAllocBase m e) =>
  FixedBuf e ->
  e (Ptr ()) ->
  e Int64 ->
  m ()
fixedFreeImpl (FixedBuf hdr) ptr _ =
  whenM_ (lnot (isNullVoidPtr ptr)) $ do
    let f = projectPtrs hdr
    lastP <- peek (getPtrOf (fbsLastPtr f))
    whenM_ (sameVoidPtr ptr (toVoidPtr lastP)) $ do
      prevOff <- peek (getPtrOf (fbsLastOffset f))
      poke (getPtrOf (fbsOffset f)) prevOff
      poke (getPtrOf (fbsLastPtr f)) nullPtr

-- Host <-> C FFI Marshalling Instances

type instance JanusCHost (FixedBuf (JanusCT _)) = FixedBuf Identity

deriving via
  ViaPtrHandle t (CStruct FixedBufSpine) (FixedBuf Identity)
  instance
    JanusCArg t (FixedBuf (JanusCT t))

deriving via
  ViaPtrHandle t (CStruct FixedBufSpine) (FixedBuf Identity)
  instance
    JanusCValRet t (FixedBuf (JanusCT t))
