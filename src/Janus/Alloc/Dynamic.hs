{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Default dynamic heap allocation policy backed strictly by Janus's
-- 64-byte SIMD-aligned memory primitives ('Janus.Command.Memory').
module Janus.Alloc.Dynamic
  ( CmdAllocBase,
    CmdDynamicAligned (..),
    nullVoidPtr,
    isNullVoidPtr,
    sameVoidPtr,
    alignUpWord64,
    dynamicAllocAligned,
    dynamicAllocZeroedAligned,
    dynamicReallocAligned,
    dynamicFree,
  )
where

import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Janus.Command.Cond (CmdCond (..), whenM_)
import qualified Janus.Command.Memory as Mem
import Janus.Command.Memory
  ( CmdDynamicAligned (..),
    CmdMem,
    CmdMemCopy (..),
    ExpPtr (..),
    janusSIMDAlignment,
  )
import Janus.Command.Ref (CmdRef (..))
import Janus.Constraints (CmdJanus)
import Janus.Data.HKD (ExpStructPtr, ptrToWord64Expr)
import Janus.Expression.Bits (ExpBits (..))
import Janus.Expression.Bool (ExpBool (..))
import Janus.Expression.Cast (ExpPtrCast (..))
import Janus.Expression.Cond (ExpCond (..))
import Janus.Expression.Eq (ExpEq (..))
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed (JanusTyped)

-- | Core constraint bundle satisfied by all Janus backends.
type CmdAllocBase m e = CmdJanus m e

nullVoidPtr :: forall e. (ExpPtr e Word8, ExpPtrCast e, JanusTyped e Word8) => e (Ptr ())
nullVoidPtr = toVoidPtr (nullPtr @e @Word8)

isNullVoidPtr :: forall e. (ExpPtrWordOps e) => e (Ptr ()) -> e Bool
isNullVoidPtr p = ptrToWord64Expr (fromVoidPtr p :: e (Ptr Word8)) `eq` 0

sameVoidPtr :: forall e. (ExpPtrWordOps e) => e (Ptr ()) -> e (Ptr ()) -> e Bool
sameVoidPtr p1 p2 =
  ptrToWord64Expr (fromVoidPtr p1 :: e (Ptr Word8))
    `eq` ptrToWord64Expr (fromVoidPtr p2 :: e (Ptr Word8))

type ExpPtrWordOps e =
  ( ExpStructPtr e,
    ExpPtrCast e,
    ExpEq e Word64,
    Num (e Word64),
    JanusTyped e Word8
  )

alignUpWord64 :: (ExpBits e Word64, Num (e Word64)) => e Word64 -> e Word64 -> e Word64
alignUpWord64 x align = (x + (align - 1)) `iand` inot (align - 1)

-- | Allocate @sz@ bytes from Janus dynamic heap with alignment @max 64 align@
-- using Janus 'Mem.malloc' / 'Mem.alignedMalloc'.
dynamicAllocAligned ::
  forall m e.
  (CmdAllocBase m e) =>
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
dynamicAllocAligned sz align = do
  r <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (align `le` fromIntegral janusSIMDAlignment)
    (Mem.malloc sz >>= writeRef r . fromVoidPtr)
    (Mem.alignedMalloc sz align >>= writeRef r . fromVoidPtr)
  toVoidPtr <$> readRef r

dynamicAllocZeroedAligned ::
  forall m e.
  (CmdAllocBase m e) =>
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
dynamicAllocZeroedAligned sz align = do
  r <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (align `le` fromIntegral janusSIMDAlignment)
    (Mem.calloc 1 sz >>= writeRef r . fromVoidPtr)
    ( do
        vp <- Mem.alignedMalloc sz align
        whenM_ (lnot (isNullVoidPtr vp) `land` (sz `gt` 0)) $
          cmdMemset vp 0 sz
        writeRef r (fromVoidPtr vp)
    )
  toVoidPtr <$> readRef r

dynamicReallocAligned ::
  forall m e.
  (CmdAllocBase m e) =>
  e (Ptr ()) ->
  e Int64 ->
  e Int64 ->
  e Int64 ->
  m (e (Ptr ()))
dynamicReallocAligned oldPtr oldSz newSz align = do
  r <- newRef (nullPtr @e @Word8)
  ifThenElseM_
    (align `le` fromIntegral janusSIMDAlignment)
    (Mem.realloc oldPtr newSz >>= writeRef r . fromVoidPtr)
    ( ifThenElseM_
        (isNullVoidPtr oldPtr)
        (Mem.alignedMalloc newSz align >>= writeRef r . fromVoidPtr)
        ( ifThenElseM_
            (newSz `le` 0)
            (Mem.free oldPtr >> writeRef r (nullPtr @e @Word8))
            ( do
                np <- Mem.alignedMalloc newSz align
                whenM_ (lnot (isNullVoidPtr np)) $ do
                  let copyLen = ifThenElse (oldSz `lt` newSz) oldSz newSz
                  cmdMemcpy np oldPtr copyLen
                  Mem.free oldPtr
                writeRef r (fromVoidPtr np)
            )
        )
    )
  toVoidPtr <$> readRef r

dynamicFree :: (CmdMem m e) => e (Ptr ()) -> m ()
dynamicFree = Mem.free
