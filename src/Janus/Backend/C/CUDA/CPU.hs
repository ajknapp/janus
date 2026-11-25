{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Janus.Backend.C.CUDA.CPU where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Bits (Bits)
import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Proxy (Proxy)
import Data.Word (Word64, Word8)
import Foreign.C (CChar, CFile, CInt, CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Janus.Backend.C.CUDA.Runtime
import Janus.Backend.C.CUDA.Types
import qualified Janus.Command.Memory as M
import Janus.Command.Case
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.TicToc
import Janus.Command.While
import Janus.Constraints (CmdJanus, CmdJanusIO, ExpJanus)
import Janus.Data.HKD (ExpPtrWord (..), ExpStructPtr (..))
import Janus.Expression.Bits
import Janus.Expression.Cast
import Janus.Expression.Cond
import Janus.Expression.Eq
import Janus.Expression.Extract
import Janus.Expression.Inject
import Janus.Expression.Integral
import Janus.Expression.Let
import Janus.Expression.Math
import Janus.Expression.MurmurHash
import Janus.Expression.Ord
import Janus.Typed (JanusTyped)
import Test.DejaFu.Conc (ConcIO)

$( let mkCUDACPUBackend expTy monadTy refTy baseM =
         [d|
           deriving via Identity instance ExpCond $expTy

           deriving via Identity instance ExpLet $expTy

           deriving via Identity instance ExpPtrCast $expTy

           instance ExpPtrWord $expTy where
             ptrToWord64Expr :: forall a. $expTy (Ptr a) -> $expTy Word64
             ptrToWord64Expr = coerce (ptrToWord64Expr @Identity @a)
             word64ToPtrExpr :: forall a. (JanusTyped $expTy a) => $expTy Word64 -> $expTy (Ptr a)
             word64ToPtrExpr = coerce (word64ToPtrExpr @Identity @a)

           instance ExpStructPtr $expTy where
             structFieldPtr :: forall structTy field. (JanusTyped $expTy field) => $expTy (Ptr structTy) -> Int -> String -> $expTy (Ptr field)
             structFieldPtr = coerce (structFieldPtr @Identity @structTy @field)

           deriving via Identity a instance (Num a) => Num ($expTy a)

           deriving via Identity a instance (Bounded a) => Bounded ($expTy a)

           deriving via Identity a instance (Fractional a) => Fractional ($expTy a)

           deriving via Identity a instance (Floating a) => Floating ($expTy a)

           deriving via Identity a instance (FMA a) => FMA ($expTy a)

           deriving via Identity a instance (Hypot a) => Hypot ($expTy a)

           instance (Bits a, Integral a) => ExpBits $expTy a where
             iand :: $expTy a -> $expTy a -> $expTy a
             iand = coerce (iand @Identity @a)
             ior :: $expTy a -> $expTy a -> $expTy a
             ior = coerce (ior @Identity @a)
             ieor :: $expTy a -> $expTy a -> $expTy a
             ieor = coerce (ieor @Identity @a)
             inot :: $expTy a -> $expTy a
             inot = coerce (inot @Identity @a)
             lshift :: $expTy a -> $expTy a -> $expTy a
             lshift = coerce (lshift @Identity @a)
             rshift :: $expTy a -> $expTy a -> $expTy a
             rshift = coerce (rshift @Identity @a)

           instance (Num a, Eq a) => ExpBoolCast $expTy a where
             fromBool :: $expTy Bool -> $expTy a
             fromBool = coerce (fromBool @Identity @a)
             toBool :: $expTy a -> $expTy Bool
             toBool = coerce (toBool @Identity @a)

           instance (Real a, Fractional b) => ExpFloatingCast $expTy a b where
             toFloating :: $expTy a -> $expTy b
             toFloating = coerce (toFloating @Identity @a @b)

           instance (Integral a, Num b) => ExpIntegralCast $expTy a b where
             toIntegral :: $expTy a -> $expTy b
             toIntegral = coerce (toIntegral @Identity @a @b)

           instance (Eq a) => ExpEq $expTy a where
             eq :: $expTy a -> $expTy a -> $expTy Bool
             eq = coerce (eq @Identity @a)
             ne :: $expTy a -> $expTy a -> $expTy Bool
             ne = coerce (ne @Identity @a)

           instance ExpExtract $expTy a where
             extract :: $expTy a -> a
             extract = coerce (extract @Identity @a)

           instance ExpInject $expTy a where
             inject :: a -> $expTy a
             inject = coerce (inject @Identity @a)

           instance (Integral a) => ExpIntegral $expTy a where
             quotient :: $expTy a -> $expTy a -> $expTy a
             quotient = coerce (quotient @Identity @a)
             remainder :: $expTy a -> $expTy a -> $expTy a
             remainder = coerce (remainder @Identity @a)

           instance ExpComplex $expTy a where
             rePart :: $expTy (Complex a) -> $expTy a
             rePart = coerce (rePart @Identity @a)
             imPart :: $expTy (Complex a) -> $expTy a
             imPart = coerce (imPart @Identity @a)

           instance (ExpMurmurHash Identity (Identity a)) => ExpMurmurHash $expTy ($expTy a) where
             murmurHashWithSalt :: $expTy Word64 -> $expTy a -> $expTy Word64
             murmurHashWithSalt = coerce (murmurHashWithSalt @Identity @(Identity a))

           instance (Ord a) => ExpOrd $expTy a where
             lt :: $expTy a -> $expTy a -> $expTy Bool
             lt = coerce (lt @Identity @a)
             le :: $expTy a -> $expTy a -> $expTy Bool
             le = coerce (le @Identity @a)
             gt :: $expTy a -> $expTy a -> $expTy Bool
             gt = coerce (gt @Identity @a)
             ge :: $expTy a -> $expTy a -> $expTy Bool
             ge = coerce (ge @Identity @a)

           instance (Storable a) => M.ExpSized $expTy a where
             sizeOf :: Proxy a -> $expTy Int64
             sizeOf = coerce (M.sizeOf @Identity @a)
             alignOf :: Proxy a -> $expTy Int64
             alignOf = coerce (M.alignOf @Identity @a)

           instance (Storable a) => M.ExpPtr $expTy a where
             nullPtr :: $expTy (Ptr a)
             nullPtr = coerce (M.nullPtr @Identity @a)
             ptrAdd :: $expTy (Ptr a) -> $expTy Int64 -> $expTy (Ptr a)
             ptrAdd = coerce (M.ptrAdd @Identity @a)
             ptrIndex :: $expTy (Ptr a) -> $expTy Int64 -> $expTy (Ptr a)
             ptrIndex = coerce (M.ptrIndex @Identity @a)

           instance MonadReader (CUDAThreadInfo $expTy) $monadTy where
             ask :: $monadTy (CUDAThreadInfo $expTy)
             ask = coerce (cudaAskThreadInfo @($baseM) @($expTy))
             local :: forall a. (CUDAThreadInfo $expTy -> CUDAThreadInfo $expTy) -> $monadTy a -> $monadTy a
             local = coerce (cudaLocalThreadInfo @($baseM) @($expTy) @a)

           instance M.CmdMem $monadTy $expTy where
             malloc :: $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             malloc = coerce (cudaMalloc @($baseM))
             calloc :: $expTy Int64 -> $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             calloc = coerce (cudaCalloc @($baseM))
             realloc :: $expTy (Ptr ()) -> $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             realloc = coerce (cudaRealloc @($baseM))
             free :: $expTy (Ptr ()) -> $monadTy ()
             free = coerce (cudaFree @($baseM))

           instance M.CmdDynamicAligned $monadTy $expTy where
             alignedMalloc :: $expTy Int64 -> $expTy Int64 -> $monadTy ($expTy (Ptr ()))
             alignedMalloc = coerce (cudaAlignedMalloc @($baseM))

           instance M.CmdMemCopy $monadTy $expTy where
             cmdMemcpy :: $expTy (Ptr ()) -> $expTy (Ptr ()) -> $expTy Int64 -> $monadTy ()
             cmdMemcpy = coerce (cudaMemcpy @($baseM))
             cmdMemmove :: $expTy (Ptr ()) -> $expTy (Ptr ()) -> $expTy Int64 -> $monadTy ()
             cmdMemmove = coerce (cudaMemmove @($baseM))
             cmdMemset :: $expTy (Ptr ()) -> $expTy Word8 -> $expTy Int64 -> $monadTy ()
             cmdMemset = coerce (cudaMemset @($baseM))

           instance (Storable a) => M.CmdStorable $monadTy $expTy a where
             peek :: $expTy (Ptr a) -> $monadTy ($expTy a)
             peek = coerce (cudaPeek @($baseM) @a)
             poke :: $expTy (Ptr a) -> $expTy a -> $monadTy ()
             poke = coerce (cudaPoke @($baseM) @a)
             peekElemOff :: $expTy (Ptr a) -> $expTy Int64 -> $monadTy ($expTy a)
             peekElemOff = coerce (cudaPeekElemOff @($baseM) @a)
             pokeElemOff :: $expTy (Ptr a) -> $expTy a -> $expTy Int64 -> $monadTy ()
             pokeElemOff = coerce (cudaPokeElemOff @($baseM) @a)

           instance (Storable a) => M.CmdRefPtr $monadTy $expTy a where
             withRefPtr :: forall b. $refTy a -> ($expTy (Ptr a) -> $monadTy b) -> $monadTy b
             withRefPtr = coerce (cudaWithRefPtr @($baseM) @a @b)

           instance CmdCase $monadTy $expTy where
             switchCase_ :: $expTy Int64 -> [(Int, $monadTy ())] -> $monadTy () -> $monadTy ()
             switchCase_ = coerce (cudaSwitchCase_ @($baseM))

           instance CmdCond $monadTy $expTy where
             ifThenElseM_ :: $expTy Bool -> $monadTy () -> $monadTy () -> $monadTy ()
             ifThenElseM_ = coerce (cudaIfThenElseM_ @($baseM))

           instance CmdIO $monadTy $expTy where
             stdout :: $expTy (Ptr CFile)
             stdout = coerce (stdout @IO @Identity)
             stderr :: $expTy (Ptr CFile)
             stderr = coerce (stderr @IO @Identity)
             fopen :: $expTy (Ptr CChar) -> $expTy (Ptr CChar) -> $monadTy ($expTy (Ptr CFile))
             fopen = coerce (cudaFopen @($baseM))
             fclose :: $expTy (Ptr CFile) -> $monadTy ($expTy CInt)
             fclose = coerce (cudaFclose @($baseM))

           instance CmdString $monadTy $expTy where
             withString :: forall a. String -> ($expTy CString -> $monadTy a) -> $monadTy a
             withString = coerce (cudaWithString @($baseM) @a)

           instance CmdPutString $monadTy $expTy where
             hputString :: $expTy (Ptr CFile) -> $expTy CString -> $monadTy ()
             hputString = coerce (cudaHputString @($baseM))

           instance (Show a) => CmdFormat $monadTy $expTy a where
             hformat :: $expTy (Ptr CFile) -> $expTy a -> String -> $monadTy ()
             hformat = coerce (cudaHformat @($baseM) @a)

           instance CmdRef $monadTy $expTy where
             type Ref $monadTy $expTy a = $refTy a
             letM = pure
             newRef :: forall a. (JanusTyped $expTy a) => $expTy a -> $monadTy ($refTy a)
             newRef = coerce (cudaNewRef @($baseM) @a)
             newRef' :: forall a. (JanusTyped $expTy a) => $monadTy ($refTy a)
             newRef' = coerce (cudaNewRef' @($baseM) @a)
             readRef :: forall a. (JanusTyped $expTy a) => $refTy a -> $monadTy ($expTy a)
             readRef = coerce (cudaReadRef @($baseM) @a)
             writeRef :: forall a. (JanusTyped $expTy a) => $refTy a -> $expTy a -> $monadTy ()
             writeRef = coerce (cudaWriteRef @($baseM) @a)

           instance RefCell $monadTy $expTy ($expTy a) where
             newCell x = do
               r <- newRef x
               pure (Cell (readRef r) (writeRef r))
             newCell' = do
               r <- newRef'
               pure (Cell (readRef r) (writeRef r))

           instance CmdRange $monadTy $expTy

           instance CmdWhile $monadTy $expTy where
             whileM :: $expTy Bool -> ($refTy Bool -> $monadTy ()) -> $monadTy ()
             whileM = coerce (cudaWhileM @($baseM))

           instance CmdTicToc $monadTy $expTy where
             tic :: $monadTy (Tic $expTy)
             tic = coerce (cudaTic @($baseM))
             toc :: $monadTy (Toc $expTy)
             toc = coerce (cudaToc @($baseM))

           instance ExpJanus $expTy

           instance CmdJanus $monadTy $expTy

           instance CmdJanusIO $monadTy $expTy
           |]
    in fmap concat $
         sequence
           [ mkCUDACPUBackend [t|CUDAIO|] [t|CUDAIOM|] [t|CUDAIORef|] [t|IO|],
             mkCUDACPUBackend [t|CUDADejaFu|] [t|CUDADejaFuM|] [t|CUDADejaFuRef|] [t|ConcIO|]
           ]
 )
