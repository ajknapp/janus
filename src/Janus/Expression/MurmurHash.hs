{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Expression.MurmurHash where

import Control.Lens (ix, (%~), (&), (+~), (.~), (^.))
import Control.Monad.State (modify)
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Loc (noLoc)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Word
import Foreign.Ptr (Ptr, ptrToWordPtr)
import GHC.Float
import GHC.Generics
import Janus.Backend.C
import Janus.Command.Cond (CmdCond, whenM_)
import Janus.Command.Memory (CmdStorable (..))
import Janus.Command.Ref (CmdRef (..), RefCell)
import Janus.Command.While (CmdWhile)
import Janus.Control.Drain (drainFoldM, runDrain)
import Janus.Control.Stream (rangeS)
import Janus.Expression.Bits
import Janus.Expression.Bool (ExpBool)
import Janus.Expression.Cast
import Janus.Expression.Let
import Janus.Expression.Ord (ExpOrd (..))
import Janus.Typed
import Language.C.Quote
  ( BlockItem (BlockDecl, BlockStm),
    Decl (DeclRoot),
    DeclSpec (DeclSpec),
    Exp (Cast, FnCall, UnOp, Var),
    Id (Id),
    Init (Init),
    InitGroup (InitGroup),
    Stm (Exp),
    Type (Type),
    TypeSpec (Tnamed),
    UnOp (AddrOf),
  )
import Language.Haskell.TH (Type (ConT))

type ExpHash' e = (JanusTyped e Word64, ExpBits e Word64, ExpLet e, Num (e Word64))

defaultHashSalt :: (Num (e Word64)) => e Word64
defaultHashSalt = 0x9e3779b97f4a7c15

-- constants from https://mostlymangling.blogspot.com/2019/12/
hash64 :: (ExpHash' e) => e Word64 -> e Word64
hash64 = flip let_ id . shiftEor 27 . (* 0x1c69b3f74ac4ae35) . shiftEor 33 . (* 0x3c79ac492ba7b653) . shiftEor 27
  where
    shiftEor i h = let_ h $ \h' -> h' `ieor` (h' `rshift` i)

hash64WithSalt :: (ExpHash' e) => e Word64 -> e Word64 -> e Word64
hash64WithSalt salt x = hash64 (hash64 salt `ieor` hash64 x)

class ExpMurmurHash e a where
  murmurHashWithSalt :: e Word64 -> a -> e Word64

murmurHash :: (Num (e Word64), ExpMurmurHash e a) => a -> e Word64
murmurHash = murmurHashWithSalt defaultHashSalt

instance ExpMurmurHash Identity (Identity Bool) where
  murmurHashWithSalt seed (Identity b) = hash64WithSalt seed (Identity (if b then 1 else 0))

instance ExpMurmurHash (JanusCT t) (JanusCT t Bool) where
  murmurHashWithSalt seed b = hash64WithSalt seed (fromBool b)

instance ExpMurmurHash Identity (Identity (Ptr p)) where
  murmurHashWithSalt seed (Identity p) =
    hash64WithSalt seed (Identity (fromIntegral (ptrToWordPtr p) :: Word64))

instance ExpMurmurHash (JanusCT t) (JanusCT t (Ptr p)) where
  murmurHashWithSalt seed (JanusC mp) =
    let asW64 = JanusC $ do
          addHeader "stdint.h"
          RVal pExp <- mp
          let uptrTy = Type (DeclSpec [] [] (Tnamed (Id "uintptr_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc
              u64Ty = Type (DeclSpec [] [] (Tnamed (Id "uint64_t" noLoc) [] noLoc) noLoc) (DeclRoot noLoc) noLoc
          pure $ RVal $ Cast u64Ty (Cast uptrTy pExp noLoc) noLoc
     in hash64WithSalt seed asW64

$( let inst t =
         [d|
           instance ExpMurmurHash (JanusCT target) (JanusCT target $t) where
             murmurHashWithSalt seed x = hash64WithSalt seed (toIntegral x)

           instance ExpMurmurHash Identity (Identity $t) where
             murmurHashWithSalt seed x = hash64WithSalt seed (toIntegral x)
           |]
       tys = Set.toList (foldl1 Set.union [janusSignedIntTypes, janusUnsignedIntTypes])
    in concat <$> traverse (inst . pure . ConT) tys
 )

jcBitcastToUInt :: forall src dst t. (JanusCTyped src, JanusCTyped dst) => Int -> JanusCT t src -> JanusCT t dst
jcBitcastToUInt bytes (JanusC mx) = JanusC $ do
  addHeader "string.h"
  addHeader "stdint.h"
  RVal xExp <- mx
  srcTy <- getJanusCType (Proxy @src)
  srcVar <- emitLocalVal srcTy xExp
  JCType dstSpec dstDec <- getJanusCType (Proxy @dst)
  fn <- getFunction
  fname <- askFuncName
  let c = fn ^. jcfVarCounter
      dstId = Id ("x_" <> show c) noLoc
      dstVar = Var dstId noLoc
      decl = BlockDecl $ InitGroup dstSpec [] [Init dstId dstDec Nothing Nothing [] noLoc] noLoc
      cp =
        BlockStm $
          Exp
            ( Just $
                FnCall
                  (Var (Id "memcpy" noLoc) noLoc)
                  [UnOp AddrOf dstVar noLoc, UnOp AddrOf srcVar noLoc, signedIntConst bytes]
                  noLoc
            )
            noLoc
  modify $ \st -> st & ix fname .~ (fn & jcfVarCounter +~ 1 & jcfBlock %~ flip appendBlock [decl, cp])
  pure (RVal dstVar)

instance ExpMurmurHash Identity (Identity Float) where
  murmurHashWithSalt salt x = hash64WithSalt salt (fromIntegral $ fmap castFloatToWord32 x)

instance ExpMurmurHash Identity (Identity Double) where
  murmurHashWithSalt salt x = hash64WithSalt salt (fmap castDoubleToWord64 x)

instance ExpMurmurHash (JanusCT t) (JanusCT t Float) where
  murmurHashWithSalt salt x =
    let w32 = jcBitcastToUInt @Float @Word32 4 x
     in hash64WithSalt salt (toIntegral w32)

instance ExpMurmurHash (JanusCT t) (JanusCT t Double) where
  murmurHashWithSalt salt x =
    let w64 = jcBitcastToUInt @Double @Word64 8 x
     in hash64WithSalt salt w64

class GExpHash e f where
  ghashWithSalt :: e Word64 -> f a -> e Word64

instance (ExpMurmurHash e a) => GExpHash e (K1 i a) where
  ghashWithSalt salt (K1 x) = murmurHashWithSalt salt x

instance (ExpHash' e, GExpHash e f, GExpHash e g) => GExpHash e (f :*: g) where
  ghashWithSalt salt (f :*: g) = hash64WithSalt salt (ghashWithSalt salt f `ieor` ghashWithSalt salt g)

instance (GExpHash e f) => GExpHash e (M1 i c f) where
  ghashWithSalt salt (M1 x) = ghashWithSalt salt x

defaultHashWithSalt :: (GExpHash e (Rep a), Generic a) => e Word64 -> a -> e Word64
defaultHashWithSalt salt x = ghashWithSalt salt (from x)

type CmdHashBytes m e =
  ( ExpHash' e,
    CmdRef m e,
    RefCell m e (e Word64),
    CmdCond m e,
    CmdWhile m e,
    CmdStorable m e Word8,
    ExpBool e,
    ExpBits e Int64,
    ExpOrd e Int64,
    ExpIntegralCast e Int64 Word64,
    ExpIntegralCast e Word8 Word64,
    Num (e Int64),
    JanusTyped e Bool,
    JanusTyped e Int64
  )

-- | Alignment-safe, endianness-deterministic 64-bit MurmurHash over an arbitrary
-- @[ptr .. ptr + byteLen - 1]@ byte slice, folding 8-byte blocks + tail bytes via 'rangeS'.
hashBytesWithSalt ::
  forall m e.
  (CmdHashBytes m e) =>
  e Word64 ->
  e (Ptr Word8) ->
  e Int64 ->
  m (e Word64)
hashBytesWithSalt salt ptr byteLen = do
  outRef <- newRef (hash64WithSalt salt 0)
  whenM_ (byteLen `gt` 0) $ do
    let h0 = hash64WithSalt salt (toIntegral byteLen)
        numBlocks = byteLen `rshift` 3
        tailStart = numBlocks `lshift` 3
        tailRem = byteLen `iand` 7
    hBlocks <-
      runDrain
        ( drainFoldM h0 $ \acc bi -> do
            let base = bi `lshift` 3
            b0 <- toIntegral <$> peekElemOff ptr base
            b1 <- toIntegral <$> peekElemOff ptr (base + 1)
            b2 <- toIntegral <$> peekElemOff ptr (base + 2)
            b3 <- toIntegral <$> peekElemOff ptr (base + 3)
            b4 <- toIntegral <$> peekElemOff ptr (base + 4)
            b5 <- toIntegral <$> peekElemOff ptr (base + 5)
            b6 <- toIntegral <$> peekElemOff ptr (base + 6)
            b7 <- toIntegral <$> peekElemOff ptr (base + 7)
            let w =
                  b0
                    `ior` (b1 `lshift` 8)
                    `ior` (b2 `lshift` 16)
                    `ior` (b3 `lshift` 24)
                    `ior` (b4 `lshift` 32)
                    `ior` (b5 `lshift` 40)
                    `ior` (b6 `lshift` 48)
                    `ior` (b7 `lshift` 56)
            pure (hash64WithSalt acc w)
        )
        (rangeS 0 numBlocks)
    writeRef outRef hBlocks
    whenM_ (tailRem `gt` 0) $ do
      tailW <-
        runDrain
          ( drainFoldM (0 :: e Word64) $ \tw k -> do
              bk <- toIntegral <$> peekElemOff ptr (tailStart + k)
              let sh = toIntegral (k `lshift` 3) :: e Word64
              pure (tw `ior` (bk `lshift` sh))
          )
          (rangeS 0 tailRem)
      writeRef outRef (hash64WithSalt hBlocks tailW)
  readRef outRef

hashBytes ::
  (CmdHashBytes m e) =>
  e (Ptr Word8) ->
  e Int64 ->
  m (e Word64)
hashBytes = hashBytesWithSalt defaultHashSalt
