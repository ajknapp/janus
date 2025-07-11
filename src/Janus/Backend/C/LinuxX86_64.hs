{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Janus.Backend.C.LinuxX86_64 where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import Data.Complex
import Data.Loc
import qualified Data.Map as Map
import Data.Proxy
import Data.Word
import Janus.Backend.C
import Janus.Command.Array
import Janus.Command.Case
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.TicToc
import Janus.Command.While
import Janus.Expression.Bits
import Janus.Expression.Bool
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
import Janus.Typed
import Language.C.Quote

newtype LinuxX86_64C a = LinuxX86_64C {getLinuxX86_64C :: JanusC a}

newtype LinuxX86_64CM a = LinuxX86_64CM {getLinuxX86_64CM :: JanusCM a}
  deriving (Functor, Applicative, Monad, MonadFix) via JanusCM

type instance JanusTyped LinuxX86_64C = JanusCTyped

deriving via JanusC a instance (JanusLitC a, Num a) => Num (LinuxX86_64C a)

deriving via JanusC a instance (JanusLitC a, Fractional a) => Fractional (LinuxX86_64C a)

deriving via JanusC Float instance Floating (LinuxX86_64C Float)

deriving via JanusC Double instance Floating (LinuxX86_64C Double)

deriving via JanusC (Complex Float) instance Floating (LinuxX86_64C (Complex Float))

deriving via JanusC (Complex Double) instance Floating (LinuxX86_64C (Complex Double))

deriving via JanusC instance ExpBool LinuxX86_64C

instance (ExpBits JanusC a) => ExpBits LinuxX86_64C a where
  iand = coerce (iand @JanusC @a)
  ior = coerce (ior @JanusC @a)
  ieor = coerce (ieor @JanusC @a)
  inot = coerce (inot @JanusC @a)
  lshift = coerce (lshift @JanusC @a)
  rshift = coerce (rshift @JanusC @a)

instance (ExpBoolCast JanusC a) => ExpBoolCast LinuxX86_64C a where
  toBool = coerce (toBool @JanusC @a)
  fromBool = coerce (fromBool @JanusC @a)

instance ExpIntegralCast JanusC a b => ExpIntegralCast LinuxX86_64C a b where
  toIntegral = coerce (toIntegral @JanusC @a @b)

instance (ExpFloatingCast JanusC a b) => ExpFloatingCast LinuxX86_64C a b where
  toFloating = coerce (toFloating @JanusC @a @b)

deriving via JanusC instance ExpCond LinuxX86_64C

instance (ExpEq JanusC a) => ExpEq LinuxX86_64C a where
  eq = coerce (eq @JanusC @a)
  ne = coerce (ne @JanusC @a)

instance (ExpExtract JanusC a) => ExpExtract LinuxX86_64C a where
  extract = coerce (extract @JanusC @a)

instance (ExpInject JanusC a) => ExpInject LinuxX86_64C a where
  inject = coerce (inject @JanusC @a)

instance (Num a, JanusLitC a, ExpIntegral JanusC a) => ExpIntegral LinuxX86_64C a where
  remainder = coerce (remainder @JanusC @a)
  quotient = coerce (quotient @JanusC @a)

deriving via JanusC instance ExpLet LinuxX86_64C

deriving via JanusC a instance (FMA (JanusC a), JanusLitC a, Num a) => FMA (LinuxX86_64C a)

deriving via JanusC a instance (Hypot (JanusC a), JanusLitC a, Floating (LinuxX86_64C a)) => Hypot (LinuxX86_64C a)

instance (ExpMurmurHash JanusC a) => ExpMurmurHash LinuxX86_64C a where
  murmurHashWithSalt = coerce (murmurHashWithSalt @JanusC @a)

instance (ExpOrd JanusC a) => ExpOrd LinuxX86_64C a where
  lt = coerce (lt @JanusC @a)
  le = coerce (le @JanusC @a)
  gt = coerce (gt @JanusC @a)
  ge = coerce (ge @JanusC @a)

instance (JanusCTyped a, ExpSized JanusC a) => ExpSized LinuxX86_64C a where
  sizeOf = coerce (sizeOf @JanusC @a)
  alignOf = coerce (alignOf @JanusC @a)

instance (ExpPtr JanusC a) => ExpPtr LinuxX86_64C a where
  nullPtr = coerce (nullPtr @JanusC @a)
  ptrAdd = coerce (ptrAdd @JanusC @a)
  ptrIndex = coerce (ptrIndex @JanusC @a)

instance CmdMem LinuxX86_64CM LinuxX86_64C where
  malloc = coerce (malloc @JanusCM @JanusC)
  calloc = coerce (calloc @JanusCM @JanusC)
  realloc = coerce (realloc @JanusCM @JanusC)
  free = coerce (free @JanusCM @JanusC)

instance (ExpSized LinuxX86_64C a, JanusCTyped a) => CmdStorable LinuxX86_64CM LinuxX86_64C a where
  peek = coerce (peek @JanusCM @JanusC @a)
  poke = coerce (poke @JanusCM @JanusC @a)
  peekElemOff = coerce (peekElemOff @JanusCM @JanusC @a)
  pokeElemOff = coerce (pokeElemOff @JanusCM @JanusC @a)

instance CmdCase LinuxX86_64CM LinuxX86_64C where
  switchCase_ = coerce (switchCase_ @JanusCM @JanusC)

instance CmdCond LinuxX86_64CM LinuxX86_64C where
  ifThenElseM_ = coerce (ifThenElseM_ @JanusCM @JanusC)

instance CmdIO LinuxX86_64CM LinuxX86_64C where
  stdout = coerce (stdout @JanusCM @JanusC)
  stderr = coerce (stderr @JanusCM @JanusC)
  fopen = coerce (fopen @JanusCM @JanusC)
  fclose = coerce (fclose @JanusCM @JanusC)

instance CmdString LinuxX86_64CM LinuxX86_64C where
  withString s f = LinuxX86_64CM (withString @JanusCM @JanusC s $ getLinuxX86_64CM . f . LinuxX86_64C)

instance CmdPutString LinuxX86_64CM LinuxX86_64C where
  hputString = coerce (hputString @JanusCM @JanusC)

instance (CmdFormat JanusCM JanusC a) => CmdFormat LinuxX86_64CM LinuxX86_64C a where
  hformat = coerce (hformat @JanusCM @JanusC @a)

newtype LinuxX86_64CRef a = LinuxX86_64CRef {getLinuxX86_64CRef :: JanusCRef a}

instance CmdRef LinuxX86_64CM LinuxX86_64C where
  type Ref LinuxX86_64CM LinuxX86_64C a = LinuxX86_64CRef a
  letM (LinuxX86_64C a) = LinuxX86_64CM $ LinuxX86_64C <$> letM a
  newRef (LinuxX86_64C a) = LinuxX86_64CM (LinuxX86_64CRef <$> newRef a)
  newRef' = LinuxX86_64CM (LinuxX86_64CRef <$> newRef')
  readRef (LinuxX86_64CRef r) = LinuxX86_64CM (LinuxX86_64C <$> readRef r)
  writeRef (LinuxX86_64CRef r) (LinuxX86_64C a) = LinuxX86_64CM (writeRef r a)

instance CmdRange LinuxX86_64CM LinuxX86_64C where
  rangeM = coerce (rangeM @JanusCM @JanusC)

instance CmdWhile LinuxX86_64CM LinuxX86_64C where
  whileM = coerce (whileM @JanusCM @JanusC)

--------------------------------------------------------------------------------

ticTocAsmBlock ::
  (LinuxX86_64C a -> b) -> String -> LinuxX86_64CM b
ticTocAsmBlock newty str = LinuxX86_64CM $ do
  fname <- askFuncName
  JCType spec dec <- getJanusCType (Proxy @Word64)
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> do
      let c = f ^. jcfVarCounter
          vid = Id ("x_" <> show c) noLoc
          ini = Init vid dec Nothing Nothing [] noLoc
          asm = Asm True [] (StringLit [show str] "" noLoc) [AsmOut Nothing "\"=a\"" vid] [] ["\"rdx\""] noLoc

          block = appendBlock (f ^. jcfBlock) [BlockDecl (InitGroup spec [] [ini] noLoc), BlockStm asm]
      put $
        jcs
          & at fname . _Just . jcfVarCounter +~ 1
          & at fname . _Just . jcfBlock .~ block
      pure $ newty $ LinuxX86_64C $ JanusC $ pure $ RVal (Var vid noLoc)
    Nothing -> error "tic: the impossible happened"

instance CmdTicToc LinuxX86_64CM LinuxX86_64C where
  tic = ticTocAsmBlock Tic "rdtsc\n\tlfence\n\tshl $32, %%rdx\n\tor %%rdx, %0"
  toc = ticTocAsmBlock Toc "rdtscp\n\tshl $32, %%rdx\n\tor %%rdx, %0"
