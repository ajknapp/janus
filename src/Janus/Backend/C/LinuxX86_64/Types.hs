{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Janus.Backend.C.LinuxX86_64.Types where

import Control.Lens
import Control.Monad.State (get, put)
import Data.Loc
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Word
import Janus.Backend.C.Raw
import Janus.Backend.C.Target
import Janus.Command.Ref (JanusCRefT)
import Janus.Command.TicToc
import Language.C.Quote

--------------------------------------------------------------------------------
-- LinuxX86_64 Target Descriptor & Type Aliases
--------------------------------------------------------------------------------

-- | Target tag for Linux x86-64 hosts with ISA feature gating (SSE2..AVX2/FMA/BMI2) and SLEEF vector math.
data LinuxX86_64

type LinuxX86_64C = JanusCT LinuxX86_64

type LinuxX86_64CM = JanusCMT LinuxX86_64

type LinuxX86_64CRef = JanusCRefT LinuxX86_64

askX86Features :: (MonadTargetFeatures m) => m X86Features
askX86Features = askCTargetFeatures @LinuxX86_64

instance JanusCTarget LinuxX86_64 where
  type CTargetFeatures LinuxX86_64 = X86Features
  cTargetFeatureKey _ = x86FeatureKey
  observeCTargetFeatures _ = observeHostX86Features
  ensureCTargetAdmitted _ req = do
    observed <- observeHostX86Features
    ensureX86Admitted observed req
  cTargetBanner _ fs =
    "/* janus-x86-target: " <> unwords (x86CompilerFlags fs) <> " */\n"
  cTargetBuildFlags _ fs funcs = do
    let needsSleef = any (Set.member "sleef.h" . _jcfHeaders) funcs
    (sleefCFlags, sleefLDFlags) <- if needsSleef then observeSleefFlags else pure ([], [])
    pure (x86CompilerFlags fs <> sleefCFlags, sleefLDFlags)

--------------------------------------------------------------------------------
-- Cycle-Counter Hardware Intrinsics (RDTSC / RDTSCP)
--------------------------------------------------------------------------------

ticTocAsmBlock ::
  (LinuxX86_64C a -> b) -> String -> [String] -> LinuxX86_64CM b
ticTocAsmBlock newty str clobbers = do
  fname <- askFuncName
  JCType spec dec <- getJanusCType (Proxy @Word64)
  jcs <- get
  case Map.lookup fname jcs of
    Just f -> do
      let c = f ^. jcfVarCounter
          vid = Id ("x_" <> show c) noLoc
          ini = Init vid dec Nothing Nothing [] noLoc
          asm = Asm True [] (StringLit [show str] "" noLoc) [AsmOut Nothing "\"=a\"" vid] [] clobbers noLoc
          block = appendBlock (f ^. jcfBlock) [BlockDecl (InitGroup spec [] [ini] noLoc), BlockStm asm]
      put $
        jcs
          & at fname . _Just . jcfVarCounter +~ 1
          & at fname . _Just . jcfBlock .~ block
      pure $ newty $ JanusC $ pure $ RVal (Var vid noLoc)
    Nothing -> error "tic: the impossible happened"

instance CmdTicToc LinuxX86_64CM LinuxX86_64C where
  tic = ticTocAsmBlock Tic "rdtsc\n\tlfence\n\tshl $32, %%rdx\n\tor %%rdx, %0" ["\"rdx\"", "\"cc\"", "\"memory\""]
  toc = ticTocAsmBlock Toc "rdtscp\n\tshl $32, %%rdx\n\tor %%rdx, %0" ["\"rdx\"", "\"rcx\"", "\"cc\"", "\"memory\""]
