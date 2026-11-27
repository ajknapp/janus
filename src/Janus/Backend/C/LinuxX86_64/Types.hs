{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Janus.Backend.C.LinuxX86_64.Types where

import qualified Data.Set as Set
import Janus.Backend.C.Quote (freshCVar, jcStms)
import Janus.Backend.C.Raw
import Janus.Backend.C.Target
import Janus.Command.Ref (JanusCRefT)
import Janus.Command.TicToc

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

instance CmdTicToc LinuxX86_64CM LinuxX86_64C where
  tic = do
    (vid, vexp) <- freshCVar "x"
    [jcStms|
      #include <stdint.h>
      uint64_t $id:vid;
      asm volatile ("rdtsc\n\tlfence\n\tshl $32, %%rdx\n\tor %%rdx, %0" : "=a" ($id:vid) : : "rdx", "cc", "memory");
    |]
    pure (Tic (JanusC (pure (RVal vexp))))
  toc = do
    (vid, vexp) <- freshCVar "x"
    [jcStms|
      #include <stdint.h>
      uint64_t $id:vid;
      asm volatile ("rdtscp\n\tshl $32, %%rdx\n\tor %%rdx, %0" : "=a" ($id:vid) : : "rdx", "rcx", "cc", "memory");
    |]
    pure (Toc (JanusC (pure (RVal vexp))))
