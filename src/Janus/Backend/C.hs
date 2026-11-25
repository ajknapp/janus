{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Target-polymorphic C backend for Janus.
--
-- All public code generation, compilation, and execution entrypoints in this
-- module ('prepareJanusCUnit', 'showJanusC', 'printJanusC', 'acquireJanusC',
-- 'compileJanusC', 'withJanusC', and their @*With@ / @*Named@ variants) take
-- @('CSEM' ('JanusCMT' t) ('JanusCT' t), 'CSEExp' ('JanusCT' t))@ programs and
-- run algorithmic Common Subexpression Elimination ('Janus.CSE') automatically
-- before emitting C source.
--
-- Low-level un-CSE'd AST emitters ('prepareRawJanusCUnit', 'showRawJanusC',
-- 'withRawJanusC', etc.) remain available via the re-exported
-- "Janus.Backend.C.Raw" definitions.
module Janus.Backend.C
  ( module Janus.Backend.C.Raw,

    -- * CSE-Backed C Expression, Command, & Reference Type Aliases
    JanusCTCSE,
    JanusCMTCSE,
    JanusCRefTCSE,
    JanusCCSE,
    JanusCMCSE,
    JanusCRefCSE,

    -- * Target-Polymorphic CSE-to-C Compilation & Evaluation Classes
    JanusCCSEParam,
    JanusCCSEEvaluate,
    JanusCCSEEval,
    janusCMangledSymbol,
    mkCTargetFeatureMap,

    -- * CSE-Backed Compilation, Inspection & Evaluation Entrypoints
    prepareJanusCUnit,
    showJanusCWith,
    showJanusC,
    printJanusCWith,
    printJanusC,
    acquireJanusCNamedWith,
    acquireJanusCWith,
    acquireJanusCNamed,
    acquireJanusC,
    compileJanusCNamedWith,
    compileJanusCWith,
    compileJanusCNamed,
    compileJanusC,
    withJanusCNamedWith,
    withJanusCWith,
    withJanusCNamed,
    withJanusC,
  )
where

import qualified Data.Dependent.HashMap as DHM
import Data.Functor.Identity (Identity (..))
import qualified Data.Kind as K
import Data.Proxy (Proxy (..))
import Foreign.ForeignPtr (ForeignPtr)
import Janus.Backend.C.Build (CBuildConfig)
import Janus.Backend.C.Raw
import Janus.Backend.C.Target (TargetFeatureMap)
import Janus.CSE
  ( CSEExp,
    CSEInterpret,
    CSEM,
    CSEOptimize,
    CSERef,
    CSETarget,
    withCSEWithFeatures,
  )
import Janus.Command.Ref ()
import Janus.SPMD.Host ()
import Janus.SPMD.Types (SPMD, SPMDM)

--------------------------------------------------------------------------------
-- CSE-Backed C Expression, Command, & Reference Type Aliases
--------------------------------------------------------------------------------

type JanusCTCSE t = CSEExp (JanusCT t)

type JanusCMTCSE t = CSEM (JanusCMT t) (JanusCT t)

type JanusCRefTCSE t = CSERef (JanusCMT t) (JanusCT t)

type JanusCCSE = CSEExp JanusC

type JanusCMCSE = CSEM JanusCM JanusC

type JanusCRefCSE = CSERef JanusCM JanusC

--------------------------------------------------------------------------------
-- Target-Polymorphic CSE-to-C Compilation & Evaluation Classes
--------------------------------------------------------------------------------

class
  (JanusCTarget t, CSEOptimize r, CSEInterpret r, JanusCParam t (CSETarget r)) =>
  JanusCCSEParam (t :: K.Type) (r :: K.Type)
    | r -> t

instance
  ( JanusCTarget t,
    CSEOptimize (CSEExp (JanusCT t) a),
    CSEInterpret (CSEExp (JanusCT t) a),
    JanusCParam t (CSETarget (CSEExp (JanusCT t) a))
  ) =>
  JanusCCSEParam t (CSEExp (JanusCT t) a)

instance
  ( JanusCTarget t,
    CSEOptimize (CSEM (JanusCMT t) (JanusCT t) res),
    CSEInterpret (CSEM (JanusCMT t) (JanusCT t) res),
    JanusCParam t (CSETarget (CSEM (JanusCMT t) (JanusCT t) res))
  ) =>
  JanusCCSEParam t (CSEM (JanusCMT t) (JanusCT t) res)

instance
  ( JanusCTarget t,
    CSEOptimize (SPMD n (CSEExp (JanusCT t)) a),
    CSEInterpret (SPMD n (CSEExp (JanusCT t)) a),
    JanusCParam t (CSETarget (SPMD n (CSEExp (JanusCT t)) a))
  ) =>
  JanusCCSEParam t (SPMD n (CSEExp (JanusCT t)) a)

instance
  ( JanusCTarget t,
    CSEOptimize (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res),
    CSEInterpret (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res),
    JanusCParam t (CSETarget (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res))
  ) =>
  JanusCCSEParam t (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res)

instance
  ( CSEOptimize (p -> r),
    CSEInterpret (p -> r),
    JanusCParam t (CSETarget (p -> r)),
    JanusCCSEParam t r
  ) =>
  JanusCCSEParam t (p -> r)

class
  (JanusCCSEParam t r, JanusCEvaluate t (CSETarget r)) =>
  JanusCCSEEvaluate (t :: K.Type) (r :: K.Type)
    | r -> t

instance
  ( JanusCCSEParam t (CSEExp (JanusCT t) a),
    JanusCEvaluate t (CSETarget (CSEExp (JanusCT t) a))
  ) =>
  JanusCCSEEvaluate t (CSEExp (JanusCT t) a)

instance
  ( JanusCCSEParam t (CSEM (JanusCMT t) (JanusCT t) res),
    JanusCEvaluate t (CSETarget (CSEM (JanusCMT t) (JanusCT t) res))
  ) =>
  JanusCCSEEvaluate t (CSEM (JanusCMT t) (JanusCT t) res)

instance
  ( JanusCCSEParam t (SPMD n (CSEExp (JanusCT t)) a),
    JanusCEvaluate t (CSETarget (SPMD n (CSEExp (JanusCT t)) a))
  ) =>
  JanusCCSEEvaluate t (SPMD n (CSEExp (JanusCT t)) a)

instance
  ( JanusCCSEParam t (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res),
    JanusCEvaluate t (CSETarget (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res))
  ) =>
  JanusCCSEEvaluate t (SPMDM n (CSEM (JanusCMT t) (JanusCT t)) (CSEExp (JanusCT t)) res)

instance
  ( JanusCCSEParam t (p -> r),
    JanusCEvaluate t (CSETarget (p -> r)),
    JanusCCSEEvaluate t r
  ) =>
  JanusCCSEEvaluate t (p -> r)

type JanusCCSEEval r = JanusCEval (CSETarget r)

janusCMangledSymbol :: forall t r. (JanusCCSEParam t r) => Proxy r -> String -> String
janusCMangledSymbol _ = jcMangledSymbol @t @(CSETarget r) (Proxy @(CSETarget r))

mkCTargetFeatureMap :: forall t. (JanusCTarget t) => CTargetFeatures t -> TargetFeatureMap
mkCTargetFeatureMap fs = DHM.singleton (cTargetFeatureKey (Proxy @t)) (Identity fs)

--------------------------------------------------------------------------------
-- CSE-Backed Compilation, Inspection & Evaluation Entrypoints
--------------------------------------------------------------------------------

prepareJanusCUnit ::
  forall t r.
  (JanusCCSEParam t r) =>
  CTargetFeatures t ->
  String ->
  r ->
  IO (String, [String], [String])
prepareJanusCUnit reqFeatures name prog =
  prepareRawJanusCUnit @t @(CSETarget r)
    reqFeatures
    name
    (withCSEWithFeatures (mkCTargetFeatureMap @t reqFeatures) prog)

showJanusCWith :: forall t r. (JanusCCSEParam t r) => CTargetFeatures t -> String -> r -> String
showJanusCWith fs name a =
  showRawJanusCWith @t @(CSETarget r)
    fs
    name
    (withCSEWithFeatures (mkCTargetFeatureMap @t fs) a)

showJanusC :: forall t r. (JanusCCSEParam t r) => String -> r -> IO String
showJanusC name a = do
  fs <- observeCTargetFeatures (Proxy @t)
  pure (showJanusCWith @t fs name a)

printJanusCWith :: forall t r. (JanusCCSEParam t r) => CTargetFeatures t -> r -> IO ()
printJanusCWith fs = putStrLn . showJanusCWith @t fs "janus_main"

printJanusC :: forall t r. (JanusCCSEParam t r) => r -> IO ()
printJanusC a = showJanusC @t "janus_main" a >>= putStrLn

acquireJanusCNamedWith ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r, ForeignPtr ())
acquireJanusCNamedWith name reqFeatures config a =
  acquireRawJanusCNamedWith @t @(CSETarget r)
    name
    reqFeatures
    config
    (withCSEWithFeatures (mkCTargetFeatureMap @t reqFeatures) a)

acquireJanusCWith ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r, ForeignPtr ())
acquireJanusCWith = acquireJanusCNamedWith "janus_main"

acquireJanusCNamed ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r, ForeignPtr ())
acquireJanusCNamed name config a = do
  fs <- resolveCTargetFeatures (Proxy @t) config
  acquireJanusCNamedWith name fs config a

acquireJanusC ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r, ForeignPtr ())
acquireJanusC = acquireJanusCNamed "janus_main"

compileJanusCNamedWith ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r)
compileJanusCNamedWith name reqFeatures config a =
  fst <$> acquireJanusCNamedWith name reqFeatures config a

compileJanusCWith ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r)
compileJanusCWith = compileJanusCNamedWith "janus_main"

compileJanusCNamed ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r)
compileJanusCNamed name config a =
  fst <$> acquireJanusCNamed name config a

compileJanusC ::
  forall t r.
  (JanusCCSEEvaluate t r) =>
  CBuildConfig ->
  r ->
  IO (JanusCCSEEval r)
compileJanusC = compileJanusCNamed "janus_main"

withJanusCNamedWith ::
  (JanusCCSEEvaluate t r) =>
  String ->
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  (JanusCCSEEval r -> IO a) ->
  IO a
withJanusCNamedWith name fs config a k =
  compileJanusCNamedWith name fs config a >>= k

withJanusCWith ::
  (JanusCCSEEvaluate t r) =>
  CTargetFeatures t ->
  CBuildConfig ->
  r ->
  (JanusCCSEEval r -> IO a) ->
  IO a
withJanusCWith = withJanusCNamedWith "janus_main"

withJanusCNamed ::
  (JanusCCSEEvaluate t r) =>
  String ->
  CBuildConfig ->
  r ->
  (JanusCCSEEval r -> IO a) ->
  IO a
withJanusCNamed name config a k =
  compileJanusCNamed name config a >>= k

withJanusC ::
  (JanusCCSEEvaluate t r) =>
  CBuildConfig ->
  r ->
  (JanusCCSEEval r -> IO a) ->
  IO a
withJanusC = withJanusCNamed "janus_main"
