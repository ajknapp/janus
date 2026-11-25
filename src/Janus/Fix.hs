{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-cse #-}

module Janus.Fix
  ( -- * Polymorphic Local Function Fixpoint Class
    ExpFix (..),
    jfix,

    -- * Composable C Local Function Parameter, Field, & Return Classes
    JanusCFixElem (..),
    JanusCFixField (..),
    GJanusCFixLeaves (..),
    JanusCFixParam (..),
    JanusCFixMonRet (..),
    JanusCFix (..),

    -- * Flat Heterogeneous Local Arguments (`CmdLocalFun` Backing)
    LocalArgs (..),
    mapLocalArgs,
    localArgsTypeReps,
    jfixLocalArgsM,
    jfixLocalArgsM_,
  )
where

import Control.Lens hiding (from, to)
import Control.Monad (unless)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (gets, modify)
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 (unpack)
import Data.Function (fix)
import Data.Kind as K
import Data.Loc (noLoc)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (TypeRep, Typeable, typeRep)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import GHC.Generics
import Janus.Backend.C.Raw
import Language.C.Quote
import Text.PrettyPrint.Mainland (pretty)
import Text.PrettyPrint.Mainland.Class (ppr)

--------------------------------------------------------------------------------
-- Leaf C Element Types Supported in `JanusCFix` (Scalars, Pointers, Structs, `Ptr ()`)
--------------------------------------------------------------------------------

class JanusCFixElem (a :: K.Type) where
  jcfixElemType :: Proxy a -> JanusCMT s JCType
  jcfixElemTypeRep :: Proxy a -> TypeRep

instance {-# OVERLAPPABLE #-} (JanusCTyped a) => JanusCFixElem a where
  jcfixElemType = getJanusCType
  jcfixElemTypeRep = typeRep

instance {-# OVERLAPPING #-} JanusCFixElem (Ptr ()) where
  jcfixElemType _ = pure voidPtrType
  jcfixElemTypeRep _ = typeRep (Proxy @(Ptr ()))

localFun :: JCFuncInfo -> JanusCMT t a -> JanusCMT t a
localFun x = local (const x)

--------------------------------------------------------------------------------
-- Generic Leaf Decomposition for Handles, `Buf` Wrappers, & `HKD` Records
--------------------------------------------------------------------------------

class JanusCFixField (t :: K.Type) (field :: K.Type) where
  jcfixFieldTypes :: Proxy t -> Proxy field -> [TypeRep]
  jcfixFieldElemCTypes :: Proxy t -> Proxy field -> JanusCM [JCType]
  jcfixFieldExtractExps :: Proxy t -> field -> JanusCM [Exp]
  jcfixFieldMakeParams :: Proxy t -> Int -> (Int, JanusCM [Param], field)
  jcfixFieldRebuild :: Proxy t -> [Exp] -> (field, [Exp])

instance (JanusCFixElem a) => JanusCFixField t (JanusCT t a) where
  jcfixFieldTypes _ _ = [jcfixElemTypeRep (Proxy @a)]
  jcfixFieldElemCTypes _ _ = pure <$> jcfixElemType (Proxy @a)
  jcfixFieldExtractExps _ (JanusC m) = do
    RVal e <- m
    pure [e]
  jcfixFieldMakeParams _ n =
    let aid = mkArgId n
        mkP = do
          JCType spec dec <- jcfixElemType (Proxy @a)
          pure [Param (Just aid) spec dec noLoc]
     in (n + 1, mkP, JanusC (pure (RVal (Var aid noLoc))))
  jcfixFieldRebuild _ (e : rest) = (JanusC (pure (RVal e)), rest)
  jcfixFieldRebuild _ [] = error "jcfixFieldRebuild: insufficient expressions"

instance
  (Generic (sub (JanusCT t)), GJanusCFixLeaves t (Rep (sub (JanusCT t)))) =>
  JanusCFixField t (sub (JanusCT t))
  where
  jcfixFieldTypes pt _ = gFixLeafTypes pt (Proxy @(Rep (sub (JanusCT t))))
  jcfixFieldElemCTypes pt _ = gFixLeafCTypes pt (Proxy @(Rep (sub (JanusCT t))))
  jcfixFieldExtractExps pt s = gFixExtractExps pt (from s)
  jcfixFieldMakeParams pt n0 =
    let (n1, mp, repS) = gFixMakeParams @t @(Rep (sub (JanusCT t))) pt n0
     in (n1, mp, to repS)
  jcfixFieldRebuild pt es0 =
    let (repS, es1) = gFixRebuild @t @(Rep (sub (JanusCT t))) pt es0
     in (to repS, es1)

class GJanusCFixLeaves (t :: K.Type) (rep :: K.Type -> K.Type) where
  gFixLeafTypes :: Proxy t -> Proxy rep -> [TypeRep]
  gFixLeafCTypes :: Proxy t -> Proxy rep -> JanusCM [JCType]
  gFixExtractExps :: Proxy t -> rep p -> JanusCM [Exp]
  gFixMakeParams :: Proxy t -> Int -> (Int, JanusCM [Param], rep p)
  gFixRebuild :: Proxy t -> [Exp] -> (rep p, [Exp])

instance (GJanusCFixLeaves t f) => GJanusCFixLeaves t (M1 i c f) where
  gFixLeafTypes pt _ = gFixLeafTypes pt (Proxy @f)
  gFixLeafCTypes pt _ = gFixLeafCTypes pt (Proxy @f)
  gFixExtractExps pt (M1 x) = gFixExtractExps pt x
  gFixMakeParams pt n0 =
    let (n1, mp, r) = gFixMakeParams @t @f pt n0
     in (n1, mp, M1 r)
  gFixRebuild pt es =
    let (r, rest) = gFixRebuild @t @f pt es
     in (M1 r, rest)

instance GJanusCFixLeaves t U1 where
  gFixLeafTypes _ _ = []
  gFixLeafCTypes _ _ = pure []
  gFixExtractExps _ U1 = pure []
  gFixMakeParams _ n = (n, pure [], U1)
  gFixRebuild _ es = (U1, es)

instance (GJanusCFixLeaves t f, GJanusCFixLeaves t g) => GJanusCFixLeaves t (f :*: g) where
  gFixLeafTypes pt _ = gFixLeafTypes pt (Proxy @f) <> gFixLeafTypes pt (Proxy @g)
  gFixLeafCTypes pt _ = (<>) <$> gFixLeafCTypes pt (Proxy @f) <*> gFixLeafCTypes pt (Proxy @g)
  gFixExtractExps pt (fx :*: gx) = (<>) <$> gFixExtractExps pt fx <*> gFixExtractExps pt gx
  gFixMakeParams pt n0 =
    let (n1, mp1, rf) = gFixMakeParams @t @f pt n0
        (n2, mp2, rg) = gFixMakeParams @t @g pt n1
     in (n2, (<>) <$> mp1 <*> mp2, rf :*: rg)
  gFixRebuild pt es0 =
    let (rf, es1) = gFixRebuild @t @f pt es0
        (rg, es2) = gFixRebuild @t @g pt es1
     in (rf :*: rg, es2)

instance (JanusCFixField t field) => GJanusCFixLeaves t (K1 i field) where
  gFixLeafTypes pt _ = jcfixFieldTypes pt (Proxy @field)
  gFixLeafCTypes pt _ = jcfixFieldElemCTypes pt (Proxy @field)
  gFixExtractExps pt (K1 x) = jcfixFieldExtractExps pt x
  gFixMakeParams pt n0 =
    let (n1, mp, x) = jcfixFieldMakeParams @t @field pt n0
     in (n1, mp, K1 x)
  gFixRebuild pt es0 =
    let (x, es1) = jcfixFieldRebuild @t @field pt es0
     in (K1 x, es1)

--------------------------------------------------------------------------------
-- Composable Parameter & Monadic Return Classes for `JanusCFix`
--------------------------------------------------------------------------------

class JanusCFixParam (t :: K.Type) (p :: K.Type) where
  jcfixParamTypes :: Proxy t -> Proxy p -> [TypeRep]
  default jcfixParamTypes ::
    (GJanusCFixLeaves t (Rep p)) =>
    Proxy t ->
    Proxy p ->
    [TypeRep]
  jcfixParamTypes pt _ = gFixLeafTypes pt (Proxy @(Rep p))

  jcfixExtractArgs :: Proxy t -> p -> JanusCM [Exp]
  default jcfixExtractArgs ::
    (Generic p, GJanusCFixLeaves t (Rep p)) =>
    Proxy t ->
    p ->
    JanusCM [Exp]
  jcfixExtractArgs pt x = gFixExtractExps pt (from x)

  jcfixMakeFormal :: Proxy t -> Int -> (Int, JanusCM [Param], p)
  default jcfixMakeFormal ::
    (Generic p, GJanusCFixLeaves t (Rep p)) =>
    Proxy t ->
    Int ->
    (Int, JanusCM [Param], p)
  jcfixMakeFormal pt n0 =
    let (n1, mp, repP) = gFixMakeParams @t @(Rep p) pt n0
     in (n1, mp, to repP)

instance (JanusCFixElem a) => JanusCFixParam t (JanusCT t a) where
  jcfixParamTypes _ _ = [jcfixElemTypeRep (Proxy @a)]
  jcfixExtractArgs _ (JanusC m) = do
    RVal e <- m
    pure [e]
  jcfixMakeFormal _ n =
    let aid = mkArgId n
        mkP = do
          JCType spec dec <- jcfixElemType (Proxy @a)
          pure [Param (Just aid) spec dec noLoc]
     in (n + 1, mkP, JanusC (pure (RVal (Var aid noLoc))))

instance JanusCFixParam t () where
  jcfixParamTypes _ _ = []
  jcfixExtractArgs _ () = pure []
  jcfixMakeFormal _ n = (n, pure [], ())

instance (JanusCFixParam t p1, JanusCFixParam t p2) => JanusCFixParam t (p1, p2) where
  jcfixParamTypes pt _ = jcfixParamTypes pt (Proxy @p1) <> jcfixParamTypes pt (Proxy @p2)
  jcfixExtractArgs pt (a, b) = (<>) <$> jcfixExtractArgs pt a <*> jcfixExtractArgs pt b
  jcfixMakeFormal pt n0 =
    let (n1, mp1, f1) = jcfixMakeFormal @t @p1 pt n0
        (n2, mp2, f2) = jcfixMakeFormal @t @p2 pt n1
     in (n2, (<>) <$> mp1 <*> mp2, (f1, f2))

instance
  (JanusCFixParam t p1, JanusCFixParam t p2, JanusCFixParam t p3) =>
  JanusCFixParam t (p1, p2, p3)
  where
  jcfixParamTypes pt _ =
    jcfixParamTypes pt (Proxy @p1) <> jcfixParamTypes pt (Proxy @p2) <> jcfixParamTypes pt (Proxy @p3)
  jcfixExtractArgs pt (a, b, c) =
    (\x y z -> x <> y <> z)
      <$> jcfixExtractArgs pt a
      <*> jcfixExtractArgs pt b
      <*> jcfixExtractArgs pt c
  jcfixMakeFormal pt n0 =
    let (n1, mp1, f1) = jcfixMakeFormal @t @p1 pt n0
        (n2, mp2, f2) = jcfixMakeFormal @t @p2 pt n1
        (n3, mp3, f3) = jcfixMakeFormal @t @p3 pt n2
     in (n3, (\x y z -> x <> y <> z) <$> mp1 <*> mp2 <*> mp3, (f1, f2, f3))

instance
  (JanusCFixParam t p1, JanusCFixParam t p2, JanusCFixParam t p3, JanusCFixParam t p4) =>
  JanusCFixParam t (p1, p2, p3, p4)
  where
  jcfixParamTypes pt _ =
    jcfixParamTypes pt (Proxy @p1)
      <> jcfixParamTypes pt (Proxy @p2)
      <> jcfixParamTypes pt (Proxy @p3)
      <> jcfixParamTypes pt (Proxy @p4)
  jcfixExtractArgs pt (a, b, c, d) =
    (\w x y z -> w <> x <> y <> z)
      <$> jcfixExtractArgs pt a
      <*> jcfixExtractArgs pt b
      <*> jcfixExtractArgs pt c
      <*> jcfixExtractArgs pt d
  jcfixMakeFormal pt n0 =
    let (n1, mp1, f1) = jcfixMakeFormal @t @p1 pt n0
        (n2, mp2, f2) = jcfixMakeFormal @t @p2 pt n1
        (n3, mp3, f3) = jcfixMakeFormal @t @p3 pt n2
        (n4, mp4, f4) = jcfixMakeFormal @t @p4 pt n3
     in (n4, (\w x y z -> w <> x <> y <> z) <$> mp1 <*> mp2 <*> mp3 <*> mp4, (f1, f2, f3, f4))

class JanusCFixMonRet (t :: K.Type) (res :: K.Type) where
  jcfixRetTypes :: Proxy t -> Proxy res -> [TypeRep]
  default jcfixRetTypes ::
    (GJanusCFixLeaves t (Rep res)) =>
    Proxy t ->
    Proxy res ->
    [TypeRep]
  jcfixRetTypes pt _ = gFixLeafTypes pt (Proxy @(Rep res))

  jcfixRetElemCTypes :: Proxy t -> Proxy res -> JanusCM [JCType]
  default jcfixRetElemCTypes ::
    (GJanusCFixLeaves t (Rep res)) =>
    Proxy t ->
    Proxy res ->
    JanusCM [JCType]
  jcfixRetElemCTypes pt _ = gFixLeafCTypes pt (Proxy @(Rep res))

  jcfixExtractRetExps :: Proxy t -> res -> JanusCM [Exp]
  default jcfixExtractRetExps ::
    (Generic res, GJanusCFixLeaves t (Rep res)) =>
    Proxy t ->
    res ->
    JanusCM [Exp]
  jcfixExtractRetExps pt x = gFixExtractExps pt (from x)

  jcfixRebuildRet :: Proxy t -> [Exp] -> (res, [Exp])
  default jcfixRebuildRet ::
    (Generic res, GJanusCFixLeaves t (Rep res)) =>
    Proxy t ->
    [Exp] ->
    (res, [Exp])
  jcfixRebuildRet pt es =
    let (repR, rest) = gFixRebuild @t @(Rep res) pt es
     in (to repR, rest)

instance JanusCFixMonRet t () where
  jcfixRetTypes _ _ = []
  jcfixRetElemCTypes _ _ = pure []
  jcfixExtractRetExps _ () = pure []
  jcfixRebuildRet _ es = ((), es)

instance (JanusCFixElem a) => JanusCFixMonRet t (JanusCT t a) where
  jcfixRetTypes _ _ = [jcfixElemTypeRep (Proxy @a)]
  jcfixRetElemCTypes _ _ = pure <$> jcfixElemType (Proxy @a)
  jcfixExtractRetExps _ (JanusC m) = do
    RVal e <- m
    pure [e]
  jcfixRebuildRet _ (e : rest) = (JanusC (pure (RVal e)), rest)
  jcfixRebuildRet _ [] = error "jcfixRebuildRet: insufficient expressions"

instance (JanusCFixMonRet t r1, JanusCFixMonRet t r2) => JanusCFixMonRet t (r1, r2) where
  jcfixRetTypes pt _ = jcfixRetTypes pt (Proxy @r1) <> jcfixRetTypes pt (Proxy @r2)
  jcfixRetElemCTypes pt _ = (<>) <$> jcfixRetElemCTypes pt (Proxy @r1) <*> jcfixRetElemCTypes pt (Proxy @r2)
  jcfixExtractRetExps pt (a, b) = (<>) <$> jcfixExtractRetExps pt a <*> jcfixExtractRetExps pt b
  jcfixRebuildRet pt es0 =
    let (r1, es1) = jcfixRebuildRet @t @r1 pt es0
        (r2, es2) = jcfixRebuildRet @t @r2 pt es1
     in ((r1, r2), es2)

instance
  (JanusCFixMonRet t r1, JanusCFixMonRet t r2, JanusCFixMonRet t r3) =>
  JanusCFixMonRet t (r1, r2, r3)
  where
  jcfixRetTypes pt _ =
    jcfixRetTypes pt (Proxy @r1) <> jcfixRetTypes pt (Proxy @r2) <> jcfixRetTypes pt (Proxy @r3)
  jcfixRetElemCTypes pt _ =
    (\a b c -> a <> b <> c)
      <$> jcfixRetElemCTypes pt (Proxy @r1)
      <*> jcfixRetElemCTypes pt (Proxy @r2)
      <*> jcfixRetElemCTypes pt (Proxy @r3)
  jcfixExtractRetExps pt (a, b, c) =
    (\x y z -> x <> y <> z)
      <$> jcfixExtractRetExps pt a
      <*> jcfixExtractRetExps pt b
      <*> jcfixExtractRetExps pt c
  jcfixRebuildRet pt es0 =
    let (r1, es1) = jcfixRebuildRet @t @r1 pt es0
        (r2, es2) = jcfixRebuildRet @t @r2 pt es1
        (r3, es3) = jcfixRebuildRet @t @r3 pt es2
     in ((r1, r2, r3), es3)

instance
  (JanusCFixMonRet t r1, JanusCFixMonRet t r2, JanusCFixMonRet t r3, JanusCFixMonRet t r4) =>
  JanusCFixMonRet t (r1, r2, r3, r4)
  where
  jcfixRetTypes pt _ =
    jcfixRetTypes pt (Proxy @r1)
      <> jcfixRetTypes pt (Proxy @r2)
      <> jcfixRetTypes pt (Proxy @r3)
      <> jcfixRetTypes pt (Proxy @r4)
  jcfixRetElemCTypes pt _ =
    (\a b c d -> a <> b <> c <> d)
      <$> jcfixRetElemCTypes pt (Proxy @r1)
      <*> jcfixRetElemCTypes pt (Proxy @r2)
      <*> jcfixRetElemCTypes pt (Proxy @r3)
      <*> jcfixRetElemCTypes pt (Proxy @r4)
  jcfixExtractRetExps pt (a, b, c, d) =
    (\w x y z -> w <> x <> y <> z)
      <$> jcfixExtractRetExps pt a
      <*> jcfixExtractRetExps pt b
      <*> jcfixExtractRetExps pt c
      <*> jcfixExtractRetExps pt d
  jcfixRebuildRet pt es0 =
    let (r1, es1) = jcfixRebuildRet @t @r1 pt es0
        (r2, es2) = jcfixRebuildRet @t @r2 pt es1
        (r3, es3) = jcfixRebuildRet @t @r3 pt es2
        (r4, es4) = jcfixRebuildRet @t @r4 pt es3
     in ((r1, r2, r3, r4), es4)

--------------------------------------------------------------------------------
-- Multi-Leaf Return Struct Typedef & Monadic Call Emission Helpers
--------------------------------------------------------------------------------

getMultiRetStructType :: [JCType] -> JanusCM JCType
getMultiRetStructType elemTys = do
  fname <- askFuncName
  _ <- getFunction
  let fgs =
        [ FieldGroup sp [Field (Just (Id ("f_" <> show i) noLoc)) (Just dc) Nothing noLoc] noLoc
          | (i, JCType sp dc) <- zip [0 :: Int ..] elemTys
        ]
      spec = DeclSpec [] [] (Tstruct Nothing (Just fgs) [] noLoc) noLoc
      specRendered = pretty 120 (ppr spec)
      specSha = SHA256.hash (Text.encodeUtf8 (Text.pack specRendered))
      tyname = "tuple_" <> take 16 (unpack (B16.encode specSha))
      tyId = Id tyname noLoc
      spec' = DeclSpec [] [] (Tnamed tyId [] noLoc) noLoc
      td = JCTypedef spec tyname
  modify $ \s ->
    s
      & ix fname . jcfTypedefs %~ (\tds -> if td `elem` tds then tds else tds <> [td])
  pure (JCType spec' (DeclRoot noLoc))

finishMultiLeafFunction :: JCType -> [Exp] -> JanusCMT t ()
finishMultiLeafFunction (JCType spec dec) fieldExps = do
  fn <- getFunction
  fname <- askFuncName
  tqs <- askTypeQuals
  let c = fn ^. jcfVarCounter
      retId = Id ("x_" <> show c) noLoc
      declItem = BlockDecl $ InitGroup spec [] [Init retId dec Nothing Nothing [] noLoc] noLoc
      assignItems =
        [ BlockStm $
            Exp
              ( Just $
                  Assign
                    (Member (Var retId noLoc) (Id ("f_" <> show i) noLoc) noLoc)
                    JustAssign
                    fe
                    noLoc
              )
              noLoc
          | (i, fe) <- zip [0 :: Int ..] fieldExps
        ]
      fn' =
        fn
          & jcfBlock %~ flip appendBlock (declItem : assignItems)
          & jcfVarCounter +~ 1
          & jcfExp ?~ Var retId noLoc
          & jcfType ?~ JCType (setTypeQualifiers tqs spec) dec
  modify $ \s -> s & ix fname .~ fn'

emitMonadicCallSiteWith ::
  forall t res.
  JanusCM [JCType] ->
  ([Exp] -> res) ->
  String ->
  JCFuncInfo ->
  Maybe Params ->
  JanusCM [Exp] ->
  JanusCMT t res
emitMonadicCallSiteWith retElemTysM rebuildRetFn name funInfo mbParams argsM = do
  callerElemTys <- liftJanusCM retElemTysM
  args' <- liftJanusCM argsM
  let callExp = FnCall (Var (Id name noLoc) noLoc) args' noLoc
      emitProto (JCType rSpec rDec) =
        case mbParams of
          Nothing -> pure ()
          Just params'' -> do
            let proto =
                  InitGroup
                    (setTypeQualifiers (jcTypeQuals funInfo) rSpec)
                    []
                    [Init (Id name noLoc) (Proto rDec params'' noLoc) Nothing Nothing [] noLoc]
                    noLoc
            modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
  case callerElemTys of
    [] -> do
      emitProto (JCType (DeclSpec [] [] (Tvoid noLoc) noLoc) (DeclRoot noLoc))
      modifyFunction $ \fn -> fn & jcfBlock %~ flip appendBlock [BlockStm $ Exp (Just callExp) noLoc]
      pure (rebuildRetFn [])
    [ty0] -> do
      emitProto ty0
      v <- emitLocalVal ty0 callExp
      pure (rebuildRetFn [v])
    _ -> do
      structTy <- liftJanusCM (getMultiRetStructType callerElemTys)
      emitProto structTy
      v <- emitLocalVal structTy callExp
      let projExps =
            [ Member v (Id ("f_" <> show i) noLoc) noLoc
              | i <- [0 .. length callerElemTys - 1]
            ]
      pure (rebuildRetFn projExps)

emitMonadicCallSite ::
  forall t res.
  (JanusCFixMonRet t res) =>
  String ->
  JCFuncInfo ->
  Maybe Params ->
  JanusCM [Exp] ->
  JanusCMT t res
emitMonadicCallSite =
  let pt = Proxy @t
      pres = Proxy @res
   in emitMonadicCallSiteWith
        (jcfixRetElemCTypes pt pres)
        (fst . jcfixRebuildRet pt)

defineMonadicLocalBodyWith ::
  forall t res.
  JanusCM [JCType] ->
  (res -> JanusCM [Exp]) ->
  String ->
  JCFuncInfo ->
  JanusCM [Param] ->
  JanusCMT t res ->
  JanusCMT t Params
defineMonadicLocalBodyWith retElemTysM extractRetFn name funInfo paramsM body = do
  -- Ensure parameter types & headers are also registered in the caller's JCFunc
  _ <- liftJanusCM paramsM
  alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup name)
  localParams <- liftJanusCM (localFun funInfo paramsM)
  let params'' = Params localParams False noLoc
  unless alreadyDefined $
    localFun funInfo $ do
      _ <- getFunction
      modify $ \s -> s & ix name . jcfParams .~ params''
      resVal <- body
      localElemTys <- liftJanusCM retElemTysM
      retExps <- liftJanusCM (extractRetFn resVal)
      case (localElemTys, retExps) of
        ([], _) -> finishFunction_
        ([JCType sp dc], [e0]) -> do
          tqs <- askTypeQuals
          modifyFunction $ \f ->
            f
              & jcfExp ?~ e0
              & jcfType ?~ JCType (setTypeQualifiers tqs sp) dc
        (_, es) -> do
          structTy <- liftJanusCM (getMultiRetStructType localElemTys)
          finishMultiLeafFunction structTy es
  pure params''

defineMonadicLocalBody ::
  forall t res.
  (JanusCFixMonRet t res) =>
  String ->
  JCFuncInfo ->
  JanusCM [Param] ->
  JanusCMT t res ->
  JanusCMT t Params
defineMonadicLocalBody =
  let pt = Proxy @t
      pres = Proxy @res
   in defineMonadicLocalBodyWith
        (jcfixRetElemCTypes pt pres)
        (jcfixExtractRetExps pt)

--------------------------------------------------------------------------------
-- Curried `JanusCFix` Class & Instances
--------------------------------------------------------------------------------

class JanusCFix (t :: K.Type) (r :: K.Type) | r -> t where
  jcfixSigTypes :: Proxy r -> ([TypeRep], [TypeRep])
  jcfun :: String -> JanusCM [Exp] -> JanusCM [Param] -> Int -> r -> r
  jcfake :: Proxy r -> String -> JanusCM [Exp] -> r

instance (JanusCFixMonRet t res) => JanusCFix t (JanusCMT t res) where
  jcfixSigTypes _ = ([], jcfixRetTypes (Proxy @t) (Proxy @res))
  jcfun name argsM paramsM _ body = do
    fi <- ask
    let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
    params'' <- defineMonadicLocalBody @t @res name funInfo paramsM body
    emitMonadicCallSite @t @res name funInfo (Just params'') argsM
  jcfake _ name argsM = do
    fi <- ask
    let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
    emitMonadicCallSite @t @res name funInfo Nothing argsM

instance (JanusCFixElem a) => JanusCFix t (JanusCT t a) where
  jcfixSigTypes _ = ([], [jcfixElemTypeRep (Proxy @a)])
  jcfun name argsM paramsM _ body = JanusC $ do
    fi <- ask
    let funInfo = fi & jcfiName .~ name & jcfiIsLocal .~ True
    -- Register parameter typedefs/headers in caller as well as local function
    _ <- paramsM
    alreadyDefined <- gets (maybe False (isJust . _jcfType) . Map.lookup name)
    localParams <- localFun funInfo paramsM
    let params'' = Params localParams False noLoc
    unless alreadyDefined $
      localFun funInfo $ do
        _ <- getFunction
        modify $ \s -> s & ix name . jcfParams .~ params''
        JCType lSpec lDec <- jcfixElemType (Proxy @a)
        RVal bodyExp <- getJanusC body
        tqs <- askTypeQuals
        modifyFunction $ \f ->
          f
            & jcfExp ?~ bodyExp
            & jcfType ?~ JCType (setTypeQualifiers tqs lSpec) lDec
    JCType spec dec <- jcfixElemType (Proxy @a)
    let proto =
          InitGroup
            (setTypeQualifiers (jcTypeQuals funInfo) spec)
            []
            [Init (Id name noLoc) (Proto dec params'' noLoc) Nothing Nothing [] noLoc]
            noLoc
    modifyFunction $ \f -> f & jcfProtos %~ Set.insert proto
    args' <- argsM
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) args' noLoc
  jcfake _ name argsM = JanusC $ do
    args' <- argsM
    pure $ RVal $ FnCall (Var (Id name noLoc) noLoc) args' noLoc

instance (JanusCFixParam t p, JanusCFix t r) => JanusCFix t (p -> r) where
  jcfixSigTypes _ =
    let (inRest, outTys) = jcfixSigTypes @t @r (Proxy @r)
     in (jcfixParamTypes (Proxy @t) (Proxy @p) <> inRest, outTys)
  jcfun name argsM paramsM n body x =
    let (n', mkPs, formalX) = jcfixMakeFormal @t @p (Proxy @t) n
        argsM' = (<>) <$> argsM <*> jcfixExtractArgs (Proxy @t) x
        paramsM' = (<>) <$> paramsM <*> mkPs
     in jcfun name argsM' paramsM' n' (body formalX)
  jcfake _ name argsM x =
    jcfake (Proxy @r) name ((<>) <$> argsM <*> jcfixExtractArgs (Proxy @t) x)

jfix ::
  forall t r.
  (JanusCFix t r) =>
  String ->
  (r -> r) ->
  r
jfix name f =
  let (inTys, outTys) = jcfixSigTypes @t @r (Proxy @r)
      mangled = mangleJanusLocalName name inTys outTys
   in jcfun mangled (pure []) (pure []) 0 (f (jcfake (Proxy @r) mangled (pure [])))

--------------------------------------------------------------------------------
-- Flat Heterogeneous Local Arguments (`CmdLocalFun` Backing)
--------------------------------------------------------------------------------

data LocalArgs (e :: K.Type -> K.Type) (ts :: [K.Type]) where
  LANil :: LocalArgs e '[]
  LACons ::
    (Typeable a, Storable a, JanusCTyped a) =>
    !(e a) ->
    !(LocalArgs e ts) ->
    LocalArgs e (a ': ts)

mapLocalArgs :: (forall x. e x -> f x) -> LocalArgs e ts -> LocalArgs f ts
mapLocalArgs _ LANil = LANil
mapLocalArgs fn (LACons x rest) = LACons (fn x) (mapLocalArgs fn rest)

localArgsTypeReps :: LocalArgs e ts -> [TypeRep]
localArgsTypeReps LANil = []
localArgsTypeReps (LACons (_ :: e a) rest) = typeRep (Proxy @a) : localArgsTypeReps rest

extractLocalArgsC :: LocalArgs (JanusCT t) ts -> JanusCM [Exp]
extractLocalArgsC LANil = pure []
extractLocalArgsC (LACons (JanusC m) rest) = do
  RVal e <- m
  es <- extractLocalArgsC rest
  pure (e : es)

makeFormalLocalArgsC :: forall t proxy ts. Int -> LocalArgs proxy ts -> (Int, JanusCM [Param], LocalArgs (JanusCT t) ts)
makeFormalLocalArgsC n LANil = (n, pure [], LANil)
makeFormalLocalArgsC n (LACons (_ :: proxy a) rest) =
  let aid = mkArgId n
      mkP = do
        JCType spec dec <- getJanusCType (Proxy @a)
        pure (Param (Just aid) spec dec noLoc)
      formalHead = JanusC (pure (RVal (Var aid noLoc)))
      (n', mkPs, formalRest) = makeFormalLocalArgsC (n + 1) rest
   in (n', (:) <$> mkP <*> mkPs, LACons formalHead formalRest)

jfixLocalArgsM ::
  forall t ts b.
  (Typeable b, Storable b, JanusCTyped b) =>
  String ->
  ((LocalArgs (JanusCT t) ts -> JanusCMT t (JanusCT t b)) -> LocalArgs (JanusCT t) ts -> JanusCMT t (JanusCT t b)) ->
  LocalArgs (JanusCT t) ts ->
  JanusCMT t (JanusCT t b)
jfixLocalArgsM baseName step actualArgs = do
  fi <- ask
  let inTys = localArgsTypeReps actualArgs
      outTys = [typeRep (Proxy @b)]
      mangled = mangleJanusLocalName baseName inTys outTys
      funInfo = fi & jcfiName .~ mangled & jcfiIsLocal .~ True
      (_, paramsM, formalArgs) = makeFormalLocalArgsC @t 0 actualArgs
      retTysM = pure <$> getJanusCType (Proxy @b)
      extractOne (JanusC m) = do
        RVal e <- m
        pure [e]
      rebuildOne [e] = JanusC (pure (RVal e))
      rebuildOne _ = error "jfixLocalArgsM: expected single return expression"
      selfFake callArgs =
        emitMonadicCallSiteWith @t @(JanusCT t b) retTysM rebuildOne mangled funInfo Nothing (extractLocalArgsC callArgs)
  params'' <- defineMonadicLocalBodyWith @t @(JanusCT t b) retTysM extractOne mangled funInfo paramsM (step selfFake formalArgs)
  emitMonadicCallSiteWith @t @(JanusCT t b) retTysM rebuildOne mangled funInfo (Just params'') (extractLocalArgsC actualArgs)

jfixLocalArgsM_ ::
  forall t ts.
  String ->
  ((LocalArgs (JanusCT t) ts -> JanusCMT t ()) -> LocalArgs (JanusCT t) ts -> JanusCMT t ()) ->
  LocalArgs (JanusCT t) ts ->
  JanusCMT t ()
jfixLocalArgsM_ baseName step actualArgs = do
  fi <- ask
  let inTys = localArgsTypeReps actualArgs
      mangled = mangleJanusLocalName baseName inTys []
      funInfo = fi & jcfiName .~ mangled & jcfiIsLocal .~ True
      (_, paramsM, formalArgs) = makeFormalLocalArgsC @t 0 actualArgs
      selfFake callArgs =
        emitMonadicCallSite @t @() mangled funInfo Nothing (extractLocalArgsC callArgs)
  params'' <- defineMonadicLocalBody @t @() mangled funInfo paramsM (step selfFake formalArgs)
  emitMonadicCallSite @t @() mangled funInfo (Just params'') (extractLocalArgsC actualArgs)

--------------------------------------------------------------------------------
-- Backend-Polymorphic `ExpFix` Class
--------------------------------------------------------------------------------

-- only use fun directly beneath a toplevel, pointfree definition like so:
-- foo :: e a -> e b -> e c -> e d
-- foo = fun (Proxy @e) "foo" $ \self x y z -> ... x ... y ... (bar z)
-- to prevent scope extrusion problems
class ExpFix (e :: K.Type -> K.Type) r where
  fun :: Proxy e -> String -> (r -> r) -> r

instance ExpFix Identity r where
  fun _ _ = fix
  {-# INLINE fun #-}

instance (JanusCFix t r) => ExpFix (JanusCT t) r where
  fun _ = jfix
