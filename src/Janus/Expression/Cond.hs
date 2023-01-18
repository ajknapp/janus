{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Janus.Expression.Cond where

import Control.Lens
import Data.Loc
import Data.Monoid
import Data.Proxy
import Janus.Backend.C
import Janus.Expression.Bool
import Janus.Expression.Cast
import Janus.Expression.Let
import Janus.Typed
import Language.C.Quote

class (ExpBool e) => ExpCond e where
  ifThenElse :: (JanusTyped e a) => e Bool -> e a -> e a -> e a

instance ExpCond Identity where
  ifThenElse (Identity p) (Identity a) (Identity b) = Identity $ if p then a else b

instance ExpCond JanusC where
  ifThenElse (JanusC c) (JanusC t) (JanusC f) = JanusC $ do
    RVal c' <- c
    let proxify :: JanusCM (RVal a) -> Proxy a
        proxify _ = Proxy
    JCType spec dec <- getJanusCType (proxify t)
    fn <- getFunction
    let i = fn ^. jcfVarCounter
        vid = Id ("x_" <> show i) noLoc
        var = Var vid noLoc
    modifyFunction $ \jfn ->
      jfn
        & jcfBlock .~ mempty
        & jcfVarCounter +~ 1
    RVal t' <- t
    fn' <- getFunction
    let tassign = [BlockStm $ Exp (Just $ Assign var JustAssign t' noLoc) noLoc]
        tblock = appendBlock (fn' ^. jcfBlock) tassign
    modifyFunction $ \jfn -> jfn & jcfBlock .~ mempty
    RVal f' <- f
    fn'' <- getFunction
    let fassign = [BlockStm $ Exp (Just $ Assign var JustAssign f' noLoc) noLoc]
        fblock = appendBlock (fn'' ^. jcfBlock) fassign
        ini = Init vid dec Nothing Nothing [] noLoc
        vardec = BlockDecl $ InitGroup spec [] [ini] noLoc
        ifteStm =
          BlockStm $
            If
              c'
              (Block (appEndo tblock []) noLoc)
              (Just $ Block (appEndo fblock []) noLoc)
              noLoc
    modifyFunction $ \jfn -> jfn & jcfBlock .~ appendBlock (fn ^. jcfBlock) [vardec, ifteStm]
    pure $ RVal var

branchFreeCond ::
  (JanusTyped e b, ExpLet e, ExpBoolCast e b, Num (e b)) =>
  e Bool ->
  e b ->
  e b ->
  e b
branchFreeCond p t f = let_ (fromBool p) $ \p' -> p' * t + (1 - p') * f
