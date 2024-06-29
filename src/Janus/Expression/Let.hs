module Janus.Expression.Let where

import Control.Lens
import Control.Monad.State
import Data.Loc
import Data.Proxy
import Janus.Backend.C
import Janus.Typed
import Language.C.Quote

class ExpLet e where
  let_ :: (JanusTyped e a, JanusTyped e b) => e a -> (e a -> e b) -> e b

instance ExpLet Identity where
  let_ x f = f x
  {-# INLINE let_ #-}

instance ExpLet JanusC where
  let_ j@(JanusC x) f = JanusC $ do
    let proxify :: JanusC a -> Proxy a
        proxify _ = Proxy
    JCType spec dec <- getJanusCType (proxify j)
    fname <- askFuncName
    RVal x'' <- x
    fn <- getFunction
    let c = fn ^. jcfVarCounter
        var1 = Id ("x_" <> show c) noLoc
        ini = Init var1 dec Nothing (Just $ ExpInitializer x'' noLoc) [] noLoc
        val1 = BlockDecl $ InitGroup spec [] [ini] noLoc
        fn' =
          fn
            & jcfBlock %~ flip appendBlock [val1]
            & jcfVarCounter +~ 1
    modify $ \s -> s & ix fname .~ fn'
    getJanusC $ f $ JanusC $ pure $ RVal (Var var1 noLoc)

share :: (ExpLet e, JanusTyped e a) => e a -> e a
share = flip let_ id
{-# INLINE share #-}
