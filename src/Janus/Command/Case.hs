{-# LANGUAGE FunctionalDependencies #-}

module Janus.Command.Case where

import Control.Lens
import Data.Int
import Data.Loc
import Data.Monoid
import Data.Traversable
import Janus.Backend.C
import Janus.Command.Ref
import Janus.Typed
import Language.C.Quote

class CmdCase m e | m -> e, e -> m where
  switchCase_ :: e Int64 -> [(Int, m ())] -> m () -> m ()

instance CmdCase IO Identity where
  switchCase_ _ [] def = def
  switchCase_ i' ((i, c) : cs) def =
    if fromIntegral (runIdentity i') == i
      then c
      else switchCase_ i' cs def

instance CmdCase JanusCM JanusC where
  switchCase_ (JanusC tag) cases def = do
    RVal tag' <- tag
    fn <- getFunction
    let block = fn ^. jcfBlock
    blockItems <- for cases $ \(i,m) -> do
      modifyFunction $ \f -> f & jcfBlock .~ mempty
      m
      lit <- jlitc (fromIntegral i :: Int64)
      jcs'' <- getFunction
      let block' = appendBlock (jcs'' ^. jcfBlock) [BlockStm (Break noLoc)]
      pure $ BlockStm (Case lit (Block (appEndo block' []) noLoc) noLoc)
    modifyFunction $ \f -> f & jcfBlock .~ mempty
    def
    fn'' <- getFunction
    let defStm = appEndo (fn'' ^. jcfBlock) []
        switchStm = Switch tag' (Block (blockItems <> [BlockStm (Default (Block (defStm <> [BlockStm (Break noLoc)]) noLoc) noLoc)]) noLoc) noLoc
    modifyFunction $ \f -> f & jcfBlock .~ appendBlock block [BlockStm switchStm]

switchCase ::
  (JanusTyped e a, CmdRef m e, CmdCase m e) =>
  e Int64 ->
  [(Int, m (e a))] ->
  m (e a) ->
  m (e a)
switchCase t cases def = do
  r <- newRef'
  switchCase_ t (fmap (\(i, c) -> (i, c >>= writeRef r)) cases) (def >>= writeRef r)
  readRef r
