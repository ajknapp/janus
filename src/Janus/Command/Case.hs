{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}

module Janus.Command.Case where

import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.Loc
import Data.Traversable
import Janus.Backend.C.Quote (captureBlock_, jcStms)
import Janus.Backend.C.Raw
import Janus.Command.Ref
import Janus.Typed
import Language.C.Quote (BlockItem (..), Stm (..))

class CmdCase m e | m -> e, e -> m where
  switchCase_ :: e Int64 -> [(Int, m ())] -> m () -> m ()

instance CmdCase IO Identity where
  switchCase_ _ [] def = def
  switchCase_ i' ((i, c) : cs) def =
    if fromIntegral (runIdentity i') == i
      then c
      else switchCase_ i' cs def

instance CmdCase (JanusCMT t) (JanusCT t) where
  switchCase_ (JanusC tag) cases def = do
    RVal tag' <- liftJanusCM tag
    caseStms <- for cases $ \(i, m) -> do
      caseItems <- captureBlock_ m
      lit <- liftJanusCM $ jlitc (fromIntegral i :: Int64)
      pure (Case lit (Block (caseItems <> [BlockStm (Break noLoc)]) noLoc) noLoc)
    defItems <- captureBlock_ def
    [jcStms|
      switch ($exp:tag') {
        $stms:caseStms
        default: {
          $items:defItems
          break;
        }
      }
    |]

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
