module Janus.Expression.Let.Do where

import Janus.Expression.Let
import Janus.Typed

(>>=) :: (ExpLet e, JanusTyped e a, JanusTyped e b) => e a -> (e a -> e b) -> e b
(>>=) = let_
