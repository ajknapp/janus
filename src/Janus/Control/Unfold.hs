{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Janus.Control.Unfold where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Int
import GHC.Float
import Janus.Command.Ref
import Janus.Expression.Bool
import Janus.Expression.Let
import Janus.Expression.Ord
import Janus.Typed
import Prelude hiding (id, (.))

data Unfold m e a = forall r s.
  Unfold
  { ufInit :: m r,
    ufContinue :: s -> e Bool,
    ufRead :: r -> m s,
    ufStep :: s -> m (s, a),
    ufWrite :: r -> s -> m ()
  }

instance (Functor m) => Functor (Unfold m e) where
  fmap f (Unfold init' continue' read' step' write') =
    Unfold init' continue' read' (fmap (second f) . step') write'

instance (ExpBool e, Applicative m) => Applicative (Unfold m e) where
  pure a = Unfold init' continue' read' step' write'
    where
      init' = pure ()
      continue' _ = true
      read' _ = pure ()
      step' s = pure (s, a)
      write' _ _ = pure ()
  Unfold finit fcontinue fread fstep fwrite <*> Unfold xinit xcontinue xread xstep xwrite =
    Unfold init' continue' read' step' write'
    where
      init' = (,) <$> finit <*> xinit
      continue' (fs, xs) = fcontinue fs `lor` xcontinue xs
      read' (fr, xr) = (,) <$> fread fr <*> xread xr
      step' (fs, xs) = (\(s, f) (s', a) -> ((s, s'), f a)) <$> fstep fs <*> xstep xs
      write' (fr, xr) (fs, xs) = fwrite fr fs *> xwrite xr xs

instance (Applicative m, ExpBool e, Num a) => Num (Unfold m e a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Applicative m, ExpBool e, Fractional a) => Fractional (Unfold m e a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance (Applicative m, ExpBool e, Floating a) => Floating (Unfold m e a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  log1p = fmap log1p
  expm1 = fmap expm1
  log1pexp = fmap log1pexp
  log1mexp = fmap log1mexp

range :: (Applicative m, JanusTyped e Int64, Num (e Int64), CmdRef m e, ExpLet e, ExpOrd e Int64) => e Int64 -> e Int64 -> Unfold m e (e Int64)
range l u = Unfold init' continue' read' step' write'
  where
    init' = newRef l
    continue' i = i `lt` u
    read' = readRef
    step' s = let s' = s + 1 in pure (s', s)
    write' = writeRef

weldU :: (Monad m) => Kleisli m a b -> Unfold m e a -> Unfold m e b
weldU (Kleisli m) (Unfold uinit ucont uread ustep uwrite) = Unfold uinit ucont uread (ustep >=> (\(s', a) -> (s',) <$> m a)) uwrite
