{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Janus.Control.Fold where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Int
import Data.Profunctor

import Prelude hiding ((.), id)

import Janus.Control.Unfold
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Bool
import Janus.Expression.Eq
import Janus.Expression.Cond
import Janus.Expression.Ord
import Janus.Typed

data Fold m a b
  = forall r s. Fold
  { flInit :: m r
  , flRead :: r -> m s
  , flStep :: s -> a -> m s
  , flWrite :: r -> s -> m ()
  , flExtract :: r -> m b
  }

instance Functor m => Functor (Fold m a) where
  fmap f (Fold init' read' step' write' extract') = Fold init' read' step' write' (fmap f . extract')

instance Applicative m => Applicative (Fold m a) where
  pure a = Fold (pure ()) (const (pure ())) (\_ _ -> pure ()) (\_ _ -> pure ()) (\_ -> pure a)
  Fold finit fread fstep fwrite fextract <*> Fold xinit xread xstep xwrite xextract
    = Fold init' read' step' write' extract'
    where
      init' = (,) <$> finit <*> xinit
      read' (fs,xs) = (,) <$> fread fs <*> xread xs
      step' (fs,xs) a = (,) <$> fstep fs a <*> xstep xs a
      write' (fr,xr) (fs,xs) = fwrite fr fs *> xwrite xr xs
      extract' (fr,xr) = ($) <$> fextract fr <*> xextract xr

instance Functor m => Profunctor (Fold m) where
  rmap = fmap
  lmap f (Fold init' read' step' write' extract') = Fold init' read' (\s a -> step' s (f a)) write' extract'

instance Applicative m => Choice (Fold m) where
  left' (Fold init' read' step' write' extract') = Fold init' read' step'' write' (fmap Left . extract')
    where
      step'' s ea = case ea of
        Left a -> step' s a
        Right _ -> pure s
  right' (Fold init' read' step' write' extract') = Fold init' read' step'' write' (fmap Right . extract')
    where
      step'' s ea = case ea of
        Left _ -> pure s
        Right b -> step' s b

instance (Applicative m, Semigroup b) => Semigroup (Fold m a b) where
  f <> g = (<>) <$> f <*> g

instance (Applicative m, Monoid b) => Monoid (Fold m a b) where
  mempty = pure mempty

instance (Applicative m, Num b) => Num (Fold m a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Applicative m, Fractional b) => Fractional (Fold m a b) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)
  recip = fmap recip

instance (Applicative m, Floating b) => Floating (Fold m a b) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
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

foldF :: (CmdRef m e, JanusTyped e s) => e s -> (e s -> a -> e s) -> Fold m a (e s)
foldF init' step' = Fold (newRef init') readRef (\s a -> pure $ step' s a) writeRef readRef

foldFM :: (CmdRef m e, JanusTyped e s) => e s -> (e s -> a -> m (e s)) -> Fold m a (e s)
foldFM init' step' = Fold (newRef init') readRef step' writeRef readRef

lengthF :: (Num (e Int64), JanusTyped e Int64, CmdRef m e) => Fold m a (e Int64)
lengthF = foldF 0 (\s _ -> s+1)

sumF :: (Num (e a), JanusTyped e a, CmdRef m e) => Fold m (e a) (e a)
sumF = foldF 0 (+)

productF :: (Num (e a), JanusTyped e a, CmdRef m e) => Fold m (e a) (e a)
productF = foldF 1 (*)

meanF :: (Fractional (e a), JanusTyped e a, CmdRef m e) => Fold m (e a) (e a)
meanF = Fold init' read' step' write' extract'
  where
    init' = liftA2 (,) (newRef 0) (newRef 0)
    read' (rx,rn) = liftA2 (,) (readRef rx) (readRef rn)
    step' (xbar,n) x = let n' = n+1 in pure (xbar + (x-xbar)/n', n')
    write' (rx,rn) (x,n) = writeRef rx x *> writeRef rn n
    extract' (rx,_) = readRef rx

allF :: (CmdRef m e, ExpBool e, JanusTyped e Bool) => (a -> e Bool) -> Fold m a (e Bool)
allF f = foldF true (\s a -> f a `land` s)

anyF :: (CmdRef m e, ExpBool e, JanusTyped e Bool) => (a -> e Bool) -> Fold m a (e Bool)
anyF f = foldF false (\s a -> f a `lor` s)

minimumF :: (Bounded (e a), JanusTyped e a, CmdRef m e, ExpOrd e a, ExpCond e) => Fold m (e a) (e a)
minimumF = foldF maxBound min'

maximumF :: (Bounded (e a), JanusTyped e a, CmdRef m e, ExpOrd e a, ExpCond e) => Fold m (e a) (e a)
maximumF = foldF minBound max'

elemF :: (CmdRef m e, ExpBool e, JanusTyped e Bool, ExpEq e a) => e a -> Fold m (e a) (e Bool)
elemF a = foldF false (\_ a' -> a `eq` a')

preweldF :: Monad m => (a -> m b) -> Fold m b c -> Fold m a c
preweldF m (Fold finit fread fstep fwrite fextract) = Fold finit fread (\s a -> m a >>= fstep s) fwrite fextract

postweldF :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
postweldF m (Fold finit fread fstep fwrite fextract) = Fold finit fread fstep fwrite (fextract >=> m)

drain :: (Monad m, CmdWhile m e, JanusTyped e Bool) => Unfold m e a -> Fold m a b -> m b
drain (Unfold uinit ucont uread ustep uwrite) (Fold finit fread fstep fwrite fextract) = do
  ur <- uinit
  fr <- finit
  ui <- uread ur
  whileM (ucont ui) $ \c -> do
    us <- uread ur
    fs <- fread fr
    (us',a) <- ustep us
    fs' <- fstep fs a
    fwrite fr fs' >> uwrite ur us'
    writeRef c (ucont us')
  fextract fr
