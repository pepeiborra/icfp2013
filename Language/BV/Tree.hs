{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Language.BV.Tree where
import Prelude.Extras

import Bound
import Control.Applicative
import Data.Foldable
import Data.Traversable

data Two = V1 | V2 deriving (Eq, Ord, Read, Show)

data Free f a = V a
              | In { out :: f (Free f a) }
              | Fold !(Free f a) !(Free f a) (Scope Two (Free f) a)
              | Hole ![a] -- hole carrying the free vars bound by the context around it

open (In x) = Just x
open _      = Nothing

instance Functor f => Monad (Free f) where
  return = V
  Hole vv       >>= f = error "Hole vv >>= f"
  V a           >>= f = f a
  Fold e e0 lam >>= f = Fold (e >>= f) (e0 >>= f) (lam >>>= f)
  In g          >>= f = In $ fmap (>>= f) g

--foldFix :: Functor f => (f a -> a) -> (a -> a -> a) -> Free f a -> a
foldFix _   _    _     (V a)             = a
foldFix phi fold hole (In f)             = phi  (foldFix phi fold hole `fmap` f)
foldFix phi fold hole (Fold a1 a2 scope) = fold (foldFix phi fold hole a1) (foldFix phi fold hole a2) scope
foldFix _   _    hole (Hole vv) = hole vv

foldHoles :: (Applicative m, Traversable f, Eq a) =>
             (forall v. Eq v => [v] -> m(Free f v)) -> Free f a -> m (Free f a)
foldHoles h (V a) = pure (V a)
foldHoles h (In f) = In <$> traverse (foldHoles h) f
foldHoles h (Fold v1 v2 f) = Fold <$> foldHoles h v1 <*> foldHoles h v2 <*> (toScope <$> foldHoles h (fromScope f))
foldHoles h (Hole vv) = h vv
{-
deriving instance (Functor f, Eq1 f, Eq a, Eq (f(Free f a))) => Eq (Free f a)
deriving instance (Functor f, Ord1 f, Ord a, Ord (f(Free f a))) => Ord (Free f a)
deriving instance (Functor f, Read1 f, Read a, Read (f(Free f a))) => Read (Free f a)
deriving instance (Functor f, Show1 f, Show a, Show (f(Free f a))) => Show (Free f a)

instance (Functor f, Eq1 f) => Eq1 (Free f)
-}
deriving instance Functor     f => Functor     (Free f)
deriving instance Foldable    f => Foldable    (Free f)
deriving instance Traversable f => Traversable (Free f)

