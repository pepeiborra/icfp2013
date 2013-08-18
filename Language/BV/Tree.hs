{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Language.BV.Tree where
import Prelude.Extras

import Bound
import Control.Applicative
import Data.Foldable
import Data.Traversable

data Two = V1 | V2 deriving (Eq, Ord, Read, Show)

data Free m f a where
    V    :: a -> Free m f a
    In   :: { measureFun :: forall a. f(Free m f a) -> m, measure :: m, out :: f (Free m f a) } -> Free m f a
    Fold :: !(Free m f a) -> !(Free m f a) -> (Scope Two (Free m f) a) -> Free m f a

open In{out} = Just out
open _      = Nothing

instance Functor f => Monad (Free m f) where
  return = V
  V a           >>= f = f a
  Fold e e0 lam >>= f = Fold (e >>= f) (e0 >>= f) (lam >>>= f)
  In measurefun _ g >>= f = let g' = fmap (>>= f) g in In measurefun (measurefun g') g'

hoist :: (Functor f, Functor g) =>
         (forall a. f a -> g a) -> (forall a. g(Free m g a) -> m) -> Free m f a -> Free m g a
hoist phi measureFun (V a) = V a
hoist phi measureFun (In{out}) = let out' = hoist phi measureFun <$> phi out
                                 in In measureFun (measureFun out') out'
hoist phi measureFun (Fold v1 v2 e) =
    Fold (hoist phi measureFun v1)
         (hoist phi measureFun v2)
         (toScope $ hoist phi measureFun $ fromScope e)

foldFix _   _    (V a)              = a
foldFix phi fold In{out}            = phi  (foldFix phi fold `fmap` out)
foldFix phi fold (Fold a1 a2 scope) = fold (foldFix phi fold a1) (foldFix phi fold a2) scope

foldFixM _   _   (V a)               = return a
foldFixM phi fold In{out}            = phi =<< (foldFixM phi fold `Data.Traversable.mapM` out)
foldFixM phi fold (Fold a1 a2 scope) =
  let go = foldFixM phi fold in
  do { b1 <- go a1 ; b2 <- go a2 ; fold b1 b2 scope }

instance Functor f => Functor (Free m f) where
    {-# INLINE fmap #-}
    fmap f (In measurefun _ out) = In measurefun (measurefun out') out' where
       out' = fmap (fmap f) out
    fmap f (V a) = V (f a)
    fmap f (Fold v1 v2 e) = Fold (fmap f v1) (fmap f v2) (fmap f e)

instance Traversable f => Foldable (Free m f) where foldMap = foldMapDefault

instance Traversable f => Traversable (Free m f) where
    {-# INLINE traverse #-}
    traverse f (In mF _ out) = mk <$> traverse (traverse f) out
        where mk out' = In mF (mF out') out'
    traverse f (V a) = V <$> f a
    traverse f (Fold v1 v2 e) = Fold <$> traverse f v1 <*> traverse f v2 <*> traverse f e