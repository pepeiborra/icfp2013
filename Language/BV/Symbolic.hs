{-# LANGUAGE ViewPatterns #-}
module Language.BV.Symbolic where

import Bound
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.BV.Syntax
import Language.BV.Tree

size :: Exp a -> Int
size = foldFix go fold hole . fmap (const 1) where
  fold e1 e2 _ = (2 + e1 + e2)
  go (Op1 _ e) = 1 + e
  go (Op2 _ e1 e2) = 1 + e1 + e2
  go (C _) = 1
  go (If e1 e2 e3) = 1 + e1 + e2 + e3
  hole _ = 0

sizeP (Prog e) = 1 + size (unscope e)

ops :: Exp a -> Set Op
ops = foldFix go fold hole . fmap mempty where
  go (Op1 o e)     = Set.singleton o <> e
  go (Op2 o e1 e2) = Set.singleton o <> e1 <> e2
  go (If e1 e2 e3) = Set.singleton IfOp <> e1 <> e2 <> e3
  go (C _)         = mempty
  fold e1 e2 e3    = Set.singleton FoldOp <> e1 <> e2 <> ops (unscope e3)
  hole _ = mempty

opsP = ops . unscope . unProg

operators (Prog (unscope -> e@Fold{})) = Set.singleton TFold <> ops e
operators (Prog (unscope -> e)) = ops e
