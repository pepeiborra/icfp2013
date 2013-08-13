{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Language.BV.Syntax where

import Bound
import Control.Applicative
import Data.Char
import Data.Foldable (Foldable)
import Data.Traversable
import Data.Bits
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word
import Prelude.Extras
import Language.BV.Tree

newtype Prog a = Prog {unProg :: Scope () (Free ExpF) a} deriving (Eq,Ord)

deriving instance Eq a => Eq (Free ExpF a)
deriving instance Ord a => Ord (Free ExpF a)
deriving instance Show a => Show (Free ExpF a)
instance Eq1 (Free ExpF)
instance Ord1 (Free ExpF)
instance Show1 (Free ExpF)

mkProg exp = let var = "IN" in Prog $ abstract1 var $ exp (V var)

type Value = Word64

data ExpF k =
             If !k k k
           | Op1 Op !k
           | Op2 Op !k !k
           | C Word64
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

type Exp a = Free ExpF a

var = V
v_in = var "inp"
if0 a b c = In (If a b c)
fold' e e0 v1 v2 body = Fold e e0 exp
  where
    exp = abstract (\b -> if b == v1 then Just V1 else
                          if b == v2 then Just V2 else Nothing)
                   body

fold e e0 body = fold' e e0 "arg" "v0" (body (V "arg") (V "v0"))

op1 op = In . Op1 op
op2 op a = In . Op2 op a
constant = In . C
hole = In Hole
neg  = In . Op1 Not

c0 = constant 0
c1 = constant 1

plus = op2 Plus

data Op  = Not | Shl1 | Shr1 | Shr4 | Shr16  -- arity one
         | And | Or | Xor | Plus             -- arity two
         | FoldOp | IfOp | TFold             -- (META) arity three
         | C1                                -- META deduced from eval(0)
         | Bonus
           deriving (Eq, Ord, Read, Show)

shl1 = In . Op1 Shl1
shr1 = In . Op1 Shr1

readOp :: String -> Op
readOp "if0"  = IfOp
readOp "fold" = FoldOp
readOp "tfold" = TFold
readOp x = (tryRead . capitalize) x where
    capitalize (c : rest) = toUpper c : rest
    tryRead x =
      case readsPrec 0 x of
        [(y,"")] -> y
        _ -> error ("no parse: " ++ x)

ops1 = [Not, Shl1, Shr1, Shr4, Shr16]
ops2 = [And, Or, Xor, Plus]

instance Eq1 ExpF
instance Ord1 ExpF
instance Show1 ExpF
instance Read1 ExpF

