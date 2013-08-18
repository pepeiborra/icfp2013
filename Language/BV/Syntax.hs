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

type Value = Word64

data ExpFV v k =
             If !k k k
           | Op1 Op !k
           | Op2 Op !k !k
           | C v
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

type ExpF = ExpFV Word64

data ConstantValueOrBottom = ConstantValue {-#UNPACK#-} !Word64 | Bottom
instance Eq ConstantValueOrBottom where
  ConstantValue a == ConstantValue b = a == b
  _ == _ = False
instance Ord ConstantValueOrBottom where
  ConstantValue a `compare` ConstantValue b = compare a b
  ConstantValue{} `compare` _ = GT
  _ `compare` _ = LT

type Exp = Free ConstantValueOrBottom ExpF

data Op  = Not | Shl1 | Shr1 | Shr4 | Shr16  -- arity one
         | And | Or | Xor | Plus             -- arity two
         | FoldOp | IfOp | TFold             -- (META) arity three
         | C1                                -- META deduced from eval(0)
         | Bonus
           deriving (Eq, Ord, Read, Show)

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

