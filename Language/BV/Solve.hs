{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns  #-}
module Language.BV.Solve where

import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.Stream
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Word
import Language.BV.Gen
import Language.BV.Eval
import Language.BV.Symbolic
import Language.BV.Syntax
import Language.BV.Tree

data Problem = Problem
               { probId   :: Text,
                 probSize :: Int,
                 uses     :: Set Op,
                 values   :: [(Word64, Word64)]
               } deriving (Eq, Read, Show, Ord)

data Constraint = Size Int
                | Uses (Set Op)
                | Value Word64 Word64
                deriving (Eq, Ord, Show)

problemConstraints p =
    Size (probSize p) :
    Uses (uses p) :
    [ Value i o | (i,o) <- values p ]

satisfies :: Show a => Prog a -> Constraint -> Bool
satisfies p (Size x)    = sizeP p == x
satisfies p (Uses ops)  = operators p == ops
satisfies p (Value i o) = eval p i == o

data W a = W a {-# UNPACK #-} !Int deriving Functor
instance Monad W where
    return x = W x 0
    W a w >>= f = case f a of W a' w' -> W a' (w + w')

runW (W a w) = (a, w)
tell w = W () w

solutions generate p = (sols, 0::Integer) where
  sols =
        filter (\sol -> all (\x -> satisfies sol x) valueconstraints)
               (fmap close $ generate (probSize p - 1) (uses p) start)

  start = Hole ["inp"]
  close = Prog . abstract1 "inp"
  sizeC : usesC : valueconstraints = problemConstraints p

data Count b = Count {-# UNPACK #-} !Int b
-- keep track of the evaluation count
-- written recursively instead of using the Writer monad,
-- otherwise it blows the stack and performs terribly
solutionsCount generate p = go (0::Int) [] (fmap close $ generate (probSize p - 1) (uses p) start)
 where
  go !count acc [] = Count count acc
  go !count acc (s:ss) =
      case go' 0 s valueconstraints of
        Count count' True  -> go (count + count') (s:acc) ss
        Count count' False -> go (count + count') acc ss

  go' !count s [] = Count count True
  go' !count s (c:cc) = if satisfies s c then go' (count+1) s cc else Count (count+1) False

  start = Hole ["inp"]
  close = Prog . abstract1 "inp"
  sizeC : usesC : valueconstraints = problemConstraints p

solve = head . fst . solutions generate

allM f [] = return True
allM f (x:xx) = do
  b <- f x
  if b then allM f xx else return False