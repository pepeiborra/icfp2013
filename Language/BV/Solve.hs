{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns  #-}
module Language.BV.Solve where

import Bound
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Stream
import Data.Binary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S
import Database.Redis hiding (eval)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word
import Language.BV.Gen
import Language.BV.Eval
import Language.BV.Symbolic
import Language.BV.Syntax
import Language.BV.Parser
import Language.BV.Tree
import Language.BV.Program

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

satisfies :: Show a => Constraint -> Prog a -> Bool
satisfies (Size x)    p = sizeP p == x
satisfies (Uses ops)  p = operators p `Set.isSubsetOf` ops
satisfies (Value i o) p = eval p i == o

data W a = W a {-# UNPACK #-} !Int deriving Functor
instance Monad W where
    return x = W x 0
    W a w >>= f = case f a of W a' w' -> W a' (w + w')

runW (W a w) = (a, w)
tell w = W () w

solutions generate p =
    filter (\sol -> all (`satisfies` sol) valueconstraints)
           (map close $ generate (probSize p - 1) (uses p))
 where
  sizeC : usesC : valueconstraints = problemConstraints p

solveSearch :: Problem -> [Prog String]
solveSearch = solutions generate

solveRedis :: Problem -> Redis [Prog String]
solveRedis p = do
  let key = -- assume that the database is indexed by the expected outputs
            -- which are computed from a standard set of inputs
            L.toStrict $ encode (map snd $ values p)

  len <- scard key
  case len of
    Right l | l > 0 -> do
             Right all <- smembers key
             return $ filter (satisfies (Uses (uses p))) $ map (close . read . S.unpack) all
    _ -> return []

solve p = do
  redisSols <- solveRedis p
  let redisSolsSet = Set.fromList redisSols
  let searchSols = filter (`Set.notMember` redisSolsSet) $ solveSearch p
  return (redisSols, searchSols)

allM f [] = return True
allM f (x:xx) = do
  b <- f x
  if b then allM f xx else return False