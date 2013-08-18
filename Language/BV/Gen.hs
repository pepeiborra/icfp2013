{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Language.BV.Gen where

import Control.Applicative
import Control.Monad
import Control.Monad.Stream
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Array
import Data.Foldable (foldMap)
import Data.Function (fix)
import Data.Set (Set)
import qualified Data.Set as Set

import Language.BV.Eval
import Language.BV.Simplifier
import Language.BV.Symbolic
import Language.BV.Syntax
import Language.BV.Tree

generate = generateMemo

generateMemo maxsize ops = foldMap toList g where
    go i = generateOpen ops (g!) i
    g = listArray (1,maxsize) [ go i | i <- [1 .. maxsize]]

-- TODO
-- generateTabling :: Set Ops -> Int -> Redis [Exp a]

generateOpen ops rec (maxsize::Int) = gen maxsize where
    getGen i = toList (rec i)
    gen 1 = Set.fromList [ c0, c1, v_in ]
    gen i =
          Set.fromList $ concat
          [ [ op1 Shl1  arg
              | Shl1 `Set.member` ops
              , arg <- getGen(i-1)
              , arg /= c0
              , not(isNot arg) ],
            filterOut isRedundantShr
              [ op1 Shr1  arg
                | Shr1 `Set.member` ops
                , arg <- getGen(i-1)
                , arg /= c0
                , arg /= c1
                , not (isNot arg)
                , arg /= shl1 c1 ],
            filterOut isRedundantShr
              [ op1 Shr4  arg
                | Shr4 `Set.member` ops
                , arg <- getGen(i-1)
                , arg /= c0
                , arg /= c1
                , not (isNot arg)
                , arg /= shl1(shl1(shl1(shl1 c1)))
                ],
            filterOut isRedundantShr
              [ op1 Shr16 arg
                | Shr16 `Set.member` ops
                , arg <- getGen(i-1)
                , arg /= c0
                , arg /= c1
                , not(isNot arg)
                ],
            [ op1 Not   arg | Not `Set.member` ops, arg <- getGen(i-1), not(isNot arg)],
            filterOut deMorgan
              [ op2 And  arg1 arg2
                | i > 1
                , And  `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- getGen j
                , arg1 /= c0, arg1 /= neg c0
                , arg2 <- getGen (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 And arg2],
            filterOut deMorgan
              [ op2 Or   arg1 arg2
                | i > 1
                , Or   `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- getGen j
                , arg1 /= c0, arg1 /= neg c0
                , arg2 <- getGen (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 Or  arg2],
            [ op2 Xor  arg1 arg2
                | i > 1
                , Xor  `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- getGen j
                , arg1 /= c0, arg1 /= neg c0
                , arg1 /= c1 -- (xor x 1) == shl1(shr1 x)
                , arg2 <- getGen (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 Xor arg2],
            filterOut plusXnotX
            [ op2 Plus arg1 arg2
                | i > 1
                , Plus `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- getGen j
                , arg1 /= c0, arg1 /= neg c0
                , arg2 <- getGen (i-j-1)
                , arg2 /= c0, arg1 /= neg c0
                , (if Shl1 `Set.member` ops then assocAndIdemp else assoc) arg1 Plus arg2],
            filterOut isSameIfStart
            [ if0 c t e
              | i > 3
              , IfOp `Set.member` ops
              , j <- [1..i-3]
              , h <- [1..i-j-2]
              , c <- getGen (i-j-h-1)
              , c `notElem` [c0,c1]
              , not (isNot c)
              , t <- getGen j
              , e <- getGen h
              , t /= e ]
          ]

    assocAndIdemp arg1 op arg2
        | not(arg1 > arg2) = False
        | (open -> Just (Op2 op' arg1' arg2')) <- arg1, op == op' = assoc arg2 op arg1' && assoc arg2 op arg2'
        | (open -> Just (Op2 op' arg1' arg2')) <- arg2, op == op' = assoc arg1 op arg1' && assoc arg1 op arg2'
        | otherwise = True
    assoc arg1 op arg2
        | not(arg1 >= arg2) = False
        | (open -> Just (Op2 op' arg1' arg2')) <- arg1, op == op' = assoc arg2 op arg1' && assoc arg2 op arg2'
        | (open -> Just (Op2 op' arg1' arg2')) <- arg2, op == op' = assoc arg1 op arg1' && assoc arg1 op arg2'
        | otherwise = True

    isRedundantShr (open -> Just (Op1 Shr1 (open->Just(Op1 Shr1 (open->Just(Op1 Shr1 (open->Just(Op1 Shr1 _)))))))) = (Shr4 `Set.member` ops)
    isRedundantShr (open -> Just (Op1 Shr4 (open->Just(Op1 Shr4 (open->Just(Op1 Shr4 (open->Just(Op1 Shr4 _)))))))) = (Shr16 `Set.member` ops)
    isRedundantShr (open -> Just (Op1 Shr1 (open->Just(Op1 Shr4 _)))) = True
    isRedundantShr (open -> Just (Op1 Shr1 (open->Just(Op1 Shr16 _)))) = True
    isRedundantShr (open -> Just (Op1 Shr4 (open->Just(Op1 Shr16 _)))) = True
    isRedundantShr (open -> Just (Op1 shr  (open->Just(Op1 Shl1 (open->Just(Op1 Shr1 _)))))) = shr `elem` [Shr1,Shr4,Shr16]
    isRedundantShr _ = False


    isNot (open -> Just (Op1 Not _)) = True
    isNot _ = False

    deMorgan (open -> Just (Op2 And (open->Just(Op1 Not _)) (open->Just(Op1 Not _)))) = Or `Set.member` ops
    deMorgan (open -> Just (Op2 Or (open->Just(Op1 Not _)) (open->Just(Op1 Not _))))  = And `Set.member` ops
    deMorgan _ = False

    isSameIfStart(open -> Just (If _ (open->Just(Op1 op _))   (open->Just(Op1 op' _  )))) = op == op'
    isSameIfStart(open -> Just (If _ (open->Just(Op2 op _ _)) (open->Just(Op2 op' _ _)))) = op == op'
    isSameIfStart(open -> Just (If a (open->Just(C 0)) a')) = a == a'
    isSameIfStart(open -> Just (If a a' (open->Just(C 0)))) = a == a'
    isSameIfStart _ = False

    plusXnotX(open -> Just (Op2 Plus x (open->Just(Op1 Not y)))) = x == y
    plusXnotX(open -> Just (Op2 Plus (open->Just(Op1 Not y)) x)) = x == y
    plusXnotX _ = False

filterOut p = filter (not.p)
(f .|. g) x = f x || g x