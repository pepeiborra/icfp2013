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
import Data.Set (Set)
import qualified Data.Set as Set

import Language.BV.Simplifier
import Language.BV.Symbolic
import Language.BV.Syntax
import Language.BV.Tree

generate maxsize ops _ = generateDyn maxsize ops

generateDyn maxsize ops = foldMap toList g where
    g = listArray (1,maxsize) [ gen i | i <- [1 .. maxsize]]
    gen 1 = [ c0, c1, v_in ]
    gen i =
          let gi1 = g ! (i-1) in
          concat
          [ [ op1 Shl1  arg | Shl1 `Set.member` ops, arg <- gi1 ],
            filterOut isShr [ op1 Shr1  arg | Shr1 `Set.member` ops, arg <- gi1, arg /= c0, arg /= c1, arg /= shl1 c1 ],
            filterOut isShr [ op1 Shr4  arg | Shr4 `Set.member` ops, arg <- gi1, arg /= c0, arg /= c1, arg /= shl1(shl1(shl1(shl1 c1)))],
            filterOut isShr [ op1 Shr16 arg | Shr16 `Set.member` ops, arg <- gi1, arg /= c0 ],
            [ op1 Not   arg | Not `Set.member` ops, arg <- gi1, case arg of (In(Op1 Not _)) -> False ; _ -> True ],
            filterOut deMorgan
              [ op2 And  arg1 arg2
                | i > 1
                , And  `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- g! j
                , arg1 /= c0, arg1 /= neg c0
                , arg2 <- g! (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 And arg2],
            filterOut deMorgan
              [ op2 Or   arg1 arg2
                | i > 1
                , Or   `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- g! j
                , arg1 /= c0, arg1 /= neg c0
                , arg2 <- g! (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 Or  arg2],
            [ op2 Xor  arg1 arg2
                | i > 1
                , Xor  `Set.member` ops
                , j <- [1..i-2]
                , arg1 <- g! j
                , arg1 /= c0, arg1 /= neg c0
                , arg1 /= c1 -- (xor x 1) == shl1(shr1 x)
                , arg2 <- g! (i-j-1)
                , arg2 /= c0, arg2 /= neg c0
                , assocAndIdemp arg1 Xor arg2],
            [ op2 Plus arg1 arg2 | i > 1, Plus `Set.member` ops, j <- [1..i-2], arg1 <- g! j, arg1 /= c0, arg2 <- g! (i-j-1), arg2 /= c0, (if Shl1 `Set.member` ops then assocAndIdemp else assoc) arg1 Plus arg2],
            filterOut isSameIfStart [ if0 c t e | i > 3, IfOp `Set.member` ops, j <- [1..i-3], h <- [1..i-j-2], c <- g! (i-j-h-1), c `notElem` [c0,c1], t <- g! j, e <- g! h, t /= e ]
          ]

    assocAndIdemp arg1 op arg2
        | not(arg1 > arg2) = False
        | In(Op2 op' arg1' arg2') <- arg1, op == op' = assoc arg2 op arg1' && assoc arg2 op arg2'
        | In(Op2 op' arg1' arg2') <- arg2, op == op' = assoc arg1 op arg1' && assoc arg1 op arg2'
        | otherwise = True
    assoc arg1 op arg2
        | not(arg1 >= arg2) = False
        | In(Op2 op' arg1' arg2') <- arg1, op == op' = assoc arg2 op arg1' && assoc arg2 op arg2'
        | In(Op2 op' arg1' arg2') <- arg2, op == op' = assoc arg1 op arg1' && assoc arg1 op arg2'
        | otherwise = True

    isShr (In(Op1 Shr1 (In(Op1 Shr1 (In(Op1 Shr1 (In(Op1 Shr1 _)))))))) = (Shr4 `Set.member` ops)
    isShr (In(Op1 Shr4 (In(Op1 Shr4 (In(Op1 Shr4 (In(Op1 Shr4 _)))))))) = (Shr16 `Set.member` ops)
    isShr _ = False

    deMorgan (In(Op2 And (In(Op1 Not _)) (In(Op1 Not _)))) = Or `Set.member` ops
    deMorgan (In(Op2 Or (In(Op1 Not _)) (In(Op1 Not _))))  = And `Set.member` ops
    deMorgan _ = False

    isSameIfStart(In(If _ (In(Op1 op _)) (In(Op1 op' _)))) = op == op'
    isSameIfStart(In(If _ (In(Op2 op _ _)) (In(Op2 op' _ _)))) = op == op'
    isSameIfStart _ = False



filterOut p = filter (not.p)
(f .|. g) x = f x || g x