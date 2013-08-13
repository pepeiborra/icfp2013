{-# LANGUAGE FlexibleInstances #-}
module Language.BV.NFData where

import Bound
import Control.DeepSeq

import Language.BV.Tree
import Language.BV.Syntax

instance NFData (Prog String) where
    rnf (Prog x) = rnf (unscope x)

instance NFData a => NFData (Free ExpF a) where
    rnf (V x)  = rnf x
    rnf (In f) = rnf f
    rnf (Fold a b f) = rnf a `seq` rnf b `seq` rnf (unscope f)

instance NFData a => NFData (ExpF a) where
    rnf (Op1 op arg) = rnf op `seq` rnf arg
    rnf (Op2 op arg1 arg2) = rnf op `seq` rnf arg1 `seq` rnf arg2
    rnf (If a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (C x) = rnf x

instance NFData Op
instance NFData Two

instance (NFData a, NFData b) => NFData (Var a b) where
    rnf (F a) = rnf a
    rnf (B b) = rnf b