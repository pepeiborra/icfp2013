module Language.BV.Eval where

import Data.Bits
import Data.Word

import Bound
import Language.BV.Syntax
import Language.BV.Tree


nf :: Show a => Exp a -> Word64
nf = foldFix go fold hole . fmap (\s -> error("nf: var " ++ show s)) where
  go(Op2 And   v1 v2)   = (v1 .&. v2)
  go(Op2 Or    v1 v2)   = (v1 .|. v2)
  go(Op2 Xor   v1 v2)   = (v1 `xor` v2)
  go(Op2 Plus  v1 v2)   = (v1 + v2)
  go(Op1 Not   v1)      = (complement v1)
  go(Op1 Shl1  v1)      = (v1 `unsafeShiftL` 1)
  go(Op1 Shr1  v1)      = (v1 `unsafeShiftR` 1)
  go(Op1 Shr4  v1)      = (v1 `unsafeShiftR` 4)
  go(Op1 Shr16 v1)      = (v1 `unsafeShiftR` 16)
  go(If 0 etrue efalse) = etrue
  go(If _ etrue efalse) = efalse
  go(C x)               = x
  fold v0 v1 fexp =
    nf $
       foldr (instantiate2 fexp)
             (constant v1)
             (map constant $ unpack v0)

  hole = error "nf: hole"

  unpack :: Word64 -> [Word64]
  unpack w = map (\s -> shiftR(w .&. shiftL (0xFF :: Word64) (s*8)) (s*8)) [0..7]


instantiate2 fexp x1 x2 = instantiate (\x -> case x of V1 -> x1 ; V2 -> x2) fexp

--eval :: Prog a -> Double -> Double
eval f x = nf(instantiate1 (constant x) (unProg f))
