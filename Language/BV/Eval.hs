{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.BV.Eval where

import Control.Monad
import Data.Bits
import Data.Word
import Prelude.Extras

import Bound
import Language.BV.Syntax
import Language.BV.Tree

instance MonadPlus Identity where mzero = error "free var" ; mplus _ _ = error "mplus"

instance Eq1 Exp
instance Ord1 Exp

instance Eq a => Eq(Exp a) where
  V x == V y = x == y
  In{measure=Bottom,out=t1} == In{measure=Bottom,out=t2} = t1==t2
  In{measure=m,out=t1} == In{measure=m',out=t2} = m==m'
  Fold a1 a2 e == Fold b1 b2 e' = a1 == b1 && a2 == b2 && e ==# e'
  _ == _ = False

instance Ord a => Ord(Exp a) where
  V x `compare` V y = x `compare` y
  In{measure=Bottom,out=t1} `compare` In{measure=Bottom,out=t2} = t1`compare`t2
  In{measure=m,out=t1} `compare` In{measure=m',out=t2} = m`compare`m'
  Fold a1 a2 e `compare` Fold b1 b2 e' = (a1,a2,e) `compare` (b1,b2,e')
  V{} `compare` _   = GT
  _   `compare` V{} = LT
  In{}   `compare` Fold{} = LT
  Fold{} `compare` In{}   = LT

nf = runIdentity . nfM

nfM :: (MonadPlus m) => Exp a -> m Word64
nfM = foldFix nfIn nfFold . fmap (const mzero) where
nfIn(Op2 And   v1 v2)   = liftM2 (.&.) v1 v2
nfIn(Op2 Or    v1 v2)   = liftM2 (.|.) v1 v2
nfIn(Op2 Xor   v1 v2)   = liftM2 xor v1 v2
nfIn(Op2 Plus  v1 v2)   = liftM2 (+) v1 v2
nfIn(Op1 Not   v1)      = liftM complement v1
nfIn(Op1 Shl1  v1)      = liftM (`unsafeShiftL` 1)  v1
nfIn(Op1 Shr1  v1)      = liftM (`unsafeShiftR` 1)  v1
nfIn(Op1 Shr4  v1)      = liftM (`unsafeShiftR` 4)  v1
nfIn(Op1 Shr16 v1)      = liftM (`unsafeShiftR` 16) v1
nfIn(If x etrue efalse) = do { v <- x; if v == 0 then etrue else efalse}
nfIn(C x)               = return x
nfFold m0 m1 fexp = do
    v0 <- m0
    v1 <- m1
    nfM $
       foldr (instantiate2 fexp)
             (constant v1)
             (map constant $ unpack v0)
 where
  unpack :: Word64 -> [Word64]
  unpack w = map (\s -> shiftR(w .&. shiftL (0xFF :: Word64) (s*8)) (s*8)) [0..7]


instantiate2 fexp x1 x2 = instantiate (\x -> case x of V1 -> x1 ; V2 -> x2) fexp

-- smart constructors
in_ :: ExpF (Exp a) -> Exp a
in_ x = In measure (measure x) x where
   measure ::  ExpF(Exp a) -> ConstantValueOrBottom
   measure x = fromMaybe $ nfIn $ fmap(foldFix nfIn nfFold . fmap(const mzero)) x
   fromMaybe Nothing = Bottom
   fromMaybe(Just x) = ConstantValue x

var :: a -> Exp a
var = V
v_in = var "inp"
if0 a b c = in_ (If a b c)
fold' e e0 v1 v2 body = Fold e e0 exp
  where
    exp = abstract (\b -> if b == v1 then Just V1 else
                          if b == v2 then Just V2 else Nothing)
                   body

fold e e0 body = fold' e e0 "arg" "v0" (body (V "arg") (V "v0"))

op1 op = in_ . Op1 op
op2 op a = in_ . Op2 op a
constant = in_ . C
neg  = in_ . Op1 Not

c0 = constant 0
c1 = constant 1

plus = op2 Plus

shl1 = in_ . Op1 Shl1
shr1 = in_ . Op1 Shr1

-- | Identity functor and monad.
newtype Identity a = Identity { runIdentity :: a }

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
    {-# INLINE return #-}
    return a = Identity a
    {-# INLINE (>>=) #-}
    m >>= k  = k (runIdentity m)
