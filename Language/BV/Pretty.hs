{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Language.BV.Pretty where

import Bound
import Bound.Scope
import Data.Char
import Data.Foldable (Foldable)
import Text.PrettyPrint.HughesPJClass
import Text.Parsec.Error

import Language.BV.Eval (instantiate2,v_in)
import Language.BV.Syntax
import Language.BV.Tree
import Language.BV.Program

instance Show (Prog String) where show = show . pPrint
instance Show (Exp  String) where show = show . pPrint

instance Pretty Op where
  pPrint = text . map toLower . show

instance (Functor f, Foldable f, Pretty1 f) => Pretty (Free m f String) where
  pPrintPrec level prec (V v)  = text v
  pPrintPrec level prec (In _ _ f) = pPrintPrec1 level prec f
  pPrintPrec level prec (Fold e0 e1 body) =
      let body' = instantiate2 body (V x) (V acc)
          x   = nameVar "x" level
          acc = nameVar "acc" level
      in parens(text "fold" <+> pPrintPrec level prec e0 <+> pPrintPrec level prec e1 <+>
             parens (text "lambda" <+> (parens (text x <+> text acc)
                                   <+> pPrintPrec (succ level) prec body')))

nameVar s (PrettyLevel x) = s ++ show x

deriving instance Enum PrettyLevel

instance Pretty1 ExpF where
  pPrintPrec1 level prec (If e1 e2 e3) =
      let go = pPrintPrec level prec in
      parens(text "if0" <+> go e1 <+> go e2 <+> go e3)
  pPrintPrec1 level prec (C w) = text (show w)
  pPrintPrec1 level prec (Op1 op e) = parens (pPrint op <+> pPrintPrec level prec e)
  pPrintPrec1 level prec (Op2 op e1 e2) = parens (pPrint op <+> pPrintPrec level prec e1 <+> pPrintPrec level prec e2)

instance Pretty (Prog String) where
  pPrint sc =
      parens(text "lambda (inp)" <+> pPrint (instantiate1 v_in $ unProg sc))

instance Pretty (ParseError) where pPrint = text . show

class Pretty1 f where
    pPrintPrec1 :: Pretty a => PrettyLevel -> Rational -> f a -> Doc
    pPrint1 :: Pretty a => f a -> Doc
    pPrint1 = pPrintPrec1 (PrettyLevel 0) 0
