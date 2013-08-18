{-# LANGUAGE RecordWildCards, NamedFieldPuns, DisambiguateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.BV.Parser where

import Bound
import Control.Applicative -- hiding (Alternative(..))
import Control.Monad
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Language.BV.Eval
import Language.BV.Program
import Language.BV.Syntax
import Prelude hiding (exp)

{-
 program    P ::= "(" "lambda" "(" id ")" e ")"
 expression e ::= "0" | "1" | id
               | "(" "if0" e e e ")"
               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
               | "(" op1 e ")"
               | "(" op2 e e ")"
          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
          op2 ::= "and" | "or" | "xor" | "plus"
          id  ::= [a-z][a-z_0-9]*
-}

instance Read (Prog String) where
    readsPrec _ s = case parse ((,) <$> program <*> getInput) "" s of
                      Left err -> []
                      Right x -> [x]

instance Read (Exp String) where
    readsPrec _ s = case parse ((,) <$> exp <*> getInput) "" s of
                      Left err -> []
                      Right res -> [res]

parseProgram = parse program ""

program = parens (close' <$> (symbol "lambda" *> parens identifier) <*> exp)

exp =
    constant 0 <$ symbol "0" <|>
    constant 1 <$ symbol "1" <|>
    var <$> identifier <|>
    parens complexExp

complexExp =
    (op1  <$> op1Parser  <*> exp) <|>
    (op2  <$> op2Parser  <*> exp <*> exp) <|>
    (if0  <$> (symbol "if0" *> exp) <*> exp <*> exp) <|>
    (mkFo <$> (symbol "fold" *> exp) <*> exp <*> foldBody)
  where
    mkFo e1 e2 ((v1,v2), body)= fold' e1 e2 v1 v2 body
    foldBody = parens ((,) <$> (symbol "lambda" *> parens ((,) <$> identifier <*> identifier))
                           <*> exp)

op1Parser =
    Not   <$ symbol "not"   <|>
    try(Shl1  <$ symbol "shl1")  <|>
    try(Shr16 <$ symbol "shr16") <|>
    try(Shr1  <$ symbol "shr1")  <|>
    (Shr4  <$ symbol "shr4") 

op2Parser =
    And  <$ symbol "and"  <|>
    Or   <$ symbol "or"   <|>
    Xor  <$ symbol "xor"  <|>
    Plus <$ symbol "plus"

T.TokenParser{..} = T.makeTokenParser haskellDef