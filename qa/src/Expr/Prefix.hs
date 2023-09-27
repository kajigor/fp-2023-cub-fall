module Expr.Prefix where

import Expr.Lexer
import Expr.Ast
import Expr.Combinators
import Control.Applicative

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Number
-- +1*23 -> BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))
parse :: String -> Maybe (String, Expr)
parse = runParser (spaces *> parsePrefix)

parsePrefix :: Parser Expr
parsePrefix =
        (BinOp <$> op <*> parsePrefix <*> parsePrefix)
    <|> number
  where
    op = anyOf [plus, minus, star, division, hat]