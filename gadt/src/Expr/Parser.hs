{-# LANGUAGE FlexibleContexts #-}

module Expr.Parser (parse) where

import Expr.Untyped

import Control.Monad.Reader
import Text.Parsec hiding (Empty, parse)
import Control.Monad.Combinators.Expr

type Parser = ParsecT String () (Reader [String])

parse :: String -> Maybe Expr
parse s =
  case runReader (runParserT (whitespace *> expression <* eof) () "" s) [] of
    Left _  -> Nothing
    Right x -> Just x

intLit :: Parser Expr
intLit = try $ do
  sign <- option ' ' (char '-')
  digits <- many1 digit
  whitespace
  return $ IntLit (read (sign:digits))

keyword :: Stream s m Char => String -> ParsecT s u m ()
keyword kw = try $ do
  _ <- string kw
  notFollowedBy alphaNum

boolLit :: Parser Expr
boolLit = do
  b <- (keyword "True" *> return True) <|> (keyword "False" *> return False)
  whitespace
  return $ BoolLit b

ifThenElse :: Parser Expr
ifThenElse = do
  keyword "if" <* whitespace
  c <- expression
  keyword "then" <* whitespace
  t <- expression
  keyword "else" <* whitespace
  e <- expression
  return $ If c t e

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany space


term :: Parser Expr
term = choice
  [ parens expression
  , intLit
  , boolLit
  , ifThenElse
  ]

expression :: Parser Expr
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "?" IsZero ]
  , [ binary "+" Add ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ string name)

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ string name)