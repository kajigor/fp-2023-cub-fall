{-# LANGUAGE GADTs, RankNTypes, TypeOperators, StandaloneDeriving #-}

module Expr.Untyped where

data Expr
  = IntLit Int
  | BoolLit Bool
  | Add Expr Expr
  | IsZero Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

eval :: Expr -> Int
eval (IntLit i) = i
eval (BoolLit i) = if i then 0 else 1
eval (Add x y) = eval x + eval y
eval (IsZero x) = if eval x == 0 then 0 else 1
eval (If c t e) = if eval c == 0 then eval t else eval e

expr0 = If (Add (IntLit 13) (IntLit (-13))) (IntLit 7) (IntLit 42)
expr1 = Add (BoolLit True) (IntLit 42)
