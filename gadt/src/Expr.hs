{-# LANGUAGE GADTs #-}

module Expr where

data Expr a where
  IntLit :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (IntLit i) = i
eval (BoolLit b) = b
eval (Add x y) = eval x + eval y
eval _ = error "Boom"

data Expr'
  = IntLit' Int
  | BoolLit' Bool
  | Add' Expr' Expr'
  | IsZero' Expr'
  | If' Expr' Expr' Expr'
  deriving (Show, Eq)

eval' :: Expr' -> Int
eval' (IntLit' i) = i
eval' (BoolLit' i) = if i then 0 else 1
eval' (Add' x y) = eval' x + eval' y
eval' (IsZero' x) = if eval' x == 0 then 0 else 1
eval' (If' c t e) = if eval' c == 0 then eval' t else eval' e

-- expr0 = If (Add (IntLit 13) (IntLit (-13))) (IntLit 7) (IntLit 42)
