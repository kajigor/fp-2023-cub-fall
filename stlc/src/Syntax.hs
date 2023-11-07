{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Text.Printf (printf)
import Data.Char (chr)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Set as S

data Type
  = TyVar String
  | Arrow Type Type
  | Bool
  deriving (Eq, Ord)

instance Show Type where
  show (TyVar v) = v
  show (Arrow t1@(Arrow _ _) t2) = printf  "(%s) -> %s" (show t1) (show t2)
  show (Arrow t1 t2) = printf  "%s -> %s" (show t1) (show t2)
  show (Bool) = "Bool"

data Term a
  = Var a
  | Abs a Type (Term a)
  | App (Term a) (Term a)
  | BoolLit Bool
  | If (Term a) (Term a) (Term a)
  | Let a (Term a) (Term a)
  deriving (Eq)

instance Show (Term String) where
  show (Var x) = x
  show (Abs x typ t) = printf "λ%s:%s.%s" x (show typ) (show t)
  show (App t1 t2) = printf "(%s) (%s)" (show t1) (show t2)
  show (BoolLit b) = printf "%s" (show b)
  show (If c t e) = printf "if %s then %s else %s" (show c) (show t) (show e)
  show (Let x v t) = printf "let %s = %s in %s" x (show v) (show t)

freeVars :: Ord a => Term a -> S.Set a
freeVars (Var a) = S.singleton a
freeVars (Abs a _ t) = S.delete a (freeVars t)
freeVars (App t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (BoolLit _) = S.empty
freeVars (If c t e) = S.unions (freeVars <$> [c, t, e])
freeVars (Let x v t) = S.union (freeVars v) (S.delete x (freeVars t))