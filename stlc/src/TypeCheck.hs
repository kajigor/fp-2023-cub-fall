module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard)

type Env = M.Map String Type

typeCheckEmpty :: Term String -> Maybe Type
typeCheckEmpty = typeCheck M.empty

typeCheck :: Env -> Term String -> Maybe Type
typeCheck env (Var v) =
  M.lookup v env
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  return $ Arrow t t1
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  case t1 of
    Arrow t11 t12 | t2 == t11 -> return t12
    _ -> Nothing
typeCheck _ (BoolLit _) =
  return Bool
typeCheck env (If c t e) = do
  ct <- typeCheck env c
  guard (ct == Bool)
  tt <- typeCheck env t
  et <- typeCheck env e
  guard (tt == et)
  return tt