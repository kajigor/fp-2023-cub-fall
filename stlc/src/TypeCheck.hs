module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard)
import ProofTree

typeCheckEmpty :: Term String -> Either TypeError ProofTree
typeCheckEmpty t = typeCheck M.empty t

typeCheck :: Env -> Term String -> Either TypeError ProofTree
typeCheck env (Var v) =
  case M.lookup v env of
    Just t -> axiom v t
    Nothing -> Left $ UnknownVariable v env
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  abstraction x t t1
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  application t1 t2
typeCheck _ (BoolLit b) =
  boolAxiom b
typeCheck env (If c t e) = do
  ct <- typeCheck env c
  tt <- typeCheck env t
  et <- typeCheck env e
  tif ct tt et
typeCheck env (Let x v t) = do
  vt <- typeCheck env v
  let env' = M.insert x (resultType vt) env
  tt <- typeCheck env' t
  tlet x vt tt
