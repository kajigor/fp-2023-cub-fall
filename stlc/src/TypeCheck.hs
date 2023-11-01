module TypeCheck where

import Syntax
import qualified Data.Map as M
import Control.Monad (guard)

type Env = M.Map String Type
type Error = String

typeCheckEmpty :: Term String -> Either Error Type
typeCheckEmpty = typeCheck M.empty

labelGuard :: Bool -> e -> Either e ()
labelGuard True _  = return ()
labelGuard False e = Left e

typeCheck :: Env -> Term String -> Either Error Type
typeCheck env (Var v) =
  case M.lookup v env of
    Just t -> return t
    Nothing -> Left $ concat ["Unknown variable ", show v, " in environment ", show env, "."]
typeCheck env (Abs x t b) = do
  let env' = M.insert x t env
  t1 <- typeCheck env' b
  return $ Arrow t t1
typeCheck env (App m n) = do
  t1 <- typeCheck env m
  t2 <- typeCheck env n
  case t1 of
    Arrow t11 t12 | t2 == t11 -> return t12
                  | otherwise -> Left $ concat ["Invalid argument type: expected ", show t11, " as input type for " ++ show m ++ ", got ", show n, " : ", show t2]
    _ -> Left $ concat ["Expected ", show m, " to be an arrow type, got ", show t1]
typeCheck _ (BoolLit _) =
  return Bool
typeCheck env (If c t e) = do
  ct <- typeCheck env c
  labelGuard (ct == Bool) $ concat ["Expected condition to be Bool, got ", show c, " : ", show ct]
  tt <- typeCheck env t
  et <- typeCheck env e
  labelGuard (tt == et) $ concat ["Expected branches to match, got ", show t, " : ", show tt, " and ", show e, " : ", show et]
  return tt
typeCheck env (Let x v t) = do
  vt <- typeCheck env v
  let env' = M.insert x vt env
  typeCheck env' t