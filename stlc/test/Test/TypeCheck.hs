module Test.TypeCheck where

import Test.Tasty.HUnit (Assertion, (@?=))

import Syntax
import TypeCheck
import qualified Data.Map as M

unit_typecheck = do
    check (Abs "x" Bool (Var "x")) (Arrow Bool Bool)
    check (If (App (Abs "x" Bool (Var "x")) (BoolLit True)) (BoolLit True) (BoolLit False)) Bool
    check (Abs "f" (Arrow (TyVar "b") (TyVar "c")) (Abs "g" (Arrow (TyVar "a") (TyVar "b")) (Abs "x" (TyVar "a") (App (Var "f") (App (Var "g") (Var "x")))))) (Arrow (Arrow (TyVar "b") (TyVar "c")) (Arrow (Arrow (TyVar "a") (TyVar "b")) (Arrow (TyVar "a") (TyVar "c"))))
  where
    check term typ =
      typeCheckEmpty term @?= Right typ

unit_typecheck_env = do
    checkEnv (M.singleton "f" (Arrow Bool Bool)) (Abs "x" Bool (App (Var "f") (If (Var "x") (BoolLit False) (Var "x")))) (Arrow Bool Bool)
  where
    checkEnv env term typ = typeCheck env term @?= Right typ
