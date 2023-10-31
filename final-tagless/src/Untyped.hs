module Untyped where

import Common
import Control.Monad.State
import Debug.Trace

data Dsl
  = IntConst Int

  | Bin (BinOp (Int -> Int -> Int)) Dsl Dsl

  | Var String
  | Lam String Dsl
  | App Dsl Dsl
  deriving (Show)

newtype DslState = DslState { getState :: [(String, Int)] }

eval :: Dsl -> Int
eval term = evalState (eval' term) (DslState [])

eval' :: Dsl -> State DslState Int
eval' (IntConst n) = return n
eval' (Bin (BinOp _ f) x y) = do
  x <- eval' x
  y <- eval' y
  return $ x `f` y
eval' (Var v) = do
  state <- get
  case lookup v (getState state) of
    Just x -> return x
    Nothing -> error "No such var in scope"
eval' term@(App (Lam v e) arg) = do
  arg <- eval' arg
  state <- get
  put $ DslState { getState = (v, arg) : getState state }
  eval' e
eval' t = error $ show t

term = App (Lam "x" (Bin (BinOp "+" (+)) (Var "x") (Var "x"))) (IntConst 42)

-- pair = \f.\s.\b.b f s
pair = Lam "f" $ Lam "s" $ Lam "b" $ App (App (Var "b") (Var "f")) (Var "s")

-- frst = \p. p tru
frst = Lam "p" $ App (Var "p") tru

-- tru = \t.\f.t
tru = Lam "t" $ Lam "_f" $ Var "t"

fstOfPair = App frst (App (App pair (Var "u")) (Var "w"))

