{-# LANGUAGE NoMonomorphismRestriction #-}

module Final where

import Common
import Text.Printf
import Control.Monad.State hiding (fix)
import Prelude hiding (compare)

-- Finally tagless, partially evaluated
-- Oleg Kiselyov

-- data Dsl env t where
--   IntConst :: Int -> Dsl env Int
--   BoolConst :: Bool -> Dsl env Bool

--   IntBin  :: BinOp (Int -> Int -> Int)    -> Dsl env Int  -> Dsl env Int  -> Dsl env Int
--   BoolBin :: BinOp (Bool -> Bool -> Bool) -> Dsl env Bool -> Dsl env Bool -> Dsl env Bool
--   Compare :: BinOp (Int -> Int -> Bool)   -> Dsl env Int  -> Dsl env Int  -> Dsl env Bool
--   IfE :: Dsl env Bool -> Dsl env t -> Dsl env t -> Dsl env t

--   Var :: env t -> Dsl env t
--   Lam :: (Dsl env t1 -> Dsl env t2) -> Dsl env (t1 -> t2)
--   App :: Dsl env (t1 -> t2) -> Dsl env t1 -> Dsl env t2
--   Fix :: (Dsl env t -> Dsl env t) -> Dsl env t

class Calc repr where
  intConst :: Int -> repr Int
  intBin :: BinOp (Int -> Int -> Int) -> repr Int -> repr Int -> repr Int

class Lam repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fix :: (repr a -> repr a) -> repr a

instance Calc Env where
  intConst n = Env n
  intBin (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)

instance Lam Env where
  lam f = Env $ \x -> unEnv (f (Env x))
  app f x = Env $
    let f' = unEnv f in
    let x' = unEnv x in
    f' x'
  fix f = Env $ fx (unEnv . f . Env) where fx f = f (fx f)

instance Calc S where
  intConst n = S $ return $ show n
  intBin op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y

instance Lam S where
  lam f = S $ do
    v <- newVar
    let var = printf "x.%s" (show v) :: String
    x <- unS (f (S $ return var))
    return $ printf "\\%s -> %s" var x
  app x y = S $ do
    x <- unS x
    y <- unS y
    return $ printf "(%s) (%s)" x y
  fix f = S $ do
    v <- newVar
    let self = printf "self.%s" (show v) :: String
    b <- unS (f (S $ return self))
    return $ printf "(fix %s . %s)" self b

term :: (Calc repr, Lam repr) => repr Int
term = app (lam $ \x -> intBin (BinOp "+" (+)) x x) (intConst 42)


class Cond repr where
  boolConst :: Bool -> repr Bool

  boolBin :: BinOp (Bool -> Bool -> Bool) -> repr Bool -> repr Bool -> repr Bool
  compare :: BinOp (Int -> Int -> Bool) -> repr Int -> repr Int -> repr Bool
  ifExpr :: repr Bool -> repr t -> repr t -> repr t

instance Cond Env where
  boolConst b = Env b

  boolBin (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)
  compare (BinOp _ f) x y = Env $ f (unEnv x) (unEnv y)
  ifExpr cond thn els = Env $ if unEnv cond then unEnv thn else unEnv els

instance Cond S where
  boolConst b = S $ return $ show b

  boolBin op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y
  compare op x y = S $ do
    x <- unS x
    y <- unS y
    return $ formatBinOp op x y
  ifExpr cond thn els = S $ do
    cond <- unS cond
    thn <- unS thn
    els <- unS els
    return $ printf "if %s then %s else %s" cond thn els

eval :: Env a -> a
eval = unEnv

view :: S a -> String
view x = evalState (unS x) (VarState 0)

andOp :: Cond repr => repr Bool -> repr Bool -> repr Bool
andOp = boolBin (BinOp "&&" (&&))

mulOp :: Calc repr => repr Int -> repr Int -> repr Int
mulOp = intBin (BinOp "*" (*))

addOp :: Calc repr => repr Int -> repr Int -> repr Int
addOp = intBin (BinOp "+" (+))

leqOp :: Cond repr => repr Int -> repr Int -> repr Bool
leqOp = compare (BinOp "<=" (<=))

tipow :: (Cond repr, Calc repr, Lam repr) => repr (Int -> Int -> Int)
tipow  = lam (\x -> fix (\self -> lam (\n ->
                        ifExpr  (leqOp n (intConst 0))
                                (intConst 1)
                                (mulOp x (app self (addOp n (intConst (-1))))))))

tipowApplied :: (Cond repr, Calc repr, Lam repr) => Int -> Int -> repr Int
tipowApplied x y = app (app tipow (intConst x)) (intConst y)

main = do
  let expr = tipowApplied 4 2
  putStrLn $ view expr
  putStrLn $ show $ eval expr
