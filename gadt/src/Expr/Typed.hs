{-# LANGUAGE GADTs, RankNTypes, TypeOperators, StandaloneDeriving #-}

module Expr.Typed where

import Data.Type.Equality
import qualified Expr.Untyped as U
import Expr.Type

data Expr a where
  IntLit :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Pair :: Expr a -> Expr b -> Expr (a, b)
  Fst :: Expr (a, b) -> Expr a
  Snd :: Expr (a, b) -> Expr b

eval :: Expr a -> a
eval (IntLit i) = i
eval (BoolLit b) = b
eval (Add x y) = eval x + eval y
eval (IsZero x) = eval x == 0
eval (If c t e) = if eval c then eval t else eval e
eval (Pair a b) = (eval a, eval b)
eval (Fst ab) = fst $ eval ab
eval (Snd ab) = snd $ eval ab

check :: U.Expr -> (forall a. Show a => Expr a -> Maybe b) -> Maybe b
check e k = go e $ \e _ -> k e
  where
    go :: U.Expr -> (forall a. Show a => Expr a -> Type a -> Maybe b) -> Maybe b
    go (U.IntLit i) k = k (IntLit i) Int
    go (U.BoolLit b) k = k (BoolLit b) Bool
    go (U.Add x y) k =
      go x $ \x tx ->
        go y $ \y ty ->
          case eq tx ty of
            Just Refl ->
              case eq ty Int of
                Just Refl -> k (Add x y) Int
                _ -> Nothing
            _ -> Nothing
    go (U.IsZero x) k =
      go x $ \x tx ->
        case eq tx Int of
          Just Refl -> k (IsZero x) Bool
          _ -> Nothing
    go (U.If c t e) k =
      go c $ \c tc ->
        case eq tc Bool of
          Just Refl ->
            go t $ \t tt ->
              go e $ \e te ->
                case eq tt te of
                  Just Refl -> k (If c t e) te
                  _ -> Nothing
          _ -> Nothing
    go (U.Pair a b) k =
      go a $ \a ta ->
        go b $ \b tb ->
          k (Pair a b) (TPair ta tb)
    go (U.Fst ab) k =
      go ab $ \ab tab ->
        case tab of
          TPair ta _ -> k (Fst ab) ta
          _ -> Nothing
    go (U.Snd ab) k =
      go ab $ \ab tab ->
        case tab of
          TPair _ tb -> k (Snd ab) tb
          _ -> Nothing