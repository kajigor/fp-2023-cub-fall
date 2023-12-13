{-# LANGUAGE GADTs, RankNTypes, TypeOperators, StandaloneDeriving #-}

module Expr.Type where

import Data.Type.Equality

data Type a where
  Int :: Type Int
  Bool :: Type Bool
  TPair :: (Show a, Show b) => Type a -> Type b -> Type (a, b)

eq :: Type a -> Type b -> Maybe (a :~: b)
eq Int Int = Just Refl
eq Bool Bool = Just Refl
eq (TPair a b) (TPair a' b') = case eq a a' of
  Just Refl -> case eq b b' of
    Just Refl -> Just Refl
    _ -> Nothing
  _ -> Nothing
eq _ _ = Nothing