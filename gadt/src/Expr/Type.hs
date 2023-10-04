{-# LANGUAGE GADTs, RankNTypes, TypeOperators, StandaloneDeriving #-}

module Expr.Type where

import Data.Type.Equality

data Type a where
  Int :: Type Int
  Bool :: Type Bool

eq :: Type a -> Type b -> Maybe (a :~: b)
eq Int Int = Just Refl
eq Bool Bool = Just Refl
eq _ _ = Nothing