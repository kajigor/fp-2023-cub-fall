{-# LANGUAGE RankNTypes #-}
module FIList where

-- equivalent to []
data IList a
  = Nil
  | Cons a (IList a)

sumIList Nil = 0
sumIList (Cons h t) = h + sumIList t

newtype FList a = FList { runFList :: forall r. (a -> r -> r) -> r -> r }

convertI2F :: [a] -> FList a
convertI2F lst = FList $ \c n -> foldr c n lst

convertF2I :: FList a -> [a]
convertF2I lst = runFList lst (:) []

appendF :: FList a -> FList a -> FList a
appendF (FList xs) (FList ys) = FList $ \cons nil -> xs cons $ ys cons nil

-- foldrF :: (a -> b -> b) -> b -> FList a -> b

mapF :: (a -> b) -> FList a -> FList b
mapF f (FList xs) = FList $ \cons nil -> xs (\h t -> cons (f h) t) nil