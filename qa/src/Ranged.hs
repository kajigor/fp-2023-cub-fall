{-# LANGUAGE InstanceSigs #-}
module Ranged where

-- Type class for containers with the minimum and the maximum values
class Ranged t where
  maximumValue :: Ord a => t a -> a
  minimumValue :: Ord a => t a -> a

-- To find the minimum and maximum values of a simple list, we just traverse the list
instance Ranged [] where
  maximumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h > curMax = go h t
                        | otherwise = go curMax t

  minimumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h < curMax = go h t
                        | otherwise = go curMax t

-- A special newtype for a sorted list
newtype SortedList a = Sorted { getSorted :: [a] } deriving (Show, Eq)

-- The minimum and the maximum values of a sorted list are always on its ends
instance Ranged SortedList where
  maximumValue = last . getSorted
  minimumValue = head . getSorted
