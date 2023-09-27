module Reverse where

-- Simple implementation of list reversal.
-- It's obvious that it works correctly.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h : t) = reverseList t ++ [h]

-- reverse (reverse xs) === xs

-- Faster implementation of list reversal.
-- It's less clear that it works correctly.
fastReverseList :: [a] -> [a]
fastReverseList =
    go []
  where
    go acc [] = acc
    go acc (h : t) = go (h : acc) t
