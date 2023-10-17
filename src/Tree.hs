{-# LANGUAGE FunctionalDependencies #-}

module Tree (Tree(insert, remove, member, fromOrdList, toOrdList)) where

class Ord a => Tree tree a | tree -> a where
    insert :: a -> tree -> tree
    remove :: a -> tree -> tree
    member :: a -> tree -> Bool
    fromOrdList :: [a] -> tree
    toOrdList :: tree -> [a]
