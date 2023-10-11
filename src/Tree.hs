{-# LANGUAGE MultiParamTypeClasses #-}

module Tree (Tree(insert, remove, member, fromOrdList)) where

class Ord a => Tree tree a where
    insert :: a -> tree -> tree
    remove :: a -> tree -> tree
    member :: a -> tree -> Bool
    fromOrdList :: [a] -> tree

