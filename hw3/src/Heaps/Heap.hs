module Heaps.Heap where


class Heap h where

    empty :: h a

    isEmpty :: h a -> Bool

    leaf :: a -> h a

    findMin :: (Ord a) => h a -> a

    merge :: (Ord a) => h a -> h a -> h a

    insert :: (Ord a) => a -> h a -> h a
    insert x h = merge (leaf x) h

    insertAll :: (Ord a) => [a] -> h a -> h a
    insertAll xs h = foldr insert h xs

    deleteMin :: (Ord a) => h a -> h a

    popMin :: (Ord a) => h a -> (a, h a)
    popMin h = (findMin h, deleteMin h)
