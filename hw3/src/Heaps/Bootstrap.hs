{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Heaps.Bootstrap where 

import Heaps.Heap

data BootstrapHeap h a = Empty | Full { root :: a, rest :: (h (BootstrapHeap h a)) }

deriving instance (Show a, Show (h (BootstrapHeap h a))) => Show (BootstrapHeap h a)
deriving instance (Eq a, Eq (h (BootstrapHeap h a))) => Eq (BootstrapHeap h a)
deriving instance (Ord a, Ord (h (BootstrapHeap h a))) => Ord (BootstrapHeap h a)

instance (forall a. Ord a => Ord (h (BootstrapHeap h a)), Heap h) => Heap (BootstrapHeap h) where

    empty = Empty

    isEmpty Empty = True
    isEmpty _ = False

    leaf x = Full x empty

    findMin Empty = error "empty"
    findMin (Full x _) = x

    merge a Empty = a
    merge Empty b = b
    merge a@(Full x xs) b@(Full y ys) | x <= y = Full x (insert b xs)
                                      | otherwise = Full y (insert a ys)

    deleteMin Empty = error "empty"
    deleteMin (Full x xs) 
        | isEmpty xs = Empty
        | otherwise = let (Full y ys, xs') = popMin xs in Full y (merge ys xs')