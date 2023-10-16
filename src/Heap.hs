{-# LANGUAGE FunctionalDependencies #-}

module Heap (Heap(insert, merge, getMin, popMin, isEmpty, emptyHeap)) where

class Ord a => Heap heap a | heap -> a where
    insert :: a -> heap -> heap
    merge :: heap -> heap -> heap
    getMin :: heap -> a
    popMin :: heap -> (a, heap)
    isEmpty :: heap -> Bool
    emptyHeap :: heap