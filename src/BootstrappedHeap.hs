{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module BootstrappedHeap (BootstrappedHeap, insert, merge, getMin, popMin, emptyHeap, isEmpty) where

import qualified SkewBinaryHeap as SBHeap
import Heap

data R a = R a (SBHeap.SkewBinaryHeap (R a)) deriving (Eq, Ord, Show)

data BootstrappedHeap a = Empty | BootstrappedHeap (R a) deriving Show

instance Ord a => Heap (BootstrappedHeap a) a where
    insert :: a -> BootstrappedHeap a -> BootstrappedHeap a
    insert x = merge (BootstrappedHeap (R x SBHeap.emptyHeap))

    merge :: BootstrappedHeap a -> BootstrappedHeap a -> BootstrappedHeap a
    merge Empty heap = heap
    merge heap Empty = heap
    merge (BootstrappedHeap r1@(R x h1)) (BootstrappedHeap r2@(R y h2))
        | x <= y = BootstrappedHeap $ R x (SBHeap.insert r2 h1)
        | otherwise = BootstrappedHeap $ R y (SBHeap.insert r1 h2)

    getMin :: BootstrappedHeap a -> a
    getMin (BootstrappedHeap (R a _)) = a

    popMin :: BootstrappedHeap a -> (a, BootstrappedHeap a)
    popMin (BootstrappedHeap (R x h))
        | SBHeap.isEmpty h = (x, Empty)
        | otherwise = (x, BootstrappedHeap $ R y (SBHeap.merge h1 h2))
            where
                (R y h1, h2) = SBHeap.popMin h
    
    isEmpty :: BootstrappedHeap a -> Bool
    isEmpty Empty = True
    isEmpty _ = False

    emptyHeap :: BootstrappedHeap a
    emptyHeap = Empty