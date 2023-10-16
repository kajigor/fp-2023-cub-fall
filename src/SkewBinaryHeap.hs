{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module SkewBinaryHeap (SkewBinaryHeap, insert, merge, getMin, popMin, isEmpty, emptyHeap) where
import Heap

data Tree a = Node {
    rank :: Int,
    value :: a,
    singletonNodes :: [a],
    nodes :: [Tree a]
} deriving (Eq, Ord, Show)

type SkewBinaryHeap a = [Tree a]

instance Ord a => Heap (SkewBinaryHeap a) a where
    insert :: a -> SkewBinaryHeap a -> SkewBinaryHeap a
    insert x ts@(t1 : t2 : rs)
        | rank t1 == rank t2 = skewLink x t1 t2 : rs
        | otherwise = heapFrom x : ts
    insert x ts = heapFrom x : ts

    merge :: SkewBinaryHeap a -> SkewBinaryHeap a -> SkewBinaryHeap a
    merge ts1 ts2 = _merge (normalizeHeap ts1) (normalizeHeap ts2)
        where
            _merge :: Ord a => SkewBinaryHeap a -> SkewBinaryHeap a -> SkewBinaryHeap a
            _merge [] ts2 = ts2
            _merge ts1 [] = ts1
            _merge h1@(t1 : ts1) h2@(t2 : ts2)
                | rank t1 < rank t2 = t1 : _merge ts1 h2
                | rank t1 > rank t2 = t2 : _merge h1 ts2
                | otherwise = normalizeHeap $ link t1 t2 : _merge ts1 ts2

    getMin :: SkewBinaryHeap a -> a
    getMin [t] = value t
    getMin (t : ts) = min (value t) (getMin ts)

    popMin :: SkewBinaryHeap a -> (a, SkewBinaryHeap a)
    popMin ts = do
        let (t, ts') = getMinTree ts
        let heap = foldl (flip insert) (merge (reverse $ nodes t) ts') (singletonNodes t)
        (value t, heap)

        where
            getMinTree [t] = (t, [])
            getMinTree (t : ts) = do
                let (t', ts') = getMinTree ts in
                    if value t <= value t'
                        then (t, ts)
                        else (t', t : ts')
    
    isEmpty :: SkewBinaryHeap a -> Bool
    isEmpty [] = True
    isEmpty _ = False

    emptyHeap :: SkewBinaryHeap a
    emptyHeap = []



heapFrom x = Node { rank = 0, value = x, singletonNodes = [], nodes = [] }

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node { value = x1, rank = r }) t2@(Node { value = x2 })
    | x1 <= x2 = t1 { rank = r + 1, nodes = t2 : nodes t1 }
    | otherwise = t2 { rank = r + 1, nodes = t1 : nodes t2 }

normalizeHeap :: Ord a => SkewBinaryHeap a -> SkewBinaryHeap a
normalizeHeap heap@(t1 : t2 : ts)
    | rank t1 == rank t2 = normalizeHeap $ link t1 t2 : ts
    | otherwise = heap
normalizeHeap heap = heap


skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = do
    let t@(Node { value = y }) = link t1 t2
    if x <= y
        then t { value = x, singletonNodes = y : singletonNodes t }
        else t { singletonNodes = x : singletonNodes t }
