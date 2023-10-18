module Heaps.SkewHeap where

import Heaps.Heap

data SkewTree a = Node { rank :: Int, val :: a, singles :: [a], children :: [SkewTree a] } deriving (Show, Eq, Ord)

newtype SkewHeap a = SkewHeap [SkewTree a] deriving (Show, Eq, Ord)

link :: (Ord a) => SkewTree a -> SkewTree a -> SkewTree a
link a@(Node { val = x }) b@(Node { val = y }) | x <= y = a { rank = rank a + 1, children = b : children a }
                                               | otherwise = b { rank = rank b + 1, children = a : children b }

skewLink :: (Ord a) => a -> SkewTree a -> SkewTree a -> SkewTree a
skewLink x a b = let c = link a b in c { singles = x : singles c }

insTree :: (Ord a) => SkewTree a -> SkewHeap a -> SkewHeap a
insTree t (SkewHeap []) = SkewHeap [t]
insTree t (SkewHeap (t':h)) 
    | rank t < rank t' = SkewHeap $ t : t' : h
    | otherwise = insTree (link t t') (SkewHeap h)

norm :: (Ord a) => SkewHeap a -> SkewHeap a
norm (SkewHeap []) = SkewHeap []
norm (SkewHeap (t:h)) = insTree t (SkewHeap h)

mergeNorm :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeNorm (SkewHeap []) b = b
mergeNorm a (SkewHeap []) = a
mergeNorm a@(SkewHeap (x:xs)) b@(SkewHeap (y:ys)) 
    | rank x < rank y = insTree x (mergeNorm xs' b)
    | rank y < rank x = insTree y (mergeNorm a ys')
    | otherwise = insTree (link x y) (merge xs' ys')
    where
        xs' = SkewHeap xs
        ys' = SkewHeap ys


splitMin :: (Ord a) => SkewHeap a -> (SkewTree a, SkewHeap a)
splitMin (SkewHeap []) = error "empty"
splitMin (SkewHeap [t]) = (t, empty)
splitMin (SkewHeap (t:h)) = let (t', SkewHeap h') = splitMin (SkewHeap h) in if val t < val t' then (t, SkewHeap h) else (t', SkewHeap $ t : h')

leaf' :: a -> SkewTree a
leaf' x = Node { rank = 0, val = x, singles = [], children = [] }

instance Heap SkewHeap where

    empty = SkewHeap []

    isEmpty (SkewHeap []) = True
    isEmpty _ = False

    leaf x = SkewHeap [leaf' x]

    merge a b = mergeNorm (norm a) (norm b)

    findMin (SkewHeap []) = error "empty"
    findMin (SkewHeap [t]) = val t
    findMin (SkewHeap (t:h)) = min (val t) (findMin (SkewHeap h))

    insert x (SkewHeap h@(a:b:ts)) | rank a == rank b = SkewHeap $ skewLink x a b : ts
                        | otherwise = SkewHeap $ leaf' x : h
    insert x (SkewHeap h) = SkewHeap $ leaf' x : h

    deleteMin h = let (t, h') = splitMin h in insertAll (singles t) $ mergeNorm (SkewHeap $ reverse (children t)) (norm h')
