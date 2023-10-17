module RandomAccessList (RAList, emptyRAList, isEmpty, cons, head, tail, lookup, update, toList, fromList) where
import Prelude hiding (tail, head, lookup)

data Tree a = Leaf a | Node a (Tree a) (Tree a)
type RAList a = [(Int, Tree a)]

emptyRAList :: RAList a
emptyRAList = []

isEmpty :: RAList a -> Bool
isEmpty [] = True
isEmpty _ = False

cons :: a -> RAList a -> RAList a
cons x ts@( (w1, t1) : (w2, t2) : ts' )
    | w1 == w2 = (w1 + w2 + 1, Node x t1 t2) : ts'
    | otherwise = (1, Leaf x) : ts
cons x ts = (1, Leaf x) : ts

head :: RAList a -> a
head ((1, Leaf x) : _) = x
head ((_, Node x _ _) : _) = x

tail :: RAList a -> RAList a
tail ((1, Leaf _) : ts) = ts
tail ((w, Node _ t1 t2) : ts) = (w `div` 2, t1) : (w `div` 2, t2) : ts

lookup :: Int -> RAList a -> a
lookup i ((w, t) : ts)
    | i < w = lookupTree i w t
    | otherwise = lookup (i - w) ts

update :: Int -> a -> RAList a -> RAList a
update i y ((w, t) : ts)
    | i < w = (w, updateTree i y w t) : ts
    | otherwise = (w, t) : update (i - w) y ts

toList :: RAList a -> [a]
toList = foldr (appendTreeToList . snd) []
    where
        appendTreeToList :: Tree a -> [a] -> [a]
        appendTreeToList (Leaf x) list = x : list
        appendTreeToList (Node x t1 t2) list =
            x : appendTreeToList t1 (appendTreeToList t2 list)

fromList :: [a] -> RAList a
fromList = foldr cons emptyRAList

lookupTree :: Int -> Int -> Tree a -> a
lookupTree 0 1 (Leaf x) = x
lookupTree 0 _ (Node x _ _) = x
lookupTree i w (Node _ t1 t2)
    | i - 1 < w `div` 2 = lookupTree (i - 1) (w `div` 2) t1
    | otherwise = lookupTree (i - 1 - w `div` 2) (w `div` 2) t2
lookupTree _ _ _ = undefined

updateTree :: Int -> a -> Int -> Tree a -> Tree a
updateTree 0 y 1 (Leaf _) = Leaf y
updateTree 0 y _ (Node _ t1 t2) = Node y t1 t2
updateTree i y w (Node x t1 t2)
    | i - 1 < w `div` 2 = Node x (updateTree (i - 1) y (w `div` 2) t1) t2
    | otherwise = Node x t1 (updateTree (i - 1 - w `div` 2) y (w `div` 2) t2)