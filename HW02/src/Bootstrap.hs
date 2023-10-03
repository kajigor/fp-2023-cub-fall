module Bootstrap where 

import qualified SkewHeap as P

data Heap a = Empty | Full { root :: a, rest :: (P.SkewHeap (Heap a)) } deriving (Show, Eq, Ord)

leaf :: a -> Heap a
leaf x = Full x []

findMin :: Heap a -> a
findMin Empty = error "empty"
findMin (Full x _) = x

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge a Empty = a
merge Empty b = b
merge a@(Full x xs) b@(Full y ys) | x <= y = Full x (P.insert b xs)
                                  | otherwise = Full y (P.insert a ys)

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (leaf x) h

insertAll :: (Ord a) => [a] -> Heap a -> Heap a
insertAll xs h = foldr insert h xs

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "empty"
deleteMin (Full x []) = Empty
deleteMin (Full x xs) = let (Full y ys, xs') = P.popMin xs in Full y (P.merge ys xs')

popMin :: (Ord a) => Heap a -> (a, Heap a)
popMin h = (findMin h, deleteMin h)