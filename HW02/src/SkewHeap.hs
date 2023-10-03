module SkewHeap where


data SkewTree a = Node { rank :: Int, val :: a, singles :: [a], children :: [SkewTree a] } deriving (Show, Eq, Ord)

type SkewHeap a = [SkewTree a]

link :: (Ord a) => SkewTree a -> SkewTree a -> SkewTree a
link a@(Node { val = x }) b@(Node { val = y }) | x <= y = a { rank = rank a + 1, children = b : children a }
                                               | otherwise = b { rank = rank b + 1, children = a : children b }

skewLink :: (Ord a) => a -> SkewTree a -> SkewTree a -> SkewTree a
skewLink x a b = let c = link a b in c { singles = x : singles c }

leaf :: a -> SkewTree a
leaf x = Node { rank = 0, val = x, singles = [], children = [] }

insert :: (Ord a) => a -> SkewHeap a -> SkewHeap a 
insert x h@(a:b:ts) | rank a == rank b = skewLink x a b : ts
                    | otherwise = leaf x : h
insert x h = leaf x : h

insertAll :: (Ord a) => [a] -> SkewHeap a -> SkewHeap a
insertAll x h = foldr insert h x

insTree :: (Ord a) => SkewTree a -> SkewHeap a -> SkewHeap a
insTree t [] = [t]
insTree t (t':h) | rank t < rank t' = t : h
                 | otherwise = insTree (link t t') h

norm :: (Ord a) => SkewHeap a -> SkewHeap a
norm [] = []
norm (t:h) = insTree t h

mergeNorm :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeNorm [] b = b
mergeNorm a [] = a
mergeNorm a@(x:xs) b@(y:ys) | rank x < rank y = x : mergeNorm xs b
                            | rank y < rank x = y : mergeNorm a ys
                            | otherwise = insTree (link x y) (merge xs ys)

merge :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
merge a b = mergeNorm (norm a) (norm b)

findMin :: (Ord a) => SkewHeap a -> a
findMin [] = error "empty"
findMin [t] = val t
findMin (t:h) = min (val t) (findMin h)

splitMin :: (Ord a) => SkewHeap a -> (SkewTree a, SkewHeap a)
splitMin [] = error "empty"
splitMin [t] = (t, [])
splitMin (t:h) = let (t', h') = splitMin h in if val t < val t' then (t, h) else (t', t : h')

deleteMin :: (Ord a) => SkewHeap a -> SkewHeap a
deleteMin h = let (t, h') = splitMin h in insertAll (singles t) $ mergeNorm (reverse (children t)) (norm h')

popMin :: (Ord a) => SkewHeap a -> (a, SkewHeap a)
popMin h = (findMin h, deleteMin h)