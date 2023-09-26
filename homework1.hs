data Colour = R | B deriving (Show, Eq)
data Tree a = E | T Colour (Tree a) a (Tree a) deriving (Show, Eq)

-- Classic Functional Red-Black Tree

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c t1 a t2 = T c t1 a t2

insert :: Ord a => a -> Tree a -> Tree a
insert x s = makeBlack (ins s)
    where
        ins E = T R E x E
        ins s@(T colour a y b)
            | x < y = balance colour (ins a) y b
            | x > y = balance colour a y (ins b)
            | otherwise = s
        makeBlack (T _ a y b) = T B a y b 

-- Imperative-like Red-Black Tree 

balance' :: Colour -> Tree a -> a -> Tree a -> Tree a 
    -- Color flips 
balance' B (T R a@(T R _ _ _) x b) y (T R c z d) = T R (T B a x b) y (T B c z d) 
balance' B (T R a x b@(T R _ _ _)) y (T R c z d) = T R (T B a x b) y (T B c z d) 
balance' B (T R a x b) y (T R c@(T R _ _ _) z d) = T R (T B a x b) y (T B c z d) 
balance' B (T R a x b) y (T R c z d@(T R _ _ _)) = T R (T B a x b) y (T B c z d) 
    -- Single rotations 
balance' B (T R a@(T R _ _ _)x b) y c = T B a x (T R b y c) 
balance' B a x (T R b y c@(T R _ _ _)) = T B (T R a x b) y c 
    -- Double rotations 
balance' B (T R a x (T R b y c)) z d = T B (T R a x b) y (T R c z d) 
balance' B a x (T R (T R b y c) z d) = T B (T R a x b) y (T R c z d) 
    -- No balancing necessary
balance' color a x b = T color a x b 

    -- Upgraded insert 

insert' :: Ord a => a -> Tree a -> Tree a 
insert' x s = makeBlack (ins s) 
    where 
        ins E = T R E x E 
        ins s@(T colour a y b) 
            | x < y = balance' colour (ins a) y b 
            | x > y = balance' colour a y (ins b) 
            | otherwise = s 
        makeBlack (T _ a y b) = T B a y b

-- Implementation of fromOrdList 

fromOrdList :: Ord a => [a] -> Tree a
fromOrdList xs = paintRedAtDepth (build xs) (height (build xs))

build :: Ord a => [a] -> Tree a
build xs@(_:_:_:_:_:_:_:_:_) =
    let (ys, z:zs) = splitAt (length xs `div` 2) xs
        (l, m, r) = (ys, z, zs)
    in T B (build l) m (build r)
build [] = E
build [x] = T B E x E

height :: Tree a -> Int
height E = 0
height (T _ l _ r) = 1 + max (height l) (height r)

paintRedAtDepth :: Tree a -> Int -> Tree a
paintRedAtDepth (T c l m r) d
    | d == 1 = T R l m r
    | d > 1 = T c (paintRedAtDepth l (d - 1)) m (paintRedAtDepth r (d - 1))
paintRedAtDepth E _ = E

-- And just with fold, wrong optimization, but still works

fromOrdList' :: Ord a => [a] -> Tree a
fromOrdList' = foldl (flip insert) E 

-- Left and right balance

lbalance :: Tree a -> a -> Tree a -> Tree a
lbalance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance a x b = T B a x b

rbalance :: Tree a -> a -> Tree a -> Tree a
rbalance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance a x b = T B a x b 

-- Upgraded insert

insert'' :: Ord a => a -> Tree a -> Tree a
insert'' x s = makeBlack (ins s)
    where
        ins E = T R E x E
        ins s@(T colour a y b)
            | x < y = lbalance (ins a) y b
            | x > y = rbalance a y (ins b)
            | otherwise = s
        makeBlack (T _ a y b) = T B a y b

-- Testing 

main :: IO ()
main = do
    let sortedList = [1..9]
        tree = fromOrdList sortedList
    print tree