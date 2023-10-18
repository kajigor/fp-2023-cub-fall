{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RBTree () where

data Color = R | B
data RBTree x = Leaf | Node Color (RBTree x) x (RBTree x)


pattern Red :: RBTree x -> x -> RBTree x -> RBTree x
pattern Red l x r = Node R l x r

pattern Blk :: RBTree x -> x -> RBTree x -> RBTree x
pattern Blk l x r = Node B l x r

asBlack (Node _ l m r) = Blk l m r
asBlack Leaf = Leaf

asRed (Node _ l m r) = Red l m r
asRed Leaf = Leaf

insert :: (Ord x) => RBTree x -> x -> RBTree x
insert t x = asBlack (ins t)
    where
        ins Leaf = Node R Leaf x Leaf
        ins n@(Node c l y r)
            | x < y = balance $ Node c (ins l) y r
            | x > y = balance $ Node c l y (ins r)
            | otherwise = n

balance (Blk (Red (Red a b c) d e) f g) = Red (Blk a b c) d (Blk e f g)
balance (Blk (Red a b (Red c d e)) f g) = Red (Blk a b c) d (Blk e f g)
balance (Blk a b (Red (Red c d e) f g)) = Red (Blk a b c) d (Blk e f g)
balance (Blk a b (Red c d (Red e f g))) = Red (Blk a b c) d (Blk e f g)
balance n = n

insert' :: (Ord x) => RBTree x -> x -> RBTree x
insert' t x = asBlack (ins t)
    where
        ins Leaf = Node R Leaf x Leaf
        ins n@(Node c l y r)
            | x < y = lbalance $ Node c (ins l) y r
            | x > y = rbalance $ Node c l y (ins r)
            | otherwise = n
        
lbalance (Blk (Red (Red a b c) d e) f g) = Red (Blk a b c) d (Blk e f g)
lbalance (Blk (Red a b (Red c d e)) f g) = Red (Blk a b c) d (Blk e f g)
lbalance n = n

rbalance (Blk a b (Red (Red c d e) f g)) = Red (Blk a b c) d (Blk e f g)
rbalance (Blk a b (Red c d (Red e f g))) = Red (Blk a b c) d (Blk e f g)
rbalance n = n


fromOrdList :: (Ord x) => [x] -> RBTree x
fromOrdList = foldl insert Leaf


remove :: (Ord a) => RBTree a -> a -> RBTree a
remove t x = asBlack (del t)
    where
        del n@(Node _ l y r)
            | x < y = delL n
            | x > y = delR n
            | otherwise = fuse l r

        delL t@(Blk a b c) = balL $ Blk (del a) b c
        delL t@(Red a b c) = Red (del a) b c

        delR t@(Blk a b c) = balR $ Blk a b (del c)
        delR t@(Red a b c) = Red a b (del c)

        balL (Blk a@(Red _ _ _) b c) = Red (asBlack a) b c
        balL (Blk a b c@(Blk _ _ _)) = rbalance (Blk a b (asRed c))
        balL (Blk a b (Red (Blk c d e) f g@(Blk _ _ _))) = Red (Blk a b c) d (rbalance (Blk e f (asRed g)))
        balL tree = tree

        balR (Blk a b c@(Red _ _ _)) = Red a b (asBlack c)
        balR (Blk a@(Blk _ _ _) b c) = lbalance (Blk (asRed a) b c)
        balR (Blk (Red a@(Blk _ _ _) b (Blk c d e)) f g) = Red (lbalance (Blk (asRed a) b c)) d (Blk e f g)
        balR tree = tree

        fuse Leaf t = t
        fuse t Leaf = t
        fuse a@(Blk _ _ _) (Red b c d) = Red (fuse a b) c d
        fuse (Red a b c) d@(Blk _ _ _) = Red a b (fuse c d)
        fuse (Red a b c) (Red e f g)  =
            let cde = fuse c e
            in case cde of
                (Red c' d' e') -> (Red (Red a b c') d' (Red e' f g))
                (Blk _ _ _)   -> (Red a b (Red cde f g))
        fuse (Blk a b c) (Blk e f g)  =
            let cde = fuse c e
            in case cde of
                (Red c' d' e') -> (Red (Blk a b c') d' (Blk e' f g))
                (Blk _ _ _) -> balL (Blk a b (Blk cde f g))