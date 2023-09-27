{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RBTree () where

data Color = R | B
data RBTree x = Leaf | Node Color (RBTree x) x (RBTree x)


pattern Red :: RBTree x -> x -> RBTree x -> RBTree x
pattern Red l x r = Node R l x r

pattern Black :: RBTree x -> x -> RBTree x -> RBTree x
pattern Black l x r = Node B l x r

insert :: (Ord x) => RBTree x -> x -> RBTree x
insert t x = asBlack (ins t)
    where
        asBlack (Node _ l m r) = Black l m r
        asBlack Leaf = Leaf

        ins Leaf = Node R Leaf x Leaf
        ins n@(Node c l y r)
            | x < y = balance $ Node c (ins l) y r
            | x > y = balance $ Node c l y (ins r)
            | otherwise = n

        balance (Black (Red (Red a b c) d e) f g) = Red (Black a b c) d (Black e f g)
        balance (Black (Red a b (Red c d e)) f g) = Red (Black a b c) d (Black e f g)
        balance (Black a b (Red (Red c d e) f g)) = Red (Black a b c) d (Black e f g)
        balance (Black a b (Red c d (Red e f g))) = Red (Black a b c) d (Black e f g)
        balance n = n

insert' :: (Ord x) => RBTree x -> x -> RBTree x
insert' t x = asBlack (ins t)
    where
        asBlack (Node _ l m r) = Black l m r
        asBlack Leaf = Leaf

        ins Leaf = Node R Leaf x Leaf
        ins n@(Node c l y r)
            | x < y = lbalance $ Node c (ins l) y r
            | x > y = rbalance $ Node c l y (ins r)
            | otherwise = n
        
        lbalance (Black (Red (Red a b c) d e) f g) = Red (Black a b c) d (Black e f g)
        lbalance (Black (Red a b (Red c d e)) f g) = Red (Black a b c) d (Black e f g)
        lbalance n = n

        rbalance (Black a b (Red (Red c d e) f g)) = Red (Black a b c) d (Black e f g)
        rbalance (Black a b (Red c d (Red e f g))) = Red (Black a b c) d (Black e f g)
        rbalance n = n


fromOrdList :: (Ord x) => [x] -> RBTree x
fromOrdList = foldl insert Leaf

