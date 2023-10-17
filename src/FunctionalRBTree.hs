{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FunctionalRBTree ( FunctionalRBTree, FunctionalDividedRBTree ) where
import Tree
import RBTreeCommon

newtype FunctionalRBTree a = FunctionalRBTree (RBTree a)

instance Ord a => Tree (FunctionalRBTree a) a where
    insert x (FunctionalRBTree tree) = FunctionalRBTree $ insertImpl balance x tree
    remove x (FunctionalRBTree tree) = FunctionalRBTree $ removeImpl balance x tree
    member x (FunctionalRBTree tree) = memberImpl x tree
    fromOrdList = FunctionalRBTree . fromOrdListImpl
    toOrdList (FunctionalRBTree tree) = toOrdListImpl tree

instance Show a => Show (FunctionalRBTree a) where
   show (FunctionalRBTree tree) = show tree


newtype FunctionalDividedRBTree a = FunctionalDividedRBTree (RBTree a)

instance Ord a => Tree (FunctionalDividedRBTree a) a where
    insert x (FunctionalDividedRBTree tree) = FunctionalDividedRBTree $ insertDividedImpl balanceL balanceR x tree
    remove x (FunctionalDividedRBTree tree) = FunctionalDividedRBTree $ removeImpl balance x tree
    member x (FunctionalDividedRBTree tree) = memberImpl x tree
    fromOrdList = FunctionalDividedRBTree . fromOrdListImpl
    toOrdList (FunctionalDividedRBTree tree) = toOrdListImpl tree

instance Show a => Show (FunctionalDividedRBTree a) where
   show (FunctionalDividedRBTree tree) = show tree

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance color left x right = Node color left x right

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balanceL Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balanceL color left x right = Node color left x right

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceR Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balanceR color left x right = Node color left x right
