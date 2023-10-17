{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module ImperativeRBTree ( ImperativeRBTree, ImperativeDividedRBTree ) where

import Tree
import RBTreeCommon

newtype ImperativeRBTree a = ImperativeRBTree (RBTree a)

instance Ord a => Tree (ImperativeRBTree a) a where
    insert x (ImperativeRBTree tree) = ImperativeRBTree $ insertImpl balanceImperative x tree
    remove x (ImperativeRBTree tree) = ImperativeRBTree $ removeImpl balanceImperative x tree
    member x (ImperativeRBTree tree) = memberImpl x tree
    fromOrdList = ImperativeRBTree . fromOrdListImpl
    toOrdList (ImperativeRBTree tree) = toOrdListImpl tree

instance Show a => Show (ImperativeRBTree a) where
   show (ImperativeRBTree tree) = show tree

newtype ImperativeDividedRBTree a = ImperativeDividedRBTree (RBTree a)

instance Ord a => Tree (ImperativeDividedRBTree a) a where
    insert x (ImperativeDividedRBTree tree) = ImperativeDividedRBTree $ insertDividedImpl balanceImperativeL balanceImperativeR x tree
    remove x (ImperativeDividedRBTree tree) = ImperativeDividedRBTree $ removeImpl balanceImperative x tree
    member x (ImperativeDividedRBTree tree) = memberImpl x tree
    fromOrdList = ImperativeDividedRBTree . fromOrdListImpl
    toOrdList (ImperativeDividedRBTree tree) = toOrdListImpl tree

instance Show a => Show (ImperativeDividedRBTree a) where
   show (ImperativeDividedRBTree tree) = show tree

balanceImperative :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceImperative Black (Node Red a@(Node Red _ _ _) x b) y (Node Red c z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperative Black (Node Red a x b@(Node Red _ _ _)) y (Node Red c z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperative Black (Node Red a x b) y (Node Red c@(Node Red _ _ _) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperative Black (Node Red a x b) y (Node Red c z d@(Node Red _ _ _)) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperative Black (Node Red a@(Node Red _ _ _) x b) y c = Node Black a x (Node Red b y c)
balanceImperative Black a x (Node Red b y c@(Node Red _ _ _)) = Node Black (Node Red a x b) y c
balanceImperative Black (Node Red a x (Node Red b y c)) z d = Node Black (Node Red a x b) y (Node Red c z d)
balanceImperative Black a x (Node Red (Node Red b y c) z d) = Node Black (Node Red a x b) y (Node Red c z d)
balanceImperative color left x right = Node color left x right

balanceImperativeL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceImperativeL Black (Node Red a@(Node Red _ _ _) x b) y (Node Red c z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperativeL Black (Node Red a x b@(Node Red _ _ _)) y (Node Red c z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperativeL Black (Node Red a@(Node Red _ _ _) x b) y c = Node Black a x (Node Red b y c)
balanceImperativeL Black (Node Red a x (Node Red b y c)) z d = Node Black (Node Red a x b) y (Node Red c z d)
balanceImperativeL color left x right = Node color left x right

balanceImperativeR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceImperativeR Black (Node Red a x b) y (Node Red c@(Node Red _ _ _) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperativeR Black (Node Red a x b) y (Node Red c z d@(Node Red _ _ _)) = Node Red (Node Black a x b) y (Node Black c z d)
balanceImperativeR Black a x (Node Red b y c@(Node Red _ _ _)) = Node Black (Node Red a x b) y c
balanceImperativeR Black a x (Node Red (Node Red b y c) z d) = Node Black (Node Red a x b) y (Node Red c z d)
balanceImperativeR color left x right = Node color left x right
