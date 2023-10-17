{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module Test.TreeProp where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import FunctionalRBTree
import ImperativeRBTree
import Tree
import Gen.GenCommon
import Gen.GenTree

import Control.Monad
import Data.List (sort, nub)

checkEqual :: MonadTest m => (tree1, tree2) -> [OpType tree1 tree2] -> m (tree1, tree2)
checkEqual = foldM (\(treel, treer) ((opl, opr), _) -> handleResult (opl treel) (opr treer))
  where
    handleResult :: MonadTest m => (tree1, ReturnType) -> (tree2, ReturnType) -> m (tree1, tree2)
    handleResult (treeL, retL) (treeR, retR) = do
      retL === retR
      return (treeL, treeR)

prop_functionalRBTreeEqualToFunctionalDividedRBTree :: Property
prop_functionalRBTreeEqualToFunctionalDividedRBTree = property $ do
  list <- forAllWith (show . fmap snd) (genOpList 1 10 10 :: Gen [OpType (FunctionalRBTree Int) (FunctionalDividedRBTree Int)])
  let emptyTreeL = fromOrdList ([]::[Int]) :: FunctionalRBTree Int
  let emptyTreeR = fromOrdList ([]::[Int]) :: FunctionalDividedRBTree Int

  _ <- checkEqual (emptyTreeL, emptyTreeR) list
  toOrdList emptyTreeL === toOrdList emptyTreeR

prop_functionalRBTreeEqualToImperativeRBTree  :: Property
prop_functionalRBTreeEqualToImperativeRBTree = property $ do
  list <- forAllWith (show . fmap snd) (genOpList 1 10 10 :: Gen [OpType (FunctionalRBTree Int) (ImperativeRBTree Int)])
  let emptyTreeL = fromOrdList ([]::[Int]) :: FunctionalRBTree Int
  let emptyTreeR = fromOrdList ([]::[Int]) :: ImperativeRBTree Int

  _ <- checkEqual (emptyTreeL, emptyTreeR) list
  toOrdList emptyTreeL === toOrdList emptyTreeR

prop_imperativeDividedRBTreeEqualToImperativeRBTree  :: Property
prop_imperativeDividedRBTreeEqualToImperativeRBTree = property $ do
  list <- forAllWith (show . fmap snd) (genOpList 1 10 10 :: Gen [OpType (ImperativeRBTree Int) (ImperativeDividedRBTree Int)])
  let emptyTreeL = fromOrdList ([]::[Int]) :: ImperativeRBTree Int
  let emptyTreeR = fromOrdList ([]::[Int]) :: ImperativeDividedRBTree Int

  _ <- checkEqual (emptyTreeL, emptyTreeR) list
  toOrdList emptyTreeL === toOrdList emptyTreeR

prop_toListFromListFunctionalRBTree :: Property
prop_toListFromListFunctionalRBTree = property $ do
  list <- forAll $ genUniqueSortedList 1 10 10
  list === toOrdList (fromOrdList list :: FunctionalRBTree Int)

prop_toListFromListFunctionalDividedRBTree :: Property
prop_toListFromListFunctionalDividedRBTree = property $ do
  list <- forAll $ genUniqueSortedList 1 10 10
  list === toOrdList (fromOrdList list :: FunctionalDividedRBTree Int)

prop_toListFromListImperativeRBTree :: Property
prop_toListFromListImperativeRBTree = property $ do
  list <- forAll $ genUniqueSortedList 1 10 10
  list === toOrdList (fromOrdList list :: ImperativeRBTree Int)

prop_toListFromListImperativeDividedRBTree :: Property
prop_toListFromListImperativeDividedRBTree = property $ do
  list <- forAll $ genUniqueSortedList 1 10 10
  list === toOrdList (fromOrdList list :: ImperativeDividedRBTree Int)

prop_insertToList :: Property
prop_insertToList = property $ do
  list <- forAll $ genList 1 10 10
  let emptyFTree = fromOrdList ([]::[Int]) :: FunctionalRBTree Int
  
  let newList = toOrdList $ foldr insert emptyFTree list
  nub (sort list) === newList


props :: [TestTree]
props =
  [ testProperty "FunctionalRBTree behaves as FunctionalDividedRBTree" prop_functionalRBTreeEqualToFunctionalDividedRBTree
  , testProperty "FunctionalRBTree behaves as ImperativeRBTree" prop_functionalRBTreeEqualToImperativeRBTree
  , testProperty "ImperativeRBTree behaves as ImperativeDividedRBTree" prop_imperativeDividedRBTreeEqualToImperativeRBTree
  , testProperty "list == toOrdList (fromOrdList list)" prop_toListFromListFunctionalRBTree
  , testProperty "list == toOrdList (fromOrdList list)" prop_toListFromListFunctionalDividedRBTree
  , testProperty "list == toOrdList (fromOrdList list)" prop_toListFromListImperativeRBTree
  , testProperty "list == toOrdList (fromOrdList list)" prop_toListFromListImperativeDividedRBTree
  , testProperty "list == toOrdList (insert list functionalTree)" prop_insertToList
  ]
