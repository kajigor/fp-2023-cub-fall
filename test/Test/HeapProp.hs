module Test.HeapProp where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import SkewBinaryHeap
import BootstrappedHeap
import Heap
import Gen.GenCommon

import Data.List (unfoldr, sort)

prop_fromListToListSkew :: Property
prop_fromListToListSkew = property $ do
  list <- forAll $ genList 1 10 10
  let heap = fromList list :: SkewBinaryHeap Int
  let newList = unfoldr pop heap
  sort list === newList

prop_fromListToListBootstrapped :: Property
prop_fromListToListBootstrapped = property $ do
  list <- forAll $ genList 1 10 10
  let heap = fromList list :: BootstrappedHeap Int
  let newList = unfoldr pop heap
  sort list === newList

prop_mergeSkew :: Property
prop_mergeSkew = property $ do
  list <- forAll $ genListOfLists 1 10 10
  let newHeap = emptyHeap :: SkewBinaryHeap Int
  let heap = foldr (\l -> merge (fromList l :: SkewBinaryHeap Int)) newHeap list
  let newList = unfoldr pop heap
  sort (concat list) === newList

prop_mergeBootstrapped :: Property
prop_mergeBootstrapped = property $ do
  list <- forAll $ genListOfLists 1 10 10
  let newHeap = emptyHeap :: BootstrappedHeap Int
  let heap = foldr (\l -> merge (fromList l :: BootstrappedHeap Int)) newHeap list
  let newList = unfoldr pop heap
  sort (concat list) === newList

fromList :: Heap heap a => [a] -> heap
fromList = foldr insert emptyHeap

pop :: Heap heap a => heap -> Maybe (a, heap)
pop heap
  | isEmpty heap = Nothing
  | otherwise = Just $ popMin heap

props :: [TestTree]
props =
  [ testProperty "list == toList (fromList list) SkewBinaryHeap" prop_fromListToListSkew
  , testProperty "list == toList (fromList list) BootstrappedHeap" prop_fromListToListBootstrapped
  , testProperty "merge SkewBinaryHeap" prop_mergeSkew
  , testProperty "merge BootstrappedHeap" prop_mergeBootstrapped
  ]