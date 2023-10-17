module Test.QueueProp where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import NaiveQueue
import BankersQueue
import PhysicistsQueue
import RealTimeQueue
import Queue
import Gen.GenCommon

import Data.List (unfoldr)
import Prelude hiding (head, tail)

prop_fromListToListNative :: Property
prop_fromListToListNative = property $ do
  list <- forAll $ genList 1 10 10
  let emptyQueue = emptyNativeQueue
  let queue = foldr enqueue emptyQueue list
  let newList = unfoldr pop queue
  list === newList

prop_fromListToListBankers :: Property
prop_fromListToListBankers = property $ do
  list <- forAll $ genList 1 10 10
  let emptyQueue = emptyBankersQueue
  let queue = foldr enqueue emptyQueue list
  let newList = unfoldr pop queue
  list === newList

prop_fromListToListPhysicists :: Property
prop_fromListToListPhysicists = property $ do
  list <- forAll $ genList 1 10 10
  let emptyQueue = emptyPhysicistsQueue
  let queue = foldr enqueue emptyQueue list
  let newList = unfoldr pop queue
  list === newList

prop_fromListToListRealTime :: Property
prop_fromListToListRealTime = property $ do
  list <- forAll $ genList 1 10 10
  let emptyQueue = emptyRealTimeQueue
  let queue = foldr enqueue emptyQueue list
  let newList = unfoldr pop queue
  list === newList

pop :: Queue queue a => queue -> Maybe (a, queue)
pop queue
  | empty queue = Nothing
  | otherwise = Just (head queue, tail queue)

props :: [TestTree]
props =
  [ testProperty "list == toList (fromList list) NativeQueue" prop_fromListToListNative
  , testProperty "list == toList (fromList list) BankersQueue" prop_fromListToListBankers
  , testProperty "list == toList (fromList list) PhysicistsQueue" prop_fromListToListPhysicists
  , testProperty "list == toList (fromList list) RealTimeQueue" prop_fromListToListRealTime
  ]
