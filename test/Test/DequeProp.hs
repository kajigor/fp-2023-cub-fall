module Test.DequeProp where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import BankersDeque
import Gen.GenCommon

import Data.List (unfoldr)
import Prelude hiding (init, last, tail, head)

prop_consPopBack :: Property
prop_consPopBack = property $ do
  list <- forAll $ genList 1 10 10
  let empty = emptyBankersDeque
  let deque = foldr cons empty list
  let newList = reverse $ unfoldr popBack deque
  list === newList

prop_consPopFront :: Property
prop_consPopFront = property $ do
  list <- forAll $ genList 1 10 10
  let empty = emptyBankersDeque
  let deque = foldr cons empty list
  let newList = unfoldr popFront deque
  list === newList

prop_snocPopBack :: Property
prop_snocPopBack = property $ do
  list <- forAll $ genList 1 10 10
  let empty = emptyBankersDeque
  let deque = foldr snoc empty $ reverse list
  let newList = reverse $ unfoldr popBack deque
  list === newList

prop_snocPopFront :: Property
prop_snocPopFront = property $ do
  list <- forAll $ genList 1 10 10
  let empty = emptyBankersDeque
  let deque = foldr snoc empty $ reverse list
  let newList = unfoldr popFront deque
  list === newList

popFront :: BankersDeque a -> Maybe (a, BankersDeque a)
popFront deque
  | isEmpty deque = Nothing
  | otherwise = Just (head deque, tail deque)

popBack :: BankersDeque a -> Maybe (a, BankersDeque a)
popBack deque
  | isEmpty deque = Nothing
  | otherwise = Just (last deque, init deque)

props :: [TestTree]
props =
  [ testProperty "list == toListViaLast (fromListViaCons list) BankersDeque" prop_consPopBack
  , testProperty "list == toListViaLast (fromListViaCons list) BankersDeque" prop_consPopFront
  , testProperty "list == toListViaHead (fromListViaSnoc list) BankersDeque" prop_snocPopBack
  , testProperty "list == toListViaHead (fromListViaSnoc list) BankersDeque" prop_snocPopFront
  ]