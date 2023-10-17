module Test.Deque where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (init, last, tail, head)

import BankersDeque

unit_sizeZero :: Assertion
unit_sizeZero = do
  isEmpty emptyBankersDeque @?= True

unit_sizeOne :: Assertion
unit_sizeOne = do
  let emptyDeque = emptyBankersDeque :: BankersDeque Int
  let deque = cons 1 emptyDeque
  head deque @?= 1
  tail deque @?= emptyDeque
  last deque @?= 1
  init deque @?= emptyDeque
  isEmpty deque @?= False
  
unit_sizeTwo :: Assertion
unit_sizeTwo = do
  let emptyDeque = emptyBankersDeque :: BankersDeque Int
  let deque = cons 1 (cons 2 emptyDeque)
  head deque @?= 1
  head (tail deque) @?= 2
  last deque @?= 2
  last (init deque) @?= 1
  isEmpty deque @?= False
  
unitTests :: [TestTree]
unitTests =
  [ testCase "BankersDeque size == 0" unit_sizeZero
  , testCase "BankersDeque size == 1" unit_sizeOne
  , testCase "BankersDeque size == 2" unit_sizeTwo
  ]