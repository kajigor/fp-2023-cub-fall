module Test.Reverse where

import Reverse

import Test.Tasty
import Test.Tasty.HUnit

emptyList :: [Int]
emptyList = []

unit_reverseList :: Assertion
unit_reverseList = do
  reverseList emptyList @?= emptyList
  reverseList [0] @?= [0]
  reverseList [0,1] @?= [1,0]
  reverseList [0,1,2] @?= [2,1,0]

unit_fastReverseList :: Assertion
unit_fastReverseList = do
  fastReverseList emptyList @?= emptyList
  fastReverseList [0] @?= [0]
  fastReverseList [0,1] @?= [1,0]
  fastReverseList [0,1,2] @?= [2,1,0]

unitTests :: [TestTree]
unitTests =
  [ testCase "reverseList reverses lists" unit_reverseList
  , testCase "fastReverseList reverses lists" unit_fastReverseList]

