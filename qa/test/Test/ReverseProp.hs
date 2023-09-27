module Test.ReverseProp where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Reverse

-- QuickCheck
-- This is the example of using the Hedgehog library for property-based testing.
-- The testing works like this:
-- * input data is randomly generated
-- * a function is run on the input data
-- * it is checked if a given property holds
-- * if the property is not satisfied, then the input data is shrunk in order to simplify debugging

-- Integer number generators
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 10)

-- Generator for lists of numbers with lengths in the specified range
genList :: Int -> Int -> Gen [Int]
genList minLength maxLength =
  Gen.list (Range.constant minLength maxLength) genInt

-- By reversing a list twice we should get the same list
prop_reverseList :: Property
prop_reverseList = property $ do
  list <- forAll $ genList 0 10
  reverseList (reverseList list) === list

-- The fast list reversal should give the same result as the simple reversal function.
prop_fastReverseList :: Property
prop_fastReverseList = property $ do
  list <- forAll $ genList 0 10
  reverseList list === fastReverseList list

props :: [TestTree]
props =
  [ testProperty "Reversing list twice gets the input list" prop_reverseList
  , testProperty "Fast reverse gives the same " prop_fastReverseList
  ]