module ListGen where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Ranged
import Data.List (sort)

-- Integer number generators
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 10)

-- Generator for lists of numbers with lengths in the specified range
genList :: Int -> Int -> Gen [Int]
genList minLength maxLength =
  Gen.list (Range.constant minLength maxLength) genInt

-- Generator for sorted lists of random lengths
genSortedList :: Gen (SortedList Int)
genSortedList = do
  minLength <- Gen.integral (Range.constant 1 100)
  maxLength <- Gen.integral (Range.constant minLength 100)
  Sorted . sort <$> genList minLength maxLength