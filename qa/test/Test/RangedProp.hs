module Test.RangedProp where

import Ranged

import ListGen

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- All elements of the list are not bigger than the maximum value
prop_maximum :: Property
prop_maximum = undefined

-- All elements of the list are not smaller than the minimum value
prop_minimum :: Property
prop_minimum = undefined

-- Minimum value works the same on the regular and sorted lists
prop_minimumSorted :: Property
prop_minimumSorted = undefined

-- Maximum value works the same on the regular and sorted lists
prop_maximumSorted :: Property
prop_maximumSorted = undefined

props =
  [ testProperty "Maximum value is not less than all elements of the list" prop_maximum
  , testProperty "Minimum value is not more than all elements of the list" prop_minimum
  , testProperty "Maximum value is not less than all elements of the sorted list" prop_maximumSorted
  , testProperty "Minimum value is not more than all elements of the sorted list" prop_minimumSorted
  ]