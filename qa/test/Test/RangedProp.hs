module Test.RangedProp where

import Ranged

import ListGen

import Hedgehog
import Test.Tasty.Hedgehog

-- All elements of the list are not bigger than the maximum value
prop_maximum :: Property
prop_maximum = property $ do
  list <- forAll $ genList 1 100
  let maxValue = maximumValue list
  assert (all (<= maxValue) list)

-- All elements of the list are not smaller than the minimum value
prop_minimum :: Property
prop_minimum = property $ do
  list <- forAll $ genList 1 100
  let minValue = minimumValue list
  assert (all (>= minValue) list)

-- Minimum value works the same on the regular and sorted lists
prop_minimumSorted :: Property
prop_minimumSorted = property $ do
  list <- forAll genSortedList
  minimumValue list === minimumValue (getSorted list)

-- Maximum value works the same on the regular and sorted lists
prop_maximumSorted :: Property
prop_maximumSorted = property $ do
  list <- forAll genSortedList
  maximumValue list === maximumValue (getSorted list)

props =
  [ testProperty "Maximum value is not less than all elements of the list" prop_maximum
  , testProperty "Minimum value is not more than all elements of the list" prop_minimum
  , testProperty "Maximum value is not less than all elements of the sorted list" prop_maximumSorted
  , testProperty "Minimum value is not more than all elements of the sorted list" prop_minimumSorted
  ]