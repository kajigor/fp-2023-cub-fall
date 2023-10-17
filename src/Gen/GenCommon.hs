module Gen.GenCommon where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.List (sort, nub)

genInt :: Int -> Gen Int
genInt n = Gen.int (Range.constant n 10)

genList :: Int -> Int -> Int -> Gen [Int]
genList minLength maxLength n = Gen.list (Range.constant minLength maxLength) (genInt n)

genUniqueSortedList :: Int -> Int -> Int -> Gen [Int]
genUniqueSortedList minLength maxLength n = nub . sort <$> genList minLength maxLength n
