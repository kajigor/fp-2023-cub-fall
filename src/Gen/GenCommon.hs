module Gen.GenCommon where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInt :: Int -> Gen Int
genInt n = Gen.int (Range.constant 0 n)

genList :: Int -> Int -> Int -> Gen [Int]
genList minLength maxLength n = Gen.list (Range.constant minLength maxLength) (genInt n)

genListOfLists :: Int -> Int -> Int -> Gen [[Int]]
genListOfLists minLength maxLength n = 
    Gen.list (Range.constant minLength maxLength) (genList minLength maxLength n)
