module Test.Heap where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (head)

import SkewBinaryHeap
import BootstrappedHeap

unit_skewHeapGetMin :: Assertion
unit_skewHeapGetMin = do
  let newHeap = emptyHeap :: SkewBinaryHeap Int
  let heap = insert 2 newHeap
  getMin heap @?= 2
  getMin (insert 1 heap) @?= 1
  getMin (insert 3 heap) @?= 2

unit_bootstrappedHeapGetMin :: Assertion
unit_bootstrappedHeapGetMin = do
  let newHeap = emptyHeap :: BootstrappedHeap Int
  let heap = insert 2 newHeap
  getMin heap @?= 2
  getMin (insert 1 heap) @?= 1
  getMin (insert 3 heap) @?= 2

unit_skewHeapIsEmpty :: Assertion
unit_skewHeapIsEmpty = do
  let newHeap = emptyHeap :: SkewBinaryHeap Int
  isEmpty newHeap @?= True
  isEmpty (insert 1 newHeap) @?= False
  
unit_bootstrappedHeapIsEmpty :: Assertion
unit_bootstrappedHeapIsEmpty = do
  let newHeap = emptyHeap :: BootstrappedHeap Int
  isEmpty newHeap @?= True
  isEmpty (insert 1 newHeap) @?= False
  
unitTests :: [TestTree]
unitTests =
  [ testCase "SkewBinaryHeap getMin" unit_skewHeapGetMin
  , testCase "BootstrappedHeap getMin" unit_bootstrappedHeapGetMin
  , testCase "SkewBinaryHeap isEmpty" unit_skewHeapIsEmpty
  , testCase "BootstrappedHeap isEmpty" unit_bootstrappedHeapIsEmpty
  ]