import Test.Tasty

import qualified Test.HeapProp
import qualified Test.Heap
import qualified Test.DequeProp
import qualified Test.Deque
import qualified Test.Djkstra

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "HeapProp" Test.HeapProp.props
                , testGroup "Heap" Test.Heap.unitTests
                , testGroup "DequeProp" Test.DequeProp.props
                , testGroup "Deque" Test.Deque.unitTests
                , testGroup "Djkstra" Test.Djkstra.unitTests
                ])