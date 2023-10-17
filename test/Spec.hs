import Test.Tasty

import qualified Test.TreeProp
import qualified Test.Tree
import qualified Test.QueueProp
import qualified Test.Queue

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "TreeProp" Test.TreeProp.props
                , testGroup "Tree" Test.Tree.unitTests
                , testGroup "QueueProp" Test.QueueProp.props
                , testGroup "Queue" Test.Queue.unitTests
                ])