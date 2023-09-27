import Test.Tasty

import qualified Test.Reverse
import qualified Test.ReverseProp
import qualified Test.RangedProp
import qualified Test.Expr

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Reverse" Test.Reverse.unitTests
                , testGroup "ReverseProp" Test.ReverseProp.props
                , testGroup "ExpressionParser" Test.Expr.props
                ])