import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Lib

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "RegexProps" props
                ])

props :: [TestTree]
props =
  [ testProperty "Union `a` `a` = `a`" prop_unionAA
  , testProperty "Union `a` `b` = Union `b` `a`" prop_unionABBA
  ]

genChar = Gen.enum 'A' 'Z'

genString = Gen.list (Range.linear 0 2) genChar

prop_unionAA :: Property
prop_unionAA = property $ do
    string1 <- forAll genString
    string2 <- forAll genString
    match string1 (string string2 `union` string string2) === match string1 (string string2)

prop_unionABBA :: Property
prop_unionABBA = property $ do
    string1 <- forAll genString
    string2 <- forAll genString
    string3 <- forAll genString
    match string1 (string string2 `union` string string3) === match string1 (string string3 `union` string string2)
