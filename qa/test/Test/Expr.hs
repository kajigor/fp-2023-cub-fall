module Test.Expr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Expr.Ast
import qualified Expr.Infix as Infix
import qualified Expr.Prefix as Prefix
import qualified Expr.Parser as Parser

-- Here, we test the parsers for arithmetic expressions.
-- It is not easy to generate syntactically correct strings,
-- so we generate expressions instead and then transform them into strings.
-- The parser is then run on the generated strings.
-- This is not ideal, since the printer only ever makes one string from one expression,
-- while the user can come up with a multitude of different representations.
-- It is still better than nothing.


-- There are a limited number of operations, so the generator picks one of them
genOp :: Gen Op
genOp = Gen.element [Plus, Minus, Mult, Div, Pow]

-- If we have a recursive ADT, use Gen.recursive
genExpr :: Int -> Gen Expr
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
    ]
    [ -- recursive generators
      binOpGen
    ]
  where
    numGen = Number <$> Gen.int (Range.constant 0 n)
    binOpGen = do
      op <- genOp
      Gen.subterm2 (genExpr n) (genExpr n) (BinOp op)

-- parser . printer == id
parserPrinterIsId :: MonadTest m => (Expr -> String) -> (String -> Maybe Expr) -> Expr -> m ()
parserPrinterIsId printer parser ast =
  undefined

prop_printerParserInfix :: Property
prop_printerParserInfix =
  undefined

prop_printerParserPrefix :: Property
prop_printerParserPrefix =
  undefined

props :: [TestTree]
props =
  [ testProperty "`parser . printer == id` for Infix" prop_printerParserInfix
  , testProperty "`parser . printer == id` for Prefix" prop_printerParserPrefix
  ]
