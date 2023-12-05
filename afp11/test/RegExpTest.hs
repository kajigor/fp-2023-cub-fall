{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module RegExpTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Test.Tasty.HUnit as Unit
import Control.Monad

import Control.Applicative (Alternative, (<|>), empty)
import RegExp

minspreadSize :: Int
minspreadSize = 100

strSize :: Int
strSize = 10

genChar :: MonadGen m => m Char
genChar = Gen.enum 'a' 'z'

genStr :: MonadGen m => m String
genStr = Gen.list (Range.linear 0 strSize) genChar

genRegExp :: (Ord s) => MonadGen m => m s -> m (RegExp s)
genRegExp chars = Gen.recursive Gen.choice
    [   return bot
    ,   symbol <$> chars
    ,   return wildcard
    ,   return top
    ]
    [   Gen.subterm2 (genRegExp chars) (genRegExp chars) (<>)
    ,   Gen.subterm2 (genRegExp chars) (genRegExp chars) (><)
    ,   Gen.subtermM (genRegExp chars) $ \r -> do
            n <- Gen.int (Range.linear 0 minspreadSize)
            k <- Gen.int (Range.linear 0 minspreadSize)
            return $ minmax (fromIntegral n) (fromIntegral (n+k)) r
    ]

genr :: MonadGen m => m (RegExp Char)
genr = genRegExp genChar

hprop_combinatorsProduceNormalForm :: Property
hprop_combinatorsProduceNormalForm = withTests 1000 . property $ do
    r <- forAll genr
    assert (r `structuralEq` normalize r)

hprop_derivativeIsNormal :: Property
hprop_derivativeIsNormal = withTests 1000 . property $ do
    r <- forAll genr
    c <- forAll genChar
    let r' = derivative c r
    assert (r' `structuralEq` normalize r') 

-- Is there some better way than testing random strings on random expressions? Matches are overwhelmingly improbable
hprop_derivativeMatchCorrespondance :: Property
hprop_derivativeMatchCorrespondance = withTests 10000 . property $ do
    r <- forAll genr
    c <- forAll genChar
    s <- forAll genStr
    matches r (c:s) === matches (derivative c r) s