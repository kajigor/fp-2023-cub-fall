module IP.Gen.GenIP where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import Data.List (intercalate)

import IP.Lookup.IPTypes

genIP :: Gen IP
genIP = IP <$> Gen.word32 Range.linearBounded

genIPComponents :: Gen [Word8]
genIPComponents = Gen.list (Range.singleton 4) genOctet
  where genOctet = Gen.word8 Range.linearBounded

genIPString :: Gen String
genIPString = undefined

genIPRange :: Gen IPRange
genIPRange = do
  (IP ip1) <- genIP
  ip2 <- undefined
  pure $ IPRange (IP ip1) (IP ip2)

genInvalidIPRange :: Gen IPRange
genInvalidIPRange = undefined

genIPRangeDBSized :: Int -> Int -> Gen IPRangeDB
genIPRangeDBSized minLen maxLen = IPRangeDB <$> Gen.list (Range.constant minLen maxLen) genIPRange

genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = undefined

genIPList :: Int -> IO [IP]
genIPList n = Gen.sample $ Gen.list (Range.constant n n) genIP