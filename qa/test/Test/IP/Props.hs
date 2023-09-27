module Test.IP.Props where

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Test.Tasty
import Test.Tasty.Hedgehog

import IP.Lookup.IPTypes
import IP.Gen.GenIP
import IP.Lookup.ParseIP
import IP.Lookup.LookupIP

prop_buildIPs :: Property
prop_buildIPs = property $ do
  ipcs <- forAll genIPComponents
  let ip = buildIP ipcs
  buildIP_foldr ipcs === ip
  buildIP_foldl ipcs === ip
  buildIP_foldl_shl ipcs === ip

prop_parseIP :: Property
prop_parseIP = property $ do
  ip <- forAll genIP
  parseIP (show ip) === Just ip

prop_parseIP_show :: Property
prop_parseIP_show = property $ do
  ip <- forAll genIP
  tripping ip show parseIP

prop_parseIPRange_show :: Property
prop_parseIPRange_show = property $ do
  ipr <- forAll genIPRange
  tripping ipr show parseIPRange

prop_parseIPRanges_show :: Property
prop_parseIPRanges_show = property $ do
  iprdb <- forAll genIPRangeDB
  tripping iprdb show parseIPRanges

prop_no_parseInvalidIPRange :: Property
prop_no_parseInvalidIPRange = property $ do
  inv_ip <- forAll genInvalidIPRange
  parseIPRange (show inv_ip) === Nothing

prop_lookupIP_empty :: Property
prop_lookupIP_empty = undefined

prop_lookupIP_bordersIncluded :: Property
prop_lookupIP_bordersIncluded = undefined

props :: [TestTree]
props = [
   testProperty "buildIP implementations agrees with each other" prop_buildIPs
 , testProperty "parseIP works as expected" prop_parseIP
 , testProperty "parseIP agrees with show" prop_parseIP_show
 , testProperty "parseIPRange agrees with show" prop_parseIPRange_show
 , testProperty "parseIPRanges agrees with show" prop_parseIPRanges_show
 , testProperty "no parse of invalid IP ranges" prop_no_parseInvalidIPRange
 , testProperty "no ip in empty list" prop_lookupIP_empty
 , testProperty "lookupIP includes borders" prop_lookupIP_bordersIncluded
 ]