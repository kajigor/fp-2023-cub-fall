module Test.Tree where

import FunctionalRBTree
import Tree

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (delete)

unit_remove :: Assertion
unit_remove = do
  let list = [1, 2, 4, 5, 6]
  let fTree = fromOrdList list :: FunctionalRBTree Int

  delete 1 list @?= toOrdList (remove 1 fTree)  
  delete 2 list @?= toOrdList (remove 2 fTree)  
  delete 4 list @?= toOrdList (remove 4 fTree)  
  delete 5 list @?= toOrdList (remove 5 fTree)  
  delete 6 list @?= toOrdList (remove 6 fTree)  
  list @?= toOrdList (remove 0 fTree)  
  list @?= toOrdList (remove 3 fTree)  
  list @?= toOrdList (remove 7 fTree)  

unit_member :: Assertion
unit_member = do
  let list = [1, 2, 4, 5, 6]
  let fTree = fromOrdList list :: FunctionalRBTree Int

  member 0 fTree @?= False
  member 1 fTree @?= True
  member 2 fTree @?= True
  member 3 fTree @?= False
  member 4 fTree @?= True
  member 5 fTree @?= True
  member 6 fTree @?= True
  member 7 fTree @?= False

unitTests :: [TestTree]
unitTests =
  [ testCase "remove" unit_remove
  , testCase "member" unit_member]