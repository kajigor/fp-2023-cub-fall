{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Test.Queue where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Test.Tasty.HUnit as Unit
import Control.Monad

import Queues.Queue
import Queues.Deque

import Prelude hiding (head, tail, last)


type T = Int

mn_elem :: Int 
mn_elem = 0
mx_elem :: Int
mx_elem = 10

mn_size :: Int
mn_size = 1
mx_size :: Int
mx_size = 10

genElem :: Gen T
genElem = Gen.int (Range.constant mn_elem mx_elem)

genQueue' :: (Queue q) => Gen (q T)
genQueue' = do
    elems <- Gen.list (Range.constant mn_size mx_size) genElem
    return $ foldl enqueue empty elems

-- Give empty queue priority for special cases
genQueue :: (Queue q) => Gen (q T)
genQueue = Gen.choice [Gen.constant empty, genQueue']

last :: (Queue q) => q a -> a
last q | isEmpty (tail q) = head q
       | otherwise = last $ tail q

asList :: (Queue q) => q a -> [a]
asList q | isEmpty q = []
         | otherwise = head q : asList (tail q)

x =@= y = asList x === asList y

unit_emptyIsEmpty :: forall q. (Queue q) => Unit.Assertion
unit_emptyIsEmpty = Unit.assertBool "Empty queue must pass isEmpty" $ isEmpty (empty :: q T)

prop_enqueueIsNotEmpty_random :: forall q. (Queue q, Show (q T)) => Property
prop_enqueueIsNotEmpty_random = property $ do
    q <- forAll $ genQueue @q
    e <- forAll $ genElem
    assert $ not $ isEmpty (enqueue q e)

prop_emptyEnqueueHeadMatch :: forall q. (Queue q, Show (q T)) => Property
prop_emptyEnqueueHeadMatch = property $ do
    let q = empty @q
    e <- forAll $ genElem
    head (enqueue q e) === e

prop_enqueueNonEmptyHead :: forall q. (Queue q, Show (q T)) => Property
prop_enqueueNonEmptyHead = property $ do
    q <- forAll $ genQueue' @q
    e <- forAll $ genElem
    guard (not $ isEmpty q)
    head (enqueue q e) === head q

prop_enqueueLastMatch :: forall q. (Queue q, Show (q T)) => Property
prop_enqueueLastMatch = property $ do
    q <- forAll $ genQueue @q
    e <- forAll $ genElem
    last (enqueue q e) === e

prop_nonEmptyEnqueueTailCommute :: forall q. (Queue q, Show (q T)) => Property
prop_nonEmptyEnqueueTailCommute = property $ do
    q <- forAll $ genQueue' @q
    e <- forAll $ genElem
    guard (not $ isEmpty q)
    enqueue (tail q) e =@= tail (enqueue q e)

-- TODO: Tests unique to Deque

name :: String -> String -> String
name s rest = "(" ++ s ++ ") " ++ rest

queueTests :: forall q. (Queue q, Show (q T)) => String -> [TestTree]
queueTests s = 
    [ Unit.testCase (name s "empty is empty") (unit_emptyIsEmpty @q)
    , testProperty (name s "equeue is not empty (random)") (prop_enqueueIsNotEmpty_random @q)
    , testProperty (name s "head after enqueue in empty returns same elemnt") (prop_emptyEnqueueHeadMatch @q)
    , testProperty (name s "head after enqueue in non-empty does not change") (prop_enqueueNonEmptyHead @q)
    , testProperty (name s "last after enqueue returns same elemnt") (prop_enqueueLastMatch @q)
    , testProperty (name s "enqueue and tail commute over non-empty") (prop_nonEmptyEnqueueTailCommute @q)
    ]

allQueueTests :: [TestTree]
allQueueTests = concat 
    [ queueTests @NaiveQueue "naive"
    , queueTests  @BankersQueue "bankers"
    , queueTests @PhysicistsQueue "physicists"
    , queueTests @RTQueue "realtime"
    , queueTests @Forward "deque (forward)"
    , queueTests @Backward "deque (backward)"
    ]