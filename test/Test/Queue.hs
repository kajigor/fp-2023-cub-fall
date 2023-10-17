module Test.Queue where

import NaiveQueue
import BankersQueue
import PhysicistsQueue
import RealTimeQueue
import Queue

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (head)

unit_headNative :: Assertion
unit_headNative = do
  let queue = enqueue 1 emptyNativeQueue
  head queue @?= 1
  let newQueue = enqueue 2 queue
  head newQueue @?= 1

unit_headBankers :: Assertion
unit_headBankers = do
  let queue = enqueue 1 emptyBankersQueue
  head queue @?= 1
  let newQueue = enqueue 2 queue
  head newQueue @?= 1

unit_headPhysicists :: Assertion
unit_headPhysicists = do
  let queue = enqueue 1 emptyPhysicistsQueue
  head queue @?= 1
  let newQueue = enqueue 2 queue
  head newQueue @?= 1

unit_headRealTime :: Assertion
unit_headRealTime = do
  let queue = enqueue 1 emptyRealTimeQueue
  head queue @?= 1
  let newQueue = enqueue 2 queue
  head newQueue @?= 1

unit_emptyNative :: Assertion
unit_emptyNative = do
  let queue = emptyNativeQueue
  empty queue @?= True
  let newQueue = enqueue 2 queue
  empty newQueue @?= False

unit_emptyBankers :: Assertion
unit_emptyBankers = do
  let queue = emptyBankersQueue
  empty queue @?= True
  let newQueue = enqueue 2 queue
  empty newQueue @?= False

unit_emptyPhysicists :: Assertion
unit_emptyPhysicists = do
  let queue = emptyPhysicistsQueue
  empty queue @?= True
  let newQueue = enqueue 2 queue
  empty newQueue @?= False

unit_emptyRealTime :: Assertion
unit_emptyRealTime = do
  let queue = emptyRealTimeQueue
  empty queue @?= True
  let newQueue = enqueue 2 queue
  empty newQueue @?= False


unitTests :: [TestTree]
unitTests =
  [ testCase "head NativeQueue" unit_headNative
  , testCase "head BankersQueue" unit_headBankers
  , testCase "head PhysicistsQueue" unit_headPhysicists
  , testCase "head RealTimeQueue" unit_headRealTime
  , testCase "empty NativeQueue" unit_emptyNative
  , testCase "empty BankersQueue" unit_emptyBankers
  , testCase "empty PhysicistsQueue" unit_emptyPhysicists
  , testCase "empty RealTimeQueue" unit_emptyRealTime
  ]