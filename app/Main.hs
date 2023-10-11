module Main (main) where
import Criterion.Main

import Tree
import FunctionalRBTree (FunctionalRBTree, FunctionalDividedRBTree)
import ImperativeRBTree (ImperativeRBTree, ImperativeDividedRBTree)

import Queue
import NaiveQueue (NaiveQueue, emptyNativeQueue)
import BankersQueue (BankersQueue, emptyBankersQueue)
import PhysicistsQueue (PhysicistsQueue, emptyPhysicistsQueue)
import RealTimeQueue (RealTimeQueue, emptyRealTimeQueue)

filteredNumbers f = filter f [0..100000]::[Int]

main :: IO ()
main = defaultMain [
    bgroup "RBTree" [
        bench "Functional" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::FunctionalRBTree Int)) (filteredNumbers even),
        bench "FunctionalDivided" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::FunctionalDividedRBTree Int)) (filteredNumbers even),
        bench "Imperative" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::ImperativeRBTree Int)) (filteredNumbers even),
        bench "ImperativeDivided" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::ImperativeDividedRBTree Int)) (filteredNumbers even)
    ],
    bgroup "Queue" [
        bench "NaiveQueue insert" $ whnf (foldr enqueue (emptyNativeQueue::NaiveQueue Int)) ([0..1000000]::[Int]),
        bench "BankersQueue insert" $ whnf (foldr enqueue (emptyBankersQueue::BankersQueue Int)) ([0..1000000]::[Int]),
        bench "PhysicistsQueue insert" $ whnf (foldr enqueue (emptyPhysicistsQueue::PhysicistsQueue Int)) ([0..1000000]::[Int]),
        bench "RealTimeQueue insert" $ whnf (foldr enqueue (emptyRealTimeQueue::RealTimeQueue Int)) ([0..1000000]::[Int])
    ]
    ]