module Main (main) where
import FunctionalRBTree (FunctionalRBTree, FunctionalDividedRBTree)
import ImperativeRBTree (ImperativeRBTree, ImperativeDividedRBTree)
import Tree
import Criterion.Main

filteredNumbers f = filter f [0..100000]::[Int]

main :: IO ()
main = defaultMain [
    bench "FunctionalRBTree" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::FunctionalRBTree Int)) (filteredNumbers even),
    bench "FunctionalDividedRBTree" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::FunctionalDividedRBTree Int)) (filteredNumbers even),
    bench "ImperativeRBTree" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::ImperativeRBTree Int)) (filteredNumbers even),
    bench "ImperativeDividedRBTree" $ whnf (foldr insert (fromOrdList (filteredNumbers odd)::ImperativeDividedRBTree Int)) (filteredNumbers even)
    ]