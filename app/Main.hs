module Main (main) where

import RandomAccessList
import Dijkstra

main :: IO ()
main = do
    let graph = fromList [
            [Edge 2 1, Edge 7 3],
            [Edge 2 0, Edge 2 2, Edge 3 3],
            [Edge 2 1, Edge 4 3, Edge 6 4],
            [Edge 7 0, Edge 3 1, Edge 4 2, Edge 1 4],
            [Edge 6 2, Edge 1 3],
            [Edge 5 6],
            [Edge 5 5]
            ]
    let distances = dijkstra graph 0
    print $ toList distances
