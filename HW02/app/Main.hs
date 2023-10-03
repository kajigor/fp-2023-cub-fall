module Main (main) where

import Dijkstra
import Deque

v1 = V 0 [E 5 1, E 3 2]
v2 = V 1 [E 2 2, E 4 0, E 2 3]
v3 = V 2 [E 1 1, E 1 0, E 8 3]
v4 = V 3 [E 1 0]

g = [v1, v2, v3, v4]

main :: IO ()
main = putStrLn $ showDistance g $ dijkstra g 0