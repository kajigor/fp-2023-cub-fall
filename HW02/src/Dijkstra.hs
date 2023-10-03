module Dijkstra where

import qualified Bootstrap as Heap

type Weight = Int
type Idx = Int

data Edge = E { weight :: Weight, target :: Idx } deriving (Show, Eq)
data Vertex = V { idx :: Idx, edges :: [Edge] } deriving (Show, Eq)

data Distance = Dist Weight | Inf deriving (Show, Eq, Ord)

dadd :: Distance -> Int -> Distance
dadd Inf _ = Inf
dadd (Dist a) b = Dist $ a + b

data VertexDist = VD { dist :: Distance, vert :: Idx, prev :: Idx, step :: Weight } deriving (Show, Eq, Ord)

type DistanceMap = Idx -> Distance
type Graph = [Vertex]

update :: Idx -> Distance -> DistanceMap -> DistanceMap
update v w d = \x -> if x == v then w else (d x)

putEdges :: Distance -> Vertex -> Heap.Heap VertexDist -> Heap.Heap VertexDist
putEdges d v = Heap.insertAll $ map (\(E w' v') -> VD (dadd d w') v' (idx v) w') (edges v)

dijkstra :: Graph -> Idx -> DistanceMap
dijkstra g v = let d = update v (Dist 0) (const Inf) in dijkstraStep g d (putEdges (Dist 0) (g !! v) Heap.Empty)

dijkstraStep :: Graph -> DistanceMap -> Heap.Heap VertexDist -> DistanceMap
dijkstraStep _ d Heap.Empty = d
dijkstraStep g d h = 
    let 
        (VD _ v p s, h') = Heap.popMin h
        w' = dadd (d p) s 
    in if d v <= w' then dijkstraStep g d h'
    else let d' = update v w' d in dijkstraStep g d' (putEdges w' (g !! v) h')

showDistance :: Graph -> DistanceMap -> String
showDistance g d = unlines $ map showVertex g
    where
        showVertex v = show (idx v) ++ " - " ++ show (d (idx v))