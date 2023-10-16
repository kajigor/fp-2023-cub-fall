module Dijkstra (dijkstra, Edge(Edge), Vertex, Graph) where
import RandomAccessList
import BootstrappedHeap
import Prelude hiding (lookup)

data Distance = Distance Int | Inf deriving (Eq, Ord)

instance Show Distance where
   show Inf = show "Inf"
   show (Distance dist) = show dist

data Edge = Edge { w :: Int, to :: Int } deriving Show
type Vertex = [Edge]
type Graph = RAList Vertex

type DijkstraQueue = BootstrappedHeap (Int, Int)

dijkstra :: Graph -> Int -> RAList Distance
dijkstra g v = do
    let vertexCount = length $ toList g
    let emptyDistanceList = fromList (replicate vertexCount Inf)
    let distanceList = update v (Distance 0) emptyDistanceList

    let emptyQueue = emptyHeap :: DijkstraQueue
    let queue = insert (0, v) emptyQueue

    solveDijkstra queue distanceList g

solveDijkstra :: DijkstraQueue -> RAList Distance -> Graph -> RAList Distance
solveDijkstra queue distanceList g
    | BootstrappedHeap.isEmpty queue = distanceList
    | otherwise = do
        let ((currentDistance, v), queue') = popMin queue
        if Distance currentDistance /= lookup v distanceList
            then solveDijkstra queue' distanceList g
            else do
                let (queue'', distanceList') = relaxEdges queue' distanceList (lookup v g) currentDistance
                solveDijkstra queue'' distanceList' g
                
relaxEdges :: DijkstraQueue -> RAList Distance -> [Edge] -> Int -> (DijkstraQueue, RAList Distance)
relaxEdges queue distanceList [] _ = (queue, distanceList)
relaxEdges queue distanceList ((Edge w to) : edges) currentDistance = do
    let distanceTo = lookup to distanceList
    let newDistanceTo = currentDistance + w
    let (queue', distanceList') = if Distance newDistanceTo < distanceTo
        then do
            let newDistanceList = update to (Distance newDistanceTo) distanceList
            let newQueue = insert (newDistanceTo, to) queue
            (newQueue, newDistanceList)
        else (queue, distanceList)
    relaxEdges queue' distanceList' edges currentDistance