module Queues.Queue where

import Prelude hiding (head, tail)
import Debug.Trace 

class Queue q where

    empty :: q a

    isEmpty :: q a -> Bool

    head :: q a -> a

    enqueue :: q a -> a -> q a

    tail :: q a -> q a

newtype NaiveQueue a = NQ ([a], [a]) deriving (Show, Eq)

balanceNaive (NQ ([], r)) = NQ (reverse r, [])
balanceNaive q = q

instance Queue NaiveQueue where

    empty = NQ ([], [])

    isEmpty (NQ ([], _)) = True
    isEmpty _ = False
    head (NQ (x:_, _)) = x

    enqueue (NQ (f, r)) x = balanceNaive $ NQ (f, x:r)
    
    tail (NQ (x:f, r)) = balanceNaive $ NQ (f, r)

data BankersQueue a = BQ { bfront :: [a], brev :: [a], bfl :: Int, brl :: Int } deriving (Show, Eq)


-- Is memoization ensured?..
balanceBankers q | bfl q < brl q = q { bfront = bfront q ++ reverse (brev q), brev = [], bfl = bfl q + brl q, brl = 0 }
                 | otherwise = q

instance Queue BankersQueue where
    empty = BQ { bfront = [], brev = [], bfl = 0, brl = 0 }

    isEmpty (BQ { bfl = 0 }) = True
    isEmpty _ = False

    head (BQ { bfront = x:_ }) = x

    enqueue q x = balanceBankers $ q { brev = x : brev q, brl = 1 + brl q }

    tail q@(BQ { bfront = _:f, bfl = l }) = balanceBankers $ q { bfront = f, bfl = l - 1 }


data PhysicistsQueue a = PQ { pw :: [a], pfront :: [a], prev :: [a], pfl :: Int, prl :: Int } deriving (Show, Eq)

-- Matches on every constructor of a list, as well as evalutating list elements to WHNF
force :: [a] -> [a]
force [] = []
force (x:xs) = seq x $ x : force xs

balancePhysicists q = balanceW (balanceRev q)
    where
        balanceRev q | pfl q < prl q = let w' = force (pfront q) in PQ { pw = w', pfront = w' ++ reverse (prev q), prev = [], pfl = pfl q + prl q, prl = 0 }
                     | otherwise = q

        balanceW q@(PQ { pw = [], pfront = f }) = q { pw = force f }
        balanceW q = q

instance Queue PhysicistsQueue where
    
    empty = PQ { pw = [], pfront = [], prev = [], pfl = 0, prl = 0 }

    isEmpty (PQ { pfl = 0 }) = True
    isEmpty _ = False

    head (PQ { pw = x:_ }) = x

    enqueue q x = balancePhysicists $ q { prev = x : prev q, prl = 1 + prl q }

    tail q@(PQ { pw = _:w', pfront = _:f, pfl = l }) = balancePhysicists $ q { pw = w', pfront = force f, pfl = l - 1 }


data RTQueue a = RQ { rfront :: [a], rrev :: [a], schedule :: [a] } deriving (Show, Eq)

rotate [] []    a = a
rotate [] (y:_) a = y:a
rotate (x:f') (y:r') a = x : rotate f' r' (y:a)

balanceRT q@(RQ { schedule = x:s' }) = seq x $ q { schedule = s' }
balanceRT (RQ { rfront = f, rrev = r, schedule = [] }) = let f' = rotate f r [] in RQ { rfront = f', rrev = [], schedule = f' }

instance Queue RTQueue where

    empty = RQ { rfront = [], rrev = [], schedule = [] }

    isEmpty (RQ { rfront = [] }) = True 
    isEmpty _ = False

    head (RQ { rfront = x:_ }) = x

    enqueue q x = balanceRT $ q { rrev = x : rrev q }

    tail q@(RQ { rfront = _:f' }) = balanceRT $ q { rfront = f' }