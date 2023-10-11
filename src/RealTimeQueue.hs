{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module RealTimeQueue ( RealTimeQueue, emptyRealTimeQueue ) where

import Queue

data RealTimeQueue a = RealTimeQueue [a] [a] [a]

instance Queue (RealTimeQueue a) a where
    empty (RealTimeQueue [] [] []) = True
    empty _ = False

    enqueue x (RealTimeQueue f r s) = checkInvariants $ RealTimeQueue f (x : r) s

    head (RealTimeQueue (x : xs) _ _) = x
    
    tail (RealTimeQueue (_ : xs) r s) = checkInvariants $ RealTimeQueue xs r s

emptyRealTimeQueue = RealTimeQueue [] [] []

checkInvariants (RealTimeQueue f r (x : xs)) = RealTimeQueue f r xs
checkInvariants (RealTimeQueue f r []) = do
    let f' = rotate f r []
    RealTimeQueue f' [] f'

rotate [] (y : _) a = y : a
rotate (x: xs) (y: ys) a = x : rotate xs ys (y : a)