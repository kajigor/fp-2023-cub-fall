{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module NaiveQueue ( NaiveQueue, emptyNativeQueue ) where

import Queue

data NaiveQueue a = NaiveQueue [a] [a]

instance Queue (NaiveQueue a) a where
    empty (NaiveQueue [] []) = True
    empty _ = False

    enqueue x (NaiveQueue [] _) = NaiveQueue [x] []
    enqueue x (NaiveQueue f r) = NaiveQueue f (x : r)

    head (NaiveQueue (x : xs) _) = x
    
    tail (NaiveQueue [x] r) = NaiveQueue (reverse r) []
    tail (NaiveQueue (x : xs) r) = NaiveQueue xs r


emptyNativeQueue = NaiveQueue [] []