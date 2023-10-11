{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Queue (Queue(empty, enqueue, head, tail)) where

class Queue queue a where
    empty :: queue -> Bool
    enqueue :: a -> queue -> queue
    head :: queue -> a
    tail :: queue -> queue
