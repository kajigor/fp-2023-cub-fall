{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BankersQueue ( BankersQueue, emptyBankersQueue ) where

import Queue

data BankersQueue a = BankersQueue [a] Int [a] Int

instance Queue (BankersQueue a) a where
    empty (BankersQueue [] _ _ _) = True
    empty _ = False

    enqueue x (BankersQueue [] _ _ _) = BankersQueue [x] 1 [] 0
    enqueue x (BankersQueue f fLength r rLength) = checkInvariants $ BankersQueue f fLength (x : r) (rLength + 1)

    head (BankersQueue (x : xs) _ _ _) = x
    
    tail (BankersQueue (_ : xs) fLength r rLength) = checkInvariants $ BankersQueue xs (fLength - 1) r rLength

emptyBankersQueue = BankersQueue [] 0 [] 0

checkInvariants queue@(BankersQueue f fLength r rLength)
    | fLength > rLength = queue
    | otherwise = BankersQueue (f ++ reverse r) (fLength + rLength) [] 0