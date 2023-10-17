{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns #-}

module BankersDeque where

data BankersDeque a = BankersDeque {
    front :: [a],
    frontLength :: Int,
    rear :: [a],
    rearLength :: Int
} deriving (Eq, Show)

cons :: a -> BankersDeque a -> BankersDeque a
cons x deque@(BankersDeque { front, frontLength }) = 
    checkInvariants $ deque { front = x : front, frontLength = frontLength + 1 }

head :: BankersDeque a -> a
head (BankersDeque { front = [], rear = [x] }) = x
head (BankersDeque { front = (x : _) }) = x

tail :: BankersDeque a -> BankersDeque a
tail deque@(BankersDeque { front = [], rear = [x] }) = emptyBankersDeque
tail deque@(BankersDeque { front = (_ : xs), frontLength }) = 
    checkInvariants $ deque { front = xs, frontLength = frontLength - 1 }

snoc :: a -> BankersDeque a -> BankersDeque a
snoc x deque@(BankersDeque { rear, rearLength }) = 
    checkInvariants $ deque { rear = x : rear, rearLength = rearLength + 1 }

last :: BankersDeque a -> a
last (BankersDeque { front = [x], rear = [] }) = x
last (BankersDeque { rear = (x : _) }) = x

init :: BankersDeque a -> BankersDeque a
init deque@(BankersDeque { front = [x], rear = [] }) = emptyBankersDeque
init deque@(BankersDeque { rear = (_ : xs), rearLength }) = 
    checkInvariants $ deque { rear = xs, rearLength = rearLength - 1 }


isEmpty (BankersDeque [] _ [] _) = True
isEmpty _ = False

emptyBankersDeque = BankersDeque [] 0 [] 0

checkInvariants deque@(BankersDeque f fLength r rLength)
    | fLength > 2 * rLength + 1 = do
        let f' = take fLength' f
        let r' = r ++ reverse (drop fLength' f)
        BankersDeque f' fLength' r' rLength'
    | rLength > 2 * fLength + 1 = do
        let f' = f ++ reverse (drop rLength' r)
        let r' = take rLength' r
        BankersDeque f' fLength' r' rLength'
    | otherwise = deque
    where
        fLength' = (fLength + rLength) `div` 2
        rLength' = fLength + rLength - fLength'
