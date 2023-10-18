{-# LANGUAGE PatternSynonyms #-}
module Queues.Deque where

import Queues.Queue
import Debug.Trace
data Lst a = L { lst :: [a], len :: Int } deriving (Show, Eq)

{-# COMPLETE Nil, Cons #-}

pattern Nil :: Lst a
pattern Nil <- L [] _ where
    Nil = L [] 0

pattern Cons :: a -> [a] -> Int -> Lst a
pattern Cons x xs n = L (x:xs) n

fromList :: [a] -> Lst a
fromList l = L l (length l)

lstCons :: a -> Lst a -> Lst a
lstCons x (L xs n) = L (x:xs) (n+1)

lstPop :: Lst a -> Lst a
lstPop Nil = error "Pop of empty list"
lstPop (Cons _ xs n) = L xs (n-1)

lstPopLast :: Lst a -> Lst a
lstPopLast Nil = error "Pop of empty list"
lstPopLast (Cons _ _ 1) = Nil
lstPopLast (Cons x xs n) = Cons x (init xs) (n - 1)

lstConcat :: Lst a -> Lst a -> Lst a
lstConcat (L xs n) (L ys m) = L (xs ++ ys) (n + m)

lstReverse :: Lst a -> Lst a 
lstReverse (L xs n) = L (reverse xs) n

splitLst :: Lst a -> (Lst a, Lst a)
splitLst l = helper 0 l
    where 
        helper _ Nil = (Nil, Nil)
        helper n b@(Cons y _ m) | n >= 3 * m = (Nil, b)
                                | otherwise = let (a, b') = helper (n+1) (lstPop b) in (lstCons y a, b')


-- idea: two lists, forward and reversed, whenever one is twice the size of the other, balance them to equal size
-- Let's call the difference between list sizes 'the gap'
-- balancing: split the bigger list at 3:1 ratio, reverse smaller part, add to smaller list. After balancing, the gap is at most 1 (for odd lengths)
-- push/pop front/back increase the gap at most by 1, let us say each operation allocates 3 credits
-- before a next balance can happen, the gap must increase to size / 3, so we have at least size credits
-- balancing time is bounded by size, so spending size credits gives us constant amortized time
data Deque a = Q { front :: Lst a, back :: Lst a } deriving (Show)

emptyDeque :: Deque a
emptyDeque = Q { front = Nil, back = Nil }

isDequeEmppty :: Deque a -> Bool
isDequeEmppty (Q Nil Nil) = True
isDequeEmppty _ = False

pushFront :: a -> Deque a -> Deque a
pushFront x q = balance $ q { front = lstCons x (front q) }

pushBack :: a -> Deque a -> Deque a
pushBack x q = balance $ q { back = lstCons x (back q) }

peekFront :: Deque a -> a
peekFront (Q { front = Cons x _ _ }) = x
peekFront (Q { front = Nil, back = (L {lst = b}) }) = last b
peekFront _ = error "empty"

peekBack :: Deque a -> a
peekBack (Q { back = Cons x _ _ }) = x
peekBack (Q { back = Nil, front = (L {lst = f}) }) = last f
peekBack _ = error "empty"

popFront :: Deque a -> Deque a
popFront q@(Q {front = Nil }) = q { back = lstPopLast (back q) }
popFront q = balance $ q { front = lstPop (front q) }

popBack :: Deque a -> Deque a
popBack q@(Q { back = Nil }) = q { front = lstPopLast (front q) }
popBack q = balance $ q { back = lstPop (back q) }

balance :: Deque a -> Deque a
balance q@(Q Nil Nil) = q
balance q@(Q Nil (Cons _ _ 1)) = q
balance q@(Q (Cons _ _ 1) Nil) = q
balance q@(Q (L _ n) (L _ m)) | n > 2 * m = let (f', b') = splitLst (front q) in Q { front = f', back = lstConcat (back q) (lstReverse b') }
                              | m > 2 * n = let (b', f') = splitLst (back q) in Q { front = lstConcat (front q) (lstReverse f'), back = b' }
                              | otherwise = q

newtype Forward a = FW (Deque a) deriving (Show)
newtype Backward a = BW (Deque a) deriving (Show)

instance Queue Forward where 

    empty = FW emptyDeque

    isEmpty (FW q) = isDequeEmppty q

    head (FW q) = peekFront q

    enqueue (FW q) e = FW $ pushBack e q

    tail (FW q) = FW $ popFront q

instance Queue Backward where 

    empty = BW emptyDeque

    isEmpty (BW q) = isDequeEmppty q

    head (BW q) = peekBack q

    enqueue (BW q) e = BW $ pushFront e q

    tail (BW q) = BW $ popBack q