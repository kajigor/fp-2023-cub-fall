{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module PhysicistsQueue ( PhysicistsQueue, emptyPhysicistsQueue ) where

import Queue

data PhysicistsQueue a = PhysicistsQueue [a] [a] Int [a] Int

instance Queue (PhysicistsQueue a) a where
    empty (PhysicistsQueue [] _ _ _ _) = True
    empty _ = False

    enqueue x (PhysicistsQueue w f fLength r rLength) = checkInvariants $ PhysicistsQueue w f fLength (x : r) (rLength + 1)

    head (PhysicistsQueue (x : xs) _ _ _ _) = x
    
    tail (PhysicistsQueue (_ : xs) f fLength r rLength) = checkInvariants $ PhysicistsQueue xs f (fLength - 1) r rLength

emptyPhysicistsQueue = PhysicistsQueue [] [] 0 [] 0

checkInvariants q = checkW $ checkR q

checkW (PhysicistsQueue [] f fLength r rLength) = PhysicistsQueue f f fLength r rLength
checkW q = q

checkR queue@(PhysicistsQueue w f fLength r rLength)
    | fLength >= rLength = queue
    | otherwise = PhysicistsQueue f (f ++ reverse r) (fLength + rLength) [] 0