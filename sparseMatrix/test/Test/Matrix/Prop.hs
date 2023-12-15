{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Test.Matrix.Prop where

import Matrix
import QuadTree
import Test.Matrix.Gen
import Hedgehog

hprop_unitAdd :: Property
hprop_unitAdd = property $ do
  let zeroQuadTree b = Cell b 0 
  b <- forAll $ genNat 5
  m <- forAll $ genMatrix b 10
  zeroQuadTree b === m `mult` zeroQuadTree b
  zeroQuadTree b === zeroQuadTree b `mult` m

hprop_commAdd :: Property
hprop_commAdd = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10 
  m2 <- forAll $ genMatrix b 10 
  m1 `add` m2 === m2 `add` m1

hprop_subNeg :: Property
hprop_subNeg = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10 
  m2 <- forAll $ genMatrix b 10 
  m1 `add` neg m2 === m1 `sub` m2

hprop_scalarMultDistr :: Property
hprop_scalarMultDistr = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10 
  m2 <- forAll $ genMatrix b 10 
  c1 <- forAll $ genInt 10 
  c2 <- forAll $ genInt 10
  c1 `scalarMult` (m1 `add` m2) === (c1 `scalarMult` m1) `add` (c1 `scalarMult` m2)
  (c1 + c2) `scalarMult` m1 === (c1 `scalarMult` m1) `add` (c2 `scalarMult` m1)

hprop_unitMult :: Property
hprop_unitMult = property $ do 
  b <- forAll $ genNat 5 
  m <- forAll $ genMatrix b 10 
  let unit = diagonalM b 1  
  m === m `mult` unit
  m === unit `mult` m
 
hprop_assocMult :: Property
hprop_assocMult = property $ do
  b <- forAll $ genNat 5
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  m3 <- forAll $ genMatrix b 10
  (m1 `mult` m2) `mult` m3 === m1 `mult` (m2 `mult` m3)

hprop_addTranspose :: Property
hprop_addTranspose = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  transpose (m1 `add` m2) === transpose m1 `add` transpose m2

hprop_multTranspose :: Property
hprop_multTranspose = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  transpose (m1 `mult` m2) === transpose m2 `mult` transpose m1

hprop_scalarMultTranspose :: Property
hprop_scalarMultTranspose = property $ do 
  b <- forAll $ genNat 5 
  m <- forAll $ genMatrix b 10
  c <- forAll $ genInt 10
  transpose (c `scalarMult` m) === c `scalarMult` transpose m

hprop_subTranspose :: Property
hprop_subTranspose = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  transpose (m1 `sub` m2) === transpose m1 `sub` transpose m2

hprop_transposeTwice :: Property
hprop_transposeTwice = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10
  transpose (transpose m1) === m1

hprop_multDistrib :: Property
hprop_multDistrib = property $ do 
  b <- forAll $ genNat 5 
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  m3 <- forAll $ genMatrix b 10
  (m1 `mult` m2) `add` (m1 `mult` m3) === m1 `mult` (m2 `add` m3)
  (m2 `mult` m1) `add` (m3 `mult` m1) === (m2 `add` m3) `mult` m1

hprop_assocAdd :: Property
hprop_assocAdd = property $ do
  b <- forAll $ genNat 5
  m1 <- forAll $ genMatrix b 10
  m2 <- forAll $ genMatrix b 10
  m3 <- forAll $ genMatrix b 10
  (m1 `add` m2) `add` m3 === m1 `add` (m2 `add` m3)
