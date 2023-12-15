{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Matrix where

import QuadTree

diagonalM bound x =
  foldr (\b m -> insert m (Point b b) x) (Cell bound 0) [0 .. 2 ^ bound - 1]

class (Num a) => Matrix matrix a where
  add :: matrix a -> matrix a -> matrix a

  sub :: matrix a -> matrix a -> matrix a

  neg :: matrix a -> matrix a

  mult :: matrix a -> matrix a -> matrix a

  transpose :: matrix a -> matrix a

  scalarMult :: a -> matrix a -> matrix a

instance (Eq bound, Integral bound, Eq a, Num a) => Matrix (QuadTree bound) a where
  add (Cell boundl al) (Cell boundr ar)
    | boundl == boundr = Cell boundl (al + ar)
    | otherwise = undefined
  add (Cell boundl al) (Quad boundr nw ne sw se)
    | boundl == boundr = Quad boundr (add smallerCell nw) (add smallerCell ne) (add smallerCell sw) (add smallerCell se)
    | otherwise = undefined
    where
      smallerCell = Cell (boundl - 1) al
  add a@(Quad {}) b@(Cell _ _) = add b a
  add (Quad boundl nwl nel swl sel) (Quad boundr nwr ner swr ser)
    | boundl == boundr = simplify (Quad boundl (add nwl nwr) (add nel ner) (add swl swr) (add sel ser))
    | otherwise = undefined

  sub a b = add a (neg b)

  neg = scalarMult (-1)


  mult :: (Eq bound, Integral bound, Eq a, Num a) => QuadTree bound a -> QuadTree bound a -> QuadTree bound a
  mult (Cell boundl l) (Cell boundr r)
    | boundl == boundr = Cell boundl (sum $ replicateI (2 ^ boundl) (l * r))
    | otherwise = undefined
  mult (Cell bound a) q@(Quad {}) = mult (Quad bound (Cell (bound - 1) a) (Cell (bound - 1) a) (Cell (bound - 1) a) (Cell (bound - 1) a)) q
  mult q@(Quad {}) (Cell bound a) = mult q (Quad bound (Cell (bound - 1) a) (Cell (bound - 1) a) (Cell (bound - 1) a) (Cell (bound - 1) a))
  mult (Quad boundl a11 a12 a21 a22) (Quad boundr b11 b12 b21 b22)
    | boundl == boundr =
        simplify
          ( Quad
              boundl
              ((a11 `mult` b11) `add` (a12 `mult` b21))
              ((a11 `mult` b12) `add` (a12 `mult` b22))
              ((a21 `mult` b11) `add` (a22 `mult` b21))
              ((a21 `mult` b12) `add` (a22 `mult` b22))
          )
    | otherwise = undefined

  transpose c@(Cell _ _) = c
  transpose (Quad bound nw ne sw se) = Quad bound (transpose nw) (transpose sw) (transpose ne) (transpose se)

  scalarMult a (Cell bound b) = Cell bound (a * b)
  scalarMult a (Quad bound nw ne sw se) 
    | a == 0 = Cell bound 0
    | otherwise = Quad bound (scalarMult a nw) (scalarMult a ne) (scalarMult a sw) (scalarMult a se)

simplify :: (Integral bound, Eq a) => QuadTree bound a -> QuadTree bound a
simplify q@(Quad bound (Cell b1 a1) (Cell b2 a2) (Cell b3 a3) (Cell b4 a4))
  | b1 == b2 && b2 == b3 && b3 == b4 && a1 == a2 && a2 == a3 && a3 == a4 = Cell bound a1
  | otherwise = q
simplify q = q

isEqual (Cell boundl al) (Cell boundr ar) = boundl == boundr && al == ar
isEqual (Quad boundl nwl nel swl sel) (Quad boundr nwr ner swr ser) = boundl == boundr && isEqual nwl nwr && isEqual nel ner && isEqual swl swr && isEqual sel ser
isEqual _ _ = False

from2dList :: (Integral bound, Eq a) => [[a]] -> QuadTree bound a
from2dList list = do
  let listOfCells = (map . map) (Cell 0) list
  join 1 listOfCells
  where
    join bound listOfQuads = do
      let newList = map (pairIntoQuads bound) (listToPairs listOfQuads)
      case newList of
        [[x]] -> x
        _ -> join (bound + 1) newList

listToPairs :: [a] -> [(a, a)]
listToPairs [] = []
listToPairs (x : y : xs) = (x, y) : listToPairs xs

pairIntoQuads :: (Integral bound, Eq a) => bound -> ([QuadTree bound a], [QuadTree bound a]) -> [QuadTree bound a]
pairIntoQuads bound (f, s) = map (\((nw, sw), (ne, se)) -> simplify $ Quad bound nw ne sw se) (listToPairs (zip f s))

replicateI 0 x = []
replicateI a x = x : replicateI (a - 1) x