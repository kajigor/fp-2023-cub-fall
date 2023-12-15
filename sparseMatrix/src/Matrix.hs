{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Matrix where

import QuadTree

diagonalM :: (Eq a, Num a) => Int -> a -> SquareQuadTree a
diagonalM bound x =
  foldr (\b m -> insert m (Point b b) x) (Cell bound 0) [0..2^bound-1]

class (Functor matrix, Num a) => Matrix matrix a where
  add :: matrix a -> matrix a -> matrix a

  sub :: matrix a -> matrix a -> matrix a
  sub n m = n `add` (neg m)

  neg :: matrix a -> matrix a
  neg = (negate <$>)

  mult :: matrix a -> matrix a -> matrix a

  transpose :: matrix a -> matrix a

  scalarMult :: a -> matrix a -> matrix a

instance (Eq bound, Integral bound, Eq a, Num a, Show bound) => Matrix (QuadTree bound) a where

  add :: QuadTree bound a -> QuadTree bound a -> QuadTree bound a
  add m1 m2 | getBound m1 == getBound m2 = m1 `helper` m2
            | otherwise = error "Unmatched dimensions"
    where
      helper (Cell b 0) m = m
      helper n (Cell b 0) = n
      helper (Cell b v) (Cell b' v') = Cell b (v + v')
      helper (Quad b x y z w) (Quad b' x' y' z' w') = quad (x `helper` x') (y `helper` y') (z `helper` z') (w `helper` w')
      helper n@(Quad b _ _ _ _) (Cell b' v) = n `helper` (denormalized b' v)
      helper n@(Cell {}) m@(Quad {}) = m `add` n

  transpose :: QuadTree bound a -> QuadTree bound a
  transpose m@(Cell {}) = m
  transpose (Quad b x y z w) = Quad b (transpose x) (transpose z) (transpose y) (transpose w)

  scalarMult :: a -> QuadTree bound a -> QuadTree bound a
  scalarMult 0 m = Cell (getBound m) 0
  scalarMult 1 m = m
  scalarMult a m = (a *) <$> m -- normalized as long as there isn't any zero divisors in a

  mult m1 m2 | getBound m1 == getBound m2 = m1 `helper` m2
             | otherwise = error $ "Unmatched dimensions"
    where

      adjust k x = (2 ^ k) * x

      helper (Cell b 0) m = Cell b 0
      helper n (Cell b 0) = Cell b 0
      helper (Cell b v) (Cell b' v') = Cell b (adjust b (v * v'))
      helper n@(Quad b _ _ _ _) (Cell b' v) = n `helper` (denormalized b' v)
      helper (Cell b v) m@(Quad b' _ _ _ _) = (denormalized b v) `helper` m
      helper (Quad b x y z w) (Quad b' x' y' z' w') = quad 
        ((x `helper` x') `add` (y `helper` z')) 
        ((x `helper` y') `add` (y `helper` w')) 
        ((z `helper` x') `add` (w `helper` z')) 
        ((z `helper` y') `add` (w `helper` w'))