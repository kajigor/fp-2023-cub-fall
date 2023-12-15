{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Matrix where

import QuadTree

diagonalM :: (Eq a, Num a) => Int -> a -> SquareQuadTree a
diagonalM bound x =
  foldr (\b m -> insert m (Point b b) x) (Cell bound 0) [0..2^bound-1]

class Num a => Matrix matrix a where
  add :: matrix a -> matrix a -> matrix a

  sub :: matrix a -> matrix a -> matrix a

  neg :: matrix a -> matrix a

  mult :: matrix a -> matrix a -> matrix a

  transpose :: matrix a -> matrix a

  scalarMult :: a -> matrix a -> matrix a

instance (Eq bound, Integral bound, Eq a, Num a, Show bound) => Matrix (QuadTree bound) a where

  add :: QuadTree bound a -> QuadTree bound a -> QuadTree bound a
  add (Cell b 0) m | b == getBound m = m
  add n (Cell b 0) | getBound n == b = n
  add (Cell b v) (Cell b' v') | b == b' = Cell b (v + v')
  add (Quad b x y z w) (Quad b' x' y' z' w') | b == b' = quad (x `add` x') (y `add` y') (z `add` z') (w `add` w')
  add n@(Quad b _ _ _ _) (Cell b' v) | b == b' = let 
      c = Cell (pred b') v
      m = Quad b c c c c in n `add` m
  add n@(Cell {}) m@(Quad {}) = m `add` n
  add _ _ = error "Unmatched dimensions"

  sub :: QuadTree bound a -> QuadTree bound a -> QuadTree bound a
  sub n m = n `add` neg m

  neg :: QuadTree bound a -> QuadTree bound a
  neg = (negate <$>)

  transpose :: QuadTree bound a -> QuadTree bound a
  transpose m@(Cell {}) = m
  transpose (Quad b x y z w) = Quad b (transpose x) (transpose z) (transpose y) (transpose w)

  scalarMult :: a -> QuadTree bound a -> QuadTree bound a
  scalarMult a = normalize . ((a *) <$>)

  mult (Cell b 0) m | b == getBound m = Cell b 0
  mult n (Cell b 0) | getBound n == b = Cell b 0
  mult (Cell b v) (Cell b' v') | b == b' = Cell b (adjust b (v * v'))
    where
      adjust 0 x = x
      adjust k x = adjust (pred k) (2 * x)
  mult (Quad b x y z w) (Quad b' x' y' z' w') | b == b' = quad 
    ((x `mult` x') `add` (y `mult` z')) 
    ((x `mult` y') `add` (y `mult` w')) 
    ((z `mult` x') `add` (w `mult` z')) 
    ((z `mult` y') `add` (w `mult` w'))
  mult n@(Quad b _ _ _ _) (Cell b' v) | b == b' = let 
      c = Cell (pred b') v
      m = Quad b c c c c 
      in n `mult` m
  mult (Cell b v) m@(Quad b' _ _ _ _) | b == b' = let 
      c = Cell (pred b') v
      n = Quad b c c c c 
      in n `mult` m
  mult n m = error $ "Unmatched dimensions: " ++ show (getBound n) ++ " / " ++ show (getBound m)
  