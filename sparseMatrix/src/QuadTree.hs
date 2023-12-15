{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QuadTree where

import Data.List (intercalate)

data QuadTree bound a 
  = Cell bound a
  | Quad bound
         (QuadTree bound a) -- nw 
         (QuadTree bound a) -- ne 
         (QuadTree bound a) -- sw 
         (QuadTree bound a) -- se 
  deriving (Eq)

getBound :: QuadTree bound a -> bound
getBound (Cell b _) = b 
getBound (Quad b _ _ _ _) = b 

quad :: (Eq bound, Enum bound, Eq a)
     => QuadTree bound a 
     -> QuadTree bound a 
     -> QuadTree bound a 
     -> QuadTree bound a 
     -> QuadTree bound a
quad x = Quad (succ $ getBound x) x

cell :: bound -> a -> QuadTree bound a
cell = Cell 

-- QuadTree with a bound b is a square QuadTree of size 2^b * 2^b
type SquareQuadTree a = QuadTree Int a

data Point = Point { x :: Int, y :: Int }

data BreadKrumb = NW | NE | SW | SE
  deriving (Show, Eq)

getTrail :: Int -> Point -> [BreadKrumb]
getTrail bound (Point x y) =
  let halfsize = 2^(bound-1) in
    case (x < halfsize, y < halfsize) of
      (True, True) -> NW : getTrail (bound-1) (Point x y)
      (True, False) -> SW : getTrail (bound-1) (Point x (y - halfsize))
      (False, True) -> NE : getTrail (bound-1) (Point (x - halfsize) y)
      (False, False) -> SE : getTrail (bound-1) (Point (x - halfsize) (y - halfsize))

insert :: Eq a => SquareQuadTree a -> Point -> a -> SquareQuadTree a
insert (Cell size oldVal) _ newVal = Cell size (if oldVal == newVal then oldVal else newVal)
insert (Quad size nw ne sw se) point val =
  case getTrail size point of
    []      -> Quad size nw ne sw se
    NW:trail -> Quad size (insert nw point val) ne sw se
    NE:trail -> Quad size nw (insert ne point val) sw se
    SW:trail -> Quad size nw ne (insert sw point val) se
    SE:trail -> Quad size nw ne sw (insert se point val)

instance Show a => Show (SquareQuadTree a) where
  show quadTree =
      let list = to2dList quadTree in
      intercalate "\n" $ map (unwords . map show) list
    where
      to2dList (Cell size x) =
        let n = 2^size in
        replicate n (replicate n x)
      to2dList (Quad _ nw ne sw se) =
        let nw' = to2dList nw in
        let ne' = to2dList ne in
        let sw' = to2dList sw in
        let se' = to2dList se in
        let n = zipWith (++) nw' ne' in
        let s = zipWith (++) sw' se' in
        n ++ s
