module Test.Matrix.UnitTest where 


import Test.Tasty.HUnit (Assertion, (@?=))
import Matrix
import QuadTree

type SquareIntMatrix = SquareQuadTree Int 

m1x1 :: Int -> SquareIntMatrix
m1x1 = cell 0

m2x2 ::  Int -> Int -> Int -> Int -> SquareIntMatrix
m2x2 nw ne sw se = quad (m1x1 nw) (m1x1 ne) (m1x1 sw) (m1x1 se)

matrix1, matrix2, matrix3, examplematrix1, examplematrix2, examplematrix1plus2, examplematrix1mult2, m16, m16multm16 :: SquareIntMatrix
matrix1 = m2x2 1 2 3 4
matrix2 = m2x2 5 6 7 8
matrix3 = m2x2 1 (-3) 5 (-7)

-- 0 0 0 0 1 1 2 2
-- 0 0 0 0 1 1 2 2
-- 0 0 0 0 3 4 7 7
-- 0 0 0 0 5 6 7 7
-- 1 1 2 2 8 8 8 8
-- 1 1 2 2 8 8 8 8
-- 3 3 4 4 8 8 8 8
-- 3 3 4 4 8 8 8 8
examplematrix1 =
  Quad 3  (Cell 2 0)
          (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Quad 1 (Cell 0 3)
                          (Cell 0 4)
                          (Cell 0 5)
                          (Cell 0 6))
                  (Cell 1 7))
          (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Cell 1 3)
                  (Cell 1 4))
          (Cell 2 8)

-- 1 1 2 2 0 0 0 0 
-- 1 1 2 2 0 0 0 0 
-- 3 4 7 7 0 0 0 0 
-- 5 6 7 7 0 0 0 0 
-- 8 8 8 8 1 1 2 2
-- 8 8 8 8 1 1 2 2
-- 8 8 8 8 3 3 4 4
-- 8 8 8 8 3 3 4 4
examplematrix2 =
  Quad 3  (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Quad 1 (Cell 0 3)
                          (Cell 0 4)
                          (Cell 0 5)
                          (Cell 0 6))
                  (Cell 1 7))
          (Cell 2 0)
          (Cell 2 8)
          (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Cell 1 3)
                  (Cell 1 4))

examplematrix1plus2 =
  Quad 3  (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Quad 1 (Cell 0 3)
                          (Cell 0 4)
                          (Cell 0 5)
                          (Cell 0 6))
                  (Cell 1 7))
          (Quad 2 (Cell 1 1)
                  (Cell 1 2)
                  (Quad 1 (Cell 0 3)
                          (Cell 0 4)
                          (Cell 0 5)
                          (Cell 0 6))
                  (Cell 1 7))
          (Quad 2 (Cell 1 9)
                  (Cell 1 10)
                  (Cell 1 11)
                  (Cell 1 12))
          (Quad 2 (Cell 1 9)
                  (Cell 1 10)
                  (Cell 1 11)
                  (Cell 1 12))

examplematrix1mult2 = 
  Quad 3 (Quad 2 (Cell 1 48)
                 (Cell 1 48)
                 (Quad 1 (Cell 0 168)
                         (Cell 0 168)
                         (Cell 0 200)
                         (Cell 0 200))
                 (Quad 1 (Cell 0 168)
                         (Cell 0 168)
                         (Cell 0 200)
                         (Cell 0 200)))
         (Quad 2 (Cell 1 14)
                 (Cell 1 20)
                 (Quad 1 (Cell 0 49)
                         (Cell 0 49)
                         (Cell 0 53)
                         (Cell 0 53))
                 (Quad 1 (Cell 0 70)
                         (Cell 0 70)
                         (Cell 0 78)
                         (Cell 0 78)))
         (Quad 2 (Quad 1 (Cell 0 274)
                         (Cell 0 278)
                         (Cell 0 274)
                         (Cell 0 278))
                 (Cell 1 288)
                 (Quad 1 (Cell 0 294)
                         (Cell 0 302)
                         (Cell 0 294)
                         (Cell 0 302))
                 (Cell 1 324))
         (Quad 2 (Cell 1 64)
                 (Cell 1 96)
                 (Cell 1 64)
                 (Cell 1 96))

m16 = Quad 2 (Quad 1 (m1x1 1) (m1x1 2) (m1x1 5) (m1x1 6))
             (Quad 1 (m1x1 3) (m1x1 4) (m1x1 7) (m1x1 8))
             (Quad 1 (m1x1 9) (m1x1 10) (m1x1 13) (m1x1 14))
             (Quad 1 (m1x1 11) (m1x1 12) (m1x1 15) (m1x1 16))

m16multm16 = 
   Quad 2 (Quad 1 (m1x1 90) (m1x1 100) (m1x1 202) (m1x1 228))
          (Quad 1 (m1x1 110) (m1x1 120) (m1x1 254) (m1x1 280))
          (Quad 1 (m1x1 314) (m1x1 356) (m1x1 426) (m1x1 484))
          (Quad 1 (m1x1 398) (m1x1 440) (m1x1 542) (m1x1 600))

unit_addition :: Assertion
unit_addition = do 
  matrix1 `add` matrix2 @?= m2x2 6 8 10 12 
  matrix1 `add` matrix3 @?= m2x2 2 (-1) 8 (-3)
  matrix2 `add` matrix3 @?= m2x2 6 3 12 1 
  examplematrix1 `add` examplematrix2 @?= examplematrix1plus2

unit_multiplication :: Assertion
unit_multiplication = do 
  matrix1 `mult` matrix2 @?= m2x2 19 22 43 50
  matrix1 `mult` matrix3 @?= m2x2 11 (-17) 23 (-37)
  matrix2 `mult` matrix3 @?= m2x2 35 (-57) 47 (-77) 
  examplematrix1 `mult` examplematrix2 @?= examplematrix1mult2
  m16 `mult` m16 @?= m16multm16

unit_sub :: Assertion
unit_sub = do 
  matrix1 `sub` matrix2 @?= Cell 1 (-4)
  matrix2 `sub` matrix1 @?= Cell 1 4

unit_neg :: Assertion
unit_neg = do 
  neg matrix1 @?= m2x2 (-1) (-2) (-3) (-4)
  neg matrix2 @?= m2x2 (-5) (-6) (-7) (-8)
  neg matrix3 @?= m2x2 (-1) 3 (-5) 7 

unit_scalarMult :: Assertion
unit_scalarMult = do 
  10 `scalarMult` matrix1 @?= m2x2 10 20 30 40
  10 `scalarMult` matrix2 @?= m2x2 50 60 70 80
  10 `scalarMult` matrix3 @?= m2x2 10 (-30) 50 (-70)

unit_transpose :: Assertion
unit_transpose = do 
  transpose matrix1 @?= m2x2 1 3 2 4 
  transpose matrix2 @?= m2x2 5 7 6 8
  transpose matrix3 @?= m2x2 1 5 (-3) (-7)
