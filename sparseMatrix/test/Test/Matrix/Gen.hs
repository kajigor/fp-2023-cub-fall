{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Test.Matrix.Gen where 

import Matrix 
import QuadTree

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genNat n = Gen.int (Range.constant 0 n)
genInt n = Gen.int (Range.constantFrom 0 (-n) n)

genMatrix size maxInt
  | size <= 0 = genCell 
  | otherwise = do 
      Gen.recursive
        Gen.choice
        [ -- non-recursive generators
          genCell
        ]
        [ -- recursive generators
          genQuad
        ]
  where 
    genCell = do 
      x <- genInt maxInt
      return $ Cell size x 
    genQuad = do 
      nw <- genMatrix (size - 1) maxInt 
      ne <- genMatrix (size - 1) maxInt 
      sw <- genMatrix (size - 1) maxInt 
      se <- genMatrix (size - 1) maxInt 
      return $ simplify (Quad size nw ne sw se)
