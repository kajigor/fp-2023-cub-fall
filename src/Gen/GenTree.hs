{-# LANGUAGE FlexibleContexts #-}

module Gen.GenTree where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Gen.GenCommon
import Tree

data ReturnType = None | Bool Bool deriving (Eq, Show)
type OpType listL listR = ((listL -> (listL, ReturnType), listR -> (listR, ReturnType)), String)

insert' :: Tree tree a => a -> tree -> (tree, ReturnType)
insert' x tree = (insert x tree, None)

remove' :: Tree tree a => a -> tree -> (tree, ReturnType)
remove' x tree = (remove x tree, None)

member' :: Tree tree a => a -> tree -> (tree, ReturnType)
member' x tree = (tree, Bool $ member x tree)

genInsert :: (Tree tree1 Int, Tree tree2 Int) => Int -> Gen (OpType tree1 tree2)
genInsert n = (\x -> ((insert' x, insert' x), "insert " ++ show x)) <$> genInt n

genRemove :: (Tree tree1 Int, Tree tree2 Int) => Int -> Gen (OpType tree1 tree2)
genRemove n = (\x -> ((remove' x, remove' x), "remove " ++ show x)) <$> genInt n

genMember :: (Tree tree1 Int, Tree tree2 Int) => Int -> Gen (OpType tree1 tree2)
genMember n = (\x -> ((member' x, member' x), "member " ++ show x)) <$> genInt n

genOperation :: (Tree tree1 Int, Tree tree2 Int) => Int -> Gen (OpType tree1 tree2)
genOperation n = Gen.choice
  [ genInsert n
  , genRemove n
  , genMember n
  ]

genOpList :: (Tree tree1 Int, Tree tree2 Int) => Int -> Int -> Int -> Gen [OpType tree1 tree2]
genOpList minLength maxLength n =
  Gen.list (Range.constant minLength maxLength) (genOperation n)
