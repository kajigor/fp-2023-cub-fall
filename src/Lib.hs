module Lib
    ( match, string, wildcard, union, concatenation, star, plus, question, repeatN, repeatBetween, anyOf
    ) where

import Data.List ( foldl' )

data Regex a =
    Bottom |
    String [a] |
    Wildcard |
    Union (Regex a) (Regex a) |
    Concatenation (Regex a) (Regex a) |
    Repeat (Regex a) Int Int |
    Kleene (Regex a)

match :: Eq a => [a] -> Regex a -> Bool
match str regex = matchEmpty $ foldl' (flip derivative) regex str

matchEmpty :: Regex a -> Bool
matchEmpty Bottom = False
matchEmpty (String []) = True
matchEmpty (String _) = False
matchEmpty Wildcard = False
matchEmpty (Union a b) = matchEmpty a || matchEmpty b
matchEmpty (Concatenation a b) = matchEmpty a && matchEmpty b
matchEmpty (Repeat _ 0 _) = True
matchEmpty (Repeat r _ _) = matchEmpty r
matchEmpty (Kleene _) = True

derivative :: Eq a => a -> Regex a -> Regex a
derivative _ Bottom = Bottom
derivative a (String []) = Bottom
derivative a (String (b : s))
    | a == b = String s
    | otherwise = Bottom
derivative _ Wildcard = String []
derivative a (Union r1 r2) = Union (derivative a r1) (derivative a r2)
derivative a (Concatenation r1 r2)
    | matchEmpty r1 = Union (Concatenation (derivative a r1) r2) (derivative a r2)
    | otherwise = Concatenation (derivative a r1) r2
derivative _ (Repeat _ _ 0) = Bottom
derivative a (Repeat r from to) = Concatenation (derivative a r) (Repeat r (max (from - 1) 0) (to - 1))
derivative a k@(Kleene r) = Concatenation (derivative a r) k

string :: [a] -> Regex a
string = String

wildcard :: Regex a
wildcard = Wildcard

union :: Regex a -> Regex a -> Regex a
union = Union

concatenation :: Regex a -> Regex a -> Regex a
concatenation = Concatenation

star :: Regex a -> Regex a
star = Kleene

plus :: Regex a -> Regex a
plus a = Concatenation a (Kleene a)

question :: Regex a -> Regex a
question a = Repeat a 0 1

repeatN :: Regex a -> Int -> Regex a
repeatN a n = Repeat a n n

repeatBetween :: Regex a -> Int -> Int -> Regex a
repeatBetween = Repeat

anyOf :: [a] -> Regex a
anyOf chars = foldr1 Union (map (\x -> string [x]) chars)