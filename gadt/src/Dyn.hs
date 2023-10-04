{-# LANGUAGE GADTs #-}

module Dyn where

data Dyn a where
  S :: String -> Dyn String
  C :: Char -> Dyn Char
  B :: Bool -> Dyn Bool

getValue :: Dyn a -> a
getValue (S s) = s
getValue (C c) = c
getValue (B b) = b

printValue :: Dyn a -> IO ()
printValue (S s) = print s
printValue (C c) = print c
printValue (B b) = print b

data Wrapped where
  Wrap :: Dyn a -> Wrapped

fromString :: String -> Wrapped
fromString str
  | str == "yes" = Wrap $ B True
  | str == "no" = Wrap $ B False
  | length str == 1 = Wrap $ C $ head str
  | otherwise = Wrap $  S str

printWrapped :: Wrapped -> IO ()
printWrapped (Wrap v) = printValue v