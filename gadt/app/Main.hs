module Main (main) where

import Expr.Parser (parse)
import Expr.Run

main :: IO ()
main = do
    s <- getLine
    print (run s)
