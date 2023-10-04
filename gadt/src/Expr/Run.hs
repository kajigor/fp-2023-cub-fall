module Expr.Run (run) where

import Expr.Parser
import Expr.Typed

-- ghci> run "if ?(-1+1) then (if ?0 then 42 else 777) else 13"
-- Just "42"
run :: String -> Maybe String
run str = do
  e <- parse str
  check e $ Just . show . eval
