module Error where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

data LambdaError
  = ParseError String
  | TypeCheckError Err
  deriving Show

data Err
  = VarNotInEnv String
  | TypeMismatch
  | BranchesWDiffTypes
  | CondNotBool
  deriving Show