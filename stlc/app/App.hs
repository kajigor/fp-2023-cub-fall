module App where

-- optparse-applicative
import Options.Applicative
import Parser (parseLambdaTerm, ParserError, ParserError(..), mapLeft, prettyParserError)
import ProofTree (resultType, printProof, TypeError, TypeError(..))
import TypeCheck
import Data.Text (Text)

data Transformation
  = TypeCheck
  | Parse

data Action = Action
  { transformation :: Transformation
  , input :: Text
  , output :: FilePath
  }

data Args = Args
  { transformationArg :: Transformation
  , inputArg :: Text
  , outputArg :: FilePath
  }

actionParser :: Parser Args
actionParser =
  Args <$> parseTransformation
       <*> inputParser
       <*> outputParser

inputParser :: Parser Text
inputParser = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "String input"
  )

outputParser :: Parser FilePath
outputParser = strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "Proof output file"
  )

parseTransformation :: Parser Transformation
parseTransformation =
      typeCheckParser
  <|> parserParser

parserParser :: Parser Transformation
parserParser = flag' Parse
  (  long "parse"
  <> short 'p'
  <> help "Parse the term"
  )


typeCheckParser :: Parser Transformation
typeCheckParser = flag' TypeCheck
  (  long "typeCheck"
  <> short 'c'
  <> help "Type check the term"
  )

transform :: Args -> IO Action
transform (Args transformation input output) = do
  return $ Action transformation input output

data Error = InParser ParserError | InTypecheck TypeError

liftParser = mapLeft InParser 

liftTypecheck = mapLeft InTypecheck

prettyError :: Error -> String
prettyError (InParser err) = unlines ["Syntax error: ", prettyParserError err]
prettyError (InTypecheck err) = unlines ["Type error: ", printTypeError err]
  where
    
    printTypeError (IncompatibleContexts a b) = concat ["Incompatible contexts: ", show a, " and ", show b]
    printTypeError (IncompatibleVariable x t e) = concat ["Expected variable ", show x, " in enviromnent ", show e, " to have type ", show t]
    printTypeError (IncompatibleArgument m argT n nt) = concat ["Invalid argument type: expected ", show argT, " as input type for ", show m, ", got ", show n, " : ", show nt]
    printTypeError (ExpectedArrow t tt) = concat ["Expected ", show t, " : ", show tt, " to be an arrow type"]
    printTypeError (ExpectedBool c ct) = concat ["Expected condition to be Bool, got ", show c, " : ", show ct]
    printTypeError (ExpectedSame t tt e et) = concat ["Expected branches to match, got ", show t, " : ", show tt, " and ", show e, " : ", show et]
    printTypeError (UnknownVariable v e) = concat ["Unknown variable ", show v, " in environment ", show e]

printEither :: Show a => Either Error a -> IO ()
printEither (Left err) = do
  putStrLn $ prettyError err
printEither (Right x) = do
  putStrLn "Ok"
  print x

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  case transformation action of
    TypeCheck ->
      runTypeCheck (liftParser $ parseLambdaTerm (input action)) (output action)
    Parse ->
      printEither $ liftParser $ parseLambdaTerm (input action)

runTypeCheck term output = do
  case term >>= (liftTypecheck . typeCheckEmpty) of
    Left err -> do
      putStrLn $ prettyError err
    Right x -> do
      putStrLn "Ok"
      print (resultType x)
      writeFile output (printProof x)


runApp :: IO ()
runApp = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "STLC type checker"
      <> header "stlc"
      )
