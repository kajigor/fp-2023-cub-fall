module App where

-- optparse-applicative
import Options.Applicative
import Parser (parseLambdaTerm)
import ProofTree (resultType, printProof)
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

printEither :: Show a => Either String a -> IO ()
printEither (Left err) = do
  putStrLn "Error"
  putStrLn err
printEither (Right x) = do
  putStrLn "Ok"
  print x

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  case transformation action of
    TypeCheck ->
      runTypeCheck (parseLambdaTerm (input action)) (output action)
    Parse ->
      printEither $ parseLambdaTerm (input action)

runTypeCheck term output = do
  case term >>= typeCheckEmpty of
    Left err -> do
      putStrLn "Error"
      putStrLn err
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
