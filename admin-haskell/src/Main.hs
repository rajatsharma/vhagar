{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)

data Option
  = Init
  | Model

logExit :: Text -> IO a
logExit msg = do
  print msg
  exitFailure

data Args = Args {option :: Option, secondary :: Text, tertiary :: [Text]}

data Column = Column {name :: Text, columnType :: Text, sqlType :: Text}

(~>) :: String -> String -> Text -> Text
(~>) a b = T.replace (T.pack a) (T.pack b)

handleSquares :: Text -> Text
handleSquares = ("[" ~> "<") . ("]" ~> ">")

getSqlType :: Text -> Text
getSqlType =
  handleSquares
    . ("String" ~> "Varchar")
    . ("i32" ~> "Int4")
    . ("uuid" ~> "Uuid")
    . ("bool" ~> "Bool")
    . ("f64" ~> "float8")
    . ("Option" ~> "Nullable")
    . ("Vec" ~> "Array")

getRustType :: Text -> Text
getRustType = handleSquares . ("uuid" ~> "uuid::Uuid")

processColumn :: Text -> IO Column
processColumn columnStr = do
  case T.splitOn ":" columnStr of
    [name, typename] ->
      pure
        Column
          { name = name,
            columnType = getRustType typename,
            sqlType = getSqlType typename
          }
    [name] -> logExit $ "No Type supplied for:" <> name
    illegal -> logExit $ "Illegal pattern:" <> columnStr

model :: Args -> IO ()
model args = do
  let entity = secondary args
  let columns = processColumn `map` tertiary args
  pure ()

argsInspector :: [Text] -> IO Args
argsInspector ["gen-model"] = do
  logExit "Command gen-model requires one more argument model-name and optional list of columns followed by it"
argsInspector ["gen-model", modelname] = do
  putStrLn "No columns supplied model will be created with only one column id:Int"
  pure Args {option = Model, secondary = modelname, tertiary = []}
argsInspector ("gen-model" : modelname : xs) = pure Args {option = Model, secondary = modelname, tertiary = xs}
argsInspector ("init" : projectName : xs) = pure Args {option = Init, secondary = projectName, tertiary = []}
argsInspector null = logExit "Required arg, nothing supplied"

main :: IO ()
main = do
  args <- getArgs
  verifiedArgs <- argsInspector $ T.pack `map` args
  pure ()
