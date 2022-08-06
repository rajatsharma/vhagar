{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text.IO as T
import GHC.IO (unsafePerformIO)
import GODSL
  ( GoStructField (GoStructField),
    entityGenerateMarker,
    goStruct,
    parseGqlField,
  )
import GQLDSL (GraphqlField (..), createCreateMutation, createDeleteMutation, createGetQuery, createGraphqlCreateInput, createGraphqlDeleteInput, createGraphqlType, createListQuery, emptyMutation, emptyQuery, field, fieldType, inputGenerateMarker, mutationGenerateMarker, queryGenerateMarker, typeGenerateMarker)
import GenUtils ((++>), (+>>), (~>))
import Initialise (InitArgs (..), initialise)
import ModelGen (ModelArgs (..), model, regenerate)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.Process (callCommand, readProcess, runCommand)
import Prelude hiding (readFile, writeFile)

data Command
  = Init
  | Model
  | Regen

logExit :: Text -> IO a
logExit msg = do
  putStrLn $ unpack msg
  exitFailure

processField :: Text -> GraphqlField
processField columnStr = unsafePerformIO $ do
  case T.splitOn ":" columnStr of
    [name, typename] ->
      pure
        GraphqlField {field = unpack name, fieldType = unpack typename}
    [name] -> logExit $ "No Type supplied for:" <> name
    _ -> logExit $ "Illegal pattern:" <> columnStr

modelArgsInspector :: [Text] -> IO ModelArgs
modelArgsInspector ["gen-model"] = do
  logExit "Command gen-model requires one more argument model-name and optional list of columns followed by it"
modelArgsInspector ["gen-model", _] = logExit "Plural form of model is also required"
modelArgsInspector ["gen-model", name, namePlural] = do
  putStrLn "No columns supplied model will be created with only one column id:Int"
  pure ModelArgs {modelName = name, modelNamePlural = namePlural, fields = []}
modelArgsInspector ("gen-model" : name : namePlural : xs) = pure ModelArgs {modelName = name, modelNamePlural = namePlural, fields = processField `map` xs}
modelArgsInspector null = logExit "Required arg, nothing supplied"

initArgsInspector :: [Text] -> IO InitArgs
initArgsInspector ["init"] = logExit "Required arg, nothing supplied"
initArgsInspector ["init", name] = pure InitArgs {projectName = name}
initArgsInspector _ = logExit "Required arg, nothing supplied"

argsInspector :: [String] -> IO Command
argsInspector ("init" : _) = pure Init
argsInspector ("gen-model" : _) = pure Model
argsInspector ("regen" : _) = pure Regen
argsInspector _ = logExit "Invalid command"

main :: IO ()
main = do
  args <- getArgs
  command <- argsInspector args
  let textArgs = pack `map` args
  case command of
    Model -> do
      verifiedArgs <- modelArgsInspector textArgs
      model verifiedArgs
    Init -> do
      verifiedArgs <- initArgsInspector textArgs
      initialise verifiedArgs
    Regen -> regenerate
