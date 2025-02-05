{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersperse)
import Data.Text (Text, pack, splitOn, unpack)
import GHC.IO (unsafePerformIO)
import GQLDSL (GraphqlField (..))
import Initialise (InitArgs (..), initialise)
import ModelGen (ModelArgs (..), model, regenerate)
import Options.Applicative
  ( Alternative (many),
    Applicative (pure, (<*>)),
    Parser,
    command,
    execParser,
    fullDesc,
    header,
    helper,
    idm,
    info,
    metavar,
    progDesc,
    strArgument,
    subparser,
    (<$>),
    (<**>),
  )
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.Process (callCommand, readProcess, runCommand)
import Prelude hiding (readFile, writeFile)

data Vhagar
  = Init String
  | Model String String [String]
  | Regen

logExit :: Text -> IO a
logExit msg = do
  putStrLn $ unpack msg
  exitFailure

processField :: Text -> GraphqlField
processField columnStr = unsafePerformIO $ do
  case splitOn ":" columnStr of
    [name, typename] ->
      pure
        GraphqlField {field = unpack name, fieldType = unpack typename}
    [name] -> logExit $ "No Type supplied for:" <> name
    _ -> logExit $ "Illegal pattern:" <> columnStr

createInit :: Parser Vhagar
createInit = Init <$> strArgument (metavar "PROJECT_NAME")

createModel :: Parser Vhagar
createModel =
  Model
    <$> strArgument (metavar "MODEL_NAME")
    <*> strArgument (metavar "MODEL_NAME_PLURAL")
    <*> many (strArgument (metavar "FIELD..."))

createRegen :: Parser Vhagar
createRegen = pure Regen

vhagarParser :: Parser Vhagar
vhagarParser =
  subparser $
    command "init" (info createInit $ progDesc "Initialise project")
      <> command "gen-model" (info createModel $ progDesc "Generate graphql model and db entity")
      <> command "regen" (info createRegen $ progDesc "Regenerate schema")

runner :: Vhagar -> IO ()
runner (Init name) = initialise $ InitArgs $ pack name
runner (Model modelName modelNamePlural modelFields) = do
  if null modelFields then logExit "No fields supplied" else pure ()
  let fields = processField . pack <$> modelFields
  model $ ModelArgs (pack modelName) (pack modelNamePlural) fields
runner Regen = regenerate

shell :: IO ()
shell = runner =<< execParser opts
  where
    opts =
      info (vhagarParser <**> helper) $
        fullDesc
          <> progDesc "Enter Command to run, see available commands for command descriptions."
          <> header "Vhagar"

main :: IO ()
main = shell
