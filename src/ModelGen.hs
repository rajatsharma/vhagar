{-# LANGUAGE OverloadedStrings #-}

module ModelGen where

import Control.Exception (try)
import Data.Text (Text, pack, toTitle, unpack)
import Data.Text.IO (readFile, writeFile)
import GHC.IO.Exception (IOException (IOError))
import GODSL
  ( GoStructField (GoStructField),
    entityGenerateMarker,
    goStruct,
    migrationGenerateMarker,
    parseGqlField,
    useUuidImport,
  )
import GQLDSL
  ( GraphqlField (GraphqlField),
    createCreateMutation,
    createDeleteMutation,
    createGetQuery,
    createGraphqlCreateInput,
    createGraphqlDeleteInput,
    createGraphqlType,
    createListQuery,
    inputGenerateMarker,
    mutationGenerateMarker,
    queryGenerateMarker,
    typeGenerateMarker,
  )
import GenUtils (lineline, linetab, toTitleString, (++>), (+>>))
import Soothsayer ((***))
import System.Process (callCommand)
import Prelude hiding (readFile, writeFile)

data ModelArgs = ModelArgs {modelName :: Text, modelNamePlural :: Text, fields :: [GraphqlField]}

regenerate :: IO ()
regenerate = callCommand "go run github.com/99designs/gqlgen generate"

graphqlGenerator :: String -> String -> String -> [GraphqlField] -> Text -> Text
graphqlGenerator entity entityPlural pkgName fields =
  gType
    . input
    . query
    . mutation
  where
    fieldsWithId = fields ++ [GraphqlField "id" "ID!"]
    gqlType = createGraphqlType entity pkgName fieldsWithId
    gqlCreateInput = createGraphqlCreateInput entity fieldsWithId
    gqlDeleteInput = createGraphqlDeleteInput entity fieldsWithId
    gqlListQuery = createListQuery entity entityPlural
    gqlGetQuery = createGetQuery entity
    gqlCreateMutation = createCreateMutation entity
    gqlDeleteMutation = createDeleteMutation entity
    gType = typeGenerateMarker ++> gqlType
    input = inputGenerateMarker ++> (gqlCreateInput `lineline` gqlDeleteInput)
    query = queryGenerateMarker +>> (gqlListQuery `linetab` gqlGetQuery)
    mutation = mutationGenerateMarker +>> (gqlCreateMutation `linetab` gqlDeleteMutation)

entityGenerator :: String -> [GoStructField] -> Text -> Text
entityGenerator entity entityFields = useUuidImport . (entityGenerateMarker ++> entityStruct)
  where
    fieldsWithId = entityFields ++ [GoStructField "ID" "uuid.UUID" "`json:\"id\" gorm:\"primaryKey\"`"]
    entityStruct = goStruct entity fieldsWithId

model :: ModelArgs -> IO ()
model args = do
  let schemaFile = "./graph/schema.graphqls"
  let entityFile = "./graph/entity/entity_gen.go"
  let migrateFile = "./graph/entity/migrate.go"
  let entity = unpack $ modelName args
  let entityPlural = unpack $ modelNamePlural args
  let gqlFields = fields args

  pkgName <- readFile ".vhagar"

  schemaContents <- readFile schemaFile
  let generateGraphql = graphqlGenerator entity entityPlural (unpack pkgName) gqlFields
  writeFile schemaFile $ generateGraphql schemaContents

  entityContents <- readFile entityFile
  let goEntityFields = parseGqlField <$> gqlFields
  let generateEntity = entityGenerator entity goEntityFields
  writeFile entityFile $ generateEntity entityContents

  migrateFileContents <- readFile migrateFile
  let generateMigration = migrationGenerateMarker +>> ("db.AutoMigrate(&{0}{})" *** [toTitleString entity])
  writeFile migrateFile $ generateMigration migrateFileContents

  regenResult <- try regenerate :: IO (Either IOError ())
  case regenResult of
    Left e -> do
      writeFile entityFile entityContents
      writeFile schemaFile schemaContents
      writeFile migrateFile migrateFileContents
      regenerate
      putStrLn "Schema generation failed, reverting..."
    Right _ -> pure ()
