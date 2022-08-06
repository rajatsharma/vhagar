{-# LANGUAGE OverloadedStrings #-}

module ModelGen where

import Data.Text (Text, pack, toTitle, unpack)
import Data.Text.IO (readFile, writeFile)
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
import GenUtils (toTitleString, (++>), (+>>))
import Soothsayer ((***))
import System.Process (callCommand)
import Prelude hiding (readFile, writeFile)

data ModelArgs = ModelArgs {modelName :: Text, modelNamePlural :: Text, fields :: [GraphqlField]}

identity :: a -> a
identity x = x

regenerate :: IO ()
regenerate = callCommand "go run github.com/99designs/gqlgen generate"

graphqlGenerator :: String -> String -> [GraphqlField] -> Text -> Text
graphqlGenerator entity entityPlural fields =
  identity
    . generateType
    . generateCreateInput
    . generateDeleteInput
    . generateListQuery
    . generateGetQuery
    . generateCreateMutation
    . generateDeleteMutation
  where
    fieldsWithId = fields ++ [GraphqlField "id" "ID!"]
    gqlType = createGraphqlType entity fieldsWithId
    gqlCreateInput = createGraphqlCreateInput entity fieldsWithId
    gqlDeleteInput = createGraphqlDeleteInput entity fieldsWithId
    gqlListQuery = createListQuery entity entityPlural
    gqlGetQuery = createGetQuery entity
    gqlCreateMutation = createCreateMutation entity
    gqlDeleteMutation = createDeleteMutation entity
    generateType = typeGenerateMarker ++> gqlType
    generateCreateInput = inputGenerateMarker ++> gqlCreateInput
    generateDeleteInput = inputGenerateMarker ++> gqlDeleteInput
    generateListQuery = queryGenerateMarker +>> gqlListQuery
    generateGetQuery = queryGenerateMarker +>> gqlGetQuery
    generateCreateMutation = mutationGenerateMarker +>> gqlCreateMutation
    generateDeleteMutation = mutationGenerateMarker +>> gqlDeleteMutation

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

  schemaContents <- readFile schemaFile
  let generateGraphql = graphqlGenerator entity entityPlural gqlFields
  writeFile schemaFile $ generateGraphql schemaContents

  entityContents <- readFile entityFile
  let goEntityFields = parseGqlField <$> gqlFields
  let generateEntity = entityGenerator entity goEntityFields
  writeFile entityFile $ generateEntity entityContents

  migrateFileContents <- readFile migrateFile
  let generateMigration = migrationGenerateMarker +>> ("db.AutoMigrate(&{0}{})" *** [toTitleString entity])
  writeFile migrateFile $ generateMigration migrateFileContents
  regenerate
