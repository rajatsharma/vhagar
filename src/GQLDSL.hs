{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GQLDSL where

import Data.List (intercalate)
import Data.Text (Text, pack, toTitle, unpack)
import GenUtils (toTitleString)
import Markers (mutationGenerateMarker, queryGenerateMarker)
import NeatInterpolation (text)
import Soothsayer ((***))

data GraphqlField = GraphqlField {field :: String, fieldType :: String}

createGraphqlField :: GraphqlField -> String
createGraphqlField gqlField = "\t{0}: {1}" *** [field gqlField, fieldType gqlField]

createGraphqlType :: String -> String -> [GraphqlField] -> String
createGraphqlType name pkgName typeTuples = "type {0} @goModel(model: \"{2}/graph/entity.{0}\") {\n{1}\n}" *** [toTitleString name, intercalate "\n" (createGraphqlField <$> typeTuples), pkgName]

filterIdType :: [GraphqlField] -> [GraphqlField]
filterIdType typeTuples = (\tt -> fieldType tt /= "ID!") `filter` typeTuples

createGraphqlCreateInput :: String -> [GraphqlField] -> String
createGraphqlCreateInput name typeTuples = "input Create{0}Input {\n{1}\n}" *** [toTitleString name, intercalate "\n" (createGraphqlField <$> filterIdType typeTuples)]

createGraphqlDeleteInput :: String -> [GraphqlField] -> String
createGraphqlDeleteInput name typeTuples = "input Delete{0}Input {\n\tid: ID!\n}" *** [toTitleString name]

createListQuery :: String -> String -> String
createListQuery name namePlural = "{0}: [{1}!]!" *** [namePlural, toTitleString name]

createGetQuery :: String -> String
createGetQuery name = "{0}(id: ID!): {1}!" *** [name, toTitleString name]

createCreateMutation :: String -> String
createCreateMutation name = "create{0}(input: Create{0}Input): {0}!" *** [toTitleString name]

createDeleteMutation :: String -> String
createDeleteMutation name = "delete{0}(input: Delete{0}Input): {0}!" *** [toTitleString name]

emptyQuery :: Text
emptyQuery = pack $ "type Query {\n\t{0}\n}" *** [queryGenerateMarker]

emptyMutation :: Text
emptyMutation = pack $ "type Mutation {\n\t{0}\n}" *** [mutationGenerateMarker]

goModeldirective :: Text
goModeldirective =
  [text|
    directive @goModel(model: String, models: [String!]) on OBJECT
      | INPUT_OBJECT
      | SCALAR
      | ENUM
      | INTERFACE
      | UNION|]
