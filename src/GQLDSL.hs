{-# LANGUAGE OverloadedStrings #-}

module GQLDSL where

import Data.List (intercalate)
import Data.Text (Text, pack, toTitle, unpack)
import GenUtils (toTitleString)
import Soothsayer ((***))

data GraphqlField = GraphqlField {field :: String, fieldType :: String}

typeGenerateMarker :: String
typeGenerateMarker = "# kensai-generate-marker-type"

inputGenerateMarker :: String
inputGenerateMarker = "# kensai-generate-marker-input"

queryGenerateMarker :: String
queryGenerateMarker = "# kensai-generate-marker-query"

mutationGenerateMarker :: String
mutationGenerateMarker = "# kensai-generate-marker-mutation"

createGraphqlField :: GraphqlField -> String
createGraphqlField gqlField = "\t{0}: {1}" *** [field gqlField, fieldType gqlField]

createGraphqlType :: String -> [GraphqlField] -> String
createGraphqlType name typeTuples = "type {0} {\n{1}\n}" *** [toTitleString name, intercalate "\n" (createGraphqlField <$> typeTuples)]

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
emptyQuery = "type Query {\n\t# kensai-generate-marker-query\n}"

emptyMutation :: Text
emptyMutation = "type Mutation {\n\t# kensai-generate-marker-mutation\n}"
