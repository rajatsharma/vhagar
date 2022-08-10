{-# LANGUAGE OverloadedStrings #-}

module GODSL where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isSuffixOf, pack, stripSuffix, toTitle, unpack)
import GQLDSL (GraphqlField (field, fieldType))
import GenUtils (line, linetab, toTitleString, (~>))
import Soothsayer ((***))

data GoFuncArg = GoFuncArg {argName :: String, argType :: String}

data GoStructField = GoStructField {structFieldName :: String, structFieldType :: String, structFieldComments :: String}

createGoArg :: GoFuncArg -> String
createGoArg arg = "{0} {1}" *** [argName arg, argType arg]

goFunc :: Text -> [GoFuncArg] -> String -> String
goFunc name args body = "func {0}({1}) {" *** [unpack name, intercalate "," (createGoArg <$> args)] `linetab` body `line` "}"

goPackage :: String -> String
goPackage name = "package {0}" *** [name]

goImport :: String -> String -> String
goImport name alias = "import {1} \"{0}\"" *** [name, alias]

createGoStructField :: GoStructField -> String
createGoStructField field = "\t{0} {1} {2}" *** [structFieldName field, structFieldType field, structFieldComments field]

goStruct :: String -> [GoStructField] -> String
goStruct name field = "type {0} struct {\n{1}\n}" *** [toTitleString name, intercalate "\n" (createGoStructField <$> field)]

handleOptional :: Text -> Text
handleOptional gqlType
  | "!" `isSuffixOf` gqlType = fromMaybe "" $ stripSuffix "!" gqlType
  | otherwise = "*" <> gqlType

getGoType :: Text -> Text
getGoType = handleOptional . ("String" ~> "string") . ("Int" ~> "uint") . ("Float" ~> "float32") . ("Boolean" ~> "bool")

parseGqlField :: GraphqlField -> GoStructField
parseGqlField gqlField =
  GoStructField
    { structFieldName = toTitleString $ field gqlField,
      structFieldType = unpack $ getGoType $ pack $ fieldType gqlField,
      structFieldComments = "`json:\"{0}\"`" *** [field gqlField]
    }

useUuidImport :: Text -> Text
useUuidImport = goImport "github.com/google/uuid" "_" ~> goImport "github.com/google/uuid" ""
