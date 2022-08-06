{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Initialise where

import Data.FileEmbed
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.IO (readFile, writeFile)
import GHC.TypeLits (ErrorMessage (Text))
import GODSL (GoFuncArg (GoFuncArg), entityGenerateMarker, goFunc, goImport, goPackage, migrationGenerateMarker)
import GQLDSL
  ( emptyMutation,
    emptyQuery,
    goModeldirective,
    inputGenerateMarker,
    typeGenerateMarker,
  )
import GenUtils (lineline)
import Soothsayer ((***))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand, readProcess, runCommand)
import Prelude hiding (readFile, writeFile)

newtype InitArgs = InitArgs {projectName :: Text}

writeMigrationFile :: IO ()
writeMigrationFile = do
  let entityPackage = goPackage "entity"
  let gormImport = goImport "gorm.io/gorm" ""
  let migrateAllFunc = goFunc "MigrateAll" [GoFuncArg "db" "*gorm.DB"] migrationGenerateMarker
  let contents = entityPackage `lineline` gormImport `lineline` migrateAllFunc
  writeFile "./graph/entity/migrate.go" $ pack contents

writeEntityFile :: IO ()
writeEntityFile = do
  let entityPackage = goPackage "entity"
  let gormImport = goImport "github.com/google/uuid" "_"
  let contents = entityPackage `lineline` gormImport `lineline` entityGenerateMarker
  writeFile "./graph/entity/entity_gen.go" $ pack contents

initialise :: InitArgs -> IO ()
initialise args = do
  let name = projectName args
  callCommand $ "go mod init " ++ unpack name
  writeFile "tools.go" "// +build tools\npackage tools\nimport _ \"github.com/99designs/gqlgen\""
  toolsGo <- readProcess "gofmt" ["tools.go"] []
  writeFile "tools.go" $ pack toolsGo
  callCommand "go mod tidy"
  callCommand "go run github.com/99designs/gqlgen init"
  let emptySchema = intercalate "\n\n" [goModeldirective, pack typeGenerateMarker, pack inputGenerateMarker, emptyQuery, emptyMutation]
  currentDirectory <- getCurrentDirectory
  let directoryName = takeBaseName currentDirectory

  writeFile "./graph/schema.graphqls" emptySchema
  writeFile ".vhagar" $ pack $ "{0}" *** [unpack name]

  let serverGoContents = $(embedStringFile "template/server.go.template") *** [unpack name]
  writeFile "server.go" $ pack serverGoContents
  let makefileContents = $(embedStringFile "template/Makefile.template") *** [directoryName]
  writeFile "Makefile" $ pack makefileContents
  let dockerComposeContents = $(embedStringFile "template/docker-compose-dev.yml") *** [directoryName]
  writeFile "docker-compose-dev.yml" $ pack dockerComposeContents

  callCommand "go get -u gorm.io/gorm"
  callCommand "go get -u gorm.io/driver/postgres"
  callCommand "go get -u github.com/google/uuid"

  createDirectoryIfMissing True "./graph/entity"

  writeMigrationFile
  writeEntityFile

  callCommand "go mod tidy"
