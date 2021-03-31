module Main where

import Prelude
import Data.List ((!!))
import Data.Maybe (fromMaybe, isNothing)
import Effect (Effect)
import Effect.Class.Console (log)
import FS (copyDir, replace)
import Inflections (pascalCase, plural)
import Node.Args (args)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), defaultSpawnOptions, onExit, spawn, stdout)
import Node.Encoding as Encoding
import Node.FS.Sync as S
import Node.Globals (__dirname)
import Node.Path as Path
import Node.Process (exit)
import Node.Stream (onData)
import Text.Handlebars (compile)

appendPointer :: String
appendPointer = "//Replace me"

path :: Array Path.FilePath -> Path.FilePath
path = Path.concat

appendTo :: String -> String -> Effect Unit
appendTo file contents = replace { files: file, from: appendPointer, to: contents <> "\n" <> appendPointer }

makeCompilerWithVariables :: forall a. a -> String -> String
makeCompilerWithVariables variables template = compile template variables

readFile :: String -> Effect String
readFile = S.readTextFile Encoding.UTF8

appendFile :: String -> String -> Effect Unit
appendFile = S.appendTextFile Encoding.UTF8

getTemplate :: String -> Effect String
getTemplate templatePath = readFile (path [ __dirname, "templates", templatePath <> ".rs.hbs" ])

writeFile :: String -> String -> Effect Unit
writeFile = S.writeTextFile Encoding.UTF8

fmt :: Effect Unit
fmt = do
  command <- spawn "cargo" [ "fmt" ] defaultSpawnOptions
  onExit command \exit -> case exit of
    Normally 0 -> pure unit
    other -> log $ "Cargo fmt failed: " <> show other
  onData (stdout command) (Buffer.toString Encoding.UTF8 >=> log)

dieselSetup :: Effect Unit
dieselSetup = do
  command <- spawn "diesel" [ "setup" ] defaultSpawnOptions
  onExit command \exit -> case exit of
    Normally 0 -> pure unit
    other -> log $ "Diesel setup failed: " <> show other
  onData (stdout command) (Buffer.toString Encoding.UTF8 >=> log)

generateCode :: String -> Effect Unit
generateCode entity = do
  queryTemplate <- getTemplate "query"
  mutationTemplate <- getTemplate "mutation"
  schemaTemplate <- getTemplate "schema"
  modelTemplate <- getTemplate "model"
  let
    entityPlural = plural entity

    modelPlural = pascalCase $ plural entity

    modelSingular = pascalCase entity

    compile = makeCompilerWithVariables { "entity_plural": entityPlural, "model_plural": modelPlural, "model_singular": modelSingular, "mod": entity, "entity_singular": entity }

    queryContents = compile queryTemplate

    mutationContents = compile mutationTemplate

    schemaContents = compile schemaTemplate

    modelContents = compile modelTemplate
  S.mkdir $ path [ "src", entity ]
  appendTo "./src/graphql/query.rs" queryContents
  appendTo "./src/graphql/mutation.rs" mutationContents
  appendTo "./src/schema.rs" schemaContents
  writeFile (path [ "src", entity, "model.rs" ]) modelContents
  writeFile (path [ "src", entity, "mod.rs" ]) "mod model;\n\npub use model::*;"
  appendFile (path [ "src", "lib.rs" ]) $ "mod " <> entity <> ";"
  log "done"

initProject :: String -> Effect Unit
initProject projectName = do
  copyDir (path [ __dirname, "src" ]) (path [ "src" ])
  copyDir (path [ __dirname, "Cargo.toml" ]) (path [ "Cargo.toml" ])
  copyDir (path [ __dirname, "Cargo.lock" ]) (path [ "Cargo.lock" ])
  copyDir (path [ __dirname, ".env.example" ]) (path [ ".env.example" ])
  copyDir (path [ __dirname, ".editorconfig" ]) (path [ ".editorconfig" ])
  dieselSetup

getEntity :: Effect String
getEntity = do
  let
    entity = args !! 1
  if (isNothing entity) then do
    log "Entity not passed"
    exit 1
  else
    pure $ fromMaybe "" entity

data Option
  = Init
  | Generate
  | None

option :: String -> Option
option "init" = Init

option "generate" = Generate

option _ = None

generate :: Effect Unit
generate = do
  entity <- getEntity
  generateCode entity
  fmt

init :: Effect Unit
init = do
  name <- getEntity
  initProject name

main :: Effect Unit
main = do
  let
    command = fromMaybe "" $ args !! 0
  case option command of
    Init -> init
    Generate -> generate
    _ -> log "Command not found."
