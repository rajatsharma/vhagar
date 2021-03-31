module Admin.Main where

import Prelude
import Data.List ((!!))
import Data.Maybe (fromMaybe, isNothing)
import Effect (Effect)
import Effect.Class.Console (log)
import FS (replace)
import Inflections (pascalCase, plural)
import Node.Args (args)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync as S
import Node.Globals (__dirname)
import Node.Path as Path
import Node.ChildProcess (Exit(..), defaultSpawnOptions, onExit, spawn, stdout)
import Node.Process (exit)
import Text.Handlebars (compile)
import Node.Stream (onData)

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
  cargoFormatCommand <- spawn "cargo" [ "fmt" ] defaultSpawnOptions
  onExit cargoFormatCommand \exit -> case exit of
    Normally 0 -> pure unit
    other -> log $ "Cargo fmt failed: " <> show other
  onData (stdout cargoFormatCommand) (Buffer.toString Encoding.UTF8 >=> log)

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

getEntity :: Effect String
getEntity = do
  let
    entity = args !! 0
  if (isNothing entity) then do
    log "Entity not passed"
    exit 1
  else
    pure $ fromMaybe "" entity

main :: Effect Unit
main = do
  entity <- getEntity
  generateCode entity
  fmt
