module Main where

import Prelude

import CommonUtils.Handlebars (compile)
import CommonUtils.Inflections (pascalCase, plural)
import CommonUtils.Node.FileOps (copyDir, mkPath, readTextFileSync, replace, writeTextFileSync)
import CommonUtils.Node.Process (argsList)
import Data.List (List(..), (:), toUnfoldable)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), defaultSpawnOptions, onExit, spawn, stdout)
import Node.Encoding as Encoding
import Node.FS.Sync as S
import Node.Globals (__dirname)
import Node.Process (exit)
import Node.Stream (onData)

type Args = { option:: Option, secondary:: String, tertiary:: Array String }

appendPointer :: String
appendPointer = "//Replace me"

logExit :: forall a. String -> Effect a
logExit logStr = do
  log logStr
  exit 1

appendTo :: String -> String -> Effect Unit
appendTo file contents = replace { files: file, from: appendPointer, to: contents <> "\n" <> appendPointer }

makeCompilerWithVariables :: forall a. a -> String -> String
makeCompilerWithVariables variables template = compile template variables

appendFile :: String -> String -> Effect Unit
appendFile = S.appendTextFile Encoding.UTF8

getTemplate :: String -> Effect String
getTemplate templatePath = readTextFileSync (mkPath [ __dirname, "templates", templatePath <> ".rs.hbs" ])

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

model :: Args -> Effect Unit
model args = do
  let entity = args.secondary
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
  S.mkdir $ mkPath [ "src", entity ]
  appendTo "./src/graphql/query.rs" queryContents
  appendTo "./src/graphql/mutation.rs" mutationContents
  appendTo "./src/schema.rs" schemaContents
  writeTextFileSync (mkPath [ "src", entity, "model.rs" ]) modelContents
  writeTextFileSync (mkPath [ "src", entity, "mod.rs" ]) "mod model;\n\npub use model::*;"
  appendFile (mkPath [ "src", "lib.rs" ]) $ "mod " <> entity <> ";"
  log "done"

init :: Args -> Effect Unit
init args = do
  let name = args.secondary
  copyDir (mkPath [ __dirname, "src" ]) (mkPath [ "src" ])
  copyDir (mkPath [ __dirname, "Cargo.toml" ]) (mkPath [ "Cargo.toml" ])
  copyDir (mkPath [ __dirname, "Cargo.lock" ]) (mkPath [ "Cargo.lock" ])
  copyDir (mkPath [ __dirname, ".env.example" ]) (mkPath [ ".env.example" ])
  copyDir (mkPath [ __dirname, ".editorconfig" ]) (mkPath [ ".editorconfig" ])
  dieselSetup

data Option
  = Init
  | Model

instance showOption :: Show Option where
  show Init = "init"
  show Model = "model"

argsInspector :: List String -> Effect Args
argsInspector ("gen-model" : Nil) = do
  logExit "Command gen-model requires one more argument model-name and optional list of columns followed by it"

argsInspector ("gen-model" : modelname : Nil) = do
  log "No columns supplied model will be created with only one column id:Int"
  pure { option: Model, secondary: modelname, tertiary: [] }

argsInspector ("gen-model" : modelname : xs) = pure { option: Model, secondary: modelname, tertiary: (toUnfoldable xs) }
argsInspector ("init" : projectName: xs) = pure { option: Init, secondary: projectName, tertiary: [] }
argsInspector null = logExit "Required arg, nothing supplied"

main :: Effect Unit
main = do
  args <- argsList
  verifiedArgs <- argsInspector args
  case verifiedArgs.option of
    Model -> model verifiedArgs
    Init -> init verifiedArgs
