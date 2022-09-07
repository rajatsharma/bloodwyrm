{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ModelGen where

import Data.Text (Text, pack, unpack)
import Data.Text.IO (readFile, writeFile)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GenUtils ((++>), (//>))
import LoadEnv (loadEnv)
import Markers (migrationMarker, migrationModMarker, mutationMarker, mutationModMarker, queryMarker, queryModMarker)
import Soothsayer ((***), (<~*))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import Text.Mustache (Template, ToMustache (toMustache), object, substitute, (~>))
import Text.Mustache.Compile (embedSingleTemplate)
import Prelude hiding (readFile, writeFile)

data ModelArgs = ModelArgs {modelName :: Text, modelNamePlural :: Text, modelNamePascal :: Text, modelNamePascalPlural :: Text, columns :: [Column]}

instance ToMustache ModelArgs where
  toMustache modelArgs =
    object
      [ "entityName" ~> modelName modelArgs,
        "entityNamePascal" ~> modelNamePascal modelArgs,
        "columns" ~> columns modelArgs,
        "entityNamePascalPlural" ~> modelNamePascalPlural modelArgs
      ]

data Column = Column {columnName :: Text, columnType :: Text, columnRequired :: Bool, columnNamePascal :: Text}

instance ToMustache Column where
  toMustache column =
    object
      [ "columnName" ~> columnName column,
        "columnType" ~> columnType column,
        "columnNamePascal" ~> columnNamePascal column,
        "columnRequired" ~> columnRequired column
      ]

writeFileFromTemplate :: ToMustache p => Template -> p -> FilePath -> IO ()
writeFileFromTemplate template substitutions path = do
  writeFile path fileContents
  where
    fileContents = substitute template substitutions

applySubstitutions :: (Text -> Text) -> FilePath -> IO ()
applySubstitutions substitutor substitutee = do
  fileContents <- readFile substitutee
  writeFile substitutee $ substitutor fileContents

applyQuerySubstututions :: Text -> Text -> Text -> Text
applyQuerySubstututions modelName' modelNamePascal' = (queryMarker ++> ("{0}::{1}Query" <~* [modelName', modelNamePascal'])) . (queryModMarker //> ("pub mod {0};" <~* [modelName']))

applyMutationSubstututions :: Text -> Text -> Text -> Text
applyMutationSubstututions modelName' modelNamePascal' = (mutationMarker ++> ("{0}::{1}Mutation" <~* [modelName', modelNamePascal'])) . (mutationModMarker //> ("pub mod {0};" <~* [modelName']))

applyMigrationSubstututions :: Text -> Text -> Text
applyMigrationSubstututions migrationFile = (migrationMarker ++> ("Box::new({0}::Migration)" <~* [migrationFile])) . (migrationModMarker //> ("mod {0};" <~* [migrationFile]))

model :: ModelArgs -> IO ()
model args = do
  let modelName' = modelName args
  let modelNamePascal' = modelNamePascal args
  currentDirectory <- getCurrentDirectory
  now <- getCurrentTime
  let graphqlDir = currentDirectory </> "src/graphql"
  let migratorDir = currentDirectory </> "migration/src"
  let migrationName = "m%Y%m%d_%H%M%S_create_{0}" *** [unpack modelName']
  let migrationFileName = formatTime defaultTimeLocale migrationName now
  let applyQuerySubstututions' = applyQuerySubstututions modelName' modelNamePascal'
  let applyMutationSubstututions' = applyMutationSubstututions modelName' modelNamePascal'
  let applyMigrationSubstututions' = applyMigrationSubstututions $ pack migrationFileName

  writeFileFromTemplate $(embedSingleTemplate "templates/query.rs.mustache") args $ graphqlDir </> "query" </> unpack modelName' ++ ".rs"

  writeFileFromTemplate $(embedSingleTemplate "templates/mutation.rs.mustache") args $ graphqlDir </> "mutation" </> unpack modelName' ++ ".rs"

  writeFileFromTemplate $(embedSingleTemplate "templates/migration.rs.mustache") args $ migratorDir </> migrationFileName ++ ".rs"

  applySubstitutions applyQuerySubstututions' $ graphqlDir </> "query/mod.rs"
  applySubstitutions applyMutationSubstututions' $ graphqlDir </> "mutation/mod.rs"
  applySubstitutions applyMigrationSubstututions' $ migratorDir </> "lib.rs"

  callCommand "cargo fmt"
  loadEnv
  callCommand "sea-orm-cli migrate up"
  callCommand "sea-orm-cli generate entity -o src/entity"
