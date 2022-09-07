{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isSuffixOf, pack, splitOn, stripSuffix, toTitle, unpack)
import GHC.IO (unsafePerformIO)
import Initialise (InitArgs (..), initialise)
import ModelGen (Column (..), ModelArgs (ModelArgs, modelNamePlural), model)
import Options.Applicative
  ( Alternative (many),
    Applicative (pure, (<*>)),
    Parser,
    command,
    execParser,
    fullDesc,
    header,
    helper,
    idm,
    info,
    metavar,
    progDesc,
    strArgument,
    subparser,
    (<$>),
    (<**>),
  )
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.Process (callCommand, readProcess, runCommand)
import Text.Inflections (toCamelCased)
import Prelude hiding (readFile, writeFile)

data Ceraxes
  = Init String
  | Model String String [String]

logExit :: Text -> IO a
logExit msg = do
  putStrLn $ unpack msg
  exitFailure

handleOptional :: Text -> (Bool, Text)
handleOptional typeId
  | "!" `isSuffixOf` typeId = (True, fromMaybe "" $ stripSuffix "!" typeId)
  | otherwise = (False, typeId)

getRustType :: Text -> Text
getRustType "string" = "String"
getRustType "integer" = "i32"
getRustType _ = "String"

processField :: Text -> Column
processField columnStr = unsafePerformIO $ do
  case splitOn ":" columnStr of
    [name, typename] -> do
      let (isRequired, type') = handleOptional typename
      let rustType = getRustType typename
      pure Column {columnName = name, columnType = type', columnRequired = isRequired, columnNamePascal = fromRight "" $ toCamelCased True name, columnTypeRust = rustType}
    [name] -> logExit $ "No Type supplied for:" <> name
    _ -> logExit $ "Illegal pattern:" <> columnStr

createInit :: Parser Ceraxes
createInit = Init <$> strArgument (metavar "PROJECT_NAME")

createModel :: Parser Ceraxes
createModel =
  Model
    <$> strArgument (metavar "MODEL_NAME")
    <*> strArgument (metavar "MODEL_NAME PLURAL")
    <*> many (strArgument (metavar "FIELD..."))

ceraxesParser :: Parser Ceraxes
ceraxesParser =
  subparser $
    command "init" (info createInit $ progDesc "Initialise project")
      <> command "gen-model" (info createModel $ progDesc "Generate graphql model and db entity")

runner :: Ceraxes -> IO ()
runner (Init name) = initialise $ InitArgs $ pack name
runner (Model modelName modelNamePlural modelFields) = do
  if null modelFields then logExit "No fields supplied" else pure ()
  let fields = processField . pack <$> modelFields
  let modelNameText = pack modelName
  let modelNamePluralText = pack modelNamePlural
  let modelNamePascal = fromRight "" $ toCamelCased True modelNameText
  let modelNamePascalPlural = fromRight "" $ toCamelCased True $ pack modelNamePlural
  model $ ModelArgs modelNameText modelNamePluralText modelNamePascal modelNamePascalPlural fields

shell :: IO ()
shell = runner =<< execParser opts
  where
    opts =
      info (ceraxesParser <**> helper) $
        fullDesc
          <> progDesc "Enter Command to run, see available commands for command descriptions."
          <> header "Ceraxes"

main :: IO ()
main = shell
