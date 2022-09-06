{-# LANGUAGE TemplateHaskell #-}

module Initialise where

import Control.Monad (forM_)
import Data.ByteString as ByteString (ByteString, writeFile)
import Data.FileEmbed (embedDir, embedFile)
import Data.Text (Text, unpack)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude hiding (readFile, writeFile)

newtype InitArgs = InitArgs {projectName :: Text}

createAndWriteFile :: FilePath -> ByteString -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  ByteString.writeFile path content

initialise :: InitArgs -> IO ()
initialise args = do
  let name = unpack $ projectName args
  let baseProject = $(embedDir "bootstrap")
  let envFile = $(embedFile "./bootstrap/.env")
  forM_ baseProject $ uncurry createAndWriteFile
