{-# LANGUAGE OverloadedStrings #-}

module Markers where

import Data.Text (Text)

queryMarker :: Text
queryMarker = "/*Ceraxes Query Marker*/"

queryModMarker :: Text
queryModMarker = "/*Ceraxes Query Mod Marker*/"

mutationMarker :: Text
mutationMarker = "/*Ceraxes Mutation Marker*/"

mutationModMarker :: Text
mutationModMarker = "/*Ceraxes Query Mod Marker*/"

migrationMarker :: Text
migrationMarker = "/*Ceraxed Migrator Marker*/"

migrationModMarker :: Text
migrationModMarker = "/*Ceraxed Migrator Mod Marker*/"
