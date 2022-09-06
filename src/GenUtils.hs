{-# LANGUAGE OverloadedStrings #-}

module GenUtils where

import Data.Text (Text, replace)
import Soothsayer ((<~*))

(~>) :: Text -> Text -> Text -> Text
(~>) = replace

comma :: Text -> Text -> Text
comma a b = "{0}, {1}" <~* [a, b]

line :: Text -> Text -> Text
line a b = "{0} \n {1}" <~* [a, b]

(++>) :: Text -> Text -> Text -> Text
(++>) marker content = marker ~> (content `comma` marker)

(//>) :: Text -> Text -> Text -> Text
(//>) marker content = marker ~> (content `line` marker)
