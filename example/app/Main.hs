{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Text
import Text.Megaparsec
import Text.Mustache
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  template <- compileMustacheDir "main" "templates"
  print template
  TIO.putStr $ renderMustache template $ object
      [ "subnets" .= ["pen" :: Text, "candle", "egg"]
      ]