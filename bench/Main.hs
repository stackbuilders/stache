{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Mustache.Compile
import Text.Mustache.Parser
import Text.Mustache.Render
import Text.Mustache.Type

----------------------------------------------------------------------------
-- Benchmarks

main :: IO ()
main = defaultMain [parserBench, renderBench]

parserBench :: Benchmark
parserBench =
  bgroup
    "parser"
    [ bparser
        "simple block of text"
        "bench-data/lorem-ipsum.mustache",
      bparser
        "text with escaped var"
        "bench-data/escaped-var.mustache",
      bparser
        "text with unescaped var"
        "bench-data/unescaped-var.mustache",
      bparser
        "text with unescaped var special"
        "bench-data/unescaped-var-spec.mustache",
      bparser
        "a section"
        "bench-data/section.mustache",
      bparser
        "an inverted section"
        "bench-data/inverted-section.mustache",
      bparser
        "nested sections"
        "bench-data/nested-sections.mustache",
      bparser
        "text with partial"
        "bench-data/partial.mustache",
      bparser
        "comprehensive template"
        "bench-data/comprehensive.mustache"
    ]

renderBench :: Benchmark
renderBench =
  bgroup
    "render"
    [ brender
        "simple block of text"
        "bench-data/lorem-ipsum.mustache"
        Null,
      brender
        "text with escaped var"
        "bench-data/escaped-var.mustache"
        (object ["escaped-variable" .= ("escaped variable" :: Text)]),
      brender
        "text with unescaped var"
        "bench-data/unescaped-var.mustache"
        (object ["unescaped-variable" .= ("unescaped variable" :: Text)]),
      brender
        "text with unescaped var special"
        "bench-data/unescaped-var-spec.mustache"
        (object ["unescaped-variable" .= ("unescaped variable" :: Text)]),
      brender
        "a section"
        "bench-data/section.mustache"
        (object ["section" .= True]),
      brender
        "an inverted section"
        "bench-data/inverted-section.mustache"
        (object ["section" .= False]),
      brender
        "nested sections"
        "bench-data/nested-sections.mustache"
        ( object
            [ "section-a" .= True,
              "section-b" .= True,
              "section-c" .= True
            ]
        ),
      brender'
        "text with partial"
        "bench-data/"
        "partial"
        Null,
      brender'
        "comprehensive template"
        "bench-data/"
        "comprehensive"
        ( object
            [ "first-thing" .= ("the first thing" :: Text),
              "second-thing" .= ("the second thing" :: Text),
              "third-thing" .= ("the third thing" :: Text),
              "items" .= [1 .. 10 :: Int]
            ]
        )
    ]

bparser :: String -> FilePath -> Benchmark
bparser desc path =
  env
    (T.readFile path)
    (bench desc . nf (either (error . errorBundlePretty) id . parseMustache path))

brender :: String -> FilePath -> Value -> Benchmark
brender desc path value =
  env
    (compileMustacheFile path)
    (bench desc . nf (`renderMustache` value))

brender' :: String -> FilePath -> PName -> Value -> Benchmark
brender' desc path pname value =
  env
    (compileMustacheDir pname path)
    (bench desc . nf (`renderMustache` value))

----------------------------------------------------------------------------
-- Orphan instances

instance NFData Node

instance NFData Template
