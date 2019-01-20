{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.Text (Text)
import Text.Megaparsec
import Text.Mustache.Compile
import Text.Mustache.Parser
import Text.Mustache.Render
import Text.Mustache.Type
import qualified Data.Text.IO as T

----------------------------------------------------------------------------
-- Benchmarks

main :: IO ()
main = defaultMain [ parserBench, parserWithLocBench, renderBench ]

pBench :: String -> (String -> FilePath -> Benchmark) -> Benchmark
pBench title parseF = bgroup title
    [ parseF "simple block of text"
        "bench-data/lorem-ipsum.mustache"
    , parseF "text with escaped var"
        "bench-data/escaped-var.mustache"
    , parseF "text with unescaped var"
        "bench-data/unescaped-var.mustache"
    , parseF "text with unescaped var special"
        "bench-data/unescaped-var-spec.mustache"
    , parseF "a section"
        "bench-data/section.mustache"
    , parseF "an inverted section"
        "bench-data/inverted-section.mustache"
    , parseF "nested sections"
        "bench-data/nested-sections.mustache"
    , parseF "text with partial"
        "bench-data/partial.mustache"
    , parseF "comprehensive template"
        "bench-data/comprehensive.mustache"
    ]

parserBench :: Benchmark
parserBench = pBench "parser" bparser

parserWithLocBench :: Benchmark
parserWithLocBench = pBench "parserWithLocs" bparserWithLocs

renderBench :: Benchmark
renderBench = bgroup "render"
  [ brender "simple block of text"
      "bench-data/lorem-ipsum.mustache"
      Null
  , brender "text with escaped var"
      "bench-data/escaped-var.mustache"
      (object ["escaped-variable" .= ("escaped variable" :: Text)])
  , brender "text with unescaped var"
      "bench-data/unescaped-var.mustache"
      (object ["unescaped-variable" .= ("unescaped variable" :: Text)])
  , brender "text with unescaped var special"
      "bench-data/unescaped-var-spec.mustache"
      (object ["unescaped-variable" .= ("unescaped variable" :: Text)])
  , brender "a section"
      "bench-data/section.mustache"
      (object ["section" .= True])
  , brender "an inverted section"
      "bench-data/inverted-section.mustache"
      (object ["section" .= False])
  , brender "nested sections"
      "bench-data/nested-sections.mustache"
      (object [ "section-a" .= True
              , "section-b" .= True
              , "section-c" .= True ])
  , brender' "text with partial"
      "bench-data/" "partial"
      Null
  , brender' "comprehensive template"
      "bench-data/" "comprehensive"
      (object [ "first-thing"  .= ("the first thing" :: Text)
              , "second-thing" .= ("the second thing" :: Text)
              , "third-thing"  .= ("the third thing" :: Text)
              , "items" .= [1..10 :: Int] ])
  ]

bparser :: String -> FilePath -> Benchmark
bparser desc path = env (T.readFile path)
  (bench desc . nf (either (error . errorBundlePretty) id . parseMustache path))

bparserWithLocs :: String -> FilePath -> Benchmark
bparserWithLocs desc path = env (T.readFile path)
  (bench desc . nf (either (error . errorBundlePretty) id . parseMustacheWithPositions path))

brender :: String -> FilePath -> Value -> Benchmark
brender desc path value = env (compileMustacheFile path)
  (bench desc . nf (`renderMustache` value))

brender' :: String -> FilePath -> PName -> Value -> Benchmark
brender' desc path pname value = env (compileMustacheDir pname path)
  (bench desc . nf (`renderMustache` value))

----------------------------------------------------------------------------
-- Orphan instances

#if !MIN_VERSION_megaparsec(5,0,1)
instance NFData Pos where
  rnf = rnf . unPos
#endif

instance NFData Node

instance NFData Template
