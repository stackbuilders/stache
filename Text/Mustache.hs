-- |
-- Module      :  Text.Mustache
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a Haskell implementation of Mustache templates. The
-- implementation conforms to the version 1.1.3 of official Mustache
-- specification <https://github.com/mustache/spec>. It is extremely simple
-- and straightforward to use with minimal but complete API—three functions
-- to compile templates (from directory, from file, and from lazy text) and
-- one to render them.
--
-- The implementation uses the Megaparsec parsing library to parse the
-- templates which results in superior quality of error messages.
--
-- For rendering you only need to create Aeson's 'Data.Aeson.Value' where
-- you put the data to interpolate. Since the library re-uses Aeson's
-- instances and most data types in the Haskell ecosystem are instances of
-- classes like 'Data.Aeson.ToJSON', the whole process is very simple for
-- the end user.
--
-- Template Haskell helpers for compilation of templates at compile time are
-- available in the "Text.Mustache.Compile.TH" module. The helpers currently
-- work only with GHC 8 and later.
--
-- One feature that is not currently supported is lambdas. The feature is
-- marked as optional in the spec and can be emulated via processing of
-- parsed template representation. The decision to drop lambdas is
-- intentional, for the sake of simplicity and better integration with
-- Aeson.
--
-- Here is an example of basic usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Data.Aeson
-- > import Data.Text
-- > import Text.Megaparsec
-- > import Text.Mustache
-- > import qualified Data.Text.Lazy.IO as TIO
-- >
-- > main :: IO ()
-- > main = do
-- >   let res = compileMustacheText "foo"
-- >         "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n"
-- >   case res of
-- >     Left err -> putStrLn (parseErrorPretty err)
-- >     Right template -> TIO.putStr $ renderMustache template $ object
-- >       [ "name"   .= ("John" :: Text)
-- >       , "things" .= ["pen" :: Text, "candle", "egg"]
-- >       ]
--
-- If I run the program, it prints the following:
--
-- > Hi, John! You have:
-- >   * pen
-- >   * candle
-- >   * egg
--
-- For more information about Mustache templates the following links may be
-- helpful:
--
--     * The official Mustache site: <https://mustache.github.io/>
--     * The manual: <https://mustache.github.io/mustache.5.html>
--     * The specification: <https://github.com/mustache/spec>
--     * Stack Builders Stache tutorial: <https://www.stackbuilders.com/tutorials/haskell/mustache-templates/>

module Text.Mustache
  ( -- * Types
    Template (..)
  , Node (..)
  , Key (..)
  , PName (..)
  , MustacheException (..)
    -- * Compiling
  , compileMustacheDir
  , compileMustacheFile
  , compileMustacheText
    -- * Rendering
  , renderMustache )
where

import Text.Mustache.Compile
import Text.Mustache.Render
import Text.Mustache.Type
