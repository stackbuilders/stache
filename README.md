# Stache

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/stache.svg?style=flat)](https://hackage.haskell.org/package/stache)
[![Stackage Nightly](http://stackage.org/package/stache/badge/nightly)](http://stackage.org/nightly/package/stache)
[![Stackage LTS](http://stackage.org/package/stache/badge/lts)](http://stackage.org/lts/package/stache)
[![Build Status](https://travis-ci.org/stackbuilders/stache.svg?branch=master)](https://travis-ci.org/stackbuilders/stache)
[![Coverage Status](https://coveralls.io/repos/github/stackbuilders/stache/badge.svg?branch=master)](https://coveralls.io/github/stackbuilders/stache?branch=master)

This is a Haskell implementation of Mustache templates. The implementation
conforms to the version 1.1.3 of official [Mustache specification]
(https://github.com/mustache/spec). It is extremely simple and
straightforward to use with minimal but complete API — three functions to
compile templates (from directory, from file, and from lazy text) and one to
render them.

The implementation uses the Megaparsec parsing library to parse the
templates which is results in superior quality of error messages.

For rendering you only need to create Aeson's `Value` where you put the data
to interpolate. Since the library re-uses Aeson's instances and most data
types in Haskell ecosystem are instances of classes like
`Data.Aeson.ToJSON`, the whole process is very simple for end user.

Template Haskell helpers for compilation of templates at compile time are
available in the `Text.Mustache.Compile.TH` module. The helpers are
currently available only for GHC 8 users though.

One feature that is not currently supported is lambdas. The feature is
marked as optional in the spec and can be emulated via processing of parsed
template representation. The decision to drop lambdas is intentional, for
the sake of simplicity and better integration with Aeson.

## Quick start

Here is an example of basic usage:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Text
import Text.Megaparsec
import Text.Mustache
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
  let res = compileMustacheText "foo"
        "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n"
  case res of
    Left err -> putStrLn (parseErrorPretty err)
    Right template -> TIO.putStr $ renderMustache template $ object
      [ "name"   .= ("John" :: Text)
      , "things" .= ["pen" :: Text, "candle", "egg"]
      ]
```

If I run the program, it prints the following:

```
Hi, John! You have:
  * pen
  * candle
  * egg
```

For more information about Mustache templates the following links may be
helpful:

* The official Mustache site: https://mustache.github.io/
* The manual: https://mustache.github.io/mustache.5.html
* The specification: https://github.com/mustache/spec

## License

Copyright © 2016 Stack Builders

Distributed under BSD 3 clause license.
