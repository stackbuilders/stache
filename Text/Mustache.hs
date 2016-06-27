-- |
-- Module      :  Text.Mustache
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for Mustache templates. TODO Write normal header.

module Text.Mustache
  ( -- * Types
    Template (..)
  , Node (..)
  , Key (..)
  , MustacheException (..)
    -- * Compiling
  , compileMustacheDir
  , compileMustacheFile
  , compileMustacheText
    -- * Rendering
  , renderMustache
    -- * Re-exports from Aeson
  , object
  , (.=) )
where

import Data.Aeson (object, (.=))
import Text.Mustache.Compile
import Text.Mustache.Render
import Text.Mustache.Type

-- :set -XOverloadedStrings
-- compileMustacheDir "/home/mark/Downloads/my-staches/" (Key "foo")
