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
  , renderMustache )
where

import Text.Mustache.Compile
import Text.Mustache.Render
import Text.Mustache.Type
