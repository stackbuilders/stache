-- |
-- Module      :  Text.Mustache.Type
-- Copyright   :  © 2016 Stack Buliders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the package. You don't usually need to import the module,
-- because "Text.Mustache" re-exports everything you may need, import that
-- module instead.

{-# LANGUAGE DeriveDataTypeable #-}

module Text.Mustache.Type
  ( Template (..)
  , Node (..)
  , Key (..)
  , MustacheException (..) )
where

import Control.Monad.Catch (Exception)
import Data.Data (Data)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.Megaparsec

-- | Mustache template with metadata.

data Template = Template
  { templateActual :: Key
  , templateCache  :: Map Key [Node]
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | Structural element of template.

data Node
  = TextBlock       Text -- ^ Plain text contained between tags
  | EscapedVar      Key
  | UnescapedVar    Key
  | Section         Key [Node]
  | InvertedSection Key [Node]
  | Partial         Key Pos
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Identifier for values to interpolate.

newtype Key = Key { unKey :: Text }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Exception that is thrown when parsing of a template has failed.

data MustacheException = MustacheException (ParseError Char Dec)
  deriving (Eq, Show, Typeable)

instance Exception MustacheException
