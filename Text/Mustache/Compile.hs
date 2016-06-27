-- |
-- Module      :  Text.Mustache.Compile
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mustache 'Template' creation from file or a 'Text' value.

module Text.Mustache.Compile
  ( compileMustacheDir
  , compileMustacheFile
  , compileMustacheText )
where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except
import Data.Text (Text)
import System.Directory
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type
import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified System.FilePath as F

-- | Compile all templates in specified directory and select one. Template
-- files should have extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.

compileMustacheDir :: (MonadIO m, MonadThrow m)
  => FilePath          -- ^ Directory with templates
  -> Key               -- ^ Which template to select
  -> m Template        -- ^ The resulting template
compileMustacheDir path key =
  liftIO (getDirectoryContents path) >>=
  filterM isMustacheFile . fmap (F.combine (F.takeDirectory path)) >>=
  fmap selectKey . foldM f (Template undefined M.empty)
  where
    selectKey t = t { templateActual = key }
    f (Template _ old) fp = do
      Template _ new <- compileMustacheFile fp
      return (Template undefined (M.union new old))

-- | Compile single Mustache template and select it.
--
-- The action can throw the same exceptions as 'T.readFile'.

compileMustacheFile :: (MonadIO m, MonadThrow m)
  => FilePath          -- ^ Location of the file
  -> m Template
compileMustacheFile path = liftIO (T.readFile path) >>=
  withException . compileMustacheText (pathToKey path)

-- | Compile Mustache template from 'Text' value. The cache will contain
-- only this template named according to given 'Key'.

compileMustacheText
  :: Key               -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Either (ParseError Char Dec) Template -- ^ The result
compileMustacheText key txt =
  Template key . M.singleton key <$> parseMustache "" txt

----------------------------------------------------------------------------
-- Helpers

-- | Check if given 'FilePath' point to a mustache file.

isMustacheFile :: MonadIO m => FilePath -> m Bool
isMustacheFile path = do
  liftIO (putStrLn path)
  exists <- liftIO (doesFileExist path)
  let rightExtension = F.takeExtension path == ".mustache"
  liftIO (print (exists && rightExtension))
  return (exists && rightExtension)

-- | Build a 'Key' from given 'FilePath'.

pathToKey :: FilePath -> Key
pathToKey = Key . T.pack . F.takeBaseName
{-# INLINE pathToKey #-}

-- | Throw 'MustacheException' if argument is 'Left' or return the result
-- inside 'Right'.

withException :: MonadThrow m
  => Either (ParseError Char Dec) Template -- ^ Value to process
  -> m Template        -- ^ The result
withException = either (throwM . MustacheException) return
{-# INLINE withException #-}
