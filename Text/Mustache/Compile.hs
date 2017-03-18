-- |
-- Module      :  Text.Mustache.Compile
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mustache 'Template' creation from file or a 'Text' value. You don't
-- usually need to import the module, because "Text.Mustache" re-exports
-- everything you may need, import that module instead.

{-# LANGUAGE CPP #-}

module Text.Mustache.Compile
  ( compileMustacheDir
  , getMustacheFilesInDir
  , compileMustacheFile
  , compileMustacheText )
where

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except
import Data.Text.Lazy (Text)
import System.Directory
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type
import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TL
import qualified System.FilePath   as F

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- | Compile all templates in specified directory and select one. Template
-- files should have extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized. This function /does not/ scan the directory recursively.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.

compileMustacheDir :: (MonadIO m, MonadThrow m)
  => PName             -- ^ Which template to select after compiling
  -> FilePath          -- ^ Directory with templates
  -> m Template        -- ^ The resulting template
compileMustacheDir pname path =
  getMustacheFilesInDir path >>=
  liftM selectKey . foldM f (Template undefined M.empty)
  where
    selectKey t = t { templateActual = pname }
    f (Template _ old) fp = do
      Template _ new <- compileMustacheFile fp
      return (Template undefined (M.union new old))

-- | Return a list of templates found in given directory. The returned paths
-- are absolute.
--
-- @since 0.2.2

getMustacheFilesInDir :: MonadIO m
  => FilePath          -- ^ Directory with templates
  -> m [FilePath]
getMustacheFilesInDir path =
  liftIO (getDirectoryContents path) >>=
  filterM isMustacheFile . fmap (F.combine path) >>=
  mapM (liftIO . makeAbsolute)

-- | Compile single Mustache template and select it.
--
-- The action can throw the same exceptions as 'T.readFile'.

compileMustacheFile :: (MonadIO m, MonadThrow m)
  => FilePath          -- ^ Location of the file
  -> m Template
compileMustacheFile path =
  liftIO (TL.readFile path) >>= withException . compile
  where
    pname = pathToPName path
    compile = fmap (Template pname . M.singleton pname) . parseMustache path

-- | Compile Mustache template from a lazy 'Text' value. The cache will
-- contain only this template named according to given 'PName'.

compileMustacheText
  :: PName             -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Either (ParseError Char Dec) Template -- ^ The result
compileMustacheText pname txt =
  Template pname . M.singleton pname <$> parseMustache "" txt

----------------------------------------------------------------------------
-- Helpers

-- | Check if given 'FilePath' points to a mustache file.

isMustacheFile :: MonadIO m => FilePath -> m Bool
isMustacheFile path = do
  exists <- liftIO (doesFileExist path)
  let rightExtension = F.takeExtension path == ".mustache"
  return (exists && rightExtension)

-- | Build a 'PName' from given 'FilePath'.

pathToPName :: FilePath -> PName
pathToPName = PName . T.pack . F.takeBaseName

-- | Throw 'MustacheException' if argument is 'Left' or return the result
-- inside 'Right'.

withException :: MonadThrow m
  => Either (ParseError Char Dec) Template -- ^ Value to process
  -> m Template        -- ^ The result
withException = either (throwM . MustacheParserException) return
