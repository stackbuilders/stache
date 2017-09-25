-- |
-- Module      :  Text.Mustache.Compile
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mustache 'Template' creation from file or a 'Text' value. You don't
-- usually need to import the module, because "Text.Mustache" re-exports
-- everything you may need, import that module instead.

{-# LANGUAGE CPP #-}

module Text.Mustache.Compile
  ( compileMustacheDir
  , compileMustacheDirCustom
  , getMustacheFilesInDir
  , getTemplateFilesInDir
  , compileMustacheFile
  , compileMustacheText
  , isMustacheFile )
where

import Control.Exception
import Control.Monad.Except
import Data.Text (Text)
import Data.Void
import System.Directory
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type
import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified System.FilePath as F

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Compile all templates in specified directory and select one. Template
-- files should have the extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized. This function /does not/ scan the directory recursively.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.

compileMustacheDir :: MonadIO m
  => PName              -- ^ Which template to select after compiling
  -> FilePath           -- ^ Directory with templates
  -> m Template         -- ^ The resulting template
compileMustacheDir pname path = compileMustacheDirCustom isMustacheFile pname path

-- | Compile all templates in specified directory and select one. Template
-- files are identified via the passed predicate, the predicate operates
-- on the relative filepath. This function /does not/ scan the
-- directory recursively.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.
--
-- @since 1.1.3

compileMustacheDirCustom :: MonadIO m
  => (FilePath -> Bool) -- ^ Template selection predicate
  -> PName              -- ^ Which template to select after compiling
  -> FilePath           -- ^ Directory with templates
  -> m Template         -- ^ The resulting template
compileMustacheDirCustom predicate pname path =
  getTemplateFilesInDir predicate path >>=
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
  => FilePath           -- ^ Directory with templates
  -> m [FilePath]
getMustacheFilesInDir = getTemplateFilesInDir isMustacheFile

-- | Compile a single Mustache template and select it.
--
-- The action can throw the same exceptions as 'T.readFile'.

compileMustacheFile :: MonadIO m
  => FilePath           -- ^ Location of the file
  -> m Template
compileMustacheFile path = liftIO $ do
  input <- T.readFile path
  withException input (compile input)
  where
    pname = pathToPName path
    compile = fmap (Template pname . M.singleton pname) . parseMustache path

-- | Compile Mustache template from a lazy 'Text' value. The cache will
-- contain only this template named according to given 'PName'.

compileMustacheText
  :: PName             -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Either (ParseError Char Void) Template -- ^ The result
compileMustacheText pname txt =
  Template pname . M.singleton pname <$> parseMustache "" txt

-- | Default Mustache file predicate.

isMustacheFile :: FilePath -> Bool
isMustacheFile path = F.takeExtension path == ".mustache"

----------------------------------------------------------------------------
-- Helpers

-- | Return a list of templates found via a predicate in given directory.
-- The returned paths are absolute.

getTemplateFilesInDir :: MonadIO m
  => (FilePath -> Bool) -- ^ Mustache file selection predicate
  -> FilePath           -- ^ Directory with templates
  -> m [FilePath]
getTemplateFilesInDir predicate path = liftIO $
  getDirectoryContents path >>=
  filterM (templateFilter predicate) . fmap (F.combine path) >>=
  mapM makeAbsolute

-- | Check if a given 'FilePath' points to a template file
-- useing a predicate.

templateFilter :: (FilePath -> Bool) -> FilePath -> IO Bool
templateFilter predicate path = do
  exists <- doesFileExist path
  return (exists && predicate path)

-- | Build a 'PName' from given 'FilePath'.

pathToPName :: FilePath -> PName
pathToPName = PName . T.pack . F.takeBaseName

-- | Throw 'MustacheException' if argument is 'Left' or return the result
-- inside 'Right'.

withException
  :: Text               -- ^ Original input
  -> Either (ParseError Char Void) Template -- ^ Value to process
  -> IO Template        -- ^ The result
withException input = either (throwIO . MustacheParserException input) return
