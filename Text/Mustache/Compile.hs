-- |
-- Module      :  Text.Mustache.Compile
-- Copyright   :  © 2016–2019 Stack Builders
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
  , compileMustacheDir'
  , getMustacheFilesInDir
  , getMustacheFilesInDir'
  , isMustacheFile
  , compileMustacheFile
  , compileMustacheText )
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

-- | Compile all templates in specified directory and select one. Template
-- files should have the extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized. This function /does not/ scan the directory recursively.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.
--
-- > compileMustacheDir = complieMustacheDir' isMustacheFile

compileMustacheDir :: MonadIO m
  => PName             -- ^ Which template to select after compiling
  -> FilePath          -- ^ Directory with templates
  -> m Template        -- ^ The resulting template
compileMustacheDir = compileMustacheDir' isMustacheFile

-- | The same as 'compileMustacheDir', but allows using a custom predicate
-- for template selection.
--
-- @since 1.2.0

compileMustacheDir' :: MonadIO m
  => (FilePath -> Bool) -- ^ Template selection predicate
  -> PName             -- ^ Which template to select after compiling
  -> FilePath          -- ^ Directory with templates
  -> m Template        -- ^ The resulting template
compileMustacheDir' predicate pname path =
  getMustacheFilesInDir' predicate path >>=
  fmap selectKey . foldM f (Template undefined M.empty)
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
getMustacheFilesInDir = getMustacheFilesInDir' isMustacheFile

-- | Return a list of templates found via a predicate in given directory.
-- The returned paths are absolute.
--
-- @since 1.2.0

getMustacheFilesInDir' :: MonadIO m
  => (FilePath -> Bool) -- ^ Mustache file selection predicate
  -> FilePath          -- ^ Directory with templates
  -> m [FilePath]
getMustacheFilesInDir' predicate path = liftIO $
  getDirectoryContents path >>=
  filterM f . fmap (F.combine path) >>=
  mapM makeAbsolute
  where
    f p = (&& predicate p) <$> doesFileExist p

-- | The default Mustache file predicate.
--
-- @since 1.2.0

isMustacheFile :: FilePath -> Bool
isMustacheFile path = F.takeExtension path == ".mustache"

-- | Compile a single Mustache template and select it.
--
-- The action can throw the same exceptions as 'T.readFile'.

compileMustacheFile :: MonadIO m
  => FilePath          -- ^ Location of the file
  -> m Template
compileMustacheFile path = liftIO $ do
  input <- T.readFile path
  withException (compile input)
  where
    pname = pathToPName path
    compile = fmap (Template pname . M.singleton pname) . parseMustache path

-- | Compile Mustache template from a lazy 'Text' value. The cache will
-- contain only this template named according to given 'PName'.

compileMustacheText
  :: PName             -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Either (ParseErrorBundle Text Void) Template -- ^ The result
compileMustacheText pname txt =
  Template pname . M.singleton pname <$> parseMustache "" txt

----------------------------------------------------------------------------
-- Helpers

-- | Build a 'PName' from given 'FilePath'.

pathToPName :: FilePath -> PName
pathToPName = PName . T.pack . F.takeBaseName

-- | Throw 'MustacheException' if argument is 'Left' or return the result
-- inside 'Right'.

withException
  :: Either (ParseErrorBundle Text Void) Template -- ^ Value to process
  -> IO Template       -- ^ The result
withException = either (throwIO . MustacheParserException) return
