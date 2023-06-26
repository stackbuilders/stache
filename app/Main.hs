{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Data.Text.Lazy.IO qualified as T
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import Development.GitRev
import Options.Applicative
import Paths_stache (version)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Mustache

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  template <-
    sconcat
      <$> mapM (compileMustacheDir optTarget) optTemplateDirs
  context <-
    foldl' mergeContexts emptyContext
      <$> mapM loadContext optContextFiles
  let rendered = renderMustache template context
  case optOutputFile of
    Nothing -> T.putStrLn rendered
    Just ofile -> T.writeFile ofile rendered

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
data Opts = Opts
  { -- | Context files.
    optContextFiles :: [FilePath],
    -- | Where to save the result.
    optOutputFile :: Maybe FilePath,
    -- | Name of the template to render.
    optTarget :: PName,
    -- | Directories with templates.
    optTemplateDirs :: NonEmpty FilePath
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "Command line interface to the Stache template processor",
      header "stache—a simple implementation of Mustache templates"
    ]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      unwords
        [ "stache",
          showVersion version,
          $gitBranch,
          $gitHash
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> many
      ( (strOption . mconcat)
          [ long "context",
            short 'c',
            metavar "CONTEXT",
            help "Context file in YAML or JSON format"
          ]
      )
    <*> (optional . strOption . mconcat)
      [ long "ofile",
        short 'o',
        metavar "OFILE",
        help "Save the rendered document to this file (otherwise write to stdout)"
      ]
    <*> (argument str . mconcat)
      [ metavar "TARGET",
        help "Name of the template to render"
      ]
    <*> (fmap NE.fromList . some)
      ( (argument str . mconcat)
          [ metavar "DIR",
            help "Template directories"
          ]
      )

----------------------------------------------------------------------------
-- Helpers

-- | File context from a YAML or JSON file.
loadContext :: FilePath -> IO Value
loadContext file = do
  let readYaml =
        either
          (Left . Yaml.prettyPrintParseException)
          Right
          <$> Yaml.decodeFileEither file
  econtext <- case takeExtension file of
    ".yml" -> readYaml
    ".yaml" -> readYaml
    _ -> Aeson.eitherDecodeFileStrict file
  case econtext of
    Left err -> spitErrorAndDie err
    Right v@(Aeson.Object _) -> return v
    Right _ -> spitErrorAndDie "context file should contain an object"

mergeContexts :: Value -> Value -> Value
mergeContexts (Aeson.Object m0) (Aeson.Object m1) =
  Aeson.Object (Aeson.KeyMap.union m0 m1)
mergeContexts _ _ = error "context merge failed"

emptyContext :: Value
emptyContext = Aeson.object []

spitErrorAndDie :: String -> IO a
spitErrorAndDie err = do
  hPutStrLn stderr err
  exitFailure
