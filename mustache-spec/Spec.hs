{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Yaml
import Test.Hspec
import Text.Megaparsec
import Text.Mustache
import Text.Mustache.Parser

-- | Representation of information contained in a Mustache spec file.
data SpecFile = SpecFile
  { -- | Textual overview of this spec file
    specOverview :: Text,
    -- | The actual collection of tests
    specTests :: [Test]
  }

instance FromJSON SpecFile where
  parseJSON = withObject "Mustache spec file" $ \o -> do
    specOverview <- o .: "overview"
    specTests <- o .: "tests"
    return SpecFile {..}

-- | Representation of a single test.
data Test = Test
  { testName :: String,
    testDesc :: String,
    testData :: Value,
    testTemplate :: Text,
    testExpected :: Text,
    testPartials :: Map Text Text
  }

instance FromJSON Test where
  parseJSON = withObject "Test" $ \o -> do
    testName <- o .: "name"
    testDesc <- o .: "desc"
    testData <- o .: "data"
    testTemplate <- o .: "template"
    testExpected <- o .: "expected"
    testPartials <- o .:? "partials" .!= M.empty
    return Test {..}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specData "Comments" $(embedFile "specification/comments.yml")
  specData "Delimiters" $(embedFile "specification/delimiters.yml")
  specData "Interpolation" $(embedFile "specification/interpolation.yml")
  specData "Inverted" $(embedFile "specification/inverted.yml")
  specData "Partials" $(embedFile "specification/partials.yml")
  specData "Sections" $(embedFile "specification/sections.yml")

specData :: String -> ByteString -> Spec
specData aspect bytes = describe aspect $ do
  let handleError = expectationFailure . errorBundlePretty
  case decodeEither' bytes of
    Left err ->
      it "should load YAML specs first" $
        expectationFailure (prettyPrintParseException err)
    Right SpecFile {..} ->
      forM_ specTests $ \Test {..} ->
        it (testName ++ ": " ++ testDesc) $
          case compileMustacheText (PName $ T.pack testName) testTemplate of
            Left perr -> handleError perr
            Right Template {..} -> do
              ps1 <- forM (M.keys testPartials) $ \k -> do
                let pname = PName k
                case parseMustache (T.unpack k) (testPartials ! k) of
                  Left perr -> handleError perr >> undefined
                  Right ns -> return (pname, ns)
              let ps2 = M.fromList ps1 `M.union` templateCache
              TL.toStrict (renderMustache (Template templateActual ps2) testData)
                `shouldBe` testExpected
