{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Yaml
import Test.Hspec
import Text.Megaparsec
import Text.Mustache
import qualified Data.Text as T

-- | Representation of information contained in a Mustache spec file.

data SpecFile = SpecFile
  { specOverview :: Text   -- ^ Textual overview of this spec file
  , specTests    :: [Test] -- ^ The actual collection of tests
  }

instance FromJSON SpecFile where
  parseJSON = withObject "Mustache spec file" $ \o -> do
    specOverview <- o .: "overview"
    specTests    <- o .: "tests"
    return SpecFile {..}

-- | Representation of a single test.

data Test = Test
  { testName     :: String
  , testDesc     :: String
  , testData     :: Value
  , testTemplate :: Text
  , testExpected :: Text
  }

instance FromJSON Test where
  parseJSON = withObject "Test" $ \o -> do
    testName     <- o .: "name"
    testDesc     <- o .: "desc"
    testData     <- o .: "data"
    testTemplate <- o .: "template"
    testExpected <- o .: "expected"
    return Test {..}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specData "Comments"      $(embedFile "specs/comments.yml")
  specData "Delimiters"    $(embedFile "specs/delimiters.yml")
  specData "Interpolation" $(embedFile "specs/interpolation.yml")
  specData "Inverted"      $(embedFile "specs/inverted.yml")
  specData "Partials"      $(embedFile "specs/partials.yml")
  specData "Sections"      $(embedFile "specs/sections.yml")

specData :: String -> ByteString -> Spec
specData aspect bytes = describe aspect $ do
  case decodeEither' bytes of
    Left err ->
      it "should load YAML specs first" $
        expectationFailure (prettyPrintParseException err)
    Right SpecFile {..} ->
      forM_ specTests $ \Test {..} ->
        it (testName ++ ": " ++ testDesc) $
          case compileMustacheText (Key $ T.pack testName) testTemplate of
            Left perr ->
              expectationFailure (parseErrorPretty perr)
            Right template ->
              renderMustache template testData `shouldBe` testExpected
