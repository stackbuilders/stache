{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.TypeSpec
  ( main
  , spec )
where

import Data.Semigroup ((<>))
import Test.Hspec
import Text.Mustache.Type
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Template instances" $
    context "the Semigroup instance" $ do
      it "the resulting template inherits focus of the left one" $
        templateActual (templateA <> templateB)
          `shouldBe` templateActual templateA
      it "the resulting template merges caches with left bias" $
        templateCache (templateA <> templateB)
          `shouldBe` M.fromList
            [ ("c", [TextBlock "foo"])
            , ("d", [TextBlock "bar"])
            , ("e", [TextBlock "baz"]) ]

templateA :: Template
templateA = Template "a" $ M.fromList
  [ ("c", [TextBlock "foo"])
  , ("d", [TextBlock "bar"]) ]

templateB :: Template
templateB = Template "b" $ M.fromList
  [ ("c", [TextBlock "bar"])
  , ("e", [TextBlock "baz"]) ]
