{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.RenderSpec
  ( main
  , spec )
where

import Data.Aeson (object, KeyValue (..), Value (..))
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec
import Text.Mustache.Render
import Text.Mustache.Type
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "renderMustache" $ do
  let r ns value =
        let template = Template (Key "test") (M.singleton (Key "test") ns)
        in renderMustache template value
  it "leaves text block “as is”" $
    r [TextBlock "a text block"] Null `shouldBe` "a text block"
  it "renders escaped variables correctly" $
    r [EscapedVar (Key "foo")]
      (object ["foo" .= ("<html>&\"something\"</html>" :: Text)])
      `shouldBe` "&lt;html&gt;&amp;&quot;something&quot;&lt;/html&gt;"
  it "renders unescaped variables “as is”" $
    r [UnescapedVar (Key "foo")]
      (object ["foo" .= ("<html>&\"something\"</html>" :: Text)])
      `shouldBe` "<html>&\"something\"</html>"
  context "when rendering a section" $ do
    let nodes = [Section (Key "foo") [UnescapedVar (Key "bar"), TextBlock "*"]]
    context "when the key is not present" $
      it "renders nothing" $
        r nodes (object []) `shouldBe` ""
    context "when the key is present" $ do
      context "when the key is a “false” value" $ do
        it "skips the Null value" $
          r nodes (object ["foo" .= Null]) `shouldBe` ""
        it "skips false Boolean" $
          r nodes (object ["foo" .= False]) `shouldBe` ""
        it "skips empty list" $
          r nodes (object ["foo" .= ([] :: [Text])]) `shouldBe` ""
        it "skips empty object" $
          r nodes (object ["foo" .= object []]) `shouldBe` ""
      context "when the key is a Boolean true" $
        it "renders the section without interpolation" $
          r [Section (Key "foo") [TextBlock "brr"]]
            (object ["foo" .= object ["bar" .= True]])
            `shouldBe` "brr"
      context "when the key is an object" $
        it "uses it to render section once" $
          r nodes (object ["foo" .= object ["bar" .= ("huh?" :: Text)]])
            `shouldBe` "huh?*"
      context "when the key is a singleton list" $
        it "uses it to render section once" $
          r nodes (object ["foo" .= object ["bar" .= ("huh!" :: Text)]])
            `shouldBe` "huh!*"
      context "when the key is a list of Boolean trues" $
        it "renders the section as many times as there are elements" $
          r [Section (Key "foo") [TextBlock "brr"]]
            (object ["foo" .= [True, True]])
            `shouldBe` "brrbrr"
      context "wehn the key is a list of objects" $
        it "renders the section many times changing context" $
          r nodes (object ["foo" .= [object ["bar" .= x] | x <- [1..4] :: [Int]]])
            `shouldBe` "1*2*3*4*"
  context "when rendering an inverted section" $ do
    let nodes = [InvertedSection (Key "foo") [TextBlock "Here!"]]
    context "when the key is not present" $
      it "renders the inverse section" $
        r nodes (object []) `shouldBe` "Here!"
    context "when the key is present" $ do
      context "when the key is a “false” value" $ do
        it "renders with Null value" $
          r nodes (object ["foo" .= Null]) `shouldBe` "Here!"
        it "renders with false Boolean" $
          r nodes (object ["foo" .= False]) `shouldBe` "Here!"
        it "renders with empty list" $
          r nodes (object ["foo" .= ([] :: [Text])]) `shouldBe` "Here!"
        it "renders with empty object" $
          r nodes (object ["foo" .= object []]) `shouldBe` "Here!"
      context "when the key is a “true” value" $ do
        it "skips true Boolean" $
          r nodes (object ["foo" .= True]) `shouldBe` ""
        it "skips non-empty object" $
          r nodes (object ["foo" .= object ["bar" .= True]]) `shouldBe` ""
        it "skips non-empty list" $
          r nodes (object ["foo" .= [True]]) `shouldBe` ""
  context "when rendering a partial" $ do
    let nodes = [ TextBlock "   "
                , Partial (Key "partial") (unsafePos 4)
                , TextBlock "*" ]
    it "skips missing partial" $
      r nodes Null `shouldBe` "   *"
    it "renders partial correctly" $
      let template = Template (Key "test") $
            M.fromList [ (Key "test", nodes)
                       , (Key "partial", [TextBlock "one\ntwo\nthree"]) ]
      in renderMustache template Null `shouldBe`
           "   one\n   two\n   three*"
