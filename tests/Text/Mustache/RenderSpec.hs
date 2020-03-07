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
  let w ns value =
        let template = Template "test" (M.singleton "test" ns)
        in fst (renderMustacheW template value)
      r ns value =
        let template = Template "test" (M.singleton "test" ns)
        in renderMustache template value
      key = Key . pure
  it "leaves text block “as is”" $
    r [TextBlock "a text block"] Null `shouldBe` "a text block"
  it "renders escaped variables correctly" $
    r [EscapedVar (key "foo")]
      (object ["foo" .= ("<html>&\"'something'\"</html>" :: Text)])
      `shouldBe` "&lt;html&gt;&amp;&quot;&#39;something&#39;&quot;&lt;/html&gt;"
  it "renders unescaped variables “as is”" $
    r [UnescapedVar (key "foo")]
      (object ["foo" .= ("<html>&\"something\"</html>" :: Text)])
      `shouldBe` "<html>&\"something\"</html>"
  context "when rendering a variable" $ do
    context "when variable does not exist" $
      it "generates correct warning" $
        w [EscapedVar (key "foo")] (object []) `shouldBe`
          [MustacheVariableNotFound (key "foo")]
    context "when variable is not non-scalar" $
      it "generates correct warning" $
        w [EscapedVar (key "foo")] (object ["foo" .= object []]) `shouldBe`
          [MustacheDirectlyRenderedValue (key "foo")]
  context "when rendering a section" $ do
    let nodes = [Section (key "foo") [UnescapedVar (key "bar"), TextBlock "*"]]
    context "when the key is not present" $ do
      it "renders nothing" $
        r nodes (object []) `shouldBe` ""
      it "generates correct warning" $
        w nodes (object []) `shouldBe` [MustacheVariableNotFound (key "foo")]
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
        it "skips empty string" $
          r nodes (object ["foo" .= ("" :: Text)]) `shouldBe` ""
      context "when the key is a Boolean true" $
        it "renders the section without interpolation" $
          r [Section (key "foo") [TextBlock "brr"]]
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
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= [True, True]])
            `shouldBe` "brrbrr"
      context "when the key is a list of objects" $
        it "renders the section many times changing context" $
          r nodes (object ["foo" .= [object ["bar" .= x] | x <- [1..4] :: [Int]]])
            `shouldBe` "1*2*3*4*"
      context "when the key is a number" $ do
        it "renders the section" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= (5 :: Int)])
            `shouldBe` "brr"
        it "uses the key as context" $
          r [Section (key "foo") [EscapedVar (Key [])]]
            (object ["foo" .= (5 :: Int)])
            `shouldBe` "5"
      context "when the key is a non-empty string" $ do
        it "renders the section" $
          r [Section (key "foo") [TextBlock "brr"]]
            (object ["foo" .= ("x" :: Text)])
            `shouldBe` "brr"
        it "uses the key as context" $
          r [Section (key "foo") [EscapedVar (Key [])]]
            (object ["foo" .= ("x" :: Text)])
            `shouldBe` "x"
  context "when rendering an inverted section" $ do
    let nodes = [InvertedSection (key "foo") [TextBlock "Here!"]]
    context "when the key is not present" $ do
      it "renders the inverse section" $
        r nodes (object []) `shouldBe` "Here!"
      it "generates correct warning" $
        w nodes (object []) `shouldBe` [MustacheVariableNotFound (key "foo")]
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
    let nodes = [ Partial "partial" (Just $ mkPos 4)
                , TextBlock "*" ]
    it "skips missing partial" $
      r nodes Null `shouldBe` "   *"
    it "renders partial correctly" $
      let template = Template "test" $
            M.fromList [ ("test", nodes)
                       , ("partial", [TextBlock "one\ntwo\nthree"]) ]
      in renderMustache template Null `shouldBe`
           "   one\n   two\n   three*"
  context "when rendering a nested partial" $
    it "renders outer partial correctly" $
      let template = Template "outer" $
            M.fromList [ ("inner",  [TextBlock "x"])
                       , ("middle", [Partial "inner"  (Just $ mkPos 1)])
                       , ("outer",  [Partial "middle" (Just $ mkPos 1)])
                       ]
      in renderMustache template Null `shouldBe` "x"
  context "when using dotted keys inside a section" $
    it "it should be equivalent to access via one more section" $
      r [ Section (key "things")
          [ EscapedVar (Key ["atts", "color"])
          , TextBlock " == "
          , Section (key "atts") [EscapedVar (key "color")] ] ]
        (object ["things" .=
                 [object
                  ["atts" .=
                   object ["color" .= ("blue" :: Text)]]]])
        `shouldBe` "blue == blue"
