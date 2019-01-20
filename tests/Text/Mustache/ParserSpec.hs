{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.ParserSpec
  ( main
  , spec )
where

import Data.Semigroup ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type
import Data.Text (Text)
import Data.Void (Void)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  testsuite "parseMustache" (parseMustache "") (\name _ _ -> Key { unKey = pure name, keySourcePos = Nothing})
  testsuite "parseMustacheWithPositions" (parseMustacheWithPositions "") (\name line col -> Key { unKey = pure name, keySourcePos = Just SourcePos { sourceName = "", sourceLine = mkPos line, sourceColumn = mkPos col}})
  
testsuite :: String -> (Text -> Either (ParseErrorBundle Text Void) [Node]) -> (Text -> Int -> Int -> Key) -> SpecWith ()
testsuite title p key = describe title $ do
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n"
      `shouldParse` [TextBlock "test12356p0--=-34{}jnv,\n"]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (key "name" 1 4)]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (key "name" 1 5)]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (key "name" 1 5)]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (key "name" 1 3)]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (key "name" 1 4)]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (key "name" 1 4)]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (key "var-name" 1 4)]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (key "var_name" 1 4)]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (key "section" 1 4) []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (key "section" 1 5)
         [ TextBlock "Hi, "
         , EscapedVar (key "name" 1 21)
         , TextBlock "!\n"]]
  context "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}" `shouldParse`
        [InvertedSection (key "section" 1 4) []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}" `shouldParse`
        [InvertedSection (key "section" 1 5) [TextBlock "No one here?!\n"]]
  context "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}" `shouldParse`
        [Partial "that-s_my-partial" (Just $ mkPos 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial "that-s_my-partial" (Just $ mkPos 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [Partial "next_one" (Just $ mkPos 4)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedVar (key "var" 1 14), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedVar (key "var" 1 18), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>" `shouldParse`
        [UnescapedVar (key "var" 1 15)]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedVar (key "var" 1 15), Section (key "section" 1 35) []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (key "section" 1 4) [], EscapedVar (key "var" 1 38)]
  context "when given malformed input" $ do
    it "rejects unclosed tags" $ do
      let s = "{{ name "
      p s `shouldFailWith` err 8 (ueof <> etoks "}}")
    it "rejects unknown tags" $ do
      let s = "{{? boo }}"
      p s `shouldFailWith` err 2 (utoks "?" <> elabel "key")
