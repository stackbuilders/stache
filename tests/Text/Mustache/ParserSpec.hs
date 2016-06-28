{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.ParserSpec
  ( main
  , spec )
where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseMustache" $ do
  let p = parseMustache ""
  it "parses text" $
    let t = "test12356p0--=-34{}jnv,\n"
    in p t `shouldParse` [TextBlock t]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (Key "name")]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (Key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (Key "name")]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (Key "name")]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (Key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (Key "name")]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (Key "var-name")]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (Key "var_name")]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (Key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (Key "section")
         [ TextBlock "Hi, "
         , EscapedVar (Key "name")
         , TextBlock "!\n"]]
  context "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}" `shouldParse`
        [InvertedSection (Key "section") []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}" `shouldParse`
        [InvertedSection (Key "section") [TextBlock "No one here?!\n"]]
  context "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}" `shouldParse`
        [Partial (Key "that-s_my-partial") (unsafePos 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial (Key "that-s_my-partial") (unsafePos 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [TextBlock "   ", Partial (Key "next_one") (unsafePos 4)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedVar (Key "var"), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedVar (Key "var"), TextBlock "{{ var  }}"]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedVar (Key "var"), Section (Key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (Key "section") [], EscapedVar (Key "var")]
  context "when given malformed input" $ do
    let pos l c = SourcePos "" (unsafePos l) (unsafePos c) :| []
        ne      = NE.fromList
    it "rejects unclosed tags" $
      p "{{ name" `shouldFailWith` ParseError
        { errorPos        = pos 1 8
        , errorUnexpected = S.singleton EndOfInput
        , errorExpected   = S.fromList
          [Tokens (ne "}}"), Label (ne "rest of key")]
        , errorCustom     = S.empty }
    it "rejects unknown tags" $
      p "{{? boo }}" `shouldFailWith` ParseError
        { errorPos        = pos 1 3
        , errorUnexpected = S.singleton (Tokens $ ne "?")
        , errorExpected   = S.singleton (Label  $ ne "key")
        , errorCustom     = S.empty }
