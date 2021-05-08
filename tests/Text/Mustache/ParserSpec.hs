{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.ParserSpec
  ( main,
    spec,
  )
where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseMustache" $ do
  let p = parseMustache ""
      key = Key . pure
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n"
      `shouldParse` [TextBlock "test12356p0--=-34{}jnv,\n"]
  context "when parsing a variable" $ do
    context "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (key "name")]
      it "parses escaped {{ nested.variable }}" $
        p "{{ a.b }}" `shouldParse` [EscapedVar (Key ["a", "b"])]
      it "parses escaped {{ nested . variable . with . spaces }}" $
        p "{{   a . b . c   }}" `shouldParse` [EscapedVar (Key ["a ", " b ", " c"])]
    context "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (key "name")]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (key "var-name")]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (key "var_name")]
  context "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}"
        `shouldParse` [ Section
                          (key "section")
                          [ TextBlock "Hi, ",
                            EscapedVar (key "name"),
                            TextBlock "!\n"
                          ]
                      ]
  context "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}"
        `shouldParse` [InvertedSection (key "section") []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}"
        `shouldParse` [InvertedSection (key "section") [TextBlock "No one here?!\n"]]
  context "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}"
        `shouldParse` [Partial "that-s_my-partial" (Just $ mkPos 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}"
        `shouldParse` [Partial "that-s_my-partial" (Just $ mkPos 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}"
        `shouldParse` [Partial "next_one" (Just $ mkPos 4)]
  context "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}"
        `shouldParse` [EscapedVar (key "var"), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}"
        `shouldParse` [EscapedVar (key "var"), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>"
        `shouldParse` [UnescapedVar (key "var")]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-"
        `shouldParse` [EscapedVar (key "var"), Section (key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>"
        `shouldParse` [Section (key "section") [], EscapedVar (key "var")]
  context "when given malformed input" $ do
    let keyConstErr = "key-constituent characters"
    it "rejects unclosed tags" $ do
      let s = "{{ name "
      p s `shouldFailWith` err 8 (ueof <> etoks "}}" <> etok '.' <> elabel keyConstErr)
    it "rejects il-formed expressions" $ do
      let s = "{{ foo..bar }}"
      p s `shouldFailWith` err 7 (utoks "." <> elabel keyConstErr)
