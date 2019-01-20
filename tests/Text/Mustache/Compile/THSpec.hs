{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Mustache.Compile.THSpec
  ( main
  , spec )
where

import Test.Hspec

#if MIN_VERSION_template_haskell(2,11,0)
import Data.Semigroup ((<>))
import Text.Mustache.Type
import Text.Megaparsec (SourcePos(..), mkPos)
import qualified Data.Map                 as M
import qualified Text.Mustache.Compile.TH as TH
#endif

main :: IO ()
main = hspec spec

spec :: Spec
#if MIN_VERSION_template_haskell(2,11,0)
spec = do
  describe "mustache" $
    it "compiles template using QuasiQuotes at compile time" $
      [TH.mustache|From Quasi-Quote!
|]
        `shouldBe` qqTemplate
  describe "mustacheWithLocs" $
    it "compiles template using QuasiQuotes at compile time" $
      [TH.mustacheWithLocs|Hello {{name}} from Quasi-Quote!
|]
        `shouldBe` qqWithLocTemplate
  describe "compileMustacheText" $
    it "compiles template from text at compile time" $
      $(TH.compileMustacheText "foo" "This is the ‘foo’.\n")
        `shouldBe` fooTemplate
  describe "compileMustacheFile" $
    it "compiles template from file at compile time" $
      $(TH.compileMustacheFile "templates/foo.mustache")
        `shouldBe` fooTemplate
  describe "compileMustacheDir" $
    it "compiles templates from directory at compile time" $
      $(TH.compileMustacheDir "foo" "templates/")
        `shouldBe` (fooTemplate <> barTemplate)

qqTemplate :: Template
qqTemplate = Template "quasi-quoted" $
  M.singleton "quasi-quoted" [TextBlock "From Quasi-Quote!\n"]

qqWithLocTemplate :: Template
qqWithLocTemplate = Template "quasi-quoted" $
    M.singleton "quasi-quoted" [ TextBlock "Hello "
                               , EscapedVar (Key ["name"] (Just SourcePos { sourceName = "", sourceColumn = mkPos 9, sourceLine = mkPos 1}))
                               , TextBlock " from Quasi-Quote!\n"]
  
fooTemplate :: Template
fooTemplate = Template "foo" $
  M.singleton "foo" [TextBlock "This is the ‘foo’.\n"]

barTemplate :: Template
barTemplate = Template "bar" $
  M.singleton "bar" [TextBlock "And this is the ‘bar’.\n"]
#else
spec = return ()
#endif
