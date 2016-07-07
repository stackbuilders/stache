{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Mustache.Compile.THSpec
  ( main
  , spec )
where

import Test.Hspec

#if MIN_VERSION_template_haskell(2,11,0)
import Data.Semigroup ((<>))
import Text.Mustache.Type
import qualified Data.Map                 as M
import qualified Text.Mustache.Compile.TH as TH
#endif

main :: IO ()
main = hspec spec

spec :: Spec
#if MIN_VERSION_template_haskell(2,11,0)
spec = do
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

fooTemplate :: Template
fooTemplate = Template "foo" $
  M.singleton "foo" [TextBlock "This is the ‘foo’.\n"]

barTemplate :: Template
barTemplate = Template "bar" $
  M.singleton "bar" [TextBlock "And this is the ‘bar’.\n"]
#else
spec = return ()
#endif
