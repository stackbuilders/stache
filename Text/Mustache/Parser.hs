-- |
-- Module      :  Text.Mustache.Parser
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Megaparsec parser for Mustache templates.

{-# LANGUAGE DeriveDataTypeable #-}

module Text.Mustache.Parser
  ( parseMustache )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.Megaparsec
import Text.Mustache.Type
import qualified Data.Text             as T
import qualified Text.Megaparsec.Lexer as L

----------------------------------------------------------------------------
-- Parser

-- | Parse given Mustache template.

parseMustache
  :: FilePath
     -- ^ Location of file to parse
  -> Text
     -- ^ File contents (Mustache template)
  -> Either (ParseError Char Dec) [Node]
     -- ^ Parsed nodes or parse error
parseMustache = parse (evalStateT (pMustache <* eof) (Delimiters "{{" "}}"))

pMustache :: Parser [Node]
pMustache = catMaybes <$> many (choice alts)
  where
    alts =
      [ Just <$> pTextBlock
      , Just <$> pUnescapedVariable
      , Just <$> pUnescapedSpecial
      , Just <$> pSection "#" Section
      , Just <$> pSection "^" InvertedSection
      , Just <$> pPartial
      , Nothing <$ pComment
      , Nothing <$ pSetDelimiters
      , Just <$> pEscapedVariable ]

pTextBlock :: Parser Node
pTextBlock = do
  start <- gets openingDel
  (void . notFollowedBy . string) start
  TextBlock . T.pack <$>
    someTill anyChar ((void . lookAhead . string) start <|> eof)

pUnescapedVariable :: Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"
{-# INLINE pUnescapedVariable #-}

pUnescapedSpecial :: Parser Node
pUnescapedSpecial =
  UnescapedVar <$> between (symbol "{{{") (string "}}}") pKey
{-# INLINE pUnescapedSpecial #-}

pSection :: String -> (Key -> [Node] -> Node) -> Parser Node
pSection suffix f = do
  key   <- pTag suffix
  nodes <- pMustache
  pClosingTag key
  return (f key nodes)
{-# INLINE pSection #-}

pPartial :: Parser Node
pPartial = do
  pos <- L.indentLevel
  key <- pTag ">"
  return (Partial key pos)
{-# INLINE pPartial #-}

pComment :: Parser ()
pComment = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . symbol) (start ++ "!")
  manyTill anyChar (string end)
{-# INLINE pComment #-}

pSetDelimiters :: Parser ()
pSetDelimiters = void $ do
  start <- gets openingDel
  end   <- gets closingDel
  (void . string) (start ++ "=")
  start' <- pDelimiter
  sc
  end'   <- pDelimiter
  (void . string) ("=" ++ end)
  put (Delimiters start' end')
{-# INLINE pSetDelimiters #-}

pEscapedVariable :: Parser Node
pEscapedVariable = EscapedVar <$> pTag ""
{-# INLINE pEscapedVariable #-}

pTag :: String -> Parser Key
pTag suffix = do
  start <- gets openingDel
  end   <- gets closingDel
  notFollowedBy (string $ start ++ "/")
  between (symbol $ start ++ suffix) (string end) pKey
{-# INLINE pTag #-}

pClosingTag :: Key -> Parser ()
pClosingTag key = do
  start <- gets openingDel
  end   <- gets closingDel
  let str = T.unpack (unKey key)
  void $ between (symbol $ start ++ "/") (string end) (symbol str)
{-# INLINE pClosingTag #-}

pKey :: Parser Key
pKey = Key . T.pack <$> lexeme (some ch <?> "key")
  where ch = alphaNumChar <|> oneOf "-_"
{-# INLINE pKey #-}

pDelimiter :: Parser String
pDelimiter = some (satisfy delChar) <?> "delimiter"
  where delChar x = not (isSpace x) && x /= '='
{-# INLINE pDelimiter #-}

----------------------------------------------------------------------------
-- Auxiliary types

-- | Type of Mustache parser monad stack.

type Parser = StateT Delimiters (Parsec Dec Text)

-- | State used in Mustache parser. It includes currently set opening and
-- closing delimiters.

data Delimiters = Delimiters
  { openingDel :: String
  , closingDel :: String }

----------------------------------------------------------------------------
-- Lexer helpers

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc
