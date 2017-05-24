-- |
-- Module      :  Text.Mustache.Parser
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Megaparsec parser for Mustache templates. You don't usually need to
-- import the module, because "Text.Mustache" re-exports everything you may
-- need, import that module instead.

module Text.Mustache.Parser
  ( parseMustache )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text)
import Text.Megaparsec
import Text.Mustache.Type
import qualified Data.Text             as T
import qualified Text.Megaparsec.Lexer as L

----------------------------------------------------------------------------
-- Parser

-- | Parse a given Mustache template.

parseMustache
  :: FilePath
     -- ^ Location of the file to parse
  -> Text
     -- ^ File contents (Mustache template)
  -> Either (ParseError Char Dec) [Node]
     -- ^ Parsed nodes or parse error
parseMustache = parse $
  evalStateT (pMustache eof) (Delimiters "{{" "}}")

pMustache :: Parser () -> Parser [Node]
pMustache = fmap catMaybes . manyTill (choice alts)
  where
    alts =
      [ Nothing <$  withStandalone pComment
      , Just    <$> pSection "#" Section
      , Just    <$> pSection "^" InvertedSection
      , Just    <$> pStandalone (pPartial Just)
      , Just    <$> pPartial (const Nothing)
      , Nothing <$  withStandalone pSetDelimiters
      , Just    <$> pUnescapedVariable
      , Just    <$> pUnescapedSpecial
      , Just    <$> pEscapedVariable
      , Just    <$> pTextBlock ]
{-# INLINE pMustache #-}

pTextBlock :: Parser Node
pTextBlock = do
  start <- gets openingDel
  (void . notFollowedBy . string) start
  let terminator = choice
        [ (void . lookAhead . string) start
        , pBol
        , eof ]
  TextBlock . T.pack <$> someTill anyChar terminator
{-# INLINE pTextBlock #-}

pUnescapedVariable :: Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"
{-# INLINE pUnescapedVariable #-}

pUnescapedSpecial :: Parser Node
pUnescapedSpecial = do
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ "{") (string $ "}" ++ end) $
    UnescapedVar <$> pKey
{-# INLINE pUnescapedSpecial #-}

pSection :: String -> (Key -> [Node] -> Node) -> Parser Node
pSection suffix f = do
  key   <- withStandalone (pTag suffix)
  nodes <- (pMustache . withStandalone . pClosingTag) key
  return (f key nodes)
{-# INLINE pSection #-}

pPartial :: (Pos -> Maybe Pos) -> Parser Node
pPartial f = do
  pos <- f <$> L.indentLevel
  key <- pTag ">"
  let pname = PName $ T.intercalate (T.pack ".") (unKey key)
  return (Partial pname pos)
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
  (void . symbol) (start ++ "=")
  start' <- pDelimiter <* scn
  end'   <- pDelimiter <* scn
  (void . string) ("=" ++ end)
  put (Delimiters start' end')
{-# INLINE pSetDelimiters #-}

pEscapedVariable :: Parser Node
pEscapedVariable = EscapedVar <$> pTag ""
{-# INLINE pEscapedVariable #-}

withStandalone :: Parser a -> Parser a
withStandalone p = pStandalone p <|> p
{-# INLINE withStandalone #-}

pStandalone :: Parser a -> Parser a
pStandalone p = pBol *> try (between sc (sc <* (void eol <|> eof)) p)
{-# INLINE pStandalone #-}

pTag :: String -> Parser Key
pTag suffix = do
  start <- gets openingDel
  end   <- gets closingDel
  between (symbol $ start ++ suffix) (string end) pKey
{-# INLINE pTag #-}

pClosingTag :: Key -> Parser ()
pClosingTag key = do
  start <- gets openingDel
  end   <- gets closingDel
  let str = keyToString key
  void $ between (symbol $ start ++ "/") (string end) (symbol str)
{-# INLINE pClosingTag #-}

pKey :: Parser Key
pKey = (fmap Key . lexeme . label "key") (implicit <|> other)
  where
    implicit = [] <$ char '.'
    other    = sepBy1 (T.pack <$> some ch) (char '.')
    ch       = alphaNumChar <|> oneOf "-_"
{-# INLINE pKey #-}

pDelimiter :: Parser String
pDelimiter = some (satisfy delChar) <?> "delimiter"
  where delChar x = not (isSpace x) && x /= '='
{-# INLINE pDelimiter #-}

pBol :: Parser ()
pBol = do
  level <- L.indentLevel
  unless (level == unsafePos 1) empty
{-# INLINE pBol #-}

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
-- Lexer helpers and other

scn :: Parser ()
scn = L.space (void spaceChar) empty empty
{-# INLINE scn #-}

sc :: Parser ()
sc = L.space (void $ oneOf " \t") empty empty
{-# INLINE sc #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn
{-# INLINE lexeme #-}

symbol :: String -> Parser String
symbol = L.symbol scn
{-# INLINE symbol #-}

keyToString :: Key -> String
keyToString (Key []) = "."
keyToString (Key ks) = intercalate "." (T.unpack <$> ks)
{-# INLINE keyToString #-}
