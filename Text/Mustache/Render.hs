-- |
-- Module      :  Text.Mustache.Render
-- Copyright   :  © 2016 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for rendering Mustache templates.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Mustache.Render
  ( renderMustache )
where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Megaparsec.Pos (Pos, unPos)
import Text.Mustache.Type
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as H
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Vector          as V

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- | Render a Mustache 'Template' using Aeson's 'Value' to get actual values
-- for interpolation.

renderMustache :: Template -> Value -> Text
renderMustache Template {..} =
  renderNodes (M.findWithDefault [] templateActual templateCache)
  where
    renderNodes ns v = T.concat (r v <$> ns)
    r _ (TextBlock    t) = t
    r v (EscapedVar   k) = escapeHtml (g k v)
    r v (UnescapedVar k) = g k v
    r v (Section   k ns) =
      let l = lookupValue k v
      in if isBlank l
           then ""
           else T.concat $
             case l of
               Object _ -> [renderNodes ns l]
               Array xs -> renderNodes ns <$> V.toList xs
               _        -> []
    r v (InvertedSection k ns) =
      let l = lookupValue k v
      in if isBlank l
           then renderNodes ns l
           else ""
    r v (Partial  k pos) =
      fixupIndentation pos (renderMustache (Template k templateCache) v)
    g k v = renderValue (lookupValue k v)

-- | Add indentation (the size is passed in as 'Pos' value) to every line of
-- given text except for the first one.

fixupIndentation :: Pos -> Text -> Text
fixupIndentation pos = T.replace "\n" ("\n" <> indent)
  where
    indent = T.replicate (n - 1) " "
    n = fromIntegral (unPos pos)
{-# INLINE fixupIndentation #-}

-- | Select invisible values.

isBlank :: Value -> Bool
isBlank Null         = True
isBlank (Bool False) = True
isBlank (Object   m) = H.null m
isBlank (Array    a) = V.null a
isBlank _            = False
{-# INLINE isBlank #-}

-- | Lookup value in Aeson's 'Value' and return it if it's found. If the
-- value is missing, return 'Null'.

lookupValue :: Key -> Value -> Value
lookupValue (Key k) (Object m) = fromMaybe Null (H.lookup k m)
lookupValue _       _          = Null
{-# INLINE lookupValue #-}

-- | Render Aeson's 'Value' /without/ HTML escaping.

renderValue :: Value -> Text
renderValue Null         = ""
renderValue (String str) = str
renderValue value        = (T.decodeUtf8 . B.toStrict . encode) value
{-# INLINE renderValue #-}

-- | Escape HTML represented as strict 'Text'.

escapeHtml :: Text -> Text
escapeHtml txt = foldr (uncurry T.replace) txt
  [ ("\"", "&quot;")
  , ("<",  "&lt;")
  , (">",  "&gt;")
  , ("&",  "&amp;") ]
{-# INLINE escapeHtml #-}
