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
import Data.Foldable (asum)
import Data.List (tails)
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
import Data.Monoid (mempty)
#endif

-- | Render a Mustache 'Template' using Aeson's 'Value' to get actual values
-- for interpolation.

renderMustache :: Template -> Value -> Text
renderMustache Template {..} =
  renderNodes mempty (M.findWithDefault [] templateActual templateCache)
  where
    renderNodes p ns v = T.concat (r p v <$> ns)
    r _ _ (TextBlock    t) = t
    r p v (EscapedVar   k) = escapeHtml (g p k v)
    r p v (UnescapedVar k) = g p k v
    r p v (Section   k ns) =
      let l  = lookupValue p k v
          p' = p <> k
      in if isBlank l
           then ""
           else T.concat $
             case l of
               Object _  -> [renderNodes p' ns v]
               Array xs  -> renderNodes p ns <$> V.toList xs -- FIXME
               Bool True -> [renderNodes p' ns v]
               String _  -> [renderNodes p' ns v]
               _         -> []
    r p v (InvertedSection k ns) =
      let l = lookupValue p k v
      in if isBlank l
           then renderNodes p ns l
           else ""
    r _ v (Partial name pos) =
      fixupIndentation pos (renderMustache (Template name templateCache) v)
    g p k v = renderValue (lookupValue p k v)

-- | Add indentation (the size is passed in as 'Pos' value) to every line of
-- given text except for the first one.

fixupIndentation :: Maybe Pos -> Text -> Text
fixupIndentation Nothing    txt = txt
fixupIndentation (Just pos) txt = indent <> T.replace "\n" ("\n" <> indent) txt
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

lookupValue
  :: Key               -- ^ The key prefix (entering a section changes it)
  -> Key               -- ^ The key to look up
  -> Value             -- ^ The context
  -> Value             -- ^ The resulting value
lookupValue _ (Key []) v = v
lookupValue p k v = fromMaybe Null $
  if (null . drop 1 . unKey) k
    then let f x = simpleLookup (x <> k) v
         in asum (fmap (f . Key ) . reverse . tails $ unKey p)
    else simpleLookup (p <> k) v -- ???
{-# INLINE lookupValue #-}

-- | TODO

simpleLookup :: Key -> Value -> Maybe Value
simpleLookup (Key [])     obj        = return obj
simpleLookup (Key (k:ks)) (Object m) = H.lookup k m >>= simpleLookup (Key ks)
simpleLookup _            _          = Nothing -- ??
{-# INLINE simpleLookup #-}

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
