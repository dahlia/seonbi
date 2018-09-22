{-# LANGUAGE LambdaCase #-}
module Text.Seonbi.Html.Clipper
    ( clipPrefixText
    , clipSuffixText
    , clipText
    ) where

import Control.Monad
import Data.List (dropWhileEnd)

import Data.Text

import Text.Seonbi.Html

-- | Clip the given prefix text and suffix text from the HTML fragments.
-- It simply is composed of 'clipPrefixText' and 'clipSuffixText' functions.
-- It returns 'Nothing' if any of a prefix and a suffix does not match.
clipText :: Text -> Text -> [HtmlEntity] -> Maybe [HtmlEntity]
clipText prefix suffix =
    clipSuffixText suffix <=< clipPrefixText prefix

-- | Clip the given prefix text from the HTML fragments.  If its first
-- text element does not have the same prefix, or the first element is not
-- an 'HtmlText' node, or the list of HTML fragments have nothing at all,
-- it returns 'Nothing'.
--
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> clipPrefixText "foo" [HtmlText [] "bar", HtmlStartTag [] P ""]
-- Nothing
-- >>> clipPrefixText "foo" [HtmlStartTag [] P "", HtmlText [] "foo"]
-- Nothing
-- >>> clipPrefixText "foo" []
-- Nothing
--
-- If the first element is an 'HtmlText' node, and its 'rawText' contains
-- the common prefix text, it returns a 'Just' value holding a list of
-- HTML fragments with the common prefix removed.
--
-- >>> clipPrefixText "foo" [HtmlText [] "foobar", HtmlStartTag [] P ""]
-- Just [HtmlText {... "bar"},HtmlStartTag {...}]
-- >>> clipPrefixText "foo" [HtmlText [] "foo", HtmlStartTag [] P ""]
-- Just [HtmlStartTag {..., tag = P, ...}]
--
-- A given text is treated as a raw text, which means even if some HTML
-- entities refer to the same characters it may fails to match unless
-- they share the exactly same representation, e.g.:
--
-- >>> clipPrefixText "&amp;" [HtmlText [] "&AMP;"]
-- Nothing
--
-- In the same manner, it doesn't find a prefix from 'HtmlCdata', e.g.:
--
-- >>> clipPrefixText "foo" [HtmlCdata [] "foo", HtmlStartTag [] P ""]
-- Nothing
--
-- In order to remove a prefix from both 'HtmlText' and 'HtmlCdata',
-- apply 'normalizeText' first so that all 'HtmlCdata' entities are transformed
-- to equivalent 'HtmlText' entities:
--
-- >>> import Text.Seonbi.Html.TextNormalizer (normalizeText)
-- >>> let normalized = normalizeText [HtmlCdata [] "foo", HtmlStartTag [] P ""]
-- >>> clipPrefixText "foo" normalized
-- Just [HtmlStartTag {..., tag = P, ...}]
--
-- Plus, it works even if HTML fragments contain some 'HtmlComment' entities,
-- but these are not touched at all, e.g.:
--
-- >>> clipPrefixText "bar" [HtmlComment [] "foo", HtmlText [] "barbaz"]
-- Just [HtmlComment {... "foo"},HtmlText {... "baz"}]
clipPrefixText :: Text -> [HtmlEntity] -> Maybe [HtmlEntity]
clipPrefixText prefix []
  | Data.Text.null prefix = Just []
  | otherwise = Nothing
clipPrefixText prefix (x@HtmlComment {} : xs) =
    (x :) <$> clipPrefixText prefix xs
clipPrefixText prefix (x@HtmlText { rawText = rawText' } : xs)
  | prefix == rawText' = Just xs
  | prefix `isPrefixOf` rawText' = Just $
      x { rawText = Data.Text.drop (Data.Text.length prefix) rawText' } : xs
  | otherwise = Nothing
clipPrefixText _ _ = Nothing

-- | Clip the given suffix text from the HTML fragments, in the same manner
-- to 'clipPrefixText'.
clipSuffixText :: Text -> [HtmlEntity] -> Maybe [HtmlEntity]
clipSuffixText suffix []
  | Data.Text.null suffix = Just []
  | otherwise = Nothing
clipSuffixText suffix entities =
    case Prelude.last entities' of
        e@HtmlText { rawText = rawText' }
          | suffix == rawText' -> Just (init' ++ comments)
          | suffix `isSuffixOf` rawText' ->
              let
                  sLen = Data.Text.length suffix
                  rtLen = Data.Text.length rawText'
                  clipped = Data.Text.take (rtLen - sLen) rawText'
              in
                  Just (init' ++ e { rawText = clipped } : comments)
          | otherwise -> Nothing
        _ -> Nothing
  where
    entities' :: [HtmlEntity]
    entities' = (`Data.List.dropWhileEnd` entities) $ \ case
        HtmlComment {} -> True
        _ -> False
    init' :: [HtmlEntity]
    init' = Prelude.init entities'
    comments :: [HtmlEntity]
    comments = Prelude.drop (Prelude.length entities') entities
