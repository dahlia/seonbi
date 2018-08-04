{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.TextNormalizer
    ( escapeHtmlEntities
    , normalizeCdata
    , normalizeText
    ) where

import Control.Exception
import Data.List

import Data.Text hiding (groupBy, map)

import Text.Seonbi.Html.Entity

-- | As 'scanHtml' may emit two or more continuous 'HtmlText' fragments even
-- if these can be represented as only one 'HtmlText' fragment, it makes
-- postprocessing hard.
--
-- The 'normalizeText' function concatenates such continuous 'HtmlText'
-- fragments into one if possible so that postprocessing can be easy:
--
-- >>> :set -XOverloadedStrings -XOverloadedLists
-- >>> normalizeText [HtmlText [] "Hello, ", HtmlText [] "world!"]
-- [HtmlText {tagStack = fromList [], rawText = "Hello, world!"}]
--
-- It also transforms all 'HtmlCdata' fragments into an 'HtmlText' together.
--
-- >>> :{
-- normalizeText [ HtmlText [] "foo "
--               , HtmlCdata [] "<bar>", HtmlText [] " baz!"
--               ]
-- :}
-- [HtmlText {tagStack = fromList [], rawText = "foo &lt;bar&gt; baz!"}]
normalizeText :: [HtmlEntity] -> [HtmlEntity]
normalizeText fragments =
    [ case map normalizeCdata frags of
        [f] ->
            f
        frags'@(HtmlText { tagStack = s }:_) ->
            HtmlText
                { tagStack = s
                , rawText = Data.Text.concat $ map rawText frags'
                }
        frags' ->
            throw $ AssertionFailed
                ("Unexpected error occured; grouping does not work well: " ++
                    show frags')
    | frags <- groupBy isSibling fragments
    ]
  where
    isSibling :: HtmlEntity -> HtmlEntity -> Bool
    isSibling HtmlText  { tagStack = a } HtmlText  { tagStack = b } = a == b
    isSibling HtmlText  { tagStack = a } HtmlCdata { tagStack = b } = a == b
    isSibling HtmlCdata { tagStack = a } HtmlText  { tagStack = b } = a == b
    isSibling HtmlCdata { tagStack = a } HtmlCdata { tagStack = b } = a == b
    isSibling _ _ = False

-- | Transform a given 'HtmlCdata' node into an equivalent 'HtmlText' node.
--
-- >>> import Text.Seonbi.Html.Tag
-- >>> normalizeCdata HtmlCdata { tagStack = [P], text = "<p id=\"foo\">" }
-- HtmlText {tagStack = fromList [P], rawText = "&lt;p id=&quot;foo&quot;&gt;"}
normalizeCdata :: HtmlEntity -> HtmlEntity
normalizeCdata HtmlCdata { tagStack = s, text = t } =
    HtmlText { tagStack = s, rawText = escapeHtmlEntities t }
normalizeCdata entity = entity

-- | Escape special (control) characters into corresponding character entities
-- in the given HTML text.
--
-- >>> escapeHtmlEntities "<foo & \"bar\">"
-- "&lt;foo &amp; &quot;bar&quot;&gt;"
escapeHtmlEntities :: Text -> Text
escapeHtmlEntities =
    Data.Text.concatMap $ \ case
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        c -> Data.Text.singleton c
