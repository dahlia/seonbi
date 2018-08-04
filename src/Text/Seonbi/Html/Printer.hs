{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.Printer
    ( printHtml
    , printXhtml
    ) where

import Data.Char
import Data.List

import qualified Data.Text
import Data.Text.Lazy

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag

-- | Print the list of 'HtmlEntity' into a lazy 'Text'.
--
-- >>> :set -XOverloadedStrings
-- >>> import Text.Seonbi.Html.Scanner
-- >>> let Done "" tokens = scanHtml "<p>Hello,<br>\n<em>world</em>!</p>"
-- >>> printHtml tokens
-- "<p>Hello,<br>\n<em>world</em>!</p>"
printHtml :: [HtmlEntity] -> Text
printHtml = printHtml' False

-- | Similar to 'printHtml' except it renders void (self-closing) tags as
-- like @<br/>@ instead of @<br>@.
--
-- >>> :set -XOverloadedStrings
-- >>> import Text.Seonbi.Html.Scanner
-- >>> let Done "" tokens = scanHtml "<p>Hello,<br>\n<em>world</em>!</p>"
-- >>> printXhtml tokens
-- "<p>Hello,<br/>\n<em>world</em>!</p>"
--
-- Note that normal tags are not rendered as self-closed; only void tags
-- according to HTML specification are:
--
-- >>> let Done "" tokens' = scanHtml "<p></p><p><br></p>"
-- >>> printXhtml tokens'
-- "<p></p><p><br/></p>"
printXhtml :: [HtmlEntity] -> Text
printXhtml = printHtml' True

printHtml' :: Bool -> [HtmlEntity] -> Text
printHtml' xhtml =
    Data.Text.Lazy.concat . Prelude.concatMap render . Data.List.groupBy isVoid
  where
    isVoid :: HtmlEntity -> HtmlEntity -> Bool
    isVoid (HtmlStartTag stck tg _) (HtmlEndTag stck' tg') =
        htmlTagKind tg == Void && stck == stck' && tg == tg'
    isVoid _ _ = False
    render :: [HtmlEntity] -> [Text]
    render [a@HtmlStartTag { tag = t, rawAttributes = at }, b@HtmlEndTag {}] =
        if isVoid a b
        then
            [ "<"
            , fromStrict (htmlTagName t)
            , renderAttrs at
            , if xhtml then "/>" else ">"
            ]
        else e a ++ e b
    render entities = Prelude.concatMap e entities
    e :: HtmlEntity -> [Text]
    e HtmlStartTag { tag = t, rawAttributes = a } =
        ["<", fromStrict (htmlTagName t), renderAttrs a, ">"]
    e HtmlEndTag { tag = t } = ["</", fromStrict (htmlTagName t), ">"]
    e HtmlText { rawText = t } = [fromStrict t]
    e HtmlCdata { text = t } = ["<![CDATA[", fromStrict t, "]]>"]
    e HtmlComment { comment = c } = ["<!--", fromStrict c, "-->"]
    renderAttrs :: Data.Text.Text -> Text
    renderAttrs "" = ""
    renderAttrs attrs
      | isSpace (Data.Text.head attrs) = fromStrict attrs
      | otherwise = ' ' `cons` fromStrict attrs
