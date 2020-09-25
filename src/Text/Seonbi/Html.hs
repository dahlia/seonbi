-- | Since Seonbi's primitive unit to transform is HTML, this module deals with
-- HTML.
module Text.Seonbi.Html
    ( -- * HTML scanner
      --
      -- | See more on "Text.Seonbi.Html.Scanner" module.
      Result (..)
    , scanHtml
      -- * HTML printer
      --
      -- | See more on "Text.Seonbi.Html.Printer" module.
    , printHtml
    , printXhtml
    , -- * HTML entities
      --
      -- | See more on "Text.Seonbi.Html.Entity" module.
      HtmlEntity (..)
    , HtmlRawAttrs
      -- * HTML tags
      --
      -- | See more on "Text.Seonbi.Html.Tag" module.
    , HtmlTag (..)
    , HtmlTagKind (..)
    , htmlTagKind
    , htmlTagName
      -- * HTML text normalization
      --
      -- | See more on "Text.Seonbi.Html.TextNormalizer" module.
    , normalizeText
      -- * HTML hierarchical stacks
      --
      -- | See more on "Text.Seonbi.Html.TagStack" module.
    , HtmlTagStack
    ) where

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Printer
import Text.Seonbi.Html.Scanner
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.TagStack
import Text.Seonbi.Html.TextNormalizer
