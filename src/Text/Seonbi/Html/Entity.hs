module Text.Seonbi.Html.Entity
    ( HtmlEntity (..)
    , HtmlRawAttrs
    ) where

import Data.Text

import Text.Seonbi.Html.Tag (HtmlTag)
import Text.Seonbi.Html.TagStack (HtmlTagStack)

-- | All element attributes in a string.
type HtmlRawAttrs = Text

-- | An event entity emitted by 'scanHtml'.
data HtmlEntity
    -- | Represent a token which [opens an HTML element
    -- ](https://www.w3.org/TR/html5/syntax.html#start-tags).
    --
    -- Note that 'rawAttributes' is not a parsed and structured data but a raw
    -- string as its name implies.
    --
    -- The 'tagStack' doesn't include the corresponding opened 'tag'.
    = HtmlStartTag
        { -- | A stack of 'HtmlTag's that represents a hierarchy of a currently
          -- parsing position in an 'HtmlTag' tree.
          tagStack :: HtmlTagStack
        , tag :: HtmlTag
        , rawAttributes :: HtmlRawAttrs
        }
    -- | Represent a token which [closes an HTML element
    -- ](https://www.w3.org/TR/html5/syntax.html#end-tags).
    -- The 'tagStack' doesn't include the corresponding closed 'tag'.
    | HtmlEndTag
        { -- | A stack of 'HtmlTag's that represents a hierarchy of a currently
          -- parsing position in an 'HtmlTag' tree.
          tagStack :: HtmlTagStack
        , tag :: HtmlTag
        }
    -- | Represent a token of a text node.  Note that 'rawText' is not a parsed
    -- and structured data but a raw string as its name implies.  There can be
    -- continuously more than one 'HtmlText' values can be emitted even if they
    -- are not separated by element openings or closings.
    | HtmlText
        { -- | A stack of 'HtmlTag's that represents a hierarchy of a currently
          -- parsing position in an 'HtmlTag' tree.
          tagStack :: HtmlTagStack
        , rawText :: Text
        }
    -- | Represent a token of a
    -- [CDATA section](https://www.w3.org/TR/html5/syntax.html#cdata-sections).
    | HtmlCdata
        { -- | A stack of 'HtmlTag's that represents a hierarchy of a currently
          -- parsing position in an 'HtmlTag' tree.
          tagStack :: HtmlTagStack
        , text :: Text
        }
    -- | Represent a token of an
    -- [HTML comment](https://www.w3.org/TR/html5/syntax.html#comments).
    | HtmlComment
        { -- | A stack of 'HtmlTag's that represents a hierarchy of a currently
          -- parsing position in an 'HtmlTag' tree.
          tagStack :: HtmlTagStack
        , comment :: Text
        }
    deriving (Eq, Ord, Show)
