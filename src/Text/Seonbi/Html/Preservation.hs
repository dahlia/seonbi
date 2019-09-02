{-# LANGUAGE NamedFieldPuns #-}
module Text.Seonbi.Html.Preservation
    ( isPreservedEntity
    , isPreservedTag
    , isPreservedTagStack
    ) where

import Prelude hiding (any)

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.TagStack

-- | 'True' if the given tag should be preserved from transformation.
isPreservedTag :: HtmlTag -> Bool
isPreservedTag tag' =
    case tag' of
        Code -> True
        Kbd -> True
        Pre -> True
        TextArea -> True
        _ ->
            case htmlTagKind tag' of
                Normal -> False
                EscapableRawText -> False
                _ -> True

-- | 'True' if the given tag stack should be preserved from transformation.
isPreservedTagStack :: HtmlTagStack -> Bool
isPreservedTagStack = any isPreservedTag

-- | 'True' if the given HTML entity should be preserved from transformation.
isPreservedEntity :: HtmlEntity -> Bool
isPreservedEntity HtmlComment {} =
    True
isPreservedEntity HtmlStartTag { tagStack, tag } =
    isPreservedTag tag || isPreservedTagStack tagStack
isPreservedEntity HtmlEndTag { tagStack, tag } =
    isPreservedTag tag || isPreservedTagStack tagStack
isPreservedEntity entity =
    isPreservedTagStack $ tagStack entity
