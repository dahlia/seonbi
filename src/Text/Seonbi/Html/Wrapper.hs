module Text.Seonbi.Html.Wrapper
    ( isWrappedBy
    , isWrappedBy'
    , wrap
    ) where

import Text.Seonbi.Html
import Text.Seonbi.Html.TagStack

-- | Wraps given entities with an element.
wrap :: HtmlTagStack -> HtmlTag -> HtmlRawAttrs -> [HtmlEntity] -> [HtmlEntity]
wrap baseStack tag' attributes entities = (:)
    (HtmlStartTag baseStack tag' attributes)
    [ e { tagStack = rebase' (tagStack e) }
    | e <- entities
    ] ++ [HtmlEndTag baseStack tag']
  where
    newBaseStack :: HtmlTagStack
    newBaseStack = push tag' baseStack
    rebase' :: HtmlTagStack -> HtmlTagStack
    rebase' = rebase baseStack newBaseStack

-- | A shortcut to 'isWrappedBy'' of wildcard attributes match.
isWrappedBy :: [HtmlEntity] -> HtmlTag -> Bool
isWrappedBy entities tag' =
    isWrappedBy' entities tag' Nothing

-- | 'True' if the given @['HtmlEntity']@ is wrapped by a tag and attributes.
-- E.g.:
--
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> :{
-- let entities = 
--         [ HtmlStartTag [] Em " id=foo"
--         , HtmlText [Em] "Hello"
--         , HtmlEndTag [] Em
--         ] :: [HtmlEntity]
-- :}
--
-- >>> isWrappedBy' entities Em $ Just " id=foo"
-- True
-- >>> isWrappedBy' entities Div $ Just " id=foo"
-- False
-- >>> isWrappedBy' entities Em $ Just " id=wrong"
-- False
--
-- In order to match to any attributes (wildcard match), give 'Nothing' to
-- the third argument:
--
-- >>> isWrappedBy' entities Em Nothing
-- True
-- >>> isWrappedBy' entities Span Nothing
-- False
--
-- Or you can use 'isWrappedBy' function which is a shortcut for that.
isWrappedBy' :: [HtmlEntity] -> HtmlTag -> Maybe HtmlRawAttrs -> Bool
isWrappedBy' entities@(HtmlStartTag s t a : _) tag' attributes =
    case Prelude.last entities of
        HtmlEndTag s' t' ->
            t == tag' && t' == tag' && s == s' && maybe True (== a) attributes
        _ ->
            False
isWrappedBy' _ _ _ = False
