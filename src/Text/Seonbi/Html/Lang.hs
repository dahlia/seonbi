{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.Lang
    ( LangHtmlEntity (..)
    , LanguageTag
    , annotateWithLang
    , extractLang
    ) where

import Control.Applicative
import Data.Char (isSpace)
import Data.Maybe

import Data.Attoparsec.Text
import Data.Text

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag (HtmlTag)

-- | Represents a language tag.  Although it is defined as an alias for 'Text',
-- it can be structured in the future.  Do not use its contents directly.
type LanguageTag = Text

-- | Extracts the language tag from the given raw HTML attributes if it has
-- @lang@ attribute.
--
-- >>> extractLang ""
-- Nothing
-- >>> extractLang "lang=en"
-- Just "en"
-- >>> extractLang "lang=\"ko-KR\""
-- Just "ko-kr"
-- >>> extractLang "lang='ko-Hang'"
-- Just "ko-hang"
extractLang
    :: HtmlRawAttrs
    -- ^ A raw HTML attributes to extract the language tag from.
    -> Maybe LanguageTag
    -- ^ A language tag extracted from the given raw HTML attributes.
    -- If the given raw HTML attributes does not have @lang@ attribute or
    -- its value is invalid, 'Nothing' is returned.
extractLang attrs =
    case parseOnly parser' attrs of
        Right (Just lang') ->
            let lt = toLower . strip . normalizeEntities $ lang'
            in if Data.Text.null lt then Nothing else Just lt
        _ -> Nothing
  where
    parser' :: Parser (Maybe Text)
    parser' = do
        attrs' <- langAttr `sepBy` space
        skipSpace
        return $ listToMaybe $ catMaybes attrs'
    langAttr :: Parser (Maybe Text)
    langAttr = do
        (isLang, cont) <- attrIsLang
        value <- if cont then attrValue else return ""
        return (if isLang then Just value else Nothing)
    attrIsLang :: Parser (Bool, Bool)
    attrIsLang = choice
        [ asciiCI "lang=" >> return (True, True)
        , do { _ <- takeWhile1 (/= '=')
             ; eq <- optional (char '=')
             ; return (False, isJust eq)
             }
        ]
    attrValue :: Parser Text
    attrValue = choice
        [ do { skip (== '"'); v <- takeTill (== '"'); skip (== '"'); return v }
        , do { skip (== '\'')
             ; v <- takeTill (== '\'')
             ; skip (== '\''); return v
             }
        , takeWhile1 (not . isSpace)
        ]
    normalizeEntities :: Text -> Text
    normalizeEntities
        = Data.Text.replace "&hyphen;" "-"
        . Data.Text.replace "&dash;" "-"
        . Data.Text.replace "&#8208;" "-"
        . Data.Text.replace "&#x2010;" "-"
        . Data.Text.replace "&#X2010;" "-"

-- | Annotates 'HtmlEntity' with the 'lang' tag extracted from it or its
-- ancestors.
data LangHtmlEntity = LangHtmlEntity
    { -- | The @lang@ tag extracted from the HTML 'entity' or its ancestors.
      lang :: Maybe LanguageTag
      -- | The annotated HTML 'entity'.
    , entity :: HtmlEntity
    } deriving (Show, Eq)

-- | Annotates the given HTML entities with the language tag extracted from
-- their @lang@ attributes.  If a parent entity has @lang@ attribute, its
-- all descendants are annotated with the same language tag.
annotateWithLang :: [HtmlEntity] -> [LangHtmlEntity]
annotateWithLang =
    annotate []
  where
    annotate :: [(HtmlTag, Maybe LanguageTag)]
             -> [HtmlEntity]
             -> [LangHtmlEntity]
    annotate _ [] = []
    annotate stack (x@HtmlStartTag { tag = tag', rawAttributes = attrs } : xs) =
        LangHtmlEntity thisLang x : annotate nextStack xs
      where
        parentLang :: Maybe LanguageTag
        parentLang = case stack of
            (_, l):_ -> l
            _ -> Nothing
        thisLang :: Maybe LanguageTag
        thisLang = extractLang attrs <|> parentLang
        nextStack :: [(HtmlTag, Maybe LanguageTag)]
        nextStack = (tag', thisLang) : stack
    annotate stack (x@HtmlEndTag { tag = tag' } : xs) =
        LangHtmlEntity thisLang x : annotate nextStack xs
      where
        (nextStack, thisLang) = case stack of
            [] -> ([], Nothing)
            s@((t, lang'):ys) ->
                (if t == tag' then ys else s, lang')
    annotate stack (x : xs) =
        LangHtmlEntity parentLang x : annotate stack xs
      where
        parentLang :: Maybe LanguageTag
        parentLang = case stack of
            (_, l):_ -> l
            _ -> Nothing
