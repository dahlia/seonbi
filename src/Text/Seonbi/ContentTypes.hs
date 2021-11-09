{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.Seonbi.ContentTypes
    ( ContentType
    , HtmlTransformer
    , TextTransformer
    , asHtmlTransformer
    , asHtmlTransformer'
    , asPlainTextTransformer
    , asXhtmlTransformer
    , contentTypeFromText
    , contentTypes
    , contentTypeText
    , transformWithContentType
    ) where

#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif

import Control.Monad.Fail (MonadFail)
import Data.List

import Data.CaseInsensitive
import Data.Set
import Data.Text as ST
import Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import HTMLEntities.Decoder

import Text.Seonbi.Html
import qualified Text.Seonbi.Html.TagStack as TagStack

-- | Represents a function that transforms an 'HtmlEntity' list.
type HtmlTransformer m
    = (Monad m, MonadFail m) => [HtmlEntity] -> m [HtmlEntity]

-- | Represents a function that transforms a text.
type TextTransformer m
    = (Monad m, MonadFail m) => LT.Text -> m LT.Text

-- | Represents a function that transforms an 'HtmlTransformer' into
-- a 'TextTransformer'.
type TransformerTransformer m
    = (Monad m, MonadFail m) => HtmlTransformer m -> TextTransformer m

-- | Gets a 'TransformerTransformer' that transforms 'HtmlTransformer' into
-- a 'TextTransformer' which transforms an HTML/XHTML text.
asHtmlTransformer'
    :: (Monad m, MonadFail m)
    => Bool
    -- ^ 'True' for XHTML, and 'False' for HTML.
    -> TransformerTransformer m
    -- ^ A 'TransformerTransformer' that transforms an 'HtmlTransformer' into
    -- a 'TextTransformer' which transforms an HTML/XHTML text.
asHtmlTransformer' xhtml transformer htmlText = do
    case scanHtml htmlText of
        Done "" input -> do
            output <- transformer input
            return $ printHtml' output
        _ ->
            fail "failed to parse input"
  where
    printHtml' :: [HtmlEntity] -> LT.Text
    printHtml'
      | xhtml = printXhtml
      | otherwise = printHtml

-- | Transforms an 'HtmlTransformer' into a 'TextTransformer' which transforms
-- an HTML text.
asHtmlTransformer :: (Monad m, MonadFail m) => TransformerTransformer m
asHtmlTransformer = asHtmlTransformer' False

-- | Transforms an 'HtmlTransformer' into a 'TextTransformer' which transforms
-- an XHTML text.
asXhtmlTransformer :: (Monad m, MonadFail m) => TransformerTransformer m
asXhtmlTransformer = asHtmlTransformer' True

asPlainTextTransformer :: (Monad m, MonadFail m) => TransformerTransformer m
asPlainTextTransformer transformer text' = do
    let entities = [HtmlCdata TagStack.empty $ LT.toStrict text']
    output <- transformer entities
    return . LT.concat $
        [ case e of
            HtmlCdata _ cdata -> LT.fromStrict cdata
            HtmlText _ rawText' -> toLazyText $ htmlEncodedText rawText'
            _ -> LT.empty
        | e <- output
        ]

-- | Represents a case-insensitive content type.
type ContentType = CI ST.Text

-- | Converts a 'Text' to a 'ContentType'.
contentTypeFromText :: ST.Text -> ContentType
contentTypeFromText = mk

-- | Converts a 'ContentType' to a 'Text'.
contentTypeText :: ContentType -> ST.Text
contentTypeText = original

newtype TransformerTransformer' m =
    TransformerTransformer' (TransformerTransformer m)
transformers :: (Monad m, MonadFail m)
             => [(ContentType, TransformerTransformer' m)]
transformers =
    [ ("text/html", TransformerTransformer' asHtmlTransformer)
    , ("application/xhtml+xml", TransformerTransformer' asXhtmlTransformer)
    , ("text/plain", TransformerTransformer' asPlainTextTransformer)
    ]

-- | Supported content types.
contentTypes :: Set ContentType
contentTypes = (Data.Set.fromList . Prelude.map fst)
    (transformers :: [(ContentType, TransformerTransformer' IO)])

getTransformerTransformer :: (Monad m, MonadFail m)
                          => ContentType
                          -> Maybe (TransformerTransformer' m)
getTransformerTransformer contentType =
    snd <$> Data.List.find ((== contentType) . fst) transformers

-- | Applies an 'HtmlTransformer' to the given text with respect to the
-- given content type.
transformWithContentType
    :: (Monad m, MonadFail m)
    => ContentType
    -- ^ A content type.  If the content type is unsupported (i.e. not in
    -- 'contentTypes'), this function fails.
    -> HtmlTransformer m
    -- ^ An 'HtmlTransformer' to apply.
    -> LT.Text
    -- ^ A input text to transform.
    -> m LT.Text
    -- ^ A transformed text.
transformWithContentType contentType transformer inputText =
    case getTransformerTransformer contentType of
        Nothing -> fail $ ST.unpack $
            "unknown content type: " <> contentTypeText contentType
        Just (TransformerTransformer' transformTransformer) ->
            transformTransformer transformer inputText
