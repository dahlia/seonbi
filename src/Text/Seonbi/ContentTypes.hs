{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.Seonbi.ContentTypes
    ( ContentType
    , HtmlTransformer
    , TextTransformer
    , asCommonMarkTransformer
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

import Control.Monad (forM)
import Control.Monad.Fail (MonadFail)
import Data.Maybe (fromMaybe, isNothing)
import Data.List
import Text.Read (readMaybe)

import CMark
import Data.Set
import Data.Text as ST
import Data.Text.Encoding
import Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import HTMLEntities.Builder
import HTMLEntities.Decoder
import Network.HTTP.Media.Accept
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader

import Text.Seonbi.Html
import Text.Seonbi.Html.Tag (headingLevel, headingTag')
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

-- | Transforms an 'HtmlTransformer' into a 'TextTransformer' which transforms
-- a plain text.
asPlainTextTransformer :: (Monad m, MonadFail m) => TransformerTransformer m
asPlainTextTransformer transformer text' = do
    let escaped = toLazyText $ HTMLEntities.Builder.text $ LT.toStrict text'
    let entities = [HtmlText TagStack.empty $ LT.toStrict escaped]
    output <- transformer entities
    return $ printText output

-- | Transforms an 'HtmlTransformer' into a 'TextTransformer' which transforms
-- a CommonMark (Markdown) text.
asCommonMarkTransformer :: (Monad m, MonadFail m) => TransformerTransformer m
asCommonMarkTransformer transformer input = do
    let inputNode = commonmarkToNode [optSourcePos, optUnsafe] $
            LT.toStrict input
    inputEntities <- fromNode [] inputNode
    outputEntities <- transformer $ normalizeText inputEntities
    let outputNodes = toNode outputEntities
    let outputNode = case outputNodes of
            [node@(Node _ DOCUMENT _)] -> node
            nodes -> Node Nothing DOCUMENT nodes
    return $ LT.fromStrict $
        nodeToCommonmark [optSourcePos , optUnsafe] Nothing outputNode
  where
    fromNode :: (Monad m, MonadFail m) => HtmlTagStack -> Node -> m [HtmlEntity]
    fromNode stack (Node posInfo nodeType children) = case nodeType of
        DOCUMENT ->
            nodeWithChildren Article
        THEMATIC_BREAK -> return
            [ HtmlStartTag stack HR (posAttr posInfo)
            , HtmlEndTag stack HR
            ]
        PARAGRAPH ->
            nodeWithChildren P
        BLOCK_QUOTE ->
            nodeWithChildren BlockQuote
        HTML_BLOCK html ->
            case scanHtml $ LT.fromStrict html of
                Done "" input' -> return $ rebaseStack input'
                _ -> return [HtmlCdata stack html]
        CUSTOM_BLOCK _ _ ->
            return []
        CODE_BLOCK info text' -> return
            [ HtmlStartTag stack Pre (posAttr posInfo <> attr' "info" info)
            , HtmlCdata (nextStack Pre) text'
            , HtmlEndTag stack Pre
            ]
        HEADING level ->
            nodeWithChildren $ headingTag' level
        LIST listAttrs ->
            nodeWithChildren' UL $ attr' "list-attrs" listAttrs
        ITEM ->
            nodeWithChildren LI
        TEXT text' ->
            return [HtmlCdata stack text']
        SOFTBREAK -> return
            [ HtmlStartTag stack BR (posAttr posInfo <> attr' "softbreak" True)
            , HtmlEndTag stack BR
            ]
        LINEBREAK -> return
            [ HtmlStartTag stack BR (posAttr posInfo)
            , HtmlEndTag stack BR
            ]
        HTML_INLINE html ->
            case scanHtml $ LT.fromStrict html of
                Done "" input' -> return $ rebaseStack input'
                _ -> return [HtmlCdata stack html]
        CUSTOM_INLINE _ _ ->
            return []
        CODE text' -> return
            [ HtmlStartTag stack Code (posAttr posInfo)
            , HtmlCdata (nextStack Code) text'
            , HtmlEndTag stack Code
            ]
        EMPH ->
            nodeWithChildren Em
        STRONG ->
            nodeWithChildren Strong
        LINK href title ->
            nodeWithChildren' A $ rawAttr " href" href <> rawAttr " title" title
        IMAGE src title ->
            nodeWithChildren' Img $ rawAttr " src" src <> rawAttr " title" title
      where
        nextStack :: HtmlTag -> HtmlTagStack
        nextStack = (`TagStack.push` stack)
        nodeWithChildren :: (Monad m, MonadFail m) => HtmlTag -> m [HtmlEntity]
        nodeWithChildren tag' = nodeWithChildren' tag' ""
        nodeWithChildren' :: (Monad m, MonadFail m)
                          => HtmlTag -> ST.Text -> m [HtmlEntity]
        nodeWithChildren' tag' extraAttrs = do
            mid <- forM children $ do
                fromNode (nextStack tag')
            let middle = Data.List.concat mid
            return $ HtmlStartTag stack tag' (posAttr posInfo <> extraAttrs) :
                middle ++ [HtmlEndTag stack tag']
        rebase' :: HtmlTagStack -> HtmlTagStack
        rebase' = TagStack.rebase [] stack
        rebaseStack :: [HtmlEntity] -> [HtmlEntity]
        rebaseStack = Prelude.map (\e -> e { tagStack = rebase' $ tagStack e })
    toNode :: [HtmlEntity] -> [Node]
    toNode [] = []
    toNode (x:xs) = case x of
        HtmlComment _ comment' ->
            Node Nothing (htmlNode comment') [] : toNode xs
        HtmlCdata _ cdata ->
            Node Nothing (TEXT cdata) [] : toNode xs
        HtmlText _ rawText' ->
            Node Nothing (TEXT $ unescape rawText') [] : toNode xs
        HtmlEndTag _ _ ->
            toNode xs
        start@(HtmlStartTag stack tag' attrs) ->
            let (children', rest) = Data.List.break (endOf stack tag') xs
                (end, rest') = case rest of
                    end'@(HtmlEndTag endStack endTag):afterEnd ->
                        if endStack == stack && endTag == tag'
                            then ([end'], afterEnd)
                            else ([], rest)
                    _ -> ([], rest)
                posInfo = fromMaybe Nothing
                    (getAttr attrs "posinfo" :: Maybe (Maybe PosInfo))
                softbreak = getAttr attrs "softbreak" :: Maybe Bool
                childrenHtmlNode = htmlNode $ LT.toStrict $ printHtml $
                    start : children' ++ end
                nodeType = case tag' of
                    Article -> DOCUMENT
                    BlockQuote -> BLOCK_QUOTE
                    HR -> THEMATIC_BREAK
                    P -> PARAGRAPH
                    Pre -> case getAttr "info" attrs of
                        Just info -> CODE_BLOCK info $ printText' children'
                        Nothing -> childrenHtmlNode
                    UL ->
                        maybe childrenHtmlNode LIST (getAttr attrs "list-attrs")
                    LI -> ITEM
                    BR ->
                        if softbreak == Just True then SOFTBREAK else LINEBREAK
                    Code -> CODE $ printText' children'
                    Em -> EMPH
                    Strong -> STRONG
                    A -> LINK
                        (fromMaybe ST.empty $ getRawAttr attrs "href")
                        (fromMaybe ST.empty $ getRawAttr attrs "title")
                    Img -> IMAGE
                        (fromMaybe ST.empty $ getRawAttr attrs "src")
                        (fromMaybe ST.empty $ getRawAttr attrs "title")
                    _ ->
                        maybe childrenHtmlNode HEADING (headingLevel tag')
                (nodeType', nodeChildren) =
                    if isNothing posInfo && isNothing softbreak
                        then (childrenHtmlNode, [])
                        else (nodeType, toNode children')
            in
                Node posInfo nodeType' nodeChildren : toNode rest'
      where
        block :: Bool
        block = case TagStack.last (tagStack x) of
            Just A -> False
            Just Em -> False
            Just H1 -> False
            Just H2 -> False
            Just H3 -> False
            Just H4 -> False
            Just H5 -> False
            Just H6 -> False
            Just P -> False
            Just Strong -> False
            Just tag' -> isNothing (headingLevel tag')
            _ -> True
        htmlNode :: ST.Text -> NodeType
        htmlNode
          | block = HTML_BLOCK
          | otherwise = HTML_INLINE
    unescape :: ST.Text -> ST.Text
    unescape = toStrict . toLazyText . htmlEncodedText
    rawAttr :: ST.Text -> ST.Text -> ST.Text
    rawAttr name value = ST.append name $ toStrict $
        "=\"" <> toLazyText (HTMLEntities.Builder.text value) <> "\""
    attr :: Show a => ST.Text -> a -> ST.Text
    attr name value =
        rawAttr ("data-seonbi-cmark-" <> name) $ ST.pack (show value)
    attr' :: Show a => ST.Text -> a -> ST.Text
    attr' name = ST.cons ' ' . attr name
    posAttr :: Maybe PosInfo -> ST.Text
    posAttr = attr "posinfo"
    getRawAttr :: HtmlRawAttrs -> ST.Text -> Maybe ST.Text
    getRawAttr attrs name =
        case ST.breakOn prefix attrs of
            (_, "") -> Nothing
            (_, head') ->
                case ST.break (== '"') (ST.drop (ST.length prefix) head') of
                    (_, "") -> Nothing
                    (value, _) ->
                        Just $ toStrict $ toLazyText $ htmlEncodedText value
      where
        prefix :: ST.Text
        prefix = name <> "=\""
    getAttr :: Read a => HtmlRawAttrs -> ST.Text -> Maybe a
    getAttr attrs name =
        case getRawAttr attrs ("data-seonbi-cmark-" <> name) of
            Nothing -> Nothing
            Just value -> readMaybe $ ST.unpack value
    endOf :: HtmlTagStack -> HtmlTag -> HtmlEntity -> Bool
    endOf stack tag' (HtmlEndTag endStack endTag) =
        not (endStack `TagStack.descendsFrom` stack) || endTag == tag'
    endOf _ _ _ = False
    printText' :: [HtmlEntity] -> ST.Text
    printText' = toStrict . printText


-- | Represents a case-insensitive content type.
type ContentType = MediaType

-- | Converts a 'Text' to a 'ContentType'.
contentTypeFromText :: ST.Text -> Maybe ContentType
contentTypeFromText = parseAccept . encodeUtf8

-- | Converts a 'ContentType' to a 'Text'.
contentTypeText :: ContentType -> ST.Text
contentTypeText = decodeUtf8 . renderHeader

newtype TransformerTransformer' m =
    TransformerTransformer' (TransformerTransformer m)
transformers :: (Monad m, MonadFail m)
             => [(ContentType, TransformerTransformer' m)]
transformers =
    [ ("text/html", TransformerTransformer' asHtmlTransformer)
    , ("application/xhtml+xml", TransformerTransformer' asXhtmlTransformer)
    , ("text/plain", TransformerTransformer' asPlainTextTransformer)
    , ("text/markdown", TransformerTransformer' asCommonMarkTransformer)
    ]

-- | Supported content types.
contentTypes :: Set ContentType
contentTypes = (Data.Set.fromList . Prelude.map fst)
    (transformers :: [(ContentType, TransformerTransformer' IO)])

getTransformerTransformer :: (Monad m, MonadFail m)
                          => ContentType
                          -> Maybe (TransformerTransformer' m)
getTransformerTransformer contentType =
    snd <$> Data.List.find (matches contentType . fst) transformers

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
