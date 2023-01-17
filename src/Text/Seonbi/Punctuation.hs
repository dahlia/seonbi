{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- | This module deals with punctuations in Korean text.
module Text.Seonbi.Punctuation
    ( -- * Arrows
      ArrowTransformationOption (..)
    , transformArrow
      -- * Quotes
    , CitationQuotes (..)
    , Quotes (..)
    , QuotePair (..)
    , angleQuotes
    , cornerBrackets
    , curvedQuotes
    , curvedSingleQuotesWithQ
    , guillemets
    , horizontalCornerBrackets
    , horizontalCornerBracketsWithQ
    , quoteCitation
    , transformQuote
    , verticalCornerBrackets
    , verticalCornerBracketsWithQ
      -- * Stops: periods, commas, & interpuncts
    , Stops (..)
    , horizontalStops
    , horizontalStopsWithSlashes
    , normalizeStops
    , transformEllipsis
    , verticalStops
      -- * Dashes
    , transformEmDash
    ) where

import Prelude hiding (takeWhile)

import Control.Monad
import Data.Char (isSpace)
import Data.Either
import Data.List (minimumBy)
import Data.Maybe
import Data.Ord
import Numeric

import Data.Attoparsec.Text
import Data.Set
import Data.Text hiding (any, length, takeWhile)
import qualified Data.Text

import Text.Seonbi.Html
import Text.Seonbi.Html.Clipper
import Text.Seonbi.Html.Lang
import Text.Seonbi.Html.Preservation
import Text.Seonbi.Html.Wrapper
import Text.Seonbi.PairedTransformer

-- | A set of quoting parentheses to be used by 'quoteCitation' function.
--
-- There are two presets: 'angleQuotes' and 'cornerBrackets'.  These both
-- surround titles with a @\<cite>@ tag.  In order to disable surrounded
-- elements, set 'htmlElement' field to 'Nothing', e.g.:
--
-- @
-- 'angleQuotes' { 'htmlElement' = 'Nothing' }
-- @
data CitationQuotes = CitationQuotes
    { -- | The leading and trailing punctuations to surround a title of
      -- novel, newspaper, magazine, movie, television program, etc.
      title :: (Text, Text)
    , -- | The leading and trailing punctuations to surround a title of
      -- short story, chapter, article, episode, etc.
      subtitle :: (Text, Text)
    , -- | Optional pair of an HTML element and its attributes to surround
      -- citations.  E.g., if it is @'Just' ('Cite', " class=\"autogen\")@
      -- titles are transformed like @\<cite class="autogen">이런 날\</cite>@.
      htmlElement :: Maybe (HtmlTag, HtmlRawAttrs)
    } deriving (Eq, Show)

-- | Cite a title using angle quotes, used by South Korean orthography in
-- horizontal writing (橫書), e.g., 《나비와 엉겅퀴》 or 〈枾崎의 바다〉.
angleQuotes :: CitationQuotes
angleQuotes = CitationQuotes
    { title = ("&#12298;", "&#12299;")
    , subtitle = ("&#12296;", "&#12297;")
    , htmlElement = Just (Cite, "")
    }

-- | Cite a title using corner brackets, used by South Korean orthography in
-- vertical writing (縱書) and Japanese orthography,
-- e.g., 『나비와 엉겅퀴』 or 「枾崎의 바다」.
cornerBrackets :: CitationQuotes
cornerBrackets = CitationQuotes
    { title = ("&#12302;", "&#12303;")
    , subtitle = ("&#12300;", "&#12301;")
    , htmlElement = Just (Cite, "")
    }

-- | People tend to cite the title of a work (e.g., a book, a paper, a poem,
-- a song, a film, a TV show, a game) by wrapping inequality symbols
-- like @\<\<나비와 엉겅퀴>>@ or @\<枾崎의 바다>@ instead of proper angle quotes
-- like @《나비와 엉겅퀴》@ or @〈枾崎의 바다〉@.
--
-- This transforms, in the given HTML fragments, all folk-citing quotes into
-- typographic citing quotes:
--
-- - Pairs of less-than and greater-than inequality symbols (@<@ & @>@) into
--   pairs of proper angle quotes (@〈@ & @〉@)
-- - Pairs of two consecutive inequality symbols (@<<@ & @>>@) into
--   pairs of proper double angle quotes (@《@ & @》@)
quoteCitation :: CitationQuotes -- ^ Quoting parentheses to wrap titles.
              -> [HtmlEntity] -- ^ The input HTML entities to transform.
              -> [HtmlEntity]
quoteCitation quotes =
    transformPairs pairedTransformer
  where
    pairedTransformer :: PairedTransformer TitlePunct
    pairedTransformer = PairedTransformer
        { ignoresTagStack = isPreservedTagStack
        , matchStart = \ _ -> matcher $ parser openTitle openSubtitle
        , matchEnd = matcher $ parser closeTitle closeSubtitle
        , areMatchesPaired = (==)
        , transformPair = transformPair'
        }
    transformPair' :: TitlePunct -> TitlePunct -> [HtmlEntity] -> [HtmlEntity]
    transformPair' punct _ buffer =
        case cited of
            [] -> []
            entities@(x : _) ->
                let
                    ts = tagStack x
                    makeText = HtmlText ts
                    category = case punct of
                        DoubleAngle -> title
                        DoubleCorner -> title
                        DoubleInequal -> title
                        Angle -> subtitle
                        Corner -> subtitle
                        Inequal -> subtitle
                    (startP, endP) = category quotes
                in
                    makeText startP : entities ++ [makeText endP]
      where
        buffer' :: [HtmlEntity]
        buffer' = Prelude.drop 1 $ Prelude.take (length buffer - 1) buffer
        cited :: [HtmlEntity]
        cited = case (htmlElement quotes, buffer') of
            (Nothing, b) -> b
            (_, []) -> []
            (Just (tag', ""), x : _) ->
                if buffer' `isWrappedBy` tag'
                    then buffer'
                    else wrap (tagStack x) tag' "" buffer'
            (Just (tag', attrs), x : _) ->
                if isWrappedBy' buffer' tag' (Just attrs)
                    then buffer'
                    else wrap (tagStack x) tag' attrs buffer'
    specialChars :: Set Char
    specialChars =
        [ '<', '>', '&'
        , '\x3008', '\x3009', '\x300a', '\x300b', '\x300e', '\x300f'
        ]
    matcher :: Parser [Either Text (TitlePunct, Text, Text)]
            -> Text
            -> Maybe (TitlePunct, Text, Text, Text)
    matcher parser' text' = case parseOnly parser' text' of
        Left _ -> Nothing
        Right matches -> case partitionEithers matches of
            (l, [(punct, m, post)]) -> Just (punct, Data.Text.concat l, m, post)
            _ -> Nothing
    parser :: Parser (TitlePunct, Text)
           -> Parser (TitlePunct, Text)
           -> Parser [Either Text (TitlePunct, Text, Text)]
    parser title' subtitle' = many' $ choice
        [ Left <$> takeWhile1 (`notElem` specialChars)
        , do
            (punct, m) <- title'
            remain <- takeWhile (const True)
            return $ Right (punct, m, remain)
        , do
            (punct, m) <- subtitle'
            remain <- takeWhile (const True)
            return $ Right (punct, m, remain)
        , Left . Data.Text.singleton <$> anyChar
        ]
    openTitle :: Parser (TitlePunct, Text)
    openTitle = choice
        [ leftDoubleAngle
        , leftDoubleCorner
        , (DoubleInequal,) <$> double' lt
        ]
    closeTitle :: Parser (TitlePunct, Text)
    closeTitle = choice
        [ rightDoubleAngle
        , rightDoubleCorner
        , (DoubleInequal,) <$> double' gt
        ]
    double' :: Parser Text -> Parser Text
    double' p = do
        t <- p
        t' <- p
        return (t `append` t')
    openSubtitle :: Parser (TitlePunct, Text)
    openSubtitle = choice [leftAngle, (Inequal,) <$> lt]
    closeSubtitle :: Parser (TitlePunct, Text)
    closeSubtitle = choice [rightAngle, (Inequal,) <$> gt]
    leftAngle :: Parser (TitlePunct, Text)
    leftAngle = (Angle,) <$> choice
        [ Data.Text.singleton <$> char '\x3008'
        , string "&#12296;"
        , asciiCI "&#x3008;"
        ]
    rightAngle :: Parser (TitlePunct, Text)
    rightAngle = (Angle,) <$> choice
        [ Data.Text.singleton <$> char '\x3009'
        , string "&#12297;"
        , asciiCI "&#x3009;"
        ]
    leftDoubleAngle :: Parser (TitlePunct, Text)
    leftDoubleAngle = (DoubleAngle,) <$> choice
        [ Data.Text.singleton <$> char '\x300a'
        , string "&#12298;"
        , asciiCI "&#x300a;"
        ]
    rightDoubleAngle :: Parser (TitlePunct, Text)
    rightDoubleAngle = (DoubleAngle,) <$> choice
        [ Data.Text.singleton <$> char '\x300b'
        , string "&#12299;"
        , asciiCI "&#x300b;"
        ]
    leftDoubleCorner :: Parser (TitlePunct, Text)
    leftDoubleCorner = (DoubleCorner,) <$> choice
        [ Data.Text.singleton <$> char '\x300e'
        , string "&#12302;"
        , asciiCI "&#x300e;"
        ]
    rightDoubleCorner :: Parser (TitlePunct, Text)
    rightDoubleCorner = (DoubleCorner,) <$> choice
        [ Data.Text.singleton <$> char '\x300f'
        , string "&#12303;"
        , asciiCI "&#x300f;"
        ]

data TitlePunct
    = DoubleAngle | Angle
    | DoubleCorner | Corner
    | DoubleInequal | Inequal
    deriving (Eq, Show)


-- | A set of stops—'period', 'comma', and 'interpunct'—to be used by
-- 'normalizeStops' function.
--
-- There are three presets: 'horizontalStops', 'verticalStops', and
-- 'horizontalStopsWithSlashes'.
data Stops = Stops
    { period :: Text
    , comma :: Text
    , interpunct :: Text
    } deriving (Eq, Show)

-- | Stop sentences in the modern Korean style which follows Western stops.
-- E.g.:
--
-- > 봄·여름·가을·겨울. 어제, 오늘.
horizontalStops :: Stops
horizontalStops = Stops
    { period = ". "
    , comma = ", "
    , interpunct = "·"
    }

-- | Stop sentences in the pre-modern Korean style which follows Chinese stops.
-- E.g.:
--
-- > 봄·여름·가을·겨울。어제、오늘。
verticalStops :: Stops
verticalStops = Stops
    { period = "。"
    , comma = "、"
    , interpunct = "·"
    }

-- | Similar to 'horizontalStops' except slashes are used instead of
-- interpuncts. E.g.:
--
-- > 봄/여름/가을/겨울. 어제, 오늘.
horizontalStopsWithSlashes :: Stops
horizontalStopsWithSlashes = Stops
    { period = ". "
    , comma = ", "
    , interpunct = "/"
    }


-- | Normalizes sentence stops (periods, commas, and interpuncts).
normalizeStops :: Stops -> [HtmlEntity] -> [HtmlEntity]
normalizeStops stops input = (`fmap` annotatedEntities) $ \ case
    LangHtmlEntity { lang = l
                   , entity = e@HtmlText { tagStack = stack, rawText = txt }
                   } ->
        if isPreservedTagStack stack || isNeverKorean l
        then e
        else e { rawText = replaceText txt }
    LangHtmlEntity { entity = e } ->
        e
  where
    annotatedEntities :: [LangHtmlEntity]
    annotatedEntities = (annotateWithLang . normalizeText) input
    replaceText :: Text -> Text
    replaceText txt =
        case parseOnly parser txt of
            Left _ -> error "unexpected error: failed to parse text node"
            Right t -> t
    parser :: Parser Text
    parser = do
        chunks <- many' $ choice
            [ stops'
            , Data.Text.singleton <$> anyChar
            ]
        endOfInput
        return $ Data.Text.concat chunks
    stops' :: Parser Text
    stops' = choice
        [ do { ending <- period'
             ; return (toEntity $ adjustEnding ending $ period stops)
             }
        , do { ending <- comma'
             ; return (toEntity $ adjustEnding ending $ comma stops)
             }
        , do { ending <- interpunct'
             ; return (toEntity $ adjustEnding ending $ interpunct stops)
             }
        ]
    adjustEnding :: Ending -> Text -> Text
    adjustEnding ending text
      | Data.Text.length text > 0 && isSpace (Data.Text.last text) =
            stripEnd text <> case ending of { TrailingChars c -> c
                                            ; TrailingSpaces s -> s
                                            ; Ending -> Data.Text.empty
                                            }
      | otherwise = text <> case ending of { TrailingChars c -> c
                                           ; _ -> Data.Text.empty
                                           }
    toEntity :: Text -> Text
    toEntity = Data.Text.concatMap $ \ c ->
        if c < '\x80' -- ASCII compatible characters
        then Data.Text.singleton c
        else Data.Text.concat ["&#x", pack $ showHex (fromEnum c) "", ";"]
    period' :: Parser Ending
    period' = choice
        [ char '.' >> boundary
        , char '。' >> trailingSpaces
        , string "&period;" >> boundary
        , string "&#46;" >> boundary
        , string "&#12290;" >> trailingSpaces
        , asciiCI "&#x2e;" >> boundary
        , asciiCI "&#x3002;" >> trailingSpaces
        ]
    comma' :: Parser Ending
    comma' = choice
        [ char '、' >> trailingSpaces
        , string "," >> boundary
        , string "&comma;" >> boundary
        , string "&#44;" >> boundary
        , string "&#12289;" >> trailingSpaces
        , asciiCI "&#x2c;" >> boundary
        , asciiCI "&#x3001;" >> trailingSpaces
        ]
    interpunct' :: Parser Ending
    interpunct' = choice
        [ char '·' >> return ""
        , string "&middot;"
        , string "&centerdot;"
        , string "&CenterDot;"
        , string "&#183;"
        , asciiCI "&#xb7;"
        ] >> return Ending
    closingChars :: String
    closingChars =
        [ '"', '”', '\'', '’', ')', ']', '}', '」', '』', '〉', '》', '）', '〕'
        , '］', '｝', '｠', '】', '〗', '〙', '〛', '›', '»'
        ]
    closingEntities :: [Text]
    closingEntities =
        [ "&quot;", "&QUOT;"                               -- "
        , "&apos;"                                         -- '
        , "&rpar;"                                         -- )
        , "&rsqb;", "&rbrack;"                             -- ]
        , "&rcub;", "&rbrace;"                             -- }
        , "&raquo;"                                        -- »
        , "&rsquo;", "&rsquor;", "&CloseCurlyQuote;"       -- ’
        , "&rdquo;", "&rdquor;", "&CloseCurlyDoubleQuote;" -- ”
        , "&rsaquo;"                                       -- ›
        ]
    closing :: Parser Text
    closing = choice $
        [string [c] | c <- closingChars] ++
        [string e | e <- closingEntities] ++
        [asciiCI $ pack $ "&#x" ++ showHex (fromEnum c) "" ++ ";"
        | c <- closingChars
        ] ++
        [string $ "&#" <> pack (show c) <> ";" | c <- closingChars]
    ending' :: Parser Ending
    ending' = choice
        [ endOfInput >> return Ending
        , TrailingChars <$> closing
        ]
    boundary :: Parser Ending
    boundary = choice
        [ ending'
        , TrailingSpaces <$> takeWhile1 isSpace
        ]
    trailingSpaces :: Parser Ending
    trailingSpaces = choice
        [ boundary
        , return $ TrailingSpaces " "
        ]


data Ending = TrailingChars Text | TrailingSpaces Text | Ending


-- | Substitution options for 'transformArrow' function.  These options can
-- be composited as an element of a set.
--
-- - @[]@: Transform only leftwards and rightwards arrows.
-- - @['LeftRight']@: Transform bi-directional arrows as well as left/rightwards
-- arrows.
-- - @['DoubleArrow']@: Transform double arrows as well as single arrows.
-- - @['LeftRight', 'DoubleArrow']@: Transform all types of arrows.
data ArrowTransformationOption
    -- | A bidirect arrow (e.g., ↔︎).
    = LeftRight
    -- | An arrow which has two lines (e.g., ⇒).
    | DoubleArrow
    deriving (Eq, Ord, Show)

-- | Transforms hyphens and less-than and greater-than inequality symbols that
-- mimic arrows into actual arrow characters:
--
-- - @->@ turns into @→@ (U+2192 RIGHTWARDS ARROW).
-- - @<-@ turns into @←@ (U+2190 LEFTWARDS ARROW).
-- - @\<->@ turns into @↔@ (U+2194 LEFT RIGHT ARROW)
--   if 'LeftRight' is configured.
-- - @=>@ turns into @⇒@ (U+21D2 RIGHTWARDS DOUBLE ARROW)
--   if 'DoubleArrow' is configured.
-- - @<=@ turns into @⇐@ (U+21D0 LEFTWARDS DOUBLE ARROW)
--   if 'DoubleArrow' is configured.
-- - @\<=>@ turns into @⇔@ (U+21D4 LEFT RIGHT DOUBLE ARROW)
--   if both 'DoubleArrow' and 'LeftRight' are configured at a time.
transformArrow :: Set ArrowTransformationOption -> [HtmlEntity] -> [HtmlEntity]
transformArrow options input = (`fmap` normalizeText input) $ \ case
    e@HtmlText { tagStack = stack, rawText = txt } ->
        if isPreservedTagStack stack
        then e
        else e { rawText = replaceText txt }
    e ->
        e
  where
    replaceText :: Text -> Text
    replaceText txt =
        case parseOnly parser txt of
            Left _ -> error "unexpected error: failed to parse text node"
            Right t -> t
    specialChars :: Set Char
    specialChars = if DoubleArrow `Prelude.elem` options
       then ['<', '>', '&', '-', '=']
       else ['<', '>', '&', '-']
    parser :: Parser Text
    parser = do
        chunks <- many' $ choice
            [ takeWhile1 (`notElem` specialChars)
            , choice arrows
            , Data.Text.singleton <$> anyChar
            ]
        endOfInput
        return $ Data.Text.concat chunks
    arrows :: [Parser Text]
    arrows = catMaybes
        [ if DoubleArrow `Prelude.elem` options
             && LeftRight `Prelude.elem` options
          then Just doubleLeftRight
          else Nothing
        , if DoubleArrow `Prelude.elem` options
          then Just doubleLeft
          else Nothing
        , if DoubleArrow `Prelude.elem` options
          then Just doubleRight
          else Nothing
        , if LeftRight `Prelude.elem` options
          then Just leftRight
          else Nothing
        , Just left
        , Just right
        ]
    doubleLeftRight :: Parser Text
    doubleLeftRight = lt >> equals >> gt >> return "&hArr;"
    doubleLeft :: Parser Text
    doubleLeft = lt >> equals >> return "&lArr;"
    doubleRight :: Parser Text
    doubleRight = equals >> gt >> return "&rArr;"
    leftRight :: Parser Text
    leftRight = lt >> hyphen >> gt >> return "&harr;"
    left :: Parser Text
    left = lt >> hyphen >> return "&larr;"
    right :: Parser Text
    right = hyphen >> gt >> return "&rarr;"
    hyphen :: Parser ()
    hyphen = void $ choice
        [ char '-' >> return ""
        , string "&hyphen;"
        , string "&dash;"
        , string "&#45;"
        , asciiCI "&#x2d;"
        ]
    equals :: Parser ()
    equals = void $ choice
        [ char '=' >> return ""
        , string "&equals;"
        , string "&61;"
        , asciiCI "&#x3d;"
        ]

lt :: Parser Text
lt = choice
    [ Data.Text.singleton <$> char '<'
    , string "&lt;"
    , string "&#60;"
    , asciiCI "&#x3c;"
    ]

gt :: Parser Text
gt = choice
    [ Data.Text.singleton <$> char '>'
    , string "&gt;"
    , string "&#62;"
    , asciiCI "&#x3e;"
    ]

-- | Until 2015, the National Institute of Korean Language (國立國語院) had
-- allowed to use only ellipses (@…@) for omitted word, phrase, line,
-- paragraph, or speechlessness.  However, people tend to use three or more
-- consecutive periods (@...@) instead of a proper ellipsis.
-- Although NIKL has started to allow consecutive periods besides an ellipsis
-- for these uses, ellipses are still a proper punctuation in principle.
--
-- This transforms, in the given HTML fragments, all three consecutive periods
-- into proper ellipses.
transformEllipsis :: [HtmlEntity] -> [HtmlEntity]
transformEllipsis = transformText $ \ txt ->
    case parseOnly parser txt of
        Left _ -> error "unexpected error: failed to parse text node"
        Right t -> t
  where
    parser :: Parser Text
    parser = do
        chunks <- many' $ choice
            [ takeWhile1 (`notElem` (['&', '.', '。'] :: Set Char))
            , ellipsis
            , Data.Text.singleton <$> anyChar
            ]
        endOfInput
        return $ Data.Text.concat chunks
    ellipsis :: Parser Text
    ellipsis = do
        void $ choice
            [ period >> period >> period
            , chinesePeriod >> chinesePeriod >> chinesePeriod
            ]
        return "&hellip;"
    period :: Parser Text
    period = choice
        [ string "."
        , string "&period;"
        , string "&#46;"
        , asciiCI "&#x2e;"
        ]
    chinesePeriod :: Parser Text
    chinesePeriod = choice
        [ string "。"
        , string "&#12290;"
        , asciiCI "&#x3002;"
        ]

-- | Pairs of substitute folk single and double quotes.
-- Used by 'transformQuote' function.
--
-- The are three presets: 'curvedQuotes', 'guillemets', and
-- 'curvedSingleQuotesWithQ':
--
-- - 'curvedQuotes' uses South Korean curved quotation marks which follows
--   English quotes (@‘@: U+2018, @’@: U+2019, @“@: U+201C, @”@: U+201D)
-- - 'guillemets' uses North Korean angular quotation marks, influenced
--   by Russian guillemets but with some adjustments to replace guillemets with
--   East Asian angular quotes (@〈@: U+3008, @〉@: U+3009, @《@: U+300A,
--   @》@: U+300B).
-- - 'curvedSingleQuotesWithQ' is the almost same to 'curvedQuotes' but
--   wrap text with a @\<q>@ tag instead of curved double quotes.
data Quotes = Quotes
    { singleQuotes :: QuotePair
    , doubleQuotes :: QuotePair
    } deriving (Eq, Ord, Show)

-- | A pair of an opening quote and a closing quote.
data QuotePair
    -- | Wrap the quoted text with a pair of punctuation characters.
    = QuotePair Text Text
    -- | Wrap the quoted text (HTML elements) with an element like @\<q>@ tag.
    | HtmlElement HtmlTag HtmlRawAttrs
    deriving (Eq, Ord, Show)

-- | English-style curved quotes (@‘@: U+2018, @’@: U+2019, @“@: U+201C,
-- @”@: U+201D), which are used by South Korean orthography.
curvedQuotes :: Quotes
curvedQuotes = Quotes
    { singleQuotes = QuotePair "&lsquo;" "&rsquo;"
    , doubleQuotes = QuotePair "&ldquo;" "&rdquo;"
    }

-- | Vertical corner brackets (@﹁@: U+FE41, @﹂@: U+FE42, @﹃@: U+FE43,
-- @﹄@: U+FE44), which are used by East Asian orthography.
verticalCornerBrackets :: Quotes
verticalCornerBrackets = Quotes
    { singleQuotes = QuotePair "&#xfe41;" "&#xfe42;"
    , doubleQuotes = QuotePair "&#xfe43;" "&#xfe44;"
    }

-- | Traditional horizontal corner brackets (@「@: U+300C, @」@: U+300D,
-- @『@: U+300E, @』@: U+300F), which are used by East Asian orthography.
horizontalCornerBrackets :: Quotes
horizontalCornerBrackets = Quotes
    { singleQuotes = QuotePair "&#x300c;" "&#x300d;"
    , doubleQuotes = QuotePair "&#x300e;" "&#x300f;"
    }

-- | East Asian guillemets (@〈@: U+3008, @〉@: U+3009, @《@: U+300A, @》@:
-- U+300B), which are used by North Korean orthography.
guillemets :: Quotes
guillemets = Quotes
    { singleQuotes = QuotePair "&#x3008;" "&#x3009;"
    , doubleQuotes = QuotePair "&#x300a;" "&#x300b;"
    }

-- | Use English-style curved quotes (@‘@: U+2018, @’@: U+2019) for single
-- quotes, and HTML @\<q\>@ tags for double quotes.
curvedSingleQuotesWithQ :: Quotes
curvedSingleQuotesWithQ = Quotes
    { singleQuotes = QuotePair "&lsquo;" "&rsquo;"
    , doubleQuotes = HtmlElement Q ""
    }

-- | Use vertical corner brackets (@﹁@: U+FE41, @﹂@: U+FE42) for single quotes,
-- and HTML @\<q\>@ tags for double quotes.
verticalCornerBracketsWithQ :: Quotes
verticalCornerBracketsWithQ = Quotes
    { singleQuotes = QuotePair "&#xfe41;" "&#xfe42;"
    , doubleQuotes = HtmlElement Q ""
    }

-- | Use horizontal corner brackets (@「@: U+300C, @」@: U+300D)
-- for single quotes, and HTML @\<q\>@ tags for double quotes.
horizontalCornerBracketsWithQ :: Quotes
horizontalCornerBracketsWithQ = Quotes
    { singleQuotes = QuotePair "&#x300c;" "&#x300d;"
    , doubleQuotes = HtmlElement Q ""
    }

-- | Transform pairs of apostrophes (@'@: U+0027) and straight double
-- quotes (@"@: U+0022) into more appropriate quotation marks like
-- typographic single quotes (@‘@: U+2018, @’@: U+2019) and
-- double quotes (@“@: U+201C, @”@: U+201D), or rather wrap them with an HTML
-- element like @\<q>@ tag.  It depends on the options passed to the first
-- parameter; see also 'Quotes'.
transformQuote :: Quotes -- ^ Pair of quoting punctuations and wrapping element.
               -> [HtmlEntity] -- ^ The input HTML entities to transform.
               -> [HtmlEntity]
transformQuote Quotes { .. } = transformPairs $
    PairedTransformer
        { ignoresTagStack = isPreservedTagStack
        , matchStart = matchStart'
        , matchEnd = matchEnd'
        , areMatchesPaired = \ (punct, text) (punct', text') ->
            arePaired punct punct' && text == text'
        , transformPair = transformPair'
        }
  where
    punctuations :: [(QuotePunct, [Text])]
    punctuations =
        [ ( Apostrophe
          , ["'", "&apos;", "&#39;", "&#x27;", "&#X27;"]
          )
        , ( DoubleQuote
          , ["\"", "&quot;", "&QUOT;", "&#34;", "&#x22;", "&#X22;"]
          )
        , ( DoubleQuote
          , ["\"", "&quot;", "&QUOT;", "&#34;", "&#x22;", "&#X22;"]
          )
        , ( OpeningSingleQuote
          , [ "\x2018", "&lsquo;", "&OpenCurlyQuote;"
            , "&#8216;", "&#x2018;", "&#X2018;"
            ]
          )
        , ( ClosingSingleQuote
          , [ "\x2019", "&rsquo;", "&rsquor;", "&CloseCurlyQuote;"
            , "&#8217;", "&#x2019;", "&#X2019;"
            ]
          )
        , ( OpeningDoubleQuote
          , [ "\x201c", "&ldquo;", "&OpenCurlyDoubleQuote;"
            , "&#8220;", "&#x201c;", "&#x201C;", "&#X201c;", "&#X201C;"
            ]
          )
        , ( ClosingDoubleQuote
          , [ "\x201d", "&rdquo;", "&rdquor;", "&CloseCurlyDoubleQuote;"
            , "&#8221;", "&#x201d;", "&#x201D;", "&#X201d;", "&#X201D;"
            ]
          )
        ]
    matchStart' :: [(QuotePunct, Text)]
                -> Text
                -> Maybe ((QuotePunct, Text), Text, Text, Text)
    matchStart' prevMatches text
      | Prelude.null prevMatcherCandidates = Nothing
      | otherwise =
            let (matcher, entity, (pre, post)) = minimumBy
                    (comparing $ \ (_, _, (pre', _)) -> Data.Text.length pre')
                    prevMatcherCandidates
            in
                if Data.Text.null post then
                   Nothing
                else
                    Just
                        ( (matcher, entity)
                        , pre
                        , entity
                        , Data.Text.drop (Data.Text.length entity) post
                        )
      where
        prevMatchers :: Set QuotePunct
        prevMatchers = Data.Set.fromList (fst <$> prevMatches)
        prevMatcherCandidates :: [(QuotePunct, Text, (Text, Text))]
        prevMatcherCandidates =
            [ (matcher', entity', breakOn entity' text)
            | (matcher', entities) <- punctuations
            , opens matcher'
            , matcher' `Data.Set.notMember` prevMatchers
            , entity' <- entities
            ]
    matchEnd' :: Text -> Maybe ((QuotePunct, Text), Text, Text, Text)
    matchEnd' text
      | Prelude.null matcherCandidates = Nothing
      | otherwise =
            let (matcher, entity, (pre, post)) = minimumBy
                    (comparing $ \ (_, _, (pre', _)) -> Data.Text.length pre')
                    matcherCandidates
            in
                if Data.Text.null post then
                    Nothing
                else
                    Just
                        ( (matcher, entity)
                        , pre
                        , entity
                        , Data.Text.drop (Data.Text.length entity) post
                        )
      where
        matcherCandidates :: [(QuotePunct, Text, (Text, Text))]
        matcherCandidates =
            [ (matcher', entity', breakOn entity' text)
            | (matcher', entities) <- punctuations
            , closes matcher'
            , entity' <- entities
            ]
    transformPair' :: (QuotePunct, Text)
                   -> (QuotePunct, Text)
                   -> [HtmlEntity]
                   -> [HtmlEntity]
    transformPair' (punct, start) (_, end) buffer@(firstEntity : _) =
        case clipText start end buffer of
            Nothing -> buffer
            Just es -> case pair of
                QuotePair open close ->
                    HtmlText tagStack' open : es ++ [HtmlText tagStack' close]
                HtmlElement tag attrs ->
                    wrap tagStack' tag attrs es
      where
        pair :: QuotePair
        pair = case punct of
            DoubleQuote -> doubleQuotes
            OpeningDoubleQuote -> doubleQuotes
            ClosingDoubleQuote -> doubleQuotes
            _ -> singleQuotes
        tagStack' :: HtmlTagStack
        tagStack' = tagStack firstEntity
    transformPair' _ _ [] = []
    arePaired :: QuotePunct -> QuotePunct -> Bool
    arePaired OpeningSingleQuote = (== ClosingSingleQuote)
    arePaired OpeningDoubleQuote = (== ClosingDoubleQuote)
    arePaired punct = (== punct)

data QuotePunct
    = DoubleQuote
    | Apostrophe
    | OpeningSingleQuote | ClosingSingleQuote
    | OpeningDoubleQuote | ClosingDoubleQuote
    deriving (Eq, Ord, Show)

opens :: QuotePunct -> Bool
opens DoubleQuote = True
opens Apostrophe = True
opens OpeningSingleQuote = True
opens OpeningDoubleQuote = True
opens _ = False

closes :: QuotePunct -> Bool
closes DoubleQuote = True
closes Apostrophe = True
closes ClosingSingleQuote = True
closes ClosingDoubleQuote = True
closes _ = False

-- | Transform the following folk em dashes into proper em dashes
-- (@—@: @U+2014 EM DASH@):
--
-- - A hyphen (@-@: @U+002D HYPHEN-MINUS@) surrounded by spaces.
-- - Two or three consecutive hyphens (@--@ or @---@).
-- - A hangul vowel @ㅡ@ (@U+3161 HANGUL LETTER EU@) surrounded by spaces.
--   There are Korean people that use a hangul vowel @ㅡ@ ("eu") instead of
--   an em dash due to their ignorance or negligence.
transformEmDash :: [HtmlEntity] -> [HtmlEntity]
transformEmDash = transformText $ \ txt ->
    case parseOnly parser txt of
        Left _ -> error "unexpected error: failed to parse text node"
        Right t -> t
  where
    parser :: Parser Text
    parser = do
        chunks <- many' $ choice
            [ takeWhile1 $ \ c ->
                not ( isSpace c
                    || c `Prelude.elem` (['&', '-', '\x3161'] :: Set Char)
                    )
            , emDash
            , Data.Text.singleton <$> anyChar
            ]
        endOfInput
        return $ Data.Text.concat chunks
    emDash :: Parser Text
    emDash = choice
        [ hyphens
        , takeWhile1 isSpace >> choice [eu, hyphen] >> takeWhile1 isSpace
        ] >> return "&mdash;"
    hyphens :: Parser Text
    hyphens = hyphen >> hyphen >> option "" hyphen
    hyphen :: Parser Text
    hyphen = choice $ Prelude.map string
        ["-", "&#45;", "&#x2d;", "&#x2D;", "&#X2d;", "&#X2D;"]
    eu :: Parser Text
    eu = choice $ Prelude.map string
        ["\x3161", "&#12641;", "&#x3161;", "&#X3161;"]

transformText :: (Text -> Text) -> [HtmlEntity] -> [HtmlEntity]
transformText replace' = fmap $ \ case
    e@HtmlText { tagStack = stack, rawText = txt } ->
        if isPreservedTagStack stack
        then e
        else e { rawText = replace' txt }
    e ->
        e
