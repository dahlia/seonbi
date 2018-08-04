{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Text.Seonbi.Punctuation
    ( ArrowTransformationOption (..)
    , CitationQuotes (..)
    , angleQuotes
    , cornerBrackets
    , quoteCitation
    , transformArrow
    ) where

import Prelude hiding (takeWhile)

import Control.Monad
import Data.Either
import Data.Maybe

import Data.Attoparsec.Text
import Data.Set
import Data.Text hiding (any, length, takeWhile)

import Text.Seonbi.Html
import qualified Text.Seonbi.Html.TagStack as TagStack
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
      -- novel, newspaper, magazine, movie, televison program, etc.
      title :: (Text, Text)
    , -- | The leading and trailing punctuations to surround a title of
      -- short story, chapter, article, episode, etc.
      subtitle :: (Text, Text)
    , -- | Optional pair of an HTML element and its attributes to surround
      -- citations.  E.g., if it is @'Just' ('Cite', " class=\"autogen\")@
      -- titles are transformed like @\<cite class="autogen">이런 날\</cite>@.
      htmlElement :: Maybe (HtmlTag, HtmlRawAttrs)
    }

-- | Cite a title using angle quotes,
-- e.g., 《나비와 엉겅퀴》 or 〈枾崎의 바다〉.
angleQuotes :: CitationQuotes
angleQuotes = CitationQuotes
    { title = ("&#12298;", "&#12299;")
    , subtitle = ("&#12296;", "&#12297;")
    , htmlElement = Just (Cite, "")
    }

-- | Cite a title using corner brackets,
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
        { ignoresTagStack = ignoresTagStack'
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


-- | Substitution options for 'transformArrow' function.  These options can
-- be composited as an element of a set.
--
-- - @[]@: Transform only leftwards and rightwards arrows.
-- - @['LeftRight']@: Transform bi-directional arrows as well as left/rightwards
-- arrows.
-- - @['DoubleArrow']@: Transform double arrows as well as single arrows.
-- - @['LeftRight', 'DoubleArrow']@: Transform all types of arrows.
data ArrowTransformationOption
    = LeftRight
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
    e@HtmlText { tagStack = stack, rawText = txt } -> if ignoresTagStack' stack
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
    specialChars = if DoubleArrow `elem` options
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
        [ if DoubleArrow `elem` options && LeftRight `elem` options
             then Just doubleLeftRight
             else Nothing
        , if DoubleArrow `elem` options then Just doubleLeft else Nothing
        , if DoubleArrow `elem` options then Just doubleRight else Nothing
        , if LeftRight `elem` options then Just leftRight else Nothing
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

ignoredTags :: Set HtmlTag
ignoredTags = [Code, Kbd, Pre]

ignoresTagStack' :: HtmlTagStack -> Bool
ignoresTagStack' stack =
    any (`TagStack.elem` stack) ignoredTags ||
        TagStack.any (not . isNormal . htmlTagKind) stack
  where
    isNormal :: HtmlTagKind -> Bool
    isNormal Normal = True
    isNormal _ = False

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
