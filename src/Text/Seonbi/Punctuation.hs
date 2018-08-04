{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Punctuation
    ( ArrowTransformationOption (..)
    , transformArrow
    ) where

import Control.Monad
import Data.Maybe

import Data.Attoparsec.Text
import Data.Set
import Data.Text hiding (any)

import Text.Seonbi.Html
import qualified Text.Seonbi.Html.TagStack as TagStack

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
    e@HtmlText { tagStack = stack, rawText = txt } -> if ignoresTagStack stack
        then e
        else e { rawText = replaceText txt }
    e ->
        e
  where
    ignoredTags :: Set HtmlTag
    ignoredTags = [Code, Kbd, Pre]
    ignoresTagStack :: HtmlTagStack -> Bool
    ignoresTagStack stack =
        any (`TagStack.elem` stack) ignoredTags ||
        TagStack.any ((\ case { Normal -> False
                              ; _ -> True
                              }) . htmlTagKind) stack
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
    lt :: Parser ()
    lt = void $ choice
        [ char '<' >> return ""
        , string "&lt;"
        , string "&#60;"
        , asciiCI "&#x3c;"
        ]
    gt :: Parser ()
    gt = void $ choice
        [ char '>' >> return ""
        , string "&gt;"
        , string "&#62;"
        , asciiCI "&#x3e;"
        ]
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
