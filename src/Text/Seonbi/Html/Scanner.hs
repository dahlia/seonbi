{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Text.Seonbi.Html.Scanner
    ( Result (..)
    , scanHtml
    ) where

import Data.Char
import Prelude hiding (takeWhile)

import Data.Attoparsec.Text.Lazy
import Data.Map.Strict
import qualified Data.Text
import qualified Data.Text.Lazy

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.TagStack

htmlFragments :: Parser [HtmlEntity]
htmlFragments = do
    result <- option [] $ fragments Text.Seonbi.Html.TagStack.empty
    txt <- htmlText Text.Seonbi.Html.TagStack.empty
    endOfInput
    return $ case txt of
        HtmlText { rawText = "" } -> result
        _ -> result ++ [txt]

fragments :: HtmlTagStack -> Parser [HtmlEntity]
fragments tagStack' = do
    txt <- htmlText tagStack'
    (entities, nextStack) <- htmlEntity tagStack'
    nextChunk <- option [] $ fragments nextStack
    let chunks = entities ++ nextChunk
    return $ case txt of
        HtmlText { rawText = "" } -> chunks
        txt' -> txt' : chunks

htmlText :: HtmlTagStack -> Parser HtmlEntity
htmlText tagStack' = do
    texts <- many' textFragment
    return $ mkText $ Data.Text.concat texts
  where
      mkText :: Data.Text.Text -> HtmlEntity
      mkText txt = HtmlText { tagStack = tagStack', rawText = txt }

textFragment :: Parser Data.Text.Text
textFragment = choice
    [ takeWhile1 (/= '<')
    , do
        a <- char '<'
        b <- satisfy $ \ c ->
            not (c == '!' || c == '/' || isAsciiUpper c || isAsciiLower c)
        return $ Data.Text.pack [a, b]
    ]

htmlEntity :: HtmlTagStack -> Parser ([HtmlEntity], HtmlTagStack)
htmlEntity tagStack' = choice
    [ htmlComment tagStack'
    , cdata tagStack'
    , startTag tagStack'
    , endTag tagStack'
    -- fallback:
    , (, tagStack') . (: []) . HtmlText tagStack' . Data.Text.singleton
        <$> anyChar
    ]

-- https://www.w3.org/TR/html5/syntax.html#comments
htmlComment :: HtmlTagStack -> Parser ([HtmlEntity], HtmlTagStack)
htmlComment tagStack' = do
    _ <- string "<!--"
    contents <- many' $ choice
        [ takeWhile1 (/= '-')
        , do
              a <- char '-'
              b <- notChar '-'
              return $ Data.Text.pack [a, b]
        , do
            a <- string "--"
            b <- notChar '>'
            return $ Data.Text.snoc a b
        ]
    _ <- string "-->"
    return
        ( [ HtmlComment
                { tagStack = tagStack'
                , comment = Data.Text.concat contents
                }
          ]
        , tagStack'
        )

-- https://www.w3.org/TR/html5/syntax.html#cdata-sections
cdata :: HtmlTagStack -> Parser ([HtmlEntity], HtmlTagStack)
cdata tagStack' = do
    _ <- string "<![CDATA["
    contents <- many' $ choice
        [ takeWhile1 (/= ']')
        , do
            a <- char ']'
            b <- notChar ']'
            return $ Data.Text.pack [a, b]
        , do
            a <- string "]]"
            b <- notChar '>'
            return $ Data.Text.snoc a b
        ]
    _ <- string "]]>"
    return
        ( [HtmlCdata { tagStack = tagStack', text = Data.Text.concat contents }]
        , tagStack'
        )

-- https://www.w3.org/TR/html5/syntax.html#start-tags
startTag :: HtmlTagStack -> Parser ([HtmlEntity], HtmlTagStack)
startTag tagStack' = do
    _ <- char '<'
    tag' <- htmlTag
    attributes <- many' $ choice
        [ do
            s <- char '"'
            c <- takeWhile (/= '"')
            e <- char '"'
            return (Data.Text.cons s $ Data.Text.snoc c e)
        , do
            s <- char '\''
            c <- takeWhile (/= '\'')
            e <- char '\''
            return (Data.Text.cons s $ Data.Text.snoc c e)
        , takeWhile1 $ \ c -> c /= '/' && c /= '>'
        ]
    selfClosing <- option ' ' $ char '/'
    _ <- char '>'
    let (trailingEntities, nextTagStack) =
            if selfClosing == '/' || htmlTagKind tag' == Void
            then ([HtmlEndTag { tagStack = tagStack', tag = tag' }], tagStack')
            else ([], push tag' tagStack')
    return
        ( HtmlStartTag
            { tagStack = tagStack'
            , tag = tag'
            , rawAttributes = Data.Text.concat attributes
            } : trailingEntities
        , nextTagStack
        )

-- https://www.w3.org/TR/html5/syntax.html#end-tags
endTag :: HtmlTagStack -> Parser ([HtmlEntity], HtmlTagStack)
endTag tagStack' = do
    _ <- string "</"
    tag' <- htmlTag
    _ <- char '>'
    return $ case htmlTagKind tag' of
        Void -> ([], tagStack')
        _ ->
            let
                nextTagStack = pop tag' tagStack'
            in
                ( [HtmlEndTag { tagStack = nextTagStack, tag = tag' }]
                , nextTagStack
                )

htmlTag :: Parser HtmlTag
htmlTag = do
    name <- tagName
    case Data.Map.Strict.lookup (Data.Text.toLower name) htmlTagNames of
        Just t -> return t
        _ -> fail ("failed to parse; invalid tag: " ++ Data.Text.unpack name)

tagName :: Parser Data.Text.Text
tagName = do
    first <- satisfy $ \ c -> isAsciiUpper c || isAsciiLower c
    rest <- takeWhile $ \ c -> isAsciiUpper c || isAsciiLower c || isDigit c
    return $ Data.Text.cons first rest

scanHtml :: Data.Text.Lazy.Text -> Result [HtmlEntity]
scanHtml = parse htmlFragments
