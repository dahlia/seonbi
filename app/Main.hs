{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char
import Data.Maybe
import Prelude hiding (getContents, putStr)
import System.Exit
import System.IO (hPutStrLn, stderr)

import Codec.Text.IConv
import Data.ByteString.Lazy
import Data.Set
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Options.Applicative
import Text.Html.Encoding.Detection (detect)

import Text.Seonbi.Html
import Text.Seonbi.Punctuation

toUnicode :: EncodingName -> ByteString -> Text
toUnicode encodingName =
    case normalizeEncodingName encodingName of
        "utf8" -> decodeUtf8
        "utf16le" -> decodeUtf16LE
        "utf16be" -> decodeUtf16BE
        "utf32le" -> decodeUtf32LE
        "utf32be" -> decodeUtf32BE
        _ -> decodeUtf8 . convert encodingName "UTF-8"

fromUnicode :: EncodingName -> Text -> ByteString
fromUnicode encodingName =
    case normalizeEncodingName encodingName of
        "utf8" -> encodeUtf8
        "utf16le" -> encodeUtf16LE
        "utf16be" -> encodeUtf16BE
        "utf32le" -> encodeUtf32LE
        "utf32be" -> encodeUtf32BE
        _ -> convert "UTF-8" encodingName . encodeUtf8

-- | Normalize the encoding name.
--
-- >>> normalizeEncodingName "UTF-8"
-- "utf8"
-- >>> normalizeEncodingName "UTF-16LE"
-- "utf16le"
-- >>> normalizeEncodingName "EUC-KR"
-- "euckr"
normalizeEncodingName :: EncodingName -> EncodingName
normalizeEncodingName =
    Prelude.filter (\ c -> isAscii c && isAlphaNum c) . fmap Data.Char.toLower

data Seonbi = Seonbi
    { encoding :: String
    , xhtml :: Bool
    , leftRight :: Bool
    , doubleArrow :: Bool
    } deriving (Eq, Show)

parser :: Parser Seonbi
parser = Seonbi
    <$> strOption
        ( long "encoding"
        <> short 'e'
        <> metavar "ENCODING"
        <> value ""
        <> help "Character encoding (e.g., UTF-8, EUC-KR)"
        )
    <*> switch
        ( long "xhtml"
        <> short 'x'
        <> help "XHTML mode"
        )
    <*> flag True False
        ( long "no-two-way-arrows"
        <> short 'T'
        <> help "Do not transform two-way arrows (<->, <=>)"
        )
    <*> flag True False
        ( long "no-double-arrows"
        <> short 'D'
        <> help "Do not transform double arrows (<=, =>, <=>)"
        )

parserInfo :: ParserInfo Seonbi
parserInfo = info (parser <**> helper)
    ( fullDesc
    <> progDesc "Korean typographic adjustment processor"
    )

main :: IO ()
main = do
    options <- execParser parserInfo
    contents <- getContents
    let encodingName = case encoding options of
            "" -> fromMaybe "UTF-8" $ detect contents
            enc -> enc
    let arrowOptions = Data.Set.fromList $ catMaybes
            [ if leftRight options then Just LeftRight else Nothing
            , if doubleArrow options then Just DoubleArrow else Nothing
            ]
    let print' = if xhtml options then printXhtml else printHtml
    let result = scanHtml $ toUnicode encodingName contents
    case result of
        Done "" input ->
            let
                output = transformArrow arrowOptions input
            in
                putStr $ fromUnicode encodingName $ print' output
        _ -> do
            hPutStrLn stderr "error: failed to parse input"
            exitFailure
