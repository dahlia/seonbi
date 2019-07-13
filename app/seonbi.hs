{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Version
import Prelude hiding (getContents, putStr)
import System.Exit
import System.IO (hPutStrLn, stderr)

import Cases
import Codec.Text.IConv
import Data.ByteString.Lazy
import qualified Data.Text as T
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Options.Applicative
import Text.Html.Encoding.Detection (detect)

import qualified Paths_seonbi as Meta
import Text.Seonbi.Facade

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
    , config :: Configuration
    , debug :: Bool
    , version :: Bool
    } deriving (Eq, Show)

-- | Similar to 'auto', except it uses @spinal-case@ instead of @PascalCase@.
enum :: Read a => ReadM a
enum = eitherReader $ \ arg -> case reads (pascalize arg) of
    [(r, "")] -> return r
    _ -> Left $ "cannot parse value `" ++ arg ++ "'"
  where
    pascalize :: String -> String
    pascalize = T.unpack . process title camel . T.pack

enumKeyword' :: (Enum a, Show a) => a -> T.Text
enumKeyword' = spinalize . T.pack . show

enumKeyword :: (Enum a, Show a) => a -> String
enumKeyword = T.unpack . enumKeyword'

enumKeywords :: forall a . (Enum a, Show a) => Proxy a -> String
enumKeywords _ = T.unpack $ T.intercalate ", " $
    fmap enumKeyword' [(toEnum 0 :: a) ..]

parser :: Parser Seonbi
parser = Seonbi
    <$> strOption
        ( long "encoding"
        <> short 'e'
        <> metavar "ENCODING"
        <> value ""
        <> help "Character encoding (e.g., UTF-8, EUC-KR)"
        )
    <*> ( Configuration
        <$> ( flag' Nothing
                ( long "no-quote"
                <> short 'Q'
                <> help "Do not transform any quotes at all"
                )
            <|> option (fmap Just enum)
                ( long "quote"
                <> short 'q'
                <> metavar "QUOTE_STYLE"
                <> value (Just CurvedQuotes)
                <> help ("Quoting style.  Available styles: " ++
                         enumKeywords (Proxy :: Proxy QuoteOption) ++
                         "  [default: " ++ enumKeyword CurvedQuotes ++ "]")
                )
            )
        <*> option (fmap Just enum)
            ( long "cite"
            <> short 'c'
            <> metavar "CITE_STYLE"
            <> value Nothing
            <> help ("Transform citating quotes.  Available styles: " ++
                     enumKeywords (Proxy :: Proxy CiteOption))
            )
        <*> ( flag' Nothing
                ( long "no-arrow"
                <> short 'A'
                <> help "Do not transform any arrows at all"
                )
            <|> ( fmap Just . ArrowOption
                <$> switch
                    ( long "bidir-arrow"
                    <> short 'b'
                    <> help "Transform bi-directional arrows as well"
                    )
                <*> switch
                    ( long "double-arrow"
                    <> short 'd'
                    <> help "Transform double arrows as well"
                    )
                )
            )
        <*> switch
            ( long "ellipsis"
            <> short 'E'
            <> help "Transform triple periods into a proper ellipsis"
            )
        <*> ( flag' Nothing 
                ( long "maintain-hanja"
                <> short 'H'
                <> help "Leave Sino-Korean words as are"
                )
            <|> ( fmap Just . HanjaOption
                <$> option enum
                    ( long "render-hanja"
                    <> short 'r'
                    <> metavar "RENDERING_STYLE"
                    <> value DisambiguatingHanjaInParentheses
                    <> help ("How to render Sino-Korean words.  " ++
                             "Available styles: " ++
                             enumKeywords (Proxy :: Proxy HanjaRenderingOption)
                             ++ "  [default: " ++
                             enumKeyword DisambiguatingHanjaInParentheses ++
                             "]")
                    )
                <*> ( HanjaReadingOption []
                    <$> flag True False 
                        ( long "no-initial-sound-law"
                        <> short 'I'
                        <> help ("Do not apply Initial Sound Law (頭音法則) " ++
                                 "Sino-Korean words")
                        )
                    )
                )
            )
        <*> switch
            ( long "xhtml"
            <> short 'x'
            <> help "XHTML mode"
            )
        )
    <*> switch
        ( long "debug"
        <> help "Debug mode"
        )
    <*> switch
        ( long "version"
        <> short 'v'
        <> help "Show version"
        )

parserInfo :: ParserInfo Seonbi
parserInfo = info (parser <**> helper)
    ( fullDesc
    <> progDesc "Korean typographic adjustment processor"
    )

main :: IO ()
main = do
    options@Seonbi { encoding, config, debug, version } <- execParser parserInfo
    when version $ do
        Prelude.putStrLn $ showVersion Meta.version
        exitSuccess
    let whenDebug = when debug
    let debugPrint = whenDebug . hPutStrLn stderr
    debugPrint ("options: " ++ show options)
    contents <- getContents
    let encodingName = case encoding of
            "" -> fromMaybe "UTF-8" $ detect contents
            enc -> enc
    debugPrint ("encoding: " ++ encodingName)
    let result = transformHtmlLazyText config $ toUnicode encodingName contents
    case result of
        Just output ->
            putStr $ fromUnicode encodingName output
        Nothing -> do
            hPutStrLn stderr "error: failed to parse input"
            exitFailure
