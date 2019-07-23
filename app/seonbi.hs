{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Data.Char
import Data.List (intercalate)
import Data.Maybe
import Data.Proxy
import Data.Version
import Prelude hiding (getContents, putStr, readFile, writeFile)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.IO.Error

import Cases
import Codec.Text.IConv
import Data.ByteString.Lazy
import Data.Map.Strict
import qualified Data.Text as T
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Options.Applicative
import Text.Html.Encoding.Detection (detect)

import qualified Paths_seonbi as Meta
import Text.Seonbi.Facade
import Text.Seonbi.Trie as Trie
import Text.Seonbi.Html.Entity

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
    { output :: FilePath
    , encoding :: String
    , config :: Configuration IO ()
    , dictionaries :: [FilePath]
    , noKrStdict :: Bool
    , debug :: Bool
    , version :: Bool
    , input :: FilePath
    } deriving (Show)

presets :: Map String (Configuration IO ())
presets =
    [ ("ko-kp", ko_KP)
    , ("ko-kr", ko_KR)
    ]

preset :: ReadM (Configuration IO ())
preset = eitherReader $ \ arg ->
    case Data.Map.Strict.lookup (normalize <$> arg) presets of
        Just c -> Right c
        _ -> Left $ "no such preset: \"" ++ arg ++ "\""
  where
    normalize :: Char -> Char
    normalize = Data.Char.toLower . hyphenize
    hyphenize :: Char -> Char
    hyphenize '_' = '-'
    hyphenize c = c

hanjaReading :: ReadM (T.Text, T.Text)
hanjaReading = eitherReader $ \ arg ->
    case T.breakOn ":" $ T.pack arg of
        (_, "") -> Left $ "colon is missing: \"" ++ arg ++ "\""
        ("", _) -> Left $ "hanja writing is missing: \"" ++ arg ++ "\""
        (_, ":") -> Left $ "phonetic reading is missing: \"" ++ arg ++ "\""
        (writing, reading) -> Right (writing, T.drop 1 reading)

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
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> value "-"
        <> help ("Output file path.  A hyphen (-) means standard output.  " ++
                 "To specify an actual file named \"-\", prepend its " ++
                 "relative path, e.g., \"./-\"  [default: -]")
        )
    <*> strOption
        ( long "encoding"
        <> short 'e'
        <> metavar "ENCODING"
        <> value ""
        <> help "Character encoding (e.g., UTF-8, EUC-KR)"
        )
    <*> ( option preset
            ( long "preset"
            <> short 'p'
            <> help ("Use a preset instead of below style settings (this " ++
                     "resjects any other style options below).  " ++
                     "Available presets: " ++
                     Data.List.intercalate ", " (Data.Map.Strict.keys presets))
            )
        <|> ( Configuration Nothing
            <$> switch
                ( long "xhtml"
                <> short 'x'
                <> help "XHTML mode"
                )
            <*> ( flag' Nothing
                    ( long "no-quote"
                    <> short 'Q'
                    <> help ("Do not transform any quotes at all.  " ++
                             "This rejects -q/--quote option")
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
                    <> help ("Do not transform any arrows at all.  " ++
                             "This rejects -b/--bidir-arrow and " ++
                             "-d/--double-arrow options")
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
                    <> help ("Leave Sino-Korean words as are.  This rejects " ++
                             "-r/--render-hanja and " ++
                             "-I/--no-initial-sound-law options")
                    )
                <|> ( fmap Just . HanjaOption
                    <$> option enum
                        ( long "render-hanja"
                        <> short 'r'
                        <> metavar "RENDERING_STYLE"
                        <> value DisambiguatingHanjaInParentheses
                        <> help ("How to render Sino-Korean words.  " ++
                                 "Available styles: " ++
                                 enumKeywords
                                    (Proxy :: Proxy HanjaRenderingOption) ++
                                 "  [default: " ++
                                 enumKeyword DisambiguatingHanjaInParentheses ++
                                 "]")
                        )
                    <*> ( HanjaReadingOption
                        <$> flag True False
                            ( long "no-initial-sound-law"
                            <> short 'I'
                            <> help ("Do not apply Initial Sound Law " ++
                                     "(頭音法則) Sino-Korean words.  " ++
                                     "This implies -S/--no-kr-stdict")
                            )
                        <*> ( Trie.fromList
                            <$> many
                                ( option hanjaReading
                                    ( long "read-hanja"
                                    <> short 'R'
                                    <> metavar "HANJA:HANGUL"
                                    <> help ("Add a custum reading of " ++
                                             "Sino-Korean word.  This " ++
                                             "option can be multiple, " ++
                                             "e.g., \"-R 孫文:쑨원 " ++
                                             "-R 毛澤東:마오쩌둥\".  " ++
                                             "Prior to -D/--dict options")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    <*> many
        ( strOption
            ( long "dict"
            <> short 'D'
            <> metavar "FILE"
            <> help ("Give a custom dictionary to phonetize Sino-Korean " ++
                     "words.  A dictionry file should be TSV (tab-separated " ++
                     "values) format of two columns; the first column is " ++
                     "hanja and the second column is hangul.  " ++
                     "This option can be multiple.")
            )
        )
    <*> switch
        ( long "no-kr-stdict"
        <> short 'S'
        <> help ("Do not use Standard Korean Language Dictionary " ++
                 "(標準國語大辭典) by South Korean NIKL (國立國語院)")
        )
    <*> switch
        ( long "debug"
        <> hidden
        <> help "Debug mode"
        )
    <*> switch
        ( long "version"
        <> short 'v'
        <> hidden
        <> help "Show version"
        )
    <**> helper
    <*> argument str
        ( metavar "FILE"
        <> value "-"
        <> help ("Input HTML file.  A hyphen (-) means standard input.  " ++
                 "To specify an actual file named \"-\", prepend its " ++
                 "relative path, e.g., \"./-\"  [default: -]")
        )

parserInfo :: ParserInfo Seonbi
parserInfo = info parser
    ( fullDesc
    <> progDesc "Korean typographic adjustment processor"
    )

showHtml :: HtmlEntity -> T.Text
showHtml HtmlStartTag { tag,  rawAttributes } =
    T.concat ["<", T.pack (show tag), " ", rawAttributes, ">"]
showHtml HtmlEndTag { tag } =
    T.concat ["</", T.pack (show tag), ">"]
showHtml HtmlText { rawText } =
    T.concat ["!text  ", rawText]
showHtml HtmlCdata { text } =
    T.concat ["!cdata ", text]
showHtml HtmlComment { comment } =
    T.concat ["<!-- ", comment, " -->"]

main :: IO ()
main = do
    options@Seonbi
        { encoding
        , config
        , dictionaries
        , noKrStdict
        , debug
        , version
        , input
        , output
        } <- execParser parserInfo
    let debugLogger' = if debug then Just logger else Nothing
    config' <- case hanja config of
        Nothing ->
            return config { debugLogger = debugLogger' }
        Just hanja'@HanjaOption
                { reading = HanjaReadingOption initialSoundLaw dict } -> do
            dicts <- forM dictionaries readDictionaryFile
            let customDict = Prelude.foldl unionL dict dicts
            dict' <- if not initialSoundLaw || noKrStdict
                then return customDict
                else do
                    krStDict <- catchIOError southKoreanDictionary $
                        const (return [])
                    return $ unionL customDict krStDict
            let reading' = (reading hanja') { dictionary = dict' }
            return config
                { debugLogger = debugLogger'
                , hanja = Just hanja' { reading = reading' }
                }
    when version $ do
        Prelude.putStrLn $ showVersion Meta.version
        exitSuccess
    let whenDebug = when debug
    let debugPrint = whenDebug . hPutStrLn stderr
    debugPrint ("options: " ++ show options)
    contents <- if input == "-"
        then getContents
        else catchIOError (readFile input) $ \ e -> do
            hPutStrLn stderr $ ioeGetErrorString e ++
                case ioeGetFileName e of
                    Just msg -> ": " ++ msg
                    Nothing -> ""
            exitFailure
    let encodingName = case encoding of
            "" -> fromMaybe "UTF-8" $ detect contents
            enc -> enc
    debugPrint ("encoding: " ++ encodingName)
    result <- catchIOError
        (transformHtmlLazyText config' $ toUnicode encodingName contents)
        (\ e -> hPutStrLn stderr (ioeGetErrorString e) >> exitFailure)
    let resultBytes = fromUnicode encodingName result
    if output == "-"
    then
        putStr resultBytes
    else
        writeFile output resultBytes
  where
    logger :: HtmlEntity -> IO ()
    logger = hPutStrLn stderr . T.unpack . showHtml
