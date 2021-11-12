{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Maybe (catMaybes)
import Data.String
import Data.Version
import GHC.Exts (IsList (..))
import System.IO

import Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text
import Data.Text.Encoding
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Options.Applicative

import qualified Paths_seonbi as Meta
import Text.Seonbi.Facade
import Text.Seonbi.Trie as Trie

data Input = Input
    { source :: Text
    , configuration :: Configuration IO ()
    , warnings :: [Text]
    } deriving (Show)

instance FromJSON Input where
    parseJSON = withObject "Input" $ \ v -> do
        sourceMaybe <- v .:? "content"
        (source', w1) <- case sourceMaybe of
            Just s -> return (s, Nothing)
            Nothing -> do
                sourceHtml' <- v .:? "sourceHtml"
                case sourceHtml' of
                    Just h -> return
                        ( h
                        , Just $ "key \"sourceHtml\" is deprecated in " <>
                                 "favour of \"content\""
                        )
                    Nothing -> fail "key \"content\" not present"
        preset <- v .:? "preset"
        contentTypeMaybe <- v .:? "contentType"
        (contentType', w2) <- case contentTypeMaybe of
            Just t -> return (t, Nothing)
            Nothing -> do
                xhtml <- v .:? "xhtml"
                case xhtml of
                    Just x -> return
                        ( if x then "application/xhtml+xml" else "text/html"
                        , Just $ "key \"xhtml\" is deprecated in favour of " <>
                                 "\"contentType\""
                        )
                    Nothing -> return ("text/html", Nothing)
        config <- case preset of
            Just locale ->
                let presets' = presets :: M.Map Text (Configuration IO ())
                    m = M.lookup (replace "_" "-" $ toLower locale) presets'
                in
                    case m of
                        Just p -> return p
                        Nothing -> fail $ unpack $ Data.Text.concat
                            [ "No such preset: "
                            , locale
                            , "; available presets: "
                            , intercalate ", " (M.keys presets')
                            ]
            Nothing -> do
                quote' <- v .:? "quote"
                cite' <- v .:? "cite"
                arrow' <- v .:? "arrow"
                ellipsis' <- v .:? "ellipsis" .!= False
                emDash' <- v .:? "emDash" .!= False
                stop' <- v .:? "stop"
                hanja' <- v .:? "hanja" .!= Nothing
                return Configuration
                    { debugLogger = Nothing
                    , contentType = contentType'
                    , quote = quote'
                    , cite = cite'
                    , arrow = arrow'
                    , ellipsis = ellipsis'
                    , emDash = emDash'
                    , stop = stop'
                    , hanja = hanja'
                    }
        return $ Input
            { source = source'
            , configuration = config { contentType = contentType' }
            , warnings = catMaybes [w1, w2]
            }

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \ t ->
        if contentTypeFromText t `S.member` contentTypes
        then return (contentTypeFromText t)
        else fail $ unpack $ Data.Text.concat
            [ "Unknown content type: "
            , t
            , "; available content types: "
            , intercalate ", " $ contentTypeText <$> S.elems contentTypes
            ]

instance FromJSON QuoteOption
instance FromJSON CiteOption
instance FromJSON ArrowOption
instance FromJSON StopOption
instance FromJSON HanjaRenderingOption

instance FromJSON HanjaOption where
    parseJSON = withObject "HanjaOption" $ \ v -> HanjaOption
        <$> v .: "rendering"
        <*> v .: "reading"

instance FromJSON HanjaReadingOption where
    parseJSON = withObject "HanjaReadingOption" $ \ v -> do
        initialSoundLaw <- v .:? "initialSoundLaw" .!= False
        wordMap <- v .:? "dictionary" .!= []
        let wordPairs = GHC.Exts.toList (wordMap :: Object)
        dictionary <- forM wordPairs $ \ (key, val) -> do
            val' <- withText "Hangul string" return val
            return (key, val')
        let customDict = Trie.fromList dictionary
        dictIds <- v .:? "useDictionaries" .!= []
        useDictionaries <- forM (dictIds :: Array) $
            withText "Dictionary ID string" getDictById
        let dict = Prelude.foldl unionL customDict useDictionaries
        return $ HanjaReadingOption initialSoundLaw dict
      where
        getDictById :: Text -> Data.Aeson.Types.Parser HanjaDictionary
        getDictById "kr-stdict" = return southKoreanDictionaryUnsafe
        getDictById dictId = fail ("No such dictionary ID: " ++ unpack dictId)
        southKoreanDictionaryUnsafe :: HanjaDictionary
        southKoreanDictionaryUnsafe = case hanja ko_KR' of
            Just HanjaOption { reading = HanjaReadingOption { dictionary } } ->
                dictionary
            Nothing ->
                Trie.empty
        ko_KR' :: Configuration IO ()
        ko_KR' = ko_KR


app :: AppOptions -> Application
app AppOptions { allowOrigin, debugDelayMs } request respond =
    case requestMethod request of
        "POST" -> do
            inputJson <- lazyRequestBody request
            threadDelay (debugDelayMs * 1000)
            case eitherDecode' inputJson of
                Right (Input source config warnings) -> do
                    result <- transformHtmlText config source
                    let type' = contentType config
                    let warningComments =
                            if Prelude.null warnings
                            then Data.Text.empty
                            else Data.Text.concat
                                [ "<!--\n"
                                , Data.Text.intercalate "\n" warnings
                                , "\n-->"
                                ]
                    respond' status200 $ object $
                        [ "success" .= Bool True
                        , "content" .= String result
                        , "warnings" .= Array
                            (GHC.Exts.fromList $ String <$> warnings)
                        , "contentType" .= String (contentTypeText type')
                        ]
                        ++
                        [ "resultHtml" .= String (warningComments <> result)
                        | type' == "text/html" ||
                          type' == "application/xhtml+xml"
                        ]
                Left msg -> respond' status400 $ object
                    [ "success" .= Bool False
                    , "message" .= String (pack msg)
                    ]
        "OPTIONS" ->
            respond' status200 Null
        method -> respond' status405 $ object
            [ "success" .= Bool False
            , "message" .= String ("Unsupported method: " <> decodeUtf8 method)
            ]
  where
    respond' :: ToJSON a => Status -> a -> IO ResponseReceived
    respond' status value' =
        respond $ responseLBS status headers (encode value')
    headers :: [Header]
    headers = headerAdder
        [ ("Content-Type", "application/json")
        , ("Access-Control-Allow-Headers", "content-type")
        ]
    headerAdder :: [Header] -> [Header]
    headerAdder = case allowOrigin of
        Just origin -> (("Access-Control-Allow-Origin", origin) :)
        Nothing -> id

string :: IsString a => ReadM a
string = maybeReader (Just . fromString)

showHostPreference :: HostPreference -> String
showHostPreference h = case show h of
    "HostAny" -> "[::]"
    "HostIPv4" -> "0.0.0.0"
    "HostIPv4Only" -> "0.0.0.0"
    "HostIPv6" -> "[::]"
    "HostIPv6Only" -> "[::]"
    'H' : 'o' : 's' : 't' : ' ' : '"' : a ->
        Prelude.take (Prelude.length a - 1) a
    _ -> "?"

data CliOptions = CliOptions
    { serverSettings :: Settings
    , appOptions :: AppOptions
    }

data AppOptions = AppOptions
    { allowOrigin :: Maybe B.ByteString
    , debugDelayMs :: Int
    } deriving (Show, Eq)

parser :: Parser CliOptions
parser = CliOptions
    <$> ( setHost
        <$> option string
            ( long "host"
            <> short 'H'
            <> metavar "HOST"
            <> value "*"
            <> help "Host address to listen (default: [::/0])"
            )
        <*> ((`setPort` defaultSettings)
            <$> option auto
                ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 3800
                <> showDefault
                <> help "Port number to listen"
                )
            )
        )
    <*> ( AppOptions
        <$> (
                optional
                    ( strOption
                        ( long "allow-origin"
                        <> short 'o'
                        <> metavar "ORIGIN"
                        <> help "Allow cross-origin (i.e., CORS)"
                        )
                    )
                <|> pure Nothing
            )
        <*> option auto
            ( long "debug-delay"
            <> metavar "MS"
            <> value 0
            <> showDefault
            <> help "Delay time for client development"
            )
        )
    <**> helper

parserInfo :: ParserInfo CliOptions
parserInfo = info parser
    ( fullDesc
    <> progDesc "Seonbi HTTP API server"
    )

serverName :: B.ByteString
serverName =
    "Seonbi/" `B.append` encodeUtf8 (pack $ showVersion Meta.version)

main :: IO ()
main = do
    CliOptions
        { serverSettings = settings
        , appOptions
        } <- execParser parserInfo
    let serverSettings' = setServerName serverName settings
    let netloc = showHostPreference (getHost serverSettings') ++ ":" ++
            show (getPort serverSettings')
    let url = "http://" ++ netloc ++ "/"
    hPutStrLn stderr url
    runSettings serverSettings' $ app appOptions
