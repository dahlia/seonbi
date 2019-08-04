{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Data.String
import Data.Version
import System.IO

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
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
    { sourceHtml :: Text
    , configuration :: Configuration IO ()
    } deriving (Show)

instance FromJSON Input where
    parseJSON = withObject "Input" $ \ v -> do
        sourceHtml' <- v .: "sourceHtml"
        preset <- v .:? "preset"
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
                xhtml' <- v .: "xhtml"
                quote' <- v .: "quote"
                cite' <- v .: "cite"
                arrow' <- v .: "arrow"
                ellipsis' <- v .: "ellipsis"
                emDash' <- v .: "emDash"
                hanja' <- v .: "hanja"
                return Configuration
                    { debugLogger = Nothing
                    , xhtml = xhtml'
                    , quote = quote'
                    , cite = cite'
                    , arrow = arrow'
                    , ellipsis = ellipsis'
                    , emDash = emDash'
                    , hanja = hanja'
                    }
        return $ Input sourceHtml' config

instance FromJSON QuoteOption
instance FromJSON CiteOption
instance FromJSON ArrowOption
instance FromJSON HanjaRenderingOption

instance FromJSON HanjaOption where
    parseJSON = withObject "HanjaOption" $ \ v -> HanjaOption
        <$> v .: "rendering"
        <*> v .: "reading"

instance FromJSON HanjaReadingOption where
    parseJSON = withObject "HanjaReadingOption" $ \ v ->
        (`HanjaReadingOption` Trie.empty)
            <$> v .: "initialSoundLaw"

app :: Int -> Application
app debugDelay request respond =
    case requestMethod request of
        "POST" -> do
            inputJson <- lazyRequestBody request
            threadDelay debugDelay
            case eitherDecode' inputJson of
                Right (Input source config) -> do
                    result <- transformHtmlText config source
                    respond' status200 $ object
                        [ "success" .= Bool True
                        , "resultHtml" .= String result
                        ]
                Left msg -> respond' status400 $ object
                    [ "success" .= Bool False
                    , "message" .= String (pack msg)
                    ]
        method -> respond' status405 $ object
            [ "success" .= Bool False
            , "message" .= String ("Unsupported method: " <> decodeUtf8 method)
            ]
  where
    respond' :: ToJSON a => Status -> a -> IO ResponseReceived
    respond' status value' =
        respond $ responseLBS
            status
            [("Content-Type", "application/json")]
            (encode value')

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
    , debugDelayMs :: Int
    }

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
    <*> option auto
        ( long "debug-delay"
        <> metavar "MS"
        <> value 0
        <> showDefault
        <> help "Delay time for client development"
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
        , debugDelayMs
        } <- execParser parserInfo
    let serverSettings' = setServerName serverName settings
    let netloc = showHostPreference (getHost serverSettings') ++ ":" ++
            show (getPort serverSettings')
    let url = "http://" ++ netloc ++ "/"
    hPutStrLn stderr url
    runSettings serverSettings' $ app (debugDelayMs * 1000)
