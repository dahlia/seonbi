{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.String
import Data.Version
import System.IO

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Options.Applicative

import qualified Paths_seonbi as Meta
import Text.Seonbi.Facade

app :: Application
app request respond =
    case requestMethod request of
        "POST" -> do
            input <- lazyRequestBody request
            let source = decodeUtf8 input
            result <- transformHtmlLazyText ko_KR source
            let output = encodeUtf8 (result `snoc` '\n')
            respond $ responseLBS
                status200
                [("Content-Type", "text/html; charset=utf-8")]
                output
        _ -> 
            respond $ responseLBS
                status405
                [("Content-Type", "text/text")]
                ""

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

parser :: Parser Settings
parser = setHost
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
    <**> helper

parserInfo :: ParserInfo Settings
parserInfo = info parser
    ( fullDesc
    <> progDesc "Seonbi HTTP API server"
    )

serverName :: B.ByteString
serverName =
    "Seonbi/" `B.append` E.encodeUtf8 (T.pack $ showVersion Meta.version)

main :: IO ()
main = do
    settings <- setServerName serverName <$> execParser parserInfo
    let netloc = showHostPreference (getHost settings) ++ ":" ++
            show (getPort settings)
    let url = "http://" ++ netloc ++ "/"
    hPutStrLn stderr url
    runSettings settings app
