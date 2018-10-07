{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Prelude hiding (concat)
import System.IO (Handle, IOMode (..), hClose, hSetEncoding, utf8, withFile)

import Codec.Archive.Zip
import Data.ByteString.Lazy (ByteString, hPut)
import Data.Text
import Data.Text.IO (hGetLine, hPutStrLn)
import Distribution.Simple
import Network.HTTP.Client
import System.Directory
import System.FilePath
import System.IO.Temp

unihanUrl :: String
unihanUrl = "http://ftp.unicode.org/Public/11.0.0/ucd/Unihan.zip"

kHangulPath :: FilePath
kHangulPath = "src" </> "Text" </> "Seonbi" </> "kHangul.txt"

main :: IO ()
main = do
    exist <- doesFileExist kHangulPath
    unless exist $ do
        data' <- downloadUnihan
        extractUnihanReadings data' $ \ txtPath -> do
            values <- withFile txtPath ReadMode (extractProp "kHangul")
            withFile kHangulPath WriteMode $ \ handle -> do
                hSetEncoding handle utf8
                forM_ values $ \ (char, value) ->
                    hPutStrLn handle $ concat [char, "\t", value]
    defaultMain

extractProp :: Text -> Handle -> IO [(Text, Text)]
extractProp property handle = do
    hSetEncoding handle utf8
    line <- hGetLine handle
    case line of
        "" ->
            return []
        line' ->
            case breakOn "\t" line' of
                (_, "") ->
                    extractProp property handle
                (char, rest)
                  | "U+" `isPrefixOf` char && "\t" `isPrefixOf` rest ->
                    case breakOn "\t" $ Data.Text.tail rest of
                        (_, "") ->
                            extractProp property handle
                        (prop, value) | prop == property ->
                            ((char, value) :) <$> extractProp property handle
                        _ ->
                            extractProp property handle
                _ ->
                    extractProp property handle
    

extractUnihanReadings :: ByteString -> (FilePath -> IO a) -> IO a
extractUnihanReadings data' callback =
    withSystemTempFile "Unihan.zip" $ \ zipPath handle -> do
        hPut handle data'
        hClose handle
        let entryName = "Unihan_Readings.txt"
        withSystemTempFile entryName $ \ txtPath handle' -> do
            hClose handle'
            sel <- mkEntrySelector entryName
            withArchive zipPath (saveEntry sel txtPath)
            callback txtPath

downloadUnihan :: IO ByteString
downloadUnihan = do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest unihanUrl
    res <- httpLbs req mgr
    return $ responseBody res
