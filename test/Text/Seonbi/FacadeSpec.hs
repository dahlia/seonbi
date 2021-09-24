{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.FacadeSpec (spec) where

import Control.Monad

import Data.Algorithm.Diff
import Data.Text.Lazy
import Data.Text.Lazy.IO
import System.Directory
import System.FilePath
import Test.Hspec

import Text.Seonbi.Facade

dataDirPath :: FilePath
dataDirPath = "test" </> "data"

inputExtension :: String
inputExtension = ".ko-Kore.html"

outputExtensions :: Monad m => [(String, Configuration m a)]
outputExtensions =
    [ (".ko-KR.html", ko_KR)
    , (".ko-KP.html", ko_KP)
    ]

shouldHaveSameText :: (HasCallStack) => Text -> Text -> Expectation
actual `shouldHaveSameText` expected =
    unless (actual == expected) (expectationFailure msg)
  where
    expectedLines :: [Text]
    expectedLines = Data.Text.Lazy.lines expected
    actualLines :: [Text]
    actualLines = Data.Text.Lazy.lines actual
    diffLines :: [Diff Text]
    diffLines = getDiff expectedLines actualLines
    diff :: Text
    diff = Data.Text.Lazy.unlines
        [ case d of
            First line -> "- " <> line
            Second line -> "+ " <> line
            Both line _ -> "  " <> line
        | d <- diffLines
        ]
    msg :: String
    msg = "Two values are not equal:\n\n--- expected\n+++ actual\n\n" ++
        unpack diff

spec :: Spec
spec = do
    testData <- runIO $ do
        files <- listDirectory dataDirPath
        let inputFiles = [f | f <- files, inputExtension `isExtensionOf` f]
        testFiles <- filterM
            (\(_, o, _) -> doesFileExist (dataDirPath </> o))
            [ (i, dropExtension i -<.> oExt, oCfg)
            | i <- inputFiles
            , (oExt, oCfg) <- outputExtensions
            ]
        forM testFiles $ \ (input, output, cfg) -> do
            inputData <- Data.Text.Lazy.IO.readFile (dataDirPath </> input)
            outputData <- Data.Text.Lazy.IO.readFile (dataDirPath </> output)
            return (input, output, inputData, outputData, cfg)
    describe "transformHtmlLazyText" $
        forM_ testData $ \ (iname, oname, input, output, cfg) ->
            specify (iname ++ " -> " ++ oname) $ do
                let Just noOpResult = transformHtmlLazyText noOp input
                noOpResult `shouldHaveSameText` input
                let Just cfgResult = transformHtmlLazyText cfg input
                cfgResult `shouldHaveSameText` output
  where
    noOp :: Monad m => Configuration m a
    noOp = Configuration
        { quote = Nothing
        , cite = Nothing
        , arrow = Nothing
        , ellipsis = False
        , emDash = False
        , stop = Nothing
        , hanja = Nothing
        , xhtml = False
        , debugLogger = Nothing
        }
