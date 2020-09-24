{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.FacadeSpec (spec) where

import Control.Monad

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
    forM_ testData $ \ (iname, oname, input, output, cfg) ->
        specify ("transformHtmlLazyText: " ++ iname ++ " -> " ++ oname) $ do
            transformHtmlLazyText noOp input `shouldBe` Just input
            transformHtmlLazyText cfg input `shouldBe` Just output
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
