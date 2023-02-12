{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import Data.Char
import GHC.IO.Encoding

import Test.Hspec.Runner

import qualified Spec

main :: IO ()
main = do
    TextEncoding { textEncodingName } <- getLocaleEncoding
    when ("cp437" == (toLower <$> textEncodingName)) $ setLocaleEncoding utf8
    hspecWith defaultConfig Spec.spec
