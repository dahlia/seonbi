import Control.Monad
import GHC.IO.Encoding
import System.Info (os)

import System.IO.CodePage (withCP65001)
import Test.Hspec.Runner

import qualified Spec

main :: IO ()
main = withCP65001 $ do
    when (System.Info.os == "ming32") $ setLocaleEncoding utf8
    hspecWith defaultConfig Spec.spec
