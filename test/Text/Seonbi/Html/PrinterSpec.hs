{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.PrinterSpec (spec) where

import Data.Text.Lazy
import Test.Hspec

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Printer
import Text.Seonbi.Html.Tag

sample :: [HtmlEntity]
sample =
    [ HtmlComment { tagStack = [], comment = " foo " }
    , HtmlStartTag { tagStack = [], tag = P, rawAttributes = " id=\"a\"" }
    , HtmlText { tagStack = [P], rawText = "Hello," }
    , HtmlStartTag { tagStack = [P], tag = BR, rawAttributes = "" }
    , HtmlEndTag { tagStack = [P], tag = BR }
    , HtmlText { tagStack = [P], rawText = "\n" }
    , HtmlStartTag { tagStack = [P], tag = Em, rawAttributes = "class=\"b\"" }
    , HtmlCdata { tagStack = [P, Em], text = "world" }
    , HtmlEndTag { tagStack = [P], tag = Em }
    , HtmlText { tagStack = [P], rawText = "!" }
    , HtmlEndTag { tagStack = [], tag = P }
    , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , HtmlEndTag { tagStack = [], tag = P }
    ]

spec :: Spec
spec = do
    specify "printHtml" $
        printHtml sample `shouldBe` Data.Text.Lazy.concat
            [ "<!-- foo --><p id=\"a\">Hello,<br>\n"
            , "<em class=\"b\"><![CDATA[world]]></em>!</p><p></p>"
            ]
    specify "printXhtml" $
        printXhtml sample `shouldBe` Data.Text.Lazy.concat
            [ "<!-- foo --><p id=\"a\">Hello,<br/>\n"
            , "<em class=\"b\"><![CDATA[world]]></em>!</p><p></p>"
            ]
