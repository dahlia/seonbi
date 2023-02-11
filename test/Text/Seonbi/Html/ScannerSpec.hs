{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.ScannerSpec (spec) where

import Test.Hspec

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Scanner
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.TextNormalizer

shouldBeDone :: (Eq a, Show a) => Result a -> a -> Expectation
shouldBeDone (Done "" r) expected = do
    r `shouldBe` expected
shouldBeDone result expected =
    expectationFailure $ "Expected Done \"\" " ++ show expected ++
        ", but got " ++ show result

spec :: Spec
spec =
    describe "scanHtml" $ do
        it "returns an empty list if the input is empty" $
            scanHtml "" `shouldBeDone` []

        it "parses text nodes" $
            scanHtml "foobar" `shouldBeDone`
                [HtmlText { tagStack = [], rawText = "foobar" }]

        it "parses HTML comments" $ do
            scanHtml "<!-- foo -->" `shouldBeDone`
                [HtmlComment { tagStack = [], comment = " foo " }]
            scanHtml "<!-- foo- -->" `shouldBeDone`
                [HtmlComment { tagStack = [], comment = " foo- " }]
            scanHtml "<!-- foo-> -->" `shouldBeDone`
                [HtmlComment { tagStack = [], comment = " foo-> " }]
            scanHtml "<!-- foo-- -->" `shouldBeDone`
                [HtmlComment { tagStack = [], comment = " foo-- " }]
            scanHtml "foo <!-- bar -->" `shouldBeDone`
                [ HtmlText { tagStack = [], rawText = "foo " }
                , HtmlComment { tagStack = [], comment = " bar " }
                ]
            scanHtml "<!-- foo --> bar" `shouldBeDone`
                [ HtmlComment { tagStack = [], comment = " foo " }
                , HtmlText { tagStack = [], rawText = " bar" }
                ]
            scanHtml "foo <!-- bar --> baz" `shouldBeDone`
                [ HtmlText { tagStack = [], rawText = "foo " }
                , HtmlComment { tagStack = [], comment = " bar " }
                , HtmlText { tagStack = [], rawText = " baz" }
                ]
            scanHtml "<p>foo <!-- bar baz --> qux</p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [P], rawText = "foo " }
                , HtmlComment { tagStack = [P], comment = " bar baz " }
                , HtmlText { tagStack = [P], rawText = " qux" }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
            scanHtml "<p>foo <!-- <b>bar</b> baz --> qux</p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [P], rawText = "foo " }
                , HtmlComment { tagStack = [P], comment = " <b>bar</b> baz " }
                , HtmlText { tagStack = [P], rawText = " qux" }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "parses CDATA sections" $ do
            scanHtml "<![CDATA[foo]]>" `shouldBeDone`
                [HtmlCdata { tagStack = [], text = "foo" }]
            scanHtml "foo <![CDATA[bar]]>" `shouldBeDone`
                [ HtmlText { tagStack = [], rawText = "foo " }
                , HtmlCdata { tagStack = [], text = "bar" }
                ]
            scanHtml "<![CDATA[foo]]> bar" `shouldBeDone`
                [ HtmlCdata { tagStack = [], text = "foo" }
                , HtmlText { tagStack = [], rawText = " bar" }
                ]
            scanHtml "foo <![CDATA[bar]]> baz" `shouldBeDone`
                [ HtmlText { tagStack = [], rawText = "foo " }
                , HtmlCdata { tagStack = [], text = "bar" }
                , HtmlText { tagStack = [], rawText = " baz" }
                ]
            scanHtml "<![CDATA[foo] ]]>" `shouldBeDone`
                [HtmlCdata { tagStack = [], text = "foo] " }]
            scanHtml "<p>foo <![CDATA[bar baz]]> qux</p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [P], rawText = "foo " }
                , HtmlCdata { tagStack = [P], text = "bar baz" }
                , HtmlText { tagStack = [P], rawText = " qux" }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
            scanHtml "<p>foo <![CDATA[<b>bar</b> baz]]> qux</p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [P], rawText = "foo " }
                , HtmlCdata { tagStack = [P], text = "<b>bar</b> baz" }
                , HtmlText { tagStack = [P], rawText = " qux" }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "treats malformed CDATA sections as text nodes" $
            (normalizeText <$> scanHtml "<![CDATA[foo") `shouldBeDone`
                [HtmlText { tagStack = [], rawText = "<![CDATA[foo" }]

        it "parses html start tags" $ do
            scanHtml "<p>" `shouldBeDone`
                [HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }]
            scanHtml "<p><em>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag { tagStack = [P], tag = Em, rawAttributes = "" }
                ]

        it "parses HTML start tags having attributes" $ do
            scanHtml "<p class=foo>" `shouldBeDone`
                [ HtmlStartTag
                    { tagStack = []
                    , tag = P
                    , rawAttributes = " class=foo"
                    }
                ]
            scanHtml "<a href=\"https://example.com/\">" `shouldBeDone`
                [ HtmlStartTag
                    { tagStack = []
                    , tag = A
                    , rawAttributes = " href=\"https://example.com/\""
                    }
                ]

        it "parses html end tags" $ do
            scanHtml "<p></p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
            scanHtml "<p><em>test</em></p>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag { tagStack = [P], tag = Em, rawAttributes = "" }
                , HtmlText { tagStack = [P, Em], rawText = "test" }
                , HtmlEndTag { tagStack = [P], tag = Em }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "can parse even if tags end in wrong order" $ do
            scanHtml "<p><em>test</p></em>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag { tagStack = [P], tag = Em, rawAttributes = "" }
                , HtmlText { tagStack = [P, Em], rawText = "test" }
                , HtmlEndTag { tagStack = [Em], tag = P }
                , HtmlEndTag { tagStack = [], tag = Em }
                ]
            scanHtml "<p><b class=\"baz\">Hel<i>lo Wo</b>rld</i></p>"
                `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag
                    { tagStack = [P]
                    , tag = B
                    , rawAttributes = " class=\"baz\""
                    }
                , HtmlText { tagStack = [P, B], rawText = "Hel" }
                , HtmlStartTag
                    { tagStack = [P, B]
                    , tag = I
                    , rawAttributes = ""
                    }
                , HtmlText { tagStack = [P, B, I], rawText = "lo Wo" }
                , HtmlEndTag { tagStack = [P, I], tag = B }
                , HtmlText { tagStack = [P, I], rawText = "rld" }
                , HtmlEndTag { tagStack = [P], tag = I }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "can parse XHTML-style self-closing tags" $
            scanHtml "<p><em/>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag { tagStack = [P], tag = Em, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = Em }
                ]

        it "emits both start and end tags for void tags (e.g., <hr>)" $ do
            scanHtml "<p><hr>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag { tagStack = [P], tag = HR, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = HR }
                ]
            scanHtml
                "<div><p>foo</p><hr><p>bar <img src=\"a.jpg\"></p><hr></div>"
                `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = Div, rawAttributes = "" }
                , HtmlStartTag { tagStack = [Div], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [Div, P], rawText = "foo" }
                , HtmlEndTag { tagStack = [Div], tag = P }
                , HtmlStartTag
                    { tagStack = [Div]
                    , tag = HR
                    , rawAttributes = ""
                    }
                , HtmlEndTag { tagStack = [Div], tag = HR }
                , HtmlStartTag { tagStack = [Div], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [Div, P], rawText = "bar " }
                , HtmlStartTag
                    { tagStack = [Div, P]
                    , tag = Img
                    , rawAttributes = " src=\"a.jpg\""
                    }
                , HtmlEndTag { tagStack = [Div, P], tag = Img }
                , HtmlEndTag { tagStack = [Div], tag = P }
                , HtmlStartTag
                    { tagStack = [Div]
                    , tag = HR
                    , rawAttributes = ""
                    }
                , HtmlEndTag { tagStack = [Div], tag = HR }
                , HtmlEndTag { tagStack = [], tag = Div }
                ]
            scanHtml "<embed></embed>" `shouldBeDone`
                [ HtmlStartTag
                    { tagStack = []
                    , tag = Embed
                    , rawAttributes = ""
                    }
                , HtmlEndTag { tagStack = [], tag = Embed }
                ]

        it "can parses a flat element" $
            scanHtml "<div>Hello</div>" `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = Div, rawAttributes = "" }
                , HtmlText { tagStack = [Div], rawText = "Hello" }
                , HtmlEndTag { tagStack = [], tag = Div }
                ]

        it "can parses nested elements" $ do
            scanHtml "<div><p class=foo>Hello</p><p class='bar'>World</p></div>"
                `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = Div, rawAttributes = "" }
                , HtmlStartTag
                    { tagStack = [Div]
                    , tag = P
                    , rawAttributes = " class=foo"
                    }
                , HtmlText { tagStack = [Div, P], rawText = "Hello" }
                , HtmlEndTag { tagStack = [Div], tag = P }
                , HtmlStartTag
                    { tagStack = [Div]
                    , tag = P
                    , rawAttributes = " class='bar'"
                    }
                , HtmlText { tagStack = [Div, P], rawText = "World" }
                , HtmlEndTag { tagStack = [Div], tag = P }
                , HtmlEndTag { tagStack = [], tag = Div }
                ]
            scanHtml "<p><b class=\"baz\">Hel<i>lo Wo</i>rld</b></p>"
                `shouldBeDone`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlStartTag
                    { tagStack = [P]
                    , tag = B
                    , rawAttributes = " class=\"baz\""
                    }
                , HtmlText { tagStack = [P, B], rawText = "Hel" }
                , HtmlStartTag
                    { tagStack = [P, B]
                    , tag = I
                    , rawAttributes = ""
                    }
                , HtmlText { tagStack = [P, B, I], rawText = "lo Wo" }
                , HtmlEndTag { tagStack = [P, B], tag = I }
                , HtmlText { tagStack = [P, B], rawText = "rld" }
                , HtmlEndTag { tagStack = [P], tag = B }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "can parses an HTML fragment having multiple root elements" $
            scanHtml
                "<p\nid=\"a\">Hello <b>world</b>!</p>\n<p>Second paragraph.</p>"
                `shouldBeDone`
                [ HtmlStartTag
                    { tagStack = []
                    , tag = P
                    , rawAttributes = "\nid=\"a\""
                    }
                , HtmlText { tagStack = [P], rawText = "Hello " }
                , HtmlStartTag { tagStack = [P], tag = B, rawAttributes = "" }
                , HtmlText { tagStack = [P, B], rawText = "world" }
                , HtmlEndTag { tagStack = [P], tag = B }
                , HtmlText { tagStack = [P], rawText = "!" }
                , HtmlEndTag { tagStack = [], tag = P }
                , HtmlText { tagStack = [], rawText = "\n" }
                , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText { tagStack = [P], rawText = "Second paragraph." }
                , HtmlEndTag { tagStack = [], tag = P }
                ]

        it "treats an invalid tag as a text node" $
            (normalizeText <$> scanHtml "<invalid>") `shouldBeDone`
                [HtmlText { tagStack = [], rawText = "<invalid>" }]
