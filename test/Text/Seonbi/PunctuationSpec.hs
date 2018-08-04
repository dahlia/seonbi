{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.PunctuationSpec (spec) where

import Control.Monad
import Data.Maybe

import Data.Set
import Data.Text
import Test.Hspec

import Text.Seonbi.Html
import Text.Seonbi.Punctuation

arrowSample :: HtmlTag -> [HtmlEntity]
arrowSample tag' =
    [ HtmlStartTag { tagStack = [], tag = tag', rawAttributes = "" }
    , HtmlText
        { tagStack = [tag']
        , rawText = "A -&gt; B, B &lt;- A, C &lt;-&gt; D"
        }
    , HtmlStartTag { tagStack = [tag'], tag = BR, rawAttributes = "" }
    , HtmlEndTag { tagStack = [tag'], tag = BR }
    , HtmlText
        { tagStack = [tag']
        , rawText = "a =&#62; b, b &#60;= a, c &#X3C;=&#x3e; d"
        }
    , HtmlEndTag { tagStack = [], tag = tag' }
    ]

titleInputSample :: [HtmlEntity]
titleInputSample =
    [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , HtmlText { tagStack = [P], rawText = "&lt;&lt;無情" }
    , HtmlText { tagStack = [P], rawText = "&gt;&gt;, <&#x3c;흙>" }
    , HtmlText { tagStack = [P], rawText = "&#62; 等을 쓴 " }
    , HtmlText { tagStack = [P], rawText = "李光洙의 日本 이름은 " }
    , HtmlText { tagStack = [P], rawText = "香山光郞다." }
    , HtmlEndTag { tagStack = [], tag = P }
    ]

titleOutputSample :: Text -> Text -> Bool -> [HtmlEntity]
titleOutputSample start end citeTag = catMaybes
    [ Just HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , Just HtmlText { tagStack = [P], rawText = start }
    , cite HtmlStartTag { tagStack = [P], tag = Cite , rawAttributes = "" }
    , Just HtmlText { tagStack = titleStack, rawText = "無情" }
    , cite HtmlEndTag { tagStack = [P], tag = Cite }
    , Just HtmlText { tagStack = [P], rawText = end }
    , Just HtmlText { tagStack = [P], rawText = ", " }
    , Just HtmlText { tagStack = [P], rawText = start }
    , cite HtmlStartTag { tagStack = [P], tag = Cite , rawAttributes = "" }
    , Just HtmlText { tagStack = titleStack, rawText = "흙" }
    , cite HtmlEndTag { tagStack = [P], tag = Cite }
    , Just HtmlText { tagStack = [P], rawText = end }
    , Just HtmlText
        { tagStack = [P]
        , rawText = " 等을 쓴 李光洙의 日本 이름은 香山光郞다."
        }
    , Just HtmlEndTag { tagStack = [], tag = P }
    ]
  where
    titleStack :: HtmlTagStack
    titleStack = if citeTag then [P, Cite] else [P]
    cite :: HtmlEntity -> Maybe HtmlEntity
    cite = if citeTag then Just else const Nothing

ignoredTags :: [HtmlTag]
ignoredTags = [Code, Kbd, Pre, Script, Style, TextArea]

spec :: Spec
spec = do
    describe "quoteCitation" $ do
        let angleQuotesNoCite = angleQuotes { htmlElement = Nothing }
            cornerBracketsNoCite = cornerBrackets { htmlElement = Nothing }
        specify "angleQuotes w/ citeTag" $
            quoteCitation angleQuotes titleInputSample `shouldBe`
                titleOutputSample "&#12298;" "&#12299;" True
        specify "angleQuotes w/o citeTag" $
            quoteCitation angleQuotesNoCite titleInputSample
                `shouldBe` titleOutputSample "&#12298;" "&#12299;" False
        specify "cornerBrackets w/ citeTag" $
            quoteCitation cornerBrackets titleInputSample `shouldBe`
                titleOutputSample "&#12302;" "&#12303;" True
        specify "cornerBrackets w/o citeTag" $
            quoteCitation cornerBracketsNoCite titleInputSample `shouldBe`
                titleOutputSample "&#12302;" "&#12303;" False
        it "is idempotent" $ do
            let f = quoteCitation angleQuotes
            normalizeText (f (f titleInputSample)) `shouldBe`
                normalizeText (quoteCitation angleQuotes titleInputSample)
        forM_ ignoredTags $ \ tag' ->
            it ("ignores <" ++ unpack (htmlTagName tag') ++ "> tags") $
                (normalizeText . quoteCitation angleQuotes)
                    [ HtmlStartTag
                        { tagStack = []
                        , tag = Div
                        , rawAttributes = ""
                        }
                    , HtmlStartTag
                        { tagStack = [Div]
                        , tag = P
                        , rawAttributes = ""
                        }
                    , HtmlText
                        { tagStack = [Div, P]
                        , rawText = "이 안의 &#x3c;타이틀&gt;은 바뀐다."
                        }
                    , HtmlEndTag { tagStack = [Div], tag = P }
                    , HtmlStartTag
                        { tagStack = [Div]
                        , tag = tag'
                        , rawAttributes = ""
                        }
                    , HtmlText
                        { tagStack = [Div, tag']
                        , rawText = "this is not a&lt;title&#x3e; but_code"
                        }
                    , HtmlEndTag { tagStack = [Div], tag = tag' }
                    , HtmlEndTag { tagStack = [], tag = Div }
                    ]
                    `shouldBe`
                    [ HtmlStartTag
                        { tagStack = []
                        , tag = Div
                        , rawAttributes = ""
                        }
                    , HtmlStartTag
                        { tagStack = [Div]
                        , tag = P
                        , rawAttributes = ""
                        }
                    , HtmlText
                        { tagStack = [Div, P]
                        , rawText = "이 안의 &#12296;"
                        }
                    , HtmlStartTag
                        { tagStack = [Div, P]
                        , tag = Cite
                        , rawAttributes = ""
                        }
                    , HtmlText { tagStack = [Div, P, Cite], rawText = "타이틀" }
                    , HtmlEndTag { tagStack = [Div, P], tag = Cite }
                    , HtmlText
                        { tagStack = [Div, P]
                        , rawText = "&#12297;은 바뀐다."
                        }
                    , HtmlEndTag { tagStack = [Div], tag = P }
                    , HtmlStartTag
                        { tagStack = [Div]
                        , tag = tag'
                        , rawAttributes = ""
                        }
                    , HtmlText
                        { tagStack = [Div, tag']
                        , rawText = "this is not a&lt;title&#x3e; but_code"
                        }
                    , HtmlEndTag { tagStack = [Div], tag = tag' }
                    , HtmlEndTag { tagStack = [], tag = Div }
                    ]
    describe "transformArrow" $ do
        specify "[]" $
            transformArrow [] (arrowSample P) `shouldBe`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "A &rarr; B, B &larr; A, C &larr;&gt; D"
                    }
                , HtmlStartTag { tagStack = [P], tag = BR, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = BR }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "a =&#62; b, b &#60;= a, c &#X3C;=&#x3e; d"
                    }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
        specify "[LeftRight]" $
            transformArrow [LeftRight] (arrowSample P) `shouldBe`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "A &rarr; B, B &larr; A, C &harr; D"
                    }
                , HtmlStartTag { tagStack = [P], tag = BR, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = BR }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "a =&#62; b, b &#60;= a, c &#X3C;=&#x3e; d"
                    }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
        specify "[DoubleArrow]" $
            transformArrow [DoubleArrow] (arrowSample P) `shouldBe`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "A &rarr; B, B &larr; A, C &larr;&gt; D"
                    }
                , HtmlStartTag { tagStack = [P], tag = BR, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = BR }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "a &rArr; b, b &lArr; a, c &lArr;&#x3e; d"
                    }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
        specify "[LeftRight, DoubleArrow]" $
            transformArrow [LeftRight, DoubleArrow] (arrowSample P) `shouldBe`
                [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "A &rarr; B, B &larr; A, C &harr; D"
                    }
                , HtmlStartTag { tagStack = [P], tag = BR, rawAttributes = "" }
                , HtmlEndTag { tagStack = [P], tag = BR }
                , HtmlText
                    { tagStack = [P]
                    , rawText = "a &rArr; b, b &lArr; a, c &hArr; d"
                    }
                , HtmlEndTag { tagStack = [], tag = P }
                ]
        forM_ ignoredTags $ \ tag' ->
            it ("ignores <" ++ unpack (htmlTagName tag') ++ "> tags") $ do
                let optionsSet =
                        [ []
                        , [LeftRight]
                        , [DoubleArrow]
                        , [LeftRight, DoubleArrow]
                        ] :: [Set ArrowTransformationOption]
                forM_ optionsSet $ \ options ->
                    let
                        entities = transformArrow options (arrowSample tag')
                    in
                        entities `shouldBe` entities
