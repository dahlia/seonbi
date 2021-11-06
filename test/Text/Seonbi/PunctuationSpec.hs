{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.Seonbi.PunctuationSpec (spec) where

import Control.Monad
import Data.Maybe
import System.IO.Unsafe

import Data.Set
import Data.Text
import System.Random
import Test.Hspec
import Text.InterpolatedString.Perl6 (qc)

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

ellipsisSample :: Text -> Text -> [HtmlEntity]
ellipsisSample periods periods' =
    [ HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , HtmlText
        { tagStack = [P]
        , rawText = Data.Text.concat
            [ "These periods"
            , periods
            , " should be an ellipsis."
            ]
        }
    , HtmlEndTag { tagStack = [], tag = P }
    , HtmlStartTag { tagStack = [], tag = Pre, rawAttributes = "" }
    , HtmlText
        { tagStack = [Pre]
        , rawText = "This should be ignored" `Data.Text.append` periods'
        }
    , HtmlEndTag { tagStack = [], tag = Pre }
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

transformQuote' :: Quotes -> [HtmlEntity] -> [HtmlEntity]
transformQuote' settings = normalizeText . transformQuote settings

quoteInputSample :: [HtmlEntity]
quoteInputSample =
    [ HtmlStartTag { tagStack = [], tag = H1, rawAttributes = "" }
    , HtmlText { tagStack = [H1], rawText = "太陽의風俗" }
    , HtmlEndTag { tagStack = [], tag = H1 }
    , HtmlStartTag { tagStack = [], tag = H2, rawAttributes = "" }
    , HtmlText { tagStack = [H2], rawText = "어떤親한&quot;詩의벗&quot;에게" }
    , HtmlEndTag { tagStack = [], tag = H2 }
    , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , HtmlText { tagStack = [P], rawText = "(前略) " }
    , HtmlCdata
        { tagStack = [P]
        , text = "嘆息. 그것은 紳士와淑女들의 午後의禮儀가아니고 무엇이냐? "
        }
    , HtmlText
        { tagStack = [P]
        , rawText = Data.Text.concat
            [ "秘密. 어쩌면 그렇게도 粉바른할머니인 十九世紀的 "
            , "'비&#x30fc;너쓰'냐? "
            ]
        }
    , HtmlCdata
        { tagStack = [P]
        , text = "너는 그것들에게서 지금도 곰팽이냄새를 맡지못하느냐?"
        }
    , HtmlText { tagStack = [P], rawText = " (後略)" }
    , HtmlEndTag { tagStack = [], tag = P }
    ]

quoteOutputSample :: [HtmlEntity] -> [HtmlEntity] -> [HtmlEntity]
quoteOutputSample singleQuoted doubleQuoted = normalizeText $
    [ HtmlStartTag { tagStack = [], tag = H1, rawAttributes = "" }
    , HtmlText { tagStack = [H1], rawText = "太陽의風俗" }
    , HtmlEndTag { tagStack = [], tag = H1 }
    , HtmlStartTag { tagStack = [], tag = H2, rawAttributes = "" }
    , HtmlText { tagStack = [H2], rawText = "어떤親한" }
    ] ++ doubleQuoted ++
    [ HtmlText { tagStack = [H2], rawText = "에게" }
    , HtmlEndTag { tagStack = [], tag = H2 }
    , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
    , HtmlText
        { tagStack = [P]
        , rawText = Data.Text.concat
            [ "(前略) 嘆息. 그것은 紳士와淑女들의 午後의禮儀가아니고 무엇이냐? "
            , "秘密. 어쩌면 그렇게도 粉바른할머니인 十九世紀的 "
            ]
        }
    ] ++ singleQuoted ++
    [ HtmlText
        { tagStack = [P]
        , rawText =
            "냐? 너는 그것들에게서 지금도 곰팽이냄새를 맡지못하느냐? (後略)"
        }
    , HtmlEndTag { tagStack = [], tag = P }
    ]

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

    specify "transformEllipsis" $ do
        let sample x = ellipsisSample x x
        transformEllipsis (sample "...") `shouldBe`
            ellipsisSample "&hellip;" "..."
        transformEllipsis (sample "&period;.&#46;") `shouldBe`
            ellipsisSample "&hellip;" "&period;.&#46;"
        transformEllipsis (sample "。&#12290;&#x3002;") `shouldBe`
            ellipsisSample "&hellip;" "。&#12290;&#x3002;"

    describe "transformQuote" $ do
        it "transforms apostrophes and straight quotes into typographic ones" $
            transformQuote' curvedQuotes quoteInputSample `shouldBe`
                quoteOutputSample
                    [ HtmlText
                        { tagStack = [P]
                        , rawText = "&lsquo;비&#x30fc;너쓰&rsquo;"
                        }
                    ]
                    [ HtmlText
                        { tagStack = [H2]
                        , rawText = "&ldquo;詩의벗&rdquo;"
                        }
                    ]
        it "transforms apostrophes and straight quotes into guillements" $
            transformQuote' guillemets quoteInputSample `shouldBe`
                quoteOutputSample
                    [ HtmlText
                        { tagStack = [P]
                        , rawText = "&#x3008;비&#x30fc;너쓰&#x3009;"
                        }
                    ]
                    [ HtmlText
                        { tagStack = [H2]
                        , rawText = "&#x300a;詩의벗&#x300b;"
                        }
                    ]
        it "transforms straight quotes into <q> elements" $
            transformQuote' curvedSingleQuotesWithQ quoteInputSample `shouldBe`
                quoteOutputSample
                    [ HtmlText
                        { tagStack = [P]
                        , rawText = "&lsquo;비&#x30fc;너쓰&rsquo;"
                        }
                    ]
                    [ HtmlStartTag
                        { tagStack = [H2]
                        , tag = Q
                        , rawAttributes = ""
                        }
                    , HtmlText { tagStack = [H2, Q], rawText = "詩의벗" }
                    , HtmlEndTag { tagStack = [H2], tag = Q }
                    ]
        it "transforms nested quotes" $ do
            let input =
                    [ HtmlStartTag [] P ""
                    , HtmlCdata [P] "A \"nest"
                    , HtmlStartTag [P] Em ""
                    , HtmlCdata [P, Em] "ed"
                    , HtmlEndTag [P] Em
                    , HtmlCdata [P] " 'quote' "
                    , HtmlText [P] "sentence&quot; here."
                    , HtmlEndTag [] P
                    ]
            transformQuote' curvedQuotes input `shouldBe`
                [ HtmlStartTag [] P ""
                , HtmlText [P] "A &ldquo;nest"
                , HtmlStartTag [P] Em ""
                , HtmlText [P, Em] "ed"
                , HtmlEndTag [P] Em
                , HtmlText
                    [P]
                    " &lsquo;quote&rsquo; sentence&rdquo; here."
                , HtmlEndTag [] P
                ]
            transformQuote' curvedSingleQuotesWithQ input `shouldBe`
                [ HtmlStartTag [] P ""
                , HtmlText [P] "A "
                , HtmlStartTag [P] Q ""
                , HtmlText [P, Q] "nest"
                , HtmlStartTag [P, Q] Em ""
                , HtmlText [P, Q, Em] "ed"
                , HtmlEndTag [P, Q] Em
                , HtmlText [P, Q] " &lsquo;quote&rsquo; sentence"
                , HtmlEndTag [P] Q
                , HtmlText [P] " here."
                , HtmlEndTag [] P
                ]
            transformQuote' curvedQuotes [HtmlText [] "&quot;'“‘ "] `shouldBe`
                [HtmlText [] "&quot;'“‘ "]
        forM_ ignoredTags $ \ tag' -> do
            let tagStr = '<' : unpack (htmlTagName tag') ++ ">"
            it ("does not transform anything within " ++ tagStr ++ " tags") $ do
                let input =
                        [ HtmlStartTag [] tag' ""
                        , HtmlText
                            { tagStack = [tag']
                            , rawText = Data.Text.append
                                "It should not be changed: "
                                "&quot;unchanged&quot;."
                            }
                        , HtmlEndTag [] tag'
                        ]
                transformQuote' curvedQuotes input `shouldBe` input
                transformQuote' guillemets input `shouldBe` input
                transformQuote' curvedSingleQuotesWithQ input `shouldBe` input

    describe "transformEmDash" $ do
        it "transforms a single hyphen surrounded by spaces into an em dash" $
            transformEmDash
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "A hyphen - U+002D HYPHEN-MINUS &#45; to em dash."
                , HtmlEndTag [] P
                ] `shouldBe`
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "A hyphen&mdash;U+002D HYPHEN-MINUS&mdash;to em dash."
                , HtmlEndTag [] P
                ]
        it "transforms two/three consecutive hyphens into an em dash" $
            transformEmDash
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "A hyphen--U+002D HYPHEN-MINUS&#45;&#x2d;to em dash."
                , HtmlEndTag [] P
                ] `shouldBe`
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "A hyphen&mdash;U+002D HYPHEN-MINUS&mdash;to em dash."
                , HtmlEndTag [] P
                ]
        it "transforms a hangul vowel 'eu' surrounded by spaces to an em dash" $
            transformEmDash
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "한글 모음 ㅡ U+3161 HANGUL LETTER EU &#x3161; 을 줄표로"
                , HtmlEndTag [] P
                ] `shouldBe`
                [ HtmlStartTag [] P ""
                , HtmlText [P]
                    "한글 모음&mdash;U+3161 HANGUL LETTER EU&mdash;을 줄표로"
                , HtmlEndTag [] P
                ]

    describe "normalizeStops" $ do
        let periods =
                [ ". ", "&period; ", "&#46; ", "&#x2e; "
                , "。", "&#12290;", "&#x3002;"
                ] :: [Text]
        let commas =
                [ ", ", "&comma; ", "&#44; ", "&#x2c; "
                , "、", "&#12289;", "&#x3001;"
                ] :: [Text]
        let interpuncts =
                [ "·", "&middot;", "&centerdot;", "&CenterDot;"
                , "&#xB7;", "&#xb7;", "&#183;"
                ] :: [Text]
        let s = stripEnd
        let examples =
                [ [qc|봄{i1}여름{i2}가을{i3}겨울{p1}(括弧{s p3}) 어제{c}오늘{s p2}|]
                | p1 <- periods, p2 <- periods, p3 <- periods
                , c <- commas
                , i1 <- interpuncts, i2 <- interpuncts, i3 <- interpuncts
                ] :: [Text]
        let gen = unsafePerformIO getStdGen :: StdGen
        let randomInts = randomRs (0, 499) gen :: [Int]
        let sampledExamples =
                [ e
                | (e, r) <- Prelude.zip examples randomInts
                , r < 1
                ] :: [Text]
        forM_ sampledExamples $ \ example' ->
            let input =
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] example'
                    , HtmlEndTag [] P
                    ] :: [HtmlEntity]
            in specify ("normalizeStops " ++ show (unpack example')) $ do
                normalizeStops horizontalStops input `shouldBe`
                    [ HtmlStartTag [] P ""
                    , HtmlText [P]
                        "봄&#xb7;여름&#xb7;가을&#xb7;겨울. (括弧.) 어제, 오늘."
                    , HtmlEndTag [] P
                    ]
                normalizeStops verticalStops input `shouldBe`
                    [ HtmlStartTag [] P ""
                    , HtmlText [P]
                        ("봄&#xb7;여름&#xb7;가을&#xb7;겨울&#x3002;(括弧&#x3002;) " <>
                            "어제&#x3001;오늘&#x3002;")
                    , HtmlEndTag [] P
                    ]
                normalizeStops horizontalStopsWithSlashes input `shouldBe`
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "봄/여름/가을/겨울. (括弧.) 어제, 오늘."
                    , HtmlEndTag [] P
                    ]
        it "normalizes stops followed by boundaries as well" $ do
            let input =
                    [ HtmlStartTag [] P ""
                    , HtmlStartTag [P] Span ""
                    , HtmlText [P, Span] "쉼표,"
                    , HtmlEndTag [P] Span
                    , HtmlStartTag [P] Span ""
                    , HtmlText [P, Span] "마침표."
                    , HtmlEndTag [P] Span
                    , HtmlEndTag [] P
                    ] :: [HtmlEntity]
            normalizeStops verticalStops input `shouldBe`
                    [ HtmlStartTag [] P ""
                    , HtmlStartTag [P] Span ""
                    , HtmlText [P, Span] "쉼표&#x3001;"
                    , HtmlEndTag [P] Span
                    , HtmlStartTag [P] Span ""
                    , HtmlText [P, Span] "마침표&#x3002;"
                    , HtmlEndTag [P] Span
                    , HtmlEndTag [] P
                    ]
        it "does not touch inside non-Korean contents" $ do
            let input =
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "《大學》의 「三綱領」에 다음과 같은 말이 나온다。"
                    , HtmlEndTag [] P
                    , HtmlStartTag [] BlockQuote "lang=lzh"
                    , HtmlText [BlockQuote] "大學之道，在明明德，在親民，在止於至善。"
                    , HtmlEndTag [] BlockQuote
                    ] :: [HtmlEntity]
            normalizeStops horizontalStops input `shouldBe`
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "《大學》의 「三綱領」에 다음과 같은 말이 나온다."
                    , HtmlEndTag [] P
                    , HtmlStartTag [] BlockQuote "lang=lzh"
                    , HtmlText [BlockQuote] "大學之道，在明明德，在親民，在止於至善。"
                    , HtmlEndTag [] BlockQuote
                    ]
