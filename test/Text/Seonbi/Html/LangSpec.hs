{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.LangSpec (spec) where

import Test.Hspec

import Text.Seonbi.Html
import Text.Seonbi.Html.Lang

source :: [HtmlEntity]
source =
    [ HtmlStartTag
        { tagStack = []
        , tag = P
        , rawAttributes = "id=\"foo\" lang=\"en\""
        }
    , HtmlText { tagStack = [P], rawText = "English" }
    , HtmlEndTag { tagStack = [], tag = P }
    , HtmlStartTag { tagStack = [], tag = Div, rawAttributes = "" }
    , HtmlStartTag
        { tagStack = [Div]
        , tag = P
        , rawAttributes = "class=bar lang=ja"
        }
    , HtmlStartTag { tagStack = [Div, P], tag = B, rawAttributes = "" }
    , HtmlText { tagStack = [Div, P, B], rawText = "日本語" }
    , HtmlEndTag { tagStack = [Div, P], tag = B }
    , HtmlStartTag
        { tagStack = [Div, P]
        , tag = Span
        , rawAttributes = "lang='yue-Hant'"
        }
    , HtmlText { tagStack = [Div, P, Span], rawText = "與" }
    , HtmlStartTag { tagStack = [Div, P, Span], tag = B, rawAttributes = "" }
    , HtmlText { tagStack = [Div, P, Span, B], rawText = "與粵語" }
    , HtmlEndTag { tagStack = [Div, P, Span], tag = B }
    , HtmlEndTag { tagStack = [Div, P], tag = Span }
    , HtmlEndTag { tagStack = [Div], tag = P }
    , HtmlEndTag { tagStack = [], tag = Div }
    ]

annotated :: [LangHtmlEntity]
annotated =
    [ LangHtmlEntity
        (Just "en")
        HtmlStartTag
            { tagStack = []
            , tag = P
            , rawAttributes = "id=\"foo\" lang=\"en\""
            }
    , LangHtmlEntity
        (Just "en")
        HtmlText { tagStack = [P], rawText = "English" }
    , LangHtmlEntity (Just "en") HtmlEndTag { tagStack = [], tag = P }
    , LangHtmlEntity
        Nothing
        HtmlStartTag { tagStack = [], tag = Div, rawAttributes = "" }
    , LangHtmlEntity
        (Just "ja")
        HtmlStartTag
            { tagStack = [Div]
            , tag = P
            , rawAttributes = "class=bar lang=ja"
            }
    , LangHtmlEntity
        (Just "ja")
        HtmlStartTag { tagStack = [Div, P], tag = B, rawAttributes = "" }
    , LangHtmlEntity
        (Just "ja")
        HtmlText { tagStack = [Div, P, B], rawText = "日本語" }
    , LangHtmlEntity (Just "ja") HtmlEndTag { tagStack = [Div, P], tag = B }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlStartTag
            { tagStack = [Div, P]
            , tag = Span
            , rawAttributes = "lang='yue-Hant'"
            }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlText { tagStack = [Div, P, Span], rawText = "與" }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlStartTag { tagStack = [Div, P, Span], tag = B, rawAttributes = "" }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlText { tagStack = [Div, P, Span, B], rawText = "與粵語" }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlEndTag { tagStack = [Div, P, Span], tag = B }
    , LangHtmlEntity
        (Just "yue-hant")
        HtmlEndTag { tagStack = [Div, P], tag = Span }
    , LangHtmlEntity (Just "ja") HtmlEndTag { tagStack = [Div] , tag = P }
    , LangHtmlEntity Nothing HtmlEndTag { tagStack = [], tag = Div }
    ]

spec :: Spec
spec = do
    specify "extractLang" $ do
        extractLang "" `shouldBe` Nothing
        extractLang "lang=en" `shouldBe` Just "en"
        extractLang "lang=en-US" `shouldBe` Just "en-us"
        extractLang "lang='ko-KR'" `shouldBe` Just "ko-kr"
        extractLang "lang=\"zh-Hant\"" `shouldBe` Just "zh-hant"
        extractLang "lang=\"yue-Hans-HK\"" `shouldBe` Just "yue-hans-hk"
        extractLang "id=\"foo\" lang=\"en\"" `shouldBe` Just "en"
        extractLang "id=\"foo\" lang=zh-CN class=bar" `shouldBe` Just "zh-cn"
    specify "annotateWithLang" $ do
        annotateWithLang [] `shouldBe` []
        annotateWithLang source `shouldBe` annotated
