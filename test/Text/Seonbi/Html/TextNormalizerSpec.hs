{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.TextNormalizerSpec (spec) where

import Control.Monad

import Test.Hspec

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.TagStack
import Text.Seonbi.Html.TextNormalizer

spec :: Spec
spec = do
    specify "normalizeText" $
        normalizeText
            [ HtmlText { tagStack = [], rawText = "foo " }
            , HtmlText { tagStack = [], rawText = "&amp; bar" }
            , HtmlCdata { tagStack = [], text = " & baz " }
            , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
            , HtmlText { tagStack = [P], rawText = "qux " }
            , HtmlCdata { tagStack = [P], text = "& \"quux\"" }
            , HtmlEndTag { tagStack = [], tag = P }
            , HtmlCdata { tagStack = [], text = " <end>" }
            ] `shouldBe`
            [ HtmlText { tagStack = [], rawText = "foo &amp; bar &amp; baz " }
            , HtmlStartTag { tagStack = [], tag = P, rawAttributes = "" }
            , HtmlText
                { tagStack = [P]
                , rawText = "qux &amp; &quot;quux&quot;"
                }
            , HtmlEndTag { tagStack = [], tag = P }
            , HtmlText { tagStack = [], rawText = " &lt;end&gt;" }
            ]

    describe "normalizeCdata" $ do
        let s1 = [] :: HtmlTagStack
        let s2 = [Div, P] :: HtmlTagStack
        specify "HtmlStartTag" $ do
            let entity1 = HtmlStartTag
                    { tagStack = s1
                    , tag = P
                    , rawAttributes = ""
                    }
            normalizeCdata entity1 `shouldBe` entity1
            let entity2 = HtmlStartTag
                    { tagStack = s2
                    , tag = P
                    , rawAttributes = " class=\"entity2\""
                    }
            normalizeCdata entity2 `shouldBe` entity2
        let stacks = [s1, s2] :: [HtmlTagStack]
        forM_ stacks $ \ s -> do
            specify ("HtmlEndTag: " ++ show s) $ do
                let e = HtmlEndTag { tagStack = s, tag = P }
                normalizeCdata e `shouldBe` e
            specify ("HtmlText: " ++ show s) $ do
                let e = HtmlText { tagStack = s, rawText = "foo &amp; bar" }
                normalizeCdata e `shouldBe` e
            specify ("HtmlComment: " ++ show s) $ do
                let e = HtmlComment { tagStack = s, comment = "foo" }
                normalizeCdata e `shouldBe` e
            specify ("HtmlCdata: " ++ show s) $ do
                let e = HtmlCdata { tagStack = s, text = "<p>foo & bar</p>" }
                normalizeCdata e `shouldBe`
                    HtmlText
                        { tagStack = s
                        , rawText = "&lt;p&gt;foo &amp; bar&lt;/p&gt;"
                        }

    specify "escapeHtmlEntities" $ do
        escapeHtmlEntities "<p id=\"foo\">" `shouldBe`
            "&lt;p id=&quot;foo&quot;&gt;"
        escapeHtmlEntities "AT&T" `shouldBe`
            "AT&amp;T"
