{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.PunctuationSpec (spec) where

import Control.Monad

import Data.Set
import Data.Text
import Test.Hspec

import Text.Seonbi.Html
import Text.Seonbi.Punctuation

sample :: HtmlTag -> [HtmlEntity]
sample tag' =
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

spec :: Spec
spec =
    describe "transformArrow" $ do
        specify "[]" $
            transformArrow [] (sample P) `shouldBe`
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
            transformArrow [LeftRight] (sample P) `shouldBe`
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
            transformArrow [DoubleArrow] (sample P) `shouldBe`
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
            transformArrow [LeftRight, DoubleArrow] (sample P) `shouldBe`
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
        let ignoredTags = [Code, Kbd, Pre, Script, Style, TextArea] :: [HtmlTag]
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
                        entities = transformArrow options (sample tag')
                    in
                        entities `shouldBe` entities
