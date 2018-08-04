{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.WrapperSpec (spec) where

import Test.Hspec

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag
import Text.Seonbi.Html.Wrapper

spec :: Spec
spec =
    specify "wrap" $
        wrap [Div, Article] BlockQuote " class=\"q\""
            [ HtmlStartTag
                { tagStack = [Div, Article]
                , tag = P
                , rawAttributes = ""
                }
            , HtmlText { tagStack = [Div, Article, P], rawText = "foo" }
            , HtmlStartTag
                { tagStack = [Div, Article, P]
                , tag = Em
                , rawAttributes = ""
                }
            , HtmlCdata { tagStack = [Div, Article, P, Em], text = "bar" }
            , HtmlEndTag { tagStack = [Div, Article, P], tag = Em }
            , HtmlComment { tagStack = [Div, Article, P], comment = " baz " }
            , HtmlEndTag { tagStack = [Div, Article], tag = P }
            ] `shouldBe`
            [ HtmlStartTag
                { tagStack = [Div, Article]
                , tag = BlockQuote
                , rawAttributes = " class=\"q\""
                }
            , HtmlStartTag
                { tagStack = [Div, Article, BlockQuote]
                , tag = P
                , rawAttributes = ""
                }
            , HtmlText
                { tagStack = [Div, Article, BlockQuote, P]
                , rawText = "foo"
                }
            , HtmlStartTag
                { tagStack = [Div, Article, BlockQuote, P]
                , tag = Em
                , rawAttributes = ""
                }
            , HtmlCdata
                { tagStack = [Div, Article, BlockQuote, P, Em]
                , text = "bar"
                }
            , HtmlEndTag { tagStack = [Div, Article, BlockQuote, P], tag = Em }
            , HtmlComment
                { tagStack = [Div, Article, BlockQuote, P]
                , comment = " baz "
                }
            , HtmlEndTag { tagStack = [Div, Article, BlockQuote], tag = P }
            , HtmlEndTag { tagStack = [Div, Article], tag = BlockQuote }
            ]
