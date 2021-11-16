{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.ContentTypesSpec (spec) where

import Data.Text
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder

import Test.Hspec

import Text.Seonbi.Html
import Text.Seonbi.ContentTypes
import qualified HTMLEntities.Builder
import HTMLEntities.Decoder

textReverser :: (Monad m, MonadFail m) => HtmlTransformer m
textReverser entities =
    return $ reverseText <$> entities
  where
    reverseText :: HtmlEntity -> HtmlEntity
    reverseText e@HtmlText { rawText = t } =
        e { rawText = encode $ Data.Text.reverse $ decode t }
    reverseText e@HtmlCdata { text = t } =
        e { text = Data.Text.reverse t }
    reverseText e =
        e
    decode :: Text -> Text
    decode = LT.toStrict . toLazyText . htmlEncodedText
    encode :: Text -> Text
    encode = LT.toStrict . toLazyText . HTMLEntities.Builder.text

spec :: Spec
spec = do
    specify "asHtmlTransformer" $ do
        r <- asHtmlTransformer textReverser "<p>foo <em>bar</em><br> baz</p>"
        r `shouldBe` "<p> oof<em>rab</em><br>zab </p>"
    specify "asXhtmlTransformer" $ do
        r <- asXhtmlTransformer textReverser "<p>foo <em>bar</em><br> baz</p>"
        r `shouldBe` "<p> oof<em>rab</em><br/>zab </p>"
    specify "asPlainTextTransformer" $ do
        r <- asPlainTextTransformer textReverser
                "<p>foo <em>bar</em><br> baz</p>"
        r `shouldBe` ">p/<zab >rb<>me/<rab>me< oof>p<"
    specify "asCommonMarkTransformer" $ do
        r <- asCommonMarkTransformer textReverser
                "# Foo\n\nBar *Baz*\nQux\n\n> Quote <em>tag</em>\n"
        r `shouldBe` "# ooF\n\n raB*zaB*\nxuQ\n\n>  etouQ<em>gat</em>\n"
    specify "transformWithContentType" $ do
        let input = "*foo* <em>bar</em><br>"
        h <- transformWithContentType "text/html" textReverser input
        h `shouldBe` " *oof*<em>rab</em><br>"
        x <- transformWithContentType "application/xhtml+xml" textReverser input
        x `shouldBe` " *oof*<em>rab</em><br/>"
        p <- transformWithContentType "text/plain" textReverser input
        p `shouldBe` ">rb<>me/<rab>me< *oof*"
        m <- transformWithContentType "text/markdown" textReverser input
        m `shouldBe` "*oof* <em>rab</em><br>\n"
