{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.ContentTypesSpec (spec) where

import Data.Text

import Test.Hspec

import Text.Seonbi.Html
import Text.Seonbi.ContentTypes

textReverser :: (Monad m, MonadFail m) => HtmlTransformer m
textReverser entities =
    return $ reverseText <$> entities
  where
    reverseText :: HtmlEntity -> HtmlEntity
    reverseText e@HtmlText { rawText = t } = e { rawText = Data.Text.reverse t }
    reverseText e@HtmlCdata { text = t } = e { text = Data.Text.reverse t }
    reverseText e = e

spec :: Spec
spec = do
    specify "asHtmlTransformer" $ do
        r <- asHtmlTransformer textReverser "<p>foo <em>bar</em><br> baz</p>"
        r `shouldBe` "<p> oof<em>rab</em><br>zab </p>"
    specify "asXhtmlTransformer" $ do
        r <- asXhtmlTransformer textReverser "<p>foo <em>bar</em><br> baz</p>"
        r `shouldBe` "<p> oof<em>rab</em><br/>zab </p>"
    specify "transformWithContentType" $ do
        let input = "<p>foo <em>bar</em><br></p>"
        h <- transformWithContentType "text/html" textReverser input
        h `shouldBe` "<p> oof<em>rab</em><br></p>"
        x <- transformWithContentType "application/xhtml+xml" textReverser input
        x `shouldBe` "<p> oof<em>rab</em><br/></p>"

