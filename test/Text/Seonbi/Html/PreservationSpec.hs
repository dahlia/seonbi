{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.PreservationSpec (spec) where

import Test.Hspec

import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Preservation
import Text.Seonbi.Html.Tag

spec :: Spec
spec = do
    specify "isPreservedTag" $ do
        P `shouldNotSatisfy` isPreservedTag
        Em `shouldNotSatisfy` isPreservedTag
        Title `shouldNotSatisfy` isPreservedTag
        Canvas `shouldSatisfy` isPreservedTag
        Code `shouldSatisfy` isPreservedTag
        Kbd `shouldSatisfy` isPreservedTag
        Pre `shouldSatisfy` isPreservedTag
        Script `shouldSatisfy` isPreservedTag
        Style `shouldSatisfy` isPreservedTag
        Template `shouldSatisfy` isPreservedTag
        TextArea `shouldSatisfy` isPreservedTag
    specify "isPreservedTagStack" $ do
        [] `shouldNotSatisfy` isPreservedTagStack
        [P, Em] `shouldNotSatisfy` isPreservedTagStack
        [Html, Head, Title] `shouldNotSatisfy` isPreservedTagStack
        [Div, Script] `shouldSatisfy` isPreservedTagStack
        [Html, Head, Style] `shouldSatisfy` isPreservedTagStack
        [P, Kbd] `shouldSatisfy` isPreservedTagStack
        [Pre, Code] `shouldSatisfy` isPreservedTagStack
        [Template, P] `shouldSatisfy` isPreservedTagStack
    specify "shouldBePreserved" $ do
        HtmlStartTag [] P "" `shouldNotSatisfy` isPreservedEntity
        HtmlEndTag [] P `shouldNotSatisfy` isPreservedEntity
        HtmlText [] "" `shouldNotSatisfy` isPreservedEntity
        HtmlCdata [] "" `shouldNotSatisfy` isPreservedEntity
        HtmlComment [] " ... " `shouldSatisfy` isPreservedEntity
        HtmlStartTag [P] Em "" `shouldNotSatisfy` isPreservedEntity
        HtmlEndTag [P] Em `shouldNotSatisfy` isPreservedEntity
        HtmlText [P] "" `shouldNotSatisfy` isPreservedEntity
        HtmlCdata [P] "" `shouldNotSatisfy` isPreservedEntity
        HtmlComment [P] " ... " `shouldSatisfy` isPreservedEntity
        HtmlStartTag [P] Code "" `shouldSatisfy` isPreservedEntity
        HtmlEndTag [P] Code `shouldSatisfy` isPreservedEntity
        HtmlStartTag [Pre] Span "" `shouldSatisfy` isPreservedEntity
        HtmlEndTag [Pre] Span `shouldSatisfy` isPreservedEntity
        HtmlText [Pre] "" `shouldSatisfy` isPreservedEntity
        HtmlCdata [Pre] "" `shouldSatisfy` isPreservedEntity
        HtmlComment [Pre] " ... " `shouldSatisfy` isPreservedEntity
