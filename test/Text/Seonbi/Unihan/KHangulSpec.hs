{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Unihan.KHangulSpec (spec) where

import Data.Either

import Data.Aeson
import Data.Map.Strict
import Test.Hspec

import Text.Seonbi.Unihan.KHangul

spec :: Spec
spec = do
    describe "kHangulData'" $
        it "should be loaded" $
            kHangulData' `shouldSatisfy` isRight
    describe "kHangulData" $
        it "contains Hanja Hangul readings" $
            Data.Map.Strict.lookup '識' kHangulData `shouldBe` Just
                [ ('식', HanjaReadingCitation KS_X_1001 [Education])
                , ('지', HanjaReadingCitation KS_X_1001 [PersonalName])
                ]
    describe "HanjaReadingCitation" $
        specify "parseJSON" $ do
            decode "\"\"" `shouldBe` Just (HanjaReadingCitation NonStandard [])
            decode "\"E\"" `shouldBe` Just
                (HanjaReadingCitation NonStandard [Education])
            decode "\"N\"" `shouldBe` Just
                (HanjaReadingCitation NonStandard [PersonalName])
            decode "\"EN\"" `shouldBe` Just
                (HanjaReadingCitation NonStandard [Education, PersonalName])
            decode "\"0\"" `shouldBe` Just (HanjaReadingCitation KS_X_1001 [])
            decode "\"1\"" `shouldBe` Just (HanjaReadingCitation KS_X_1002 [])
            decode "\"0E\"" `shouldBe` Just
                (HanjaReadingCitation KS_X_1001 [Education])
            decode "\"1N\"" `shouldBe` Just
                (HanjaReadingCitation KS_X_1002 [PersonalName])
            decode "\"2\"" `shouldBe` (Nothing :: Maybe HanjaReadingCitation)
            decode "\"00\"" `shouldBe` (Nothing :: Maybe HanjaReadingCitation)
            decode "\"0Z\"" `shouldBe` (Nothing :: Maybe HanjaReadingCitation)
            decode "0" `shouldBe` (Nothing :: Maybe HanjaReadingCitation)
            decode "null" `shouldBe` (Nothing :: Maybe HanjaReadingCitation)
