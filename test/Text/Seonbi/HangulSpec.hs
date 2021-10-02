{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.HangulSpec (spec) where

import Test.Hspec

import Text.Seonbi.Hangul


spec :: Spec
spec = do
    specify "isHangulSyllable" $ do
        '가' `shouldSatisfy` isHangulSyllable
        '글' `shouldSatisfy` isHangulSyllable
        'A' `shouldNotSatisfy` isHangulSyllable
        '?' `shouldNotSatisfy` isHangulSyllable
        '字' `shouldNotSatisfy` isHangulSyllable
    describe "toJamoTriple" $ do
        it "returns only initial cosonant and vowel if there is no batchim" $
            toJamoTriple '가' `shouldBe` Just ('ᄀ', 'ᅡ', Nothing)
        it "returns all of triple if there is a batchim" $ do
            toJamoTriple '글' `shouldBe` Just ('ᄀ', 'ᅳ', Just 'ᆯ')
            toJamoTriple '를' `shouldBe` Just ('ᄅ', 'ᅳ', Just 'ᆯ')
        it "returns Nothing for non-Hangul letters" $ do
            toJamoTriple 'A' `shouldBe` Nothing
            toJamoTriple '?' `shouldBe` Nothing
            toJamoTriple '字' `shouldBe` Nothing
    specify "fromJamoTriple" $ do
        fromJamoTriple ('ᄀ', 'ᅡ', Nothing) `shouldBe` Just '가'
        fromJamoTriple ('ᄀ', 'ᅳ', Just 'ᆯ') `shouldBe` Just '글'
        fromJamoTriple ('ᄅ', 'ᅳ', Just 'ᆯ') `shouldBe` Just '를'
        fromJamoTriple ('ᄓ', 'ᅳ', Nothing) `shouldBe` Nothing
        fromJamoTriple ('ᄀ', 'ᅶ', Nothing) `shouldBe` Nothing
        fromJamoTriple ('ᄀ', 'ᅳ', Just 'ᅡ') `shouldBe` Nothing
        fromJamoTriple ('ᄀ', 'ᅳ', Just 'ᇇ') `shouldBe` Nothing
