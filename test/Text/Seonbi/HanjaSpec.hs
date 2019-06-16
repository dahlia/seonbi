{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.HanjaSpec (spec) where

import Test.Hspec

import Text.Seonbi.Hanja
import Text.Seonbi.Html

inputFixture :: [HtmlEntity]
inputFixture =
    [ HtmlStartTag [] H1 ""
    , HtmlText [H1] "異蹟"
    , HtmlEndTag [] H1
    , HtmlStartTag [] P ""
    , HtmlText [P] "발에 터분한 것을 다 빼어 바리고"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "黃昏이 湖水우로 걸어 오듯이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "나도 삽분삽분 걸어 보리이까?"
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "내사 이 湖水가로"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "부르는 이 없이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "불리워 온것은"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "참말 異蹟이외다."
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "오늘 따라"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "戀情, 自惚, 猜忌, 이것들이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "자꼬 金메달처럼 만져지는구려"
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "하나, 내 모든 것을 餘念없이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "물결에 써서 보내려니"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "당신은 湖面으로 나를 불러 내소서."
    , HtmlEndTag [] P
    ]

outputFixture :: [HtmlEntity]
outputFixture =
    [ HtmlStartTag [] H1 ""
    , HtmlText [H1] "리적"
    , HtmlEndTag [] H1
    , HtmlStartTag [] P ""
    , HtmlText [P] "발에 터분한 것을 다 빼어 바리고"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "황혼이 호수우로 걸어 오듯이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "나도 삽분삽분 걸어 보리이까?"
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "내사 이 호수가로"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "부르는 이 없이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "불리워 온것은"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "참말 리적이외다."
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "오늘 따라"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "련정, 자홀, 시기, 이것들이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "자꼬 금메달처럼 만져지는구려"
    , HtmlEndTag [] P
    , HtmlStartTag [] P ""
    , HtmlText [P] "하나, 내 모든 것을 여념없이"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "물결에 써서 보내려니"
    , HtmlStartTag [P] BR "", HtmlEndTag [P] BR
    , HtmlText [P] "당신은 호면으로 나를 불러 내소서."
    , HtmlEndTag [] P
    ]

spec :: Spec
spec = do
    describe "phoneticizeHanjaChar" $ do
        it "returns the input as is if not a Hanja character" $ do
            phoneticizeHanjaChar 'A' `shouldBe` 'A'
            phoneticizeHanjaChar '가' `shouldBe` '가'
        it "returns the most frequent way to read" $ do
            phoneticizeHanjaChar '金' `shouldBe` '금'
            phoneticizeHanjaChar '北' `shouldBe` '북'
        it "does not follow Initial Sound Law" $ do
            phoneticizeHanjaChar '六' `shouldBe` '륙'
            phoneticizeHanjaChar '禮' `shouldBe` '례'
    describe "phoneticizeHanjaWord" $ do
        it "returns the input as is if there are no Hanja at all" $ do
            phoneticizeHanjaWord "foo" `shouldBe` "foo"
            phoneticizeHanjaWord "테스트" `shouldBe` "테스트"
        it "transforms Hanja chars into the corresponding Hangul readings" $ do
            phoneticizeHanjaWord "漢字" `shouldBe` "한자"
            phoneticizeHanjaWord "言文" `shouldBe` "언문"
            phoneticizeHanjaWord "餘念" `shouldBe` "여념"
    specify "phoneticizeHanja" $
        normalizeText (phoneticizeHanja inputFixture) `shouldBe`
            normalizeText outputFixture
    describe "convertInitialSoundLaw" $ do
        specify "녀, 뇨, 뉴, 니 should be 여, 요, 유, 이" $ do
            convertInitialSoundLaw '녀' `shouldBe` '여'
            convertInitialSoundLaw '뉴' `shouldBe` '유'
            convertInitialSoundLaw '년' `shouldBe` '연'
            convertInitialSoundLaw '니' `shouldBe` '이'
            convertInitialSoundLaw '뇨' `shouldBe` '요'
            convertInitialSoundLaw '닉' `shouldBe` '익'
        specify "랴, 려, 례, 료, 류, 리 should be 야, 여, 예, 요, 유, 이" $ do
            convertInitialSoundLaw '량' `shouldBe` '양'
            convertInitialSoundLaw '룡' `shouldBe` '용'
            convertInitialSoundLaw '력' `shouldBe` '역'
            convertInitialSoundLaw '류' `shouldBe` '유'
            convertInitialSoundLaw '례' `shouldBe` '예'
            convertInitialSoundLaw '림' `shouldBe` '임'
        specify "라, 래, 로, 뢰, 루, 르 should be 나, 내, 노, 뇌, 누, 느" $ do
            convertInitialSoundLaw '락' `shouldBe` '낙'
            convertInitialSoundLaw '뢰' `shouldBe` '뇌'
            convertInitialSoundLaw '래' `shouldBe` '내'
            convertInitialSoundLaw '루' `shouldBe` '누'
            convertInitialSoundLaw '로' `shouldBe` '노'
            convertInitialSoundLaw '릉' `shouldBe` '능'
    specify "revertInitialSoundLaw" $ do
        revertInitialSoundLaw '여' `shouldBe` ['녀', '려']
        revertInitialSoundLaw '유' `shouldBe` ['뉴', '류']
        revertInitialSoundLaw '연' `shouldBe` ['년', '련']
        revertInitialSoundLaw '이' `shouldBe` ['니', '리']
        revertInitialSoundLaw '요' `shouldBe` ['뇨', '료']
        revertInitialSoundLaw '입' `shouldBe` ['닙', '립']
        revertInitialSoundLaw '양' `shouldBe` ['량']
        revertInitialSoundLaw '예' `shouldBe` ['례']
        revertInitialSoundLaw '낙' `shouldBe` ['락']
        revertInitialSoundLaw '뇌' `shouldBe` ['뢰']
        revertInitialSoundLaw '내' `shouldBe` ['래']
        revertInitialSoundLaw '누' `shouldBe` ['루']
        revertInitialSoundLaw '노' `shouldBe` ['로']
        revertInitialSoundLaw '능' `shouldBe` ['릉']
