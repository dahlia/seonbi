{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.FacadeSpec (spec) where

import Data.String (IsString)

import Test.Hspec

import Text.Seonbi.Facade

input :: (IsString a, Monoid a) => a
input =
    "<blockquote><p>아이들에게 하로의 乾燥한 學課로<br>" <>
    "해말간 倦怠가 깃들고,<br>" <>
    "&quot;矛盾&quot; 두자를 理解치 못하도록<br>" <>
    "머리가 單純하였구나.</p>" <>
    "</blockquote><p>尹東柱 &lt;이런날&gt;</p>"

output :: (IsString a, Monoid a) => a
output =
    "<blockquote><p>아이들에게 하로의 건조한 학과로<br>" <>
    "해말간 권태가 깃들고,<br>" <>
    "&ldquo;모순&rdquo; 두자를 이해치 못하도록<br>" <>
    "머리가 단순하였구나.</p>" <>
    "</blockquote><p>윤동주 &#12296;이런날&#12297;</p>"

spec :: Spec
spec =
    specify "transformHtmlLazyText" $ do
        let noOp = Configuration
                { quote = Nothing
                , cite = Nothing
                , arrow = Nothing
                , ellipsis = False
                , emDash = False
                , hanja = Nothing
                , xhtml = False
                , debugLogger = Nothing
                }
        transformHtmlLazyText noOp input `shouldBe` Just input
        transformHtmlLazyText ko_KR input `shouldBe` Just output
