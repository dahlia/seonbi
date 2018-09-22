{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Seonbi.Html.ClipperSpec (spec) where

import Control.Monad

import Data.Text
import Test.Hspec

import Text.Seonbi.Html.Clipper
import Text.Seonbi.Html.Entity
import Text.Seonbi.Html.Tag

spec :: Spec
spec = do
    describe "clipPrefixText" $ do
        it "returns Nothing if entities are empty and a prefix is not empty" $
            clipPrefixText "foo" [] `shouldBe` Nothing
        it "returns Nothing if the first entity is not an HtmlText" $
            forM_ (["", "foo"] :: [Text]) $ \ prefix -> do
                clipPrefixText prefix
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ] `shouldBe` Nothing
                clipPrefixText prefix
                    [ HtmlComment [] "foo"
                    , HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ] `shouldBe` Nothing
                clipPrefixText prefix [HtmlEndTag [] P] `shouldBe` Nothing
                clipPrefixText prefix [HtmlCdata [] "foo"] `shouldBe` Nothing
        it "returns Just [] if entities are empty and a prefix is empty too" $
            clipPrefixText "" [] `shouldBe` Just []
        it "returns entities with the prefix text dropped" $ do
            clipPrefixText "foo"
                [ HtmlText [] "foobar"
                , HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                ]
                `shouldBe` Just
                    [ HtmlText [] "bar"
                    , HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ]
            clipPrefixText "foo"
                [ HtmlText [] "foo"
                , HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                ]
                `shouldBe` Just
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ]
        it "ignores HtmlComment entities but preseves them" $ do
            clipPrefixText "foo"
                [ HtmlComment [] "comment"
                , HtmlText [] "foobar"
                , HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                ]
                `shouldBe` Just
                    [ HtmlComment [] "comment"
                    , HtmlText [] "bar"
                    , HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ]
            clipPrefixText "foo"
                [ HtmlComment [] "comment"
                , HtmlText [] "foo"
                , HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                ]
                `shouldBe` Just
                    [ HtmlComment [] "comment"
                    , HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ]

    describe "clipSuffixText" $ do
        it "returns Nothing if entities are empty and a suffix is not empty" $
            clipSuffixText "foo" [] `shouldBe` Nothing
        it "returns Nothing if the last entity is not an HtmlText" $
            forM_ (["", "foo"] :: [Text]) $ \ suffix -> do
                clipSuffixText suffix
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ] `shouldBe` Nothing
                clipSuffixText suffix
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    , HtmlComment [] "foo"
                    ] `shouldBe` Nothing
                clipSuffixText suffix [HtmlEndTag [] P] `shouldBe` Nothing
                clipSuffixText suffix [HtmlCdata [] "foo"] `shouldBe` Nothing
        it "returns Just [] if entities are empty and a suffix is empty too" $
            clipSuffixText "" [] `shouldBe` Just []
        it "returns entities with the suffix text dropped" $ do
            clipSuffixText "bar"
                [ HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                , HtmlText [] "foobar"
                ]
                `shouldBe` Just
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    , HtmlText [] "foo"
                    ]
            clipSuffixText "foo"
                [ HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                , HtmlText [] "foo"
                ]
                `shouldBe` Just
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    ]
        it "ignores HtmlComment entities but preseves them" $ do
            clipSuffixText "bar"
                [ HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                , HtmlText [] "foobar"
                , HtmlComment [] "comment"
                ]
                `shouldBe` Just
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    , HtmlText [] "foo"
                    , HtmlComment [] "comment"
                    ]
            clipSuffixText "foo"
                [ HtmlStartTag [] P ""
                , HtmlText [P] "foo"
                , HtmlEndTag [] P
                , HtmlText [] "foo"
                , HtmlComment [] "comment"
                ]
                `shouldBe` Just
                    [ HtmlStartTag [] P ""
                    , HtmlText [P] "foo"
                    , HtmlEndTag [] P
                    , HtmlComment [] "comment"
                    ]

    specify "clipText" $ do
        clipText "foo" "baz"
            [ HtmlText [] "foo"
            , HtmlStartTag [] P ""
            , HtmlText [P] "bar"
            , HtmlEndTag [] P
            , HtmlText [] "baz"
            ] `shouldBe` Just
                [ HtmlStartTag [] P ""
                , HtmlText [P] "bar"
                , HtmlEndTag [] P
                ]
        clipText "foo" "quux"
            [ HtmlText [] "foobar"
            , HtmlStartTag [] P ""
            , HtmlText [P] "baz"
            , HtmlEndTag [] P
            , HtmlText [] "quxquux"
            ] `shouldBe` Just
                [ HtmlText [] "bar"
                , HtmlStartTag [] P ""
                , HtmlText [P] "baz"
                , HtmlEndTag [] P
                , HtmlText [] "qux"
                ]
