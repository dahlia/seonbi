{-# LANGUAGE LambdaCase #-}
module Text.Seonbi.Html.Tag
    ( HtmlTag (..)
    , HtmlTagKind (..)
    , headingLevel
    , headingTag
    , headingTag'
    , htmlTagKind
    , htmlTagName
    , htmlTagNames
    , htmlTags
    ) where

import Data.Maybe
import Data.Map.Strict
import Data.Set
import Data.Text

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Gen
-- >>> :{
-- instance Arbitrary HtmlTag where
--     arbitrary = elements $ Data.Set.toList htmlTags
-- :}

-- | The six [kinds of HTML elements
-- ](https://www.w3.org/TR/html5/syntax.html#writing-html-documents-elements).
data HtmlTagKind
    = Void
    | Template'
    | RawText
    | EscapableRawText
    | Foreign
    | Normal
    deriving (Eq, Ord, Show)

-- | HTML tags.  This enumeration type contains both HTML 5 and 4 tags for
-- maximum compatibility.
data HtmlTag
    -- CHECK: When a new tag is added, add it into the list of htmlTags (see
    -- below).
    = A
    | Abbr
    | Acronym
    | Address
    | Area
    | Article
    | Aside
    | Audio
    | B
    | Base
    | Bdi
    | Bdo
    | Big
    | BlockQuote
    | Body
    | BR
    | Button
    | Canvas
    | Caption
    | Center
    | Cite
    | Code
    | Col
    | ColGroup
    | Data
    | DataList
    | DD
    | Del
    | Details
    | Dfn
    | Dialog
    | Div
    | DL
    | DT
    | Em
    | Embed
    | FieldSet
    | FigCaption
    | Figure
    | Footer
    | Font
    | Form
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
    | Head
    | Header
    | HR
    | Html
    | I
    | IFrame
    | Img
    | Input
    | Ins
    | Kbd
    | Label
    | Legend
    | LI
    | Link
    | Main
    | Map
    | Mark
    | Meta
    | Meter
    | Nav
    | NoBR
    | NoScript
    | Object
    | OL
    | OptGroup
    | Option
    | Output
    | P
    | Param
    | Picture
    | Pre
    | Progress
    | Q
    | RB
    | RP
    | RT
    | RTC
    | Ruby
    | S
    | Samp
    | Script
    | Select
    | Section
    | Small
    | Source
    | Span
    | Strike
    | Strong
    | Style
    | Sub
    | Summary
    | Sup
    | Table
    | TBody
    | TD
    | Template
    | TFoot
    | TextArea
    | TH
    | THead
    | Time
    | Title
    | TR
    | Track
    | TT
    | U
    | UL
    | Var
    | Video
    | WBR
    | XMP
    deriving (Eq, Ord, Show)

-- | List all supported HTML tags.
--
-- >>> htmlTags
-- fromList [A,Abbr,Acronym,Address,...,UL,Var,Video,WBR,XMP]
htmlTags :: Set HtmlTag
htmlTags = Data.Set.fromList
    [ A, Abbr, Acronym, Address, Area, Article, Aside, Audio
    , B, Base, Bdi, Bdo, Big, BlockQuote, Body, BR, Button
    , Canvas, Caption, Center, Cite, Code, Col, ColGroup
    , Data, DataList, DD, Del, Details, Dfn, Dialog, Div, DL, DT
    , Em, Embed
    , FieldSet, FigCaption, Figure, Font, Footer, Form
    , H1, H2, H3, H4, H5, H6, Head, Header, HR, Html
    , I, IFrame, Img, Input, Ins
    , Kbd
    , Label, Legend, LI, Link
    , Main, Map, Mark, Meta, Meter
    , Nav, NoBR, NoScript
    , Object, OL, OptGroup, Option, Output
    , P, Param, Picture, Pre, Progress
    , Q
    , RB, RP, RT, RTC, Ruby
    , S, Samp, Script, Select, Section, Small, Source
    , Span, Strike, Strong, Style, Sub, Summary, Sup
    , Table, TBody, TD, Template, TFoot, TextArea
    , TH, THead, Time, Title, TR, Track, TT
    , U, UL
    , Var, Video
    , WBR
    , XMP
    ]

-- | The name of an 'HtmlTag' in lowercase.
--
-- >>> htmlTagName TextArea
-- "textarea"
--
-- prop> \ t -> htmlTagName t == (toLower $ pack $ show (t :: HtmlTag))
htmlTagName :: HtmlTag -> Text
htmlTagName = toLower . pack . show

-- | The map of tag names to 'HtmlTag' values.
--
-- >>> :set -XOverloadedStrings
-- >>> Data.Map.Strict.lookup "blockquote" htmlTagNames
-- Just BlockQuote
--
-- prop> \ t -> Data.Map.Strict.lookup (htmlTagName t) htmlTagNames == Just t
htmlTagNames :: Map Text HtmlTag
htmlTagNames =
    Data.Map.Strict.fromList
        [(htmlTagName t, t) | t <- Data.Set.toList htmlTags]

-- | The kind of an 'HtmlTag'.
--
-- >>> Data.Set.filter ((== EscapableRawText) . htmlTagKind) htmlTags
-- fromList [TextArea,Title]
htmlTagKind :: HtmlTag -> HtmlTagKind
htmlTagKind = \ case
    A -> Normal
    Abbr -> Normal
    Acronym -> Normal
    Address -> Normal
    Area -> Void
    Article -> Normal
    Aside -> Normal
    Audio -> Normal
    B -> Normal
    Base -> Void
    Bdi -> Normal
    Bdo -> Normal
    Big -> Normal
    BlockQuote -> Normal
    Body -> Normal
    BR -> Void
    Button -> Normal
    Canvas -> Foreign
    Caption -> Normal
    Center -> Normal
    Cite -> Normal
    Code -> Normal
    Col -> Void
    ColGroup -> Normal
    Data -> Normal
    DataList -> Normal
    DD -> Normal
    Del -> Normal
    Details -> Normal
    Dfn -> Normal
    Dialog -> Normal
    Div -> Normal
    DL -> Normal
    DT -> Normal
    Em -> Normal
    Embed -> Void
    FieldSet -> Normal
    FigCaption -> Normal
    Figure -> Normal
    Font -> Normal
    Footer -> Normal
    Form -> Normal
    H1 -> Normal
    H2 -> Normal
    H3 -> Normal
    H4 -> Normal
    H5 -> Normal
    H6 -> Normal
    Head -> Normal
    Header -> Normal
    HR -> Void
    Html -> Normal
    I -> Normal
    IFrame -> Normal
    Img -> Void
    Input -> Void
    Ins -> Normal
    Kbd -> Normal
    Label -> Normal
    Legend -> Normal
    LI -> Normal
    Link -> Void
    Main -> Normal
    Map -> Normal
    Mark -> Normal
    Meta -> Void
    Meter -> Normal
    Nav -> Normal
    NoBR -> Normal
    NoScript -> Normal
    Object -> Normal
    OL -> Normal
    OptGroup -> Normal
    Option -> Normal
    Output -> Normal
    P -> Normal
    Param -> Void
    Picture -> Normal
    Pre -> Normal
    Progress -> Normal
    Q -> Normal
    RB -> Normal
    RP -> Normal
    RT -> Normal
    RTC -> Normal
    Ruby -> Normal
    S -> Normal
    Samp -> Normal
    Script -> RawText
    Select -> Normal
    Section -> Normal
    Small -> Normal
    Source -> Void
    Span -> Normal
    Strike -> Normal
    Strong -> Normal
    Style -> RawText
    Sub -> Normal
    Summary -> Normal
    Sup -> Normal
    Table -> Normal
    TBody -> Normal
    TD -> Normal
    Template -> Template'
    TFoot -> Normal
    TextArea -> EscapableRawText
    TH -> Normal
    THead -> Normal
    Time -> Normal
    Title -> EscapableRawText
    TR -> Normal
    Track -> Void
    TT -> Normal
    U -> Normal
    UL -> Normal
    Var -> Normal
    Video -> Normal
    WBR -> Void
    XMP -> RawText

-- | Get the heading level of an 'HtmlTag', if it is a heading tag
-- ('H1' to 'H6').
--
-- >>> headingLevel H1
-- Just 1
-- >>> headingLevel H6
-- Just 6
-- >>> headingLevel P
-- Nothing
headingLevel :: HtmlTag -> Maybe Int
headingLevel = \ case
    H1 -> Just 1
    H2 -> Just 2
    H3 -> Just 3
    H4 -> Just 4
    H5 -> Just 5
    H6 -> Just 6
    _ -> Nothing

-- | Get the heading tag with the given heading level.  If the level is
-- invalid, then 'Nothing' is returned.
--
-- >>> headingTag 1
-- Just H1
-- >>> headingTag 6
-- Just H6
-- >>> headingTag 7
-- Nothing
headingTag :: Int -> Maybe HtmlTag
headingTag = \ case
    1 -> Just H1
    2 -> Just H2
    3 -> Just H3
    4 -> Just H4
    5 -> Just H5
    6 -> Just H6
    _ -> Nothing

-- | Get the heading tag with the given heading level.  If the level is
-- greater than 6, then 'H6' is returned.  If the level is less than 1,
-- then 'H1' is returned.
--
-- >>> headingTag' 1
-- H1
-- >>> headingTag' 6
-- H6
-- >>> headingTag' 0
-- H1
-- >>> headingTag' 7
-- H6
headingTag' :: Int -> HtmlTag
headingTag' level =
    fromMaybe (if level > 6 then H6 else H1) $ headingTag level
