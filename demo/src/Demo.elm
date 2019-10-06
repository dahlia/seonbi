module Demo exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as Cdn
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as GCol
import Bootstrap.Grid.Row as GRow
import Bootstrap.Modal as Modal
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode
import Json.Encode
import List
import Markdown.Block
import Markdown.HtmlString
import Maybe
import Regex
import Set
import String
import SyntaxHighlight exposing (gitHub, toBlockHtml, useTheme, xml)
import Tuple
import Url


apiServerUrl : String
apiServerUrl =
    "https://seonbi.herokuapp.com/"


initialContent : String
initialContent =
    """# 선비: 韓國語를 爲한 SmartyPants

선비는 韓國 國立國語院의 <<[한글 맞춤法][1]>> 또는 北朝鮮의 <<朝鮮말規範集>>에서
定한 句讀法에 맞도록 글의 句讀點 等의 使用을 校正해주는 HTML 前處理器이다.
([SmartyPants]가 英語에 對해 해주는 處理와 비슷하다.)

또한, `ko-Kore` 텍스트, 卽, 國漢文混用體를 `ko-Hang` 텍스트, 卽, 한글專用으로도
變換한다.

선비는 Haskell 라이브러리 또는 CLI 또는 HTTP API로 쓸 수 있으며, 어떤 方式이든
아래의 變換을 한다 (各各은 켜고 끌 수 있다).

- 모든 漢字語(例: `漢字`)를 한글로 (例: `한자`).
- 直線形 따옴標(`"` 및  `'`)를 曲線形 따옴標(`“`·`”` 및 `‘`·`’`)로.
- 連달아 찍은 마침標(`...`)를 말줄임標(`…`)로.
- 數學 不等號 짝(`<`와 `>`)을 제대로 된 홑화살括弧 짝(`〈`와 `〉`)으로.
- 두 겹의 數學 不等號 짝(`<<`와 `>>`)을 제대로 된 겹화살括弧 짝(`《`와
  `》`)으로.
- 空白으로 둘러쌓인 하이픈(`-`)이나 한글 母音 으(`ㅡ`), 또는 둘이나 세 番
  連續된 하이픈(`--`이나 `---`)을 제대로 된 줄標(`—`)로.
- “보다 작다” 不等號와 이어지는 하이픈 또는 等號(`<-`, `<=`)를 왼쪽 화살標(`←`,
  `⇐`)로.
- 하이픈 또는 等號와 이어지는 “보다 크다” 不等號(`->` `=>`)를 오른족 화살標(`→`,
  `⇒`)로.
- 不等號로 둘러쌓인 하이픈 또는 等號(`<->`, `<=>`)를 양쪽 화살標(`↔`, `⇔`)로.

變換은 HTML 水準에서 이뤄지므로, CommonMark나 Markdown, Textile 等의 마크업
言語와도 잘 붙는다.  SmartyPants와 마찬가지로, 文字 그대로 解釋되어야 하는
`<pre>`·`<code>`·`<script>`·`<kbd>` 같은 HTML 태그 안쪽은 變換되지 않는다.

[1]: http://kornorms.korean.go.kr/regltn/regltnView.do
[SmartyPants]: https://daringfireball.net/projects/smartypants/
"""


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { source : Source
    , lastCustomOptions : CustomOptions
    , loading : Bool
    , lastTransformation : Maybe Source
    , resultHtml : Maybe String
    , tabState : Tab.State
    , customDictionaryVisibility : Modal.Visibility
    , customDictionarySource : String
    , customDictionary : Dict.Dict String String
    }


type alias Source =
    { text : Maybe String
    , html : String
    , options : Options
    }


type Options
    = KoKr
    | KoKp
    | Custom CustomOptions


type alias CustomOptions =
    { xhtml : Bool
    , quote : QuoteOption
    , cite : Maybe CiteOption
    , arrow : Maybe ArrowOption
    , ellipsis : Bool
    , emDash : Bool
    , hanja : Maybe HanjaOption
    }


type QuoteOption
    = CurvedQuotes
    | Guillemets
    | CurvedSingleQuotesWithQ


type CiteOption
    = AngleQuotes
    | CornerBrackets
    | AngleQuotesWithCite
    | CornerBracketsWithCite


type alias ArrowOption =
    { bidirArrow : Bool
    , doubleArrow : Bool
    }


type alias HanjaOption =
    { rendering : HanjaRenderingOption
    , reading : HanjaReadingOption
    }


type HanjaRenderingOption
    = HangulOnly
    | HanjaInParentheses
    | DisambiguatingHanjaInParentheses
    | HanjaInRuby


type alias HanjaReadingOption =
    { initialSoundLaw : Bool
    , useDictionaries : Set.Set String
    , dictionary : Dict.Dict String String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { source =
                { text = Just initialContent
                , html = renderMarkdown initialContent
                , options = KoKr
                }
            , lastCustomOptions =
                { xhtml = False
                , quote = CurvedQuotes
                , cite = Just AngleQuotes
                , arrow = Just { bidirArrow = True, doubleArrow = True }
                , ellipsis = True
                , emDash = True
                , hanja =
                    Just
                        { rendering = DisambiguatingHanjaInParentheses
                        , reading =
                            { initialSoundLaw = True
                            , useDictionaries = Set.singleton "kr-stdict"
                            , dictionary = Dict.empty
                            }
                        }
                }
            , loading = True
            , lastTransformation = Nothing
            , resultHtml = Just ""
            , tabState = Tab.initialState
            , customDictionaryVisibility = Modal.hidden
            , customDictionarySource = ""
            , customDictionary = Dict.empty
            }
    in
    ( model, transform model.source )


type Msg
    = UpdateSourceText String
    | UpdateSourceHtml String
    | BeginTransform
    | EndTransform Source (Result Http.Error String)
    | ChangeTab Tab.State
    | UpdateOptions Options
    | ShowCustomDictionary
    | CloseCustomDictionary
    | UpdateCustomDictionarySourcre String


makeInput : Source -> ( String, Json.Encode.Value )
makeInput source =
    ( apiServerUrl
    , Json.Encode.object <|
        (::) ( "sourceHtml", Json.Encode.string source.html ) <|
            case source.options of
                KoKr ->
                    [ ( "preset", Json.Encode.string "ko-kr" ) ]

                KoKp ->
                    [ ( "preset", Json.Encode.string "ko-kp" ) ]

                Custom options ->
                    [ ( "xhtml", Json.Encode.bool options.xhtml )
                    , ( "quote"
                      , Json.Encode.string <|
                            case options.quote of
                                CurvedQuotes ->
                                    "CurvedQuotes"

                                Guillemets ->
                                    "Guillemets"

                                CurvedSingleQuotesWithQ ->
                                    "CurvedSingleQuotesWithQ"
                      )
                    , ( "cite"
                      , case options.cite of
                            Nothing ->
                                Json.Encode.null

                            Just c ->
                                Json.Encode.string <|
                                    case c of
                                        AngleQuotes ->
                                            "AngleQuotes"

                                        CornerBrackets ->
                                            "CornerBrackets"

                                        AngleQuotesWithCite ->
                                            "AngleQuotesWithCite"

                                        CornerBracketsWithCite ->
                                            "CornerBracketsWithCite"
                      )
                    , ( "arrow"
                      , case options.arrow of
                            Nothing ->
                                Json.Encode.null

                            Just a ->
                                Json.Encode.object
                                    [ ( "bidirArrow"
                                      , Json.Encode.bool a.bidirArrow
                                      )
                                    , ( "doubleArrow"
                                      , Json.Encode.bool a.doubleArrow
                                      )
                                    ]
                      )
                    , ( "ellipsis", Json.Encode.bool options.ellipsis )
                    , ( "emDash", Json.Encode.bool options.emDash )
                    , ( "hanja"
                      , case options.hanja of
                            Nothing ->
                                Json.Encode.null

                            Just { rendering, reading } ->
                                [ ( "rendering"
                                  , Json.Encode.string <|
                                        case rendering of
                                            HangulOnly ->
                                                "HangulOnly"

                                            HanjaInParentheses ->
                                                "HanjaInParentheses"

                                            DisambiguatingHanjaInParentheses ->
                                                "DisambiguatingHanjaInParentheses"

                                            HanjaInRuby ->
                                                "HanjaInRuby"
                                  )
                                , ( "reading"
                                  , [ ( "initialSoundLaw"
                                      , Json.Encode.bool reading.initialSoundLaw
                                      )
                                    , ( "useDictionaries"
                                      , Json.Encode.set
                                            Json.Encode.string
                                            reading.useDictionaries
                                      )
                                    , ( "dictionary"
                                      , Json.Encode.dict
                                            identity
                                            Json.Encode.string
                                            reading.dictionary
                                      )
                                    ]
                                        |> Json.Encode.object
                                  )
                                ]
                                    |> Json.Encode.object
                      )
                    ]
    )


transform : Source -> Cmd Msg
transform source =
    let
        ( url_, input ) =
            makeInput source
    in
    Http.post
        { url = url_
        , body = Http.jsonBody input
        , expect =
            Http.expectJson (EndTransform source) <|
                Json.Decode.field "resultHtml" Json.Decode.string
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSourceText markdown ->
            ( { model
                | source =
                    { text = Just markdown
                    , html = renderMarkdown markdown
                    , options = model.source.options
                    }
                , resultHtml =
                    case model.resultHtml of
                        Nothing ->
                            Just ""

                        r ->
                            r
              }
            , Cmd.none
            )

        UpdateSourceHtml sourceHtml_ ->
            ( { model
                | source =
                    { text = Nothing
                    , html = sourceHtml_
                    , options = model.source.options
                    }
              }
            , Cmd.none
            )

        BeginTransform ->
            ( { model | loading = True }, transform model.source )

        EndTransform source result ->
            ( case result of
                Ok resultHtml_ ->
                    { model
                        | resultHtml = Just resultHtml_
                        , loading = False
                        , lastTransformation = Just source
                    }

                _ ->
                    { model | resultHtml = Nothing, loading = False }
            , Cmd.none
            )

        ChangeTab tabState ->
            ( { model | tabState = tabState }, Cmd.none )

        UpdateOptions options ->
            let
                source =
                    model.source
            in
            ( { model
                | source = { source | options = options }
                , lastCustomOptions =
                    case options of
                        Custom o ->
                            o

                        _ ->
                            model.lastCustomOptions
              }
            , Cmd.none
            )

        ShowCustomDictionary ->
            ( { model
                | customDictionaryVisibility = Modal.shown
                , customDictionarySource =
                    Dict.toList model.customDictionary
                        |> List.map (\( k, v ) -> k ++ " → " ++ v ++ "\n")
                        |> String.concat
              }
            , Cmd.none
            )

        CloseCustomDictionary ->
            ( { model | customDictionaryVisibility = Modal.hidden }, Cmd.none )

        UpdateCustomDictionarySourcre source ->
            let
                arrowPattern =
                    Regex.fromString " *-> *"
                        |> Maybe.withDefault Regex.never

                incompletePattern =
                    Regex.fromString "(^|(?:.|\n)*\n)((?:[^-→\n]|-[^>\n])*)\n+$"
                        |> Maybe.withDefault Regex.never

                completePattern =
                    Regex.fromString
                        "(?:^|\n)((?:[^-→\n]|-[^>])+) *(?:->|→) ([^\n]+)*"
                        |> Maybe.withDefault Regex.never

                newSource =
                    Regex.replace arrowPattern (\_ -> " → ") <|
                        case Regex.find incompletePattern source of
                            [] ->
                                source

                            { submatches } :: _ ->
                                case submatches of
                                    [ prefix, Just incomplete ] ->
                                        Maybe.withDefault "" prefix
                                            ++ incomplete
                                            ++ " → "

                                    _ ->
                                        source

                dict =
                    Regex.find completePattern newSource
                        |> List.filterMap
                            (\match ->
                                case match.submatches of
                                    [ Just key, Just value ] ->
                                        Just
                                            ( String.trim key
                                            , String.trim value
                                            )

                                    _ ->
                                        Nothing
                            )
                        |> Dict.fromList

                source_ =
                    model.source

                updateHanjaOptions hanjaOptions =
                    { hanjaOptions
                        | reading =
                            let
                                reading =
                                    hanjaOptions.reading
                            in
                            { reading
                                | dictionary = dict
                            }
                    }

                updateCustomOptions customOptions =
                    { customOptions
                        | hanja =
                            Maybe.map
                                updateHanjaOptions
                                customOptions.hanja
                    }
            in
            ( { model
                | customDictionarySource = newSource
                , customDictionary = dict
                , source =
                    { source_
                        | options =
                            case source_.options of
                                Custom customOptions ->
                                    Custom <|
                                        updateCustomOptions customOptions

                                other ->
                                    other
                    }
                , lastCustomOptions =
                    updateCustomOptions model.lastCustomOptions
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Grid.container [ Spacing.mt3 ]
        [ Cdn.stylesheet
        , Grid.row []
            [ Grid.col []
                [ Tab.config ChangeTab
                    |> Tab.items
                        [ viewMarkdownTab model
                        , viewHtmlTab model
                        , viewHttpTab model
                        ]
                    |> Tab.view model.tabState
                ]
            , Grid.col []
                [ Tab.config ChangeTab
                    |> Tab.items
                        [ viewRenderTab model
                        , viewCodeTab model
                        ]
                    |> Tab.view model.tabState
                ]
            ]
        , Grid.row []
            [ Grid.col [] [ viewOptions model ] ]
        , Grid.row [ GRow.attrs [ Spacing.mt3 ] ]
            [ Grid.col []
                [ Button.button
                    [ Button.onClick BeginTransform
                    , case model.resultHtml of
                        Just _ ->
                            Button.primary

                        Nothing ->
                            Button.secondary
                    , Button.large
                    , Button.disabled <|
                        model.loading
                            || model.lastTransformation
                            == Just model.source
                    , Button.attrs [ Size.w100 ]
                    ]
                    [ text <|
                        if model.loading then
                            "(Being transformed…)"

                        else
                            case model.resultHtml of
                                Just _ ->
                                    "Transform"

                                Nothing ->
                                    "(Server did not respond.)"
                    ]
                ]
            ]
        ]


tabPaneAttrs : List (Attribute msg)
tabPaneAttrs =
    [ Spacing.mt3
    , style "height" "calc(100vh - 380px)"
    , style "width" "540px"
    , style "overflow" "scroll"
    ]


viewMarkdownTab : Model -> Tab.Item Msg
viewMarkdownTab model =
    Tab.item
        { id = "commonmark"
        , link =
            Tab.link [] [ text "Markdown" ]
        , pane =
            Tab.pane
                tabPaneAttrs
                [ Textarea.textarea
                    [ Textarea.onInput UpdateSourceText
                    , Textarea.rows 24
                    , Textarea.attrs [ Size.h100 ]
                    , Textarea.value <|
                        case model.source.text of
                            Just text ->
                                text

                            Nothing ->
                                "(HTML cannot be reverted to Markdown.  "
                                    ++ "If you change this Markdown text, "
                                    ++ "the raw HTML you wrote will go.)"
                    ]
                ]
        }


viewHtmlTab : Model -> Tab.Item Msg
viewHtmlTab model =
    Tab.item
        { id = "html"
        , link = Tab.link [] [ text "HTML" ]
        , pane =
            Tab.pane
                tabPaneAttrs
                [ Textarea.textarea
                    [ Textarea.onInput UpdateSourceHtml
                    , Textarea.rows 20
                    , Textarea.value model.source.html
                    ]
                ]
        }


viewHttpTab : Model -> Tab.Item Msg
viewHttpTab model =
    let
        ( urlString, args ) =
            makeInput model.source

        url =
            case Url.fromString urlString of
                Just u ->
                    u

                Nothing ->
                    { protocol = Url.Http
                    , host = ""
                    , port_ = Nothing
                    , path = ""
                    , query = Nothing
                    , fragment = Nothing
                    }

        port_ =
            case url.port_ of
                Just portNum ->
                    ":" ++ String.fromInt portNum

                Nothing ->
                    ""

        queryString =
            case url.query of
                Just qs ->
                    "?" ++ qs

                Nothing ->
                    ""
    in
    Tab.item
        { id = "http"
        , link = Tab.link [] [ text "HTTP" ]
        , pane =
            Tab.pane
                tabPaneAttrs
                [ pre [] <|
                    [ text "POST "
                    , text (url.path ++ queryString)
                    , text " HTTP/1.1"
                    , br [] []
                    , text "Host: "
                    , text (url.host ++ port_)
                    , br [] []
                    , text "Content-Type: application/json"
                    , br [] []
                    , br [] []
                    , text (Json.Encode.encode 2 args)
                    ]
                ]
        }


viewRenderTab : Model -> Tab.Item Msg
viewRenderTab model =
    Tab.item
        { id = "render"
        , link = Tab.link [] [ text "Render" ]
        , pane =
            Tab.pane tabPaneAttrs <|
                case model.resultHtml of
                    Just html ->
                        case Html.Parser.run html of
                            Ok nodes ->
                                Html.Parser.Util.toVirtualDom nodes

                            Err _ ->
                                []

                    _ ->
                        []
        }


viewCodeTab : Model -> Tab.Item Msg
viewCodeTab model =
    Tab.item
        { id = "code"
        , link = Tab.link [] [ text "Code" ]
        , pane =
            Tab.pane tabPaneAttrs
                [ div [] <|
                    case model.resultHtml of
                        Just resultHtml ->
                            [ useTheme gitHub
                            , xml resultHtml
                                |> Result.map (toBlockHtml (Just 1))
                                |> Result.withDefault (text resultHtml)
                            ]

                        Nothing ->
                            []
                ]
        }


renderMarkdown : String -> String
renderMarkdown =
    Markdown.Block.parse Nothing
        >> Markdown.HtmlString.render


viewOptions : Model -> Html Msg
viewOptions model =
    let
        options =
            model.source.options

        lastOptions =
            model.lastCustomOptions

        custom =
            case options of
                Custom _ ->
                    True

                _ ->
                    False

        preset =
            not custom

        hanjaTransformation =
            case options of
                Custom { hanja } ->
                    hanja /= Nothing

                _ ->
                    False

        checkboxOn enabled getOption updateOption label =
            let
                checked =
                    case options of
                        Custom o ->
                            getOption o

                        _ ->
                            False

                event =
                    UpdateOptions << Custom << updateOption <| not checked
            in
            Form.label
                [ onClick event ]
                [ Checkbox.checkbox
                    [ Checkbox.disabled (not enabled)
                    , Checkbox.checked checked
                    , Checkbox.onCheck (\_ -> event)
                    , Checkbox.inline
                    ]
                    label
                ]

        checkbox =
            checkboxOn custom

        listAt list idx =
            if idx < 0 then
                Nothing

            else if idx == 0 then
                List.head list

            else
                case list of
                    [] ->
                        Nothing

                    _ :: xs ->
                        listAt xs (idx - 1)

        select attrs getOption updateOption items =
            Select.select
                ([ Select.onChange <|
                    \v ->
                        (UpdateOptions << Custom << updateOption) <|
                            Maybe.andThen
                                (Maybe.andThen Tuple.first << listAt items)
                                (String.toInt v)
                 ]
                    ++ attrs
                )
            <|
                List.indexedMap
                    (\i ( v, label ) ->
                        Select.item
                            [ value (String.fromInt i)
                            , selected <|
                                case options of
                                    Custom o ->
                                        getOption o == v

                                    _ ->
                                        False
                            ]
                            [ text label ]
                    )
                    items
    in
    Form.form []
        [ Form.row []
            [ Form.colLabel [ GCol.sm1 ] [ text "Preset" ]
            , Form.col [ GCol.sm6, GCol.attrs [ Spacing.mt2 ] ] <|
                Radio.radioList
                    "preset"
                    [ Radio.create
                        [ Radio.inline
                        , Radio.checked (options == KoKr)
                        , Radio.onClick (UpdateOptions KoKr)
                        ]
                        "South Korean"
                    , Radio.create
                        [ Radio.inline
                        , Radio.checked (options == KoKp)
                        , Radio.onClick (UpdateOptions KoKp)
                        ]
                        "North Korean"
                    , Radio.create
                        [ Radio.inline
                        , Radio.checked custom
                        , Radio.onClick <| UpdateOptions (Custom lastOptions)
                        ]
                        "Custom"
                    ]
            , Form.colLabel [ GCol.sm1 ] [ text "Format" ]
            , Form.col [ GCol.sm4, GCol.attrs [ Spacing.mt2 ] ] <|
                [ checkbox
                    (\o -> o.xhtml)
                    (\checked -> { lastOptions | xhtml = checked })
                    "XHTML"
                ]
            ]
        , Form.row []
            [ Form.colLabel [ GCol.sm1 ] [ text "Quotes" ]
            , Form.col [ GCol.sm6, GCol.attrs [ Spacing.mt2 ] ] <|
                Radio.radioList
                    "quote"
                    [ Radio.create
                        [ Radio.inline
                        , Radio.disabled preset
                        , Radio.checked <|
                            case options of
                                Custom o ->
                                    o.quote == CurvedQuotes

                                _ ->
                                    False
                        , Radio.onClick <|
                            UpdateOptions <|
                                Custom
                                    { lastOptions | quote = CurvedQuotes }
                        ]
                        "Curved quotes"
                    , Radio.create
                        [ Radio.inline
                        , Radio.disabled preset
                        , Radio.checked <|
                            case options of
                                Custom o ->
                                    o.quote == Guillemets

                                _ ->
                                    False
                        , Radio.onClick <|
                            UpdateOptions <|
                                Custom
                                    { lastOptions | quote = Guillemets }
                        ]
                        "Guillemets"
                    , Radio.create
                        [ Radio.inline
                        , Radio.disabled preset
                        , Radio.checked <|
                            case options of
                                Custom o ->
                                    o.quote == CurvedSingleQuotesWithQ

                                _ ->
                                    False
                        , Radio.onClick <|
                            UpdateOptions <|
                                Custom
                                    { lastOptions
                                        | quote = CurvedSingleQuotesWithQ
                                    }
                        ]
                        "Curved quotes with <q>"
                    ]
            , Form.colLabel [ GCol.sm1 ] [ text "Punctuation" ]
            , Form.col [ GCol.sm4, GCol.attrs [ Spacing.mt2 ] ] <|
                [ checkbox
                    (\o -> o.ellipsis)
                    (\checked -> { lastOptions | ellipsis = checked })
                    "Ellipsis"
                , checkbox
                    (\o -> o.emDash)
                    (\checked -> { lastOptions | emDash = checked })
                    "Em dash"
                ]
            ]
        , Form.row []
            [ Form.colLabel [ GCol.sm1 ] [ text "Arrow" ]
            , Form.col [ GCol.sm6, GCol.attrs [ Spacing.mt2 ] ] <|
                [ checkbox
                    (\o -> o.arrow /= Nothing)
                    (\checked ->
                        { lastOptions
                            | arrow =
                                if checked then
                                    Just
                                        { bidirArrow = True
                                        , doubleArrow = True
                                        }

                                else
                                    Nothing
                        }
                    )
                    "Arrow"
                , checkboxOn
                    (case options of
                        Custom { arrow } ->
                            arrow /= Nothing

                        _ ->
                            False
                    )
                    (\o ->
                        case o.arrow of
                            Just a ->
                                a.bidirArrow

                            _ ->
                                False
                    )
                    (\checked ->
                        { lastOptions
                            | arrow =
                                Just
                                    { bidirArrow = checked
                                    , doubleArrow =
                                        Maybe.map
                                            (\a -> a.doubleArrow)
                                            lastOptions.arrow
                                            |> Maybe.withDefault False
                                    }
                        }
                    )
                    "Bidirection"
                , checkboxOn
                    (case options of
                        Custom { arrow } ->
                            arrow /= Nothing

                        _ ->
                            False
                    )
                    (\o ->
                        case o.arrow of
                            Just a ->
                                a.doubleArrow

                            _ ->
                                False
                    )
                    (\checked ->
                        { lastOptions
                            | arrow =
                                Just
                                    { bidirArrow =
                                        Maybe.map
                                            (\a -> a.bidirArrow)
                                            lastOptions.arrow
                                            |> Maybe.withDefault False
                                    , doubleArrow = checked
                                    }
                        }
                    )
                    "Double"
                ]
            , Form.colLabel [ GCol.sm1 ] [ text "Citation" ]
            , Form.col [ GCol.sm4 ]
                [ select
                    [ Select.disabled preset ]
                    (\o -> o.cite)
                    (\v -> { lastOptions | cite = v })
                    [ ( Nothing, "As is" )
                    , ( Just AngleQuotes, "Angle quotes" )
                    , ( Just AngleQuotesWithCite, "Angle quotes with <cite>" )
                    , ( Just CornerBrackets, "Corner brackets" )
                    , ( Just CornerBracketsWithCite
                      , "Corner brackets with <cite>"
                      )
                    ]
                ]
            ]
        , Form.row []
            [ Form.colLabel [ GCol.sm1 ] [ text "Hanja" ]
            , Form.col [ GCol.sm4, GCol.attrs [] ]
                [ select
                    [ Select.disabled preset ]
                    (\o -> Maybe.map (\h -> h.rendering) o.hanja)
                    (\v ->
                        { lastOptions
                            | hanja =
                                Maybe.map
                                    (\r ->
                                        { rendering = r
                                        , reading =
                                            Maybe.map
                                                (\h -> h.reading)
                                                lastOptions.hanja
                                                |> Maybe.withDefault
                                                    { initialSoundLaw = True
                                                    , useDictionaries = Set.empty
                                                    , dictionary = model.customDictionary
                                                    }
                                        }
                                    )
                                    v
                        }
                    )
                    [ ( Nothing, "As is" )
                    , ( Just HangulOnly, "Hangul only" )
                    , ( Just HanjaInParentheses, "Hanja in parentheses" )
                    , ( Just DisambiguatingHanjaInParentheses
                      , "Disambiguating hanja in parentheses"
                      )
                    , ( Just HanjaInRuby, "Hanja in <ruby>" )
                    ]
                ]
            , Form.col [ GCol.sm2 ]
                [ Button.button
                    [ Button.small
                    , Button.outlineSecondary
                    , Button.attrs [ Spacing.p2 ]
                    , Button.onClick ShowCustomDictionary
                    , Button.disabled (not hanjaTransformation)
                    ]
                    [ text <|
                        "Custom dictionary ("
                            ++ String.fromInt (Dict.size model.customDictionary)
                            ++ ")"
                    ]
                , Modal.config CloseCustomDictionary
                    |> Modal.large
                    |> Modal.hideOnBackdropClick True
                    |> Modal.h5 []
                        [ text <|
                            "Custom hanja readings ("
                                ++ String.fromInt (Dict.size model.customDictionary)
                                ++ ")"
                        ]
                    |> Modal.body []
                        [ Textarea.textarea
                            [ Textarea.rows 24
                            , Textarea.attrs
                                [ placeholder <|
                                    "Sino-Korean word → Hangul reading\n"
                                        ++ "漢字語 → 한자어"
                                ]
                            , Textarea.value model.customDictionarySource
                            , Textarea.onInput UpdateCustomDictionarySourcre
                            ]
                        , Alert.simpleWarning [ Spacing.mt3 ]
                            [ text <|
                                "This data will be gone if you "
                                    ++ "refresh this page."
                            ]
                        ]
                    |> Modal.footer []
                        [ Button.button
                            [ Button.outlinePrimary
                            , Button.onClick CloseCustomDictionary
                            ]
                            [ text "Close" ]
                        ]
                    |> Modal.view model.customDictionaryVisibility
                ]
            , Form.col [ GCol.sm5, GCol.attrs [ Spacing.mt2 ] ]
                [ checkboxOn
                    hanjaTransformation
                    (\o ->
                        case o.hanja of
                            Just h ->
                                h.reading.initialSoundLaw

                            Nothing ->
                                False
                    )
                    (\i ->
                        { lastOptions
                            | hanja =
                                Maybe.map
                                    (\h ->
                                        { rendering = h.rendering
                                        , reading =
                                            let
                                                r =
                                                    h.reading
                                            in
                                            { r | initialSoundLaw = i }
                                        }
                                    )
                                    lastOptions.hanja
                        }
                    )
                    "Initial Sound Law"
                , checkboxOn
                    hanjaTransformation
                    (\o ->
                        case o.hanja of
                            Just h ->
                                Set.member "kr-stdict" h.reading.useDictionaries

                            Nothing ->
                                False
                    )
                    (\usesKrDict ->
                        { lastOptions
                            | hanja =
                                Maybe.map
                                    (\{ reading, rendering } ->
                                        { rendering = rendering
                                        , reading =
                                            { reading
                                                | useDictionaries =
                                                    (if usesKrDict then
                                                        Set.insert

                                                     else
                                                        Set.remove
                                                    )
                                                        "kr-stdict"
                                                        reading.useDictionaries
                                            }
                                        }
                                    )
                                    lastOptions.hanja
                        }
                    )
                    "South Korean Standard Dictionary"
                ]
            ]
        ]
