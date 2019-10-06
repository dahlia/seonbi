module Markdown.HtmlString exposing (render)

import List
import Markdown.Block exposing (..)
import Markdown.Inline exposing (..)
import Maybe exposing (andThen, withDefault)
import String


escape : String -> String
escape =
    String.replace "&" "&amp;"
        >> String.replace "<" "&lt;"
        >> String.replace ">" "&gt;"
        >> String.replace "\"" "&quot;"


render : List (Block b i) -> String
render blocks =
    List.map renderBlock blocks |> String.concat


renderBlock : Block b i -> String
renderBlock block =
    case block of
        BlankLine text ->
            escape text

        ThematicBreak ->
            "\n<hr>\n"

        Heading _ level inlines ->
            "\n<h"
                ++ String.fromInt level
                ++ ">"
                ++ renderInlines inlines
                ++ "</h"
                ++ String.fromInt level
                ++ ">\n"

        CodeBlock _ code ->
            "<pre>" ++ escape code ++ "</pre>\n"

        Paragraph _ text ->
            "<p>" ++ renderInlines text ++ "</p>\n"

        BlockQuote blocks ->
            "<blockquote>\n" ++ render blocks ++ "</blockquote>\n"

        List list items ->
            let
                ( open, close ) =
                    case list.type_ of
                        Unordered ->
                            ( "<ul>", "</ul>" )

                        Ordered start ->
                            ( "<ol start=\"" ++ String.fromInt start ++ "\">"
                            , "</ol>"
                            )

                renderItem =
                    \blocks ->
                        "<li>" ++ render blocks ++ "</li>\n"
            in
            open
                ++ "\n"
                ++ String.concat (List.map renderItem items)
                ++ close
                ++ "\n"

        PlainInlines inlines ->
            renderInlines inlines

        Markdown.Block.Custom _ blocks ->
            render blocks


renderInlines : List (Inline i) -> String
renderInlines inlines =
    List.map renderInline inlines
        |> String.concat


renderInline : Inline i -> String
renderInline inline =
    case inline of
        Text text ->
            escape text

        HardLineBreak ->
            "<br>\n"

        CodeInline text ->
            "<code>" ++ escape text ++ "</code>"

        Link href title label ->
            "<a href=\""
                ++ escape href
                ++ "\""
                ++ (title
                        |> andThen (\t -> Just <| " title=\"" ++ t ++ "\"")
                        |> withDefault ""
                   )
                ++ ">"
                ++ (List.map renderInline label |> String.concat)
                ++ "</a>"

        Image src title alt ->
            "<img src=\""
                ++ escape src
                ++ "\""
                ++ (title
                        |> andThen (\t -> Just <| " title=\"" ++ t ++ "\"")
                        |> withDefault ""
                   )
                ++ " alt=\""
                ++ (List.map simplifyInline alt |> String.concat)
                ++ "\">"

        HtmlInline tag attrs inlines ->
            renderHtmlInline tag attrs inlines

        Emphasis 1 inlines ->
            "<em>" ++ renderInlines inlines ++ "</em>"

        Emphasis _ inlines ->
            "<strong>" ++ renderInlines inlines ++ "</strong>"

        Markdown.Inline.Custom _ inlines ->
            renderInlines inlines


simplifyInlines : List (Inline i) -> String
simplifyInlines inlines =
    List.map simplifyInline inlines |> String.concat


simplifyInline : Inline i -> String
simplifyInline inline =
    case inline of
        Text text ->
            escape text

        HardLineBreak ->
            "\n"

        CodeInline text ->
            escape text

        Link _ _ label ->
            simplifyInlines label

        Image _ _ alt ->
            simplifyInlines alt

        HtmlInline _ _ inlines ->
            simplifyInlines inlines

        Emphasis _ inlines ->
            simplifyInlines inlines

        Markdown.Inline.Custom _ inlines ->
            simplifyInlines inlines


renderHtmlInline :
    String
    -> List ( String, Maybe String )
    -> List (Inline i)
    -> String
renderHtmlInline tag attrs inlines =
    let
        attrsString =
            String.concat <| List.map renderAttr attrs

        renderAttr =
            \( attr, value ) ->
                case value of
                    Just v ->
                        " " ++ attr ++ "=\"" ++ escape v ++ "\""

                    Nothing ->
                        " " ++ attr
    in
    "<"
        ++ tag
        ++ attrsString
        ++ ">"
        ++ renderInlines inlines
        ++ "</"
        ++ tag
        ++ ">"
