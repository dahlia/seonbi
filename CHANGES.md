Seonbi changelog
================

Version 0.4.0
-------------

To be released.

 -  Introduced GitHub action `dahlia/seonbi/setup` to install Seonbi during
    GitHub Actions workflow.

 -  Since this version, it supports GHC 9.2.* at most.

 -  The `iconv` flag became ignored on Windows, because the flag had never
    worked on Windows.

 -  Previously, `ContentType` type (and `-t`/`--content-type` in CLI and
    `"contentType"` field in HTTP API) was a mere case-insensitive string.
    Now, it is loosely structured so that it can contain parameters.  You can
    treat it as a media type (MIME type).

    For example, `text/markdown` and `text/markdown; variant=Original` both
    use the same parser and formatter under the hood.

     -  `Text.Seonbi.ContentTypes.ContentType` type became an alias of
        `Network.HTTP.Media.MediaType` type (was `CI Text`).
     -  The type of `Text.Seonbi.ContentTypes.contentTypeFromText` function
        became `Text -> Maybe ContentType` (was `Text -> ContentType`).
     -  `Text.Seonbi.ContentTypes.transformWithContentType` function became
        able to handle media types with parameters, and now it falls back
        more-specific media types to less-specific media types if there is no
        exactly matched one.

 -  For `text/html` and `application/xhtml+xml` formats, some obsolete HTML
    tags are no more ignored, but now recognized as valid HTML tags.  Here are
    the list of added tags:

     -  `<center>`
     -  `<font>`
     -  `<nobr>`
     -  `<strike>`
     -  `<xmp>`

    Haskell API-wise, the following constructors were added to
    the `Text.Seonbi.Html.Tag.HtmlTag` type:

     -  `Center`
     -  `Font`
     -  `NoBR`
     -  `Strike`
     -  `XMP`

 -  Deno module became possible to use an already deployed Seonbi API server
    instead of spawning a local API server.  The `new Seonbi()` constructor
    now takes `{ apiUrl: string }` besides existing configuration forms.


Version 0.3.6
-------------

Released on February 16, 2023.

 -  The `iconv` flag became ignored on Windows, because the flag had never
    worked on Windows.

 -  Fixed a bug that `seonbi.exe --help` had suddenly terminated with
    the below error message on Windows:  [[#30]]

    ~~~~
    <stdout>: commitBuffer: invalid argument (invalid character)
    ~~~~

[#30]: https://github.com/dahlia/seonbi/issues/30


Version 0.3.5
-------------

Released on February 13, 2023.

 -  Official builds for Windows now contain required dynamic libraries (_*.dll_)
    besides executables (*seonbi.exe* & *seonbi-api.exe*).  [[#2]]
 -  Fixed a bug where Sino-Korean words that were not listed in the dictionary
    sometimes had been inaccurately phoneticized so that Initial Sound Law
    (頭音法則) had been applied to non-initial hanja of a word too. [[#20]]
 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2023-02.

[#2]: https://github.com/dahlia/seonbi/issues/2
[#20]: https://github.com/dahlia/seonbi/issues/20


Version 0.3.4
-------------

Released on January 18, 2023.

 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2022-12.


Version 0.3.3
-------------

Released on September 9, 2022.

 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2022-09.
 -  Fixed a bug that Sino-Korean words are not phoneticized to Korean custom
    readings (習慣音) according to folk readings (俗音) and erroneous
    readings (訛音).  Now such words are phoneticized to the most usual readings
    according to *Standard Korean Language Dictionary*.  For example,
    *困難* is no more phoneticized to *곤난* but *곤란* instead. [[#22]]

[#22]: https://github.com/dahlia/seonbi/issues/22


Version 0.3.2
-------------

Released on August 11, 2022.

 -  HTTP API now allows any headers from cross-origin.  (Previously, it allowed
    only `Content-Type`.)
 -  HTTP API now respond with header `Vary: Origin` for more accurate cache
    control.


Version 0.3.1
-------------

Released on November 27, 2021.

 -  Added prebuilt executable binaries for Apple Silicon (macos-arm64).


Version 0.3.0
-------------

Released on November 18, 2021.

 -  Since this version, it requires GHC 8.8.* at least, and supports GHC 9.0.*
    at most.

 -  Now supports several content types besides HTML/XHTML.  The below list
    shows all supported content types:  [[#18]]

     -  `text/html` (previously non-XHTML mode)
     -  `application/xhtml+xml` (previously XHTML mode)
     -  `text/plain` (added)
     -  `text/markdown` (added)

    The below Haskell APIs changed:

     -  Added `Text.Seonbi.ContentTypes` module.
     -  Added `contentType` field for `Configuration m a`.
     -  Removed `xhtml` field for `Configuration m a` in favor of
        new `contentType` field for the same type.

    The below CLI options changed:

     -  Added `-t`/`--content-type` option with the default value `text/html`.
     -  Removed Removed `-x`/`--xhtml` option in favor of new
        `-t`/`--content-type` option.  In order to use XHTML mode, give it
        `-t application/xhtml+xml` option.

    The below HTTP APIs changed:

     -  Added a mandatory field `"content"` to requests.
     -  Deprecated the `"sourceHtml"` field of requests in favor of the new
        `"content"` field.
     -  Added an optional field `"contentType"` with the default value
        `"text/html"` to requests.
     -  Deprecated the `"xhtml"` field of requests in favor of the new
        `"contentType"` field.  The legacy field will be gone in the next
        minor release.
        In order to use XHTML mode, configure `"contentType"` field with
        `"application/xhtml+xml"`.
     -  Added `"content"` field to responses.
     -  Deprecated the `"resultHtml"` field of responses in favor of the new
        `"content"` field.  The legacy field is not provided for non-HTML
        types, and will be gone in the next minor release.
     -  Added `"contentType"` field to responses.
     -  Added `"warnings"` field to responses.

 -  Added `Text.Seonbi.Html.Lang` module.

 -  Some transformations inappropriate for non-Korean contents are no more
    applied to elements written in other languages than Korean.  The below
    functions respect elements `lang` attributes:  [[#10]]

     -  `Text.Seonbi.Hanja.phoneticizeHanja`
     -  `Text.Seonbi.Punctuation.normalizeStops`

 -  Removed several functions from `Text.Seonbi.Trie` module:

     -  `toListBy`
     -  `lookupBy`
     -  `submap`
     -  `match`
     -  `matches`
     -  `alterBy`
     -  `adjust`
     -  `delete`
     -  `mapBy`
     -  `filterMap`

 -  `Text.Seonbi.Trie.Trie` type is not an instance of the following typeclasses
    anymore:

     -  `Generic a => Generic (Trie a)`
     -  `Binary a => Binary (Trie a)`
     -  `Generic1 Trie`
     -  `type Rep (Trie a)`
     -  `type Rep1 Trie`

 -  Added `Text.Seonbi.Html.Printer.printText` function.
 -  Added `Text.Seonbi.Html.Tag.headingLevel` function.
 -  Added `Text.Seonbi.Html.Tag.headingTag` function.
 -  Added `Text.Seonbi.Html.Tag.headingTag'` function.
 -  Added `Text.Seonbi.Html.TagStack.last` function.

[#10]: https://github.com/dahlia/seonbi/issues/10
[#18]: https://github.com/dahlia/seonbi/issues/18


Version 0.2.3
-------------

Released on September 26, 2021.

 -  Fixed stops normalizer's bug where trailing spaces following stops had been
    trimmed after normalized.
 -  Fixed stops normalizer's buf where unnecessary trailing spaces following
    stops had been inserted after normalized.  In particular, unnecessary
    spaces between stops and closing parentheses/brackets are no more inserted.


Version 0.2.2
-------------

Released on September 25, 2021.

 -  Fixed stops normalizer's bug where unnecessary trailing spaces following
    stops had been left even after normalized.
 -  Fixed stops normalizer's bug where commas followed by tag boundaries had
    been not normalized.


Version 0.2.1
-------------

Released on September 23, 2021.

 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2021-09.


Version 0.2.0
-------------

Released on May 26, 2021.

 -  Added stops (periods/commas/interpuncts) normalizer.

    Haskell API-wise, the below types and functions were added:

     -  `Text.Seonbi.Punctuation.Stops` data type
     -  `Text.Seonbi.Punctuation.normalizeStops` function
     -  `Text.Seonbi.Punctuation.horizontalStops` function
     -  `Text.Seonbi.Punctuation.horizontalStopsWithSlashes` function
     -  `Text.Seonbi.Punctuation.verticalStops` function
     -  `Text.Seonbi.Facade.StopOption` data type
     -  `stop` field in `Text.Seonbi.Facade.Configuration` data constructor

    CLI-wise, the `-s`/`--stop` option was added.

    HTTP API-wise, the optional field `"stop"` was added.

 -  `Text.Seonbi.Punctuation.transformEllipsis` became aware of Chinese stops
    (`。。。`) besides Western stops (`...`).

 -  Added options to use horizontal/vertical corner brackets for quotes.

    Haskell API-wise, the below functions were added:

     -  `Text.Seonbi.Punctuation.verticalCornerBrackets` function
     -  `Text.Seonbi.Punctuation.horizontalCornerBrackets` function
     -  `Text.Seonbi.Punctuation.verticalCornerBracketsWithQ` function
     -  `Text.Seonbi.Punctuation.horizontalCornerBracketsWithQ` function
     -  `VerticalCornerBrackets` data constructor for
        `Text.Seonbi.Facade.QuoteOption` type
     -  `HorizontalCornerBrackets` data constructor for
        `Text.Seonbi.Facade.QuoteOption` type
     -  `VerticalCornerBracketsWithQ` data constructor for
        `Text.Seonbi.Facade.QuoteOption` type
     -  `HorizontalCornerBracketsWithQ` data constructor for
        `Text.Seonbi.Facade.QuoteOption` type

    CLI-wise, the `-q`/`--quote` option became to have the below new styles:

     -  `vertical-corner-brackets`
     -  `horizontal-corner-brakcets`
     -  `vertical-corner-brackets-with-q`
     -  `horizontal-corner-brakcets-with-q`

    HTTP API-wise, the optional field `"quote"` became to have the below new
    styles:

     -  `VerticalCornerBrackets`
     -  `HorizontalCornerBrackets`
     -  `VerticalCornerBracketsWithQ`
     -  `HorizontalCornerBracketsWithQ`

 -  The CLI option `-x`/`--xhtml` became usable with the `-p`/`--preset` option
    at a time.

 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2021-05.


Version 0.1.1
-------------

Released on October 7, 2019.

 -  Added the `embed-dictionary` flag to the Cabal package.
 -  Fixed a bug that *Standard Korean Language Dictionary* had not been
    included in executables if `flag(static)` is turned on.  [[#1]]

[#1]: https://github.com/dahlia/seonbi/issues/1


Version 0.1.0
-------------

Released on October 6, 2019.
