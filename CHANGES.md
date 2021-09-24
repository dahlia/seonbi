Seonbi changelog
================

Version 0.2.2
-------------

To be released.

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
