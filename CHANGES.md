Seonbi changelog
================

Version 0.2.0
-------------

To be released.

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

 -  The CLI option `-x`/`--xhtml` became usable with the `-p`/`--preset` option
    at a time.

 -  Updated the *Standard Korean Language Dictionary* data
    (*data/ko-kr-stdict.tsv*) to the revision 2020-08-01.


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
