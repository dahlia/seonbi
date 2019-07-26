Seonbi: SmartyPants for Korean language
=======================================

Seonbi (선비) is is an HTML preprocessor that makes typographic adjustments
to an HTML so that the result uses accurate punctuations according to
the modern Korean orthography.
(It's similar to what [SmartyPants] does for a text written in English.)

It also transforms a `ko-Kore` text (國漢文混用; [Korean mixed script]) into
a `ko-Hang` text (한글전용; Hangul-only script).

Seonbi provides the Haskell library and the CLI; both can perform the following
transformations:

 -  All hanja words (e.g., `漢字`) into corresponding hangul-only words
    (e.g., `한자`)
 -  Straight quotes and apostrophes (`"` & `'`) into curly quotes HTML
    entities (`“`, `”`, `‘`, & `’`)
 -  Three consecutive periods (`...`) into an ellipsis entity (`…`)
 -  Pairs of less-than and greater-than inequality symbols (`<` & `>`) into
    pairs of proper angle quotes (`〈` & `〉`)
 -  Pairs of two consecutive inequality symbols (`<<` & `>>`) into
    pairs of proper double angle quotes (`《` & `》`)
 -  A hyphen (`-`) or hangul vowel *eu* (`ㅡ`) surrounded by spaces, or
    two/three consecutive hyphens (`--` or `---`) into a proper em dash (`—`)
 -  A less-than inequality symbol followed by a hyphen or an equality
    symbol (`<-`, `<=`) into arrows to the left (`←`, `⇐`)
 -  A hyphen or an equality symbol followed by a greater-than inequality
    symbol (`->`, `=>`) into arrows to the right (`→`, `⇒`)
 -  A hyphen or an equality symbol wrapped by inequality symbols (`<->`, `<=>`)
    into bi-directional arrows (`↔`, `⇔`)

Since its transformations work in HTML-level, it also plays well with web
markup languages like CommonMark, Markdown, and Textile.  In a similar way to
SmartyPants, it either does not modify characters within several sensitive
HTML elements like `<pre>`/`<code>`/`<script>`/`<kbd>`.

[SmartyPants]: https://daringfireball.net/projects/smartypants/
[Korean mixed script]: https://en.wikipedia.org/wiki/Korean_mixed_script


CLI
---

The `seonbi` command basically takes the input HTML as standard input, and
then transformes it into the output HTML as standard output:

    seonbi < input.html > output.html

You could pass a filename as an argument instead (and it is `-` by default):

    seonbi input.html > output.html

There is `-o`/`--output` option as well:

    seonbi -o output.html input.html

Although it automatically detects text encoding of the input file,
you could explicitly specify `-e`/`--encoding`:

    seobni -e euc-kr -o output.html input.html

Although there are several style options, e.g., `-q`/`--quote`, `-c`/`--cite`,
`-r`/`--render-hanja`, in most cases, giving `-p`/`--preset` is enough:

    echo '平壤 冷麵' | seonbi -p ko-kr  # 평양 냉면
    echo '平壤 冷麵' | seonbi -p ko-kp  # 평양 랭면

Read `-h`/`--help` for details:

    seonbi --help


License
-------

Distributed under LGPL 2.1 or later.


Etymology
---------

*[Seonbi]* (선비) means a classical scholar during Joseon periods (14c–19c).
Today there's a meme that calls a person who feels morally superior or has
elitism *seonbi* in the Korean internet.  So *seonbi* and *smarty pants* have
some things in common.

[Seonbi]: https://en.wikipedia.org/wiki/Seonbi
