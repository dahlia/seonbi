Seonbi: SmartyPants for Korean language
=======================================

Seonbi does typographic adjustments to an HTML so that the result uses accurate
punctuations according to the modern Korean orthography.  (It's similar to
what [SmartyPants] does for a text written in English.)

Seonbi provides the Haskell library and the CLI; both can perform the following
transformations:

 -  Three consecutive periods (`...`) into an ellipsis entity (`…`)
 -  Pairs of less-than and greater-than inequality symbols (`<` & `>`) into
    pairs of proper angle quotes (`〈` & `〉`)
 -  Pairs of two consecutive inequality symbols (`<<` & `>>`) into
    pairs of proper double angle quotes (`《` & `》`)
 -  A less-than inequality symbol followed by a hyphen or an equality
    symbol (`<-`, `<=`) into arrows to left (`←`, `⇐`)
 -  A hyphen or an equality symbol followed by a greater-than inequality
    symbol (`->`, `=>`) into arrows to right (`→`, `⇒`)
 -  A hyphen or an equality symbol wrapped by inequality symbols (`<->`, `<=>`)
    into bi-directional arrows (`↔`, `⇔`)

Since its transformations work in HTML-level, it also plays well with web
markup languages like CommonMark, Markdown, and Textile.

[SmartyPants]: https://daringfireball.net/projects/smartypants/


License
-------

Distributed under LGPL 2.1 or later.
