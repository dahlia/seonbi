Extract hanja words from *Standard Korean Language Dictionary* (標準國語大辭典)
===============================================================================

This Python script extracts Sino-Korean words from *Standard Korean Language
Dictionary* (標準國語大辭典) published by NIKL (國立國語院) of South Korea.

First of all, this script requires Python 3.6 or higher.  Though it might work
on older versions, I've never tested.  I'm sure it won't work on Python 2.
It also works well with PyPy3.5 v6.0 or higher, and is even about 2 times
faster than CPython --- so I recommend PyPy more than CPython.

Note that this script does not depend on any other than the Python standard
library.

NIKL has distributed *Standard Korean Language Dictionary* under CC BY-SA
`since 11th March, 2019`__.  The data can be downloaded from the
`Standard Korean Language Dictionary`__ website --- although this website
does not have English version and you need to make an account to download
the data.  Or, in short, you could download using ``curl`` in one-shot::

    # Works as of February 2023.
    curl \
        -LJ \
        -X POST \
        -F link_key=1093190 \
        -F pageUnit=10 \
        -F pageIndex=1 \
        -o stdict.zip \
        https://stdict.korean.go.kr/common/download.do

The data is contained by a *.zip* archive, and if you extract it there are
several XML data files.  This script reads the *.zip* archive (not *.xml* files)
and then prints the result in the TSV format that Seonbi can interpret::

    ./main.py stdict.zip | sort > kr-stdict.tsv

__ https://stdict.korean.go.kr/notice/noticeView.do?board_no=1129
__ https://stdict.korean.go.kr/
