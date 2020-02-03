#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
# Extract Sino-Korean words from Standard Korean Language Dictionary
# Copyright (C) 2019  Hong Minhee
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
import argparse
import csv
import io
import itertools
import os
import os.path
import re
import sys
import tempfile
import xml.dom.pulldom
import zipfile

HANJA_PATTERN = '''
    # Ideographic Description Character
    [\u2f00-\u2fff] |
    # U+3007 IDEOGRAPHIC NUMBER ZERO (〇)
    \u3007 |
    # CJK Unified Ideographs Extension A
    [\u3400-\u4dbf] |
    # CJK Unified Ideographs
    [\u4e00-\u9fcc] |
    # CJK Compatibility Ideographs
    [\uf900-\ufaff] |
    # CJK Unified Ideographs Extension B
    [\U00020000-\U0002a6d6] |
    # CJK Unified Ideographs Extension C
    [\U0002a700-\U0002b734] |
    # CJK Unified Ideographs Extension D
    [\U0002b740-\U0002b81d] |
    # CJK Unified Ideographs Extension E
    [\U0002b820-\U0002cea1] |
    # CJK Unified Ideographs Extension F
    [\U0002ceb0-\U0002ebe0] |
    # CJK Compatibility Ideographs Supplement
    [\U0002f800-\U0002fa1f]
'''
HANJA_RE = re.compile(HANJA_PATTERN, re.VERBOSE)
SQUARE_BRACKETS_HANJA_RE = re.compile(
    f'^\[((?:{HANJA_PATTERN})+)\]$'
, re.VERBOSE)
HANJA_ONLY_RE = re.compile(f'^(?:{HANJA_PATTERN})+$', re.VERBOSE)
DISAMBIGUATOR = re.compile(r'\d{2}$|(?:^|(?<=[가-힣]))-(?:(?=[가-힣])|$)')


EQU_RE = re.compile('<equ>&#x([A-Fa-f0-9]+);</equ>')

EQU_TABLE = {
    0xe000: '⿰魚空',  # ⿰魚空
    0xe004: '氛',
    0xe005: '⿱艹詢',  # ⿱艹詢
    0xe008: '옴',  # https://en.wikipedia.org/wiki/Om
    0x1e45: 'n',  # n in linga/lingam.  https://en.wikipedia.org/wiki/Lingam
}


def expand_equ(match):
    codepoint = int(match.group(1), 16)
    character = chr(codepoint)
    if HANJA_RE.match(character):
        return character
    return EQU_TABLE.get(codepoint, match.group(0))


def filter_xml(xml_path, output, hanja_only=True, meaning_column=False):
    doc = xml.dom.pulldom.parse(xml_path)
    has_hanja = HANJA_RE.search
    match_square_brackets_hanja = SQUARE_BRACKETS_HANJA_RE.match
    if hanja_only:
        includes = HANJA_ONLY_RE.match
    else:
        includes = lambda _: True
    equ_re = EQU_RE
    writer = csv.writer(output, 'excel-tab')
    write = writer.writerow
    START_ELEMENT = xml.dom.pulldom.START_ELEMENT
    END_ELEMENT = xml.dom.pulldom.END_ELEMENT
    CHARACTERS = xml.dom.pulldom.CHARACTERS
    word = None
    hangul = None
    origin = None
    origin_lang = None
    origin_type = None
    sense = None
    skip = False
    for ev, node in doc:
        tag = node.tagName if ev == START_ELEMENT or ev == END_ELEMENT else ''
        if ev == START_ELEMENT and tag == 'relation_info':
            skip = True
            continue
        elif ev == END_ELEMENT and tag == 'relation_info':
            skip = False
            continue
        elif skip:
            continue
        if word is None:
            if ev == START_ELEMENT and tag == 'word_info':
                word = {'meaning': [], 'hanja': []}
                hangul = None
                origin = None
                sense = None
        else:
            if ev == START_ELEMENT and tag == 'conju_info':
                word = None
                continue
            if hangul is None:
                if ev == START_ELEMENT and tag == 'word':
                    hangul = ''
            else:
                if ev == CHARACTERS:
                    hangul += node.data
                elif ev == END_ELEMENT and tag == 'word':
                    word['reading'] = \
                        DISAMBIGUATOR.sub('', hangul.strip()).replace('^', ' ')
                    hangul = None
            if origin is None:
                if ev == START_ELEMENT and tag == 'original_language_info':
                    origin = ('', None)
            else:
                if origin_lang is None:
                    if ev == START_ELEMENT and tag == 'original_language':
                        origin_lang = ''
                else:
                    if ev == CHARACTERS:
                        origin_lang += node.data
                    elif ev == END_ELEMENT and tag == 'original_language':
                        origin = (origin_lang, origin[1])
                        origin_lang = None
                if origin_type is None:
                    if ev == START_ELEMENT and tag == 'language_type':
                        origin_type = ''
                else:
                    if ev == CHARACTERS:
                        origin_type += node.data
                    elif ev == END_ELEMENT and tag == 'language_type':
                        origin = (origin[0], origin_type.strip())
                        origin_type = None
                if ev == END_ELEMENT and tag == 'original_language_info':
                    if origin[1] == '/(병기)':
                        if word['hanja']:
                            word['hanja'].append('')
                    elif has_hanja(origin[0]):
                        square_brackets_hanja = \
                            match_square_brackets_hanja(origin[0])
                        if square_brackets_hanja:
                            hanja_only = square_brackets_hanja.group(1)
                        else:
                            hanja_only = origin[0]
                        if includes(hanja_only):
                            if word['hanja']:
                                word['hanja'][-1] += hanja_only
                            else:
                                word['hanja'].append(hanja_only)
                    origin = None
            if sense is None:
                if ev == START_ELEMENT and tag == 'definition':
                    sense = ''
            else:
                if ev == CHARACTERS:
                    sense += node.data
                elif ev == END_ELEMENT and tag == 'definition':
                    sense = sense.replace('\r', '').replace('\n', ' ')
                    word['meaning'].append(sense)
                    sense = None
            if ev == END_ELEMENT and tag == 'word_info':
                if any(m.endswith(' 우리 한자음으로 읽은 이름.')
                       for m in word['meaning']):
                    word = None
                    continue
                reading = word['reading']
                meaning = ' '.join(
                    f'{i + 1}. {m}' for (i, m) in enumerate(word['meaning'])
                )
                for hanja in word['hanja']:
                    hanja = hanja.strip()
                    if not hanja:
                        continue
                    if meaning_column:
                        write((hanja, reading, meaning))
                    else:
                        write((hanja, reading))
                word = None


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Extract Sino-Korean words from Standard Korean Language Dictionary'
        )
    )
    parser.add_argument(
        'zip_file',
        metavar='FILE',
        type=argparse.FileType('rb'),
        help=(
            'a dictionary .zip file consists of .xml files.  download one '
            'from https://stdict.korean.go.kr/'
        )
    )
    parser.add_argument(
        '-m', '--meaning-column',
        action='store_true',
        default=False,
        help='include the meaning column'
    )
    args = parser.parse_args()
    with args.zip_file as f, \
         zipfile.ZipFile(f) as zf, \
         tempfile.TemporaryDirectory() as td, \
         io.TextIOWrapper(sys.stdout.buffer,
                          encoding='utf-8',
                          newline='',
                          write_through=True) as bstdout:
        zf.extractall(td)
        for filename in os.listdir(td):
            if filename == '.' or filename == '..':
                continue
            try:
                filter_xml(
                    os.path.join(td, filename),
                    bstdout,
                    meaning_column=args.meaning_column
                )
            except (KeyboardInterrupt, BrokenPipeError):
                raise SystemExit(130)


if __name__ == '__main__':
    main()
