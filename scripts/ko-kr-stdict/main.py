#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
# Extract Sino-Korean words from Standard Korean Language Dictionary
# Copyright (C) 2019--2021  Hong Minhee
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
import sqlite3
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
    f'\[((?:{HANJA_PATTERN})+)\]$'
, re.VERBOSE | re.UNICODE)
HANJA_ONLY_RE = re.compile(f'^(?:(?:{HANJA_PATTERN})▽?)+$', re.VERBOSE)
DISAMBIGUATOR = re.compile(r'\d{2}$|(?:^|(?<=[가-힣]))-(?:(?=[가-힣])|$)')


EQU_RE = re.compile('<equ>&#x([A-Fa-f0-9]+);</equ>')

EQU_TABLE = {
    0xe000: '⿰魚空',  # https://www.cns11643.gov.tw/wordView.jsp?ID=948526
    0xe004: '氛',
    0xe005: '⿱艹詢',  # https://hc.jsecs.org/irg/ws2021/app/?id=03429
    0xe008: '옴',  # https://en.wikipedia.org/wiki/Om
    0x1e45: 'n',  # n in linga/lingam.  https://en.wikipedia.org/wiki/Lingam
}


def expand_equ(match):
    codepoint = int(match.group(1), 16)
    character = chr(codepoint)
    if HANJA_RE.match(character):
        return character
    return EQU_TABLE.get(codepoint, match.group(0))


def filter_xml(xml_path, hanja_only=True):
    doc = xml.dom.pulldom.parse(xml_path)
    has_hanja = HANJA_RE.search
    match_square_brackets_hanja = SQUARE_BRACKETS_HANJA_RE.search
    if hanja_only:
        includes = HANJA_ONLY_RE.match
    else:
        includes = has_hanja
    equ_re = EQU_RE
    START_ELEMENT = xml.dom.pulldom.START_ELEMENT
    END_ELEMENT = xml.dom.pulldom.END_ELEMENT
    CHARACTERS = xml.dom.pulldom.CHARACTERS
    word = None
    hangul = None
    hangul_filled = False
    origin = None
    origin_lang = None
    origin_type = None
    sense = None
    skip = False
    for ev, node in doc:
        tag = node.tagName if ev == START_ELEMENT or ev == END_ELEMENT else ''
        if ev == START_ELEMENT and tag == 'item':
            word = None
            hangul = None
            hangul_filled = False
            origin = None
            origin_lang = None
            origin_type = None
            sense = None
            skip = False
            continue
        elif ev == START_ELEMENT and tag in ('relation_info', 'conju_info'):
            skip = True
            continue
        elif ev == END_ELEMENT and tag in ('relation_info', 'conju_info'):
            skip = False
            continue
        elif skip:
            continue
        if word is None:
            if ev == START_ELEMENT and tag == 'word_info':
                word = {'meaning': [], 'origin': []}
                hangul = None
                hangul_filled = False
                origin = None
                origin_lang = None
                origin_type = None
                sense = None
        else:
            if hangul is None and not hangul_filled:
                if ev == START_ELEMENT and tag == 'word':
                    hangul = ''
            elif not hangul_filled:
                if ev == CHARACTERS:
                    hangul += node.data
                elif ev == END_ELEMENT and tag == 'word':
                    word['reading'] = DISAMBIGUATOR.sub('', hangul.strip()) \
                        .strip('-') \
                        .replace('^', ' ')
                    hangul = None
                    hangul_filled = True
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
                        if word['origin']:
                            word['origin'].append('')
                    else:
                        if has_hanja(origin[0]):
                            square_brackets_hanja = \
                                match_square_brackets_hanja(origin[0])
                            if square_brackets_hanja:
                                origin_sub = square_brackets_hanja.group(1)
                            else:
                                origin_sub = origin[0]
                        else:
                            origin_sub = origin[0]
                        if word['origin']:
                            word['origin'][-1] += origin_sub
                        else:
                            word['origin'].append(origin_sub)
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
                word['origin'] = list(filter(includes, word['origin']))
                if not word['origin']:
                    continue
                if any(m.startswith('→ ') or
                       m.endswith(' 우리 한자음으로 읽은 이름.')
                       for m in word['meaning']):
                    word = None
                    continue
                reading = word['reading']
                meaning = ' '.join(
                    f'{i + 1}. {m}' for (i, m) in enumerate(word['meaning'])
                )
                for hanja in word['origin']:
                    hanja = hanja.strip().replace('▽', '')
                    if not hanja:
                        continue
                    yield hanja, reading, meaning
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
        with sqlite3.connect(os.path.join(td, '.__tmpdic__.db'),
                             isolation_level=None) as db:
            cursor = db.cursor()
            cursor.execute('''
                CREATE TABLE dic (
                    hanja text PRIMARY KEY,
                    reading text,
                    meaning hanja
                )
            ''')
            for filename in os.listdir(td):
                if filename.startswith('.'):
                    continue
                try:
                    words = filter_xml(os.path.join(td, filename))
                    for hanja, reading, meaning in words:
                        cursor.execute(
                            'SELECT meaning FROM dic WHERE hanja = ?',
                            (hanja,)
                        )
                        existing = cursor.fetchone()
                        if existing:
                            if len(existing[0]) < len(meaning):
                                cursor.execute('''
                                    UPDATE dic
                                    SET reading = ?, meaning = ?
                                    WHERE hanja = ?
                                ''', (reading, meaning, hanja))
                            continue
                        cursor.execute('''
                            INSERT INTO dic (hanja, reading, meaning)
                            VALUES (?, ?, ?)
                        ''', (hanja, reading, meaning))
                except (KeyboardInterrupt, BrokenPipeError):
                    raise SystemExit(130)
            if args.meaning_column:
                cursor.execute('SELECT hanja, reading, meaning FROM dic')
            else:
                cursor.execute('SELECT hanja, reading FROM dic')
            writer = csv.writer(bstdout, 'excel-tab')
            write = writer.writerow
            for row in cursor:
                write(tuple(row))


if __name__ == '__main__':
    main()
