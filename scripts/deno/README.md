[Seonbi] client library for Deno
================================

[![Latest version][Tag badge]][Deno module]

*[Seonbi] is an HTML preprocessor that makes typographic/orthographic
adjustments on Korean text.  See the [website][Seonbi] for details.*

This directory contains a simple client library which manages and communicates
with Seonbi HTTP API server.  The `transform()` function and `Seonbi` class
automatically downloads the Seonbi executable binary and runs the server under
the hood.

Here's an example code for one-shot transformation:

~~~~ typescript
import { transform } from "https://deno.land/x/seonbi/mod.ts";

const input = "디노를 通해 쓰는 선비";
const output = transform(input);
console.log(output);  // 디노를 통해 쓰는 선비
~~~~

When there are multiple inputs to transform, makes a `Seonbi` instance and
call its `transform()` method multiple times so that the server subprocess
are not spawned needlessly more than once:


~~~~ typescript
import { Seonbi } from "https://deno.land/x/seonbi/mod.ts";

const inputs = [
  "序詩",
  "看板 없는 거리",
  "太初의 아침",
  "무서운 時間",
  "눈 오는 地圖",
  "별 헤는 밤",
  "슬픈 族屬",
];
const seonbi = new Seonbi();
const outputs = await Promise.all(inputs.map(input => seonbi.transform(input)));
console.log(outputs);
/*
[
  "서시",
  "간판 없는 거리",
  "태초의 아침",
  "무서운 시간",
  "눈 오는 지도",
  "별 헤는 밤",
  "슬픈 족속",
]
*/
~~~~

[Seonbi]: https://github.com/dahlia/seonbi
[Tag badge]: https://img.shields.io/github/v/tag/dahlia/seonbi
[Deno module]: https://deno.land/x/seonbi
