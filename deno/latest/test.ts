import {
  Configuration,
  DEFAULT_CONFIGURATION,
  Options,
  Seonbi,
  transform,
} from "./mod.ts";
import { assertEquals } from "https://deno.land/std@0.106.0/testing/asserts.ts";

const hanjaInParens: Options = {
  contentType: "text/html",
  quote: "CurvedQuotes",
  cite: null,
  arrow: null,
  ellipsis: false,
  emDash: false,
  stop: null,
  hanja: {
    rendering: "HanjaInParentheses",
    reading: {
      initialSoundLaw: true,
      useDictionaries: ["kr-stdict"],
      dictionary: {},
    },
  },
};

const customDict: Options = {
  ...hanjaInParens,
  hanja: {
    rendering: "HanjaInParentheses",
    reading: {
      initialSoundLaw: true,
      useDictionaries: [],
      dictionary: { "言語": "말", "文字": "글" },
    },
  },
};

let config: Configuration = {
  ...DEFAULT_CONFIGURATION,
  process: { distType: "nightly" },
};

try {
  const binPath = Deno.env.get("SEONBI_API");
  if (binPath != null && "process" in config) config.process = { binPath };
} catch (e) {
  if (!(e instanceof Deno.errors.PermissionDenied)) throw e;
}

try {
  const port = Deno.env.get("SEONBI_API_PORT");
  if (port != null && port.match(/^[0-9]+$/) && "process" in config) {
    config.port = parseInt(port);
  }
} catch (e) {
  if (!(e instanceof Deno.errors.PermissionDenied)) throw e;
}

try {
  const apiUrl = Deno.env.get("SEONBI_API_URL");
  if (apiUrl != null) config = { apiUrl };
} catch (e) {
  if (!(e instanceof Deno.errors.PermissionDenied)) throw e;
}

Deno.test("transform()", async () => {
  const koKr = await transform("<p>言語와 文字</p>", config);
  assertEquals(koKr, "<p>언어와 문자</p>");
});

Deno.test("Seonbi#start()", async () => {
  const seonbi = new Seonbi(config);
  await seonbi.start();
  try {
    for (let i = 0; i < 5; i++) {
      try {
        const response = await fetch(seonbi.apiUrl);
        assertEquals(
          { message: "Unsupported method: GET", success: false },
          await response.json(),
        );
        break;
      } catch (e) {
        if (
          !(e instanceof TypeError) ||
          e.message.indexOf("os error 61") < 0 &&
            e.message.indexOf("os error 111") < 0
        ) {
          throw e;
        }

        return new Promise((r) => setTimeout(r, 1000));
      }
    }
  } finally {
    await seonbi.stop();
  }
});

function withSeonbi(fn: (s: Seonbi) => Promise<void>): () => Promise<void> {
  return async () => {
    const seonbi = new Seonbi(config);
    await seonbi.start();
    try {
      await fn(seonbi);
    } finally {
      await seonbi.stop();
    }
  };
}

function testWithSeonbi(label: string, fn: (s: Seonbi) => Promise<void>): void {
  Deno.test(label, withSeonbi(fn));
}

testWithSeonbi("Seonbi#transform()", async (seonbi: Seonbi) => {
  assertEquals(
    await seonbi.transform("<p>言語와 文字</p>"),
    "<p>언어와 문자</p>",
  );
  assertEquals(
    await seonbi.transform("<p>言語와 文字</p>", hanjaInParens),
    "<p>언어(言語)와 문자(文字)</p>",
  );
  assertEquals(
    await seonbi.transform("<p>言語와 文字</p>", customDict),
    "<p>말(言語)와 글(文字)</p>",
  );
});
