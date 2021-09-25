// Seonbi client library for Deno
// Copyright (C) 2021  Hong Minhee
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
import { readAll, writeAll } from "https://deno.land/std@0.108.0/io/mod.ts";
import { join } from "https://deno.land/std@0.108.0/path/mod.ts";
import * as process from "https://deno.land/x/process@v0.3.0/mod.ts";

/**
 * Preset options.
 *
 * - `ko-kr`: South Korean orthography
 * - `ko-kp`: North Korean orthography
 */
export interface Preset {
  preset: "ko-kr" | "ko-kp";
}

/**
 * Built-in dictionaries.  Currently supports only one:
 *
 * - `kr-stdict`: 《標準國語大辭典》 (Standard Korean Language Dictionary)
 */
export type Dictionary = "kr-stdict";

/**
 * Options for transformation.
 * See also <https://github.com/dahlia/seonbi#http-api>.
 */
export interface Options {
  /** Whether to format the result in XHTML. */
  xhtml: boolean;
  /** Quoting options. */
  quote:
    | "CurvedQuotes"
    | "VerticalCornerBrackets"
    | "HorizontalCornerBrackets"
    | "Guillemets"
    | "CurvedSingleQuotesWithQ"
    | "VerticalCornerBracketsWithQ"
    | "HorizontalCornerBracketsWithQ"
    | null;
  /** Citing options. */
  cite:
    | "AngleQuotes"
    | "AngleQuotesWithCite"
    | "CornerBrackets"
    | "CornerBracketsWithCite"
    | null;
  /** How to transform arrows. */
  arrow: null | {
    bidirArrow: boolean;
    doubleArrow: boolean;
  };
  /** How to transform ellipses. */
  ellipsis: boolean;
  /** How to transform em dashes. */
  emDash: boolean;
  /** How to transform full and half stops. */
  stop:
    | "Horizontal"
    | "HorizontalWithSlashes"
    | "Vertical"
    | null;
  /** How to transform Sino-Korean words. */
  hanja: null | {
    rendering:
      | "HangulOnly"
      | "HanjaInParentheses"
      | "DisambiguatingHanjaInParentheses"
      | "HanjaInRuby";
    reading: {
      initialSoundLaw: boolean;
      useDictionaries: Dictionary[];
      dictionary: Record<string, string>;
    };
  };
}

/**
 * Configuration for spawning and communicating with a Seonbi API server.
 */
export interface Configuration {
  /** The port number to communicate in. */
  port: number;
  /**
   * The way to run the API server.
   *
   * - `"download"`: Downloads a Seonbi binary just in time.
   * - `"started"`: Assumes the API server is already running.
   * - `{ binPath: string }`: Runs the executable binary of the API server
   *   placed in the `binPath`.
   * - `{ distType: DistType }`: Downloads a Seonbi binary of the specified
   *   `DistType`, which is `"stable"` or `"nightly"`, just in time.
   * - `{ downloadPath: string, distType?: DistType }`: Downloads a Seonbi
   *    binary of the specified `DistType`, which is `"stable"` (default) or
   *    `"nightly"` just in time, and place it in the `downloadPath`.
   */
  process:
    | "download"
    | "started"
    | { binPath: string }
    | { distType: DistType }
    | { downloadPath: string; distType?: DistType };
}

/** The default configuration. */
export const DEFAULT_CONFIGURATION: Configuration = {
  port: 3800,
  process: "download",
};

const STABLE_DOWNLOAD_URLS: Record<typeof Deno.build.os, string> = {
  "linux":
    "https://github.com/dahlia/seonbi/releases/download/0.2.3/seonbi-0.2.3.linux-x86_64.tar.bz2",
  "darwin":
    "https://github.com/dahlia/seonbi/releases/download/0.2.3/seonbi-0.2.3.macos-x86_64.tar.bz2",
  "windows":
    "https://github.com/dahlia/seonbi/releases/download/0.2.3/seonbi-0.2.3.win64.zip",
} as const;

const NIGHTLY_DOWNLOAD_URLS: Record<typeof Deno.build.os, string> = {
  "linux":
    "https://dahlia.github.io/seonbi/dists/latest/seonbi.linux-x86_64.tar.bz2",
  "darwin":
    "https://dahlia.github.io/seonbi/dists/latest/seonbi.macos-x86_64.tar.bz2",
  "windows": "https://dahlia.github.io/seonbi/dists/latest/seonbi.win64.zip",
} as const;

/** The type of distribution to download. */
type DistType = "stable" | "nightly";

const DOWNLOAD_URLS: Record<DistType, Record<typeof Deno.build.os, string>> = {
  "stable": STABLE_DOWNLOAD_URLS,
  "nightly": NIGHTLY_DOWNLOAD_URLS,
};

/**
 * The object to run and manage a Seonbi API server and communicate with it.
 */
export class Seonbi implements Configuration {
  readonly host: string;
  readonly port: number;
  readonly process:
    | "download"
    | "started"
    | { binPath: string }
    | { distType: DistType }
    | { downloadPath: string; distType?: DistType };
  #proc?: Deno.Process;

  constructor(configuration: Configuration = DEFAULT_CONFIGURATION) {
    this.host = "127.0.0.1";
    this.port = configuration.port;
    this.process = configuration.process;
  }

  /** Whether it the API server is running. */
  get running(): boolean {
    return this.#proc != null;
  }

  /**
   * Transforms the `sourceHtml`.
   */
  async transform(
    sourceHtml: string,
    options: Preset | Options = { preset: "ko-kr" },
  ): Promise<string> {
    await this.start();
    const permDesc: Deno.PermissionDescriptor = {
      name: "net",
      host: `${this.host}:${this.port}`,
    };
    const { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const payload = { sourceHtml, ...options };
    const response = await fetch(`http://${this.host}:${this.port}/`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json" }),
      body: new Blob([JSON.stringify(payload)], { type: "application/json" }),
    });
    const result = await response.json();
    if (result.success && result?.resultHtml != null) return result.resultHtml;
    throw new Error(result?.message ?? "Unexpected error.");
  }

  static readonly #cache: Record<string, string> = {};

  #getCache(key: string): string | undefined {
    if (key in Seonbi.#cache) return Seonbi.#cache[key];
    try {
      return localStorage?.getItem(key) ?? undefined;
    } catch (e) {
      if (!(e instanceof ReferenceError)) throw e;
    }
  }

  #setCache(key: string, value: string): void {
    Seonbi.#cache[key] = value;
    try {
      localStorage?.setItem(key, value);
    } catch (e) {
      if (!(e instanceof ReferenceError)) throw e;
    }
  }

  /** Starts the API server in background. */
  async start(): Promise<void> {
    if (this.running || this.process === "started") return;
    const permDesc: Deno.PermissionDescriptor = { name: "run" };
    const { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    let binPath: string;
    if (
      this.process === "download" ||
      typeof this.process == "object" &&
        ("distType" in this.process || "downloadPath" in this.process)
    ) {
      let exists = false;
      let cachedPath;
      try {
        cachedPath = this.#getCache("seonbi_api_path");
        if (cachedPath != null) {
          try {
            const stat = await Deno.stat(cachedPath);
            if (stat.isFile) exists = true;
          } catch (e) {
            if (!(e instanceof Deno.errors.NotFound)) throw e;
          }
        }
      } catch (e) {
        if (!(e instanceof ReferenceError)) throw e;
      }
      if (cachedPath != null && exists) binPath = cachedPath;
      else {
        const distType: DistType =
          typeof this.process == "object" && "distType" in this.process &&
            this.process.distType != null
            ? this.process.distType
            : "stable";
        binPath = await this.#install(distType);
        if (typeof this.process == "object" && "downloadPath" in this.process) {
          await Deno.copyFile(binPath, this.process.downloadPath);
          binPath = this.process.downloadPath;
        }
        this.#setCache("seonbi_api_path", binPath);
      }
    } else {
      binPath = this.process.binPath;
    }
    const cmd = [binPath, `--host=${this.host}`, `--port=${this.port}`];
    try {
      this.#proc = Deno.run({
        cmd,
        stdout: "null",
        stderr: "piped",
      });
    } catch (e) {
      if (e instanceof Error) e.message = `${cmd.join(" ")}: ${e.message}`;
      throw e;
    }
    await this.#proc.stderr?.read(new Uint8Array([0]));
  }

  /**
   * Stops the running server process if it is.
   */
  async stop(): Promise<void> {
    if (this.#proc == null || this.process === "started") return;
    const pid = this.#proc.pid;
    if (Deno.build.os == "windows") {
      // As of Deno 1.14.0, Process<T>.kill() does not support Windows:
      await process.kill(pid, { force: true, tree: true });
    } else {
      this.#proc.kill("SIGINT");
    }
    this.#proc.stderr?.close();
    await this.#proc.status();
    this.#proc.close();
    while (await process.get(pid) != null) {
      await process.kill(pid, { force: true, tree: true });
    }
  }

  async #downloadDist(distType: DistType): Promise<string> {
    const downloadUrl = DOWNLOAD_URLS[distType][Deno.build.os];
    const url = new URL(downloadUrl);
    const suffix = url.pathname.substr(url.pathname.lastIndexOf("/") + 1);
    const tmpDirEnv = Deno.build.os === "windows" ? "TEMP" : "TMPDIR";
    let permDesc: Deno.PermissionDescriptor = {
      name: "env",
      variable: tmpDirEnv,
    };
    let { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    permDesc = { name: "write", path: Deno.env.get(tmpDirEnv) };
    perm = (await Deno.permissions.query(permDesc)).state;
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const tempPath = await Deno.makeTempFile({ prefix: "seonbi-", suffix });
    await this.#download(url, tempPath);
    permDesc = { name: "read", path: tempPath };
    perm = (await Deno.permissions.query(permDesc)).state;
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const st = await Deno.stat(tempPath);
    if (st.isFile && st.size > 0) return tempPath;
    throw new Error("Failed to download: " + tempPath);
  }

  async #download(url: URL, destPath: string): Promise<void> {
    let permDesc: Deno.PermissionDescriptor = { name: "write", path: destPath };
    let { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const destFile = await Deno.open(destPath, { write: true });
    let downloaded = 0;
    try {
      let response: Response;
      while (true) {
        const permDesc = { name: "net", host: url.hostname } as const;
        const { state: perm } = await Deno.permissions.query(permDesc);
        if (perm !== "granted") await Deno.permissions.request(permDesc);
        response = await fetch(url.href, { redirect: "manual" });
        if (300 <= response.status && response.status < 400) {
          const redir = response.headers.get("Location");
          if (redir != null) {
            response.body?.cancel();
            url = new URL(redir);
            continue;
          }
        }
        break;
      }
      if (!response.ok) {
        throw new Error(
          `Failed to request: ${response.status} ${response.statusText}.`,
        );
      }
      const responseBody = response.body;
      if (responseBody == null) throw new Error("No response body.");
      try {
        const contentLength = response.headers.get("Content-Length");
        const total = contentLength == null ? null : parseInt(contentLength);
        for await (const chunk of responseBody) {
          await writeAll(destFile, chunk);
          downloaded += chunk.byteLength;
        }
        if (total != null && downloaded < total) {
          throw new Error(`Incomplete download: ${downloaded}/${total}.`);
        }
      } finally {
        responseBody.cancel();
      }
    } finally {
      destFile.close();
    }
    permDesc = { name: "read", path: destPath };
    perm = (await Deno.permissions.query(permDesc)).state;
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const stat = await Deno.stat(destPath);
    if (stat.size < downloaded) {
      throw new Error(`Incomplete download: ${stat.size}/${downloaded}.`);
    }
  }

  async #install(distType: DistType): Promise<string> {
    const zipPath = await this.#downloadDist(distType);
    try {
      let permDesc: Deno.PermissionDescriptor = { name: "write" };
      let { state: perm } = await Deno.permissions.query(permDesc);
      if (perm !== "granted") await Deno.permissions.request(permDesc);
      const tempDir = await Deno.makeTempDir({ prefix: "seonbi-" });
      if (zipPath.endsWith(".tar.bz2")) {
        await this.#untar(zipPath, tempDir);
      } else if (zipPath.toLowerCase().endsWith(".zip")) {
        await this.#unzip(zipPath, tempDir);
      } else {
        throw new Error(`Unsupported compression format: ${zipPath}.`);
      }
      permDesc = { name: "read", path: tempDir };
      perm = (await Deno.permissions.query(permDesc)).state;
      if (perm !== "granted") await Deno.permissions.request(permDesc);
      for await (const entry of Deno.readDir(tempDir)) {
        if (!entry.isFile) continue;
        if (
          entry.name === "seonbi-api" ||
          entry.name.toLowerCase() === "seonbi-api.exe"
        ) {
          return join(tempDir, entry.name);
        }
      }
      throw new Error(
        `Failed to install seonbi-api (extraction directory: ${tempDir}).`,
      );
    } finally {
      await Deno.remove(zipPath);
    }
  }

  async #untar(src: string, dst: string): Promise<void> {
    const permDesc: Deno.PermissionDescriptor = { name: "run" };
    const { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const proc = Deno.run({
      cmd: ["tar", "xvfj", src],
      cwd: dst,
      stdout: "null",
      stderr: "piped",
    });
    try {
      const stderr = await readAll(proc.stderr);
      const st = await proc.status();
      if (!st.success) {
        const error = new TextDecoder().decode(stderr);
        throw new Error("Failed to untar:\n" + error);
      }
    } finally {
      proc.stderr.close();
      proc.close();
    }
  }

  async #unzip(src: string, dst: string): Promise<void> {
    const permDesc: Deno.PermissionDescriptor = { name: "run" };
    const { state: perm } = await Deno.permissions.query(permDesc);
    if (perm !== "granted") await Deno.permissions.request(permDesc);
    const proc = Deno.run({
      cmd: [
        "powershell",
        "Expand-Archive",
        "-Path",
        src,
        "-DestinationPath",
        dst,
      ],
      stdout: "null",
      stderr: "piped",
    });
    try {
      const stderr = await readAll(proc.stderr);
      const st = await proc.status();
      if (!st.success) {
        const error = new TextDecoder().decode(stderr);
        throw new Error("Failed to zip:\n" + error);
      }
    } finally {
      proc.stderr.close();
      proc.close();
    }
  }
}

/**
 * A shortcut function equivalent to one-shot `Seonbi#transform()` method.
 * This automatically starts the server before transforming, and stops it
 * after finished.
 */
export async function transform(
  sourceHtml: string,
  configuration: Configuration = DEFAULT_CONFIGURATION,
  options: Preset | Options = { preset: "ko-kr" },
): Promise<string> {
  const i = new Seonbi(configuration);
  try {
    return await i.transform(sourceHtml, options);
  } finally {
    await i.stop();
  }
}

export default Seonbi;
