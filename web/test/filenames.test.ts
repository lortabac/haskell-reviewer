// No two source files whose import paths differ only in case
// (Source.tsx and source.ts): macOS and Windows file systems can't
// tell them apart, so "./Source" finds the wrong one there. Runs on
// Linux too, where both work.
import { readdirSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";

const root = join(import.meta.dirname, "..");
const dirs = ["src", "test", "e2e"];

// The path as an import names it, lower case: without .ts, .tsx, .js,
// .mjs (".test" and ".spec" stay: they are part of the name).
function importKey(path: string): string {
  return path.replace(/\.(tsx?|m?js)$/, "").toLowerCase();
}

// Groups of paths with the same key.
function clashes(paths: string[]): string[][] {
  const byKey = new Map<string, string[]>();
  for (const p of paths) {
    const k = importKey(p);
    byKey.set(k, [...(byKey.get(k) ?? []), p]);
  }
  return [...byKey.values()].filter((g) => g.length > 1);
}

function files(dir: string): string[] {
  return readdirSync(join(root, dir), { recursive: true, withFileTypes: true })
    .filter((e) => e.isFile())
    .map((e) => relative(root, join(e.parentPath, e.name)));
}

describe("file names", () => {
  it("catches names that differ only in case or extension", () => {
    expect(
      clashes([
        "src/docs/Source.tsx",
        "src/docs/source.ts",
        "src/docs/source.test.ts",
        "src/docs/sections.ts",
      ]),
    ).toEqual([["src/docs/Source.tsx", "src/docs/source.ts"]]);
  });

  it("no two files in src, test, e2e that a case-blind disk mixes up", () => {
    const groups = clashes(dirs.flatMap(files));
    expect(
      groups.map((g) => `same name but for case or extension: ${g.join(", ")}`),
    ).toEqual([]);
  });
});
