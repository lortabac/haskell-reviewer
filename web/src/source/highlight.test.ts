// @vitest-environment node
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  displayClasses,
  type LexedSource,
  packRuns,
  type Run,
} from "../api/source";
import { highlightOf, lineKinds, lineRuns, outlineByLine } from "./highlight";

const run = (
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
  cls = 0,
  link = 0,
): Run => ({ startLine, startCol, endLine, endCol, cls, link });

describe("lineRuns", () => {
  it("runs on one line stay as they are, class names", () => {
    const r = lineRuns(
      packRuns([run(1, 1, 1, 3, 0, 2), run(1, 4, 1, 5, 2)]),
      2,
    );
    expect(r[1]).toEqual([
      { from: 1, to: 3, cls: "keyword", link: 2 },
      { from: 4, to: 5, cls: "variable", link: 0 },
    ]);
    expect(r[2]).toEqual([]);
  });

  it("a run over lines is cut; an end at (L + 1, 1) gives nothing there", () => {
    const r = lineRuns(packRuns([run(1, 5, 3, 3, 5), run(4, 1, 6, 1, 7)]), 6);
    const seg = (from: number, to: number, cls: string) => [
      { from, to, cls, link: 0 },
    ];
    expect(r[1]).toEqual(seg(5, Infinity, "comment"));
    expect(r[2]).toEqual(seg(1, Infinity, "comment"));
    expect(r[3]).toEqual(seg(1, 3, "comment"));
    expect(r[4]).toEqual(seg(1, Infinity, "cppExcluded"));
    expect(r[5]).toEqual(seg(1, Infinity, "cppExcluded"));
    expect(r[6]).toEqual([]);
  });

  it("clipped at the line count", () => {
    // A region run of the last line: (3, 1)-(4, 1) in a 3-line file.
    const r = lineRuns(packRuns([run(2, 1, 4, 1, 7), run(9, 1, 9, 2)]), 3);
    expect(r).toHaveLength(4);
    expect(r[3]).toEqual([
      { from: 1, to: Infinity, cls: "cppExcluded", link: 0 },
    ]);
  });

  it("no runs", () => {
    expect(lineRuns([], 0)).toEqual([[]]);
    expect(lineRuns([], 2)).toEqual([[], [], []]);
  });
});

describe("lineKinds", () => {
  it("directives and excluded lines; excluded wins; prose is code", () => {
    // kind, first, last: directives 0, excluded 1, prose 2.
    const k = lineKinds([0, 1, 1, 1, 2, 3, 0, 3, 4, 2, 5, 9], 6);
    expect(k.slice(1)).toEqual([
      "directive",
      "excluded",
      "excluded",
      "directive",
      "code",
      "code",
    ]);
  });
});

describe("outline", () => {
  it("the first entry per line", () => {
    const m = outlineByLine([
      ["value", "val:a", "a", 3, 3, 3],
      ["value", "val:b", "b", 3, 3, 4],
      ["type", "type:T", "T", 5, 5, 5],
    ]);
    expect([...m.entries()].map(([l, e]) => [l, e[1]])).toEqual([
      [3, "val:a"],
      [5, "type:T"],
    ]);
  });

  it("entries past the file's end are dropped", () => {
    const s: LexedSource = {
      generation: 1,
      path: "A.hs",
      ref: "A",
      tier: { kind: "syntactic", reason: "no-hie" },
      partial: false,
      highlight: "lexed",
      runs: [],
      regions: [],
      targets: [],
      outline: [
        ["value", "val:a", "a", 2, 2, 2],
        ["value", "val:b", "b", 3, 3, 3],
      ],
    };
    const h = highlightOf(s, 2);
    expect(h.outline.map((e) => e[1])).toEqual(["val:a"]);
    expect([...h.byLine.keys()]).toEqual([2]);
  });
});

describe("classes", () => {
  it("each class has a colour in code.css", () => {
    const css = readFileSync(
      join(import.meta.dirname, "../styles/code.css"),
      "utf-8",
    );
    for (const c of displayClasses) {
      expect(css, c).toMatch(
        new RegExp(`\\.hl-${c}\\b[^{]*\\{\\s*color: var\\(--color-hl-`),
      );
    }
  });
});
