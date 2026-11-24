import { describe, expect, it } from "vitest";
import type { DiffRun } from "../api/diff";
import { createFinder } from "../source/find";
import { lineIndex } from "../source/lines";
import { cellAt, cellLine, diffFinder, diffStatus, isFor } from "./find";
import { rowsOf } from "./rows";
import { type DiffLayout, splitOf } from "./split";

function make(base: string, head: string, runs: DiffRun[], layout: DiffLayout) {
  const rows = rowsOf(runs);
  return diffFinder(
    rows,
    createFinder(base, lineIndex(base)),
    createFinder(head, lineIndex(head)),
    layout === "split" ? splitOf(rows) : undefined,
  );
}

// Each hit as "side:row:col".
function hits(f: ReturnType<typeof make>, q: string): string[] {
  const m = f.search(q);
  return Array.from({ length: m.count }, (_, i) => {
    const c = cellAt(f, m.line[i]!);
    return `${c.side}:${c.row}:${m.from[i]}`;
  });
}

// Rows: 0 equal, 1 removed (b2), 2 removed (b3), 3 added (h2), 4 equal.
const base = "x a\nold a\nold2 a\ny a\n";
const head = "x a\nnew a\ny a\n";
const runs: DiffRun[] = [
  ["equal", 1],
  ["delete", 2],
  ["insert", 1],
  ["equal", 1],
];

describe("diffFinder", () => {
  it("unified: each row's shown text, equal rows once", () => {
    const f = make(base, head, runs, "unified");
    expect(hits(f, "a")).toEqual([
      "head:0:2",
      "base:1:4",
      "base:2:5",
      "head:3:4",
      "head:4:2",
    ]);
    expect(f.search("a").line).toEqual(Uint32Array.of(1, 2, 3, 4, 5));
  });

  it("split: base before head on a row, equal rows twice", () => {
    const f = make(base, head, runs, "split");
    // Split rows: 0 = 0/0, 1 = 1/3, 2 = 2/-, 3 = 4/4.
    expect(hits(f, "a")).toEqual([
      "base:0:2",
      "head:0:2",
      "base:1:4",
      "head:3:4",
      "base:2:5",
      "base:4:2",
      "head:4:2",
    ]);
    expect(f.search("a").line).toEqual(Uint32Array.of(1, 2, 3, 4, 5, 7, 8));
  });

  it("a base-only text in an equal row: split finds it, unified not", () => {
    // ws=ignore: the rows are equal, the texts differ.
    const b = "foo  bar\n";
    const h = "foo bar\n";
    const r: DiffRun[] = [["equal", 1]];
    expect(hits(make(b, h, r, "unified"), "  ")).toEqual([]);
    expect(hits(make(b, h, r, "split"), "  ")).toEqual(["base:0:3"]);
  });

  it("counts hits in every row, shown or not", () => {
    const n = 200;
    const t = "z\n".repeat(n);
    const f = make(t, t, [["equal", n]], "unified");
    expect(f.search("z").count).toBe(n);
  });

  it("cellLine and cellAt go both ways", () => {
    for (const layout of ["unified", "split"] as const) {
      const f = make(base, head, runs, layout);
      for (let r = 0; r < f.rows.count; r++) {
        for (const side of ["base", "head"] as const) {
          const v = cellLine(f, r, side);
          if (v < 0) continue;
          const c = cellAt(f, v);
          expect(c.row).toBe(r);
          if (layout === "split") expect(c.side).toBe(side);
        }
      }
    }
    const u = make(base, head, runs, "unified");
    expect(cellLine(u, 0, "base")).toBe(-1);
    // Split row 2's head cell is empty.
    const s = make(base, head, runs, "split");
    expect(cellAt(s, 6)).toEqual({ row: -1, side: "head" });
  });

  it("smart case", () => {
    const f = make("Foo\n", "foo\n", [["equal", 1]], "split");
    expect(hits(f, "foo")).toEqual(["base:0:0", "head:0:0"]);
    expect(hits(f, "Foo")).toEqual(["base:0:0"]);
  });

  it("never hits a CR or a BOM", () => {
    const f = make("one\ntwo\n", "﻿one\r\ntwo\r\n", [["equal", 2]], "split");
    expect(hits(f, "\r")).toEqual([]);
    expect(hits(f, "﻿")).toEqual([]);
    expect(hits(f, "one")).toEqual(["base:0:0", "head:0:0"]);
  });

  it("says where a hit is", () => {
    const u = make(base, head, runs, "unified");
    const m = u.search("a");
    expect(diffStatus(u, m, 1)).toBe("Match 2 of 5, removed line 2");
    expect(diffStatus(u, m, 0)).toBe("Match 1 of 5, line 1, unchanged");
    expect(diffStatus(u, m, -1)).toBe("No matches");
    const s = make(base, head, runs, "split");
    const ms = s.search("a");
    expect(diffStatus(s, ms, 0)).toBe("Match 1 of 7, base line 1, unchanged");
    expect(diffStatus(s, ms, 3)).toBe("Match 4 of 7, added line 2");
    expect(diffStatus(s, s.search("zzz"), 0)).toBe("No matches");
  });

  it("isFor: same rows and same layout only", () => {
    const rows = rowsOf(runs);
    const split = splitOf(rows);
    const none = { search: () => ({ count: 0 }) as never };
    const u = diffFinder(rows, none, none, undefined);
    const s = diffFinder(rows, none, none, split);
    expect(isFor(u, { rows, split: undefined })).toBe(true);
    expect(isFor(u, { rows, split })).toBe(false);
    expect(isFor(s, { rows, split })).toBe(true);
    expect(isFor(s, { rows: rowsOf(runs), split })).toBe(false);
  });
});
