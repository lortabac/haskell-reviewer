import { describe, expect, it } from "vitest";
import type { DiffRun } from "../api/diff";
import {
  blockStarts,
  defaultShown,
  expand,
  gapKey,
  hunkStarts,
  keyAt,
  rowsOf,
  stepTo,
  visibleOf,
} from "./rows";
import {
  lineOn,
  noLine,
  sideOfRow,
  splitCellLabel,
  splitOf,
  splitShown,
  splitSpace,
  unifiedSpace,
} from "./split";

// Each split row as "base/head", "-" for an empty cell.
function pairs(runs: DiffRun[]): string[] {
  const s = splitOf(rowsOf(runs));
  return Array.from({ length: s.count }, (_, d) => {
    const c = (r: number) => (r < 0 ? "-" : String(r));
    return `${c(s.base[d] ?? -1)}/${c(s.head[d] ?? -1)}`;
  });
}

describe("splitOf", () => {
  it("equal only: one row each, both sides", () => {
    expect(pairs([["equal", 3]])).toEqual(["0/0", "1/1", "2/2"]);
  });

  it("removed only, added only", () => {
    expect(pairs([["delete", 2]])).toEqual(["0/-", "1/-"]);
    expect(pairs([["insert", 2]])).toEqual(["-/0", "-/1"]);
  });

  it("2 removed, 4 added", () => {
    expect(
      pairs([
        ["equal", 1],
        ["delete", 2],
        ["insert", 4],
        ["equal", 1],
      ]),
    ).toEqual(["0/0", "1/3", "2/4", "-/5", "-/6", "7/7"]);
  });

  it("3 removed, 1 added", () => {
    expect(
      pairs([
        ["delete", 3],
        ["insert", 1],
      ]),
    ).toEqual(["0/3", "1/-", "2/-"]);
  });

  it("interleaved: d1/i1, d2/i2", () => {
    expect(
      pairs([
        ["delete", 1],
        ["insert", 1],
        ["delete", 1],
        ["insert", 1],
      ]),
    ).toEqual(["0/1", "2/3"]);
  });

  it("an empty file", () => {
    const s = splitOf(rowsOf([]));
    expect(s.count).toBe(0);
    expect(visibleOf(splitShown(s, new Uint8Array(0))).length).toBe(0);
  });

  it("20k against 20k: 20k rows", () => {
    const s = splitOf(
      rowsOf([
        ["delete", 20000],
        ["insert", 20000],
      ]),
    );
    expect(s.count).toBe(20000);
    expect(s.base[19999]).toBe(19999);
    expect(s.head[19999]).toBe(39999);
    expect(s.at[39999]).toBe(19999);
  });

  it("each unified row maps to the split row that holds it", () => {
    const rows = rowsOf([
      ["equal", 1],
      ["delete", 2],
      ["insert", 4],
      ["equal", 1],
    ]);
    const s = splitOf(rows);
    for (let r = 0; r < rows.count; r++) {
      const d = s.at[r] ?? 0;
      expect(s.base[d] === r || s.head[d] === r).toBe(true);
    }
  });
});

describe("split shown and gaps", () => {
  const runs: DiffRun[] = [
    ["equal", 10],
    ["delete", 1],
    ["insert", 3],
    ["equal", 10],
  ];
  const rows = rowsOf(runs);
  const s = splitOf(rows);
  const space = splitSpace(s);

  it("gaps: same length as in unified, start from head", () => {
    const v = visibleOf(space.shown(defaultShown(rows)));
    // 7 hidden, 3 context, 3 change rows, 3 context, 7 hidden.
    expect(v.length).toBe(11);
    expect(v.gap[0]).toBe(1);
    expect(v.len[0]).toBe(7);
    expect(v.gap[10]).toBe(1);
    expect(v.start[10]).toBe(16);
    expect(v.len[10]).toBe(7);
    // Split row 16 holds unified row 17 (the extra added rows came
    // first).
    expect(space.rowOf(16, "head")).toBe(17);
    expect(space.rowOf(16, "base")).toBe(17);
  });

  it("expanding by the unified start opens the same rows", () => {
    const shown = defaultShown(rows);
    const v = visibleOf(space.shown(shown));
    const d = v.start[10] ?? 0;
    const next = expand(shown, space.rowOf(d, "head"), v.len[10] ?? 0, "all");
    const w = visibleOf(space.shown(next));
    expect(w.gap[w.length - 1]).toBe(0);
    expect(keyAt(w, w.length - 1)).toBe(s.count - 1);
    expect(keyAt(v, 0)).toBe(gapKey(0));
  });

  it("the unified space is the identity", () => {
    const u = unifiedSpace(rows);
    const shown = defaultShown(rows);
    expect(u.shown(shown)).toBe(shown);
    expect(u.rowOf(11, "base")).toBe(11);
    expect(u.displayOf(11)).toBe(11);
  });

  it("blocks and hunks map to their row and side", () => {
    const b = blockStarts(rows);
    expect(b).toEqual([10]);
    expect(hunkStarts(rows)).toEqual([10]);
    expect(space.displayOf(10)).toBe(10);
    expect(sideOfRow(rows, 10)).toBe("base");
    // A block that starts with an added row lands on head.
    const r2 = rowsOf([
      ["equal", 1],
      ["insert", 1],
    ]);
    expect(sideOfRow(r2, blockStarts(r2)[0] ?? 0)).toBe("head");
  });
});

describe("splitCellLabel", () => {
  const rows = rowsOf([
    ["equal", 2],
    ["delete", 1],
    ["insert", 3],
    ["equal", 1],
  ]);

  it("names each cell", () => {
    expect(splitCellLabel(rows, 2, "base")).toBe("removed line 3");
    expect(splitCellLabel(rows, 3, "head")).toBe("added line 3");
    expect(splitCellLabel(rows, 6, "base")).toBe("base line 4, unchanged");
    expect(splitCellLabel(rows, 6, "head")).toBe("line 6, unchanged");
    expect(noLine.base).toBe("No base line");
    expect(noLine.head).toBe("No head line");
  });

  it("lines per side", () => {
    expect(lineOn(rows, 2, "base")).toBe(3);
    expect(lineOn(rows, 2, "head")).toBe(0);
  });
});

describe("first and last rows of a split row", () => {
  // Blocks: D,D,I,I at 1-4; I,D at 6-7.
  const rows = rowsOf([
    ["equal", 1],
    ["delete", 2],
    ["insert", 2],
    ["equal", 1],
    ["insert", 1],
    ["delete", 1],
  ]);
  const space = splitSpace(splitOf(rows));
  const blocks = blockStarts(rows);
  // The step the diff page takes from a split row (DiffView go).
  const from = (d: number, dir: 1 | -1) =>
    stepTo(blocks, dir === 1 ? space.last(d) : space.first(d), dir);

  it("spans", () => {
    expect([space.first(1), space.last(1)]).toEqual([1, 3]);
    expect([space.first(4), space.last(4)]).toEqual([6, 7]);
    expect([space.first(0), space.last(0)]).toEqual([0, 0]);
    expect(blocks).toEqual([1, 6]);
  });

  it("back and forth never land on the same split row", () => {
    // D,D,I,I: either half of its first row.
    expect(from(1, -1)).toBeUndefined();
    expect(from(1, 1)).toBe(6);
    expect(space.displayOf(6)).toBe(4);
    // I,D: its one row, base's removed row is the later one.
    expect(from(4, -1)).toBe(1);
    expect(from(4, 1)).toBeUndefined();
    // Landing sides: base for D first, head for I first.
    expect(sideOfRow(rows, 1)).toBe("base");
    expect(sideOfRow(rows, 6)).toBe("head");
  });
});
