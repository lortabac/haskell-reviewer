import { describe, expect, it } from "vitest";
import type { DiffRun } from "../api/diff";
import type { DiffSessionSide } from "../sides";
import { mapLine } from "./mapLine";
import {
  blocks,
  deleteRow,
  equalRow,
  insertRow,
  type Rows,
  rowsOf,
} from "./rows";
import { splitOf } from "./split";

// marks.sh, src/Marks.hs (base → head).
const marks: DiffRun[] = [
  ["delete", 2],
  ["equal", 3],
  ["insert", 2],
  ["equal", 1],
  ["delete", 1],
  ["insert", 2],
  ["equal", 3],
  ["delete", 2],
];

describe("mapLine on marks.sh", () => {
  const rows = rowsOf(marks);
  const head = (n: number) => mapLine(rows, "head", n);
  it("an equal line: exact", () => {
    expect(head(9)).toEqual({ line: 8, how: "exact" });
    expect(head(1)).toEqual({ line: 3, how: "exact" });
  });
  it("in an insertion: the nearest equal line", () => {
    expect(head(4)).toEqual({ line: 5, how: "nearest" });
    expect(head(5)).toEqual({ line: 6, how: "nearest" });
  });
  it("a replaced line: by index, then the last one", () => {
    expect(head(7)).toEqual({ line: 7, how: "paired" });
    expect(head(8)).toEqual({ line: 7, how: "paired" });
  });
  it("from base: removed at the start and the end", () => {
    expect(mapLine(rows, "base", 1)).toEqual({ line: 1, how: "nearest" });
    expect(mapLine(rows, "base", 2)).toEqual({ line: 1, how: "nearest" });
    expect(mapLine(rows, "base", 12)).toEqual({ line: 11, how: "nearest" });
  });
  it("out of range, or nothing on the other side", () => {
    expect(head(0)).toBeUndefined();
    expect(head(12)).toBeUndefined();
    expect(mapLine(rowsOf([["insert", 3]]), "head", 1)).toBeUndefined();
  });
  it("a tie goes up", () => {
    const r = rowsOf([
      ["equal", 1],
      ["insert", 3],
      ["equal", 1],
    ]);
    expect(mapLine(r, "head", 3)).toEqual({ line: 1, how: "nearest" });
    expect(mapLine(r, "head", 4)).toEqual({ line: 2, how: "nearest" });
  });
});

// A small seeded generator (mulberry32).
function rng(seed: number): () => number {
  let a = seed;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function randomRuns(next: () => number): DiffRun[] {
  const ops = ["equal", "delete", "insert"] as const;
  const out: DiffRun[] = [];
  const n = Math.floor(next() * 8);
  for (let i = 0; i < n; i++) {
    out.push([ops[Math.floor(next() * 3)]!, 1 + Math.floor(next() * 4)]);
  }
  return out;
}

const sides: DiffSessionSide[] = ["base", "head"];

describe("mapLine, random runs", () => {
  it("holds its rules", () => {
    const next = rng(349);
    for (let run = 0; run < 5000; run++) {
      const rows = rowsOf(randomRuns(next));
      for (const from of sides) check(rows, from);
    }
  });
});

function check(rows: Rows, from: DiffSessionSide): void {
  const count = from === "base" ? rows.baseCount : rows.headCount;
  const otherCount = from === "base" ? rows.headCount : rows.baseCount;
  const lineRow = from === "base" ? rows.baseRow : rows.headRow;
  const there = from === "base" ? rows.head : rows.base;
  const back = from === "base" ? "head" : "base";
  const own = from === "base" ? deleteRow : insertRow;
  const split = splitOf(rows);
  const bs = blocks(rows);
  let last = 0;
  for (let n = 1; n <= count; n++) {
    const m = mapLine(rows, from, n);
    if (otherCount === 0) {
      expect(m).toBeUndefined();
      continue;
    }
    if (m === undefined) throw new Error("undefined");
    // In range, never going down.
    expect(m.line).toBeGreaterThanOrEqual(1);
    expect(m.line).toBeLessThanOrEqual(otherCount);
    expect(m.line).toBeGreaterThanOrEqual(last);
    last = m.line;
    const r = lineRow[n] ?? 0;
    if (rows.kind[r] === equalRow) {
      expect(m.how).toBe("exact");
      expect(mapLine(rows, back, m.line)).toEqual({ line: n, how: "exact" });
      continue;
    }
    const [s, e] = bs.find(([a, b]) => a <= r && r < b)!;
    const theirs: number[] = [];
    for (let k = s; k < e; k++) {
      if (rows.kind[k] !== own && rows.kind[k] !== equalRow) {
        theirs.push(there[k] ?? 0);
      }
    }
    if (theirs.length > 0) {
      expect(m.how).toBe("paired");
      // Same split row as splitOf, else the other side's last line.
      const d = split.at[r] ?? 0;
      const cell = (from === "base" ? split.head : split.base)[d] ?? -1;
      const want = cell >= 0 ? (there[cell] ?? 0) : theirs.at(-1);
      expect(m.line).toBe(want);
    } else {
      expect(m.how).toBe("nearest");
      const up = s > 0 ? r - s + 1 : Infinity;
      const down = e < rows.count ? e - r : Infinity;
      const want = up <= down ? there[s - 1] : there[e];
      expect(m.line).toBe(want);
    }
  }
}
