import { describe, expect, it } from "vitest";
import { randomRuns, rng } from "../../test/random";
import type { DiffRun } from "../api/diff";
import type { DiffSessionSide } from "../sides";
import {
  diffLineOf,
  diffPlaceFrom,
  diffPlaceOf,
  keepDiffLine,
} from "./keepLine";
import { deleteRow, equalRow, insertRow, type Rows, rowsOf } from "./rows";

const head = (line: number) => ({ side: "head", line });
const base = (line: number) => ({ side: "base", line });

describe("keepDiffLine", () => {
  // base 1-4, head: two new lines after base 1.
  const old = rowsOf([
    ["equal", 1],
    ["insert", 2],
    ["equal", 3],
  ]);
  // Two more new lines at the top of head.
  const now = rowsOf([
    ["insert", 2],
    ["equal", 1],
    ["insert", 2],
    ["equal", 3],
  ]);
  it("a base line stays", () => {
    for (let l = 1; l <= 4; l++) {
      expect(keepDiffLine(old, now, "base", l)).toEqual(base(l));
    }
  });
  it("a head line goes through base", () => {
    expect(keepDiffLine(old, now, "head", 1)).toEqual(head(3));
    // In the insertion: from the equal line above it.
    expect(keepDiffLine(old, now, "head", 2)).toEqual(head(4));
    expect(keepDiffLine(old, now, "head", 3)).toEqual(head(5));
    expect(keepDiffLine(old, now, "head", 6)).toEqual(head(8));
  });
  it("an insertion nearer the equal line below: from that line", () => {
    // base 1-3; head: 1, 4 new lines, then base 2-3. Head line 5 is
    // one above base 2 (head line 6).
    const a = rowsOf([
      ["equal", 1],
      ["insert", 4],
      ["equal", 2],
    ]);
    const b = rowsOf([
      ["insert", 3],
      ["equal", 1],
      ["insert", 4],
      ["equal", 2],
    ]);
    expect(diffPlaceOf(a, "head", 5).base).toEqual({ line: 2, offset: -1 });
    expect(keepDiffLine(a, b, "head", 5)).toEqual(head(8));
  });
  it("clamped to the side", () => {
    const short = rowsOf([["equal", 2]]);
    expect(keepDiffLine(old, short, "head", 6)).toEqual(head(2));
    expect(keepDiffLine(old, short, "base", 4)).toEqual(base(2));
  });
  it("an added file: the same line, clamped", () => {
    const added = rowsOf([["insert", 5]]);
    expect(diffPlaceOf(added, "head", 3)).toEqual(head(3));
    expect(keepDiffLine(added, rowsOf([["insert", 2]]), "head", 3)).toEqual(
      head(2),
    );
    // Base came back: still the same line.
    expect(keepDiffLine(added, old, "head", 3)).toEqual(head(3));
  });
  it("head removed since: the base line it went through", () => {
    const removed = rowsOf([["delete", 4]]);
    expect(keepDiffLine(old, removed, "head", 6)).toEqual(base(4));
  });
  it("a place reads back from history state", () => {
    const p = diffPlaceOf(old, "head", 3);
    expect(diffPlaceFrom(JSON.parse(JSON.stringify(p)))).toEqual(p);
    expect(diffPlaceFrom(base(2))).toEqual(base(2));
    expect(diffPlaceFrom({ side: "tree", line: 2 })).toBeUndefined();
    expect(diffPlaceFrom({ side: "head", line: 2, base: 1 })).toBeUndefined();
  });
});

const ops = ["equal", "delete", "insert"] as const;

// The rows' kinds as runs again.
function runsOf(kinds: readonly number[]): DiffRun[] {
  const out: DiffRun[] = [];
  for (const k of kinds) {
    const op = ops[k]!;
    const last = out.at(-1);
    if (last?.[0] === op) last[1]++;
    else out.push([op, 1]);
  }
  return out;
}

// k new head lines before row r.
function insertBefore(rows: Rows, r: number, k: number): Rows {
  const kinds = [...rows.kind];
  kinds.splice(r, 0, ...Array<number>(k).fill(insertRow));
  return rowsOf(runsOf(kinds));
}

// Head loses the lines of the equal rows [r, r + k): base only now.
function dropHead(rows: Rows, r: number, k: number): Rows {
  const kinds = [...rows.kind];
  kinds.fill(deleteRow, r, r + k);
  return rowsOf(runsOf(kinds));
}

// The first equal row at r or after it.
function equalFrom(rows: Rows, r: number): number | undefined {
  for (let i = r; i < rows.count; i++) {
    if (rows.kind[i] === equalRow) return i;
  }
  return undefined;
}

const sides: DiffSessionSide[] = ["base", "head"];

describe("keepDiffLine, random runs", () => {
  it("the same rows: the same line", () => {
    const next = rng(356);
    for (let run = 0; run < 3000; run++) {
      const rows = rowsOf(randomRuns(next));
      for (const s of sides) {
        const count = s === "base" ? rows.baseCount : rows.headCount;
        for (let l = 1; l <= count; l++) {
          expect(keepDiffLine(rows, rows, s, l)).toEqual({ side: s, line: l });
        }
      }
    }
  });
  it("head lines put in anywhere: head lines past the next equal row move, base stays", () => {
    const next = rng(357);
    for (let run = 0; run < 3000; run++) {
      const rows = rowsOf(randomRuns(next));
      // Any row, in a change block too.
      const r = Math.floor(next() * (rows.count + 1));
      const e = equalFrom(rows, r);
      const k = 1 + Math.floor(next() * 4);
      const now = insertBefore(rows, r, k);
      for (let l = 1; l <= rows.baseCount; l++) {
        expect(keepDiffLine(rows, now, "base", l)).toEqual(base(l));
      }
      if (e === undefined) continue;
      for (let l = rows.head[e]!; l <= rows.headCount; l++) {
        expect(keepDiffLine(rows, now, "head", l)).toEqual(head(l + k));
      }
    }
  });
  it("head lines taken out above an equal row: head lines below it move up", () => {
    const next = rng(359);
    for (let run = 0; run < 3000; run++) {
      const rows = rowsOf(randomRuns(next));
      // k equal rows from r, then one more that stays.
      const r = Math.floor(next() * rows.count);
      let k = 0;
      while (r + k + 1 < rows.count && rows.kind[r + k] === equalRow) {
        if (next() < 0.5) break;
        k++;
      }
      if (k === 0 || rows.kind[r + k] !== equalRow) continue;
      const now = dropHead(rows, r, k);
      for (let l = rows.head[r + k]!; l <= rows.headCount; l++) {
        expect(keepDiffLine(rows, now, "head", l)).toEqual(head(l - k));
      }
    }
  });
  it("any new rows: in the side", () => {
    const next = rng(358);
    for (let run = 0; run < 3000; run++) {
      const a = rowsOf(randomRuns(next));
      const b = rowsOf(randomRuns(next));
      for (const s of sides) {
        const count = s === "base" ? a.baseCount : a.headCount;
        for (let l = 1; l <= count; l++) {
          const got = diffLineOf(diffPlaceOf(a, s, l), b);
          const there = got.side === "base" ? b.baseCount : b.headCount;
          expect(got.line).toBeGreaterThanOrEqual(1);
          expect(got.line).toBeLessThanOrEqual(Math.max(there, 1));
          // Only base when head is gone.
          if (got.side !== s) expect(b.headCount).toBe(0);
        }
      }
    }
  });
});
