import { decode } from "cbor-x/decode-no-eval";
import { describe, expect, it } from "vitest";
import { cborFixture, cborFixtures } from "../../test/fixtures";
import { decodeDiffView, type DiffRun } from "../api/diff";
import { equalRow, rowsOf } from "../diff/rows";
import {
  addedLine,
  changeStatus,
  changeWord,
  edgeAbove,
  edgeBelow,
  modifiedLine,
  removedLine,
  sideChanges,
} from "./diffMarks";

const head = (runs: DiffRun[]) => sideChanges(rowsOf(runs), "head");
const base = (runs: DiffRun[]) => sideChanges(rowsOf(runs), "base");
const arr = (a: Uint8Array) => [...a].slice(1);

describe("sideChanges, head", () => {
  it("a pure insert: added", () => {
    const c = head([
      ["equal", 2],
      ["insert", 2],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([0, 0, addedLine, addedLine, 0]);
    expect(c.starts).toEqual([3]);
  });
  it("a pure delete in the middle: removed below the line before", () => {
    const c = head([
      ["equal", 2],
      ["delete", 3],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([0, 0, 0]);
    expect(arr(c.edge)).toEqual([0, edgeBelow, 0]);
    expect(c.starts).toEqual([2]);
  });
  it("a delete at the start: line 1 above", () => {
    const c = head([
      ["delete", 1],
      ["equal", 2],
    ]);
    expect(arr(c.edge)).toEqual([edgeAbove, 0]);
    expect(c.starts).toEqual([1]);
  });
  it("a delete at the end: the last line below", () => {
    const c = head([
      ["equal", 2],
      ["delete", 1],
    ]);
    expect(arr(c.edge)).toEqual([0, edgeBelow]);
  });
  it("line 1 can have both", () => {
    const c = head([
      ["delete", 1],
      ["equal", 1],
      ["delete", 1],
      ["equal", 1],
    ]);
    expect(arr(c.edge)).toEqual([edgeAbove | edgeBelow, 0]);
    expect(c.starts).toEqual([1]);
  });
  it("a mixed block: all added lines are modified", () => {
    const c = head([
      ["equal", 1],
      ["delete", 1],
      ["insert", 3],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([
      0,
      modifiedLine,
      modifiedLine,
      modifiedLine,
      0,
    ]);
    expect(c.starts).toEqual([2]);
  });
  it("delete, insert, delete in one block", () => {
    const c = head([
      ["equal", 1],
      ["delete", 1],
      ["insert", 1],
      ["delete", 1],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([0, modifiedLine, 0]);
  });
  it("all equal, or empty: nothing", () => {
    expect(head([["equal", 3]]).starts).toEqual([]);
    expect(head([]).starts).toEqual([]);
    expect(arr(head([["equal", 3]]).own)).toEqual([0, 0, 0]);
  });
  it("a new file: all added; a removed file: no lines", () => {
    const c = head([["insert", 3]]);
    expect(arr(c.own)).toEqual([addedLine, addedLine, addedLine]);
    expect(c.starts).toEqual([1]);
    expect(head([["delete", 3]]).starts).toEqual([]);
  });
});

describe("sideChanges, base", () => {
  it("a pure delete: removed; a pure insert: added below", () => {
    const c = base([
      ["equal", 1],
      ["delete", 2],
      ["equal", 1],
      ["insert", 1],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([0, removedLine, removedLine, 0, 0]);
    expect(arr(c.edge)).toEqual([0, 0, 0, edgeBelow, 0]);
    expect(c.starts).toEqual([2, 4]);
  });
  it("an insert at the start: line 1 above; mixed: modified", () => {
    expect(
      arr(
        base([
          ["insert", 1],
          ["equal", 1],
        ]).edge,
      ),
    ).toEqual([edgeAbove]);
    const c = base([
      ["delete", 2],
      ["insert", 1],
      ["equal", 1],
    ]);
    expect(arr(c.own)).toEqual([modifiedLine, modifiedLine, 0]);
  });
  it("starts have no duplicates", () => {
    const c = base([
      ["equal", 1],
      ["insert", 1],
      ["equal", 1],
      ["insert", 1],
      ["equal", 1],
    ]);
    expect(c.starts).toEqual([1, 2]);
  });
});

describe("words", () => {
  const c = head([
    ["equal", 1],
    ["insert", 1],
    ["delete", 1],
    ["equal", 1],
  ]);
  it("own and edge lines", () => {
    // Insert then delete: one block, so modified.
    expect(changeWord(c, 2, "head")).toBe("modified");
    expect(changeWord(c, 1, "head")).toBeUndefined();
    expect(changeStatus(c, 2, "head")).toBe("modified line 2");
    expect(changeStatus(c, 1, "head")).toBe("line 1");
  });
  it("edges", () => {
    const e = head([
      ["equal", 1],
      ["delete", 1],
      ["equal", 1],
    ]);
    expect(changeWord(e, 1, "head")).toBe("lines removed below");
    expect(changeStatus(e, 1, "head")).toBe("line 1, lines removed below");
    const b = base([
      ["insert", 1],
      ["equal", 1],
      ["insert", 1],
    ]);
    expect(changeWord(b, 1, "base")).toBe("lines added above and below");
  });
});

describe("every api/diff fixture", () => {
  const names = [...cborFixtures.keys()].filter((n) => n.startsWith("diff-"));
  it.each(names)("%s: changed rows marked, edges on equal rows", (name) => {
    const v = decodeDiffView(decode(cborFixture(name)));
    if (v.diff.kind !== "lines") return;
    const rows = rowsOf(v.diff.runs);
    for (const side of ["base", "head"] as const) {
      const c = sideChanges(rows, side);
      const lines = side === "head" ? rows.head : rows.base;
      const mine = side === "head" ? 2 : 1;
      for (let r = 0; r < rows.count; r++) {
        const l = lines[r] ?? 0;
        if (rows.kind[r] === mine) expect(c.own[l]).toBeGreaterThan(0);
        if (rows.kind[r] === equalRow) expect(c.own[l]).toBe(0);
      }
      for (let l = 1; l < c.edge.length; l++) {
        if ((c.edge[l] ?? 0) > 0) {
          const r = (side === "head" ? rows.headRow : rows.baseRow)[l] ?? 0;
          expect(rows.kind[r]).toBe(equalRow);
        }
      }
      expect(c.starts).toEqual([...c.starts].sort((a, b) => a - b));
    }
  });
});
