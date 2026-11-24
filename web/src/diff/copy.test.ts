import { describe, expect, it } from "vitest";
import type { DiffRun } from "../api/diff";
import type { DiffSessionSide } from "../sides";
import { lineIndex } from "../source/lines";
import {
  breakAfter,
  type CopyModel,
  diffAllText,
  diffCopyText,
  type DiffPoint,
  selectedRange,
} from "./copy";
import { allShown, gapKey, rowsOf, visibleOf } from "./rows";
import { splitOf } from "./split";

function model(
  base: string,
  head: string,
  runs: DiffRun[],
  split = false,
  shown?: Uint8Array,
): CopyModel {
  const rows = rowsOf(runs);
  const s = splitOf(rows);
  const vis = visibleOf(
    split
      ? (shown ?? new Uint8Array(s.count).fill(1))
      : (shown ?? allShown(rows)),
  );
  return {
    rows,
    split: split ? s : undefined,
    base: { text: base, index: lineIndex(base) },
    head: { text: head, index: lineIndex(head) },
    vis,
  };
}

const before: DiffPoint = { kind: "before" };
const after: DiffPoint = { kind: "after" };
const at = (key: number, col: number, side?: DiffSessionSide): DiffPoint => ({
  kind: "at",
  key,
  side,
  col,
});
// Inside a gap row's cell.
const inGap = (key: number): DiffPoint => ({
  kind: "at",
  key,
  side: undefined,
  col: 0,
  whole: true,
});

// Rows: 0 a (equal), 1 b, 2 c (removed), 3 X, 4 Y (added), 5 d
// (equal). Split: 0 a|a, 1 b|X, 2 c|Y, 3 d|d.
const base = "a\nb\nc\nd\n";
const head = "a\nX\nY\nd\n";
const runs: DiffRun[] = [
  ["equal", 1],
  ["delete", 2],
  ["insert", 2],
  ["equal", 1],
];

describe("unified copy", () => {
  it("each row's own text across a block, no signs", () => {
    const m = model(base, head, runs);
    expect(diffCopyText(m, at(0, 0), at(5, 1), "head")).toBe(
      "a\nb\nc\nX\nY\nd",
    );
    expect(diffCopyText(m, at(1, 1), at(3, 0), "head")).toBe("\nc\n");
    expect(diffAllText(m, "head")).toBe("a\nb\nc\nX\nY\nd\n");
  });

  it("before and after: the first row's start, the last row's break", () => {
    const m = model(base, head, runs);
    expect(diffCopyText(m, before, at(1, 1), "head")).toBe("a\nb");
    expect(diffCopyText(m, at(4, 0), after, "head")).toBe("Y\nd\n");
    expect(diffCopyText(m, before, after, "base")).toBe(diffAllText(m, "base"));
  });

  it("CRLF and LF per side; an equal row takes head's", () => {
    const m = model("a\r\nb\r\nc\r\n", "a\nX\nc\n", [
      ["equal", 1],
      ["delete", 1],
      ["insert", 1],
      ["equal", 1],
    ]);
    expect(diffAllText(m, "head")).toBe("a\nb\r\nX\nc\n");
    const eol = model("one\ntwo\n", "one\r\ntwo\r\n", [["equal", 2]]);
    expect(diffAllText(eol, "head")).toBe("one\r\ntwo\r\n");
  });

  it("no BOM; no final newline gives \\n between runs, nothing at the end", () => {
    const bom = model("text\n", "﻿text\n", [["equal", 1]]);
    expect(diffAllText(bom, "head")).toBe("text\n");
    expect(diffCopyText(bom, before, at(0, 2), "head")).toBe("te");
    const m = model("a\nb", "a\nc", [
      ["equal", 1],
      ["delete", 1],
      ["insert", 1],
    ]);
    expect(diffAllText(m, "head")).toBe("a\nb\nc");
  });

  it("a lone \\r at a file's end: \\n between runs, kept at the end", () => {
    const m = model("a\nb\r", "a\nc\r", [
      ["equal", 1],
      ["delete", 1],
      ["insert", 1],
    ]);
    expect(diffAllText(m, "head")).toBe("a\nb\nc\r");
  });

  it("a surrogate pair at an end is taken whole", () => {
    const m = model("x\u{1f600}y\n", "x\u{1f600}y\n", [["equal", 1]]);
    expect(diffCopyText(m, at(0, 2), at(0, 4), "head")).toBe("\u{1f600}y");
  });

  it("hidden gap lines are copied", () => {
    const b = "1\n2\n3\n4\n5\nold\n";
    const h = "1\n2\n3\n4\n5\nnew\n";
    const r: DiffRun[] = [
      ["equal", 5],
      ["delete", 1],
      ["insert", 1],
    ];
    // Rows 0-2 hidden: one gap, then 3, 4, 5, 6.
    const shown = Uint8Array.from([0, 0, 0, 1, 1, 1, 1]);
    const m = model(b, h, r, false, shown);
    expect(diffCopyText(m, inGap(gapKey(0)), at(4, 1), "head")).toBe(
      "1\n2\n3\n4\n5",
    );
    // As the later end: the end of its last row.
    expect(diffCopyText(m, before, inGap(gapKey(0)), "head")).toBe("1\n2\n3");
    // At the gap row's start (the rowgroup before it): none of it.
    expect(diffCopyText(m, before, at(gapKey(0), 0), "head")).toBe("");
    // At its end: all of it.
    expect(diffCopyText(m, before, at(gapKey(0), Infinity), "head")).toBe(
      "1\n2\n3",
    );
  });
});

describe("split copy", () => {
  it("only the anchor's side", () => {
    const m = model(base, head, runs, true);
    expect(diffCopyText(m, at(0, 0, "base"), at(2, 1, "base"), "base")).toBe(
      "a\nb\nc",
    );
    expect(diffCopyText(m, at(1, 0, "head"), at(3, 1, "head"), "head")).toBe(
      "X\nY\nd",
    );
  });

  it("an end in the other column snaps to the side", () => {
    const m = model(base, head, runs, true);
    // Later end on head's row 2: base's last line at row ≤ 2, its end.
    expect(diffCopyText(m, at(1, 1, "base"), at(2, 1, "head"), "base")).toBe(
      "\nc",
    );
    // Earlier end in head's cell of row 1, after base's: from row 2.
    expect(diffCopyText(m, at(1, 1, "head"), at(3, 1, "head"), "base")).toBe(
      "c\nd",
    );
    // Later end in base's cell of row 2, before head's: up to row 1.
    expect(diffCopyText(m, at(0, 0, "head"), at(2, 1, "base"), "head")).toBe(
      "a\nX",
    );
    // In base's half (col 0 or Infinity, as a gutter or the clip's end):
    // the line's start or end.
    expect(diffCopyText(m, at(0, 0, "base"), at(2, 0, "base"), "base")).toBe(
      "a\nb\n",
    );
    expect(
      diffCopyText(m, at(0, Infinity, "base"), at(3, 1, "base"), "base"),
    ).toBe("\nb\nc\nd");
  });

  it("empty cells add nothing and snap", () => {
    // Rows: 0 a|a, 1 b|X, 2 -|Y, 3 d|d.
    const m = model(
      "a\nb\nd\n",
      "a\nX\nY\nd\n",
      [
        ["equal", 1],
        ["delete", 1],
        ["insert", 2],
        ["equal", 1],
      ],
      true,
    );
    expect(m.split?.base[2]).toBe(-1);
    expect(diffCopyText(m, at(2, 0, "base"), at(3, 1, "base"), "base")).toBe(
      "d",
    );
    expect(diffCopyText(m, at(0, 0, "base"), at(2, 0, "base"), "base")).toBe(
      "a\nb",
    );
    // Both ends in the empty cell: nothing.
    expect(diffCopyText(m, at(2, 0, "base"), at(2, 0, "base"), "base")).toBe(
      "",
    );
  });

  it("select-all: the side's file, no BOM; after keeps the last break", () => {
    const m = model("one\ntwo\n", "﻿one\r\ntwo\r\n", [["equal", 2]], true);
    expect(diffAllText(m, "base")).toBe("one\ntwo\n");
    expect(diffAllText(m, "head")).toBe("one\r\ntwo\r\n");
    expect(diffCopyText(m, before, after, "head")).toBe("one\r\ntwo\r\n");
  });

  it("a gap: its first row as the earlier end, its last as the later", () => {
    const b = "1\n2\n3\nold\n";
    const h = "1\n2\n3\nnew\n";
    const m = model(
      b,
      h,
      [
        ["equal", 3],
        ["delete", 1],
        ["insert", 1],
      ],
      true,
      Uint8Array.from([0, 0, 1, 1]),
    );
    expect(diffCopyText(m, inGap(gapKey(0)), at(3, 2, "base"), "base")).toBe(
      "1\n2\n3\nol",
    );
    expect(diffCopyText(m, before, inGap(gapKey(0)), "head")).toBe("1\n2");
    // The gap row's own start and end: before or after its lines.
    expect(diffCopyText(m, before, at(gapKey(0), 0), "head")).toBe("");
    expect(
      diffCopyText(m, at(gapKey(0), Infinity), at(2, 1, "head"), "head"),
    ).toBe("3");
  });
});

describe("breakAfter", () => {
  it("the line's own break", () => {
    const t = (s: string) => ({ text: s, index: lineIndex(s) });
    expect(breakAfter(t("a\r\nb\n"), 1)).toBe("\r\n");
    expect(breakAfter(t("a\r\nb\n"), 2)).toBe("\n");
    expect(breakAfter(t("a\nb"), 2)).toBe("");
    expect(breakAfter(t("a\nb\r"), 2)).toBe("\r");
  });
});

describe("selectedRange", () => {
  it("unified: the side's lines of the rows in the range", () => {
    const m = model(base, head, runs);
    expect(selectedRange(m, at(0, 0), at(5, 1), "head")).toEqual({
      side: "head",
      from: 1,
      to: 4,
    });
    expect(selectedRange(m, at(0, 1), at(4, 1), "base")).toEqual({
      side: "base",
      from: 1,
      to: 3,
    });
    // Only removed rows, head asked: base's.
    expect(selectedRange(m, at(1, 0), at(2, 1), "head")).toEqual({
      side: "base",
      from: 2,
      to: 3,
    });
    // An end at the start of a later row stops at the row before.
    expect(selectedRange(m, at(3, 0), at(5, 0), "head")).toEqual({
      side: "head",
      from: 2,
      to: 3,
    });
    expect(selectedRange(m, before, before, "head")).toBeUndefined();
  });

  it("split: the anchor side's lines, ends snapped", () => {
    const m = model(base, head, runs, true);
    expect(
      selectedRange(m, at(1, 0, "base"), at(3, 0, "head"), "base"),
    ).toEqual(
      // The head cell of row 3 comes after base's: base's row 3 is in.
      { side: "base", from: 2, to: 4 },
    );
    expect(
      selectedRange(m, at(0, 1, "head"), at(2, 1, "head"), "head"),
    ).toEqual({ side: "head", from: 1, to: 3 });
    expect(selectedRange(m, before, after, "base")).toEqual({
      side: "base",
      from: 1,
      to: 4,
    });
  });
});
