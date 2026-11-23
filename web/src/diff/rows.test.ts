import { describe, expect, it } from "vitest";
import type { DiffRun } from "../api/diff";
import {
  allShown,
  blockStarts,
  buttonText,
  countsMatch,
  defaultShown,
  expand,
  gapButtons,
  gapLabel,
  hunkStarts,
  indexOfKey,
  indexOfRow,
  keyAt,
  reveal,
  rowLabel,
  rowsOf,
  stepTo,
  visibleOf,
  type Visible,
} from "./rows";

// The visible list as text: "r<row>" for a line row, "gap<start>+<len>".
function show(v: Visible): string[] {
  return Array.from({ length: v.length }, (_, i) =>
    v.gap[i] === 1 ? `gap${v.start[i]}+${v.len[i]}` : `r${v.start[i]}`,
  );
}

const vis = (runs: DiffRun[]) => show(visibleOf(defaultShown(rowsOf(runs))));

describe("rowsOf", () => {
  it("numbers each side", () => {
    const r = rowsOf([
      ["equal", 2],
      ["delete", 1],
      ["insert", 2],
      ["equal", 1],
    ]);
    expect(r.count).toBe(6);
    expect([...r.kind]).toEqual([0, 0, 1, 2, 2, 0]);
    expect([...r.base]).toEqual([1, 2, 3, 0, 0, 4]);
    expect([...r.head]).toEqual([1, 2, 0, 3, 4, 5]);
    expect(r.baseCount).toBe(4);
    expect(r.headCount).toBe(5);
    expect([...r.baseRow].slice(1)).toEqual([0, 1, 2, 5]);
    expect([...r.headRow].slice(1)).toEqual([0, 1, 3, 4, 5]);
  });

  it("empty", () => {
    const r = rowsOf([]);
    expect(r.count).toBe(0);
    expect(visibleOf(defaultShown(r)).length).toBe(0);
    expect(countsMatch(r, 0, 0)).toBe(true);
  });

  it("line counts against the texts", () => {
    const r = rowsOf([
      ["equal", 2],
      ["delete", 1],
      ["insert", 3],
    ]);
    expect(countsMatch(r, 3, 5)).toBe(true);
    expect(countsMatch(r, 3, 4)).toBe(false);
    expect(countsMatch(r, 2, 5)).toBe(false);
  });
});

describe("default context", () => {
  it("added only, removed only: all shown", () => {
    expect(vis([["insert", 3]])).toEqual(["r0", "r1", "r2"]);
    expect(vis([["delete", 2]])).toEqual(["r0", "r1"]);
  });

  it("unchanged: one gap", () => {
    expect(vis([["equal", 50]])).toEqual(["gap0+50"]);
  });

  it("a change at the start or the end", () => {
    expect(
      vis([
        ["insert", 1],
        ["equal", 10],
      ]),
    ).toEqual(["r0", "r1", "r2", "r3", "gap4+7"]);
    expect(
      vis([
        ["equal", 10],
        ["delete", 1],
      ]),
    ).toEqual(["gap0+7", "r7", "r8", "r9", "r10"]);
  });

  it("an equal run of 6 between changes: no gap; 7: a gap of 1", () => {
    const six = vis([
      ["delete", 1],
      ["equal", 6],
      ["insert", 1],
    ]);
    expect(six).toHaveLength(8);
    expect(six.some((s) => s.startsWith("gap"))).toBe(false);
    expect(
      vis([
        ["delete", 1],
        ["equal", 7],
        ["insert", 1],
      ]),
    ).toEqual(["r0", "r1", "r2", "r3", "gap4+1", "r5", "r6", "r7", "r8"]);
  });

  it("a run of 3 at the start: no gap", () => {
    expect(
      vis([
        ["equal", 3],
        ["insert", 1],
      ]),
    ).toEqual(["r0", "r1", "r2", "r3"]);
  });
});

describe("keys", () => {
  const v = visibleOf(
    defaultShown(
      rowsOf([
        ["equal", 10],
        ["delete", 1],
      ]),
    ),
  );
  it("line rows by row, gaps by -(1 + start)", () => {
    expect(keyAt(v, 0)).toBe(-1);
    expect(keyAt(v, 1)).toBe(7);
    expect(indexOfKey(v, 7)).toBe(1);
    expect(indexOfKey(v, -1)).toBe(0);
    expect(indexOfKey(v, 3)).toBe(-1);
    expect(indexOfKey(v, -4)).toBe(-1);
  });
  it("a hidden row is in its gap", () => {
    expect(indexOfRow(v, 0)).toBe(0);
    expect(indexOfRow(v, 6)).toBe(0);
    expect(indexOfRow(v, 10)).toBe(4);
  });
});

describe("expand", () => {
  const rows = rowsOf([
    ["delete", 1],
    ["equal", 56],
    ["insert", 1],
  ]);
  // Gap rows 4..53 (50 lines).
  const shown = defaultShown(rows);
  const gaps = (s: Uint8Array) =>
    show(visibleOf(s)).filter((x) => x.startsWith("gap"));

  it("starts with one gap of 50", () => {
    expect(gaps(shown)).toEqual(["gap4+50"]);
    expect(gapButtons(rows, 4, 50)).toEqual(["down", "up", "all"]);
  });

  it("down shows the top 20, up the bottom 20, all the rest", () => {
    const down = expand(shown, 4, 50, "down");
    expect(gaps(down)).toEqual(["gap24+30"]);
    const up = expand(down, 24, 30, "up");
    expect(gaps(up)).toEqual(["gap24+10"]);
    // 10 left: one button.
    expect(gapButtons(rows, 24, 10)).toEqual(["all"]);
    expect(gaps(expand(up, 24, 10, "all"))).toEqual([]);
    // The input is never changed.
    expect(gaps(shown)).toEqual(["gap4+50"]);
  });

  it("whole file", () => {
    expect(show(visibleOf(allShown(rows)))).toHaveLength(58);
  });

  it("edge gaps get the one button that makes sense", () => {
    const top = rowsOf([
      ["equal", 40],
      ["insert", 1],
    ]);
    expect(gapButtons(top, 0, 37)).toEqual(["up", "all"]);
    const end = rowsOf([
      ["insert", 1],
      ["equal", 40],
    ]);
    expect(gapButtons(end, 4, 37)).toEqual(["down", "all"]);
    expect(gapButtons(rowsOf([["equal", 40]]), 0, 40)).toEqual(["down", "all"]);
  });

  it("a hidden line opens its gap around it", () => {
    const r = reveal(shown, 30);
    expect(gaps(r)).toEqual(["gap4+23", "gap34+20"]);
    // Near the gap's edge: no gap of 0 left.
    expect(gaps(reveal(shown, 5))).toEqual(["gap9+45"]);
  });
});

describe("labels (G23)", () => {
  const rows = rowsOf([
    ["equal", 1],
    ["delete", 2],
    ["insert", 1],
    ["equal", 30],
  ]);
  it("rows", () => {
    expect(rowLabel(rows, 0)).toBe("line 1, unchanged");
    expect(rowLabel(rows, 1)).toBe("removed line 2");
    expect(rowLabel(rows, 3)).toBe("added line 2");
    expect(rowLabel(rows, 4)).toBe("line 3, unchanged, base line 4");
  });
  it("gaps and their buttons", () => {
    // Rows 7..33: head lines 6..32.
    expect(gapLabel(rows, 7, 27)).toBe("27 lines hidden, 6 to 32");
    expect(gapLabel(rows, 7, 1)).toBe("1 line hidden, 6");
    expect(buttonText(rows, 7, 27, "down")).toEqual({
      text: "20",
      name: "20 lines, 6 to 25",
    });
    expect(buttonText(rows, 7, 27, "up")).toEqual({
      text: "20",
      name: "20 lines, 13 to 32",
    });
    expect(buttonText(rows, 7, 27, "all")).toEqual({
      text: "All 27",
      name: "All 27 lines, 6 to 32",
    });
    expect(buttonText(rows, 7, 12, "all")).toEqual({
      text: "Show 12",
      name: "Show 12 lines, 6 to 17",
    });
    expect(buttonText(rows, 7, 1, "all")).toEqual({
      text: "Show 1",
      name: "Show 1 line, 6",
    });
  });
});

describe("hunks and blocks", () => {
  // Changes at rows 2-3, 10 (6 equal between: one hunk), 18 (7
  // equal: a new hunk).
  const r = rowsOf([
    ["equal", 2],
    ["delete", 1],
    ["insert", 1],
    ["equal", 6],
    ["insert", 1],
    ["equal", 7],
    ["delete", 1],
    ["equal", 4],
  ]);

  it("blocks: each run of changes", () => {
    expect(blockStarts(r)).toEqual([2, 10, 18]);
  });

  it("hunks: blocks joined at context 3", () => {
    expect(hunkStarts(r)).toEqual([2, 18]);
    expect(hunkStarts(r, 2)).toEqual([2, 10, 18]);
  });

  it("no changes: none", () => {
    const same = rowsOf([["equal", 5]]);
    expect(blockStarts(same)).toEqual([]);
    expect(hunkStarts(same)).toEqual([]);
  });

  it("a change at the start and the end", () => {
    const e = rowsOf([
      ["insert", 1],
      ["equal", 9],
      ["delete", 2],
    ]);
    expect(blockStarts(e)).toEqual([0, 10]);
    expect(hunkStarts(e)).toEqual([0, 10]);
  });

  it("stepTo: next after, previous before, none at the ends", () => {
    const s = [2, 10, 18];
    expect(stepTo(s, 0, 1)).toBe(2);
    expect(stepTo(s, 2, 1)).toBe(10);
    expect(stepTo(s, 5, -1)).toBe(2);
    expect(stepTo(s, 10, -1)).toBe(2);
    expect(stepTo(s, 18, 1)).toBeUndefined();
    expect(stepTo(s, 2, -1)).toBeUndefined();
    expect(stepTo([], 0, 1)).toBeUndefined();
  });
});
