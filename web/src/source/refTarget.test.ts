import { describe, expect, it } from "vitest";
import type { Target } from "../api/source";
import type { Seg } from "./highlight";
import { localHits } from "./localRefs";
import { binderMap, cpOf, refTargetAt } from "./refTarget";

// Line 1: "f ys = g ys 🙂ys"; ys binds at cp 3, used at 10 and 16 (after
// an astral char).
const text = ["f ys = g ys 🙂ys", "other"];
const lineText = (n: number) => text[n - 1] ?? "";
const targets: Target[] = [
  ["local", 1, 3],
  ["decl", "M", "M.hs", 1, "val:g", [], 1, null, false],
  ["external", "Data.Char", null, "listed", 1, "toUpper"],
];
const seg = (from: number, to: number, link: number): Seg => ({
  from,
  to,
  cls: null,
  link,
});
const runs: Seg[][] = [
  [],
  [seg(3, 5, 0), seg(8, 9, 2), seg(10, 12, 1), seg(14, 16, 1)],
  [seg(1, 6, 3)],
];
const binders = binderMap(targets);
const at = (line: number, col: number) =>
  refTargetAt(runs, targets, binders, lineText, { line, col });

describe("cpOf", () => {
  it("counts a pair once", () => {
    expect(cpOf("a🙂b", 0)).toBe(1);
    expect(cpOf("a🙂b", 3)).toBe(3);
    expect(cpOf("ab", 9)).toBe(3);
  });
});

describe("refTargetAt", () => {
  it("binder, use, declaration, external, nothing", () => {
    expect(at(1, 2)).toEqual({
      kind: "local",
      name: "ys",
      binder: { line: 1, cp: 3 },
    });
    expect(at(1, 9)).toEqual({
      kind: "local",
      name: "ys",
      binder: { line: 1, cp: 3 },
    });
    expect(at(1, 7)).toEqual({
      kind: "decl",
      name: "g",
      ref: "M",
      id: "val:g",
    });
    expect(at(2, 1)).toEqual({ kind: "other" });
    expect(at(1, 4)).toEqual({ kind: "none" });
  });
});

describe("localHits", () => {
  it("binder and uses, UTF-16 columns", () => {
    const { matches, binderAt } = localHits(
      runs,
      targets,
      { line: 1, cp: 3 },
      lineText,
    );
    expect(binderAt).toBe(0);
    expect([...matches.from]).toEqual([2, 9, 14]);
    expect([...matches.to]).toEqual([4, 11, 16]);
  });
  it("a binder that is gone", () => {
    const h = localHits(runs, targets, { line: 1, cp: 4 }, lineText);
    expect(h.binderAt).toBe(-1);
  });
});
