import { describe, expect, it } from "vitest";
import { rng } from "../../test/random";
import type { OutlineEntry } from "../api/source";
import { declAtLine } from "../keys/outline";
import {
  keepSourceLine,
  lineOf,
  placeOf,
  sourceKeepFrom,
  sourcePlaceFrom,
} from "./keepLine";

// kind, id, label, name line, first line, last line.
const e = (id: string, first: number, last: number) =>
  ["val", id, id, first, first, last] as unknown as OutlineEntry;

const old = [e("val:a", 3, 5), e("type:T", 8, 12), e("con:C", 10, 10)];

describe("keepSourceLine", () => {
  it("inside an entry: its first line plus the offset", () => {
    const moved = [e("val:a", 6, 8), e("type:T", 11, 15), e("con:C", 13, 13)];
    expect(keepSourceLine(old, moved, 30, 4)).toBe(7);
    // The smallest entry: con:C.
    expect(keepSourceLine(old, moved, 30, 10)).toBe(13);
    expect(keepSourceLine(old, moved, 30, 12)).toBe(15);
  });
  it("clamped to the entry when it got shorter", () => {
    expect(keepSourceLine(old, [e("val:a", 3, 3)], 30, 5)).toBe(3);
  });
  it("between entries: from the last line of the one above", () => {
    expect(keepSourceLine(old, [e("val:a", 10, 12)], 30, 7)).toBe(14);
  });
  it("gone, or no entry: the same line, clamped to the file", () => {
    expect(keepSourceLine(old, [], 30, 4)).toBe(4);
    expect(keepSourceLine(old, undefined, 30, 4)).toBe(4);
    expect(keepSourceLine(old, old, 30, 1)).toBe(1);
    expect(keepSourceLine(old, [], 3, 9)).toBe(3);
    expect(keepSourceLine([], [], 0, 9)).toBe(1);
  });
  it("two entries with one id: by which one", () => {
    const two = [e("inst:X", 1, 2), e("inst:X", 5, 6)];
    const moved = [e("inst:X", 1, 2), e("inst:X", 9, 10)];
    expect(keepSourceLine(two, moved, 20, 6)).toBe(10);
    expect(keepSourceLine(two, moved, 20, 2)).toBe(2);
  });
  it("a place reads back from history state", () => {
    const p = placeOf(old, 11);
    expect(sourcePlaceFrom(JSON.parse(JSON.stringify(p)))).toEqual(p);
    expect(sourcePlaceFrom({ ...p, decl: { ...p.decl, from: "x" } })).toBe(
      undefined,
    );
    expect(sourcePlaceFrom({ line: 3 })).toEqual({ line: 3 });
    expect(sourcePlaceFrom({ line: "3" })).toBeUndefined();
    expect(sourcePlaceFrom({ line: 3, decl: { id: 1 } })).toBeUndefined();
    expect(sourcePlaceFrom(null)).toBeUndefined();
  });
  it("a kept page reads back; a top needs its below", () => {
    const k = {
      cursor: placeOf(old, 4),
      col: 2,
      marks: [placeOf(old, 3), placeOf(old, 5)],
      top: { place: placeOf(old, 1), below: 20 },
    };
    expect(sourceKeepFrom(JSON.parse(JSON.stringify(k)))).toEqual(k);
    expect(sourceKeepFrom({ ...k, top: { place: placeOf(old, 1) } })).toEqual({
      cursor: k.cursor,
      col: 2,
      marks: k.marks,
    });
  });
});

// Top-level entries with gaps between, some with one entry inside.
// The file's line count too.
function randomOutline(next: () => number): {
  outline: OutlineEntry[];
  count: number;
} {
  const outline: OutlineEntry[] = [];
  let line = 1 + Math.floor(next() * 4);
  const n = Math.floor(next() * 6);
  for (let i = 0; i < n; i++) {
    const first = line;
    const last = first + Math.floor(next() * 6);
    // Same ids now and then.
    outline.push(e(`val:${Math.floor(next() * 4)}`, first, last));
    if (last > first && next() < 0.4) {
      const f = first + 1 + Math.floor(next() * (last - first));
      outline.push(e(`con:${i}`, f, f + Math.floor(next() * (last - f + 1))));
    }
    line = last + 1 + Math.floor(next() * 3);
  }
  return { outline, count: line + Math.floor(next() * 3) };
}

// k new lines at line q: old line x >= q is now x + k.
function insertAt(
  outline: readonly OutlineEntry[],
  q: number,
  k: number,
): OutlineEntry[] {
  const at = (x: number) => (x >= q ? x + k : x);
  return outline.map(([kind, id, label, l, f, t]) => [
    kind,
    id,
    label,
    at(l),
    at(f),
    at(t),
  ]);
}

describe("keepSourceLine, random outlines", () => {
  it("the same outline: the same line", () => {
    const next = rng(356);
    for (let run = 0; run < 2000; run++) {
      const { outline, count } = randomOutline(next);
      for (let l = 1; l <= count; l++) {
        expect(keepSourceLine(outline, outline, count, l)).toBe(l);
      }
    }
  });
  it("lines put in above the line's entry: the line moves with them", () => {
    const next = rng(357);
    for (let run = 0; run < 2000; run++) {
      const { outline, count } = randomOutline(next);
      const k = 1 + Math.floor(next() * 5);
      const q = 1 + Math.floor(next() * count);
      const moved = insertAt(outline, q, k);
      for (let l = 1; l <= count; l++) {
        const d = declAtLine(outline, l);
        const got = keepSourceLine(outline, moved, count + k, l);
        // The line the place counts from.
        const anchor = d === undefined ? undefined : l <= d[5] ? d[4] : d[5];
        if (anchor !== undefined && q <= anchor) expect(got).toBe(l + k);
        else if (d === undefined) expect(got).toBe(l);
        // Always in the file.
        expect(got).toBeGreaterThanOrEqual(1);
        expect(got).toBeLessThanOrEqual(count + k);
      }
    }
  });
  it("any new outline: in the file, and in the entry when it is there", () => {
    const next = rng(358);
    for (let run = 0; run < 2000; run++) {
      const a = randomOutline(next);
      const b = randomOutline(next);
      for (let l = 1; l <= a.count; l++) {
        const p = placeOf(a.outline, l);
        const got = lineOf(p, b.outline, b.count);
        expect(got).toBeGreaterThanOrEqual(1);
        expect(got).toBeLessThanOrEqual(b.count);
        if (p.decl?.from !== "first") continue;
        const { id, nth } = p.decl;
        const same = b.outline.filter((x) => x[1] === id)[nth];
        if (same !== undefined && same[5] <= b.count) {
          expect(got).toBeGreaterThanOrEqual(same[4]);
          expect(got).toBeLessThanOrEqual(same[5]);
        }
      }
    }
  });
});
