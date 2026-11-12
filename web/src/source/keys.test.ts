import { describe, expect, it } from "vitest";
import { isSelectAll, keyTarget, landOn, type Mods, nextStop } from "./keys";

const none: Mods = { ctrl: false, meta: false, alt: false, shift: false };
const ctrl: Mods = { ...none, ctrl: true };

describe("keyTarget", () => {
  it.each([
    ["ArrowDown", none, 11],
    ["ArrowUp", none, 9],
    ["PageDown", none, 30],
    ["PageUp", none, 1],
    ["Home", none, 1],
    ["End", none, 100],
    ["Home", ctrl, 1],
    ["End", ctrl, 100],
  ] as const)("%s %o from 10", (key, mods, to) => {
    expect(keyTarget(key, mods, 10, 100, 20)).toBe(to);
  });

  it("clamps into the file", () => {
    expect(keyTarget("ArrowUp", none, 1, 100, 20)).toBe(1);
    expect(keyTarget("PageDown", none, 95, 100, 20)).toBe(100);
  });

  it.each([
    ["ArrowLeft", none],
    ["a", none],
    ["ArrowDown", ctrl],
    ["ArrowDown", { ...none, shift: true }],
    ["End", { ...none, alt: true }],
    ["Home", { ...none, meta: true }],
  ] as const)("%s %o is not ours", (key, mods) => {
    expect(keyTarget(key, mods, 10, 100, 20)).toBeUndefined();
  });
});

describe("isSelectAll", () => {
  it("Ctrl+A and Cmd+A", () => {
    expect(isSelectAll("a", ctrl)).toBe(true);
    expect(isSelectAll("A", { ...none, meta: true })).toBe(true);
    expect(isSelectAll("a", none)).toBe(false);
    expect(isSelectAll("a", { ...ctrl, shift: true })).toBe(false);
  });
});

describe("token stops", () => {
  // Links at 2-4 and 6-9.
  const stops = [
    { start: 2, end: 4 },
    { start: 6, end: 9 },
  ];
  const cellAt = (col: number) => ({ link: false, col });
  const linkAt = (col: number) => ({ link: true, col });

  it("→ goes to the next link; none: undefined", () => {
    expect(nextStop(stops, cellAt(0), 1)).toEqual(linkAt(2));
    // The cell at a link's start sits before it.
    expect(nextStop(stops, cellAt(2), 1)).toEqual(linkAt(2));
    expect(nextStop(stops, cellAt(3), 1)).toEqual(linkAt(6));
    expect(nextStop(stops, linkAt(2), 1)).toEqual(linkAt(6));
    expect(nextStop(stops, linkAt(6), 1)).toBeUndefined();
    expect(nextStop([], cellAt(0), 1)).toBeUndefined();
  });

  it("← goes to the previous link, then the cell at the first one", () => {
    expect(nextStop(stops, linkAt(6), -1)).toEqual(linkAt(2));
    expect(nextStop(stops, linkAt(2), -1)).toEqual(cellAt(2));
    expect(nextStop(stops, cellAt(2), -1)).toBeUndefined();
    expect(nextStop(stops, cellAt(5), -1)).toEqual(linkAt(2));
    expect(nextStop(stops, cellAt(20), -1)).toEqual(linkAt(6));
    expect(nextStop([], cellAt(3), -1)).toBeUndefined();
  });

  it("a link at column 0", () => {
    const s = [{ start: 0, end: 3 }];
    expect(nextStop(s, cellAt(0), 1)).toEqual(linkAt(0));
    expect(nextStop(s, linkAt(0), -1)).toEqual(cellAt(0));
    expect(landOn(s, 0)).toEqual(linkAt(0));
  });

  it("landOn: the link under the goal, else the cell", () => {
    expect(landOn(stops, 3)).toEqual(linkAt(2));
    expect(landOn(stops, 2)).toEqual(linkAt(2));
    // End exclusive.
    expect(landOn(stops, 4)).toEqual(cellAt(4));
    expect(landOn(stops, 30)).toEqual(cellAt(30));
    expect(landOn([], 5)).toEqual(cellAt(5));
    // At a border between two links: the one starting there.
    expect(
      landOn(
        [
          { start: 0, end: 2 },
          { start: 2, end: 4 },
        ],
        2,
      ),
    ).toEqual(linkAt(2));
  });
});
