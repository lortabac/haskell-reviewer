import { describe, expect, it } from "vitest";
import { isSelectAll, keyTarget, type Mods } from "./keys";

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
