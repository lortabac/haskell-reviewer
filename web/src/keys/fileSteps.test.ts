import { describe, expect, it } from "vitest";
import { stepIndex } from "./fileSteps";

describe("stepIndex", () => {
  // 10 20 30
  const xs = [10, 20, 30];
  const from = (x: number, dir: 1 | -1) =>
    stepIndex(xs.length, (i) => (xs[i] ?? 0) - x, dir);

  it("from a listed item: its neighbours, no wrap", () => {
    expect(from(10, 1)).toBe(1);
    expect(from(10, -1)).toBeUndefined();
    expect(from(30, 1)).toBeUndefined();
    expect(from(30, -1)).toBe(1);
  });

  it("from a place not listed: the items around it", () => {
    expect(from(15, 1)).toBe(1);
    expect(from(15, -1)).toBe(0);
    expect(from(5, -1)).toBeUndefined();
    expect(from(35, 1)).toBeUndefined();
  });

  it("an empty list: nothing", () => {
    expect(stepIndex(0, () => 0, 1)).toBeUndefined();
    expect(stepIndex(0, () => 0, -1)).toBeUndefined();
  });
});
