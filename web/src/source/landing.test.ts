import { describe, expect, it } from "vitest";
import { shouldReland } from "./landing";

describe("shouldReland", () => {
  const land = { line: 16, col: 24 };
  it("same place, focus in the view", () => {
    expect(shouldReland(land, { line: 16, col: 24 }, true)).toBe(true);
  });
  it("skipped once the cursor moved or focus left", () => {
    expect(shouldReland(land, { line: 16, col: 30 }, true)).toBe(false);
    expect(shouldReland(land, { line: 17, col: 24 }, true)).toBe(false);
    expect(shouldReland(land, { line: 16, col: 24 }, false)).toBe(false);
    expect(shouldReland(land, undefined, true)).toBe(false);
  });
});
