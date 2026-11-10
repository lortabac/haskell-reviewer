import { describe, expect, it } from "vitest";
import { withPins } from "./virtual";

const range = { startIndex: 50, endIndex: 52, overscan: 2, count: 100 };

describe("withPins", () => {
  it("no pins: the window with overscan", () => {
    expect(withPins(range, [])).toEqual([48, 49, 50, 51, 52, 53, 54]);
  });

  it("pins before, inside and after, sorted, once", () => {
    expect(withPins(range, [99, 3, 50, 3])).toEqual([
      3, 48, 49, 50, 51, 52, 53, 54, 99,
    ]);
  });

  it("drops pins out of the file", () => {
    expect(withPins(range, [-1, 100, 1.5, NaN])).toEqual([
      48, 49, 50, 51, 52, 53, 54,
    ]);
  });
});
