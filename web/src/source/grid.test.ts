import { describe, expect, it } from "vitest";
import { revealX } from "./grid";

describe("revealX", () => {
  it("in view: no scroll", () => {
    expect(revealX({ left: 50, right: 80 }, 40, 100)).toBe(0);
  });

  it("under the gutter: left", () => {
    expect(revealX({ left: 30, right: 60 }, 40, 100)).toBe(-10);
  });

  it("past the right edge: right, keeping its start in view", () => {
    expect(revealX({ left: 90, right: 120 }, 40, 100)).toBe(20);
    expect(revealX({ left: 60, right: 200 }, 40, 100)).toBe(20);
  });

  it("whole px: the end fully in, the start never under the gutter", () => {
    expect(revealX({ left: 39.5, right: 60 }, 40, 100)).toBe(-1);
    expect(revealX({ left: 90, right: 100.2 }, 40, 100)).toBe(1);
    // Wider than the room: the start term wins, rounded down.
    expect(revealX({ left: 60.5, right: 200 }, 40, 100)).toBe(20);
  });
});
