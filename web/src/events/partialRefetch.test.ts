import { describe, expect, it } from "vitest";
import { refetchStep, refetchThrottleMs } from "./partialRefetch";

const at = (generation: number, ready: number) => ({
  generation,
  total: 10,
  ready,
  failed: 0,
});

describe("refetchStep", () => {
  it("another snapshot: older or newer", () => {
    expect(refetchStep(at(1, 3), 2, 0, 5000)).toBe("older");
    expect(refetchStep(at(3, 3), 2, 0, 5000)).toBe("newer");
  });

  it("done: whatever the time", () => {
    expect(refetchStep(at(2, 10), 2, 1000, 1001)).toBe("done");
    expect(refetchStep({ ...at(2, 9), failed: 1 }, 2, 0, 0)).toBe("done");
  });

  it("throttled within the window, due after it", () => {
    expect(refetchStep(at(2, 3), 2, 1000, 1000 + refetchThrottleMs - 1)).toBe(
      "throttled",
    );
    expect(refetchStep(at(2, 3), 2, 1000, 1000 + refetchThrottleMs)).toBe(
      "due",
    );
  });

  it("no answer yet: any snapshot is ours", () => {
    expect(refetchStep(at(7, 3), undefined, 0, 5000)).toBe("due");
  });
});
