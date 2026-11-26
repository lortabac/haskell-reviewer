import { createRoot } from "solid-js";
import { describe, expect, it } from "vitest";
import { createGenerationState, type GenerationState } from "./generation";

function make(): GenerationState {
  return createRoot(() => createGenerationState());
}

describe("createGenerationState (D-354)", () => {
  it("the first value is the baseline: no apply", () => {
    const g = make();
    expect(g.current()).toBeUndefined();
    g.seen(3);
    expect(g.current()).toBe(3);
    expect(g.applied()).toBeUndefined();
    g.seen(3);
    expect(g.applied()).toBeUndefined();
  });

  it("a rise is applied with a notice, once per value", () => {
    const g = make();
    g.seen(1);
    g.seen(2);
    expect(g.current()).toBe(2);
    expect(g.applied()).toEqual({ n: 1, notice: true });
    g.seen(2);
    expect(g.applied()).toEqual({ n: 1, notice: true });
    g.seen(4);
    expect(g.current()).toBe(4);
    expect(g.applied()).toEqual({ n: 2, notice: true });
  });

  it("an older answer: one silent apply for that generation", () => {
    const g = make();
    // A new tab: the worker replays 4 before the page's answer of 3.
    g.seen(4);
    g.seen(3);
    expect(g.current()).toBe(4);
    expect(g.applied()).toEqual({ n: 1, notice: false });
    g.seen(3);
    g.seen(2);
    expect(g.applied()).toEqual({ n: 1, notice: false });
  });

  it("a change with no baseline is applied (a failed start-up listing)", () => {
    const g = make();
    g.seen(2, true);
    expect(g.current()).toBe(2);
    expect(g.applied()).toEqual({ n: 1, notice: true });
    // The event of the same reload: nothing more.
    g.seen(2, true);
    expect(g.applied()).toEqual({ n: 1, notice: true });
  });

  it("an older answer after a rise: nothing more", () => {
    const g = make();
    g.seen(1);
    g.seen(2);
    // The old page's late answer.
    g.seen(1);
    expect(g.applied()).toEqual({ n: 1, notice: true });
    // The next rise.
    g.seen(3);
    g.seen(2);
    expect(g.applied()).toEqual({ n: 2, notice: true });
  });
});
