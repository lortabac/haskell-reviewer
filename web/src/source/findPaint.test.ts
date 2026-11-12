import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { canPaint, createPaint } from "./findPaint";

class FakeHighlight extends Set<Range> {
  priority = 0;
}

let registry: Map<string, FakeHighlight>;

beforeEach(() => {
  registry = new Map();
  vi.stubGlobal("Highlight", FakeHighlight);
  vi.stubGlobal("CSS", { highlights: registry });
});
afterEach(() => vi.unstubAllGlobals());

describe("findPaint", () => {
  it("does nothing without the API", () => {
    vi.unstubAllGlobals();
    expect(canPaint()).toBe(false);
    createPaint().set([document.createRange()], undefined);
  });

  it("sets and clears the ranges of one view", () => {
    const a = document.createRange();
    const b = document.createRange();
    const c = document.createRange();
    const one = createPaint();
    one.set([a, b], c);
    expect(registry.get("code-find")?.size).toBe(2);
    expect(registry.get("code-find-current")?.has(c)).toBe(true);
    expect(registry.get("code-find-current")?.priority).toBe(1);
    one.set([a], undefined);
    expect(registry.get("code-find")?.size).toBe(1);
    expect(registry.has("code-find-current")).toBe(false);
    one.clear();
    expect(registry.size).toBe(0);
  });

  it("keeps the ranges of another view", () => {
    const a = document.createRange();
    const b = document.createRange();
    const one = createPaint();
    const two = createPaint();
    one.set([a], undefined);
    two.set([b], undefined);
    one.clear();
    expect(registry.get("code-find")?.has(b)).toBe(true);
    expect(registry.get("code-find")?.has(a)).toBe(false);
  });
});
