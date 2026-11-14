import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { stepSection } from "./docsKeys";

// docs.css gives sections a 16px scroll margin.
beforeEach(() => {
  const real = window.getComputedStyle.bind(window);
  vi.spyOn(window, "getComputedStyle").mockImplementation((el) => {
    const st = real(el);
    return el instanceof HTMLElement && el.matches("section.decl")
      ? { ...st, scrollMarginTop: "16px" }
      : st;
  });
});

afterEach(() => {
  vi.restoreAllMocks();
  document.body.replaceChildren();
});

// Sections at these tops on screen, with a 16px scroll margin.
function page(tops: number[]): { root: HTMLElement; secs: HTMLElement[] } {
  const root = document.createElement("div");
  const secs = tops.map((top) => {
    const s = document.createElement("section");
    s.className = "decl";
    s.tabIndex = -1;
    s.getBoundingClientRect = () => ({ top, bottom: top + 100 }) as DOMRect;
    s.scrollIntoView = () => undefined;
    root.append(s);
    return s;
  });
  document.body.append(root);
  return { root, secs };
}

describe("stepSection, no focus", () => {
  it("a section at its scroll margin: j goes past it, k before it", () => {
    const { root, secs } = page([-200, 16, 300]);
    expect(stepSection(root, 1)).toBe(true);
    expect(document.activeElement).toBe(secs[2]);
    document.body.focus();
    (document.activeElement as HTMLElement | null)?.blur();
    expect(stepSection(root, -1)).toBe(true);
    expect(document.activeElement).toBe(secs[0]);
  });

  it("from the top of the page: j is the first one below", () => {
    const { root, secs } = page([120, 400]);
    stepSection(root, 1);
    expect(document.activeElement).toBe(secs[0]);
  });

  it("with focus: the next and the previous", () => {
    const { root, secs } = page([16, 300, 600]);
    secs[1]?.focus();
    stepSection(root, 1);
    expect(document.activeElement).toBe(secs[2]);
    stepSection(root, -1);
    stepSection(root, -1);
    expect(document.activeElement).toBe(secs[0]);
    expect(stepSection(root, -1)).toBe(false);
  });
});
