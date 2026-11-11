import { describe, expect, it } from "vitest";
import { highlightDecor, highlightedLine } from "./decor";
import type { Highlight } from "./highlight";

const h: Highlight = {
  runs: [[], [{ from: 1, to: 3, cls: "keyword", link: 0 }], []],
  kinds: ["code", "excluded", "directive"],
  outline: [["instance", "inst:Show:Maybe a", null, 2, 2, 2]],
  byLine: new Map([[2, ["instance", "inst:Show:Maybe a", null, 2, 2, 2]]]),
};

describe("decor", () => {
  it("highlightedLine: the line's segs as nodes", () => {
    const p = document.createElement("p");
    p.append(...highlightedLine(h)(1, "if x"));
    expect(p.querySelector(".hl-keyword")?.textContent).toBe("if");
    expect(p.textContent).toBe("if x");
  });

  it("CPP kinds and a docs marker, not a Tab stop", () => {
    const d = highlightDecor(h, (id) => `/docs#${id}`);
    expect(d(1)).toEqual({ kind: "excluded", mark: undefined });
    const two = d(2);
    expect(two?.kind).toBe("directive");
    const a = two?.mark as HTMLAnchorElement;
    expect(a.getAttribute("href")).toBe("/docs#inst:Show:Maybe a");
    expect(a.tabIndex).toBe(-1);
    expect(a.className).toBe("code-outline-mark");
    expect(a.getAttribute("aria-label")).toBe("Docs: instance Show (Maybe a)");
    expect(a.title).toBe("Docs: instance Show (Maybe a)");
    expect(a.textContent).toBe("");
  });

  it("no docs page: no marker; a plain line: nothing", () => {
    const d = highlightDecor(
      { ...h, kinds: ["code", "code", "code"] },
      () => undefined,
    );
    expect(d(2)).toBeUndefined();
    expect(d(1)).toBeUndefined();
  });
});
