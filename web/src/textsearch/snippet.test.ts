import { describe, expect, it } from "vitest";
import { fixture } from "../../test/fixtures";
import { decodeTextSearch, type TextHit } from "../api/textSearch";
import {
  groupHits,
  hitState,
  skippedText,
  textSnippet,
  textSummary,
} from "./snippet";

const hit = (
  text: string,
  matches: [number, number][],
  lineCol = 1,
): TextHit => ({
  path: "a.hs",
  line: 3,
  lineCol,
  text,
  matches,
});

const show = (h: TextHit, lead?: number) => {
  const s = textSnippet(h, lead);
  return (
    (s.cut ? "…" : "") +
    s.parts.map((p) => (p.hit ? `[${p.text}]` : p.text)).join("")
  );
};

describe("textSnippet", () => {
  it("marks every hit; a whole line loses its indent", () => {
    expect(
      show(
        hit("  main = main", [
          [3, 7],
          [10, 14],
        ]),
      ),
    ).toBe("[main] = [main]");
  });

  it("a cut line keeps its start and gets …", () => {
    expect(show(hit("  x main", [[184, 188]], 180))).toBe("…  x [main]");
  });

  it("columns are code points: astral and non-ASCII text", () => {
    expect(
      show(
        hit("𝑥 naïve ü", [
          [3, 8],
          [9, 10],
        ]),
      ),
    ).toBe("𝑥 [naïve] [ü]");
  });

  it("a hit past the cut is cut or dropped", () => {
    expect(
      show(
        hit("abc", [
          [2, 9],
          [20, 22],
        ]),
      ),
    ).toBe("a[bc]");
  });

  it("lead: start a little before the first hit", () => {
    const text = `${"x".repeat(50)}main`;
    expect(show(hit(text, [[51, 55]]), 5)).toBe("…xxxxx[main]");
    // Close to the start: no cut.
    expect(show(hit("  ab main", [[6, 10]]), 5)).toBe("ab [main]");
  });

  it("a hit in the indent still shows", () => {
    expect(show(hit("    x", [[1, 3]]))).toBe("[  ]  x");
  });
});

describe("text search words", () => {
  const all = decodeTextSearch(fixture("text-search"));
  const capped = decodeTextSearch(fixture("text-search-capped"));

  it("summary, capped and none", () => {
    expect(textSummary(all)).toBe(
      `${all.total} matching lines in ${all.files} files.`,
    );
    expect(textSummary(capped)).toMatch(
      /^Showing the first [\d,]+ of [\d,]+ matching lines\.$/,
    );
    expect(textSummary({ total: 0, files: 0, results: [] })).toBe(
      "No matches.",
    );
    expect(
      textSummary({ total: 1, files: 1, results: [hit("main", [[1, 5]])] }),
    ).toBe("1 matching line in 1 file.");
  });

  it("skipped", () => {
    expect(
      skippedText({ binary: 0, tooLarge: 0, unreadable: 0, overBudget: 0 }),
    ).toBe("");
    expect(
      skippedText({ binary: 2, tooLarge: 1, unreadable: 0, overBudget: 0 }),
    ).toBe("3 files not searched: 2 binary, 1 too large.");
    expect(
      skippedText({ binary: 0, tooLarge: 0, unreadable: 1, overBudget: 1200 }),
    ).toBe(
      "1,201 files not searched: 1 unreadable, 1,200 over the memory limit.",
    );
  });

  it("groups are runs of a path", () => {
    const g = groupHits(all.results);
    expect(g.map((x) => x.path)).toEqual([
      ...new Set(all.results.map((r) => r.path)),
    ]);
    expect(g.reduce((n, x) => n + x.hits.length, 0)).toBe(all.results.length);
  });

  it("the landing state is the first hit", () => {
    expect(
      hitState(
        hit("main main", [
          [1, 5],
          [6, 10],
        ]),
      ),
    ).toEqual({
      codeJump: true,
      cp: 1,
      len: 4,
    });
  });
});
