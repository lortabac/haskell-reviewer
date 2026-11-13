import { describe, expect, it } from "vitest";
import { decodeRefs } from "../api/refs";
import { fixture } from "../../test/fixtures";
import { groupRefs, guessCount, summary } from "./groups";
import { snippetParts } from "./snippet";

const refs = decodeRefs(fixture("refs"));

describe("groupRefs", () => {
  it("runs of (ref, path), server order", () => {
    const g = groupRefs(refs.results);
    expect(g.map((x) => [x.ref, x.items.length])).toEqual([
      ["R.Shape", 4],
      ["R.Guess", 1],
      ["R.Use", 5],
    ]);
    expect(g[0]!.items[0]!.key).not.toBe(g[0]!.items[1]!.key);
  });

  it("the same module split by another is two groups", () => {
    const [a, b] = refs.results as [
      (typeof refs.results)[0],
      (typeof refs.results)[0],
    ];
    expect(groupRefs([a, { ...b, ref: "X" }, a]).length).toBe(3);
  });
});

describe("summary", () => {
  it("counts", () => {
    expect(summary(refs)).toBe("10 references in 3 modules.");
    expect(guessCount(refs.results)).toBe("1 is a guess.");
    expect(summary({ total: 1, results: refs.results.slice(0, 1) })).toBe(
      "1 reference in 1 module.",
    );
    expect(summary({ total: 0, results: [] })).toBe("No references found.");
  });

  it("capped", () => {
    expect(summary({ total: 4145, results: refs.results })).toBe(
      "Showing the first 10 of 4,145 references.",
    );
  });
});

describe("snippetParts", () => {
  const r = refs.results[1]!;
  it("a whole line loses its indent", () => {
    expect(snippetParts(r)).toEqual({
      cut: false,
      before: "",
      hit: "area",
      after: " :: a -> Double",
    });
  });
  it("a cut line starts with the ellipsis, keeps its text", () => {
    const s = snippetParts({
      ...r,
      line: "  xx🙂area yy",
      lineCol: 5,
      span: [3, 10, 3, 14],
    });
    expect(s).toEqual({
      cut: true,
      before: "  xx🙂",
      hit: "area",
      after: " yy",
    });
  });
  it("a token past the line's end is cut", () => {
    const s = snippetParts({
      ...r,
      line: "ab",
      span: [1, 2, 2, 9],
      lineCol: 1,
    });
    expect(s.hit).toBe("b");
  });
});
