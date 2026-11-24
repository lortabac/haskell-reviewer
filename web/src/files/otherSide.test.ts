import { describe, expect, it } from "vitest";
import { rowsOf } from "../diff/rows";
import { sideChanges } from "../source/diffMarks";
import type { FileMark } from "./changes";
import { cantMap, markdownTarget, otherFile, sourceTarget } from "./otherSide";
import type { ChangesState } from "./sourceChanges";

// marks.sh's src/Marks.hs.
const rows = rowsOf([
  ["delete", 2],
  ["equal", 3],
  ["insert", 2],
  ["equal", 1],
  ["delete", 1],
  ["insert", 2],
  ["equal", 3],
  ["delete", 2],
]);
const lines: ChangesState = {
  kind: "lines",
  rows,
  changes: sideChanges(rows, "head"),
};
const modified: FileMark = { status: "modified" };

function at(
  over: Partial<Parameters<typeof otherFile>[0]> = {},
): ReturnType<typeof otherFile> {
  return otherFile({
    side: "head",
    path: "src/Marks.hs",
    mark: modified,
    list: "trusted",
    state: lines,
    line: 9,
    ...over,
  });
}

describe("otherFile", () => {
  it("an equal line: the same code, no words", () => {
    expect(at()).toEqual({ kind: "go", path: "src/Marks.hs", line: 8 });
  });

  it("in an insertion, or a replaced line: words", () => {
    expect(at({ line: 4 })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      line: 5,
      words: "Line 4 isn't on base: nearest line 5.",
    });
    expect(at({ line: 7 })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      line: 7,
      words: "Line 7 changed: base line 7 shown.",
    });
  });

  it("added on head, removed on base, empty: off", () => {
    expect(at({ mark: { status: "added" } })).toEqual({
      kind: "off",
      why: "Only on head.",
    });
    expect(at({ side: "base", mark: { status: "removed" } })).toEqual({
      kind: "off",
      why: "Only on base.",
    });
    expect(
      at({ mark: undefined, state: { kind: "empty-file", added: true } }),
    ).toEqual({ kind: "off", why: "Only on head." });
  });

  it("loading: off, and says so", () => {
    expect(at({ list: "loading" })).toEqual({
      kind: "off",
      why: "Loading changes…",
    });
    expect(at({ state: { kind: "loading" } })).toEqual({
      kind: "off",
      why: "Loading changes…",
    });
  });

  it("renamed: the other path", () => {
    const mark: FileMark = {
      status: "renamed",
      other: "ren/Old.hs",
      similarity: 90,
    };
    const r = rowsOf([
      ["equal", 2],
      ["delete", 1],
      ["insert", 1],
      ["equal", 5],
    ]);
    expect(
      at({
        path: "ren/New.hs",
        mark,
        line: 5,
        state: { kind: "lines", rows: r, changes: sideChanges(r, "head") },
      }),
    ).toEqual({ kind: "go", path: "ren/Old.hs", line: 5 });
  });

  it("unlisted and the list is trusted: same line, whatever the state", () => {
    expect(at({ mark: undefined, state: { kind: "unchanged" } })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      line: 9,
    });
    expect(at({ mark: undefined, state: { kind: "loading" } })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      line: 9,
    });
  });

  it("no rows: the file's top, and why", () => {
    expect(at({ state: { kind: "slow" } })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      words: cantMap.slow,
    });
    expect(at({ state: { kind: "not-text" } })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
      words: cantMap.notText,
    });
    // Unlisted in a cut list, and the fetch failed: the same path.
    expect(
      at({ mark: undefined, list: "partial", state: { kind: "error" } }),
    ).toEqual({ kind: "go", path: "src/Marks.hs", words: cantMap.error });
  });

  it("no lines on the page, or no cursor: the path only", () => {
    expect(at({ state: undefined, line: undefined })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
    });
    expect(at({ line: undefined })).toEqual({
      kind: "go",
      path: "src/Marks.hs",
    });
  });
});

describe("sourceTarget, markdownTarget", () => {
  it("top for the header; the exact place only on use", () => {
    const leave = () => undefined;
    let used = 0;
    const t = sourceTarget(
      { kind: "go", path: "a b.hs" },
      "head",
      () => {
        used++;
        return { kind: "go", path: "a b.hs", line: 3, words: "W." };
      },
      leave,
    );
    expect(used).toBe(0);
    if (t.kind !== "go") throw new Error(t.kind);
    expect(t.side).toBe("base");
    expect(t.top).toBe("/src/base/a%20b.hs");
    expect(t.leave).toBe(leave);
    expect(t.at?.()).toEqual({
      href: "/src/base/a%20b.hs#L3",
      state: { codeJump: true },
      words: "W.",
    });
    expect(used).toBe(1);
    expect(sourceTarget({ kind: "off", why: "X." }, "base")).toEqual({
      kind: "off",
      side: "head",
      why: "X.",
    });
  });

  it("the top when there is no line", () => {
    const t = sourceTarget({ kind: "go", path: "a.hs", words: "W." }, "head");
    if (t.kind !== "go") throw new Error(t.kind);
    expect(t.at?.()).toEqual({ href: "/src/base/a.hs", words: "W." });
  });

  it("Markdown keeps the hash", () => {
    const t = markdownTarget(
      { kind: "go", path: "README.md" },
      "head",
      () => "#intro",
    );
    if (t.kind !== "go") throw new Error(t.kind);
    expect(t.top).toBe("/md/base/README.md");
    expect(t.at?.()).toEqual({ href: "/md/base/README.md#intro" });
  });
});
