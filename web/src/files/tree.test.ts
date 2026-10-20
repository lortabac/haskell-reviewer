import { describe, expect, it } from "vitest";
import type { Entry } from "../api/files";
import { ancestors, buildTree, compareCodePoints, type DirNode } from "./tree";

function file(path: string): Entry {
  return { path, kind: "file" };
}

// The tree as nested names, for easy compare.
function shape(node: DirNode): unknown[] {
  return [
    ...node.dirs.map((d) => ({ [d.name + "/"]: shape(d) })),
    ...node.files.map((f) => f.name),
  ];
}

describe("buildTree", () => {
  it("nests, folders first, code point order", () => {
    const tree = buildTree(
      [
        "z.hs",
        "a-c/x",
        "a/b/c.hs",
        "a/a.hs",
        "B.hs",
        "a/b/d.hs",
        "\u{1F600}.hs",
        "�.hs",
      ].map(file),
    );
    expect(shape(tree)).toEqual([
      { "a/": [{ "b/": ["c.hs", "d.hs"] }, "a.hs"] },
      { "a-c/": ["x"] },
      "B.hs",
      "z.hs",
      "�.hs",
      "\u{1F600}.hs",
    ]);
    expect(tree.dirs[0]?.dirs[0]?.path).toBe("a/b");
  });

  it("keeps the entry", () => {
    const sub: Entry = { path: "v/dep", kind: "submodule", commit: null };
    expect(buildTree([sub]).dirs[0]?.files[0]?.entry).toBe(sub);
  });

  it("empty list", () => {
    expect(buildTree([])).toEqual({ name: "", path: "", dirs: [], files: [] });
  });
});

describe("compareCodePoints", () => {
  it("orders by code point, not UTF-16 unit", () => {
    // U+1F600 is a surrogate pair (0xD83D ...), below U+FFFD in UTF-16.
    expect(compareCodePoints("�", "\u{1F600}")).toBeLessThan(0);
    expect(compareCodePoints("a", "ab")).toBeLessThan(0);
    expect(compareCodePoints("b", "a")).toBeGreaterThan(0);
    expect(compareCodePoints("x", "x")).toBe(0);
  });
});

describe("ancestors", () => {
  it("lists the folders", () => {
    expect(ancestors("a/b/c.hs")).toEqual(["a", "a/b"]);
    expect(ancestors("c.hs")).toEqual([]);
  });
});
