import { describe, expect, it } from "vitest";
import type { Entry } from "../api/files";
import {
  ancestors,
  buildTree,
  compareCodePoints,
  compareTreePaths,
  type DirNode,
  sortTreePaths,
} from "./tree";

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

// A small seeded generator (mulberry32).
function rng(seed: number): () => number {
  let a = seed;
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// The old code point compare, by iterator: the reference.
function refCompare(a: string, b: string): number {
  const ia = a[Symbol.iterator]();
  const ib = b[Symbol.iterator]();
  for (;;) {
    const ca = ia.next();
    const cb = ib.next();
    if (ca.done === true) return cb.done === true ? 0 : -1;
    if (cb.done === true) return 1;
    const d = (ca.value.codePointAt(0) ?? 0) - (cb.value.codePointAt(0) ?? 0);
    if (d !== 0) return d;
  }
}

// The old tree: each folder sorts its own folders and files by name.
// The reference for buildTree's order.
function refOrder(paths: readonly string[]): string[] {
  interface Node {
    dirs: Map<string, Node>;
    files: string[];
  }
  const root: Node = { dirs: new Map(), files: [] };
  for (const p of paths) {
    const parts = p.split("/");
    parts.pop();
    let n = root;
    for (const part of parts) {
      let c = n.dirs.get(part);
      if (c === undefined) {
        c = { dirs: new Map(), files: [] };
        n.dirs.set(part, c);
      }
      n = c;
    }
    n.files.push(p);
  }
  const last = (p: string) => p.split("/").at(-1) ?? "";
  const walk = (n: Node): string[] => [
    ...[...n.dirs.entries()]
      .sort(([a], [b]) => refCompare(a, b))
      .flatMap(([, c]) => walk(c)),
    ...n.files.sort((a, b) => refCompare(last(a), last(b))),
  ];
  return walk(root);
}

const sign = (n: number) => Math.sign(n);

// Few names, so paths share folders and a name is both a file and a
// folder at times. Some are above U+FFFF, some in E000-FFFF.
const names = [
  "a",
  "b",
  "B",
  "a-c",
  "a.hs",
  "\u{1F600}",
  "\uFFFD",
  "\uE000",
  "z",
];

function randomPaths(next: () => number): string[] {
  const out = new Set<string>();
  const n = Math.floor(next() * 30);
  for (let i = 0; i < n; i++) {
    const depth = 1 + Math.floor(next() * 4);
    const parts = Array.from(
      { length: depth },
      () => names[Math.floor(next() * names.length)] ?? "a",
    );
    out.add(parts.join("/"));
  }
  return [...out];
}

function flatten(node: DirNode): string[] {
  return [
    ...node.dirs.flatMap(flatten),
    ...node.files.map((f) => f.entry.path),
  ];
}

describe("tree order (D-350)", () => {
  it("compareCodePoints agrees with code point order (random strings)", () => {
    const next = rng(3);
    const chars = [
      "a",
      "z",
      "\u00e9",
      "\uD7FF",
      "\uE000",
      "\uFFFD",
      "\u{10000}",
      "\u{1F600}",
    ];
    const word = () =>
      Array.from(
        { length: Math.floor(next() * 4) },
        () => chars[Math.floor(next() * chars.length)] ?? "a",
      ).join("");
    for (let i = 0; i < 5000; i++) {
      const a = word();
      const b = word();
      expect(sign(compareCodePoints(a, b))).toBe(sign(refCompare(a, b)));
    }
  });

  it("buildTree and sortTreePaths match the old per-folder sort", () => {
    const next = rng(7);
    for (let run = 0; run < 2000; run++) {
      const paths = randomPaths(next);
      const want = refOrder(paths);
      const tree = buildTree(paths.map((path) => ({ path, kind: "file" })));
      expect(flatten(tree)).toEqual(want);
      expect(sortTreePaths([...paths].reverse())).toEqual(want);
    }
  });

  it("folders first, then code point order", () => {
    expect(compareTreePaths("a/x", "a.hs")).toBeLessThan(0);
    expect(compareTreePaths("z/x", "a")).toBeLessThan(0);
    expect(compareTreePaths("a", "b")).toBeLessThan(0);
    expect(compareTreePaths("\uFFFD", "\u{1F600}")).toBeLessThan(0);
    expect(compareTreePaths("a/b", "a/b")).toBe(0);
  });
});
