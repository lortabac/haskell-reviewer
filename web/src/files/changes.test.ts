import { describe, expect, it } from "vitest";
import { fixture } from "../../test/fixtures";
import { decodeDiffFiles } from "../api/diffFiles";
import type { Entry } from "../api/files";
import {
  changedFilter,
  type FileMark,
  fileMarks,
  renameText,
  unionEntries,
} from "./changes";
import { buildTree, type DirNode, type GhostEntry } from "./tree";

const diff = decodeDiffFiles(fixture("diff-files"));

const file = (path: string): Entry => ({ path, kind: "file" });

const ghost = (path: string): GhostEntry => ({
  path,
  kind: "ghost",
  side: "base",
});

// Every path in the tree, folders with a "/".
function paths(n: DirNode): string[] {
  return [
    ...n.dirs.flatMap((d) => [d.path + "/", ...paths(d)]),
    ...n.files.map((f) => f.entry.path),
  ];
}

describe("fileMarks", () => {
  it("head: added, modified and renamed by path; removed is a ghost", () => {
    const m = fileMarks(diff, "head");
    expect(m.marks.get("copy/dup.txt")?.status).toBe("added");
    expect(m.marks.get("src/A.hs")?.status).toBe("modified");
    expect(m.marks.get("exact/b.txt")).toEqual({
      status: "renamed",
      other: "exact/a.txt",
      similarity: 100,
    });
    expect(m.marks.has("exact/a.txt")).toBe(false);
    expect(m.marks.has("sim40/old.txt")).toBe(false);
    expect(m.ghosts).toEqual([
      { path: "sim40/old.txt", kind: "ghost", side: "base" },
    ]);
  });

  it("base: renames by their old path; added is a ghost", () => {
    const m = fileMarks(diff, "base");
    expect(m.marks.get("sim40/old.txt")?.status).toBe("removed");
    expect(m.marks.get("src/A.hs")?.status).toBe("modified");
    expect(m.marks.get("exact/a.txt")).toEqual({
      status: "renamed",
      other: "exact/b.txt",
      similarity: 100,
    });
    expect(m.marks.has("exact/b.txt")).toBe(false);
    expect(m.ghosts).toEqual([
      { path: "copy/dup.txt", kind: "ghost", side: "head" },
    ]);
  });

  it("tree: nothing", () => {
    const m = fileMarks(diff, "tree");
    expect(m.marks.size).toBe(0);
    expect(m.ghosts).toEqual([]);
  });
});

describe("renameText", () => {
  const text = (m: FileMark | undefined, side: "base" | "head") =>
    m?.status === "renamed" ? renameText(m, side) : undefined;

  it("from on head, to on base, with the percent", () => {
    const head = fileMarks(diff, "head").marks.get("run/b.sh");
    const base = fileMarks(diff, "base").marks.get("run/a.sh");
    expect(text(head, "head")).toBe("renamed from run/a.sh (75%)");
    expect(text(base, "base")).toBe("renamed to run/b.sh (75%)");
  });
});

describe("unionEntries", () => {
  it("adds the ghosts, but not at a path the side has", () => {
    const u = unionEntries([file("a"), file("c")], [ghost("b"), ghost("c")]);
    expect(u).toEqual([file("a"), file("c"), ghost("b")]);
  });

  it("ghosts sort with the rest in the tree", () => {
    const t = buildTree(
      unionEntries([file("d/a"), file("d/c")], [ghost("d/b"), ghost("e/x")]),
    );
    expect(paths(t)).toEqual(["d/", "d/a", "d/b", "d/c", "e/", "e/x"]);
  });
});

describe("changedFilter", () => {
  const t = buildTree([
    ...["a/b/c.txt", "a/b/d.txt", "a/e.txt", "f/g.txt", "top.txt"].map(file),
    ghost("h/gone.txt"),
  ]);
  const marks = new Map<string, FileMark>([
    ["a/b/d.txt", { status: "modified" }],
    ["top.txt", { status: "added" }],
  ]);

  it("keeps marked files and ghosts, and the folders above them", () => {
    const f = changedFilter(t, marks);
    expect([...f.dirs].sort()).toEqual(["", "a", "a/b", "h"]);
    expect(f.count).toBe(3);
    const kept = (n: DirNode): string[] => [
      ...n.dirs.flatMap(kept),
      ...n.files.filter(f.keep).map((x) => x.entry.path),
    ];
    expect(kept(t)).toEqual(["a/b/d.txt", "h/gone.txt", "top.txt"]);
  });

  it("nothing marked: nothing kept", () => {
    const f = changedFilter(buildTree([file("x")]), new Map());
    expect(f.count).toBe(0);
    expect(f.dirs.size).toBe(0);
  });
});
