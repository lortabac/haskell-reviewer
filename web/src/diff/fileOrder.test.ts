import { describe, expect, it } from "vitest";
import type { ChangedFile, DiffFiles, FileStatus } from "../api/diffFiles";
import { fileMarks } from "../files/changes";
import { sortTreePaths } from "../files/tree";
import { diffOrder, fileStep, sourceOrder, stepPath } from "./fileOrder";

describe("stepPath", () => {
  const paths = sortTreePaths(["src/A.hs", "src/C.hs", "x.txt"]);

  it("from a listed path: the next and previous, no wrap", () => {
    expect(stepPath(paths, "src/A.hs", 1)).toBe(1);
    expect(stepPath(paths, "src/A.hs", -1)).toBeUndefined();
    expect(stepPath(paths, "x.txt", 1)).toBeUndefined();
    expect(stepPath(paths, "x.txt", -1)).toBe(1);
  });

  it("from a path not listed: its place in the order", () => {
    expect(stepPath(paths, "src/B.hs", 1)).toBe(1);
    expect(stepPath(paths, "src/B.hs", -1)).toBe(0);
    expect(stepPath(paths, "a.txt", 1)).toBe(2);
    expect(stepPath(paths, "a.txt", -1)).toBe(1);
    expect(stepPath(paths, "src/sub/D.hs", 1)).toBe(0);
  });

  it("an empty list: nothing", () => {
    expect(stepPath([], "a", 1)).toBeUndefined();
    expect(stepPath([], "a", -1)).toBeUndefined();
  });
});

const change = (
  path: string,
  status: FileStatus,
  oldPath: string | null = null,
): ChangedFile => ({
  path,
  oldPath,
  status,
  similarity: oldPath === null ? null : 90,
  note: null,
});

// Like marks.sh.
const files: DiffFiles = {
  generation: 1,
  base: { label: "main", commit: null },
  head: { label: "feature", commit: null },
  files: [
    change("added.txt", "added"),
    change("eol.txt", "modified"),
    change("ren/New.hs", "renamed", "ren/Old.hs"),
    change("removed.txt", "removed"),
    change("src/Marks.hs", "modified"),
  ],
  outside: null,
  renameLimitHit: false,
  truncated: false,
};

describe("the orders per view", () => {
  it("diff: every file at its head path, removed too; made once", () => {
    const o = diffOrder(files);
    expect(o.paths).toEqual([
      "ren/New.hs",
      "src/Marks.hs",
      "added.txt",
      "eol.txt",
      "removed.txt",
    ]);
    expect(o.heads.get("ren/Old.hs")).toBe("ren/New.hs");
    expect(diffOrder(files)).toBe(o);
  });

  it("source: this side's own files, no ghosts", () => {
    expect(sourceOrder(fileMarks(files, "head"))).toEqual([
      "ren/New.hs",
      "src/Marks.hs",
      "added.txt",
      "eol.txt",
    ]);
    const base = fileMarks(files, "base");
    expect(sourceOrder(base)).toEqual([
      "ren/Old.hs",
      "src/Marks.hs",
      "eol.txt",
      "removed.txt",
    ]);
    expect(sourceOrder(base)).toBe(sourceOrder(base));
  });
});

describe("fileStep", () => {
  const paths = diffOrder(files).paths;
  const href = (p: string) => `/diff/${p}`;

  it("the file and its words", () => {
    expect(fileStep(paths, "src/Marks.hs", 1, href)).toEqual({
      href: "/diff/added.txt",
      words: "added.txt, changed file 3 of 5.",
    });
    expect(fileStep(paths, "src/Marks.hs", -1, href)).toEqual({
      href: "/diff/ren/New.hs",
      words: "ren/New.hs, changed file 1 of 5.",
    });
  });

  it("the ends and an empty list", () => {
    expect(fileStep(paths, "removed.txt", 1, href)).toBe(
      "No more changed files.",
    );
    expect(fileStep(paths, "ren/New.hs", -1, href)).toBe(
      "No more changed files.",
    );
    expect(fileStep([], "a", 1, href)).toBe("No changed files.");
  });
});
