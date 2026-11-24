import { describe, expect, it } from "vitest";
import type { DiffFiles } from "../api/diffFiles";
import { fileMarks } from "../files/changes";
import type { Side } from "../sides";
import { diffHrefFor, diffPathFor } from "./link";

const files = (truncated = false): DiffFiles =>
  ({
    truncated,
    files: [
      { path: "a.hs", status: "modified" },
      { path: "new.hs", oldPath: "old.hs", status: "renamed", similarity: 90 },
    ],
  }) as unknown as DiffFiles;

function api(
  side: Side,
  f: DiffFiles | undefined,
  state: { loading?: boolean; failed?: boolean } = {},
) {
  return {
    files: () => f,
    marks: () => (f === undefined ? undefined : fileMarks(f, side)),
    loading: () => state.loading === true,
    failed: () => state.failed === true,
  };
}

describe("diffPathFor", () => {
  it("a listed file: its path", () => {
    expect(diffPathFor(api("head", files()), "head", "a.hs")).toBe("a.hs");
    expect(diffPathFor(api("base", files()), "base", "a.hs")).toBe("a.hs");
  });
  it("a rename: the head path from both sides", () => {
    expect(diffPathFor(api("head", files()), "head", "new.hs")).toBe("new.hs");
    expect(diffPathFor(api("base", files()), "base", "old.hs")).toBe("new.hs");
  });
  it("not listed: none", () => {
    expect(diffPathFor(api("head", files()), "head", "b.hs")).toBeUndefined();
  });
  it("not listed, but the list is cut, loading or failed: the path", () => {
    expect(diffPathFor(api("head", files(true)), "head", "b.hs")).toBe("b.hs");
    expect(
      diffPathFor(api("head", undefined, { loading: true }), "head", "b.hs"),
    ).toBe("b.hs");
    expect(
      diffPathFor(api("head", undefined, { failed: true }), "head", "b.hs"),
    ).toBe("b.hs");
  });
  it("browse mode: none", () => {
    expect(diffPathFor(api("tree", undefined), "tree", "a.hs")).toBeUndefined();
  });
});

describe("diffHrefFor", () => {
  it("the diff URL, with a line on a side; or none", () => {
    expect(diffHrefFor(api("head", files()), "head", "a.hs")).toBe(
      "/diff/a.hs",
    );
    expect(
      diffHrefFor(api("base", files()), "base", "old.hs", {
        side: "base",
        line: 3,
      }),
    ).toBe("/diff/new.hs#base-L3");
    expect(diffHrefFor(api("head", files()), "head", "b.hs")).toBeUndefined();
  });
});
