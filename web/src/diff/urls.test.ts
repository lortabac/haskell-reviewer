import { describe, expect, it } from "vitest";
import {
  diffHref,
  diffPathFromPathname,
  parseDiffHash,
  wsFromSearch,
} from "./urls";

describe("diff URLs", () => {
  it.each(["a.hs", "src/A/B.hs", "a#b/c?d.hs", "100%/x.hs", "héllo/日本.hs"])(
    "round trip: %s",
    (path) => {
      const href = diffHref(path);
      expect(href.startsWith("/diff/")).toBe(true);
      expect(href).not.toMatch(/[#? ]/);
      expect(diffPathFromPathname("/tok" + href)).toBe(path);
    },
  );

  it("ws and a line", () => {
    expect(diffHref("a b.hs", { ws: true })).toBe("/diff/a%20b.hs?ws=ignore");
    expect(diffHref("a.hs", { at: { side: "base", line: 7 } })).toBe(
      "/diff/a.hs#base-L7",
    );
    expect(diffHref("a.hs", { ws: true, at: { side: "head", line: 1 } })).toBe(
      "/diff/a.hs?ws=ignore#head-L1",
    );
  });

  it("no path, or another view: undefined", () => {
    expect(diffPathFromPathname("/tok/diff")).toBeUndefined();
    expect(diffPathFromPathname("/tok/diff/")).toBeUndefined();
    expect(diffPathFromPathname("/tok/src/head/a.hs")).toBeUndefined();
    expect(diffPathFromPathname("/tok/diff/a/../b")).toBeUndefined();
  });

  it("parses one line on one side", () => {
    expect(parseDiffHash("#base-L3")).toEqual({ side: "base", line: 3 });
    expect(parseDiffHash("#head-L120")).toEqual({ side: "head", line: 120 });
    for (const h of [
      "",
      "#L3",
      "#tree-L3",
      "#head-L0",
      "#head-L3-L4",
      "#head-3",
    ]) {
      expect(parseDiffHash(h), h).toBeUndefined();
    }
  });

  it("ws flag", () => {
    expect(wsFromSearch("?ws=ignore")).toBe(true);
    expect(wsFromSearch("?ws=other")).toBe(false);
    expect(wsFromSearch("")).toBe(false);
  });
});
