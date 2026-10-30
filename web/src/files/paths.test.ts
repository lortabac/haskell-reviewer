import { describe, expect, it } from "vitest";
import {
  apiFileUrl,
  hrefFor,
  lineHref,
  parseLineHash,
  pathFromPathname,
  segmentsAfter,
  withBase,
} from "./paths";

describe("file paths in URLs", () => {
  it.each([
    "a.hs",
    "src/A/B.hs",
    "with space/x y.hs",
    "a#b/c?d.hs",
    "100%/x%2Fy.hs",
    "héllo/日本.hs",
    "emoji/😀.hs",
    "a+b/c&d=e;f:g@h.hs",
  ])("round trip: %s", (path) => {
    const href = hrefFor(path);
    expect(href.startsWith("/src/tree/")).toBe(true);
    expect(href).not.toMatch(/[#? ]/);
    expect(pathFromPathname("/tok" + href)).toBe(path);
  });

  it("encodes each segment, keeps the slashes", () => {
    expect(hrefFor("a b/c#d")).toBe("/src/tree/a%20b/c%23d");
    expect(apiFileUrl("x/%y")).toBe("/api/file/tree/x/%25y");
  });

  it.each([
    "/tok",
    "/tok/",
    "/tok/src",
    "/tok/src/tree",
    "/tok/src/tree/",
    "/tok/src/head/a.hs",
    "/tok/docs/tree/a.hs",
    "/tok/src/tree/a//b",
    "/tok/src/tree/a/",
    "/tok/src/tree/./a",
    "/tok/src/tree/a/..",
    "/tok/src/tree/%2E%2E/a",
    "/tok/src/tree/%2e",
    "/tok/src/tree/a%2Fb",
    "/tok/src/tree/%E0",
    "/tok/src/tree/%",
  ])("rejects %s", (pathname) => {
    expect(pathFromPathname(pathname)).toBeUndefined();
  });
});

describe("withBase", () => {
  it.each([
    ["/tok", "/tok/src/tree/a"],
    ["/tok/", "/tok/src/tree/a"],
    ["/", "/src/tree/a"],
    ["", "/src/tree/a"],
    [undefined, "/src/tree/a"],
  ])("%j", (base, out) => {
    expect(withBase(base, "/src/tree/a")).toBe(out);
  });
});

describe("segmentsAfter", () => {
  it("decodes each segment after the prefix", () => {
    expect(
      segmentsAfter("/tok/docs/tree/a%2Eb/C%20d", ["docs", "tree"]),
    ).toEqual(["a.b", "C d"]);
    expect(segmentsAfter("/tok/x/y", ["x"])).toEqual(["y"]);
  });

  it.each([
    "/tok/docs/tree",
    "/tok/docs/tree/",
    "/tok/docs/head/A",
    "/tok/src/tree/A",
    "/tok/docs/tree/a%2Fb",
    "/tok/docs/tree/..",
    "/tok/docs/tree/%E0",
  ])("rejects %s", (pathname) => {
    expect(segmentsAfter(pathname, ["docs", "tree"])).toBeUndefined();
  });
});

describe("line hashes", () => {
  it.each([
    ["#L3", { from: 3, to: 3 }],
    ["#L3-L7", { from: 3, to: 7 }],
    ["#L7-L7", { from: 7, to: 7 }],
  ])("%s", (hash, out) => {
    expect(parseLineHash(hash)).toEqual(out);
  });

  it.each([
    "",
    "#",
    "#L0",
    "#L7-L3",
    "#L",
    "#l3",
    "#L3-",
    "#L3-7",
    "#L03",
    "#L3x",
    "#L1234567890",
    "#val%3Ax",
  ])("rejects %j", (hash) => {
    expect(parseLineHash(hash)).toBeUndefined();
  });

  it("lineHref", () => {
    expect(lineHref("a b/c.hs", 12)).toBe("/src/tree/a%20b/c.hs#L12");
  });
});
