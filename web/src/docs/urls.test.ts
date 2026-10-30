import { describe, expect, it } from "vitest";
import { docsHref } from "../api/module";
import {
  anchorFromHash,
  anchorName,
  apiModuleUrl,
  queryFromUrl,
  refFromPathname,
  unescapeHaddock,
} from "./urls";

describe("docs URLs", () => {
  it.each([
    "Data.Map",
    "pkg:exe:app:Main",
    "scripts/build-docs.hs",
    "a#b/C d.hs",
    "x%y",
  ])("round trip: %s", (ref) => {
    expect(refFromPathname("/tok" + docsHref(ref))).toBe(ref);
  });

  it("keeps the ref as typed: escaped or not", () => {
    expect(refFromPathname("/tok/docs/tree/pkg:lib:Foo")).toBe("pkg:lib:Foo");
    expect(refFromPathname("/tok/docs/tree/pkg%3Alib%3AFoo")).toBe(
      "pkg:lib:Foo",
    );
  });

  it.each([
    "/tok",
    "/tok/docs",
    "/tok/docs/tree",
    "/tok/docs/tree/",
    "/tok/docs/head/A",
    "/tok/src/tree/A.hs",
    "/tok/docs/tree/a%2Fb",
    "/tok/docs/tree/a/../b",
    "/tok/docs/tree/a//b",
    "/tok/docs/tree/%E0",
  ])("rejects %s", (pathname) => {
    expect(refFromPathname(pathname)).toBeUndefined();
  });

  it("module API URL", () => {
    expect(apiModuleUrl("pkg:lib:Foo")).toBe(
      "/api/module/tree/pkg%3Alib%3AFoo",
    );
    expect(apiModuleUrl("scripts/x y.hs")).toBe(
      "/api/module/tree/scripts/x%20y.hs",
    );
  });
});

describe("anchors", () => {
  it.each([
    ["", null],
    ["#", null],
    ["#type%3AShape", "type:Shape"],
    ["#type:Shape", "type:Shape"],
    ["#inst%3AShow%3AMaybe%20a", "inst:Show:Maybe a"],
    ["#v:x", "v:x"],
    ["#%E0", { bad: true }],
  ])("%j", (hash, out) => {
    expect(anchorFromHash(hash)).toEqual(out);
  });

  it.each([
    ["type:Foo", "Foo"],
    ["field:T.f", "f"],
    ["field:Config.port", "port"],
    ["v:x", "x"],
    ["t:T", "T"],
    ["val:<.>", "<.>"],
    ["inst:Show:Maybe a", "Show"],
    ["junk", "junk"],
  ])("name of %s", (anchor, name) => {
    expect(anchorName(anchor)).toBe(name);
  });
});

describe("queryFromUrl", () => {
  it.each([
    ["/tok/docs/tree/No.Such", "No.Such"],
    ["/tok/docs/tree/pkg%3Aexe%3Aapp%3AMain", "pkg:exe:app:Main"],
    ["/tok/docs/tree/scripts/x.hs", "scripts/x.hs"],
    ["/tok/docs/tree/a%2Fb", "a/b"],
    ["/tok/docs/tree/%E0", "%E0"],
    ["/tok/src/tree/a/b.hs", "a/b.hs"],
    ["/tok/changes", ""],
    ["/tok/docs/head/X", ""],
  ])("%s", (pathname, q) => {
    expect(queryFromUrl(pathname)).toBe(q);
  });
});

describe("unescapeHaddock", () => {
  it.each([
    ["foldl-39-", "foldl'"],
    ["-60--43--62-", "<+>"],
    ["-45-", "-"],
    ["x-128512-", "x😀"],
  ])("%s -> %s", (raw, name) => {
    expect(unescapeHaddock(raw)).toBe(name);
  });

  it.each(["area", "-", "a-b", "-99999999-"])("%s: nothing", (raw) => {
    expect(unescapeHaddock(raw)).toBeUndefined();
  });
});
