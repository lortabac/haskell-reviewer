import { describe, expect, it } from "vitest";
import { docsHref } from "./api/module";
import { refsUrl } from "./api/refs";
import { searchUrl } from "./api/search";
import { defaultSide, sessionSides } from "./api/session";
import { textSearchUrl } from "./api/textSearch";
import { apiModulesUrl, apiModuleUrl, refFromPathname } from "./docs/urls";
import {
  apiFilesUrl,
  apiFileUrl,
  apiMarkdownUrl,
  apiSourceUrl,
  hrefFor,
  lineHref,
  mdHref,
  mdPathFromPathname,
  pathFromPathname,
} from "./files/paths";
import { refsFromLocation, refsHref } from "./refs/urls";
import { onSide, urlSide } from "./session";
import { isSide, sides } from "./sides";
import { textSearchHref } from "./textsearch/urls";

describe("every URL helper puts the side in", () => {
  it.each(sides)("%s", (s) => {
    expect(hrefFor(s, "a/b.hs")).toBe(`/src/${s}/a/b.hs`);
    expect(lineHref(s, "a.hs", 3)).toBe(`/src/${s}/a.hs#L3`);
    expect(mdHref(s, "R.md", "x")).toBe(`/md/${s}/R.md#x`);
    expect(docsHref(s, "A", "v:a")).toBe(`/docs/${s}/A#v%3Aa`);
    expect(refsHref(s, "A", "v:a")).toBe(`/refs/${s}/A?decl=v%3Aa`);
    expect(textSearchHref(s, "x")).toBe(`/search/${s}?q=x`);
    expect(apiFileUrl(s, "a.hs")).toBe(`/api/file/${s}/a.hs`);
    expect(apiSourceUrl(s, "a.hs")).toBe(`/api/source/${s}/a.hs`);
    expect(apiMarkdownUrl(s, "R.md")).toBe(`/api/markdown/${s}/R.md`);
    expect(apiFilesUrl(s)).toBe(`/api/files/${s}`);
    expect(apiModuleUrl(s, "A")).toBe(`/api/module/${s}/A`);
    expect(apiModulesUrl(s)).toBe(`/api/modules/${s}`);
    expect(searchUrl(s, "x")).toBe(`/api/search/${s}?q=x`);
    expect(refsUrl(s, "A", "v:a")).toBe(`/api/refs/${s}?module=A&decl=v%3Aa`);
    expect(textSearchUrl(s, "x", 5)).toBe(`/api/text-search/${s}?q=x&limit=5`);
  });
});

describe("parsers take only their side", () => {
  it.each(sides)("%s", (s) => {
    const other = sides.find((o) => o !== s) ?? "tree";
    expect(pathFromPathname("/tok" + hrefFor(s, "a.hs"), s)).toBe("a.hs");
    expect(pathFromPathname("/tok" + hrefFor(s, "a.hs"), other)).toBe(
      undefined,
    );
    expect(mdPathFromPathname("/tok" + mdHref(s, "R.md"), s)).toBe("R.md");
    expect(mdPathFromPathname("/tok" + mdHref(s, "R.md"), other)).toBe(
      undefined,
    );
    expect(refFromPathname("/tok" + docsHref(s, "A"), s)).toBe("A");
    expect(refFromPathname("/tok" + docsHref(s, "A"), other)).toBe(undefined);
    const [path, query] = refsHref(s, "A", "v:a").split("?") as [
      string,
      string,
    ];
    expect(refsFromLocation(s, "/tok" + path, "?" + query)).toEqual({
      module: "A",
      decl: "v:a",
    });
    expect(refsFromLocation(other, "/tok" + path, "?" + query)).toBe(undefined);
  });

  it("a view's parser rejects another view", () => {
    expect(pathFromPathname("/tok/docs/head/A", "head")).toBe(undefined);
    expect(refFromPathname("/tok/src/head/A.hs", "head")).toBe(undefined);
    expect(mdPathFromPathname("/tok/src/head/R.md", "head")).toBe(undefined);
  });
});

describe("urlSide", () => {
  it.each([
    ["/tok/docs/head/A", "head"],
    ["/tok/src/base/a.hs", "base"],
    ["/tok/md/tree/R.md", "tree"],
    ["/tok/refs/nope/A", "nope"],
    ["/tok/search/head", "head"],
    ["/tok/docs/", undefined],
    ["/tok/docs", undefined],
    ["/tok", undefined],
    ["/tok/", undefined],
    ["/tok/changes/head", undefined],
  ])("%s", (pathname, side) => {
    expect(urlSide(pathname)).toBe(side);
  });
});

describe("onSide", () => {
  it("swaps the side, keeps the rest", () => {
    expect(onSide("/tok/docs/tree/A/B", "", "#v:a", "head")).toBe(
      "/docs/head/A/B#v:a",
    );
    expect(onSide("/tok/search/tree", "?q=x", "", "head")).toBe(
      "/search/head?q=x",
    );
  });
});

describe("sides and sessions", () => {
  it("isSide", () => {
    expect(sides.every(isSide)).toBe(true);
    expect(isSide("nope")).toBe(false);
    expect(isSide(undefined)).toBe(false);
  });

  it("the default side and the sides of each mode", () => {
    const diff = {
      mode: "diff",
      spec: "a..b",
      base: { label: "a", commit: "1" },
      head: { label: "b", commit: "2" },
      mergeBases: null,
    } as const;
    expect(sessionSides({ mode: "browse" })).toEqual(["tree"]);
    expect(defaultSide({ mode: "browse" })).toBe("tree");
    expect(sessionSides(diff)).toEqual(["base", "head"]);
    expect(defaultSide(diff)).toBe("head");
  });
});
