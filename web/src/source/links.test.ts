import { describe, expect, it } from "vitest";
import type { Target } from "../api/source";
import type { Seg } from "./highlight";
import {
  hoogleHref,
  jumpOf,
  type LinkInfo,
  linkInfos,
  linkName,
  pageLinkInfos,
  shortName,
} from "./links";

const view = { path: "src/A.hs", ref: "A" };
const one = (t: Target): LinkInfo => {
  const [info] = linkInfos(view, [t], "/tok");
  if (info === undefined) throw new Error("no info");
  return info;
};

describe("linkInfos", () => {
  it("a local use: its binder's line here", () => {
    const i = one(["local", 7, 3]);
    expect(i).toEqual({
      href: "/tok/src/tree/src/A.hs#L7",
      about: "bound at line 7",
      guess: false,
      external: false,
      here: true,
    });
    expect(linkName("x", i)).toBe("x, bound at line 7");
  });

  it("a use of a declaration: its file and line", () => {
    const i = one(["decl", "B", "src/B.hs", 4, "val:y", [], 1, null, false]);
    expect(i.href).toBe("/tok/src/tree/src/B.hs#L4");
    expect(linkName("y", i)).toBe("y, defined in src/B.hs line 4");
    expect(i.here).toBe(false);
    expect(
      one(["decl", "A", "src/A.hs", 4, "val:y", [], 1, null, false]).here,
    ).toBe(true);
  });

  it("no line: the file", () => {
    const i = one(["decl", "B", "src/B.hs", null, "val:y", [], 1, null, false]);
    expect(i.href).toBe("/tok/src/tree/src/B.hs");
    expect(i.about).toBe("defined in src/B.hs");
  });

  it("a definition: its docs", () => {
    const i = one(["decl", "A", "src/A.hs", 2, "val:f x", [], 1, null, true]);
    expect(i.href).toBe("/tok/docs/tree/A#val%3Af%20x");
    expect(linkName("f", i)).toBe("f, documentation");
  });

  it("a module: its file, or its docs with no file", () => {
    expect(one(["module", "B", "src/B.hs", [], 1]).href).toBe(
      "/tok/src/tree/src/B.hs",
    );
    const i = one(["module", "B.C", null, ["not-ready"], 1]);
    expect(i.href).toBe("/tok/docs/tree/B.C");
    expect(linkName("C.x", i)).toBe("C.x, module B.C, resolved heuristically");
  });

  it("reasons make a guess", () => {
    const i = one([
      "decl",
      "B",
      "b",
      1,
      "val:y",
      ["ambiguous"],
      2,
      null,
      false,
    ]);
    expect(i.guess).toBe(true);
    expect(linkName("y", i)).toBe(
      "y, defined in b line 1, resolved heuristically",
    );
  });

  it("external: Hoogle, a new tab; a guess unless listed", () => {
    const listed = one([
      "external",
      "Data.Map",
      "containers",
      "listed",
      1,
      "insert +module:Data.Map",
    ]);
    expect(listed.href).toBe(
      "https://hoogle.haskell.org/?hoogle=insert%20%2Bmodule%3AData.Map",
    );
    expect(listed.external).toBe(true);
    expect(linkName("M.insert", listed)).toBe(
      "M.insert, Data.Map (containers), search on Hoogle, opens in a new tab",
    );
    const guess = one(["external", "Prelude", null, "one-of", 2, "map"]);
    expect(linkName("map", guess)).toBe(
      "map, one of 2 modules, search on Hoogle, resolved heuristically, opens in a new tab",
    );
    const only = one(["external", "P", "base", "only-open", 1, "x"]);
    expect(only.guess).toBe(true);
    expect(linkName("x", only)).toBe(
      "x, P (base), search on Hoogle, resolved heuristically, opens in a new tab",
    );
  });

  it("Hoogle queries are encoded", () => {
    expect(hoogleHref("<> +module:Data.Semigroup")).toBe(
      "https://hoogle.haskell.org/?hoogle=%3C%3E%20%2Bmodule%3AData.Semigroup",
    );
    expect(hoogleHref("foldl'")).toBe(
      "https://hoogle.haskell.org/?hoogle=foldl'",
    );
  });

  it("base / and none: no double slash", () => {
    expect(linkInfos(view, [["local", 1, 1]], "/")[0]?.href).toBe(
      "/src/tree/src/A.hs#L1",
    );
    expect(linkInfos(view, [["local", 1, 1]], undefined)[0]?.href).toBe(
      "/src/tree/src/A.hs#L1",
    );
  });
});

describe("jumpOf", () => {
  const seg = (from: number, to: number, link: number): Seg => ({
    from,
    to,
    cls: "variable",
    link,
  });
  const targets: Target[] = [
    ["local", 2, 5],
    ["decl", "A", "src/A.hs", 2, "val:f", [], 1, null, true],
    ["decl", "A", "src/A.hs", 2, "val:f", [], 1, null, false],
    ["decl", "A", "src/A.hs", 9, "val:g", [], 1, null, false],
    ["external", "P", null, "listed", 1, "x"],
  ];
  // Line 2: "\u{1d465} f x"; f (def) at code point 3, x at 5. The
  // astral character is 2 UTF-16 units.
  const lines = ["", "", "\u{1d465} f x"];
  const runs: Seg[][] = [[], [], [seg(3, 4, 2), seg(5, 6, 0)]];
  const line = (n: number) => lines[n] ?? "";

  it("a local: the binder, UTF-16", () => {
    expect(jumpOf(1, targets, runs, line)).toEqual({ line: 2, col: 5 });
  });

  it("a use here: the definition's name on its line", () => {
    expect(jumpOf(3, targets, runs, line)).toEqual({ line: 2, col: 3 });
  });

  it("no definition seg on the line: its start", () => {
    expect(jumpOf(4, targets, runs, line)).toEqual({ line: 9, col: 0 });
  });

  it("a definition or external: none", () => {
    expect(jumpOf(2, targets, runs, line)).toBeUndefined();
    expect(jumpOf(5, targets, runs, line)).toBeUndefined();
    expect(jumpOf(9, targets, runs, line)).toBeUndefined();
  });
});

describe("pageLinkInfos", () => {
  const page = { ref: "A", typedRef: "A.Typed" };
  const infos = (...t: Target[]) => pageLinkInfos(page, t, "/tok");

  it("a declaration: its docs; this module as typed in the URL", () => {
    const [here, other] = infos(
      ["decl", "A", "src/A.hs", 4, "val:y", [], 1, null, false],
      ["decl", "B", "src/B.hs", 4, "val:y", ["ambiguous"], 2, null, true],
    );
    expect(here).toEqual({
      href: "/tok/docs/tree/A.Typed#val%3Ay",
      about: "documentation",
      guess: false,
      external: false,
      here: false,
    });
    expect(other?.href).toBe("/tok/docs/tree/B#val%3Ay");
    expect(linkName("y", other as LinkInfo)).toBe(
      "y, documentation in B, resolved heuristically",
    );
  });

  it("a module: its docs; an external: Hoogle in a new tab; a local: none", () => {
    const [m, e, l] = infos(
      ["module", "B.C", "src/B/C.hs", [], 1],
      ["external", "Data.Char", null, "only-open", 1, "toUpper"],
      ["local", 3, 1],
    );
    expect(m?.href).toBe("/tok/docs/tree/B.C");
    expect(e).toMatchObject({ external: true, guess: true });
    expect(e?.href).toBe(hoogleHref("toUpper"));
    // The full name, as in the source view.
    expect(linkName("toUpper", e as LinkInfo)).toBe(
      "toUpper, Data.Char, search on Hoogle, resolved heuristically, opens in a new tab",
    );
    expect(l).toBeUndefined();
  });

  it("short names: the text and the marks", () => {
    const i = (guess: boolean, external: boolean): LinkInfo => ({
      href: "#",
      about: "long words",
      guess,
      external,
      here: false,
    });
    expect(shortName("x", i(false, false))).toBe("x");
    expect(shortName("x", i(true, true))).toBe(
      "x, resolved heuristically, opens in a new tab",
    );
  });
});
