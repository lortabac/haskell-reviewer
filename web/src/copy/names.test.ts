import { describe, expect, it } from "vitest";
import type { Target } from "../api/source";
import { location, qualified, stripQualifier, targetCopyText } from "./names";

describe("qualified", () => {
  it("joins; operators stay bare", () => {
    expect(qualified("Data.Foo", "bar")).toBe("Data.Foo.bar");
    expect(qualified("Data.Foo", "<+>")).toBe("Data.Foo.<+>");
    expect(qualified("Main", "Just")).toBe("Main.Just");
  });
});

describe("stripQualifier", () => {
  it.each([
    ["M.lookup", "lookup"],
    ["A.B.Foo", "Foo"],
    ["Map.!", "!"],
    ["M..", "."],
    [".", "."],
    ["lookup", "lookup"],
    ["Foo", "Foo"],
    ["Foo.", "Foo."],
    ["Ä.Öl", "Öl"],
    ["M.x'", "x'"],
  ])("%s -> %s", (a, b) => {
    expect(stripQualifier(a)).toBe(b);
  });
});

describe("location", () => {
  it("a line, a range", () => {
    expect(location("src/Foo.hs", 12)).toBe("src/Foo.hs:12");
    expect(location("src/Foo.hs", 12, 12)).toBe("src/Foo.hs:12");
    expect(location("src/Foo.hs", 12, 15)).toBe("src/Foo.hs:12-15");
  });
});

describe("targetCopyText", () => {
  const mods: Record<string, string> = { "r:A": "Data.A" };
  const name = (ref: string) => mods[ref];
  const ext = (how: "listed" | "only-open" | "one-of") =>
    ["external", "Data.Char", null, how, 1, "q"] as Target;
  const decl = (ref: string) =>
    ["decl", ref, "A.hs", 1, "val:f", [], 1, null, false] as Target;

  it("a declaration: module from the tree, qualifier stripped", () => {
    expect(targetCopyText("M.f", decl("r:A"), name)).toBe("Data.A.f");
    expect(targetCopyText("f", decl("r:B"), name)).toBeUndefined();
  });

  it("a decl or module whose module has no name: nothing", () => {
    const none = () => undefined;
    expect(targetCopyText("f", decl("r:A"), none)).toBeUndefined();
    const t = ["module", "r:A", "A.hs", [], 1] as Target;
    expect(targetCopyText("A", t, none)).toBeUndefined();
  });

  it("a module of ours", () => {
    const t = ["module", "r:A", "A.hs", [], 1] as Target;
    expect(targetCopyText("A", t, name)).toBe("Data.A");
  });

  it("external: listed and only-open qualify, one-of does not", () => {
    expect(targetCopyText("toUpper", ext("listed"), name)).toBe(
      "Data.Char.toUpper",
    );
    expect(targetCopyText("toUpper", ext("only-open"), name)).toBe(
      "Data.Char.toUpper",
    );
    expect(targetCopyText("toUpper", ext("one-of"), name)).toBeUndefined();
  });

  it("external: the module token itself, whatever the guess", () => {
    expect(targetCopyText("Data.Char", ext("one-of"), name)).toBe("Data.Char");
  });

  it("a local: nothing", () => {
    expect(targetCopyText("x", ["local", 1, 1], name)).toBeUndefined();
  });
});
