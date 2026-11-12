import { describe, expect, it } from "vitest";
import { reasonCodes } from "../api/source";
import { cardOf, reasonText } from "./hover";

describe("reasonText", () => {
  it("has words for every reason code, all different", () => {
    const words = reasonCodes.map((r) => reasonText(r, 2));
    expect(words.every((w) => w.length > 0 && w.endsWith("."))).toBe(true);
    expect(new Set(words).size).toBe(reasonCodes.length);
  });

  it("ambiguous: the number of candidates", () => {
    expect(reasonText("ambiguous", 3)).toBe("One of 3 candidates in scope.");
  });
});

describe("cardOf", () => {
  it("an exact declaration: its signature and module", () => {
    expect(
      cardOf([
        "decl",
        "Links.Types",
        "Links/Types.hs",
        5,
        "val:area",
        [],
        1,
        "area :: Shape -> Double",
        false,
      ]),
    ).toEqual({
      code: "area :: Shape -> Double",
      lines: ["Module Links.Types."],
    });
  });

  it("no signature, a guess: said, with each reason once", () => {
    expect(
      cardOf([
        "decl",
        "A",
        "A.hs",
        2,
        "val:f",
        ["ambiguous", "not-ready", "ambiguous"],
        4,
        null,
        false,
      ]),
    ).toEqual({
      code: null,
      lines: [
        "No type signature.",
        "Module A.",
        "Resolved heuristically: One of 4 candidates in scope. A module it depends on wasn't indexed yet.",
      ],
    });
  });

  it("a module", () => {
    expect(cardOf(["module", "B.C", null, [], 1])).toEqual({
      code: null,
      lines: ["Module B.C. The declaration wasn't found in it."],
    });
    expect(cardOf(["module", "B", "B.hs", ["import-cycle"], 1]).lines).toEqual([
      "Module B. The declaration wasn't found in it.",
      "Resolved heuristically: Its module is in an import cycle.",
    ]);
  });

  it("a local", () => {
    expect(cardOf(["local", 16, 4])).toEqual({
      code: null,
      lines: ["Local, bound at line 16."],
    });
  });

  it("externals: listed, only open, one of", () => {
    expect(
      cardOf(["external", "Data.List", "base", "listed", 1, "sort"]).lines,
    ).toEqual(["From Data.List (base). Search on Hoogle."]);
    expect(
      cardOf(["external", "Prelude", null, "only-open", 1, "sum"]).lines,
    ).toEqual([
      "From Prelude, the only open import that could have it. A guess. Search on Hoogle.",
    ]);
    expect(
      cardOf(["external", "Data.Char", "base", "one-of", 2, "toUpper"]).lines,
    ).toEqual(["From one of 2 open imports. A guess. Search on Hoogle."]);
  });
});
