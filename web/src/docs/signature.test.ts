import { describe, expect, it } from "vitest";
import { type Code, codeText } from "../api/code";
import { isOperator, nameTokens, ownSignature, wrapParts } from "./signature";

// Rough tokens for tests: words, brackets, arrows; spaces as "sp".
function lex(s: string): Code {
  const out: Code = [];
  const re =
    /(\s+)|(::|->|=>|→|⇒|⊸|\(#|#\)|[()[\]{},])|("[^"]*")|([\p{Lu}][\w']*)|([\p{Ll}_][\w']*)|([^\s\w()[\]{},]+)/gu;
  for (const m of s.matchAll(re)) {
    if (m[1] !== undefined) out.push(["sp", m[1]]);
    else if (m[2] !== undefined) out.push(["pun", m[2]]);
    else if (m[3] !== undefined) out.push(["str", m[3]]);
    else if (m[4] !== undefined) out.push(["con", m[4]]);
    else if (m[5] !== undefined) out.push(["var", m[5]]);
    else if (m[6] !== undefined) out.push(["op", m[6]]);
  }
  return out;
}

const parts = (s: string) => wrapParts(lex(s)).map(codeText);

describe("wrapParts", () => {
  it("cuts before top-level arrows, drops the space", () => {
    expect(parts("f :: Int -> Bool -> Bool")).toEqual([
      "f :: Int",
      "-> Bool",
      "-> Bool",
    ]);
    expect(parts("f :: (Eq a, Show a) => a -> String")).toEqual([
      "f :: (Eq a, Show a)",
      "=> a",
      "-> String",
    ]);
  });

  it("not inside brackets", () => {
    expect(
      parts("f :: (a -> b) -> [a -> b] -> {x :: a -> b} -> (# a -> b #)"),
    ).toEqual([
      "f :: (a -> b)",
      "-> [a -> b]",
      "-> {x :: a -> b}",
      "-> (# a -> b #)",
    ]);
  });

  it("unicode arrows", () => {
    expect(parts("f ∷ Eq a ⇒ a → a ⊸ b")).toEqual([
      "f ∷ Eq a",
      "⇒ a",
      "→ a",
      "⊸ b",
    ]);
  });

  it("not in strings; nothing to cut; arrow first", () => {
    expect(parts('f :: "->" -> T')).toEqual(['f :: "->"', "-> T"]);
    expect(parts("data T a")).toEqual(["data T a"]);
    expect(parts("-> a")).toEqual(["-> a"]);
    expect(wrapParts([])).toEqual([]);
  });

  it("keeps every token but the spaces before arrows", () => {
    const code = lex("f :: a -> b");
    expect(wrapParts(code).flat()).toEqual(code.filter((_, i) => i !== 5));
  });
});

describe("ownSignature", () => {
  it("many names on one line: linear, not quadratic", () => {
    const count = 20000;
    const code: Code = [];
    for (let i = 0; i < count; i++) {
      if (i > 0) code.push(["pun", ","], ["sp", " "]);
      code.push(["var", `f${i}`]);
    }
    code.push(["sp", " "], ["pun", "::"], ["sp", " "], ["con", "Int"]);
    const start = performance.now();
    for (let i = 0; i < count; i++) {
      expect(codeText(ownSignature(code, `f${i}`))).toBe(`f${i} :: Int`);
    }
    // Scanning the code for each name (the old way) took seconds.
    expect(performance.now() - start).toBeLessThan(2000);
  });

  it("one name out of a shared signature", () => {
    expect(
      codeText(
        ownSignature(lex("area, perimeter :: Shape -> Double"), "perimeter"),
      ),
    ).toBe("perimeter :: Shape -> Double");
  });

  it("operators get parens", () => {
    const code = ownSignature(lex("(<+>), (<->) :: T -> T -> T"), "<->");
    expect(codeText(code)).toBe("(<->) :: T -> T -> T");
    expect(code[1]).toEqual(["op", "<->"]);
    expect(nameTokens(":+")).toEqual([
      ["pun", "("],
      ["conop", ":+"],
      ["pun", ")"],
    ]);
  });

  it("UnicodeSyntax ∷", () => {
    const code: Code = [
      ["var", "f"],
      ["pun", ","],
      ["sp", " "],
      ["var", "g"],
      ["sp", " "],
      ["pun", "∷"],
      ["sp", " "],
      ["con", "T"],
    ];
    expect(codeText(ownSignature(code, "g"))).toBe("g ∷ T");
  });

  it("record GADT: cut at the first top-level ::", () => {
    expect(codeText(ownSignature(lex("A, B :: {x :: Int} -> T"), "B"))).toBe(
      "B :: {x :: Int} -> T",
    );
  });

  it("keeps a leading keyword; no :: gives the name", () => {
    const pat: Code = [["kw", "pattern"], ["sp", " "], ...lex("A, B :: T")];
    expect(codeText(ownSignature(pat, "B"))).toBe("pattern B :: T");
    expect(codeText(ownSignature(lex("x"), "y"))).toBe("y");
  });

  it("isOperator", () => {
    expect(isOperator("<+>")).toBe(true);
    expect(isOperator(":|")).toBe(true);
    expect(isOperator("_x")).toBe(false);
    expect(isOperator("éa")).toBe(false);
    expect(nameTokens("Just")).toEqual([["con", "Just"]]);
  });
});
