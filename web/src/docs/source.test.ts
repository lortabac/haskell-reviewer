import { describe, expect, it } from "vitest";
import type { SourcePart, Span } from "../api/module";
import { firstCodeLine, sourceRows, textParts, type TextPart } from "./source";

const part = (span: Span, text: string): TextPart => ({ span, text });

// Rows as text: "n| line" or "⋯ a–b".
const show = (parts: TextPart[]) =>
  sourceRows(parts).map((r) =>
    r.kind === "line" ? `${r.line}| ${r.text}` : `⋯ ${r.from}–${r.to}`,
  );

describe("sourceRows", () => {
  it("one part, padded to its column", () => {
    expect(show([part([3, 5, 4, 8], "f = 1\n    + 2")])).toEqual([
      "3|     f = 1",
      "4|     + 2",
    ]);
  });

  it("next line: a new row with its indent", () => {
    expect(
      show([part([1, 1, 1, 9], "f :: Int"), part([2, 3, 2, 8], "f = 1")]),
    ).toEqual(["1| f :: Int", "2|   f = 1"]);
  });

  it("further down: a gap row", () => {
    expect(
      show([
        part([42, 1, 42, 13], "infixl 6 <+>"),
        part([44, 1, 44, 12], "a <+> b = a"),
      ]),
    ).toEqual(["42| infixl 6 <+>", "⋯ 43–43", "44| a <+> b = a"]);
  });

  it("same line: touching parts join, a gap gives …", () => {
    expect(
      show([
        part([74, 1, 76, 3], "instance C [a] where\n  type E [a] = a\n  "),
        part([76, 3, 76, 13], "empty = []"),
        part([76, 13, 77, 3], "\n  "),
        part([77, 3, 77, 15], "insert = (:)"),
      ]),
    ).toEqual([
      "74| instance C [a] where",
      "75|   type E [a] = a",
      "76|   empty = []",
      "77|   insert = (:)",
    ]);
    expect(
      show([part([35, 12, 35, 19], "newtype"), part([35, 21, 35, 23], "Eq")]),
    ).toEqual(["35|            newtype … Eq"]);
  });

  it("a span ending at column 1 ends on the line before", () => {
    expect(
      show([part([1, 1, 3, 1], "a\nb\n"), part([3, 1, 3, 2], "c")]),
    ).toEqual(["1| a", "2| b", "3| c"]);
  });

  it("CRLF: no \\r", () => {
    expect(show([part([1, 1, 2, 4], "a\r\nbcd")])).toEqual(["1| a", "2| bcd"]);
  });

  it("unsorted parts are sorted; .lhs and CPP lines as written", () => {
    expect(
      show([
        part([5, 1, 5, 8], "> f = 1"),
        part([2, 1, 3, 7], "#if X\n> f :: Int"),
      ]),
    ).toEqual(["2| #if X", "3| > f :: Int", "⋯ 4–4", "5| > f = 1"]);
  });

  it("nothing: no rows", () => {
    expect(sourceRows([])).toEqual([]);
  });
});

describe("textParts and firstCodeLine", () => {
  const parts: SourcePart[] = [
    { span: [37, 1, 37, 31], role: "doc", sharedWith: 10 },
    { span: [38, 1, 38, 35], role: "head", sharedWith: 10 },
    { span: [40, 1, 40, 16], role: "body", text: "perimeter _ = 0" },
    { span: [50, 1, 50, 2], role: "body", sharedWith: 99 },
  ];

  it("resolves shared parts, drops the ones it can't", () => {
    const sent = (p: { span: Span; sharedWith: number }) =>
      p.sharedWith === 10 ? { text: `from 10 at ${p.span[0]}` } : undefined;
    expect(textParts(parts, sent).map((p) => p.text)).toEqual([
      "from 10 at 37",
      "from 10 at 38",
      "perimeter _ = 0",
    ]);
  });

  it("first code line: not docs, earliest", () => {
    expect(firstCodeLine(parts)).toBe(38);
    expect(firstCodeLine([parts[0] as SourcePart])).toBeUndefined();
  });
});
