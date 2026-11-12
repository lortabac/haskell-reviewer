import { describe, expect, it } from "vitest";
import type { SourcePart, Span } from "../api/module";
import {
  firstCodeLine,
  rowSegs,
  sourceRows,
  textParts,
  type SourceRow,
  type TextPart,
} from "./source";

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

const spans = (parts: TextPart[]) =>
  sourceRows(parts).map((r) => (r.kind === "line" ? r.spans : []));

describe("row spans", () => {
  it("pad is in no span; the text maps to its line and column", () => {
    expect(spans([part([3, 5, 4, 8], "f = 1\n    + 2")])).toEqual([
      [{ at: 4, line: 3, col: 5, len: 5 }],
      [{ at: 0, line: 4, col: 1, len: 7 }],
    ]);
  });

  it("touching parts: two spans; a gap: the filler is in none", () => {
    expect(
      spans([part([1, 1, 1, 4], "abc"), part([1, 4, 1, 6], "de")]),
    ).toEqual([
      [
        { at: 0, line: 1, col: 1, len: 3 },
        { at: 3, line: 1, col: 4, len: 2 },
      ],
    ]);
    expect(
      spans([part([1, 1, 1, 4], "abc"), part([1, 8, 1, 10], "de")]),
    ).toEqual([
      [
        { at: 0, line: 1, col: 1, len: 3 },
        { at: 6, line: 1, col: 8, len: 2 },
      ],
    ]);
  });

  it("astral characters count as one; CRLF is dropped", () => {
    expect(spans([part([1, 1, 2, 3], "\u{1d465}y\r\nab")])).toEqual([
      [{ at: 0, line: 1, col: 1, len: 2 }],
      [{ at: 0, line: 2, col: 1, len: 2 }],
    ]);
  });

  it("a span ending at column 1: the line break is not in it", () => {
    expect(spans([part([1, 1, 3, 1], "a\nb\n")])).toEqual([
      [{ at: 0, line: 1, col: 1, len: 1 }],
      [{ at: 0, line: 2, col: 1, len: 1 }],
    ]);
  });

  it("an overlap: its text has no span", () => {
    expect(
      spans([part([1, 1, 2, 4], "abc\ndef"), part([1, 2, 1, 3], "b")]),
    ).toEqual([
      [{ at: 0, line: 1, col: 1, len: 3 }],
      [{ at: 0, line: 2, col: 1, len: 3 }],
    ]);
  });
});

describe("rowSegs", () => {
  const seg = (from: number, to: number, link = 0) => ({
    from,
    to,
    cls: "variable",
    link,
  });
  const row = (...sp: SourceRow[]) => {
    const r = sp[0];
    if (r?.kind !== "line") throw new Error("not a line");
    return r;
  };

  it("shifts to the row's columns, clips to the span", () => {
    // "  f = 1" from column 3 of line 5: runs on line 5 and 6.
    const r = row(...sourceRows([part([5, 3, 5, 8], "f = 1")]));
    const runs = [
      [],
      [],
      [],
      [],
      [],
      [seg(1, 4), seg(5, 6, 2), seg(7, Infinity)],
    ];
    expect(rowSegs(r, runs)).toEqual([seg(3, 4), seg(5, 6, 2), seg(7, 8)]);
  });

  it("two parts on a row: each its own columns", () => {
    const r = row(
      ...sourceRows([part([1, 1, 1, 4], "abc"), part([1, 8, 1, 10], "hi")]),
    );
    // Row: "abc … hi": h is at row column 7, from file column 8.
    expect(rowSegs(r, [[], [seg(1, 2), seg(8, 9, 1), seg(20, 25)]])).toEqual([
      seg(1, 2),
      seg(7, 8, 1),
    ]);
  });

  it("astral before: code points", () => {
    const r = row(...sourceRows([part([1, 1, 1, 4], "\u{1d465}yz")]));
    expect(rowSegs(r, [[], [seg(3, 4)]])).toEqual([seg(3, 4)]);
  });

  it("no spans: no segs", () => {
    const rows = sourceRows([
      part([1, 1, 2, 4], "abc\ndef"),
      part([1, 2, 1, 3], "b"),
    ]);
    expect(rows.length).toBe(2);
    expect(rowSegs(row(...rows), [[], [seg(1, 2)]])).toEqual([seg(1, 2)]);
  });
});
