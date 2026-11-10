import { describe, expect, it } from "vitest";
import { maxPieces, pieces, plainPieces, styledPieces } from "./display";
import type { Seg } from "./highlight";
import { lineNodes } from "./paint";

describe("pieces", () => {
  it("plain text is one piece", () => {
    expect(pieces("a\tb")).toEqual([{ text: "a\tb", ctrl: false }]);
    expect(pieces("")).toEqual([]);
  });

  it("control characters become glyphs, one unit each", () => {
    const line = "a\u0000\u001b[b\r\u007f\u0085\u2028\u2029\u000b\u000c";
    const ps = pieces(line);
    expect(ps).toEqual([
      { text: "a", ctrl: false },
      { text: "\u2400\u241b", ctrl: true },
      { text: "[b", ctrl: false },
      { text: "\u240d\u2421\u2424\u2424\u2424\u240b\u240c", ctrl: true },
    ]);
    expect(ps.map((p) => p.text).join("")).toHaveLength(line.length);
  });

  it("C1 and bidi controls too", () => {
    const line = "x\u0086\u009f\u202ey\u2066\u2069";
    const ps = pieces(line);
    expect(ps).toEqual([
      { text: "x", ctrl: false },
      { text: "\u2426\u2426\u2194", ctrl: true },
      { text: "y", ctrl: false },
      { text: "\u2194\u2194", ctrl: true },
    ]);
    expect(ps.map((p) => p.text).join("")).toHaveLength(line.length);
  });

  it("bidi marks: LRM, RLM, ALM", () => {
    expect(pieces("a\u200e\u200fb\u061c")).toEqual([
      { text: "a", ctrl: false },
      { text: "\u2194\u2194", ctrl: true },
      { text: "b", ctrl: false },
      { text: "\u2194", ctrl: true },
    ]);
  });

  it("astral text and other marks stay as they are", () => {
    expect(pieces("\ud835\udc65\u200b\u0001")).toEqual([
      { text: "\ud835\udc65\u200b", ctrl: false },
      { text: "\u2401", ctrl: true },
    ]);
  });
});

const seg = (
  from: number,
  to: number,
  cls: string | null = "keyword",
  link = 0,
): Seg => ({
  from,
  to,
  cls,
  link,
});

describe("styledPieces", () => {
  it("gaps plain, runs with their class and link", () => {
    // "x = Just 1": x variable, Just typeCon (link 3).
    const ps = styledPieces("x = Just 1", [
      seg(1, 2, "variable"),
      seg(5, 9, "typeCon", 3),
    ]);
    expect(ps).toEqual([
      { text: "x", cls: "variable", link: 0, ctrl: false },
      { text: " = ", cls: null, link: 0, ctrl: false },
      { text: "Just", cls: "typeCon", link: 3, ctrl: false },
      { text: " 1", cls: null, link: 0, ctrl: false },
    ]);
  });

  it("code point columns: astral and tab", () => {
    const line = "\t\u{1d465} y";
    const ps = styledPieces(line, [seg(2, 3, "variable"), seg(4, 5)]);
    expect(ps.map((p) => [p.text, p.cls])).toEqual([
      ["\t", null],
      ["\u{1d465}", "variable"],
      [" ", null],
      ["y", "keyword"],
    ]);
  });

  it("control characters inside a run: glyphs, class kept", () => {
    const ps = styledPieces("a\u0007b", [seg(1, 4)]);
    expect(ps).toEqual([
      { text: "a", cls: "keyword", link: 0, ctrl: false },
      { text: "␇", cls: "keyword", link: 0, ctrl: true },
      { text: "b", cls: "keyword", link: 0, ctrl: false },
    ]);
  });

  it("to the line's end, past it, overlapping, empty: cut, never broken", () => {
    const texts = (line: string, segs: Seg[]) =>
      styledPieces(line, segs).map((p) => p.text);
    expect(texts("ab", [seg(2, Infinity)])).toEqual(["a", "b"]);
    expect(texts("ab", [seg(5, 9)])).toEqual(["ab"]);
    expect(texts("abcd", [seg(1, 3), seg(2, 4)])).toEqual(["ab", "c", "d"]);
    expect(styledPieces("", [seg(1, Infinity)])).toEqual([]);
  });

  it("texts joined are the line's (glyphs for controls)", () => {
    const lines = ["", "abc", "a\u{1d465}\tb\u0000c", "‮x "];
    for (const line of lines) {
      for (const segs of [
        [],
        [seg(1, 2)],
        [seg(1, 3), seg(3, Infinity, null)],
        [seg(2, 3), seg(4, 99, "comment", 1)],
      ]) {
        const text = styledPieces(line, segs)
          .map((p) => p.text)
          .join("");
        expect(text).toBe(
          pieces(line)
            .map((p) => p.text)
            .join(""),
        );
        expect(text).toHaveLength(line.length);
      }
    }
  });

  it("the piece cap: at it highlighted, past it plain", () => {
    // One char per seg, no gaps: one piece each.
    const at = "x".repeat(maxPieces);
    const segs = Array.from({ length: maxPieces }, (_, i) => seg(i + 1, i + 2));
    expect(styledPieces(at, segs)).toHaveLength(maxPieces);
    expect(styledPieces(at, segs)[0]?.cls).toBe("keyword");
    // Too many segs: plain at once.
    const more = at + "x";
    expect(
      styledPieces(more, [...segs, seg(maxPieces + 1, maxPieces + 2)]),
    ).toEqual(plainPieces(more));
    // Segs at the cap, but a control glyph splits the last one: plain.
    const ctrl = "x".repeat(maxPieces - 1) + "a\u0007";
    const split = [
      ...segs.slice(0, maxPieces - 1),
      seg(maxPieces, maxPieces + 2),
    ];
    expect(styledPieces(ctrl, split)).toEqual(plainPieces(ctrl));
  });
});

describe("lineNodes", () => {
  it("text as text, classes as spans, never markup", () => {
    const p = document.createElement("p");
    p.append(
      ...lineNodes([
        { text: "<b>", cls: "keyword", link: 0, ctrl: false },
        { text: " x", cls: null, link: 0, ctrl: false },
        { text: "\u2407", cls: "keyword", link: 0, ctrl: true },
      ]),
    );
    expect(p.querySelector("b")).toBeNull();
    expect(p.textContent).toBe("<b> x\u2407");
    expect(
      [...p.childNodes].map((n) => (n as Element).className ?? "#text"),
    ).toEqual(["hl-keyword", "#text", "code-ctrl"]);
  });
});
