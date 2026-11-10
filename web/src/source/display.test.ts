import { describe, expect, it } from "vitest";
import { pieces } from "./display";

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
