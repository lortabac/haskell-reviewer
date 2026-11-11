import { describe, expect, it } from "vitest";
import { utf16Cols } from "./columns";

describe("utf16Cols", () => {
  it("ASCII: one less", () => {
    expect(utf16Cols("abcdef", [1, 2, 4, 7])).toEqual([0, 1, 3, 6]);
  });

  it("an astral character is 1 code point, 2 units", () => {
    const line = "a\u{1d465}b";
    // Before it, after it, after b.
    expect(utf16Cols(line, [2, 3, 4])).toEqual([1, 3, 4]);
    // At the start and at the end.
    expect(utf16Cols("\u{1d465}x", [1, 2, 3])).toEqual([0, 2, 3]);
    expect(utf16Cols("x\u{1d465}", [2, 3])).toEqual([1, 3]);
  });

  it("tab, lone CR and a combining mark are 1 each", () => {
    expect(utf16Cols("\tx", [2])).toEqual([1]);
    expect(utf16Cols("a\rb", [3])).toEqual([2]);
    expect(utf16Cols("éx", [3])).toEqual([2]);
  });

  it("clamped: past the end, below 1, empty line", () => {
    expect(utf16Cols("ab", [1, 3, 99, 0xffffffff])).toEqual([0, 2, 2, 2]);
    expect(utf16Cols("ab", [0])).toEqual([0]);
    expect(utf16Cols("", [1, 5])).toEqual([0, 0]);
    expect(utf16Cols("ab", [])).toEqual([]);
  });

  it("a lone surrogate is 1", () => {
    expect(utf16Cols("\ud835x", [2, 3])).toEqual([1, 2]);
  });
});
