import { describe, expect, it } from "vitest";
import { parseBoxQuery, quoted, textTooLong } from "./query";

describe("parseBoxQuery", () => {
  it.each([
    ["", { kind: "name", text: "" }],
    ["map", { kind: "name", text: "map" }],
    ["  map ", { kind: "name", text: "map" }],
    ['a"b', { kind: "name", text: 'a"b' }],
    ['"', { kind: "open", text: "" }],
    ['"fo', { kind: "open", text: "fo" }],
    [' "fo ', { kind: "open", text: "fo " }],
    ['"a ', { kind: "open", text: "a " }],
    ['" ', { kind: "open", text: " " }],
    ['""', { kind: "text", text: "" }],
    ['"foo"', { kind: "text", text: "foo" }],
    [' "foo" ', { kind: "text", text: "foo" }],
    ['" a b "', { kind: "text", text: " a b " }],
    ['"a "b"', { kind: "text", text: 'a "b' }],
    ['"""', { kind: "text", text: '"' }],
    ['"foo "  ', { kind: "text", text: "foo " }],
    ["“foo”", { kind: "name", text: "“foo”" }],
  ])("%j", (raw, want) => {
    expect(parseBoxQuery(raw)).toEqual(want);
  });

  it("quoted is the inverse for text", () => {
    for (const t of ["", "foo", ' a "b '])
      expect(parseBoxQuery(quoted(t))).toEqual({ kind: "text", text: t });
  });
});

describe("textTooLong", () => {
  it("counts code points, 128 is fine", () => {
    expect(textTooLong("a".repeat(128))).toBe(false);
    expect(textTooLong("𝑥".repeat(128))).toBe(false);
    expect(textTooLong("a".repeat(129))).toBe(true);
  });
});
