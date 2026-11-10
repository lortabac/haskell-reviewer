import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { lineIndex, lineText, offsetOf } from "./lines";

// Shared with the server's test (SourceTextTest.hs): same rule.
interface Case {
  name: string;
  text: string;
  lines: string[];
}
const cases = JSON.parse(
  readFileSync(
    join(import.meta.dirname, "../../../fixtures/lines/cases.json"),
    "utf8",
  ),
) as Case[];

describe("lineIndex", () => {
  it.each(cases.map((c) => [c.name, c] as const))("%s", (_, c) => {
    const index = lineIndex(c.text);
    expect(index.count).toBe(c.lines.length);
    const got = c.lines.map((_, i) => lineText(c.text, index, i + 1));
    expect(got).toEqual(c.lines);
    expect(lineText(c.text, index, c.lines.length + 1)).toBe("");
    expect(lineText(c.text, index, 0)).toBe("");
  });

  it("has the shared cases", () => {
    expect(cases.length).toBeGreaterThan(10);
  });

  it("notes a BOM", () => {
    expect(lineIndex("\ufeffa").bom).toBe(true);
    expect(lineIndex("a\ufeff").bom).toBe(false);
  });

  it.each([
    ["", 0],
    ["abc\nab", 3],
    ["\t", 8],
    ["ab\tc", 9],
    ["\t\t", 16],
    ["\ud835\udc65", 1],
    ["\u6f22\u5b57", 4],
    ["a\r\n", 1],
  ])("maxCols of %j is %i", (text, n) => {
    expect(lineIndex(text).maxCols).toBe(n);
  });
});

describe("offsetOf", () => {
  const text = "\ufeffab\r\ncd\n";
  const index = lineIndex(text);

  it.each([
    [1, 0, 1],
    [1, 2, 3],
    // Past the end of the line: the line's end, before CRLF.
    [1, 9, 3],
    [2, 0, 5],
    [2, 2, 7],
    // Clamped into the file.
    [0, 0, 1],
    [9, 9, 7],
    [1, -3, 1],
  ])("line %i col %i is %i", (n, col, off) => {
    expect(offsetOf(index, n, col)).toBe(off);
  });

  it("no lines", () => {
    expect(offsetOf(lineIndex(""), 1, 0)).toBe(0);
    expect(offsetOf(lineIndex("\ufeff"), 1, 0)).toBe(1);
  });
});
