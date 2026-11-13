import { describe, expect, it } from "vitest";
import type { OutlineEntry } from "../api/source";
import { declAtLine, outlineStep } from "./outline";

// kind, id, label, name line, first line, last line.
const e = (id: string, line: number, first: number, last: number) =>
  ["val", id, id, line, first, last] as unknown as OutlineEntry;

const entries = [
  e("val:a", 3, 2, 5),
  e("type:T", 8, 8, 20),
  e("con:C", 10, 10, 10),
  e("val:b", 30, 29, 31),
];

describe("outlineStep", () => {
  it("next and previous by name line", () => {
    expect(outlineStep(entries, 1, 1)).toBe(3);
    expect(outlineStep(entries, 3, 1)).toBe(8);
    expect(outlineStep(entries, 9, 1)).toBe(10);
    expect(outlineStep(entries, 30, 1)).toBeUndefined();
    expect(outlineStep(entries, 30, -1)).toBe(10);
    expect(outlineStep(entries, 9, -1)).toBe(8);
    expect(outlineStep(entries, 3, -1)).toBeUndefined();
  });

  it("no entries: nothing", () => {
    expect(outlineStep([], 5, 1)).toBeUndefined();
  });
});

describe("declAtLine", () => {
  it("the smallest entry that holds the line", () => {
    expect(declAtLine(entries, 4)?.[1]).toBe("val:a");
    expect(declAtLine(entries, 10)?.[1]).toBe("con:C");
    expect(declAtLine(entries, 15)?.[1]).toBe("type:T");
  });

  it("between entries: the one above; before all: none", () => {
    expect(declAtLine(entries, 25)?.[1]).toBe("type:T");
    expect(declAtLine(entries, 1)).toBeUndefined();
  });
});
