import { describe, expect, it } from "vitest";
import {
  createFinder,
  inLine,
  type Matches,
  nearest,
  noMatches,
  status,
  step,
} from "./find";
import { lineIndex } from "./lines";

function run(text: string, q: string): Matches {
  return createFinder(text, lineIndex(text)).search(q);
}

// "line:from-to" of each hit.
function spots(m: Matches): string[] {
  return Array.from(
    { length: m.count },
    (_, i) => `${m.line[i]}:${m.from[i]}-${m.to[i]}`,
  );
}

describe("search", () => {
  it("is literal", () => {
    expect(spots(run("a.c\nabc\n", "a.c"))).toEqual(["1:0-3"]);
    expect(run("abc\n", "").count).toBe(0);
  });

  it("smart case: lower case ignores case, a capital is exact", () => {
    const t = "Foo foo FOO\n";
    expect(spots(run(t, "foo"))).toEqual(["1:0-3", "1:4-7", "1:8-11"]);
    expect(spots(run(t, "Foo"))).toEqual(["1:0-3"]);
    expect(run(t, "fOo").count).toBe(0);
  });

  it("keeps UTF-16 offsets when lower case changes the length", () => {
    // U+0130 lowercases to two units: it stays as it is.
    expect(spots(run("İx x\n", "x"))).toEqual(["1:1-2", "1:3-4"]);
  });

  it("folds each char alone: no final sigma", () => {
    // "ΑΣ" would give "ας" as a whole string.
    expect(spots(run("\u0391\u03a3\n", "\u03b1\u03c3"))).toEqual(["1:0-2"]);
    expect(run("\u0391\u03a3\n", "\u03b1\u03c2").count).toBe(0);
  });

  it("counts astral characters as two columns", () => {
    expect(spots(run("a\u{1d465}y\n", "y"))).toEqual(["1:3-4"]);
  });

  it("skips the BOM, the CR of CRLF and anything across a break", () => {
    expect(spots(run("﻿ab\r\nab\n", "ab"))).toEqual(["1:0-2", "2:0-2"]);
    expect(run("ab\r\ncd\n", "b\r").count).toBe(0);
    expect(run("ab\ncd\n", "b\nc").count).toBe(0);
    expect(run("ab\ncd\n", "\n").count).toBe(0);
  });

  it("a lone CR is text", () => {
    expect(spots(run("one\rhalf\n", "half"))).toEqual(["1:4-8"]);
  });

  it("tabs match as themselves", () => {
    expect(spots(run("a\tb\n", "\tb"))).toEqual(["1:1-3"]);
  });

  it("does not overlap", () => {
    expect(spots(run("aaaaa\n", "aa"))).toEqual(["1:0-2", "1:2-4"]);
  });

  it("finds a hit after a dropped one", () => {
    // "a\na" crosses a break; the next "a\n" start still counts.
    expect(spots(run("a\r\na\n", "a"))).toEqual(["1:0-1", "2:0-1"]);
  });

  it("is fast and complete on 20k lines", () => {
    const text = Array.from({ length: 20000 }, (_, i) => `l${i + 1}`)
      .join("\n")
      .concat("\n");
    const m = run(text, "l");
    expect(m.count).toBe(20000);
    expect(m.line[19999]).toBe(20000);
    expect(spots(run(text, "l19999"))).toEqual(["19999:0-6"]);
  });
});

describe("moving", () => {
  const m = run("ab\nxx\nab ab\nab\n", "ab");
  // hits: 1:0, 3:0, 3:3, 4:0

  it("inLine", () => {
    expect(inLine(m, 3)).toEqual([1, 3]);
    expect(inLine(m, 2)).toEqual([1, 1]);
    expect(inLine(m, 9)).toEqual([4, 4]);
  });

  it("nearest: at or after, wrapping", () => {
    expect(nearest(m, 1, 0)).toBe(0);
    expect(nearest(m, 1, 1)).toBe(1);
    expect(nearest(m, 3, 1)).toBe(2);
    expect(nearest(m, 4, 1)).toBe(0);
    expect(nearest(noMatches, 1, 0)).toBe(-1);
  });

  it("step wraps both ways", () => {
    expect(step(m, 3, 1)).toBe(0);
    expect(step(m, 0, -1)).toBe(3);
    expect(step(noMatches, 0, 1)).toBe(-1);
  });

  it("status", () => {
    expect(status(m, 1)).toBe("Match 2 of 4, line 3");
    expect(status(noMatches, -1)).toBe("No matches");
  });
});
