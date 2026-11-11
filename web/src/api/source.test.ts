import { describe, expect, it } from "vitest";
import { DecodeError } from "./decode";
import {
  decodeSourceNotFound,
  decodeSourceView,
  expandRuns,
  packRuns,
} from "./source";
import { fixture } from "../../test/fixtures";

const head = {
  generation: 1,
  path: "src/A.hs",
  ref: "A",
  tier: { kind: "syntactic", reason: "no-hie" },
  partial: false,
};

const lexed = {
  ...head,
  highlight: "lexed",
  // Line 1: 1-7 keyword; 8-9 linked to target 1. Line 2: a directive
  // line. Line 3: 1-2.
  runs: [
    1, 1, 0, 7, 0, 0, 0, 8, 0, 9, 1, 1, 1, 1, 1, 1, 8, 0, 1, 1, 0, 2, 2, 0,
  ],
  regions: [0, 2, 2],
  targets: [["local", 3, 1]],
  outline: [["value", "val:x", "x", 3, 3, 3]],
};

describe("source view", () => {
  it("takes a good one, plain or lexed", () => {
    expect(decodeSourceView(lexed)).toEqual(lexed);
    const plain = { ...head, ref: null, highlight: "plain" };
    expect(decodeSourceView(plain)).toEqual(plain);
  });

  it("takes every target shape", () => {
    const targets = [
      ["local", 1, 1],
      ["decl", "A", "src/A.hs", null, "val:x", ["ambiguous"], 2, null],
      ["decl", "A", "src/A.hs", 3, "val:y", [], 1, "y :: Int"],
      ["module", "B", null, ["not-ready", "this-module"], 1],
      ["external", "Data.List", "base", "listed", 1, "sort +module:Data.List"],
    ];
    const v = { ...lexed, targets };
    expect(decodeSourceView(v)).toEqual(v);
  });

  it("rejects bad shapes", () => {
    const withRuns = (runs: number[]) => ({ ...lexed, runs });
    for (const bad of [
      null,
      { ...lexed, generation: -1 },
      { ...lexed, ref: undefined },
      { ...lexed, tier: { kind: "precise", reason: "no-hie" } },
      { ...lexed, partial: "no" },
      { ...lexed, highlight: "fancy" },
      { ...lexed, runs: undefined },
      // Not 6 a run.
      withRuns([1, 1, 0, 7, 0]),
      // A class past the last.
      withRuns([1, 1, 0, 7, 9, 0]),
      // A link past the table.
      withRuns([1, 1, 0, 7, 0, 2]),
      // Overlapping runs.
      withRuns([1, 1, 0, 7, 0, 0, 0, 5, 0, 9, 1, 0]),
      // Ends before it starts, or empty.
      withRuns([1, 5, 0, 3, 0, 0]),
      withRuns([1, 5, 0, 5, 0, 0]),
      // A region run not of whole lines.
      withRuns([1, 1, 1, 2, 7, 0]),
      withRuns([1, 2, 1, 1, 8, 0]),
      withRuns([1, -1, 0, 3, 0, 0]),
      { ...lexed, regions: [0, 2] },
      { ...lexed, regions: [3, 2, 2] },
      { ...lexed, regions: [0, 3, 2] },
      { ...lexed, targets: [["local", 1]] },
      { ...lexed, targets: [["global", 1, 1]] },
      { ...lexed, targets: [["decl", "A", "a", 1, "x", ["guess"], 1, null]] },
      // No sig, or not text.
      { ...lexed, targets: [["decl", "A", "a", 1, "x", [], 1]] },
      { ...lexed, targets: [["decl", "A", "a", 1, "x", [], 1, 3]] },
      { ...lexed, targets: [["module", "A", 1, [], 1]] },
      { ...lexed, targets: [["module", "A", null, [], 0]] },
      { ...lexed, targets: [["decl", "A", "a", 1, "x", [], 0, null]] },
      { ...lexed, targets: [["external", "M", null, "listed", 0, "q"]] },
      { ...lexed, targets: [["external", "M", null, "maybe", 1, "q"]] },
      { ...lexed, outline: [["splice", "x", "x", 1, 1, 1]] },
      { ...lexed, outline: [["instance", "inst:C:Int", null, 1, 1, 1]] },
      { ...lexed, outline: [["value", "val:x", "x", 1, 1]] },
    ]) {
      expect(() => decodeSourceView(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("not found", () => {
    const good = { generation: 1, kind: "not-found", what: "file" };
    expect(decodeSourceNotFound(good)).toEqual(good);
    for (const bad of [
      { ...good, what: "module" },
      { ...good, kind: "gone" },
      { kind: "not-found", what: "file" },
    ]) {
      expect(() => decodeSourceNotFound(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });
});

describe("expandRuns", () => {
  it("makes lines absolute", () => {
    expect(expandRuns(lexed.runs)).toEqual([
      { startLine: 1, startCol: 1, endLine: 1, endCol: 7, cls: 0, link: 0 },
      { startLine: 1, startCol: 8, endLine: 1, endCol: 9, cls: 1, link: 1 },
      { startLine: 2, startCol: 1, endLine: 3, endCol: 1, cls: 8, link: 0 },
      { startLine: 3, startCol: 1, endLine: 3, endCol: 2, cls: 2, link: 0 },
    ]);
  });

  it("round-trips with packRuns, on every fixture", () => {
    for (const name of [
      "source-parsed",
      "source-cpp",
      "source-partial",
      "source-warm-up",
    ]) {
      const v = decodeSourceView(fixture(name));
      if (v.highlight !== "lexed") throw new Error(name);
      expect(packRuns(expandRuns(v.runs)), name).toEqual(v.runs);
    }
  });
});
