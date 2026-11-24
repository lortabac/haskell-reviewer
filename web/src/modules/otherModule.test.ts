import { describe, expect, it } from "vitest";
import type { FileMark } from "../files/changes";
import { otherModule } from "./otherModule";
import type { RefInfo } from "./store";

function info(
  ref: string,
  id: string,
  path: string | null,
  change: RefInfo["change"] = null,
): RefInfo {
  return { ref, id, name: ref, path, change, groups: [] };
}

// By ref and by id, as indexRefs.
function index(...xs: RefInfo[]): Map<string, RefInfo> {
  const m = new Map<string, RefInfo>();
  for (const x of xs) {
    if (!m.has(x.ref)) m.set(x.ref, x);
    if (!m.has(x.id)) m.set(x.id, x);
  }
  return m;
}

const ok = (refs: Map<string, RefInfo>) => ({ kind: "ok" as const, refs });

const a = info("A", "p:lib:A", "src/A.hs", "modified");

function run(o: Partial<Parameters<typeof otherModule>[0]> = {}) {
  return otherModule({
    side: "head",
    ref: "A",
    here: index(a),
    other: ok(index(info("A", "p:lib:A", "src/A.hs", "modified"))),
    marks: undefined,
    ...o,
  });
}

describe("otherModule", () => {
  it("by id, with the other tree's ref", () => {
    expect(run()).toEqual({ kind: "go", ref: "A" });
    // A is ambiguous on the other side: its ref there is the id.
    expect(
      run({
        other: ok(
          index(
            info("p:lib:A", "p:lib:A", "src/A.hs"),
            info("q:lib:A", "q:lib:A", "q/A.hs"),
          ),
        ),
      }),
    ).toEqual({ kind: "go", ref: "p:lib:A" });
  });

  it("an id that is only some other module's ref there: not a match", () => {
    expect(run({ other: ok(index(info("p:lib:A", "x", "x.hs"))) })).toEqual({
      kind: "off",
      why: "Only on head.",
    });
  });

  it("only here: off", () => {
    const c = info("C", "p:lib:C", "src/C.hs", "added");
    expect(run({ ref: "C", here: index(a, c) })).toEqual({
      kind: "off",
      why: "Only on head.",
    });
  });

  it("a renamed file: the module at the other path", () => {
    const ren = info("Ren", "ren/New.hs", "ren/New.hs", "added");
    const marks = new Map<string, FileMark>([
      [
        "ren/New.hs",
        { status: "renamed", other: "ren/Old.hs", similarity: 90 },
      ],
    ]);
    expect(
      run({
        ref: "Ren",
        here: index(ren),
        other: ok(index(info("Ren", "ren/Old.hs", "ren/Old.hs", "removed"))),
        marks,
      }),
    ).toEqual({ kind: "go", ref: "Ren" });
    // Pairing failed (no marks): no rename to follow.
    expect(
      run({
        ref: "Ren",
        here: index(ren),
        other: ok(index(info("Ren", "ren/Old.hs", "ren/Old.hs", "removed"))),
        marks: undefined,
      }),
    ).toEqual({ kind: "off", why: "Only on head." });
  });

  it("the other tree loading: off, and says so", () => {
    expect(run({ other: { kind: "loading" } })).toEqual({
      kind: "off",
      why: "Loading base's modules…",
    });
  });

  it("the other tree failed: by id unless added or removed", () => {
    expect(run({ other: { kind: "failed" } })).toEqual({
      kind: "go",
      ref: "p:lib:A",
    });
    const c = info("C", "p:lib:C", "src/C.hs", "added");
    expect(
      run({ ref: "C", here: index(c), other: { kind: "failed" } }),
    ).toEqual({ kind: "off", why: "Only on head." });
  });

  it("not in this side's tree: the same ref", () => {
    expect(run({ ref: "Z" })).toEqual({ kind: "go", ref: "Z" });
  });
});
