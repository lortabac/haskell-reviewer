import { describe, expect, it } from "vitest";
import type { CodeToken } from "../api/code";
import { decodeModulePage, type ModulePage } from "../api/module";
import { fixture } from "../../test/fixtures";
import { type NameInline, type Overlay, relink, type Relinked } from "./relink";

type Json = Record<string, unknown>;
type Raw = Json & { parse: Json & { targets: unknown[] } };

// The Page fixture: Double is target 1 (code tokens), Missing target
// 10 (a doc name).
const raw = (): Raw => structuredClone(fixture("module-page")) as Raw;
const page = (r: Raw): ModulePage => decodeModulePage(r);

const start = (p: ModulePage): Overlay => ({
  partial: p.partial,
  targets: p.parse.kind === "parsed" ? p.parse.targets : [],
  links: new Map(),
});

const decls = (r: Raw) => r.parse.declarations as Json[];
// area's signature: [..., ["con", "Double", null, 1]] at 11.
const areaSig = (r: Raw) =>
  (decls(r)[5]!.signature as { code: unknown[] }).code;
const missing = (r: Raw) =>
  (
    (r.parse.header as Json).haddock as {
      doc: { blocks: { inlines: Json[] }[] };
    }
  ).doc.blocks[0]!.inlines[3]!;

const parsed = (p: ModulePage) => {
  if (p.parse.kind !== "parsed") throw new Error("not parsed");
  return p.parse;
};
const shownSig = (p: ModulePage) => {
  const d = parsed(p).declarations[5];
  if (d?.kind !== "value" || d.signature === null || !("code" in d.signature))
    throw new Error("no sig");
  return d.signature.code;
};
const shownMissing = (p: ModulePage) => {
  const h = parsed(p).header.haddock;
  return h!.doc.blocks[0]!.kind === "paragraph"
    ? (h!.doc.blocks[0]!.inlines[3] as NameInline)
    : undefined;
};

function changed(r: ReturnType<typeof relink>): Relinked {
  if (typeof r === "string") throw new Error(`got ${r}`);
  return r;
}

describe("relink", () => {
  it("an equal answer: same", () => {
    const p = page(raw());
    expect(relink(p, start(p), page(raw()))).toBe("same");
  });

  it("only partial: no changes, the partial flag", () => {
    const a = raw();
    a.partial = true;
    const p = page(a);
    const r = changed(relink(p, start(p), page(raw())));
    expect(r.partial).toBe(false);
    expect(r.changes.size).toBe(0);
    expect(r.targets).toBe(start(p).targets);
  });

  it("a token loses, gains and changes its link", () => {
    const a = raw();
    areaSig(a)[11] = ["con", "Double"];
    const p = page(a);
    const tok = shownSig(p)[11]!;
    // Gains Double's link.
    const r = changed(relink(p, start(p), page(raw())));
    expect(r.changes.get(tok)).toEqual(["con", "Double", null, 1]);
    expect(r.changes.size).toBe(1);
    // Loses it again, over the overlay.
    const now: Overlay = {
      partial: r.partial,
      targets: r.targets,
      links: new Map(r.changes),
    };
    const r2 = changed(relink(p, now, page(a)));
    expect(r2.changes.get(tok)).toEqual(["con", "Double"]);
    // Changes: to Int's target (8).
    const c = raw();
    areaSig(c)[11] = ["con", "Double", null, 8];
    const r3 = changed(relink(p, now, page(c)));
    expect(r3.changes.get(tok)).toEqual(["con", "Double", null, 8]);
    // Over the overlay, the answer it already shows: same.
    expect(relink(p, now, page(raw()))).toBe("same");
  });

  it("a doc name's link", () => {
    const a = raw();
    missing(a).link = null;
    const p = page(a);
    const n = shownMissing(p)!;
    const r = changed(relink(p, start(p), page(raw())));
    expect(r.changes.get(n)).toEqual({ ...n, link: 10 });
  });

  it("reordered targets, links following: same", () => {
    const b = raw();
    const ts = b.parse.targets;
    const k = ts.length;
    b.parse.targets = [...ts].reverse();
    const flip = (v: unknown): void => {
      if (Array.isArray(v)) {
        if (v.length === 4 && v[2] === null) v[3] = k + 1 - (v[3] as number);
        else v.forEach(flip);
      } else if (typeof v === "object" && v !== null) {
        for (const [key, x] of Object.entries(v)) {
          if (key === "targets") continue;
          if (key === "link" && typeof x === "number") {
            (v as Json).link = k + 1 - x;
          } else flip(x);
        }
      }
    };
    flip(b.parse);
    const p = page(raw());
    expect(relink(p, start(p), page(b))).toBe("same");
  });

  it("a new target (a guess becomes real): appended, links remapped", () => {
    const b = raw();
    const real = ["external", "Prelude", null, "listed", 1, "Double"];
    b.parse.targets = [real, ...b.parse.targets];
    // Every link moves up by one; area's Double goes to the new one.
    const bump = (v: unknown): void => {
      if (Array.isArray(v)) {
        if (v.length === 4 && v[2] === null) v[3] = (v[3] as number) + 1;
        else v.forEach(bump);
      } else if (typeof v === "object" && v !== null) {
        for (const [key, x] of Object.entries(v)) {
          if (key === "targets") continue;
          if (key === "link" && typeof x === "number") (v as Json).link = x + 1;
          else bump(x);
        }
      }
    };
    bump(b.parse);
    areaSig(b)[11] = ["con", "Double", null, 1];
    const p = page(raw());
    const s = start(p);
    const r = changed(relink(p, s, page(b)));
    expect(r.targets.length).toBe(s.targets.length + 1);
    expect(r.targets.slice(0, s.targets.length)).toEqual(s.targets);
    expect(r.targets.at(-1)).toEqual(real);
    expect(r.changes.size).toBe(1);
    expect(r.changes.get(shownSig(p)[11]!)).toEqual<CodeToken>([
      "con",
      "Double",
      null,
      s.targets.length + 1,
    ]);
  });

  it("anything else differs: other", () => {
    const p = page(raw());
    const cases: ((r: Raw) => void)[] = [
      (r) => (r.generation = 2),
      (r) => (r.id = "x"),
      (r) => (r.ref = "Other"),
      (r) => (r.path = "Other.hs"),
      (r) => (decls(r)[5]!.line = 99),
      (r) => (areaSig(r)[11] = ["con", "Float", null, 1]),
      (r) => (missing(r).written = "Gone"),
      (r) => decls(r).pop(),
      (r) =>
        (r.parse = {
          kind: "text-only",
          reason: { kind: "too-slow" },
          warnings: [],
        } as never),
    ];
    for (const f of cases) {
      const b = raw();
      f(b);
      expect(relink(p, start(p), page(b))).toBe("other");
    }
  });
});
