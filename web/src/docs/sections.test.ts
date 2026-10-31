/* eslint-disable @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call -- wire JSON edited by hand */
import { describe, expect, it } from "vitest";
import { decodeModulePage, type ParsedModule } from "../api/module";
import { fixture } from "../../test/fixtures";
import { pageIndex } from "./pageIndex";
import { type Block, body, synopsis } from "./sections";

function load(name: string, edit?: (p: Record<string, unknown>) => void) {
  const raw = structuredClone(fixture(name)) as {
    parse: Record<string, unknown>;
  };
  edit?.(raw.parse);
  const p = decodeModulePage(raw).parse;
  if (p.kind !== "parsed") throw new Error("not parsed");
  return p;
}

// A block as short text.
function show(b: Block): string {
  switch (b.kind) {
    case "heading":
      return `# ${b.level} ${b.inlines.map((i) => ("text" in i ? i.text : "")).join("")}`;
    case "doc":
      return `doc @${b.depth}`;
    case "warning":
      return "warning";
    case "decl":
      return `${b.decl.name ?? b.decl.kind} @${b.depth}`;
    case "reexport":
      return `reexport ${b.item.name.occ}`;
    case "module":
      return `module ${b.name.value}`;
    case "nested":
      return `nested ${b.name.occ}`;
  }
}

const run = (p: ParsedModule, exportedOnly = false) =>
  body(p, pageIndex(p), exportedOnly);

const span = [1, 1, 1, 2];
const item = (occ: string, keyword: string | null = null) => ({
  kind: "item",
  span,
  name: { span, qualifier: null, occ, keyword },
  subs: { kind: "none" },
  doc: null,
  warning: null,
});

describe("sections", () => {
  it("mode C: one heading, source order, no instances", () => {
    const b = run(load("module-page"));
    expect(b.mode).toBe("C");
    const shown = b.blocks.map(show);
    expect(shown[0]).toBe("# 1 Declarations");
    expect(shown.slice(1, 4)).toEqual(["Shape @1", "Box @1", "area @1"]);
    expect(shown).not.toContain("instance @1");
    expect(b.instances.map((d) => d.kind)).toEqual(["instance"]);
  });

  it("mode A: headings and chunks in place", () => {
    const b = run(load("module-page-sections"));
    expect(b.mode).toBe("A");
    expect(b.blocks.map(show)).toEqual([
      "# 1 Body heading",
      "doc @1",
      "x @1",
      "y @1",
    ]);
  });

  it("mode B: export order, keywords, nested names, self module", () => {
    const p = load("module-page", (p) => {
      p.exports = [
        {
          kind: "section",
          level: 1,
          heading: { span, inlines: [{ kind: "text", text: "S" }] },
        },
        item("Unit", "pattern"),
        item("area"),
        item("area"),
        item("empty"),
        item("Other"),
        { kind: "module", name: { span, value: "Page" }, warning: null },
      ];
    });
    const b = run(p);
    expect(b.mode).toBe("B");
    const shown = b.blocks.map(show);
    expect(shown.slice(0, 5)).toEqual([
      "# 1 S",
      "Unit @1",
      "area @1",
      "nested empty",
      "reexport Other",
    ]);
    // module Page: the rest it exports, then what it doesn't.
    expect(shown[5]).toBe("Shape @1");
    const not = shown.indexOf("# 1 Private");
    expect(not).toBeGreaterThan(5);
    expect(shown.slice(not + 1)).toContain("P2 @1");
    expect(shown.filter((s) => s === "area @1")).toHaveLength(1);
  });

  it("mode A: a pragma part far above doesn't move a declaration", () => {
    const p = load("module-page-sections", (p) => {
      const ds = p.declarations as { name: string; source: unknown[] }[];
      // infixr 5 `y` at the top, before the body heading.
      ds[1]?.source.push({
        span: [3, 1, 3, 13],
        role: "pragma",
        text: "infixr 5 `y`",
      });
      // And y listed first: placed by position, not by list order.
      ds.reverse();
    });
    expect(run(p).blocks.map(show)).toEqual([
      "# 1 Body heading",
      "doc @1",
      "x @1",
      "y @1",
    ]);
  });

  it("mode A: heading levels never skip one", () => {
    const p = load("module-page-sections", (p) => {
      (p.docItems as { kind: string; level: number }[])[0]!.level = 3;
    });
    expect(run(p).blocks.map(show)[0]).toBe("# 1 Body heading");
  });

  it("mode B: a nested name by namespace", () => {
    const p = load("module-page", (p) => {
      p.exports = [
        {
          kind: "section",
          level: 1,
          heading: { span, inlines: [{ kind: "text", text: "S" }] },
        },
        // Elem is a type, upper case: found among types.
        item("Elem"),
        // gval is a field: a value.
        item("gval"),
        // Circle is a constructor (a value): not found as a type.
        item("Circle"),
        // pattern GInt: a constructor, among values.
        item("GInt", "pattern"),
      ];
    });
    expect(run(p).blocks.map(show).slice(1, 5)).toEqual([
      "nested Elem",
      "nested gval",
      "reexport Circle",
      "nested GInt",
    ]);
  });

  it("mode B: listed twice keeps each doc; module Self skips named ones", () => {
    const doc = (text: string) => ({
      span,
      doc: {
        blocks: [{ kind: "paragraph", inlines: [{ kind: "text", text }] }],
        since: null,
      },
    });
    const p = load("module-page", (p) => {
      p.exports = [
        {
          kind: "section",
          level: 1,
          heading: { span, inlines: [{ kind: "text", text: "S" }] },
        },
        { ...item("area"), doc: doc("one") },
        { kind: "module", name: { span, value: "Page" }, warning: null },
        { ...item("area"), doc: doc("two") },
        item("Shape"),
      ];
    });
    const shown = run(p).blocks.map(show);
    expect(shown.slice(1, 4)).toEqual(["area @1", "doc @1", "Box @1"]);
    expect(shown.filter((s) => s === "doc @1")).toHaveLength(2);
    // Shape is named later: it stays there, last.
    expect(shown.indexOf("Shape @1")).toBe(shown.indexOf("# 1 Private") - 1);
  });

  it("mode C with nothing: no blocks (the page says No declarations.)", () => {
    const p = load("module-page", (p) => {
      p.declarations = [];
      p.pragmas = [];
    });
    expect(run(p).blocks).toEqual([]);
  });

  it("exportedOnly: a class kept for a method exported alone", () => {
    const p = load("module-page", (p) => {
      const c = (p.declarations as Record<string, unknown>[]).find(
        (d) => d.name === "Container",
      ) as { exported: boolean; methods: { exported: boolean }[] };
      c.exported = false;
      for (const m of c.methods) m.exported = false;
      c.methods[0]!.exported = true;
    });
    expect(run(p, true).blocks.map(show)).toContain("Container @1");
  });

  it("exportedOnly: instances attached to a hidden type are hidden too (D-204)", () => {
    const p = load("module-page", (p) => {
      const box = (p.declarations as Record<string, unknown>[]).find(
        (d) => d.name === "Box",
      ) as { exported: boolean; constructors: { exported: boolean }[] };
      box.exported = false;
      box.constructors[0]!.exported = false;
    });
    const b = run(p, true);
    expect(b.blocks.map(show)).not.toContain("Box @1");
    const ns = (ds: typeof b.instances) =>
      ds.map((d) => (d.kind === "splice" ? -1 : d.n));
    // Box's (9, 51) and V's (42, 43) are attached, all their listers
    // hidden: gone. 21 is a true orphan (attached: false): stays.
    expect(ns(b.instances)).toEqual([21]);
    expect(ns(run(p).instances)).toEqual([21]);
  });

  it("exportedOnly: home is the first shown lister", () => {
    const p = load("module-page", (p) => {
      const ds = p.declarations as Record<string, any>[];
      const shape = ds.find((d) => d.name === "Shape")!;
      shape.exported = false;
      for (const c of shape.constructors) {
        c.exported = false;
        for (const f of c.fields) f.exported = false;
      }
      // instance Ord Shape (20): Shape lists it first, Box later.
      ds.find((d) => d.name === "Box")!.instances.push(20);
    });
    const b = run(p, true);
    expect(b.homes.get(20)?.name).toBe("Box");
    const ns = b.instances.map((d) => (d.kind === "splice" ? -1 : d.n));
    expect(ns).not.toContain(20);
    // Shape's derived ones: Shape is hidden and is their only lister,
    // so they're hidden too (D-204), not in the block.
    expect(ns).not.toContain(5);
  });

  it("exportedOnly: no Private heading, no hidden ones", () => {
    const b = run(load("module-page"), true);
    const shown = b.blocks.map(show);
    expect(shown).not.toContain("P2 @1");
    expect(shown).not.toContain("splice @1");
    expect(shown).toContain("Shape @1");
  });

  it("synopsis: types, classes, values, pattern synonyms", () => {
    const p = load("module-page");
    const s = synopsis(p);
    expect(new Set(s.map((d) => d.kind))).toEqual(
      new Set(["type", "class", "value", "patsyn"]),
    );
    expect(s).toHaveLength(19);
    expect(synopsis(p, true).map((d) => d.name)).not.toContain("P2");
  });

  it("synopsis uses the same filter as the body (C1): a kept private class", () => {
    const p = load("module-page", (p) => {
      const c = (p.declarations as Record<string, unknown>[]).find(
        (d) => d.name === "Container",
      ) as { exported: boolean; methods: { exported: boolean }[] };
      c.exported = false;
      for (const m of c.methods) m.exported = false;
      c.methods[0]!.exported = true;
    });
    // Container is kept in the body (a method is exported alone): the
    // synopsis must agree, not just check d.exported.
    expect(run(p, true).blocks.map(show)).toContain("Container @1");
    expect(synopsis(p, true).map((d) => d.name)).toContain("Container");
  });
});
