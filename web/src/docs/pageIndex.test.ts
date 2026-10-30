import { describe, expect, it } from "vitest";
import { decodeModulePage, type ParsedModule } from "../api/module";
import { fixture } from "../../test/fixtures";
import { pageIndex } from "./pageIndex";

function page(): ParsedModule {
  const p = decodeModulePage(fixture("module-page")).parse;
  if (p.kind !== "parsed") throw new Error("not parsed");
  return p;
}

describe("pageIndex", () => {
  it("by id: top-level and nested", () => {
    const ix = pageIndex(page());
    for (const id of [
      "type:Shape",
      "con:Circle",
      "field:Shape.width",
      "inst:Eq:Shape",
      "class:Container",
      "val:empty",
      "inst:Elem:[a]",
    ]) {
      const d = ix.byId.get(id);
      expect(d && ix.idOf(d), id).toBe(id);
    }
    expect(ix.byId.get("type:Nope")).toBeUndefined();
  });

  it("a field in two constructors: the first", () => {
    const ix = pageIndex(page());
    const ff = ix.decls.filter((d) => ix.idOf(d) === "field:Fld.ff");
    expect(ff).toHaveLength(2);
    expect(ix.byId.get("field:Fld.ff")).toBe(ff[0]);
  });

  it("Haddock names: v: values, t: types and classes", () => {
    const ix = pageIndex(page());
    const v = (n: string) => {
      const d = ix.byName.value.get(n);
      return d && ix.idOf(d);
    };
    const t = (n: string) => {
      const d = ix.byName.type.get(n);
      return d && ix.idOf(d);
    };
    expect(v("area")).toBe("val:area");
    expect(v("Circle")).toBe("con:Circle");
    expect(v("width")).toBe("field:Shape.width");
    expect(v("Unit")).toBe("pat:Unit");
    expect(v("empty")).toBe("val:empty");
    expect(v("Shape")).toBeUndefined();
    expect(t("Shape")).toBe("type:Shape");
    expect(t("Container")).toBe("class:Container");
    expect(t("Elem")).toBe("type:Elem");
    expect(t("area")).toBeUndefined();
    // type data constructors are types.
    expect(t("Zero")).toBe("con:Zero");
    expect(v("Zero")).toBeUndefined();
  });

  it("parents, top-level, instance homes", () => {
    const ix = pageIndex(page());
    const width = ix.byId.get("field:Shape.width");
    if (width === undefined) throw new Error("no width");
    expect(ix.parentOf(width)?.name).toBe("Rect");
    expect(ix.topOf(width).name).toBe("Shape");
    const shape = ix.byId.get("type:Shape");
    if (shape === undefined) throw new Error("no Shape");
    expect(ix.topOf(shape)).toBe(shape);
    expect(ix.parentOf(shape)).toBeUndefined();
    const home = (id: string) => {
      const d = ix.byId.get(id);
      const h = d && ix.instanceHome.get(d.n);
      return h && ix.idOf(h);
    };
    expect(home("inst:Eq:Shape")).toBe("type:Shape");
    expect(home("inst:Container:[a]")).toBe("class:Container");
    expect(home("inst:Elem:[a]")).toBe("type:Elem");
    expect(home("inst:Family:Shape")).toBe("type:Family");
    expect(home("inst:Num:Bool")).toBeUndefined();
  });

  it("idOf is computed once per declaration", () => {
    const ix = pageIndex(page());
    const d = ix.decls[0];
    if (d === undefined) throw new Error("empty");
    expect(ix.idOf(d)).toBe(ix.idOf(d));
    expect(pageIndex(ix.page)).toBe(ix);
  });

  it("shared pieces, by sender and span", () => {
    const ix = pageIndex(page());
    const per = ix.byId.get("val:perimeter");
    if (per === undefined || !("signature" in per)) throw new Error("no sig");
    const sig = per.signature;
    if (sig === null || !("sharedWith" in sig)) throw new Error("not shared");
    expect(ix.sentSignature(sig.sharedWith)?.names).toEqual([
      "area",
      "perimeter",
    ]);
    const doc = per.docs[0];
    if (doc === undefined || !("sharedWith" in doc)) throw new Error("doc");
    expect(ix.sentDoc(doc)?.span).toEqual(doc.span);
    expect(ix.sentDoc({ ...doc, span: [1, 1, 1, 1] })).toBeUndefined();
    const part = "source" in per ? per.source[1] : undefined;
    if (part === undefined || !("sharedWith" in part)) throw new Error("part");
    expect(ix.sentPart(part)?.text).toBe("area, perimeter :: Shape -> Double");
    const h2 = ix.byId.get("val:helper2");
    const w = h2?.warning;
    if (!w || !("sharedWith" in w)) throw new Error("warning");
    expect(ix.sentWarning(w)?.messages).toEqual(["Use area"]);
    const py = ix.byId.get("field:P2.py");
    if (py === undefined || !("type" in py) || Array.isArray(py.type)) {
      throw new Error("py");
    }
    expect(ix.sentFieldType(py.type.sharedWith)).toEqual([["con", "Double"]]);
    expect(ix.sentSignature(99999)).toBeUndefined();
  });

  it("repeated handles: the first wins; gaps are fine", () => {
    const p = page();
    const decls = p.declarations;
    const shape = decls[0];
    if (shape === undefined || shape.kind === "splice")
      throw new Error("empty");
    // A CPP copy of Shape: same handle, after it.
    const copy = { ...shape, name: "Copy" } as typeof shape;
    const q: ParsedModule = {
      ...p,
      declarations: [shape, copy, ...decls.slice(1)],
    };
    const ix = pageIndex(q);
    expect(ix.byHandle.get(shape.n)?.name).toBe("Shape");
    expect(ix.decls.filter((d) => d.n === shape.n)).toHaveLength(1);
    // Gaps: P2's second field (py) is sent, but handles skip numbers
    // elsewhere; lookups of missing handles are undefined.
    expect(ix.idOfHandle(100000)).toBeUndefined();
  });
});
