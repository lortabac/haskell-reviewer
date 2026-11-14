import { describe, expect, it } from "vitest";
import { fixture } from "../../test/fixtures";
import { decodeModulePage, type ParsedModule } from "../api/module";
import { declCopyText, declLine, hasCopyText } from "./copyText";
import { pageIndex } from "./pageIndex";

function page(): ParsedModule {
  const p = decodeModulePage(fixture("module-page")).parse;
  if (p.kind !== "parsed") throw new Error("not parsed");
  return p;
}

describe("declCopyText", () => {
  const p = page();
  const ix = pageIndex(p);
  const get = (id: string) => {
    const d = ix.byId.get(id);
    if (d === undefined) throw new Error(id);
    return d;
  };

  it("a name is qualified by the module; nested too", () => {
    expect(declCopyText(get("type:Shape"), ix, "Shapes")).toBe("Shapes.Shape");
    expect(declCopyText(get("con:Circle"), ix, "Shapes")).toBe("Shapes.Circle");
    expect(declCopyText(get("field:Shape.width"), ix, "Shapes")).toBe(
      "Shapes.width",
    );
  });

  it("no header: Main", () => {
    expect(declCopyText(get("type:Shape"), ix, "Main")).toBe("Main.Shape");
  });

  it("an instance: its head, no module", () => {
    const d = get("inst:Eq:Shape");
    expect(declCopyText(d, ix, "Shapes")).toMatch(/^instance Eq /);
    expect(hasCopyText(d)).toBe(true);
  });

  it("a family instance: its head", () => {
    const fi = ix.decls.find((d) => "kind" in d && d.kind === "faminst");
    expect(fi).toBeDefined();
    if (fi === undefined) return;
    expect(declCopyText(fi, ix, "Shapes")).toMatch(/ instance /);
  });

  it("a splice: nothing", () => {
    const splice = {
      kind: "splice",
      line: 7,
    } as unknown as Parameters<typeof declCopyText>[0];
    expect(declCopyText(splice, ix, "Shapes")).toBeUndefined();
    expect(hasCopyText(splice)).toBe(false);
  });

  it("declLine: the code, else the name's line", () => {
    expect(declLine(get("type:Shape"))).toBeGreaterThan(0);
    const c = get("con:Circle");
    expect(declLine(c)).toBe(c.line);
  });
});
