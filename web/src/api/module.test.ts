import { describe, expect, it } from "vitest";
import { decodeCode } from "./code";
import { DecodeError } from "./decode";
import {
  byHandle,
  decodeModulePage,
  docsHref,
  findDecl,
  idText,
  type AnyDecl,
} from "./module";
import { decodeBlock, decodeDoc, decodeInline } from "./richtext";
import { fixture } from "../../test/fixtures";

function page() {
  const p = decodeModulePage(fixture("module-page")).parse;
  if (p.kind !== "parsed") throw new Error("not parsed");
  return p;
}

describe("findDecl", () => {
  it("finds top-level and nested declarations", () => {
    const p = page();
    for (const id of [
      "type:Shape",
      "con:Rect",
      "field:Shape.width",
      "class:Container",
      "val:empty",
      "type:Elem",
      "inst:Container:[a]",
      "inst:Elem:[a]",
      "field:G.gval",
      "val:perimeter",
    ]) {
      const d = findDecl(p, id);
      expect(d === "not-found" ? d : idText(p, d.id), id).toBe(id);
    }
  });

  it("not found is a value, not an error", () => {
    expect(findDecl(page(), "type:Nope")).toBe("not-found");
    expect(findDecl(page(), "")).toBe("not-found");
  });
});

describe("docsHref", () => {
  it("percent-encodes the ref by segment and the anchor whole", () => {
    expect(docsHref("Data.Map")).toBe("/docs/tree/Data.Map");
    expect(docsHref("scripts/build-docs.hs", "v:x")).toBe(
      "/docs/tree/scripts/build-docs.hs#v%3Ax",
    );
    expect(docsHref("a#b/C d.hs")).toBe("/docs/tree/a%23b/C%20d.hs");
    expect(docsHref("A", "inst:Show:Maybe a")).toBe(
      "/docs/tree/A#inst%3AShow%3AMaybe%20a",
    );
    expect(docsHref("A", "#%/?")).toBe("/docs/tree/A#%23%25%2F%3F");
    expect(docsHref("A", "val:é→")).toBe("/docs/tree/A#val%3A%C3%A9%E2%86%92");
  });
});

describe("shared pieces", () => {
  it("every handle names one declaration; shared pieces are on it", () => {
    const p = page();
    const decls = byHandle(p);
    // Handles are unique.
    expect(decls.size).toBe(new Set([...decls.keys()]).size);
    const spans = (d: AnyDecl): unknown[] => [
      ...d.docs.map((x) => x.span),
      ...(d.warning ? [d.warning.span] : []),
      ...("source" in d ? d.source.map((x) => x.span) : []),
    ];
    let count = 0;
    // Each shared doc, warning or source part: the sender has it, at the
    // same span, sent (not shared again).
    for (const d of decls.values()) {
      const pieces = [
        ...d.docs,
        ...(d.warning ? [d.warning] : []),
        ...("source" in d ? d.source : []),
      ];
      for (const x of pieces) {
        if (!("sharedWith" in x)) continue;
        count++;
        const sender = decls.get(x.sharedWith);
        expect(sender, JSON.stringify(x)).toBeDefined();
        if (!sender) continue;
        expect(spans(sender)).toContainEqual(x.span);
        const there = [
          ...sender.docs,
          ...(sender.warning ? [sender.warning] : []),
          ...("source" in sender ? sender.source : []),
        ].find((y) => JSON.stringify(y.span) === JSON.stringify(x.span));
        expect(there && "sharedWith" in there).toBe(false);
      }
    }
    expect(count).toBeGreaterThan(5);
    // A shared signature points to the first name.
    const area = findDecl(p, "val:area");
    const g = findDecl(p, "val:perimeter");
    expect(g !== "not-found" && "signature" in g && g.signature).toEqual({
      sharedWith: area !== "not-found" ? area.n : -1,
    });
  });

  it("a field in two constructors: the shared doc is the second's", () => {
    const p = page();
    const decls = byHandle(p);
    const fs = [...decls.values()].filter(
      (d) => idText(p, d.id) === "field:Fld.ff",
    );
    expect(fs.length).toBe(2);
    const fh = findDecl(p, "field:Fld.fh");
    expect(fh).not.toBe("not-found");
    if (fh === "not-found") return;
    const doc = fh.docs[0];
    expect(doc && "sharedWith" in doc).toBe(true);
    if (!doc || !("sharedWith" in doc)) return;
    // Not the first ff (FA's, no doc): the second (FB's), with the doc.
    expect(doc.sharedWith).toBe(fs[1]?.n);
    const sent = decls.get(doc.sharedWith)?.docs[0];
    expect(sent && "doc" in sent && sent.span).toEqual(doc.span);
  });

  it("rejects bad shared shapes", () => {
    const page = (sig: unknown) => ({
      generation: 1,
      id: "M.hs",
      ref: "M",
      path: "M.hs",
      package: null,
      component: null,
      tier: { kind: "syntactic", reason: "no-hie" },
      parse: {
        kind: "parsed",
        header: { name: null, haddock: null, warning: null },
        exports: null,
        imports: [],
        extensions: { language: "GHC2021", enabled: [], fromPragmas: [] },
        warnings: [],
        idParts: [],
        declarations: [
          {
            kind: "value",
            n: 0,
            id: "val:g",
            name: "g",
            exported: true,
            docs: [],
            warning: null,
            fixity: null,
            line: 1,
            source: [],
            signature: sig,
            definition: "equations",
            foreignExports: [],
          },
        ],
        docItems: [],
        pragmas: [],
      },
    });
    expect(() => decodeModulePage(page({ sharedWith: 1 }))).not.toThrow();
    for (const bad of [
      { sharedWith: "val:f" },
      { sharedWith: -1 },
      { sharedWith: 1, code: [] },
      { sharedWith: null },
    ]) {
      expect(() => decodeModulePage(page(bad)), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });
});

describe("page parts reject bad shapes", () => {
  it("links: http and https only", () => {
    const link = (url: string) => ({ kind: "link", url, label: null });
    for (const url of ["http://a.b", "HTTPS://a.b/c?d#e"]) {
      expect(decodeInline(link(url), "$")).toEqual(link(url));
    }
    for (const url of [
      // Built, so the lint doesn't take it for a script URL.
      ["javascript", "alert(1)"].join(":"),
      "data:x",
      "/rel",
      "http://",
      "",
    ]) {
      expect(() => decodeInline(link(url), "$"), url).toThrow(DecodeError);
    }
  });

  it("numbers from the text are decimal text", () => {
    const list = (number: unknown) => ({
      kind: "numbered",
      items: [{ number, blocks: [] }],
    });
    expect(decodeBlock(list("9007199254740993"), "$")).toEqual(
      list("9007199254740993"),
    );
    expect(decodeBlock(list(null), "$")).toEqual(list(null));
    for (const bad of [1, "1.5", "-1", "", "x"]) {
      expect(() => decodeBlock(list(bad), "$"), String(bad)).toThrow(
        DecodeError,
      );
    }
    const since = (version: unknown) => ({
      blocks: [],
      since: { package: null, version },
    });
    expect(decodeDoc(since("1.2.3"), "$")).toEqual(since("1.2.3"));
    for (const bad of [1, "1.", "a.b", ""]) {
      expect(() => decodeDoc(since(bad), "$"), String(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("code: [class, text] or [class, text, handle]", () => {
    const good = [
      ["kw", "data"],
      ["sp", " "],
      ["con", "T", 3],
    ];
    expect(decodeCode(good, "$")).toEqual(good);
    for (const bad of [
      [["kw"]],
      [["nope", "x"]],
      [["kw", 1]],
      [["con", "T", null]],
      [["con", "T", "type:T"]],
      [["con", "T", -1]],
      [["con", "T", 3, "x"]],
      {},
    ]) {
      expect(() => decodeCode(bad, "$"), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("inline and block kinds", () => {
    for (const bad of [
      { kind: "html", text: "<b>" },
      { kind: "name", written: "x", ns: "kind", target: null },
      { kind: "math", style: "big", tex: "x" },
      { kind: "module", module: "M", anchor: null, ref: null },
    ]) {
      expect(() => decodeInline(bad, "$"), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
    for (const bad of [
      { kind: "image", url: "http://a.b" },
      { kind: "table", header: [[{ colspan: 1, inlines: [] }]], body: [] },
    ]) {
      expect(() => decodeBlock(bad, "$"), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });
});
