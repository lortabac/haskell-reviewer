// A hand-made module page: the module-page fixture with edits, for
// what the fixture doesn't have (UNPACK, foreign imports, pattern
// synonym directions, data instances, export list cases...).
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { cbor, fixture, stubFetch } from "../../test/fixtures";
import { mountAppAt, unmountApp } from "../../test/mount";
import { decodeModulePage } from "../api/module";
import { defaultPrefs } from "../api/prefs";
import { readable } from "./Decl";
import { pageIndex } from "./pageIndex";

/* eslint-disable @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return -- wire JSON edited by hand */

type Json = any;

const span = [1, 1, 1, 2];
const code = (...words: string[]): Json[] =>
  words.flatMap((w, i) => [
    ...(i === 0 ? [] : [["sp", " "]]),
    [/^[A-Z]/.test(w) ? "con" : /^[a-z_]/.test(w) ? "var" : "pun", w],
  ]);
const kw = (k: string, ...rest: string[]): Json[] => [
  ["kw", k],
  ["sp", " "],
  ...code(...rest),
];
const para = (text: string) => ({
  span,
  doc: {
    blocks: [{ kind: "paragraph", inlines: [{ kind: "text", text }] }],
    since: null,
  },
});
const info = (n: number, id: string, name: string | null) => ({
  n,
  id,
  name,
  exported: true,
  docs: [],
  warning: null,
  fixity: null,
  line: 1,
});

// The fixture, with every kind of extra.
function handmade(): Json {
  const v: Json = structuredClone(fixture("module-page"));
  const ds: Json[] = v.parse.declarations;
  const by = (name: string) => ds.find((d) => d.name === name);
  const shape = by("Shape");
  shape.kindSig = kw("type", "Shape", "::", "Type");
  shape.roles = kw("type", "role", "Shape");
  const circle = shape.constructors[0];
  circle.args[0] = {
    ...circle.args[0],
    strictness: "strict",
    unpack: "unpack",
    doc: para("The radius."),
  };
  const width = shape.constructors[1].fields[0];
  width.strictness = "lazy";
  width.unpack = "nounpack";
  const helper = by("helper");
  helper.definition = "foreign-import";
  helper.foreignExports = [kw("foreign", "export", "ccall", "helper")];
  const unit = by("Unit");
  ds.push(
    {
      ...structuredClone(unit),
      ...info(100, "pat:Mono", "Mono"),
      signature: null,
      direction: "unidirectional",
      fields: [info(101, "val:ux", "ux"), info(102, "val:uy", "uy")],
      source: [{ span: [200, 1, 200, 20], role: "head", text: "pattern Uni" }],
    },
    {
      ...structuredClone(unit),
      ...info(103, "pat:Expl", "Expl"),
      signature: null,
      direction: "explicit",
      fields: [],
      source: [{ span: [201, 1, 201, 20], role: "head", text: "pattern Ex" }],
    },
  );
  const container = by("Container");
  container.methods[0].defaultSig = kw("default", "empty", "::", "f");
  container.associated[0].defaults = [kw("type", "Elem", "f", "=", "f")];
  by("DF").instances = [110];
  v.parse.idParts.push("Int");
  ds.push({
    kind: "faminst",
    ...info(110, "", null),
    id: { inst: "DF", head: [v.parse.idParts.length - 1] },
    family: "DF",
    head: kw("data", "instance", "DF", "Int"),
    form: "data",
    constructors: [
      {
        ...info(111, "con:DA", "DA"),
        shape: "prefix",
        sharedWith: null,
        code: code("DA", "Int"),
        args: [
          { type: code("Int"), strictness: "none", unpack: "none", doc: null },
        ],
        fields: [],
      },
    ],
    derivings: [],
    orphan: false,
    attached: true,
    source: [
      {
        span: [210, 1, 210, 30],
        role: "head",
        text: "data instance DF Int = DA Int",
      },
    ],
  });
  return v;
}

let frames: FrameRequestCallback[] = [];

beforeEach(() => {
  frames = [];
  vi.stubGlobal("requestAnimationFrame", (f: FrameRequestCallback) => {
    frames.push(f);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => undefined);
  vi.spyOn(HTMLElement.prototype, "scrollIntoView").mockImplementation(
    () => undefined,
  );
});

afterEach(() => {
  unmountApp();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

function flushFrames(): void {
  const fs = frames;
  frames = [];
  for (const f of fs) f(0);
}

function stub(value: Json): void {
  stubFetch({
    "/tok/api/files/tree": [200, fixture("files")],
    "/tok/api/modules/tree": [200, fixture("modules")],
    "/tok/api/module/tree/Page": [200, cbor(value)],
  });
}

async function ready(main: HTMLElement): Promise<HTMLElement> {
  await vi.waitFor(() => {
    expect(main.querySelector(".decl")).not.toBeNull();
  });
  return main;
}

async function open(value: Json, hash = ""): Promise<HTMLElement> {
  stub(value);
  const main = mountAppAt(`/tok/docs/tree/Page${hash}`).querySelector("main");
  if (main === null) throw new Error("no main");
  return ready(main);
}

// The whole app, with "exported only" on from the start.
async function openExportedOnly(value: Json, hash = ""): Promise<HTMLElement> {
  stub(value);
  const main = mountAppAt(`/tok/docs/tree/Page${hash}`, {
    ...defaultPrefs,
    exportedOnly: true,
  }).querySelector("main");
  if (main === null) throw new Error("no main");
  return ready(main);
}

function decl(main: Element, head: string): HTMLElement {
  const s = [...main.querySelectorAll<HTMLElement>("section.decl")].find((el) =>
    el.querySelector(":scope > .decl-head .sig")?.textContent.startsWith(head),
  );
  if (s === undefined) throw new Error(`no declaration ${head}`);
  return s;
}

const texts = (els: Iterable<Element>) => [...els].map((e) => e.textContent);

describe("hand-made page: details", () => {
  it("kind signature, roles, UNPACK, strictness, argument docs", async () => {
    const main = await open(handmade());
    const shape = decl(main, "data Shape");
    expect(texts(shape.querySelectorAll(":scope > .decl-line"))).toEqual([
      "type Shape :: Type",
      "type role Shape",
    ]);
    expect(texts(shape.querySelectorAll(".decl-args li code"))).toEqual([
      "{-# UNPACK #-} !Double",
    ]);
    expect(shape.querySelector(".decl-args .doc")?.textContent).toBe(
      "The radius.",
    );
    expect(texts(shape.querySelectorAll(".subdecl .subdecl .sig"))).toContain(
      "width :: {-# NOUNPACK #-} ~Double",
    );
  });

  it("foreign import badge and foreign exports", async () => {
    const main = await open(handmade());
    const h = decl(main, "helper,");
    // helper isn't exported.
    expect(texts(h.querySelectorAll(".decl-badge"))).toEqual([
      "foreign import",
    ]);
    expect(texts(h.querySelectorAll(".decl-lines li"))).toEqual([
      "foreign export ccall helper",
    ]);
  });

  it("pattern synonyms: directions and fields", async () => {
    const main = await open(handmade());
    const uni = decl(main, "pattern Mono");
    expect(uni.querySelector(".decl-note")?.textContent).toBe(
      "Unidirectional: matches only.",
    );
    expect(texts(uni.querySelectorAll(".subdecl .sig"))).toEqual(["ux", "uy"]);
    expect(
      decl(main, "pattern Expl").querySelector(".decl-note")?.textContent,
    ).toBe("Bidirectional, with its own builder.");
  });

  it("default signature, associated defaults", async () => {
    const main = await open(handmade());
    const c = decl(main, "class Container f");
    expect(c.textContent).toContain("default empty :: f");
    expect(c.textContent).toContain("type Elem f = f");
  });

  it("data instance with constructors, under its family; anchor opens it", async () => {
    const main = await open(handmade(), "#con%3ADA");
    flushFrames();
    const df = decl(main, "data family DF a");
    const list = df.querySelector<HTMLDetailsElement>("details.decl-instances");
    expect(list?.open).toBe(true);
    expect(list?.querySelector("summary")?.textContent).toBe("Instances (1)");
    const da = document.activeElement;
    expect(da?.className).toBe("subdecl");
    expect(da?.querySelector(".sig")?.textContent).toBe("DA Int");
    expect(list?.contains(da ?? null)).toBe(true);
  });

  it("source button and link: readable names for screen readers", async () => {
    const main = await open(handmade());
    const df = decl(main, "data family DF a");
    const list = df.querySelector<HTMLDetailsElement>("details.decl-instances");
    if (list === null) throw new Error("no list");
    list.open = true;
    list.dispatchEvent(new Event("toggle"));
    expect(
      decl(list, "data instance").querySelector(".decl-source-toggle")
        ?.textContent,
    ).toBe("Show source of data instance DF Int");
    const box = decl(main, "newtype Box a").querySelector("details");
    if (box === null) throw new Error("no list");
    box.open = true;
    box.dispatchEvent(new Event("toggle"));
    expect(
      decl(box, "deriving newtype Eq").querySelector(".decl-source-toggle")
        ?.textContent,
    ).toBe("Show source of instance Eq (Box a)");
    expect(
      decl(main, "$(pure [])").querySelector(".decl-source-toggle")
        ?.textContent,
    ).toMatch(/^Show source of the splice at line \d+$/);
  });

  it("readable names from id parts: a class operator with ':'", () => {
    const v = handmade();
    v.parse.idParts.push("T a");
    const d = {
      ...structuredClone(
        v.parse.declarations.find((x: Json) => x.kind === "instance"),
      ),
      id: { inst: ":<:", head: [v.parse.idParts.length - 1] },
    };
    v.parse.declarations.push(d);
    const page = decodeModulePage(v).parse;
    if (page.kind !== "parsed") throw new Error("not parsed");
    const decoded = page.declarations[page.declarations.length - 1];
    if (decoded === undefined) throw new Error("no decl");
    expect(readable(decoded, pageIndex(page))).toBe("instance :<: (T a)");
  });

  it("repeated handle (CPP): the first copy is the anchor", async () => {
    const v = handmade();
    const ds: Json[] = v.parse.declarations;
    const area = ds.find((d) => d.name === "area");
    const copy = structuredClone(area);
    copy.docs = [];
    ds.push(copy);
    const main = await open(v, "#val%3Aarea");
    flushFrames();
    const secs = [...main.querySelectorAll("section.decl")].filter(
      (s) => s.getAttribute("data-handle") === String(area.n),
    );
    expect(secs).toHaveLength(2);
    expect(document.activeElement).toBe(secs[0]);
  });

  it("2000 names on one signature and a big doc: one link each", async () => {
    const v = handmade();
    const ds: Json[] = v.parse.declarations;
    const area = ds.find((d) => d.name === "area");
    // A big doc (about 20 KB), shared by all names.
    const big = "x".repeat(100);
    area.docs[0].doc.blocks = Array.from({ length: 200 }, () => ({
      kind: "paragraph",
      inlines: [{ kind: "text", text: big }],
    }));
    const count = 2000;
    for (let i = 0; i < count; i++) {
      const name = `p${i}`;
      area.signature.names.push(name);
      // As the server sends it: all names in the code, before "::".
      const colons = area.signature.code.findIndex((t: Json) => t[1] === "::");
      area.signature.code.splice(
        colons - 1,
        0,
        ["pun", ","],
        ["sp", " "],
        ["var", name],
      );
      ds.push({
        ...structuredClone(ds.find((d) => d.name === "perimeter")),
        ...info(1000 + i, `val:${name}`, name),
        docs: [{ span: area.docs[0].span, sharedWith: area.n }],
        source: [],
        signature: { sharedWith: area.n },
      });
    }
    const start = performance.now();
    const main = await open(v);
    const took = performance.now() - start;
    const lines = [...main.querySelectorAll(".decl-shared")];
    const count_ = (text: string) =>
      lines.filter((p) => p.textContent === text).length;
    // perimeter and the 2000: one signature link and one docs link
    // each, to area.
    expect(count_("Shares its signature with area.")).toBe(count + 1);
    expect(count_("Same docs as area.")).toBe(count + 1);
    expect(
      lines
        .filter((p) => p.textContent.endsWith(" area."))
        .every(
          (p) =>
            p.querySelector("a")?.getAttribute("href") ===
            "/tok/docs/tree/Page#val%3Aarea",
        ),
    ).toBe(true);
    // The big doc once: else 2000 × 20 KB = 40 MB of text.
    expect(main.querySelectorAll(".doc p").length).toBeLessThan(400);
    expect(main.textContent.length).toBeLessThan(2_000_000);
    // Rough bound: quadratic would be 4 million links.
    expect(main.querySelectorAll("a").length).toBeLessThan(10 * count);
    expect(took).toBeLessThan(20_000);
  });
});

describe("hand-made page: exported only", () => {
  it("hidden declaration: anchor gives the notice, with its name", async () => {
    const main = await openExportedOnly(handmade(), "#type%3AP2");
    flushFrames();
    await vi.waitFor(() => {
      const notice = main.querySelector(".decl-missing")?.textContent;
      expect(notice).toContain("P2");
      expect(notice).toContain("is hidden by Exported only.");
    });
    expect(main.querySelector("section.decl[data-handle='30']")).toBeNull();
  });

  it("instances of a hidden type: hidden too; Show all reveals it", async () => {
    const v = handmade();
    const box = v.parse.declarations.find((d: Json) => d.name === "Box");
    box.exported = false;
    box.constructors[0].exported = false;
    const main = await openExportedOnly(v, "#inst%3AEq%3ABox%20a");
    flushFrames();
    // Box is hidden; its only lister, so the instance is too (D-204),
    // not even in the Instances block.
    expect(
      [...main.querySelectorAll(".decl-head .sig")].map((s) => s.textContent),
    ).not.toContain("deriving newtype Eq");
    const notice = main.querySelector(".decl-missing");
    expect(notice?.textContent).toContain("is hidden by Exported only.");
    notice?.querySelector<HTMLButtonElement>("button")?.click();
    flushFrames();
    const focused = document.activeElement as HTMLElement | null;
    expect(focused?.querySelector(".decl-head .sig")?.textContent).toBe(
      "deriving newtype Eq",
    );
    expect(main.querySelector(".decl-missing")).toBeNull();
  });

  it("a method exported alone keeps its class", async () => {
    const v = handmade();
    const c = v.parse.declarations.find((d: Json) => d.name === "Container");
    c.exported = false;
    for (const m of c.methods) m.exported = false;
    c.associated[0].exported = false;
    c.methods[1].exported = true;
    await openExportedOnly(v, "#val%3Ainsert");
    flushFrames();
    expect(document.activeElement?.querySelector(".sig")?.textContent).toBe(
      "insert :: Elem f -> f -> f",
    );
  });
});

describe("hand-made page: export list (mode B)", () => {
  const section = (text: string, level = 1) => ({
    kind: "section",
    level,
    heading: { span, inlines: [{ kind: "text", text }] },
  });
  const item = (occ: string, extra: Json = {}) => ({
    kind: "item",
    span,
    name: { span, qualifier: null, occ, keyword: null },
    subs: { kind: "none" },
    doc: null,
    warning: null,
    ...extra,
  });
  const warning = (m: string) => ({
    span,
    kind: "warning",
    category: null,
    messages: [m],
  });

  function modeB(): Json {
    const v = handmade();
    v.parse.exports = [
      // Starts at level 2: shown as h2, no skip.
      section("Deep", 2),
      item("area", { doc: para("First listing.") }),
      { kind: "module", name: { span, value: "Page" }, warning: null },
      section("Later"),
      item("area", {
        doc: para("Second listing."),
        warning: warning("Careful"),
      }),
      item("Box"),
      item("empty"),
      { kind: "module", name: { span, value: "L" }, warning: null },
      { kind: "module", name: { span, value: "Foo" }, warning: null },
    ];
    // Foo is in the tree (modules fixture), but here it's an alias.
    v.parse.imports = [
      {
        span,
        name: { span, value: "Data.List" },
        package: null,
        source: false,
        safe: false,
        qualified: "post",
        level: "normal",
        as: { span, value: "Foo" },
        list: null,
      },
    ];
    return v;
  }

  it("order, docs and warnings per listing, module Self, aliases", async () => {
    const main = await open(modeB());
    const order = [
      ...main.querySelectorAll(
        ".docs-section, section.decl, .docs-page > .doc, .docs-reexport, .docs-page > .decl-warning",
      ),
    ].map((e) =>
      e.matches("section")
        ? `decl ${e.querySelector(".sig")?.textContent ?? ""}`
        : `${e.tagName} ${e.textContent}`,
    );
    // From the first heading (the module doc comes before).
    order.splice(0, order.indexOf("H2 Deep"));
    const at = (s: string) => order.indexOf(s);
    expect(order[0]).toBe("H2 Deep");
    expect(order[1]).toBe("decl area, perimeter :: Shape -> Double");
    expect(order[2]).toBe("DIV First listing.");
    // module Page: exported ones not named in the list; Box is named
    // later, so it stays there.
    expect(order[3]).toBe("decl data Shape");
    expect(at("decl newtype Box a")).toBeGreaterThan(at("H2 Later"));
    expect(order.slice(at("H2 Later") + 1, at("H2 Later") + 3)).toEqual([
      "DIV Second listing.",
      "DIV Warning: Careful",
    ]);
    expect(order).toContain("P empty (in Container)");
    // Foo is an alias of Data.List: no link to the tree's Foo.
    const foo = [...main.querySelectorAll(".docs-reexport")].find((p) =>
      p.textContent.startsWith("module Foo"),
    );
    expect(foo?.querySelector("a")).toBeNull();
    // Every heading one level below or at most one deeper.
    expect(main.querySelector("h3.docs-section")).toBeNull();
  });

  it("an exported declaration the list misses: Also exported", async () => {
    const v = modeB();
    v.parse.exports = v.parse.exports.filter(
      (e: Json) => !(e.kind === "module" && e.name.value === "Page"),
    );
    const main = await open(v);
    const heads = texts(main.querySelectorAll(".docs-section"));
    expect(heads).toEqual([
      "Deep",
      "Later",
      "Also exported",
      "Private",
      "Instances",
    ]);
    const also = [...main.querySelectorAll(".docs-section")].find(
      (h) => h.textContent === "Also exported",
    );
    expect(also?.nextElementSibling?.querySelector(".sig")?.textContent).toBe(
      "data Shape",
    );
  });
});
