// Feeds the API fixtures (fixtures/api, written by the Haskell tests) to
// the decoders production uses. See Note [API fixtures] in
// HaskellReviewer.Server.Wire.
import { describe, expect, it } from "vitest";
import { fromCbor } from "./client";
import { DecodeError } from "./decode";
import { decodeHello, decodeIndexing, parseEventData } from "./events";
import {
  decodeFileList,
  decodeFileView,
  entryKinds,
  fileViewKinds,
} from "./files";
import {
  decodeModuleTree,
  moduleRoles,
  noSourceReasons,
  noticeKinds,
  otherKinds,
  sourceKinds,
  type ModuleEntry,
} from "./modules";
import {
  conShapes,
  declKinds,
  decodeModuleNotFound,
  decodeModulePage,
  docItemKinds,
  exportKinds,
  importLevels,
  instanceForms,
  listKinds,
  nameKeywords,
  pragmaKinds,
  qualifiedKinds,
  reasonKinds,
  subsKinds,
  typeForms,
  warningKinds,
  type ItemName,
  type ModulePage,
} from "./module";
import { decodePrefs, sidebarViews, themes } from "./prefs";
import {
  decodeSearch,
  searchKinds,
  searchNamespaces,
  searchUrl,
} from "./search";
import { blockKinds, inlineKinds } from "./richtext";
import {
  cbor,
  cborFixture,
  cborFixtures,
  fixture,
  fixtures,
} from "../../test/fixtures";

// Every fixture, with the decoder for it. One per file view kind.
const decoders: Record<string, (value: unknown) => unknown> = {
  "events-hello": decodeHello,
  "events-indexing": decodeIndexing,
  indexing: decodeIndexing,
  files: decodeFileList,
  "file-text": decodeFileView,
  "file-binary": decodeFileView,
  "file-too-large": decodeFileView,
  "file-symlink": decodeFileView,
  "file-submodule": decodeFileView,
  modules: decodeModuleTree,
  "modules-implicit": decodeModuleTree,
  "module-parsed": decodeModulePage,
  "module-parse-error": decodeModulePage,
  "module-pragma-error": decodeModulePage,
  "module-preprocessed": decodeModulePage,
  "module-cpp": decodeModulePage,
  "module-crashed": decodeModulePage,
  "module-too-slow": decodeModulePage,
  "module-generated": decodeModulePage,
  "module-no-source": decodeModulePage,
  "module-not-text": decodeModulePage,
  "module-too-large": decodeModulePage,
  "module-page": decodeModulePage,
  "module-page-sections": decodeModulePage,
  "module-not-found": decodeModuleNotFound,
  prefs: decodePrefs,
  "prefs-defaults": decodePrefs,
  search: decodeSearch,
  "search-qualified": decodeSearch,
  "search-partial": decodeSearch,
};

// The fixtures that are CBOR (D-51), with a JSON dump next to them.
const cborNames = Object.keys(decoders).filter(
  (n) =>
    n.startsWith("file-") ||
    (n.startsWith("module-") && n !== "module-not-found"),
);

const sorted = (xs: Iterable<string>) => [...new Set(xs)].sort();

describe("API fixtures", () => {
  it("one decoder per fixture, and the other way round", () => {
    expect([...fixtures.keys()].sort()).toEqual(Object.keys(decoders).sort());
  });

  it("every fixture decodes to itself: no field is dropped", () => {
    for (const [name, decode] of Object.entries(decoders)) {
      expect(decode(fixture(name)), name).toEqual(fixture(name));
    }
  });

  it("CBOR fixtures: cbor-x gives the dump, the decoder takes it", () => {
    expect([...cborFixtures.keys()].sort()).toEqual([...cborNames].sort());
    for (const name of cborNames) {
      const value = fromCbor(cborFixture(name));
      expect(value, name).toEqual(fixture(name));
      expect(decoders[name]?.(value), name).toEqual(fixture(name));
    }
  });

  it("the file view fixtures cover every kind", () => {
    const kinds = [...fixtures.keys()]
      .filter((n) => n.startsWith("file-"))
      .map((n) => decodeFileView(fixture(n)).kind);
    expect(kinds.sort()).toEqual([...fileViewKinds].sort());
  });

  it("the file list fixture covers every entry kind and a null commit", () => {
    const list = decodeFileList(fixture("files"));
    const kinds = new Set(list.entries.map((e) => e.kind));
    expect([...kinds].sort()).toEqual([...entryKinds].sort());
    expect(
      list.entries.some((e) => e.kind === "submodule" && e.commit === null),
    ).toBe(true);
  });

  it("the module tree fixtures cover every kind, role and reason", () => {
    const trees = [fixture("modules"), fixture("modules-implicit")].map(
      decodeModuleTree,
    );
    const entries = trees.flatMap((t) =>
      t.packages.flatMap((p) => p.components.flatMap((c) => c.modules)),
    );
    expect(sorted(trees.flatMap((t) => t.notices.map((n) => n.kind)))).toEqual(
      sorted(noticeKinds),
    );
    expect(sorted(entries.map((e) => e.role))).toEqual(sorted(moduleRoles));
    expect(sorted(entries.map((e) => e.source.kind))).toEqual(
      sorted(sourceKinds),
    );
    expect(
      sorted(
        entries.flatMap((e) =>
          e.source.kind === "no-source" ? [e.source.reason] : [],
        ),
      ),
    ).toEqual(sorted(noSourceReasons));
    const others = trees.flatMap((t) => t.other);
    expect(sorted(others.map((o) => o.kind))).toEqual(sorted(otherKinds));
    // A loose file with no module name, a short ref and a long one.
    expect(others.some((o) => o.kind === "module" && o.name === null)).toBe(
      true,
    );
    expect(entries.some((e) => e.ref === e.name)).toBe(true);
    expect(entries.some((e) => e.ref === e.id)).toBe(true);
    expect(trees.some((t) => t.broken.length > 0)).toBe(true);
    expect(trees.some((t) => t.warnings.length > 0)).toBe(true);
  });

  it("the module fixtures cover every reason", () => {
    const views = [...fixtures.keys()]
      .filter((n) => n.startsWith("module-") && n !== "module-not-found")
      .map((n) => decodeModulePage(fixture(n)));
    const reasons = views.flatMap((v) =>
      v.parse.kind === "text-only" ? [v.parse.reason.kind] : [],
    );
    expect(sorted(reasons)).toEqual(sorted(reasonKinds));
    expect(views.some((v) => v.parse.kind === "parsed")).toBe(true);
    expect(views.some((v) => v.path === null)).toBe(true);
    expect(views.some((v) => v.package !== null)).toBe(true);
    expect(views.some((v) => v.package === null)).toBe(true);
  });

  it("the page fixtures cover every kind of declaration, doc and pragma", () => {
    const parsed = ["module-page", "module-page-sections"].flatMap((n) => {
      const p = decodeModulePage(fixture(n)).parse;
      return p.kind === "parsed" ? [p] : [];
    });
    const decls = parsed.flatMap((p) => p.declarations);
    const kinds = new Set<string>();
    const walk = (v: unknown): void => {
      if (Array.isArray(v)) v.forEach(walk);
      else if (typeof v === "object" && v !== null) {
        const k = (v as { kind?: unknown }).kind;
        if (typeof k === "string") kinds.add(k);
        Object.values(v).forEach(walk);
      }
    };
    parsed.forEach(walk);
    for (const k of [...blockKinds, ...inlineKinds, ...declKinds]) {
      expect(kinds.has(k), k).toBe(true);
    }
    const types = decls.flatMap((d) => (d.kind === "type" ? [d] : []));
    expect(sorted(types.map((t) => t.form))).toEqual(sorted(typeForms));
    expect(
      sorted(types.flatMap((t) => t.constructors.map((c) => c.shape))),
    ).toEqual(sorted(conShapes));
    expect(
      sorted(
        decls.flatMap((d) => (d.kind === "instance" ? [d.form.kind] : [])),
      ),
    ).toEqual(sorted(instanceForms));
    expect(sorted(parsed.flatMap((p) => p.pragmas.map((x) => x.kind)))).toEqual(
      sorted(pragmaKinds),
    );
    expect(
      sorted(parsed.flatMap((p) => p.docItems.map((x) => x.kind))),
    ).toEqual(sorted(docItemKinds));
    // Placement: some instances attached, one not.
    const attached = decls.flatMap((d) =>
      d.kind === "instance" ? [d.attached] : [],
    );
    expect(sorted(attached.map(String))).toEqual(["false", "true"]);
  });

  it("the parsed module fixture covers every kind of export and import", () => {
    const view: ModulePage = decodeModulePage(fixture("module-parsed"));
    expect(view.parse.kind).toBe("parsed");
    if (view.parse.kind !== "parsed") return;
    const m = view.parse;
    const exports = m.exports ?? [];
    const items = exports.flatMap((e) => (e.kind === "item" ? [e] : []));
    const names: ItemName[] = items.map((i) => i.name);
    const warnings = [
      ...(m.header.warning ? [m.header.warning] : []),
      ...items.flatMap((i) => (i.warning ? [i.warning] : [])),
    ];
    expect(sorted(exports.map((e) => e.kind))).toEqual(sorted(exportKinds));
    expect(sorted(items.map((i) => i.subs.kind))).toEqual(sorted(subsKinds));
    expect(
      sorted(names.flatMap((n) => (n.keyword ? [n.keyword] : []))),
    ).toEqual(sorted(nameKeywords));
    expect(sorted(warnings.map((w) => w.kind))).toEqual(sorted(warningKinds));
    expect(sorted(m.imports.map((i) => i.qualified))).toEqual(
      sorted(qualifiedKinds),
    );
    expect(sorted(m.imports.map((i) => i.level))).toEqual(sorted(importLevels));
    expect(
      sorted(m.imports.flatMap((i) => (i.list ? [i.list.kind] : []))),
    ).toEqual(sorted(listKinds));
    expect(m.imports.some((i) => i.source)).toBe(true);
    expect(m.imports.some((i) => i.safe)).toBe(true);
    expect(m.imports.some((i) => i.package !== null)).toBe(true);
    expect(m.warnings.length).toBeGreaterThan(0);
  });

  it("the prefs fixtures: the defaults, and other values", () => {
    expect(decodePrefs(fixture("prefs-defaults"))).toEqual({
      theme: "system",
      exportedOnly: false,
      sidebar: "auto",
    });
    // Every key off its default.
    const other = decodePrefs(fixture("prefs"));
    expect(other.theme).not.toBe("system");
    expect(other.exportedOnly).toBe(true);
    expect(other.sidebar).not.toBe("auto");
    expect(themes).toContain(other.theme);
    expect(sidebarViews).toContain(other.sidebar);
  });

  it("the search fixtures cover every kind and namespace", () => {
    const answers = ["search", "search-qualified", "search-partial"].map((n) =>
      decodeSearch(fixture(n)),
    );
    const results = answers.flatMap((a) => a.results);
    expect(sorted(results.map((r) => r.kind))).toEqual(sorted(searchKinds));
    expect(
      sorted(results.flatMap((r) => (r.kind === "decl" ? [r.ns] : []))),
    ).toEqual(sorted(searchNamespaces));
    const decls = results.flatMap((r) => (r.kind === "decl" ? [r] : []));
    expect(decls.some((d) => !d.exported)).toBe(true);
    expect(decls.some((d) => d.parent !== null)).toBe(true);
    expect(sorted(answers.map((a) => String(a.partial)))).toEqual([
      "false",
      "true",
    ]);
    // A qualified query: insert from two modules.
    const qualified = decodeSearch(fixture("search-qualified"));
    expect(qualified.query).toBe("Data.Map.insert");
    expect(qualified.results.map((r) => r.ref)).toEqual([
      "Data.Map",
      "Data.IntMap",
    ]);
  });

  it("text comes through as is", () => {
    const view = decodeFileView(fromCbor(cborFixture("file-text")));
    expect(view.kind).toBe("text");
    if (view.kind !== "text") return;
    expect(view.text).toContain("<script>alert(1)</script>");
    expect(view.text).toContain("\r\n");
    expect(view.text).toContain("héllo");
  });
});

describe("fromCbor", () => {
  it("takes what JSON has", () => {
    const value = {
      a: [1, -2, "x", true, null, {}],
      b: { c: [] },
      n: [2 ** 53 - 1, -(2 ** 53 - 1)],
    };
    expect(fromCbor(cbor(value))).toEqual(value);
  });

  it("rejects what JSON doesn't have", () => {
    for (const bad of [
      [0xf7], // undefined
      [0xfb, 0x3f, 0xf8, 0, 0, 0, 0, 0, 0], // 1.5
      [0xf9, 0x7e, 0x00], // NaN
      [0x41, 0x78], // bytes
      [0xc1, 0x00], // tag 1: a Date
      [0xd8, 0x64, 0x00], // tag 100
      [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], // a bigint
      [0x1b, 0, 0x20, 0, 0, 0, 0, 0, 0], // 2^53
      [0x3b, 0, 0x20, 0, 0, 0, 0, 0, 0], // -2^53 - 1
      [0x81, 0xf7], // undefined in an array
      [0xa1, 0x61, 0x61, 0xf7], // undefined in a map
    ]) {
      expect(() => fromCbor(new Uint8Array(bad)), String(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("rejects cut input and bytes after the item", () => {
    for (const bad of [[], [0x82, 0x01], [0x01, 0x01]]) {
      expect(() => fromCbor(new Uint8Array(bad)), String(bad)).toThrow();
    }
  });
});

describe("decoders reject bad shapes", () => {
  const good = { generation: 1, kind: "text", text: "x" };

  it("module not found", () => {
    const good = { generation: 1, kind: "not-found", what: "module" };
    expect(decodeModuleNotFound(good)).toEqual(good);
    for (const bad of [
      { ...good, kind: "text" },
      { ...good, what: "file" },
      { ...good, generation: -1 },
      { kind: "not-found", what: "module" },
    ]) {
      expect(() => decodeModuleNotFound(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("file view", () => {
    for (const bad of [
      null,
      [],
      "x",
      {},
      { ...good, generation: "1" },
      { ...good, generation: 1.5 },
      { ...good, generation: -1 },
      { generation: 1, kind: "binary", size: -2 },
      { generation: 1, kind: "too-large", size: 2, limit: -1 },
      { ...good, kind: "nope" },
      { generation: 1, kind: "text" },
      { generation: 1, kind: "binary", size: "2" },
      { generation: 1, kind: "too-large", size: 2 },
      { generation: 1, kind: "symlink", target: "x" },
      { generation: 1, kind: "symlink", target: 1, resolved: null },
      { generation: 1, kind: "submodule" },
      { generation: 1, kind: "submodule", commit: 7 },
    ]) {
      expect(() => decodeFileView(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("file list", () => {
    const list = {
      generation: 1,
      source: "git",
      entries: [{ path: "a", kind: "file" }],
      skippedNames: 0,
      truncated: false,
    };
    expect(decodeFileList(list).entries).toEqual([{ path: "a", kind: "file" }]);
    for (const bad of [
      { ...list, source: "svn" },
      { ...list, entries: {} },
      { ...list, entries: [{ path: "a" }] },
      { ...list, entries: [{ path: 1, kind: "file" }] },
      { ...list, entries: [{ path: "a", kind: "submodule" }] },
      { ...list, truncated: "no" },
      { ...list, skippedNames: -1 },
      { ...list, skippedNames: undefined },
    ]) {
      expect(() => decodeFileList(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("module tree", () => {
    const entry: ModuleEntry = {
      name: "A",
      id: "p:lib:A",
      ref: "A",
      role: "exposed",
      source: { kind: "module", path: "A.hs" },
    };
    const withModule = (m: object) => ({
      generation: 1,
      notices: [{ kind: "stack" }],
      packages: [
        {
          name: "p",
          version: "1",
          cabalFile: "p.cabal",
          components: [{ name: "lib", buildable: true, modules: [m] }],
        },
      ],
      broken: [],
      other: [{ kind: "boot", path: "A.hs-boot" }],
      warnings: [],
    });
    const tree = withModule(entry);
    expect(decodeModuleTree(tree)).toEqual(tree);
    for (const bad of [
      { ...tree, generation: -1 },
      { ...tree, notices: [{ kind: "cabal" }] },
      { ...tree, notices: [{ kind: "hpack" }] },
      { ...tree, notices: [{ kind: "hpack", dirs: [1] }] },
      { ...tree, packages: [{ name: "p" }] },
      { ...tree, broken: [{ cabalFile: "x" }] },
      { ...tree, other: [{ kind: "module", path: "x.hs" }] },
      { ...tree, other: [{ kind: "elf", path: "x" }] },
      { ...tree, warnings: [1] },
      { ...tree, warnings: undefined },
      withModule({ ...entry, role: "hidden" }),
      withModule({ ...entry, ref: undefined }),
      withModule({ ...entry, source: { kind: "module" } }),
      withModule({ ...entry, source: { kind: "generated", path: "x.y" } }),
      withModule({ ...entry, source: { kind: "no-source", reason: "lost" } }),
      withModule({ ...entry, source: { kind: "binary", path: "x" } }),
    ]) {
      expect(() => decodeModuleTree(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("module page", () => {
    const view = {
      generation: 1,
      id: "p:lib:A",
      ref: "A",
      path: "A.hs",
      package: { name: "p", version: "1" },
      component: "lib",
      tier: { kind: "syntactic", reason: "no-hie" },
      parse: {
        kind: "text-only",
        reason: { kind: "too-slow" },
        warnings: [],
      },
    };
    expect(decodeModulePage(view)).toEqual(view);
    const parsed = (m: object) => ({
      ...view,
      parse: {
        kind: "parsed",
        header: { name: null, haddock: null, warning: null },
        exports: null,
        imports: [],
        extensions: { language: "GHC2021", enabled: [], fromPragmas: [] },
        warnings: [],
        idParts: [],
        declarations: [],
        docItems: [],
        pragmas: [],
        ...m,
      },
    });
    expect(decodeModulePage(parsed({}))).toEqual(parsed({}));
    const reason = (r: object) => ({
      ...view,
      parse: { kind: "text-only", reason: r, warnings: [] },
    });
    for (const bad of [
      { ...view, generation: -1 },
      { ...view, path: undefined },
      { ...view, package: { name: "p" } },
      { ...view, component: undefined },
      { ...view, tier: { kind: "precise", reason: "no-hie" } },
      { ...view, parse: { kind: "maybe" } },
      { ...view, parse: { ...view.parse, warnings: undefined } },
      reason({ kind: "nope" }),
      reason({ kind: "parse-error", message: "x", code: null }),
      reason({ kind: "parse-error", message: "x", code: null, span: [1, 2] }),
      reason({
        kind: "parse-error",
        message: "x",
        code: null,
        span: [1, 2, 3, -4],
      }),
      reason({ kind: "generated" }),
      reason({ kind: "no-source", reason: "lost" }),
      reason({ kind: "too-large", size: 1 }),
      parsed({ exports: [{ kind: "item" }] }),
      parsed({ exports: [{ kind: "thing" }] }),
      parsed({ header: { name: null, haddock: null } }),
      parsed({ imports: [{ span: [1, 1, 1, 2] }] }),
      parsed({ extensions: { language: "GHC2021", enabled: [] } }),
      parsed({ declarations: undefined }),
      parsed({ declarations: [{ kind: "thing" }] }),
      parsed({ docItems: [{ kind: "chunk", name: "x" }] }),
      parsed({ pragmas: [{ kind: "rules", span: [1, 1, 1, 2], names: [] }] }),
    ]) {
      expect(() => decodeModulePage(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("indexing", () => {
    const good = { generation: 1, total: 3, ready: 2, failed: 1 };
    expect(decodeIndexing(good)).toEqual(good);
    for (const bad of [
      null,
      { ...good, total: undefined },
      { ...good, ready: -1 },
      { ...good, failed: "1" },
      { ...good, generation: 1.5 },
      { ...good, ready: 3 },
    ]) {
      expect(() => decodeIndexing(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("prefs", () => {
    const good = { theme: "dark", exportedOnly: true, sidebar: "files" };
    expect(decodePrefs(good)).toEqual(good);
    for (const bad of [
      null,
      [],
      {},
      { ...good, theme: "blue" },
      { ...good, theme: undefined },
      { ...good, exportedOnly: "true" },
      { ...good, sidebar: null },
    ]) {
      expect(() => decodePrefs(bad), JSON.stringify(bad)).toThrow(DecodeError);
    }
  });

  it("search", () => {
    const decl = {
      kind: "decl",
      ns: "field",
      name: "port",
      parent: "Config",
      anchor: "field:Config.port",
      module: "Util",
      ref: "Util",
      exported: true,
    };
    const good = {
      generation: 1,
      query: "port",
      partial: false,
      matches: 2,
      results: [decl, { kind: "module", name: "Port", ref: "Port" }],
    };
    expect(decodeSearch(good)).toEqual(good);
    const withResult = (r: object) => ({ ...good, results: [r] });
    for (const bad of [
      null,
      { ...good, generation: -1 },
      { ...good, query: undefined },
      { ...good, partial: "no" },
      { ...good, matches: 1 },
      { ...good, results: {} },
      withResult({ kind: "file", name: "x", ref: "x" }),
      withResult({ kind: "module", name: "x" }),
      withResult({ ...decl, ns: "inst" }),
      withResult({ ...decl, parent: null }),
      withResult({ ...decl, ns: "val" }),
      withResult({ ...decl, anchor: undefined }),
      withResult({ ...decl, exported: "yes" }),
    ]) {
      expect(() => decodeSearch(bad), JSON.stringify(bad)).toThrow(DecodeError);
    }
  });

  it("search URL: everything encoded, + and spaces too", () => {
    expect(searchUrl("<+> a&b=c#")).toBe(
      "/api/search/tree?q=%3C%2B%3E%20a%26b%3Dc%23",
    );
    expect(searchUrl("Data.Map.insert")).toBe(
      "/api/search/tree?q=Data.Map.insert",
    );
  });

  it("hello", () => {
    expect(decodeHello(parseEventData("{}"))).toEqual({});
    for (const bad of [undefined, 1, "{", "[]", "null", "1"]) {
      expect(() => decodeHello(parseEventData(bad))).toThrow(DecodeError);
    }
  });
});
