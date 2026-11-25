// Feeds the API fixtures (fixtures/api, written by the Haskell tests) to
// the decoders production uses. See Note [API fixtures] in
// HaskellReviewer.Server.Wire.
import { describe, expect, it } from "vitest";
import { fromCbor } from "./client";
import { DecodeError } from "./decode";
import {
  decodeGeneration,
  decodeHello,
  decodeIndexing,
  parseEventData,
} from "./events";
import { decodeReloaded } from "./reload";
import {
  decodeFileList,
  decodeFileView,
  entryKinds,
  fileViewKinds,
} from "./files";
import { changeNotes, decodeDiffFiles, statuses } from "./diffFiles";
import {
  bomChanges,
  decodeDiffView,
  diffKinds,
  diffUrl,
  ops,
  sideKinds,
  statuses as diffStatuses,
} from "./diff";
import {
  decodeModuleTree,
  moduleChanges,
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
import { decodeMarkdownPage } from "./markdown";
import { decodePrefs, diffLayouts, sidebarViews, themes } from "./prefs";
import { decodeSession } from "./session";
import { decodeRefs, decodeRefsNotFound, refKinds, refsUrl } from "./refs";
import {
  decodeSearch,
  searchKinds,
  searchNamespaces,
  searchUrl,
} from "./search";
import { blockKinds, inlineKinds } from "./richtext";
import { decodeTextSearch, textSearchUrl } from "./textSearch";
import {
  decodeSourceNotFound,
  decodeSourceView,
  displayClasses,
  expandRuns,
  guessHows,
  outlineKinds,
  reasonCodes,
  regionKinds,
  targetKinds,
} from "./source";
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
  "events-generation": decodeGeneration,
  reload: decodeReloaded,
  indexing: decodeIndexing,
  files: decodeFileList,
  "file-text": decodeFileView,
  "file-binary": decodeFileView,
  "file-too-large": decodeFileView,
  "file-symlink": decodeFileView,
  "file-submodule": decodeFileView,
  "file-filtered": decodeFileView,
  "file-not-local": decodeFileView,
  modules: decodeModuleTree,
  "modules-implicit": decodeModuleTree,
  "modules-diff": decodeModuleTree,
  "diff-files": decodeDiffFiles,
  "diff-files-worktree": decodeDiffFiles,
  "diff-files-limit": decodeDiffFiles,
  "diff-modified": decodeDiffView,
  "diff-renamed": decodeDiffView,
  "diff-added": decodeDiffView,
  "diff-removed": decodeDiffView,
  "diff-unchanged": decodeDiffView,
  "diff-binary": decodeDiffView,
  "diff-too-large": decodeDiffView,
  "diff-submodule": decodeDiffView,
  "diff-symlink": decodeDiffView,
  "diff-type-changed": decodeDiffView,
  "diff-eol": decodeDiffView,
  "diff-ws-ignore": decodeDiffView,
  "diff-too-slow": decodeDiffView,
  "diff-not-local": decodeDiffView,
  "diff-filtered": decodeDiffView,
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
  "module-page-links": decodeModulePage,
  "module-page-partial": decodeModulePage,
  "module-not-found": decodeModuleNotFound,
  prefs: decodePrefs,
  "prefs-defaults": decodePrefs,
  refs: decodeRefs,
  "refs-partial": decodeRefs,
  "refs-not-found": decodeRefsNotFound,
  search: decodeSearch,
  "search-qualified": decodeSearch,
  "search-partial": decodeSearch,
  "source-parsed": decodeSourceView,
  "source-cpp": decodeSourceView,
  "source-partial": decodeSourceView,
  "source-text-only": decodeSourceView,
  "source-plain": decodeSourceView,
  "source-markdown": decodeSourceView,
  "markdown-page": decodeMarkdownPage,
  "markdown-not-text": decodeMarkdownPage,
  "markdown-filtered": decodeMarkdownPage,
  "markdown-not-local": decodeMarkdownPage,
  "source-not-local": decodeSourceView,
  "source-warm-up": decodeSourceView,
  "source-not-found": decodeSourceNotFound,
  "text-search": decodeTextSearch,
  "text-search-capped": decodeTextSearch,
  "session-browse": decodeSession,
  "session-diff": decodeSession,
  "session-three-dot": decodeSession,
  "session-worktree": decodeSession,
};

// The fixtures that are CBOR (D-51), with a JSON dump next to them.
const cborNames = Object.keys(decoders).filter(
  (n) =>
    n.startsWith("file-") ||
    (n.startsWith("module-") && n !== "module-not-found") ||
    (n.startsWith("source-") && n !== "source-not-found") ||
    n.startsWith("markdown-") ||
    (n.startsWith("diff-") && !n.startsWith("diff-files")),
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

  it("the file view fixtures cover every kind, filtered or not", () => {
    const views = [...fixtures.keys()]
      .filter((n) => n.startsWith("file-"))
      .map((n) => decodeFileView(fixture(n)));
    expect(sorted(views.map((v) => v.kind))).toEqual(sorted(fileViewKinds));
    expect(sorted(views.map((v) => String(v.filtered)))).toEqual([
      "false",
      "true",
    ]);
  });

  it("the Markdown fixtures cover every kind, filtered or not", () => {
    const pages = [...fixtures.keys()]
      .filter((n) => n.startsWith("markdown-"))
      .map((n) => decodeMarkdownPage(fixture(n)));
    expect(sorted(pages.map((p) => p.kind))).toEqual([
      "doc",
      "not-local",
      "not-text",
    ]);
    expect(sorted(pages.map((p) => String(p.filtered)))).toEqual([
      "false",
      "true",
    ]);
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

  it("the diff fixtures cover every marker, status and note", () => {
    const tree = decodeModuleTree(fixture("modules-diff"));
    const marked = [
      ...tree.packages.flatMap((p) => p.components.flatMap((c) => c.modules)),
      ...tree.other.flatMap((o) => (o.kind === "module" ? [o] : [])),
    ];
    expect(sorted(marked.flatMap((m) => (m.change ? [m.change] : [])))).toEqual(
      sorted(moduleChanges),
    );
    expect(marked.some((m) => m.change === null)).toBe(true);
    expect(sorted(marked.map((m) => String(m.settingsChanged)))).toEqual([
      "false",
      "true",
    ]);
    const diffs = ["diff-files", "diff-files-worktree", "diff-files-limit"].map(
      (n) => decodeDiffFiles(fixture(n)),
    );
    const files = diffs.flatMap((d) => d.files);
    expect(sorted(files.map((f) => f.status))).toEqual(sorted(statuses));
    expect(sorted(files.flatMap((f) => (f.note ? [f.note] : [])))).toEqual(
      sorted(changeNotes),
    );
    expect(diffs.some((d) => d.head.commit === null)).toBe(true);
    expect(diffs.some((d) => d.outside === null)).toBe(true);
    expect(diffs.some((d) => d.outside?.truncated === true)).toBe(true);
    expect(sorted(diffs.map((d) => String(d.renameLimitHit)))).toEqual([
      "false",
      "true",
    ]);
    expect(sorted(diffs.map((d) => String(d.truncated)))).toEqual([
      "false",
      "true",
    ]);
    for (const d of diffs) {
      const paths = d.files.map((f) => f.path);
      expect(paths, "sorted by path").toEqual([...paths].sort());
    }
  });

  it("the diff view fixtures cover every value", () => {
    const views = [...fixtures.keys()]
      .filter((n) => n.startsWith("diff-") && !n.startsWith("diff-files"))
      .map((n) => decodeDiffView(fixture(n)));
    expect(sorted(views.map((v) => v.status))).toEqual(sorted(diffStatuses));
    expect(sorted(views.flatMap((v) => (v.note ? [v.note] : [])))).toEqual(
      sorted(changeNotes),
    );
    const sides = views.flatMap((v) => [v.base, v.head]);
    expect(sorted(sides.map((s) => s.kind))).toEqual(sorted(sideKinds));
    expect(sides.some((s) => s.kind === "submodule" && s.commit === null)).toBe(
      true,
    );
    expect(sides.some((s) => s.kind === "submodule" && s.commit !== null)).toBe(
      true,
    );
    const bodies = views.map((v) => v.diff);
    expect(sorted(bodies.map((d) => d.kind))).toEqual(sorted(diffKinds));
    const lines = bodies.flatMap((d) => (d.kind === "lines" ? [d] : []));
    expect(sorted(lines.flatMap((d) => d.runs.map((r) => r[0])))).toEqual(
      sorted(ops),
    );
    expect(sorted(lines.flatMap((d) => (d.bom ? [d.bom] : [])))).toEqual(
      sorted(bomChanges),
    );
    expect(lines.some((d) => d.crlfLines > 0)).toBe(true);
    expect(sorted(views.map((v) => String(v.ignoreWhitespace)))).toEqual([
      "false",
      "true",
    ]);
    expect(views.some((v) => v.similarity !== null)).toBe(true);
  });

  it("diff view: bad values are errors", () => {
    const good = fixture("diff-modified") as Record<string, unknown>;
    const diff = good.diff as Record<string, unknown>;
    for (const bad of [
      { ...good, status: "copied" },
      { ...good, ignoreWhitespace: undefined },
      { ...good, base: { kind: "dir" } },
      { ...good, head: { kind: "binary" } },
      { ...good, diff: { kind: "huge" } },
      { ...good, diff: { ...diff, runs: [["equal", 0]] } },
      { ...good, diff: { ...diff, runs: [["same", 1]] } },
      { ...good, diff: { ...diff, runs: [["equal", 1, 2]] } },
      { ...good, diff: { ...diff, bom: "both" } },
    ]) {
      expect(() => decodeDiffView(bad)).toThrow(DecodeError);
    }
  });

  it("diff URL: each segment encoded, ws only when ignored", () => {
    expect(diffUrl("src/A.hs", false)).toBe("/api/diff/src/A.hs");
    expect(diffUrl("a b/#?%.hs", true)).toBe(
      "/api/diff/a%20b/%23%3F%25.hs?ws=ignore",
    );
  });

  it("diff files: bad values are errors", () => {
    const good = fixture("diff-files") as { files: object[] };
    const file = good.files[0];
    for (const bad of [
      { ...good, outside: undefined },
      { ...good, renameLimitHit: null },
      { ...good, truncated: undefined },
      { ...good, head: { label: "x" } },
      { ...good, files: [{ ...file, status: "copied" }] },
      { ...good, files: [{ ...file, note: "binary" }] },
      { ...good, files: [{ ...file, similarity: -1 }] },
      { ...good, files: [{ ...file, oldPath: undefined }] },
    ]) {
      expect(() => decodeDiffFiles(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
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
    // Markdown nodes come from a Markdown file.
    const markdown = decodeMarkdownPage(fixture("markdown-page"));
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
    walk(markdown);
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
      singleKeyShortcuts: true,
      changedOnly: false,
      diffLayout: "unified",
    });
    // Every key off its default.
    const other = decodePrefs(fixture("prefs"));
    expect(other.theme).not.toBe("system");
    expect(other.exportedOnly).toBe(true);
    expect(other.sidebar).not.toBe("auto");
    expect(other.singleKeyShortcuts).toBe(false);
    expect(other.changedOnly).toBe(true);
    expect(other.diffLayout).toBe("split");
    expect(diffLayouts).toContain(other.diffLayout);
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

  it("the refs fixtures cover every kind, guesses, partial", () => {
    const answers = ["refs", "refs-partial"].map((n) => decodeRefs(fixture(n)));
    const results = answers.flatMap((a) => a.results);
    expect(sorted(results.map((r) => r.kind))).toEqual(sorted(refKinds));
    expect(sorted(results.map((r) => String(r.heuristic)))).toEqual([
      "false",
      "true",
    ]);
    expect(sorted(answers.map((a) => String(a.partial)))).toEqual([
      "false",
      "true",
    ]);
    // Home first, then by module name.
    const refs = decodeRefs(fixture("refs"));
    expect([...new Set(refs.results.map((r) => r.ref))]).toEqual([
      "R.Shape",
      "R.Guess",
      "R.Use",
    ]);
    expect(refs.total).toBe(refs.results.length);
    expect(decodeRefsNotFound(fixture("refs-not-found")).what).toBe("decl");
  });

  it("the text search fixtures: kinds skipped, capped, long line", () => {
    const all = decodeTextSearch(fixture("text-search"));
    const capped = decodeTextSearch(fixture("text-search-capped"));
    expect(all.total).toBe(all.results.length);
    expect(capped.total).toBe(all.total);
    expect(capped.results).toEqual(all.results.slice(0, 2));
    expect(all.skipped.binary).toBeGreaterThan(0);
    expect(all.skipped.tooLarge).toBeGreaterThan(0);
    expect(all.skipped.unreadable).toBeGreaterThan(0);
    // A long line is cut; 25 hits on a line send 20.
    expect(all.results.some((r) => r.lineCol > 1)).toBe(true);
    expect(Math.max(...all.results.map((r) => r.matches.length))).toBe(20);
  });

  it("the source fixtures cover every value", () => {
    const views = [...fixtures.keys()]
      .filter((n) => n.startsWith("source-") && n !== "source-not-found")
      .map((n) => decodeSourceView(fixture(n)));
    expect(sorted(views.map((v) => v.highlight))).toEqual([
      "lexed",
      "not-local",
      "plain",
    ]);
    expect(sorted(views.map((v) => String(v.partial)))).toEqual([
      "false",
      "true",
    ]);
    expect(sorted(views.map((v) => String(v.filtered)))).toEqual([
      "false",
      "true",
    ]);
    expect(views.some((v) => v.ref === null)).toBe(true);
    const cover = decodeSourceView(fixture("source-warm-up"));
    if (cover.highlight !== "lexed") throw new Error("not lexed");
    const ts = cover.targets;
    expect(sorted(ts.map((t) => t[0]))).toEqual(sorted(targetKinds));
    const reasons = ts.flatMap((t) =>
      t[0] === "decl" ? t[5] : t[0] === "module" ? t[3] : [],
    );
    expect(sorted(reasons)).toEqual(sorted(reasonCodes));
    const defs = ts.flatMap((t) => (t[0] === "decl" ? [String(t[8])] : []));
    expect(sorted([...new Set(defs)])).toEqual(["false", "true"]);
    const externals = ts.flatMap((t) => (t[0] === "external" ? [t] : []));
    expect(sorted(externals.map((t) => t[3]))).toEqual(sorted(guessHows));
    expect(sorted(externals.map((t) => String(t[2] === null)))).toEqual([
      "false",
      "true",
    ]);
    const runs = expandRuns(cover.runs);
    expect(sorted(runs.map((r) => String(r.cls)))).toEqual(
      sorted(displayClasses.map((_, i) => String(i))),
    );
    expect(runs.some((r) => r.link !== 0)).toBe(true);
    const kinds = cover.regions.filter((_, i) => i % 3 === 0);
    expect(sorted(kinds.map(String))).toEqual(
      sorted(regionKinds.map((_, i) => String(i))),
    );
    expect(sorted(cover.outline.map((e) => e[0]))).toEqual(
      sorted(outlineKinds),
    );
    const labels = cover.outline.map((e) => e[2]);
    expect(labels).toContain("instance C Int Int");
    expect(labels).toContain("data instance V Int");
    expect(labels).toContain("instance Show T");
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
  const good = { generation: 1, filtered: false, kind: "text", text: "x" };

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
    const head = { generation: 1, filtered: false };
    expect(decodeFileView(good)).toEqual(good);
    for (const bad of [
      null,
      [],
      "x",
      {},
      { ...good, generation: "1" },
      { ...good, generation: 1.5 },
      { ...good, generation: -1 },
      { ...head, kind: "binary", size: -2 },
      { ...head, kind: "too-large", size: 2, limit: -1 },
      { ...good, kind: "nope" },
      { ...head, kind: "text" },
      { ...head, kind: "binary", size: "2" },
      { ...head, kind: "too-large", size: 2 },
      { ...head, kind: "symlink", target: "x" },
      { ...head, kind: "symlink", target: 1, resolved: null },
      { ...head, kind: "submodule" },
      { ...head, kind: "submodule", commit: 7 },
      { ...good, filtered: "no" },
      { generation: 1, kind: "text", text: "x" },
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
      change: "renamed",
      settingsChanged: true,
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
      withModule({ ...entry, change: "moved" }),
      withModule({ ...entry, change: undefined }),
      withModule({ ...entry, settingsChanged: null }),
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
      partial: false,
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
        targets: [],
        declarations: [],
        docItems: [],
        pragmas: [],
        ...m,
      },
    });
    expect(decodeModulePage(parsed({}))).toEqual(parsed({}));
    // A pragma with this code.
    const code = (c: unknown[]) => ({
      kind: "rules",
      span: [1, 1, 1, 2],
      names: [],
      code: c,
    });
    const linked = parsed({
      targets: [["external", "Prelude", null, "only-open", 1, "T"]],
      pragmas: [code([["con", "T", null, 1]])],
    });
    expect(decodeModulePage(linked)).toEqual(linked);
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
      { ...view, partial: undefined },
      parsed({ targets: undefined }),
      parsed({ targets: [["local", 1, 1]] }),
      // Links past the table: a code token, a doc name.
      parsed({ targets: [], pragmas: [code([["con", "T", null, 1]])] }),
      parsed({ pragmas: [code([["con", "T", 1, 1]])] }),
      parsed({ pragmas: [code([["con", "T", null, 0]])] }),
      parsed({
        docItems: [
          {
            kind: "heading",
            level: 1,
            inlines: [
              { kind: "name", written: "x", ns: "any", target: null, link: 1 },
            ],
            span: [1, 1, 1, 2],
          },
        ],
      }),
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
    const good = { generation: 1, side: "tree", total: 3, ready: 2, failed: 1 };
    expect(decodeIndexing(good)).toEqual(good);
    expect(decodeIndexing({ ...good, side: "base" }).side).toBe("base");
    for (const bad of [
      null,
      { ...good, side: undefined },
      { ...good, side: "work" },
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

  it("session: the fixtures, and bad payloads", () => {
    expect(decodeSession(fixture("session-browse"))).toEqual({
      mode: "browse",
    });
    const three = decodeSession(fixture("session-three-dot"));
    expect(three.mode === "diff" && three.mergeBases).toBe(2);
    const work = decodeSession(fixture("session-worktree"));
    expect(work.mode === "diff" && work.head).toEqual({
      label: "working tree",
      commit: null,
    });
    const good = fixture("session-diff") as Record<string, unknown>;
    for (const bad of [
      null,
      {},
      { mode: "browser" },
      { ...good, spec: undefined },
      { ...good, base: { label: "main", commit: null } },
      { ...good, base: { label: "main" } },
      { ...good, head: { label: "x" } },
      { ...good, mergeBases: 0 },
      { ...good, mergeBases: undefined },
    ]) {
      expect(() => decodeSession(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("prefs", () => {
    const good = {
      theme: "dark",
      exportedOnly: true,
      sidebar: "files",
      singleKeyShortcuts: false,
      changedOnly: true,
      diffLayout: "split",
    };
    expect(decodePrefs(good)).toEqual(good);
    for (const bad of [
      null,
      [],
      {},
      { ...good, theme: "blue" },
      { ...good, theme: undefined },
      { ...good, exportedOnly: "true" },
      { ...good, sidebar: null },
      { ...good, singleKeyShortcuts: undefined },
      { ...good, singleKeyShortcuts: "false" },
      { ...good, changedOnly: undefined },
      { ...good, changedOnly: 1 },
      { ...good, diffLayout: undefined },
      { ...good, diffLayout: "wide" },
      { ...good, diffLayout: true },
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
    expect(searchUrl("tree", "<+> a&b=c#")).toBe(
      "/api/search/tree?q=%3C%2B%3E%20a%26b%3Dc%23",
    );
    expect(searchUrl("tree", "Data.Map.insert")).toBe(
      "/api/search/tree?q=Data.Map.insert",
    );
  });

  it("refs", () => {
    const ref = {
      module: "A",
      ref: "A",
      path: "src/A.hs",
      span: [1, 2, 1, 3],
      kind: "use",
      heuristic: true,
      reasons: ["ambiguous"],
      line: "x = a",
      lineCol: 1,
    };
    const good = {
      generation: 1,
      module: "A",
      decl: "val:a",
      partial: false,
      total: 2,
      results: [ref, { ...ref, heuristic: false, reasons: [], path: null }],
    };
    expect(decodeRefs(good)).toEqual(good);
    const withRef = (r: object) => ({ ...good, results: [r] });
    for (const bad of [
      null,
      { ...good, generation: -1 },
      { ...good, decl: undefined },
      { ...good, partial: "no" },
      { ...good, total: 1 },
      { ...good, results: {} },
      withRef({ ...ref, kind: "def" }),
      withRef({ ...ref, span: [1, 2, 3] }),
      withRef({ ...ref, reasons: [] }),
      withRef({ ...ref, heuristic: false }),
      withRef({ ...ref, reasons: ["nope"] }),
      withRef({ ...ref, line: undefined }),
      withRef({ ...ref, lineCol: 0 }),
      withRef({ ...ref, path: undefined }),
    ]) {
      expect(() => decodeRefs(bad), JSON.stringify(bad)).toThrow(DecodeError);
    }
    const missing = { generation: 1, kind: "not-found", what: "module" };
    expect(decodeRefsNotFound(missing)).toEqual(missing);
    for (const bad of [
      { ...missing, what: "file" },
      { ...missing, kind: "text" },
    ]) {
      expect(() => decodeRefsNotFound(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("refs URL: both params encoded, + too", () => {
    expect(refsUrl("tree", "R.Shape", "val:<+>")).toBe(
      "/api/refs/tree?module=R.Shape&decl=val%3A%3C%2B%3E",
    );
    expect(refsUrl("tree", "p:lib:A", "field:T.f")).toBe(
      "/api/refs/tree?module=p%3Alib%3AA&decl=field%3AT.f",
    );
  });

  it("text search", () => {
    const hit = {
      path: "a.hs",
      line: 3,
      lineCol: 1,
      text: "main = x",
      matches: [[1, 5]],
    };
    const good = {
      generation: 1,
      query: "main",
      partial: false,
      stopped: false,
      total: 2,
      files: 1,
      skipped: { binary: 0, tooLarge: 1, unreadable: 0, overBudget: 0 },
      results: [hit],
    };
    expect(decodeTextSearch(good)).toEqual(good);
    const withHit = (h: object) => ({ ...good, results: [h] });
    for (const bad of [
      null,
      { ...good, generation: -1 },
      { ...good, stopped: "no" },
      { ...good, total: 0 },
      { ...good, files: 3 },
      { ...good, skipped: { ...good.skipped, overBudget: -1 } },
      { ...good, skipped: undefined },
      withHit({ ...hit, line: 0 }),
      withHit({ ...hit, lineCol: 0 }),
      withHit({ ...hit, matches: [[1]] }),
      withHit({ ...hit, matches: [[3, 3]] }),
      withHit({ ...hit, matches: [[0, 2]] }),
      withHit({ ...hit, text: undefined }),
    ]) {
      expect(() => decodeTextSearch(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("text search URL: q encoded, + and spaces too; limit if given", () => {
    expect(textSearchUrl("tree", 'a "b+c')).toBe(
      "/api/text-search/tree?q=a%20%22b%2Bc",
    );
    expect(textSearchUrl("tree", "where", 8)).toBe(
      "/api/text-search/tree?q=where&limit=8",
    );
  });

  it("generation and reload: the fixtures, and bad payloads", () => {
    expect(decodeGeneration(fixture("events-generation"))).toEqual({
      generation: 2,
    });
    expect(decodeReloaded(fixture("reload"))).toEqual({ generation: 2 });
    for (const bad of [
      null,
      [],
      {},
      { generation: 0 },
      { generation: -1 },
      { generation: 1.5 },
      { generation: "2" },
    ]) {
      expect(() => decodeGeneration(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
      expect(() => decodeReloaded(bad), JSON.stringify(bad)).toThrow(
        DecodeError,
      );
    }
  });

  it("hello", () => {
    expect(decodeHello(parseEventData("{}"))).toEqual({});
    for (const bad of [undefined, 1, "{", "[]", "null", "1"]) {
      expect(() => decodeHello(parseEventData(bad))).toThrow(DecodeError);
    }
  });
});
