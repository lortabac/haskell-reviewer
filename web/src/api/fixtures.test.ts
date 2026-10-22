// Feeds the API fixtures (fixtures/api, written by the Haskell tests) to
// the decoders production uses. See Note [API fixtures] in
// HaskellReviewer.Server.Wire.
import { describe, expect, it } from "vitest";
import { DecodeError } from "./decode";
import { decodeHello, parseEventData } from "./events";
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
import { fixture, fixtures } from "../../test/fixtures";

// Every fixture, with the decoder for it. One per file view kind.
const decoders: Record<string, (value: unknown) => unknown> = {
  "events-hello": decodeHello,
  files: decodeFileList,
  "file-text": decodeFileView,
  "file-binary": decodeFileView,
  "file-too-large": decodeFileView,
  "file-symlink": decodeFileView,
  "file-submodule": decodeFileView,
  modules: decodeModuleTree,
  "modules-implicit": decodeModuleTree,
};

describe("API fixtures", () => {
  it("one decoder per fixture, and the other way round", () => {
    expect([...fixtures.keys()].sort()).toEqual(Object.keys(decoders).sort());
  });

  it("every fixture decodes to itself: no field is dropped", () => {
    for (const [name, decode] of Object.entries(decoders)) {
      expect(decode(fixture(name)), name).toEqual(fixture(name));
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
    const sorted = (xs: Iterable<string>) => [...new Set(xs)].sort();
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

  it("text comes through as is", () => {
    const view = decodeFileView(fixture("file-text"));
    expect(view.kind).toBe("text");
    if (view.kind !== "text") return;
    expect(view.text).toContain("<script>alert(1)</script>");
    expect(view.text).toContain("\r\n");
    expect(view.text).toContain("héllo");
  });
});

describe("decoders reject bad shapes", () => {
  const good = { generation: 1, kind: "text", text: "x" };

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

  it("hello", () => {
    expect(decodeHello(parseEventData("{}"))).toEqual({});
    for (const bad of [undefined, 1, "{", "[]", "null", "1"]) {
      expect(() => decodeHello(parseEventData(bad))).toThrow(DecodeError);
    }
  });
});
