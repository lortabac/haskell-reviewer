import { describe, expect, it } from "vitest";
import type { ChangedFile, FileStatus } from "../api/diffFiles";
import type { ModuleChange, ModuleTree } from "../api/modules";
import { unionModules } from "../modules/union";
import {
  fileCounts,
  filesLine,
  moduleCounts,
  modulesLine,
  outsideCount,
  outsideLine,
  outsideTip,
} from "./summary";

const file = (path: string, status: FileStatus): ChangedFile => ({
  path,
  oldPath: null,
  status,
  similarity: null,
  note: null,
});

const mod = (name: string, change: ModuleChange | null, settings = false) => ({
  name,
  id: `p:lib:${name}`,
  ref: name,
  role: "exposed" as const,
  source: { kind: "module" as const, path: `src/${name}.hs` },
  change,
  settingsChanged: settings,
});

const tree = (
  mods: ReturnType<typeof mod>[],
  other: ModuleTree["other"] = [],
): ModuleTree => ({
  generation: 1,
  notices: [],
  packages: [
    {
      name: "p",
      version: "1",
      cabalFile: "p.cabal",
      components: [{ name: "lib", buildable: true, modules: mods }],
    },
  ],
  broken: [],
  other,
  warnings: [],
});

const loose = (path: string, change: ModuleChange | null) => ({
  kind: "module" as const,
  path,
  name: null,
  id: path,
  ref: path,
  change,
  settingsChanged: false,
});

const counts = (head: ModuleTree, base: ModuleTree | undefined) =>
  modulesLine(moduleCounts(unionModules(head, base, "head")));

describe("counts", () => {
  it("files by status, zero parts left out", () => {
    const c = fileCounts([
      file("a", "added"),
      file("b", "added"),
      file("c", "modified"),
    ]);
    expect(filesLine(c)).toBe("3 files changed: 2 added, 1 modified.");
    expect(filesLine(fileCounts([file("a", "removed")]))).toBe(
      "1 file changed: 1 removed.",
    );
    expect(filesLine(fileCounts([]))).toBe("No files changed under ROOT.");
  });

  it("modules: removed are head's ghosts, the rest and settings from head", () => {
    const head = tree(
      [mod("A", "modified", true), mod("C", "added"), mod("D", null, true)],
      [loose("x/L.hs", "renamed"), { kind: "boot", path: "src/A.hs-boot" }],
    );
    const base = tree([mod("A", "modified"), mod("B", "removed")]);
    expect(counts(head, base)).toBe(
      "4 modules changed: 1 added, 1 removed, 1 modified, 1 renamed; " +
        "settings changed in 2 modules.",
    );
    expect(counts(head, undefined)).toBe(
      "3 modules changed: 1 added, 1 modified, 1 renamed; " +
        "settings changed in 2 modules.",
    );
  });

  it("a removed loose module at a path head has is not counted (no ghost)", () => {
    const head = tree([], [loose("x/L.hs", "added")]);
    const base = tree(
      [],
      [loose("x/L.hs", "removed"), loose("y/M.hs", "removed")],
    );
    expect(counts(head, base)).toBe("2 modules changed: 1 added, 1 removed.");
  });

  it("none; settings only", () => {
    expect(counts(tree([mod("A", null)]), undefined)).toBe(
      "No modules changed.",
    );
    expect(
      counts(tree([mod("A", null, true), mod("B", null, true)]), undefined),
    ).toBe("No modules changed; settings changed in 2 modules.");
  });

  it("a package and a component only on base: their modules are removed", () => {
    const comp = (name: string, mods: ReturnType<typeof mod>[]) => ({
      name,
      buildable: true,
      modules: mods,
    });
    const pkg = (cabalFile: string, comps: ReturnType<typeof comp>[]) => ({
      name: cabalFile,
      version: "1",
      cabalFile,
      components: comps,
    });
    const head = {
      ...tree([]),
      packages: [pkg("p.cabal", [comp("lib", [mod("A", "renamed")])])],
    };
    const base = {
      ...tree([]),
      packages: [
        pkg("p.cabal", [
          comp("lib", [mod("Z", "renamed")]),
          comp("test:t", [mod("T", "removed"), mod("U", "removed")]),
        ]),
        pkg("gone.cabal", [comp("lib", [mod("G", "removed", true)])]),
      ],
    };
    const c = moduleCounts(unionModules(head, base, "head"));
    expect(c.counts).toEqual({ added: 0, removed: 3, modified: 0, renamed: 1 });
    expect(c.settings).toBe(0);
  });
});

describe("outside ROOT", () => {
  it("N+ when truncated", () => {
    expect(outsideLine({ total: 1, paths: [], truncated: false })).toBe(
      "1 file changed outside ROOT.",
    );
    expect(outsideCount({ total: 3, paths: [], truncated: false })).toBe("3");
    expect(outsideCount({ total: 1200, paths: [], truncated: true })).toBe(
      "1,200+",
    );
  });

  it("the tip: the first 10 paths, then the rest", () => {
    const paths = Array.from({ length: 12 }, (_, i) => `o/${i}`);
    expect(
      outsideTip({ total: 3, paths: paths.slice(0, 3), truncated: false }),
    ).toBe("o/0\no/1\no/2");
    expect(outsideTip({ total: 12, paths, truncated: false })).toBe(
      [...paths.slice(0, 10), "and 2 more, listed on Home"].join("\n"),
    );
    expect(outsideTip({ total: 12, paths, truncated: true })).toContain(
      "and 2+ more, listed on Home",
    );
  });
});
