import { describe, expect, it } from "vitest";
import type {
  ModuleChange,
  ModuleEntry,
  ModuleTree,
  OtherFile,
} from "../api/modules";
import { componentKey, otherKey, packageKey } from "./keys";
import {
  changedModules,
  keepModule,
  keepOther,
  type ModuleView,
  unionModules,
} from "./union";

function mod(
  name: string,
  change: ModuleChange | null = null,
  settingsChanged = false,
): ModuleEntry {
  return {
    name,
    id: `p:lib:${name}`,
    ref: name,
    role: "exposed",
    source: { kind: "module", path: `src/${name}.hs` },
    change,
    settingsChanged,
  };
}

function loose(path: string, change: ModuleChange | null = null): OtherFile {
  return {
    kind: "module",
    path,
    name: null,
    id: path,
    ref: path,
    change,
    settingsChanged: false,
  };
}

function tree(
  components: Record<string, ModuleEntry[]>,
  other: OtherFile[] = [],
  cabalFile = "p/p.cabal",
): ModuleTree {
  return {
    generation: 1,
    notices: [],
    packages: [
      {
        name: "p",
        version: "1",
        cabalFile,
        components: Object.entries(components).map(([name, modules]) => ({
          name,
          buildable: true,
          modules,
        })),
      },
    ],
    broken: [{ cabalFile: "bad/bad.cabal", problem: "syntax" }],
    other,
    warnings: [],
  };
}

// "*" for a ghost.
const g = (on: string | undefined): string => (on === undefined ? "" : "*");

// Each row as "pkg/comp/Mod" with "*" for a ghost.
function rows(v: ModuleView): string[] {
  return [
    ...v.packages.flatMap((p) =>
      p.components.flatMap((c) =>
        c.modules.map(
          (m) =>
            `${p.cabalFile}${g(p.ghostOn)}/${c.name}${g(c.ghostOn)}/${m.entry.name}${g(m.ghostOn)}`,
        ),
      ),
    ),
    ...v.other.map((o) => o.file.path + g(o.ghostOn)),
  ];
}

const head = tree(
  { lib: [mod("C", "added"), mod("A", "modified"), mod("D")] },
  [loose("x/New.hs", "added"), { kind: "boot", path: "A.hs-boot" }],
);
const base = tree(
  {
    lib: [mod("A", "modified"), mod("B", "removed"), mod("D")],
    "test:t": [mod("T", "removed")],
  },
  [loose("x/Old.hs", "removed")],
);

describe("unionModules", () => {
  it("head: base's removed modules are ghosts, in name order", () => {
    expect(rows(unionModules(head, base, "head"))).toEqual([
      "p/p.cabal/lib/A",
      "p/p.cabal/lib/B*",
      "p/p.cabal/lib/C",
      "p/p.cabal/lib/D",
      "p/p.cabal/test:t*/T*",
      "A.hs-boot",
      "x/New.hs",
      "x/Old.hs*",
    ]);
  });

  it("base: head's added modules are ghosts", () => {
    expect(rows(unionModules(base, head, "base"))).toEqual([
      "p/p.cabal/lib/A",
      "p/p.cabal/lib/B",
      "p/p.cabal/lib/C*",
      "p/p.cabal/lib/D",
      "p/p.cabal/test:t/T",
      "x/New.hs*",
      "x/Old.hs",
    ]);
  });

  it("a package only on the other side is a ghost package, after the rest", () => {
    const other = tree({ lib: [mod("Q", "removed")] }, [], "q/q.cabal");
    const v = unionModules(head, other, "head");
    expect(v.packages.map((p) => [p.cabalFile, p.ghostOn])).toEqual([
      ["p/p.cabal", undefined],
      ["q/q.cabal", "base"],
    ]);
  });

  it("no other tree, or no markers (pairing failed): no ghosts", () => {
    const plain = rows(unionModules(head, undefined, "head"));
    expect(plain.some((r) => r.endsWith("*"))).toBe(false);
    const unmarked = tree({ lib: [mod("B")] }, [loose("x/Old.hs")]);
    expect(rows(unionModules(head, unmarked, "head"))).toEqual(plain);
  });

  it("tree side: no ghosts", () => {
    expect(
      rows(unionModules(head, base, "tree")).some((r) => r.endsWith("*")),
    ).toBe(false);
  });

  it("a ghost at a loose path the side has is dropped", () => {
    const other = tree({}, [loose("x/New.hs", "removed")]);
    expect(
      rows(unionModules(head, other, "head")).filter((r) =>
        r.startsWith("x/New"),
      ),
    ).toEqual(["x/New.hs"]);
  });
});

describe("changedModules", () => {
  const v = unionModules(head, base, "head");

  it("the groups with a changed module or a ghost; broken counts", () => {
    const f = changedModules(v);
    expect([...f.groups].sort()).toEqual(
      [
        packageKey("p/p.cabal"),
        componentKey("p/p.cabal", "lib"),
        componentKey("p/p.cabal", "test:t"),
        otherKey,
      ].sort(),
    );
    // A, B*, C, T*, x/New.hs, x/Old.hs*, and the broken package.
    expect(f.count).toBe(7);
  });

  it("keeps ghosts and changed rows; boot and unmarked files go", () => {
    const lib = v.packages[0]?.components[0]?.modules ?? [];
    expect(lib.filter(keepModule).map((m) => m.entry.name)).toEqual([
      "A",
      "B",
      "C",
    ]);
    expect(v.other.filter(keepOther).map((o) => o.file.path)).toEqual([
      "x/New.hs",
      "x/Old.hs",
    ]);
  });

  it("settings changed counts as changed", () => {
    const t = tree({ lib: [mod("S", null, true), mod("U")] });
    const lib = unionModules(t, undefined, "head").packages[0]?.components[0];
    expect(lib?.modules.filter(keepModule).map((m) => m.entry.name)).toEqual([
      "S",
    ]);
  });

  it("nothing changed: no groups, only broken packages count", () => {
    const t = tree({ lib: [mod("U")] });
    const f = changedModules(unionModules(t, undefined, "head"));
    expect(f.groups.size).toBe(0);
    expect(f.count).toBe(1);
  });
});
