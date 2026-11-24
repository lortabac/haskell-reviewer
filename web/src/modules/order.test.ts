import { describe, expect, it } from "vitest";
import type {
  ModuleChange,
  ModuleEntry,
  ModuleTree,
  OtherFile,
} from "../api/modules";
import { type ModuleOrder, moduleOrder, moduleStep } from "./order";

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
): ModuleTree {
  return {
    generation: 1,
    notices: [],
    packages: [
      {
        name: "p",
        version: "1",
        cabalFile: "p/p.cabal",
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

describe("moduleOrder", () => {
  it("tree order, one per ref, no no-source; changed marked; made once", () => {
    const gone: ModuleEntry = {
      ...mod("Z", "modified"),
      source: { kind: "no-source", reason: "missing" },
    };
    const t = tree(
      {
        lib: [mod("C", "added"), mod("A", "modified"), mod("D"), gone],
        "test:t": [mod("A", "modified"), mod("S", null, true)],
      },
      [loose("x/New.hs", "added"), { kind: "boot", path: "A.hs-boot" }],
    );
    const o = moduleOrder(t);
    expect(o.all.map((m) => m.name)).toEqual(["A", "C", "D", "S", "x/New.hs"]);
    expect(o.changed.map((i) => o.all[i]?.ref)).toEqual([
      "A",
      "C",
      "S",
      "x/New.hs",
    ]);
    expect(o.at.get("D")).toBe(2);
    expect(moduleOrder(t)).toBe(o);
  });
});

describe("moduleStep", () => {
  // A B* C D* E, * changed.
  const order: ModuleOrder = {
    all: ["A", "B", "C", "D", "E"].map((r) => ({ ref: r, name: `M.${r}` })),
    at: new Map(["A", "B", "C", "D", "E"].map((r, i) => [r, i])),
    changed: [1, 3],
  };
  const href = (r: string) => `/docs/head/${r}`;

  it("from changed and unchanged modules", () => {
    expect(moduleStep(order, "A", 1, href)).toEqual({
      href: "/docs/head/B",
      words: "M.B, changed module 1 of 2.",
    });
    expect(moduleStep(order, "B", 1, href)).toMatchObject({
      href: "/docs/head/D",
    });
    expect(moduleStep(order, "C", -1, href)).toMatchObject({
      href: "/docs/head/B",
    });
    expect(moduleStep(order, "E", -1, href)).toMatchObject({
      href: "/docs/head/D",
      words: "M.D, changed module 2 of 2.",
    });
  });

  it("the ends, a ref not in the tree, no changes", () => {
    expect(moduleStep(order, "D", 1, href)).toBe("No more changed modules.");
    expect(moduleStep(order, "B", -1, href)).toBe("No more changed modules.");
    expect(moduleStep(order, "Gone", 1, href)).toMatchObject({
      href: "/docs/head/B",
    });
    expect(moduleStep(order, "Gone", -1, href)).toBe(
      "No more changed modules.",
    );
    expect(moduleStep({ ...order, changed: [] }, "A", 1, href)).toBe(
      "No changed modules.",
    );
  });
});
