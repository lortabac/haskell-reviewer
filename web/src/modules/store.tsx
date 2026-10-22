// The module tree, loaded once and shared by the sidebar, the project
// notices and the file view.
import {
  createContext,
  createMemo,
  createResource,
  type JSX,
  type ParentProps,
  type Resource,
  useContext,
} from "solid-js";
import { getJson, type Result } from "../api/client";
import {
  decodeModuleTree,
  type ModuleSource,
  type ModuleTree,
} from "../api/modules";

// Keys of the disclosure groups in the module tree.
// JSON, since paths and component names can hold ":".
export const packageKey = (cabalFile: string): string =>
  JSON.stringify(["p", cabalFile]);
export const componentKey = (cabalFile: string, comp: string): string =>
  JSON.stringify(["c", cabalFile, comp]);
export const otherKey = "other";

export interface SourceInfo {
  // Not "no-source": those have no path.
  source: ModuleSource;
  // The groups to open to show it.
  groups: string[];
}

// Source path -> where it shows in the tree. The first entry wins when
// a file shows in several components.
export function indexSources(tree: ModuleTree): Map<string, SourceInfo> {
  const out = new Map<string, SourceInfo>();
  for (const p of tree.packages) {
    for (const c of p.components) {
      for (const m of c.modules) {
        const s = m.source;
        if (s.kind === "no-source" || out.has(s.path)) continue;
        out.set(s.path, {
          source: s,
          groups: [packageKey(p.cabalFile), componentKey(p.cabalFile, c.name)],
        });
      }
    }
  }
  for (const o of tree.other) {
    if (out.has(o.path)) continue;
    out.set(o.path, {
      source: { kind: "module", path: o.path },
      groups: [otherKey],
    });
  }
  return out;
}

export interface Modules {
  tree: Resource<Result<ModuleTree>>;
  // The tree once loaded, else undefined (loading or failed).
  ok: () => ModuleTree | undefined;
  bySource: () => ReadonlyMap<string, SourceInfo>;
  // Anything to show in the Modules view.
  hasModules: () => boolean;
}

const ModulesContext = createContext<Modules>();

export function ModulesProvider(props: ParentProps): JSX.Element {
  const [tree] = createResource(() =>
    getJson("/api/modules/tree", decodeModuleTree),
  );
  const ok = createMemo(() => {
    const r = tree();
    return r?.kind === "ok" ? r.value : undefined;
  });
  const bySource = createMemo(() => {
    const t = ok();
    return t === undefined ? new Map<string, SourceInfo>() : indexSources(t);
  });
  const hasModules = createMemo(() => {
    const t = ok();
    return (
      t !== undefined &&
      (t.packages.length > 0 ||
        t.broken.length > 0 ||
        t.other.some((o) => o.kind === "module"))
    );
  });
  const value: Modules = { tree, ok, bySource, hasModules };
  return (
    <ModulesContext.Provider value={value}>
      {props.children}
    </ModulesContext.Provider>
  );
}

export function useModules(): Modules {
  const m = useContext(ModulesContext);
  if (m === undefined) throw new Error("useModules: no ModulesProvider");
  return m;
}
