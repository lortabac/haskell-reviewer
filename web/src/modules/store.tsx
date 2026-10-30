// The module tree, loaded once and shared by the sidebar, the project
// notices, the file view and the search box.
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

export interface RefInfo {
  // The ref URLs use (the tree's, even when looked up by id).
  ref: string;
  // Null: no source.
  path: string | null;
  // The groups to open to show it.
  groups: string[];
}

// Module ref and id -> where it shows in the tree. A URL can have
// either (the ref is kept as typed, D-182).
export function indexRefs(tree: ModuleTree): Map<string, RefInfo> {
  const out = new Map<string, RefInfo>();
  const add = (keys: string[], info: RefInfo) => {
    for (const k of keys) if (!out.has(k)) out.set(k, info);
  };
  for (const p of tree.packages) {
    for (const c of p.components) {
      for (const m of c.modules) {
        add([m.ref, m.id], {
          ref: m.ref,
          path: m.source.kind === "no-source" ? null : m.source.path,
          groups: [packageKey(p.cabalFile), componentKey(p.cabalFile, c.name)],
        });
      }
    }
  }
  for (const o of tree.other) {
    if (o.kind !== "module") continue;
    add([o.ref, o.id], { ref: o.ref, path: o.path, groups: [otherKey] });
  }
  return out;
}

// A module the search box can find: one with a docs page link in the
// sidebar (it has a source).
export interface ModuleName {
  // The name, or the path for a loose file that isn't a module name.
  name: string;
  ref: string;
}

export function moduleNames(tree: ModuleTree): ModuleName[] {
  const out: ModuleName[] = [];
  for (const p of tree.packages) {
    for (const c of p.components) {
      for (const m of c.modules) {
        if (m.source.kind !== "no-source") {
          out.push({ name: m.name, ref: m.ref });
        }
      }
    }
  }
  for (const o of tree.other) {
    if (o.kind === "module") out.push({ name: o.name ?? o.path, ref: o.ref });
  }
  return out;
}

export interface Modules {
  tree: Resource<Result<ModuleTree>>;
  // The tree once loaded, else undefined (loading or failed).
  ok: () => ModuleTree | undefined;
  bySource: () => ReadonlyMap<string, SourceInfo>;
  byRef: () => ReadonlyMap<string, RefInfo>;
  names: () => readonly ModuleName[];
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
  const byRef = createMemo(() => {
    const t = ok();
    return t === undefined ? new Map<string, RefInfo>() : indexRefs(t);
  });
  const names = createMemo(() => {
    const t = ok();
    return t === undefined ? [] : moduleNames(t);
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
  const value: Modules = { tree, ok, bySource, byRef, names, hasModules };
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
