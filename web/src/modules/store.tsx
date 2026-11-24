// The module tree of the page's side, loaded once per side and shared
// by the sidebar, the project notices, the file view and the search
// box.
import {
  createContext,
  createMemo,
  createResource,
  type JSX,
  type ParentProps,
  useContext,
} from "solid-js";
import { getJson, type Result } from "../api/client";
import {
  decodeModuleTree,
  type ModuleChange,
  type ModuleSource,
  type ModuleTree,
} from "../api/modules";
import { apiModulesUrl } from "../docs/urls";
import { usePageSide, useSession } from "../session";
import { otherSide, type Side } from "../sides";

import { componentKey, otherKey, packageKey } from "./keys";

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
  // The long form; pairs a module with the other side's (D-349).
  id: string;
  // Its diff marker; null: unchanged, or browse mode.
  change: ModuleChange | null;
  // The module's name. Null: a loose file with no module name.
  name: string | null;
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
          id: m.id,
          change: m.change,
          name: m.name,
          path: m.source.kind === "no-source" ? null : m.source.path,
          groups: [packageKey(p.cabalFile), componentKey(p.cabalFile, c.name)],
        });
      }
    }
  }
  for (const o of tree.other) {
    if (o.kind !== "module") continue;
    add([o.ref, o.id], {
      ref: o.ref,
      id: o.id,
      change: o.change,
      name: o.name,
      path: o.path,
      groups: [otherKey],
    });
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
  // The answer for the page's side; undefined while loading.
  tree: () => Result<ModuleTree> | undefined;
  loading: () => boolean;
  // The tree once loaded, else undefined (loading or failed).
  ok: () => ModuleTree | undefined;
  bySource: () => ReadonlyMap<string, SourceInfo>;
  byRef: () => ReadonlyMap<string, RefInfo>;
  names: () => readonly ModuleName[];
  // Anything to show in the Modules view.
  hasModules: () => boolean;
  // Diff mode: the other side's tree, for ghosts (D-337). Fetched once
  // the page side's tree is in. Undefined while loading or failed.
  other: () => ModuleTree | undefined;
  // The other side's tree couldn't be loaded.
  otherFailed: () => boolean;
  // indexRefs of other(), made once.
  otherByRef: () => ReadonlyMap<string, RefInfo> | undefined;
}

const ModulesContext = createContext<Modules>();

// The module tree of the page's side. The last tree of each side is
// kept, so going back to a side is instant (D-332).
export function ModulesProvider(props: ParentProps): JSX.Element {
  const session = useSession();
  const side = usePageSide();
  const trees = new Map<Side, Result<ModuleTree>>();
  const load = async (s: Side) => {
    const got =
      trees.get(s) ?? (await getJson(apiModulesUrl(s), decodeModuleTree));
    if (got.kind === "ok") trees.set(s, got);
    return { side: s, got };
  };
  const [answer] = createResource(side, load);
  // Only the page's side: never the last side's tree while loading.
  const tree = (): Result<ModuleTree> | undefined => {
    const r = answer();
    // Safety net: createResource already drops late answers.
    return r?.side === side() ? r.got : undefined;
  };
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
  // After the page side's tree, so it never slows that one down.
  const wantOther = (): Side | undefined =>
    session.mode === "diff" && ok() !== undefined
      ? otherSide(side())
      : undefined;
  const [otherAnswer] = createResource(wantOther, load);
  const otherResult = (): Result<ModuleTree> | undefined => {
    const r = otherAnswer();
    const want = wantOther();
    return want !== undefined && r?.side === want ? r.got : undefined;
  };
  const other = (): ModuleTree | undefined => {
    const r = otherResult();
    return r?.kind === "ok" ? r.value : undefined;
  };
  const otherByRef = createMemo(() => {
    const t = other();
    return t === undefined ? undefined : indexRefs(t);
  });
  const value: Modules = {
    tree,
    loading: () => answer.loading || tree() === undefined,
    ok,
    bySource,
    byRef,
    names,
    hasModules,
    other,
    otherFailed: () => {
      const r = otherResult();
      return r !== undefined && r.kind !== "ok";
    },
    otherByRef,
  };
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
